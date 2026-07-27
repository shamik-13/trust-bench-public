**free
//**********************************************************************
//  変更履歴
//  版数   年月日      担当     概要
//  0.10   2019.04.01  山下     初版作成
//  0.20   2020.10.15  杉本     リボ利用可否判定追加
//  0.30   2022.06.20  宮崎     承認番号採番処理を内製化
//  0.40   2024.02.05  岡部     限度枠仮押さえ更新時の例外処理追加
//**********************************************************************

ctl-opt dftactgrp(*no) actgrp(*caller)
        option(*srcstmt:*nodebugio)
        bnddir('QC2LE')
        datfmt(*iso) timfmt(*iso);

dcl-f CDMEMF  usage(*input) keyed;
dcl-f CDLIMTF usage(*update) keyed;
dcl-f CDREVF  usage(*input) keyed;
dcl-f CDAUTHF usage(*output);

 /copy QRPGLESRC,CDMEMC
 /copy QRPGLESRC,CDLIMTC
 /copy QRPGLESRC,CDREVFC
 /copy QRPGLESRC,CDAUTHFC

dcl-pi CA106R;
  piCardNo       char(16) const;
  piMemberSts    char(1)  const;
  piAvailAmt     packed(13:0) const;
  piRevoKbn      char(1)  const;
  piShopCd       char(10) const;
  piUseAmt       packed(13:0) const;
  piTranDate     packed(8:0)  const;
  piTranTime     packed(6:0)  const;
  poResultKbn    char(1);
  poReasonCd     char(3);
  poAuthNo       char(12);
  poApprovalCd   char(6);
end-pi;

dcl-ds Inq qualified;
  CardNo         char(16);
  MemberSts      char(1);
  AvailAmt       packed(13:0);
  RevoKbn        char(1);
  ShopCd         char(10);
  UseAmt         packed(13:0);
  TranDate       packed(8:0);
  TranTime       packed(6:0);
end-ds;

dcl-ds Wk qualified;
  AuthNo         char(12);
  ApprovalCd     char(6);
  ResultKbn      char(1);
  ReasonCd       char(3);
  CallRtnCd      char(2);
  ChkCd          char(2);
  HoldAmt        packed(13:0);
  NewUsedAmt     packed(13:0);
  AvailAfter     packed(13:0);
  FeeRate        packed(5:4) inz(0.0125);
  FeeAmt         packed(13:0);
  ErrSw          ind inz(*off);
end-ds;

dcl-s wkMsg          char(80);
dcl-s wkToday        packed(8:0);
dcl-s wkDateZ        zoned(8:0);
dcl-s wkTimeZ        zoned(6:0);
dcl-s wkLimitFound   ind inz(*off);
dcl-s wkMemberFound  ind inz(*off);
dcl-s wkRevoFound    ind inz(*off);
dcl-s wkAuthWritten  ind inz(*off);

dcl-c C_OK           '0';
dcl-c C_HOLD         '1';
dcl-c C_DENY         '2';

dcl-c R_NORMAL       '000';
dcl-c R_CARDERR      '101';
dcl-c R_MEMSTOP      '102';
dcl-c R_LIMIT        '201';
dcl-c R_REVO         '301';
dcl-c R_REVO_STOP    '302';
dcl-c R_SHOPCHK      '401';
dcl-c R_SYSERR       '900';

dcl-c RV_ACTIVE      '01';
dcl-c RV_SUSPEND     '02';
dcl-c RV_CANCEL      '03';

dcl-c REVO_RATE      0.0125;

//**********************************************************************
//  主処理
//**********************************************************************

clear poResultKbn;
clear poReasonCd;
clear poAuthNo;
clear poApprovalCd;

Inq.CardNo    = piCardNo;
Inq.MemberSts = piMemberSts;
Inq.AvailAmt  = piAvailAmt;
Inq.RevoKbn   = piRevoKbn;
Inq.ShopCd    = piShopCd;
Inq.UseAmt    = piUseAmt;
Inq.TranDate  = piTranDate;
Inq.TranTime  = piTranTime;

Wk.ResultKbn  = C_DENY;
Wk.ReasonCd   = R_SYSERR;
Wk.HoldAmt    = Inq.UseAmt;
wkToday       = %dec(%char(%date():*iso0):8:0);

exsr SrValidateRequest;

if not Wk.ErrSw;
  exsr SrReadMember;
endif;

if not Wk.ErrSw;
  exsr SrReadLimit;
endif;

if not Wk.ErrSw and Inq.RevoKbn = '1';
  exsr SrCheckRevo;
endif;

if not Wk.ErrSw;
  exsr SrShopCheck;
endif;

if not Wk.ErrSw;
  exsr SrDecision;
endif;

if not Wk.ErrSw and Wk.ResultKbn = C_OK;
  exsr SrNumbering;
endif;

if not Wk.ErrSw and Wk.ResultKbn = C_OK;
  exsr SrWriteAuth;
endif;

if not Wk.ErrSw and Wk.ResultKbn = C_OK;
  exsr SrUpdateLimit;
endif;

poResultKbn  = Wk.ResultKbn;
poReasonCd   = Wk.ReasonCd;
poAuthNo     = Wk.AuthNo;
poApprovalCd = Wk.ApprovalCd;

*inlr = *on;
return;

//**********************************************************************
//  入力検証
//**********************************************************************
begsr SrValidateRequest;

  if %trim(Inq.CardNo) = *blanks
     or %len(%trim(Inq.CardNo)) < 14;
    Wk.ResultKbn = C_DENY;
    Wk.ReasonCd  = R_CARDERR;
    Wk.ErrSw     = *on;
  endif;

  if not Wk.ErrSw and Inq.UseAmt <= 0;
    Wk.ResultKbn = C_DENY;
    Wk.ReasonCd  = R_LIMIT;
    Wk.ErrSw     = *on;
  endif;

  if not Wk.ErrSw and Inq.MemberSts <> '1';
    Wk.ResultKbn = C_DENY;
    Wk.ReasonCd  = R_MEMSTOP;
    Wk.ErrSw     = *on;
  endif;

endsr;

//**********************************************************************
//  会員状態取得
//**********************************************************************
begsr SrReadMember;

  wkMemberFound = *off;

  monitor;
    chain Inq.CardNo CDMEMR;
    if %found(CDMEMF);
      wkMemberFound = *on;
    endif;
  on-error;
    wkMsg = '会員ファイル読込エラー';
    Wk.ResultKbn = C_HOLD;
    Wk.ReasonCd  = R_SYSERR;
    Wk.ErrSw     = *on;
  endmon;

  if not Wk.ErrSw and not wkMemberFound;
    Wk.ResultKbn = C_DENY;
    Wk.ReasonCd  = R_CARDERR;
    Wk.ErrSw     = *on;
  endif;

  if not Wk.ErrSw;
    select;
    when CM-MEMBER-STS = '1';
      // 有効会員
    when CM-MEMBER-STS = '2';
      Wk.ResultKbn = C_DENY;
      Wk.ReasonCd  = R_MEMSTOP;
      Wk.ErrSw     = *on;
    when CM-MEMBER-STS = '3';
      Wk.ResultKbn = C_DENY;
      Wk.ReasonCd  = R_CARDERR;
      Wk.ErrSw     = *on;
    other;
      Wk.ResultKbn = C_HOLD;
      Wk.ReasonCd  = R_SYSERR;
      Wk.ErrSw     = *on;
    endsl;
  endif;

endsr;

//**********************************************************************
//  限度枠取得
//**********************************************************************
begsr SrReadLimit;

  wkLimitFound = *off;

  monitor;
    chain Inq.CardNo CDLIMTR;
    if %found(CDLIMTF);
      wkLimitFound = *on;
    endif;
  on-error;
    wkMsg = '限度枠ファイル読込エラー';
    Wk.ResultKbn = C_HOLD;
    Wk.ReasonCd  = R_SYSERR;
    Wk.ErrSw     = *on;
  endmon;

  if not Wk.ErrSw and not wkLimitFound;
    Wk.ResultKbn = C_DENY;
    Wk.ReasonCd  = R_LIMIT;
    Wk.ErrSw     = *on;
  endif;

endsr;

//**********************************************************************
//  リボ利用可否確認
//**********************************************************************
begsr SrCheckRevo;

  wkRevoFound = *off;

  monitor;
    chain Inq.CardNo CDREVR;
    if %found(CDREVF);
      wkRevoFound = *on;
    endif;
  on-error;
    wkMsg = 'リボファイル読込エラー';
    Wk.ResultKbn = C_HOLD;
    Wk.ReasonCd  = R_SYSERR;
    Wk.ErrSw     = *on;
  endmon;

  if not Wk.ErrSw and not wkRevoFound;
    Wk.ResultKbn = C_DENY;
    Wk.ReasonCd  = R_REVO;
    Wk.ErrSw     = *on;
  endif;

  if not Wk.ErrSw;
    select;
    when RV-REV-STATUS = RV_ACTIVE;
      if %trim(RV-REV-COURSE-CD) = *blanks;
        Wk.ResultKbn = C_DENY;
        Wk.ReasonCd  = R_REVO;
        Wk.ErrSw     = *on;
      endif;

    when RV-REV-STATUS = RV_SUSPEND or RV-REV-STATUS = RV_CANCEL;
      Wk.ResultKbn   = C_DENY;
      Wk.ReasonCd    = R_REVO_STOP;
      Wk.ErrSw       = *on;

    other;
      Wk.ResultKbn = C_HOLD;
      Wk.ReasonCd  = R_REVO;
      Wk.ErrSw     = *on;
    endsl;
  endif;

endsr;

//**********************************************************************
//  加盟店簡易確認
//**********************************************************************
begsr SrShopCheck;

  // 加盟店コードの妥当性を簡易確認する。
  //   空白          → 加盟店エラー（拒否）
  //   先頭が'9'      → 要確認加盟店（保留）
  //   それ以外       → 通常加盟店
  clear Wk.ChkCd;

  select;
  when %trim(Inq.ShopCd) = *blanks;
    Wk.ChkCd = '20';
  when %subst(Inq.ShopCd:1:1) = '9';
    Wk.ChkCd = '10';
  other;
    Wk.ChkCd = '00';
  endsl;

  if not Wk.ErrSw;
    select;
    when Wk.ChkCd = '00';
      // 通常加盟店
    when Wk.ChkCd = '10';
      Wk.ResultKbn = C_HOLD;
      Wk.ReasonCd  = R_SHOPCHK;
      Wk.ErrSw     = *on;
    other;
      Wk.ResultKbn = C_DENY;
      Wk.ReasonCd  = R_SHOPCHK;
      Wk.ErrSw     = *on;
    endsl;
  endif;

endsr;

//**********************************************************************
//  与信判定
//**********************************************************************
begsr SrDecision;

  Wk.AvailAfter = Inq.AvailAmt - Wk.HoldAmt;

  if Inq.AvailAmt < Wk.HoldAmt;
    Wk.ResultKbn = C_DENY;
    Wk.ReasonCd  = R_LIMIT;
    Wk.ErrSw     = *on;
  else;

    select;
    when Inq.RevoKbn = '1' and RV-REV-STATUS = RV_ACTIVE;
      Wk.FeeAmt = %int(RV-REV-BAL * REVO_RATE);
      Wk.ResultKbn = C_OK;
      Wk.ReasonCd  = R_NORMAL;

    when Inq.RevoKbn = '0';
      Wk.ResultKbn = C_OK;
      Wk.ReasonCd  = R_NORMAL;

    other;
      Wk.ResultKbn = C_DENY;
      Wk.ReasonCd  = R_REVO;
      Wk.ErrSw     = *on;
    endsl;

  endif;

endsr;

//**********************************************************************
//  承認番号採番
//**********************************************************************
begsr SrNumbering;

  clear Wk.AuthNo;
  clear Wk.ApprovalCd;
  clear Wk.CallRtnCd;

  // 承認番号は取引日時から採番し、承認コードは時刻6桁を流用する。
  monitor;
    wkDateZ = Inq.TranDate;
    wkTimeZ = Inq.TranTime;
    Wk.AuthNo     = %char(wkDateZ) + %subst(%char(wkTimeZ):1:4);
    Wk.ApprovalCd = %char(wkTimeZ);
    Wk.CallRtnCd  = '00';
  on-error;
    Wk.CallRtnCd = '99';
    Wk.ResultKbn = C_HOLD;
    Wk.ReasonCd  = R_SYSERR;
    Wk.ErrSw     = *on;
  endmon;

  if not Wk.ErrSw and Wk.CallRtnCd <> '00';
    Wk.ResultKbn = C_HOLD;
    Wk.ReasonCd  = R_SYSERR;
    Wk.ErrSw     = *on;
  endif;

endsr;

//**********************************************************************
//  承認明細記録
//**********************************************************************
begsr SrWriteAuth;

  wkAuthWritten = *off;

  clear CDAUTHR;

  AU-CARD-NO     = Inq.CardNo;
  AU-AUTH-NO     = Wk.AuthNo;
  AU-APPROVAL-CD = Wk.ApprovalCd;
  AU-SHOP-CD     = Inq.ShopCd;
  AU-USE-AMT     = Inq.UseAmt;
  AU-AUTH-DATE   = Inq.TranDate;
  AU-AUTH-TIME   = Inq.TranTime;
  AU-RESULT-KBN  = Wk.ResultKbn;
  AU-REASON-CD   = Wk.ReasonCd;
  AU-REVO-KBN    = Inq.RevoKbn;

  if Inq.RevoKbn = '1';
    AU-REV-COURSE-CD = RV-REV-COURSE-CD;
  else;
    clear AU-REV-COURSE-CD;
  endif;

  monitor;
    write CDAUTHR;
    wkAuthWritten = *on;
  on-error;
    Wk.ResultKbn = C_HOLD;
    Wk.ReasonCd  = R_SYSERR;
    Wk.ErrSw     = *on;
  endmon;

endsr;

//**********************************************************************
//  限度枠仮押さえ更新
//**********************************************************************
begsr SrUpdateLimit;

  monitor;
    chain(e) Inq.CardNo CDLIMTR;
    if %found(CDLIMTF);
      Wk.NewUsedAmt = LM-USED-AMT + Wk.HoldAmt;
      LM-USED-AMT   = Wk.NewUsedAmt;
      LM-UPD-DATE   = wkToday;
      LM-UPD-TIME   = Inq.TranTime;
      update CDLIMTR;
    else;
      Wk.ResultKbn = C_HOLD;
      Wk.ReasonCd  = R_LIMIT;
      Wk.ErrSw     = *on;
    endif;
  on-error;
    Wk.ResultKbn = C_HOLD;
    Wk.ReasonCd  = R_SYSERR;
    Wk.ErrSw     = *on;
  endmon;

endsr;
