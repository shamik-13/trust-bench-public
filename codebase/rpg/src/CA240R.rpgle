**free
ctl-opt dftactgrp(*no) actgrp(*caller)
        option(*srcstmt:*nodebugio)
        main(Main);

//---------------------------------------------------------------------
// 変更履歴
// 版数  年月日    担当   概要
// 0.10  20240115  YK     初版作成（取消受付）
// 0.20  20240209  YK     金額照合と売上確定判定を追加
// 0.30  20240318  MN     保留期限判定と履歴項目更新を追加
//---------------------------------------------------------------------

/copy QRPGLESRC,CDAUTHC

dcl-pr CA240R extpgm('CA240R');
  pReqCardNo       char(19) const;
  pReqAuthNo       char(6)  const;
  pReqTranYmd      char(8)  const;
  pReqTranHms      char(6)  const;
  pReqAmount       packed(11:0) const;
  pReqTermId       char(8)  const;
  pReqShopCd       char(10) const;
  pReqOpeId        char(8)  const;
  pRtnCd           char(2);
  pRtnMsg          char(60);
  pOrgTranYmd      char(8);
  pOrgTranHms      char(6);
  pOrgAmount       packed(11:0);
end-pr;

dcl-pi CA240R;
  pReqCardNo       char(19) const;
  pReqAuthNo       char(6)  const;
  pReqTranYmd      char(8)  const;
  pReqTranHms      char(6)  const;
  pReqAmount       packed(11:0) const;
  pReqTermId       char(8)  const;
  pReqShopCd       char(10) const;
  pReqOpeId        char(8)  const;
  pRtnCd           char(2);
  pRtnMsg          char(60);
  pOrgTranYmd      char(8);
  pOrgTranHms      char(6);
  pOrgAmount       packed(11:0);
end-pi;

dcl-f CDAUTHF usage(*update) keyed usropn rename(CDAUTHR:AuthR);

dcl-ds AuthRec likeds(cdauthf_rec);

dcl-ds Req qualified;
  CardNo           char(19);
  AuthNo           char(6);
  TranYmd          char(8);
  TranHms          char(6);
  Amount           packed(11:0);
  TermId           char(8);
  ShopCd           char(10);
  OpeId            char(8);
end-ds;

dcl-ds Wk qualified;
  Found            ind inz(*off);
  UpdOk            ind inz(*off);
  Err              ind inz(*off);
  Today            char(8);
  NowTime          char(6);
  AuthKey          char(25);
  MaskCard         char(19);
  HoldYmd          char(8);
  CaptFlg          char(1);
  Status           char(1);
  ReasonCd         char(2);
  SaveRtnCd        char(2);
  SaveMsg          char(60);
  DiffAmount       packed(11:0);
  ZeroAmt          packed(11:0) inz(0);
end-ds;

dcl-s Ix              packed(3:0);
dcl-s NumericOk       ind;
dcl-s WorkDigit       char(1);
dcl-s WorkCard        char(19);
dcl-s WorkYmd         char(8);
dcl-s WorkHms         char(6);

dcl-c ST_APPROVED     'A';
dcl-c ST_CANCELLED    'C';
dcl-c ST_DECLINED     'D';

dcl-c RTN_OK          '00';
dcl-c RTN_REQERR      '10';
dcl-c RTN_NOTFOUND    '14';
dcl-c RTN_AMTDIFF     '54';
dcl-c RTN_CAPTURED    '55';
dcl-c RTN_EXPIRED     '56';
dcl-c RTN_CANCELED    '57';
dcl-c RTN_SYSERR      '90';

dcl-proc Main;
  Init();
  ValidateRequest();

  if pRtnCd = *blanks;
    monitor;
      if not %open(CDAUTHF);
        open CDAUTHF;
      endif;

      ChainAuth();

      if Wk.Found;
        JudgeCancel();
        if pRtnCd = RTN_OK;
          UpdateCancel();
        endif;
      else;
        SetReturn(RTN_NOTFOUND:'承認番号またはカード番号が一致しません');
      endif;

    on-error;
      Wk.Err = *on;
      SetReturn(RTN_SYSERR:'取消受付処理でファイル入出力エラーが発生しました');
    endmon;
  endif;

  if %open(CDAUTHF);
    close CDAUTHF;
  endif;

  return;
end-proc;

dcl-proc Init;
  clear Req;
  clear Wk;

  Req.CardNo  = %trim(pReqCardNo);
  Req.AuthNo  = %trim(pReqAuthNo);
  Req.TranYmd = pReqTranYmd;
  Req.TranHms = pReqTranHms;
  Req.Amount  = pReqAmount;
  Req.TermId  = %trim(pReqTermId);
  Req.ShopCd  = %trim(pReqShopCd);
  Req.OpeId   = %trim(pReqOpeId);

  pRtnCd      = *blanks;
  pRtnMsg     = *blanks;
  pOrgTranYmd = *blanks;
  pOrgTranHms = *blanks;
  pOrgAmount  = 0;

  Wk.Today    = %char(%date():*iso0);
  Wk.NowTime  = %char(%time():*hms0);
  Wk.MaskCard = MaskCardNo(Req.CardNo);
end-proc;

dcl-proc ValidateRequest;
  if Req.CardNo = *blanks
     or Req.AuthNo = *blanks
     or Req.TranYmd = *blanks
     or Req.TranHms = *blanks;
    SetReturn(RTN_REQERR:'取消要求の必須項目が不足しています');
    return;
  endif;

  if Req.Amount <= Wk.ZeroAmt;
    SetReturn(RTN_REQERR:'取消要求金額が不正です');
    return;
  endif;

  if not IsNumeric(%subst(Req.AuthNo:1:6));
    SetReturn(RTN_REQERR:'承認番号が不正です');
    return;
  endif;

  WorkCard = %xlate('- ':'' : Req.CardNo);
  if %len(%trim(WorkCard)) < 14 or %len(%trim(WorkCard)) > 16;
    SetReturn(RTN_REQERR:'カード番号桁数が不正です');
    return;
  endif;

  if not IsNumeric(%trim(WorkCard));
    SetReturn(RTN_REQERR:'カード番号が不正です');
    return;
  endif;

  if not IsNumeric(Req.TranYmd) or not IsNumeric(Req.TranHms);
    SetReturn(RTN_REQERR:'取引日時が不正です');
    return;
  endif;
end-proc;

dcl-proc ChainAuth;
  dcl-s wkCardNo char(16);

  wkCardNo   = %xlate('- ':'' : Req.CardNo);
  Wk.AuthKey = %trim(wkCardNo) + Req.AuthNo;

  chain (wkCardNo:Req.AuthNo) AuthR AuthRec;

  if %found(CDAUTHF);
    Wk.Found    = *on;
    pOrgTranYmd = %char(AuthRec.au_auth_dt);
    pOrgTranHms = %subst(AuthRec.au_auth_tm:1:6);
    pOrgAmount  = AuthRec.au_auth_amt;
    if AuthRec.au_hold_exp_dt > 0;
      Wk.HoldYmd = %char(AuthRec.au_hold_exp_dt);
    else;
      Wk.HoldYmd = *blanks;
    endif;
    Wk.Status   = AuthRec.au_auth_result;
  else;
    Wk.Found = *off;
  endif;
end-proc;

dcl-proc JudgeCancel;
  Wk.DiffAmount = AuthRec.au_auth_amt - Req.Amount;

  select;
  when Wk.Status = ST_CANCELLED;
    SetReturn(RTN_CANCELED:'既に取消済みの承認です');

  when Wk.DiffAmount <> Wk.ZeroAmt;
    SetReturn(RTN_AMTDIFF:'承認金額と取消要求金額が一致しません');
    KeepHistory(RTN_AMTDIFF);

  when Wk.HoldYmd <> *blanks and Wk.HoldYmd < Req.TranYmd;
    SetReturn(RTN_EXPIRED:'承認保留期限を経過しています');
    KeepHistory(RTN_EXPIRED);

  when Wk.Status <> ST_APPROVED;
    SetReturn(RTN_CANCELED:'取消対象外の承認状態です');
    KeepHistory(RTN_CANCELED);

  other;
    SetReturn(RTN_OK:'取消を受け付けました');
  endsl;
end-proc;

dcl-proc UpdateCancel;
  AuthRec.au_auth_result = ST_CANCELLED;
  AuthRec.au_approval_cd = Req.AuthNo;

  update AuthR AuthRec;
  Wk.UpdOk = *on;
end-proc;

dcl-proc KeepHistory;
  dcl-pi *n;
    inReasonCd char(2) const;
  end-pi;

  Wk.SaveRtnCd = inReasonCd;
end-proc;

dcl-proc SetReturn;
  dcl-pi *n;
    inRtnCd     char(2) const;
    inMsg       char(60) const;
  end-pi;

  pRtnCd  = inRtnCd;
  pRtnMsg = inMsg;
end-proc;

dcl-proc IsNumeric;
  dcl-pi *n ind;
    inValue varchar(32) const;
  end-pi;

  NumericOk = *on;

  if %len(%trim(inValue)) = 0;
    return *off;
  endif;

  for Ix = 1 to %len(%trim(inValue));
    WorkDigit = %subst(%trim(inValue):Ix:1);
    if WorkDigit < '0' or WorkDigit > '9';
      NumericOk = *off;
      leave;
    endif;
  endfor;

  return NumericOk;
end-proc;

dcl-proc MaskCardNo;
  dcl-pi *n char(19);
    inCardNo char(19) const;
  end-pi;

  WorkCard = %trim(inCardNo);

  if %len(%trim(WorkCard)) >= 10;
    return %subst(WorkCard:1:6)
         + '******'
         + %subst(WorkCard:%len(%trim(WorkCard))-3:4);
  endif;

  return '************';
end-proc;
