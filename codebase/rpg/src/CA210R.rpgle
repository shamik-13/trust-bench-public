**free
//**********************************************************************
//  変更履歴
//  版数  年月日    担当    概要
//  1.00  20210401  山下    初版作成
//  1.10  20220715  杉本    延滞カードの請求対象判定を見直し
//  1.20  20231108  森      簡易リスク判定連携と否認履歴出力を追加
//  1.30  20250521  田辺    金額形式検証と保留期限算出を強化
//**********************************************************************

ctl-opt dftactgrp(*no) actgrp('CAONLINE')
        option(*srcstmt:*nodebugio)
        main(Main);

//**********************************************************************
//  レコード様式
//**********************************************************************
/copy CDCARDFC
/copy CDMSTC
/copy CDAUTHC

//**********************************************************************
//  外部プログラム
//**********************************************************************
dcl-pr CA220R extpgm('CA220R');
  pCardNo        char(16) const;
  pProcDate      packed(8:0) const;
  pProcTime      packed(6:0) const;
  pAvailAmt      packed(13:0);
  pRespCd        char(2);
  pReason        char(40);
end-pr;

dcl-pr CA230R extpgm('CA230R');
  pCardNo        char(16) const;
  pMerchantId    char(15) const;
  pTermId        char(8)  const;
  pReqDate       char(8)  const;
  pReqTime       char(6)  const;
  pAmount        packed(11:0) const;
  pMcc           char(4)  const;
  pEntryMode     char(2)  const;
  pEci           char(2)  const;
  pAuthType      char(1)  const;
  pDecision      char(1);
  pWarnCd        char(2);
  pReason        char(40);
end-pr;

//**********************************************************************
//  入出力パラメータ
//**********************************************************************
dcl-pi *n;
  piCardNo       char(16) const;
  piShopNo       char(10) const;
  piTermNo       char(8)  const;
  piReqAmtText   char(12) const;
  piTranYmd      char(8)  const;
  piTranHms      char(6)  const;
  piAuthResult   char(1);
  piAuthNo       char(6);
  piReasonCd     char(2);
  piOperatorMsg  char(60);
end-pi;

//**********************************************************************
//  ファイル
//**********************************************************************
dcl-f CDCARDF usage(*input) keyed;
dcl-f CDMEMSTATF usage(*input) keyed;
dcl-f CDAUTHF usage(*output) keyed rename(CDAUTHR:AuthR);

dcl-ds Card    likeds(cdcardf_rec);
dcl-ds Mem     likeds(cdmemstatf_rec);
dcl-ds AuthOut likeds(cdauthf_rec);

//**********************************************************************
//  定数
//**********************************************************************
dcl-c C_CARD_OK        '01';
dcl-c C_CARD_STOP      '02';
dcl-c C_CARD_CLOSE     '03';
dcl-c C_CARD_DELINQ    '09';

dcl-c C_BILL_CONF      'C';
dcl-c C_BILL_HOLD      'H';
dcl-c C_BILL_SKIP      'S';

dcl-c C_APPR           'A';
dcl-c C_DECL           'D';
dcl-c C_HOLD           'H';

dcl-c R_OK             '00';
dcl-c R_FORMAT         '10';
dcl-c R_CARD_NOTF      '11';
dcl-c R_MEMBER_NOTF    '12';
dcl-c R_CARD_STOP      '21';
dcl-c R_CARD_CLOSE     '22';
dcl-c R_LIMIT          '31';
dcl-c R_RISK           '41';
dcl-c R_SYSTEM         '99';

//**********************************************************************
//  作業領域
//**********************************************************************
dcl-ds Req qualified;
  CardNo          char(16);
  ShopNo          char(10);
  TermNo          char(8);
  AmtText         char(12);
  TranYmd         char(8);
  TranHms         char(6);
  ReqAmt          packed(11:0);
end-ds;

dcl-ds Auth qualified;
  Result          char(1);
  AuthNo          char(6);
  ReasonCd        char(2);
  BillStatus      char(1);
  HoldLimitYmd    char(8);
  AvailAmt        packed(13:0);
  RiskDecision    char(1);
  LimitRc         char(2);
  RiskRc          char(2);
  LimitReason     char(40);
  RiskReason      char(40);
end-ds;

dcl-ds Work qualified;
  I               int(10);
  DigitCnt        int(10);
  SumOdd          int(10);
  SumEven         int(10);
  OneDigit        int(10);
  CheckDigit      int(10);
  CalcDigit       int(10);
  Num             packed(12:0);
  TmpAmt          packed(11:0);
  NowTime         timestamp;
  HoldDate        date;
  Seq             packed(7:0);
  WriteOk         ind;
  CardFound       ind;
  MemberFound     ind;
  AmtOk           ind;
  CardOk          ind;
end-ds;

dcl-s wkChar       char(1);
dcl-s wkDate       date;
dcl-s wkMsg        char(60);

//**********************************************************************
//  主処理
//**********************************************************************
dcl-proc Main;

  exsr Init;
  exsr ValidateRequest;

  if Auth.ReasonCd = R_OK;
    exsr ReadCard;
  endif;

  if Auth.ReasonCd = R_OK;
    exsr ReadMember;
  endif;

  if Auth.ReasonCd = R_OK;
    exsr CheckCardStatus;
  endif;

  if Auth.ReasonCd = R_OK;
    exsr CallLimit;
  endif;

  if Auth.ReasonCd = R_OK;
    exsr CallRisk;
  endif;

  if Auth.ReasonCd = R_OK;
    exsr Approve;
  else;
    exsr Decline;
  endif;

  exsr WriteHistory;
  exsr SetReturn;

  return;

  //********************************************************************
  //  初期化
  //********************************************************************
  begsr Init;

    clear Req;
    clear Auth;
    clear Work;

    Req.CardNo  = piCardNo;
    Req.ShopNo  = piShopNo;
    Req.TermNo  = piTermNo;
    Req.AmtText = piReqAmtText;
    Req.TranYmd = piTranYmd;
    Req.TranHms = piTranHms;

    Auth.Result       = C_DECL;
    Auth.ReasonCd     = R_OK;
    Auth.BillStatus   = C_BILL_HOLD;
    Auth.HoldLimitYmd = *blanks;
    Auth.AuthNo       = *blanks;
    Auth.LimitRc      = R_OK;
    Auth.RiskRc       = R_OK;

    piAuthResult  = C_DECL;
    piAuthNo      = *blanks;
    piReasonCd    = *blanks;
    piOperatorMsg = *blanks;

  endsr;

  //********************************************************************
  //  受付項目検証
  //********************************************************************
  begsr ValidateRequest;

    if %trim(Req.CardNo) = *blanks
       or %len(%trim(Req.CardNo)) <> 16
       or %trim(Req.ShopNo) = *blanks
       or %trim(Req.TermNo) = *blanks
       or %trim(Req.TranYmd) = *blanks
       or %trim(Req.TranHms) = *blanks;
      Auth.ReasonCd = R_FORMAT;
      wkMsg = '受付電文の必須項目が不足しています';
      leavesr;
    endif;

    Work.DigitCnt = 0;
    Work.SumOdd   = 0;
    Work.SumEven  = 0;

    for Work.I = 1 to 16;
      wkChar = %subst(Req.CardNo: Work.I: 1);
      if wkChar < '0' or wkChar > '9';
        Auth.ReasonCd = R_FORMAT;
        wkMsg = 'カード番号の形式が不正です';
        leavesr;
      endif;

      Work.OneDigit = %int(wkChar);
      if Work.I < 16;
        if %rem(Work.I: 2) = 1;
          Work.OneDigit = Work.OneDigit * 2;
          if Work.OneDigit > 9;
            Work.OneDigit = Work.OneDigit - 9;
          endif;
          Work.SumOdd += Work.OneDigit;
        else;
          Work.SumEven += Work.OneDigit;
        endif;
      else;
        Work.CheckDigit = Work.OneDigit;
      endif;
    endfor;

    Work.CalcDigit = %rem((10 - %rem(Work.SumOdd + Work.SumEven: 10)): 10);
    if Work.CalcDigit <> Work.CheckDigit;
      Auth.ReasonCd = R_FORMAT;
      wkMsg = 'カード番号のチェック桁が不正です';
      leavesr;
    endif;

    Work.AmtOk = *on;
    for Work.I = 1 to %len(Req.AmtText);
      wkChar = %subst(Req.AmtText: Work.I: 1);
      if wkChar = ' ';
        iterate;
      endif;
      if wkChar < '0' or wkChar > '9';
        Work.AmtOk = *off;
        leave;
      endif;
    endfor;

    if not Work.AmtOk or %trim(Req.AmtText) = *blanks;
      Auth.ReasonCd = R_FORMAT;
      wkMsg = '取引金額の形式が不正です';
      leavesr;
    endif;

    monitor;
      Work.Num = %dec(%trim(Req.AmtText): 12: 0);
      Req.ReqAmt = %dec(Work.Num: 11: 0);
    on-error;
      Auth.ReasonCd = R_FORMAT;
      wkMsg = '取引金額の桁数が不正です';
      leavesr;
    endmon;

    if Req.ReqAmt <= 0;
      Auth.ReasonCd = R_FORMAT;
      wkMsg = '取引金額がゼロ以下です';
      leavesr;
    endif;

    monitor;
      wkDate = %date(Req.TranYmd: *iso0);
    on-error;
      Auth.ReasonCd = R_FORMAT;
      wkMsg = '取引日の形式が不正です';
      leavesr;
    endmon;

    if Req.TranHms < '000000' or Req.TranHms > '235959';
      Auth.ReasonCd = R_FORMAT;
      wkMsg = '取引時刻の形式が不正です';
      leavesr;
    endif;

  endsr;

  //********************************************************************
  //  カード照会
  //********************************************************************
  begsr ReadCard;

    chain Req.CardNo CDCARDF Card;
    if not %found(CDCARDF);
      Auth.ReasonCd = R_CARD_NOTF;
      wkMsg = 'カード番号が登録されていません';
      leavesr;
    endif;

    Work.CardFound = *on;

  endsr;

  //********************************************************************
  //  会員照会
  //********************************************************************
  begsr ReadMember;

    chain Card.cf_member_id CDMEMSTATF Mem;
    if not %found(CDMEMSTATF);
      Auth.ReasonCd = R_MEMBER_NOTF;
      wkMsg = '会員情報が登録されていません';
      leavesr;
    endif;

    Work.MemberFound = *on;

  endsr;

  //********************************************************************
  //  カード状態判定
  //********************************************************************
  begsr CheckCardStatus;

    select;
    when Card.cf_card_status = C_CARD_OK;
      Auth.BillStatus = C_BILL_HOLD;
      Work.CardOk = *on;

    when Card.cf_card_status = C_CARD_DELINQ;
      Auth.BillStatus = C_BILL_HOLD;
      Work.CardOk = *on;

    when Card.cf_card_status = C_CARD_STOP;
      Auth.ReasonCd = R_CARD_STOP;
      Auth.BillStatus = C_BILL_SKIP;
      wkMsg = 'カードは利用停止中です';

    when Card.cf_card_status = C_CARD_CLOSE;
      Auth.ReasonCd = R_CARD_CLOSE;
      Auth.BillStatus = C_BILL_SKIP;
      wkMsg = 'カードは解約済です';

    other;
      Auth.ReasonCd = R_CARD_STOP;
      Auth.BillStatus = C_BILL_SKIP;
      wkMsg = 'カード状態が利用不可です';
    endsl;

  endsr;

  //********************************************************************
  //  利用可能枠照会
  //********************************************************************
  begsr CallLimit;

    monitor;
      callp CA220R(Req.CardNo
                 : %dec(Req.TranYmd: 8: 0)
                 : %dec(Req.TranHms: 6: 0)
                 : Auth.AvailAmt
                 : Auth.LimitRc
                 : Auth.LimitReason);
    on-error;
      Auth.ReasonCd = R_SYSTEM;
      wkMsg = '利用可能枠照会で異常が発生しました';
      leavesr;
    endmon;

    if Auth.LimitRc <> R_OK;
      Auth.ReasonCd = R_LIMIT;
      wkMsg = '利用可能枠を超過しています';
      leavesr;
    endif;

    if Auth.AvailAmt < Req.ReqAmt;
      Auth.ReasonCd = R_LIMIT;
      wkMsg = '利用可能枠が不足しています';
      leavesr;
    endif;

  endsr;

  //********************************************************************
  //  簡易リスク判定
  //********************************************************************
  begsr CallRisk;

    monitor;
      callp CA230R(Req.CardNo
                 : Req.ShopNo
                 : Req.TermNo
                 : Req.TranYmd
                 : Req.TranHms
                 : Req.ReqAmt
                 : *blanks
                 : '05'
                 : *blanks
                 : '0'
                 : Auth.RiskDecision
                 : Auth.RiskRc
                 : Auth.RiskReason);
    on-error;
      Auth.ReasonCd = R_SYSTEM;
      wkMsg = 'リスク判定で異常が発生しました';
      leavesr;
    endmon;

    if Auth.RiskDecision = C_DECL;
      Auth.ReasonCd = R_RISK;
      wkMsg = 'リスク判定により否認されました';
      leavesr;
    endif;

    if Auth.RiskDecision = C_HOLD;
      Auth.ReasonCd = R_RISK;
      wkMsg = '高リスク取引として保留されました';
      leavesr;
    endif;

  endsr;

  //********************************************************************
  //  承認設定
  //********************************************************************
  begsr Approve;

    Auth.Result     = C_APPR;
    Auth.ReasonCd   = R_OK;
    Auth.BillStatus = C_BILL_CONF;

    Work.NowTime = %timestamp();
    Work.Seq = %dec(%subst(%char(Work.NowTime: *iso0): 9: 6): 6: 0);
    Work.Seq = %rem(Work.Seq + %dec(%subst(Req.CardNo: 11: 6): 6: 0): 1000000);
    Auth.AuthNo = %editc(Work.Seq: 'X');

    Work.HoldDate = %date(Req.TranYmd: *iso0) + %days(7);
    Auth.HoldLimitYmd = %char(Work.HoldDate: *iso0);

    wkMsg = '承認しました';

  endsr;

  //********************************************************************
  //  否認設定
  //********************************************************************
  begsr Decline;

    Auth.Result = C_DECL;
    Auth.AuthNo = *blanks;

    if Auth.ReasonCd = *blanks or Auth.ReasonCd = R_OK;
      Auth.ReasonCd = R_SYSTEM;
      wkMsg = '判定結果が未設定です';
    endif;

    if Auth.BillStatus = *blanks;
      Auth.BillStatus = C_BILL_HOLD;
    endif;

  endsr;

  //********************************************************************
  //  履歴出力
  //********************************************************************
  begsr WriteHistory;

    clear AuthOut;

    AuthOut.au_auth_id     = %editc(Work.Seq: 'X');
    AuthOut.au_card_no     = Req.CardNo;
    AuthOut.au_merchant_id = Req.ShopNo;
    AuthOut.au_auth_dt     = %dec(Req.TranYmd: 8: 0);
    AuthOut.au_auth_tm     = Req.TranHms;
    AuthOut.au_auth_amt    = Req.ReqAmt;
    AuthOut.au_currency_cd = 'JPY';
    AuthOut.au_auth_result = Auth.Result;
    AuthOut.au_approval_cd = Auth.AuthNo;
    if Auth.HoldLimitYmd <> *blanks;
      AuthOut.au_hold_exp_dt = %dec(Auth.HoldLimitYmd: 8: 0);
    endif;

    Work.WriteOk = *off;

    dou Work.WriteOk;
      monitor;
        write AuthR AuthOut;
        Work.WriteOk = *on;
      on-error;
        Work.Seq += 1;
        if Work.Seq > 9999999;
          Work.Seq = 1;
        endif;
        AuthOut.au_auth_id = %editc(Work.Seq: 'X');

        if Work.Seq = 9999999;
          Auth.Result = C_DECL;
          Auth.ReasonCd = R_SYSTEM;
          wkMsg = '承認履歴の出力に失敗しました';
          Work.WriteOk = *on;
        endif;
      endmon;
    enddo;

  endsr;

  //********************************************************************
  //  応答設定
  //********************************************************************
  begsr SetReturn;

    piAuthResult = Auth.Result;
    piAuthNo     = Auth.AuthNo;
    piReasonCd   = Auth.ReasonCd;

    select;
    when wkMsg <> *blanks;
      piOperatorMsg = wkMsg;
    when Auth.Result = C_APPR;
      piOperatorMsg = '承認しました';
    when Auth.ReasonCd = R_LIMIT;
      piOperatorMsg = '利用可能枠を超過しています';
    when Auth.ReasonCd = R_RISK;
      piOperatorMsg = 'リスク判定により否認されました';
    other;
      piOperatorMsg = '取引は否認されました';
    endsl;

  endsr;

end-proc;
