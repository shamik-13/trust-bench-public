**free
//*********************************************************************
//  変更履歴
//  版数  年月日      担当     概要
//  1.00  2023/04/01  NAKAMURA 新規作成
//  1.01  2023/09/12  SATO     一時増枠表示期間の判定追加
//  1.02  2024/02/19  KOBAYASHI Java照会サービス返却値の優先表示対応
//*********************************************************************

ctl-opt dftactgrp(*no) actgrp(*caller)
        option(*srcstmt:*nodebugio)
        bnddir('QC2LE')
        main(CA212R);

 /copy CDCARDFC2
 /copy CDBALFC2
 /copy CDLIMC

dcl-pr CA212R extpgm('CA212R');
  pCardNo        char(16) const;
  pReqAmt        packed(13:0) const;
  pReqCur        char(3) const;
  pAuthNo        char(8) const;
  pOperator      char(10) const;
  pDecision      char(1);
  pDeclineReason char(3);
  pDispBaseLimit packed(13:0);
  pDispBillBal   packed(13:0);
  pWarnMsg        char(80);
end-pr;

dcl-pr LimitInquiryService extpgm('LIMINQSRV');
  sCardNo        char(16) const;
  sBaseCur       char(3) const;
  sBaseLimit     packed(13:0);
  sBillBal       packed(13:0);
  sAvailAmt      packed(13:0);
  sReturnCd      char(2);
  sMessage       char(60);
end-pr;

dcl-pi CA212R;
  iCardNo        char(16) const;
  iReqAmt        packed(13:0) const;
  iReqCur        char(3) const;
  iAuthNo        char(8) const;
  iOperator      char(10) const;
  oDecision      char(1);
  oDeclineReason char(3);
  oDispBaseLimit packed(13:0);
  oDispBillBal   packed(13:0);
  oWarnMsg        char(80);
end-pi;

dcl-c C_STS_VALID     '01';
dcl-c C_STS_STOP      '02';
dcl-c C_STS_CLOSE     '03';
dcl-c C_STS_OVERDUE   '09';

dcl-c C_DEC_APPROVE   'A';
dcl-c C_DEC_DECLINE   'D';

dcl-c C_RSN_LIMIT     'LIM';
dcl-c C_RSN_STATUS    'STS';
dcl-c C_RSN_CUR       'CUR';

dcl-c C_AUTH_HOLD     '00';
dcl-c C_AUTH_CANCEL   '20';
dcl-c C_AUTH_SALES    '30';

dcl-c C_BASE_CUR      'JPY';

dcl-ds Wk qualified inz;
  today          packed(8:0);
  cardNo         char(16);
  cardOk         ind;
  balOk          ind;
  limOk          ind;
  srvOk          ind;
  serviceCd      char(2);
  serviceMsg     char(60);
  baseLimit      packed(13:0);
  billBal        packed(13:0);
  tempLimit      packed(13:0);
  availAmt       packed(13:0);
  serviceLimit   packed(13:0);
  serviceBal     packed(13:0);
  serviceAvail   packed(13:0);
  holdAmt        packed(13:0);
  wkAmt          packed(13:0);
  wkText         char(80);
end-ds;

dcl-s idx            int(5);
dcl-s digitCnt       int(5);
dcl-s sumOdd         int(5);
dcl-s sumEven        int(5);
dcl-s chkDigit       int(5);
dcl-s wkDigit        int(5);
dcl-s wkChar         char(1);
dcl-s expiredTemp    ind inz(*off);

CA212R();

dcl-proc CA212R;

  reset Wk;
  clear oDecision;
  clear oDeclineReason;
  clear oDispBaseLimit;
  clear oDispBillBal;
  clear oWarnMsg;

  Wk.today = %dec(%char(%date():*iso0):8:0);
  Wk.cardNo = iCardNo;

  oDecision = C_DEC_DECLINE;
  oDeclineReason = C_RSN_STATUS;

  exsr ValidateCardNo;
  if not Wk.cardOk;
    oWarnMsg = 'カード番号不正';
    return;
  endif;

  exsr ReadCard;
  if not Wk.cardOk;
    oWarnMsg = '会員属性未登録';
    return;
  endif;

  if CF_CARD_STATUS <> C_STS_VALID;
    oDeclineReason = C_RSN_STATUS;
    select;
    when CF_CARD_STATUS = C_STS_STOP;
      oWarnMsg = '利用停止カード';
    when CF_CARD_STATUS = C_STS_CLOSE;
      oWarnMsg = '解約済カード';
    when CF_CARD_STATUS = C_STS_OVERDUE;
      oWarnMsg = '延滞カード';
    other;
      oWarnMsg = 'カード状態不正';
    endsl;
    return;
  endif;

  if iReqCur <> C_BASE_CUR;
    oDeclineReason = C_RSN_CUR;
    oWarnMsg = '取扱通貨対象外';
    return;
  endif;

  exsr ReadBalance;
  exsr ReadLimit;

  Wk.baseLimit = CF_BASE_LIMIT;
  Wk.billBal   = CB_BILL_BALANCE;
  Wk.tempLimit = 0;

  if Wk.limOk;
    if CL_TEMP_LIMIT_AMT > 0;
      if CL_VALID_FROM <= Wk.today and CL_VALID_TO >= Wk.today;
        Wk.tempLimit = CL_TEMP_LIMIT_AMT;
      elseif CL_VALID_TO < Wk.today;
        expiredTemp = *on;
      endif;
    endif;
  endif;

  Wk.baseLimit += Wk.tempLimit;

  monitor;
    callp LimitInquiryService(Wk.cardNo:
                              C_BASE_CUR:
                              Wk.serviceLimit:
                              Wk.serviceBal:
                              Wk.serviceAvail:
                              Wk.serviceCd:
                              Wk.serviceMsg);
    if Wk.serviceCd = '00';
      Wk.srvOk = *on;
    endif;
  on-error;
    Wk.srvOk = *off;
    Wk.serviceMsg = '照会サービス応答なし';
  endmon;

  if Wk.srvOk;
    oDispBaseLimit = Wk.serviceLimit;
    oDispBillBal   = Wk.serviceBal;
    Wk.availAmt    = Wk.serviceAvail;
  else;
    oDispBaseLimit = Wk.baseLimit;
    oDispBillBal   = Wk.billBal;
    Wk.availAmt    = Wk.baseLimit - Wk.billBal;
  endif;

  exsr AdjustExistingHold;

  if expiredTemp;
    oWarnMsg = '一時増枠期限切れ';
  elseif not Wk.srvOk;
    oWarnMsg = %trim(Wk.serviceMsg);
  endif;

  if iReqAmt <= 0;
    oDeclineReason = C_RSN_LIMIT;
    if oWarnMsg = *blank;
      oWarnMsg = '利用金額不正';
    endif;
    return;
  endif;

  if Wk.availAmt + Wk.holdAmt >= iReqAmt;
    oDecision = C_DEC_APPROVE;
    oDeclineReason = *blank;
  else;
    oDecision = C_DEC_DECLINE;
    oDeclineReason = C_RSN_LIMIT;
    if oWarnMsg = *blank;
      oWarnMsg = '利用可能枠超過';
    endif;
  endif;

  return;

  begsr ValidateCardNo;
    Wk.cardOk = *off;
    digitCnt = 0;
    sumOdd = 0;
    sumEven = 0;

    for idx = 1 to %len(%trim(Wk.cardNo));
      wkChar = %subst(Wk.cardNo:idx:1);
      if wkChar < '0' or wkChar > '9';
        leavesr;
      endif;

      digitCnt += 1;
      wkDigit = %int(wkChar);

      if %rem(digitCnt:2) = 1;
        wkDigit *= 2;
        if wkDigit > 9;
          wkDigit -= 9;
        endif;
        sumOdd += wkDigit;
      else;
        sumEven += wkDigit;
      endif;
    endfor;

    if digitCnt <> 16;
      leavesr;
    endif;

    chkDigit = %rem(sumOdd + sumEven:10);
    if chkDigit = 0;
      Wk.cardOk = *on;
    endif;
  endsr;

  begsr ReadCard;
    Wk.cardOk = *off;
    monitor;
      chain Wk.cardNo CDCARDR;
      if %found(CDCARDR);
        Wk.cardOk = *on;
      endif;
    on-error;
      Wk.cardOk = *off;
      oWarnMsg = '会員属性読込エラー';
    endmon;
  endsr;

  begsr ReadBalance;
    Wk.balOk = *off;
    clear CDBALR;
    monitor;
      chain Wk.cardNo CDBALR;
      if %found(CDBALR);
        Wk.balOk = *on;
      else;
        CB_BILL_BALANCE = 0;
        CB_UNBILLED_AMT = 0;
      endif;
    on-error;
      Wk.balOk = *off;
      CB_BILL_BALANCE = 0;
      CB_UNBILLED_AMT = 0;
      if oWarnMsg = *blank;
        oWarnMsg = '請求残高読込エラー';
      endif;
    endmon;
  endsr;

  begsr ReadLimit;
    Wk.limOk = *off;
    clear CDLIMR;
    monitor;
      chain Wk.cardNo CDLIMR;
      if %found(CDLIMR);
        Wk.limOk = *on;
      else;
        CL_TEMP_LIMIT_AMT = 0;
        CL_VALID_FROM = 0;
        CL_VALID_TO = 0;
      endif;
    on-error;
      Wk.limOk = *off;
      CL_TEMP_LIMIT_AMT = 0;
      CL_VALID_FROM = 0;
      CL_VALID_TO = 0;
      if oWarnMsg = *blank;
        oWarnMsg = '増枠情報読込エラー';
      endif;
    endmon;
  endsr;

  begsr AdjustExistingHold;
    Wk.holdAmt = 0;

    if iAuthNo = *blank;
      leavesr;
    endif;

    select;
    when AU_AUTH_RESULT = C_AUTH_HOLD;
      Wk.holdAmt = AU_HOLD_AMT;
    when AU_AUTH_RESULT = C_AUTH_CANCEL;
      Wk.holdAmt = 0;
    when AU_AUTH_RESULT = C_AUTH_SALES;
      Wk.holdAmt = 0;
    other;
      Wk.holdAmt = 0;
    endsl;
  endsr;

end-proc;
