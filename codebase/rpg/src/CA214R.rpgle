**free
ctl-opt dftactgrp(*no) actgrp(*caller)
        option(*srcstmt:*nodebugio)
        datfmt(*iso) timfmt(*iso)
        main(CA214R);

//==============================================================
// 変更履歴
// 版数  年月日      担当     概要
// 0001  2024-02-05  MKI001   一時増枠申込受付 新規作成
// 0002  2024-08-19  MKI017   有効期間上限判定と既存増枠重複判定を追加
// 0003  2025-03-11  MKI024   審査前チェックサービス連携を追加
//==============================================================

// カード基本
/copy QRPGLESRC,CDCARDFC2
// 一時増枠申込
/copy QRPGLESRC,CDLIMC

dcl-pr TemporaryLimitService extpgm('CDTLMSV');
  pReqNo          packed(11:0) const;
  pMemberNo       char(12)     const;
  pCardNo         char(16)     const;
  pBaseLimit      packed(13:0) const;
  pAddLimit       packed(13:0) const;
  pUseFrom        date(*iso)   const;
  pUseTo          date(*iso)   const;
  pServiceResult  char(2);
  pServiceCode    char(3);
  pServiceMsg     char(80);
end-pr;

dcl-pr CA214R;
  inMemberNo        char(12)     const;
  inCardNo          char(16)     const;
  inApplyLimitAmt   packed(13:0) const;
  inUseFrom         date(*iso)   const;
  inUseTo           date(*iso)   const;
  inCurrency        char(3)      const;
  inOperatorId      char(10)     const;
  outDecisionKbn    char(1);
  outDeclineReason  char(3);
  outApplyNo        packed(11:0);
  outReturnCd       char(2);
  outMessage        char(80);
end-pr;

dcl-pi CA214R;
  inMemberNo        char(12)     const;
  inCardNo          char(16)     const;
  inApplyLimitAmt   packed(13:0) const;
  inUseFrom         date(*iso)   const;
  inUseTo           date(*iso)   const;
  inCurrency        char(3)      const;
  inOperatorId      char(10)     const;
  outDecisionKbn    char(1);
  outDeclineReason  char(3);
  outApplyNo        packed(11:0);
  outReturnCd       char(2);
  outMessage        char(80);
end-pi;

dcl-f CDCARDF keyed usage(*input) extfile('CDCARDF');
dcl-f CDLIMF  keyed usage(*update:*output) extfile('CDLIMF');

dcl-ds wk qualified inz;
  sysDate             date(*iso);
  sysTime             time(*iso);
  maxUseDays          packed(3:0);
  useDays             packed(5:0);
  baseLimitAmt        packed(13:0);
  afterLimitAmt       packed(13:0);
  reqNo               packed(11:0);
  svcResult           char(2);
  svcCode             char(3);
  svcMsg              char(80);
  dupFound            ind;
  cardFound           ind;
  serviceNg           ind;
  fileNg              ind;
  reject              ind;
end-ds;

dcl-s C_CARD_STS_OK      char(2) inz('01');
dcl-s C_CUR_JPY          char(3) inz('JPY');
dcl-s C_DEC_APPROVE      char(1) inz('A');
dcl-s C_DEC_DECLINE      char(1) inz('D');
dcl-s C_REASON_LIM       char(3) inz('LIM');
dcl-s C_REASON_STS       char(3) inz('STS');
dcl-s C_REASON_CUR       char(3) inz('CUR');
dcl-s C_AUTH_HOLD        char(2) inz('00');
dcl-s C_AUTH_CANCEL      char(2) inz('20');
dcl-s C_AUTH_SALES       char(2) inz('30');
dcl-s C_APPLY_WAIT       char(1) inz('0');
dcl-s C_OP_TEXT          char(20) inz('申請中登録');

wk.sysDate = %date();
wk.sysTime = %time();
wk.maxUseDays = 90;
wk.reqNo = 0;

outDecisionKbn = *blank;
outDeclineReason = *blank;
outApplyNo = 0;
outReturnCd = '00';
outMessage = *blank;

monitor;

  // 通貨対象外はカード照会前に否認する。
  if inCurrency <> C_CUR_JPY;
    exsr SetDeclineCur;
    return;
  endif;

  // 入力期間の妥当性。上限超過は枠超過扱いで返す。
  if inUseFrom < wk.sysDate or inUseTo < inUseFrom;
    exsr SetDeclineLim;
    return;
  endif;

  wk.useDays = %diff(inUseTo: inUseFrom: *days) + 1;
  if wk.useDays > wk.maxUseDays;
    exsr SetDeclineLim;
    return;
  endif;

  if inApplyLimitAmt <= 0;
    exsr SetDeclineLim;
    return;
  endif;

  // カード状態確認
  chain (inMemberNo: inCardNo) CDCARDF;
  if not %found(CDCARDF);
    exsr SetDeclineSts;
    return;
  endif;

  wk.cardFound = *on;

  if CF_CARD_STATUS <> C_CARD_STS_OK;
    exsr SetDeclineSts;
    return;
  endif;

  if CF_BASE_CURRENCY <> C_CUR_JPY;
    exsr SetDeclineCur;
    return;
  endif;

  wk.baseLimitAmt = CF_SHOP_LIMIT_AMT;
  wk.afterLimitAmt = wk.baseLimitAmt + inApplyLimitAmt;

  // 会員上限を超える増枠は受付しない。
  if wk.afterLimitAmt > CF_MAX_LIMIT_AMT;
    exsr SetDeclineLim;
    return;
  endif;

  // 既存の有効ホールド、申請中、承認済期間との重複確認。
  setll (inMemberNo: inCardNo) CDLIMF;
  dou %eof(CDLIMF);
    reade (inMemberNo: inCardNo) CDLIMF;
    if %eof(CDLIMF);
      leave;
    endif;

    if CL_USE_TO < inUseFrom or CL_USE_FROM > inUseTo;
      iter;
    endif;

    select;
    when CL_APPLY_STATUS = C_APPLY_WAIT;
      wk.dupFound = *on;

    when CL_AUTH_RESULT = C_AUTH_HOLD;
      wk.dupFound = *on;

    when CL_AUTH_RESULT = C_AUTH_CANCEL;
      iter;

    when CL_AUTH_RESULT = C_AUTH_SALES;
      wk.dupFound = *on;

    other;
      iter;
    endsl;

    if wk.dupFound;
      leave;
    endif;
  enddo;

  if wk.dupFound;
    exsr SetDeclineLim;
    return;
  endif;

  // 申込番号採番。日次オンライン内での簡易採番。
  wk.reqNo = %dec(%char(%date(): *iso0): 8: 0) * 1000
           + %dec(%subst(%char(%time(): *iso0): 5: 2): 2: 0) * 10
           + %rem(%dec(%subst(%char(%time(): *iso0): 7: 2): 2: 0): 10);

  clear CDLIMR;
  CL_APPLY_NO       = wk.reqNo;
  CL_MEMBER_NO      = inMemberNo;
  CL_CARD_NO        = inCardNo;
  CL_APPLY_DATE     = wk.sysDate;
  CL_APPLY_TIME     = wk.sysTime;
  CL_USE_FROM       = inUseFrom;
  CL_USE_TO         = inUseTo;
  CL_CURRENCY       = inCurrency;
  CL_BASE_LIMIT_AMT = wk.baseLimitAmt;
  CL_ADD_LIMIT_AMT  = inApplyLimitAmt;
  CL_AFTER_LIMIT_AMT= wk.afterLimitAmt;
  CL_APPLY_STATUS   = C_APPLY_WAIT;
  CL_AUTH_RESULT    = C_AUTH_HOLD;
  CL_DECISION_KBN   = *blank;
  CL_DECLINE_REASON = *blank;
  CL_OPER_ID        = inOperatorId;
  CL_OPER_TEXT      = C_OP_TEXT;
  CL_CREATE_DATE    = wk.sysDate;
  CL_CREATE_TIME    = wk.sysTime;
  CL_UPDATE_DATE    = wk.sysDate;
  CL_UPDATE_TIME    = wk.sysTime;

  write CDLIMR;

  callp TemporaryLimitService(
          wk.reqNo:
          inMemberNo:
          inCardNo:
          wk.baseLimitAmt:
          inApplyLimitAmt:
          inUseFrom:
          inUseTo:
          wk.svcResult:
          wk.svcCode:
          wk.svcMsg);

  if wk.svcResult <> '00';
    wk.serviceNg = *on;
    outDecisionKbn = C_DEC_DECLINE;
    outDeclineReason = C_REASON_LIM;
    outApplyNo = wk.reqNo;
    outReturnCd = wk.svcResult;
    outMessage = wk.svcMsg;
    return;
  endif;

  outDecisionKbn = C_DEC_APPROVE;
  outDeclineReason = *blank;
  outApplyNo = wk.reqNo;
  outReturnCd = '00';
  outMessage = '申請を受け付けました';

on-error;
  wk.fileNg = *on;
  outDecisionKbn = C_DEC_DECLINE;
  outDeclineReason = C_REASON_LIM;
  outApplyNo = wk.reqNo;
  outReturnCd = '99';
  outMessage = '一時増枠申込受付で異常が発生しました';
endmon;

return;

//--------------------------------------------------------------
// 否認設定
//--------------------------------------------------------------
begsr SetDeclineSts;
  outDecisionKbn = C_DEC_DECLINE;
  outDeclineReason = C_REASON_STS;
  outApplyNo = 0;
  outReturnCd = '10';
  outMessage = 'カード状態を確認してください';
endsr;

begsr SetDeclineCur;
  outDecisionKbn = C_DEC_DECLINE;
  outDeclineReason = C_REASON_CUR;
  outApplyNo = 0;
  outReturnCd = '11';
  outMessage = '取扱通貨対象外です';
endsr;

begsr SetDeclineLim;
  outDecisionKbn = C_DEC_DECLINE;
  outDeclineReason = C_REASON_LIM;
  outApplyNo = 0;
  outReturnCd = '12';
  outMessage = '一時増枠条件を確認してください';
endsr;
