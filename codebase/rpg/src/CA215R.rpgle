**free
//**********************************************************************
//  変更履歴
//  版数   年月日      担当       概要
//  1.00   2019/04/01  ITO        初版作成
//  1.10   2020/11/16  SATO       状態同期遅延時の再確認要求を追加
//  1.20   2022/07/04  KAWAI      盗難紛失・再発行差分の端末表示を追加
//  1.30   2024/02/19  MORI       既存ホールド判定と通貨判定を整理
//**********************************************************************

ctl-opt dftactgrp(*no)
        actgrp(*caller)
        option(*srcstmt:*nodebugio)
        main(CA215R);

 /copy CDCARDFC2
 /copy CDSTSC

dcl-pr CA215R extpgm('CA215R');
  pTanmatsuId     char(08) const;
  pTantoCd        char(10) const;
  pCardNo         char(16) const;
  pReqAmt         packed(13:0) const;
  pCurrencyCd     char(03) const;
  pShopCd         char(10) const;
  pAuthNo         char(12);
  pDecisionKbn    char(01);
  pDeclineReason  char(03);
  pMessage        char(80);
end-pr;

dcl-pr CardStatusSyncService extpgm('CDSTSYNJ');
  sCardNo         char(16) const;
  sTanmatsuId     char(08) const;
  sTantoCd        char(10) const;
  sReqKbn         char(01) const;
  sResultCd       char(02);
  sResultMsg      char(80);
end-pr;

dcl-pi CA215R;
  pTanmatsuId     char(08) const;
  pTantoCd        char(10) const;
  pCardNo         char(16) const;
  pReqAmt         packed(13:0) const;
  pCurrencyCd     char(03) const;
  pShopCd         char(10) const;
  pAuthNo         char(12);
  pDecisionKbn    char(01);
  pDeclineReason  char(03);
  pMessage        char(80);
end-pi;

dcl-ds AuthReq qualified inz;
  tanmatsuId      char(08);
  tantoCd         char(10);
  cardNo          char(16);
  shopCd          char(10);
  reqAmt          packed(13:0);
  currencyCd      char(03);
end-ds;

dcl-ds AuthAns qualified inz;
  decisionKbn     char(01);
  declineReason   char(03);
  authNo          char(12);
  operatorMsg     char(80);
end-ds;

dcl-ds SyncCtl qualified inz;
  resultCd        char(02);
  resultMsg       char(80);
  reqKbn          char(01);
  delayMinutes    packed(5:0);
  needRecheck     ind;
end-ds;

dcl-ds ViewLine qualified inz;
  cardStatus      char(02);
  syncStatus      char(02);
  stopDiff        char(01);
  reissueDiff     char(01);
  lostDiff        char(01);
  displayText     char(132);
end-ds;

dcl-s WkNowTs         timestamp;
dcl-s WkSyncTs        timestamp;
dcl-s WkDelaySec      packed(9:0);
dcl-s WkFoundCard     ind inz(*off);
dcl-s WkFoundSync     ind inz(*off);
dcl-s WkValidReq      ind inz(*on);
dcl-s WkErrMsg        char(80) inz(*blank);

dcl-c C_STS_VALID     '01';
dcl-c C_STS_STOP      '02';
dcl-c C_STS_CANCEL    '03';
dcl-c C_STS_ARREARS   '09';

dcl-c C_DEC_APPROVE   'A';
dcl-c C_DEC_DECLINE   'D';

dcl-c C_HOLD_OK       '00';
dcl-c C_HOLD_CANCEL   '20';
dcl-c C_HOLD_SALES    '30';

dcl-c C_RSN_LIMIT     'LIM';
dcl-c C_RSN_STATUS    'STS';
dcl-c C_RSN_CURRENCY  'CUR';

dcl-c C_BASE_CUR      'JPY';
dcl-c C_SYNC_LIMIT    15;

monitor;

  clear AuthReq;
  clear AuthAns;
  clear SyncCtl;
  clear ViewLine;

  AuthReq.tanmatsuId = pTanmatsuId;
  AuthReq.tantoCd    = pTantoCd;
  AuthReq.cardNo     = pCardNo;
  AuthReq.reqAmt     = pReqAmt;
  AuthReq.currencyCd = pCurrencyCd;
  AuthReq.shopCd     = pShopCd;

  pDecisionKbn   = *blank;
  pDeclineReason = *blank;
  pAuthNo        = *blank;
  pMessage       = *blank;

  WkNowTs = %timestamp();

  if AuthReq.cardNo = *blank or
     AuthReq.reqAmt <= 0 or
     AuthReq.tanmatsuId = *blank;

    WkValidReq = *off;
    AuthAns.decisionKbn = C_DEC_DECLINE;
    AuthAns.declineReason = C_RSN_STATUS;
    AuthAns.operatorMsg = '入力内容を確認してください';
  endif;

  if WkValidReq;

    chain AuthReq.cardNo CDCARDFR;
    if %found(CDCARDFR);
      WkFoundCard = *on;
    else;
      WkFoundCard = *off;
      AuthAns.decisionKbn = C_DEC_DECLINE;
      AuthAns.declineReason = C_RSN_STATUS;
      AuthAns.operatorMsg = 'カードマスタ未登録';
    endif;

  endif;

  if WkValidReq and WkFoundCard;

    chain AuthReq.cardNo CDSTSCR;
    if %found(CDSTSCR);
      WkFoundSync = *on;
      WkSyncTs = CS_SYNC_TIMESTAMP;
    else;
      WkFoundSync = *off;
      WkSyncTs = *loval;
    endif;

    ViewLine.cardStatus = CF_CARD_STATUS;

    if WkFoundSync;
      ViewLine.syncStatus = CS_CARD_STATUS;
      if CF_STOP_FLG <> CS_STOP_FLG;
        ViewLine.stopDiff = 'Y';
      else;
        ViewLine.stopDiff = 'N';
      endif;
      if CF_REISSUE_FLG <> CS_REISSUE_FLG;
        ViewLine.reissueDiff = 'Y';
      else;
        ViewLine.reissueDiff = 'N';
      endif;
      if CF_LOST_FLG <> CS_LOST_FLG;
        ViewLine.lostDiff = 'Y';
      else;
        ViewLine.lostDiff = 'N';
      endif;
    else;
      ViewLine.syncStatus = '**';
      ViewLine.stopDiff = 'Y';
      ViewLine.reissueDiff = 'Y';
      ViewLine.lostDiff = 'Y';
    endif;

    if WkFoundSync;
      WkDelaySec = %diff(WkNowTs: WkSyncTs: *seconds);
      SyncCtl.delayMinutes = WkDelaySec / 60;
    else;
      SyncCtl.delayMinutes = 99999;
    endif;

    if SyncCtl.delayMinutes > C_SYNC_LIMIT;
      SyncCtl.needRecheck = *on;
      SyncCtl.reqKbn = '1';

      callp CardStatusSyncService(AuthReq.cardNo:
                                  AuthReq.tanmatsuId:
                                  AuthReq.tantoCd:
                                  SyncCtl.reqKbn:
                                  SyncCtl.resultCd:
                                  SyncCtl.resultMsg);

      if SyncCtl.resultCd <> '00';
        AuthAns.operatorMsg =
          '状態再確認要求済、同期応答未完了';
      endif;
    endif;

  endif;

  if WkValidReq and WkFoundCard;

    select;

    when AuthReq.currencyCd <> C_BASE_CUR;
      AuthAns.decisionKbn = C_DEC_DECLINE;
      AuthAns.declineReason = C_RSN_CURRENCY;
      AuthAns.operatorMsg = '取扱通貨対象外';

    when CF_CARD_STATUS <> C_STS_VALID;
      AuthAns.decisionKbn = C_DEC_DECLINE;
      AuthAns.declineReason = C_RSN_STATUS;

      select;
      when CF_CARD_STATUS = C_STS_STOP;
        AuthAns.operatorMsg = '利用停止カード';
      when CF_CARD_STATUS = C_STS_CANCEL;
        AuthAns.operatorMsg = '解約済カード';
      when CF_CARD_STATUS = C_STS_ARREARS;
        AuthAns.operatorMsg = '延滞カード';
      other;
        AuthAns.operatorMsg = 'カード状態不正';
      endsl;

    when WkFoundSync and CS_CARD_STATUS <> C_STS_VALID;
      AuthAns.decisionKbn = C_DEC_DECLINE;
      AuthAns.declineReason = C_RSN_STATUS;
      AuthAns.operatorMsg = '同期状態が利用不可';

    when CF_STOP_FLG = '1' or CF_LOST_FLG = '1';
      AuthAns.decisionKbn = C_DEC_DECLINE;
      AuthAns.declineReason = C_RSN_STATUS;
      AuthAns.operatorMsg = '停止または盗難紛失登録あり';

    when WkFoundSync and
         (CS_STOP_FLG = '1' or CS_LOST_FLG = '1');
      AuthAns.decisionKbn = C_DEC_DECLINE;
      AuthAns.declineReason = C_RSN_STATUS;
      AuthAns.operatorMsg = '同期側で停止または盗難紛失あり';

    other;
      // 当端末は状態照会のみ。利用可能枠の算定は
      // Java与信エンジン(AuthService)が保有するため、ここでは行わない。
      // 状態正常であれば端末担当者へ状態確認OKを表示する。
      AuthAns.decisionKbn = C_DEC_APPROVE;
      AuthAns.operatorMsg = '状態確認OK';

    endsl;

  endif;

  if AuthAns.decisionKbn = *blank;
    AuthAns.decisionKbn = C_DEC_DECLINE;
    AuthAns.declineReason = C_RSN_STATUS;
    AuthAns.operatorMsg = '判定未完了';
  endif;

  if AuthAns.decisionKbn = C_DEC_APPROVE;
    AuthAns.declineReason = *blank;
  endif;

  pDecisionKbn   = AuthAns.decisionKbn;
  pDeclineReason = AuthAns.declineReason;
  pAuthNo        = AuthAns.authNo;

  if SyncCtl.needRecheck and AuthAns.decisionKbn = C_DEC_APPROVE;
    pMessage = '承認、状態同期遅延あり';
  else;
    pMessage = AuthAns.operatorMsg;
  endif;

on-error;
  WkErrMsg = 'カード状態照会処理で異常終了';
  pDecisionKbn   = C_DEC_DECLINE;
  pDeclineReason = C_RSN_STATUS;
  pAuthNo        = *blank;
  pMessage       = WkErrMsg;
endmon;

return;
