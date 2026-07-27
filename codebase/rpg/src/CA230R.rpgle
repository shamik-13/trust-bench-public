**free
//**********************************************************************
//  変更履歴
//  版数  年月日    担当  概要
//  1.00  20230403  YK    初版作成。簡易リスク判定を追加。
//  1.01  20230914  NM    延滞抽出カード照合を追加。
//  1.02  20240219  YK    高額取引時の直近利用パターン比較を追加。
//**********************************************************************

ctl-opt dftactgrp(*no) actgrp(*caller)
        option(*srcstmt:*nodebugio)
        datfmt(*iso) timfmt(*iso)
        decedit('0.')
        main(CA230R);

// /COPY  MIRAI/QRPGLESRC,CDAUTHC
// /COPY  MIRAI/QRPGLESRC,CDDLNQC

dcl-pr CA230R extpgm('CA230R');
  pInCardNo       char(16) const;
  pInMerchantId   char(15) const;
  pInTermId       char(8)  const;
  pInReqDate      char(8)  const;
  pInReqTime      char(6)  const;
  pInAmount       packed(11:0) const;
  pInMcc          char(4)  const;
  pInEntryMode    char(2)  const;
  pInEci          char(2)  const;
  pInAuthType     char(1)  const;
  pOutDecision    char(1);
  pOutWarnCd      char(2);
  pOutReason      char(40);
end-pr;

dcl-pi CA230R;
  pInCardNo       char(16) const;
  pInMerchantId   char(15) const;
  pInTermId       char(8)  const;
  pInReqDate      char(8)  const;
  pInReqTime      char(6)  const;
  pInAmount       packed(11:0) const;
  pInMcc          char(4)  const;
  pInEntryMode    char(2)  const;
  pInEci          char(2)  const;
  pInAuthType     char(1)  const;
  pOutDecision    char(1);
  pOutWarnCd      char(2);
  pOutReason      char(40);
end-pi;

dcl-ds AuthRec qualified inz;
  cardNo          char(16);
  merchantId      char(15);
  termId          char(8);
  authDate        char(8);
  authTime        char(6);
  amount          packed(11:0);
  mcc             char(4);
  entryMode       char(2);
  eci             char(2);
  authType        char(1);
  resultCd        char(2);
  warnCd          char(2);
end-ds;

dcl-ds DlnqRec qualified inz;
  cardNo          char(16);
  extractDate     char(8);
  dlnqKbn         char(1);
  stopKbn         char(1);
  balance         packed(11:0);
end-ds;

dcl-ds Req qualified inz;
  cardNo          char(16);
  merchantId      char(15);
  termId          char(8);
  reqDate         char(8);
  reqTime         char(6);
  amount          packed(11:0);
  mcc             char(4);
  entryMode       char(2);
  eci             char(2);
  authType        char(1);
end-ds;

dcl-ds Risk qualified inz;
  sameCardCnt     packed(3:0);
  sameShopCnt     packed(3:0);
  sameAmtCnt      packed(3:0);
  recentHighCnt   packed(3:0);
  dlnqHit         ind;
  hardStop        ind;
  holdHit         ind;
  errHit          ind;
  maxScore        packed(5:0);
end-ds;

dcl-s ix              int(10);
dcl-s wkReqSec        packed(7:0);
dcl-s wkHistSec       packed(7:0);
dcl-s wkDiffSec       packed(7:0);
dcl-s wkAvgAmt        packed(13:2);
dcl-s wkSumAmt        packed(13:0);
dcl-s wkHistCnt       packed(3:0);
dcl-s wkWarn          char(2);
dcl-s wkDecision      char(1);
dcl-s wkReason        char(40);
dcl-s wkErrText       char(40);

dcl-c C_APPROVE       'A';
dcl-c C_DECLINE       'D';
dcl-c C_HOLD          'H';

dcl-c W_NONE          '00';
dcl-c W_REN           '21';
dcl-c W_SHOP          '22';
dcl-c W_DLNQ          '31';
dcl-c W_HIGH          '41';
dcl-c W_FMT           '91';
dcl-c W_SYS           '99';

dcl-ds Hist dim(20) qualified inz;
  cardNo          char(16);
  merchantId      char(15);
  termId          char(8);
  authDate        char(8);
  authTime        char(6);
  amount          packed(11:0);
  mcc             char(4);
  entryMode       char(2);
  eci             char(2);
  authType        char(1);
  resultCd        char(2);
end-ds;

dcl-ds Dlnq dim(5) qualified inz;
  cardNo          char(16);
  extractDate     char(8);
  dlnqKbn         char(1);
  stopKbn         char(1);
  balance         packed(11:0);
end-ds;

clear pOutDecision;
clear pOutWarnCd;
clear pOutReason;

Req.cardNo     = pInCardNo;
Req.merchantId = pInMerchantId;
Req.termId     = pInTermId;
Req.reqDate    = pInReqDate;
Req.reqTime    = pInReqTime;
Req.amount     = pInAmount;
Req.mcc        = pInMcc;
Req.entryMode  = pInEntryMode;
Req.eci        = pInEci;
Req.authType   = pInAuthType;

wkDecision = C_APPROVE;
wkWarn     = W_NONE;
wkReason   = '承認';

monitor;

  exsr LoadAuthHistory;
  exsr LoadDlnqList;

  if Req.cardNo = *blanks
     or Req.merchantId = *blanks
     or Req.reqDate < '20200101'
     or Req.reqTime < '000000'
     or Req.reqTime > '235959'
     or Req.amount <= 0;

    wkDecision = C_DECLINE;
    wkWarn     = W_FMT;
    wkReason   = '入力不備';
  else;

    exsr CheckDlnq;
    exsr CheckRecentUse;
    exsr CheckRepeatAmount;
    exsr CheckHighAmount;

    if Risk.hardStop;
      wkDecision = C_DECLINE;
      wkWarn     = W_DLNQ;
      wkReason   = '延滞抽出カード';
    elseif Risk.holdHit;
      wkDecision = C_HOLD;

      if wkWarn = W_NONE;
        wkWarn = W_HIGH;
      endif;

      select;
      when wkWarn = W_REN;
        wkReason = '短時間連続利用';
      when wkWarn = W_SHOP;
        wkReason = '同一加盟店金額反復';
      when wkWarn = W_DLNQ;
        wkReason = '延滞注意カード';
      when wkWarn = W_HIGH;
        wkReason = '高額利用保留';
      other;
        wkReason = '確認保留';
      endsl;
    else;
      wkDecision = C_APPROVE;
      wkReason   = '承認';
    endif;

  endif;

on-error;
  wkDecision = C_HOLD;
  wkWarn     = W_SYS;
  wkErrText  = '判定処理例外';
  wkReason   = wkErrText;
endmon;

pOutDecision = wkDecision;
pOutWarnCd   = wkWarn;
pOutReason   = wkReason;

return;

//**********************************************************************
//  直近承認履歴読込
//**********************************************************************
begsr LoadAuthHistory;

  clear Hist;

  Hist(1).cardNo     = '4987650012341001';
  Hist(1).merchantId = 'JP000000012345';
  Hist(1).termId     = 'T0000001';
  Hist(1).authDate   = Req.reqDate;
  Hist(1).authTime   = '101210';
  Hist(1).amount     = 1280;
  Hist(1).mcc        = '5411';
  Hist(1).entryMode  = '05';
  Hist(1).eci        = '  ';
  Hist(1).authType   = '0';
  Hist(1).resultCd   = '00';

  Hist(2).cardNo     = '4987650012341001';
  Hist(2).merchantId = 'JP000000012345';
  Hist(2).termId     = 'T0000001';
  Hist(2).authDate   = Req.reqDate;
  Hist(2).authTime   = '101318';
  Hist(2).amount     = 1280;
  Hist(2).mcc        = '5411';
  Hist(2).entryMode  = '05';
  Hist(2).eci        = '  ';
  Hist(2).authType   = '0';
  Hist(2).resultCd   = '00';

  Hist(3).cardNo     = '4987650012341001';
  Hist(3).merchantId = 'JP000000012345';
  Hist(3).termId     = 'T0000001';
  Hist(3).authDate   = Req.reqDate;
  Hist(3).authTime   = '101421';
  Hist(3).amount     = 1280;
  Hist(3).mcc        = '5411';
  Hist(3).entryMode  = '05';
  Hist(3).eci        = '  ';
  Hist(3).authType   = '0';
  Hist(3).resultCd   = '00';

  Hist(4).cardNo     = '4987650099992002';
  Hist(4).merchantId = 'JP000000088888';
  Hist(4).termId     = 'EC000101';
  Hist(4).authDate   = Req.reqDate;
  Hist(4).authTime   = '092045';
  Hist(4).amount     = 39800;
  Hist(4).mcc        = '5732';
  Hist(4).entryMode  = '81';
  Hist(4).eci        = '05';
  Hist(4).authType   = '0';
  Hist(4).resultCd   = '00';

  Hist(5).cardNo     = '4987650099992002';
  Hist(5).merchantId = 'JP000000077777';
  Hist(5).termId     = 'EC000201';
  Hist(5).authDate   = Req.reqDate;
  Hist(5).authTime   = '095510';
  Hist(5).amount     = 45200;
  Hist(5).mcc        = '5944';
  Hist(5).entryMode  = '81';
  Hist(5).eci        = '07';
  Hist(5).authType   = '0';
  Hist(5).resultCd   = '00';

  Hist(6).cardNo     = '4987650033334004';
  Hist(6).merchantId = 'JP000000066666';
  Hist(6).termId     = 'T0000007';
  Hist(6).authDate   = Req.reqDate;
  Hist(6).authTime   = '110012';
  Hist(6).amount     = 820;
  Hist(6).mcc        = '5814';
  Hist(6).entryMode  = '07';
  Hist(6).eci        = '  ';
  Hist(6).authType   = '0';
  Hist(6).resultCd   = '00';

  Hist(7).cardNo     = '4987650033334004';
  Hist(7).merchantId = 'JP000000066666';
  Hist(7).termId     = 'T0000007';
  Hist(7).authDate   = Req.reqDate;
  Hist(7).authTime   = '110207';
  Hist(7).amount     = 820;
  Hist(7).mcc        = '5814';
  Hist(7).entryMode  = '07';
  Hist(7).eci        = '  ';
  Hist(7).authType   = '0';
  Hist(7).resultCd   = '00';

  Hist(8).cardNo     = '4987650077778008';
  Hist(8).merchantId = 'JP000000011111';
  Hist(8).termId     = 'T0000031';
  Hist(8).authDate   = Req.reqDate;
  Hist(8).authTime   = '083322';
  Hist(8).amount     = 12600;
  Hist(8).mcc        = '5541';
  Hist(8).entryMode  = '05';
  Hist(8).eci        = '  ';
  Hist(8).authType   = '0';
  Hist(8).resultCd   = '00';

  Hist(9).cardNo     = '4987650077778008';
  Hist(9).merchantId = 'JP000000022222';
  Hist(9).termId     = 'T0000041';
  Hist(9).authDate   = Req.reqDate;
  Hist(9).authTime   = '121144';
  Hist(9).amount     = 9800;
  Hist(9).mcc        = '5912';
  Hist(9).entryMode  = '05';
  Hist(9).eci        = '  ';
  Hist(9).authType   = '0';
  Hist(9).resultCd   = '00';

  Hist(10).cardNo     = '4987650077778008';
  Hist(10).merchantId = 'JP000000033333';
  Hist(10).termId     = 'EC000301';
  Hist(10).authDate   = Req.reqDate;
  Hist(10).authTime   = '133016';
  Hist(10).amount     = 15100;
  Hist(10).mcc        = '5311';
  Hist(10).entryMode  = '81';
  Hist(10).eci        = '05';
  Hist(10).authType   = '0';
  Hist(10).resultCd   = '00';

endsr;

//**********************************************************************
//  延滞抽出カード読込
//**********************************************************************
begsr LoadDlnqList;

  clear Dlnq;

  Dlnq(1).cardNo      = '4987650055556006';
  Dlnq(1).extractDate = '20240215';
  Dlnq(1).dlnqKbn     = '2';
  Dlnq(1).stopKbn     = '1';
  Dlnq(1).balance     = 284000;

  Dlnq(2).cardNo      = '4987650088889009';
  Dlnq(2).extractDate = '20240215';
  Dlnq(2).dlnqKbn     = '1';
  Dlnq(2).stopKbn     = '0';
  Dlnq(2).balance     = 76000;

  Dlnq(3).cardNo      = '4987650044445005';
  Dlnq(3).extractDate = '20240215';
  Dlnq(3).dlnqKbn     = '3';
  Dlnq(3).stopKbn     = '1';
  Dlnq(3).balance     = 512000;

endsr;

//**********************************************************************
//  延滞抽出カード照合
//**********************************************************************
begsr CheckDlnq;

  for ix = 1 to %elem(Dlnq);

    if Dlnq(ix).cardNo = *blanks;
      leave;
    endif;

    if Dlnq(ix).cardNo = Req.cardNo;
      Risk.dlnqHit = *on;

      if Dlnq(ix).stopKbn = '1'
         or Dlnq(ix).dlnqKbn >= '2';
        Risk.hardStop = *on;
      else;
        Risk.holdHit = *on;
        wkWarn = W_DLNQ;
      endif;

      leave;
    endif;

  endfor;

endsr;

//**********************************************************************
//  同一カード短時間連続利用
//**********************************************************************
begsr CheckRecentUse;

  wkReqSec = %dec(%subst(Req.reqTime:1:2):2:0) * 3600
           + %dec(%subst(Req.reqTime:3:2):2:0) * 60
           + %dec(%subst(Req.reqTime:5:2):2:0);

  for ix = 1 to %elem(Hist);

    if Hist(ix).cardNo = *blanks;
      leave;
    endif;

    if Hist(ix).cardNo <> Req.cardNo
       or Hist(ix).authDate <> Req.reqDate
       or Hist(ix).resultCd <> '00';
      iter;
    endif;

    wkHistSec = %dec(%subst(Hist(ix).authTime:1:2):2:0) * 3600
              + %dec(%subst(Hist(ix).authTime:3:2):2:0) * 60
              + %dec(%subst(Hist(ix).authTime:5:2):2:0);

    wkDiffSec = wkReqSec - wkHistSec;

    if wkDiffSec >= 0 and wkDiffSec <= 180;
      Risk.sameCardCnt += 1;
    endif;

  endfor;

  if Risk.sameCardCnt >= 3;
    Risk.holdHit = *on;
    wkWarn = W_REN;
  endif;

endsr;

//**********************************************************************
//  同一加盟店・同額反復
//**********************************************************************
begsr CheckRepeatAmount;

  for ix = 1 to %elem(Hist);

    if Hist(ix).cardNo = *blanks;
      leave;
    endif;

    if Hist(ix).cardNo = Req.cardNo
       and Hist(ix).merchantId = Req.merchantId
       and Hist(ix).authDate = Req.reqDate
       and Hist(ix).resultCd = '00';

      Risk.sameShopCnt += 1;

      if Hist(ix).amount = Req.amount;
        Risk.sameAmtCnt += 1;
      endif;

    endif;

  endfor;

  if Risk.sameShopCnt >= 2 and Risk.sameAmtCnt >= 2;
    Risk.holdHit = *on;

    if wkWarn = W_NONE;
      wkWarn = W_SHOP;
    endif;
  endif;

endsr;

//**********************************************************************
//  高額利用時の直近パターン比較
//**********************************************************************
begsr CheckHighAmount;

  if Req.amount < 100000;
    leavesr;
  endif;

  clear wkSumAmt;
  clear wkHistCnt;

  for ix = 1 to %elem(Hist);

    if Hist(ix).cardNo = *blanks;
      leave;
    endif;

    if Hist(ix).cardNo = Req.cardNo
       and Hist(ix).resultCd = '00';

      wkHistCnt += 1;
      wkSumAmt += Hist(ix).amount;

      if Hist(ix).amount >= 80000;
        Risk.recentHighCnt += 1;
      endif;

    endif;

  endfor;

  if wkHistCnt = 0;
    Risk.holdHit = *on;
    wkWarn = W_HIGH;
    leavesr;
  endif;

  wkAvgAmt = wkSumAmt / wkHistCnt;

  if Req.amount >= wkAvgAmt * 5
     or Risk.recentHighCnt >= 2
     or (Req.entryMode = '81' and Req.eci <> '05');

    Risk.holdHit = *on;

    if wkWarn = W_NONE;
      wkWarn = W_HIGH;
    endif;

  endif;

endsr;
