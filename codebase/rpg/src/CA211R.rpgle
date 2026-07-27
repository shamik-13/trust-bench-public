**free
//**********************************************************************
//  変更履歴
//    版数   年月日     担当     概要
//    1.00   20190401   MIYATA   新規作成
//    1.10   20200615   MIYATA   海外加盟店判定追加
//    1.20   20211108   KAWAI    MCC未設定時の理由区分追加
//    1.30   20230320   KAWAI    高リスク加盟店サービス連携追加
//**********************************************************************
ctl-opt dftactgrp(*no) actgrp('CAUTH')
        option(*srcstmt:*nodebugio)
        datfmt(*iso) timfmt(*iso)
        alwnull(*usrctl);

dcl-f CDMERF keyed usage(*input) usropn rename(CDMERR:CDMERF_R);

// 加盟店マスタ
/copy QRPGLESRC,CDMERC

dcl-pr CA211R extpgm('CA211R');
  pReqMercCd      char(15) const;
  pReqTranAmt     packed(13:0) const;
  pReqCurCd       char(3) const;
  pReqTermId      char(8) const;
  pAnsReason      char(2);
  pAnsMercNm      char(40);
  pAnsMcc         char(4);
  pAnsCountry     char(3);
  pAnsRiskNote    char(1);
  pAnsStatus      char(1);
  pMsgText        char(60);
end-pr;

dcl-pi CA211R;
  pReqMercCd      char(15) const;
  pReqTranAmt     packed(13:0) const;
  pReqCurCd       char(3) const;
  pReqTermId      char(8) const;
  pAnsReason      char(2);
  pAnsMercNm      char(40);
  pAnsMcc         char(4);
  pAnsCountry     char(3);
  pAnsRiskNote    char(1);
  pAnsStatus      char(1);
  pMsgText        char(60);
end-pi;

dcl-pr MerchantRiskService extpgm('MRRSKSRV');
  rMercCd         char(15) const;
  rMcc            char(4) const;
  rCountry        char(3) const;
  rTranAmt        packed(13:0) const;
  rCurCd          char(3) const;
  rTermId         char(8) const;
  rRiskNote       char(1);
  rScore          packed(5:0);
  rSvcStatus      char(1);
end-pr;

dcl-ds wk qualified inz;
  mercCd          char(15);
  tranAmt         packed(13:0);
  curCd           char(3);
  termId          char(8);
  riskScore       packed(5:0);
  svcStatus       char(1);
  yenAmt          packed(13:0);
  floorAmt        packed(13:0);
  authOk          ind;
  found           ind;
  callRisk        ind;
  stopFlg         char(1);
  highRiskFlg     char(1);
  overseasFlg     char(1);
end-ds;

dcl-s wkRate          packed(9:4) inz(1);
dcl-s wkRetry         packed(1:0) inz(0);
dcl-s wkErrText       char(60) inz(*blank);

// 初期化
clear pAnsReason;
clear pAnsMercNm;
clear pAnsMcc;
clear pAnsCountry;
clear pAnsRiskNote;
clear pAnsStatus;
clear pMsgText;

wk.mercCd   = %trim(pReqMercCd);
wk.tranAmt  = pReqTranAmt;
wk.curCd    = %trim(pReqCurCd);
wk.termId   = %trim(pReqTermId);
wk.authOk   = *off;
wk.found    = *off;
wk.callRisk = *off;

pAnsStatus = '9';
pAnsReason = '96';
pMsgText   = 'システムエラー';

// 入力最低限チェック
if wk.mercCd = *blank;
  pAnsStatus = '1';
  pAnsReason = '03';
  pMsgText   = '加盟店コード未入力';
  return;
endif;

if wk.tranAmt <= 0;
  pAnsStatus = '1';
  pAnsReason = '13';
  pMsgText   = '金額エラー';
  return;
endif;

if wk.curCd = *blank;
  wk.curCd = '392';
endif;

// 通貨換算（オンライン判定用概算）
select;
when wk.curCd = '392';
  wkRate = 1;
when wk.curCd = '840';
  wkRate = 150.0000;
when wk.curCd = '978';
  wkRate = 162.0000;
when wk.curCd = '156';
  wkRate = 21.0000;
other;
  wkRate = 1;
endsl;

wk.yenAmt = %dech(wk.tranAmt * wkRate: 13: 0);

// 加盟店マスタ照会
monitor;
  if not %open(CDMERF);
    open CDMERF;
  endif;

  chain wk.mercCd CDMERF_R;
  if %found(CDMERF);
    wk.found = *on;
  endif;

on-error;
  wkErrText = '加盟店マスタ読込エラー';
  pAnsStatus = '9';
  pAnsReason = '96';
  pMsgText   = wkErrText;
  return;
endmon;

if not wk.found;
  pAnsStatus = '1';
  pAnsReason = '03';
  pMsgText   = '加盟店未登録';
  return;
endif;

// 応答項目編集
pAnsMercNm  = CDMERC.MERNAME;
pAnsMcc     = CDMERC.MCCCD;
pAnsCountry = CDMERC.CNTRYCD;

wk.stopFlg     = CDMERC.STOPFLG;
wk.highRiskFlg = CDMERC.HIRISK;
wk.overseasFlg = *blank;

if CDMERC.CNTRYCD <> *blank and CDMERC.CNTRYCD <> '392';
  wk.overseasFlg = '1';
endif;

// 加盟店状態をオンライン理由区分へ変換
select;
when wk.stopFlg = '1';
  pAnsStatus = '1';
  pAnsReason = '57';
  pMsgText   = '停止加盟店';
  return;

when CDMERC.MCCCD = *blank or CDMERC.MCCCD = '0000';
  pAnsStatus = '1';
  pAnsReason = '62';
  pMsgText   = 'ＭＣＣ未設定';
  return;

when wk.overseasFlg = '1';
  pAnsStatus = '1';
  pAnsReason = '58';
  pMsgText   = '海外国コード加盟店';
  return;

other;
  pAnsStatus = '0';
  pAnsReason = '00';
  pMsgText   = '正常';
endsl;

// 取扱限度チェック
wk.floorAmt = CDMERC.FLOORAMT;
if wk.floorAmt > 0 and wk.yenAmt > wk.floorAmt;
  pAnsStatus = '1';
  pAnsReason = '61';
  pMsgText   = '加盟店限度超過';
  return;
endif;

// 高リスク加盟店はリスクサービスへ照会
if wk.highRiskFlg = '1';
  wk.callRisk = *on;
endif;

if wk.callRisk;
  pAnsRiskNote = '0';
  wk.svcStatus = *blank;
  wk.riskScore = 0;
  wkRetry = 0;

  dou wkRetry >= 2 or wk.svcStatus = '0';
    wkRetry += 1;

    monitor;
      callp MerchantRiskService(
              wk.mercCd:
              CDMERC.MCCCD:
              CDMERC.CNTRYCD:
              wk.yenAmt:
              wk.curCd:
              wk.termId:
              pAnsRiskNote:
              wk.riskScore:
              wk.svcStatus);

    on-error;
      wk.svcStatus = '9';
      pAnsRiskNote = '1';
    endmon;
  enddo;

  if wk.svcStatus <> '0';
    pAnsRiskNote = '1';
    pAnsStatus   = '0';
    pAnsReason   = '00';
    pMsgText     = 'リスクサービス未応答';
    return;
  endif;

  if wk.riskScore >= 900;
    pAnsRiskNote = '1';
  elseif wk.riskScore >= 700;
    pAnsRiskNote = '2';
  else;
    pAnsRiskNote = '0';
  endif;
else;
  pAnsRiskNote = '0';
endif;

pAnsStatus = '0';
pAnsReason = '00';
pMsgText   = '正常';

return;
