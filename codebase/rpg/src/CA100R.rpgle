**free
ctl-opt dftactgrp(*no) actgrp(*caller)
        option(*srcstmt:*nodebugio)
        datfmt(*iso) timfmt(*iso)
        bnddir('QC2LE');

//
// 変更履歴
// 版数  年月日      担当     概要
// 0001  2023-04-01  森田     初版作成
// 0002  2024-02-15  高橋     加盟店稼働判定追加
// 0003  2025-09-30  森田     Java与信ゲートウェイ連携追加
//

/copy QRPGLESRC,CDCARDFC2
/copy QRPGLESRC,CDMERC
/copy QRPGLESRC,CDAUTHFC

dcl-pr AuthGatewayService extpgm('AUTHGWSVC');
  pAuthNo        char(12) const;
  pCardNo        char(16) const;
  pMerchantCd    char(10) const;
  pAmount        packed(13:0) const;
  pCurrency      char(3) const;
  pRetCd         char(2);
end-pr;

dcl-pi *n;
  inCardNo       char(16) const;
  inReqAmt       packed(13:0) const;
  inMerchantCd   char(10) const;
  inCurrency     char(3) const;
  outDecision    char(1);
  outReason      char(3);
  outAuthNo      char(12);
  outMsg         char(60);
end-pi;

dcl-ds Req qualified inz;
  cardNo         char(16);
  merchantCd     char(10);
  amount          packed(13:0);
  currency        char(3);
end-ds;

dcl-ds Work qualified inz;
  nowDate         date;
  nowTime         time;
  retCd           char(2);
  authNo          char(12);
  checkDigit      zoned(1:0);
  sum             packed(5:0);
  idx             packed(3:0);
  digit           packed(2:0);
  dbl             packed(2:0);
  oddEven         packed(1:0);
  cardFound       ind;
  mercFound       ind;
  ok              ind;
  duplicateHold   ind;
end-ds;

dcl-ds Msg qualified inz;
  text            char(60);
end-ds;

dcl-s ZEROAMT       packed(13:0) inz(0);
dcl-s BASECUR       char(3) inz('JPY');
dcl-s STS_ACTIVE    char(2) inz('01');
dcl-s DEC_APPROVE   char(1) inz('A');
dcl-s DEC_DECLINE   char(1) inz('D');
dcl-s RSN_LIMIT     char(3) inz('LIM');
dcl-s RSN_STATUS    char(3) inz('STS');
dcl-s RSN_CUR       char(3) inz('CUR');

Req.cardNo     = inCardNo;
Req.merchantCd = inMerchantCd;
Req.amount     = inReqAmt;
Req.currency   = inCurrency;

outDecision = *blank;
outReason   = *blank;
outAuthNo   = *blank;
outMsg      = *blank;

Work.nowDate = %date();
Work.nowTime = %time();
Work.ok = *on;

exsr ValidRequest;

if Work.ok;
  exsr ReadCard;
endif;

if Work.ok;
  exsr ReadMerchant;
endif;

if Work.ok;
  exsr CheckOpenAuth;
endif;

if Work.ok;
  exsr AcceptAuth;
endif;

*inlr = *on;
return;

begsr ValidRequest;

  if %trim(Req.cardNo) = *blank
     or %len(%trim(Req.cardNo)) <> 16;
    Work.ok = *off;
    outDecision = DEC_DECLINE;
    outReason = RSN_STATUS;
    outMsg = 'カード番号桁数エラー';
    leavesr;
  endif;

  Work.sum = 0;
  Work.oddEven = 0;

  for Work.idx = 16 downto 1;
    monitor;
      Work.digit = %dec(%subst(Req.cardNo:Work.idx:1):1:0);
    on-error;
      Work.ok = *off;
      outDecision = DEC_DECLINE;
      outReason = RSN_STATUS;
      outMsg = 'カード番号数字エラー';
      leavesr;
    endmon;

    if Work.oddEven = 1;
      Work.dbl = Work.digit * 2;
      if Work.dbl > 9;
        Work.dbl -= 9;
      endif;
      Work.sum += Work.dbl;
      Work.oddEven = 0;
    else;
      Work.sum += Work.digit;
      Work.oddEven = 1;
    endif;
  endfor;

  if %rem(Work.sum:10) <> 0;
    Work.ok = *off;
    outDecision = DEC_DECLINE;
    outReason = RSN_STATUS;
    outMsg = 'カード番号検査桁エラー';
    leavesr;
  endif;

  if Req.amount <= ZEROAMT;
    Work.ok = *off;
    outDecision = DEC_DECLINE;
    outReason = RSN_LIMIT;
    outMsg = '金額指定エラー';
    leavesr;
  endif;

  if Req.currency <> BASECUR;
    Work.ok = *off;
    outDecision = DEC_DECLINE;
    outReason = RSN_CUR;
    outMsg = '取扱通貨対象外';
    leavesr;
  endif;

endsr;

begsr ReadCard;

  Work.cardFound = *off;

  monitor;
    chain Req.cardNo CDCARDF;
    if %found(CDCARDF);
      Work.cardFound = *on;
    endif;
  on-error;
    Work.cardFound = *off;
  endmon;

  if not Work.cardFound;
    Work.ok = *off;
    outDecision = DEC_DECLINE;
    outReason = RSN_STATUS;
    outMsg = 'カード未登録';
    leavesr;
  endif;

  select;
  when CF_CARD_STATUS = STS_ACTIVE;
    // 有効
  when CF_CARD_STATUS = '02';
    Work.ok = *off;
    outDecision = DEC_DECLINE;
    outReason = RSN_STATUS;
    outMsg = '利用停止カード';
  when CF_CARD_STATUS = '03';
    Work.ok = *off;
    outDecision = DEC_DECLINE;
    outReason = RSN_STATUS;
    outMsg = '解約カード';
  when CF_CARD_STATUS = '09';
    Work.ok = *off;
    outDecision = DEC_DECLINE;
    outReason = RSN_STATUS;
    outMsg = '延滞カード';
  other;
    Work.ok = *off;
    outDecision = DEC_DECLINE;
    outReason = RSN_STATUS;
    outMsg = 'カード状態不正';
  endsl;

  if not Work.ok;
    leavesr;
  endif;

  if Req.amount > CF_AVAIL_LIMIT;
    Work.ok = *off;
    outDecision = DEC_DECLINE;
    outReason = RSN_LIMIT;
    outMsg = '利用可能枠超過';
    leavesr;
  endif;

endsr;

begsr ReadMerchant;

  Work.mercFound = *off;

  monitor;
    chain Req.merchantCd CDMERC;
    if %found(CDMERC);
      Work.mercFound = *on;
    endif;
  on-error;
    Work.mercFound = *off;
  endmon;

  if not Work.mercFound;
    Work.ok = *off;
    outDecision = DEC_DECLINE;
    outReason = RSN_STATUS;
    outMsg = '加盟店未登録';
    leavesr;
  endif;

  if MC_ACTIVE_KBN <> '1';
    Work.ok = *off;
    outDecision = DEC_DECLINE;
    outReason = RSN_STATUS;
    outMsg = '加盟店稼働停止';
    leavesr;
  endif;

  if MC_CURRENCY <> BASECUR;
    Work.ok = *off;
    outDecision = DEC_DECLINE;
    outReason = RSN_CUR;
    outMsg = '加盟店通貨対象外';
    leavesr;
  endif;

endsr;

begsr CheckOpenAuth;

  Work.duplicateHold = *off;

  setll Req.cardNo CDAUTHF;
  dou %eof(CDAUTHF);
    reade Req.cardNo CDAUTHF;
    if %eof(CDAUTHF);
      leave;
    endif;

    if AU_CARD_NO = Req.cardNo
       and AU_MERCHANT_CD = Req.merchantCd
       and AU_AUTH_AMT = Req.amount
       and AU_AUTH_DATE = Work.nowDate
       and AU_AUTH_RESULT = '00';
      Work.duplicateHold = *on;
      leave;
    endif;
  enddo;

  if Work.duplicateHold;
    Work.ok = *off;
    outDecision = DEC_DECLINE;
    outReason = RSN_LIMIT;
    outMsg = '同一与信受付済';
    leavesr;
  endif;

endsr;

begsr AcceptAuth;

  // 受付番号採番。日次オンライン内での簡易採番（年月日下6桁＋時分秒）。
  monitor;
    Work.authNo = %subst(%char(%date():*iso0):3:6)
                + %char(%time():*hms0);
    Work.retCd = '00';
  on-error;
    Work.retCd = '99';
  endmon;

  if Work.retCd <> '00';
    Work.ok = *off;
    outDecision = DEC_DECLINE;
    outReason = RSN_STATUS;
    outMsg = '受付番号採番エラー';
    leavesr;
  endif;

  clear CDAUTHFR;
  AU_AUTH_NO        = Work.authNo;
  AU_CARD_NO        = Req.cardNo;
  AU_MERCHANT_CD    = Req.merchantCd;
  AU_AUTH_AMT       = Req.amount;
  AU_CURRENCY       = Req.currency;
  AU_AUTH_DATE      = Work.nowDate;
  AU_AUTH_TIME      = Work.nowTime;
  AU_AUTH_RESULT    = '10';
  AR_DECISION_KBN   = *blank;
  AR_DECLINE_REASON = *blank;

  monitor;
    write CDAUTHFR;
  on-error;
    Work.ok = *off;
    outDecision = DEC_DECLINE;
    outReason = RSN_STATUS;
    outMsg = '与信受付登録エラー';
    leavesr;
  endmon;

  monitor;
    callp AuthGatewayService(Work.authNo:
                             Req.cardNo:
                             Req.merchantCd:
                             Req.amount:
                             Req.currency:
                             Work.retCd);
  on-error;
    Work.retCd = '99';
  endmon;

  outAuthNo = Work.authNo;

  if Work.retCd = '00';
    outDecision = DEC_APPROVE;
    outReason = *blank;
    outMsg = '与信判定依頼受付中';
  else;
    outDecision = DEC_DECLINE;
    outReason = RSN_STATUS;
    outMsg = '与信ゲートウェイ連携エラー';
  endif;

endsr;
