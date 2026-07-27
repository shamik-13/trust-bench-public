**free
//*********************************************************************
//* 変更履歴
//* 版数  年月日    担当  概要
//* 1.00  20240115  YKM   海外ATMオーソリ取引登録 新規作成
//* 1.01  20240308  YKM   チャネル別取引区分判定をCA301Rへ集約
//* 1.02  20240521  NKT   海外ATMキャッシング区分C2付与を明確化
//*********************************************************************
ctl-opt dftactgrp(*no) actgrp('MRAUTH')
        option(*srcstmt:*nodebugio)
        bnddir('QC2LE')
        decedit('0.');

 /copy QRPGLESRC,CDTXNFC

dcl-pi *n;
  pReqCardNo       char(16)   const;
  pReqAuthDt       zoned(8:0) const;
  pReqAuthTm       zoned(6:0) const;
  pReqChannelKbn   char(2)    const;
  pReqCashKbn      char(1)    const;
  pReqAmt          packed(13:0) const;
  pReqMerId        char(15)   const;
  pReqCountryCd    char(3)    const;
  pReqCcyCd        char(3)    const;
  pResCode         char(2);
  pResAuthCd       char(6);
  pResTxnKbn       char(2);
  pResMsg          char(60);
end-pi;

dcl-f CDTXNF usage(*update:*output) keyed usropn rename(CDTXNR:TXNR);

dcl-ds AuthReq qualified inz;
  cardNo           char(16);
  authDt           zoned(8:0);
  authTm           zoned(6:0);
  channelKbn       char(2);
  cashKbn          char(1);
  amt              packed(13:0);
  merId            char(15);
  countryCd        char(3);
  ccyCd            char(3);
end-ds;

dcl-ds AuthCtl qualified inz;
  txnKbn           char(2);
  feeKbn           char(2);
  setlKbn          char(1);
  authCd           char(6);
  apprvFlg         ind;
  declineCd        char(2);
  message          char(60);
end-ds;

dcl-ds LimitCtl qualified inz;
  dayLimit         packed(13:0);
  cashLimit        packed(13:0);
  minAmt           packed(13:0);
  useAmt           packed(13:0);
  remainAmt        packed(13:0);
end-ds;

dcl-ds Work qualified inz;
  i                int(10);
  digit            int(10);
  sum              int(10);
  weight           int(10);
  cardOk           ind;
  cashReq          ind;
  overseas         ind;
  atmUse           ind;
  authSeq          packed(7:0);
  wrtRetry         int(10);
end-ds;

dcl-c CH_DOM_SHOP     '01';
dcl-c CH_DOM_ATM      '02';
dcl-c CH_EC           '03';
dcl-c CH_OVS_ATM      '04';
dcl-c CH_OVS_SHOP     '05';

dcl-c TX_DOM_SHOP     'P1';
dcl-c TX_OVS_SHOP     'P2';
dcl-c TX_DOM_CASH     'C1';
dcl-c TX_OVS_CASH     'C2';

dcl-c FEE_NONE        '00';
dcl-c FEE_OVS_ATM     'FA';
dcl-c SETL_HOLD       'H';

dcl-c OK              '00';
dcl-c NG_FORMAT       '12';
dcl-c NG_CHANNEL      '14';
dcl-c NG_AMOUNT       '51';
dcl-c NG_LIMIT        '61';
dcl-c NG_SYSTEM       '96';

pResCode = NG_SYSTEM;
pResAuthCd = *blank;
pResTxnKbn = *blank;
pResMsg = *blank;

AuthReq.cardNo = pReqCardNo;
AuthReq.authDt = pReqAuthDt;
AuthReq.authTm = pReqAuthTm;
AuthReq.channelKbn = pReqChannelKbn;
AuthReq.cashKbn = pReqCashKbn;
AuthReq.amt = pReqAmt;
AuthReq.merId = pReqMerId;
AuthReq.countryCd = pReqCountryCd;
AuthReq.ccyCd = pReqCcyCd;

monitor;
  if not %open(CDTXNF);
    open CDTXNF;
  endif;

  exsr CheckCard;
  if not Work.cardOk;
    AuthCtl.declineCd = NG_FORMAT;
    AuthCtl.message = 'カード番号エラー';
    exsr ReturnDecline;
    return;
  endif;

  exsr CheckRequest;
  if AuthCtl.declineCd <> *blank;
    exsr ReturnDecline;
    return;
  endif;

  exsr SetTxnKbn;
  exsr CheckLimit;

  if AuthCtl.declineCd <> *blank;
    exsr ReturnDecline;
    return;
  endif;

  exsr MakeAuthCode;
  exsr WriteTxn;

  pResCode = OK;
  pResAuthCd = AuthCtl.authCd;
  pResTxnKbn = AuthCtl.txnKbn;
  pResMsg = '承認';

on-error;
  pResCode = NG_SYSTEM;
  pResAuthCd = *blank;
  pResTxnKbn = AuthCtl.txnKbn;
  pResMsg = 'システムエラー';
endmon;

if %open(CDTXNF);
  close CDTXNF;
endif;

*inlr = *on;
return;

//*********************************************************************
//* カード番号検査
//*********************************************************************
begsr CheckCard;
  Work.cardOk = *off;
  Work.sum = 0;
  Work.weight = 2;

  if %trim(AuthReq.cardNo) = *blank;
    leavesr;
  endif;

  for Work.i = 16 downto 1;
    if %subst(AuthReq.cardNo:Work.i:1) < '0'
       or %subst(AuthReq.cardNo:Work.i:1) > '9';
      leavesr;
    endif;

    Work.digit = %int(%subst(AuthReq.cardNo:Work.i:1));

    if Work.weight = 2;
      Work.digit *= 2;
      if Work.digit > 9;
        Work.digit -= 9;
      endif;
      Work.weight = 1;
    else;
      Work.weight = 2;
    endif;

    Work.sum += Work.digit;
  endfor;

  if %rem(Work.sum:10) = 0;
    Work.cardOk = *on;
  endif;
endsr;

//*********************************************************************
//* 入力検査
//*********************************************************************
begsr CheckRequest;
  clear AuthCtl.declineCd;
  clear AuthCtl.message;

  select;
  when AuthReq.channelKbn = CH_DOM_SHOP
    or AuthReq.channelKbn = CH_DOM_ATM
    or AuthReq.channelKbn = CH_EC
    or AuthReq.channelKbn = CH_OVS_ATM
    or AuthReq.channelKbn = CH_OVS_SHOP;

  other;
    AuthCtl.declineCd = NG_CHANNEL;
    AuthCtl.message = '利用チャネルエラー';
    leavesr;
  endsl;

  if AuthReq.amt <= 0;
    AuthCtl.declineCd = NG_AMOUNT;
    AuthCtl.message = '金額エラー';
    leavesr;
  endif;

  if AuthReq.channelKbn = CH_DOM_ATM or AuthReq.channelKbn = CH_OVS_ATM;
    if AuthReq.cashKbn <> '1';
      AuthCtl.declineCd = NG_CHANNEL;
      AuthCtl.message = 'ATM取引種別エラー';
      leavesr;
    endif;
  endif;

  if (AuthReq.channelKbn = CH_DOM_SHOP
      or AuthReq.channelKbn = CH_EC
      or AuthReq.channelKbn = CH_OVS_SHOP)
     and AuthReq.cashKbn = '1';
    AuthCtl.declineCd = NG_CHANNEL;
    AuthCtl.message = '加盟店取引種別エラー';
    leavesr;
  endif;

  if AuthReq.channelKbn = CH_OVS_ATM
     or AuthReq.channelKbn = CH_OVS_SHOP;
    if AuthReq.countryCd = *blank or AuthReq.ccyCd = *blank;
      AuthCtl.declineCd = NG_FORMAT;
      AuthCtl.message = '海外取引国通貨エラー';
      leavesr;
    endif;
  endif;
endsr;

//*********************************************************************
//* 取引区分付与
//*********************************************************************
begsr SetTxnKbn;
  Work.cashReq = (AuthReq.cashKbn = '1');
  Work.overseas = (AuthReq.channelKbn = CH_OVS_ATM
                   or AuthReq.channelKbn = CH_OVS_SHOP);
  Work.atmUse = (AuthReq.channelKbn = CH_DOM_ATM
                 or AuthReq.channelKbn = CH_OVS_ATM);

  AuthCtl.feeKbn = FEE_NONE;
  AuthCtl.setlKbn = SETL_HOLD;

  select;
  when AuthReq.channelKbn = CH_OVS_ATM and Work.cashReq;
    AuthCtl.txnKbn = TX_OVS_CASH;
    AuthCtl.feeKbn = FEE_OVS_ATM;

  when AuthReq.channelKbn = CH_DOM_ATM and Work.cashReq;
    AuthCtl.txnKbn = TX_DOM_CASH;

  when AuthReq.channelKbn = CH_OVS_SHOP and not Work.cashReq;
    AuthCtl.txnKbn = TX_OVS_SHOP;

  when (AuthReq.channelKbn = CH_DOM_SHOP
        or AuthReq.channelKbn = CH_EC) and not Work.cashReq;
    AuthCtl.txnKbn = TX_DOM_SHOP;

  other;
    AuthCtl.declineCd = NG_CHANNEL;
    AuthCtl.message = '取引区分判定エラー';
  endsl;
endsr;

//*********************************************************************
//* 限度額判定
//*********************************************************************
begsr CheckLimit;
  if AuthCtl.declineCd <> *blank;
    leavesr;
  endif;

  LimitCtl.minAmt = 1;
  LimitCtl.useAmt = AuthReq.amt;

  select;
  when AuthCtl.txnKbn = TX_OVS_CASH;
    LimitCtl.cashLimit = 300000;
    LimitCtl.dayLimit = 500000;

  when AuthCtl.txnKbn = TX_DOM_CASH;
    LimitCtl.cashLimit = 500000;
    LimitCtl.dayLimit = 500000;

  when AuthCtl.txnKbn = TX_OVS_SHOP;
    LimitCtl.cashLimit = 0;
    LimitCtl.dayLimit = 1000000;

  when AuthCtl.txnKbn = TX_DOM_SHOP;
    LimitCtl.cashLimit = 0;
    LimitCtl.dayLimit = 1500000;

  other;
    AuthCtl.declineCd = NG_CHANNEL;
    AuthCtl.message = '限度額区分エラー';
    leavesr;
  endsl;

  if LimitCtl.useAmt < LimitCtl.minAmt;
    AuthCtl.declineCd = NG_AMOUNT;
    AuthCtl.message = '最低利用金額未満';
    leavesr;
  endif;

  if (AuthCtl.txnKbn = TX_OVS_CASH or AuthCtl.txnKbn = TX_DOM_CASH)
     and LimitCtl.useAmt > LimitCtl.cashLimit;
    AuthCtl.declineCd = NG_LIMIT;
    AuthCtl.message = 'キャッシング限度額超過';
    leavesr;
  endif;

  if LimitCtl.useAmt > LimitCtl.dayLimit;
    AuthCtl.declineCd = NG_LIMIT;
    AuthCtl.message = '日次限度額超過';
    leavesr;
  endif;

  LimitCtl.remainAmt = LimitCtl.dayLimit - LimitCtl.useAmt;
endsr;

//*********************************************************************
//* 承認番号採番
//*********************************************************************
begsr MakeAuthCode;
  Work.authSeq = %rem((AuthReq.authTm * 97) + %dec(%subst(AuthReq.cardNo:11:6):6:0):1000000);
  if Work.authSeq = 0;
    Work.authSeq = 1;
  endif;

  AuthCtl.authCd = %editc(Work.authSeq:'X');
  AuthCtl.authCd = %subst('000000':1:6-%len(%trim(AuthCtl.authCd)))
                 + %trim(AuthCtl.authCd);
endsr;

//*********************************************************************
//* 取引登録
//*********************************************************************
begsr WriteTxn;
  Work.wrtRetry = 0;

  dou Work.wrtRetry >= 2;
    monitor;
      clear TXNR;

      TXN_CARD_NO = AuthReq.cardNo;
      TXN_DT = AuthReq.authDt;
      TXN_TM = AuthReq.authTm;
      TXN_CHANNEL_KBN = AuthReq.channelKbn;
      TXN_KBN = AuthCtl.txnKbn;
      TXN_AMT = AuthReq.amt;
      TXN_AUTH_CD = AuthCtl.authCd;
      TXN_MER_ID = AuthReq.merId;
      TXN_COUNTRY_CD = AuthReq.countryCd;
      TXN_CCY_CD = AuthReq.ccyCd;
      TXN_FEE_KBN = AuthCtl.feeKbn;
      TXN_SETL_KBN = AuthCtl.setlKbn;
      TXN_ENTRY_PGM = 'CA301R';

      write TXNR;
      leave;

    on-error;
      Work.wrtRetry += 1;
      if Work.wrtRetry >= 2;
        pResCode = NG_SYSTEM;
        pResMsg = '取引登録エラー';
        return;
      endif;
    endmon;
  enddo;
endsr;

//*********************************************************************
//* 否認応答編集
//*********************************************************************
begsr ReturnDecline;
  pResCode = AuthCtl.declineCd;
  pResAuthCd = *blank;
  pResTxnKbn = AuthCtl.txnKbn;
  pResMsg = AuthCtl.message;
endsr;
