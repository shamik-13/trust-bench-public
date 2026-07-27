**free
//**********************************************************************
//  変更履歴
//  版数  年月日    担当     概要
//  001   20240401  山下     初版作成
//  002   20240517  佐伯     確定済明細の返品受付を追加
//  003   20240628  佐伯     事故扱いカードの例外登録を追加
//**********************************************************************
ctl-opt dftactgrp(*no) actgrp('CAONL')
        option(*srcstmt:*nodebugio)
        datfmt(*iso) timfmt(*iso)
        decedit('0.')
        main(CA101R);

dcl-f CDSALEF   usage(*input:*update) keyed extfile('CDSALEF');
dcl-f CDCAPF    usage(*input)         keyed extfile('CDCAPF');
dcl-f CDCARDF   usage(*input)         keyed extfile('CDCARDF');
dcl-f CDRTNF    usage(*output)             extfile('CDRTNF');
dcl-f CDEXCPF   usage(*output)             extfile('CDEXCPF');

/copy CDSALEFC
/copy CDCAPFC
/copy CDCARDFC3
/copy CDRTNC
/copy CDEXCPC

dcl-pr CA120R extpgm('CA120R');
  pCardNo        char(16) const;
  pTranKbn       char(2)  const;
  pSaleId        char(20) const;
  pAmount        packed(13:0) const;
  pCurrency      char(3)  const;
  pRespCd        char(2);
  pRespMsg       char(40);
end-pr;

dcl-pi CA101R;
  iShopId        char(10);
  iTermId        char(8);
  iOpeId         char(8);
  iReqYmd        packed(8:0);
  iReqHms        packed(6:0);
  iSaleId        char(20);
  iCardNo        char(16);
  iReturnAmt     packed(13:0);
  iCurrency      char(3);
  oAuthNo        char(10);
  oRespCd        char(2);
  oRespMsg       char(40);
  oHoldFlg       char(1);
end-pi;

dcl-ds wk qualified inz;
  today          packed(8:0);
  now            packed(6:0);
  saleFound      ind;
  capFound       ind;
  cardFound      ind;
  fixedSale      ind;
  accident       ind;
  overAmount     ind;
  feeAmt         packed(13:0);
  rtnAmt         packed(13:0);
  billAmt        packed(13:0);
  capAmt         packed(13:0);
  alreadyRtnAmt  packed(13:0);
  availAmt       packed(13:0);
  rtnSeq         packed(7:0);
  excSeq         packed(7:0);
  ca120Cd        char(2);
  ca120Msg       char(40);
end-ds;

dcl-s saveRespCd       char(2) inz;
dcl-s saveRespMsg      char(40) inz;
dcl-s sqlStateText     char(5) inz;

dcl-c RESP_OK          '00';
dcl-c RESP_HOLD        '91';
dcl-c RESP_NG          '05';
dcl-c RESP_NOTFOUND    '14';
dcl-c RESP_AMOUNT      '61';
dcl-c RESP_STOP        '57';

dcl-c STS_CARD_OK      '01';
dcl-c STS_CARD_STOP    '02';
dcl-c STS_CARD_CLOSE   '03';
dcl-c STS_CARD_DELAY   '09';

dcl-c CAP_FIXED        'C';
dcl-c CAP_SKIP         'S';
dcl-c CAP_HOLD         'H';

dcl-c CUR_JPY          'JPY';

dcl-proc CA101R;

  exsr Init;
  exsr ReadSale;
  exsr ReadCard;

  if not wk.saleFound;
    oRespCd  = RESP_NOTFOUND;
    oRespMsg = '元売上なし';
    oHoldFlg = '0';
    return;
  endif;

  if not wk.cardFound;
    exsr PutException;
    oRespCd  = RESP_HOLD;
    oRespMsg = 'カード確認保留';
    oHoldFlg = '1';
    return;
  endif;

  exsr CheckCard;
  exsr ReadCapture;
  exsr CalcReturn;

  if wk.accident;
    exsr PutException;
    oRespCd  = RESP_HOLD;
    oRespMsg = '事故扱い保留';
    oHoldFlg = '1';
    return;
  endif;

  if wk.overAmount;
    exsr PutException;
    oRespCd  = RESP_HOLD;
    oRespMsg = '返品額超過保留';
    oHoldFlg = '1';
    return;
  endif;

  monitor;
    if wk.fixedSale;
      exsr PutReturn;
      callp CA120R(iCardNo:'RT':iSaleId:wk.rtnAmt:iCurrency:
                   wk.ca120Cd:wk.ca120Msg);

      if wk.ca120Cd <> RESP_OK;
        exsr PutException;
        oRespCd  = RESP_HOLD;
        oRespMsg = '返品応答保留';
        oHoldFlg = '1';
      else;
        oRespCd  = RESP_OK;
        oRespMsg = '返品受付済';
        oHoldFlg = '0';
      endif;

    else;
      exsr CancelSale;
      callp CA120R(iCardNo:'CN':iSaleId:wk.rtnAmt:iCurrency:
                   wk.ca120Cd:wk.ca120Msg);

      if wk.ca120Cd <> RESP_OK;
        exsr PutException;
        oRespCd  = RESP_HOLD;
        oRespMsg = '取消応答保留';
        oHoldFlg = '1';
      else;
        oRespCd  = RESP_OK;
        oRespMsg = '取消受付済';
        oHoldFlg = '0';
      endif;
    endif;

  on-error;
    saveRespCd  = oRespCd;
    saveRespMsg = oRespMsg;
    exsr PutException;
    oRespCd  = RESP_HOLD;
    oRespMsg = '受付処理保留';
    oHoldFlg = '1';
  endmon;

  return;

  begsr Init;
    clear wk;
    oAuthNo  = *blanks;
    oRespCd  = *blanks;
    oRespMsg = *blanks;
    oHoldFlg = '0';

    wk.today = iReqYmd;
    wk.now   = iReqHms;
    wk.rtnAmt = iReturnAmt;

    // 海外利用の事務手数料は売上確定時に確定済。返品はその確定値を
    // 元売上から引き継ぐのみで、当オンライン処理では料率を持たない。
    wk.feeAmt = 0;
  endsr;

  begsr ReadSale;
    chain (iSaleId:iCardNo) CDSALEF;
    if %found(CDSALEF);
      wk.saleFound = *on;
      wk.billAmt = SD_BILL_AMT;
      wk.alreadyRtnAmt = SD_RTN_AMT;
      wk.feeAmt = SD_FEE_AMT;
      oAuthNo = SD_AUTH_NO;
    endif;
  endsr;

  begsr ReadCard;
    chain iCardNo CDCARDF;
    if %found(CDCARDF);
      wk.cardFound = *on;
    endif;
  endsr;

  begsr CheckCard;
    select;
    when CF_CARD_STATUS = STS_CARD_OK;
      wk.accident = *off;

    when CF_CARD_STATUS = STS_CARD_STOP
      or CF_CARD_STATUS = STS_CARD_CLOSE
      or CF_CARD_STATUS = STS_CARD_DELAY;
      wk.accident = *on;

    other;
      wk.accident = *on;
    endsl;
  endsr;

  begsr ReadCapture;
    wk.fixedSale = *off;
    wk.capFound = *off;

    setll (iSaleId:iCardNo) CDCAPF;
    dou %eof(CDCAPF);
      reade (iSaleId:iCardNo) CDCAPF;
      if %eof(CDCAPF);
        leave;
      endif;

      select;
      when BC_CAP_STATUS = CAP_FIXED;
        wk.capFound = *on;
        wk.fixedSale = *on;
        wk.capAmt += BC_CAP_AMT;

      when BC_CAP_STATUS = CAP_SKIP;
        iter;

      when BC_CAP_STATUS = CAP_HOLD;
        wk.capFound = *on;

      other;
        iter;
      endsl;
    enddo;
  endsr;

  begsr CalcReturn;
    if wk.fixedSale;
      wk.availAmt = wk.capAmt - wk.alreadyRtnAmt;
    else;
      wk.availAmt = wk.billAmt - wk.alreadyRtnAmt;
    endif;

    if wk.rtnAmt <= 0;
      wk.overAmount = *on;
    elseif wk.rtnAmt > wk.availAmt;
      wk.overAmount = *on;
    else;
      wk.overAmount = *off;
    endif;
  endsr;

  begsr PutReturn;
    clear RRCDRTN;

    RN_SHOP_ID      = iShopId;
    RN_TERM_ID      = iTermId;
    RN_OPE_ID       = iOpeId;
    RN_SALE_ID      = iSaleId;
    RN_CARD_NO      = iCardNo;
    RN_RTN_YMD      = wk.today;
    RN_RTN_HMS      = wk.now;
    RN_RTN_AMT      = wk.rtnAmt;
    RN_FEE_AMT      = wk.feeAmt;
    RN_CURRENCY     = iCurrency;
    RN_AUTH_NO      = oAuthNo;
    RN_PROC_KBN     = 'R';
    RN_ONL_STATUS   = '0';
    RN_REG_PGM      = 'CA101R';

    write RRCDRTN;
  endsr;

  begsr CancelSale;
    SD_CANCEL_YMD = wk.today;
    SD_CANCEL_HMS = wk.now;
    SD_CANCEL_OPE = iOpeId;
    SD_SALE_STATUS = '9';
    SD_RTN_AMT += wk.rtnAmt;
    SD_UPD_PGM = 'CA101R';

    update RRCDSALE;
  endsr;

  begsr PutException;
    clear RRCDEXCP;

    EX_SHOP_ID      = iShopId;
    EX_TERM_ID      = iTermId;
    EX_OPE_ID       = iOpeId;
    EX_SALE_ID      = iSaleId;
    EX_CARD_NO      = iCardNo;
    EX_REQ_YMD      = wk.today;
    EX_REQ_HMS      = wk.now;
    EX_REQ_AMT      = wk.rtnAmt;
    EX_CURRENCY     = iCurrency;
    EX_CARD_STATUS  = CF_CARD_STATUS;
    EX_CAP_STATUS   = BC_CAP_STATUS;
    EX_BILL_AMT     = wk.billAmt;
    EX_CAP_AMT      = wk.capAmt;
    EX_RTN_AMT      = wk.alreadyRtnAmt;
    EX_FEE_AMT      = wk.feeAmt;
    EX_AUTH_NO      = oAuthNo;
    EX_PGM_ID       = 'CA101R';

    select;
    when not wk.cardFound;
      EX_EXCP_KBN = 'C01';
      EX_REASON   = 'カード未登録';

    when wk.accident;
      EX_EXCP_KBN = 'C09';
      EX_REASON   = '事故扱いカード';

    when wk.overAmount;
      EX_EXCP_KBN = 'A01';
      EX_REASON   = '返品額超過';

    when saveRespCd <> *blanks;
      EX_EXCP_KBN = 'S01';
      EX_REASON   = saveRespMsg;

    other;
      EX_EXCP_KBN = 'S99';
      EX_REASON   = '受付異常';
    endsl;

    EX_HOLD_FLG = '1';
    EX_REG_YMD  = wk.today;
    EX_REG_HMS  = wk.now;

    write RRCDEXCP;
  endsr;

end-proc;
