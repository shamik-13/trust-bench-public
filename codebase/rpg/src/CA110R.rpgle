**free
//**********************************************************************
//  変更履歴
//  版数  年月日     担当     概要
//  1.00  2022/04/01 宮田     初版作成
//  1.01  2023/01/16 川瀬     外貨売上時の手数料区分連携を追加
//  1.02  2024/09/10 中野     加盟店停止時の例外即時出力対応
//  1.03  2025/05/22 柴田     承認状態照合と金額超過判定の見直し
//**********************************************************************
ctl-opt dftactgrp(*no)
        actgrp('CAONLINE')
        option(*srcstmt:*nodebugio)
        bnddir('QC2LE')
        datfmt(*iso)
        timfmt(*iso);

//---------------------------------------------------------------------
//  ファイル定義
//---------------------------------------------------------------------
dcl-f CDCARDF  usage(*input)  keyed rename(CDCARDFR:CardR);
dcl-f CDAUTHF  usage(*input)  keyed rename(CDAUTHFR:AuthR);
dcl-f CDMERCF  usage(*input)  keyed rename(CDMERCFR:MercR);
dcl-f CDSALEF  usage(*output) keyed rename(CDSALEFR:SaleR);
dcl-f CDEXCPF  usage(*output) keyed rename(CDEXCPFR:ExcpR);

//---------------------------------------------------------------------
//  レコード様式コピー
//---------------------------------------------------------------------
/copy CDCARDFC3
/copy CDAUTHF2C
/copy CDMERCC
/copy CDSALEFC
/copy CDEXCPC

//---------------------------------------------------------------------
//  外部プログラム定義
//---------------------------------------------------------------------
dcl-pr CA120R extpgm('CA120R');
  pCardNo       char(16) const;
  pMercNo       char(10) const;
  pSaleAmt      packed(13:0) const;
  pCurrCd       char(3) const;
  pFeeAmt       packed(13:0);
  pRate         packed(7:4);
  pRetCd        char(2);
end-pr;

dcl-pr CA130R extpgm('CA130R');
  pCardNo       char(16) const;
  pMercNo       char(10) const;
  pAuthNo       char(6)  const;
  pReason       char(3)  const;
  pOpeText      char(40) const;
end-pr;

//---------------------------------------------------------------------
//  *ENTRY
//---------------------------------------------------------------------
dcl-pi *n;
  inTerminalId  char(8)  const;
  inCardNo      char(16) const;
  inMercNo      char(10) const;
  inSaleAmtC    char(12) const;
  inCurrCd      char(3)  const;
  inAuthNo      char(6)  const;
  outRespCd     char(3);
  outRespText   char(40);
end-pi;

//---------------------------------------------------------------------
//  作業域
//---------------------------------------------------------------------
dcl-ds Ws qualified inz;
  saleAmt        packed(13:0);
  feeAmt         packed(13:0);
  totalAmt       packed(13:0);
  rate           packed(7:4);
  availAmt       packed(13:0);
  capStatus      char(1);
  reason         char(3);
  retCd          char(2);
  nowDate        packed(8:0);
  nowTime        packed(6:0);
  authFound      ind;
  cardFound      ind;
  mercFound      ind;
  exceptOut      ind;
  saleWritten    ind;
  validInput     ind;
  work12         zoned(12:0);
end-ds;

dcl-ds KeyCard qualified inz;
  cardNo         char(16);
end-ds;

dcl-ds KeyAuth qualified inz;
  cardNo         char(16);
  authNo         char(6);
end-ds;

dcl-ds KeyMerc qualified inz;
  mercNo         char(10);
end-ds;

dcl-c C_STAT_OK       '01';
dcl-c C_STAT_STOP     '02';
dcl-c C_STAT_CLOSE    '03';
dcl-c C_STAT_ARREAR   '09';

dcl-c C_CAP_CONFIRM   'C';
dcl-c C_CAP_SKIP      'S';
dcl-c C_CAP_HOLD      'H';

dcl-c C_CURR_JPY      'JPY';

dcl-c R_OK            '000';
dcl-c R_FORMAT        '101';
dcl-c R_CARD_NONE     '201';
dcl-c R_CARD_STOP     '202';
dcl-c R_CARD_CLOSE    '203';
dcl-c R_CARD_ARREAR   '209';
dcl-c R_MERC_NONE     '301';
dcl-c R_MERC_STOP     '302';
dcl-c R_AUTH_NONE     '401';
dcl-c R_AUTH_STAT     '402';
dcl-c R_AUTH_AMT      '403';
dcl-c R_FEE_ERR       '501';
dcl-c R_FILE_ERR      '901';

//---------------------------------------------------------------------
//  主処理
//---------------------------------------------------------------------
clear Ws;
outRespCd   = *blanks;
outRespText = *blanks;

Ws.nowDate = %dec(%char(%date():*iso0):8:0);
Ws.nowTime = %dec(%char(%time():*hms0):6:0);

exsr CheckInput;

if not Ws.validInput;
  exsr SetResponse;
  *inlr = *on;
  return;
endif;

monitor;
  exsr ReadMaster;
  exsr DecideSale;

  if Ws.reason = R_OK;
    exsr WriteSale;
  endif;

  if Ws.exceptOut;
    exsr WriteException;
  endif;

on-error;
  Ws.reason = R_FILE_ERR;
  outRespCd = Ws.reason;
  outRespText = 'システム例外';
endmon;

exsr SetResponse;

*inlr = *on;
return;

//---------------------------------------------------------------------
//  入力検査
//---------------------------------------------------------------------
begsr CheckInput;

  Ws.validInput = *off;
  Ws.reason = R_FORMAT;

  if %trim(inTerminalId) = *blanks;
    leavesr;
  endif;

  if %check('0123456789': inCardNo) <> 0;
    leavesr;
  endif;

  if %check('0123456789': inMercNo) <> 0;
    leavesr;
  endif;

  if %check('0123456789': inAuthNo) <> 0;
    leavesr;
  endif;

  if %check('0123456789': inSaleAmtC) <> 0;
    leavesr;
  endif;

  if inCurrCd <> C_CURR_JPY and
     inCurrCd <> 'USD' and
     inCurrCd <> 'EUR' and
     inCurrCd <> 'CNY' and
     inCurrCd <> 'KRW';
    leavesr;
  endif;

  monitor;
    Ws.work12 = %dec(inSaleAmtC:12:0);
    Ws.saleAmt = Ws.work12;
  on-error;
    leavesr;
  endmon;

  if Ws.saleAmt <= 0;
    leavesr;
  endif;

  Ws.reason = R_OK;
  Ws.validInput = *on;

endsr;

//---------------------------------------------------------------------
//  マスタ読込
//---------------------------------------------------------------------
begsr ReadMaster;

  Ws.cardFound = *off;
  Ws.authFound = *off;
  Ws.mercFound = *off;

  KeyCard.cardNo = inCardNo;
  chain KeyCard CDCARDF;
  if %found(CDCARDF);
    Ws.cardFound = *on;
  endif;

  KeyMerc.mercNo = inMercNo;
  chain KeyMerc CDMERCF;
  if %found(CDMERCF);
    Ws.mercFound = *on;
  endif;

  KeyAuth.cardNo = inCardNo;
  KeyAuth.authNo = inAuthNo;
  chain KeyAuth CDAUTHF;
  if %found(CDAUTHF);
    Ws.authFound = *on;
  endif;

endsr;

//---------------------------------------------------------------------
//  売上判定
//---------------------------------------------------------------------
begsr DecideSale;

  Ws.reason = R_OK;
  Ws.exceptOut = *off;
  Ws.capStatus = C_CAP_CONFIRM;
  Ws.feeAmt = 0;
  Ws.totalAmt = Ws.saleAmt;

  if not Ws.cardFound;
    Ws.reason = R_CARD_NONE;
    Ws.capStatus = C_CAP_SKIP;
    leavesr;
  endif;

  if not Ws.mercFound;
    Ws.reason = R_MERC_NONE;
    Ws.capStatus = C_CAP_SKIP;
    leavesr;
  endif;

  select;
  when CF_CARD_STATUS = C_STAT_OK;
    // 継続
  when CF_CARD_STATUS = C_STAT_STOP;
    Ws.reason = R_CARD_STOP;
    Ws.capStatus = C_CAP_SKIP;
    Ws.exceptOut = *on;
    leavesr;
  when CF_CARD_STATUS = C_STAT_CLOSE;
    Ws.reason = R_CARD_CLOSE;
    Ws.capStatus = C_CAP_SKIP;
    Ws.exceptOut = *on;
    leavesr;
  when CF_CARD_STATUS = C_STAT_ARREAR;
    Ws.reason = R_CARD_ARREAR;
    Ws.capStatus = C_CAP_HOLD;
    Ws.exceptOut = *on;
    leavesr;
  other;
    Ws.reason = R_CARD_STOP;
    Ws.capStatus = C_CAP_SKIP;
    Ws.exceptOut = *on;
    leavesr;
  endsl;

  if MC_STOP_KBN = '1' or MC_USE_KBN = '9';
    Ws.reason = R_MERC_STOP;
    Ws.capStatus = C_CAP_SKIP;
    Ws.exceptOut = *on;
    leavesr;
  endif;

  if not Ws.authFound;
    Ws.reason = R_AUTH_NONE;
    Ws.capStatus = C_CAP_SKIP;
    leavesr;
  endif;

  if BC_CAP_STATUS <> C_CAP_CONFIRM;
    Ws.reason = R_AUTH_STAT;
    Ws.capStatus = C_CAP_SKIP;
    leavesr;
  endif;

  if BC_CARD_NO <> inCardNo or
     BC_MERC_NO <> inMercNo or
     BC_AUTH_NO <> inAuthNo;
    Ws.reason = R_AUTH_STAT;
    Ws.capStatus = C_CAP_SKIP;
    leavesr;
  endif;

  if BC_CURR_CD <> inCurrCd;
    Ws.reason = R_AUTH_STAT;
    Ws.capStatus = C_CAP_SKIP;
    leavesr;
  endif;

  if inCurrCd <> C_CURR_JPY;
    callp CA120R(inCardNo:
                 inMercNo:
                 Ws.saleAmt:
                 inCurrCd:
                 Ws.feeAmt:
                 Ws.rate:
                 Ws.retCd);

    if Ws.retCd <> '00';
      Ws.reason = R_FEE_ERR;
      Ws.capStatus = C_CAP_HOLD;
      leavesr;
    endif;

    Ws.totalAmt = Ws.saleAmt + Ws.feeAmt;
  endif;

  if Ws.totalAmt > BC_AUTH_AMT;
    Ws.reason = R_AUTH_AMT;
    Ws.capStatus = C_CAP_HOLD;
    Ws.exceptOut = *on;
    leavesr;
  endif;

  Ws.availAmt = CF_LIMIT_AMT - CF_USED_AMT;
  if Ws.totalAmt > Ws.availAmt;
    Ws.reason = R_AUTH_AMT;
    Ws.capStatus = C_CAP_HOLD;
    Ws.exceptOut = *on;
    leavesr;
  endif;

endsr;

//---------------------------------------------------------------------
//  売上追記
//---------------------------------------------------------------------
begsr WriteSale;

  clear SaleR;

  SF_CARD_NO      = inCardNo;
  SF_MERC_NO      = inMercNo;
  SF_AUTH_NO      = inAuthNo;
  SF_TERM_ID      = inTerminalId;
  SF_SALE_DATE    = Ws.nowDate;
  SF_SALE_TIME    = Ws.nowTime;
  SF_CURR_CD      = inCurrCd;
  SF_SALE_AMT     = Ws.saleAmt;
  SF_FEE_AMT      = Ws.feeAmt;
  SF_TOTAL_AMT    = Ws.totalAmt;
  SF_CAP_STATUS   = Ws.capStatus;
  SF_REASON_CD    = Ws.reason;
  SF_INPUT_PGM    = 'CA110R';
  SF_UPD_DATE     = Ws.nowDate;
  SF_UPD_TIME     = Ws.nowTime;

  write SaleR;
  Ws.saleWritten = *on;

endsr;

//---------------------------------------------------------------------
//  例外追記
//---------------------------------------------------------------------
begsr WriteException;

  clear ExcpR;

  EF_CARD_NO      = inCardNo;
  EF_MERC_NO      = inMercNo;
  EF_AUTH_NO      = inAuthNo;
  EF_TERM_ID      = inTerminalId;
  EF_OCCUR_DATE   = Ws.nowDate;
  EF_OCCUR_TIME   = Ws.nowTime;
  EF_REASON_CD    = Ws.reason;
  EF_CURR_CD      = inCurrCd;
  EF_REQ_AMT      = Ws.saleAmt;
  EF_FEE_AMT      = Ws.feeAmt;
  EF_TOTAL_AMT    = Ws.totalAmt;
  EF_CARD_STATUS  = CF_CARD_STATUS;
  EF_CAP_STATUS   = Ws.capStatus;
  EF_PGM_ID       = 'CA110R';

  select;
  when Ws.reason = R_MERC_STOP;
    EF_OPE_TEXT = '加盟店停止';
  when Ws.reason = R_AUTH_AMT;
    EF_OPE_TEXT = '承認金額超過';
  when Ws.reason = R_CARD_STOP;
    EF_OPE_TEXT = 'カード利用停止';
  when Ws.reason = R_CARD_CLOSE;
    EF_OPE_TEXT = 'カード解約';
  when Ws.reason = R_CARD_ARREAR;
    EF_OPE_TEXT = 'カード延滞';
  other;
    EF_OPE_TEXT = '受付例外';
  endsl;

  write ExcpR;

  callp CA130R(inCardNo:
               inMercNo:
               inAuthNo:
               Ws.reason:
               EF_OPE_TEXT);

endsr;

//---------------------------------------------------------------------
//  応答編集
//---------------------------------------------------------------------
begsr SetResponse;

  outRespCd = Ws.reason;

  select;
  when Ws.reason = R_OK;
    outRespText = '受付完了';
  when Ws.reason = R_FORMAT;
    outRespText = '入力形式不正';
  when Ws.reason = R_CARD_NONE;
    outRespText = 'カード番号未登録';
  when Ws.reason = R_CARD_STOP;
    outRespText = 'カード利用停止';
  when Ws.reason = R_CARD_CLOSE;
    outRespText = 'カード解約';
  when Ws.reason = R_CARD_ARREAR;
    outRespText = 'カード延滞';
  when Ws.reason = R_MERC_NONE;
    outRespText = '加盟店未登録';
  when Ws.reason = R_MERC_STOP;
    outRespText = '加盟店停止';
  when Ws.reason = R_AUTH_NONE;
    outRespText = '承認番号未登録';
  when Ws.reason = R_AUTH_STAT;
    outRespText = '承認状態不一致';
  when Ws.reason = R_AUTH_AMT;
    outRespText = '承認金額超過';
  when Ws.reason = R_FEE_ERR;
    outRespText = '外貨手数料算出不可';
  when Ws.reason = R_FILE_ERR;
    outRespText = 'システム例外';
  other;
    outRespText = '受付不可';
  endsl;

endsr;
