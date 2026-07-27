**free
//**********************************************************************
//*  プログラム : CA114R
//*  名称       : 国内キャッシングオーソリ
//*  目的       : 国内ATMキャッシング要求の暗証照合済みフラグ、チャネル区分、
//*               利用可能キャッシング枠を検証し、国内用取引区分を設定する。
//*
//*  変更履歴
//*  版数  年月日    担当     概要
//*  1.00  20190401  佐藤     初版作成
//*  1.10  20210614  高橋     暗証照合済みフラグ判定を追加
//*  1.20  20230919  中村     国内ATM網追加に伴いチャネル区分02判定を追加
//*  1.21  20240508  中村     手数料対象候補フラグの戻し項目を追加
//**********************************************************************

ctl-opt dftactgrp(*no)
        actgrp(*caller)
        option(*srcstmt:*nodebugio)
        bnddir('QC2LE')
        main(CA114R_Main);

//---------------------------------------------------------------------
//  レコード様式コピー
//---------------------------------------------------------------------
/copy QRPGLESRC,CDACCFC
/copy QRPGLESRC,CDTXNFC
/copy QRPGLESRC,CDAUTHF4C

//---------------------------------------------------------------------
//  外部プログラム定義
//---------------------------------------------------------------------
dcl-pr CA113R extpgm('CA113R');
  pAccNo        char(16) const;
  pTxnAmt       packed(13:0) const;
  pDispKbn      char(1) const;
  pAvailAmt     packed(13:0);
  pAuthKekka    char(1);
  pRespCd       char(2);
end-pr;

//---------------------------------------------------------------------
//  *ENTRY パラメータ
//---------------------------------------------------------------------
dcl-pi CA114R_Main;
  piAuthReq        likeds(AuthReqDs);
  piAuthAns        likeds(AuthAnsDs);
end-pi;

//---------------------------------------------------------------------
//  定数
//---------------------------------------------------------------------
dcl-c CH_DOM_COUNTER    '01';
dcl-c CH_DOM_ATM        '02';
dcl-c CH_EC             '03';
dcl-c CH_FOR_ATM        '04';
dcl-c CH_FOR_MERCH      '05';

dcl-c TXN_DOM_SHOP      'P1';
dcl-c TXN_FOR_SHOP      'P2';
dcl-c TXN_DOM_CASH      'C1';
dcl-c TXN_FOR_CASH      'C2';
dcl-c TXN_ANNUAL        'A1';

dcl-c FEE_NONE          '00';
dcl-c FEE_FOR_ATM       'FA';
dcl-c FEE_BRAND_ADV     'FB';

dcl-c SETL_FIXED        'D';
dcl-c SETL_HOLD         'H';

dcl-c DISP_SHOP         'S';
dcl-c DISP_CASH         'K';

dcl-c FLG_ON            '1';
dcl-c FLG_OFF           '0';

dcl-c AUTH_OK           '0';
dcl-c AUTH_NG           '1';

dcl-c RESP_OK           '00';
dcl-c RESP_FMT_ERR      '12';
dcl-c RESP_PIN_NG       '55';
dcl-c RESP_LIMIT_NG     '51';
dcl-c RESP_CH_NG        '57';
dcl-c RESP_SYS_ERR      '96';

//---------------------------------------------------------------------
//  作業領域
//---------------------------------------------------------------------
dcl-s wkAvailAmt        packed(13:0) inz(0);
dcl-s wkReqAmt          packed(13:0) inz(0);
dcl-s wkAuthKekka       char(1) inz(AUTH_NG);
dcl-s wkRespCd          char(2) inz(RESP_SYS_ERR);
dcl-s wkFeeCand         char(1) inz(FLG_OFF);
dcl-s wkErr             ind inz(*off);
dcl-s wkIx              packed(3:0) inz(0);
dcl-s wkRetryCnt        packed(1:0) inz(0);

dcl-ds AuthReqDs qualified template;
  ReqId             char(20);
  AccNo             char(16);
  CardNo            char(16);
  ChannelKbn        char(2);
  TermId            char(12);
  AtmNetCd          char(4);
  PinChkdFlg        char(1);
  TxnAmt            packed(13:0);
  ReqYmd            char(8);
  ReqHms            char(6);
  MerchantCd        char(15);
end-ds;

dcl-ds AuthAnsDs qualified template;
  ReqId             char(20);
  AuthKekka         char(1);
  RespCd            char(2);
  TxnKbn            char(2);
  FeeKbn            char(2);
  SetlKbn           char(1);
  DispKbn           char(1);
  AvailCashAmt      packed(13:0);
  FeeTaisyoKohoFlg  char(1);
  MsgTxt            char(60);
end-ds;

dcl-ds AccWork qualified inz;
  AccNo             char(16);
  CashLimitAmt      packed(13:0);
  CashUsedAmt       packed(13:0);
  CashAuthHoldAmt   packed(13:0);
  StopKbn           char(1);
  CardStatus        char(1);
end-ds;

dcl-ds TxnWork qualified inz;
  TxnKbn            char(2);
  FeeKbn            char(2);
  SetlKbn           char(1);
  DispKbn           char(1);
  ReqAmt            packed(13:0);
  AvailAmt          packed(13:0);
end-ds;

//---------------------------------------------------------------------
//  初期化
//---------------------------------------------------------------------
clear piAuthAns;
piAuthAns.ReqId            = piAuthReq.ReqId;
piAuthAns.AuthKekka        = AUTH_NG;
piAuthAns.RespCd           = RESP_SYS_ERR;
piAuthAns.TxnKbn           = *blanks;
piAuthAns.FeeKbn           = FEE_NONE;
piAuthAns.SetlKbn          = SETL_HOLD;
piAuthAns.DispKbn          = DISP_CASH;
piAuthAns.FeeTaisyoKohoFlg = FLG_OFF;
piAuthAns.MsgTxt           = *blanks;

wkReqAmt = piAuthReq.TxnAmt;

//---------------------------------------------------------------------
//  入力基本チェック
//---------------------------------------------------------------------
monitor;

  if piAuthReq.AccNo = *blanks
     or piAuthReq.CardNo = *blanks
     or wkReqAmt <= 0;

    piAuthAns.RespCd    = RESP_FMT_ERR;
    piAuthAns.MsgTxt    = '要求電文項目エラー';
    return;

  endif;

  if piAuthReq.PinChkdFlg <> FLG_ON;
    piAuthAns.RespCd    = RESP_PIN_NG;
    piAuthAns.MsgTxt    = '暗証番号未照合';
    return;
  endif;

  //-------------------------------------------------------------------
  //  国内キャッシング対象チャネル判定
  //  国内ATM網追加により02を国内キャッシングとして扱う。
  //-------------------------------------------------------------------
  select;
  when piAuthReq.ChannelKbn = CH_DOM_ATM;
    TxnWork.TxnKbn   = TXN_DOM_CASH;
    TxnWork.FeeKbn   = FEE_NONE;
    TxnWork.SetlKbn  = SETL_HOLD;
    TxnWork.DispKbn  = DISP_CASH;

  when piAuthReq.ChannelKbn = CH_DOM_COUNTER;
    TxnWork.TxnKbn   = TXN_DOM_CASH;
    TxnWork.FeeKbn   = FEE_NONE;
    TxnWork.SetlKbn  = SETL_HOLD;
    TxnWork.DispKbn  = DISP_CASH;

  other;
    piAuthAns.RespCd = RESP_CH_NG;
    piAuthAns.MsgTxt = '国内キャッシング対象外チャネル';
    return;
  endsl;

  //-------------------------------------------------------------------
  //  国内ATM網コードの簡易確認
  //-------------------------------------------------------------------
  if piAuthReq.ChannelKbn = CH_DOM_ATM;
    wkIx = 1;

    dou wkIx > 3 or wkErr;
      select;
      when wkIx = 1 and piAuthReq.AtmNetCd = 'MICS';
        wkErr = *on;
      when wkIx = 2 and piAuthReq.AtmNetCd = 'BANK';
        wkErr = *on;
      when wkIx = 3 and piAuthReq.AtmNetCd = 'POST';
        wkErr = *on;
      endsl;

      wkIx += 1;
    enddo;

    if not wkErr;
      piAuthAns.RespCd = RESP_CH_NG;
      piAuthAns.MsgTxt = '国内ATM網対象外';
      return;
    endif;
  endif;

  //-------------------------------------------------------------------
  //  与信照会
  //-------------------------------------------------------------------
  wkRetryCnt = 0;

  dow wkRetryCnt < 2;
    wkRetryCnt += 1;

    callp CA113R(piAuthReq.AccNo:
                 wkReqAmt:
                 DISP_CASH:
                 wkAvailAmt:
                 wkAuthKekka:
                 wkRespCd);

    if wkRespCd <> RESP_SYS_ERR;
      leave;
    endif;
  enddo;

  if wkRespCd <> RESP_OK;
    piAuthAns.RespCd = wkRespCd;
    piAuthAns.MsgTxt = 'キャッシング枠照会否決';
    piAuthAns.AvailCashAmt = wkAvailAmt;
    return;
  endif;

  //-------------------------------------------------------------------
  //  利用可能キャッシング枠確認
  //-------------------------------------------------------------------
  if wkAuthKekka <> AUTH_OK;
    piAuthAns.RespCd = RESP_LIMIT_NG;
    piAuthAns.MsgTxt = '利用可能枠不足';
    piAuthAns.AvailCashAmt = wkAvailAmt;
    return;
  endif;

  if wkAvailAmt < wkReqAmt;
    piAuthAns.RespCd = RESP_LIMIT_NG;
    piAuthAns.MsgTxt = '利用可能枠不足';
    piAuthAns.AvailCashAmt = wkAvailAmt;
    return;
  endif;

  TxnWork.ReqAmt   = wkReqAmt;
  TxnWork.AvailAmt = wkAvailAmt - wkReqAmt;

  //-------------------------------------------------------------------
  //  承認応答編集
  //-------------------------------------------------------------------
  piAuthAns.AuthKekka        = AUTH_OK;
  piAuthAns.RespCd           = RESP_OK;
  piAuthAns.TxnKbn           = TxnWork.TxnKbn;
  piAuthAns.FeeKbn           = TxnWork.FeeKbn;
  piAuthAns.SetlKbn          = TxnWork.SetlKbn;
  piAuthAns.DispKbn          = TxnWork.DispKbn;
  piAuthAns.AvailCashAmt     = TxnWork.AvailAmt;
  piAuthAns.FeeTaisyoKohoFlg = wkFeeCand;
  piAuthAns.MsgTxt           = '承認';

on-error;
  piAuthAns.AuthKekka        = AUTH_NG;
  piAuthAns.RespCd           = RESP_SYS_ERR;
  piAuthAns.TxnKbn           = *blanks;
  piAuthAns.FeeKbn           = FEE_NONE;
  piAuthAns.SetlKbn          = SETL_HOLD;
  piAuthAns.DispKbn          = DISP_CASH;
  piAuthAns.FeeTaisyoKohoFlg = FLG_OFF;
  piAuthAns.MsgTxt           = 'システムエラー';
endmon;

return;
