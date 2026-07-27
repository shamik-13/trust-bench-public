**free
//======================================================================
//  変更履歴
//  版数    年月日      担当        概要
//  1.00    2024/04/01  山下        初版作成
//  1.01    2024/06/18  山下        手数料プラン未設定フラグ返却を追加
//  1.02    2024/09/05  佐伯        オーソリ受付時の取消加盟店判定を追加
//======================================================================
//  プログラム : CA130R
//  機能       : 加盟店売上入力検査
//  会社       : みらいカード
//======================================================================

ctl-opt dftactgrp(*no)
        actgrp(*caller)
        option(*srcstmt:*nodebugio)
        alwnull(*usrctl)
        decedit('0.')
        datfmt(*iso)
        timfmt(*iso);

//---------------------------------------------------------------------
//  ファイル定義
//---------------------------------------------------------------------
dcl-f CDMERCF usage(*input) keyed usropn rename(CDMERCR:CDMERCR1);

/copy CDMERCC

//---------------------------------------------------------------------
//  外部インターフェース
//---------------------------------------------------------------------
dcl-pr CA130R extpgm('CA130R');
  pReqKbn        char(1)      const;
  pMercNo        char(15)     const;
  pTranAmt       packed(13:0) const;
  pSystemDate    zoned(8:0)   const;
  pSystemTime    zoned(6:0)   const;
  pOkFlg         char(1);
  pPlanNoneFlg   char(1);
  pRespCd        char(2);
  pOperMsg       char(60);
end-pr;

dcl-pi CA130R;
  pReqKbn        char(1)      const;
  pMercNo        char(15)     const;
  pTranAmt       packed(13:0) const;
  pSystemDate    zoned(8:0)   const;
  pSystemTime    zoned(6:0)   const;
  pOkFlg         char(1);
  pPlanNoneFlg   char(1);
  pRespCd        char(2);
  pOperMsg       char(60);
end-pi;

//---------------------------------------------------------------------
//  定数
//---------------------------------------------------------------------
dcl-c C_ON              '1';
dcl-c C_OFF             '0';

dcl-c C_REQ_URIAGE      '1';
dcl-c C_REQ_TORIKESHI   '2';

dcl-c C_MERC_YUKO       '1';
dcl-c C_MERC_TEISHI     '2';
dcl-c C_MERC_KAIYAKU    '9';

dcl-c C_RESP_OK         '00';
dcl-c C_RESP_MERC_NASHI '03';
dcl-c C_RESP_MERC_STOP  '57';
dcl-c C_RESP_ACCT_NASHI '76';
dcl-c C_RESP_INPUT_ERR  '80';
dcl-c C_RESP_SYS_ERR    '96';

//---------------------------------------------------------------------
//  作業領域
//---------------------------------------------------------------------
dcl-ds Wk qualified inz;
  MercNo         char(15);
  ReqKbn         char(1);
  TranAmt        packed(13:0);
  TranFeeBase    packed(13:0);
  UriageCnt      packed(3:0);
  ErrFlg         char(1);
  FoundFlg       char(1);
  OpenFlg        char(1);
end-ds;

dcl-ds Msg qualified inz;
  Text           char(60);
end-ds;

dcl-s wkRetry        packed(1:0) inz(0);
dcl-s wkDummyAmt     packed(13:0) inz(0);
dcl-s wkPlanCd       char(6) inz(*blank);
dcl-s wkSettleAcct   char(14) inz(*blank);

//---------------------------------------------------------------------
//  主処理
//---------------------------------------------------------------------
exsr InitArea;

monitor;
  if not %open(CDMERCF);
    open CDMERCF;
    Wk.OpenFlg = C_ON;
  endif;

  exsr CheckInput;

  if Wk.ErrFlg = C_OFF;
    exsr ReadMerchant;
  endif;

  if Wk.ErrFlg = C_OFF;
    exsr CheckMerchant;
  endif;

on-error;
  pOkFlg       = C_OFF;
  pPlanNoneFlg = C_OFF;
  pRespCd      = C_RESP_SYS_ERR;
  pOperMsg     = '加盟店マスタ読込でシステムエラー';
  Wk.ErrFlg    = C_ON;
endmon;

if Wk.OpenFlg = C_ON and %open(CDMERCF);
  close CDMERCF;
endif;

*inlr = *on;
return;

//---------------------------------------------------------------------
//  初期処理
//---------------------------------------------------------------------
begsr InitArea;

  Wk.MercNo       = %trim(pMercNo);
  Wk.ReqKbn       = pReqKbn;
  Wk.TranAmt      = pTranAmt;
  Wk.TranFeeBase  = 0;
  Wk.UriageCnt    = 0;
  Wk.ErrFlg       = C_OFF;
  Wk.FoundFlg     = C_OFF;
  Wk.OpenFlg      = C_OFF;

  wkRetry         = 0;
  wkDummyAmt      = 0;
  wkPlanCd        = *blank;
  wkSettleAcct    = *blank;

  pOkFlg          = C_OFF;
  pPlanNoneFlg    = C_OFF;
  pRespCd         = *blank;
  pOperMsg        = *blank;

endsr;

//---------------------------------------------------------------------
//  入力検査
//---------------------------------------------------------------------
begsr CheckInput;

  if Wk.MercNo = *blank;
    pRespCd   = C_RESP_INPUT_ERR;
    pOperMsg  = '加盟店番号未入力';
    Wk.ErrFlg = C_ON;
  elseif Wk.TranAmt <= 0;
    pRespCd   = C_RESP_INPUT_ERR;
    pOperMsg  = '売上金額エラー';
    Wk.ErrFlg = C_ON;
  elseif Wk.ReqKbn <> C_REQ_URIAGE
     and Wk.ReqKbn <> C_REQ_TORIKESHI;
    pRespCd   = C_RESP_INPUT_ERR;
    pOperMsg  = '取引区分エラー';
    Wk.ErrFlg = C_ON;
  endif;

  // 金額桁の正規化。料率計算は後続精算で実施。
  if Wk.ErrFlg = C_OFF;
    Wk.TranFeeBase = Wk.TranAmt;
    wkDummyAmt = Wk.TranFeeBase + 0;
  endif;

endsr;

//---------------------------------------------------------------------
//  加盟店マスタ読込
//---------------------------------------------------------------------
begsr ReadMerchant;

  dou Wk.FoundFlg = C_ON or wkRetry >= 2;

    monitor;
      chain(e) Wk.MercNo CDMERCF;

      if %found(CDMERCF);
        Wk.FoundFlg = C_ON;
      else;
        wkRetry += 1;
      endif;

    on-error;
      wkRetry += 1;
    endmon;

  enddo;

  if Wk.FoundFlg = C_OFF;
    pRespCd   = C_RESP_MERC_NASHI;
    pOperMsg  = '加盟店番号該当なし';
    Wk.ErrFlg = C_ON;
  endif;

endsr;

//---------------------------------------------------------------------
//  加盟店状態検査
//---------------------------------------------------------------------
begsr CheckMerchant;

  // CDMERCC 収容項目:
  //   MCMERC : 加盟店番号
  //   MCSTAT : 加盟店状態
  //   MCSETB : 精算銀行
  //   MCSETBRC: 精算支店
  //   MCSETAC: 精算口座番号
  //   MCFPLN : 手数料プランコード

  select;

  when MCSTAT = C_MERC_YUKO;
    exsr CheckSettleAccount;

  when MCSTAT = C_MERC_TEISHI;
    pRespCd   = C_RESP_MERC_STOP;
    pOperMsg  = '加盟店取扱停止中';
    Wk.ErrFlg = C_ON;

  when MCSTAT = C_MERC_KAIYAKU;
    pRespCd   = C_RESP_MERC_STOP;
    pOperMsg  = '加盟店契約解約済';
    Wk.ErrFlg = C_ON;

  other;
    pRespCd   = C_RESP_MERC_STOP;
    pOperMsg  = '加盟店状態コード不正';
    Wk.ErrFlg = C_ON;

  endsl;

  if Wk.ErrFlg = C_OFF;
    exsr CheckFeePlan;
  endif;

  if Wk.ErrFlg = C_OFF;
    pOkFlg  = C_ON;
    pRespCd = C_RESP_OK;

    if pPlanNoneFlg = C_ON;
      pOperMsg = '売上受付可・手数料プラン未設定';
    else;
      pOperMsg = '売上受付可';
    endif;
  endif;

endsr;

//---------------------------------------------------------------------
//  精算口座検査
//---------------------------------------------------------------------
begsr CheckSettleAccount;

  wkSettleAcct = %trim(MCSETB) + %trim(MCSETBRC) + %trim(MCSETAC);

  if %trim(MCSETB) = *blank
  or %trim(MCSETBRC) = *blank
  or %trim(MCSETAC) = *blank;
    pRespCd   = C_RESP_ACCT_NASHI;
    pOperMsg  = '精算口座未登録';
    Wk.ErrFlg = C_ON;
  elseif wkSettleAcct = *blank;
    pRespCd   = C_RESP_ACCT_NASHI;
    pOperMsg  = '精算口座不備';
    Wk.ErrFlg = C_ON;
  endif;

endsr;

//---------------------------------------------------------------------
//  手数料プラン検査
//---------------------------------------------------------------------
begsr CheckFeePlan;

  wkPlanCd = %trim(MCFPLN);

  if wkPlanCd = *blank;
    pPlanNoneFlg = C_ON;
  else;
    pPlanNoneFlg = C_OFF;
  endif;

endsr;
