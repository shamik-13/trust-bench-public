**free
//*********************************************************************
// 変更履歴
// 版数  年月日      担当    概要
// 1.00  2023/04/03  佐藤    初版作成
// 1.01  2023/09/18  中村    海外利用時の事務手数料判定を追加
// 1.02  2024/02/12  佐藤    未存在カードの専用ステータス返却を追加
// 1.03  2024/11/25  高橋    限度額超過候補の判定を画面返却へ追加
//*********************************************************************
// プログラム : CA120R
// 機能       : オンラインカード状態照会
// 会社       : みらいカード
//*********************************************************************

ctl-opt dftactgrp(*no)
        actgrp(*caller)
        option(*srcstmt:*nodebugio)
        bnddir('QC2LE')
        datfmt(*iso)
        timfmt(*iso);

//---------------------------------------------------------------------
// ファイル定義
//---------------------------------------------------------------------
dcl-f CDCARDF keyed usage(*input) extfile('CDCARDF') usropn;

//---------------------------------------------------------------------
// レコード複写
//---------------------------------------------------------------------
/copy CDCARDFC3

//---------------------------------------------------------------------
// 呼出インターフェース
//---------------------------------------------------------------------
dcl-pr CA120R extpgm('CA120R');
  pInCardNo        char(16)   const;
  pInUseAmt        packed(11:0) const;
  pInCurrCd        char(3)    const;
  pInShopCd        char(10)   const;
  pInTermId        char(8)    const;
  pOutJudgeKbn     char(1);
  pOutCardSts      char(2);
  pOutCapSts       char(1);
  pOutKanaMask     char(40);
  pOutAvailAmt     packed(11:0);
  pOutMsgId        char(7);
  pOutOpeMsg       char(60);
end-pr;

dcl-pi CA120R;
  pInCardNo        char(16)   const;
  pInUseAmt        packed(11:0) const;
  pInCurrCd        char(3)    const;
  pInShopCd        char(10)   const;
  pInTermId        char(8)    const;
  pOutJudgeKbn     char(1);
  pOutCardSts      char(2);
  pOutCapSts       char(1);
  pOutKanaMask     char(40);
  pOutAvailAmt     packed(11:0);
  pOutMsgId        char(7);
  pOutOpeMsg       char(60);
end-pi;

//---------------------------------------------------------------------
// 定数
//---------------------------------------------------------------------
dcl-c C_STS_VALID     '01';       // 有効
dcl-c C_STS_STOP      '02';       // 利用停止
dcl-c C_STS_CANCEL    '03';       // 解約
dcl-c C_STS_DELAY     '09';       // 延滞

dcl-c C_CAP_FIXED     'C';        // 確定
dcl-c C_CAP_SKIP      'S';        // 対象外
dcl-c C_CAP_HOLD      'H';        // 保留

dcl-c C_JPY           'JPY';
dcl-c C_JUDGE_OK      'A';        // 利用可能
dcl-c C_JUDGE_STOP    'S';        // 停止
dcl-c C_JUDGE_LIMIT   'L';        // 限度額超過候補
dcl-c C_JUDGE_NONE    'N';        // カード未存在
dcl-c C_JUDGE_ERR     'E';        // システム例外

//---------------------------------------------------------------------
// 作業領域
//---------------------------------------------------------------------
dcl-ds Wk qualified inz;
  CardNo            char(16);
  CurrCd            char(3);
  ShopCd            char(10);
  TermId            char(8);
  UseAmt            packed(11:0);
  FeeAmt            packed(11:0);
  TotalAmt          packed(11:0);
  AvailAmt          packed(11:0);
  TmpLimit          packed(11:0);
  TmpUsed           packed(11:0);
  MaskKana          char(40);
  KanaLen           packed(3:0);
  Pos               packed(3:0);
  Ch                char(1);
  Found             ind;
  FileOpened        ind;
  OverCand          ind;
  NormalEnd         ind;
end-ds;

dcl-s wkMsg          char(60);
dcl-s wkErrMsg       char(60);
dcl-s wkRemain       packed(11:0);

//---------------------------------------------------------------------
// 初期化
//---------------------------------------------------------------------
clear Wk;
clear wkMsg;
clear wkErrMsg;

eval Wk.CardNo    = %trim(pInCardNo);
eval Wk.CurrCd    = %trim(pInCurrCd);
eval Wk.ShopCd    = %trim(pInShopCd);
eval Wk.TermId    = %trim(pInTermId);
eval Wk.UseAmt    = pInUseAmt;

eval pOutJudgeKbn = C_JUDGE_ERR;
eval pOutCardSts  = *blanks;
eval pOutCapSts   = C_CAP_HOLD;
eval pOutKanaMask = *blanks;
eval pOutAvailAmt = 0;
eval pOutMsgId    = 'CA12099';
eval pOutOpeMsg   = 'カード状態照会で例外が発生しました';

//---------------------------------------------------------------------
// 主処理
//---------------------------------------------------------------------
monitor;

  if not %open(CDCARDF);
    open CDCARDF;
    eval Wk.FileOpened = *on;
  endif;

  if Wk.CardNo = *blanks;
    eval pOutJudgeKbn = C_JUDGE_NONE;
    eval pOutCardSts  = *blanks;
    eval pOutCapSts   = C_CAP_SKIP;
    eval pOutMsgId    = 'CA12001';
    eval pOutOpeMsg   = 'カード番号未入力';
    return;
  endif;

  if Wk.UseAmt < 0;
    eval pOutJudgeKbn = C_JUDGE_ERR;
    eval pOutCapSts   = C_CAP_HOLD;
    eval pOutMsgId    = 'CA12002';
    eval pOutOpeMsg   = '利用金額不正';
    return;
  endif;

  if Wk.CurrCd = *blanks;
    eval Wk.CurrCd = C_JPY;
  endif;

  chain Wk.CardNo CDCARDF;

  if not %found(CDCARDF);
    eval pOutJudgeKbn = C_JUDGE_NONE;
    eval pOutCardSts  = *blanks;
    eval pOutCapSts   = C_CAP_SKIP;
    eval pOutKanaMask = *blanks;
    eval pOutAvailAmt = 0;
    eval pOutMsgId    = 'CA12010';
    eval pOutOpeMsg   = '該当カードなし';
    return;
  endif;

  eval Wk.Found = *on;

  //-------------------------------------------------------------------
  // 会員名カナのマスク編集
  //-------------------------------------------------------------------
  eval Wk.MaskKana = *blanks;
  eval Wk.KanaLen  = %len(%trim(CF-MEM-KANA));

  if Wk.KanaLen = 0;
    eval Wk.MaskKana = *blanks;
  elseif Wk.KanaLen <= 2;
    eval Wk.Pos = 1;
    dou Wk.Pos > Wk.KanaLen;
      %subst(Wk.MaskKana:Wk.Pos:1) = '*';
      eval Wk.Pos += 1;
    enddo;
  else;
    eval %subst(Wk.MaskKana:1:1) = %subst(CF-MEM-KANA:1:1);
    eval Wk.Pos = 2;
    dou Wk.Pos >= Wk.KanaLen;
      eval %subst(Wk.MaskKana:Wk.Pos:1) = '*';
      eval Wk.Pos += 1;
    enddo;
    eval %subst(Wk.MaskKana:Wk.KanaLen:1) =
         %subst(CF-MEM-KANA:Wk.KanaLen:1);
  endif;

  //-------------------------------------------------------------------
  // 利用可能額計算
  //-------------------------------------------------------------------
  eval Wk.TmpLimit = CF-CREDIT-LIMIT;
  eval Wk.TmpUsed  = CF-USED-AMT;

  if Wk.TmpLimit < 0;
    eval Wk.TmpLimit = 0;
  endif;

  if Wk.TmpUsed < 0;
    eval Wk.TmpUsed = 0;
  endif;

  eval wkRemain = Wk.TmpLimit - Wk.TmpUsed;

  if wkRemain < 0;
    eval Wk.AvailAmt = 0;
  else;
    eval Wk.AvailAmt = wkRemain;
  endif;

  //-------------------------------------------------------------------
  // 利用可能額との照合
  //   海外利用の事務手数料は売上確定エンジンの所管であり、当照会では
  //   料率を持たない。ここでは利用金額そのもので超過候補を判定する。
  //-------------------------------------------------------------------
  eval Wk.FeeAmt   = 0;
  eval Wk.TotalAmt = Wk.UseAmt;

  if Wk.TotalAmt > Wk.AvailAmt;
    eval Wk.OverCand = *on;
  endif;

  //-------------------------------------------------------------------
  // カード状態判定
  //-------------------------------------------------------------------
  eval pOutCardSts  = CF-CARD-STATUS;
  eval pOutKanaMask = Wk.MaskKana;
  eval pOutAvailAmt = Wk.AvailAmt;

  select;
  when CF-CARD-STATUS = C_STS_VALID;

    if Wk.OverCand;
      eval pOutJudgeKbn = C_JUDGE_LIMIT;
      eval pOutCapSts   = C_CAP_HOLD;
      eval pOutMsgId    = 'CA12030';
      eval pOutOpeMsg   = '限度額超過候補';
    else;
      eval pOutJudgeKbn = C_JUDGE_OK;
      eval pOutCapSts   = C_CAP_FIXED;
      eval pOutMsgId    = 'CA12000';
      eval pOutOpeMsg   = '利用可能';
    endif;

  when CF-CARD-STATUS = C_STS_STOP;

    eval pOutJudgeKbn = C_JUDGE_STOP;
    eval pOutCapSts   = C_CAP_SKIP;
    eval pOutMsgId    = 'CA12020';
    eval pOutOpeMsg   = '利用停止カード';

  when CF-CARD-STATUS = C_STS_CANCEL;

    eval pOutJudgeKbn = C_JUDGE_STOP;
    eval pOutCapSts   = C_CAP_SKIP;
    eval pOutMsgId    = 'CA12021';
    eval pOutOpeMsg   = '解約済カード';

  when CF-CARD-STATUS = C_STS_DELAY;

    eval pOutJudgeKbn = C_JUDGE_STOP;
    eval pOutCapSts   = C_CAP_SKIP;
    eval pOutMsgId    = 'CA12022';
    eval pOutOpeMsg   = '延滞カード';

  other;

    eval pOutJudgeKbn = C_JUDGE_STOP;
    eval pOutCapSts   = C_CAP_HOLD;
    eval pOutMsgId    = 'CA12029';
    eval pOutOpeMsg   = 'カード状態区分不正';

  endsl;

  //-------------------------------------------------------------------
  // 店舗端末入力の最低限確認
  //-------------------------------------------------------------------
  if pOutJudgeKbn = C_JUDGE_OK;

    if Wk.ShopCd = *blanks or Wk.TermId = *blanks;
      eval pOutJudgeKbn = C_JUDGE_LIMIT;
      eval pOutCapSts   = C_CAP_HOLD;
      eval pOutMsgId    = 'CA12040';
      eval pOutOpeMsg   = '加盟店端末情報未設定';
    endif;

  endif;

  eval Wk.NormalEnd = *on;

on-error;

  eval pOutJudgeKbn = C_JUDGE_ERR;
  eval pOutCapSts   = C_CAP_HOLD;
  eval pOutMsgId    = 'CA12098';
  eval pOutOpeMsg   = 'カード状態照会異常';

endmon;

if Wk.FileOpened and %open(CDCARDF);
  close CDCARDF;
endif;

*inlr = *on;
return;
