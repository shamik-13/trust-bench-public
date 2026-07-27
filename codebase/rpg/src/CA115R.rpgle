**free
//**********************************************************************
//  みらいカード　オンライン承認システム
//**********************************************************************
//  プログラムＩＤ : CA115R
//  プログラム名   : 加盟店売上オンライン捕捉
//  機能概要       : 加盟店売上捕捉要求を承認ＩＤまたは取引ＩＤで照合し、
//                 : 捕捉候補ファイルへ出力する。
//**********************************************************************
//  変更履歴
//  版数  年月日    担当      概要
//  0001  20240115  佐藤      新規作成
//  0002  20240308  中村      海外加盟店売上の手数料区分判定を追加
//  0003  20240521  佐藤      部分売上対応、承認残額内の複数捕捉を許可
//  0004  20240619  田中      捕捉期限超過時の保留理由を監査ログへ出力
//**********************************************************************

ctl-opt dftactgrp(*no)
        actgrp(*caller)
        option(*srcstmt:*nodebugio)
        datfmt(*iso)
        timfmt(*iso)
        decedit('0.')
        bnddir('QC2LE');

//--------------------------------------------------------------------
//  ファイル定義
//--------------------------------------------------------------------
dcl-f CDAUTHF keyed usage(*input:*update) rename(CDAUTHFR:AUTHR);
dcl-f CDTXNF  keyed usage(*input)         rename(CDTXNFR :TXNR);
dcl-f CDCAPF  usage(*output)              rename(CDCAPFR :CAPR);
dcl-f CDLOGF  usage(*output)              rename(CDLOGFR :LOGR);

//--------------------------------------------------------------------
//  レコード様式コピー
//--------------------------------------------------------------------
/copy QRPGLESRC,CDAUTHF4C
/copy QRPGLESRC,CDTXNFC
/copy QRPGLESRC,CDCAPF2C
/copy QRPGLESRC,CDLOGFC

//--------------------------------------------------------------------
//  外部プログラム
//--------------------------------------------------------------------
dcl-pr CA113R extpgm('CA113R');
  pLogKbn        char(2)   const;
  pMemberNo      char(16)  const;
  pAuthId        char(12)  const;
  pTxnId         char(20)  const;
  pResultCd      char(3)   const;
  pMessage       char(80)  const;
end-pr;

//--------------------------------------------------------------------
//  *ENTRY パラメータ
//--------------------------------------------------------------------
dcl-pi *n;
  pIn            likeds(InParm) const;
  pOut           likeds(OutParm);
end-pi;

//--------------------------------------------------------------------
//  入出力パラメータ
//--------------------------------------------------------------------
dcl-ds InParm qualified;
  ReqId          char(20);
  MerchantNo     char(15);
  TerminalId     char(8);
  ChannelKbn     char(2);        // 01=国内店頭 02=国内ATM 03=EC 04=海外ATM 05=海外加盟店
  TxnKbn         char(2);        // P1/P2/C1/C2/A1
  AuthId         char(12);
  TxnId          char(20);
  CaptureAmt     packed(13:0);
  CaptureDate    date(*iso);
  CurrencyCd     char(3);
  OperatorId     char(10);
  RecvTime       timestamp;
end-ds;

dcl-ds OutParm qualified;
  ResultCd       char(3);
  MatchKbn       char(1);
  HoldReason     char(2);
  AuthId         char(12);
  TxnId          char(20);
  ApprovedAmt    packed(13:0);
  CapturedAmt    packed(13:0);
  RemainAmt      packed(13:0);
  FeeKbn         char(2);        // 00=なし FA=海外ATM事務手数料 FB=国際ブランド立替手数料
  SetlKbn        char(1);        // D=確定 H=保留
  DispKbn        char(1);        // S=ショッピング K=キャッシング
  Message        char(80);
end-ds;

//--------------------------------------------------------------------
//  作業領域
//--------------------------------------------------------------------
dcl-ds Wk qualified;
  FoundAuth      ind inz(*off);
  FoundTxn       ind inz(*off);
  IsPartial      ind inz(*off);
  IsExpired      ind inz(*off);
  IsAmtOver      ind inz(*off);
  IsSameShop     ind inz(*off);
  IsValidTxn     ind inz(*off);
  MatchKbn       char(1) inz('0');
  HoldReason     char(2) inz('00');
  SetlKbn        char(1) inz('H');
  FeeKbn         char(2) inz('00');
  DispKbn        char(1) inz('S');
  AuthLimitDate  date(*iso);
  CapSeq         packed(7:0) inz(0);
  AuthAmt        packed(13:0) inz(0);
  AlreadyCapAmt  packed(13:0) inz(0);
  RemainAmt      packed(13:0) inz(0);
  CaptureAmt     packed(13:0) inz(0);
  LogMsg         char(80) inz(*blank);
end-ds;

dcl-s wkToday       date(*iso) inz(*sys);
dcl-s wkNow         timestamp inz(*sys);
dcl-s wkRrn         int(10) inz(0);
dcl-s wkRetry       int(5) inz(0);
dcl-s wkMaxRetry    int(5) inz(3);
dcl-s wkSqlState    char(5) inz(*blank);
dcl-s wkResult      char(3) inz('000');

//--------------------------------------------------------------------
//  主処理
//--------------------------------------------------------------------
clear pOut;
pOut.ResultCd = '000';
pOut.MatchKbn = '0';
pOut.HoldReason = '00';
pOut.FeeKbn = '00';
pOut.SetlKbn = 'H';
pOut.Message = *blank;

monitor;

  exsr InitWork;
  exsr CheckInput;

  if pOut.ResultCd = '000';

    exsr ReadAuthorization;

    if Wk.FoundAuth;
      exsr DecideCapture;
      exsr WriteCapture;
    else;
      Wk.MatchKbn = '9';
      Wk.HoldReason = '01';
      Wk.SetlKbn = 'H';
      Wk.LogMsg = '承認照合なし：捕捉保留';
      pOut.ResultCd = '101';
      pOut.Message = Wk.LogMsg;
    endif;

    exsr WriteLog;

  endif;

on-error;
  pOut.ResultCd = '900';
  pOut.MatchKbn = '9';
  pOut.HoldReason = '99';
  pOut.SetlKbn = 'H';
  pOut.Message = '捕捉処理で例外発生';
  Wk.MatchKbn = '9';
  Wk.HoldReason = '99';
  Wk.SetlKbn = 'H';
  Wk.LogMsg = pOut.Message;
  exsr WriteLog;
endmon;

return;

//--------------------------------------------------------------------
//  初期化
//--------------------------------------------------------------------
begsr InitWork;

  clear Wk;
  Wk.MatchKbn = '0';
  Wk.HoldReason = '00';
  Wk.SetlKbn = 'H';
  Wk.FeeKbn = '00';
  Wk.DispKbn = 'S';
  Wk.CaptureAmt = pIn.CaptureAmt;
  wkToday = %date();
  wkNow = %timestamp();

endsr;

//--------------------------------------------------------------------
//  入力チェック
//--------------------------------------------------------------------
begsr CheckInput;

  if pIn.ReqId = *blank
     or pIn.MerchantNo = *blank
     or pIn.ChannelKbn = *blank
     or pIn.TxnKbn = *blank
     or Wk.CaptureAmt <= 0;

    pOut.ResultCd = '201';
    pOut.MatchKbn = '9';
    pOut.HoldReason = '90';
    pOut.SetlKbn = 'H';
    pOut.Message = '捕捉要求項目不正';
    Wk.LogMsg = pOut.Message;
    exsr WriteLog;
    return;

  endif;

  select;
  when pIn.ChannelKbn = '01'
    or pIn.ChannelKbn = '02'
    or pIn.ChannelKbn = '03'
    or pIn.ChannelKbn = '04'
    or pIn.ChannelKbn = '05';
  other;
    pOut.ResultCd = '202';
    pOut.MatchKbn = '9';
    pOut.HoldReason = '91';
    pOut.SetlKbn = 'H';
    pOut.Message = 'チャネル区分不正';
    Wk.LogMsg = pOut.Message;
    exsr WriteLog;
    return;
  endsl;

  select;
  when pIn.TxnKbn = 'P1' or pIn.TxnKbn = 'P2';
    Wk.DispKbn = 'S';
  when pIn.TxnKbn = 'C1' or pIn.TxnKbn = 'C2';
    Wk.DispKbn = 'K';
  when pIn.TxnKbn = 'A1';
    Wk.DispKbn = 'S';
  other;
    pOut.ResultCd = '203';
    pOut.MatchKbn = '9';
    pOut.HoldReason = '92';
    pOut.SetlKbn = 'H';
    pOut.Message = '取引区分不正';
    Wk.LogMsg = pOut.Message;
    exsr WriteLog;
    return;
  endsl;

  if pIn.AuthId = *blank and pIn.TxnId = *blank;
    pOut.ResultCd = '204';
    pOut.MatchKbn = '9';
    pOut.HoldReason = '93';
    pOut.SetlKbn = 'H';
    pOut.Message = '照合キー未指定';
    Wk.LogMsg = pOut.Message;
    exsr WriteLog;
    return;
  endif;

endsr;

//--------------------------------------------------------------------
//  承認照合
//--------------------------------------------------------------------
begsr ReadAuthorization;

  if pIn.AuthId <> *blank;

    chain (pIn.AuthId) CDAUTHF;
    if %found(CDAUTHF);
      Wk.FoundAuth = *on;
    endif;

  endif;

  if not Wk.FoundAuth and pIn.TxnId <> *blank;

    chain (pIn.TxnId) CDTXNF;
    if %found(CDTXNF);
      Wk.FoundTxn = *on;
      chain (TXN_AUTH_ID) CDAUTHF;
      if %found(CDAUTHF);
        Wk.FoundAuth = *on;
      endif;
    endif;

  endif;

  if Wk.FoundAuth;

    Wk.AuthAmt = AUTH_APPROVE_AMT;
    Wk.AlreadyCapAmt = AUTH_CAPTURED_AMT;
    Wk.RemainAmt = Wk.AuthAmt - Wk.AlreadyCapAmt;
    Wk.AuthLimitDate = AUTH_DATE + %days(AUTH_CAPTURE_DAYS);

    pOut.AuthId = AUTH_AUTH_ID;
    pOut.TxnId = AUTH_TXN_ID;
    pOut.ApprovedAmt = Wk.AuthAmt;
    pOut.CapturedAmt = Wk.AlreadyCapAmt;
    pOut.RemainAmt = Wk.RemainAmt;

  endif;

endsr;

//--------------------------------------------------------------------
//  捕捉判定
//--------------------------------------------------------------------
begsr DecideCapture;

  Wk.IsValidTxn = *off;
  Wk.IsSameShop = *off;
  Wk.IsExpired = *off;
  Wk.IsAmtOver = *off;
  Wk.IsPartial = *off;

  if AUTH_MERCHANT_NO = pIn.MerchantNo;
    Wk.IsSameShop = *on;
  endif;

  if AUTH_TXN_KBN = pIn.TxnKbn;
    Wk.IsValidTxn = *on;
  endif;

  if pIn.CaptureDate > Wk.AuthLimitDate;
    Wk.IsExpired = *on;
  endif;

  if Wk.CaptureAmt > Wk.RemainAmt;
    Wk.IsAmtOver = *on;
  endif;

  if Wk.CaptureAmt < Wk.RemainAmt;
    Wk.IsPartial = *on;
  endif;

  select;

  when AUTH_CANCEL_KBN = '1';
    Wk.MatchKbn = '9';
    Wk.HoldReason = '11';
    Wk.SetlKbn = 'H';
    Wk.LogMsg = '承認取消済：捕捉保留';

  when AUTH_APPROVE_KBN <> '1';
    Wk.MatchKbn = '9';
    Wk.HoldReason = '12';
    Wk.SetlKbn = 'H';
    Wk.LogMsg = '未承認取引：捕捉保留';

  when not Wk.IsSameShop;
    Wk.MatchKbn = '9';
    Wk.HoldReason = '21';
    Wk.SetlKbn = 'H';
    Wk.LogMsg = '加盟店相違：捕捉保留';

  when not Wk.IsValidTxn;
    Wk.MatchKbn = '9';
    Wk.HoldReason = '22';
    Wk.SetlKbn = 'H';
    Wk.LogMsg = '取引区分相違：捕捉保留';

  when Wk.IsExpired;
    Wk.MatchKbn = '7';
    Wk.HoldReason = '31';
    Wk.SetlKbn = 'H';
    Wk.LogMsg = '捕捉期限超過：保留';

  when Wk.IsAmtOver;
    Wk.MatchKbn = '8';
    Wk.HoldReason = '41';
    Wk.SetlKbn = 'H';
    Wk.LogMsg = '承認残額超過：差額保留';

  other;
    if Wk.IsPartial;
      Wk.MatchKbn = '2';
      Wk.HoldReason = '00';
      Wk.SetlKbn = 'D';
      Wk.LogMsg = '部分売上捕捉：候補出力';
    else;
      Wk.MatchKbn = '1';
      Wk.HoldReason = '00';
      Wk.SetlKbn = 'D';
      Wk.LogMsg = '売上捕捉：候補出力';
    endif;

  endsl;

  // 手数料区分はオーソリ時に確定済みの値を踏襲する。捕捉処理では
  // 海外加盟店売上の国際ブランド立替手数料のみ補完する。
  select;
  when pIn.ChannelKbn = '05' or pIn.TxnKbn = 'P2';
    Wk.FeeKbn = 'FB';
  other;
    Wk.FeeKbn = '00';
  endsl;

  pOut.MatchKbn = Wk.MatchKbn;
  pOut.HoldReason = Wk.HoldReason;
  pOut.SetlKbn = Wk.SetlKbn;
  pOut.FeeKbn = Wk.FeeKbn;
  pOut.DispKbn = Wk.DispKbn;
  pOut.RemainAmt = Wk.RemainAmt - Wk.CaptureAmt;
  pOut.Message = Wk.LogMsg;

endsr;

//--------------------------------------------------------------------
//  捕捉候補出力
//--------------------------------------------------------------------
begsr WriteCapture;

  clear CAPR;

  CAP_REQ_ID = pIn.ReqId;
  CAP_AUTH_ID = AUTH_AUTH_ID;
  CAP_TXN_ID = AUTH_TXN_ID;
  CAP_MEMBER_NO = AUTH_MEMBER_NO;
  CAP_MERCHANT_NO = pIn.MerchantNo;
  CAP_TERMINAL_ID = pIn.TerminalId;
  CAP_CHANNEL_KBN = pIn.ChannelKbn;
  CAP_TXN_KBN = pIn.TxnKbn;
  CAP_DISP_KBN = Wk.DispKbn;
  CAP_CAPTURE_AMT = Wk.CaptureAmt;
  CAP_APPROVE_AMT = Wk.AuthAmt;
  CAP_CAPTURED_AMT = Wk.AlreadyCapAmt;
  CAP_REMAIN_AMT = Wk.RemainAmt - Wk.CaptureAmt;
  CAP_CURRENCY_CD = pIn.CurrencyCd;
  CAP_FEE_KBN = Wk.FeeKbn;
  CAP_SETL_KBN = Wk.SetlKbn;
  CAP_MATCH_KBN = Wk.MatchKbn;
  CAP_HOLD_REASON = Wk.HoldReason;
  CAP_CAPTURE_DATE = pIn.CaptureDate;
  CAP_ENTRY_DATE = wkToday;
  CAP_ENTRY_TIME = %time(wkNow);
  CAP_OPERATOR_ID = pIn.OperatorId;

  wkRetry = 0;

  dou wkRetry >= wkMaxRetry;
    monitor;
      write CAPR;
      leave;
    on-error;
      wkRetry += 1;
      if wkRetry >= wkMaxRetry;
        pOut.ResultCd = '901';
        pOut.MatchKbn = '9';
        pOut.HoldReason = '98';
        pOut.SetlKbn = 'H';
        pOut.Message = '捕捉候補出力失敗';
        Wk.LogMsg = pOut.Message;
      endif;
    endmon;
  enddo;

  if Wk.SetlKbn = 'D' and pOut.ResultCd = '000';
    AUTH_CAPTURED_AMT += Wk.CaptureAmt;
    AUTH_LAST_CAP_DATE = pIn.CaptureDate;
    AUTH_UPDATE_DATE = wkToday;
    AUTH_UPDATE_TIME = %time(wkNow);
    update AUTHR;
  endif;

endsr;

//--------------------------------------------------------------------
//  監査ログ出力
//--------------------------------------------------------------------
begsr WriteLog;

  monitor;

    clear LOGR;

    LOG_REQ_ID = pIn.ReqId;
    LOG_PROGRAM_ID = 'CA115R';
    LOG_MEMBER_NO = AUTH_MEMBER_NO;
    LOG_AUTH_ID = pOut.AuthId;
    LOG_TXN_ID = pOut.TxnId;
    LOG_MERCHANT_NO = pIn.MerchantNo;
    LOG_CHANNEL_KBN = pIn.ChannelKbn;
    LOG_TXN_KBN = pIn.TxnKbn;
    LOG_CAPTURE_AMT = Wk.CaptureAmt;
    LOG_RESULT_CD = pOut.ResultCd;
    LOG_MATCH_KBN = Wk.MatchKbn;
    LOG_HOLD_REASON = Wk.HoldReason;
    LOG_SETL_KBN = Wk.SetlKbn;
    LOG_OPERATOR_ID = pIn.OperatorId;
    LOG_MESSAGE = Wk.LogMsg;
    LOG_ENTRY_DATE = wkToday;
    LOG_ENTRY_TIME = %time(wkNow);

    write LOGR;

    callp CA113R('15'
                :LOG_MEMBER_NO
                :LOG_AUTH_ID
                :LOG_TXN_ID
                :LOG_RESULT_CD
                :LOG_MESSAGE);

  on-error;
    // 監査ログ出力失敗はオンライン応答を止めない
  endmon;

endsr;
