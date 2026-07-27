**free
//**********************************************************************
//  変更履歴
//  版数  年月日      担当     概要
//  1.00  2023/10/02  高橋     初版作成
//  1.01  2024/02/19  森       理由コード再分類時の履歴出力を追加
//  1.02  2024/09/06  高橋     オーソリ保留解除判定を追加
//**********************************************************************
ctl-opt dftactgrp(*no) actgrp('CA260R')
        option(*srcstmt:*nodebugio)
        alwnull(*usrctl)
        bnddir('QC2LE')
        datfmt(*iso) timfmt(*iso);

//**********************************************************************
//  みらいカード  消込例外補正画面
//  例外ID指定による入金明細・残高・消込結果の横並び確認
//  金額の直接配賦は行わず、業務状態のみ更新する
//**********************************************************************

/copy CDEXCPF2C
/copy CDPAYFC
/copy CDAPPFC
/copy CDHISTC

dcl-pr PaymentExceptionService extpgm('PAYEXSVC');
  peMode            char(2)   const;
  peExceptionId     packed(11:0) const;
  peReasonCode      char(2)   const;
  peHoldRelease     char(1)   const;
  peRerunAllow      char(1)   const;
  peOperator        char(10)  const;
  peMessageId       char(7);
  peMessageText     char(80);
end-pr;

dcl-pi *n;
  inExceptionId     packed(11:0) const;
  inOperator        char(10)     const;
  inTerminal        char(10)     const;
  inFunction        char(2)      const;
  outReturnCode     char(1);
  outMessageId      char(7);
end-pi;

dcl-ds Screen qualified;
  ExceptionId       packed(11:0);
  CustomerNo        packed(11:0);
  CardNoMask        char(19);
  PayMethod         char(2);
  PayMethodName     char(12);
  PayDate           date;
  PayAmount         packed(13:0);
  AppStatus         char(1);
  AppStatusName     char(12);
  BalanceBefore     packed(13:0);
  AppliedAmount     packed(13:0);
  BalanceAfter      packed(13:0);
  AuthHoldAmount    packed(13:0);
  ReasonCode        char(2);
  ReasonName        char(24);
  RerunAllow        char(1);
  HoldRelease       char(1);
  CompleteMark      char(1);
  InputReasonCode   char(2);
  InputComplete     char(1);
  MessageId         char(7);
  MessageText       char(80);
end-ds;

dcl-ds Work qualified;
  FoundException    ind inz(*off);
  FoundPayment      ind inz(*off);
  FoundApplication  ind inz(*off);
  Changed           ind inz(*off);
  Today             date;
  NowTime           time;
  ServiceMsgId      char(7);
  ServiceMsgText    char(80);
  DiffAmount        packed(13:0);
  ReclassAllowed    ind inz(*off);
  CompleteAllowed   ind inz(*off);
  HoldAllowed       ind inz(*off);
  AuthDecision      char(1);
  AuthReason        char(2);
end-ds;

dcl-s ModeInquiry    char(2) inz('IN');
dcl-s ModeUpdate     char(2) inz('UP');
dcl-s ModeComplete   char(2) inz('CP');

dcl-c PayMethodDebit '10';
dcl-c PayMethodBank  '20';
dcl-c PayMethodCvs   '30';

dcl-c AppStatusFull  'F';
dcl-c AppStatusPart  'P';
dcl-c AppStatusOver  'O';
dcl-c AppStatusSkip  'S';

dcl-c FuncInquiry    '01';
dcl-c FuncReclass    '02';
dcl-c FuncComplete   '03';

dcl-c AuthApprove    'A';
dcl-c AuthDecline    'D';
dcl-c AuthRefer      'R';

dcl-c ReasonUnknown  '00';
dcl-c ReasonShort    '11';
dcl-c ReasonOver     '12';
dcl-c ReasonDup      '21';
dcl-c ReasonName     '31';
dcl-c ReasonAuthHold '41';
dcl-c ReasonManual   '90';

dcl-s idx            int(10);
dcl-s wkMsg          char(80);

//**********************************************************************
//  主処理
//**********************************************************************

outReturnCode = '9';
outMessageId  = *blanks;

clear Screen;
clear Work;

Work.Today = %date();
Work.NowTime = %time();

Screen.ExceptionId = inExceptionId;

monitor;
  exsr ValidateEntry;
  if outReturnCode <> '0';
    *inlr = *on;
    return;
  endif;

  exsr LoadException;
  if not Work.FoundException;
    outReturnCode = '1';
    outMessageId  = 'CA26001';
    Screen.MessageId = outMessageId;
    Screen.MessageText = '例外IDが存在しません';
    *inlr = *on;
    return;
  endif;

  exsr LoadPayment;
  exsr LoadApplication;
  exsr BuildScreen;
  exsr JudgeBusinessState;

  select;
  when inFunction = FuncInquiry;
    outReturnCode = '0';
    outMessageId  = 'CA26000';

  when inFunction = FuncReclass;
    exsr ReclassReason;

  when inFunction = FuncComplete;
    exsr CompleteInvestigation;

  other;
    outReturnCode = '1';
    outMessageId  = 'CA26002';
    Screen.MessageId = outMessageId;
    Screen.MessageText = '機能区分が不正です';
  endsl;

on-error;
  outReturnCode = '9';
  outMessageId  = 'CA26099';
  Screen.MessageId = outMessageId;
  Screen.MessageText = '消込例外補正で異常が発生しました';
endmon;

*inlr = *on;
return;

//**********************************************************************
//  入力検査
//**********************************************************************
begsr ValidateEntry;

  if inExceptionId <= 0;
    outReturnCode = '1';
    outMessageId  = 'CA26003';
    Screen.MessageId = outMessageId;
    Screen.MessageText = '例外IDを指定してください';
    leavesr;
  endif;

  if %trim(inOperator) = *blanks;
    outReturnCode = '1';
    outMessageId  = 'CA26004';
    Screen.MessageId = outMessageId;
    Screen.MessageText = '担当者が未設定です';
    leavesr;
  endif;

  if inFunction <> FuncInquiry
     and inFunction <> FuncReclass
     and inFunction <> FuncComplete;
    outReturnCode = '1';
    outMessageId  = 'CA26002';
    Screen.MessageId = outMessageId;
    Screen.MessageText = '機能区分が不正です';
    leavesr;
  endif;

  outReturnCode = '0';
  outMessageId  = *blanks;

endsr;

//**********************************************************************
//  例外情報取得
//**********************************************************************
begsr LoadException;

  Work.FoundException = *off;

  chain inExceptionId CDEXCPF2;
  if %found(CDEXCPF2);
    Work.FoundException = *on;

    Screen.CustomerNo      = EX_CUST_NO;
    Screen.CardNoMask      = EX_CARD_MASK;
    Screen.ReasonCode      = EX_REASON_CD;
    Screen.RerunAllow      = EX_RERUN_FLG;
    Screen.HoldRelease     = EX_HOLD_REL_FLG;
    Screen.CompleteMark    = EX_COMPLETE_FLG;
    Screen.InputReasonCode = EX_REASON_CD;
    Screen.InputComplete   = EX_COMPLETE_FLG;
  endif;

endsr;

//**********************************************************************
//  入金明細取得
//**********************************************************************
begsr LoadPayment;

  Work.FoundPayment = *off;

  setll inExceptionId CDPAYF;
  dou %eof(CDPAYF);
    reade inExceptionId CDPAYF;
    if %eof(CDPAYF);
      leave;
    endif;

    if PY_EXCEPTION_ID = inExceptionId;
      Work.FoundPayment = *on;

      Screen.PayMethod = PY_PAY_METHOD;
      Screen.PayDate   = PY_PAY_DATE;
      Screen.PayAmount = PY_PAY_AMOUNT;
      leave;
    endif;
  enddo;

endsr;

//**********************************************************************
//  消込結果取得
//**********************************************************************
begsr LoadApplication;

  Work.FoundApplication = *off;

  setll inExceptionId CDAPPF;
  dou %eof(CDAPPF);
    reade inExceptionId CDAPPF;
    if %eof(CDAPPF);
      leave;
    endif;

    if AP_EXCEPTION_ID = inExceptionId;
      Work.FoundApplication = *on;

      Screen.AppStatus     = AP_APP_STATUS;
      Screen.BalanceBefore = AP_BAL_BEFORE;
      Screen.AppliedAmount = AP_APPLIED_AMT;
      Screen.BalanceAfter  = AP_BAL_AFTER;
      Screen.AuthHoldAmount = AP_AUTH_HOLD_AMT;
      leave;
    endif;
  enddo;

endsr;

//**********************************************************************
//  画面項目編集
//**********************************************************************
begsr BuildScreen;

  select;
  when Screen.PayMethod = PayMethodDebit;
    Screen.PayMethodName = '口座振替';
  when Screen.PayMethod = PayMethodBank;
    Screen.PayMethodName = '振込';
  when Screen.PayMethod = PayMethodCvs;
    Screen.PayMethodName = 'コンビニ';
  other;
    Screen.PayMethodName = '不明';
  endsl;

  select;
  when Screen.AppStatus = AppStatusFull;
    Screen.AppStatusName = '完済';
  when Screen.AppStatus = AppStatusPart;
    Screen.AppStatusName = '一部消込';
  when Screen.AppStatus = AppStatusOver;
    Screen.AppStatusName = '過入金';
  when Screen.AppStatus = AppStatusSkip;
    Screen.AppStatusName = '対象外';
  other;
    Screen.AppStatusName = '未判定';
  endsl;

  select;
  when Screen.ReasonCode = ReasonUnknown;
    Screen.ReasonName = '理由未確定';
  when Screen.ReasonCode = ReasonShort;
    Screen.ReasonName = '入金不足';
  when Screen.ReasonCode = ReasonOver;
    Screen.ReasonName = '過入金';
  when Screen.ReasonCode = ReasonDup;
    Screen.ReasonName = '重複入金疑い';
  when Screen.ReasonCode = ReasonName;
    Screen.ReasonName = '名義相違';
  when Screen.ReasonCode = ReasonAuthHold;
    Screen.ReasonName = 'オーソリ保留';
  when Screen.ReasonCode = ReasonManual;
    Screen.ReasonName = '調査対象';
  other;
    Screen.ReasonName = '未登録理由';
  endsl;

  Work.DiffAmount = Screen.PayAmount - Screen.AppliedAmount;

endsr;

//**********************************************************************
//  業務状態判定
//**********************************************************************
begsr JudgeBusinessState;

  Work.ReclassAllowed  = *off;
  Work.CompleteAllowed = *off;
  Work.HoldAllowed     = *off;
  Work.AuthDecision    = AuthRefer;
  Work.AuthReason      = ReasonManual;

  if Screen.CompleteMark = '1';
    leavesr;
  endif;

  if not Work.FoundPayment or not Work.FoundApplication;
    Work.ReclassAllowed = *on;
    Work.AuthDecision   = AuthRefer;
    Work.AuthReason     = ReasonUnknown;
    leavesr;
  endif;

  select;
  when Screen.AppStatus = AppStatusFull;
    if Work.DiffAmount = 0;
      Work.CompleteAllowed = *on;
      Work.AuthDecision    = AuthApprove;
      Work.AuthReason      = ReasonUnknown;
    else;
      Work.ReclassAllowed = *on;
      Work.AuthDecision   = AuthRefer;
      Work.AuthReason     = ReasonManual;
    endif;

  when Screen.AppStatus = AppStatusPart;
    Work.ReclassAllowed = *on;
    Work.AuthDecision   = AuthRefer;
    Work.AuthReason     = ReasonShort;

  when Screen.AppStatus = AppStatusOver;
    Work.ReclassAllowed = *on;
    Work.CompleteAllowed = *on;
    Work.AuthDecision   = AuthRefer;
    Work.AuthReason     = ReasonOver;

  when Screen.AppStatus = AppStatusSkip;
    Work.ReclassAllowed = *on;
    Work.AuthDecision   = AuthDecline;
    Work.AuthReason     = ReasonManual;

  other;
    Work.ReclassAllowed = *on;
    Work.AuthDecision   = AuthRefer;
    Work.AuthReason     = ReasonUnknown;
  endsl;

  if Screen.AuthHoldAmount > 0
     and Screen.BalanceAfter <= 0
     and Screen.AppStatus = AppStatusFull;
    Work.HoldAllowed = *on;
  endif;

  if Screen.PayMethod = PayMethodDebit
     and Screen.PayDate = Work.Today
     and Screen.AppStatus <> AppStatusFull;
    Work.CompleteAllowed = *off;
  endif;

endsr;

//**********************************************************************
//  理由コード再分類
//**********************************************************************
begsr ReclassReason;

  if not Work.ReclassAllowed;
    outReturnCode = '1';
    outMessageId  = 'CA26005';
    Screen.MessageId = outMessageId;
    Screen.MessageText = '再分類できない状態です';
    leavesr;
  endif;

  if Screen.InputReasonCode = *blanks
     or Screen.InputReasonCode = ReasonUnknown;
    outReturnCode = '1';
    outMessageId  = 'CA26006';
    Screen.MessageId = outMessageId;
    Screen.MessageText = '再分類理由を指定してください';
    leavesr;
  endif;

  select;
  when Screen.InputReasonCode = ReasonShort;
    if Screen.AppStatus <> AppStatusPart;
      outReturnCode = '1';
      outMessageId  = 'CA26007';
      Screen.MessageText = '入金不足は一部消込のみ指定可能です';
      leavesr;
    endif;

  when Screen.InputReasonCode = ReasonOver;
    if Screen.AppStatus <> AppStatusOver;
      outReturnCode = '1';
      outMessageId  = 'CA26008';
      Screen.MessageText = '過入金は過入金状態のみ指定可能です';
      leavesr;
    endif;

  when Screen.InputReasonCode = ReasonDup
       or Screen.InputReasonCode = ReasonName
       or Screen.InputReasonCode = ReasonAuthHold
       or Screen.InputReasonCode = ReasonManual;
    // 個別調査理由

  other;
    outReturnCode = '1';
    outMessageId  = 'CA26009';
    Screen.MessageText = '理由コードが不正です';
    leavesr;
  endsl;

  if Work.HoldAllowed and Screen.InputReasonCode = ReasonAuthHold;
    Screen.HoldRelease = '1';
  else;
    Screen.HoldRelease = '0';
  endif;

  if Screen.AppStatus = AppStatusPart
     or Screen.AppStatus = AppStatusOver
     or Screen.InputReasonCode = ReasonDup
     or Screen.InputReasonCode = ReasonName;
    Screen.RerunAllow = '1';
  else;
    Screen.RerunAllow = '0';
  endif;

  monitor;
    callp PaymentExceptionService(ModeUpdate:
                                  Screen.ExceptionId:
                                  Screen.InputReasonCode:
                                  Screen.HoldRelease:
                                  Screen.RerunAllow:
                                  inOperator:
                                  Work.ServiceMsgId:
                                  Work.ServiceMsgText);

    if Work.ServiceMsgId <> *blanks;
      outReturnCode = '1';
      outMessageId  = Work.ServiceMsgId;
      Screen.MessageId = Work.ServiceMsgId;
      Screen.MessageText = Work.ServiceMsgText;
      leavesr;
    endif;

    exsr WriteHistory;

    outReturnCode = '0';
    outMessageId  = 'CA26010';
    Screen.MessageId = outMessageId;
    Screen.MessageText = '理由コードを再分類しました';

  on-error;
    outReturnCode = '9';
    outMessageId  = 'CA26098';
    Screen.MessageId = outMessageId;
    Screen.MessageText = '再分類登録で異常が発生しました';
  endmon;

endsr;

//**********************************************************************
//  調査完了
//**********************************************************************
begsr CompleteInvestigation;

  if not Work.CompleteAllowed;
    outReturnCode = '1';
    outMessageId  = 'CA26011';
    Screen.MessageId = outMessageId;
    Screen.MessageText = '調査完了にできない状態です';
    leavesr;
  endif;

  if Screen.RerunAllow = '1';
    outReturnCode = '1';
    outMessageId  = 'CA26012';
    Screen.MessageId = outMessageId;
    Screen.MessageText = '再実行対象は完了登録できません';
    leavesr;
  endif;

  if Screen.AuthHoldAmount > 0 and not Work.HoldAllowed;
    outReturnCode = '1';
    outMessageId  = 'CA26013';
    Screen.MessageId = outMessageId;
    Screen.MessageText = 'オーソリ保留が残っています';
    leavesr;
  endif;

  if Work.HoldAllowed;
    Screen.HoldRelease = '1';
  endif;

  monitor;
    callp PaymentExceptionService(ModeComplete:
                                  Screen.ExceptionId:
                                  Screen.ReasonCode:
                                  Screen.HoldRelease:
                                  '0':
                                  inOperator:
                                  Work.ServiceMsgId:
                                  Work.ServiceMsgText);

    if Work.ServiceMsgId <> *blanks;
      outReturnCode = '1';
      outMessageId  = Work.ServiceMsgId;
      Screen.MessageId = Work.ServiceMsgId;
      Screen.MessageText = Work.ServiceMsgText;
      leavesr;
    endif;

    Screen.CompleteMark = '1';
    exsr WriteHistory;

    outReturnCode = '0';
    outMessageId  = 'CA26020';
    Screen.MessageId = outMessageId;
    Screen.MessageText = '調査完了を登録しました';

  on-error;
    outReturnCode = '9';
    outMessageId  = 'CA26097';
    Screen.MessageId = outMessageId;
    Screen.MessageText = '完了登録で異常が発生しました';
  endmon;

endsr;

//**********************************************************************
//  履歴出力
//**********************************************************************
begsr WriteHistory;

  clear CDHISTR;

  HS_HIST_DATE     = Work.Today;
  HS_HIST_TIME     = Work.NowTime;
  HS_EXCEPTION_ID  = Screen.ExceptionId;
  HS_CUST_NO       = Screen.CustomerNo;
  HS_OPERATOR      = inOperator;
  HS_TERMINAL      = inTerminal;
  HS_BEFORE_REASON = Screen.ReasonCode;
  HS_AFTER_REASON  = Screen.InputReasonCode;
  HS_APP_STATUS    = Screen.AppStatus;
  HS_RERUN_FLG     = Screen.RerunAllow;
  HS_HOLD_REL_FLG  = Screen.HoldRelease;
  HS_COMPLETE_FLG  = Screen.CompleteMark;
  HS_AUTH_DECISION = Work.AuthDecision;
  HS_AUTH_REASON   = Work.AuthReason;

  select;
  when inFunction = FuncReclass;
    HS_ACTION_CD = 'RC';
    HS_ACTION_TEXT = '理由再分類';
  when inFunction = FuncComplete;
    HS_ACTION_CD = 'CP';
    HS_ACTION_TEXT = '調査完了';
  other;
    HS_ACTION_CD = 'IN';
    HS_ACTION_TEXT = '照会';
  endsl;

  write CDHISTR;

endsr;
