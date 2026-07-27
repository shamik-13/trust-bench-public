**free
//**********************************************************************
// 変更履歴
// 版数  年月日    担当     概要
// 0001  20240517  佐藤     初版作成
// 0002  20240703  中村     返金済明細の受付抑止追加
// 0003  20240912  佐藤     取消理由監査サブ呼出追加
//**********************************************************************
ctl-opt dftactgrp(*no) actgrp(*caller)
        option(*srcstmt:*nodebugio)
        datfmt(*iso) timfmt(*iso)
        alwnull(*usrctl);

//---------------------------------------------------------------------
//  入金取消受付画面  CA104R
//  PAY-ID指定により元入金・消込結果を照会し、権限者のみ取消受付を行う
//---------------------------------------------------------------------

/copy CDPAYFC
/copy CDAPPFC
/copy CDCANC

dcl-pr CB171S extpgm('CB171S');
  pUserId        char(10) const;
  pPayId         packed(15:0) const;
  pReason        char(2) const;
  pCancelAmt     packed(13:0) const;
  pAppNo         packed(12:0) const;
  pAuditKbn      char(1);
  pRc            char(2);
  pMsg           char(80);
end-pr;

dcl-pi *n;
  iTermId        char(10) const;
  iUserId        char(10) const;
  iAuthKbn       char(1)  const;
  iPayId         packed(15:0) const;
  oReturnCd      char(2);
  oMessage       char(80);
end-pi;

dcl-ds Scr qualified inz;
  PayId          packed(15:0);
  CardNoMask     char(19);
  MemberNo       packed(11:0);
  PayMethod      char(2);
  PayMethodNm    char(12);
  PayDate        date;
  PayAmt         packed(13:0);
  AppNo          packed(12:0);
  AppStatus      char(1);
  AppStatusNm    char(12);
  AppliedAmt     packed(13:0);
  RemainAmt      packed(13:0);
  RefundFlg      char(1);
  LimitDate      date;
  ReasonCd       char(2);
  CancelAmt      packed(13:0);
  MsgId          char(7);
  MsgText        char(80);
end-ds;

dcl-ds Work qualified inz;
  Today          date;
  TimeNow        time;
  FoundPay       ind;
  FoundApp       ind;
  CanEntry       ind;
  AuditKbn       char(1);
  SubRc          char(2);
  SubMsg         char(80);
  Err            ind;
  RetryCnt       packed(3:0);
  MaxCancelAmt   packed(13:0);
end-ds;

dcl-ds LogKey qualified inz;
  PayId          packed(15:0);
  AppNo          packed(12:0);
  SeqNo          packed(5:0);
end-ds;

dcl-s wkReasonOk     ind inz(*off);
dcl-s wkOverLimit    ind inz(*off);
dcl-s wkInputEnd     ind inz(*off);
dcl-s wkConfirm      char(1) inz('0');
dcl-s wkMsg          char(80) inz(*blank);
dcl-s wkRc           char(2) inz('00');

//---------------------------------------------------------------------
// 主処理
//---------------------------------------------------------------------
eval oReturnCd = '00';
eval oMessage  = *blank;

eval Work.Today = %date();
eval Work.TimeNow = %time();
eval Scr.PayId = iPayId;

exsr InitScreen;
exsr ReadPayment;
exsr ReadApply;

if not Work.FoundPay;
  eval oReturnCd = '11';
  eval oMessage  = 'PAY-IDが存在しません';
  *inlr = *on;
  return;
endif;

if not Work.FoundApp;
  eval oReturnCd = '12';
  eval oMessage  = '消込結果が存在しません';
  *inlr = *on;
  return;
endif;

exsr EditDisplay;
exsr CheckEntryBase;

if not Work.CanEntry;
  eval oReturnCd = '21';
  eval oMessage  = Scr.MsgText;
  *inlr = *on;
  return;
endif;

// 画面入力相当。ベンチ用の呼出パラメータはPAY-IDのみのため、
// 理由・金額は既存受付域の初期値を使用する。
dow not wkInputEnd;
  exsr AcceptInput;
  exsr ValidateInput;

  if Work.Err;
    eval oReturnCd = '31';
    eval oMessage  = Scr.MsgText;
    leave;
  endif;

  exsr CallReasonAudit;

  if Work.SubRc <> '00';
    eval oReturnCd = Work.SubRc;
    eval oMessage  = Work.SubMsg;
    leave;
  endif;

  exsr RegisterCancel;
  eval wkInputEnd = *on;
enddo;

if oReturnCd = '00';
  eval oMessage = '取消受付を登録しました';
endif;

*inlr = *on;
return;

//---------------------------------------------------------------------
// 初期化
//---------------------------------------------------------------------
begsr InitScreen;
  clear Scr;
  clear Work.Err;
  eval Work.FoundPay = *off;
  eval Work.FoundApp = *off;
  eval Work.CanEntry = *off;
  eval Work.AuditKbn = '0';
  eval Work.SubRc = '00';
  eval Work.SubMsg = *blank;
  eval Work.MaxCancelAmt = 0;
  eval Scr.PayId = iPayId;
  eval Scr.ReasonCd = *blank;
  eval Scr.CancelAmt = 0;
endsr;

//---------------------------------------------------------------------
// 入金読込
//---------------------------------------------------------------------
begsr ReadPayment;
  monitor;
    chain iPayId CDPAYR;
    if %found(CDPAYR);
      eval Work.FoundPay = *on;
      eval Scr.PayId      = PY_PAY_ID;
      eval Scr.MemberNo   = PY_MEMBER_NO;
      eval Scr.PayMethod  = PY_PAY_METHOD;
      eval Scr.PayDate    = PY_PAY_DATE;
      eval Scr.PayAmt     = PY_PAY_AMT;
      eval Scr.RefundFlg  = PY_REFUND_FLG;
      eval Scr.LimitDate  = PY_CANCEL_LIMIT;
      eval Scr.CardNoMask = %subst(PY_CARD_NO:1:6) + '******' +
                            %subst(PY_CARD_NO:13:4);
    endif;
  on-error;
    eval Work.FoundPay = *off;
    eval Scr.MsgText = '入金ファイル読込でエラーが発生しました';
  endmon;
endsr;

//---------------------------------------------------------------------
// 消込結果読込
//---------------------------------------------------------------------
begsr ReadApply;
  if not Work.FoundPay;
    leavesr;
  endif;

  monitor;
    setll iPayId CDAPPR;
    reade iPayId CDAPPR;
    dow not %eof(CDAPPR);
      if AP_PAY_ID = iPayId
         and (AP_APP_STATUS = 'F'
          or  AP_APP_STATUS = 'P'
          or  AP_APP_STATUS = 'O'
          or  AP_APP_STATUS = 'S');

        eval Work.FoundApp = *on;
        eval Scr.AppNo      = AP_APP_NO;
        eval Scr.AppStatus  = AP_APP_STATUS;
        eval Scr.AppliedAmt = AP_APPLIED_AMT;
        leave;
      endif;

      reade iPayId CDAPPR;
    enddo;
  on-error;
    eval Work.FoundApp = *off;
    eval Scr.MsgText = '消込結果ファイル読込でエラーが発生しました';
  endmon;
endsr;

//---------------------------------------------------------------------
// 表示編集
//---------------------------------------------------------------------
begsr EditDisplay;
  select;
  when Scr.PayMethod = '10';
    eval Scr.PayMethodNm = '口座振替';
  when Scr.PayMethod = '20';
    eval Scr.PayMethodNm = '振込';
  when Scr.PayMethod = '30';
    eval Scr.PayMethodNm = 'コンビニ';
  other;
    eval Scr.PayMethodNm = '不明';
  endsl;

  select;
  when Scr.AppStatus = 'F';
    eval Scr.AppStatusNm = '完済';
  when Scr.AppStatus = 'P';
    eval Scr.AppStatusNm = '一部消込';
  when Scr.AppStatus = 'O';
    eval Scr.AppStatusNm = '過入金';
  when Scr.AppStatus = 'S';
    eval Scr.AppStatusNm = '対象外';
  other;
    eval Scr.AppStatusNm = '未確定';
  endsl;

  eval Scr.RemainAmt = Scr.PayAmt - Scr.AppliedAmt;

  if Scr.RemainAmt < 0;
    eval Scr.RemainAmt = 0;
  endif;

  eval Work.MaxCancelAmt = Scr.PayAmt - Scr.RemainAmt;
endsr;

//---------------------------------------------------------------------
// 受付可否基本チェック
//---------------------------------------------------------------------
begsr CheckEntryBase;
  eval Work.CanEntry = *off;
  eval Scr.MsgText = *blank;

  if iAuthKbn <> '2' and iAuthKbn <> '9';
    eval Scr.MsgText = '権限者以外は取消受付できません';
    leavesr;
  endif;

  if Scr.RefundFlg = '1';
    eval Scr.MsgText = '返金済みのため取消受付できません';
    leavesr;
  endif;

  if Scr.LimitDate < Work.Today;
    eval Scr.MsgText = '取消期限を超過しています';
    leavesr;
  endif;

  if Scr.AppStatus = 'S';
    eval Scr.MsgText = '対象外明細は取消受付できません';
    leavesr;
  endif;

  if Work.MaxCancelAmt <= 0;
    eval Scr.MsgText = '取消可能金額がありません';
    leavesr;
  endif;

  eval Work.CanEntry = *on;
endsr;

//---------------------------------------------------------------------
// 入力受付
//---------------------------------------------------------------------
begsr AcceptInput;
  // 実画面ではDSPF入力。ここでは受付前状態として未入力のまま検証する。
  if Scr.ReasonCd = *blank;
    eval Scr.ReasonCd = '00';
  endif;

  if Scr.CancelAmt = 0;
    eval Scr.CancelAmt = Work.MaxCancelAmt;
  endif;

  eval wkConfirm = '1';
endsr;

//---------------------------------------------------------------------
// 入力検証
//---------------------------------------------------------------------
begsr ValidateInput;
  eval Work.Err = *off;
  eval wkReasonOk = *off;
  eval wkOverLimit = *off;
  eval Scr.MsgText = *blank;

  select;
  when Scr.ReasonCd = '01';
    eval wkReasonOk = *on;
  when Scr.ReasonCd = '02';
    eval wkReasonOk = *on;
  when Scr.ReasonCd = '03';
    eval wkReasonOk = *on;
  when Scr.ReasonCd = '90';
    eval wkReasonOk = *on;
  other;
    eval wkReasonOk = *off;
  endsl;

  if not wkReasonOk;
    eval Work.Err = *on;
    eval Scr.MsgText = '取消理由を確認してください';
    leavesr;
  endif;

  if Scr.CancelAmt <= 0;
    eval Work.Err = *on;
    eval Scr.MsgText = '取消金額を入力してください';
    leavesr;
  endif;

  if Scr.CancelAmt > Work.MaxCancelAmt;
    eval wkOverLimit = *on;
  endif;

  if wkOverLimit;
    eval Work.Err = *on;
    eval Scr.MsgText = '取消金額が消込済金額を超えています';
    leavesr;
  endif;

  if wkConfirm <> '1';
    eval Work.Err = *on;
    eval Scr.MsgText = '登録確認が未完了です';
    leavesr;
  endif;
endsr;

//---------------------------------------------------------------------
// 取消理由監査サブ呼出
//---------------------------------------------------------------------
begsr CallReasonAudit;
  eval Work.SubRc = '00';
  eval Work.SubMsg = *blank;
  eval Work.AuditKbn = '0';

  monitor;
    callp CB171S(iUserId
               : Scr.PayId
               : Scr.ReasonCd
               : Scr.CancelAmt
               : Scr.AppNo
               : Work.AuditKbn
               : Work.SubRc
               : Work.SubMsg);
  on-error;
    eval Work.SubRc = '91';
    eval Work.SubMsg = '取消理由監査サブで異常が発生しました';
  endmon;

  if Work.SubRc = *blank;
    eval Work.SubRc = '00';
  endif;
endsr;

//---------------------------------------------------------------------
// 取消受付登録
//---------------------------------------------------------------------
begsr RegisterCancel;
  eval wkRc = '00';
  eval wkMsg = *blank;
  eval Work.RetryCnt = 0;

  dou Work.RetryCnt >= 3;
    monitor;
      eval Work.RetryCnt += 1;

      clear CDCANR;
      eval CN_PAY_ID       = Scr.PayId;
      eval CN_APP_NO       = Scr.AppNo;
      eval CN_CANCEL_SEQ   = Work.RetryCnt;
      eval CN_MEMBER_NO    = Scr.MemberNo;
      eval CN_PAY_METHOD   = Scr.PayMethod;
      eval CN_CANCEL_AMT   = Scr.CancelAmt;
      eval CN_REASON_CD    = Scr.ReasonCd;
      eval CN_AUDIT_KBN    = Work.AuditKbn;
      eval CN_ENTRY_USER   = iUserId;
      eval CN_ENTRY_TERM   = iTermId;
      eval CN_ENTRY_DATE   = Work.Today;
      eval CN_ENTRY_TIME   = Work.TimeNow;
      eval CN_STATUS       = '0';

      write CDCANR;
      leave;

    on-error;
      eval wkRc = '92';
      eval wkMsg = '取消受付登録でエラーが発生しました';
    endmon;
  enddo;

  if wkRc <> '00';
    eval oReturnCd = wkRc;
    eval oMessage = wkMsg;
  endif;
endsr;
