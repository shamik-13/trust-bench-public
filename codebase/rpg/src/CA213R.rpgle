**free
//**********************************************************************
//  変更履歴
//  版数  年月日    担当     概要
//  0001  20240115  MKC01    初版作成
//  0002  20240409  MKC03    否認理由短文コード変換追加
//  0003  20240621  MKC02    別カード応答および二重表示判定見直し
//**********************************************************************

ctl-opt dftactgrp(*no) actgrp(*caller)
        option(*srcstmt:*nodebugio)
        alwnull(*usrctl);

//**********************************************************************
//  CA213R  与信応答参照
//    AUTH-IDで直近応答を参照し、受付中与信との整合性を確認する。
//    表示済応答、別カード応答、取消・売確済ホールドは端末返却対象外。
//**********************************************************************

/copy QRPGLESRC,CDARSPFC
/copy QRPGLESRC,CDAUTHFC

dcl-pr CA213R extpgm('CA213R');
  pAuthId       char(12);
  pTermId       char(8);
  pOperId       char(10);
  pRtnCd        char(2);
  pMsgCd        char(4);
  pDecision     char(1);
  pShortReason  char(3);
  pApprNo       char(6);
  pRespText     char(40);
end-pr;

dcl-pi CA213R;
  pAuthId       char(12);
  pTermId       char(8);
  pOperId       char(10);
  pRtnCd        char(2);
  pMsgCd        char(4);
  pDecision     char(1);
  pShortReason  char(3);
  pApprNo       char(6);
  pRespText     char(40);
end-pi;

dcl-f CDARSPF keyed usage(*input:*update) extfile('CDARSPF')
             usropn rename(CDARSPR:RspRec);
dcl-f CDAUTHF keyed usage(*input) extfile('CDAUTHF')
             usropn rename(CDAUTHR:AuthRec);

dcl-ds Rsp qualified likerec(RspRec:*all) end-ds;
dcl-ds Auth qualified likerec(AuthRec:*input) end-ds;

dcl-ds Work qualified;
  AuthId          char(12);
  CardNo          char(16);
  MaskCardNo      char(16);
  TermId          char(8);
  OperId          char(10);
  NowDate         zoned(8:0);
  NowTime         zoned(6:0);
  BaseCur         char(3) inz('JPY');
  RetCd           char(2);
  MsgCd           char(4);
  RespText        char(40);
  ShortReason     char(3);
  Decision        char(1);
  ApprNo          char(6);
  FoundRsp        ind;
  FoundAuth       ind;
  Showable        ind;
  SameCard        ind;
  SameAmount      ind;
  SameMerchant    ind;
  AlreadyShown    ind;
  HoldOpen        ind;
  StsOk           ind;
  CurOk           ind;
  ReqAmt          packed(13:0);
end-ds;

dcl-s RetryCnt        packed(3:0) inz(0);
dcl-s MaxRetry        packed(3:0) inz(3);
dcl-s RspKeyAuth      char(12);
dcl-s AuthKeyAuth     char(12);
dcl-s ErrMsg          char(40);

*inlr = *on;

clear pRtnCd;
clear pMsgCd;
clear pDecision;
clear pShortReason;
clear pApprNo;
clear pRespText;

Work.AuthId = pAuthId;
Work.TermId = pTermId;
Work.OperId = pOperId;
Work.NowDate = %dec(%char(%date():*iso0):8:0);
Work.NowTime = %dec(%char(%time():*hms0):6:0);
Work.RetCd = '00';
Work.MsgCd = '0000';
Work.RespText = *blanks;

dou RetryCnt >= MaxRetry;

  RetryCnt += 1;

  monitor;

    if not %open(CDARSPF);
      open CDARSPF;
    endif;

    if not %open(CDAUTHF);
      open CDAUTHF;
    endif;

    exsr LoadAuth;
    exsr LoadLastRsp;
    exsr JudgeResponse;
    exsr SetReturn;

    leave;

  on-error;
    ErrMsg = 'CA213R ファイル入出力エラー';
    Work.RetCd = '90';
    Work.MsgCd = 'E900';
    Work.Decision = 'D';
    Work.ShortReason = 'STS';
    Work.ApprNo = *blanks;
    Work.RespText = 'オンライン照会不可';
    if RetryCnt >= MaxRetry;
      exsr SetReturn;
    endif;
  endmon;

enddo;

if %open(CDARSPF);
  close CDARSPF;
endif;

if %open(CDAUTHF);
  close CDAUTHF;
endif;

return;

//**********************************************************************
//  受付中与信取得
//**********************************************************************
begsr LoadAuth;

  clear Auth;
  Work.FoundAuth = *off;

  AuthKeyAuth = Work.AuthId;
  chain AuthKeyAuth AuthRec Auth;

  if %found(CDAUTHF);
    Work.FoundAuth = *on;
    Work.CardNo = AU_CARD_NO;
    Work.ReqAmt = AU_AUTH_AMT;
  else;
    clear Work.CardNo;
    clear Work.ReqAmt;
  endif;

endsr;

//**********************************************************************
//  直近応答取得
//**********************************************************************
begsr LoadLastRsp;

  clear Rsp;
  Work.FoundRsp = *off;

  RspKeyAuth = Work.AuthId;
  setll RspKeyAuth RspRec;

  dow not %eof(CDARSPF);

    reade RspKeyAuth RspRec Rsp;

    if %eof(CDARSPF);
      leave;
    endif;

    Work.FoundRsp = *on;

  enddo;

endsr;

//**********************************************************************
//  応答表示可否判定
//**********************************************************************
begsr JudgeResponse;

  clear Work.Showable;
  clear Work.SameCard;
  clear Work.SameAmount;
  clear Work.SameMerchant;
  clear Work.AlreadyShown;
  clear Work.HoldOpen;
  clear Work.StsOk;
  clear Work.CurOk;

  Work.Decision = 'D';
  Work.ShortReason = 'STS';
  Work.ApprNo = *blanks;
  Work.RespText = '対象応答なし';

  if not Work.FoundAuth;
    Work.RetCd = '04';
    Work.MsgCd = 'A404';
    Work.RespText = '受付中与信なし';
    leavesr;
  endif;

  if not Work.FoundRsp;
    Work.RetCd = '04';
    Work.MsgCd = 'R404';
    Work.RespText = '応答未受信';
    leavesr;
  endif;

  if AR_CARD_NO = AU_CARD_NO;
    Work.SameCard = *on;
  endif;

  if AR_AUTH_AMT = AU_AUTH_AMT;
    Work.SameAmount = *on;
  endif;

  if AR_MERCHANT_CD = AU_MERCHANT_CD;
    Work.SameMerchant = *on;
  endif;

  if AR_DISPLAY_FLG = '1';
    Work.AlreadyShown = *on;
  endif;

  if AU_AUTH_RESULT = '00';
    Work.HoldOpen = *on;
  endif;

  if CF_CARD_STATUS = '01';
    Work.StsOk = *on;
  endif;

  if AU_CURRENCY_CD = Work.BaseCur and AR_CURRENCY_CD = Work.BaseCur;
    Work.CurOk = *on;
  endif;

  if Work.AlreadyShown;
    Work.RetCd = '08';
    Work.MsgCd = 'R208';
    Work.RespText = '応答表示済';
    leavesr;
  endif;

  if not Work.SameCard;
    Work.RetCd = '08';
    Work.MsgCd = 'R209';
    Work.RespText = 'カード番号不一致';
    leavesr;
  endif;

  if not Work.SameAmount or not Work.SameMerchant;
    Work.RetCd = '08';
    Work.MsgCd = 'R210';
    Work.RespText = '応答内容不一致';
    leavesr;
  endif;

  if not Work.HoldOpen;
    Work.RetCd = '08';
    Work.MsgCd = 'A230';
    Work.RespText = '与信状態対象外';
    leavesr;
  endif;

  Work.Showable = *on;
  Work.Decision = AR_DECISION_KBN;
  Work.ApprNo = AR_APPROVAL_NO;

  select;
  when AR_DECISION_KBN = 'A';
    Work.RetCd = '00';
    Work.MsgCd = 'A000';
    Work.ShortReason = *blanks;
    Work.RespText = '承認';

  when AR_DECISION_KBN = 'D';

    select;
    when AR_DECLINE_REASON = 'LIM';
      Work.ShortReason = 'LIM';
      Work.MsgCd = 'D110';
      Work.RespText = '枠超過';

    when AR_DECLINE_REASON = 'STS';
      Work.ShortReason = 'STS';
      Work.MsgCd = 'D120';
      Work.RespText = 'カード状態';

    when AR_DECLINE_REASON = 'CUR';
      Work.ShortReason = 'CUR';
      Work.MsgCd = 'D130';
      Work.RespText = '通貨不可';

    other;
      if not Work.StsOk;
        Work.ShortReason = 'STS';
        Work.MsgCd = 'D120';
        Work.RespText = 'カード状態';
      elseif not Work.CurOk;
        Work.ShortReason = 'CUR';
        Work.MsgCd = 'D130';
        Work.RespText = '通貨不可';
      else;
        Work.ShortReason = 'STS';
        Work.MsgCd = 'D199';
        Work.RespText = '否認';
      endif;
    endsl;

    Work.RetCd = '00';

  other;
    Work.RetCd = '08';
    Work.MsgCd = 'R211';
    Work.Decision = 'D';
    Work.ShortReason = 'STS';
    Work.ApprNo = *blanks;
    Work.RespText = '応答区分不正';
  endsl;

  if Work.Showable and Work.RetCd = '00';
    monitor;
      AR_DISPLAY_FLG = '1';
      AR_DISPLAY_DATE = Work.NowDate;
      AR_DISPLAY_TIME = Work.NowTime;
      AR_DISPLAY_TERM = Work.TermId;
      AR_DISPLAY_OPER = Work.OperId;
      update RspRec Rsp;
    on-error;
      Work.RetCd = '90';
      Work.MsgCd = 'E901';
      Work.Decision = 'D';
      Work.ShortReason = 'STS';
      Work.ApprNo = *blanks;
      Work.RespText = '表示更新不可';
    endmon;
  endif;

endsr;

//**********************************************************************
//  返却設定
//**********************************************************************
begsr SetReturn;

  pRtnCd = Work.RetCd;
  pMsgCd = Work.MsgCd;
  pDecision = Work.Decision;
  pShortReason = Work.ShortReason;
  pApprNo = Work.ApprNo;
  pRespText = Work.RespText;

endsr;
