**free
//**********************************************************************
// 変更履歴
// 版数  年月日    担当    概要
// 01.00 20231201  MK001   初版作成
// 01.01 20240214  MK017   口座停止時の予定日変更抑止を追加
// 01.02 20240520  MK017   再請求上限超過時の停止入力判定を追加
//**********************************************************************
ctl-opt dftactgrp(*no) actgrp(*caller)
        option(*srcstmt:*nodebugio) datfmt(*iso) timfmt(*iso)
        bnddir('QC2LE')
        main(CA250R);

//======================================================================
//  カード別 振替不能照会画面
//  みらいカード オーソリ後続業務
//======================================================================

dcl-f CA250D  workstn indds(DspInd) infds(DspInf);
dcl-f CDRTRYF  usage(*update:*input) keyed usropn;
dcl-f CDTRRF   usage(*input)         keyed usropn;
dcl-f CDACTF   usage(*input)         keyed usropn;
dcl-f CDAUTHF  usage(*input)         keyed usropn;

//---------------------------------------------------------------------
//  レコード定義
//---------------------------------------------------------------------
/copy QRPGLESRC,CDRTRYC
/copy QRPGLESRC,CDTRRC
/copy QRPGLESRC,CDACTC

//---------------------------------------------------------------------
//  呼出しインターフェース
//---------------------------------------------------------------------
dcl-pr CA250R extpgm('CA250R');
  pMode       char(1)   const;
  pOpeId      char(8)   const;
  pTermId     char(10)  const;
  pCardNo     char(16)  const;
  pRtnCd      char(2);
  pMsgCd      char(7);
end-pr;

dcl-pi CA250R;
  pMode       char(1)   const;
  pOpeId      char(8)   const;
  pTermId     char(10)  const;
  pCardNo     char(16)  const;
  pRtnCd      char(2);
  pMsgCd      char(7);
end-pi;

//---------------------------------------------------------------------
//  表示装置 INFDS / INDDS
//---------------------------------------------------------------------
dcl-ds DspInd qualified;
  ExitKey       ind pos(3);
  HelpKey       ind pos(4);
  F05Key        ind pos(5);
  F12Key        ind pos(12);
  ErrCard       ind pos(31);
  ErrAct        ind pos(32);
  ErrDate       ind pos(33);
  ErrStop       ind pos(34);
  ErrUpd        ind pos(35);
  ProtectUpd    ind pos(51);
  ProtectDate   ind pos(52);
  ProtectStop   ind pos(53);
  SflDsp        ind pos(71);
  SflDspCtl     ind pos(72);
  SflClr        ind pos(73);
  SflEnd        ind pos(74);
end-ds;

dcl-ds DspInf qualified;
  FileName      char(10) pos(1);
  Status        zoned(5) pos(11);
  Rrn           zoned(5) pos(378);
  Aid           char(1)  pos(369);
end-ds;

//---------------------------------------------------------------------
//  作業領域
//---------------------------------------------------------------------
dcl-ds Wk qualified inz;
  Mode          char(1);
  OpeId         char(8);
  TermId        char(10);
  CardNo        char(16);
  MaskCard      char(19);
  CustNo        char(12);
  ActNo         char(14);
  ActSts        char(1);
  ActStopRsn    char(2);
  RetryCnt      packed(3:0);
  RetryMax      packed(3:0);
  RetryAmt      packed(13:0);
  NextDate      date;
  OrgNextDate   date;
  LastFailDate  date;
  StopFlg       char(1);
  OrgStopFlg    char(1);
  StopRsn       char(2);
  UpdFlg        char(1);
  Rrn           packed(4:0);
  SflCnt        packed(4:0);
  ProcDate      date;
  ProcTime      time;
  MsgCd         char(7);
  MsgTxt        char(80);
  RtnCd         char(2);
  Eof           ind;
  Found         ind;
  ChgReq        ind;
  RefOnly       ind;
  Reject        ind;
end-ds;

dcl-ds AuthCtl qualified inz;
  CardNo        char(16);
  UseKbn        char(1);
  LimitAmt      packed(13:0);
  MonthUsed     packed(13:0);
  AvailAmt      packed(13:0);
  AuthStop      char(1);
  LostFlg       char(1);
  ExpYm         packed(6:0);
end-ds;

dcl-ds Msg qualified inz;
  Normal        char(80) inz('照会内容を確認してください。');
  NotFound      char(80) inz('対象カードの振替不能履歴がありません。');
  ActStop       char(80) inz('口座停止中のため更新できません。');
  OverMax       char(80) inz('再請求上限回数を超過しているため更新できません。');
  DateErr       char(80) inz('次回予定日は翌営業日以降を入力してください。');
  StopErr       char(80) inz('停止区分は 0 または 1 を入力してください。');
  UpdOk         char(80) inz('更新しました。');
  UpdErr        char(80) inz('更新時に障害が発生しました。');
  RefOnly       char(80) inz('照会モードのため更新できません。');
end-ds;

dcl-s KeyCardNo     char(16);
dcl-s KeySeq        packed(7:0);
dcl-s SavRrn        packed(4:0);
dcl-s InpNextDate   date;
dcl-s InpStopFlg    char(1);
dcl-s AmtWork       packed(15:2);
dcl-s Ix            int(10);
dcl-s TodayNum      packed(8:0);
dcl-s CtlRetryMax   packed(3:0) inz(3);

//---------------------------------------------------------------------
//  主処理
//---------------------------------------------------------------------
dcl-proc CA250R main;

  exsr Init;
  exsr OpenFiles;
  exsr LoadHeader;
  exsr LoadSubfile;

  dow not DspInd.ExitKey and not DspInd.F12Key;

    exsr ShowScreen;

    if DspInd.ExitKey or DspInd.F12Key;
      leave;
    endif;

    select;
    when DspInd.F05Key;
      exsr LoadHeader;
      exsr LoadSubfile;

    other;
      exsr CheckInput;

      if not Wk.Reject;
        exsr UpdateRetry;
      endif;

      exsr LoadHeader;
      exsr LoadSubfile;
    endsl;

  enddo;

  exsr CloseFiles;

  pRtnCd = Wk.RtnCd;
  pMsgCd = Wk.MsgCd;

  return;

//---------------------------------------------------------------------
//  初期処理
//---------------------------------------------------------------------
begsr Init;

  clear Wk;
  Wk.Mode     = pMode;
  Wk.OpeId    = pOpeId;
  Wk.TermId   = pTermId;
  Wk.CardNo   = pCardNo;
  Wk.ProcDate = %date();
  Wk.ProcTime = %time();
  Wk.RetryMax = CtlRetryMax;
  Wk.RtnCd    = '00';
  Wk.MsgCd    = *blanks;
  Wk.MsgTxt   = Msg.Normal;

  if Wk.Mode = 'Q';
    Wk.RefOnly = *on;
  endif;

  DspInd.ProtectUpd  = Wk.RefOnly;
  DspInd.ProtectDate = Wk.RefOnly;
  DspInd.ProtectStop = Wk.RefOnly;

endsr;

//---------------------------------------------------------------------
//  ファイルオープン
//---------------------------------------------------------------------
begsr OpenFiles;

  if not %open(CDRTRYF);
    open CDRTRYF;
  endif;

  if not %open(CDTRRF);
    open CDTRRF;
  endif;

  if not %open(CDACTF);
    open CDACTF;
  endif;

  if not %open(CDAUTHF);
    open CDAUTHF;
  endif;

endsr;

//---------------------------------------------------------------------
//  ファイルクローズ
//---------------------------------------------------------------------
begsr CloseFiles;

  if %open(CDRTRYF);
    close CDRTRYF;
  endif;

  if %open(CDTRRF);
    close CDTRRF;
  endif;

  if %open(CDACTF);
    close CDACTF;
  endif;

  if %open(CDAUTHF);
    close CDAUTHF;
  endif;

endsr;

//---------------------------------------------------------------------
//  ヘッダ読込
//---------------------------------------------------------------------
begsr LoadHeader;

  Wk.Found       = *off;
  Wk.Reject      = *off;
  Wk.MsgTxt      = Msg.Normal;
  Wk.MsgCd       = *blanks;
  Wk.CustNo      = *blanks;
  Wk.ActNo       = *blanks;
  Wk.ActSts      = *blanks;
  Wk.ActStopRsn  = *blanks;
  Wk.RetryCnt    = 0;
  Wk.RetryAmt    = 0;
  Wk.NextDate    = *loval;
  Wk.LastFailDate = *loval;
  Wk.StopFlg     = '0';
  Wk.StopRsn     = *blanks;

  DspInd.ErrCard = *off;
  DspInd.ErrAct  = *off;
  DspInd.ErrDate = *off;
  DspInd.ErrStop = *off;
  DspInd.ErrUpd  = *off;

  KeyCardNo = Wk.CardNo;
  setll KeyCardNo CDRTRYR;
  reade KeyCardNo CDRTRYR;

  if %eof(CDRTRYF);
    Wk.MsgCd       = 'CA25001';
    Wk.MsgTxt      = Msg.NotFound;
    Wk.RtnCd       = '04';
    DspInd.ErrCard = *on;
    return;
  endif;

  Wk.Found        = *on;
  Wk.CustNo       = RYCUSTNO;
  Wk.ActNo        = RYACTNO;
  Wk.RetryCnt     = RYTRYCNT;
  Wk.RetryAmt     = RYBALAMT;
  Wk.NextDate     = RYNEXTDT;
  Wk.OrgNextDate  = RYNEXTDT;
  Wk.LastFailDate = RYFAILDT;
  Wk.StopFlg      = RYSTPFLG;
  Wk.OrgStopFlg   = RYSTPFLG;
  Wk.StopRsn      = RYSTPRSN;

  chain Wk.ActNo CDACTR;
  if %found(CDACTF);
    Wk.ActSts     = ACSTS;
    Wk.ActStopRsn = ACSPRSN;
  endif;

  chain Wk.CardNo CDAUTHR;
  if %found(CDAUTHF);
    AuthCtl.CardNo    = AUCRDNO;
    AuthCtl.UseKbn    = AUUSEKBN;
    AuthCtl.LimitAmt  = AULIMAMT;
    AuthCtl.MonthUsed = AUMONUSE;
    AuthCtl.AuthStop  = AUSTPFLG;
    AuthCtl.LostFlg   = AULOSTFG;
    AuthCtl.ExpYm     = AUEXPYM;

    AuthCtl.AvailAmt = AuthCtl.LimitAmt - AuthCtl.MonthUsed;
    if AuthCtl.AvailAmt < 0;
      AuthCtl.AvailAmt = 0;
    endif;
  endif;

  Wk.MaskCard = %subst(Wk.CardNo:1:4) + '-****-****-' +
                %subst(Wk.CardNo:13:4);

  if Wk.ActSts = '9';
    DspInd.ProtectUpd  = *on;
    DspInd.ProtectDate = *on;
    DspInd.ProtectStop = *on;
  elseif Wk.RetryCnt > Wk.RetryMax;
    DspInd.ProtectDate = *on;
  else;
    DspInd.ProtectUpd  = Wk.RefOnly;
    DspInd.ProtectDate = Wk.RefOnly;
    DspInd.ProtectStop = Wk.RefOnly;
  endif;

endsr;

//---------------------------------------------------------------------
//  明細読込
//---------------------------------------------------------------------
begsr LoadSubfile;

  DspInd.SflDsp    = *off;
  DspInd.SflDspCtl = *off;
  DspInd.SflClr    = *on;
  write SFLCTL;
  DspInd.SflClr    = *off;

  Wk.SflCnt = 0;
  KeyCardNo = Wk.CardNo;

  setll KeyCardNo CDTRRR;
  dou %eof(CDTRRF);

    reade KeyCardNo CDTRRR;
    if %eof(CDTRRF);
      leave;
    endif;

    Wk.SflCnt += 1;
    SFLRRN = Wk.SflCnt;

    SFDAT   = TRFILDAT;
    SFSEQ   = TRSEQ;
    SFAUTH  = TRAUTHNO;
    SFAMT   = TRAMT;
    SFRSN   = TRRSNCD;
    SFBANK  = TRBANKCD;
    SFBRN   = TRBRNCD;
    SFACCT  = TRACTNO;
    SFTRY   = TRTRYCNT;

    select;
    when TRRSNCD = '01';
      SFRSNT = '残高不足';
    when TRRSNCD = '02';
      SFRSNT = '口座相違';
    when TRRSNCD = '03';
      SFRSNT = '口座停止';
    when TRRSNCD = '04';
      SFRSNT = '振替依頼書不備';
    other;
      SFRSNT = 'その他';
    endsl;

    write SFLREC;

    if Wk.SflCnt >= 9999;
      leave;
    endif;

  enddo;

  if Wk.SflCnt > 0;
    DspInd.SflDsp    = *on;
    DspInd.SflDspCtl = *on;
  endif;

  DspInd.SflEnd = *on;

endsr;

//---------------------------------------------------------------------
//  画面表示
//---------------------------------------------------------------------
begsr ShowScreen;

  HMODE  = Wk.Mode;
  HOPEID = Wk.OpeId;
  HTERM  = Wk.TermId;
  HCRDNO = Wk.MaskCard;
  HCUST  = Wk.CustNo;
  HACTNO = Wk.ActNo;
  HACTST = Wk.ActSts;
  HRYCNT = Wk.RetryCnt;
  HRYMAX = Wk.RetryMax;
  HRYAMT = Wk.RetryAmt;
  HNXDT  = Wk.NextDate;
  HFAIL  = Wk.LastFailDate;
  HSTPFG = Wk.StopFlg;
  HSTPRS = Wk.StopRsn;
  HAVAIL = AuthCtl.AvailAmt;
  HMSG   = Wk.MsgTxt;

  if Wk.ActSts = '9';
    HACTTX = '口座停止';
  elseif AuthCtl.AuthStop = '1';
    HACTTX = 'オーソリ停止';
  elseif AuthCtl.LostFlg = '1';
    HACTTX = '紛失';
  else;
    HACTTX = '通常';
  endif;

  exfmt SFLCTL;

  InpNextDate = HNXDT;
  InpStopFlg  = HSTPFG;

endsr;

//---------------------------------------------------------------------
//  入力検査
//---------------------------------------------------------------------
begsr CheckInput;

  Wk.Reject      = *off;
  Wk.ChgReq      = *off;
  DspInd.ErrDate = *off;
  DspInd.ErrStop = *off;
  DspInd.ErrAct  = *off;
  DspInd.ErrUpd  = *off;

  if Wk.RefOnly;
    Wk.Reject = *on;
    Wk.MsgCd  = 'CA25002';
    Wk.MsgTxt = Msg.RefOnly;
    return;
  endif;

  if not Wk.Found;
    Wk.Reject      = *on;
    Wk.MsgCd       = 'CA25001';
    Wk.MsgTxt      = Msg.NotFound;
    DspInd.ErrCard = *on;
    return;
  endif;

  if InpStopFlg <> '0' and InpStopFlg <> '1';
    Wk.Reject       = *on;
    Wk.MsgCd        = 'CA25003';
    Wk.MsgTxt       = Msg.StopErr;
    DspInd.ErrStop  = *on;
    return;
  endif;

  if InpNextDate <> Wk.OrgNextDate or InpStopFlg <> Wk.OrgStopFlg;
    Wk.ChgReq = *on;
  endif;

  if not Wk.ChgReq;
    return;
  endif;

  if Wk.ActSts = '9' or AuthCtl.AuthStop = '1';
    Wk.Reject     = *on;
    Wk.MsgCd      = 'CA25004';
    Wk.MsgTxt     = Msg.ActStop;
    DspInd.ErrAct = *on;
    return;
  endif;

  if Wk.RetryCnt >= Wk.RetryMax and InpStopFlg <> '1';
    Wk.Reject     = *on;
    Wk.MsgCd      = 'CA25005';
    Wk.MsgTxt     = Msg.OverMax;
    DspInd.ErrUpd = *on;
    return;
  endif;

  if InpStopFlg = '0';
    if InpNextDate <= Wk.ProcDate;
      Wk.Reject      = *on;
      Wk.MsgCd       = 'CA25006';
      Wk.MsgTxt      = Msg.DateErr;
      DspInd.ErrDate = *on;
      return;
    endif;
  endif;

  select;
  when %subdt(InpNextDate:*DAYS) = 1;
    Wk.Reject      = *on;
    Wk.MsgCd       = 'CA25006';
    Wk.MsgTxt      = Msg.DateErr;
    DspInd.ErrDate = *on;

  when %subdt(InpNextDate:*DAYS) = 7;
    Wk.Reject      = *on;
    Wk.MsgCd       = 'CA25006';
    Wk.MsgTxt      = Msg.DateErr;
    DspInd.ErrDate = *on;

  other;
    Wk.NextDate = InpNextDate;
    Wk.StopFlg  = InpStopFlg;
  endsl;

endsr;

//---------------------------------------------------------------------
//  更新処理
//---------------------------------------------------------------------
begsr UpdateRetry;

  if not Wk.ChgReq;
    return;
  endif;

  monitor;

    KeyCardNo = Wk.CardNo;
    KeySeq    = 0;

    chain(e) KeyCardNo CDRTRYR;
    if not %found(CDRTRYF);
      Wk.RtnCd       = '04';
      Wk.MsgCd       = 'CA25001';
      Wk.MsgTxt      = Msg.NotFound;
      DspInd.ErrCard = *on;
      return;
    endif;

    if RYTRYCNT <> Wk.RetryCnt;
      Wk.RtnCd      = '08';
      Wk.MsgCd      = 'CA25007';
      Wk.MsgTxt     = '他端末で更新済みです。再表示してください。';
      DspInd.ErrUpd = *on;
      return;
    endif;

    if Wk.StopFlg = '1';
      RYSTPFLG = '1';
      RYSTPRSN = '01';
      RYNEXTDT = *loval;
    else;
      RYSTPFLG = '0';
      RYSTPRSN = *blanks;
      RYNEXTDT = Wk.NextDate;
    endif;

    RYUPDDT  = Wk.ProcDate;
    RYUPDTM  = Wk.ProcTime;
    RYUPDOP  = Wk.OpeId;
    RYUPDTRM = Wk.TermId;

    update CDRTRYR;

    Wk.RtnCd = '00';
    Wk.MsgCd = 'CA25000';
    Wk.MsgTxt = Msg.UpdOk;

  on-error;
    Wk.RtnCd      = '99';
    Wk.MsgCd      = 'CA25099';
    Wk.MsgTxt     = Msg.UpdErr;
    DspInd.ErrUpd = *on;
  endmon;

endsr;

end-proc;
