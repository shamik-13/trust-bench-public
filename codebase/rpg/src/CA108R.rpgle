**free
ctl-opt dftactgrp(*no) actgrp(*caller)
        option(*srcstmt:*nodebugio)
        main(CA108R);

//**********************************************************************
// 変更履歴
// 版数  年月日    担当   概要
// 1.00  20240401  K.SATO 新規作成
// 1.01  20240520  M.TAN  リボ契約状態および直近請求状態の返却追加
// 1.02  20240618  K.SATO 延滞日数による応答コード正規化追加
//**********************************************************************
// みらいカード 会員ステータス照会RPG
// 会員IDまたはカード番号からオーソリ用簡約ステータスを返却する。
//**********************************************************************

dcl-f CDMEMPF   usage(*input) keyed prefix(MM_) usropn;
dcl-f CDREVFPF  usage(*input) keyed prefix(RV_) usropn;
dcl-f CDSTMTPF  usage(*input) keyed prefix(ST_) usropn;

/copy QRPGLESRC,CDMEMC
/copy QRPGLESRC,CDREVFC
/copy QRPGLESRC,CDSTMTF2C

dcl-ds Inp qualified;
  ReqKbn        char(1);
  MemberId      char(12);
  CardNo        char(16);
  AuthDate      zoned(8:0);
  AuthTime      zoned(6:0);
  ShopId        char(15);
  TranAmt       packed(13:0);
end-ds;

dcl-ds Outp qualified;
  RspCd         char(2);
  MemberId      char(12);
  CardNo        char(16);
  MemberSts     char(2);
  RevStatus     char(2);
  SlideTier     char(2);
  StmtStatus    char(1);
  RevBal        packed(13:0);
  FeeAmt        packed(11:0);
  PayAmt        packed(13:0);
  PastDueDays   packed(5:0);
  AuthSts       char(1);
  MsgId         char(7);
end-ds;

dcl-pr CA108R extpgm('CA108R');
  PiReqKbn        char(1)       const;
  PiMemberId      char(12)      const;
  PiCardNo        char(16)      const;
  PiAuthDate      zoned(8:0)    const;
  PiAuthTime      zoned(6:0)    const;
  PiShopId        char(15)      const;
  PiTranAmt       packed(13:0)  const;
  PoRspCd         char(2);
  PoMemberId      char(12);
  PoCardNo        char(16);
  PoMemberSts     char(2);
  PoRevStatus     char(2);
  PoSlideTier     char(2);
  PoStmtStatus    char(1);
  PoRevBal        packed(13:0);
  PoFeeAmt        packed(11:0);
  PoPayAmt        packed(13:0);
  PoPastDueDays   packed(5:0);
  PoAuthSts       char(1);
  PoMsgId         char(7);
end-pr;

dcl-pi CA108R;
  PiReqKbn        char(1)       const;
  PiMemberId      char(12)      const;
  PiCardNo        char(16)      const;
  PiAuthDate      zoned(8:0)    const;
  PiAuthTime      zoned(6:0)    const;
  PiShopId        char(15)      const;
  PiTranAmt       packed(13:0)  const;
  PoRspCd         char(2);
  PoMemberId      char(12);
  PoCardNo        char(16);
  PoMemberSts     char(2);
  PoRevStatus     char(2);
  PoSlideTier     char(2);
  PoStmtStatus    char(1);
  PoRevBal        packed(13:0);
  PoFeeAmt        packed(11:0);
  PoPayAmt        packed(13:0);
  PoPastDueDays   packed(5:0);
  PoAuthSts       char(1);
  PoMsgId         char(7);
end-pi;

dcl-s C_REV_OK        char(2) inz('01');
dcl-s C_REV_HOLD      char(2) inz('02');
dcl-s C_REV_CLOSE     char(2) inz('03');
dcl-s C_STMT_FIXED    char(1) inz('C');
dcl-s C_STMT_SKIP     char(1) inz('S');
dcl-s C_AUTH_OK       char(1) inz('0');
dcl-s C_AUTH_NG       char(1) inz('1');
dcl-s C_AUTH_HOLD     char(1) inz('2');
dcl-s C_FEE_RATE      packed(5:4) inz(0.0125);

dcl-s WkFoundMem      ind inz(*off);
dcl-s WkFoundRev      ind inz(*off);
dcl-s WkFoundStmt     ind inz(*off);
dcl-s WkDueDate       zoned(8:0) inz(*zero);
dcl-s WkOverLimit     packed(13:0) inz(*zero);
dcl-s WkAvailAmt      packed(13:0) inz(*zero);
dcl-s WkTmpFee        packed(15:4) inz(*zero);
dcl-s WkLoop          packed(3:0) inz(*zero);
dcl-s WkErr           char(1) inz('0');

//**********************************************************************
// 主処理
//**********************************************************************

Inp.ReqKbn   = PiReqKbn;
Inp.MemberId = PiMemberId;
Inp.CardNo   = PiCardNo;
Inp.AuthDate = PiAuthDate;
Inp.AuthTime = PiAuthTime;
Inp.ShopId   = PiShopId;
Inp.TranAmt  = PiTranAmt;

exsr InitOut;

monitor;
  if not %open(CDMEMPF);
    open CDMEMPF;
  endif;

  if not %open(CDREVFPF);
    open CDREVFPF;
  endif;

  if not %open(CDSTMTPF);
    open CDSTMTPF;
  endif;

  exsr ChkInput;

  if Outp.RspCd = '00';
    exsr GetMember;
  endif;

  if Outp.RspCd = '00';
    exsr EditMember;
  endif;

  if Outp.RspCd = '00';
    exsr GetRevolving;
  endif;

  if Outp.RspCd = '00';
    exsr GetStatement;
  endif;

  if Outp.RspCd = '00';
    exsr MakeAuthStatus;
  endif;

on-error;
  WkErr = '1';
  Outp.RspCd  = '96';
  Outp.AuthSts = C_AUTH_NG;
  Outp.MsgId  = 'CA10896';
endmon;

exsr SetReturn;

*inlr = *on;
return;

//**********************************************************************
// 初期化
//**********************************************************************
begsr InitOut;

  clear Outp;

  Outp.RspCd       = '00';
  Outp.MemberId    = Inp.MemberId;
  Outp.CardNo      = Inp.CardNo;
  Outp.MemberSts   = *blanks;
  Outp.RevStatus   = *blanks;
  Outp.SlideTier   = *blanks;
  Outp.StmtStatus  = C_STMT_SKIP;
  Outp.RevBal      = 0;
  Outp.FeeAmt      = 0;
  Outp.PayAmt      = 0;
  Outp.PastDueDays = 0;
  Outp.AuthSts     = C_AUTH_OK;
  Outp.MsgId       = *blanks;

  WkFoundMem  = *off;
  WkFoundRev  = *off;
  WkFoundStmt = *off;
  WkDueDate   = 0;
  WkOverLimit = 0;
  WkAvailAmt  = 0;
  WkTmpFee    = 0;
  WkLoop      = 0;
  WkErr       = '0';

endsr;

//**********************************************************************
// 入力検査
//**********************************************************************
begsr ChkInput;

  select;
  when Inp.ReqKbn <> '1' and Inp.ReqKbn <> '2';
    Outp.RspCd   = '12';
    Outp.AuthSts = C_AUTH_NG;
    Outp.MsgId   = 'CA10812';

  when Inp.ReqKbn = '1' and %trim(Inp.MemberId) = *blanks;
    Outp.RspCd   = '14';
    Outp.AuthSts = C_AUTH_NG;
    Outp.MsgId   = 'CA10814';

  when Inp.ReqKbn = '2' and %trim(Inp.CardNo) = *blanks;
    Outp.RspCd   = '14';
    Outp.AuthSts = C_AUTH_NG;
    Outp.MsgId   = 'CA10814';

  when Inp.AuthDate = 0;
    Outp.RspCd   = '12';
    Outp.AuthSts = C_AUTH_NG;
    Outp.MsgId   = 'CA10812';

  when Inp.TranAmt < 0;
    Outp.RspCd   = '13';
    Outp.AuthSts = C_AUTH_NG;
    Outp.MsgId   = 'CA10813';

  other;
    Outp.RspCd = '00';
  endsl;

endsr;

//**********************************************************************
// 会員検索
//**********************************************************************
begsr GetMember;

  if Inp.ReqKbn = '1';
    chain Inp.MemberId CDMEMPF;
    if %found(CDMEMPF);
      WkFoundMem = *on;
    endif;
  else;
    setll Inp.CardNo CDMEMPF;
    reade Inp.CardNo CDMEMPF;
    if %eof(CDMEMPF);
      WkFoundMem = *off;
    else;
      WkFoundMem = *on;
    endif;
  endif;

  if not WkFoundMem;
    Outp.RspCd   = '14';
    Outp.AuthSts = C_AUTH_NG;
    Outp.MsgId   = 'CA10814';
    leavesr;
  endif;

  Outp.MemberId  = MM_MBR_ID;
  Outp.CardNo    = MM_CARD_NO;
  Outp.MemberSts = MM_MBR_STATUS;

endsr;

//**********************************************************************
// 会員状態判定
//**********************************************************************
begsr EditMember;

  select;
  when MM_MBR_STATUS = '00';
    Outp.RspCd   = '00';
    Outp.AuthSts = C_AUTH_OK;

  when MM_MBR_STATUS = '10';
    Outp.RspCd   = '62';
    Outp.AuthSts = C_AUTH_HOLD;
    Outp.MsgId   = 'CA10862';

  when MM_MBR_STATUS = '20';
    Outp.RspCd   = '54';
    Outp.AuthSts = C_AUTH_NG;
    Outp.MsgId   = 'CA10854';

  when MM_MBR_STATUS = '30';
    Outp.RspCd   = '57';
    Outp.AuthSts = C_AUTH_NG;
    Outp.MsgId   = 'CA10857';

  other;
    Outp.RspCd   = '05';
    Outp.AuthSts = C_AUTH_NG;
    Outp.MsgId   = 'CA10805';
  endsl;

  if Outp.RspCd <> '00';
    leavesr;
  endif;

  WkAvailAmt = MM_CRD_LIMIT - MM_USED_AMT;

  if Inp.TranAmt > 0 and Inp.TranAmt > WkAvailAmt;
    Outp.RspCd   = '51';
    Outp.AuthSts = C_AUTH_NG;
    Outp.MsgId   = 'CA10851';
  endif;

endsr;

//**********************************************************************
// リボ契約検索
//**********************************************************************
begsr GetRevolving;

  chain Outp.MemberId CDREVFPF;

  if not %found(CDREVFPF);
    Outp.RevStatus  = C_REV_CLOSE;
    Outp.SlideTier  = *blanks;
    Outp.StmtStatus = C_STMT_SKIP;
    Outp.RevBal     = 0;
    Outp.FeeAmt     = 0;
    Outp.PayAmt     = 0;
    WkFoundRev      = *off;
    leavesr;
  endif;

  WkFoundRev     = *on;
  Outp.RevStatus = RV_REV_STATUS;
  Outp.SlideTier = RV_SLIDE_TIER;

  if RV_REV_STATUS <> C_REV_OK;
    Outp.StmtStatus = C_STMT_SKIP;
    Outp.RevBal     = 0;
    Outp.FeeAmt     = 0;
    Outp.PayAmt     = 0;
    leavesr;
  endif;

  Outp.RevBal = RV_REV_BAL;

  // 手数料は当月リボ残高×月利0.0125（円未満切捨て）で参考表示する。
  WkTmpFee = RV_REV_BAL * C_FEE_RATE;
  Outp.FeeAmt = %dec(WkTmpFee:11:0);

  // 月々の請求予定額（元金＋手数料）は確定請求の値を採用するため、
  // ここでは初期値として手数料のみを設定し、直近請求検索で上書きする。
  Outp.PayAmt = Outp.FeeAmt;

endsr;

//**********************************************************************
// 直近請求検索
//**********************************************************************
begsr GetStatement;

  if Outp.RevStatus <> C_REV_OK;
    Outp.StmtStatus  = C_STMT_SKIP;
    Outp.PastDueDays = 0;
    leavesr;
  endif;

  setll Outp.MemberId CDSTMTPF;

  dou %eof(CDSTMTPF) or WkFoundStmt;
    reade Outp.MemberId CDSTMTPF;

    if %eof(CDSTMTPF);
      leave;
    endif;

    if ST_BILL_KBN = 'R' and ST_STMT_STATUS = C_STMT_FIXED;
      WkFoundStmt = *on;
      WkDueDate = ST_DUE_DATE;
      Outp.StmtStatus = ST_STMT_STATUS;
      Outp.PayAmt = ST_PAY_AMT;
    endif;

    WkLoop += 1;
    if WkLoop > 36;
      leave;
    endif;
  enddo;

  if not WkFoundStmt;
    Outp.StmtStatus  = C_STMT_SKIP;
    Outp.PastDueDays = 0;
    leavesr;
  endif;

  if ST_UNPAID_AMT <= 0;
    Outp.PastDueDays = 0;
  elseif Inp.AuthDate > WkDueDate;
    Outp.PastDueDays = %dec(Inp.AuthDate - WkDueDate:5:0);
  else;
    Outp.PastDueDays = 0;
  endif;

endsr;

//**********************************************************************
// オーソリ用簡約状態作成
//**********************************************************************
begsr MakeAuthStatus;

  if Outp.RevStatus = C_REV_HOLD or Outp.RevStatus = C_REV_CLOSE;
    Outp.StmtStatus = C_STMT_SKIP;
    Outp.RevBal     = 0;
    Outp.FeeAmt     = 0;
    Outp.PayAmt     = 0;
  endif;

  if Outp.PastDueDays > 30;
    Outp.RspCd   = '62';
    Outp.AuthSts = C_AUTH_HOLD;
    Outp.MsgId   = 'CA10862';
    leavesr;
  endif;

  if Outp.RspCd = '00';
    Outp.AuthSts = C_AUTH_OK;
    Outp.MsgId   = 'CA10800';
  endif;

endsr;

//**********************************************************************
// 返却値設定
//**********************************************************************
begsr SetReturn;

  PoRspCd       = Outp.RspCd;
  PoMemberId    = Outp.MemberId;
  PoCardNo      = Outp.CardNo;
  PoMemberSts   = Outp.MemberSts;
  PoRevStatus   = Outp.RevStatus;
  PoSlideTier   = Outp.SlideTier;
  PoStmtStatus  = Outp.StmtStatus;
  PoRevBal      = Outp.RevBal;
  PoFeeAmt      = Outp.FeeAmt;
  PoPayAmt      = Outp.PayAmt;
  PoPastDueDays = Outp.PastDueDays;
  PoAuthSts     = Outp.AuthSts;
  PoMsgId       = Outp.MsgId;

endsr;
