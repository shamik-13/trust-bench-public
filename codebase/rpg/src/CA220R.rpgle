**free
//**********************************************************************
// 変更履歴
// 版数  年月日      担当      概要
// 1.00  2021/04/12  高橋      初版作成
// 1.10  2022/09/05  森        承認保留額集計に有効期限判定を追加
// 1.20  2024/02/19  伊藤      カード状態別の応答理由を整理
//**********************************************************************
ctl-opt dftactgrp(*no) actgrp(*caller)
        option(*srcstmt:*nodebugio)
        bnddir('QC2LE')
        datfmt(*iso) timfmt(*iso)
        decedit(*jobrun);

 /copy QRPGLESRC,CDCARDFC
 /copy QRPGLESRC,CDBALFC
 /copy QRPGLESRC,CDAUTHC

dcl-f CDCARDF usage(*input) keyed;
dcl-f CDBALF  usage(*input) keyed;
dcl-f CDAUTHF usage(*input) keyed;

dcl-ds Card  likeds(cdcardf_rec);
dcl-ds Bal   likeds(cdbalf_rec);
dcl-ds AuthH likeds(cdauthf_rec);

dcl-pr CA220R extpgm('CA220R');
  pInCardNo       char(16) const;
  pInProcDate     packed(8:0) const;
  pInProcTime     packed(6:0) const;
  pOutAvailAmt    packed(13:0);
  pOutRespCd      char(2);
  pOutReason      char(40);
end-pr;

dcl-pi CA220R;
  pInCardNo       char(16) const;
  pInProcDate     packed(8:0) const;
  pInProcTime     packed(6:0) const;
  pOutAvailAmt    packed(13:0);
  pOutRespCd      char(2);
  pOutReason      char(40);
end-pi;

dcl-ds Wk qualified inz;
  CardNo          char(16);
  ProcDate        packed(8:0);
  ProcTime        packed(6:0);
  LimitAmt        packed(13:0);
  BalanceAmt      packed(13:0);
  HoldAmt         packed(13:0);
  AvailAmt        packed(13:0);
  BalCycleDt      packed(8:0);
  AuthExpDate     packed(8:0);
  FoundCard       ind;
  FoundBalance    ind;
  EndAuth         ind;
  BadInput        ind;
end-ds;

dcl-ds Msg qualified inz;
  Normal          char(40) inz('00 利用可能枠照会正常                    ');
  NoCard          char(40) inz('14 カード番号該当なし                    ');
  StopCard        char(40) inz('54 利用停止カード                        ');
  CloseCard       char(40) inz('57 解約カード                            ');
  LostCard        char(40) inz('41 紛失扱いカード                        ');
  BadParm         char(40) inz('96 入力パラメータ不正                    ');
  FileErr         char(40) inz('98 ファイル入出力異常                    ');
end-ds;

dcl-s ZERO_AMT       packed(13:0) inz(0);
dcl-s RESP_OK        char(2) inz('00');
dcl-s RESP_NG        char(2) inz('05');
dcl-s RESP_NOCARD    char(2) inz('14');
dcl-s RESP_LOST      char(2) inz('41');
dcl-s RESP_STOP      char(2) inz('54');
dcl-s RESP_CLOSE     char(2) inz('57');
dcl-s RESP_SYS       char(2) inz('96');

dcl-s ST_ACTIVE      char(2) inz('01');
dcl-s ST_STOP        char(2) inz('02');
dcl-s ST_CLOSE       char(2) inz('03');
dcl-s ST_DELINQ      char(2) inz('09');

dcl-s BI_FIXED       char(1) inz('C');
dcl-s BI_HOLD        char(1) inz('H');
dcl-s BI_SKIP        char(1) inz('S');

dcl-s AUTH_APPR      char(1) inz('A');
dcl-s AUTH_DECL      char(1) inz('D');
dcl-s AUTH_HOLD      char(1) inz('H');

dcl-s ix             int(10) inz(0);

*inlr = *on;

clear pOutAvailAmt;
clear pOutRespCd;
clear pOutReason;

Wk.CardNo   = pInCardNo;
Wk.ProcDate = pInProcDate;
Wk.ProcTime = pInProcTime;

monitor;

  if %trim(Wk.CardNo) = *blanks
     or Wk.ProcDate = 0
     or Wk.ProcTime = 0;
    Wk.BadInput = *on;
  endif;

  if Wk.BadInput;
    pOutAvailAmt = ZERO_AMT;
    pOutRespCd   = RESP_SYS;
    pOutReason   = Msg.BadParm;
    return;
  endif;

  chain Wk.CardNo CDCARDF Card;
  if not %found(CDCARDF);
    pOutAvailAmt = ZERO_AMT;
    pOutRespCd   = RESP_NOCARD;
    pOutReason   = Msg.NoCard;
    return;
  endif;

  Wk.FoundCard = *on;
  Wk.LimitAmt  = Card.cf_credit_limit;

  select;
  when Card.cf_card_status = ST_STOP;
    pOutAvailAmt = ZERO_AMT;
    pOutRespCd   = RESP_STOP;
    pOutReason   = Msg.StopCard;
    return;

  when Card.cf_card_status = ST_CLOSE;
    pOutAvailAmt = ZERO_AMT;
    pOutRespCd   = RESP_CLOSE;
    pOutReason   = Msg.CloseCard;
    return;

  when Card.cf_card_status <> ST_ACTIVE
   and Card.cf_card_status <> ST_DELINQ;
    pOutAvailAmt = ZERO_AMT;
    pOutRespCd   = RESP_STOP;
    pOutReason   = Msg.StopCard;
    return;

  other;
  endsl;

  Wk.BalanceAmt = ZERO_AMT;
  Wk.BalCycleDt = 0;

  setll Wk.CardNo CDBALF;
  dou %eof(CDBALF);
    reade Wk.CardNo CDBALF Bal;
    if %eof(CDBALF);
      leave;
    endif;

    if Bal.bl_card_no <> Wk.CardNo;
      leave;
    endif;

    if Bal.bl_cycle_dt > Wk.BalCycleDt
       and Bal.bl_cycle_dt <= Wk.ProcDate;
      Wk.BalCycleDt   = Bal.bl_cycle_dt;
      Wk.BalanceAmt   = Bal.bl_closing_bal_amt;
      Wk.FoundBalance = *on;
    endif;
  enddo;

  Wk.HoldAmt = ZERO_AMT;

  setll Wk.CardNo CDAUTHF;
  dou Wk.EndAuth;
    reade Wk.CardNo CDAUTHF AuthH;
    if %eof(CDAUTHF);
      Wk.EndAuth = *on;
      leave;
    endif;

    if AuthH.au_card_no <> Wk.CardNo;
      Wk.EndAuth = *on;
      leave;
    endif;

    if AuthH.au_auth_result <> AUTH_APPR;
      iter;
    endif;

    Wk.AuthExpDate = AuthH.au_hold_exp_dt;

    if Wk.AuthExpDate < Wk.ProcDate;
      iter;
    endif;

    Wk.HoldAmt += AuthH.au_auth_amt;
  enddo;

  Wk.AvailAmt = Wk.LimitAmt - Wk.BalanceAmt - Wk.HoldAmt;

  if Wk.AvailAmt < ZERO_AMT;
    Wk.AvailAmt = ZERO_AMT;
  endif;

  pOutAvailAmt = Wk.AvailAmt;
  pOutRespCd   = RESP_OK;
  pOutReason   = Msg.Normal;

on-error;
  pOutAvailAmt = ZERO_AMT;
  pOutRespCd   = RESP_SYS;
  pOutReason   = Msg.FileErr;
endmon;

return;
