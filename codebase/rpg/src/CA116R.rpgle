**free
//**********************************************************************
//* 変更履歴
//* 版数  年月日      担当    概要
//* 1.00  2019.04.01  森田    CA301R近傍呼出用の海外ATM参照ラッパ新設
//* 1.01  2020.09.15  河合    CA113R会員状態参照の戻り値退避を追加
//* 1.02  2022.02.07  高橋    オンライン枠照会項目の初期化漏れを補正
//* 1.03  2024.06.18  佐伯    CDAUTHF4C連携域の応答コード設定位置を整理
//**********************************************************************
ctl-opt dftactgrp(*no) actgrp(*caller)
        option(*srcstmt:*nodebugio)
        bnddir('QC2LE')
        datfmt(*iso) timfmt(*iso);

//**********************************************************************
//*  連携域テンプレート（近傍呼出のI/O様式）
//**********************************************************************
dcl-ds TXNREC qualified template;
  CardNo             char(19);
  ChannelKbn         char(2);
  TxnKbn             char(2);
  FeeKbn             char(2);
  SetlKbn            char(1);
  DispKbn            char(1);
  TxnAmt             packed(13:0);
  FeeAmt             packed(9:0);
  BrandFeeAmt        packed(9:0);
end-ds;

dcl-ds ACCREC qualified template;
  MemberNo           char(16);
  CashAvailAmt       packed(13:0);
  LastRefDate        date;
  LastRefTime        time;
end-ds;

dcl-ds AUTH4REC qualified template;
  MemberNo           char(16);
  CardNo             char(19);
  ChannelKbn         char(2);
  DispKbn            char(1);
  TxnAmt             packed(13:0);
  TotalAmt           packed(13:0);
  CashLimit          packed(13:0);
  CashUsed           packed(13:0);
  AvailAmt           packed(13:0);
  AuthDate           date;
  AuthTime           time;
  AuthPgm            char(10);
  AuthNo             char(6);
  RespCd             char(2);
  RespText           char(40);
  SetlKbn            char(1);
  FeeKbn             char(2);
  RefRc              char(2);
  RefMsg             char(60);
end-ds;

//**********************************************************************
//*  外部プログラム定義
//**********************************************************************
dcl-pr CA113R extpgm('CA113R');
  p113InMember       char(16) const;
  p113InCardNo       char(19) const;
  p113InChannel      char(2)  const;
  p113OutStatus      char(2);
  p113OutUseStop     char(1);
  p113OutCashStop    char(1);
  p113OutShopLimit   packed(13:0);
  p113OutCashLimit   packed(13:0);
  p113OutShopUsed    packed(13:0);
  p113OutCashUsed    packed(13:0);
  p113OutRc          char(2);
  p113OutMsg         char(60);
end-pr;

//**********************************************************************
//*  エントリ定義
//**********************************************************************
dcl-pi *n;
  piTxnIo            likeds(TXNREC);
  piAccIo            likeds(ACCREC);
  piAuthIo           likeds(AUTH4REC);
end-pi;

//**********************************************************************
//*  作業域
//**********************************************************************
dcl-ds Wk qualified inz;
  MemberNo           char(16);
  CardNo             char(19);
  ChannelKbn         char(2);
  TxnKbn             char(2);
  FeeKbn             char(2);
  SetlKbn            char(1);
  DispKbn            char(1);
  ReqAmt             packed(13:0);
  FeeAmt             packed(9:0);
  BrandFeeAmt        packed(9:0);
  TotalAmt           packed(13:0);
  ShopLimit          packed(13:0);
  CashLimit          packed(13:0);
  ShopUsed           packed(13:0);
  CashUsed           packed(13:0);
  ZanAmt             packed(13:0);
  StatusKbn          char(2);
  UseStopFlg         char(1);
  CashStopFlg        char(1);
  ReferRc            char(2);
  ReferMsg           char(60);
  ErrPgm             char(10);
  ErrStep            char(20);
  DeclineCd          char(2);
  ApproveFlg         char(1);
  AuthNo             char(6);
end-ds;

dcl-ds Sv qualified inz;
  AuthResp           char(2);
  AuthText           char(40);
  LimitBefore        packed(13:0);
  LimitAfter         packed(13:0);
end-ds;

dcl-s Ix                 packed(3:0) inz(0);
dcl-s LoopEnd            ind inz(*off);
dcl-s Today              date inz(*sys);
dcl-s NowTime            time inz(*sys);
dcl-s ZeroAmt            packed(13:0) inz(0);

//**********************************************************************
//*  主処理
//**********************************************************************
clear Wk;
clear Sv;

Wk.ErrStep = '入力退避';

Wk.MemberNo   = %trim(piAccIo.MemberNo);
Wk.CardNo     = %trim(piTxnIo.CardNo);
Wk.ChannelKbn = piTxnIo.ChannelKbn;
Wk.TxnKbn     = piTxnIo.TxnKbn;
Wk.FeeKbn     = piTxnIo.FeeKbn;
Wk.SetlKbn    = piTxnIo.SetlKbn;
Wk.DispKbn    = piTxnIo.DispKbn;
Wk.ReqAmt     = piTxnIo.TxnAmt;
Wk.FeeAmt     = piTxnIo.FeeAmt;
Wk.BrandFeeAmt= piTxnIo.BrandFeeAmt;
Wk.TotalAmt   = Wk.ReqAmt + Wk.FeeAmt + Wk.BrandFeeAmt;

piAuthIo.AuthDate = Today;
piAuthIo.AuthTime = NowTime;
piAuthIo.AuthPgm  = 'CA116R';
piAuthIo.RespCd   = '91';
piAuthIo.RespText = '参照前';
piAuthIo.AuthNo   = *blanks;
piAuthIo.SetlKbn  = Wk.SetlKbn;
piAuthIo.FeeKbn   = Wk.FeeKbn;

monitor;

  Wk.ErrStep = '基本入力検査';

  if Wk.MemberNo = *blanks or Wk.CardNo = *blanks;
    Wk.DeclineCd = '14';
    Wk.ReferMsg  = '会員番号またはカード番号未設定';
    exsr SetDecline;
  elseif Wk.ChannelKbn <> '04';
    Wk.DeclineCd = '12';
    Wk.ReferMsg  = '海外ATM以外の経路';
    exsr SetDecline;
  elseif Wk.ReqAmt <= ZeroAmt;
    Wk.DeclineCd = '13';
    Wk.ReferMsg  = '取引金額不正';
    exsr SetDecline;
  elseif Wk.DispKbn <> 'K';
    Wk.DeclineCd = '12';
    Wk.ReferMsg  = '海外ATMはキャッシング参照のみ';
    exsr SetDecline;
  elseif Wk.TxnKbn <> *blanks and Wk.TxnKbn <> 'C2';
    Wk.DeclineCd = '12';
    Wk.ReferMsg  = '取引区分はCA301R決定前提';
    exsr SetDecline;
  else;

    Wk.ErrStep = '会員状態参照';

    callp CA113R(
        Wk.MemberNo:
        Wk.CardNo:
        Wk.ChannelKbn:
        Wk.StatusKbn:
        Wk.UseStopFlg:
        Wk.CashStopFlg:
        Wk.ShopLimit:
        Wk.CashLimit:
        Wk.ShopUsed:
        Wk.CashUsed:
        Wk.ReferRc:
        Wk.ReferMsg);

    Wk.ErrStep = '参照結果判定';

    select;
    when Wk.ReferRc <> '00';
      Wk.DeclineCd = '91';
      exsr SetDecline;

    when Wk.StatusKbn <> '00';
      Wk.DeclineCd = '54';
      Wk.ReferMsg  = '会員状態利用不可';
      exsr SetDecline;

    when Wk.UseStopFlg = '1';
      Wk.DeclineCd = '62';
      Wk.ReferMsg  = 'カード利用停止';
      exsr SetDecline;

    when Wk.CashStopFlg = '1';
      Wk.DeclineCd = '57';
      Wk.ReferMsg  = 'キャッシング停止';
      exsr SetDecline;

    other;

      Wk.ErrStep = 'オンライン枠算出';

      Wk.ZanAmt = Wk.CashLimit - Wk.CashUsed;

      if Wk.ZanAmt < ZeroAmt;
        Wk.ZanAmt = ZeroAmt;
      endif;

      Sv.LimitBefore = Wk.ZanAmt;

      if Wk.TotalAmt > Wk.ZanAmt;
        Wk.DeclineCd = '51';
        Wk.ReferMsg  = '海外ATM利用可能額不足';
        exsr SetDecline;
      else;
        Wk.ApproveFlg = '1';
        Sv.LimitAfter = Wk.ZanAmt - Wk.TotalAmt;
        exsr SetApprove;
      endif;

    endsl;

  endif;

on-error;
  Wk.ErrPgm    = 'CA116R';
  Wk.DeclineCd = '96';
  Wk.ReferMsg  = 'CA116R異常 ' + %trim(Wk.ErrStep);
  exsr SetDecline;
endmon;

*inlr = *on;
return;

//**********************************************************************
//*  承認応答設定
//**********************************************************************
begsr SetApprove;

  piAuthIo.RespCd       = '00';
  piAuthIo.RespText     = '承認';
  piAuthIo.MemberNo     = Wk.MemberNo;
  piAuthIo.CardNo       = Wk.CardNo;
  piAuthIo.ChannelKbn   = Wk.ChannelKbn;
  piAuthIo.DispKbn      = Wk.DispKbn;
  piAuthIo.TxnAmt       = Wk.ReqAmt;
  piAuthIo.TotalAmt     = Wk.TotalAmt;
  piAuthIo.CashLimit    = Wk.CashLimit;
  piAuthIo.CashUsed     = Wk.CashUsed;
  piAuthIo.AvailAmt     = Sv.LimitAfter;
  piAuthIo.RefRc        = Wk.ReferRc;
  piAuthIo.RefMsg       = Wk.ReferMsg;

  piAccIo.CashAvailAmt  = Sv.LimitAfter;
  piAccIo.LastRefDate   = Today;
  piAccIo.LastRefTime   = NowTime;

endsr;

//**********************************************************************
//*  否認応答設定
//**********************************************************************
begsr SetDecline;

  if Wk.DeclineCd = *blanks;
    Wk.DeclineCd = '91';
  endif;

  piAuthIo.RespCd       = Wk.DeclineCd;
  piAuthIo.RespText     = %subst(%trim(Wk.ReferMsg) + *blanks: 1: 40);
  piAuthIo.MemberNo     = Wk.MemberNo;
  piAuthIo.CardNo       = Wk.CardNo;
  piAuthIo.ChannelKbn   = Wk.ChannelKbn;
  piAuthIo.DispKbn      = Wk.DispKbn;
  piAuthIo.TxnAmt       = Wk.ReqAmt;
  piAuthIo.TotalAmt     = Wk.TotalAmt;
  piAuthIo.CashLimit    = Wk.CashLimit;
  piAuthIo.CashUsed     = Wk.CashUsed;
  piAuthIo.AvailAmt     = Wk.ZanAmt;
  piAuthIo.RefRc        = Wk.ReferRc;
  piAuthIo.RefMsg       = Wk.ReferMsg;

  piAccIo.LastRefDate   = Today;
  piAccIo.LastRefTime   = NowTime;

endsr;
