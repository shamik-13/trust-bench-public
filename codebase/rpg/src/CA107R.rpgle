**free
//**********************************************************************
//  変更履歴
//  版数  年月日    担当      概要
//  1.00  20190513  MK-開発1  新規作成
//  1.10  20201102  YS-保守   リボ停止時の利用不可理由追加
//  1.20  20220418  NT-保守   仮押さえ済額の控除対応
//  1.30  20230925  SK-保守   CDLIMTF閉塞判定追加
//**********************************************************************
//  プログラム名 : CA107R
//  機能名       : 利用可能枠照会オンライン
//  会社名       : みらいカード
//  処理概要     : カード番号をキーに総枠、リボ枠、仮押さえ済み額、
//                 直近リボ残高を読込み、オンライン画面用の利用可能額を
//                 算出する。
//**********************************************************************

ctl-opt dftactgrp(*no) actgrp('CAONLINE')
        option(*srcstmt:*nodebugio)
        main(CA107R);

dcl-f CDLIMTF usage(*input) keyed usropn;
dcl-f CDRBALF usage(*input) keyed usropn;
dcl-f CDREVF  usage(*input) keyed usropn;

/copy QRPGLESRC,CDLIMTC
/copy QRPGLESRC,CDRBALFC
/copy QRPGLESRC,CDREVFC

dcl-pr CA107R extpgm('CA107R');
  pReqCardNo        char(16) const;
  pReqShopCd        char(10) const;
  pReqTermId        char(08) const;
  pReqOpeId         char(08) const;
  pReqTraceNo       char(12) const;
  pOutResultCd      char(02);
  pOutReasonCd      char(04);
  pOutTotalLimit    packed(13:0);
  pOutRevLimit      packed(13:0);
  pOutHoldAmt       packed(13:0);
  pOutRevBal        packed(13:0);
  pOutFeeAmt        packed(13:0);
  pOutAvailTotal    packed(13:0);
  pOutAvailRev      packed(13:0);
  pOutSlideTier     char(02);
  pOutStmtStatus    char(01);
  pOutMsg           char(60);
end-pr;

dcl-proc CA107R;
  dcl-pi *n;
    pReqCardNo        char(16) const;
    pReqShopCd        char(10) const;
    pReqTermId        char(08) const;
    pReqOpeId         char(08) const;
    pReqTraceNo       char(12) const;
    pOutResultCd      char(02);
    pOutReasonCd      char(04);
    pOutTotalLimit    packed(13:0);
    pOutRevLimit      packed(13:0);
    pOutHoldAmt       packed(13:0);
    pOutRevBal        packed(13:0);
    pOutFeeAmt        packed(13:0);
    pOutAvailTotal    packed(13:0);
    pOutAvailRev      packed(13:0);
    pOutSlideTier     char(02);
    pOutStmtStatus    char(01);
    pOutMsg           char(60);
  end-pi;

  dcl-ds Wk qualified inz;
    CardNo           char(16);
    ShopCd           char(10);
    TermId           char(08);
    OpeId            char(08);
    TraceNo          char(12);
    LimitFound       ind;
    RevFound         ind;
    RbalFound        ind;
    TotalLimit       packed(13:0);
    RevLimit         packed(13:0);
    HoldAmt          packed(13:0);
    RevBal           packed(13:0);
    FeeAmt           packed(13:0);
    AvailTotal       packed(13:0);
    AvailRev         packed(13:0);
    MonthRate        packed(5:4);
    FeeWork          packed(15:4);
    ReasonCd         char(04);
    ResultCd         char(02);
    SlideTier        char(02);
    StmtStatus       char(01);
    Msg              char(60);
    ErrMsg           char(60);
  end-ds;

  dcl-s wkCardOk       ind inz(*off);
  dcl-s ix             packed(3:0) inz(1);
  dcl-s wkOddSum       packed(5:0) inz(0);
  dcl-s wkEvenSum      packed(5:0) inz(0);
  dcl-s wkDigit        packed(2:0) inz(0);
  dcl-s wkDbl          packed(2:0) inz(0);
  dcl-s wkChar         char(1) inz(*blank);
  dcl-s wkNum          zoned(1:0) inz(0);

  // 戻り値初期化
  clear pOutResultCd;
  clear pOutReasonCd;
  clear pOutTotalLimit;
  clear pOutRevLimit;
  clear pOutHoldAmt;
  clear pOutRevBal;
  clear pOutFeeAmt;
  clear pOutAvailTotal;
  clear pOutAvailRev;
  clear pOutSlideTier;
  clear pOutStmtStatus;
  clear pOutMsg;

  Wk.CardNo     = pReqCardNo;
  Wk.ShopCd     = pReqShopCd;
  Wk.TermId     = pReqTermId;
  Wk.OpeId      = pReqOpeId;
  Wk.TraceNo    = pReqTraceNo;
  Wk.MonthRate  = 0.0125;
  Wk.ResultCd   = '00';
  Wk.ReasonCd   = *blank;
  Wk.SlideTier  = 'T1';
  Wk.StmtStatus = 'C';
  Wk.Msg        = *blank;

  monitor;

    if not %open(CDLIMTF);
      open CDLIMTF;
    endif;

    if not %open(CDRBALF);
      open CDRBALF;
    endif;

    if not %open(CDREVF);
      open CDREVF;
    endif;

    // カード番号妥当性確認（桁数、数字、チェックディジット）
    wkCardOk = *on;
    if %trim(Wk.CardNo) = *blank or %len(%trimr(Wk.CardNo)) <> 16;
      wkCardOk = *off;
    endif;

    if wkCardOk;
      wkOddSum = 0;
      wkEvenSum = 0;
      ix = 1;

      dou ix > 16;
        wkChar = %subst(Wk.CardNo: ix: 1);

        select;
        when wkChar >= '0' and wkChar <= '9';
          wkNum = %dec(wkChar: 1: 0);
          wkDigit = wkNum;

          if %rem(ix: 2) = 1;
            wkDbl = wkDigit * 2;
            if wkDbl >= 10;
              wkDbl -= 9;
            endif;
            wkOddSum += wkDbl;
          else;
            wkEvenSum += wkDigit;
          endif;

        other;
          wkCardOk = *off;
          leave;
        endsl;

        ix += 1;
      enddo;

      if wkCardOk and %rem(wkOddSum + wkEvenSum: 10) <> 0;
        wkCardOk = *off;
      endif;
    endif;

    if not wkCardOk;
      Wk.ResultCd = '90';
      Wk.ReasonCd = 'CARD';
      Wk.StmtStatus = 'S';
      Wk.Msg = 'カード番号エラー';
      exsr SetReturn;
      return;
    endif;

    // 与信枠マスタ読込
    chain (Wk.CardNo) CDLIMTF;
    if %found(CDLIMTF);
      Wk.LimitFound = *on;
      Wk.TotalLimit = CL-TOTAL-LIMIT;
      Wk.RevLimit   = CL-REV-LIMIT;
      Wk.HoldAmt    = CL-HOLD-AMT;
    else;
      Wk.ResultCd = '91';
      Wk.ReasonCd = 'NOLM';
      Wk.StmtStatus = 'S';
      Wk.Msg = '与信枠未登録';
      exsr SetReturn;
      return;
    endif;

    // リボ契約マスタ読込
    chain (Wk.CardNo) CDREVF;
    if %found(CDREVF);
      Wk.RevFound = *on;
    else;
      Wk.ResultCd = '92';
      Wk.ReasonCd = 'NORB';
      Wk.StmtStatus = 'S';
      Wk.Msg = 'リボ契約未登録';
      exsr SetReturn;
      return;
    endif;

    // 直近リボ残高読込
    chain (Wk.CardNo) CDRBALF;
    if %found(CDRBALF);
      Wk.RbalFound = *on;
      Wk.RevBal = RS-REV-BAL;
      Wk.SlideTier = RS-SLIDE-TIER;
    else;
      Wk.RbalFound = *off;
      Wk.RevBal = 0;
      Wk.SlideTier = 'T1';
    endif;

    // リボ状態判定
    select;
    when RV-REV-STATUS = '01';
      Wk.StmtStatus = 'C';

    when RV-REV-STATUS = '02';
      Wk.ResultCd = '10';
      Wk.ReasonCd = 'RVSP';
      Wk.StmtStatus = 'S';
      Wk.FeeAmt = 0;
      Wk.Msg = 'リボ一時停止';
      exsr CalcAvailable;
      exsr SetReturn;
      return;

    when RV-REV-STATUS = '03';
      Wk.ResultCd = '10';
      Wk.ReasonCd = 'RVCN';
      Wk.StmtStatus = 'S';
      Wk.FeeAmt = 0;
      Wk.Msg = 'リボ解約';
      exsr CalcAvailable;
      exsr SetReturn;
      return;

    other;
      Wk.ResultCd = '10';
      Wk.ReasonCd = 'RVST';
      Wk.StmtStatus = 'S';
      Wk.FeeAmt = 0;
      Wk.Msg = 'リボ状態不正';
      exsr CalcAvailable;
      exsr SetReturn;
      return;
    endsl;

    // 枠閉塞判定
    if CL-LIMIT-STATUS = '9';
      Wk.ResultCd = '10';
      Wk.ReasonCd = 'LMCL';
      Wk.StmtStatus = 'S';
      Wk.FeeAmt = 0;
      Wk.Msg = '利用枠閉塞';
      exsr CalcAvailable;
      exsr SetReturn;
      return;
    endif;

    // 通常照会
    exsr CalcFee;
    exsr CalcAvailable;

    if Wk.AvailTotal <= 0;
      Wk.ResultCd = '11';
      Wk.ReasonCd = 'OVTL';
      Wk.Msg = '総枠余裕なし';
    elseif Wk.AvailRev <= 0;
      Wk.ResultCd = '12';
      Wk.ReasonCd = 'OVRV';
      Wk.Msg = 'リボ枠余裕なし';
    else;
      Wk.ResultCd = '00';
      Wk.ReasonCd = 'OK  ';
      Wk.Msg = '照会正常';
    endif;

    exsr SetReturn;

  on-error;
    Wk.ResultCd = '99';
    Wk.ReasonCd = 'SYS ';
    Wk.StmtStatus = 'S';
    Wk.Msg = 'オンライン照会異常';
    exsr SetReturn;
  endmon;

  return;

  // リボ手数料算出（月利 0.0125、円未満切捨て）
  begsr CalcFee;
    if Wk.RevBal > 0 and RV-REV-STATUS = '01';
      Wk.FeeWork = Wk.RevBal * Wk.MonthRate;
      Wk.FeeAmt = %int(Wk.FeeWork);
    else;
      Wk.FeeAmt = 0;
    endif;
  endsr;

  // 利用可能額算出
  begsr CalcAvailable;
    Wk.AvailTotal = Wk.TotalLimit - Wk.HoldAmt - Wk.RevBal;
    Wk.AvailRev   = Wk.RevLimit   - Wk.RevBal - Wk.FeeAmt;

    if Wk.AvailTotal < 0;
      Wk.AvailTotal = 0;
    endif;

    if Wk.AvailRev < 0;
      Wk.AvailRev = 0;
    endif;

    select;
    when Wk.AvailRev >= 300000;
      Wk.SlideTier = 'T1';
    when Wk.AvailRev >= 100000;
      Wk.SlideTier = 'T2';
    when Wk.AvailRev > 0;
      Wk.SlideTier = 'T3';
    other;
      Wk.SlideTier = 'T4';
    endsl;

    if Wk.StmtStatus <> 'S';
      Wk.StmtStatus = 'C';
    endif;
  endsr;

  // 返却領域設定
  begsr SetReturn;
    pOutResultCd   = Wk.ResultCd;
    pOutReasonCd   = Wk.ReasonCd;
    pOutTotalLimit = Wk.TotalLimit;
    pOutRevLimit   = Wk.RevLimit;
    pOutHoldAmt    = Wk.HoldAmt;
    pOutRevBal     = Wk.RevBal;
    pOutFeeAmt     = Wk.FeeAmt;
    pOutAvailTotal = Wk.AvailTotal;
    pOutAvailRev   = Wk.AvailRev;
    pOutSlideTier  = Wk.SlideTier;
    pOutStmtStatus = Wk.StmtStatus;
    pOutMsg        = Wk.Msg;
  endsr;

end-proc;
