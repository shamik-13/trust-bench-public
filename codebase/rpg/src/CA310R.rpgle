**free
ctl-opt dftactgrp(*no) actgrp('CA310R') option(*srcstmt:*nodebugio)
        main(Main);

/copy CDCAPFC
/copy CDMERCC
/copy CDCARDFC3
/copy CDCBKPC
/copy CDEXCPC

//**********************************************************************
//  変更履歴
//  版数   年月日      担当     概要
//  1.00   2024/04/12  K.SATO   初版作成
//  1.10   2024/09/03  M.IKEDA  返品済・精算取消疑いの除外記録追加
//  1.20   2025/02/18  K.SATO   外貨売上の事務手数料判定追加
//**********************************************************************

dcl-f CDCAPF  usage(*input)  keyed;
dcl-f CDMERCPF usage(*input) keyed;
dcl-f CDCARDPF usage(*input) keyed;
dcl-f CDCBKPF usage(*output:*update) keyed;
dcl-f CDEXCPF usage(*output:*update) keyed;

dcl-pr CA120R extpgm('CA120R');
  pCardNo       char(16) const;
  pAuthAmt      packed(13:0) const;
  pCurrCd       char(3) const;
  pAuthKbn      char(1) const;
  pAuthResult   char(1);
  pAuthCode     char(6);
  pAuthMsg      char(60);
end-pr;

dcl-pi *n;
  iSaleId       char(18) const;
  iCardNo       char(16) const;
  iMercId       char(10) const;
  iClaimKbn     char(2) const;
  iClaimAmt     packed(13:0) const;
  iClaimDate    packed(8:0) const;
  iOpeId        char(10) const;
  oResult       char(1);
  oCaseNo       char(12);
  oMsg          char(80);
end-pi;

dcl-ds CapRec likerec(CDCAPR:*input) qualified end-ds;
dcl-ds MercRec likerec(CDMERCR:*input) qualified end-ds;
dcl-ds CardRec likerec(CDCARDR:*input) qualified end-ds;
dcl-ds CbkRec likerec(CDCBKPR:*output) qualified end-ds;
dcl-ds ExcRec likerec(CDEXCPR:*output) qualified end-ds;

dcl-ds Work qualified;
  today         packed(8:0);
  time          packed(6:0);
  caseNo        char(12);
  excNo         char(12);
  authResult    char(1);
  authCode      char(6);
  authMsg       char(60);
  feeAmt        packed(13:0);
  baseAmt       packed(13:0);
  matchOk       ind;
  dupAdjust     ind;
  existsCbk     ind;
end-ds;

dcl-s MsgText       char(80);
dcl-s MaxDiffAmt    packed(7:0) inz(100);
dcl-s SeqNo         packed(7:0);
dcl-s DiffAmt       packed(13:0);

// 取込対象コード
dcl-c CARD_STS_VALID    '01';   // 有効
dcl-c CARD_STS_STOP     '02';   // 利用停止
dcl-c CARD_STS_CLOSE    '03';   // 解約
dcl-c CARD_STS_DELINQ   '09';   // 延滞

dcl-c CAP_STS_FIXED     'C';    // 確定
dcl-c CAP_STS_SKIP      'S';    // 対象外
dcl-c CAP_STS_HOLD      'H';    // 保留

dcl-c CURR_JPY          'JPY';

dcl-proc Main;

  oResult = '9';
  oCaseNo = *blanks;
  oMsg    = *blanks;

  Work.today = %dec(%date():*iso);
  Work.time  = %dec(%time():*hms);

  monitor;

    exsr InitWork;
    exsr ReadSale;
    exsr CheckClaim;

    if oResult <> '9';
      return;
    endif;

    if Work.dupAdjust;
      exsr WriteException;
      oResult = '2';
      oCaseNo = Work.excNo;
      oMsg = '二重調整候補として例外登録しました';
      return;
    endif;

    callp CA120R(iCardNo:
                 iClaimAmt:
                 CapRec.BC-CURR-CD:
                 'C':
                 Work.authResult:
                 Work.authCode:
                 Work.authMsg);

    if Work.authResult <> 'A';
      oResult = '8';
      oMsg = '受付時オーソリ確認不可 ' + %trim(Work.authMsg);
      return;
    endif;

    exsr WriteChargeback;

    oResult = '0';
    oCaseNo = Work.caseNo;
    oMsg = 'チャージバック案件を作成しました';

  on-error;
    oResult = '9';
    oMsg = 'CA310R 異常終了 売上ID=' + %trim(iSaleId);
  endmon;

  return;

  begsr InitWork;

    clear Work;
    Work.today = %dec(%date():*iso);
    Work.time  = %dec(%time():*hms);

    if iSaleId = *blanks
       or iCardNo = *blanks
       or iMercId = *blanks
       or iClaimAmt <= 0;
      oResult = '8';
      oMsg = '申立入力内容が不足しています';
    endif;

  endsr;

  begsr ReadSale;

    if oResult <> '9';
      leavesr;
    endif;

    chain iSaleId CDCAPF CapRec;
    if not %found(CDCAPF);
      oResult = '8';
      oMsg = '売上IDが存在しません';
      leavesr;
    endif;

    chain CapRec.BC-MERC-ID CDMERCPF MercRec;
    if not %found(CDMERCPF);
      oResult = '8';
      oMsg = '加盟店マスタが存在しません';
      leavesr;
    endif;

    chain iCardNo CDCARDPF CardRec;
    if not %found(CDCARDPF);
      oResult = '8';
      oMsg = 'カード会員が存在しません';
      leavesr;
    endif;

  endsr;

  begsr CheckClaim;

    if oResult <> '9';
      leavesr;
    endif;

    Work.matchOk = *on;

    if CapRec.BC-CARD-NO <> iCardNo;
      Work.matchOk = *off;
      MsgText = 'カード番号が売上と一致しません';
    elseif CapRec.BC-MERC-ID <> iMercId;
      Work.matchOk = *off;
      MsgText = '加盟店番号が売上と一致しません';
    elseif CardRec.CF-CARD-STATUS <> CARD_STS_VALID;
      Work.matchOk = *off;
      select;
      when CardRec.CF-CARD-STATUS = CARD_STS_STOP;
        MsgText = 'カード状態が利用停止です';
      when CardRec.CF-CARD-STATUS = CARD_STS_CLOSE;
        MsgText = 'カード状態が解約です';
      when CardRec.CF-CARD-STATUS = CARD_STS_DELINQ;
        MsgText = 'カード状態が延滞です';
      other;
        MsgText = 'カード状態が受付対象外です';
      endsl;
    elseif CapRec.BC-CAP-STATUS <> CAP_STS_FIXED;
      Work.matchOk = *off;
      select;
      when CapRec.BC-CAP-STATUS = CAP_STS_SKIP;
        MsgText = '売上が対象外です';
      when CapRec.BC-CAP-STATUS = CAP_STS_HOLD;
        MsgText = '売上が保留中です';
      other;
        MsgText = '売上確定状態ではありません';
      endsl;
    endif;

    if not Work.matchOk;
      oResult = '8';
      oMsg = MsgText;
      leavesr;
    endif;

    // 売上確定時に算定済の事務手数料を引き継ぐ。当案件登録では
    // 海外利用料率を持たず、確定済の BC-FEE-AMT をそのまま用いる。
    Work.feeAmt  = CapRec.BC-FEE-AMT;
    Work.baseAmt = CapRec.BC-SALE-AMT + Work.feeAmt;

    DiffAmt = %abs(Work.baseAmt - iClaimAmt);
    if DiffAmt > MaxDiffAmt;
      oResult = '8';
      oMsg = '申立金額が売上金額と一致しません';
      leavesr;
    endif;

    Work.dupAdjust = *off;

    if CapRec.BC-RETURN-FLG = '1'
       or CapRec.BC-RETURN-DATE <> 0
       or CapRec.BC-SETL-CAN-FLG = '1';
      Work.dupAdjust = *on;
    endif;

    chain iSaleId CDCBKPF CbkRec;
    if %found(CDCBKPF);
      Work.existsCbk = *on;
      Work.dupAdjust = *on;
    endif;

  endsr;

  begsr WriteException;

    exsr MakeExcNo;

    clear ExcRec;
    ExcRec.EX-EXC-NO      = Work.excNo;
    ExcRec.EX-SALE-ID     = iSaleId;
    ExcRec.EX-CARD-NO     = iCardNo;
    ExcRec.EX-MERC-ID     = iMercId;
    ExcRec.EX-CLAIM-KBN   = iClaimKbn;
    ExcRec.EX-CLAIM-AMT   = iClaimAmt;
    ExcRec.EX-CLAIM-DATE  = iClaimDate;
    ExcRec.EX-EXC-KBN     = 'D';
    ExcRec.EX-EXC-STS     = '0';
    ExcRec.EX-CAP-AMT     = CapRec.BC-SALE-AMT;
    ExcRec.EX-CURR-CD     = CapRec.BC-CURR-CD;
    ExcRec.EX-BASE-AMT    = Work.baseAmt;
    ExcRec.EX-FEE-AMT     = Work.feeAmt;

    if Work.existsCbk;
      ExcRec.EX-REASON-CD = 'CBK';
      ExcRec.EX-OP-TEXT   = '既存チャージバック案件あり';
    elseif CapRec.BC-RETURN-FLG = '1'
       or CapRec.BC-RETURN-DATE <> 0;
      ExcRec.EX-REASON-CD = 'RTN';
      ExcRec.EX-OP-TEXT   = '返品済み疑い';
    else;
      ExcRec.EX-REASON-CD = 'STC';
      ExcRec.EX-OP-TEXT   = '加盟店精算取消済み疑い';
    endif;

    ExcRec.EX-REG-DATE    = Work.today;
    ExcRec.EX-REG-TIME    = Work.time;
    ExcRec.EX-REG-USER    = iOpeId;
    ExcRec.EX-UPD-DATE    = Work.today;
    ExcRec.EX-UPD-TIME    = Work.time;
    ExcRec.EX-UPD-USER    = iOpeId;

    write CDEXCPR ExcRec;

  endsr;

  begsr WriteChargeback;

    exsr MakeCaseNo;

    clear CbkRec;
    CbkRec.CB-CASE-NO     = Work.caseNo;
    CbkRec.CB-SALE-ID     = iSaleId;
    CbkRec.CB-CARD-NO     = iCardNo;
    CbkRec.CB-MERC-ID     = iMercId;
    CbkRec.CB-MERC-NAME   = MercRec.MF-MERC-NAME;
    CbkRec.CB-CLAIM-KBN   = iClaimKbn;
    CbkRec.CB-CLAIM-AMT   = iClaimAmt;
    CbkRec.CB-CLAIM-DATE  = iClaimDate;
    CbkRec.CB-SALE-DATE   = CapRec.BC-SALE-DATE;
    CbkRec.CB-SALE-AMT    = CapRec.BC-SALE-AMT;
    CbkRec.CB-CURR-CD     = CapRec.BC-CURR-CD;
    CbkRec.CB-BASE-AMT    = Work.baseAmt;
    CbkRec.CB-FEE-AMT     = Work.feeAmt;
    CbkRec.CB-ACQ-ID      = CapRec.BC-ACQ-ID;
    CbkRec.CB-TERM-ID     = CapRec.BC-TERM-ID;
    CbkRec.CB-AUTH-CD     = Work.authCode;
    CbkRec.CB-CASE-STS    = '0';
    CbkRec.CB-RECV-DATE   = Work.today;
    CbkRec.CB-RECV-TIME   = Work.time;
    CbkRec.CB-RECV-USER   = iOpeId;
    CbkRec.CB-REG-DATE    = Work.today;
    CbkRec.CB-REG-TIME    = Work.time;
    CbkRec.CB-REG-USER    = iOpeId;
    CbkRec.CB-UPD-DATE    = Work.today;
    CbkRec.CB-UPD-TIME    = Work.time;
    CbkRec.CB-UPD-USER    = iOpeId;

    write CDCBKPR CbkRec;

  endsr;

  begsr MakeCaseNo;

    SeqNo = %rem(%dec(%timestamp():*mseconds):7:0) + 1;
    Work.caseNo = 'CB' +
                  %char(%subst(%char(Work.today):3:6)) +
                  %editc(SeqNo:'X');

  endsr;

  begsr MakeExcNo;

    SeqNo = %rem(%dec(%timestamp():*mseconds):7:0) + 1;
    Work.excNo = 'EX' +
                 %char(%subst(%char(Work.today):3:6)) +
                 %editc(SeqNo:'X');

  endsr;

end-proc;
