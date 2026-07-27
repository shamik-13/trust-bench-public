**free
//**********************************************************************
//  変更履歴
//  版数    年月日      担当      概要
//  01.00   2024.02.14  山下      新規作成 返金承認画面
//  01.01   2024.05.09  杉本      口座状態による抽出条件を追加
//  01.02   2024.11.21  山下      名義照合結果の確認を承認時に追加
//**********************************************************************
ctl-opt dftactgrp(*no) actgrp(*caller)
        option(*srcstmt:*nodebugio)
        main(CA105R);

 /copy QRPGLESRC,CDREFDC
 /copy QRPGLESRC,CDACTC
 /copy QRPGLESRC,CDHISTC

dcl-pr CA105R extpgm('CA105R');
  pInUser        char(10) const;
  pInTerm        char(10) const;
  pInBranch      char(3)  const;
  pInAuthNo      char(12) const;
  pOutSts        char(1);
end-pr;

dcl-pr RefundDecisionService extpgm('RFDSVC');
  pSvcReq        likeds(DecisionReq);
  pSvcRes        likeds(DecisionRes);
end-pr;

dcl-ds DecisionReq qualified inz;
  cardNo         char(16);
  refundNo       char(12);
  orgAuthNo      char(12);
  acctNo         char(14);
  refundAmt      packed(11:0);
  orgSaleAmt     packed(11:0);
  elapsedDays    packed(5:0);
  acctStatus     char(1);
  nameMatch      char(1);
  operId         char(10);
  branchCd       char(3);
end-ds;

dcl-ds DecisionRes qualified inz;
  resultCd       char(1);
  reasonCd       char(3);
  authMsg        char(60);
  riskScore      packed(5:0);
end-ds;

dcl-ds Screen qualified inz;
  minAmt         packed(11:0);
  maxAmt         packed(11:0);
  fromDays       packed(5:0);
  toDays         packed(5:0);
  acctStatus     char(1);
  selRefundNo    char(12);
  actionCd       char(1);
  holdReason     char(3);
  msgId          char(7);
  msgText        char(60);
end-ds;

dcl-ds Work qualified inz;
  authNo         char(12);
  refundNo       char(12);
  cardNo         char(16);
  maskedAcct     char(18);
  maskedCard     char(19);
  acctNameKana   char(40);
  reqNameKana    char(40);
  nameMatch      char(1);
  acctStatus     char(1);
  refundAmt      packed(11:0);
  orgSaleAmt     packed(11:0);
  elapsedDays    packed(5:0);
  approveLimit   packed(11:0);
  refundTotal    packed(11:0);
  holdReason     char(3);
  histEvent      char(2);
  errText        char(60);
  eof            ind;
  found          ind;
  confirmed      ind;
end-ds;

dcl-s pInUser        char(10);
dcl-s pInTerm        char(10);
dcl-s pInBranch      char(3);
dcl-s pInAuthNo      char(12);
dcl-s pOutSts        char(1);

dcl-s ix             packed(3:0);
dcl-s wkToday        zoned(8:0);
dcl-s wkYYYYMMDD     char(8);
dcl-s wkTime         zoned(6:0);
dcl-s wkReasonText   char(40);

dcl-proc CA105R;
  dcl-pi *n;
    pInUser        char(10) const;
    pInTerm        char(10) const;
    pInBranch      char(3)  const;
    pInAuthNo      char(12) const;
    pOutSts        char(1);
  end-pi;

  pOutSts = '0';
  Work.authNo = pInAuthNo;
  Work.approveLimit = 300000;

  exsr InitScreen;
  exsr LoadCandidate;

  dow not Work.eof;

    exsr BuildMask;
    exsr CheckNameKana;

    if not IsTarget();
      exsr ReadNext;
      iter;
    endif;

    exsr ShowDetail;

    select;
    when Screen.actionCd = '1';
      exsr ApproveRefund;

    when Screen.actionCd = '2';
      exsr HoldRefund;

    when Screen.actionCd = '9';
      leave;

    other;
      Screen.msgId = 'CA10501';
      Screen.msgText = '処理区分が不正です';
    endsl;

    exsr ReadNext;
  enddo;

  *inlr = *on;
  return;

  begsr InitScreen;
    clear Screen;
    clear Work;
    Work.authNo = pInAuthNo;
    Screen.minAmt = 1;
    Screen.maxAmt = 99999999999;
    Screen.fromDays = 0;
    Screen.toDays = 180;
    Screen.acctStatus = '1';
    Screen.actionCd = *blank;
    Screen.holdReason = *blank;
    wkYYYYMMDD = %char(%date():*iso0);
    wkToday = %dec(wkYYYYMMDD:8:0);
    wkTime = %dec(%char(%time():*hms0):6:0);
  endsr;

  begsr LoadCandidate;
    Work.eof = *off;
    Work.found = *off;

    monitor;
      chain Work.authNo CDREFR;
      if %found(CDREFR);
        Work.refundNo = RFDRFN;
        Work.cardNo = RFDCNO;
        Work.refundAmt = RFDRAM;
        Work.orgSaleAmt = RFDSAM;
        Work.elapsedDays = %diff(%date(): %date(%char(RFDODT):*iso0): *days);
        Work.acctStatus = RFDACS;
        Work.reqNameKana = RFDNME;
        Work.found = *on;
      else;
        Work.eof = *on;
        pOutSts = '1';
        Screen.msgId = 'CA10502';
        Screen.msgText = '返金候補が存在しません';
      endif;
    on-error;
      Work.eof = *on;
      pOutSts = '9';
      Screen.msgId = 'CA10590';
      Screen.msgText = '返金候補読込で異常が発生しました';
    endmon;
  endsr;

  begsr ReadNext;
    Work.eof = *on;
  endsr;

  begsr BuildMask;
    clear Work.maskedCard;
    clear Work.maskedAcct;

    Work.maskedCard = %subst(Work.cardNo:1:6) + '-******-' +
                      %subst(Work.cardNo:13:4);

    chain RFDACT CDACTR;
    if %found(CDACTR);
      Work.maskedAcct = %subst(ACTBNO:1:4) + '-****-' +
                        %subst(ACTANO:11:4);
      Work.acctNameKana = ACTNME;
      Work.acctStatus = ACTSTS;
    else;
      Work.maskedAcct = '****-****-****';
      Work.acctNameKana = *blank;
      Work.acctStatus = '9';
    endif;
  endsr;

  begsr CheckNameKana;
    Work.nameMatch = '0';

    if %trim(Work.reqNameKana) = %trim(Work.acctNameKana);
      Work.nameMatch = '1';
    elseif %scan(%trim(Work.reqNameKana): %trim(Work.acctNameKana)) > 0;
      Work.nameMatch = '2';
    endif;
  endsr;

  begsr ShowDetail;
    // 実画面では候補明細を表示し、担当者入力を受ける
    if Screen.actionCd = *blank;
      Screen.selRefundNo = Work.refundNo;
      Screen.actionCd = '2';
      Screen.holdReason = '091';
    endif;
  endsr;

  begsr ApproveRefund;
    if Work.refundAmt <= 0;
      Screen.msgId = 'CA10511';
      Screen.msgText = '返金金額が不正です';
      return;
    endif;

    if Work.refundAmt > Work.orgSaleAmt;
      Screen.msgId = 'CA10512';
      Screen.msgText = '売上金額を超過しています';
      return;
    endif;

    if Work.refundAmt > Work.approveLimit;
      Screen.msgId = 'CA10513';
      Screen.msgText = '担当者承認限度額を超過しています';
      return;
    endif;

    if Work.acctStatus <> '1';
      Screen.msgId = 'CA10514';
      Screen.msgText = '返金先口座が有効ではありません';
      return;
    endif;

    if Work.nameMatch <> '1';
      Screen.msgId = 'CA10515';
      Screen.msgText = '名義照合結果を確認してください';
      return;
    endif;

    clear DecisionReq;
    clear DecisionRes;

    DecisionReq.cardNo      = Work.cardNo;
    DecisionReq.refundNo    = Work.refundNo;
    DecisionReq.orgAuthNo   = Work.authNo;
    DecisionReq.acctNo      = RFDACT;
    DecisionReq.refundAmt   = Work.refundAmt;
    DecisionReq.orgSaleAmt  = Work.orgSaleAmt;
    DecisionReq.elapsedDays = Work.elapsedDays;
    DecisionReq.acctStatus  = Work.acctStatus;
    DecisionReq.nameMatch   = Work.nameMatch;
    DecisionReq.operId      = pInUser;
    DecisionReq.branchCd    = pInBranch;

    monitor;
      callp RefundDecisionService(DecisionReq: DecisionRes);
    on-error;
      Screen.msgId = 'CA10591';
      Screen.msgText = '返金判定サービス呼出で異常が発生しました';
      pOutSts = '9';
      return;
    endmon;

    if DecisionRes.resultCd <> 'A';
      Screen.msgId = 'CA10516';
      Screen.msgText = '返金承認不可 ' + %trim(DecisionRes.reasonCd);
      exsr WriteHoldHistory;
      return;
    endif;

    monitor;
      chain Work.refundNo CDREFR;
      if %found(CDREFR);
        RFDSTS = 'A';
        RFDAPR = pInUser;
        RFDADT = wkToday;
        RFDATM = wkTime;
        RFDNAM = Work.nameMatch;
        update CDREFR;
      endif;

      Work.histEvent = '承';
      exsr WriteHistory;

      pOutSts = '0';
      Screen.msgId = 'CA10500';
      Screen.msgText = '返金を承認しました';
    on-error;
      Screen.msgId = 'CA10592';
      Screen.msgText = '返金承認更新で異常が発生しました';
      pOutSts = '9';
    endmon;
  endsr;

  begsr HoldRefund;
    if Screen.holdReason = *blank;
      Screen.msgId = 'CA10521';
      Screen.msgText = '保留理由を入力してください';
      return;
    endif;

    Work.holdReason = Screen.holdReason;

    monitor;
      chain Work.refundNo CDREFR;
      if %found(CDREFR);
        RFDSTS = 'H';
        RFDHLD = Work.holdReason;
        RFDUPD = wkToday;
        RFDUPT = wkTime;
        RFDUPR = pInUser;
        update CDREFR;
      endif;

      Work.histEvent = '保';
      exsr WriteHistory;

      pOutSts = '0';
      Screen.msgId = 'CA10520';
      Screen.msgText = '返金を保留しました';
    on-error;
      Screen.msgId = 'CA10593';
      Screen.msgText = '返金保留更新で異常が発生しました';
      pOutSts = '9';
    endmon;
  endsr;

  begsr WriteHoldHistory;
    Work.holdReason = DecisionRes.reasonCd;
    Work.histEvent = '否';
    exsr WriteHistory;
  endsr;

  begsr WriteHistory;
    clear CDHIST;

    HSTCNO = Work.cardNo;
    HSTRFN = Work.refundNo;
    HSTANO = Work.authNo;
    HSTEDT = wkToday;
    HSTETM = wkTime;
    HSTUSR = pInUser;
    HSTTRM = pInTerm;
    HSTBRN = pInBranch;
    HSTEVT = Work.histEvent;
    HSTAMT = Work.refundAmt;
    HSTSTS = Work.acctStatus;
    HSTNAM = Work.nameMatch;
    HSTRSN = Work.holdReason;

    select;
    when Work.histEvent = '承';
      wkReasonText = '返金承認';
    when Work.histEvent = '保';
      wkReasonText = '返金保留';
    when Work.histEvent = '否';
      wkReasonText = '自動判定否決';
    other;
      wkReasonText = '返金処理';
    endsl;

    HSTTXT = wkReasonText + ' ' + %trim(Work.maskedAcct);

    write CDHIST;
  endsr;

end-proc;

dcl-proc IsTarget;
  dcl-pi *n ind;
  end-pi;

  if Work.refundAmt < Screen.minAmt;
    return *off;
  endif;

  if Work.refundAmt > Screen.maxAmt;
    return *off;
  endif;

  if Work.elapsedDays < Screen.fromDays;
    return *off;
  endif;

  if Work.elapsedDays > Screen.toDays;
    return *off;
  endif;

  if Screen.acctStatus <> *blank
     and Work.acctStatus <> Screen.acctStatus;
    return *off;
  endif;

  if Work.refundAmt = Work.orgSaleAmt
     and Work.elapsedDays > 90;
    return *off;
  endif;

  return *on;
end-proc;
