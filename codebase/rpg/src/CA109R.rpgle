**free
//**********************************************************************
//*  変更履歴
//*  版数  年月日    担当   概要
//*  1.00  20190513  S.TAN  新規作成
//*  1.10  20210308  M.KOU  コース変更時の未到来締日判定を追加
//*  1.20  20240617  R.NAK  REV状態別の受付抑止と監査項目を追加
//**********************************************************************
ctl-opt dftactgrp(*no) actgrp(*caller)
        option(*srcstmt:*nodebugio)
        datfmt(*iso) timfmt(*iso)
        bnddir('QC2LE');

//**********************************************************************
//*  みらいカード  オーソリ系
//*  CA109R  リボ申込・コース変更受付
//**********************************************************************

/copy QRPGLESRC,CDMEMC
/copy QRPGLESRC,CDREVFC
/copy QRPGLESRC,CDCOURC

dcl-pr CA109R extpgm('CA109R');
  pInMemberNo     char(12) const;
  pInAcceptKbn    char(01) const;
  pInCourseCd     char(04) const;
  pInTermId       char(08) const;
  pInOperId       char(10) const;
  pOutRtnCd       char(02);
  pOutReasonCd    char(04);
  pOutApplyDt     packed(8:0);
  pOutMessage     char(80);
end-pr;

dcl-pi CA109R;
  pInMemberNo     char(12) const;
  pInAcceptKbn    char(01) const;
  pInCourseCd     char(04) const;
  pInTermId       char(08) const;
  pInOperId       char(10) const;
  pOutRtnCd       char(02);
  pOutReasonCd    char(04);
  pOutApplyDt     packed(8:0);
  pOutMessage     char(80);
end-pi;

dcl-ds Inp qualified;
  memberNo        char(12);
  acceptKbn       char(01);
  courseCd        char(04);
  termId          char(08);
  operId          char(10);
end-ds;

dcl-ds Wk qualified;
  sysDate         date;
  sysTime         time;
  firstDay        date;
  closeDate       date;
  payDate         date;
  ymd             packed(8:0);
  nextCloseDt     packed(8:0);
  nextPayDt       packed(8:0);
  baseApplyDt     packed(8:0);
  lastCloseDt     packed(8:0);
  oldCourseCd     char(04);
  oldStatus       char(02);
  oldStartDt      packed(8:0);
  oldLimit        packed(11:0);
  courseLimit     packed(11:0);
  useableLimit    packed(11:0);
  revBal          packed(13:0);
  feeAmt          packed(13:0);
  prinAmt         packed(13:0);
  payAmt          packed(13:0);
  dateRtn         char(02);
  msg             char(80);
  foundMember     ind inz(*off);
  foundRev        ind inz(*off);
  foundCourse     ind inz(*off);
  updateOk        ind inz(*off);
  skipSw          ind inz(*off);
end-ds;

dcl-ds Audit qualified;
  pgmId           char(10) inz('CA109R');
  memberNo        char(12);
  acceptKbn       char(01);
  beforeStatus    char(02);
  beforeCourse    char(04);
  afterStatus     char(02);
  afterCourse     char(04);
  applyDt         packed(8:0);
  reasonCd        char(04);
  termId          char(08);
  operId          char(10);
  updYmd          packed(8:0);
  updHms          packed(6:0);
end-ds;

dcl-s C_STS_ACTIVE      char(02) inz('01');
dcl-s C_STS_HOLD        char(02) inz('02');
dcl-s C_STS_CANCEL      char(02) inz('03');

dcl-s C_TIER_1          char(02) inz('T1');
dcl-s C_TIER_2          char(02) inz('T2');
dcl-s C_TIER_3          char(02) inz('T3');
dcl-s C_TIER_4          char(02) inz('T4');

dcl-s C_RSLD_CONF       char(01) inz('C');
dcl-s C_RSLD_SKIP       char(01) inz('S');

dcl-s C_ACCEPT_NEW      char(01) inz('1');
dcl-s C_ACCEPT_CHG      char(01) inz('2');

dcl-s C_MONTH_RATE      packed(5:4) inz(0.0125);

dcl-s ZeroAmt           packed(13:0) inz(0);
dcl-s i                 int(10);

exec sql
  set option commit = *chg, closqlcsr = *endmod, datfmt = *iso;

//**********************************************************************
//*  初期化
//**********************************************************************
clear pOutRtnCd;
clear pOutReasonCd;
clear pOutApplyDt;
clear pOutMessage;

Inp.memberNo  = pInMemberNo;
Inp.acceptKbn = pInAcceptKbn;
Inp.courseCd  = pInCourseCd;
Inp.termId    = pInTermId;
Inp.operId    = pInOperId;

Wk.sysDate = %date();
Wk.sysTime = %time();
Wk.ymd     = %dec(Wk.sysDate:*iso);

pOutRtnCd    = '99';
pOutReasonCd = 'E999';
pOutMessage  = '受付処理を開始しました。';

Audit.memberNo = Inp.memberNo;
Audit.acceptKbn = Inp.acceptKbn;
Audit.termId = Inp.termId;
Audit.operId = Inp.operId;
Audit.updYmd = Wk.ymd;
Audit.updHms = %dec(%char(Wk.sysTime:*hms0):6:0);

//**********************************************************************
//*  入力妥当性
//**********************************************************************
dou *on;

  if %trim(Inp.memberNo) = *blank;
    pOutRtnCd = '10';
    pOutReasonCd = 'M001';
    pOutMessage = '会員番号が未入力です。';
    leave;
  endif;

  if Inp.acceptKbn <> C_ACCEPT_NEW and Inp.acceptKbn <> C_ACCEPT_CHG;
    pOutRtnCd = '10';
    pOutReasonCd = 'M002';
    pOutMessage = '受付区分が不正です。';
    leave;
  endif;

  if %trim(Inp.courseCd) = *blank;
    pOutRtnCd = '10';
    pOutReasonCd = 'M003';
    pOutMessage = 'リボコースが未入力です。';
    leave;
  endif;

  exsr LoadMember;
  if not Wk.foundMember;
    pOutRtnCd = '20';
    pOutReasonCd = 'M101';
    pOutMessage = '会員情報が存在しません。';
    leave;
  endif;

  select;
  when CM-MEMBER-STS = '0';
    pOutRtnCd = '21';
    pOutReasonCd = 'M102';
    pOutMessage = '会員状態が未発行のため受付できません。';
    leave;
  when CM-MEMBER-STS = '2';
    pOutRtnCd = '21';
    pOutReasonCd = 'M103';
    pOutMessage = '会員状態が停止中のため受付できません。';
    leave;
  when CM-MEMBER-STS = '9';
    pOutRtnCd = '21';
    pOutReasonCd = 'M104';
    pOutMessage = '退会済会員のため受付できません。';
    leave;
  other;
  endsl;

  if CM-LOST-FLG = '1' or CM-FRAUD-FLG = '1';
    pOutRtnCd = '21';
    pOutReasonCd = 'M105';
    pOutMessage = '事故登録中のため受付できません。';
    leave;
  endif;

  exsr LoadRev;
  if not Wk.foundRev and Inp.acceptKbn = C_ACCEPT_CHG;
    pOutRtnCd = '22';
    pOutReasonCd = 'R001';
    pOutMessage = 'リボ契約が未登録のため変更できません。';
    leave;
  endif;

  if Wk.foundRev;
    Audit.beforeStatus = RV-REV-STATUS;
    Audit.beforeCourse = RV-REV-COURSE-CD;
    Wk.oldStatus = RV-REV-STATUS;
    Wk.oldCourseCd = RV-REV-COURSE-CD;
    Wk.oldStartDt = RV-REV-START-DT;
    Wk.oldLimit = RV-REV-LIMIT-AMT;

    select;
    when RV-REV-STATUS = C_STS_ACTIVE;
    when RV-REV-STATUS = C_STS_HOLD;
      Wk.skipSw = *on;
      Wk.prinAmt = ZeroAmt;
      Wk.feeAmt = ZeroAmt;
      Wk.payAmt = ZeroAmt;
      pOutRtnCd = '23';
      pOutReasonCd = 'R002';
      pOutMessage = 'リボ状態が一時停止のため受付できません。';
      leave;
    when RV-REV-STATUS = C_STS_CANCEL;
      Wk.skipSw = *on;
      Wk.prinAmt = ZeroAmt;
      Wk.feeAmt = ZeroAmt;
      Wk.payAmt = ZeroAmt;
      pOutRtnCd = '23';
      pOutReasonCd = 'R003';
      pOutMessage = 'リボ状態が解約のため受付できません。';
      leave;
    other;
      pOutRtnCd = '23';
      pOutReasonCd = 'R004';
      pOutMessage = 'リボ状態が不正です。';
      leave;
    endsl;

    if Inp.acceptKbn = C_ACCEPT_NEW and RV-REV-STATUS = C_STS_ACTIVE;
      pOutRtnCd = '24';
      pOutReasonCd = 'R005';
      pOutMessage = 'リボ契約登録済のため新規申込できません。';
      leave;
    endif;
  endif;

  exsr LoadCourse;
  if not Wk.foundCourse;
    pOutRtnCd = '30';
    pOutReasonCd = 'C001';
    pOutMessage = 'リボコースが存在しません。';
    leave;
  endif;

  if RC-COURSE-STS <> '1';
    pOutRtnCd = '30';
    pOutReasonCd = 'C002';
    pOutMessage = 'リボコースが停止中です。';
    leave;
  endif;

  if RC-START-DT > Wk.ymd or
     (RC-END-DT <> 0 and RC-END-DT < Wk.ymd);
    pOutRtnCd = '30';
    pOutReasonCd = 'C003';
    pOutMessage = 'リボコースの適用期間外です。';
    leave;
  endif;

  select;
  when RC-SLIDE-TIER = C_TIER_1
    or RC-SLIDE-TIER = C_TIER_2
    or RC-SLIDE-TIER = C_TIER_3
    or RC-SLIDE-TIER = C_TIER_4;
    // コース限度額はリボコースマスタの登録値を採用する。
    Wk.courseLimit = RC-LIMIT-AMT;
  other;
    pOutRtnCd = '30';
    pOutReasonCd = 'C004';
    pOutMessage = 'スライド区分が不正です。';
    leave;
  endsl;

  if Wk.courseLimit <= 0;
    pOutRtnCd = '30';
    pOutReasonCd = 'C004';
    pOutMessage = 'コース限度額が不正です。';
    leave;
  endif;

  if RC-MIN-PAY-AMT <= 0;
    pOutRtnCd = '30';
    pOutReasonCd = 'C005';
    pOutMessage = '最小支払額が不正です。';
    leave;
  endif;

  Wk.useableLimit = CM-SHOP-LIMIT-AMT - CM-SHOP-USED-AMT;
  if Wk.useableLimit < 0;
    Wk.useableLimit = 0;
  endif;

  if Wk.courseLimit > CM-SHOP-LIMIT-AMT;
    pOutRtnCd = '31';
    pOutReasonCd = 'L001';
    pOutMessage = 'リボコース限度額が会員ショッピング枠を超過しています。';
    leave;
  endif;

  if Wk.foundRev and Wk.oldCourseCd = Inp.courseCd;
    pOutRtnCd = '32';
    pOutReasonCd = 'C006';
    pOutMessage = '変更前後のリボコースが同一です。';
    leave;
  endif;

  // 次回締日・次回支払日を当月末締め／翌月10日払いで算出する。
  monitor;
    // 当月1日 → 翌月1日 - 1日 = 当月末日
    Wk.firstDay  = Wk.sysDate - %days(%subdt(Wk.sysDate:*days) - 1);
    Wk.closeDate = Wk.firstDay + %months(1) - %days(1);
    // 締日が未到来でなければ翌月末へ繰り越す
    if Wk.closeDate <= Wk.sysDate;
      Wk.closeDate = Wk.firstDay + %months(2) - %days(1);
    endif;
    // 締日の翌日（翌月1日）＋9日 = 翌月10日が支払日
    Wk.payDate     = Wk.closeDate + %days(1) + %days(9);
    Wk.nextCloseDt = %dec(Wk.closeDate:*iso);
    Wk.nextPayDt   = %dec(Wk.payDate:*iso);
    Wk.dateRtn     = '00';
  on-error;
    Wk.dateRtn = '99';
  endmon;

  if Wk.dateRtn <> '00';
    pOutRtnCd = '40';
    pOutReasonCd = 'D001';
    pOutMessage = '次回締日取得に失敗しました。';
    leave;
  endif;

  if Wk.nextCloseDt <= Wk.ymd;
    pOutRtnCd = '40';
    pOutReasonCd = 'D002';
    pOutMessage = '未到来締日が取得できません。';
    leave;
  endif;

  Wk.baseApplyDt = Wk.nextCloseDt + 1;
  if Wk.baseApplyDt > Wk.nextPayDt and Wk.nextPayDt <> 0;
    pOutRtnCd = '40';
    pOutReasonCd = 'D003';
    pOutMessage = '適用開始日と支払日の整合性が不正です。';
    leave;
  endif;

  if Wk.foundRev and RV-LAST-CLOSE-DT >= Wk.nextCloseDt;
    pOutRtnCd = '40';
    pOutReasonCd = 'D004';
    pOutMessage = '確定済締日以前への変更はできません。';
    leave;
  endif;

  Wk.revBal = ZeroAmt;
  if Wk.foundRev;
    Wk.revBal = RV-REV-BAL-AMT;
  endif;

  if Wk.revBal < 0;
    pOutRtnCd = '41';
    pOutReasonCd = 'R006';
    pOutMessage = 'リボ残高が不正です。';
    leave;
  endif;

  Wk.feeAmt = %int(Wk.revBal * C_MONTH_RATE);
  Wk.prinAmt = RC-MIN-PAY-AMT;
  if Wk.prinAmt > Wk.revBal;
    Wk.prinAmt = Wk.revBal;
  endif;
  Wk.payAmt = Wk.prinAmt + Wk.feeAmt;

  if Wk.revBal > 0 and Wk.payAmt <= 0;
    pOutRtnCd = '41';
    pOutReasonCd = 'R007';
    pOutMessage = '請求予定額が不正です。';
    leave;
  endif;

  monitor;
    exec sql
      set transaction isolation level read committed;

    if Wk.foundRev;
      exec sql
        update CDREVF
           set REV_COURSE_CD = :Inp.courseCd,
               REV_START_DT  = :Wk.baseApplyDt,
               REV_STATUS    = :C_STS_ACTIVE,
               REV_LIMIT_AMT = :Wk.courseLimit,
               REV_FEE_AMT   = :Wk.feeAmt,
               REV_PRIN_AMT  = :Wk.prinAmt,
               REV_PAY_AMT   = :Wk.payAmt,
               REV_UPD_YMD   = :Wk.ymd,
               REV_UPD_HMS   = :Audit.updHms,
               REV_UPD_PGM   = 'CA109R',
               REV_UPD_OPID  = :Inp.operId
         where MEMBER_NO     = :Inp.memberNo
           and REV_STATUS    = :C_STS_ACTIVE;

      if SQLCOD <> 0 or SQLERRD(3) <> 1;
        exec sql rollback;
        pOutRtnCd = '50';
        pOutReasonCd = 'U001';
        pOutMessage = 'リボ契約更新に失敗しました。';
        leave;
      endif;
    else;
      exec sql
        insert into CDREVF
             ( MEMBER_NO,
               REV_COURSE_CD,
               REV_START_DT,
               REV_STATUS,
               REV_LIMIT_AMT,
               REV_BAL_AMT,
               REV_FEE_AMT,
               REV_PRIN_AMT,
               REV_PAY_AMT,
               LAST_CLOSE_DT,
               REV_ENT_YMD,
               REV_ENT_HMS,
               REV_UPD_YMD,
               REV_UPD_HMS,
               REV_UPD_PGM,
               REV_UPD_OPID )
        values
             ( :Inp.memberNo,
               :Inp.courseCd,
               :Wk.baseApplyDt,
               :C_STS_ACTIVE,
               :Wk.courseLimit,
               0,
               0,
               0,
               0,
               0,
               :Wk.ymd,
               :Audit.updHms,
               :Wk.ymd,
               :Audit.updHms,
               'CA109R',
               :Inp.operId );

      if SQLCOD <> 0;
        exec sql rollback;
        pOutRtnCd = '50';
        pOutReasonCd = 'U002';
        pOutMessage = 'リボ契約登録に失敗しました。';
        leave;
      endif;
    endif;

    Audit.afterStatus = C_STS_ACTIVE;
    Audit.afterCourse = Inp.courseCd;
    Audit.applyDt = Wk.baseApplyDt;
    Audit.reasonCd = '0000';

    exec sql
      insert into CDREVA
           ( AUD_PGM_ID,
             MEMBER_NO,
             ACCEPT_KBN,
             BEFORE_STATUS,
             BEFORE_COURSE,
             AFTER_STATUS,
             AFTER_COURSE,
             APPLY_DT,
             REASON_CD,
             TERM_ID,
             OPER_ID,
             UPD_YMD,
             UPD_HMS )
      values
           ( :Audit.pgmId,
             :Audit.memberNo,
             :Audit.acceptKbn,
             :Audit.beforeStatus,
             :Audit.beforeCourse,
             :Audit.afterStatus,
             :Audit.afterCourse,
             :Audit.applyDt,
             :Audit.reasonCd,
             :Audit.termId,
             :Audit.operId,
             :Audit.updYmd,
             :Audit.updHms );

    if SQLCOD <> 0;
      exec sql rollback;
      pOutRtnCd = '50';
      pOutReasonCd = 'U003';
      pOutMessage = 'リボ受付監査登録に失敗しました。';
      leave;
    endif;

    exec sql commit;

    Wk.updateOk = *on;

  on-error;
    exec sql rollback;
    pOutRtnCd = '90';
    pOutReasonCd = 'E001';
    pOutMessage = 'リボ受付処理で例外が発生しました。';
    leave;
  endmon;

  if Wk.updateOk;
    pOutRtnCd = '00';
    pOutReasonCd = '0000';
    pOutApplyDt = Wk.baseApplyDt;
    if Inp.acceptKbn = C_ACCEPT_NEW;
      pOutMessage = 'リボ新規申込を承認しました。';
    else;
      pOutMessage = 'リボコース変更を承認しました。';
    endif;
  endif;

  leave;
enddo;

if pOutRtnCd <> '00';
  Audit.afterStatus = Audit.beforeStatus;
  Audit.afterCourse = Audit.beforeCourse;
  Audit.applyDt = 0;
  Audit.reasonCd = pOutReasonCd;

  monitor;
    exec sql
      insert into CDREVA
           ( AUD_PGM_ID,
             MEMBER_NO,
             ACCEPT_KBN,
             BEFORE_STATUS,
             BEFORE_COURSE,
             AFTER_STATUS,
             AFTER_COURSE,
             APPLY_DT,
             REASON_CD,
             TERM_ID,
             OPER_ID,
             UPD_YMD,
             UPD_HMS )
      values
           ( :Audit.pgmId,
             :Audit.memberNo,
             :Audit.acceptKbn,
             :Audit.beforeStatus,
             :Audit.beforeCourse,
             :Audit.afterStatus,
             :Audit.afterCourse,
             :Audit.applyDt,
             :Audit.reasonCd,
             :Audit.termId,
             :Audit.operId,
             :Audit.updYmd,
             :Audit.updHms );
    exec sql commit;
  on-error;
    exec sql rollback;
  endmon;
endif;

*inlr = *on;
return;

//**********************************************************************
//*  会員情報取得
//**********************************************************************
begsr LoadMember;

  Wk.foundMember = *off;
  clear CMREC;

  monitor;
    exec sql
      select MEMBER_NO,
             MEMBER_STS,
             LOST_FLG,
             FRAUD_FLG,
             SHOP_LIMIT_AMT,
             SHOP_USED_AMT,
             ENTRY_YMD,
             BIRTH_YMD
        into :CM-MEMBER-NO,
             :CM-MEMBER-STS,
             :CM-LOST-FLG,
             :CM-FRAUD-FLG,
             :CM-SHOP-LIMIT-AMT,
             :CM-SHOP-USED-AMT,
             :CM-ENTRY-YMD,
             :CM-BIRTH-YMD
        from CDMEMF
       where MEMBER_NO = :Inp.memberNo
       fetch first 1 row only;

    if SQLCOD = 0;
      Wk.foundMember = *on;
    endif;
  on-error;
    Wk.foundMember = *off;
  endmon;

endsr;

//**********************************************************************
//*  リボ契約取得
//**********************************************************************
begsr LoadRev;

  Wk.foundRev = *off;
  clear RVREC;

  monitor;
    exec sql
      select MEMBER_NO,
             REV_COURSE_CD,
             REV_START_DT,
             REV_STATUS,
             REV_LIMIT_AMT,
             REV_BAL_AMT,
             REV_FEE_AMT,
             REV_PRIN_AMT,
             REV_PAY_AMT,
             LAST_CLOSE_DT,
             REV_ENT_YMD,
             REV_UPD_YMD
        into :RV-MEMBER-NO,
             :RV-REV-COURSE-CD,
             :RV-REV-START-DT,
             :RV-REV-STATUS,
             :RV-REV-LIMIT-AMT,
             :RV-REV-BAL-AMT,
             :RV-REV-FEE-AMT,
             :RV-REV-PRIN-AMT,
             :RV-REV-PAY-AMT,
             :RV-LAST-CLOSE-DT,
             :RV-REV-ENT-YMD,
             :RV-REV-UPD-YMD
        from CDREVF
       where MEMBER_NO = :Inp.memberNo
       fetch first 1 row only
         for update of REV_COURSE_CD,
                       REV_START_DT,
                       REV_STATUS,
                       REV_LIMIT_AMT,
                       REV_FEE_AMT,
                       REV_PRIN_AMT,
                       REV_PAY_AMT,
                       REV_UPD_YMD,
                       REV_UPD_HMS,
                       REV_UPD_PGM,
                       REV_UPD_OPID;

    if SQLCOD = 0;
      Wk.foundRev = *on;
    endif;
  on-error;
    Wk.foundRev = *off;
  endmon;

endsr;

//**********************************************************************
//*  コース情報取得
//**********************************************************************
begsr LoadCourse;

  Wk.foundCourse = *off;
  clear RCREC;

  monitor;
    exec sql
      select COURSE_CD,
             COURSE_STS,
             SLIDE_TIER,
             LIMIT_AMT,
             MIN_PAY_AMT,
             START_DT,
             END_DT
        into :RC-COURSE-CD,
             :RC-COURSE-STS,
             :RC-SLIDE-TIER,
             :RC-LIMIT-AMT,
             :RC-MIN-PAY-AMT,
             :RC-START-DT,
             :RC-END-DT
        from CDCOURF
       where COURSE_CD = :Inp.courseCd
       fetch first 1 row only;

    if SQLCOD = 0;
      Wk.foundCourse = *on;
    endif;
  on-error;
    Wk.foundCourse = *off;
  endmon;

endsr;
