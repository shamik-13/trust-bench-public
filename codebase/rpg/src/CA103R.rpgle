**free
//**********************************************************************
// 変更履歴
// 版数  年月日    担当    概要
// 01.00 20210412  佐藤    新規作成
// 01.01 20220303  中村    延滞利息表示追加
// 01.02 20231117  山田    サービス応答日差異警告追加
//**********************************************************************

ctl-opt dftactgrp(*no) actgrp('CAONLINE')
        option(*srcstmt:*nodebugio)
        main(MainProc);

// みらいカード 残高照会オンライン
// カード番号単位に残高を取得し、照会時点残高を画面へ返却する

dcl-f CA103D workstn usage(*input:*output) infds(DspInf);
dcl-f CDOSF keyed usage(*input) extfile('MIRAI/CDOSF') infds(CdosInf);
dcl-f CDLATE keyed usage(*input) extfile('MIRAI/CDLATE') infds(LateInf);

/copy MIRAI/QRPGLESRC,CDOSFC
/copy MIRAI/QRPGLESRC,CDLATEC

dcl-ds DspInf qualified;
  FileName       char(10)   pos(83);
  Status         char(5)    pos(401);
  CurRec         char(10)   pos(378);
end-ds;

dcl-ds CdosInf qualified;
  Status         char(5)    pos(401);
  OpnInd         char(1)    pos(9);
end-ds;

dcl-ds LateInf qualified;
  Status         char(5)    pos(401);
  OpnInd         char(1)    pos(9);
end-ds;

dcl-ds InParm qualified;
  TermId         char(10);
  Operator       char(10);
  Branch         char(4);
  AuthTime       zoned(14:0);
end-ds;

dcl-ds OutParm qualified;
  ResultCd       char(2);
  Message        char(80);
  AuthJudge      char(1);
  TotalBalance   packed(15:0);
end-ds;

dcl-pi *n;
  PiTermId       char(10) const;
  PiOperator     char(10) const;
  PiBranch       char(4)  const;
  PiOutResult    char(2);
  PiOutMessage   char(80);
  PiOutJudge     char(1);
end-pi;

dcl-pr BalanceInquiryService extpgm('CDBALSVC');
  SvCardNo       char(16) const;
  SvBaseDate     packed(8:0) const;
  SvRespCd       char(2);
  SvRespMsg      char(60);
  SvCycleYmd     packed(8:0);
  SvPrincipal    packed(15:0);
  SvInterest     packed(15:0);
  SvFee          packed(15:0);
  SvLimit        packed(15:0);
end-pr;

dcl-s CardNo          char(16);
dcl-s BaseYmd         packed(8:0);
dcl-s BaseTim         packed(6:0);
dcl-s SvcRespCd       char(2);
dcl-s SvcRespMsg      char(60);
dcl-s SvcCycleYmd     packed(8:0);
dcl-s SvcPrincipal    packed(15:0);
dcl-s SvcInterest     packed(15:0);
dcl-s SvcFee          packed(15:0);
dcl-s SvcLimit        packed(15:0);
dcl-s LocalCycleYmd   packed(8:0);
dcl-s LateInterest    packed(15:0);
dcl-s TotalBalance    packed(15:0);
dcl-s AvailableAmt    packed(15:0);
dcl-s WarnCycle       ind inz(*off);
dcl-s EndPgm          ind inz(*off);
dcl-s ValidCard       ind inz(*off);
dcl-s NeedLateRead    ind inz(*off);
dcl-s KeyCard         char(16);
dcl-s MsgText         char(80);
dcl-s ResultCd        char(2);
dcl-s Judge           char(1);
dcl-s PayMethod       char(2);
dcl-s AppStatus       char(1);
dcl-s RetryCnt        packed(3:0) inz(0);
dcl-s WorkAmount      packed(15:0);
dcl-s WorkDate        date;
dcl-s WorkTime        time;
dcl-s WorkStamp       timestamp;

dcl-c C_OK            '00';
dcl-c C_WARN          '04';
dcl-c C_NG            '99';
dcl-c J_APPROVE       'A';
dcl-c J_DECLINE       'D';
dcl-c J_HOLD          'H';

dcl-proc MainProc;

  InParm.TermId = PiTermId;
  InParm.Operator = PiOperator;
  InParm.Branch = PiBranch;

  exsr InitWork;
  exsr ReceiveScreen;

  dow not EndPgm;

    exsr ClearResult;
    exsr ValidateRequest;

    if ValidCard;
      exsr ReadLocalBase;
    endif;

    if ValidCard;
      exsr CallBalanceService;
    endif;

    if ValidCard and SvcRespCd = C_OK;
      exsr ReadLateBalance;
      exsr CalcBalance;
      exsr MakeDecision;
    endif;

    exsr SendScreen;
    exsr ReceiveScreen;

  enddo;

  PiOutResult = OutParm.ResultCd;
  PiOutMessage = OutParm.Message;
  PiOutJudge = OutParm.AuthJudge;

  *inlr = *on;
  return;

  begsr InitWork;
    WorkStamp = %timestamp();
    WorkDate = %date(WorkStamp);
    WorkTime = %time(WorkStamp);
    BaseYmd = %dec(%char(WorkDate:*iso0):8:0);
    BaseTim = %dec(%char(WorkTime:*iso0):6:0);
    InParm.AuthTime = (BaseYmd * 1000000) + BaseTim;

    OutParm.ResultCd = C_OK;
    OutParm.Message = *blanks;
    OutParm.AuthJudge = *blank;
    OutParm.TotalBalance = 0;

    ResultCd = C_OK;
    Judge = *blank;
    MsgText = *blanks;
  endsr;

  begsr ReceiveScreen;
    exfmt CA10301;

    if *in03 or *in12;
      EndPgm = *on;
    else;
      CardNo = SCARDNO;
    endif;
  endsr;

  begsr ClearResult;
    SvcRespCd = *blanks;
    SvcRespMsg = *blanks;
    SvcCycleYmd = 0;
    SvcPrincipal = 0;
    SvcInterest = 0;
    SvcFee = 0;
    SvcLimit = 0;
    LocalCycleYmd = 0;
    LateInterest = 0;
    TotalBalance = 0;
    AvailableAmt = 0;
    WarnCycle = *off;
    ValidCard = *off;
    NeedLateRead = *off;
    PayMethod = *blanks;
    AppStatus = *blank;
    ResultCd = C_OK;
    Judge = *blank;
    MsgText = *blanks;

    SPRNBAL = 0;
    SINTBAL = 0;
    SFEEBAL = 0;
    SLATEIN = 0;
    STOTAL = 0;
    SAVAIL = 0;
    SWARN = *blanks;
    SMSG = *blanks;
  endsr;

  begsr ValidateRequest;

    if CardNo = *blanks;
      ResultCd = C_NG;
      Judge = J_DECLINE;
      MsgText = 'カード番号を入力してください';
      ValidCard = *off;

    elseif %check('0123456789': CardNo) <> 0;
      ResultCd = C_NG;
      Judge = J_DECLINE;
      MsgText = 'カード番号に数字以外があります';
      ValidCard = *off;

    elseif %len(%trim(CardNo)) <> 16;
      ResultCd = C_NG;
      Judge = J_DECLINE;
      MsgText = 'カード番号桁数エラー';
      ValidCard = *off;

    elseif %subst(CardNo:1:6) <> '498765'
       and %subst(CardNo:1:6) <> '498766'
       and %subst(CardNo:1:6) <> '498767';
      ResultCd = C_NG;
      Judge = J_DECLINE;
      MsgText = 'みらいカード対象外';
      ValidCard = *off;

    else;
      exsr CheckDigit;
    endif;

  endsr;

  begsr CheckDigit;
    dcl-s Pos          int(10);
    dcl-s Digit        packed(2:0);
    dcl-s SumDigit     packed(3:0) inz(0);
    dcl-s Alt          ind inz(*off);

    SumDigit = 0;
    Alt = *off;

    for Pos = 16 downto 1;
      Digit = %dec(%subst(CardNo:Pos:1):1:0);
      if Alt;
        Digit = Digit * 2;
        if Digit > 9;
          Digit = Digit - 9;
        endif;
      endif;
      SumDigit += Digit;
      Alt = not Alt;
    endfor;

    if %rem(SumDigit:10) = 0;
      ValidCard = *on;
    else;
      ResultCd = C_NG;
      Judge = J_DECLINE;
      MsgText = 'カード番号検査桁エラー';
      ValidCard = *off;
    endif;

  endsr;

  begsr ReadLocalBase;

    KeyCard = CardNo;

    monitor;
      chain KeyCard CDOSF;
      if %found(CDOSF);
        LocalCycleYmd = OS_CYCLE_YMD;
        PayMethod = OS_PAY_METHOD;
        AppStatus = OS_APP_STATUS;

        select;
        when PayMethod = '10';
          NeedLateRead = *on;
        when PayMethod = '20';
          NeedLateRead = *on;
        when PayMethod = '30';
          NeedLateRead = *on;
        other;
          ResultCd = C_NG;
          Judge = J_DECLINE;
          MsgText = '支払方法区分エラー';
          ValidCard = *off;
        endsl;

        if ValidCard;
          select;
          when AppStatus = 'F';
            ResultCd = C_OK;
            Judge = J_APPROVE;
            MsgText = '完済済み';
            ValidCard = *off;
          when AppStatus = 'P';
            ValidCard = *on;
          when AppStatus = 'O';
            ValidCard = *on;
          when AppStatus = 'S';
            ResultCd = C_NG;
            Judge = J_DECLINE;
            MsgText = '照会対象外';
            ValidCard = *off;
          other;
            ResultCd = C_NG;
            Judge = J_DECLINE;
            MsgText = '申込状態区分エラー';
            ValidCard = *off;
          endsl;
        endif;

      else;
        ResultCd = C_NG;
        Judge = J_DECLINE;
        MsgText = '会員残高なし';
        ValidCard = *off;
      endif;

    on-error;
      ResultCd = C_NG;
      Judge = J_HOLD;
      MsgText = '会員残高読込エラー';
      ValidCard = *off;
    endmon;

  endsr;

  begsr CallBalanceService;

    RetryCnt = 0;

    dou RetryCnt >= 2 or SvcRespCd <> *blanks;
      RetryCnt += 1;

      monitor;
        callp BalanceInquiryService(CardNo
                                   :BaseYmd
                                   :SvcRespCd
                                   :SvcRespMsg
                                   :SvcCycleYmd
                                   :SvcPrincipal
                                   :SvcInterest
                                   :SvcFee
                                   :SvcLimit);
      on-error;
        SvcRespCd = *blanks;
      endmon;

    enddo;

    if SvcRespCd = *blanks;
      ResultCd = C_NG;
      Judge = J_HOLD;
      MsgText = '残高サービス応答なし';
      ValidCard = *off;

    elseif SvcRespCd <> C_OK;
      ResultCd = C_NG;
      Judge = J_DECLINE;
      MsgText = %trim(SvcRespMsg);
      ValidCard = *off;

    elseif SvcCycleYmd <> LocalCycleYmd;
      WarnCycle = *on;
    endif;

  endsr;

  begsr ReadLateBalance;

    if NeedLateRead;
      monitor;
        chain KeyCard CDLATE;
        if %found(CDLATE);
          if LT_CALC_YMD <= BaseYmd;
            LateInterest = LT_LATE_INTEREST;
          else;
            LateInterest = 0;
          endif;
        else;
          LateInterest = 0;
        endif;
      on-error;
        ResultCd = C_NG;
        Judge = J_HOLD;
        MsgText = '延滞利息読込エラー';
        ValidCard = *off;
      endmon;
    endif;

  endsr;

  begsr CalcBalance;

    if ValidCard;
      WorkAmount = SvcPrincipal + SvcInterest;
      WorkAmount += SvcFee;
      WorkAmount += LateInterest;

      if WorkAmount < 0;
        TotalBalance = 0;
      else;
        TotalBalance = WorkAmount;
      endif;

      AvailableAmt = SvcLimit - TotalBalance;
      if AvailableAmt < 0;
        AvailableAmt = 0;
      endif;
    endif;

  endsr;

  begsr MakeDecision;

    if not ValidCard;
      // 直前処理で判定済み
    elseif AppStatus = 'O';
      ResultCd = C_OK;
      Judge = J_APPROVE;
      MsgText = '過入金残高あり';
    elseif TotalBalance = 0;
      ResultCd = C_OK;
      Judge = J_APPROVE;
      MsgText = '残高なし';
    elseif WarnCycle;
      ResultCd = C_WARN;
      Judge = J_HOLD;
      MsgText = '更新中データあり';
    elseif TotalBalance > SvcLimit and SvcLimit > 0;
      ResultCd = C_NG;
      Judge = J_DECLINE;
      MsgText = '利用枠超過';
    else;
      ResultCd = C_OK;
      Judge = J_APPROVE;
      MsgText = '照会正常';
    endif;

  endsr;

  begsr SendScreen;

    SPRNBAL = SvcPrincipal;
    SINTBAL = SvcInterest;
    SFEEBAL = SvcFee;
    SLATEIN = LateInterest;
    STOTAL = TotalBalance;
    SAVAIL = AvailableAmt;
    SCYCLE = SvcCycleYmd;
    SLCYCLE = LocalCycleYmd;
    SRESULT = ResultCd;
    SJUDGE = Judge;
    SMSG = MsgText;

    if WarnCycle;
      SWARN = 'サービス日付とローカル日付が不一致です';
    else;
      SWARN = *blanks;
    endif;

    OutParm.ResultCd = ResultCd;
    OutParm.Message = MsgText;
    OutParm.AuthJudge = Judge;
    OutParm.TotalBalance = TotalBalance;

    write CA10302;

  endsr;

end-proc;
