**free
//**********************************************************************
//  変更履歴
//  版数  年月日    担当  概要
//  1.00  20190513  SAK  初版作成。信用枠・キャッシング枠照会を実装。
//  1.01  20210208  NYM  未確定オーソリ残を利用済額へ加算。
//  1.02  20230925  KOD  枠計算式変更。
//                    物販: 利用可能額 = 信用枠 - 売上利用済 - 未確定物販
//                    C/S : 利用可能額 = C/S枠 - C/S利用済 - 未確定C/S
//  1.03  20240520  HRT  延滞区分・停止状態時のゼロ返却とCDLOGF記録追加。
//**********************************************************************
ctl-opt dftactgrp(*no) actgrp(*caller)
        option(*srcstmt:*nodebugio)
        bnddir('QC2LE')
        datfmt(*iso) timfmt(*iso)
        decedit('0.');

 /copy QRPGLESRC,CDACCFC
 /copy QRPGLESRC,CDAUTHF4C
 /copy QRPGLESRC,CDLOGFC

dcl-pr CA113R extpgm('CA113R');
  pInCardNo        char(16) const;
  pInMbrNo         packed(7:0) const;
  pInTranKbn       char(2) const;
  pInReqAmt        packed(13:0) const;
  pOutAvailAmt     packed(13:0);
  pOutLimitAmt     packed(13:0);
  pOutUsedAmt      packed(13:0);
  pOutHoldAmt      packed(13:0);
  pOutResult       char(2);
  pOutReason       char(3);
end-pr;

dcl-pi CA113R;
  pInCardNo        char(16) const;
  pInMbrNo         packed(7:0) const;
  pInTranKbn       char(2) const;
  pInReqAmt        packed(13:0) const;
  pOutAvailAmt     packed(13:0);
  pOutLimitAmt     packed(13:0);
  pOutUsedAmt      packed(13:0);
  pOutHoldAmt      packed(13:0);
  pOutResult       char(2);
  pOutReason       char(3);
end-pi;

dcl-f CDACCF   usage(*input)  keyed usropn extfile('CDACCF');
dcl-f CDAUTHF4 usage(*input)  keyed usropn extfile('CDAUTHF4');
dcl-f CDLOGF   usage(*output)       usropn extfile('CDLOGF');

dcl-ds DsParm qualified;
  CardNo        char(16);
  MbrNo         packed(7:0);
  TranKbn       char(2);
  ReqAmt        packed(13:0);
end-ds;

dcl-ds DsCalc qualified;
  LimitAmt      packed(13:0);
  UsedAmt       packed(13:0);
  HoldAmt       packed(13:0);
  AvailAmt      packed(13:0);
  WorkAmt       packed(15:0);
  AuthCnt       packed(7:0);
end-ds;

dcl-ds DsLog qualified;
  EventKbn      char(2);
  Reason        char(3);
  OpeText       char(40);
end-ds;

dcl-s WkToday        date(*iso);
dcl-s WkNow          time(*iso);
dcl-s WkAuthLimitDt  date(*iso);
dcl-s WkReject       ind inz(*off);
dcl-s WkFoundAcc     ind inz(*off);
dcl-s WkCsTran       ind inz(*off);
dcl-s WkErr          ind inz(*off);
dcl-s WkMsg          char(52);
dcl-s WkRrn          packed(10:0);

dcl-c C_RESULT_OK    '00';
dcl-c C_RESULT_NG    '90';
dcl-c C_RSN_NORMAL   '000';
dcl-c C_RSN_PARAM    '101';
dcl-c C_RSN_NOACC    '201';
dcl-c C_RSN_STOP     '301';
dcl-c C_RSN_DELINQ   '302';
dcl-c C_RSN_LIMIT    '401';
dcl-c C_RSN_ERROR    '999';

dcl-c C_LOG_REFUSE   'RF';
dcl-c C_AUTH_OK      '00';
dcl-c C_AUTH_HOLD    '10';
dcl-c C_AUTH_REV     '30';

dcl-proc InitWork;
  clear DsParm;
  clear DsCalc;
  clear DsLog;

  DsParm.CardNo  = pInCardNo;
  DsParm.MbrNo   = pInMbrNo;
  DsParm.TranKbn = pInTranKbn;
  DsParm.ReqAmt  = pInReqAmt;

  pOutAvailAmt = 0;
  pOutLimitAmt = 0;
  pOutUsedAmt  = 0;
  pOutHoldAmt  = 0;
  pOutResult   = C_RESULT_NG;
  pOutReason   = C_RSN_ERROR;

  WkToday       = %date();
  WkNow         = %time();
  WkAuthLimitDt = WkToday - %days(14);
  WkReject      = *off;
  WkFoundAcc    = *off;
  WkCsTran      = *off;
  WkErr         = *off;
end-proc;

dcl-proc OpenFiles;
  monitor;
    if not %open(CDACCF);
      open CDACCF;
    endif;
    if not %open(CDAUTHF4);
      open CDAUTHF4;
    endif;
    if not %open(CDLOGF);
      open CDLOGF;
    endif;
  on-error;
    WkErr = *on;
    pOutResult = C_RESULT_NG;
    pOutReason = C_RSN_ERROR;
  endmon;
end-proc;

dcl-proc CloseFiles;
  monitor;
    if %open(CDACCF);
      close CDACCF;
    endif;
    if %open(CDAUTHF4);
      close CDAUTHF4;
    endif;
    if %open(CDLOGF);
      close CDLOGF;
    endif;
  on-error;
  endmon;
end-proc;

dcl-proc CheckParm;
  if DsParm.CardNo = *blank or DsParm.MbrNo <= 0;
    pOutReason = C_RSN_PARAM;
    WkReject = *on;
    return;
  endif;

  if DsParm.ReqAmt < 0;
    pOutReason = C_RSN_PARAM;
    WkReject = *on;
    return;
  endif;

  select;
  when DsParm.TranKbn = '01' or DsParm.TranKbn = '02'
    or DsParm.TranKbn = '03';
    WkCsTran = *off;
  when DsParm.TranKbn = '11' or DsParm.TranKbn = '12';
    WkCsTran = *on;
  other;
    pOutReason = C_RSN_PARAM;
    WkReject = *on;
  endsl;
end-proc;

dcl-proc ReadAccount;
  monitor;
    chain (DsParm.CardNo:DsParm.MbrNo) CDACCF;
    if %found(CDACCF);
      WkFoundAcc = *on;
    else;
      pOutReason = C_RSN_NOACC;
      WkReject = *on;
    endif;
  on-error;
    WkErr = *on;
    pOutReason = C_RSN_ERROR;
    WkReject = *on;
  endmon;
end-proc;

dcl-proc CheckAccountStatus;
  if not WkFoundAcc;
    return;
  endif;

  // 停止・延滞は照会値を返さず、拒否イベントのみ記録する。
  if ACSTS = '9' or ACSTOP = '1';
    pOutReason = C_RSN_STOP;
    WkReject = *on;
    return;
  endif;

  if ACDLQKBN <> *blank and ACDLQKBN <> '0';
    pOutReason = C_RSN_DELINQ;
    WkReject = *on;
    return;
  endif;
end-proc;

dcl-proc SetBaseLimit;
  if WkCsTran;
    DsCalc.LimitAmt = ACCSLMT;
    DsCalc.UsedAmt  = ACCSUSE;
  else;
    DsCalc.LimitAmt = ACCRLMT;
    DsCalc.UsedAmt  = ACRUSE;
  endif;

  if DsCalc.LimitAmt < 0;
    DsCalc.LimitAmt = 0;
  endif;

  if DsCalc.UsedAmt < 0;
    DsCalc.UsedAmt = 0;
  endif;
end-proc;

dcl-proc AddAuthHold;
  monitor;
    setll (DsParm.CardNo:DsParm.MbrNo) CDAUTHF4;

    dou %eof(CDAUTHF4);
      reade (DsParm.CardNo:DsParm.MbrNo) CDAUTHF4;
      if %eof(CDAUTHF4);
        leave;
      endif;

      // 承認済・保留中で取消未反映のオーソリのみ未確定残へ加算。
      if AFAUTHDT < WkAuthLimitDt;
        iter;
      endif;

      if AFAUTHSTS <> C_AUTH_OK and AFAUTHSTS <> C_AUTH_HOLD;
        iter;
      endif;

      if AFREVSTS = C_AUTH_REV;
        iter;
      endif;

      if AFTRNKBN = '11' or AFTRNKBN = '12';
        if WkCsTran;
          DsCalc.HoldAmt += AFAUTHAMT;
          DsCalc.AuthCnt += 1;
        endif;
      else;
        if not WkCsTran;
          DsCalc.HoldAmt += AFAUTHAMT;
          DsCalc.AuthCnt += 1;
        endif;
      endif;
    enddo;
  on-error;
    WkErr = *on;
    pOutReason = C_RSN_ERROR;
    WkReject = *on;
  endmon;
end-proc;

dcl-proc CalcAvailable;
  DsCalc.WorkAmt = DsCalc.LimitAmt - DsCalc.UsedAmt - DsCalc.HoldAmt;

  if DsCalc.WorkAmt < 0;
    DsCalc.AvailAmt = 0;
  elseif DsCalc.WorkAmt > 9999999999999;
    DsCalc.AvailAmt = 9999999999999;
  else;
    DsCalc.AvailAmt = DsCalc.WorkAmt;
  endif;

  if DsParm.ReqAmt > DsCalc.AvailAmt;
    pOutReason = C_RSN_LIMIT;
    pOutResult = C_RESULT_NG;
  else;
    pOutReason = C_RSN_NORMAL;
    pOutResult = C_RESULT_OK;
  endif;

  pOutLimitAmt = DsCalc.LimitAmt;
  pOutUsedAmt  = DsCalc.UsedAmt;
  pOutHoldAmt  = DsCalc.HoldAmt;
  pOutAvailAmt = DsCalc.AvailAmt;
end-proc;

dcl-proc ZeroReply;
  pOutLimitAmt = 0;
  pOutUsedAmt  = 0;
  pOutHoldAmt  = 0;
  pOutAvailAmt = 0;
  pOutResult   = C_RESULT_NG;
end-proc;

dcl-proc WriteRejectLog;
  if pOutReason = C_RSN_NORMAL;
    return;
  endif;

  DsLog.EventKbn = C_LOG_REFUSE;
  DsLog.Reason   = pOutReason;

  select;
  when pOutReason = C_RSN_PARAM;
    DsLog.OpeText = '利用可能枠照会 パラメータ不正';
  when pOutReason = C_RSN_NOACC;
    DsLog.OpeText = '利用可能枠照会 会員口座なし';
  when pOutReason = C_RSN_STOP;
    DsLog.OpeText = '利用可能枠照会 停止状態';
  when pOutReason = C_RSN_DELINQ;
    DsLog.OpeText = '利用可能枠照会 延滞区分';
  when pOutReason = C_RSN_LIMIT;
    DsLog.OpeText = '利用可能枠照会 枠不足';
  other;
    DsLog.OpeText = '利用可能枠照会 処理異常';
  endsl;

  monitor;
    clear CDLOGR;
    LGDATE   = WkToday;
    LGTIME   = WkNow;
    LGCARDNO = DsParm.CardNo;
    LGMBRNO  = DsParm.MbrNo;
    LGPGMID  = 'CA113R';
    LGEVTKBN = DsLog.EventKbn;
    LGRSNCD  = DsLog.Reason;
    LGTRNKBN = DsParm.TranKbn;
    LGREQAMT = DsParm.ReqAmt;
    LGLMTAMT = DsCalc.LimitAmt;
    LGUSEAMT = DsCalc.UsedAmt;
    LGHLDAMT = DsCalc.HoldAmt;
    LGOPTXT  = DsLog.OpeText;
    write CDLOGR;
  on-error;
  endmon;
end-proc;

InitWork();
OpenFiles();

if not WkErr;
  CheckParm();
endif;

if not WkReject and not WkErr;
  ReadAccount();
endif;

if not WkReject and not WkErr;
  CheckAccountStatus();
endif;

if WkReject;
  ZeroReply();
  WriteRejectLog();
else;
  SetBaseLimit();
  AddAuthHold();

  if WkReject or WkErr;
    ZeroReply();
    WriteRejectLog();
  else;
    CalcAvailable();

    if pOutResult <> C_RESULT_OK;
      WriteRejectLog();
    endif;
  endif;
endif;

CloseFiles();

*inlr = *on;
return;
