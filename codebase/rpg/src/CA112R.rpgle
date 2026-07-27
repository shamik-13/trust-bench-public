**free
//*********************************************************************
//*  プログラム : CA112R
//*  名称       : オンライン与信オーソリ受付
//*  会社       : みらいカード
//*--------------------------------------------------------------------
//*  変更履歴
//*  版数  年月日    担当    概要
//*  1.00  20190401  佐藤    初版作成。会員状態、限度額、重複取引確認を実装。
//*  1.10  20200713  鈴木    EC取引の重複判定に加盟店端末番号を追加。
//*  1.20  20211122  高橋    オンライン応答時間短縮のため会員照会順序を見直し。
//*  1.30  20230306  田中    海外ATM取引区分の確定をオーソリ専用処理へ委譲。
//*  1.40  20240520  中村    否認理由登録をCDAUTHFへ集約し応答遅延を抑止。
//*********************************************************************

ctl-opt dftactgrp(*no)
        actgrp('CAONLINE')
        option(*srcstmt:*nodebugio)
        datfmt(*iso)
        timfmt(*iso)
        bnddir('QC2LE');

//---------------------------------------------------------------------
//  外部記述ファイル
//---------------------------------------------------------------------
dcl-f CDACCF  usage(*input)  keyed extdesc('MIRAI/CDACCF');
dcl-f CDTXNF  usage(*update:*output) keyed extdesc('MIRAI/CDTXNF')
              usropn;
dcl-f CDAUTHF usage(*output) extdesc('MIRAI/CDAUTHF')
              usropn;
dcl-f CDLOGF  usage(*output) extdesc('MIRAI/CDLOGF')
              usropn;

//---------------------------------------------------------------------
//  レコード定義コピー
//---------------------------------------------------------------------
// /COPY MIRAI/QRPGLESRC,CDACCFC
// /COPY MIRAI/QRPGLESRC,CDTXNFC
// /COPY MIRAI/QRPGLESRC,CDAUTHF4C
// /COPY MIRAI/QRPGLESRC,CDLOGFC

//---------------------------------------------------------------------
//  呼出プログラム
//---------------------------------------------------------------------
dcl-pr CA113R extpgm('CA113R');
  pCardNo       char(16) const;
  pTxnAmt       packed(13:0) const;
  pChannelKbn   char(2) const;
  pRiskRank     char(1);
  pRiskScore    packed(5:0);
  pRtnCd        char(2);
end-pr;

dcl-pr CA114R extpgm('CA114R');
  pCardNo       char(16) const;
  pTranDate     zoned(8:0) const;
  pTranTime     zoned(6:0) const;
  pMerchantNo   char(15) const;
  pAuthNo       char(6);
  pRtnCd        char(2);
end-pr;

//---------------------------------------------------------------------
//  *ENTRY
//---------------------------------------------------------------------
dcl-pi *n;
  iCardNo       char(16) const;
  iTxnAmt       packed(13:0) const;
  iChannelKbn   char(2) const;
  iMerchantNo   char(15) const;
  iTermId       char(8)  const;
  iRequestId    char(20) const;
  oAuthKbn      char(1);
  oAuthNo       char(6);
  oReasonCd     char(3);
  oMessage      char(60);
end-pi;

//---------------------------------------------------------------------
//  作業領域
//---------------------------------------------------------------------
dcl-ds Req qualified inz;
  CardNo        char(16);
  TxnAmt        packed(13:0);
  ChannelKbn    char(2);
  MerchantNo    char(15);
  TermId        char(8);
  RequestId     char(20);
  TranDate      zoned(8:0);
  TranTime      zoned(6:0);
  TxnKbn        char(2);
  DispKbn       char(1);
  FeeKbn        char(2);
  SetlKbn       char(1);
end-ds;

dcl-ds Member qualified inz;
  Found         ind;
  CardNo        char(16);
  AcctNo        char(12);
  StatusKbn     char(1);
  StopKbn       char(1);
  ShopLimit     packed(13:0);
  CashLimit     packed(13:0);
  ShopUsed      packed(13:0);
  CashUsed      packed(13:0);
  ExpYm         zoned(6:0);
  PinNgCnt      packed(3:0);
end-ds;

dcl-ds Auth qualified inz;
  Kbn           char(1);
  No            char(6);
  ReasonCd      char(3);
  Message       char(60);
  RiskRank      char(1);
  RiskScore     packed(5:0);
  RtnCd         char(2);
  DupFound      ind;
  AvailAmt      packed(13:0);
  FeeAmt        packed(9:0);
end-ds;

dcl-ds Work qualified inz;
  Ix            int(10);
  SumOdd        int(10);
  SumEven       int(10);
  Digit         int(10);
  ModVal        int(10);
  TodayYm       zoned(6:0);
  NowTs         timestamp;
  SqlCd         packed(7:0);
end-ds;

dcl-s wkMsg       char(100);
dcl-s wkAmount    packed(13:0);
dcl-s wkRtnCd     char(2);

//---------------------------------------------------------------------
//  定数
//---------------------------------------------------------------------
dcl-c C_AUTH_OK       'A';
dcl-c C_AUTH_NG       'D';
dcl-c C_AUTH_HOLD     'H';

dcl-c C_RSN_NORMAL    '000';
dcl-c C_RSN_CARD      '101';
dcl-c C_RSN_AMT       '102';
dcl-c C_RSN_CHANNEL   '103';
dcl-c C_RSN_MEMBER    '201';
dcl-c C_RSN_STOP      '202';
dcl-c C_RSN_EXPIRE    '203';
dcl-c C_RSN_LIMIT     '301';
dcl-c C_RSN_DUP       '302';
dcl-c C_RSN_RISK      '401';
dcl-c C_RSN_SYSTEM    '900';

//*********************************************************************
//  主処理
//*********************************************************************
exsr Init;
exsr ValidateRequest;

if Auth.Kbn = *blank;
  exsr LoadMember;
endif;

if Auth.Kbn = *blank;
  exsr DecideDomain;
endif;

if Auth.Kbn = *blank;
  exsr CheckMember;
endif;

if Auth.Kbn = *blank;
  exsr CheckDuplicate;
endif;

if Auth.Kbn = *blank;
  exsr CheckRisk;
endif;

if Auth.Kbn = *blank;
  exsr CheckLimit;
endif;

if Auth.Kbn = *blank;
  exsr Approve;
endif;

exsr WriteResult;
exsr ReturnParm;

*inlr = *on;
return;

//*********************************************************************
//  初期化
//*********************************************************************
begsr Init;

  clear Req;
  clear Member;
  clear Auth;

  Req.CardNo     = iCardNo;
  Req.TxnAmt     = iTxnAmt;
  Req.ChannelKbn = iChannelKbn;
  Req.MerchantNo = iMerchantNo;
  Req.TermId     = iTermId;
  Req.RequestId  = iRequestId;

  Work.NowTs     = %timestamp();
  Req.TranDate   = %dec(%char(%date():*iso0):8:0);
  Req.TranTime   = %dec(%char(%time():*iso0):6:0);
  Work.TodayYm   = %dec(%subst(%char(%date():*iso0):1:6):6:0);

  Req.FeeKbn     = '00';
  Req.SetlKbn    = 'H';

  if not %open(CDTXNF);
    open CDTXNF;
  endif;

  if not %open(CDAUTHF);
    open CDAUTHF;
  endif;

  if not %open(CDLOGF);
    open CDLOGF;
  endif;

endSr;

//*********************************************************************
//  入力検査
//*********************************************************************
begsr ValidateRequest;

  if %trim(Req.CardNo) = *blank or %len(%trim(Req.CardNo)) <> 16;
    Auth.Kbn      = C_AUTH_NG;
    Auth.ReasonCd = C_RSN_CARD;
    Auth.Message  = 'カード番号桁数不正';
    leavesr;
  endif;

  // ルーン検査。オンライン入口での不要なDBアクセスを避ける。
  Work.SumOdd  = 0;
  Work.SumEven = 0;

  for Work.Ix = 1 to 16;
    Work.Digit = %int(%subst(Req.CardNo:Work.Ix:1));

    if %rem(Work.Ix:2) = 1;
      Work.Digit = Work.Digit * 2;
      if Work.Digit > 9;
        Work.Digit = Work.Digit - 9;
      endif;
      Work.SumOdd += Work.Digit;
    else;
      Work.SumEven += Work.Digit;
    endif;
  endfor;

  Work.ModVal = %rem(Work.SumOdd + Work.SumEven:10);
  if Work.ModVal <> 0;
    Auth.Kbn      = C_AUTH_NG;
    Auth.ReasonCd = C_RSN_CARD;
    Auth.Message  = 'カード番号検査不一致';
    leavesr;
  endif;

  if Req.TxnAmt <= 0;
    Auth.Kbn      = C_AUTH_NG;
    Auth.ReasonCd = C_RSN_AMT;
    Auth.Message  = '利用金額不正';
    leavesr;
  endif;

  select;
  when Req.ChannelKbn = '01'
    or Req.ChannelKbn = '02'
    or Req.ChannelKbn = '03'
    or Req.ChannelKbn = '04'
    or Req.ChannelKbn = '05';
    // 01=国内店頭 02=国内ATM 03=EC 04=海外ATM 05=海外加盟店
  other;
    Auth.Kbn      = C_AUTH_NG;
    Auth.ReasonCd = C_RSN_CHANNEL;
    Auth.Message  = 'チャネル区分不正';
    leavesr;
  endsl;

  if %trim(Req.MerchantNo) = *blank;
    Auth.Kbn      = C_AUTH_NG;
    Auth.ReasonCd = C_RSN_CHANNEL;
    Auth.Message  = '加盟店番号未設定';
    leavesr;
  endif;

endSr;

//*********************************************************************
//  会員照会
//*********************************************************************
begsr LoadMember;

  monitor;
    chain Req.CardNo CDACCF;
    if %found(CDACCF);
      Member.Found     = *on;
      Member.CardNo    = Req.CardNo;

      // コピー句の項目名は既存ファイル定義に合わせる。
      Member.AcctNo    = ACCTNO;
      Member.StatusKbn = MBRSTS;
      Member.StopKbn   = STOPKBN;
      Member.ShopLimit = SHPLMT;
      Member.CashLimit = CASHLMT;
      Member.ShopUsed  = SHPUSE;
      Member.CashUsed  = CASHUSE;
      Member.ExpYm     = EXPYM;
      Member.PinNgCnt  = PINNGC;
    else;
      Auth.Kbn      = C_AUTH_NG;
      Auth.ReasonCd = C_RSN_MEMBER;
      Auth.Message  = '会員なし';
    endif;

  on-error;
    Auth.Kbn      = C_AUTH_HOLD;
    Auth.ReasonCd = C_RSN_SYSTEM;
    Auth.Message  = '会員照会異常';
    exsr WriteLog;
  endmon;

endSr;

//*********************************************************************
//  取引種別決定
//*********************************************************************
begsr DecideDomain;

  select;
  when Req.ChannelKbn = '01';
    Req.TxnKbn  = 'P1';       // 国内ショッピング
    Req.DispKbn = 'S';        // ショッピング
    Req.FeeKbn  = '00';       // なし

  when Req.ChannelKbn = '02';
    Req.TxnKbn  = 'C1';       // 国内キャッシング
    Req.DispKbn = 'K';        // キャッシング
    Req.FeeKbn  = '00';

  when Req.ChannelKbn = '03';
    Req.TxnKbn  = 'P1';       // 国内ショッピング
    Req.DispKbn = 'S';
    Req.FeeKbn  = '00';

  when Req.ChannelKbn = '04';
    // 海外ATMキャッシングの取引区分・手数料はオンラインオーソリ専用
    // プログラムが確定する。ここでは表示用区分のみ設定し、取引区分は
    // 付与しない。
    Req.DispKbn = 'K';

  when Req.ChannelKbn = '05';
    Req.TxnKbn  = 'P2';       // 海外ショッピング
    Req.DispKbn = 'S';
    Req.FeeKbn  = 'FB';       // 国際ブランド立替手数料
    Auth.FeeAmt = %dec(Req.TxnAmt * 0.0160:9:0);

  other;
    Auth.Kbn      = C_AUTH_NG;
    Auth.ReasonCd = C_RSN_CHANNEL;
    Auth.Message  = '取引種別判定不能';
  endsl;

endSr;

//*********************************************************************
//  会員状態確認
//*********************************************************************
begsr CheckMember;

  if not Member.Found;
    Auth.Kbn      = C_AUTH_NG;
    Auth.ReasonCd = C_RSN_MEMBER;
    Auth.Message  = '会員状態未取得';
    leavesr;
  endif;

  if Member.StatusKbn <> '1';
    Auth.Kbn      = C_AUTH_NG;
    Auth.ReasonCd = C_RSN_MEMBER;
    Auth.Message  = '会員状態利用不可';
    leavesr;
  endif;

  if Member.StopKbn <> '0';
    Auth.Kbn      = C_AUTH_NG;
    Auth.ReasonCd = C_RSN_STOP;
    Auth.Message  = 'カード停止中';
    leavesr;
  endif;

  if Member.ExpYm < Work.TodayYm;
    Auth.Kbn      = C_AUTH_NG;
    Auth.ReasonCd = C_RSN_EXPIRE;
    Auth.Message  = '有効期限切れ';
    leavesr;
  endif;

  if Req.DispKbn = 'K' and Member.PinNgCnt >= 3;
    Auth.Kbn      = C_AUTH_NG;
    Auth.ReasonCd = C_RSN_STOP;
    Auth.Message  = '暗証番号相違回数超過';
    leavesr;
  endif;

endSr;

//*********************************************************************
//  既存オーソリ重複確認
//*********************************************************************
begsr CheckDuplicate;

  Auth.DupFound = *off;

  monitor;
    setll (Req.CardNo:Req.TranDate) CDTXNF;

    dou %eof(CDTXNF);
      reade (Req.CardNo:Req.TranDate) CDTXNF;
      if %eof(CDTXNF);
        leave;
      endif;

      if TXNAMT = Req.TxnAmt
       and MERCHNO = Req.MerchantNo
       and TERMID = Req.TermId
       and SETLKBN = 'H';

        Auth.DupFound = *on;
        leave;
      endif;
    enddo;

    if Auth.DupFound;
      Auth.Kbn      = C_AUTH_NG;
      Auth.ReasonCd = C_RSN_DUP;
      Auth.Message  = '同一オーソリ受付済';
    endif;

  on-error;
    Auth.Kbn      = C_AUTH_HOLD;
    Auth.ReasonCd = C_RSN_SYSTEM;
    Auth.Message  = '重複確認異常';
    exsr WriteLog;
  endmon;

endSr;

//*********************************************************************
//  リスク照会
//*********************************************************************
begsr CheckRisk;

  clear Auth.RiskRank;
  clear Auth.RiskScore;
  wkRtnCd = '00';

  monitor;
    callp CA113R(Req.CardNo:
                 Req.TxnAmt:
                 Req.ChannelKbn:
                 Auth.RiskRank:
                 Auth.RiskScore:
                 wkRtnCd);

    Auth.RtnCd = wkRtnCd;

    if wkRtnCd <> '00';
      Auth.Kbn      = C_AUTH_HOLD;
      Auth.ReasonCd = C_RSN_SYSTEM;
      Auth.Message  = 'リスク照会応答異常';
      leavesr;
    endif;

    if Auth.RiskRank = 'D';
      Auth.Kbn      = C_AUTH_NG;
      Auth.ReasonCd = C_RSN_RISK;
      Auth.Message  = '不正利用疑義';
      leavesr;
    endif;

    if Auth.RiskRank = 'C' and Req.TxnAmt >= 100000;
      Auth.Kbn      = C_AUTH_NG;
      Auth.ReasonCd = C_RSN_RISK;
      Auth.Message  = '高額リスク取引';
      leavesr;
    endif;

  on-error;
    Auth.Kbn      = C_AUTH_HOLD;
    Auth.ReasonCd = C_RSN_SYSTEM;
    Auth.Message  = 'リスク照会例外';
    exsr WriteLog;
  endmon;

endSr;

//*********************************************************************
//  利用可能額確認
//*********************************************************************
begsr CheckLimit;

  wkAmount = Req.TxnAmt + Auth.FeeAmt;

  select;
  when Req.DispKbn = 'S';
    Auth.AvailAmt = Member.ShopLimit - Member.ShopUsed;

  when Req.DispKbn = 'K';
    Auth.AvailAmt = Member.CashLimit - Member.CashUsed;

  other;
    Auth.Kbn      = C_AUTH_NG;
    Auth.ReasonCd = C_RSN_CHANNEL;
    Auth.Message  = '利用区分不正';
    leavesr;
  endsl;

  if Auth.AvailAmt < wkAmount;
    Auth.Kbn      = C_AUTH_NG;
    Auth.ReasonCd = C_RSN_LIMIT;
    Auth.Message  = '利用可能額不足';
    leavesr;
  endif;

  // 海外ATMは少額多重抑止のため下限を設ける。
  if Req.ChannelKbn = '04' and Req.TxnAmt < 1000;
    Auth.Kbn      = C_AUTH_NG;
    Auth.ReasonCd = C_RSN_AMT;
    Auth.Message  = '海外ATM最低金額未満';
    leavesr;
  endif;

endSr;

//*********************************************************************
//  承認番号採番
//*********************************************************************
begsr Approve;

  wkRtnCd = '00';

  monitor;
    callp CA114R(Req.CardNo:
                 Req.TranDate:
                 Req.TranTime:
                 Req.MerchantNo:
                 Auth.No:
                 wkRtnCd);

    if wkRtnCd = '00' and Auth.No <> *blank;
      Auth.Kbn      = C_AUTH_OK;
      Auth.ReasonCd = C_RSN_NORMAL;
      Auth.Message  = '承認';
    else;
      Auth.Kbn      = C_AUTH_HOLD;
      Auth.ReasonCd = C_RSN_SYSTEM;
      Auth.Message  = '承認番号採番不可';
    endif;

  on-error;
    Auth.Kbn      = C_AUTH_HOLD;
    Auth.ReasonCd = C_RSN_SYSTEM;
    Auth.Message  = '承認番号採番例外';
    exsr WriteLog;
  endmon;

endSr;

//*********************************************************************
//  結果登録
//*********************************************************************
begsr WriteResult;

  monitor;
    if Auth.Kbn = C_AUTH_OK;

      clear CDTXNR;
      CARDNO   = Req.CardNo;
      ACCTNO   = Member.AcctNo;
      TRANDATE = Req.TranDate;
      TRANTIME = Req.TranTime;
      REQID    = Req.RequestId;
      AUTHNO   = Auth.No;
      TXNKBN   = Req.TxnKbn;
      CHNLKBN  = Req.ChannelKbn;
      DISPKBN  = Req.DispKbn;
      TXNAMT   = Req.TxnAmt;
      FEEAMT   = Auth.FeeAmt;
      FEEKBN   = Req.FeeKbn;
      SETLKBN  = Req.SetlKbn;
      MERCHNO  = Req.MerchantNo;
      TERMID   = Req.TermId;
      RSKRANK  = Auth.RiskRank;
      RSKSCR   = Auth.RiskScore;
      CRTTS    = Work.NowTs;

      write CDTXNR;

    else;

      clear CDAUTHR;
      CARDNO   = Req.CardNo;
      TRANDATE = Req.TranDate;
      TRANTIME = Req.TranTime;
      REQID    = Req.RequestId;
      AUTHKBN  = Auth.Kbn;
      RSNCD    = Auth.ReasonCd;
      TXNKBN   = Req.TxnKbn;
      CHNLKBN  = Req.ChannelKbn;
      TXNAMT   = Req.TxnAmt;
      MERCHNO  = Req.MerchantNo;
      TERMID   = Req.TermId;
      RSKRANK  = Auth.RiskRank;
      RSKSCR   = Auth.RiskScore;
      MSGTXT   = Auth.Message;
      CRTTS    = Work.NowTs;

      write CDAUTHR;

    endif;

  on-error;
    wkMsg = '結果登録異常 ' + %char(%status());
    Auth.Kbn      = C_AUTH_HOLD;
    Auth.ReasonCd = C_RSN_SYSTEM;
    Auth.Message  = %subst(wkMsg:1:60);
    exsr WriteLog;
  endmon;

endSr;

//*********************************************************************
//  ログ出力
//*********************************************************************
begsr WriteLog;

  monitor;
    clear CDLOGR;
    LOGDATE = Req.TranDate;
    LOGTIME = Req.TranTime;
    PGMID   = 'CA112R';
    REQID   = Req.RequestId;
    CARDNO  = Req.CardNo;
    ERRCD   = %char(%status());
    LOGMSG  = Auth.Message;
    CRTTS   = Work.NowTs;

    write CDLOGR;

  on-error;
    // ログ出力失敗はオンライン応答を優先し握りつぶす。
  endmon;

endSr;

//*********************************************************************
//  戻り値設定
//*********************************************************************
begsr ReturnParm;

  oAuthKbn  = Auth.Kbn;
  oAuthNo   = Auth.No;
  oReasonCd = Auth.ReasonCd;
  oMessage  = Auth.Message;

endSr;
