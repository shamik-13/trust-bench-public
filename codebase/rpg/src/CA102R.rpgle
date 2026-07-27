**free
//**********************************************************************
//  変更履歴
//    版数   年月日      担当      概要
//    1.00   2023/04/03  山下      初版作成
//    1.01   2023/09/18  森        PAY-ID照会追加
//    1.02   2024/02/12  佐伯      入金履歴サービス呼出し追加
//    1.03   2024/07/29  山下      ページングキー保持対応
//**********************************************************************
ctl-opt dftactgrp(*no) actgrp(*caller)
        option(*srcstmt:*nodebugio)
        datfmt(*iso) timfmt(*iso)
        bnddir('QC2LE');

//**********************************************************************
//  CA102R  オンライン入金照会画面
//          カード番号またはPAY-IDより入金・消込結果・履歴を日付降順表示
//**********************************************************************

/copy CDPAYFC
/copy CDAPPFC
/copy CDHISTC

dcl-ds SvcReqDs qualified template;
  OperatorId                   char(10);
  BranchCd                     char(4);
  CardNo                       char(16);
  PayId                        char(20);
  FromDate                     packed(8:0);
  ToDate                       packed(8:0);
  MaxRows                      packed(3:0);
  PageKey                      char(64);
end-ds;

dcl-ds SvcRspDs qualified template;
  ReturnCd                     char(2);
  MessageId                    char(7);
  MessageText                  char(80);
  NextKey                      char(64);
  RowCount                     packed(3:0);
  TotalNyukin                  packed(13:0);
  TotalKeshikomi               packed(13:0);
end-ds;

dcl-pr PaymentHistoryServiceX extpgm('PYHISTSVCX');
  svcReq                       likeds(SvcReqDs);
  svcRsp                       likeds(SvcRspDs);
end-pr;

dcl-pi *n;
  InOperatorId                 char(10)   const;
  InBranchCd                   char(4)    const;
  InAuthRank                   char(1)    const;
  InScreenId                   char(8)    const;
  InCardNo                     char(16)   const;
  InPayId                      char(20)   const;
  InPageKey                    char(64);
  OutNextKey                   char(64);
  OutMsgId                     char(7);
  OutMsgText                   char(80);
  OutDecideCd                  char(1);
end-pi;

dcl-ds Work qualified;
  CardNo                       char(16);
  PayId                        char(20);
  MaskCardNo                   char(16);
  TargetKey                    char(20);
  ErrMsg                       char(80);
  Today                        packed(8:0);
  FromDate                     packed(8:0);
  ToDate                       packed(8:0);
  PageNo                       packed(3:0);
  MaxRows                      packed(3:0);
  RowNo                        packed(3:0);
  Found                        ind;
  MoreData                     ind;
  Authorized                   ind;
  CheckOk                      ind;
  UsePayId                     ind;
  UseCardNo                    ind;
end-ds;

dcl-ds Line qualified dim(15);
  DispDate                     packed(8:0);
  PayMethod                    char(2);
  AppStatus                    char(1);
  PayAmt                       packed(13:0);
  AppAmt                       packed(13:0);
  BalanceAmt                   packed(13:0);
  AuthNo                       char(12);
  ShopCd                       char(10);
  ShopName                     char(30);
  Text                         char(80);
end-ds;

dcl-ds Pay qualified;
  CardNo                       char(16);
  PayId                        char(20);
  PayDate                      packed(8:0);
  PayMethod                    char(2);
  PayAmt                       packed(13:0);
  PaySeq                       packed(7:0);
  EntryTime                    packed(6:0);
  BranchCd                     char(4);
  CancelFlg                    char(1);
end-ds;

dcl-ds App qualified;
  CardNo                       char(16);
  PayId                        char(20);
  AppDate                      packed(8:0);
  AppStatus                    char(1);
  AppAmt                       packed(13:0);
  RemainAmt                    packed(13:0);
  AppSeq                       packed(7:0);
  AuthNo                       char(12);
end-ds;

dcl-ds Hist qualified;
  CardNo                       char(16);
  PayId                        char(20);
  HistDate                     packed(8:0);
  AuthNo                       char(12);
  ShopCd                       char(10);
  ShopName                     char(30);
  UseAmt                       packed(13:0);
  ResultCd                     char(2);
end-ds;

dcl-s SvcReq                     likeds(SvcReqDs);
dcl-s SvcRsp                     likeds(SvcRspDs);
dcl-s i                          packed(3:0);
dcl-s j                          packed(3:0);
dcl-s sumOdd                     packed(3:0);
dcl-s sumAll                     packed(5:0);
dcl-s digit                      packed(2:0);
dcl-s wkChar                     char(1);
dcl-s wkNum                      packed(2:0);
dcl-s wkAmt                      packed(13:0);
dcl-s blankKey                   char(64) inz(*blank);

dcl-c C_AUTH_OK                  'A';
dcl-c C_AUTH_NG                  'D';
dcl-c C_STS_FULL                 'F';
dcl-c C_STS_PART                 'P';
dcl-c C_STS_OVER                 'O';
dcl-c C_STS_SKIP                 'S';

dcl-c C_PAY_KOFURI               '10';
dcl-c C_PAY_FURIKOMI             '20';
dcl-c C_PAY_CVS                  '30';

dcl-c C_MSG_NORMAL               'CA10000';
dcl-c C_MSG_NO_INPUT             'CA10201';
dcl-c C_MSG_CARD_ERR             'CA10202';
dcl-c C_MSG_AUTH_ERR             'CA10203';
dcl-c C_MSG_NOTFOUND             'CA10204';
dcl-c C_MSG_SVC_ERR              'CA10205';

//**********************************************************************
//  初期処理
//**********************************************************************
clear OutMsgId;
clear OutMsgText;
clear OutDecideCd;
clear OutNextKey;
clear Work;
clear Line;

Work.CardNo = %trim(InCardNo);
Work.PayId = %trim(InPayId);
Work.Today = %dec(%char(%date():*iso0):8:0);
Work.ToDate = Work.Today;
Work.FromDate = %dec(%char(%date() - %years(2):*iso0):8:0);
Work.MaxRows = 15;
Work.PageNo = 1;

Work.UseCardNo = (%trim(Work.CardNo) <> *blank);
Work.UsePayId = (%trim(Work.PayId) <> *blank);

if not Work.UseCardNo and not Work.UsePayId;
  OutDecideCd = C_AUTH_NG;
  OutMsgId = C_MSG_NO_INPUT;
  OutMsgText = 'カード番号またはPAY-IDを入力してください';
  *inlr = *on;
  return;
endif;

//**********************************************************************
//  入力チェック
//**********************************************************************
if Work.UseCardNo;
  Work.CheckOk = *on;
  sumAll = 0;
  sumOdd = 0;

  for i = 1 to 16;
    wkChar = %subst(Work.CardNo:i:1);
    if wkChar < '0' or wkChar > '9';
      Work.CheckOk = *off;
      leave;
    endif;

    wkNum = %dec(wkChar:1:0);
    if %rem(i:2) = 1;
      digit = wkNum * 2;
      if digit > 9;
        digit -= 9;
      endif;
      sumOdd += digit;
    else;
      sumAll += wkNum;
    endif;
  endfor;

  sumAll += sumOdd;

  if Work.CheckOk and %rem(sumAll:10) <> 0;
    Work.CheckOk = *off;
  endif;

  if not Work.CheckOk;
    OutDecideCd = C_AUTH_NG;
    OutMsgId = C_MSG_CARD_ERR;
    OutMsgText = 'カード番号のチェックディジットが不正です';
    *inlr = *on;
    return;
  endif;
endif;

if Work.UsePayId;
  if %len(%trim(Work.PayId)) < 8;
    OutDecideCd = C_AUTH_NG;
    OutMsgId = C_MSG_CARD_ERR;
    OutMsgText = 'PAY-IDの桁数が不正です';
    *inlr = *on;
    return;
  endif;
endif;

//**********************************************************************
//  照会権限チェック
//**********************************************************************
Work.Authorized = *off;

select;
when InAuthRank = '9';
  Work.Authorized = *on;
when InAuthRank = '5' and InBranchCd <> *blank;
  Work.Authorized = *on;
when InAuthRank = '3' and Work.UsePayId;
  Work.Authorized = *on;
other;
  Work.Authorized = *off;
endsl;

if not Work.Authorized;
  OutDecideCd = C_AUTH_NG;
  OutMsgId = C_MSG_AUTH_ERR;
  OutMsgText = '当該入金照会の権限がありません';
  *inlr = *on;
  return;
endif;

//**********************************************************************
//  履歴サービス照会
//**********************************************************************
clear SvcReq;
clear SvcRsp;

SvcReq.OperatorId = InOperatorId;
SvcReq.BranchCd = InBranchCd;
SvcReq.CardNo = Work.CardNo;
SvcReq.PayId = Work.PayId;
SvcReq.FromDate = Work.FromDate;
SvcReq.ToDate = Work.ToDate;
SvcReq.MaxRows = Work.MaxRows;
SvcReq.PageKey = InPageKey;

monitor;
  callp PaymentHistoryServiceX(SvcReq:SvcRsp);
on-error;
  OutDecideCd = C_AUTH_NG;
  OutMsgId = C_MSG_SVC_ERR;
  OutMsgText = '入金履歴サービスで障害が発生しました';
  *inlr = *on;
  return;
endmon;

if SvcRsp.ReturnCd <> '00';
  OutDecideCd = C_AUTH_NG;
  OutMsgId = SvcRsp.MessageId;
  if OutMsgId = *blank;
    OutMsgId = C_MSG_SVC_ERR;
  endif;
  OutMsgText = SvcRsp.MessageText;
  if OutMsgText = *blank;
    OutMsgText = '入金履歴サービスの応答が不正です';
  endif;
  *inlr = *on;
  return;
endif;

//**********************************************************************
//  入金・消込・利用履歴編集
//**********************************************************************
Work.RowNo = 0;
Work.Found = *off;
Work.MoreData = *off;

dou Work.RowNo >= Work.MaxRows;
  clear Pay;
  clear App;
  clear Hist;

  // CDPAYFC: キーはカード番号/PAY-ID/入金日降順/連番
  if Work.UseCardNo;
    chain (Work.CardNo) CDPAYR Pay;
  else;
    chain (Work.PayId) CDPAYR Pay;
  endif;

  if %found(CDPAYR);
    if Pay.CancelFlg <> '1'
       and Pay.PayDate >= Work.FromDate
       and Pay.PayDate <= Work.ToDate;

      Work.RowNo += 1;
      Work.Found = *on;

      Line(Work.RowNo).DispDate = Pay.PayDate;
      Line(Work.RowNo).PayMethod = Pay.PayMethod;
      Line(Work.RowNo).PayAmt = Pay.PayAmt;
      Line(Work.RowNo).AuthNo = *blank;
      Line(Work.RowNo).ShopCd = *blank;
      Line(Work.RowNo).ShopName = *blank;

      select;
      when Pay.PayMethod = C_PAY_KOFURI;
        Line(Work.RowNo).Text = '口座振替入金';
      when Pay.PayMethod = C_PAY_FURIKOMI;
        Line(Work.RowNo).Text = '振込入金';
      when Pay.PayMethod = C_PAY_CVS;
        Line(Work.RowNo).Text = 'コンビニ入金';
      other;
        Line(Work.RowNo).Text = '入金方法未設定';
      endsl;

      // CDAPPFC: 消込結果
      chain (Pay.CardNo:Pay.PayId:Pay.PaySeq) CDAPPR App;
      if %found(CDAPPR);
        Line(Work.RowNo).AppStatus = App.AppStatus;
        Line(Work.RowNo).AppAmt = App.AppAmt;
        Line(Work.RowNo).BalanceAmt = App.RemainAmt;
        Line(Work.RowNo).AuthNo = App.AuthNo;

        select;
        when App.AppStatus = C_STS_FULL;
          %subst(Line(Work.RowNo).Text:31:10) = '完済';
        when App.AppStatus = C_STS_PART;
          %subst(Line(Work.RowNo).Text:31:10) = '一部消込';
        when App.AppStatus = C_STS_OVER;
          %subst(Line(Work.RowNo).Text:31:10) = '過入金';
        when App.AppStatus = C_STS_SKIP;
          %subst(Line(Work.RowNo).Text:31:10) = '対象外';
        other;
          %subst(Line(Work.RowNo).Text:31:10) = '状態不明';
        endsl;
      else;
        Line(Work.RowNo).AppStatus = C_STS_SKIP;
        Line(Work.RowNo).AppAmt = 0;
        Line(Work.RowNo).BalanceAmt = Pay.PayAmt;
        %subst(Line(Work.RowNo).Text:31:10) = '未消込';
      endif;
    endif;
  endif;

  // CDHISTC: オーソリ履歴参照
  if Work.RowNo < Work.MaxRows;
    if Work.UseCardNo;
      chain (Work.CardNo) CDHISTR Hist;
    else;
      chain (Work.PayId) CDHISTR Hist;
    endif;

    if %found(CDHISTR);
      if Hist.HistDate >= Work.FromDate
         and Hist.HistDate <= Work.ToDate;

        Work.RowNo += 1;
        Work.Found = *on;

        Line(Work.RowNo).DispDate = Hist.HistDate;
        Line(Work.RowNo).PayMethod = *blank;
        Line(Work.RowNo).AppStatus = *blank;
        Line(Work.RowNo).PayAmt = 0;
        Line(Work.RowNo).AppAmt = Hist.UseAmt;
        Line(Work.RowNo).BalanceAmt = 0;
        Line(Work.RowNo).AuthNo = Hist.AuthNo;
        Line(Work.RowNo).ShopCd = Hist.ShopCd;
        Line(Work.RowNo).ShopName = Hist.ShopName;

        select;
        when Hist.ResultCd = '00';
          Line(Work.RowNo).Text = 'オーソリ承認';
        when Hist.ResultCd = '51';
          Line(Work.RowNo).Text = 'オーソリ否決 残高不足';
        when Hist.ResultCd = '54';
          Line(Work.RowNo).Text = 'オーソリ否決 有効期限';
        when Hist.ResultCd = '57';
          Line(Work.RowNo).Text = 'オーソリ否決 取扱不可';
        other;
          Line(Work.RowNo).Text = 'オーソリ照会';
        endsl;
      endif;
    endif;
  endif;

  if not %found(CDPAYR) and not %found(CDHISTR);
    leave;
  endif;

  if Work.RowNo >= Work.MaxRows;
    Work.MoreData = *on;
    leave;
  endif;
enddo;

//**********************************************************************
//  日付降順整列
//**********************************************************************
for i = 1 to Work.RowNo;
  for j = i + 1 to Work.RowNo;
    if Line(j).DispDate > Line(i).DispDate;
      wkAmt = Line(i).PayAmt;
      Line(i).PayAmt = Line(j).PayAmt;
      Line(j).PayAmt = wkAmt;

      wkAmt = Line(i).AppAmt;
      Line(i).AppAmt = Line(j).AppAmt;
      Line(j).AppAmt = wkAmt;

      wkAmt = Line(i).BalanceAmt;
      Line(i).BalanceAmt = Line(j).BalanceAmt;
      Line(j).BalanceAmt = wkAmt;

      wkAmt = Line(i).DispDate;
      Line(i).DispDate = Line(j).DispDate;
      Line(j).DispDate = wkAmt;
    endif;
  endfor;
endfor;

//**********************************************************************
//  応答編集
//**********************************************************************
if not Work.Found and SvcRsp.RowCount = 0;
  OutDecideCd = C_AUTH_NG;
  OutMsgId = C_MSG_NOTFOUND;
  OutMsgText = '該当する入金・消込履歴はありません';
  OutNextKey = blankKey;
else;
  OutDecideCd = C_AUTH_OK;
  OutMsgId = C_MSG_NORMAL;
  OutMsgText = '照会しました';

  if SvcRsp.NextKey <> *blank;
    OutNextKey = SvcRsp.NextKey;
  elseif Work.MoreData;
    OutNextKey = %trim(Work.CardNo) + '/' + %char(Work.Today);
  else;
    OutNextKey = blankKey;
  endif;
endif;

*inlr = *on;
return;
