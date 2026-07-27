**free
//**********************************************************************
//  変更履歴
//  版数  年月日    担当    概要
//  0001  20190401  佐藤    初版作成
//  0002  20210315  田村    リボ請求確定結果突合追加
//  0003  20221130  井上    オーソリ照会画面向け与信判定項目追加
//  0004  20240520  森      確定済リボ明細の表示転記処理を整理
//**********************************************************************
ctl-opt dftactgrp(*no) actgrp(*caller)
        option(*srcstmt:*nodebugio)
        bnddir('QC2LE')
        datfmt(*iso) timfmt(*iso);

//---------------------------------------------------------------------
//  明細照会オンラインRPG  CA111R
//  カード番号・対象サイクルより確定明細を照会し、オンライン表示行を作成
//---------------------------------------------------------------------

/copy CDCAPTC
/copy CDRSLDFC
/copy CDSTMTF2C
/copy CDREVFC

dcl-pr CA111R extpgm('CA111R');
  pCardNo        char(16) const;
  pCycleYm       packed(6:0) const;
  pReqAmt        packed(11:0) const;
  pTranKbn       char(2) const;
  pAuthResult    char(1);
  pAuthCode      char(6);
  pRespMsg       char(60);
  pDtlCnt        packed(3:0);
end-pr;

dcl-pi CA111R;
  pCardNo        char(16) const;
  pCycleYm       packed(6:0) const;
  pReqAmt        packed(11:0) const;
  pTranKbn       char(2) const;
  pAuthResult    char(1);
  pAuthCode      char(6);
  pRespMsg       char(60);
  pDtlCnt        packed(3:0);
end-pi;

dcl-ds StmtHdr qualified;
  CardNo         char(16);
  CycleYm        packed(6:0);
  StmtStatus     char(1);
  CloseYmd       packed(8:0);
  PayYmd         packed(8:0);
  BillAmt        packed(11:0);
  MinPayAmt      packed(11:0);
  AuthLimit      packed(11:0);
  UsedAmt        packed(11:0);
  DelqKbn        char(1);
  StopKbn        char(1);
end-ds;

dcl-ds SalesDet qualified;
  CardNo         char(16);
  CycleYm        packed(6:0);
  SlipNo         char(12);
  UseYmd         packed(8:0);
  ShopCd         char(10);
  ShopNm         char(40);
  TranKbn        char(2);
  PayKbn         char(1);
  UseAmt         packed(11:0);
  TaxAmt         packed(9:0);
  CancelKbn      char(1);
  AuthNo         char(6);
end-ds;

dcl-ds RsldDet qualified;
  CardNo         char(16);
  CycleYm        packed(6:0);
  SlipNo         char(12);
  RsldStatus     char(1);
  SlideTier      char(2);
  RevBal         packed(11:0);
  PrinAmt        packed(11:0);
  FeeAmt         packed(11:0);
  PayAmt         packed(11:0);
end-ds;

dcl-ds RevMst qualified;
  CardNo         char(16);
  RevStatus      char(2);
  StartYmd       packed(8:0);
  StopYmd        packed(8:0);
  LimitAmt       packed(11:0);
  MonthPayAmt    packed(11:0);
  FeeRate        packed(5:4);
end-ds;

dcl-ds DspLine qualified dim(30);
  UseYmd         packed(8:0);
  SlipNo         char(12);
  ShopNm         char(40);
  TranKbn        char(2);
  SlideTier      char(2);
  PrinAmt        packed(11:0);
  FeeAmt         packed(11:0);
  PayAmt         packed(11:0);
  EditText       char(70);
end-ds;

dcl-s wkIdx          packed(3:0) inz(0);
dcl-s wkDtlMax       packed(3:0) inz(30);
dcl-s wkAvailAmt     packed(11:0) inz(0);
dcl-s wkAuthNo       char(6) inz(*blank);
dcl-s wkMsg          char(60) inz(*blank);
dcl-s wkErr          ind inz(*off);
dcl-s wkFoundHdr     ind inz(*off);
dcl-s wkFoundRev     ind inz(*off);
dcl-s wkEndSales     ind inz(*off);

//---------------------------------------------------------------------
//  初期化
//---------------------------------------------------------------------
clear DspLine;
pAuthResult = '9';
pAuthCode   = *blank;
pRespMsg    = *blank;
pDtlCnt     = 0;

//---------------------------------------------------------------------
//  入力チェック
//---------------------------------------------------------------------
if pCardNo = *blank or pCycleYm < 200001;
  pAuthResult = 'D';
  pRespMsg    = '入力項目エラー';
  return;
endif;

if pReqAmt < 0;
  pAuthResult = 'D';
  pRespMsg    = '利用金額エラー';
  return;
endif;

//---------------------------------------------------------------------
//  請求ヘッダ取得
//---------------------------------------------------------------------
monitor;
  chain (pCardNo:pCycleYm) CDSTMTF2 StmtHdr;
  if %found(CDSTMTF2);
    wkFoundHdr = *on;
  endif;
on-error;
  wkErr = *on;
endmon;

if wkErr;
  pAuthResult = 'D';
  pRespMsg    = '請求ヘッダ読込エラー';
  return;
endif;

if not wkFoundHdr;
  pAuthResult = 'D';
  pRespMsg    = '対象サイクル未確定';
  return;
endif;

if StmtHdr.StmtStatus <> 'C';
  pAuthResult = 'D';
  pRespMsg    = '請求未確定または対象外';
  return;
endif;

if StmtHdr.StopKbn = '1';
  pAuthResult = 'D';
  pRespMsg    = 'カード利用停止中';
  return;
endif;

if StmtHdr.DelqKbn = '1';
  pAuthResult = 'D';
  pRespMsg    = '延滞による承認不可';
  return;
endif;

//---------------------------------------------------------------------
//  リボ契約取得
//---------------------------------------------------------------------
monitor;
  chain pCardNo CDREVF RevMst;
  if %found(CDREVF);
    wkFoundRev = *on;
  endif;
on-error;
  wkErr = *on;
endmon;

if wkErr;
  pAuthResult = 'D';
  pRespMsg    = 'リボ契約読込エラー';
  return;
endif;

//---------------------------------------------------------------------
//  オンラインオーソリ簡易判定
//---------------------------------------------------------------------
wkAvailAmt = StmtHdr.AuthLimit - StmtHdr.UsedAmt;

select;
when pTranKbn = '01';
  if pReqAmt <= wkAvailAmt;
    pAuthResult = 'A';
  else;
    pAuthResult = 'D';
    pRespMsg    = '利用可能額不足';
    return;
  endif;

when pTranKbn = '02';
  pAuthResult = 'A';

other;
  pAuthResult = 'D';
  pRespMsg    = '取引区分エラー';
  return;
endsl;

wkAuthNo = %subst(%char(%time():*hms0):1:6);
pAuthCode = wkAuthNo;

//---------------------------------------------------------------------
//  売上確定明細走査
//---------------------------------------------------------------------
setll (pCardNo:pCycleYm) CDCAPTF;
dou wkEndSales or wkIdx >= wkDtlMax;

  monitor;
    reade (pCardNo:pCycleYm) CDCAPTF SalesDet;
    if %eof(CDCAPTF);
      wkEndSales = *on;
      leave;
    endif;
  on-error;
    wkErr = *on;
    leave;
  endmon;

  if SalesDet.CancelKbn = '1';
    iter;
  endif;

  wkIdx += 1;
  clear DspLine(wkIdx);

  DspLine(wkIdx).UseYmd    = SalesDet.UseYmd;
  DspLine(wkIdx).SlipNo    = SalesDet.SlipNo;
  DspLine(wkIdx).ShopNm    = SalesDet.ShopNm;
  DspLine(wkIdx).TranKbn   = SalesDet.TranKbn;
  DspLine(wkIdx).PrinAmt   = SalesDet.UseAmt;
  DspLine(wkIdx).FeeAmt    = 0;
  DspLine(wkIdx).PayAmt    = SalesDet.UseAmt;
  DspLine(wkIdx).EditText  = %trim(SalesDet.ShopNm);

  if SalesDet.PayKbn = 'R';

    clear RsldDet;

    monitor;
      chain (SalesDet.CardNo:SalesDet.CycleYm:SalesDet.SlipNo)
            CDRSLDF RsldDet;
    on-error;
      wkErr = *on;
      leave;
    endmon;

    if %found(CDRSLDF);

      DspLine(wkIdx).SlideTier = RsldDet.SlideTier;

      if RsldDet.RsldStatus = 'C';

        if wkFoundRev and RevMst.RevStatus = '01';

          // 確定済リボ明細を表示用にそのまま転記する（再計算はしない）。
          // 元金・手数料・請求額は確定バッチが算出済の値を参照する。
          DspLine(wkIdx).PrinAmt = RsldDet.PrinAmt;
          DspLine(wkIdx).FeeAmt  = RsldDet.FeeAmt;
          DspLine(wkIdx).PayAmt  = RsldDet.PayAmt;
          DspLine(wkIdx).EditText = %trim(SalesDet.ShopNm) + ' リボ確定';

        elseif wkFoundRev and
              (RevMst.RevStatus = '02' or RevMst.RevStatus = '03');

          RsldDet.RsldStatus = 'S';
          RsldDet.PrinAmt    = 0;
          RsldDet.FeeAmt     = 0;
          RsldDet.PayAmt     = 0;

          DspLine(wkIdx).PrinAmt  = 0;
          DspLine(wkIdx).FeeAmt   = 0;
          DspLine(wkIdx).PayAmt   = 0;
          DspLine(wkIdx).EditText = 'リボ契約停止・解約により対象外';

        else;

          RsldDet.RsldStatus = 'S';
          DspLine(wkIdx).PrinAmt  = 0;
          DspLine(wkIdx).FeeAmt   = 0;
          DspLine(wkIdx).PayAmt   = 0;
          DspLine(wkIdx).EditText = 'リボ契約なし';

        endif;

      elseif RsldDet.RsldStatus = 'S';

        DspLine(wkIdx).PrinAmt  = 0;
        DspLine(wkIdx).FeeAmt   = 0;
        DspLine(wkIdx).PayAmt   = 0;
        DspLine(wkIdx).EditText = 'リボ請求対象外';

      else;

        DspLine(wkIdx).PrinAmt  = 0;
        DspLine(wkIdx).FeeAmt   = 0;
        DspLine(wkIdx).PayAmt   = 0;
        DspLine(wkIdx).EditText = 'リボ請求状態不正';

      endif;

    else;

      DspLine(wkIdx).PrinAmt  = 0;
      DspLine(wkIdx).FeeAmt   = 0;
      DspLine(wkIdx).PayAmt   = 0;
      DspLine(wkIdx).EditText = 'リボ請求未作成';

    endif;

  endif;

enddo;

if wkErr;
  pAuthResult = 'D';
  pRespMsg    = '明細読込エラー';
  pDtlCnt     = wkIdx;
  return;
endif;

pDtlCnt = wkIdx;

if pAuthResult = 'A';
  if pDtlCnt = 0;
    pRespMsg = '承認・表示明細なし';
  else;
    pRespMsg = '承認・明細照会正常';
  endif;
else;
  pRespMsg = '判定未完了';
endif;

return;
