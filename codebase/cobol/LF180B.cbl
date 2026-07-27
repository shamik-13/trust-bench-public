       IDENTIFICATION DIVISION.
       PROGRAM-ID. LF180B.
      *
      * プログラム名：返戻金支払承認バッチ
      * 概要：計算済み返戻金に対して受付状態、契約者貸付残高、
      *       支払停止条件を検査し、支払可能額の承認仕訳データを
      *       LFACJFへ作成する。貸付控除は貸付残高と未収利息に限定。
      *
      * ============================================================
      * 変更履歴
      * ============================================================
      * 版数  年月日      担当      概要
      * 001   20200101    システム  新規作成
      * ============================================================
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
      *
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LFCVRF ASSIGN TO WS-LFCVRF-FILE
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-LFCVRF-STATUS.

           SELECT LFREQF ASSIGN TO WS-LFREQF-FILE
               ORGANIZATION IS INDEXED
               RECORD KEY IS RQ-REQ-ID
               FILE STATUS IS WS-LFREQF-STATUS.

           SELECT LFLOANF ASSIGN TO WS-LFLOANF-FILE
               ORGANIZATION IS INDEXED
               RECORD KEY IS LN-POL-NO
               FILE STATUS IS WS-LFLOANF-STATUS.

           SELECT LFACJF ASSIGN TO WS-LFACJF-FILE
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-LFACJF-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  LFCVRF.
       COPY LFCVRFC.
      *
       FD  LFREQF.
       COPY LFREQC.
      *
       FD  LFLOANF.
       COPY LFLOANC.
      *
       FD  LFACJF.
       COPY LFACJC.
      *
       WORKING-STORAGE SECTION.
      * ============================================================
      * ファイルパス及びファイル制御変数
      * ============================================================
       01  WS-FILE-VARS.
           05  WS-LFCVRF-FILE             PIC X(256)
               VALUE '/batch/data/LFCVRF.seq'.
           05  WS-LFREQF-FILE             PIC X(256)
               VALUE '/batch/data/LFREQF.vsam'.
           05  WS-LFLOANF-FILE            PIC X(256)
               VALUE '/batch/data/LFLOANF.vsam'.
           05  WS-LFACJF-FILE             PIC X(256)
               VALUE '/batch/data/LFACJF.seq'.
      *
           05  WS-LFCVRF-STATUS           PIC XX.
           05  WS-LFREQF-STATUS           PIC XX.
           05  WS-LFLOANF-STATUS          PIC XX.
           05  WS-LFACJF-STATUS           PIC XX.
      *
      * ============================================================
      * 処理統計カウンタ
      * ============================================================
       01  WS-COUNTERS.
           05  WS-CV-REC-COUNT            PIC 9(8) VALUE 0.
           05  WS-ADJ-REC-COUNT           PIC 9(8) VALUE 0.
           05  WS-SKIP-COUNT              PIC 9(8) VALUE 0.
           05  WS-ERR-COUNT               PIC 9(8) VALUE 0.
      *
      * ============================================================
      * 検証及び計算用フィールド
      * ============================================================
       01  WS-VALIDATION-VARS.
           05  WS-CV-CALC-STATUS          PIC X(2).
           05  WS-REQ-STATUS              PIC X(2).
           05  WS-LOAN-STATUS             PIC X(2).
           05  WS-PAYABLE-AMT             PIC S9(13)V99 VALUE 0.
           05  WS-PAYABLE-AMT-INT         PIC S9(15) VALUE 0.
           05  WS-LOAN-DEDUCT-AMT         PIC S9(13)V99 VALUE 0.
           05  WS-ADJ-ID-COUNTER          PIC 9(8) VALUE 0.
           05  WS-ADJ-ID-STR              PIC 9(10).
      *
      * ============================================================
      * 処理フロー制御
      * ============================================================
       01  WS-PROCESS-FLAGS.
           05  WS-EOF-FLAG                PIC X VALUE 'N'.
           05  WS-LOAN-FOUND              PIC X VALUE 'N'.
           05  WS-REQ-FOUND               PIC X VALUE 'N'.
      *
      * ============================================================
       PROCEDURE DIVISION.
      * ============================================================
       MAIN-PROCEDURE.
      * ============================================================
      * ファイルオープン
      * ============================================================
           OPEN INPUT LFCVRF.
           IF WS-LFCVRF-STATUS NOT = '00'
               DISPLAY '返戻金ファイル オープン失敗 ST='
               DISPLAY WS-LFCVRF-STATUS
               MOVE 8 TO RETURN-CODE
               GOBACK
           END-IF.

           OPEN INPUT LFREQF.
           IF WS-LFREQF-STATUS NOT = '00'
               DISPLAY '申込ファイル オープン失敗 ST='
               DISPLAY WS-LFREQF-STATUS
               MOVE 8 TO RETURN-CODE
               GOBACK
           END-IF.

           OPEN INPUT LFLOANF.
           IF WS-LFLOANF-STATUS NOT = '00'
               DISPLAY '貸付ファイル オープン失敗 ST='
               DISPLAY WS-LFLOANF-STATUS
               MOVE 8 TO RETURN-CODE
               GOBACK
           END-IF.

           OPEN OUTPUT LFACJF.
           IF WS-LFACJF-STATUS NOT = '00'
               DISPLAY '仕訳ファイル オープン失敗 ST='
               DISPLAY WS-LFACJF-STATUS
               MOVE 8 TO RETURN-CODE
               GOBACK
           END-IF.
      *
      * ============================================================
      * メインループ：返戻金ファイル処理
      * ============================================================
           MOVE 'N' TO WS-EOF-FLAG.

           PERFORM UNTIL WS-EOF-FLAG = 'Y'
               READ LFCVRF INTO LFCVRF-REC
                   AT END
                       MOVE 'Y' TO WS-EOF-FLAG
                   NOT AT END
                       ADD 1 TO WS-CV-REC-COUNT
                       PERFORM VALIDATE-AND-PROCESS
               END-READ
           END-PERFORM.
      *
      * ============================================================
      * ファイルクローズ
      * ============================================================
           CLOSE LFCVRF.
           CLOSE LFREQF.
           CLOSE LFLOANF.
           CLOSE LFACJF.
      *
      * ============================================================
      * 処理終了
      * ============================================================
           IF WS-ERR-COUNT = 0
               MOVE 0 TO RETURN-CODE
           ELSE
               MOVE 4 TO RETURN-CODE
           END-IF.

           DISPLAY '返戻金支払承認バッチ終了'.
           DISPLAY '処理件数=' WS-ADJ-REC-COUNT.
           DISPLAY 'スキップ件数=' WS-SKIP-COUNT.
           DISPLAY 'エラー件数=' WS-ERR-COUNT.

           GOBACK.
      *
      * ============================================================
       VALIDATE-AND-PROCESS.
      * ============================================================
      * 返戻金レコード（LFCVRF-REC）の検証と承認仕訳作成
      * ============================================================
      * 検査1：計算状態コード確認
      * ============================================================
           MOVE CO-CALC-STATUS-KBN TO WS-CV-CALC-STATUS.

           IF WS-CV-CALC-STATUS NOT = '01'
               EVALUATE WS-CV-CALC-STATUS
                   WHEN '08'
                       ADD 1 TO WS-SKIP-COUNT
                   WHEN '09'
                       ADD 1 TO WS-ERR-COUNT
                       DISPLAY 'エラー：無効な返戻金'
                       DISPLAY 'ポリシー=' CO-POL-NO
                   WHEN OTHER
                       ADD 1 TO WS-ERR-COUNT
                       DISPLAY 'エラー：計算状態不正'
                       DISPLAY 'ポリシー=' CO-POL-NO
                       DISPLAY '状態=' WS-CV-CALC-STATUS
               END-EVALUATE
               EXIT PARAGRAPH
           END-IF.
      *
      * ============================================================
      * 検査2：申込ファイルから申請情報取得
      * ============================================================
           MOVE 'N' TO WS-REQ-FOUND.
           MOVE CO-POL-NO TO RQ-REQ-ID.

           READ LFREQF INTO LFREQF-REC
               INVALID KEY
                   ADD 1 TO WS-ERR-COUNT
                   DISPLAY 'エラー：申込レコード未検出'
                   DISPLAY 'ポリシー=' CO-POL-NO
                   EXIT PARAGRAPH
               NOT INVALID KEY
                   MOVE 'Y' TO WS-REQ-FOUND
           END-READ.
      *
      * ============================================================
      * 検査3：申請受付状態確認
      * ============================================================
           MOVE RQ-REQ-STATUS-KBN TO WS-REQ-STATUS.

           IF WS-REQ-STATUS NOT = '01'
               ADD 1 TO WS-SKIP-COUNT
               DISPLAY 'スキップ：申込未受付'
               DISPLAY 'ポリシー=' CO-POL-NO
               DISPLAY '状態=' WS-REQ-STATUS
               EXIT PARAGRAPH
           END-IF.
      *
      * ============================================================
      * 貸付情報取得（キー検索）
      * ============================================================
           MOVE 'N' TO WS-LOAN-FOUND.
           MOVE 0 TO WS-LOAN-DEDUCT-AMT.
           MOVE CO-POL-NO TO LN-POL-NO.

           READ LFLOANF INTO LFLOANF-REC
               INVALID KEY
                   CONTINUE
               NOT INVALID KEY
                   MOVE 'Y' TO WS-LOAN-FOUND
                   PERFORM CALCULATE-LOAN-DEDUCTION
           END-READ.
      *
      * ============================================================
      * 支払可能額計算
      * ============================================================
           COMPUTE WS-PAYABLE-AMT =
               CO-CV-AMT - WS-LOAN-DEDUCT-AMT.

           IF WS-PAYABLE-AMT < 0
               MOVE 0 TO WS-PAYABLE-AMT
           END-IF.
      *
      * ============================================================
      * 承認仕訳データ生成
      * ============================================================
           ADD 1 TO WS-ADJ-ID-COUNTER.
           MOVE WS-ADJ-ID-COUNTER TO WS-ADJ-ID-STR.

           INITIALIZE LFACJF-REC.
           MOVE WS-ADJ-ID-STR TO AJ-ADJ-ID.
           MOVE CO-POL-NO TO AJ-POL-NO.
           MOVE '03' TO AJ-EVENT-KBN.
           MOVE '1100' TO AJ-DR-ACCT-CD.
           MOVE '3150' TO AJ-CR-ACCT-CD.

           COMPUTE WS-PAYABLE-AMT-INT =
               FUNCTION INTEGER(WS-PAYABLE-AMT * 100).
           MOVE WS-PAYABLE-AMT-INT TO AJ-AMT.

           MOVE '00' TO AJ-POST-STATUS-KBN.

           WRITE LFACJF-REC.

           IF WS-LFACJF-STATUS NOT = '00'
               ADD 1 TO WS-ERR-COUNT
               DISPLAY 'エラー：仕訳書込失敗'
               DISPLAY 'ST=' WS-LFACJF-STATUS
               DISPLAY 'ポリシー=' CO-POL-NO
               EXIT PARAGRAPH
           END-IF.

           ADD 1 TO WS-ADJ-REC-COUNT.
      *
      * ============================================================
       CALCULATE-LOAN-DEDUCTION.
      * ============================================================
      * 貸付控除額：貸付残高 + 未収利息
      * 対象：貸付状態が正常なもののみ
      * ============================================================
           MOVE LN-LOAN-STATUS-KBN TO WS-LOAN-STATUS.

           IF WS-LOAN-STATUS = '01'
               COMPUTE WS-LOAN-DEDUCT-AMT =
                   LN-LOAN-BAL-AMT + LN-ACCRUED-INT-AMT
           ELSE
               MOVE 0 TO WS-LOAN-DEDUCT-AMT
           END-IF.
