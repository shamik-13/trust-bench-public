       IDENTIFICATION DIVISION.
       PROGRAM-ID. LF100B.
      * 責任準備金日次再計算バッチ
      * 契約状態が有効な保険契約の責任準備金を再評価し
      * 計算状態区分をLFRSVFへ更新する
      *
      * 版数   年月日     担当      概要
      * 1.0    20210615   初期     新規作成
      * 2.0    20230621   修正     入力不整合エラー処理追加

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LFPOLF2-FILE ASSIGN TO LFPOLF2
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-LFPOLF2-ST.
           SELECT LFCVPF-FILE ASSIGN TO LFCVPF
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-LFCVPF-ST.
           SELECT LFRSVF-FILE ASSIGN TO LFRSVF
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-LFRSVF-ST.

       DATA DIVISION.
       FILE SECTION.
       FD  LFPOLF2-FILE.
       COPY LFPOLF2C.

       FD  LFCVPF-FILE.
       COPY LFCVPFC.

       FD  LFRSVF-FILE.
       COPY LFRSVC.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05  WS-LFPOLF2-ST       PIC XX VALUE SPACES.
           05  WS-LFCVPF-ST        PIC XX VALUE SPACES.
           05  WS-LFRSVF-ST        PIC XX VALUE SPACES.

       01  WS-PROCESS-FLAGS.
           05  WS-EOF-LFPOLF2      PIC 9 VALUE 0.
           05  WS-EOF-LFCVPF       PIC 9 VALUE 0.
           05  WS-VALID-CONTRACT   PIC 9 VALUE 0.
           05  WS-CALC-REQUIRED    PIC 9 VALUE 0.
           05  WS-DATA-ERROR       PIC 9 VALUE 0.
           05  WS-POLICY-FOUND     PIC 9 VALUE 0.

       01  WS-COUNTERS.
           05  WS-RECORD-COUNT     PIC 9(8) VALUE 0.
           05  WS-UPDATE-COUNT     PIC 9(8) VALUE 0.
           05  WS-ERROR-COUNT      PIC 9(8) VALUE 0.
           05  WS-SKIP-COUNT       PIC 9(8) VALUE 0.

       01  WS-WORK-FIELDS.
           05  WS-CURRENT-DATE     PIC 9(8) VALUE 0.
           05  WS-ELAPSED-CALC     PIC 9(5) VALUE 0.
           05  WS-RESERVE-CALC     PIC 9(13)V99 VALUE 0.
           05  WS-STATUS-CODE      PIC 99 VALUE 0.
           05  WS-SEARCH-POL-NO    PIC X(10) VALUE SPACES.

       01  WS-CONSTANTS.
           05  CV-STATUS-TARGET    PIC 99 VALUE 01.
           05  CV-STATUS-EXCLUDE   PIC 99 VALUE 08.
           05  CV-STATUS-INVALID   PIC 99 VALUE 09.
           05  CONTRACT-ACTIVE     PIC X VALUE '1'.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM INITIALIZATION.
           PERFORM OPEN-FILES.
           IF WS-LFPOLF2-ST NOT = '00' OR
              WS-LFCVPF-ST NOT = '00' OR
              WS-LFRSVF-ST NOT = '00'
               PERFORM FILE-OPEN-ERROR
               GO TO MAIN-END
           END-IF.
           PERFORM UNTIL WS-EOF-LFCVPF = 1
               PERFORM READ-LFCVPF
               IF WS-EOF-LFCVPF = 1
                   EXIT PERFORM
               END-IF
               ADD 1 TO WS-RECORD-COUNT
               PERFORM VALIDATE-CONTRACT
               IF WS-VALID-CONTRACT = 1
                   PERFORM READ-POLICY-MASTER
                   IF WS-DATA-ERROR = 0
                       PERFORM CALCULATE-RESERVE
                       PERFORM UPDATE-RESERVE-FILE
                       IF WS-LFRSVF-ST = '00'
                           ADD 1 TO WS-UPDATE-COUNT
                       ELSE
                           ADD 1 TO WS-ERROR-COUNT
                       END-IF
                   ELSE
                       ADD 1 TO WS-ERROR-COUNT
                   END-IF
               ELSE
                   ADD 1 TO WS-SKIP-COUNT
               END-IF
           END-PERFORM.
           PERFORM CLOSE-FILES.
           PERFORM PRINT-SUMMARY.
           MOVE 0 TO RETURN-CODE.
           GO TO MAIN-END.

       MAIN-END.
           GOBACK.

       INITIALIZATION.
           MOVE 0 TO WS-EOF-LFPOLF2.
           MOVE 0 TO WS-EOF-LFCVPF.
           MOVE 0 TO WS-VALID-CONTRACT.
           MOVE 0 TO WS-DATA-ERROR.
           MOVE 0 TO WS-RECORD-COUNT.
           MOVE 0 TO WS-UPDATE-COUNT.
           MOVE 0 TO WS-ERROR-COUNT.
           MOVE 0 TO WS-SKIP-COUNT.
           ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD.

       OPEN-FILES.
           OPEN INPUT LFPOLF2-FILE.
           OPEN INPUT LFCVPF-FILE.
           OPEN OUTPUT LFRSVF-FILE.

       READ-LFCVPF.
           READ LFCVPF-FILE INTO LFCVPF-REC
               AT END
                   MOVE 1 TO WS-EOF-LFCVPF
               NOT AT END
                   MOVE '00' TO WS-LFCVPF-ST
           END-READ.

       VALIDATE-CONTRACT.
           MOVE 0 TO WS-VALID-CONTRACT.
           MOVE 0 TO WS-DATA-ERROR.
           IF CI-CV-STATUS-KBN = CV-STATUS-TARGET
               MOVE 1 TO WS-VALID-CONTRACT
           ELSE
               IF CI-CV-STATUS-KBN = CV-STATUS-INVALID
                   MOVE 1 TO WS-DATA-ERROR
               END-IF
           END-IF.

       READ-POLICY-MASTER.
           MOVE 0 TO WS-DATA-ERROR.
           MOVE 0 TO WS-POLICY-FOUND.
           MOVE 0 TO WS-EOF-LFPOLF2.
           MOVE CI-POL-NO TO WS-SEARCH-POL-NO.
           CLOSE LFPOLF2-FILE.
           OPEN INPUT LFPOLF2-FILE.
           PERFORM UNTIL WS-EOF-LFPOLF2 = 1
               READ LFPOLF2-FILE INTO LFPOLF2-REC
                   AT END
                       MOVE 1 TO WS-EOF-LFPOLF2
                   NOT AT END
                       IF PO-POL-NO = WS-SEARCH-POL-NO
                           MOVE 1 TO WS-POLICY-FOUND
                           MOVE 1 TO WS-EOF-LFPOLF2
                       END-IF
               END-READ
           END-PERFORM.
           IF WS-POLICY-FOUND = 0
               MOVE 1 TO WS-DATA-ERROR
           END-IF.

       CALCULATE-RESERVE.
           MOVE 0 TO WS-STATUS-CODE.
           IF PO-CONTRACT-STATUS-KBN = CONTRACT-ACTIVE AND
              CI-ELAPSED-MONTH-CNT >= 0 AND
              PO-SUM-INSURED-AMT > 0
               COMPUTE WS-RESERVE-CALC =
                   PO-SUM-INSURED-AMT *
                   (CI-ELAPSED-MONTH-CNT / 12)
               IF WS-RESERVE-CALC < 0
                   MOVE 1 TO WS-DATA-ERROR
               END-IF
           ELSE
               MOVE 1 TO WS-DATA-ERROR
           END-IF.

       UPDATE-RESERVE-FILE.
           IF WS-DATA-ERROR = 1
               MOVE '99' TO RS-CALC-STATUS-KBN
           ELSE
               MOVE '01' TO RS-CALC-STATUS-KBN
           END-IF.
           MOVE CI-POL-NO TO RS-POL-NO.
           MOVE WS-CURRENT-DATE TO RS-VALUATION-DATE.
           IF WS-DATA-ERROR = 0
               MOVE WS-RESERVE-CALC TO RS-RESERVE-AMT
           ELSE
               MOVE 0 TO RS-RESERVE-AMT
           END-IF.
           MOVE 0 TO RS-NET-PREMIUM-AMT.
           MOVE 0 TO RS-INTEREST-RATE-CD.
           WRITE LFRSVF-REC.

       CLOSE-FILES.
           CLOSE LFPOLF2-FILE.
           CLOSE LFCVPF-FILE.
           CLOSE LFRSVF-FILE.

       FILE-OPEN-ERROR.
           DISPLAY '警告: ファイルオープン失敗'.
           DISPLAY ' LFPOLF2=' WS-LFPOLF2-ST.
           DISPLAY ' LFCVPF=' WS-LFCVPF-ST.
           DISPLAY ' LFRSVF=' WS-LFRSVF-ST.
           MOVE 8 TO RETURN-CODE.

       PRINT-SUMMARY.
           DISPLAY '=============================='.
           DISPLAY '責任準備金日次再計算バッチ'.
           DISPLAY '処理終了'.
           DISPLAY '処理日付: ' WS-CURRENT-DATE.
           DISPLAY '入力レコード数: ' WS-RECORD-COUNT.
           DISPLAY '更新レコード数: ' WS-UPDATE-COUNT.
           DISPLAY 'スキップ数: ' WS-SKIP-COUNT.
           DISPLAY 'エラー数: ' WS-ERROR-COUNT.
           DISPLAY '=============================='.
