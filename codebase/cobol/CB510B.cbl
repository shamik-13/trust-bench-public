       IDENTIFICATION DIVISION.
       PROGRAM-ID. CB510B.
       AUTHOR. TRUST-BATCH.
      ******************************************************************
      * 売上確定バッチ
      * CDSALEFを順次読込み、カードマスタを参照して請求確定を行う。
      * 海外事務手数料はCB590Sで算定する。
      ******************************************************************

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CDSALEF
               ASSIGN TO "CDSALEF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS FS-CDSALEF.

           SELECT CDCARDF
               ASSIGN TO "CDCARDF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS CF-CARD-NO
               FILE STATUS IS FS-CDCARDF.

           SELECT CDCAPF
               ASSIGN TO "CDCAPF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS FS-CDCAPF.

       DATA DIVISION.
       FILE SECTION.

       FD  CDSALEF.
       COPY CDSALEFC.

       FD  CDCARDF.
       COPY CDCARD03.

       FD  CDCAPF.
       COPY CDCAPFC.

       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID              PIC X(08) VALUE "CB510B".
       01  WS-BASE-CURRENCY           PIC X(03) VALUE "JPY".
       01  WS-BILLABLE-STATUS         PIC X(02) VALUE "01".
       01  WS-CAPTURED-STATUS         PIC X(01) VALUE "C".
       01  WS-SKIP-STATUS             PIC X(01) VALUE "S".
       01  WS-HOLD-STATUS             PIC X(01) VALUE "H".
       01  WS-NORMAL-RET              PIC X(02) VALUE "00".
       01  WS-EOF-FLAG                PIC X(01) VALUE "N".
           88  WS-EOF                           VALUE "Y".
           88  WS-NOT-EOF                       VALUE "N".
       01  WS-HARD-ERROR-FLAG         PIC X(01) VALUE "N".
           88  WS-HARD-ERROR                   VALUE "Y".
           88  WS-NO-HARD-ERROR                VALUE "N".

       01  FS-CDSALEF                 PIC X(02) VALUE SPACES.
       01  FS-CDCARDF                 PIC X(02) VALUE SPACES.
       01  FS-CDCAPF                  PIC X(02) VALUE SPACES.

       01  WS-COUNTERS.
           05  WS-READ-CNT            PIC 9(09) VALUE ZERO.
           05  WS-CAPTURE-CNT         PIC 9(09) VALUE ZERO.
           05  WS-SKIP-CNT            PIC 9(09) VALUE ZERO.
           05  WS-HOLD-CNT            PIC 9(09) VALUE ZERO.
           05  WS-ERROR-CNT           PIC 9(09) VALUE ZERO.

       01  WS-EDIT.
           05  WS-FEE-AMT             PIC S9(13)V99 VALUE ZERO.
           05  WS-BILLED-AMT          PIC S9(13)V99 VALUE ZERO.
           05  WS-REASON-CD           PIC X(04) VALUE SPACES.
           05  WS-DISP-COUNT          PIC Z(09).

       COPY LK-FXFEE-PARM.

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE 0 TO RETURN-CODE
           PERFORM 1000-OPEN-FILES
           IF WS-NO-HARD-ERROR
               PERFORM 2000-PROCESS-ALL
           END-IF
           PERFORM 8000-CLOSE-FILES
           IF WS-HARD-ERROR
               MOVE 12 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF
           PERFORM 9000-DISPLAY-SUMMARY
           GOBACK
           .

       1000-OPEN-FILES.
           OPEN INPUT CDSALEF
           IF FS-CDSALEF NOT = "00"
               DISPLAY "CDSALEF オープン失敗 ST=" FS-CDSALEF
               PERFORM 7900-SET-HARD-ERROR
           END-IF

           IF WS-NO-HARD-ERROR
               OPEN INPUT CDCARDF
               IF FS-CDCARDF NOT = "00"
                   DISPLAY "CDCARDF オープン失敗 ST=" FS-CDCARDF
                   PERFORM 7900-SET-HARD-ERROR
               END-IF
           END-IF

           IF WS-NO-HARD-ERROR
               OPEN OUTPUT CDCAPF
               IF FS-CDCAPF NOT = "00"
                   DISPLAY "CDCAPF オープン失敗 ST=" FS-CDCAPF
                   PERFORM 7900-SET-HARD-ERROR
               END-IF
           END-IF
           .

       2000-PROCESS-ALL.
           SET WS-NOT-EOF TO TRUE
           PERFORM UNTIL WS-EOF OR WS-HARD-ERROR
               PERFORM 2100-READ-SALE
               IF WS-NOT-EOF AND WS-NO-HARD-ERROR
                   ADD 1 TO WS-READ-CNT
                   PERFORM 3000-PROCESS-SALE
               END-IF
           END-PERFORM
           .

       2100-READ-SALE.
           READ CDSALEF
               AT END
                   SET WS-EOF TO TRUE
               NOT AT END
                   IF FS-CDSALEF NOT = "00"
                       DISPLAY "CDSALEF 読込失敗 ST=" FS-CDSALEF
                       PERFORM 7900-SET-HARD-ERROR
                   END-IF
           END-READ
           .

       3000-PROCESS-SALE.
           MOVE SPACES TO WS-REASON-CD
           PERFORM 3100-VALIDATE-SALE
           IF WS-NO-HARD-ERROR
               PERFORM 3200-READ-CARD
           END-IF
           IF WS-NO-HARD-ERROR
               EVALUATE TRUE
                   WHEN FS-CDCARDF = "00"
                       PERFORM 3300-EVALUATE-CARD
                   WHEN FS-CDCARDF = "23"
                       MOVE "C404" TO WS-REASON-CD
                       PERFORM 3600-WRITE-HOLD
                   WHEN OTHER
                       DISPLAY "CDCARDF 参照失敗 ST=" FS-CDCARDF
                       DISPLAY "CARD=" SL-CARD-NO
                       PERFORM 7900-SET-HARD-ERROR
               END-EVALUATE
           END-IF
           .

       3100-VALIDATE-SALE.
           IF SL-SALE-ID = SPACES
               DISPLAY "売上ＩＤ不正"
               PERFORM 7900-SET-HARD-ERROR
           END-IF
           IF WS-NO-HARD-ERROR AND SL-CARD-NO = SPACES
               DISPLAY "カード番号不正 SALE=" SL-SALE-ID
               PERFORM 7900-SET-HARD-ERROR
           END-IF
           IF WS-NO-HARD-ERROR AND SL-SALE-AMT <= ZERO
               DISPLAY "売上金額不正 SALE=" SL-SALE-ID
               PERFORM 7900-SET-HARD-ERROR
           END-IF
           IF WS-NO-HARD-ERROR AND SL-CURRENCY-CD = SPACES
               DISPLAY "通貨コード不正 SALE=" SL-SALE-ID
               PERFORM 7900-SET-HARD-ERROR
           END-IF
           IF WS-NO-HARD-ERROR AND SL-MERCHANT-CODE = SPACES
               DISPLAY "加盟店コード不正 SALE=" SL-SALE-ID
               PERFORM 7900-SET-HARD-ERROR
           END-IF
           IF WS-NO-HARD-ERROR AND SL-SALE-DT = ZERO
               DISPLAY "売上日不正 SALE=" SL-SALE-ID
               PERFORM 7900-SET-HARD-ERROR
           END-IF
           IF WS-NO-HARD-ERROR AND SL-AUTH-ID = SPACES
               DISPLAY "承認番号不正 SALE=" SL-SALE-ID
               PERFORM 7900-SET-HARD-ERROR
           END-IF
           .

       3200-READ-CARD.
           MOVE SL-CARD-NO TO CF-CARD-NO
           READ CDCARDF
               KEY IS CF-CARD-NO
           END-READ
           .

       3300-EVALUATE-CARD.
           PERFORM 3400-VALIDATE-CARD
           IF WS-NO-HARD-ERROR
               IF CF-CARD-STATUS = WS-BILLABLE-STATUS
                   PERFORM 3500-WRITE-CAPTURE
               ELSE
                   MOVE "CSTS" TO WS-REASON-CD
                   PERFORM 3700-WRITE-SKIP
               END-IF
           END-IF
           .

       3400-VALIDATE-CARD.
           IF CF-CARD-NO = SPACES
               DISPLAY "カード番号未設定 SALE=" SL-SALE-ID
               PERFORM 7900-SET-HARD-ERROR
           END-IF
           IF WS-NO-HARD-ERROR
               IF CF-CARD-STATUS NOT = "01" AND
                  CF-CARD-STATUS NOT = "02" AND
                  CF-CARD-STATUS NOT = "03" AND
                  CF-CARD-STATUS NOT = "09"
                   DISPLAY "カード状態不正 CARD=" CF-CARD-NO
                   DISPLAY "STS=" CF-CARD-STATUS
                   PERFORM 7900-SET-HARD-ERROR
               END-IF
           END-IF
           IF WS-NO-HARD-ERROR AND CF-CREDIT-LIMIT < ZERO
               DISPLAY "与信枠不正 CARD=" CF-CARD-NO
               PERFORM 7900-SET-HARD-ERROR
           END-IF
           IF WS-NO-HARD-ERROR AND CF-MEMBER-ID = SPACES
               DISPLAY "会員番号未設定 CARD=" CF-CARD-NO
               PERFORM 7900-SET-HARD-ERROR
           END-IF
           .

       3500-WRITE-CAPTURE.
           PERFORM 4100-CALCULATE-FEE
           IF WS-NO-HARD-ERROR
               COMPUTE WS-BILLED-AMT = SL-SALE-AMT + WS-FEE-AMT
               INITIALIZE CDCAPF-REC
               MOVE SL-SALE-ID          TO BC-SALE-ID
               MOVE SL-CARD-NO          TO BC-CARD-NO
               MOVE WS-BILLED-AMT       TO BC-BILLED-AMT
               MOVE WS-FEE-AMT          TO BC-FEE-AMT
               MOVE SL-CURRENCY-CD      TO BC-CURRENCY-CD
               MOVE WS-CAPTURED-STATUS  TO BC-CAP-STATUS
               MOVE WS-PROGRAM-ID       TO BC-PROGRAM-ID
               PERFORM 5000-WRITE-CAP
               IF WS-NO-HARD-ERROR
                   ADD 1 TO WS-CAPTURE-CNT
               END-IF
           END-IF
           .

       3600-WRITE-HOLD.
           INITIALIZE CDCAPF-REC
           MOVE SL-SALE-ID              TO BC-SALE-ID
           MOVE SL-CARD-NO              TO BC-CARD-NO
           MOVE ZERO                    TO BC-BILLED-AMT
           MOVE ZERO                    TO BC-FEE-AMT
           MOVE SL-CURRENCY-CD          TO BC-CURRENCY-CD
           MOVE WS-HOLD-STATUS          TO BC-CAP-STATUS
           MOVE WS-PROGRAM-ID           TO BC-PROGRAM-ID
           PERFORM 5000-WRITE-CAP
           IF WS-NO-HARD-ERROR
               ADD 1 TO WS-HOLD-CNT
               DISPLAY "保留出力 SALE=" SL-SALE-ID
               DISPLAY "理由=" WS-REASON-CD
           END-IF
           .

       3700-WRITE-SKIP.
           INITIALIZE CDCAPF-REC
           MOVE SL-SALE-ID              TO BC-SALE-ID
           MOVE SL-CARD-NO              TO BC-CARD-NO
           MOVE ZERO                    TO BC-BILLED-AMT
           MOVE ZERO                    TO BC-FEE-AMT
           MOVE SL-CURRENCY-CD          TO BC-CURRENCY-CD
           MOVE WS-SKIP-STATUS          TO BC-CAP-STATUS
           MOVE WS-PROGRAM-ID           TO BC-PROGRAM-ID
           PERFORM 5000-WRITE-CAP
           IF WS-NO-HARD-ERROR
               ADD 1 TO WS-SKIP-CNT
               DISPLAY "対象外出力 SALE=" SL-SALE-ID
               DISPLAY "状態=" CF-CARD-STATUS
           END-IF
           .

       4100-CALCULATE-FEE.
           MOVE ZERO TO WS-FEE-AMT
           IF SL-CURRENCY-CD = WS-BASE-CURRENCY
               MOVE ZERO TO WS-FEE-AMT
           ELSE
               INITIALIZE LK-FXFEE-PARM
               MOVE SL-SALE-AMT     TO LK-FX-SALE-AMT
               MOVE SL-CURRENCY-CD  TO LK-FX-CURRENCY
               CALL "CB590S" USING LK-FXFEE-PARM
               IF LK-FX-RET = WS-NORMAL-RET
                   MOVE LK-FX-FEE-AMT TO WS-FEE-AMT
               ELSE
                   DISPLAY "海外手数料算定失敗 SALE="
                           SL-SALE-ID
                   DISPLAY "RET=" LK-FX-RET
                   PERFORM 7900-SET-HARD-ERROR
               END-IF
           END-IF
           IF WS-NO-HARD-ERROR AND WS-FEE-AMT < ZERO
               DISPLAY "海外手数料金額不正 SALE=" SL-SALE-ID
               PERFORM 7900-SET-HARD-ERROR
           END-IF
           .

       5000-WRITE-CAP.
           WRITE CDCAPF-REC
           IF FS-CDCAPF NOT = "00"
               DISPLAY "CDCAPF 書込失敗 ST=" FS-CDCAPF
               DISPLAY "SALE=" SL-SALE-ID
               PERFORM 7900-SET-HARD-ERROR
           END-IF
           .

       7900-SET-HARD-ERROR.
           SET WS-HARD-ERROR TO TRUE
           ADD 1 TO WS-ERROR-CNT
           MOVE 12 TO RETURN-CODE
           .

       8000-CLOSE-FILES.
           IF FS-CDSALEF = "00" OR FS-CDSALEF = "10"
               CLOSE CDSALEF
               IF FS-CDSALEF NOT = "00"
                   DISPLAY "CDSALEF クローズ失敗 ST=" FS-CDSALEF
                   PERFORM 7900-SET-HARD-ERROR
               END-IF
           END-IF

           IF FS-CDCARDF = "00" OR FS-CDCARDF = "23"
               CLOSE CDCARDF
               IF FS-CDCARDF NOT = "00"
                   DISPLAY "CDCARDF クローズ失敗 ST=" FS-CDCARDF
                   PERFORM 7900-SET-HARD-ERROR
               END-IF
           END-IF

           IF FS-CDCAPF = "00"
               CLOSE CDCAPF
               IF FS-CDCAPF NOT = "00"
                   DISPLAY "CDCAPF クローズ失敗 ST=" FS-CDCAPF
                   PERFORM 7900-SET-HARD-ERROR
               END-IF
           END-IF
           .

       9000-DISPLAY-SUMMARY.
           MOVE WS-READ-CNT TO WS-DISP-COUNT
           DISPLAY "売上読込件数=" WS-DISP-COUNT
           MOVE WS-CAPTURE-CNT TO WS-DISP-COUNT
           DISPLAY "確定件数=" WS-DISP-COUNT
           MOVE WS-SKIP-CNT TO WS-DISP-COUNT
           DISPLAY "対象外件数=" WS-DISP-COUNT
           MOVE WS-HOLD-CNT TO WS-DISP-COUNT
           DISPLAY "保留件数=" WS-DISP-COUNT
           MOVE WS-ERROR-CNT TO WS-DISP-COUNT
           DISPLAY "異常件数=" WS-DISP-COUNT
           DISPLAY "終了コード=" RETURN-CODE
           .
