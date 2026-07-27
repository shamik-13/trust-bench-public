       IDENTIFICATION DIVISION.
       PROGRAM-ID. CB110B.
       AUTHOR.     松本 健.
      ******************************************************************
      * 変更履歴
      * 版数  年月日    担当       概要
      * 1.00  20150610  開発一課   請求確定バッチ新規作成
      * 1.01  20180924  開発一課   カード状態別の確定/保留判定を整理
      * 1.02  20240605  保守二課   支払期日算出を CB920S へ委譲
      ******************************************************************
      * 請求サイクル締めの確定バッチ。CDBALF の締後残高を読み、CDCARDF
      * を引き当てて請求明細 CDBILLF を確定する。最低支払額は CB910S を
      * 呼び出して算定する（約定率・最低支払額は債権管理規程に従う）。
      ******************************************************************

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CDCARDF ASSIGN TO "CDCARDF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CF-CARD-NO
               FILE STATUS IS WS-CDCARDF-ST.
           SELECT CDBALF ASSIGN TO "CDBALF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-CDBALF-ST.
           SELECT CDBILLF ASSIGN TO "CDBILLF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-CDBILLF-ST.

       DATA DIVISION.
       FILE SECTION.

       FD  CDCARDF.
           COPY CDCARDFC.

       FD  CDBALF.
           COPY CDBALFC.

       FD  CDBILLF.
           COPY CDBILLFC.

       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID              PIC X(08) VALUE "CB110B".
       01  WS-CDCARDF-ST              PIC X(02) VALUE SPACES.
       01  WS-CDBALF-ST               PIC X(02) VALUE SPACES.
       01  WS-CDBILLF-ST              PIC X(02) VALUE SPACES.

       01  WS-END-FLG                 PIC X(01) VALUE "N".
           88  WS-END                         VALUE "Y".
           88  WS-NOT-END                     VALUE "N".

       01  WS-ABEND-FLG               PIC X(01) VALUE "N".
           88  WS-ABEND                       VALUE "Y".
           88  WS-NORMAL                      VALUE "N".

       01  WS-BILLABLE-FLG            PIC X(01) VALUE "N".
           88  WS-BILLABLE                    VALUE "Y".
           88  WS-NOT-BILLABLE                VALUE "N".

       01  WS-VALID-FLG               PIC X(01) VALUE "Y".
           88  WS-VALID                       VALUE "Y".
           88  WS-INVALID                     VALUE "N".

       01  WS-REC-CNT                 PIC 9(09) VALUE ZERO.
       01  WS-BILL-CNT                PIC 9(09) VALUE ZERO.
       01  WS-SKIP-CNT                PIC 9(09) VALUE ZERO.
       01  WS-HOLD-CNT                PIC 9(09) VALUE ZERO.
       01  WS-ERROR-CNT               PIC 9(09) VALUE ZERO.

       01  WS-DATE-WORK.
           05  WS-CYCLE-DATE          PIC 9(08) VALUE ZERO.
           05  WS-DATE-INT            PIC 9(08) VALUE ZERO.

       01  WS-STATUS-WORK.
           05  WS-BILL-STATUS         PIC X(01) VALUE SPACE.
           05  WS-REASON-CD           PIC X(02) VALUE SPACE.

       01  WS-CONSTANTS.
           05  CT-ST-ACTIVE           PIC X(02) VALUE "01".
           05  CT-ST-STOP             PIC X(02) VALUE "02".
           05  CT-ST-CLOSE            PIC X(02) VALUE "03".
           05  CT-ST-DELAY            PIC X(02) VALUE "09".
           05  CT-BI-COMPLETE         PIC X(01) VALUE "C".
           05  CT-BI-HOLD             PIC X(01) VALUE "H".
           05  CT-BI-SKIP             PIC X(01) VALUE "S".
           05  CT-RET-OK              PIC X(02) VALUE "00".

           COPY LK-MINPAY-PARM.
           COPY LK-BILLED-PARM.

       PROCEDURE DIVISION.
       0000-MAIN SECTION.
       0000-START.
           MOVE 0 TO RETURN-CODE
           PERFORM 1000-OPEN-FILES
           IF WS-NORMAL
               PERFORM 2000-READ-BALANCE
               PERFORM 3000-PROCESS-LOOP
                   UNTIL WS-END OR WS-ABEND
           END-IF
           PERFORM 9000-CLOSE-FILES
           IF WS-ABEND
               MOVE 12 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
               DISPLAY "ＣＢ１１０Ｂ 正常終了"
               DISPLAY "入力件数=" WS-REC-CNT
               DISPLAY "確定件数=" WS-BILL-CNT
               DISPLAY "対象外件数=" WS-SKIP-CNT
               DISPLAY "保留件数=" WS-HOLD-CNT
           END-IF
           GOBACK.

       1000-OPEN-FILES SECTION.
       1000-START.
           OPEN INPUT CDCARDF
           IF WS-CDCARDF-ST NOT = "00"
               DISPLAY "ＣＤＣＡＲＤＦ オープン失敗"
               DISPLAY "ST=" WS-CDCARDF-ST
               PERFORM 9900-ABEND
           END-IF

           IF WS-NORMAL
               OPEN INPUT CDBALF
               IF WS-CDBALF-ST NOT = "00"
                   DISPLAY "ＣＤＢＡＬＦ オープン失敗"
                   DISPLAY "ST=" WS-CDBALF-ST
                   PERFORM 9900-ABEND
               END-IF
           END-IF

           IF WS-NORMAL
               OPEN OUTPUT CDBILLF
               IF WS-CDBILLF-ST NOT = "00"
                   DISPLAY "ＣＤＢＩＬＬＦ オープン失敗"
                   DISPLAY "ST=" WS-CDBILLF-ST
                   PERFORM 9900-ABEND
               END-IF
           END-IF.

       2000-READ-BALANCE SECTION.
       2000-START.
           READ CDBALF
               AT END
                   SET WS-END TO TRUE
               NOT AT END
                   ADD 1 TO WS-REC-CNT
           END-READ
           IF WS-CDBALF-ST NOT = "00" AND WS-CDBALF-ST NOT = "10"
               DISPLAY "ＣＤＢＡＬＦ 読込失敗"
               DISPLAY "ST=" WS-CDBALF-ST
               PERFORM 9900-ABEND
           END-IF.

       3000-PROCESS-LOOP SECTION.
       3000-START.
           PERFORM 3100-VALIDATE-BALANCE
           IF WS-VALID
               PERFORM 3200-READ-CARD
               IF WS-NORMAL
                   PERFORM 3300-DECIDE-BILL
                   PERFORM 3400-WRITE-BILL
               END-IF
           ELSE
               MOVE CT-BI-HOLD TO WS-BILL-STATUS
               MOVE "BV" TO WS-REASON-CD
               PERFORM 3450-WRITE-HOLD
           END-IF
           IF WS-NORMAL
               PERFORM 2000-READ-BALANCE
           END-IF.

       3100-VALIDATE-BALANCE SECTION.
       3100-START.
           SET WS-VALID TO TRUE
           IF BL-CARD-NO = SPACES
               SET WS-INVALID TO TRUE
               DISPLAY "残高カード番号不正"
           END-IF
           IF BL-CYCLE-DT NOT NUMERIC
               SET WS-INVALID TO TRUE
               DISPLAY "締日不正"
               DISPLAY "CARD=" BL-CARD-NO
           END-IF
           IF BL-CLOSING-BAL-AMT NOT NUMERIC
               SET WS-INVALID TO TRUE
               DISPLAY "締後残高不正"
               DISPLAY "CARD=" BL-CARD-NO
           END-IF
           IF WS-VALID
               MOVE BL-CYCLE-DT TO WS-CYCLE-DATE
               COMPUTE WS-DATE-INT =
                   FUNCTION INTEGER-OF-DATE(WS-CYCLE-DATE)
               IF WS-DATE-INT = ZERO
                   SET WS-INVALID TO TRUE
                   DISPLAY "締日日付不正"
                   DISPLAY "CARD=" BL-CARD-NO
               END-IF
           END-IF.

       3200-READ-CARD SECTION.
       3200-START.
           MOVE BL-CARD-NO TO CF-CARD-NO
           READ CDCARDF KEY IS CF-CARD-NO
               INVALID KEY
                   IF WS-CDCARDF-ST = "23"
                       MOVE CT-BI-HOLD TO WS-BILL-STATUS
                       MOVE "NF" TO WS-REASON-CD
                       PERFORM 3450-WRITE-HOLD
                   ELSE
                       DISPLAY "ＣＤＣＡＲＤＦ 読込失敗"
                       DISPLAY "ST=" WS-CDCARDF-ST
                       PERFORM 9900-ABEND
                   END-IF
               NOT INVALID KEY
                   CONTINUE
           END-READ.

       3300-DECIDE-BILL SECTION.
       3300-START.
           SET WS-NOT-BILLABLE TO TRUE
           MOVE SPACES TO WS-REASON-CD
           EVALUATE CF-CARD-STATUS
               WHEN CT-ST-ACTIVE
                   SET WS-BILLABLE TO TRUE
                   MOVE CT-BI-COMPLETE TO WS-BILL-STATUS
               WHEN CT-ST-DELAY
                   SET WS-BILLABLE TO TRUE
                   MOVE CT-BI-COMPLETE TO WS-BILL-STATUS
               WHEN CT-ST-STOP
                   MOVE CT-BI-SKIP TO WS-BILL-STATUS
                   MOVE "02" TO WS-REASON-CD
               WHEN CT-ST-CLOSE
                   MOVE CT-BI-SKIP TO WS-BILL-STATUS
                   MOVE "03" TO WS-REASON-CD
               WHEN OTHER
                   MOVE CT-BI-HOLD TO WS-BILL-STATUS
                   MOVE "CS" TO WS-REASON-CD
                   DISPLAY "カード状態不正"
                   DISPLAY "CARD=" BL-CARD-NO
                   DISPLAY "ST=" CF-CARD-STATUS
           END-EVALUATE.

       3400-WRITE-BILL SECTION.
       3400-START.
           IF WS-BILLABLE
               PERFORM 3500-CALL-MINPAY
               IF WS-NORMAL
                   PERFORM 3700-CALL-BILLED-EDIT
                   IF WS-NORMAL
                       PERFORM 3800-WRITE-COMPLETE
                   END-IF
               END-IF
           ELSE
               PERFORM 3450-WRITE-HOLD
           END-IF.

       3450-WRITE-HOLD SECTION.
       3450-START.
           INITIALIZE CDBILLF-REC
           MOVE BL-CARD-NO             TO BI-CARD-NO
           MOVE BL-CYCLE-DT            TO BI-CYCLE-DT
           MOVE BL-CLOSING-BAL-AMT     TO BI-BILL-AMT
           MOVE ZERO                   TO BI-MIN-PAY-AMT
           MOVE ZERO                   TO BI-DUE-DT
           MOVE WS-BILL-STATUS         TO BI-BILL-STATUS
           MOVE WS-PROGRAM-ID          TO BI-PROGRAM-ID
           WRITE CDBILLF-REC
           IF WS-CDBILLF-ST = "00"
               IF WS-BILL-STATUS = CT-BI-SKIP
                   ADD 1 TO WS-SKIP-CNT
               ELSE
                   ADD 1 TO WS-HOLD-CNT
               END-IF
           ELSE
               DISPLAY "ＣＤＢＩＬＬＦ 書込失敗"
               DISPLAY "ST=" WS-CDBILLF-ST
               DISPLAY "CARD=" BL-CARD-NO
               PERFORM 9900-ABEND
           END-IF.

       3500-CALL-MINPAY SECTION.
       3500-START.
           INITIALIZE LK-MINPAY-PARM
           MOVE BL-CLOSING-BAL-AMT TO LK-MP-CLOSING-AMT
           MOVE CF-CARD-STATUS     TO LK-MP-CARD-STATUS
           CALL "CB910S" USING LK-MINPAY-PARM
           IF LK-MP-RET NOT = CT-RET-OK
               DISPLAY "最低支払額計算失敗"
               DISPLAY "CARD=" BL-CARD-NO
               DISPLAY "RT=" LK-MP-RET
               PERFORM 9900-ABEND
           END-IF.

       3700-CALL-BILLED-EDIT SECTION.
       3700-START.
           INITIALIZE LK-BILLED-PARM
           MOVE BL-CARD-NO         TO LK-BE-CARD-NO
           MOVE BL-CYCLE-DT        TO LK-BE-CYCLE-DT
           MOVE BL-CLOSING-BAL-AMT TO LK-BE-BILL-AMT
           CALL "CB920S" USING LK-BILLED-PARM
           IF LK-BE-RET NOT = CT-RET-OK
               DISPLAY "請求編集失敗"
               DISPLAY "CARD=" BL-CARD-NO
               DISPLAY "RT=" LK-BE-RET
               PERFORM 9900-ABEND
           END-IF.

       3800-WRITE-COMPLETE SECTION.
       3800-START.
           INITIALIZE CDBILLF-REC
           MOVE BL-CARD-NO             TO BI-CARD-NO
           MOVE BL-CYCLE-DT            TO BI-CYCLE-DT
           MOVE BL-CLOSING-BAL-AMT     TO BI-BILL-AMT
           MOVE LK-MP-MIN-PAY          TO BI-MIN-PAY-AMT
           MOVE LK-BE-DUE-DT           TO BI-DUE-DT
           MOVE CT-BI-COMPLETE         TO BI-BILL-STATUS
           MOVE WS-PROGRAM-ID          TO BI-PROGRAM-ID
           WRITE CDBILLF-REC
           IF WS-CDBILLF-ST = "00"
               ADD 1 TO WS-BILL-CNT
           ELSE
               DISPLAY "ＣＤＢＩＬＬＦ 書込失敗"
               DISPLAY "ST=" WS-CDBILLF-ST
               DISPLAY "CARD=" BL-CARD-NO
               PERFORM 9900-ABEND
           END-IF.

       9000-CLOSE-FILES SECTION.
       9000-START.
           CLOSE CDCARDF
           IF WS-CDCARDF-ST NOT = "00"
               DISPLAY "ＣＤＣＡＲＤＦ クローズ失敗"
               DISPLAY "ST=" WS-CDCARDF-ST
               PERFORM 9900-ABEND
           END-IF

           CLOSE CDBALF
           IF WS-CDBALF-ST NOT = "00"
               DISPLAY "ＣＤＢＡＬＦ クローズ失敗"
               DISPLAY "ST=" WS-CDBALF-ST
               PERFORM 9900-ABEND
           END-IF

           CLOSE CDBILLF
           IF WS-CDBILLF-ST NOT = "00"
               DISPLAY "ＣＤＢＩＬＬＦ クローズ失敗"
               DISPLAY "ST=" WS-CDBILLF-ST
               PERFORM 9900-ABEND
           END-IF.

       9900-ABEND SECTION.
       9900-START.
           SET WS-ABEND TO TRUE
           ADD 1 TO WS-ERROR-CNT
           MOVE 12 TO RETURN-CODE.
