       IDENTIFICATION DIVISION.
       PROGRAM-ID. CB230B.
      *
      * 変更履歴
      * 版数  年月日    担当    概要
      * 1.00  20240510  開発一  初版作成
      * 1.01  20240722  保守二  入金過不足判定を追加
      * 1.02  20241015  保守二  ファイル状態表示を整理
      *
      * 入金消込バッチ
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CDPAYF ASSIGN TO "CDPAYF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-CDPAYF.
           SELECT CDBILLF ASSIGN TO "CDBILLF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-CDBILLF.
           SELECT CDBALF ASSIGN TO "CDBALF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-CDBALF.

       DATA DIVISION.
       FILE SECTION.
       FD  CDPAYF.
           COPY CDPAYC.
       FD  CDBILLF.
           COPY CDBILLFC.
       FD  CDBALF.
           COPY CDBALFC.

       WORKING-STORAGE SECTION.
       01  FS-CDPAYF                 PIC XX VALUE SPACE.
       01  FS-CDBILLF                PIC XX VALUE SPACE.
       01  FS-CDBALF                 PIC XX VALUE SPACE.

       01  SW-EOF.
           05  SW-PAY-EOF            PIC X VALUE "N".
               88  PAY-EOF                 VALUE "Y".
           05  SW-BILL-EOF           PIC X VALUE "N".
               88  BILL-EOF                VALUE "Y".
           05  SW-BAL-EOF            PIC X VALUE "N".
               88  BAL-EOF                 VALUE "Y".

       01  WK-FLAGS.
           05  WK-HARD-ERR           PIC X VALUE "N".
               88  HARD-ERR                VALUE "Y".
           05  WK-BILL-FOUND         PIC X VALUE "N".
               88  BILL-FOUND              VALUE "Y".
           05  WK-BAL-FOUND          PIC X VALUE "N".
               88  BAL-FOUND               VALUE "Y".
           05  WK-PAY-CHANGED        PIC X VALUE "N".
               88  PAY-CHANGED             VALUE "Y".

       01  WK-AMOUNTS.
           05  WK-REST-AMT           PIC S9(13)V99 COMP-3 VALUE 0.
           05  WK-ALLOC-AMT          PIC S9(13)V99 COMP-3 VALUE 0.
           05  WK-BILL-ALLOC-AMT     PIC S9(13)V99 COMP-3 VALUE 0.
           05  WK-REV-ALLOC-AMT      PIC S9(13)V99 COMP-3 VALUE 0.
           05  WK-CASH-ALLOC-AMT     PIC S9(13)V99 COMP-3 VALUE 0.

       01  WK-COUNTERS.
           05  CT-PAY-READ           PIC 9(9) VALUE 0.
           05  CT-PAY-UPD            PIC 9(9) VALUE 0.
           05  CT-BILL-UPD           PIC 9(9) VALUE 0.
           05  CT-BAL-UPD            PIC 9(9) VALUE 0.
           05  CT-SKIP               PIC 9(9) VALUE 0.
           05  CT-OVER               PIC 9(9) VALUE 0.
           05  CT-ERR                PIC 9(9) VALUE 0.

       01  WK-DISPLAY.
           05  DSP-AMT               PIC -Z(12)9.99.
           05  DSP-COUNT             PIC Z(9).

       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-OPEN-FILES
           IF HARD-ERR
               MOVE 12 TO RETURN-CODE
               GOBACK
           END-IF

           PERFORM 2000-PROCESS-PAYMENTS
               UNTIL PAY-EOF OR HARD-ERR

           PERFORM 9000-CLOSE-FILES

           IF HARD-ERR
               MOVE 12 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
               PERFORM 9100-DISPLAY-SUMMARY
           END-IF

           GOBACK.

       1000-OPEN-FILES.
           OPEN I-O CDPAYF
           IF FS-CDPAYF NOT = "00"
               DISPLAY "CDPAYF オープン失敗 ST=" FS-CDPAYF
               SET HARD-ERR TO TRUE
           END-IF

           OPEN I-O CDBILLF
           IF FS-CDBILLF NOT = "00"
               DISPLAY "CDBILLF オープン失敗 ST=" FS-CDBILLF
               SET HARD-ERR TO TRUE
           END-IF

           OPEN I-O CDBALF
           IF FS-CDBALF NOT = "00"
               DISPLAY "CDBALF オープン失敗 ST=" FS-CDBALF
               SET HARD-ERR TO TRUE
           END-IF

           IF NOT HARD-ERR
               PERFORM 2100-READ-PAY
           END-IF.

       2000-PROCESS-PAYMENTS.
           ADD 1 TO CT-PAY-READ
           MOVE "N" TO WK-PAY-CHANGED
           MOVE 0 TO WK-BILL-ALLOC-AMT
                     WK-REV-ALLOC-AMT
                     WK-CASH-ALLOC-AMT

           EVALUATE TRUE
               WHEN PY-CARD-NO = SPACE
                   DISPLAY "カード番号未設定 入金ID="
                           PY-PAYMENT-ID
                   MOVE "9" TO PY-ALLOC-STATUS
                   ADD 1 TO CT-ERR
                   SET PAY-CHANGED TO TRUE
               WHEN PY-PAY-AMT <= 0
                   DISPLAY "入金額不正 入金ID="
                           PY-PAYMENT-ID
                   MOVE "9" TO PY-ALLOC-STATUS
                   ADD 1 TO CT-ERR
                   SET PAY-CHANGED TO TRUE
               WHEN PY-ALLOC-STATUS NOT = SPACE
                AND PY-ALLOC-STATUS NOT = "0"
                   ADD 1 TO CT-SKIP
               WHEN OTHER
                   MOVE PY-PAY-AMT TO WK-REST-AMT
                   PERFORM 3000-ALLOCATE-BILL
                   IF NOT HARD-ERR
                       PERFORM 4000-ALLOCATE-BALANCE
                   END-IF
                   IF NOT HARD-ERR
                       PERFORM 5000-SET-PAYMENT-STATUS
                   END-IF
           END-EVALUATE

           IF PAY-CHANGED AND NOT HARD-ERR
               REWRITE CDPAYF-REC
               IF FS-CDPAYF = "00"
                   ADD 1 TO CT-PAY-UPD
               ELSE
                   DISPLAY "CDPAYF 更新失敗 ST=" FS-CDPAYF
                   DISPLAY "入金ID=" PY-PAYMENT-ID
                   SET HARD-ERR TO TRUE
               END-IF
           END-IF

           IF NOT HARD-ERR
               PERFORM 2100-READ-PAY
           END-IF.

       2100-READ-PAY.
           READ CDPAYF
               AT END
                   SET PAY-EOF TO TRUE
               NOT AT END
                   CONTINUE
           END-READ
           IF FS-CDPAYF NOT = "00" AND FS-CDPAYF NOT = "10"
               DISPLAY "CDPAYF 読込失敗 ST=" FS-CDPAYF
               SET HARD-ERR TO TRUE
           END-IF.

       3000-ALLOCATE-BILL.
           MOVE "N" TO WK-BILL-FOUND
           MOVE "N" TO SW-BILL-EOF

           CLOSE CDBILLF
           OPEN I-O CDBILLF
           IF FS-CDBILLF NOT = "00"
               DISPLAY "CDBILLF 再オープン失敗 ST=" FS-CDBILLF
               SET HARD-ERR TO TRUE
           END-IF

           PERFORM UNTIL BILL-EOF OR BILL-FOUND OR HARD-ERR
               READ CDBILLF
                   AT END
                       SET BILL-EOF TO TRUE
                   NOT AT END
                       PERFORM 3100-CHECK-BILL
               END-READ
               IF FS-CDBILLF NOT = "00" AND FS-CDBILLF NOT = "10"
                   DISPLAY "CDBILLF 読込失敗 ST=" FS-CDBILLF
                   SET HARD-ERR TO TRUE
               END-IF
           END-PERFORM.

       3100-CHECK-BILL.
           IF BI-CARD-NO = PY-CARD-NO
              AND BI-DUE-DT >= PY-RECEIVED-DT
              AND BI-BILL-STATUS = "C"
               SET BILL-FOUND TO TRUE
               IF BI-BILL-AMT > 0
                   IF WK-REST-AMT >= BI-BILL-AMT
                       MOVE BI-BILL-AMT TO WK-ALLOC-AMT
                   ELSE
                       MOVE WK-REST-AMT TO WK-ALLOC-AMT
                   END-IF
                   SUBTRACT WK-ALLOC-AMT FROM BI-BILL-AMT
                   SUBTRACT WK-ALLOC-AMT FROM WK-REST-AMT
                   ADD WK-ALLOC-AMT TO WK-BILL-ALLOC-AMT
                   IF BI-BILL-AMT = 0
                       MOVE "S" TO BI-BILL-STATUS
                       MOVE "CB230B" TO BI-PROGRAM-ID
                   END-IF
                   REWRITE CDBILLF-REC
                   IF FS-CDBILLF = "00"
                       ADD 1 TO CT-BILL-UPD
                   ELSE
                       DISPLAY "CDBILLF 更新失敗 ST=" FS-CDBILLF
                       DISPLAY "カード=" PY-CARD-NO
                       SET HARD-ERR TO TRUE
                   END-IF
               END-IF
           END-IF.

       4000-ALLOCATE-BALANCE.
           IF WK-REST-AMT <= 0
               EXIT PARAGRAPH
           END-IF

           MOVE "N" TO WK-BAL-FOUND
           MOVE "N" TO SW-BAL-EOF

           CLOSE CDBALF
           OPEN I-O CDBALF
           IF FS-CDBALF NOT = "00"
               DISPLAY "CDBALF 再オープン失敗 ST=" FS-CDBALF
               SET HARD-ERR TO TRUE
           END-IF

           PERFORM UNTIL BAL-EOF OR BAL-FOUND OR HARD-ERR
               READ CDBALF
                   AT END
                       SET BAL-EOF TO TRUE
                   NOT AT END
                       PERFORM 4100-CHECK-BALANCE
               END-READ
               IF FS-CDBALF NOT = "00" AND FS-CDBALF NOT = "10"
                   DISPLAY "CDBALF 読込失敗 ST=" FS-CDBALF
                   SET HARD-ERR TO TRUE
               END-IF
           END-PERFORM

           IF NOT BAL-FOUND AND NOT HARD-ERR
               DISPLAY "残高未検出 カード=" PY-CARD-NO
           END-IF.

       4100-CHECK-BALANCE.
           IF BL-CARD-NO = PY-CARD-NO
               SET BAL-FOUND TO TRUE
               PERFORM 4200-ALLOC-REVOLVING
               PERFORM 4300-ALLOC-CASH
               IF WK-REV-ALLOC-AMT > 0 OR WK-CASH-ALLOC-AMT > 0
                   COMPUTE BL-CLOSING-BAL-AMT =
                       BL-REVOLVING-BAL-AMT
                     + BL-NEW-CHARGE-AMT
                     + BL-CASH-ADV-AMT
                   REWRITE CDBALF-REC
                   IF FS-CDBALF = "00"
                       ADD 1 TO CT-BAL-UPD
                   ELSE
                       DISPLAY "CDBALF 更新失敗 ST=" FS-CDBALF
                       DISPLAY "カード=" PY-CARD-NO
                       SET HARD-ERR TO TRUE
                   END-IF
               END-IF
           END-IF.

       4200-ALLOC-REVOLVING.
           IF WK-REST-AMT > 0 AND BL-REVOLVING-BAL-AMT > 0
               IF WK-REST-AMT >= BL-REVOLVING-BAL-AMT
                   MOVE BL-REVOLVING-BAL-AMT TO WK-ALLOC-AMT
               ELSE
                   MOVE WK-REST-AMT TO WK-ALLOC-AMT
               END-IF
               SUBTRACT WK-ALLOC-AMT FROM BL-REVOLVING-BAL-AMT
               SUBTRACT WK-ALLOC-AMT FROM WK-REST-AMT
               ADD WK-ALLOC-AMT TO WK-REV-ALLOC-AMT
           END-IF.

       4300-ALLOC-CASH.
           IF WK-REST-AMT > 0 AND BL-CASH-ADV-AMT > 0
               IF WK-REST-AMT >= BL-CASH-ADV-AMT
                   MOVE BL-CASH-ADV-AMT TO WK-ALLOC-AMT
               ELSE
                   MOVE WK-REST-AMT TO WK-ALLOC-AMT
               END-IF
               SUBTRACT WK-ALLOC-AMT FROM BL-CASH-ADV-AMT
               SUBTRACT WK-ALLOC-AMT FROM WK-REST-AMT
               ADD WK-ALLOC-AMT TO WK-CASH-ALLOC-AMT
           END-IF.

       5000-SET-PAYMENT-STATUS.
           IF WK-BILL-ALLOC-AMT = 0
              AND WK-REV-ALLOC-AMT = 0
              AND WK-CASH-ALLOC-AMT = 0
               MOVE "0" TO PY-ALLOC-STATUS
               ADD 1 TO CT-SKIP
           ELSE
               IF WK-REST-AMT = 0
                   MOVE "2" TO PY-ALLOC-STATUS
               ELSE
                   MOVE "1" TO PY-ALLOC-STATUS
                   ADD 1 TO CT-OVER
                   MOVE WK-REST-AMT TO DSP-AMT
                   DISPLAY "過入金未配賦 入金ID="
                           PY-PAYMENT-ID
                   DISPLAY "残額=" DSP-AMT
               END-IF
           END-IF
           SET PAY-CHANGED TO TRUE.

       9000-CLOSE-FILES.
           CLOSE CDPAYF
           IF FS-CDPAYF NOT = "00"
              AND FS-CDPAYF NOT = "42"
               DISPLAY "CDPAYF クローズ失敗 ST=" FS-CDPAYF
               SET HARD-ERR TO TRUE
           END-IF

           CLOSE CDBILLF
           IF FS-CDBILLF NOT = "00"
              AND FS-CDBILLF NOT = "42"
               DISPLAY "CDBILLF クローズ失敗 ST=" FS-CDBILLF
               SET HARD-ERR TO TRUE
           END-IF

           CLOSE CDBALF
           IF FS-CDBALF NOT = "00"
              AND FS-CDBALF NOT = "42"
               DISPLAY "CDBALF クローズ失敗 ST=" FS-CDBALF
               SET HARD-ERR TO TRUE
           END-IF.

       9100-DISPLAY-SUMMARY.
           MOVE CT-PAY-READ TO DSP-COUNT
           DISPLAY "CB230B 正常終了 入金読込件数=" DSP-COUNT
           MOVE CT-PAY-UPD TO DSP-COUNT
           DISPLAY "入金更新件数=" DSP-COUNT
           MOVE CT-BILL-UPD TO DSP-COUNT
           DISPLAY "請求更新件数=" DSP-COUNT
           MOVE CT-BAL-UPD TO DSP-COUNT
           DISPLAY "残高更新件数=" DSP-COUNT
           MOVE CT-SKIP TO DSP-COUNT
           DISPLAY "対象外件数=" DSP-COUNT
           MOVE CT-OVER TO DSP-COUNT
           DISPLAY "過入金件数=" DSP-COUNT
           MOVE CT-ERR TO DSP-COUNT
           DISPLAY "業務エラー件数=" DSP-COUNT.
