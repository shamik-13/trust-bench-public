       IDENTIFICATION DIVISION.
       PROGRAM-ID. CB240B.
      *
      * 変更履歴
      * 版数  年月日    担当    概要
      * 1.00  20240115  ＣＤ運用 初版作成
      * 1.01  20240410  ＣＤ運用 未確定請求の更新抑止を追加
      * 1.02  20240620  ＣＤ運用 サイクル重複検出を追加
      *
      * 残高繰越更新バッチ
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
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
       FD  CDBALF.
           COPY CDBALFC.
       FD  CDBILLF.
           COPY CDBILLFC.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05  WS-CDBALF-ST          PIC X(02) VALUE SPACE.
           05  WS-CDBILLF-ST         PIC X(02) VALUE SPACE.

       01  WS-SWITCHES.
           05  WS-BAL-EOF            PIC X VALUE "N".
               88  BAL-EOF                 VALUE "Y".
           05  WS-BILL-EOF           PIC X VALUE "N".
               88  BILL-EOF                VALUE "Y".
           05  WS-HARD-ERROR         PIC X VALUE "N".
               88  HARD-ERROR              VALUE "Y".
           05  WS-SUPPRESS-SW        PIC X VALUE "N".
               88  UPDATE-SUPPRESS         VALUE "Y".
           05  WS-DUP-SW             PIC X VALUE "N".
               88  DUPLICATE-BILL          VALUE "Y".

       01  WS-SAVE-BILL.
           05  WS-SV-CARD-NO         PIC X(16) VALUE SPACE.
           05  WS-SV-CYCLE-DT        PIC 9(08) VALUE ZERO.
           05  WS-SV-BILL-AMT        PIC S9(13)V99 VALUE ZERO.
           05  WS-SV-MIN-PAY-AMT     PIC S9(13)V99 VALUE ZERO.
           05  WS-SV-DUE-DT          PIC 9(08) VALUE ZERO.
           05  WS-SV-BILL-STATUS     PIC X(02) VALUE SPACE.
           05  WS-SV-PROGRAM-ID      PIC X(08) VALUE SPACE.

       01  WS-WORK.
           05  WS-NEW-REV-BAL        PIC S9(13)V99 VALUE ZERO.
           05  WS-UPDATE-CNT         PIC 9(09) VALUE ZERO.
           05  WS-SKIP-CNT           PIC 9(09) VALUE ZERO.
           05  WS-ERR-CNT            PIC 9(09) VALUE ZERO.
           05  WS-READ-BAL-CNT       PIC 9(09) VALUE ZERO.
           05  WS-READ-BILL-CNT      PIC 9(09) VALUE ZERO.

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE 0 TO RETURN-CODE
           PERFORM 1000-OPEN
           IF NOT HARD-ERROR
              PERFORM 2000-READ-BILL
              PERFORM 2100-READ-BAL
              PERFORM UNTIL BAL-EOF OR HARD-ERROR
                 PERFORM 3000-PROCESS-BAL
                 IF NOT HARD-ERROR
                    PERFORM 2100-READ-BAL
                 END-IF
              END-PERFORM
           END-IF
           PERFORM 9000-CLOSE
           IF HARD-ERROR
              MOVE 8 TO RETURN-CODE
           ELSE
              MOVE 0 TO RETURN-CODE
              DISPLAY "CB240B 正常終了 更新件数=" WS-UPDATE-CNT
              DISPLAY "抑止件数=" WS-SKIP-CNT
              DISPLAY "残高読込件数=" WS-READ-BAL-CNT
              DISPLAY "請求読込件数=" WS-READ-BILL-CNT
           END-IF
           GOBACK.

       1000-OPEN.
           OPEN I-O CDBALF
           IF WS-CDBALF-ST NOT = "00"
              DISPLAY "CDBALF オープン失敗 ST=" WS-CDBALF-ST
              SET HARD-ERROR TO TRUE
           END-IF
           IF NOT HARD-ERROR
              OPEN INPUT CDBILLF
              IF WS-CDBILLF-ST NOT = "00"
                 DISPLAY "CDBILLF オープン失敗 ST=" WS-CDBILLF-ST
                 SET HARD-ERROR TO TRUE
              END-IF
           END-IF.

       2000-READ-BILL.
           READ CDBILLF
              AT END
                 SET BILL-EOF TO TRUE
              NOT AT END
                 ADD 1 TO WS-READ-BILL-CNT
           END-READ
           IF WS-CDBILLF-ST NOT = "00" AND
              WS-CDBILLF-ST NOT = "10"
              DISPLAY "CDBILLF 読込失敗 ST=" WS-CDBILLF-ST
              SET HARD-ERROR TO TRUE
           END-IF.

       2100-READ-BAL.
           READ CDBALF
              AT END
                 SET BAL-EOF TO TRUE
              NOT AT END
                 ADD 1 TO WS-READ-BAL-CNT
           END-READ
           IF WS-CDBALF-ST NOT = "00" AND
              WS-CDBALF-ST NOT = "10"
              DISPLAY "CDBALF 読込失敗 ST=" WS-CDBALF-ST
              SET HARD-ERROR TO TRUE
           END-IF.

       3000-PROCESS-BAL.
           MOVE "N" TO WS-SUPPRESS-SW
           MOVE "N" TO WS-DUP-SW
           PERFORM 3100-SKIP-OLD-BILL
           IF BILL-EOF
              DISPLAY "請求なし CARD=" BL-CARD-NO
              ADD 1 TO WS-SKIP-CNT
           ELSE
              IF BI-CARD-NO = BL-CARD-NO
                 PERFORM 3200-CHECK-BILL
              ELSE
                 DISPLAY "請求未検出 CARD=" BL-CARD-NO
                 ADD 1 TO WS-SKIP-CNT
              END-IF
           END-IF.

       3100-SKIP-OLD-BILL.
           PERFORM UNTIL BILL-EOF OR HARD-ERROR OR
                         BI-CARD-NO >= BL-CARD-NO
              DISPLAY "残高なし請求 CARD=" BI-CARD-NO
              PERFORM 2000-READ-BILL
           END-PERFORM.

       3200-CHECK-BILL.
           IF BI-CYCLE-DT NOT = BL-CYCLE-DT
              DISPLAY "サイクル日不一致 CARD=" BL-CARD-NO
              DISPLAY "残高日=" BL-CYCLE-DT
              DISPLAY "請求日=" BI-CYCLE-DT
              ADD 1 TO WS-SKIP-CNT
           ELSE
              PERFORM 3300-SAVE-BILL
              PERFORM 3400-CONSUME-DUP-BILL
              PERFORM 3500-VALIDATE-AND-UPDATE
           END-IF.

       3300-SAVE-BILL.
           MOVE BI-CARD-NO       TO WS-SV-CARD-NO
           MOVE BI-CYCLE-DT      TO WS-SV-CYCLE-DT
           MOVE BI-BILL-AMT      TO WS-SV-BILL-AMT
           MOVE BI-MIN-PAY-AMT   TO WS-SV-MIN-PAY-AMT
           MOVE BI-DUE-DT        TO WS-SV-DUE-DT
           MOVE BI-BILL-STATUS   TO WS-SV-BILL-STATUS
           MOVE BI-PROGRAM-ID    TO WS-SV-PROGRAM-ID.

       3400-CONSUME-DUP-BILL.
           PERFORM 2000-READ-BILL
           PERFORM UNTIL BILL-EOF OR HARD-ERROR OR
                         BI-CARD-NO NOT = WS-SV-CARD-NO OR
                         BI-CYCLE-DT NOT = WS-SV-CYCLE-DT
              SET DUPLICATE-BILL TO TRUE
              DISPLAY "請求重複 CARD=" BI-CARD-NO
              DISPLAY "請求重複日=" BI-CYCLE-DT
              PERFORM 2000-READ-BILL
           END-PERFORM.

       3500-VALIDATE-AND-UPDATE.
           IF DUPLICATE-BILL
              SET UPDATE-SUPPRESS TO TRUE
              DISPLAY "重複により更新抑止 CARD=" WS-SV-CARD-NO
           END-IF

           IF WS-SV-BILL-STATUS NOT = "C "
              SET UPDATE-SUPPRESS TO TRUE
              DISPLAY "未確定抑止 CARD=" WS-SV-CARD-NO
              DISPLAY "請求状態=" WS-SV-BILL-STATUS
           END-IF

           COMPUTE WS-NEW-REV-BAL =
                   BL-REVOLVING-BAL-AMT
                 + BL-NEW-CHARGE-AMT
                 + BL-CASH-ADV-AMT
                 - WS-SV-MIN-PAY-AMT

           IF WS-NEW-REV-BAL < ZERO
              SET UPDATE-SUPPRESS TO TRUE
              DISPLAY "負値抑止 CARD=" WS-SV-CARD-NO
              DISPLAY "繰越残高=" WS-NEW-REV-BAL
           END-IF

           IF UPDATE-SUPPRESS
              ADD 1 TO WS-SKIP-CNT
           ELSE
              MOVE WS-NEW-REV-BAL TO BL-REVOLVING-BAL-AMT
              MOVE WS-SV-BILL-AMT TO BL-CLOSING-BAL-AMT
              MOVE ZERO           TO BL-NEW-CHARGE-AMT
              MOVE ZERO           TO BL-CASH-ADV-AMT
              REWRITE CDBALF-REC
              IF WS-CDBALF-ST = "00"
                 ADD 1 TO WS-UPDATE-CNT
              ELSE
                 DISPLAY "CDBALF 更新失敗 ST=" WS-CDBALF-ST
                 DISPLAY "更新失敗 CARD=" BL-CARD-NO
                 ADD 1 TO WS-ERR-CNT
                 SET HARD-ERROR TO TRUE
              END-IF
           END-IF.

       9000-CLOSE.
           IF WS-CDBALF-ST NOT = SPACE
              CLOSE CDBALF
              IF WS-CDBALF-ST NOT = "00"
                 DISPLAY "CDBALF クローズ失敗 ST=" WS-CDBALF-ST
                 SET HARD-ERROR TO TRUE
              END-IF
           END-IF
           IF WS-CDBILLF-ST NOT = SPACE
              CLOSE CDBILLF
              IF WS-CDBILLF-ST NOT = "00"
                 DISPLAY "CDBILLF クローズ失敗 ST=" WS-CDBILLF-ST
                 SET HARD-ERROR TO TRUE
              END-IF
           END-IF.
