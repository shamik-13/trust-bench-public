       IDENTIFICATION DIVISION.
       PROGRAM-ID. KZ106B.
      * 変更履歴
      * 版数  年月日(和暦)  担当                    概要
      * 1.00  令和03.04.01 システム部 勘定系チーム 年会費請求作成 新規作成
      * 1.01  令和04.10.01 システム部 勘定系チーム 請求対象抽出条件の見直し
      * 1.02  令和06.04.01 システム部 勘定系チーム 税率変更時の金額編集対応
      *---------------------------------------------------------------*
      * 年会費請求作成バッチ
      * 有効カード口座を対象に優遇・料率を参照して年会費を作成する。
      * 免除時は請求トランを出さず、履歴には免除結果を残す。
      *---------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZACCTF ASSIGN TO "KZACCTF"
              ORGANIZATION IS INDEXED
              ACCESS MODE IS SEQUENTIAL
              RECORD KEY IS AC-ACCT-ID
              FILE STATUS IS WS-ACCT-STAT.

           SELECT KZCARDF ASSIGN TO "KZCARDF"
              ORGANIZATION IS INDEXED
              ACCESS MODE IS DYNAMIC
              RECORD KEY IS CD-CARD-NO
              FILE STATUS IS WS-CARD-STAT.

           SELECT KZDGRPF ASSIGN TO "KZDGRPF"
              ORGANIZATION IS INDEXED
              ACCESS MODE IS DYNAMIC
              RECORD KEY IS DG-GROUP-CODE
              FILE STATUS IS WS-DGRP-STAT.

           SELECT KZTRANF ASSIGN TO "KZTRANF"
              ORGANIZATION IS SEQUENTIAL
              FILE STATUS IS WS-TRAN-STAT.

           SELECT KZFEEHF ASSIGN TO "KZFEEHF"
              ORGANIZATION IS SEQUENTIAL
              FILE STATUS IS WS-FEEH-STAT.

       DATA DIVISION.
       FILE SECTION.

       FD  KZACCTF.
           COPY KZACCTC.

       FD  KZCARDF.
           COPY KZCARDC.

       FD  KZDGRPF.
           COPY KZDGRPC.

       FD  KZTRANF.
           COPY KZTRANC.

       FD  KZFEEHF.
           COPY KZFEEHC.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05 WS-ACCT-STAT        PIC XX VALUE SPACES.
           05 WS-CARD-STAT        PIC XX VALUE SPACES.
           05 WS-DGRP-STAT        PIC XX VALUE SPACES.
           05 WS-TRAN-STAT        PIC XX VALUE SPACES.
           05 WS-FEEH-STAT        PIC XX VALUE SPACES.

       01  WS-RUN-FLAGS.
           05 WS-EOF              PIC X VALUE "N".
              88 ACCT-END               VALUE "Y".
              88 ACCT-MORE              VALUE "N".
           05 WS-SKIP-ACCT        PIC X VALUE "N".
              88 SKIP-ACCOUNT           VALUE "Y".
              88 USE-ACCOUNT            VALUE "N".
           05 WS-EXEMPT-SW        PIC X VALUE "N".
              88 FEE-EXEMPT            VALUE "Y".
              88 FEE-BILLABLE          VALUE "N".
           05 WS-CAP-SW           PIC X VALUE "N".
              88 FEE-CAPPED            VALUE "Y".
              88 FEE-NOT-CAPPED        VALUE "N".

       01  WS-CONSTANTS.
           05 WS-STD-GROUP        PIC X(04) VALUE "STD0".
           05 WS-DEP-GROUP        PIC X(04) VALUE "PREM".
           05 WS-ACTIVE-CARD      PIC X     VALUE "1".
           05 WS-OK               PIC XX    VALUE "00".
           05 WS-NOTF             PIC XX    VALUE "23".
           05 WS-CYCLE-DT         PIC 9(08) VALUE 20260617.
           05 WS-TRAN-CD-FEE      PIC X(04) VALUE "AFEE".
           05 WS-RATE-STD         PIC 9V9(4) VALUE 0.0150.

       01  WS-CALC.
           05 WS-GROUP-CODE       PIC X(04) VALUE SPACES.
           05 WS-FEE-RATE         PIC 9V9(4) VALUE 0.
           05 WS-FEE-RAW          PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-FEE-AMT          PIC S9(11)    COMP-3 VALUE 0.
           05 WS-FEE-YTD          PIC S9(11)    COMP-3 VALUE 0.
           05 WS-MAX-FEE          PIC S9(11)    COMP-3 VALUE 110000.
           05 WS-BASE-BAL         PIC S9(11)    COMP-3 VALUE 0.

       01  WS-COUNTERS.
           05 WS-READ-CNT         PIC 9(09) VALUE 0.
           05 WS-BILL-CNT         PIC 9(09) VALUE 0.
           05 WS-EXEMPT-CNT       PIC 9(09) VALUE 0.
           05 WS-SKIP-CNT         PIC 9(09) VALUE 0.
           05 WS-ERR-CNT          PIC 9(09) VALUE 0.

           COPY LK-GRP-PARM.
           COPY LK-RATE-PARM.
           COPY LK-RND-PARM.

       PROCEDURE DIVISION.
       MAIN-SECTION.
           PERFORM 0000-OPEN-FILES
           PERFORM 1000-PROCESS-ACCOUNTS
              UNTIL ACCT-END
           PERFORM 9000-CLOSE-FILES
           GOBACK
           .

       0000-OPEN-FILES.
           OPEN INPUT  KZACCTF
                       KZCARDF
                       KZDGRPF
                OUTPUT KZTRANF
                       KZFEEHF

           IF WS-ACCT-STAT NOT = WS-OK
              DISPLAY "KZ160B OPEN ERROR KZACCTF " WS-ACCT-STAT
              STOP RUN
           END-IF
           IF WS-CARD-STAT NOT = WS-OK
              DISPLAY "KZ160B OPEN ERROR KZCARDF " WS-CARD-STAT
              STOP RUN
           END-IF
           IF WS-DGRP-STAT NOT = WS-OK
              DISPLAY "KZ160B OPEN ERROR KZDGRPF " WS-DGRP-STAT
              STOP RUN
           END-IF
           IF WS-TRAN-STAT NOT = WS-OK
              DISPLAY "KZ160B OPEN ERROR KZTRANF " WS-TRAN-STAT
              STOP RUN
           END-IF
           IF WS-FEEH-STAT NOT = WS-OK
              DISPLAY "KZ160B OPEN ERROR KZFEEHF " WS-FEEH-STAT
              STOP RUN
           END-IF
           .

       1000-PROCESS-ACCOUNTS.
           PERFORM 1100-READ-ACCOUNT
           IF ACCT-MORE
              ADD 1 TO WS-READ-CNT
              PERFORM 1200-INITIALIZE-ACCOUNT
              PERFORM 1300-VALIDATE-ACCOUNT
              IF USE-ACCOUNT
                 PERFORM 1400-READ-CARD
              END-IF
              IF USE-ACCOUNT
                 PERFORM 1500-READ-GROUP
              END-IF
              IF USE-ACCOUNT
                 PERFORM 2000-CALCULATE-FEE
                 PERFORM 3000-WRITE-HISTORY
                 IF FEE-BILLABLE
                    PERFORM 3100-WRITE-TRAN
                 END-IF
              ELSE
                 ADD 1 TO WS-SKIP-CNT
              END-IF
           END-IF
           .

       1100-READ-ACCOUNT.
           READ KZACCTF NEXT RECORD
              AT END
                 SET ACCT-END TO TRUE
              NOT AT END
                 SET ACCT-MORE TO TRUE
           END-READ

           IF WS-ACCT-STAT NOT = WS-OK
              AND WS-ACCT-STAT NOT = "10"
              DISPLAY "KZ160B READ ERROR KZACCTF " WS-ACCT-STAT
              ADD 1 TO WS-ERR-CNT
              SET ACCT-END TO TRUE
           END-IF
           .

       1200-INITIALIZE-ACCOUNT.
           SET USE-ACCOUNT TO TRUE
           SET FEE-BILLABLE TO TRUE
           SET FEE-NOT-CAPPED TO TRUE
           MOVE ZERO TO WS-FEE-RATE
                        WS-FEE-RAW
                        WS-FEE-AMT
                        WS-FEE-YTD
                        WS-BASE-BAL
           MOVE AC-GROUP-CODE TO WS-GROUP-CODE
           .

       1300-VALIDATE-ACCOUNT.
      *    KYC未完了、カード番号なし、異常残高は請求対象外とする。
           IF AC-KYC-STATUS NOT = "1"
              SET SKIP-ACCOUNT TO TRUE
           END-IF

           IF USE-ACCOUNT
              AND AC-CARD-NO = SPACES
              SET SKIP-ACCOUNT TO TRUE
           END-IF

           IF USE-ACCOUNT
              AND AC-CYCLE-BAL < ZERO
              SET SKIP-ACCOUNT TO TRUE
           END-IF

           IF USE-ACCOUNT
              AND AC-GROUP-CODE = SPACES
              MOVE WS-STD-GROUP TO WS-GROUP-CODE
           END-IF

           IF USE-ACCOUNT
              AND AC-CREDIT-LIMIT <= ZERO
              SET SKIP-ACCOUNT TO TRUE
           END-IF
           .

       1400-READ-CARD.
           MOVE AC-CARD-NO TO CD-CARD-NO
           READ KZCARDF KEY IS CD-CARD-NO
              INVALID KEY
                 SET SKIP-ACCOUNT TO TRUE
              NOT INVALID KEY
                 CONTINUE
           END-READ

           IF WS-CARD-STAT NOT = WS-OK
              AND WS-CARD-STAT NOT = WS-NOTF
              DISPLAY "KZ160B READ ERROR KZCARDF " WS-CARD-STAT
                      " CARD " AC-CARD-NO
              ADD 1 TO WS-ERR-CNT
              SET SKIP-ACCOUNT TO TRUE
           END-IF

           IF USE-ACCOUNT
              AND CD-ACCT-ID NOT = AC-ACCT-ID
              SET SKIP-ACCOUNT TO TRUE
           END-IF

           IF USE-ACCOUNT
              AND CD-CARD-STATUS NOT = WS-ACTIVE-CARD
              SET SKIP-ACCOUNT TO TRUE
           END-IF
           .

       1500-READ-GROUP.
           MOVE WS-GROUP-CODE TO DG-GROUP-CODE

           READ KZDGRPF KEY IS DG-GROUP-CODE
              INVALID KEY
                 MOVE WS-STD-GROUP TO DG-GROUP-CODE
                 READ KZDGRPF KEY IS DG-GROUP-CODE
                    INVALID KEY
                       SET SKIP-ACCOUNT TO TRUE
                 END-READ
           END-READ

           IF WS-DGRP-STAT NOT = WS-OK
              AND WS-DGRP-STAT NOT = WS-NOTF
              DISPLAY "KZ160B READ ERROR KZDGRPF " WS-DGRP-STAT
                      " GROUP " WS-GROUP-CODE
              ADD 1 TO WS-ERR-CNT
              SET SKIP-ACCOUNT TO TRUE
           END-IF

           IF USE-ACCOUNT
              AND DG-EXEMPT-FLAG = "Y"
              SET FEE-EXEMPT TO TRUE
           END-IF

           IF USE-ACCOUNT
              MOVE DG-OL-FEE-RATE TO WS-FEE-RATE
              IF WS-FEE-RATE = ZERO
                 AND DG-GROUP-CODE NOT = "EXMP"
                 MOVE WS-RATE-STD TO WS-FEE-RATE
              END-IF
           END-IF
           .

       2000-CALCULATE-FEE.
           IF AC-OVER-AMT > ZERO
              MOVE AC-OVER-AMT TO WS-BASE-BAL
           ELSE
              MOVE AC-CYCLE-BAL TO WS-BASE-BAL
           END-IF

           INITIALIZE LK-GRP-PARM
           MOVE WS-GROUP-CODE TO LK-GRP-CODE OF LK-GRP-PARM
           MOVE AC-OVER-AMT   TO LK-OVER-AMT OF LK-GRP-PARM
           CALL "KZ120S" USING LK-GRP-PARM

           IF LK-RET-CD OF LK-GRP-PARM = WS-OK
              IF LK-EXEMPT OF LK-GRP-PARM = "Y"
                 SET FEE-EXEMPT TO TRUE
              END-IF
           END-IF

           IF FEE-BILLABLE
              INITIALIZE LK-RATE-PARM
              MOVE WS-GROUP-CODE TO LK-GRP-CODE OF LK-RATE-PARM
              MOVE AC-OVER-AMT   TO LK-OVER-AMT OF LK-RATE-PARM
              CALL "KZ125S" USING LK-RATE-PARM
              IF LK-RET-CD OF LK-RATE-PARM = WS-OK
                 AND LK-FEE-AMT OF LK-RATE-PARM > ZERO
                 MOVE LK-FEE-AMT OF LK-RATE-PARM TO WS-FEE-AMT
              ELSE
                 COMPUTE WS-FEE-RAW ROUNDED =
                    WS-BASE-BAL * WS-FEE-RATE
                 PERFORM 2100-ROUND-FEE
              END-IF
           END-IF

           IF FEE-EXEMPT
              MOVE ZERO TO WS-FEE-AMT
              ADD 1 TO WS-EXEMPT-CNT
           ELSE
              IF WS-FEE-AMT > WS-MAX-FEE
                 MOVE WS-MAX-FEE TO WS-FEE-AMT
                 SET FEE-CAPPED TO TRUE
              END-IF
              ADD 1 TO WS-BILL-CNT
           END-IF

           MOVE WS-FEE-AMT TO WS-FEE-YTD
           .

       2100-ROUND-FEE.
           INITIALIZE LK-RND-PARM
           MOVE WS-FEE-RAW TO LK-AMT-RAW
           CALL "KZ130S" USING LK-RND-PARM

           IF LK-AMT-FLOORED >= ZERO
              MOVE LK-AMT-FLOORED TO WS-FEE-AMT
           ELSE
              MOVE ZERO TO WS-FEE-AMT
           END-IF
           .

       3000-WRITE-HISTORY.
           INITIALIZE KZFEEHF-REC
           MOVE AC-ACCT-ID  TO FE-ACCT-ID
           MOVE WS-FEE-AMT  TO FE-FEE-AMT
           MOVE WS-FEE-YTD  TO FE-FEE-YTD
           MOVE WS-CYCLE-DT TO FE-CYCLE-DT

           IF FEE-CAPPED
              MOVE "Y" TO FE-CAP-FLAG
           ELSE
              MOVE "N" TO FE-CAP-FLAG
           END-IF

           IF FEE-EXEMPT
              MOVE "Y" TO FE-EXEMPT-FLAG
           ELSE
              MOVE "N" TO FE-EXEMPT-FLAG
           END-IF

           WRITE KZFEEHF-REC

           IF WS-FEEH-STAT NOT = WS-OK
              DISPLAY "KZ160B WRITE ERROR KZFEEHF " WS-FEEH-STAT
                      " ACCT " AC-ACCT-ID
              ADD 1 TO WS-ERR-CNT
           END-IF
           .

       3100-WRITE-TRAN.
           INITIALIZE KZTRANF-REC
           MOVE AC-ACCT-ID     TO TR-ACCT-ID
           MOVE WS-TRAN-CD-FEE TO TR-TRAN-CD
           MOVE WS-FEE-AMT     TO TR-TRAN-AMT

           WRITE KZTRANF-REC

           IF WS-TRAN-STAT NOT = WS-OK
              DISPLAY "KZ160B WRITE ERROR KZTRANF " WS-TRAN-STAT
                      " ACCT " AC-ACCT-ID
              ADD 1 TO WS-ERR-CNT
           END-IF
           .

       9000-CLOSE-FILES.
           CLOSE KZACCTF
                 KZCARDF
                 KZDGRPF
                 KZTRANF
                 KZFEEHF

           DISPLAY "KZ160B READ   " WS-READ-CNT
           DISPLAY "KZ160B BILL   " WS-BILL-CNT
           DISPLAY "KZ160B EXEMPT " WS-EXEMPT-CNT
           DISPLAY "KZ160B SKIP   " WS-SKIP-CNT
           DISPLAY "KZ160B ERROR  " WS-ERR-CNT
           .
