       IDENTIFICATION DIVISION.
       PROGRAM-ID. KZ170B.
      *
      * 変更履歴
      * 版数  年月日      担当                      概要
      * 1.00  平成30年04月 システム部 勘定系チーム  新規作成
      * 1.01  令和02年10月 システム部 勘定系チーム  手数料上限対応
      * 1.02  令和04年06月 システム部 勘定系チーム  利息計算見直し
       AUTHOR. BATCH-SEIGYO.
      *
      * 利用明細書作成バッチ。
      * 口座単位に取引、手数料、利息を集約し明細書を作成する。
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZACCTF ASSIGN TO "KZACCTF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS AC-ACCT-ID
               FILE STATUS IS FS-ACCT.
           SELECT KZTRANF ASSIGN TO "KZTRANF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-TRAN.
           SELECT KZFEEHF ASSIGN TO "KZFEEHF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-FEE.
           SELECT KZINTAF ASSIGN TO "KZINTAF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-INT.
           SELECT KZSTMTF ASSIGN TO "KZSTMTF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-STMT.

       DATA DIVISION.
       FILE SECTION.

       FD  KZACCTF.
           COPY KZACCTC.

       FD  KZTRANF.
           COPY KZTRANC.

       FD  KZFEEHF.
           COPY KZFEEHC.

       FD  KZINTAF.
           COPY KZINTAFC.

       FD  KZSTMTF.
           COPY KZSTMTFC.

       WORKING-STORAGE SECTION.
       01  FS-ACCT                 PIC XX VALUE SPACES.
       01  FS-TRAN                 PIC XX VALUE SPACES.
       01  FS-FEE                  PIC XX VALUE SPACES.
       01  FS-INT                  PIC XX VALUE SPACES.
       01  FS-STMT                 PIC XX VALUE SPACES.

       01  SW-EOF.
           05  ACCT-EOF            PIC X VALUE "N".
               88  END-ACCT        VALUE "Y".
           05  TRAN-EOF            PIC X VALUE "N".
               88  END-TRAN        VALUE "Y".
           05  FEE-EOF             PIC X VALUE "N".
               88  END-FEE         VALUE "Y".
           05  INT-EOF             PIC X VALUE "N".
               88  END-INT         VALUE "Y".

       01  WS-DATE.
           05  WS-DATE-YYYY        PIC 9(04).
           05  WS-DATE-MM          PIC 9(02).
           05  WS-DATE-DD          PIC 9(02).

       01  WS-CURRENT-ACCT         PIC X(16).
       01  WS-GROUP-CODE           PIC X(04).
       01  WS-OPEN-BAL             PIC S9(13)V99 COMP-3.
       01  WS-TRAN-DR              PIC S9(13)V99 COMP-3.
       01  WS-TRAN-CR              PIC S9(13)V99 COMP-3.
       01  WS-FEE-AMT              PIC S9(13)V99 COMP-3.
       01  WS-INT-AMT              PIC S9(13)V99 COMP-3.
       01  WS-CLOSE-BAL            PIC S9(13)V99 COMP-3.
       01  WS-MIN-PAY-RAW          PIC S9(13)V99 COMP-3.
       01  WS-MIN-PAY-BASE         PIC S9(13)V99 COMP-3.
       01  WS-STD-RATE             PIC 9V9(04) VALUE 0.0150.
       01  WS-MIN-RATE             PIC 9V9(04) VALUE 0.0300.
       01  WS-MIN-FLOOR            PIC S9(13)V99 COMP-3 VALUE 1000.
       01  WS-FEE-CAP              PIC S9(13)V99 COMP-3 VALUE 12000.
       01  WS-FEE-ALLOW            PIC S9(13)V99 COMP-3.
       01  WS-ERR-COUNT            PIC 9(07) COMP-3 VALUE ZERO.
       01  WS-WRITE-COUNT          PIC 9(07) COMP-3 VALUE ZERO.
       01  WS-SKIP-COUNT           PIC 9(07) COMP-3 VALUE ZERO.

       01  WS-CLASS-PARM.
           05  CL-ACCT-ID          PIC X(16).
           05  CL-GROUP-CODE       PIC X(04).
           05  CL-KYC-STATUS       PIC X(01).
           05  CL-CLOSE-BAL        PIC S9(13)V99 COMP-3.
           05  CL-CLASS-CODE       PIC X(02).

           COPY LK-RND-PARM.

       PROCEDURE DIVISION.
       MAIN-RTN.
           PERFORM INIT-RTN
           PERFORM UNTIL END-ACCT
               PERFORM PROCESS-ACCT-RTN
               PERFORM READ-ACCT-RTN
           END-PERFORM
           PERFORM CLOSE-RTN
           GOBACK.

       INIT-RTN.
           ACCEPT WS-DATE FROM DATE YYYYMMDD
           OPEN INPUT KZACCTF
                INPUT KZTRANF
                INPUT KZFEEHF
                INPUT KZINTAF
                OUTPUT KZSTMTF
           PERFORM CHECK-OPEN-STATUS
           PERFORM READ-ACCT-RTN
           PERFORM READ-TRAN-RTN
           PERFORM READ-FEE-RTN
           PERFORM READ-INT-RTN.

       CHECK-OPEN-STATUS.
           IF FS-ACCT NOT = "00"
              DISPLAY "KZ170B KZACCTF オープンエラー " FS-ACCT
              STOP RUN
           END-IF
           IF FS-TRAN NOT = "00"
              DISPLAY "KZ170B KZTRANF オープンエラー " FS-TRAN
              STOP RUN
           END-IF
           IF FS-FEE NOT = "00"
              DISPLAY "KZ170B KZFEEHF オープンエラー " FS-FEE
              STOP RUN
           END-IF
           IF FS-INT NOT = "00"
              DISPLAY "KZ170B KZINTAF オープンエラー " FS-INT
              STOP RUN
           END-IF
           IF FS-STMT NOT = "00"
              DISPLAY "KZ170B KZSTMTF オープンエラー " FS-STMT
              STOP RUN
           END-IF.

       READ-ACCT-RTN.
           READ KZACCTF
               AT END
                   SET END-ACCT TO TRUE
               NOT AT END
                   IF FS-ACCT NOT = "00"
                      DISPLAY "KZ170B KZACCTF 読込エラー " FS-ACCT
                      STOP RUN
                   END-IF
           END-READ.

       READ-TRAN-RTN.
           READ KZTRANF
               AT END
                   SET END-TRAN TO TRUE
               NOT AT END
                   IF FS-TRAN NOT = "00"
                      DISPLAY "KZ170B KZTRANF 読込エラー " FS-TRAN
                      STOP RUN
                   END-IF
           END-READ.

       READ-FEE-RTN.
           READ KZFEEHF
               AT END
                   SET END-FEE TO TRUE
               NOT AT END
                   IF FS-FEE NOT = "00"
                      DISPLAY "KZ170B KZFEEHF 読込エラー " FS-FEE
                      STOP RUN
                   END-IF
           END-READ.

       READ-INT-RTN.
           READ KZINTAF
               AT END
                   SET END-INT TO TRUE
               NOT AT END
                   IF FS-INT NOT = "00"
                      DISPLAY "KZ170B KZINTAF 読込エラー " FS-INT
                      STOP RUN
                   END-IF
           END-READ.

       PROCESS-ACCT-RTN.
           MOVE AC-ACCT-ID TO WS-CURRENT-ACCT
           MOVE AC-CYCLE-BAL TO WS-OPEN-BAL
           MOVE ZERO TO WS-TRAN-DR
                        WS-TRAN-CR
                        WS-FEE-AMT
                        WS-INT-AMT
                        WS-CLOSE-BAL
                        WS-MIN-PAY-RAW
                        WS-MIN-PAY-BASE
           PERFORM NORMALIZE-GROUP-RTN
           IF AC-KYC-STATUS = "C"
              PERFORM COLLECT-TRAN-RTN
              PERFORM COLLECT-FEE-RTN
              PERFORM COLLECT-INT-RTN
              PERFORM CALC-STMT-RTN
              PERFORM WRITE-STMT-RTN
           ELSE
              ADD 1 TO WS-SKIP-COUNT
              PERFORM DRAIN-ACCT-DETAIL-RTN
           END-IF.

       NORMALIZE-GROUP-RTN.
           EVALUATE AC-GROUP-CODE
               WHEN "STD0"
               WHEN "GLD1"
               WHEN "PLT2"
               WHEN "EXMP"
               WHEN "PREM"
                   MOVE AC-GROUP-CODE TO WS-GROUP-CODE
               WHEN OTHER
                   MOVE "STD0" TO WS-GROUP-CODE
                   ADD 1 TO WS-ERR-COUNT
           END-EVALUATE.

       COLLECT-TRAN-RTN.
           PERFORM UNTIL END-TRAN
              OR TR-ACCT-ID > WS-CURRENT-ACCT
               IF TR-ACCT-ID < WS-CURRENT-ACCT
                  ADD 1 TO WS-ERR-COUNT
                  PERFORM READ-TRAN-RTN
               ELSE
                  EVALUATE TR-TRAN-CD
                      WHEN "SALE"
                      WHEN "CASH"
                      WHEN "ADJU"
                          ADD TR-TRAN-AMT TO WS-TRAN-DR
                      WHEN "PAYM"
                      WHEN "RFND"
                      WHEN "CRDT"
                          ADD TR-TRAN-AMT TO WS-TRAN-CR
                      WHEN OTHER
                          ADD 1 TO WS-ERR-COUNT
                  END-EVALUATE
                  PERFORM READ-TRAN-RTN
               END-IF
           END-PERFORM.

       COLLECT-FEE-RTN.
           PERFORM UNTIL END-FEE
              OR FE-ACCT-ID > WS-CURRENT-ACCT
               IF FE-ACCT-ID < WS-CURRENT-ACCT
                  ADD 1 TO WS-ERR-COUNT
                  PERFORM READ-FEE-RTN
               ELSE
                  IF FE-EXEMPT-FLAG NOT = "Y"
                     IF WS-GROUP-CODE NOT = "EXMP"
                        IF FE-CAP-FLAG = "Y"
                           COMPUTE WS-FEE-ALLOW =
                               WS-FEE-CAP - FE-FEE-YTD
                           IF WS-FEE-ALLOW > ZERO
                              IF FE-FEE-AMT > WS-FEE-ALLOW
                                 ADD WS-FEE-ALLOW TO WS-FEE-AMT
                              ELSE
                                 ADD FE-FEE-AMT TO WS-FEE-AMT
                              END-IF
                           END-IF
                        ELSE
                           ADD FE-FEE-AMT TO WS-FEE-AMT
                        END-IF
                     END-IF
                  END-IF
                  PERFORM READ-FEE-RTN
               END-IF
           END-PERFORM.

       COLLECT-INT-RTN.
           PERFORM UNTIL END-INT
              OR IA-ACCT-ID > WS-CURRENT-ACCT
               IF IA-ACCT-ID < WS-CURRENT-ACCT
                  ADD 1 TO WS-ERR-COUNT
                  PERFORM READ-INT-RTN
               ELSE
                  IF WS-GROUP-CODE = "EXMP"
                     CONTINUE
                  ELSE
                     IF IA-INT-AMT = ZERO
                        COMPUTE IA-INT-AMT ROUNDED =
                            IA-CYCLE-BAL * WS-STD-RATE / 365
                     END-IF
                     ADD IA-INT-AMT TO WS-INT-AMT
                  END-IF
                  PERFORM READ-INT-RTN
               END-IF
           END-PERFORM.

       CALC-STMT-RTN.
           COMPUTE WS-CLOSE-BAL =
               WS-OPEN-BAL + WS-TRAN-DR - WS-TRAN-CR
               + WS-FEE-AMT + WS-INT-AMT
           IF WS-CLOSE-BAL < ZERO
              MOVE ZERO TO WS-MIN-PAY-RAW
           ELSE
              COMPUTE WS-MIN-PAY-BASE ROUNDED =
                  WS-CLOSE-BAL * WS-MIN-RATE
              IF WS-MIN-PAY-BASE < WS-MIN-FLOOR
                 MOVE WS-MIN-FLOOR TO WS-MIN-PAY-RAW
              ELSE
                 MOVE WS-MIN-PAY-BASE TO WS-MIN-PAY-RAW
              END-IF
              IF AC-OVER-AMT > ZERO
                 ADD AC-OVER-AMT TO WS-MIN-PAY-RAW
              END-IF
              IF WS-MIN-PAY-RAW > WS-CLOSE-BAL
                 MOVE WS-CLOSE-BAL TO WS-MIN-PAY-RAW
              END-IF
           END-IF
           MOVE WS-MIN-PAY-RAW TO LK-AMT-RAW
           CALL "KZ130S" USING LK-RND-PARM
           MOVE CL-CLASS-CODE TO CL-CLASS-CODE
           MOVE WS-CURRENT-ACCT TO CL-ACCT-ID
           MOVE WS-GROUP-CODE TO CL-GROUP-CODE
           MOVE AC-KYC-STATUS TO CL-KYC-STATUS
           MOVE WS-CLOSE-BAL TO CL-CLOSE-BAL
           CALL "KZ171S" USING WS-CLASS-PARM.

       WRITE-STMT-RTN.
           INITIALIZE KZSTMTF-REC
           MOVE WS-CURRENT-ACCT TO ST-ACCT-ID
           MOVE WS-DATE TO ST-STMT-DT
           MOVE WS-OPEN-BAL TO ST-OPEN-BAL
           MOVE WS-CLOSE-BAL TO ST-CLOSE-BAL
           MOVE WS-FEE-AMT TO ST-FEE-AMT
           MOVE WS-INT-AMT TO ST-INT-AMT
           MOVE LK-AMT-FLOORED TO ST-MIN-PAY-AMT
           WRITE KZSTMTF-REC
           IF FS-STMT = "00"
              ADD 1 TO WS-WRITE-COUNT
           ELSE
              DISPLAY "KZ170B KZSTMTF 書込エラー "
                      ST-ACCT-ID " " FS-STMT
              STOP RUN
           END-IF.

       DRAIN-ACCT-DETAIL-RTN.
           PERFORM UNTIL END-TRAN
              OR TR-ACCT-ID > WS-CURRENT-ACCT
               IF TR-ACCT-ID = WS-CURRENT-ACCT
                  PERFORM READ-TRAN-RTN
               ELSE
                  ADD 1 TO WS-ERR-COUNT
                  PERFORM READ-TRAN-RTN
               END-IF
           END-PERFORM
           PERFORM UNTIL END-FEE
              OR FE-ACCT-ID > WS-CURRENT-ACCT
               IF FE-ACCT-ID = WS-CURRENT-ACCT
                  PERFORM READ-FEE-RTN
               ELSE
                  ADD 1 TO WS-ERR-COUNT
                  PERFORM READ-FEE-RTN
               END-IF
           END-PERFORM
           PERFORM UNTIL END-INT
              OR IA-ACCT-ID > WS-CURRENT-ACCT
               IF IA-ACCT-ID = WS-CURRENT-ACCT
                  PERFORM READ-INT-RTN
               ELSE
                  ADD 1 TO WS-ERR-COUNT
                  PERFORM READ-INT-RTN
               END-IF
           END-PERFORM.

       CLOSE-RTN.
           CLOSE KZACCTF
                 KZTRANF
                 KZFEEHF
                 KZINTAF
                 KZSTMTF
           DISPLAY "KZ170B 明細書作成件数 " WS-WRITE-COUNT
           DISPLAY "KZ170B スキップ口座数 " WS-SKIP-COUNT
           DISPLAY "KZ170B 警告件数       " WS-ERR-COUNT.
