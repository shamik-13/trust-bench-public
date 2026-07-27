       IDENTIFICATION DIVISION.
       PROGRAM-ID. KZ165B.
      * 変更履歴
      * 版数  年月日       担当                       概要
      * 1.00  平成31.04.01 システム部 勘定系チーム   初版作成
      * 1.01  令和03.10.01 システム部 勘定系チーム   手数料区分見直し
      * 1.02  令和05.04.03 システム部 勘定系チーム   例外処理整理
      * ATM利用手数料集計バッチ
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZTRANF ASSIGN TO "KZTRANF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-KZTRANF.
           SELECT KZACCTF ASSIGN TO "KZACCTF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS AC-ACCT-ID
               FILE STATUS IS FS-KZACCTF.
           SELECT KZDGRPF ASSIGN TO "KZDGRPF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS DG-GROUP-CODE
               FILE STATUS IS FS-KZDGRPF.
           SELECT KZFEEHF ASSIGN TO "KZFEEHF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-KZFEEHF.
           SELECT KZGLDTF ASSIGN TO "KZGLDTF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-KZGLDTF.

       DATA DIVISION.
       FILE SECTION.
       FD  KZTRANF.
       COPY KZTRANC.
       FD  KZACCTF.
       COPY KZACCTC.
       FD  KZDGRPF.
       COPY KZDGRPC.
       FD  KZFEEHF.
       COPY KZFEEHC.
       FD  KZGLDTF.
       COPY KZGLDTFC.

       WORKING-STORAGE SECTION.
       01  FILE-STATUS-AREA.
           05 FS-KZTRANF              PIC XX VALUE SPACES.
           05 FS-KZACCTF              PIC XX VALUE SPACES.
           05 FS-KZDGRPF              PIC XX VALUE SPACES.
           05 FS-KZFEEHF              PIC XX VALUE SPACES.
           05 FS-KZGLDTF              PIC XX VALUE SPACES.

       COPY LK-RATE-PARM.
       COPY LK-RND-PARM.

       01  WS-SWITCHES.
           05 WS-EOF-SW               PIC X VALUE "N".
              88 END-OF-TRAN          VALUE "Y".
           05 WS-ABEND-SW             PIC X VALUE "N".
              88 ABEND-FOUND          VALUE "Y".
           05 WS-ATM-SW               PIC X VALUE "N".
              88 ATM-TRAN             VALUE "Y".
           05 WS-EXEMPT-SW            PIC X VALUE "N".
              88 FEE-EXEMPT           VALUE "Y".

       01  WS-WORK.
           05 WS-GROUP-CODE           PIC X(04) VALUE SPACES.
           05 WS-FEE-AMT              PIC S9(13)V99 COMP-3 VALUE ZERO.
           05 WS-RAW-FEE              PIC S9(13)V99 COMP-3 VALUE ZERO.
           05 WS-FEE-YTD              PIC S9(13)V99 COMP-3 VALUE ZERO.
           05 WS-STD-RATE             PIC 9V9999 COMP-3 VALUE 0.0150.
           05 WS-BATCH-ID             PIC X(12) VALUE SPACES.
           05 WS-CYCLE-DT             PIC 9(08) VALUE ZERO.
           05 WS-ERR-TEXT             PIC X(60) VALUE SPACES.

       01  WS-COUNTERS.
           05 WS-READ-CNT             PIC 9(09) VALUE ZERO.
           05 WS-ATM-CNT              PIC 9(09) VALUE ZERO.
           05 WS-FEE-CNT              PIC 9(09) VALUE ZERO.
           05 WS-SKIP-CNT             PIC 9(09) VALUE ZERO.
           05 WS-ERR-CNT              PIC 9(09) VALUE ZERO.
           05 WS-GL-CNT               PIC 9(09) VALUE ZERO.

       PROCEDURE DIVISION.
       MAIN-RTN.
           PERFORM INIT-RTN
           PERFORM UNTIL END-OF-TRAN OR ABEND-FOUND
               PERFORM READ-TRAN-RTN
               IF NOT END-OF-TRAN
                   ADD 1 TO WS-READ-CNT
                   PERFORM PROCESS-TRAN-RTN
               END-IF
           END-PERFORM
           PERFORM TERM-RTN
           GOBACK.

       INIT-RTN.
           ACCEPT WS-CYCLE-DT FROM DATE YYYYMMDD
           MOVE "KZ165B" TO WS-BATCH-ID(1:6)
           MOVE WS-CYCLE-DT TO WS-BATCH-ID(7:6)
           OPEN INPUT KZTRANF KZACCTF KZDGRPF
           IF FS-KZTRANF NOT = "00"
               MOVE "KZTRANF オープンエラー" TO WS-ERR-TEXT
               PERFORM SET-ABEND-RTN
           END-IF
           IF FS-KZACCTF NOT = "00"
               MOVE "KZACCTF オープンエラー" TO WS-ERR-TEXT
               PERFORM SET-ABEND-RTN
           END-IF
           IF FS-KZDGRPF NOT = "00"
               MOVE "KZDGRPF オープンエラー" TO WS-ERR-TEXT
               PERFORM SET-ABEND-RTN
           END-IF
           IF NOT ABEND-FOUND
               OPEN OUTPUT KZFEEHF KZGLDTF
               IF FS-KZFEEHF NOT = "00"
                   MOVE "KZFEEHF オープンエラー" TO WS-ERR-TEXT
                   PERFORM SET-ABEND-RTN
               END-IF
               IF FS-KZGLDTF NOT = "00"
                   MOVE "KZGLDTF オープンエラー" TO WS-ERR-TEXT
                   PERFORM SET-ABEND-RTN
               END-IF
           END-IF.

       READ-TRAN-RTN.
           READ KZTRANF
               AT END
                   SET END-OF-TRAN TO TRUE
               NOT AT END
                   IF FS-KZTRANF NOT = "00"
                       MOVE "KZTRANF 読込エラー" TO WS-ERR-TEXT
                       PERFORM SET-ABEND-RTN
                   END-IF
           END-READ.

       PROCESS-TRAN-RTN.
           MOVE "N" TO WS-ATM-SW
           EVALUATE TR-TRAN-CD
               WHEN "ATM1"
               WHEN "ATM2"
               WHEN "ATMW"
               WHEN "ATMF"
                   MOVE "Y" TO WS-ATM-SW
               WHEN OTHER
                   ADD 1 TO WS-SKIP-CNT
           END-EVALUATE
           IF ATM-TRAN
               ADD 1 TO WS-ATM-CNT
               IF TR-ACCT-ID = SPACES OR TR-TRAN-AMT <= ZERO
                   ADD 1 TO WS-ERR-CNT
               ELSE
                   PERFORM READ-ACCOUNT-RTN
                   IF FS-KZACCTF = "00"
                       PERFORM DECIDE-GROUP-RTN
                       IF NOT ABEND-FOUND
                           PERFORM CALC-FEE-RTN
                       END-IF
                       IF NOT ABEND-FOUND
                           PERFORM WRITE-FEE-HIST-RTN
                       END-IF
                       IF NOT ABEND-FOUND AND WS-FEE-AMT > ZERO
                           PERFORM WRITE-GL-RTN
                       END-IF
                   ELSE
                       ADD 1 TO WS-ERR-CNT
                   END-IF
               END-IF
           END-IF.

       READ-ACCOUNT-RTN.
           MOVE TR-ACCT-ID TO AC-ACCT-ID
           READ KZACCTF KEY IS AC-ACCT-ID
               INVALID KEY
                   CONTINUE
               NOT INVALID KEY
                   CONTINUE
           END-READ
           IF FS-KZACCTF NOT = "00" AND FS-KZACCTF NOT = "23"
               MOVE "KZACCTF 読込エラー" TO WS-ERR-TEXT
               PERFORM SET-ABEND-RTN
           END-IF.

       DECIDE-GROUP-RTN.
           MOVE AC-GROUP-CODE TO WS-GROUP-CODE
           IF WS-GROUP-CODE = SPACES
               MOVE "STD0" TO WS-GROUP-CODE
           END-IF
           MOVE "N" TO WS-EXEMPT-SW
           MOVE WS-GROUP-CODE TO DG-GROUP-CODE
           READ KZDGRPF KEY IS DG-GROUP-CODE
               INVALID KEY
                   IF WS-GROUP-CODE = "EXMP"
                       MOVE "Y" TO WS-EXEMPT-SW
                   ELSE
                       MOVE "STD0" TO WS-GROUP-CODE
                   END-IF
               NOT INVALID KEY
                   IF DG-EXEMPT-FLAG = "Y"
                       MOVE "Y" TO WS-EXEMPT-SW
                   END-IF
           END-READ
           IF FS-KZDGRPF NOT = "00" AND FS-KZDGRPF NOT = "23"
               MOVE "KZDGRPF 読込エラー" TO WS-ERR-TEXT
               PERFORM SET-ABEND-RTN
           END-IF.

       CALC-FEE-RTN.
           MOVE ZERO TO WS-FEE-AMT WS-RAW-FEE
           IF FEE-EXEMPT
               MOVE ZERO TO WS-FEE-AMT
           ELSE
               INITIALIZE LK-RATE-PARM
               MOVE WS-GROUP-CODE TO LK-GRP-CODE
               MOVE AC-OVER-AMT TO LK-OVER-AMT
               MOVE TR-TRAN-AMT TO LK-FEE-AMT
               CALL "KZ125S" USING LK-RATE-PARM
               IF LK-RET-CD = "00"
                   MOVE LK-FEE-AMT TO LK-AMT-RAW
               ELSE
                   COMPUTE WS-RAW-FEE ROUNDED =
                       TR-TRAN-AMT * WS-STD-RATE
                   MOVE WS-RAW-FEE TO LK-AMT-RAW
               END-IF
               INITIALIZE LK-RND-PARM
               CALL "KZ130S" USING LK-RND-PARM
               MOVE LK-AMT-FLOORED TO WS-FEE-AMT
           END-IF.

       WRITE-FEE-HIST-RTN.
           INITIALIZE KZFEEHF-REC
           MOVE TR-ACCT-ID TO FE-ACCT-ID
           MOVE WS-FEE-AMT TO FE-FEE-AMT
           COMPUTE WS-FEE-YTD = WS-FEE-AMT
           MOVE WS-FEE-YTD TO FE-FEE-YTD
           IF WS-FEE-AMT > 1100
               MOVE "Y" TO FE-CAP-FLAG
           ELSE
               MOVE "N" TO FE-CAP-FLAG
           END-IF
           IF FEE-EXEMPT
               MOVE "Y" TO FE-EXEMPT-FLAG
           ELSE
               MOVE "N" TO FE-EXEMPT-FLAG
           END-IF
           MOVE WS-CYCLE-DT TO FE-CYCLE-DT
           WRITE KZFEEHF-REC
           IF FS-KZFEEHF = "00"
               ADD 1 TO WS-FEE-CNT
           ELSE
               MOVE "KZFEEHF 書込エラー" TO WS-ERR-TEXT
               PERFORM SET-ABEND-RTN
           END-IF.

       WRITE-GL-RTN.
           INITIALIZE KZGLDTF-REC
           MOVE WS-BATCH-ID TO GL-GL-BATCH-ID
           MOVE TR-ACCT-ID TO GL-ACCT-ID
           MOVE "ATMFE" TO GL-ENTRY-CD
           MOVE "C" TO GL-DR-CR-KBN
           MOVE WS-FEE-AMT TO GL-ENTRY-AMT
           MOVE WS-CYCLE-DT TO GL-POST-DT
           WRITE KZGLDTF-REC
           IF FS-KZGLDTF = "00"
               ADD 1 TO WS-GL-CNT
           ELSE
               MOVE "KZGLDTF 書込エラー" TO WS-ERR-TEXT
               PERFORM SET-ABEND-RTN
           END-IF.

       SET-ABEND-RTN.
           ADD 1 TO WS-ERR-CNT
           DISPLAY "KZ165B: " WS-ERR-TEXT
           DISPLAY "ステータス TR/AC/DG/FE/GL="
               FS-KZTRANF "/" FS-KZACCTF "/" FS-KZDGRPF "/"
               FS-KZFEEHF "/" FS-KZGLDTF
           MOVE "Y" TO WS-ABEND-SW.

       TERM-RTN.
           CLOSE KZTRANF KZACCTF KZDGRPF KZFEEHF KZGLDTF
           DISPLAY "KZ165B 読込=" WS-READ-CNT
           DISPLAY "KZ165B ATM =" WS-ATM-CNT
           DISPLAY "KZ165B 手数=" WS-FEE-CNT
           DISPLAY "KZ165B 仕訳=" WS-GL-CNT
           DISPLAY "KZ165B 除外=" WS-SKIP-CNT
           DISPLAY "KZ165B ERR =" WS-ERR-CNT
           IF ABEND-FOUND
               STOP RUN
           END-IF.
