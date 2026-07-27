       IDENTIFICATION DIVISION.
       PROGRAM-ID. KZ470B.
      * 変更履歴
      * 版数  年月日(和暦)  担当                      概要
      * 1.00  令和04.03.31  システム部 勘定系チーム  新規作成
      * 1.01  令和05.09.29  システム部 勘定系チーム  期末利息締切抽出条件見直し
      * 1.02  令和06.03.29  システム部 勘定系チーム  締切結果ログ出力追加
      *---------------------------------------------------------------*
      * KZCYRF/KZCTLF/KZDBALF CLOSING CHECK                          *
      *---------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZCYRF
               ASSIGN TO "KZCYRF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-KZCYRF.

           SELECT KZDBALF
               ASSIGN TO "KZDBALF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-KZDBALF.

           SELECT KZCTLF
               ASSIGN TO "KZCTLF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CL-CONTROL-KEY
               FILE STATUS IS FS-KZCTLF.

           SELECT KZRCNF
               ASSIGN TO "KZRCNF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-KZRCNF.

       DATA DIVISION.
       FILE SECTION.

       FD  KZCYRF.
       COPY KZCYRFC.

       FD  KZDBALF.
       COPY KZDBALFC.

       FD  KZCTLF.
       COPY KZCTLFC.

       FD  KZRCNF.
       COPY KZRCNFC.

       WORKING-STORAGE SECTION.
       01  FILE-STATUS-AREA.
           05 FS-KZCYRF                  PIC XX VALUE SPACES.
           05 FS-KZDBALF                 PIC XX VALUE SPACES.
           05 FS-KZCTLF                  PIC XX VALUE SPACES.
           05 FS-KZRCNF                  PIC XX VALUE SPACES.

       01  SWITCH-AREA.
           05 SW-CYCLE-EOF               PIC X VALUE 'N'.
              88 CYCLE-EOF                    VALUE 'Y'.
           05 SW-BAL-EOF                 PIC X VALUE 'N'.
              88 BAL-EOF                      VALUE 'Y'.
           05 SW-HARD-ERROR              PIC X VALUE 'N'.
              88 HARD-ERROR                   VALUE 'Y'.
           05 SW-CYCLE-FOUND             PIC X VALUE 'N'.
              88 CYCLE-FOUND                  VALUE 'Y'.
           05 SW-TARGET-FOUND            PIC X VALUE 'N'.
              88 TARGET-FOUND                 VALUE 'Y'.
           05 SW-PREV-VALID              PIC X VALUE 'N'.
              88 PREV-VALID                   VALUE 'Y'.

       01  CONTROL-KEYS.
           05 WK-CTL-KEY                 PIC X(16) VALUE 'KZ470B'.
           05 WK-RC-KEY-PREFIX           PIC X(06) VALUE SPACES.
           05 WK-RC-SEQ                  PIC 9(06) VALUE ZERO.

       01  WORK-AREA.
           05 WK-RUN-DT                  PIC 9(08) VALUE ZERO.
           05 WK-CURRENT-DATE            PIC 9(08) VALUE ZERO.
           05 WK-TARGET-CYCLE            PIC X(10) VALUE SPACES.
           05 WK-TARGET-COUNT            PIC 9(11) VALUE ZERO.
           05 WK-DUP-COUNT               PIC 9(07) VALUE ZERO.
           05 WK-REV-COUNT               PIC 9(07) VALUE ZERO.
           05 WK-BAL-SUM                 PIC S9(15)V99 VALUE ZERO.
           05 WK-PREV-ACCT-NO            PIC X(20) VALUE SPACES.
           05 WK-PREV-CYCLE-ID           PIC X(10) VALUE SPACES.
           05 WK-PREV-START-DT           PIC 9(08) VALUE ZERO.
           05 WK-DISPOSITION             PIC X(02) VALUE SPACES.

       01  DISPLAY-AREA.
           05 DSP-COUNT                  PIC Z(10)9.
           05 DSP-AMOUNT                 PIC -ZZZ,ZZZ,ZZZ,ZZ9.99.

       PROCEDURE DIVISION.
       MAIN-SECTION.
           PERFORM 1000-INIT
           IF NOT HARD-ERROR
               PERFORM 2000-READ-CONTROL
           END-IF
           IF NOT HARD-ERROR
               PERFORM 3000-CHECK-CYCLE
           END-IF
           IF NOT HARD-ERROR
               PERFORM 4000-CHECK-BALANCE
           END-IF
           IF NOT HARD-ERROR
               PERFORM 5000-WRITE-RECON-BEFORE
           END-IF
           IF NOT HARD-ERROR
               PERFORM 6000-UPDATE-CONTROL
           END-IF
           IF NOT HARD-ERROR
               PERFORM 7000-WRITE-RECON-AFTER
           END-IF
           PERFORM 9000-FINISH
           GOBACK.

       1000-INIT.
           MOVE 0 TO RETURN-CODE
           ACCEPT WK-CURRENT-DATE FROM DATE YYYYMMDD
           MOVE WK-CURRENT-DATE TO WK-RUN-DT

           OPEN INPUT KZCYRF
           IF FS-KZCYRF NOT = '00'
               DISPLAY 'KZCYRF OPEN ERR ST=' FS-KZCYRF
               MOVE 'Y' TO SW-HARD-ERROR
               MOVE 12 TO RETURN-CODE
           END-IF

           OPEN INPUT KZDBALF
           IF FS-KZDBALF NOT = '00'
               DISPLAY 'KZDBALF OPEN ERR ST=' FS-KZDBALF
               MOVE 'Y' TO SW-HARD-ERROR
               MOVE 12 TO RETURN-CODE
           END-IF

           OPEN I-O KZCTLF
           IF FS-KZCTLF NOT = '00'
               DISPLAY 'KZCTLF OPEN ERR ST=' FS-KZCTLF
               MOVE 'Y' TO SW-HARD-ERROR
               MOVE 12 TO RETURN-CODE
           END-IF

           OPEN OUTPUT KZRCNF
           IF FS-KZRCNF NOT = '00'
               DISPLAY 'KZRCNF OPEN ERR ST=' FS-KZRCNF
               MOVE 'Y' TO SW-HARD-ERROR
               MOVE 12 TO RETURN-CODE
           END-IF.

       2000-READ-CONTROL.
           MOVE WK-CTL-KEY TO CL-CONTROL-KEY
           READ KZCTLF KEY IS CL-CONTROL-KEY
               INVALID KEY
                   DISPLAY 'KZCTLF KEY ERR'
                   DISPLAY WK-CTL-KEY
                   MOVE 'Y' TO SW-HARD-ERROR
                   MOVE 8 TO RETURN-CODE
               NOT INVALID KEY
                   IF FS-KZCTLF NOT = '00'
                       DISPLAY 'KZCTLF READ ERR ST=' FS-KZCTLF
                       MOVE 'Y' TO SW-HARD-ERROR
                       MOVE 12 TO RETURN-CODE
                   ELSE
                       PERFORM 2100-VALIDATE-CONTROL
                   END-IF
           END-READ.

       2100-VALIDATE-CONTROL.
           IF CL-CYCLE-ID = SPACES
               DISPLAY 'CONTROL CYCLE IS SPACE'
               MOVE 'Y' TO SW-HARD-ERROR
               MOVE 8 TO RETURN-CODE
           ELSE
               MOVE CL-CYCLE-ID TO WK-TARGET-CYCLE
           END-IF

           IF NOT HARD-ERROR
               IF CL-RUN-STATUS NOT = 'OPEN'
               AND CL-RUN-STATUS NOT = 'WAIT'
                   DISPLAY 'CONTROL STATUS ERR'
                   DISPLAY CL-RUN-STATUS
                   MOVE 'Y' TO SW-HARD-ERROR
                   MOVE 8 TO RETURN-CODE
               END-IF
           END-IF

           IF NOT HARD-ERROR
               IF CL-LAST-PGM-ID NOT = SPACES
               AND CL-LAST-PGM-ID NOT = 'KZ400B'
                   DISPLAY 'CONTROL LAST PGM ERR'
                   DISPLAY CL-LAST-PGM-ID
                   MOVE 'Y' TO SW-HARD-ERROR
                   MOVE 8 TO RETURN-CODE
               END-IF
           END-IF.

       3000-CHECK-CYCLE.
           PERFORM UNTIL CYCLE-EOF OR CYCLE-FOUND OR HARD-ERROR
               READ KZCYRF
                   AT END
                       MOVE 'Y' TO SW-CYCLE-EOF
                   NOT AT END
                       IF FS-KZCYRF NOT = '00'
                           DISPLAY 'KZCYRF READ ERR ST=' FS-KZCYRF
                           MOVE 'Y' TO SW-HARD-ERROR
                           MOVE 12 TO RETURN-CODE
                       ELSE
                           IF CR-CYCLE-ID = WK-TARGET-CYCLE
                               MOVE 'Y' TO SW-CYCLE-FOUND
                               PERFORM 3100-VALIDATE-CYCLE
                           END-IF
                       END-IF
               END-READ
           END-PERFORM

           IF NOT HARD-ERROR AND NOT CYCLE-FOUND
               DISPLAY 'TARGET CYCLE NOT FOUND'
               DISPLAY WK-TARGET-CYCLE
               MOVE 'Y' TO SW-HARD-ERROR
               MOVE 8 TO RETURN-CODE
           END-IF.

       3100-VALIDATE-CYCLE.
           IF CR-NOMINAL-DT = ZERO
               DISPLAY 'CYCLE NOMINAL DATE ERR'
               DISPLAY CR-CYCLE-ID
               MOVE 'Y' TO SW-HARD-ERROR
               MOVE 8 TO RETURN-CODE
           END-IF

           IF NOT HARD-ERROR
               IF CR-RESOLVED-DT = ZERO
                   DISPLAY 'CYCLE RESOLVED DATE ERR'
                   DISPLAY CR-CYCLE-ID
                   MOVE 'Y' TO SW-HARD-ERROR
                   MOVE 8 TO RETURN-CODE
               END-IF
           END-IF

           IF NOT HARD-ERROR
               IF CR-ROLLED-FLAG NOT = 'Y'
               AND CR-ROLLED-FLAG NOT = 'N'
                   DISPLAY 'CYCLE ROLLED FLAG ERR'
                   DISPLAY CR-CYCLE-ID
                   MOVE 'Y' TO SW-HARD-ERROR
                   MOVE 8 TO RETURN-CODE
               END-IF
           END-IF.

       4000-CHECK-BALANCE.
           PERFORM UNTIL BAL-EOF OR HARD-ERROR
               READ KZDBALF
                   AT END
                       MOVE 'Y' TO SW-BAL-EOF
                   NOT AT END
                       IF FS-KZDBALF NOT = '00'
                           DISPLAY 'KZDBALF READ ERR ST=' FS-KZDBALF
                           MOVE 'Y' TO SW-HARD-ERROR
                           MOVE 12 TO RETURN-CODE
                       ELSE
                           PERFORM 4100-INSPECT-BALANCE
                       END-IF
               END-READ
           END-PERFORM

           IF NOT HARD-ERROR
               PERFORM 4200-JUDGE-BALANCE
           END-IF.

       4100-INSPECT-BALANCE.
           IF DB-ACCT-NO = SPACES
               DISPLAY 'BALANCE ACCT ERR'
               MOVE 'Y' TO SW-HARD-ERROR
               MOVE 8 TO RETURN-CODE
           END-IF

           IF NOT HARD-ERROR
               IF DB-CYCLE-ID = SPACES
                   DISPLAY 'BALANCE CYCLE SPACE'
                   DISPLAY DB-ACCT-NO
                   MOVE 'Y' TO SW-HARD-ERROR
                   MOVE 8 TO RETURN-CODE
               END-IF
           END-IF

           IF NOT HARD-ERROR
               IF DB-PERIOD-START-DT = ZERO
                   DISPLAY 'BALANCE START DATE ERR'
                   DISPLAY DB-ACCT-NO
                   MOVE 'Y' TO SW-HARD-ERROR
                   MOVE 8 TO RETURN-CODE
               END-IF
           END-IF

           IF NOT HARD-ERROR
               IF PREV-VALID
               AND DB-ACCT-NO = WK-PREV-ACCT-NO
               AND DB-CYCLE-ID = WK-PREV-CYCLE-ID
                   ADD 1 TO WK-DUP-COUNT
                   DISPLAY 'BALANCE DUPLICATE'
                   DISPLAY DB-ACCT-NO
               END-IF
           END-IF

           IF NOT HARD-ERROR
               IF PREV-VALID
               AND DB-ACCT-NO = WK-PREV-ACCT-NO
               AND DB-PERIOD-START-DT < WK-PREV-START-DT
                   ADD 1 TO WK-REV-COUNT
                   DISPLAY 'BALANCE DATE REVERSE'
                   DISPLAY DB-ACCT-NO
               END-IF
           END-IF

           IF NOT HARD-ERROR
               IF DB-CYCLE-ID = WK-TARGET-CYCLE
                   ADD 1 TO WK-TARGET-COUNT
                   ADD DB-AVG-DAILY-BAL TO WK-BAL-SUM
                   MOVE 'Y' TO SW-TARGET-FOUND
               END-IF

               MOVE DB-ACCT-NO TO WK-PREV-ACCT-NO
               MOVE DB-CYCLE-ID TO WK-PREV-CYCLE-ID
               MOVE DB-PERIOD-START-DT TO WK-PREV-START-DT
               MOVE 'Y' TO SW-PREV-VALID
           END-IF.

       4200-JUDGE-BALANCE.
           IF NOT TARGET-FOUND
               DISPLAY 'TARGET BALANCE NOT FOUND'
               DISPLAY WK-TARGET-CYCLE
               MOVE 'Y' TO SW-HARD-ERROR
               MOVE 8 TO RETURN-CODE
           END-IF

           IF NOT HARD-ERROR
               IF WK-DUP-COUNT > ZERO
                   MOVE WK-DUP-COUNT TO DSP-COUNT
                   DISPLAY 'BALANCE DUP COUNT=' DSP-COUNT
                   MOVE 'Y' TO SW-HARD-ERROR
                   MOVE 8 TO RETURN-CODE
               END-IF
           END-IF

           IF NOT HARD-ERROR
               IF WK-REV-COUNT > ZERO
                   MOVE WK-REV-COUNT TO DSP-COUNT
                   DISPLAY 'BALANCE REV COUNT=' DSP-COUNT
                   MOVE 'Y' TO SW-HARD-ERROR
                   MOVE 8 TO RETURN-CODE
               END-IF
           END-IF

           IF NOT HARD-ERROR
               MOVE WK-TARGET-COUNT TO DSP-COUNT
               MOVE WK-BAL-SUM TO DSP-AMOUNT
               DISPLAY 'CHECK OK COUNT=' DSP-COUNT
               DISPLAY 'CHECK OK AMOUNT=' DSP-AMOUNT
           END-IF.

       5000-WRITE-RECON-BEFORE.
           MOVE 'BF' TO WK-DISPOSITION
           MOVE 'KZ470B' TO WK-RC-KEY-PREFIX
           ADD 1 TO WK-RC-SEQ
           PERFORM 5100-BUILD-RECON
           WRITE KZRCNF-REC
           IF FS-KZRCNF NOT = '00'
               DISPLAY 'KZRCNF WRITE ERR ST=' FS-KZRCNF
               MOVE 'Y' TO SW-HARD-ERROR
               MOVE 12 TO RETURN-CODE
           END-IF.

       5100-BUILD-RECON.
           INITIALIZE KZRCNF-REC
           STRING WK-RC-KEY-PREFIX DELIMITED BY SIZE
                  WK-DISPOSITION DELIMITED BY SIZE
                  WK-RC-SEQ DELIMITED BY SIZE
               INTO RC-RECON-KEY
           END-STRING
           MOVE WK-TARGET-CYCLE TO RC-CYCLE-ID
           MOVE WK-BAL-SUM TO RC-GL-TOTAL-AMT
           MOVE ZERO TO RC-ACCRUAL-TOTAL-AMT
           MOVE ZERO TO RC-TAX-TOTAL-AMT
           MOVE ZERO TO RC-DIFF-AMT
           MOVE WK-DISPOSITION TO RC-DISPOSITION-CD.

       6000-UPDATE-CONTROL.
           MOVE WK-RUN-DT TO CL-RUN-DT
           MOVE WK-TARGET-CYCLE TO CL-CYCLE-ID
           MOVE 'KZ410B' TO CL-RESTART-POINT
           MOVE 'READY' TO CL-RUN-STATUS
           MOVE 'KZ470B' TO CL-LAST-PGM-ID
           REWRITE KZCTLF-REC
           IF FS-KZCTLF NOT = '00'
               DISPLAY 'KZCTLF REWRITE ERR ST=' FS-KZCTLF
               MOVE 'Y' TO SW-HARD-ERROR
               MOVE 12 TO RETURN-CODE
           END-IF.

       7000-WRITE-RECON-AFTER.
           MOVE 'AF' TO WK-DISPOSITION
           ADD 1 TO WK-RC-SEQ
           PERFORM 5100-BUILD-RECON
           WRITE KZRCNF-REC
           IF FS-KZRCNF NOT = '00'
               DISPLAY 'KZRCNF WRITE ERR ST=' FS-KZRCNF
               MOVE 'Y' TO SW-HARD-ERROR
               MOVE 12 TO RETURN-CODE
           END-IF.

       9000-FINISH.
           CLOSE KZCYRF
           CLOSE KZDBALF
           CLOSE KZCTLF
           CLOSE KZRCNF

           IF RETURN-CODE = 0
               DISPLAY 'KZ470B NORMAL END'
           ELSE
               DISPLAY 'KZ470B ABNORMAL END RC=' RETURN-CODE
           END-IF.
