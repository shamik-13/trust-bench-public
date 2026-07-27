       IDENTIFICATION DIVISION.
       PROGRAM-ID. KZ495B.
      * 変更履歴
      * 版数  年月日(和暦)  担当                    概要
      * 1.00  平成28.04.01  システム部 勘定系チーム  新規作成
      * 1.10  令和02.10.15  システム部 勘定系チーム  利息再開判定条件見直し
      * 1.20  令和05.06.30  システム部 勘定系チーム  休止口座抽出処理改善

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZCTLF ASSIGN TO "KZCTLF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CL-CONTROL-KEY
               FILE STATUS IS WS-KZCTLF-ST.

           SELECT KZCYRF ASSIGN TO "KZCYRF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-KZCYRF-ST.

           SELECT KZRCNF ASSIGN TO "KZRCNF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-KZRCNF-ST.

       DATA DIVISION.
       FILE SECTION.
       FD  KZCTLF.
           COPY KZCTLFC.

       FD  KZCYRF.
           COPY KZCYRFC.

       FD  KZRCNF.
           COPY KZRCNFC.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05 WS-KZCTLF-ST              PIC XX VALUE SPACES.
           05 WS-KZCYRF-ST              PIC XX VALUE SPACES.
           05 WS-KZRCNF-ST              PIC XX VALUE SPACES.

       01  WS-SWITCHES.
           05 WS-END-CYRF               PIC X VALUE "N".
              88 END-CYRF                     VALUE "Y".
           05 WS-END-RCNF               PIC X VALUE "N".
              88 END-RCNF                     VALUE "Y".
           05 WS-CYCLE-FOUND            PIC X VALUE "N".
              88 CYCLE-FOUND                  VALUE "Y".
           05 WS-RECON-FOUND            PIC X VALUE "N".
              88 RECON-FOUND                  VALUE "Y".
           05 WS-RESTART-OK             PIC X VALUE "N".
              88 RESTART-OK                   VALUE "Y".
           05 WS-HARD-ERROR             PIC X VALUE "N".
              88 HARD-ERROR                   VALUE "Y".

       01  WS-CONTROL.
           05 WS-TARGET-KEY             PIC X(16)
              VALUE "KZ495B-CONTROL".
           05 WS-ALLOW-PGM              PIC X(08)
              VALUE "KZ470B".
           05 WS-BLOCK-PGM              PIC X(08)
              VALUE "KZ495B".
           05 WS-READY-STATUS           PIC X(01)
              VALUE "R".
           05 WS-HOLD-STATUS            PIC X(01)
              VALUE "H".

       01  WS-ACCUMULATORS.
           05 WS-RECON-COUNT            PIC 9(07) VALUE ZERO.
           05 WS-GL-TOTAL               PIC S9(15)V99 VALUE ZERO.
           05 WS-ACCRUAL-TOTAL          PIC S9(15)V99 VALUE ZERO.
           05 WS-TAX-TOTAL              PIC S9(15)V99 VALUE ZERO.
           05 WS-DIFF-TOTAL             PIC S9(15)V99 VALUE ZERO.
           05 WS-ABS-DIFF               PIC 9(15)V99 VALUE ZERO.

       01  WS-DISPLAY-FIELDS.
           05 WS-MSG-AMT                PIC -ZZZZZZZZZZZZZZ9.99.
           05 WS-MSG-CNT                PIC ZZZZZZ9.

       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-INITIALIZE
           IF NOT HARD-ERROR
               PERFORM 2000-READ-CONTROL
           END-IF
           IF NOT HARD-ERROR
               PERFORM 3000-READ-CYCLE
           END-IF
           IF NOT HARD-ERROR
               PERFORM 4000-SUM-RECON
           END-IF
           IF NOT HARD-ERROR
               PERFORM 5000-DECIDE-RESTART
           END-IF
           PERFORM 9000-FINALIZE
           GOBACK.

       1000-INITIALIZE.
           MOVE 8 TO RETURN-CODE
           OPEN I-O KZCTLF
           IF WS-KZCTLF-ST NOT = "00"
               DISPLAY "KZCTLF OPEN ERROR ST=" WS-KZCTLF-ST
               SET HARD-ERROR TO TRUE
           END-IF

           IF NOT HARD-ERROR
               OPEN INPUT KZCYRF
               IF WS-KZCYRF-ST NOT = "00"
                   DISPLAY "KZCYRF OPEN ERROR ST=" WS-KZCYRF-ST
                   SET HARD-ERROR TO TRUE
               END-IF
           END-IF

           IF NOT HARD-ERROR
               OPEN INPUT KZRCNF
               IF WS-KZRCNF-ST NOT = "00"
                   DISPLAY "KZRCNF OPEN ERROR ST=" WS-KZRCNF-ST
                   SET HARD-ERROR TO TRUE
               END-IF
           END-IF.

       2000-READ-CONTROL.
           MOVE WS-TARGET-KEY TO CL-CONTROL-KEY
           READ KZCTLF KEY IS CL-CONTROL-KEY
               INVALID KEY
                   DISPLAY "KZCTLF CONTROL NOT FOUND"
                   DISPLAY "KEY=" CL-CONTROL-KEY
                   SET HARD-ERROR TO TRUE
               NOT INVALID KEY
                   CONTINUE
           END-READ

           IF NOT HARD-ERROR
               IF CL-RUN-STATUS NOT = "A"
                   DISPLAY "RESTART STATUS INVALID="
                           CL-RUN-STATUS
                   SET HARD-ERROR TO TRUE
               END-IF
           END-IF

           IF NOT HARD-ERROR
               IF CL-CYCLE-ID = SPACES
                   DISPLAY "CYCLE ID IS BLANK"
                   SET HARD-ERROR TO TRUE
               END-IF
           END-IF

           IF NOT HARD-ERROR
               IF CL-RESTART-POINT = SPACES
                   DISPLAY "RESTART POINT IS BLANK"
                   SET HARD-ERROR TO TRUE
               END-IF
           END-IF.

       3000-READ-CYCLE.
           PERFORM UNTIL END-CYRF OR CYCLE-FOUND
               READ KZCYRF
                   AT END
                       SET END-CYRF TO TRUE
                   NOT AT END
                       IF CR-CYCLE-ID = CL-CYCLE-ID
                           SET CYCLE-FOUND TO TRUE
                       END-IF
               END-READ
           END-PERFORM

           IF NOT CYCLE-FOUND
               DISPLAY "KZCYRF CYCLE NOT FOUND"
               DISPLAY "ID=" CL-CYCLE-ID
               SET HARD-ERROR TO TRUE
           END-IF

           IF NOT HARD-ERROR
               IF CR-ROLLED-FLAG NOT = "Y" AND
                  CR-ROLLED-FLAG NOT = "N"
                   DISPLAY "ROLLED FLAG INVALID="
                           CR-ROLLED-FLAG
                   SET HARD-ERROR TO TRUE
               END-IF
           END-IF

           IF NOT HARD-ERROR
               IF CR-RESOLVED-DT = ZERO
                   DISPLAY "RESOLVED DATE IS ZERO"
                   SET HARD-ERROR TO TRUE
               END-IF
           END-IF.

       4000-SUM-RECON.
           PERFORM UNTIL END-RCNF
               READ KZRCNF
                   AT END
                       SET END-RCNF TO TRUE
                   NOT AT END
                       IF RC-CYCLE-ID = CL-CYCLE-ID
                           PERFORM 4100-ACCUM-RECON
                       END-IF
               END-READ
           END-PERFORM

           IF WS-RECON-COUNT > ZERO
               SET RECON-FOUND TO TRUE
           END-IF

           IF NOT RECON-FOUND
               DISPLAY "KZRCNF RECON NOT FOUND"
               DISPLAY "ID=" CL-CYCLE-ID
               SET HARD-ERROR TO TRUE
           END-IF.

       4100-ACCUM-RECON.
           ADD 1 TO WS-RECON-COUNT
           ADD RC-GL-TOTAL-AMT      TO WS-GL-TOTAL
           ADD RC-ACCRUAL-TOTAL-AMT TO WS-ACCRUAL-TOTAL
           ADD RC-TAX-TOTAL-AMT     TO WS-TAX-TOTAL
           ADD RC-DIFF-AMT          TO WS-DIFF-TOTAL

           IF RC-DISPOSITION-CD NOT = "A" AND
              RC-DISPOSITION-CD NOT = "C"
               DISPLAY "RECON DISPOSITION INVALID"
               DISPLAY "KEY=" RC-RECON-KEY
               DISPLAY "CD=" RC-DISPOSITION-CD
               SET HARD-ERROR TO TRUE
               SET END-RCNF TO TRUE
           END-IF.

       5000-DECIDE-RESTART.
           MOVE WS-DIFF-TOTAL TO WS-ABS-DIFF
           IF WS-DIFF-TOTAL < ZERO
               COMPUTE WS-ABS-DIFF = WS-DIFF-TOTAL * -1
           END-IF

           IF WS-ABS-DIFF > ZERO
               MOVE WS-ABS-DIFF TO WS-MSG-AMT
               DISPLAY "RESTART HOLD TAX DIFF="
                       WS-MSG-AMT
               PERFORM 5200-HOLD-CONTROL
           ELSE
               IF WS-GL-TOTAL NOT = WS-ACCRUAL-TOTAL
                   COMPUTE WS-ABS-DIFF =
                       FUNCTION ABS(WS-GL-TOTAL - WS-ACCRUAL-TOTAL)
                   MOVE WS-ABS-DIFF TO WS-MSG-AMT
                   DISPLAY "RESTART HOLD GL DIFF="
                           WS-MSG-AMT
                   PERFORM 5200-HOLD-CONTROL
               ELSE
                   PERFORM 5100-ALLOW-CONTROL
               END-IF
           END-IF.

       5100-ALLOW-CONTROL.
           MOVE WS-ALLOW-PGM    TO CL-RESTART-POINT
           MOVE WS-READY-STATUS TO CL-RUN-STATUS
           MOVE WS-BLOCK-PGM    TO CL-LAST-PGM-ID
           REWRITE KZCTLF-REC
               INVALID KEY
                   DISPLAY "KZCTLF ALLOW UPDATE ERROR ST="
                           WS-KZCTLF-ST
                   SET HARD-ERROR TO TRUE
               NOT INVALID KEY
                   SET RESTART-OK TO TRUE
                   MOVE WS-RECON-COUNT TO WS-MSG-CNT
                   DISPLAY "RESTART ALLOW KZ470B COUNT="
                           WS-MSG-CNT
           END-REWRITE.

       5200-HOLD-CONTROL.
           MOVE WS-BLOCK-PGM   TO CL-RESTART-POINT
           MOVE WS-HOLD-STATUS TO CL-RUN-STATUS
           MOVE WS-BLOCK-PGM   TO CL-LAST-PGM-ID
           REWRITE KZCTLF-REC
               INVALID KEY
                   DISPLAY "KZCTLF HOLD UPDATE ERROR ST="
                           WS-KZCTLF-ST
                   SET HARD-ERROR TO TRUE
               NOT INVALID KEY
                   DISPLAY "KZCTLF HOLD UPDATED"
           END-REWRITE.

       9000-FINALIZE.
           IF WS-KZRCNF-ST NOT = SPACES
               CLOSE KZRCNF
           END-IF
           IF WS-KZCYRF-ST NOT = SPACES
               CLOSE KZCYRF
           END-IF
           IF WS-KZCTLF-ST NOT = SPACES
               CLOSE KZCTLF
           END-IF

           IF HARD-ERROR
               MOVE 12 TO RETURN-CODE
           ELSE
               IF RESTART-OK
                   MOVE 0 TO RETURN-CODE
               ELSE
                   MOVE 8 TO RETURN-CODE
               END-IF
           END-IF.
