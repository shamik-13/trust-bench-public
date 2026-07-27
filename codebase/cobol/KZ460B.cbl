       IDENTIFICATION DIVISION.
       PROGRAM-ID. KZ460B.
      * 変更履歴
      * 版数  年月日(和暦)  担当                         概要
      * 1.00  H28.04.01    システム部 勘定系チーム      新規作成
      * 1.01  R02.10.15    システム部 勘定系チーム      利息訂正反対仕訳抽出条件見直し
      * 1.02  R06.03.18    システム部 勘定系チーム      仕訳データ出力項目追加

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZREVF ASSIGN TO "KZREVF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS RV-REVERSAL-ID
               FILE STATUS IS WS-ST-KZREVF.

           SELECT KZINTF ASSIGN TO "KZINTF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-ST-KZINTF.

           SELECT KZGLPF ASSIGN TO "KZGLPF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-ST-KZGLPF.

           SELECT KZCTLF ASSIGN TO "KZCTLF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CL-CONTROL-KEY
               FILE STATUS IS WS-ST-KZCTLF.

           SELECT KZRCNF ASSIGN TO "KZRCNF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-ST-KZRCNF.

       DATA DIVISION.
       FILE SECTION.
       FD  KZREVF.
           COPY KZREVFC.

       FD  KZINTF.
           COPY KZINTFC.

       FD  KZGLPF.
           COPY KZGLPFC.

       FD  KZCTLF.
           COPY KZCTLFC.

       FD  KZRCNF.
           COPY KZRCNFC.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05 WS-ST-KZREVF              PIC XX VALUE SPACES.
           05 WS-ST-KZINTF              PIC XX VALUE SPACES.
           05 WS-ST-KZGLPF              PIC XX VALUE SPACES.
           05 WS-ST-KZCTLF              PIC XX VALUE SPACES.
           05 WS-ST-KZRCNF              PIC XX VALUE SPACES.

       01  WS-FILE-OPEN-SWITCHES.
           05 WS-KZREVF-OPEN            PIC X VALUE 'N'.
              88 KZREVF-IS-OPEN              VALUE 'Y'.
           05 WS-KZINTF-OPEN            PIC X VALUE 'N'.
              88 KZINTF-IS-OPEN              VALUE 'Y'.
           05 WS-KZGLPF-OPEN            PIC X VALUE 'N'.
              88 KZGLPF-IS-OPEN              VALUE 'Y'.
           05 WS-KZCTLF-OPEN            PIC X VALUE 'N'.
              88 KZCTLF-IS-OPEN              VALUE 'Y'.
           05 WS-KZRCNF-OPEN            PIC X VALUE 'N'.
              88 KZRCNF-IS-OPEN              VALUE 'Y'.

       01  WS-SWITCHES.
           05 WS-EOF-KZREVF             PIC X VALUE 'N'.
              88 EOF-KZREVF                  VALUE 'Y'.
           05 WS-EOF-KZINTF             PIC X VALUE 'N'.
              88 EOF-KZINTF                  VALUE 'Y'.
           05 WS-EOF-KZGLPF             PIC X VALUE 'N'.
              88 EOF-KZGLPF                  VALUE 'Y'.
           05 WS-HARD-ERROR             PIC X VALUE 'N'.
              88 HARD-ERROR                  VALUE 'Y'.
           05 WS-FOUND-INT              PIC X VALUE 'N'.
              88 FOUND-INT                   VALUE 'Y'.
           05 WS-FOUND-GL               PIC X VALUE 'N'.
              88 FOUND-GL                    VALUE 'Y'.

       01  WS-CONSTANTS.
           05 WS-PGM-ID                 PIC X(08) VALUE 'KZ460B'.
           05 WS-CTL-KEY                PIC X(16)
              VALUE 'KZ460B-CONTROL'.
           05 WS-STATUS-RUN             PIC X VALUE 'R'.
           05 WS-STATUS-END             PIC X VALUE 'E'.
           05 WS-STATUS-ERR             PIC X VALUE 'A'.
           05 WS-CR-KBN                 PIC X VALUE 'C'.
           05 WS-GL-ACCT-CD             PIC X(10) VALUE 'INTREV    '.
           05 WS-TAX-KBN                PIC X VALUE '0'.
           05 WS-DISP-NORMAL            PIC X(02) VALUE '00'.
           05 WS-DISP-DIFF              PIC X(02) VALUE '10'.
           05 WS-DISP-REJECT            PIC X(02) VALUE '90'.

       01  WS-COUNTERS.
           05 WS-REV-READ-CNT           PIC 9(9) VALUE 0.
           05 WS-REV-OK-CNT             PIC 9(9) VALUE 0.
           05 WS-REV-NG-CNT             PIC 9(9) VALUE 0.
           05 WS-INT-LOAD-CNT           PIC 9(5) VALUE 0.
           05 WS-GL-LOAD-CNT            PIC 9(5) VALUE 0.
           05 WS-INT-WRITE-CNT          PIC 9(9) VALUE 0.
           05 WS-GL-WRITE-CNT           PIC 9(9) VALUE 0.
           05 WS-RCN-WRITE-CNT          PIC 9(9) VALUE 0.
           05 WS-IDX                    PIC 9(5) VALUE 0.
           05 WS-JOURNAL-SEQ            PIC 9(7) VALUE 0.
           05 WS-RECON-SEQ              PIC 9(7) VALUE 0.

       01  WS-AMOUNTS.
           05 WS-REV-AMT                PIC S9(13)V99 VALUE 0.
           05 WS-OPP-AMT                PIC S9(13)V99 VALUE 0.
           05 WS-ACCT-PAST-TOTAL        PIC S9(13)V99 VALUE 0.
           05 WS-ACCT-AFTER-TOTAL       PIC S9(13)V99 VALUE 0.
           05 WS-GL-MATCH-TOTAL         PIC S9(13)V99 VALUE 0.
           05 WS-ACCR-MATCH-TOTAL       PIC S9(13)V99 VALUE 0.
           05 WS-TAX-TOTAL              PIC S9(13)V99 VALUE 0.
           05 WS-DIFF-AMT               PIC S9(13)V99 VALUE 0.

       01  WS-DATES.
           05 WS-TODAY                  PIC 9(8) VALUE 0.

       01  WS-INT-TABLE.
           05 WS-INT-ENTRY OCCURS 12000 TIMES.
              10 TB-IN-ACCT-NO          PIC X(20).
              10 TB-IN-CYCLE-ID         PIC X(10).
              10 TB-IN-ACCRUAL-DT       PIC 9(8).
              10 TB-IN-ACCR-DAYS        PIC 9(4).
              10 TB-IN-INT-AMT          PIC S9(13)V99.

       01  WS-GL-TABLE.
           05 WS-GL-ENTRY OCCURS 12000 TIMES.
              10 TB-GL-JOURNAL-NO       PIC X(18).
              10 TB-GL-POST-DT          PIC 9(8).
              10 TB-GL-ACCT-NO          PIC X(20).
              10 TB-GL-CYCLE-ID         PIC X(10).
              10 TB-GL-DR-CR-KBN        PIC X.
              10 TB-GL-GL-ACCT-CD       PIC X(10).
              10 TB-GL-POST-AMT         PIC S9(13)V99.
              10 TB-GL-TAX-KBN          PIC X.
              10 TB-GL-SRC-PGM-ID       PIC X(8).

       PROCEDURE DIVISION.
       MAIN-SECTION.
           PERFORM 0000-INIT
           IF NOT HARD-ERROR
               PERFORM 1000-LOAD-INT
           END-IF
           IF NOT HARD-ERROR
               PERFORM 1100-LOAD-GL
           END-IF
           IF NOT HARD-ERROR
               PERFORM 2000-PROCESS-REVERSAL
           END-IF
           PERFORM 9000-FINALIZE
           GOBACK
           .

       0000-INIT.
           MOVE 0 TO RETURN-CODE
           ACCEPT WS-TODAY FROM DATE YYYYMMDD

           OPEN I-O KZCTLF
           IF WS-ST-KZCTLF NOT = '00'
               DISPLAY 'KZCTLF OPEN ERROR ST=' WS-ST-KZCTLF
               SET HARD-ERROR TO TRUE
               MOVE 12 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF
           SET KZCTLF-IS-OPEN TO TRUE

           MOVE WS-CTL-KEY TO CL-CONTROL-KEY
           READ KZCTLF KEY IS CL-CONTROL-KEY
               INVALID KEY
                   DISPLAY 'CONTROL RECORD NOT FOUND KEY=' WS-CTL-KEY
                   SET HARD-ERROR TO TRUE
                   MOVE 12 TO RETURN-CODE
               NOT INVALID KEY
                   CONTINUE
           END-READ
           IF HARD-ERROR
               EXIT PARAGRAPH
           END-IF

           IF CL-RUN-STATUS = WS-STATUS-RUN
               DISPLAY 'PREVIOUS RUN IS STILL ACTIVE'
               SET HARD-ERROR TO TRUE
               MOVE 8 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF

           MOVE WS-TODAY      TO CL-RUN-DT
           MOVE WS-PGM-ID     TO CL-LAST-PGM-ID
           MOVE WS-STATUS-RUN TO CL-RUN-STATUS
           REWRITE KZCTLF-REC
           IF WS-ST-KZCTLF NOT = '00'
               DISPLAY 'KZCTLF REWRITE ERROR ST=' WS-ST-KZCTLF
               SET HARD-ERROR TO TRUE
               MOVE 12 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF

           OPEN INPUT KZINTF
           IF WS-ST-KZINTF NOT = '00'
               DISPLAY 'KZINTF OPEN ERROR ST=' WS-ST-KZINTF
               SET HARD-ERROR TO TRUE
               MOVE 12 TO RETURN-CODE
           ELSE
               SET KZINTF-IS-OPEN TO TRUE
           END-IF

           OPEN INPUT KZGLPF
           IF WS-ST-KZGLPF NOT = '00'
               DISPLAY 'KZGLPF OPEN ERROR ST=' WS-ST-KZGLPF
               SET HARD-ERROR TO TRUE
               MOVE 12 TO RETURN-CODE
           ELSE
               SET KZGLPF-IS-OPEN TO TRUE
           END-IF

           OPEN INPUT KZREVF
           IF WS-ST-KZREVF NOT = '00'
               DISPLAY 'KZREVF OPEN ERROR ST=' WS-ST-KZREVF
               SET HARD-ERROR TO TRUE
               MOVE 12 TO RETURN-CODE
           ELSE
               SET KZREVF-IS-OPEN TO TRUE
           END-IF

           OPEN OUTPUT KZRCNF
           IF WS-ST-KZRCNF NOT = '00'
               DISPLAY 'KZRCNF OPEN ERROR ST=' WS-ST-KZRCNF
               SET HARD-ERROR TO TRUE
               MOVE 12 TO RETURN-CODE
           ELSE
               SET KZRCNF-IS-OPEN TO TRUE
           END-IF
           .

       1000-LOAD-INT.
           PERFORM UNTIL EOF-KZINTF OR HARD-ERROR
               READ KZINTF
                   AT END
                       SET EOF-KZINTF TO TRUE
                   NOT AT END
                       IF WS-INT-LOAD-CNT >= 12000
                           DISPLAY 'KZINTF TABLE OVERFLOW'
                           SET HARD-ERROR TO TRUE
                           MOVE 12 TO RETURN-CODE
                       ELSE
                           ADD 1 TO WS-INT-LOAD-CNT
                           MOVE IN-ACCT-NO
                             TO TB-IN-ACCT-NO(WS-INT-LOAD-CNT)
                           MOVE IN-CYCLE-ID
                             TO TB-IN-CYCLE-ID(WS-INT-LOAD-CNT)
                           MOVE IN-ACCRUAL-DT
                             TO TB-IN-ACCRUAL-DT(WS-INT-LOAD-CNT)
                           MOVE IN-ACCR-DAYS
                             TO TB-IN-ACCR-DAYS(WS-INT-LOAD-CNT)
                           MOVE IN-INT-AMT
                             TO TB-IN-INT-AMT(WS-INT-LOAD-CNT)
                       END-IF
               END-READ
               IF WS-ST-KZINTF NOT = '00' AND NOT EOF-KZINTF
                   DISPLAY 'KZINTF READ ERROR ST=' WS-ST-KZINTF
                   SET HARD-ERROR TO TRUE
                   MOVE 12 TO RETURN-CODE
               END-IF
           END-PERFORM
           IF KZINTF-IS-OPEN
               CLOSE KZINTF
               MOVE 'N' TO WS-KZINTF-OPEN
           END-IF
           MOVE SPACES TO WS-ST-KZINTF
           .

       1100-LOAD-GL.
           PERFORM UNTIL EOF-KZGLPF OR HARD-ERROR
               READ KZGLPF
                   AT END
                       SET EOF-KZGLPF TO TRUE
                   NOT AT END
                       IF WS-GL-LOAD-CNT >= 12000
                           DISPLAY 'KZGLPF TABLE OVERFLOW'
                           SET HARD-ERROR TO TRUE
                           MOVE 12 TO RETURN-CODE
                       ELSE
                           ADD 1 TO WS-GL-LOAD-CNT
                           MOVE GL-JOURNAL-NO
                             TO TB-GL-JOURNAL-NO(WS-GL-LOAD-CNT)
                           MOVE GL-POST-DT
                             TO TB-GL-POST-DT(WS-GL-LOAD-CNT)
                           MOVE GL-ACCT-NO
                             TO TB-GL-ACCT-NO(WS-GL-LOAD-CNT)
                           MOVE GL-CYCLE-ID
                             TO TB-GL-CYCLE-ID(WS-GL-LOAD-CNT)
                           MOVE GL-DR-CR-KBN
                             TO TB-GL-DR-CR-KBN(WS-GL-LOAD-CNT)
                           MOVE GL-GL-ACCT-CD
                             TO TB-GL-GL-ACCT-CD(WS-GL-LOAD-CNT)
                           MOVE GL-POST-AMT
                             TO TB-GL-POST-AMT(WS-GL-LOAD-CNT)
                           MOVE GL-TAX-KBN
                             TO TB-GL-TAX-KBN(WS-GL-LOAD-CNT)
                           MOVE GL-SRC-PGM-ID
                             TO TB-GL-SRC-PGM-ID(WS-GL-LOAD-CNT)
                       END-IF
               END-READ
               IF WS-ST-KZGLPF NOT = '00' AND NOT EOF-KZGLPF
                   DISPLAY 'KZGLPF READ ERROR ST=' WS-ST-KZGLPF
                   SET HARD-ERROR TO TRUE
                   MOVE 12 TO RETURN-CODE
               END-IF
           END-PERFORM
           IF KZGLPF-IS-OPEN
               CLOSE KZGLPF
               MOVE 'N' TO WS-KZGLPF-OPEN
           END-IF
           MOVE SPACES TO WS-ST-KZGLPF
           .

       2000-PROCESS-REVERSAL.
           OPEN EXTEND KZINTF
           IF WS-ST-KZINTF NOT = '00'
               DISPLAY 'KZINTF EXTEND ERROR ST=' WS-ST-KZINTF
               SET HARD-ERROR TO TRUE
               MOVE 12 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF
           SET KZINTF-IS-OPEN TO TRUE

           OPEN EXTEND KZGLPF
           IF WS-ST-KZGLPF NOT = '00'
               DISPLAY 'KZGLPF EXTEND ERROR ST=' WS-ST-KZGLPF
               SET HARD-ERROR TO TRUE
               MOVE 12 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF
           SET KZGLPF-IS-OPEN TO TRUE

           PERFORM UNTIL EOF-KZREVF OR HARD-ERROR
               READ KZREVF
                   AT END
                       SET EOF-KZREVF TO TRUE
                   NOT AT END
                       ADD 1 TO WS-REV-READ-CNT
                       PERFORM 2100-CHECK-REVERSAL
                       IF FOUND-INT AND FOUND-GL
                           PERFORM 2200-WRITE-CORRECTION
                       ELSE
                           ADD 1 TO WS-REV-NG-CNT
                           PERFORM 2300-WRITE-REJECT-RECON
                       END-IF
               END-READ
               IF WS-ST-KZREVF NOT = '00' AND NOT EOF-KZREVF
                   DISPLAY 'KZREVF READ ERROR ST=' WS-ST-KZREVF
                   SET HARD-ERROR TO TRUE
                   MOVE 12 TO RETURN-CODE
               END-IF
           END-PERFORM
           .

       2100-CHECK-REVERSAL.
           MOVE 'N' TO WS-FOUND-INT
           MOVE 'N' TO WS-FOUND-GL
           MOVE 0 TO WS-ACCT-PAST-TOTAL
                     WS-ACCT-AFTER-TOTAL
                     WS-GL-MATCH-TOTAL
                     WS-ACCR-MATCH-TOTAL
                     WS-TAX-TOTAL
                     WS-DIFF-AMT
           MOVE RV-REV-INT-AMT TO WS-REV-AMT
           COMPUTE WS-OPP-AMT = WS-REV-AMT * -1

           IF RV-APPROVED-ID = SPACES
               DISPLAY 'REVERSAL NOT APPROVED ID=' RV-REVERSAL-ID
               EXIT PARAGRAPH
           END-IF

           IF RV-REV-INT-AMT <= 0
               DISPLAY 'INVALID REVERSAL AMOUNT ID=' RV-REVERSAL-ID
               EXIT PARAGRAPH
           END-IF

           PERFORM VARYING WS-IDX FROM 1 BY 1
               UNTIL WS-IDX > WS-INT-LOAD-CNT
               IF TB-IN-ACCT-NO(WS-IDX) = RV-ACCT-NO
                   ADD TB-IN-INT-AMT(WS-IDX) TO WS-ACCT-PAST-TOTAL
                   IF TB-IN-CYCLE-ID(WS-IDX) = RV-CYCLE-ID
                      AND TB-IN-INT-AMT(WS-IDX) = RV-ORIG-INT-AMT
                       SET FOUND-INT TO TRUE
                       ADD TB-IN-INT-AMT(WS-IDX)
                         TO WS-ACCR-MATCH-TOTAL
                   END-IF
               END-IF
           END-PERFORM

           PERFORM VARYING WS-IDX FROM 1 BY 1
               UNTIL WS-IDX > WS-GL-LOAD-CNT
               IF TB-GL-ACCT-NO(WS-IDX) = RV-ACCT-NO
                  AND TB-GL-CYCLE-ID(WS-IDX) = RV-CYCLE-ID
                  AND TB-GL-POST-AMT(WS-IDX) = RV-ORIG-INT-AMT
                   SET FOUND-GL TO TRUE
                   ADD TB-GL-POST-AMT(WS-IDX) TO WS-GL-MATCH-TOTAL
               END-IF
           END-PERFORM

           IF NOT FOUND-INT
               DISPLAY 'SOURCE INTEREST NOT FOUND ID=' RV-REVERSAL-ID
               EXIT PARAGRAPH
           END-IF

           IF NOT FOUND-GL
               DISPLAY 'SOURCE JOURNAL NOT FOUND ID=' RV-REVERSAL-ID
               EXIT PARAGRAPH
           END-IF

           COMPUTE WS-ACCT-AFTER-TOTAL =
               WS-ACCT-PAST-TOTAL + WS-OPP-AMT

           IF WS-ACCT-AFTER-TOTAL < 0
               DISPLAY 'NEGATIVE ACCOUNT TOTAL ID=' RV-REVERSAL-ID
               MOVE 'N' TO WS-FOUND-INT
               MOVE 'N' TO WS-FOUND-GL
               EXIT PARAGRAPH
           END-IF

           COMPUTE WS-DIFF-AMT =
               WS-GL-MATCH-TOTAL - WS-ACCR-MATCH-TOTAL
           .

       2200-WRITE-CORRECTION.
           MOVE SPACES           TO KZINTF-REC
           MOVE RV-ACCT-NO       TO IN-ACCT-NO
           MOVE RV-CYCLE-ID      TO IN-CYCLE-ID
           MOVE WS-TODAY         TO IN-ACCRUAL-DT
           MOVE 0                TO IN-ACCR-DAYS
           MOVE WS-OPP-AMT       TO IN-INT-AMT
           WRITE KZINTF-REC
           IF WS-ST-KZINTF NOT = '00'
               DISPLAY 'KZINTF WRITE ERROR ST=' WS-ST-KZINTF
               SET HARD-ERROR TO TRUE
               MOVE 12 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF
           ADD 1 TO WS-INT-WRITE-CNT

           ADD 1 TO WS-JOURNAL-SEQ
           MOVE SPACES           TO KZGLPF-REC
           STRING WS-PGM-ID WS-TODAY WS-JOURNAL-SEQ
               DELIMITED BY SIZE
               INTO GL-JOURNAL-NO
           END-STRING
           MOVE WS-TODAY         TO GL-POST-DT
           MOVE RV-ACCT-NO       TO GL-ACCT-NO
           MOVE RV-CYCLE-ID      TO GL-CYCLE-ID
           MOVE WS-CR-KBN        TO GL-DR-CR-KBN
           MOVE WS-GL-ACCT-CD    TO GL-GL-ACCT-CD
           MOVE RV-REV-INT-AMT   TO GL-POST-AMT
           MOVE WS-TAX-KBN       TO GL-TAX-KBN
           MOVE WS-PGM-ID        TO GL-SRC-PGM-ID
           WRITE KZGLPF-REC
           IF WS-ST-KZGLPF NOT = '00'
               DISPLAY 'KZGLPF WRITE ERROR ST=' WS-ST-KZGLPF
               SET HARD-ERROR TO TRUE
               MOVE 12 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF
           ADD 1 TO WS-GL-WRITE-CNT
           ADD 1 TO WS-REV-OK-CNT

           PERFORM 2400-WRITE-RECON
           .

       2300-WRITE-REJECT-RECON.
           MOVE 0 TO WS-GL-MATCH-TOTAL
                     WS-ACCR-MATCH-TOTAL
                     WS-TAX-TOTAL
           MOVE RV-REV-INT-AMT TO WS-DIFF-AMT
           PERFORM 2500-BUILD-RECON-KEY
           MOVE RV-CYCLE-ID          TO RC-CYCLE-ID
           MOVE WS-GL-MATCH-TOTAL   TO RC-GL-TOTAL-AMT
           MOVE WS-ACCR-MATCH-TOTAL TO RC-ACCRUAL-TOTAL-AMT
           MOVE WS-TAX-TOTAL        TO RC-TAX-TOTAL-AMT
           MOVE WS-DIFF-AMT         TO RC-DIFF-AMT
           MOVE WS-DISP-REJECT      TO RC-DISPOSITION-CD
           WRITE KZRCNF-REC
           IF WS-ST-KZRCNF NOT = '00'
               DISPLAY 'KZRCNF REJECT WRITE ERROR ST=' WS-ST-KZRCNF
               SET HARD-ERROR TO TRUE
               MOVE 12 TO RETURN-CODE
           ELSE
               ADD 1 TO WS-RCN-WRITE-CNT
           END-IF
           .

       2400-WRITE-RECON.
           MOVE 0 TO WS-TAX-TOTAL
           PERFORM 2500-BUILD-RECON-KEY
           MOVE RV-CYCLE-ID          TO RC-CYCLE-ID
           MOVE WS-GL-MATCH-TOTAL   TO RC-GL-TOTAL-AMT
           MOVE WS-ACCR-MATCH-TOTAL TO RC-ACCRUAL-TOTAL-AMT
           MOVE WS-TAX-TOTAL        TO RC-TAX-TOTAL-AMT
           MOVE WS-DIFF-AMT         TO RC-DIFF-AMT
           IF WS-DIFF-AMT = 0
               MOVE WS-DISP-NORMAL  TO RC-DISPOSITION-CD
           ELSE
               MOVE WS-DISP-DIFF    TO RC-DISPOSITION-CD
           END-IF
           WRITE KZRCNF-REC
           IF WS-ST-KZRCNF NOT = '00'
               DISPLAY 'KZRCNF RECON WRITE ERROR ST=' WS-ST-KZRCNF
               SET HARD-ERROR TO TRUE
               MOVE 12 TO RETURN-CODE
           ELSE
               ADD 1 TO WS-RCN-WRITE-CNT
           END-IF
           .

       2500-BUILD-RECON-KEY.
           ADD 1 TO WS-RECON-SEQ
           MOVE SPACES TO KZRCNF-REC
           STRING RV-REVERSAL-ID WS-TODAY WS-RECON-SEQ
               DELIMITED BY SIZE
               INTO RC-RECON-KEY
           END-STRING
           .

       9000-FINALIZE.
           IF KZREVF-IS-OPEN
               CLOSE KZREVF
               MOVE 'N' TO WS-KZREVF-OPEN
           END-IF
           IF KZINTF-IS-OPEN
               CLOSE KZINTF
               MOVE 'N' TO WS-KZINTF-OPEN
           END-IF
           IF KZGLPF-IS-OPEN
               CLOSE KZGLPF
               MOVE 'N' TO WS-KZGLPF-OPEN
           END-IF
           IF KZRCNF-IS-OPEN
               CLOSE KZRCNF
               MOVE 'N' TO WS-KZRCNF-OPEN
           END-IF

           IF KZCTLF-IS-OPEN
               MOVE WS-CTL-KEY TO CL-CONTROL-KEY
               READ KZCTLF KEY IS CL-CONTROL-KEY
                   INVALID KEY
                       DISPLAY 'CONTROL RECORD MISSING AT END'
                       SET HARD-ERROR TO TRUE
                       MOVE 12 TO RETURN-CODE
                   NOT INVALID KEY
                       IF HARD-ERROR
                           MOVE WS-STATUS-ERR TO CL-RUN-STATUS
                       ELSE
                           MOVE WS-STATUS-END TO CL-RUN-STATUS
                       END-IF
                       MOVE WS-PGM-ID TO CL-LAST-PGM-ID
                       MOVE WS-TODAY  TO CL-RUN-DT
                       REWRITE KZCTLF-REC
                       IF WS-ST-KZCTLF NOT = '00'
                           DISPLAY 'KZCTLF FINAL REWRITE ERROR ST='
                                   WS-ST-KZCTLF
                           MOVE 12 TO RETURN-CODE
                       END-IF
               END-READ
               CLOSE KZCTLF
               MOVE 'N' TO WS-KZCTLF-OPEN
           END-IF

           DISPLAY 'KZ460B COUNT READ=' WS-REV-READ-CNT
                   ' OK=' WS-REV-OK-CNT
                   ' NG=' WS-REV-NG-CNT
           DISPLAY 'KZ460B COUNT INT=' WS-INT-WRITE-CNT
                   ' GL=' WS-GL-WRITE-CNT
                   ' RCN=' WS-RCN-WRITE-CNT

           IF HARD-ERROR
               IF RETURN-CODE = 0
                   MOVE 8 TO RETURN-CODE
               END-IF
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF
           .
