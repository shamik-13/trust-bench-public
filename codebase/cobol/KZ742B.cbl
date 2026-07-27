       IDENTIFICATION DIVISION.
       PROGRAM-ID. KZ742B.
      * 版数  年月日(和暦)  担当                      概要
      * 1.00  平成28.04.01 システム部 勘定系チーム  新規作成
      * 1.01  令和02.01.06 システム部 勘定系チーム  利子割納付額集計見直し
      * 1.02  令和05.10.02 システム部 勘定系チーム  都道府県別出力順補正
       AUTHOR. KZB.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZTXRF ASSIGN TO "KZTXRF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-KZTXRF-ST.
           SELECT KZPRIF ASSIGN TO "KZPRIF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-KZPRIF-ST.
           SELECT KZLCRF ASSIGN TO "KZLCRF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-KZLCRF-ST.
           SELECT KZADLF ASSIGN TO "KZADLF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-KZADLF-ST.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  KZTXRF.
           COPY KZTXRFC.
       FD  KZPRIF.
           COPY KZPRIFC.
       FD  KZLCRF.
           COPY KZLCRFC.
       FD  KZADLF.
           COPY KZADLFC.
      *
       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05 WS-KZTXRF-ST             PIC X(02) VALUE SPACE.
           05 WS-KZPRIF-ST             PIC X(02) VALUE SPACE.
           05 WS-KZLCRF-ST             PIC X(02) VALUE SPACE.
           05 WS-KZADLF-ST             PIC X(02) VALUE SPACE.
      *
       01  WS-FLAGS.
           05 WS-TX-EOF                PIC X VALUE "N".
              88 TX-EOF                      VALUE "Y".
           05 WS-PR-EOF                PIC X VALUE "N".
              88 PR-EOF                      VALUE "Y".
           05 WS-HARD-ERROR            PIC X VALUE "N".
              88 HARD-ERROR                  VALUE "Y".
      *
       01  WS-RUN.
           05 WS-RUN-DATE              PIC 9(08) VALUE ZERO.
           05 WS-REMITTANCE-DATE       PIC 9(08) VALUE ZERO.
           05 WS-AUDIT-SEQ             PIC 9(07) VALUE ZERO.
           05 WS-BANK-SEQ              PIC 9(06) VALUE ZERO.
           05 WS-AUDIT-ID              PIC X(20) VALUE SPACE.
           05 WS-BANK-REF-NO           PIC X(20) VALUE SPACE.
      *
       01  WS-CURRENT.
           05 WS-PREF-CD               PIC 9(02) VALUE ZERO.
           05 WS-STMT-PREF-CD          PIC 9(02) VALUE ZERO.
           05 WS-ACCT-IDX              PIC 9 VALUE ZERO.
           05 WS-TABLE-IDX             PIC 9(03) VALUE ZERO.
           05 WS-TOTAL-TAX             PIC S9(13) VALUE ZERO.
      *
       01  WS-TOTALS.
           05 WS-IN-COUNT              PIC 9(09) VALUE ZERO.
           05 WS-OUT-COUNT             PIC 9(09) VALUE ZERO.
           05 WS-EXCLUDE-COUNT         PIC 9(09) VALUE ZERO.
           05 WS-TOTAL-LOCAL-TAX       PIC S9(15) VALUE ZERO.
           05 WS-EXCLUDE-LOCAL-TAX     PIC S9(15) VALUE ZERO.
      *
       01  WS-AGG-TABLE.
           05 WS-AGG-ENTRY OCCURS 141 TIMES.
              10 WS-AGG-PREF-CD        PIC 9(02) VALUE ZERO.
              10 WS-AGG-ACCT-TYPE      PIC X(02) VALUE SPACE.
              10 WS-AGG-LOCAL-TAX      PIC S9(15) VALUE ZERO.
              10 WS-AGG-COUNT          PIC 9(09) VALUE ZERO.
      *
       01  WS-WORK.
           05 WS-I                     PIC 9(03) VALUE ZERO.
           05 WS-J                     PIC 9(03) VALUE ZERO.
           05 WS-NUM-EDIT              PIC Z,ZZZ,ZZZ,ZZZ,ZZ9.
      *
       PROCEDURE DIVISION.
       MAIN-SECTION.
           PERFORM 0000-INITIALIZE
           IF NOT HARD-ERROR
              PERFORM 1000-OPEN-FILES
           END-IF
           IF NOT HARD-ERROR
              PERFORM 2000-LOAD-FIRST
           END-IF
           IF NOT HARD-ERROR
              PERFORM 3000-MAIN-PROCESS
           END-IF
           IF NOT HARD-ERROR
              PERFORM 4000-WRITE-SUMMARY
           END-IF
           PERFORM 9000-CLOSE-FILES
           IF HARD-ERROR
              MOVE 12 TO RETURN-CODE
           ELSE
              MOVE 0 TO RETURN-CODE
              DISPLAY "KZ742B OK IN=" WS-IN-COUNT
              DISPLAY "KZ742B OUT=" WS-OUT-COUNT
              DISPLAY "KZ742B EXCLUDE=" WS-EXCLUDE-COUNT
              MOVE WS-TOTAL-LOCAL-TAX TO WS-NUM-EDIT
              DISPLAY "KZ902S LOCAL TAX=" WS-NUM-EDIT
           END-IF
           GOBACK.
      *
       0000-INITIALIZE.
           ACCEPT WS-RUN-DATE FROM DATE YYYYMMDD
           MOVE WS-RUN-DATE TO WS-REMITTANCE-DATE
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 141
              COMPUTE WS-PREF-CD = ((WS-I - 1) / 3) + 1
              COMPUTE WS-J = FUNCTION MOD((WS-I - 1) 3) + 1
              MOVE WS-PREF-CD TO WS-AGG-PREF-CD(WS-I)
              EVALUATE WS-J
                 WHEN 1
                    MOVE "01" TO WS-AGG-ACCT-TYPE(WS-I)
                 WHEN 2
                    MOVE "02" TO WS-AGG-ACCT-TYPE(WS-I)
                 WHEN 3
                    MOVE "03" TO WS-AGG-ACCT-TYPE(WS-I)
              END-EVALUATE
           END-PERFORM.
      *
       1000-OPEN-FILES.
           OPEN INPUT KZTXRF
           IF WS-KZTXRF-ST NOT = "00"
              DISPLAY "KZTXRF OPEN ST=" WS-KZTXRF-ST
              SET HARD-ERROR TO TRUE
           END-IF
           IF NOT HARD-ERROR
              OPEN INPUT KZPRIF
              IF WS-KZPRIF-ST NOT = "00"
                 DISPLAY "KZPRIF OPEN ST=" WS-KZPRIF-ST
                 SET HARD-ERROR TO TRUE
              END-IF
           END-IF
           IF NOT HARD-ERROR
              OPEN OUTPUT KZLCRF
              IF WS-KZLCRF-ST NOT = "00"
                 DISPLAY "KZLCRF OPEN ST=" WS-KZLCRF-ST
                 SET HARD-ERROR TO TRUE
              END-IF
           END-IF
           IF NOT HARD-ERROR
              OPEN OUTPUT KZADLF
              IF WS-KZADLF-ST NOT = "00"
                 DISPLAY "KZADLF OPEN ST=" WS-KZADLF-ST
                 SET HARD-ERROR TO TRUE
              END-IF
           END-IF.
      *
       2000-LOAD-FIRST.
           PERFORM 2100-READ-TX
           PERFORM 2200-READ-PR.
      *
       2100-READ-TX.
           READ KZTXRF
              AT END
                 SET TX-EOF TO TRUE
              NOT AT END
                 ADD 1 TO WS-IN-COUNT
           END-READ
           IF WS-KZTXRF-ST NOT = "00" AND WS-KZTXRF-ST NOT = "10"
              DISPLAY "KZTXRF READ ST=" WS-KZTXRF-ST
              SET HARD-ERROR TO TRUE
           END-IF.
      *
       2200-READ-PR.
           READ KZPRIF
              AT END
                 SET PR-EOF TO TRUE
           END-READ
           IF WS-KZPRIF-ST NOT = "00" AND WS-KZPRIF-ST NOT = "10"
              DISPLAY "KZPRIF READ ST=" WS-KZPRIF-ST
              SET HARD-ERROR TO TRUE
           END-IF.
      *
       3000-MAIN-PROCESS.
           PERFORM UNTIL TX-EOF OR HARD-ERROR
              PERFORM 3100-MATCH-PRIF
              IF HARD-ERROR
                 CONTINUE
              ELSE
                 PERFORM 3200-VALIDATE-AND-ADD
                 PERFORM 2100-READ-TX
              END-IF
           END-PERFORM.
      *
       3100-MATCH-PRIF.
           PERFORM UNTIL PR-EOF
              OR PR-ACCT-NO >= TR-ACCT-NO
              OR HARD-ERROR
              DISPLAY "PR ONLY ACCT=" PR-ACCT-NO
              PERFORM 2200-READ-PR
           END-PERFORM
           IF PR-EOF
              DISPLAY "NO PR FOR TX ACCT=" TR-ACCT-NO
              PERFORM 7000-WRITE-AUDIT
           ELSE
              IF PR-ACCT-NO NOT = TR-ACCT-NO
                 DISPLAY "NO PR FOR TX ACCT=" TR-ACCT-NO
                 PERFORM 7000-WRITE-AUDIT
              END-IF
           END-IF.
      *
       3200-VALIDATE-AND-ADD.
           IF PR-EOF OR PR-ACCT-NO NOT = TR-ACCT-NO
              ADD 1 TO WS-EXCLUDE-COUNT
              ADD TR-LOCAL-TAX-AMT TO WS-EXCLUDE-LOCAL-TAX
              EXIT PARAGRAPH
           END-IF
           IF PR-PAYEE-ADDR-CD(1:2) NOT NUMERIC
              DISPLAY "BAD ADDR ACCT=" TR-ACCT-NO
              PERFORM 7000-WRITE-AUDIT
              ADD 1 TO WS-EXCLUDE-COUNT
              ADD TR-LOCAL-TAX-AMT TO WS-EXCLUDE-LOCAL-TAX
              EXIT PARAGRAPH
           END-IF
           MOVE PR-PAYEE-ADDR-CD(1:2) TO WS-PREF-CD
           IF WS-PREF-CD < 1 OR WS-PREF-CD > 47
              DISPLAY "BAD PREF ACCT=" TR-ACCT-NO
              PERFORM 7000-WRITE-AUDIT
              ADD 1 TO WS-EXCLUDE-COUNT
              ADD TR-LOCAL-TAX-AMT TO WS-EXCLUDE-LOCAL-TAX
              EXIT PARAGRAPH
           END-IF
           IF PR-STATEMENT-CLASS(1:2) NUMERIC
              MOVE PR-STATEMENT-CLASS(1:2) TO WS-STMT-PREF-CD
              IF WS-STMT-PREF-CD NOT = WS-PREF-CD
                 DISPLAY "PREF MISMATCH ACCT=" TR-ACCT-NO
                 PERFORM 7000-WRITE-AUDIT
                 ADD 1 TO WS-EXCLUDE-COUNT
                 ADD TR-LOCAL-TAX-AMT TO WS-EXCLUDE-LOCAL-TAX
                 EXIT PARAGRAPH
              END-IF
           END-IF
           EVALUATE TR-ACCT-TYPE
              WHEN "01"
                 MOVE 1 TO WS-ACCT-IDX
              WHEN "02"
                 MOVE 2 TO WS-ACCT-IDX
              WHEN "03"
                 MOVE 3 TO WS-ACCT-IDX
              WHEN OTHER
                 DISPLAY "BAD TYPE ACCT=" TR-ACCT-NO
                 PERFORM 7000-WRITE-AUDIT
                 ADD 1 TO WS-EXCLUDE-COUNT
                 ADD TR-LOCAL-TAX-AMT TO WS-EXCLUDE-LOCAL-TAX
                 EXIT PARAGRAPH
           END-EVALUATE
           IF TR-GROSS-INT-AMT NOT = PR-GROSS-INT-AMT
              DISPLAY "GROSS MISMATCH ACCT=" TR-ACCT-NO
              PERFORM 7000-WRITE-AUDIT
              ADD 1 TO WS-EXCLUDE-COUNT
              ADD TR-LOCAL-TAX-AMT TO WS-EXCLUDE-LOCAL-TAX
              EXIT PARAGRAPH
           END-IF
           IF TR-LOCAL-TAX-AMT NOT = PR-LOCAL-TAX-AMT
              DISPLAY "LOCAL MISMATCH ACCT=" TR-ACCT-NO
              PERFORM 7000-WRITE-AUDIT
              ADD 1 TO WS-EXCLUDE-COUNT
              ADD TR-LOCAL-TAX-AMT TO WS-EXCLUDE-LOCAL-TAX
              EXIT PARAGRAPH
           END-IF
           COMPUTE WS-TOTAL-TAX =
              TR-NATIONAL-TAX-AMT + TR-LOCAL-TAX-AMT
           IF WS-TOTAL-TAX NOT = TR-TOTAL-TAX-AMT
              DISPLAY "TOTAL MISMATCH ACCT=" TR-ACCT-NO
              PERFORM 7000-WRITE-AUDIT
              ADD 1 TO WS-EXCLUDE-COUNT
              ADD TR-LOCAL-TAX-AMT TO WS-EXCLUDE-LOCAL-TAX
              EXIT PARAGRAPH
           END-IF
           COMPUTE WS-TABLE-IDX = ((WS-PREF-CD - 1) * 3)
                                  + WS-ACCT-IDX
           ADD TR-LOCAL-TAX-AMT TO WS-AGG-LOCAL-TAX(WS-TABLE-IDX)
           ADD 1 TO WS-AGG-COUNT(WS-TABLE-IDX)
           ADD TR-LOCAL-TAX-AMT TO WS-TOTAL-LOCAL-TAX.
      *
       4000-WRITE-SUMMARY.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 141
              IF WS-AGG-COUNT(WS-I) > 0
                 INITIALIZE KZLCRF-REC
                 ADD 1 TO WS-BANK-SEQ
                 MOVE WS-AGG-PREF-CD(WS-I) TO LC-PREFECTURE-CD
                 MOVE WS-REMITTANCE-DATE TO LC-REMITTANCE-DATE
                 MOVE WS-AGG-ACCT-TYPE(WS-I) TO LC-ACCT-TYPE
                 MOVE WS-AGG-LOCAL-TAX(WS-I) TO LC-LOCAL-TAX-AMT
                 MOVE WS-AGG-COUNT(WS-I) TO LC-RECORD-COUNT
                 INITIALIZE WS-BANK-REF-NO
                 STRING "KZ742B" WS-RUN-DATE WS-BANK-SEQ
                    DELIMITED BY SIZE INTO WS-BANK-REF-NO
                 END-STRING
                 MOVE WS-BANK-REF-NO TO LC-BANK-REF-NO
                 WRITE KZLCRF-REC
                 IF WS-KZLCRF-ST NOT = "00"
                    DISPLAY "KZLCRF WRITE ST=" WS-KZLCRF-ST
                    SET HARD-ERROR TO TRUE
                    EXIT PERFORM
                 END-IF
                 ADD 1 TO WS-OUT-COUNT
              END-IF
           END-PERFORM.
      *
       7000-WRITE-AUDIT.
           INITIALIZE KZADLF-REC
           ADD 1 TO WS-AUDIT-SEQ
           INITIALIZE WS-AUDIT-ID
           STRING "KZ742B" WS-RUN-DATE WS-AUDIT-SEQ
              DELIMITED BY SIZE INTO WS-AUDIT-ID
           END-STRING
           MOVE WS-AUDIT-ID TO AL-AUDIT-ID
           MOVE WS-RUN-DATE TO AL-RUN-DATE
           MOVE "KZTXRF" TO AL-SOURCE-DSID
           MOVE TR-LOCAL-TAX-AMT TO AL-DEBIT-AMT
           MOVE ZERO TO AL-CREDIT-AMT
           MOVE TR-LOCAL-TAX-AMT TO AL-DIFF-AMT
           MOVE "E" TO AL-STATUS-CD
           WRITE KZADLF-REC
           IF WS-KZADLF-ST NOT = "00"
              DISPLAY "KZADLF WRITE ST=" WS-KZADLF-ST
              SET HARD-ERROR TO TRUE
           END-IF.
      *
       9000-CLOSE-FILES.
           CLOSE KZTXRF
           IF WS-KZTXRF-ST NOT = "00" AND WS-KZTXRF-ST NOT = "42"
              DISPLAY "KZTXRF CLOSE ST=" WS-KZTXRF-ST
              SET HARD-ERROR TO TRUE
           END-IF
           CLOSE KZPRIF
           IF WS-KZPRIF-ST NOT = "00" AND WS-KZPRIF-ST NOT = "42"
              DISPLAY "KZPRIF CLOSE ST=" WS-KZPRIF-ST
              SET HARD-ERROR TO TRUE
           END-IF
           CLOSE KZLCRF
           IF WS-KZLCRF-ST NOT = "00" AND WS-KZLCRF-ST NOT = "42"
              DISPLAY "KZLCRF CLOSE ST=" WS-KZLCRF-ST
              SET HARD-ERROR TO TRUE
           END-IF
           CLOSE KZADLF
           IF WS-KZADLF-ST NOT = "00" AND WS-KZADLF-ST NOT = "42"
              DISPLAY "KZADLF CLOSE ST=" WS-KZADLF-ST
              SET HARD-ERROR TO TRUE
           END-IF.
