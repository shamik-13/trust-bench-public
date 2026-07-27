       IDENTIFICATION DIVISION.
       PROGRAM-ID. KZ537B.
      * 変更履歴
      * 版数  年月日(和暦)   担当                         概要
      * 1.00  平成30年04月01日 システム部 勘定系チーム     新規作成
      * 1.10  令和02年10月01日 システム部 勘定系チーム     税務署納付先判定追加
      * 1.20  令和05年01月04日 システム部 勘定系チーム     納付ファイル項目見直し
       AUTHOR. KZ-BATCH.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZTXRF ASSIGN TO "KZTXRF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-KZTXRF-ST.
           SELECT KZPRIF ASSIGN TO "KZPRIF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-KZPRIF-ST.
           SELECT KZPMRF ASSIGN TO "KZPMRF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-KZPMRF-ST.
           SELECT KZADLF ASSIGN TO "KZADLF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-KZADLF-ST.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  KZTXRF.
       COPY KZTXRFC.
      *
       FD  KZPRIF.
       COPY KZPRIFC.
      *
       FD  KZPMRF.
       COPY KZPMRFC.
      *
       FD  KZADLF.
       COPY KZADLFC.
      *
       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05 WS-KZTXRF-ST             PIC XX VALUE SPACE.
           05 WS-KZPRIF-ST             PIC XX VALUE SPACE.
           05 WS-KZPMRF-ST             PIC XX VALUE SPACE.
           05 WS-KZADLF-ST             PIC XX VALUE SPACE.
      *
       01  WS-END-SW.
           05 WS-TX-END-SW             PIC X VALUE "N".
              88 TX-END                VALUE "Y".
              88 TX-NOT-END            VALUE "N".
           05 WS-PR-END-SW             PIC X VALUE "N".
              88 PR-END                VALUE "Y".
              88 PR-NOT-END            VALUE "N".
      *
       01  WS-RUN-CTL.
           05 WS-RUN-DATE              PIC 9(08) VALUE ZERO.
           05 WS-ABEND-SW              PIC X VALUE "N".
              88 ABEND-ON              VALUE "Y".
              88 ABEND-OFF             VALUE "N".
      *
       01  WS-CURRENT-GROUP.
           05 WS-CUR-TAX-OFFICE-CD     PIC X(05) VALUE SPACE.
           05 WS-CUR-PAYMENT-DATE      PIC 9(08) VALUE ZERO.
           05 WS-CUR-NATIONAL-TAX      PIC S9(13) VALUE ZERO.
           05 WS-CUR-RECON-AMT         PIC S9(13) VALUE ZERO.
           05 WS-CUR-DETAIL-COUNT      PIC 9(09) VALUE ZERO.
      *
       01  WS-WORK.
           05 WS-TAX-OFFICE-CD         PIC X(05) VALUE SPACE.
           05 WS-CALC-NATIONAL-TAX     PIC S9(13) VALUE ZERO.
           05 WS-CALC-LOCAL-TAX        PIC S9(13) VALUE ZERO.
           05 WS-CALC-TOTAL-TAX        PIC S9(13) VALUE ZERO.
           05 WS-DIFF-AMT              PIC S9(13) VALUE ZERO.
           05 WS-RATE-NATIONAL         PIC 9V9999 VALUE ZERO.
           05 WS-RATE-LOCAL            PIC 9V9999 VALUE ZERO.
           05 WS-PAYMENT-REF-SEQ       PIC 9(07) VALUE ZERO.
           05 WS-AUDIT-SEQ             PIC 9(09) VALUE ZERO.
           05 WS-SOURCE-DSID           PIC X(08) VALUE SPACE.
           05 WS-VALID-SW              PIC X VALUE "Y".
              88 VALID-REC             VALUE "Y".
              88 INVALID-REC           VALUE "N".
      *
       01  WS-COUNTERS.
           05 WS-TX-READ-CNT           PIC 9(09) VALUE ZERO.
           05 WS-PR-READ-CNT           PIC 9(09) VALUE ZERO.
           05 WS-PM-WRITE-CNT          PIC 9(09) VALUE ZERO.
           05 WS-AL-WRITE-CNT          PIC 9(09) VALUE ZERO.
           05 WS-MATCH-CNT             PIC 9(09) VALUE ZERO.
           05 WS-UNMATCH-TX-CNT        PIC 9(09) VALUE ZERO.
           05 WS-UNMATCH-PR-CNT        PIC 9(09) VALUE ZERO.
      *
       01  WS-DISPLAY-NUM.
           05 WS-DISP-CNT              PIC Z(9).
      *
       PROCEDURE DIVISION.
       MAIN-RTN.
           PERFORM 0000-INIT
           IF ABEND-OFF
               PERFORM 1000-MAIN-PROCESS
           END-IF
           PERFORM 9000-TERM
           GOBACK.
      *
       0000-INIT.
           MOVE FUNCTION CURRENT-DATE(1:8) TO WS-RUN-DATE
           DISPLAY "KZ537B START"
      *
           OPEN INPUT KZTXRF
           IF WS-KZTXRF-ST NOT = "00"
               DISPLAY "KZTXRF OPEN ERR ST=" WS-KZTXRF-ST
               MOVE "Y" TO WS-ABEND-SW
               MOVE 12 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF
      *
           OPEN INPUT KZPRIF
           IF WS-KZPRIF-ST NOT = "00"
               DISPLAY "KZPRIF OPEN ERR ST=" WS-KZPRIF-ST
               MOVE "Y" TO WS-ABEND-SW
               MOVE 12 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF
      *
           OPEN OUTPUT KZPMRF
           IF WS-KZPMRF-ST NOT = "00"
               DISPLAY "KZPMRF OPEN ERR ST=" WS-KZPMRF-ST
               MOVE "Y" TO WS-ABEND-SW
               MOVE 12 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF
      *
           OPEN OUTPUT KZADLF
           IF WS-KZADLF-ST NOT = "00"
               DISPLAY "KZADLF OPEN ERR ST=" WS-KZADLF-ST
               MOVE "Y" TO WS-ABEND-SW
               MOVE 12 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF
      *
           PERFORM 1100-READ-TX
           PERFORM 1200-READ-PR.
      *
       1000-MAIN-PROCESS.
           PERFORM UNTIL (TX-END AND PR-END) OR ABEND-ON
               EVALUATE TRUE
                   WHEN TX-END
                       PERFORM 2300-PR-ONLY
                   WHEN PR-END
                       PERFORM 2200-TX-ONLY
                   WHEN TR-ACCT-NO = PR-ACCT-NO
                       PERFORM 2100-MATCHED
                   WHEN TR-ACCT-NO < PR-ACCT-NO
                       PERFORM 2200-TX-ONLY
                   WHEN OTHER
                       PERFORM 2300-PR-ONLY
               END-EVALUATE
           END-PERFORM
      *
           IF ABEND-OFF
               PERFORM 3300-WRITE-SUMMARY
           END-IF.
      *
       1100-READ-TX.
           READ KZTXRF
               AT END
                   SET TX-END TO TRUE
               NOT AT END
                   ADD 1 TO WS-TX-READ-CNT
           END-READ
           IF WS-KZTXRF-ST NOT = "00" AND WS-KZTXRF-ST NOT = "10"
               DISPLAY "KZTXRF READ ERR ST=" WS-KZTXRF-ST
               MOVE "Y" TO WS-ABEND-SW
               MOVE 12 TO RETURN-CODE
           END-IF.
      *
       1200-READ-PR.
           READ KZPRIF
               AT END
                   SET PR-END TO TRUE
               NOT AT END
                   ADD 1 TO WS-PR-READ-CNT
           END-READ
           IF WS-KZPRIF-ST NOT = "00" AND WS-KZPRIF-ST NOT = "10"
               DISPLAY "KZPRIF READ ERR ST=" WS-KZPRIF-ST
               MOVE "Y" TO WS-ABEND-SW
               MOVE 12 TO RETURN-CODE
           END-IF.
      *
       2100-MATCHED.
           ADD 1 TO WS-MATCH-CNT
           PERFORM 3100-VALIDATE-MATCH
           IF VALID-REC
               PERFORM 3200-CALC-TAX
               COMPUTE WS-DIFF-AMT =
                   TR-NATIONAL-TAX-AMT - WS-CALC-NATIONAL-TAX
               PERFORM 3400-ADD-DETAIL
               IF WS-DIFF-AMT NOT = ZERO
                   MOVE "KZTXRF" TO WS-SOURCE-DSID
                   PERFORM 4100-WRITE-AUDIT
               END-IF
           END-IF
           PERFORM 1100-READ-TX
           PERFORM 1200-READ-PR.
      *
       2200-TX-ONLY.
           ADD 1 TO WS-UNMATCH-TX-CNT
           MOVE TR-NATIONAL-TAX-AMT TO WS-DIFF-AMT
           MOVE "KZTXRF" TO WS-SOURCE-DSID
           PERFORM 4100-WRITE-AUDIT
           PERFORM 1100-READ-TX.
      *
       2300-PR-ONLY.
           ADD 1 TO WS-UNMATCH-PR-CNT
           MOVE PR-NATIONAL-TAX-AMT TO WS-DIFF-AMT
           MOVE "KZPRIF" TO WS-SOURCE-DSID
           PERFORM 4100-WRITE-AUDIT
           PERFORM 1200-READ-PR.
      *
       3100-VALIDATE-MATCH.
           SET VALID-REC TO TRUE
           IF TR-ACCT-TYPE NOT = "01"
              AND TR-ACCT-TYPE NOT = "02"
              AND TR-ACCT-TYPE NOT = "03"
               DISPLAY "ACCT TYPE ERR ACCT=" TR-ACCT-NO
               SET INVALID-REC TO TRUE
           END-IF
      *
           IF PR-PAYMENT-DATE = ZERO
               DISPLAY "PAY DATE ERR ACCT=" PR-ACCT-NO
               SET INVALID-REC TO TRUE
           END-IF
      *
           IF PR-PAYEE-ADDR-CD = SPACE
               DISPLAY "ADDR CD ERR ACCT=" PR-ACCT-NO
               SET INVALID-REC TO TRUE
           END-IF
      *
           IF TR-GROSS-INT-AMT NOT = PR-GROSS-INT-AMT
               DISPLAY "AMT UNMATCH ACCT=" PR-ACCT-NO
               SET INVALID-REC TO TRUE
           END-IF
      *
           IF INVALID-REC
               MOVE PR-NATIONAL-TAX-AMT TO WS-DIFF-AMT
               MOVE "KZPRIF" TO WS-SOURCE-DSID
               PERFORM 4100-WRITE-AUDIT
           END-IF.
      *
       3200-CALC-TAX.
           MOVE ZERO TO WS-RATE-NATIONAL
                        WS-RATE-LOCAL
                        WS-CALC-NATIONAL-TAX
                        WS-CALC-LOCAL-TAX
                        WS-CALC-TOTAL-TAX
      *
      *    納付額の照合は払込調書(KZPRIF)を基準とし、源泉徴収元帳
      *    (KZTXRF)との差異を突合する。税額の再計算は行わない。
           MOVE PR-NATIONAL-TAX-AMT TO WS-CALC-NATIONAL-TAX
           MOVE PR-LOCAL-TAX-AMT    TO WS-CALC-LOCAL-TAX
           COMPUTE WS-CALC-TOTAL-TAX =
               WS-CALC-NATIONAL-TAX + WS-CALC-LOCAL-TAX.
      *
       3300-WRITE-SUMMARY.
           IF WS-CUR-TAX-OFFICE-CD NOT = SPACE
               INITIALIZE KZPMRF-REC
               MOVE WS-CUR-TAX-OFFICE-CD TO PM-TAX-OFFICE-CD
               MOVE WS-CUR-PAYMENT-DATE TO PM-PAYMENT-DATE
               MOVE SPACE TO PM-ACCT-NO
               MOVE "00" TO PM-ACCT-TYPE
               MOVE WS-CUR-NATIONAL-TAX TO PM-NATIONAL-TAX-AMT
               MOVE WS-CUR-RECON-AMT TO PM-SPECIAL-RECON-AMT
               PERFORM 3500-MAKE-REF-NO
               WRITE KZPMRF-REC
               IF WS-KZPMRF-ST NOT = "00"
                   DISPLAY "KZPMRF SUM WRITE ERR ST=" WS-KZPMRF-ST
                   MOVE "Y" TO WS-ABEND-SW
                   MOVE 12 TO RETURN-CODE
               ELSE
                   ADD 1 TO WS-PM-WRITE-CNT
               END-IF
           END-IF.
      *
       3400-ADD-DETAIL.
           MOVE PR-PAYEE-ADDR-CD(1:5) TO WS-TAX-OFFICE-CD
           IF WS-CUR-TAX-OFFICE-CD = SPACE
               MOVE WS-TAX-OFFICE-CD TO WS-CUR-TAX-OFFICE-CD
               MOVE PR-PAYMENT-DATE TO WS-CUR-PAYMENT-DATE
           END-IF
      *
           IF WS-TAX-OFFICE-CD NOT = WS-CUR-TAX-OFFICE-CD
               PERFORM 3300-WRITE-SUMMARY
               MOVE WS-TAX-OFFICE-CD TO WS-CUR-TAX-OFFICE-CD
               MOVE PR-PAYMENT-DATE TO WS-CUR-PAYMENT-DATE
               MOVE ZERO TO WS-CUR-NATIONAL-TAX
                            WS-CUR-RECON-AMT
                            WS-CUR-DETAIL-COUNT
           END-IF
      *
           INITIALIZE KZPMRF-REC
           MOVE WS-TAX-OFFICE-CD TO PM-TAX-OFFICE-CD
           MOVE PR-PAYMENT-DATE TO PM-PAYMENT-DATE
           MOVE PR-ACCT-NO TO PM-ACCT-NO
           MOVE TR-ACCT-TYPE TO PM-ACCT-TYPE
           MOVE WS-CALC-NATIONAL-TAX TO PM-NATIONAL-TAX-AMT
           MOVE WS-DIFF-AMT TO PM-SPECIAL-RECON-AMT
           PERFORM 3500-MAKE-REF-NO
           WRITE KZPMRF-REC
           IF WS-KZPMRF-ST NOT = "00"
               DISPLAY "KZPMRF DET WRITE ERR ST=" WS-KZPMRF-ST
               MOVE "Y" TO WS-ABEND-SW
               MOVE 12 TO RETURN-CODE
           ELSE
               ADD 1 TO WS-PM-WRITE-CNT
               ADD 1 TO WS-CUR-DETAIL-COUNT
               ADD WS-CALC-NATIONAL-TAX TO WS-CUR-NATIONAL-TAX
               ADD WS-DIFF-AMT TO WS-CUR-RECON-AMT
           END-IF.
      *
       3500-MAKE-REF-NO.
           ADD 1 TO WS-PAYMENT-REF-SEQ
           MOVE WS-PAYMENT-REF-SEQ TO PM-PAYMENT-REF-NO.
      *
       4100-WRITE-AUDIT.
           INITIALIZE KZADLF-REC
           ADD 1 TO WS-AUDIT-SEQ
           MOVE WS-AUDIT-SEQ TO AL-AUDIT-ID
           MOVE WS-RUN-DATE TO AL-RUN-DATE
           MOVE WS-SOURCE-DSID TO AL-SOURCE-DSID
      *
           IF WS-DIFF-AMT >= ZERO
               MOVE WS-DIFF-AMT TO AL-DEBIT-AMT
               MOVE ZERO TO AL-CREDIT-AMT
           ELSE
               MOVE ZERO TO AL-DEBIT-AMT
               COMPUTE AL-CREDIT-AMT = WS-DIFF-AMT * -1
           END-IF
      *
           MOVE WS-DIFF-AMT TO AL-DIFF-AMT
           MOVE "01" TO AL-STATUS-CD
           WRITE KZADLF-REC
           IF WS-KZADLF-ST NOT = "00"
               DISPLAY "KZADLF WRITE ERR ST=" WS-KZADLF-ST
               MOVE "Y" TO WS-ABEND-SW
               MOVE 12 TO RETURN-CODE
           ELSE
               ADD 1 TO WS-AL-WRITE-CNT
           END-IF.
      *
       9000-TERM.
           IF WS-KZTXRF-ST NOT = SPACE
               CLOSE KZTXRF
           END-IF
           IF WS-KZPRIF-ST NOT = SPACE
               CLOSE KZPRIF
           END-IF
           IF WS-KZPMRF-ST NOT = SPACE
               CLOSE KZPMRF
           END-IF
           IF WS-KZADLF-ST NOT = SPACE
               CLOSE KZADLF
           END-IF
      *
           MOVE WS-TX-READ-CNT TO WS-DISP-CNT
           DISPLAY "KZTXRF READ COUNT=" WS-DISP-CNT
           MOVE WS-PR-READ-CNT TO WS-DISP-CNT
           DISPLAY "KZPRIF READ COUNT=" WS-DISP-CNT
           MOVE WS-MATCH-CNT TO WS-DISP-CNT
           DISPLAY "MATCH COUNT=" WS-DISP-CNT
           MOVE WS-PM-WRITE-CNT TO WS-DISP-CNT
           DISPLAY "KZPMRF WRITE COUNT=" WS-DISP-CNT
           MOVE WS-AL-WRITE-CNT TO WS-DISP-CNT
           DISPLAY "KZADLF WRITE COUNT=" WS-DISP-CNT
      *
           IF ABEND-ON
               DISPLAY "KZ537B ABEND"
               IF RETURN-CODE = ZERO
                   MOVE 8 TO RETURN-CODE
               END-IF
           ELSE
               DISPLAY "KZ537B END"
               MOVE 0 TO RETURN-CODE
           END-IF.
