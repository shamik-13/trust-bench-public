       IDENTIFICATION DIVISION.
       PROGRAM-ID. KZ318B.
      * 変更履歴
      * 版数  年月日(和暦)  担当                      概要
      * 1.00  令和05.12.18  システム部 勘定系チーム  新規作成
      * 1.01  令和06.01.22  システム部 勘定系チーム  NISA口座更新判定追加
      * 1.02  令和06.04.08  システム部 勘定系チーム  日次保守ログ出力改善
       AUTHOR. KZB01.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZNIMF ASSIGN TO "KZNIMF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS NI-ACCT-NO
               FILE STATUS IS WS-KZNIMF-ST.

           SELECT KZTXIF ASSIGN TO "KZTXIF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-KZTXIF-ST.

           SELECT KZADLF ASSIGN TO "KZADLF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS AL-AUDIT-ID
               FILE STATUS IS WS-KZADLF-ST.

       DATA DIVISION.
       FILE SECTION.
       FD  KZNIMF.
           COPY KZNIMFC.

       FD  KZTXIF.
           COPY KZTXIFC.

       FD  KZADLF.
           COPY KZADLFC.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05 WS-KZNIMF-ST       PIC XX VALUE SPACES.
           05 WS-KZTXIF-ST       PIC XX VALUE SPACES.
           05 WS-KZADLF-ST       PIC XX VALUE SPACES.

       01  WS-SWITCHES.
           05 WS-MASTER-EOF      PIC X VALUE 'N'.
              88 MASTER-EOF            VALUE 'Y'.
           05 WS-TX-EOF          PIC X VALUE 'N'.
              88 TX-EOF                VALUE 'Y'.
           05 WS-HARD-ERROR      PIC X VALUE 'N'.
              88 HARD-ERROR            VALUE 'Y'.
           05 WS-FOUND-PAY       PIC X VALUE 'N'.
              88 FOUND-PAY             VALUE 'Y'.
           05 WS-HAVE-PREV       PIC X VALUE 'N'.
              88 HAVE-PREV             VALUE 'Y'.
           05 WS-VALID-RECORD    PIC X VALUE 'N'.
              88 VALID-RECORD          VALUE 'Y'.
           05 WS-STATUS-OK       PIC X VALUE 'N'.
              88 STATUS-OK             VALUE 'Y'.

       01  WS-RUN-CONTROL.
           05 WS-CURRENT-DATE    PIC X(21).
           05 WS-RUN-DATE        PIC 9(08).
           05 WS-RUN-DATE-X REDEFINES WS-RUN-DATE PIC X(08).
           05 WS-RUN-TIME        PIC 9(06).
           05 WS-AUDIT-SEQ       PIC 9(08) VALUE ZERO.
           05 WS-AUDIT-SEQ-X REDEFINES WS-AUDIT-SEQ PIC X(08).

       01  WS-COUNTERS.
           05 WS-NI-READ-CNT     PIC 9(09) VALUE ZERO.
           05 WS-TI-READ-CNT     PIC 9(09) VALUE ZERO.
           05 WS-NI-REWRITE-CNT  PIC 9(09) VALUE ZERO.
           05 WS-AUDIT-WRITE-CNT PIC 9(09) VALUE ZERO.
           05 WS-OVERLAP-CNT     PIC 9(09) VALUE ZERO.
           05 WS-NOTFOUND-CNT    PIC 9(09) VALUE ZERO.
           05 WS-INVALID-CNT     PIC 9(09) VALUE ZERO.

       01  WS-PREV-MASTER.
           05 WS-PREV-ACCT-NO    PIC X(16) VALUE LOW-VALUE.
           05 WS-PREV-FROM       PIC 9(08) VALUE ZERO.
           05 WS-PREV-TO         PIC 9(08) VALUE ZERO.

       01  WS-CHECK-AREA.
           05 WS-REASON-CD       PIC X(02) VALUE SPACES.

       01  WS-CONSTANTS.
           05 WS-DSID-KZNIMF     PIC X(08) VALUE 'KZNIMF  '.
           05 WS-DSID-KZTXIF     PIC X(08) VALUE 'KZTXIF  '.
           05 WS-ST-NORMAL       PIC X(02) VALUE '00'.
           05 WS-ST-EOF          PIC X(02) VALUE '10'.
           05 WS-ACCT-TYPE-NISA  PIC X(02) VALUE '03'.
           05 WS-AUDIT-OK        PIC X(02) VALUE '00'.
           05 WS-AUDIT-WARN      PIC X(02) VALUE '04'.

       PROCEDURE DIVISION.
       MAIN-RTN.
           PERFORM 1000-INITIALIZE
           IF NOT HARD-ERROR
               PERFORM 2000-SCAN-NISA-MASTER
           END-IF
           IF NOT HARD-ERROR
               PERFORM 3000-MATCH-PAYMENT
           END-IF
           PERFORM 9000-FINALIZE
           GOBACK
           .

       1000-INITIALIZE.
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE
           MOVE WS-CURRENT-DATE(1:8) TO WS-RUN-DATE-X
           MOVE WS-CURRENT-DATE(9:6) TO WS-RUN-TIME
           MOVE 0 TO RETURN-CODE

           OPEN I-O KZNIMF
           IF WS-KZNIMF-ST NOT = WS-ST-NORMAL
               DISPLAY 'KZNIMF OPEN ERR ST=' WS-KZNIMF-ST
               MOVE 'Y' TO WS-HARD-ERROR
               MOVE 12 TO RETURN-CODE
           END-IF

           IF NOT HARD-ERROR
               OPEN INPUT KZTXIF
               IF WS-KZTXIF-ST NOT = WS-ST-NORMAL
                   DISPLAY 'KZTXIF OPEN ERR ST=' WS-KZTXIF-ST
                   MOVE 'Y' TO WS-HARD-ERROR
                   MOVE 12 TO RETURN-CODE
               END-IF
           END-IF

           IF NOT HARD-ERROR
               OPEN OUTPUT KZADLF
               IF WS-KZADLF-ST NOT = WS-ST-NORMAL
                   DISPLAY 'KZADLF OPEN ERR ST=' WS-KZADLF-ST
                   MOVE 'Y' TO WS-HARD-ERROR
                   MOVE 12 TO RETURN-CODE
               END-IF
           END-IF
           .

       2000-SCAN-NISA-MASTER.
           MOVE LOW-VALUE TO NI-ACCT-NO
           START KZNIMF KEY IS GREATER THAN NI-ACCT-NO
               INVALID KEY
                   MOVE 'Y' TO WS-MASTER-EOF
               NOT INVALID KEY
                   CONTINUE
           END-START

           PERFORM UNTIL MASTER-EOF OR HARD-ERROR
               READ KZNIMF NEXT RECORD
                   AT END
                       MOVE 'Y' TO WS-MASTER-EOF
                   NOT AT END
                       ADD 1 TO WS-NI-READ-CNT
                       PERFORM 2100-CHECK-MASTER-RECORD
               END-READ
           END-PERFORM
           .

       2100-CHECK-MASTER-RECORD.
           MOVE 'N' TO WS-VALID-RECORD
           MOVE SPACES TO WS-REASON-CD

           IF NI-ACCT-NO = SPACES
               MOVE '01' TO WS-REASON-CD
               ADD 1 TO WS-INVALID-CNT
               PERFORM 5000-WRITE-AUDIT
           ELSE
               IF NI-NISA-APPLY-FROM = ZERO
                  OR NI-NISA-APPLY-TO = ZERO
                  OR NI-NISA-APPLY-FROM > NI-NISA-APPLY-TO
                   MOVE '02' TO WS-REASON-CD
                   ADD 1 TO WS-INVALID-CNT
                   PERFORM 5000-WRITE-AUDIT
               ELSE
                   IF NI-TAX-ID = SPACES
                       MOVE '03' TO WS-REASON-CD
                       ADD 1 TO WS-INVALID-CNT
                       PERFORM 5000-WRITE-AUDIT
                   ELSE
                       IF NI-STATUS-CD NOT = '0'
                          AND NI-STATUS-CD NOT = '1'
                          AND NI-STATUS-CD NOT = '9'
                           DISPLAY 'BAD NI STATUS ACCT=' NI-ACCT-NO
                           MOVE '04' TO WS-REASON-CD
                           ADD 1 TO WS-INVALID-CNT
                           PERFORM 5000-WRITE-AUDIT
                       ELSE
                           SET VALID-RECORD TO TRUE
                       END-IF
                   END-IF
               END-IF
           END-IF

           IF VALID-RECORD
               IF HAVE-PREV
                  AND NI-ACCT-NO = WS-PREV-ACCT-NO
                  AND NI-NISA-APPLY-FROM <= WS-PREV-TO
                  AND NI-NISA-APPLY-TO >= WS-PREV-FROM
                   DISPLAY 'NISA OVERLAP ACCT=' NI-ACCT-NO
                   MOVE '05' TO WS-REASON-CD
                   ADD 1 TO WS-OVERLAP-CNT
                   PERFORM 5000-WRITE-AUDIT
               END-IF
               MOVE NI-ACCT-NO TO WS-PREV-ACCT-NO
               MOVE NI-NISA-APPLY-FROM TO WS-PREV-FROM
               MOVE NI-NISA-APPLY-TO TO WS-PREV-TO
               MOVE 'Y' TO WS-HAVE-PREV
           END-IF
           .

       3000-MATCH-PAYMENT.
           PERFORM UNTIL TX-EOF OR HARD-ERROR
               READ KZTXIF
                   AT END
                       MOVE 'Y' TO WS-TX-EOF
                   NOT AT END
                       ADD 1 TO WS-TI-READ-CNT
                       PERFORM 3100-PROCESS-PAYMENT
               END-READ
           END-PERFORM
           .

       3100-PROCESS-PAYMENT.
           MOVE 'N' TO WS-FOUND-PAY
           MOVE SPACES TO WS-REASON-CD

           IF TI-ACCT-NO = SPACES
               MOVE '11' TO WS-REASON-CD
               ADD 1 TO WS-INVALID-CNT
               PERFORM 5200-WRITE-TX-AUDIT
           ELSE
               IF TI-ACCT-TYPE NOT = '01'
                  AND TI-ACCT-TYPE NOT = '02'
                  AND TI-ACCT-TYPE NOT = '03'
                   DISPLAY 'BAD TI TYPE ACCT=' TI-ACCT-NO
                   MOVE '12' TO WS-REASON-CD
                   ADD 1 TO WS-INVALID-CNT
                   PERFORM 5200-WRITE-TX-AUDIT
               ELSE
                   IF TI-ACCT-TYPE = WS-ACCT-TYPE-NISA
                       PERFORM 3200-READ-AND-MARK-NISA
                   END-IF
               END-IF
           END-IF
           .

       3200-READ-AND-MARK-NISA.
           MOVE TI-ACCT-NO TO NI-ACCT-NO
           READ KZNIMF RECORD
               INVALID KEY
                   MOVE '13' TO WS-REASON-CD
                   ADD 1 TO WS-NOTFOUND-CNT
                   PERFORM 5200-WRITE-TX-AUDIT
               NOT INVALID KEY
                   SET FOUND-PAY TO TRUE
                   PERFORM 3300-CHECK-EFFECTIVE
           END-READ
           .

       3300-CHECK-EFFECTIVE.
           MOVE 'N' TO WS-STATUS-OK

           IF NI-STATUS-CD = '0'
              OR NI-STATUS-CD = '1'
               SET STATUS-OK TO TRUE
           ELSE
               MOVE '14' TO WS-REASON-CD
               ADD 1 TO WS-INVALID-CNT
           END-IF

           IF STATUS-OK
              AND WS-RUN-DATE >= NI-NISA-APPLY-FROM
              AND WS-RUN-DATE <= NI-NISA-APPLY-TO
               MOVE WS-RUN-DATE TO NI-LAST-MAINT-DATE
               REWRITE KZNIMF-REC
                   INVALID KEY
                       DISPLAY 'KZNIMF REWRITE ERR ACCT=' NI-ACCT-NO
                       DISPLAY 'KZNIMF REWRITE ERR ST=' WS-KZNIMF-ST
                       MOVE 'Y' TO WS-HARD-ERROR
                       MOVE 12 TO RETURN-CODE
                   NOT INVALID KEY
                       ADD 1 TO WS-NI-REWRITE-CNT
                       MOVE '00' TO WS-REASON-CD
                       PERFORM 5200-WRITE-TX-AUDIT
               END-REWRITE
           ELSE
               IF WS-REASON-CD = SPACES
                   MOVE '15' TO WS-REASON-CD
                   ADD 1 TO WS-INVALID-CNT
               END-IF
               PERFORM 5200-WRITE-TX-AUDIT
           END-IF
           .

       5000-WRITE-AUDIT.
           ADD 1 TO WS-AUDIT-SEQ
           INITIALIZE KZADLF-REC
           STRING 'KZ318B' WS-RUN-DATE-X WS-AUDIT-SEQ-X
               DELIMITED BY SIZE INTO AL-AUDIT-ID
           END-STRING
           MOVE WS-RUN-DATE TO AL-RUN-DATE
           MOVE WS-DSID-KZNIMF TO AL-SOURCE-DSID
           MOVE ZERO TO AL-DEBIT-AMT
           MOVE ZERO TO AL-CREDIT-AMT
           MOVE ZERO TO AL-DIFF-AMT
           IF WS-REASON-CD = '00'
               MOVE WS-AUDIT-OK TO AL-STATUS-CD
           ELSE
               MOVE WS-AUDIT-WARN TO AL-STATUS-CD
           END-IF
           WRITE KZADLF-REC
               INVALID KEY
                   DISPLAY 'KZADLF WRITE ERR ST=' WS-KZADLF-ST
                   MOVE 'Y' TO WS-HARD-ERROR
                   MOVE 12 TO RETURN-CODE
               NOT INVALID KEY
                   ADD 1 TO WS-AUDIT-WRITE-CNT
           END-WRITE
           .

       5200-WRITE-TX-AUDIT.
           ADD 1 TO WS-AUDIT-SEQ
           INITIALIZE KZADLF-REC
           STRING 'KZ318B' WS-RUN-DATE-X WS-AUDIT-SEQ-X
               DELIMITED BY SIZE INTO AL-AUDIT-ID
           END-STRING
           MOVE WS-RUN-DATE TO AL-RUN-DATE
           MOVE WS-DSID-KZTXIF TO AL-SOURCE-DSID
           MOVE ZERO TO AL-DEBIT-AMT
           MOVE TI-INT-AMT TO AL-CREDIT-AMT
           MOVE TI-INT-AMT TO AL-DIFF-AMT
           IF WS-REASON-CD = '00'
               MOVE WS-AUDIT-OK TO AL-STATUS-CD
           ELSE
               MOVE WS-AUDIT-WARN TO AL-STATUS-CD
           END-IF
           WRITE KZADLF-REC
               INVALID KEY
                   DISPLAY 'KZADLF WRITE ERR ST=' WS-KZADLF-ST
                   MOVE 'Y' TO WS-HARD-ERROR
                   MOVE 12 TO RETURN-CODE
               NOT INVALID KEY
                   ADD 1 TO WS-AUDIT-WRITE-CNT
           END-WRITE
           .

       9000-FINALIZE.
           IF WS-KZADLF-ST NOT = SPACES
               CLOSE KZADLF
               IF WS-KZADLF-ST NOT = WS-ST-NORMAL
                   DISPLAY 'KZADLF CLOSE ERR ST=' WS-KZADLF-ST
                   MOVE 'Y' TO WS-HARD-ERROR
                   MOVE 8 TO RETURN-CODE
               END-IF
           END-IF

           IF WS-KZTXIF-ST NOT = SPACES
               CLOSE KZTXIF
               IF WS-KZTXIF-ST NOT = WS-ST-NORMAL
                   DISPLAY 'KZTXIF CLOSE ERR ST=' WS-KZTXIF-ST
                   MOVE 'Y' TO WS-HARD-ERROR
                   MOVE 8 TO RETURN-CODE
               END-IF
           END-IF

           IF WS-KZNIMF-ST NOT = SPACES
               CLOSE KZNIMF
               IF WS-KZNIMF-ST NOT = WS-ST-NORMAL
                   DISPLAY 'KZNIMF CLOSE ERR ST=' WS-KZNIMF-ST
                   MOVE 'Y' TO WS-HARD-ERROR
                   MOVE 8 TO RETURN-CODE
               END-IF
           END-IF

           DISPLAY 'KZ318B END DATE=' WS-RUN-DATE-X
           DISPLAY 'NI READ COUNT=' WS-NI-READ-CNT
           DISPLAY 'TI READ COUNT=' WS-TI-READ-CNT
           DISPLAY 'NI REWRITE COUNT=' WS-NI-REWRITE-CNT
           DISPLAY 'AUDIT WRITE COUNT=' WS-AUDIT-WRITE-CNT
           DISPLAY 'OVERLAP COUNT=' WS-OVERLAP-CNT
           DISPLAY 'NOT FOUND COUNT=' WS-NOTFOUND-CNT
           DISPLAY 'INVALID COUNT=' WS-INVALID-CNT

           IF HARD-ERROR
               IF RETURN-CODE = 0
                   MOVE 8 TO RETURN-CODE
               END-IF
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF
           .
