       IDENTIFICATION DIVISION.
       PROGRAM-ID. JH610B.
      * 版数  年月日(和暦)  担当                       概要
      * 1.00  令和04年04月01日 システム部 情報系チーム BI連携初版
      * 1.01  令和05年10月16日 システム部 情報系チーム 引渡判定修正
      * 1.02  令和06年07月08日 システム部 情報系チーム 障害時再送対応
       AUTHOR. TRUST-BANK-BATCH.
      ******************************************************************
      * BI FILE HANDOFF BATCH
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT JHMARTF ASSIGN TO "JHMARTF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-ST-JHMARTF.

           SELECT JHMONRF ASSIGN TO "JHMONRF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-ST-JHMONRF.

           SELECT JHCTLKF ASSIGN TO "JHCTLKF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CT-JOB-ID
               FILE STATUS IS WS-ST-JHCTLKF.

           SELECT JHBIHOF ASSIGN TO "JHBIHOF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-ST-JHBIHOF.

           SELECT JHAUDTF ASSIGN TO "JHAUDTF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-ST-JHAUDTF.

       DATA DIVISION.
       FILE SECTION.

       FD  JHMARTF.
           COPY JHMRTC.

       FD  JHMONRF.
           COPY JHMONC.

       FD  JHCTLKF.
           COPY JHCTLC.

       FD  JHBIHOF.
           COPY JHBIOC.

       FD  JHAUDTF.
           COPY JHAUDC.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05 WS-ST-JHMARTF       PIC XX VALUE SPACES.
           05 WS-ST-JHMONRF       PIC XX VALUE SPACES.
           05 WS-ST-JHCTLKF       PIC XX VALUE SPACES.
           05 WS-ST-JHBIHOF       PIC XX VALUE SPACES.
           05 WS-ST-JHAUDTF       PIC XX VALUE SPACES.

       01  WS-SWITCHES.
           05 WS-MART-EOF         PIC X VALUE "N".
              88 MART-EOF               VALUE "Y".
              88 MART-NOT-EOF           VALUE "N".
           05 WS-MON-EOF          PIC X VALUE "N".
              88 MON-EOF                VALUE "Y".
              88 MON-NOT-EOF            VALUE "N".
           05 WS-HARD-ERROR       PIC X VALUE "N".
              88 HARD-ERROR             VALUE "Y".
              88 NO-HARD-ERROR          VALUE "N".

       01  WS-CONSTANTS.
           05 WS-JOB-ID           PIC X(08) VALUE "JH610B".
           05 WS-DS-MART          PIC X(08) VALUE "JHMARTF".
           05 WS-DS-MON           PIC X(08) VALUE "JHMONRF".
           05 WS-DS-BIO           PIC X(08) VALUE "JHBIHOF".
           05 WS-DS-CTL           PIC X(08) VALUE "JHCTLKF".
           05 WS-DS-AUD           PIC X(08) VALUE "JHAUDTF".
           05 WS-EV-START         PIC X(04) VALUE "STRT".
           05 WS-EV-END           PIC X(04) VALUE "END ".
           05 WS-EV-ERR           PIC X(04) VALUE "ERR ".
           05 WS-EV-SKIP          PIC X(04) VALUE "SKIP".
           05 WS-REC-HDR          PIC X VALUE "H".
           05 WS-REC-DTL          PIC X VALUE "D".
           05 WS-REC-TRL          PIC X VALUE "T".

       01  WS-DATE-TIME.
           05 WS-CURRENT-DT       PIC X(08) VALUE SPACES.
           05 WS-CURRENT-TM       PIC X(08) VALUE SPACES.
           05 WS-CURRENT-TS       PIC X(14) VALUE SPACES.
           05 WS-CDATE.
              10 WS-CD-YYYY       PIC 9(04).
              10 WS-CD-MM         PIC 9(02).
              10 WS-CD-DD         PIC 9(02).
              10 WS-CD-HH         PIC 9(02).
              10 WS-CD-MI         PIC 9(02).
              10 WS-CD-SS         PIC 9(02).
              10 WS-CD-CC         PIC 9(02).
              10 WS-CD-GMTO       PIC S9(04).

       01  WS-COUNTERS.
           05 WS-MART-IN-CNT      PIC 9(09) VALUE ZERO.
           05 WS-MON-IN-CNT       PIC 9(09) VALUE ZERO.
           05 WS-OUT-CNT          PIC 9(09) VALUE ZERO.
           05 WS-DETAIL-CNT       PIC 9(09) VALUE ZERO.
           05 WS-SKIP-CNT         PIC 9(09) VALUE ZERO.
           05 WS-AUDIT-SEQ        PIC 9(09) VALUE ZERO.

       01  WS-AMOUNTS.
           05 WS-DETAIL-AMT       PIC S9(15) VALUE ZERO.
           05 WS-WORK-AMT         PIC S9(15) VALUE ZERO.
           05 WS-ZERO-AMT         PIC S9(15) VALUE ZERO.

       01  WS-EDIT-WORK.
           05 WS-PAYLOAD          PIC X(256) VALUE SPACES.
           05 WS-PAYLOAD-LEN      PIC 9(04) VALUE ZERO.
           05 WS-MART-AMT-TXT     PIC -(14)9 VALUE ZERO.
           05 WS-MART-YTD-TXT     PIC -(14)9 VALUE ZERO.
           05 WS-MON-AMT-TXT      PIC -(14)9 VALUE ZERO.
           05 WS-MON-YTD-TXT      PIC -(14)9 VALUE ZERO.
           05 WS-MON-CNT-TXT      PIC Z(08)9 VALUE ZERO.
           05 WS-MON-ADJ-TXT      PIC Z(08)9 VALUE ZERO.

       01  WS-MESSAGES.
           05 WS-ERR-DATASET      PIC X(08) VALUE SPACES.
           05 WS-ERR-STATUS       PIC XX VALUE SPACES.
           05 WS-ERR-REASON       PIC X(40) VALUE SPACES.

       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-INITIALIZE
           PERFORM 2000-OPEN-FILES
           IF NO-HARD-ERROR
               PERFORM 3000-LOAD-CONTROL
           END-IF
           IF NO-HARD-ERROR
               PERFORM 4000-WRITE-HEADER
           END-IF
           IF NO-HARD-ERROR
               PERFORM 5000-PROCESS-MART
           END-IF
           IF NO-HARD-ERROR
               PERFORM 6000-PROCESS-MONTHLY
           END-IF
           IF NO-HARD-ERROR
               PERFORM 7000-WRITE-TRAILER
           END-IF
           IF NO-HARD-ERROR
               PERFORM 8000-UPDATE-CONTROL
           END-IF
           PERFORM 9000-CLOSE-FILES
           IF HARD-ERROR
               MOVE 8 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF
           GOBACK.

       1000-INITIALIZE.
           SET NO-HARD-ERROR TO TRUE
           SET MART-NOT-EOF TO TRUE
           SET MON-NOT-EOF TO TRUE
           ACCEPT WS-CDATE FROM DATE YYYYMMDD
           MOVE WS-CDATE(1:8) TO WS-CURRENT-DT
           ACCEPT WS-CURRENT-TM FROM TIME
           STRING WS-CURRENT-DT
                  WS-CURRENT-TM(1:6)
               DELIMITED BY SIZE
               INTO WS-CURRENT-TS
           END-STRING
           MOVE WS-JOB-ID TO CT-JOB-ID
           MOVE ZERO TO WS-MART-IN-CNT
           MOVE ZERO TO WS-MON-IN-CNT
           MOVE ZERO TO WS-OUT-CNT
           MOVE ZERO TO WS-DETAIL-CNT
           MOVE ZERO TO WS-SKIP-CNT
           MOVE ZERO TO WS-DETAIL-AMT
           MOVE ZERO TO WS-AUDIT-SEQ.

       2000-OPEN-FILES.
           OPEN INPUT JHMARTF
           IF WS-ST-JHMARTF NOT = "00"
               MOVE WS-DS-MART TO WS-ERR-DATASET
               MOVE WS-ST-JHMARTF TO WS-ERR-STATUS
               MOVE "JHMARTF OPEN FAILED" TO WS-ERR-REASON
               PERFORM 9900-HARD-ERROR
           END-IF

           IF NO-HARD-ERROR
               OPEN INPUT JHMONRF
               IF WS-ST-JHMONRF NOT = "00"
                   MOVE WS-DS-MON TO WS-ERR-DATASET
                   MOVE WS-ST-JHMONRF TO WS-ERR-STATUS
                   MOVE "JHMONRF OPEN FAILED" TO WS-ERR-REASON
                   PERFORM 9900-HARD-ERROR
               END-IF
           END-IF

           IF NO-HARD-ERROR
               OPEN I-O JHCTLKF
               IF WS-ST-JHCTLKF NOT = "00"
                   MOVE WS-DS-CTL TO WS-ERR-DATASET
                   MOVE WS-ST-JHCTLKF TO WS-ERR-STATUS
                   MOVE "JHCTLKF OPEN FAILED" TO WS-ERR-REASON
                   PERFORM 9900-HARD-ERROR
               END-IF
           END-IF

           IF NO-HARD-ERROR
               OPEN OUTPUT JHBIHOF
               IF WS-ST-JHBIHOF NOT = "00"
                   MOVE WS-DS-BIO TO WS-ERR-DATASET
                   MOVE WS-ST-JHBIHOF TO WS-ERR-STATUS
                   MOVE "JHBIHOF OPEN FAILED" TO WS-ERR-REASON
                   PERFORM 9900-HARD-ERROR
               END-IF
           END-IF

           IF NO-HARD-ERROR
               OPEN OUTPUT JHAUDTF
               IF WS-ST-JHAUDTF NOT = "00"
                   MOVE WS-DS-AUD TO WS-ERR-DATASET
                   MOVE WS-ST-JHAUDTF TO WS-ERR-STATUS
                   MOVE "JHAUDTF OPEN FAILED" TO WS-ERR-REASON
                   PERFORM 9900-HARD-ERROR
               END-IF
           END-IF.

       3000-LOAD-CONTROL.
           MOVE WS-JOB-ID TO CT-JOB-ID
           READ JHCTLKF KEY IS CT-JOB-ID
           END-READ
           EVALUATE WS-ST-JHCTLKF
               WHEN "00"
                   IF CT-STATUS-CD = "9"
                       MOVE WS-DS-CTL TO WS-ERR-DATASET
                       MOVE WS-ST-JHCTLKF TO WS-ERR-STATUS
                       MOVE "PREVIOUS ABEND STATUS" TO WS-ERR-REASON
                       PERFORM 9900-HARD-ERROR
                   ELSE
                       MOVE "1" TO CT-STATUS-CD
                       MOVE WS-CURRENT-TS TO CT-START-TS
                       MOVE ZERO TO CT-END-TS
                       MOVE ZERO TO CT-INPUT-CNT
                       MOVE ZERO TO CT-OUTPUT-CNT
                       REWRITE JHCTLKF-REC
                       END-REWRITE
                       IF WS-ST-JHCTLKF NOT = "00"
                           MOVE WS-DS-CTL TO WS-ERR-DATASET
                           MOVE WS-ST-JHCTLKF TO WS-ERR-STATUS
                           MOVE "CONTROL START UPDATE FAILED"
                               TO WS-ERR-REASON
                           PERFORM 9900-HARD-ERROR
                       ELSE
                           PERFORM 8100-WRITE-AUDIT-START
                       END-IF
                   END-IF
               WHEN OTHER
                   MOVE WS-DS-CTL TO WS-ERR-DATASET
                   MOVE WS-ST-JHCTLKF TO WS-ERR-STATUS
                   MOVE "CONTROL RECORD READ FAILED" TO WS-ERR-REASON
                   PERFORM 9900-HARD-ERROR
           END-EVALUATE.

       4000-WRITE-HEADER.
           INITIALIZE JHBIHOF-REC
           STRING WS-JOB-ID "-" WS-CURRENT-DT "-H"
               DELIMITED BY SIZE
               INTO BIO-HANDOFF-ID
           END-STRING
           MOVE WS-CURRENT-DT TO BIO-FILE-DT
           MOVE WS-REC-HDR TO BIO-RECORD-TYPE
           MOVE 40 TO BIO-PAYLOAD-LEN
           STRING "JOB=" WS-JOB-ID
                  ",DATE=" WS-CURRENT-DT
                  ",TIME=" WS-CURRENT-TM(1:6)
               DELIMITED BY SIZE
               INTO BIO-PAYLOAD-TEXT
           END-STRING
           WRITE JHBIHOF-REC
           END-WRITE
           IF WS-ST-JHBIHOF NOT = "00"
               MOVE WS-DS-BIO TO WS-ERR-DATASET
               MOVE WS-ST-JHBIHOF TO WS-ERR-STATUS
               MOVE "HEADER WRITE FAILED" TO WS-ERR-REASON
               PERFORM 9900-HARD-ERROR
           ELSE
               ADD 1 TO WS-OUT-CNT
           END-IF.

       5000-PROCESS-MART.
           PERFORM UNTIL MART-EOF OR HARD-ERROR
               READ JHMARTF
                   AT END
                       SET MART-EOF TO TRUE
                   NOT AT END
                       ADD 1 TO WS-MART-IN-CNT
                       PERFORM 5100-EDIT-MART
               END-READ
           END-PERFORM.

       5100-EDIT-MART.
           IF MRT-YYYYMM IS NOT NUMERIC
               ADD 1 TO WS-SKIP-CNT
               PERFORM 8300-WRITE-AUDIT-SKIP
           ELSE
               IF MRT-ACCT-NO = SPACES
                   ADD 1 TO WS-SKIP-CNT
                   PERFORM 8300-WRITE-AUDIT-SKIP
               ELSE
                   IF MRT-FEE-AMT < ZERO
                       ADD 1 TO WS-SKIP-CNT
                       PERFORM 8300-WRITE-AUDIT-SKIP
                   ELSE
                       MOVE MRT-FEE-AMT TO WS-WORK-AMT
                       MOVE MRT-FEE-AMT TO WS-MART-AMT-TXT
                       MOVE MRT-FEE-YTD TO WS-MART-YTD-TXT
                       PERFORM 5200-WRITE-MART-DETAIL
                   END-IF
               END-IF
           END-IF.

       5200-WRITE-MART-DETAIL.
           INITIALIZE JHBIHOF-REC
           ADD 1 TO WS-DETAIL-CNT
           STRING WS-JOB-ID "-" WS-CURRENT-DT "-M"
                  WS-DETAIL-CNT
               DELIMITED BY SIZE
               INTO BIO-HANDOFF-ID
           END-STRING
           MOVE WS-CURRENT-DT TO BIO-FILE-DT
           MOVE WS-REC-DTL TO BIO-RECORD-TYPE
           INITIALIZE WS-PAYLOAD
           MOVE ZERO TO WS-PAYLOAD-LEN
           STRING "SRC=MART"
                  "|YM=" MRT-YYYYMM
                  "|ACCT=" MRT-ACCT-NO
                  "|CUST=" MRT-CUSTOMER-ID
                  "|PROD=" MRT-TRUST-PRODUCT-CD
                  "|BR=" MRT-BRANCH-CD
                  "|FEE=" WS-MART-AMT-TXT
                  "|YTD=" WS-MART-YTD-TXT
                  "|LOAD=" MRT-LOAD-DT
               DELIMITED BY SIZE
               INTO WS-PAYLOAD
           END-STRING
           INSPECT WS-PAYLOAD TALLYING WS-PAYLOAD-LEN
               FOR CHARACTERS BEFORE INITIAL SPACE
           MOVE WS-PAYLOAD-LEN TO BIO-PAYLOAD-LEN
           MOVE WS-PAYLOAD TO BIO-PAYLOAD-TEXT
           MOVE WS-WORK-AMT TO BIO-TRAILER-AMT
           WRITE JHBIHOF-REC
           END-WRITE
           IF WS-ST-JHBIHOF NOT = "00"
               MOVE WS-DS-BIO TO WS-ERR-DATASET
               MOVE WS-ST-JHBIHOF TO WS-ERR-STATUS
               MOVE "MART DETAIL WRITE FAILED" TO WS-ERR-REASON
               PERFORM 9900-HARD-ERROR
           ELSE
               ADD 1 TO WS-OUT-CNT
               ADD WS-WORK-AMT TO WS-DETAIL-AMT
           END-IF
           MOVE ZERO TO WS-PAYLOAD-LEN.

       6000-PROCESS-MONTHLY.
           PERFORM UNTIL MON-EOF OR HARD-ERROR
               READ JHMONRF
                   AT END
                       SET MON-EOF TO TRUE
                   NOT AT END
                       ADD 1 TO WS-MON-IN-CNT
                       PERFORM 6100-EDIT-MONTHLY
               END-READ
           END-PERFORM.

       6100-EDIT-MONTHLY.
           IF MON-YYYYMM IS NOT NUMERIC
               ADD 1 TO WS-SKIP-CNT
               PERFORM 8300-WRITE-AUDIT-SKIP
           ELSE
               IF MON-TRUST-PRODUCT-CD = SPACES
                   ADD 1 TO WS-SKIP-CNT
                   PERFORM 8300-WRITE-AUDIT-SKIP
               ELSE
                   IF MON-ACCT-CNT = ZERO
                      AND MON-FEE-AMT-SUM NOT = WS-ZERO-AMT
                       ADD 1 TO WS-SKIP-CNT
                       PERFORM 8300-WRITE-AUDIT-SKIP
                   ELSE
                       MOVE MON-FEE-AMT-SUM TO WS-WORK-AMT
                       MOVE MON-FEE-AMT-SUM TO WS-MON-AMT-TXT
                       MOVE MON-FEE-YTD-SUM TO WS-MON-YTD-TXT
                       MOVE MON-ACCT-CNT TO WS-MON-CNT-TXT
                       MOVE MON-ADJUST-CNT TO WS-MON-ADJ-TXT
                       PERFORM 6200-WRITE-MONTHLY-DETAIL
                   END-IF
               END-IF
           END-IF.

       6200-WRITE-MONTHLY-DETAIL.
           INITIALIZE JHBIHOF-REC
           ADD 1 TO WS-DETAIL-CNT
           STRING WS-JOB-ID "-" WS-CURRENT-DT "-S"
                  WS-DETAIL-CNT
               DELIMITED BY SIZE
               INTO BIO-HANDOFF-ID
           END-STRING
           MOVE WS-CURRENT-DT TO BIO-FILE-DT
           MOVE WS-REC-DTL TO BIO-RECORD-TYPE
           INITIALIZE WS-PAYLOAD
           MOVE ZERO TO WS-PAYLOAD-LEN
           STRING "SRC=MON"
                  "|YM=" MON-YYYYMM
                  "|PROD=" MON-TRUST-PRODUCT-CD
                  "|BR=" MON-BRANCH-CD
                  "|SUM=" WS-MON-AMT-TXT
                  "|YTD=" WS-MON-YTD-TXT
                  "|ACCTCNT=" WS-MON-CNT-TXT
                  "|ADJCNT=" WS-MON-ADJ-TXT
               DELIMITED BY SIZE
               INTO WS-PAYLOAD
           END-STRING
           INSPECT WS-PAYLOAD TALLYING WS-PAYLOAD-LEN
               FOR CHARACTERS BEFORE INITIAL SPACE
           MOVE WS-PAYLOAD-LEN TO BIO-PAYLOAD-LEN
           MOVE WS-PAYLOAD TO BIO-PAYLOAD-TEXT
           MOVE WS-WORK-AMT TO BIO-TRAILER-AMT
           WRITE JHBIHOF-REC
           END-WRITE
           IF WS-ST-JHBIHOF NOT = "00"
               MOVE WS-DS-BIO TO WS-ERR-DATASET
               MOVE WS-ST-JHBIHOF TO WS-ERR-STATUS
               MOVE "MONTHLY DETAIL WRITE FAILED" TO WS-ERR-REASON
               PERFORM 9900-HARD-ERROR
           ELSE
               ADD 1 TO WS-OUT-CNT
               ADD WS-WORK-AMT TO WS-DETAIL-AMT
           END-IF
           MOVE ZERO TO WS-PAYLOAD-LEN.

       7000-WRITE-TRAILER.
           INITIALIZE JHBIHOF-REC
           STRING WS-JOB-ID "-" WS-CURRENT-DT "-T"
               DELIMITED BY SIZE
               INTO BIO-HANDOFF-ID
           END-STRING
           MOVE WS-CURRENT-DT TO BIO-FILE-DT
           MOVE WS-REC-TRL TO BIO-RECORD-TYPE
           MOVE 30 TO BIO-PAYLOAD-LEN
           STRING "DETAIL-CNT=" WS-DETAIL-CNT
                  ",SKIP-CNT=" WS-SKIP-CNT
               DELIMITED BY SIZE
               INTO BIO-PAYLOAD-TEXT
           END-STRING
           MOVE WS-DETAIL-CNT TO BIO-TRAILER-CNT
           MOVE WS-DETAIL-AMT TO BIO-TRAILER-AMT
           WRITE JHBIHOF-REC
           END-WRITE
           IF WS-ST-JHBIHOF NOT = "00"
               MOVE WS-DS-BIO TO WS-ERR-DATASET
               MOVE WS-ST-JHBIHOF TO WS-ERR-STATUS
               MOVE "TRAILER WRITE FAILED" TO WS-ERR-REASON
               PERFORM 9900-HARD-ERROR
           ELSE
               ADD 1 TO WS-OUT-CNT
           END-IF.

       8000-UPDATE-CONTROL.
           ACCEPT WS-CDATE FROM DATE YYYYMMDD
           MOVE WS-CDATE(1:8) TO WS-CURRENT-DT
           ACCEPT WS-CURRENT-TM FROM TIME
           STRING WS-CURRENT-DT
                  WS-CURRENT-TM(1:6)
               DELIMITED BY SIZE
               INTO WS-CURRENT-TS
           END-STRING
           MOVE WS-JOB-ID TO CT-JOB-ID
           READ JHCTLKF KEY IS CT-JOB-ID
           END-READ
           IF WS-ST-JHCTLKF = "00"
               MOVE "0" TO CT-STATUS-CD
               MOVE WS-CURRENT-TS TO CT-END-TS
               ADD WS-MART-IN-CNT TO WS-MON-IN-CNT
                   GIVING CT-INPUT-CNT
               MOVE WS-OUT-CNT TO CT-OUTPUT-CNT
               MOVE ZERO TO CT-RESTART-POS
               REWRITE JHCTLKF-REC
               END-REWRITE
               IF WS-ST-JHCTLKF = "00"
                   PERFORM 8200-WRITE-AUDIT-END
               ELSE
                   MOVE WS-DS-CTL TO WS-ERR-DATASET
                   MOVE WS-ST-JHCTLKF TO WS-ERR-STATUS
                   MOVE "CONTROL END UPDATE FAILED"
                       TO WS-ERR-REASON
                   PERFORM 9900-HARD-ERROR
               END-IF
           ELSE
               MOVE WS-DS-CTL TO WS-ERR-DATASET
               MOVE WS-ST-JHCTLKF TO WS-ERR-STATUS
               MOVE "CONTROL END READ FAILED" TO WS-ERR-REASON
               PERFORM 9900-HARD-ERROR
           END-IF.

       8100-WRITE-AUDIT-START.
           INITIALIZE JHAUDTF-REC
           ADD 1 TO WS-AUDIT-SEQ
           MOVE WS-AUDIT-SEQ TO AUD-AUDIT-SEQ
           MOVE WS-JOB-ID TO AUD-JOB-ID
           MOVE CT-BUSINESS-DT TO AUD-BUSINESS-DT
           MOVE WS-EV-START TO AUD-EVENT-CD
           MOVE WS-DS-CTL TO AUD-DATASET-ID
           MOVE ZERO TO AUD-REC-CNT
           MOVE ZERO TO AUD-AMT-TOTAL
           MOVE WS-CURRENT-TS TO AUD-EVENT-TS
           WRITE JHAUDTF-REC
           END-WRITE
           IF WS-ST-JHAUDTF NOT = "00"
               MOVE WS-DS-AUD TO WS-ERR-DATASET
               MOVE WS-ST-JHAUDTF TO WS-ERR-STATUS
               MOVE "AUDIT START WRITE FAILED" TO WS-ERR-REASON
               PERFORM 9900-HARD-ERROR
           END-IF.

       8200-WRITE-AUDIT-END.
           INITIALIZE JHAUDTF-REC
           ADD 1 TO WS-AUDIT-SEQ
           MOVE WS-AUDIT-SEQ TO AUD-AUDIT-SEQ
           MOVE WS-JOB-ID TO AUD-JOB-ID
           MOVE CT-BUSINESS-DT TO AUD-BUSINESS-DT
           MOVE WS-EV-END TO AUD-EVENT-CD
           MOVE WS-DS-BIO TO AUD-DATASET-ID
           MOVE WS-OUT-CNT TO AUD-REC-CNT
           MOVE WS-DETAIL-AMT TO AUD-AMT-TOTAL
           MOVE WS-CURRENT-TS TO AUD-EVENT-TS
           WRITE JHAUDTF-REC
           END-WRITE
           IF WS-ST-JHAUDTF NOT = "00"
               MOVE WS-DS-AUD TO WS-ERR-DATASET
               MOVE WS-ST-JHAUDTF TO WS-ERR-STATUS
               MOVE "AUDIT END WRITE FAILED" TO WS-ERR-REASON
               PERFORM 9900-HARD-ERROR
           END-IF.

       8300-WRITE-AUDIT-SKIP.
           INITIALIZE JHAUDTF-REC
           ADD 1 TO WS-AUDIT-SEQ
           MOVE WS-AUDIT-SEQ TO AUD-AUDIT-SEQ
           MOVE WS-JOB-ID TO AUD-JOB-ID
           MOVE CT-BUSINESS-DT TO AUD-BUSINESS-DT
           MOVE WS-EV-SKIP TO AUD-EVENT-CD
           MOVE WS-DS-BIO TO AUD-DATASET-ID
           MOVE 1 TO AUD-REC-CNT
           MOVE ZERO TO AUD-AMT-TOTAL
           MOVE WS-CURRENT-TS TO AUD-EVENT-TS
           WRITE JHAUDTF-REC
           END-WRITE
           IF WS-ST-JHAUDTF NOT = "00"
               MOVE WS-DS-AUD TO WS-ERR-DATASET
               MOVE WS-ST-JHAUDTF TO WS-ERR-STATUS
               MOVE "AUDIT SKIP WRITE FAILED" TO WS-ERR-REASON
               PERFORM 9900-HARD-ERROR
           END-IF.

       9000-CLOSE-FILES.
           CLOSE JHMARTF
           CLOSE JHMONRF
           CLOSE JHCTLKF
           CLOSE JHBIHOF
           CLOSE JHAUDTF.

       9900-HARD-ERROR.
           SET HARD-ERROR TO TRUE
           DISPLAY "JH610B ABEND DS=" WS-ERR-DATASET
                   " ST=" WS-ERR-STATUS
                   " REASON=" WS-ERR-REASON
           IF WS-ST-JHAUDTF = "00"
               INITIALIZE JHAUDTF-REC
               ADD 1 TO WS-AUDIT-SEQ
               MOVE WS-AUDIT-SEQ TO AUD-AUDIT-SEQ
               MOVE WS-JOB-ID TO AUD-JOB-ID
               MOVE CT-BUSINESS-DT TO AUD-BUSINESS-DT
               MOVE WS-EV-ERR TO AUD-EVENT-CD
               MOVE WS-ERR-DATASET TO AUD-DATASET-ID
               MOVE WS-OUT-CNT TO AUD-REC-CNT
               MOVE WS-DETAIL-AMT TO AUD-AMT-TOTAL
               MOVE WS-CURRENT-TS TO AUD-EVENT-TS
               WRITE JHAUDTF-REC
               END-WRITE
           END-IF.
