       IDENTIFICATION DIVISION.
       PROGRAM-ID. JH420B.
      ******************************************************************
      * 口座属性変更イベントを時刻順に処理し、口座ディメンションを更新する。
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT JHCHGEF ASSIGN TO "JHCHGEF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS CHG-CHANGE-SEQ
               FILE STATUS IS FS-JHCHGEF.
           SELECT JHCTLKF ASSIGN TO "JHCTLKF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CT-JOB-ID
               FILE STATUS IS FS-JHCTLKF.
           SELECT JHACDMF ASSIGN TO "JHACDMF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS ACD-ACCT-NO
               FILE STATUS IS FS-JHACDMF.
           SELECT JHAUDTF ASSIGN TO "JHAUDTF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS AUD-AUDIT-SEQ
               FILE STATUS IS FS-JHAUDTF.

       DATA DIVISION.
       FILE SECTION.
       FD  JHCHGEF.
           COPY JHCHGC.
       FD  JHCTLKF.
           COPY JHCTLC.
       FD  JHACDMF.
           COPY JHACDC.
       FD  JHAUDTF.
           COPY JHAUDC.

       WORKING-STORAGE SECTION.
       01  FS-AREA.
           05 FS-JHCHGEF             PIC XX VALUE SPACES.
           05 FS-JHCTLKF             PIC XX VALUE SPACES.
           05 FS-JHACDMF             PIC XX VALUE SPACES.
           05 FS-JHAUDTF             PIC XX VALUE SPACES.

       01  SW-AREA.
           05 SW-EOF                 PIC X VALUE "N".
              88 EOF-JHCHGEF              VALUE "Y".
              88 NOT-EOF-JHCHGEF          VALUE "N".
           05 SW-HARD-ERROR          PIC X VALUE "N".
              88 HARD-ERROR               VALUE "Y".
              88 NO-HARD-ERROR            VALUE "N".
           05 SW-CTL-FOUND           PIC X VALUE "N".
              88 CTL-FOUND                VALUE "Y".
              88 CTL-NOT-FOUND            VALUE "N".
           05 SW-ACD-FOUND           PIC X VALUE "N".
              88 ACD-FOUND                VALUE "Y".
              88 ACD-NOT-FOUND            VALUE "N".

       01  CONST-AREA.
           05 CN-JOB-ID              PIC X(08) VALUE "JH420B".
           05 CN-DS-CHG              PIC X(08) VALUE "JHCHGEF".
           05 CN-DS-ACD              PIC X(08) VALUE "JHACDMF".
           05 CN-DS-CTL              PIC X(08) VALUE "JHCTLKF".
           05 CN-DS-AUD              PIC X(08) VALUE "JHAUDTF".
           05 CN-STAT-RUN            PIC X(01) VALUE "R".
           05 CN-STAT-END            PIC X(01) VALUE "E".
           05 CN-STAT-ERR            PIC X(01) VALUE "A".
           05 CN-AUD-START           PIC X(08) VALUE "START".
           05 CN-AUD-END             PIC X(08) VALUE "END".
           05 CN-AUD-CAPT            PIC X(08) VALUE "CAPTURE".
           05 CN-AUD-SKIP            PIC X(08) VALUE "SKIP".
           05 CN-AUD-ERR             PIC X(08) VALUE "ERROR".
           05 CN-JUDGE-CAPTURE       PIC X(01) VALUE "C".

       01  COUNT-AREA.
           05 WS-INPUT-CNT           PIC 9(11) VALUE ZERO.
           05 WS-OUTPUT-CNT          PIC 9(11) VALUE ZERO.
           05 WS-SKIP-CNT            PIC 9(11) VALUE ZERO.
           05 WS-ERROR-CNT           PIC 9(11) VALUE ZERO.
           05 WS-AUDIT-SEQ           PIC 9(12) VALUE ZERO.
           05 WS-CTL-RUN-SEQ         PIC 9(09) VALUE ZERO.

       01  TIME-AREA.
           05 WS-CURRENT-DATE.
              10 WS-CUR-YYYY         PIC 9(04).
              10 WS-CUR-MM           PIC 9(02).
              10 WS-CUR-DD           PIC 9(02).
              10 WS-CUR-HH           PIC 9(02).
              10 WS-CUR-MI           PIC 9(02).
              10 WS-CUR-SS           PIC 9(02).
              10 WS-CUR-CC           PIC 9(02).
              10 WS-CUR-GMTOFF       PIC S9(04).
           05 WS-EVENT-TS            PIC X(20).
           05 WS-BUSINESS-DT         PIC 9(08) VALUE ZERO.

       01  EDIT-AREA.
           05 WS-VALID-CHANGE        PIC X VALUE "N".
              88 VALID-CHANGE             VALUE "Y".
              88 INVALID-CHANGE           VALUE "N".
           05 WS-NEED-UPDATE         PIC X VALUE "N".
              88 NEED-UPDATE              VALUE "Y".
              88 NO-UPDATE                VALUE "N".
           05 WS-NEW-BRANCH          PIC X(03) VALUE SPACES.
           05 WS-NEW-PRODUCT         PIC X(06) VALUE SPACES.
           05 WS-NEW-STATUS          PIC X(01) VALUE SPACES.
           05 WS-NEW-CLOSE-DT        PIC 9(08) VALUE ZERO.

       01  LK-JH421S-PARM.
           05 LK-EVT-ACCT-NO         PIC X(12).
           05 LK-EVT-AFTER-HASH      PIC X(64).
           05 LK-EVT-CHG-TS          PIC X(20).
           05 LK-JUDGE-CD            PIC X(01).
           05 LK-DETAIL-CD           PIC X(08).
           05 LK-REASON-TEXT         PIC X(80).

       PROCEDURE DIVISION.
       MAIN-SECTION.
           PERFORM 1000-INIT
           IF NO-HARD-ERROR
              PERFORM 2000-PROCESS UNTIL EOF-JHCHGEF OR HARD-ERROR
           END-IF
           PERFORM 9000-FINAL
           GOBACK.

       1000-INIT.
           MOVE 0 TO RETURN-CODE
           SET NO-HARD-ERROR TO TRUE
           SET NOT-EOF-JHCHGEF TO TRUE
           ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD
           MOVE WS-CURRENT-DATE(1:8) TO WS-BUSINESS-DT

           OPEN INPUT JHCHGEF
           IF FS-JHCHGEF NOT = "00"
              MOVE 12 TO RETURN-CODE
              SET HARD-ERROR TO TRUE
              DISPLAY "JHCHGEF OPEN ST=" FS-JHCHGEF
           END-IF

           IF NO-HARD-ERROR
              OPEN I-O JHCTLKF
              IF FS-JHCTLKF NOT = "00"
                 MOVE 12 TO RETURN-CODE
                 SET HARD-ERROR TO TRUE
                 DISPLAY "JHCTLKF OPEN ST=" FS-JHCTLKF
              END-IF
           END-IF

           IF NO-HARD-ERROR
              OPEN I-O JHACDMF
              IF FS-JHACDMF NOT = "00"
                 MOVE 12 TO RETURN-CODE
                 SET HARD-ERROR TO TRUE
                 DISPLAY "JHACDMF OPEN ST=" FS-JHACDMF
              END-IF
           END-IF

           IF NO-HARD-ERROR
              OPEN OUTPUT JHAUDTF
              IF FS-JHAUDTF NOT = "00"
                 MOVE 12 TO RETURN-CODE
                 SET HARD-ERROR TO TRUE
                 DISPLAY "JHAUDTF OPEN ST=" FS-JHAUDTF
              END-IF
           END-IF

           IF NO-HARD-ERROR
              PERFORM 1100-LOAD-CONTROL
              PERFORM 8100-WRITE-START-AUDIT
              PERFORM 2100-READ-CHANGE
           END-IF.

       1100-LOAD-CONTROL.
           MOVE CN-JOB-ID TO CT-JOB-ID
           READ JHCTLKF KEY IS CT-JOB-ID
              INVALID KEY
                 SET CTL-NOT-FOUND TO TRUE
              NOT INVALID KEY
                 SET CTL-FOUND TO TRUE
           END-READ

           IF FS-JHCTLKF NOT = "00" AND FS-JHCTLKF NOT = "23"
              MOVE 12 TO RETURN-CODE
              SET HARD-ERROR TO TRUE
              DISPLAY "JHCTLKF READ ST=" FS-JHCTLKF
           END-IF

           IF NO-HARD-ERROR AND CTL-NOT-FOUND
              INITIALIZE JHCTLKF-REC
              MOVE CN-JOB-ID TO CT-JOB-ID
              MOVE WS-BUSINESS-DT TO CT-BUSINESS-DT
              MOVE 1 TO CT-RUN-SEQ
              MOVE CN-STAT-RUN TO CT-STATUS-CD
              MOVE ZERO TO CT-RESTART-POS
              MOVE ZERO TO CT-INPUT-CNT
              MOVE ZERO TO CT-OUTPUT-CNT
              PERFORM 8200-SET-EVENT-TS
              MOVE WS-EVENT-TS TO CT-START-TS
              WRITE JHCTLKF-REC
              IF FS-JHCTLKF NOT = "00"
                 MOVE 12 TO RETURN-CODE
                 SET HARD-ERROR TO TRUE
                 DISPLAY "JHCTLKF WRITE ST=" FS-JHCTLKF
              END-IF
           END-IF

           IF NO-HARD-ERROR AND CTL-FOUND
              COMPUTE WS-CTL-RUN-SEQ =
                 FUNCTION NUMVAL(CT-RUN-SEQ) + 1
              MOVE WS-CTL-RUN-SEQ TO CT-RUN-SEQ
              MOVE CN-STAT-RUN TO CT-STATUS-CD
              MOVE WS-BUSINESS-DT TO CT-BUSINESS-DT
              PERFORM 8200-SET-EVENT-TS
              MOVE WS-EVENT-TS TO CT-START-TS
              MOVE SPACES TO CT-END-TS
              REWRITE JHCTLKF-REC
              IF FS-JHCTLKF NOT = "00"
                 MOVE 12 TO RETURN-CODE
                 SET HARD-ERROR TO TRUE
                 DISPLAY "JHCTLKF REWRITE ST=" FS-JHCTLKF
              END-IF
           END-IF.

       2000-PROCESS.
           PERFORM 2200-VALIDATE-CHANGE
           IF VALID-CHANGE
              PERFORM 2300-CALL-JUDGE
              IF NO-HARD-ERROR
                 IF LK-JUDGE-CD = CN-JUDGE-CAPTURE
                    PERFORM 2400-LOAD-ACCOUNT
                    IF NO-HARD-ERROR AND ACD-FOUND
                       PERFORM 2500-APPLY-CHANGE
                    END-IF
                 ELSE
                    ADD 1 TO WS-SKIP-CNT
                    PERFORM 8300-WRITE-SKIP-AUDIT
                 END-IF
              END-IF
           ELSE
              ADD 1 TO WS-SKIP-CNT
              PERFORM 8300-WRITE-SKIP-AUDIT
           END-IF

           IF NO-HARD-ERROR
              PERFORM 2600-UPDATE-CONTROL
              PERFORM 2100-READ-CHANGE
           END-IF.

       2100-READ-CHANGE.
           READ JHCHGEF NEXT RECORD
              AT END
                 SET EOF-JHCHGEF TO TRUE
              NOT AT END
                 ADD 1 TO WS-INPUT-CNT
           END-READ

           IF FS-JHCHGEF NOT = "00" AND FS-JHCHGEF NOT = "10"
              MOVE 12 TO RETURN-CODE
              SET HARD-ERROR TO TRUE
              DISPLAY "JHCHGEF READ ST=" FS-JHCHGEF
           END-IF.

       2200-VALIDATE-CHANGE.
           SET INVALID-CHANGE TO TRUE
           MOVE SPACES TO LK-DETAIL-CD
           MOVE SPACES TO LK-REASON-TEXT

           IF CHG-ACCT-NO = SPACES
              MOVE "EACCT001" TO LK-DETAIL-CD
              MOVE "ACCT REQUIRED" TO LK-REASON-TEXT
           ELSE
              IF CHG-CHANGE-TS = SPACES
                 MOVE "ETS0001" TO LK-DETAIL-CD
                 MOVE "TIME REQUIRED" TO LK-REASON-TEXT
              ELSE
                 IF CHG-AFTER-HASH = SPACES
                    MOVE "EHASH001" TO LK-DETAIL-CD
                    MOVE "HASH REQUIRED" TO LK-REASON-TEXT
                 ELSE
                    IF CHG-APPLY-STATUS NOT = "0"
                       MOVE "ESTS0001" TO LK-DETAIL-CD
                       MOVE "STATUS NOT PENDING" TO LK-REASON-TEXT
                    ELSE
                       EVALUATE CHG-CHANGE-TYPE
                          WHEN "BR"
                          WHEN "PR"
                          WHEN "ST"
                          WHEN "CL"
                             SET VALID-CHANGE TO TRUE
                          WHEN OTHER
                             MOVE "ETYPE001" TO LK-DETAIL-CD
                             MOVE "TYPE INVALID" TO LK-REASON-TEXT
                       END-EVALUATE
                    END-IF
                 END-IF
              END-IF
           END-IF.

       2300-CALL-JUDGE.
           MOVE CHG-ACCT-NO TO LK-EVT-ACCT-NO
           MOVE CHG-AFTER-HASH TO LK-EVT-AFTER-HASH
           MOVE CHG-CHANGE-TS TO LK-EVT-CHG-TS
           MOVE SPACES TO LK-JUDGE-CD
           MOVE SPACES TO LK-DETAIL-CD
           MOVE SPACES TO LK-REASON-TEXT

           CALL "JH421S" USING LK-JH421S-PARM
              ON EXCEPTION
                 MOVE 12 TO RETURN-CODE
                 SET HARD-ERROR TO TRUE
                 MOVE "ECALL001" TO LK-DETAIL-CD
                 MOVE "JH421S CALL ERROR" TO LK-REASON-TEXT
                 DISPLAY "JH421S CALL ERROR ACCT=" CHG-ACCT-NO
           END-CALL

           IF NO-HARD-ERROR
              IF LK-JUDGE-CD = SPACES
                 MOVE 8 TO RETURN-CODE
                 SET HARD-ERROR TO TRUE
                 MOVE "EJDG001" TO LK-DETAIL-CD
                 MOVE "JUDGE CODE BLANK" TO LK-REASON-TEXT
                 DISPLAY "JH421S JUDGE BLANK ACCT=" CHG-ACCT-NO
              END-IF
           END-IF.

       2400-LOAD-ACCOUNT.
           SET ACD-NOT-FOUND TO TRUE
           MOVE CHG-ACCT-NO TO ACD-ACCT-NO
           READ JHACDMF KEY IS ACD-ACCT-NO
              INVALID KEY
                 SET ACD-NOT-FOUND TO TRUE
              NOT INVALID KEY
                 SET ACD-FOUND TO TRUE
           END-READ

           IF FS-JHACDMF NOT = "00" AND FS-JHACDMF NOT = "23"
              MOVE 12 TO RETURN-CODE
              SET HARD-ERROR TO TRUE
              DISPLAY "JHACDMF READ ST=" FS-JHACDMF
           END-IF

           IF NO-HARD-ERROR AND ACD-NOT-FOUND
              ADD 1 TO WS-ERROR-CNT
              MOVE "EACD404" TO LK-DETAIL-CD
              MOVE "ACCOUNT NOT FOUND" TO LK-REASON-TEXT
              PERFORM 8400-WRITE-ERROR-AUDIT
           END-IF.

       2500-APPLY-CHANGE.
           SET NO-UPDATE TO TRUE
           MOVE ACD-BRANCH-CD TO WS-NEW-BRANCH
           MOVE ACD-TRUST-PRODUCT-CD TO WS-NEW-PRODUCT
           MOVE ACD-STATUS-CD TO WS-NEW-STATUS
           MOVE ACD-CLOSE-DT TO WS-NEW-CLOSE-DT

           EVALUATE CHG-CHANGE-TYPE
              WHEN "BR"
                 MOVE CHG-AFTER-HASH(1:3) TO WS-NEW-BRANCH
              WHEN "PR"
                 MOVE CHG-AFTER-HASH(1:6) TO WS-NEW-PRODUCT
              WHEN "ST"
                 MOVE CHG-AFTER-HASH(1:1) TO WS-NEW-STATUS
              WHEN "CL"
                 MOVE CHG-AFTER-HASH(1:8) TO WS-NEW-CLOSE-DT
           END-EVALUATE

           IF WS-NEW-STATUS NOT = "0" AND WS-NEW-STATUS NOT = "1"
              AND WS-NEW-STATUS NOT = "9"
              ADD 1 TO WS-ERROR-CNT
              MOVE "ESTA0002" TO LK-DETAIL-CD
              MOVE "ACCOUNT STATUS BAD" TO LK-REASON-TEXT
              PERFORM 8400-WRITE-ERROR-AUDIT
           ELSE
              IF CHG-CHANGE-TS > ACD-LAST-CHG-TS
                 SET NEED-UPDATE TO TRUE
              ELSE
                 ADD 1 TO WS-SKIP-CNT
                 MOVE "DLATE001" TO LK-DETAIL-CD
                 MOVE "OLDER CHANGE SKIP" TO LK-REASON-TEXT
                 PERFORM 8300-WRITE-SKIP-AUDIT
              END-IF
           END-IF

           IF NEED-UPDATE
              MOVE WS-NEW-BRANCH TO ACD-BRANCH-CD
              MOVE WS-NEW-PRODUCT TO ACD-TRUST-PRODUCT-CD
              MOVE WS-NEW-STATUS TO ACD-STATUS-CD
              MOVE WS-NEW-CLOSE-DT TO ACD-CLOSE-DT
              MOVE CHG-CHANGE-TS TO ACD-LAST-CHG-TS
              REWRITE JHACDMF-REC
              IF FS-JHACDMF = "00"
                 ADD 1 TO WS-OUTPUT-CNT
                 PERFORM 8500-WRITE-CAPTURE-AUDIT
              ELSE
                 MOVE 12 TO RETURN-CODE
                 SET HARD-ERROR TO TRUE
                 DISPLAY "JHACDMF REWRITE ST=" FS-JHACDMF
              END-IF
           END-IF.

       2600-UPDATE-CONTROL.
           MOVE CN-JOB-ID TO CT-JOB-ID
           READ JHCTLKF KEY IS CT-JOB-ID
              INVALID KEY
                 MOVE 12 TO RETURN-CODE
                 SET HARD-ERROR TO TRUE
                 DISPLAY "JHCTLKF CONTROL LOST"
              NOT INVALID KEY
                 MOVE CHG-CHANGE-SEQ TO CT-RESTART-POS
                 MOVE WS-INPUT-CNT TO CT-INPUT-CNT
                 MOVE WS-OUTPUT-CNT TO CT-OUTPUT-CNT
                 REWRITE JHCTLKF-REC
                 IF FS-JHCTLKF NOT = "00"
                    MOVE 12 TO RETURN-CODE
                    SET HARD-ERROR TO TRUE
                    DISPLAY "JHCTLKF UPDATE ST=" FS-JHCTLKF
                 END-IF
           END-READ.

       8000-WRITE-AUDIT.
           ADD 1 TO WS-AUDIT-SEQ
           INITIALIZE JHAUDTF-REC
           MOVE WS-AUDIT-SEQ TO AUD-AUDIT-SEQ
           MOVE CN-JOB-ID TO AUD-JOB-ID
           MOVE WS-BUSINESS-DT TO AUD-BUSINESS-DT
           PERFORM 8200-SET-EVENT-TS
           MOVE WS-EVENT-TS TO AUD-EVENT-TS
           WRITE JHAUDTF-REC
           IF FS-JHAUDTF NOT = "00"
              MOVE 12 TO RETURN-CODE
              SET HARD-ERROR TO TRUE
              DISPLAY "JHAUDTF WRITE ST=" FS-JHAUDTF
           END-IF.

       8100-WRITE-START-AUDIT.
           MOVE CN-AUD-START TO AUD-EVENT-CD
           MOVE CN-DS-CTL TO AUD-DATASET-ID
           MOVE ZERO TO AUD-REC-CNT
           MOVE ZERO TO AUD-AMT-TOTAL
           PERFORM 8000-WRITE-AUDIT.

       8200-SET-EVENT-TS.
           ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD
           ACCEPT WS-CURRENT-DATE(9:8) FROM TIME
           STRING WS-CUR-YYYY DELIMITED BY SIZE
                  "-" DELIMITED BY SIZE
                  WS-CUR-MM DELIMITED BY SIZE
                  "-" DELIMITED BY SIZE
                  WS-CUR-DD DELIMITED BY SIZE
                  "T" DELIMITED BY SIZE
                  WS-CUR-HH DELIMITED BY SIZE
                  ":" DELIMITED BY SIZE
                  WS-CUR-MI DELIMITED BY SIZE
                  ":" DELIMITED BY SIZE
                  WS-CUR-SS DELIMITED BY SIZE
              INTO WS-EVENT-TS
           END-STRING.

       8300-WRITE-SKIP-AUDIT.
           MOVE CN-AUD-SKIP TO AUD-EVENT-CD
           MOVE CN-DS-CHG TO AUD-DATASET-ID
           MOVE 1 TO AUD-REC-CNT
           MOVE ZERO TO AUD-AMT-TOTAL
           PERFORM 8000-WRITE-AUDIT.

       8400-WRITE-ERROR-AUDIT.
           MOVE CN-AUD-ERR TO AUD-EVENT-CD
           MOVE CN-DS-CHG TO AUD-DATASET-ID
           MOVE 1 TO AUD-REC-CNT
           MOVE ZERO TO AUD-AMT-TOTAL
           PERFORM 8000-WRITE-AUDIT.

       8500-WRITE-CAPTURE-AUDIT.
           MOVE CN-AUD-CAPT TO AUD-EVENT-CD
           MOVE CN-DS-ACD TO AUD-DATASET-ID
           MOVE 1 TO AUD-REC-CNT
           MOVE ZERO TO AUD-AMT-TOTAL
           PERFORM 8000-WRITE-AUDIT.

       8600-WRITE-END-AUDIT.
           MOVE CN-AUD-END TO AUD-EVENT-CD
           MOVE CN-DS-AUD TO AUD-DATASET-ID
           MOVE WS-OUTPUT-CNT TO AUD-REC-CNT
           MOVE ZERO TO AUD-AMT-TOTAL
           PERFORM 8000-WRITE-AUDIT.

       9000-FINAL.
           IF NO-HARD-ERROR
              PERFORM 8600-WRITE-END-AUDIT
           END-IF

           IF FS-JHCTLKF = "00"
              MOVE CN-JOB-ID TO CT-JOB-ID
              READ JHCTLKF KEY IS CT-JOB-ID
                 INVALID KEY
                    MOVE 8 TO RETURN-CODE
                    SET HARD-ERROR TO TRUE
                    DISPLAY "JHCTLKF END TARGET LOST"
                 NOT INVALID KEY
                    IF HARD-ERROR
                       MOVE CN-STAT-ERR TO CT-STATUS-CD
                    ELSE
                       MOVE CN-STAT-END TO CT-STATUS-CD
                    END-IF
                    PERFORM 8200-SET-EVENT-TS
                    MOVE WS-EVENT-TS TO CT-END-TS
                    MOVE WS-INPUT-CNT TO CT-INPUT-CNT
                    MOVE WS-OUTPUT-CNT TO CT-OUTPUT-CNT
                    REWRITE JHCTLKF-REC
                    IF FS-JHCTLKF NOT = "00"
                       MOVE 12 TO RETURN-CODE
                       SET HARD-ERROR TO TRUE
                       DISPLAY "JHCTLKF END ST=" FS-JHCTLKF
                    END-IF
              END-READ
           END-IF

           IF FS-JHCHGEF NOT = SPACES
              CLOSE JHCHGEF
           END-IF
           IF FS-JHCTLKF NOT = SPACES
              CLOSE JHCTLKF
           END-IF
           IF FS-JHACDMF NOT = SPACES
              CLOSE JHACDMF
           END-IF
           IF FS-JHAUDTF NOT = SPACES
              CLOSE JHAUDTF
           END-IF

           IF HARD-ERROR
              IF RETURN-CODE = 0
                 MOVE 8 TO RETURN-CODE
              END-IF
              DISPLAY "JH420B ABEND INPUT=" WS-INPUT-CNT
                      " OUTPUT=" WS-OUTPUT-CNT
                      " SKIP=" WS-SKIP-CNT
                      " ERROR=" WS-ERROR-CNT
           ELSE
              MOVE 0 TO RETURN-CODE
              DISPLAY "JH420B NORMAL INPUT=" WS-INPUT-CNT
                      " OUTPUT=" WS-OUTPUT-CNT
                      " SKIP=" WS-SKIP-CNT
                      " ERROR=" WS-ERROR-CNT
           END-IF.
