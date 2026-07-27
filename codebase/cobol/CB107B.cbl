       IDENTIFICATION DIVISION.
       PROGRAM-ID. CB107B.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CDSTMTF2
               ASSIGN       TO "CDSTMTF2"
               ORGANIZATION IS INDEXED
               ACCESS MODE  IS SEQUENTIAL
               RECORD KEY   IS ST-CARD-NO
               FILE STATUS  IS FS-CDSTMTF2.

           SELECT CDMEMF
               ASSIGN       TO "CDMEMF"
               ORGANIZATION IS INDEXED
               ACCESS MODE  IS DYNAMIC
               RECORD KEY   IS MM-MEMBER-ID
               FILE STATUS  IS FS-CDMEMF.

           SELECT CDREVF
               ASSIGN       TO "CDREVF"
               ORGANIZATION IS INDEXED
               ACCESS MODE  IS DYNAMIC
               RECORD KEY   IS RV-CARD-NO
               FILE STATUS  IS FS-CDREVF.

           SELECT CDNOTIF
               ASSIGN       TO "CDNOTIF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE  IS SEQUENTIAL
               FILE STATUS  IS FS-CDNOTIF.

       DATA DIVISION.
       FILE SECTION.

       FD  CDSTMTF2.
           COPY CDSTMTF2C.

       FD  CDMEMF.
           COPY CDMEMC.

       FD  CDREVF.
           COPY CDREVFC.

       FD  CDNOTIF.
           COPY CDNOTIC.

       WORKING-STORAGE SECTION.
       01  FS-AREA.
           05 FS-CDSTMTF2          PIC X(02) VALUE SPACES.
           05 FS-CDMEMF            PIC X(02) VALUE SPACES.
           05 FS-CDREVF            PIC X(02) VALUE SPACES.
           05 FS-CDNOTIF           PIC X(02) VALUE SPACES.

       01  SW-AREA.
           05 SW-EOF-STMT          PIC X VALUE "N".
              88 EOF-STMT               VALUE "Y".
           05 SW-HARD-ERR          PIC X VALUE "N".
              88 HARD-ERR               VALUE "Y".
           05 SW-OPEN-STMT         PIC X VALUE "N".
              88 OPEN-STMT              VALUE "Y".
           05 SW-OPEN-MEM          PIC X VALUE "N".
              88 OPEN-MEM               VALUE "Y".
           05 SW-OPEN-REV          PIC X VALUE "N".
              88 OPEN-REV               VALUE "Y".
           05 SW-OPEN-NOTIF        PIC X VALUE "N".
              88 OPEN-NOTIF             VALUE "Y".

       01  CONST-AREA.
           05 CN-REV-ACTIVE        PIC X(02) VALUE "01".
           05 CN-STMT-FIXED        PIC X     VALUE "C".
           05 CN-MEMBER-LIMIT      PIC X(02) VALUE "04".
           05 CN-NOTIF-WAIT        PIC X     VALUE "0".
           05 CN-NOTIF-LIGHT       PIC X(02) VALUE "01".
           05 CN-NOTIF-MID         PIC X(02) VALUE "02".
           05 CN-NOTIF-HEAVY       PIC X(02) VALUE "03".
           05 CN-NOTIF-STOP        PIC X(02) VALUE "04".
           05 CN-FEE-RATE          PIC 9V9999 VALUE 0.0125.

       01  WORK-AREA.
           05 WS-RUN-DATE          PIC 9(08) VALUE ZERO.
           05 WS-RUN-DATE-X        PIC X(08) VALUE SPACES.
           05 WS-DUE-DATE          PIC 9(08) VALUE ZERO.
           05 WS-DELINQ-DAYS       PIC S9(05) COMP-3 VALUE ZERO.
           05 WS-DATE-RUN-INT      PIC S9(09) COMP VALUE ZERO.
           05 WS-DATE-DUE-INT      PIC S9(09) COMP VALUE ZERO.
           05 WS-FEE-AMT           PIC S9(11) COMP-3 VALUE ZERO.
           05 WS-NOTICE-AMT        PIC S9(11) COMP-3 VALUE ZERO.
           05 WS-NOTICE-TYPE       PIC X(02) VALUE SPACES.
           05 WS-NOTICE-ID-N       PIC 9(09) VALUE ZERO.
           05 WS-NOTICE-ID-X       PIC X(12) VALUE SPACES.
           05 WS-READ-CNT          PIC 9(09) VALUE ZERO.
           05 WS-SKIP-CNT          PIC 9(09) VALUE ZERO.
           05 WS-NOTIF-CNT         PIC 9(09) VALUE ZERO.
           05 WS-LIMIT-CNT         PIC 9(09) VALUE ZERO.
           05 WS-ERR-CNT           PIC 9(09) VALUE ZERO.

       PROCEDURE DIVISION.
       MAIN-RTN.
           MOVE 0 TO RETURN-CODE
           PERFORM INIT-RTN
           IF NOT HARD-ERR
               PERFORM UNTIL EOF-STMT OR HARD-ERR
                   PERFORM READ-STMT-RTN
                   IF NOT EOF-STMT AND NOT HARD-ERR
                       ADD 1 TO WS-READ-CNT
                       PERFORM PROCESS-STMT-RTN
                   END-IF
               END-PERFORM
           END-IF
           PERFORM TERM-RTN
           GOBACK.

       INIT-RTN.
           ACCEPT WS-RUN-DATE-X FROM DATE YYYYMMDD
           MOVE WS-RUN-DATE-X TO WS-RUN-DATE
           COMPUTE WS-DATE-RUN-INT =
               FUNCTION INTEGER-OF-DATE(WS-RUN-DATE)

           OPEN INPUT CDSTMTF2
           IF FS-CDSTMTF2 NOT = "00"
               DISPLAY "CDSTMTF2 OPEN ERROR ST=" FS-CDSTMTF2
               MOVE "Y" TO SW-HARD-ERR
               MOVE 8 TO RETURN-CODE
           ELSE
               MOVE "Y" TO SW-OPEN-STMT
           END-IF

           IF NOT HARD-ERR
               OPEN I-O CDMEMF
               IF FS-CDMEMF NOT = "00"
                   DISPLAY "CDMEMF OPEN ERROR ST=" FS-CDMEMF
                   MOVE "Y" TO SW-HARD-ERR
                   MOVE 8 TO RETURN-CODE
               ELSE
                   MOVE "Y" TO SW-OPEN-MEM
               END-IF
           END-IF

           IF NOT HARD-ERR
               OPEN INPUT CDREVF
               IF FS-CDREVF NOT = "00"
                   DISPLAY "CDREVF OPEN ERROR ST=" FS-CDREVF
                   MOVE "Y" TO SW-HARD-ERR
                   MOVE 8 TO RETURN-CODE
               ELSE
                   MOVE "Y" TO SW-OPEN-REV
               END-IF
           END-IF

           IF NOT HARD-ERR
               OPEN OUTPUT CDNOTIF
               IF FS-CDNOTIF NOT = "00"
                   DISPLAY "CDNOTIF OPEN ERROR ST=" FS-CDNOTIF
                   MOVE "Y" TO SW-HARD-ERR
                   MOVE 8 TO RETURN-CODE
               ELSE
                   MOVE "Y" TO SW-OPEN-NOTIF
               END-IF
           END-IF.

       READ-STMT-RTN.
           READ CDSTMTF2 NEXT RECORD
               AT END
                   MOVE "Y" TO SW-EOF-STMT
               NOT AT END
                   IF FS-CDSTMTF2 NOT = "00"
                       DISPLAY "CDSTMTF2 READ ERROR ST=" FS-CDSTMTF2
                       MOVE "Y" TO SW-HARD-ERR
                       MOVE 8 TO RETURN-CODE
                   END-IF
           END-READ.

       PROCESS-STMT-RTN.
           IF ST-CARD-NO = SPACES
               ADD 1 TO WS-SKIP-CNT
               EXIT PARAGRAPH
           END-IF

           IF ST-STMT-STATUS NOT = CN-STMT-FIXED
               ADD 1 TO WS-SKIP-CNT
               EXIT PARAGRAPH
           END-IF

           IF ST-BILL-AMT <= ZERO
               ADD 1 TO WS-SKIP-CNT
               EXIT PARAGRAPH
           END-IF

           MOVE ST-DUE-DT TO WS-DUE-DATE
           IF WS-DUE-DATE = ZERO
               DISPLAY "INVALID DUE DATE CARD=" ST-CARD-NO
               ADD 1 TO WS-ERR-CNT
               ADD 1 TO WS-SKIP-CNT
               EXIT PARAGRAPH
           END-IF

           COMPUTE WS-DATE-DUE-INT =
               FUNCTION INTEGER-OF-DATE(WS-DUE-DATE)
           COMPUTE WS-DELINQ-DAYS =
               WS-DATE-RUN-INT - WS-DATE-DUE-INT

           IF WS-DELINQ-DAYS <= ZERO
               ADD 1 TO WS-SKIP-CNT
               EXIT PARAGRAPH
           END-IF

           PERFORM READ-REV-RTN
           IF HARD-ERR
               EXIT PARAGRAPH
           END-IF

           IF FS-CDREVF = "23"
               ADD 1 TO WS-SKIP-CNT
               EXIT PARAGRAPH
           END-IF

           IF RV-REV-STATUS NOT = CN-REV-ACTIVE
               ADD 1 TO WS-SKIP-CNT
               EXIT PARAGRAPH
           END-IF

           PERFORM READ-MEMBER-RTN
           IF HARD-ERR
               EXIT PARAGRAPH
           END-IF

           IF FS-CDMEMF = "23"
               ADD 1 TO WS-SKIP-CNT
               EXIT PARAGRAPH
           END-IF

           PERFORM DECIDE-NOTICE-RTN
           PERFORM WRITE-NOTICE-RTN
           IF HARD-ERR
               EXIT PARAGRAPH
           END-IF

           IF WS-DELINQ-DAYS >= 90
               PERFORM UPDATE-MEMBER-RTN
           END-IF.

       READ-REV-RTN.
           MOVE ST-CARD-NO TO RV-CARD-NO
           READ CDREVF KEY IS RV-CARD-NO
               INVALID KEY
                   CONTINUE
               NOT INVALID KEY
                   IF FS-CDREVF NOT = "00"
                       DISPLAY "CDREVF READ ERROR ST=" FS-CDREVF
                       MOVE "Y" TO SW-HARD-ERR
                       MOVE 8 TO RETURN-CODE
                   END-IF
           END-READ.

       READ-MEMBER-RTN.
           MOVE RV-MEMBER-ID TO MM-MEMBER-ID
           READ CDMEMF KEY IS MM-MEMBER-ID
               INVALID KEY
                   CONTINUE
               NOT INVALID KEY
                   IF FS-CDMEMF NOT = "00"
                       DISPLAY "CDMEMF READ ERROR ST=" FS-CDMEMF
                       MOVE "Y" TO SW-HARD-ERR
                       MOVE 8 TO RETURN-CODE
                   END-IF
           END-READ.

       DECIDE-NOTICE-RTN.
           MOVE CN-NOTIF-LIGHT TO WS-NOTICE-TYPE

           IF WS-DELINQ-DAYS >= 30
               MOVE CN-NOTIF-MID TO WS-NOTICE-TYPE
           END-IF

           IF WS-DELINQ-DAYS >= 60
               MOVE CN-NOTIF-HEAVY TO WS-NOTICE-TYPE
           END-IF

           IF WS-DELINQ-DAYS >= 90
               MOVE CN-NOTIF-STOP TO WS-NOTICE-TYPE
           END-IF

           IF MM-MEMBER-STATUS = CN-MEMBER-LIMIT
               MOVE CN-NOTIF-STOP TO WS-NOTICE-TYPE
           END-IF

           COMPUTE WS-FEE-AMT =
               FUNCTION INTEGER(ST-BILL-AMT * CN-FEE-RATE)
           COMPUTE WS-NOTICE-AMT = ST-BILL-AMT + WS-FEE-AMT.

       WRITE-NOTICE-RTN.
           INITIALIZE CDNOTIF-REC
           ADD 1 TO WS-NOTICE-ID-N
           MOVE WS-NOTICE-ID-N TO WS-NOTICE-ID-X
           MOVE WS-NOTICE-ID-X TO NT-NOTICE-ID
           MOVE ST-CARD-NO TO NT-CARD-NO
           MOVE WS-RUN-DATE TO NT-NOTICE-DT
           MOVE WS-NOTICE-TYPE TO NT-NOTICE-TYPE
           MOVE WS-NOTICE-AMT TO NT-NOTICE-AMT
           MOVE CN-NOTIF-WAIT TO NT-NOTICE-STATUS

           WRITE CDNOTIF-REC
           IF FS-CDNOTIF NOT = "00"
               DISPLAY "CDNOTIF WRITE ERROR ST=" FS-CDNOTIF
               MOVE "Y" TO SW-HARD-ERR
               MOVE 8 TO RETURN-CODE
           ELSE
               ADD 1 TO WS-NOTIF-CNT
           END-IF.

       UPDATE-MEMBER-RTN.
           IF MM-MEMBER-STATUS NOT = CN-MEMBER-LIMIT
               MOVE CN-MEMBER-LIMIT TO MM-MEMBER-STATUS
               MOVE WS-RUN-DATE TO MM-LAST-STATUS-DT
               REWRITE CDMEMF-REC
               IF FS-CDMEMF NOT = "00"
                   DISPLAY "CDMEMF REWRITE ERROR ST=" FS-CDMEMF
                   MOVE "Y" TO SW-HARD-ERR
                   MOVE 8 TO RETURN-CODE
               ELSE
                   ADD 1 TO WS-LIMIT-CNT
               END-IF
           END-IF.

       TERM-RTN.
           PERFORM CLOSE-FILES-RTN

           DISPLAY "CB107B READ COUNT=" WS-READ-CNT
           DISPLAY "CB107B NOTICE COUNT=" WS-NOTIF-CNT
           DISPLAY "CB107B LIMIT COUNT=" WS-LIMIT-CNT
           DISPLAY "CB107B SKIP COUNT=" WS-SKIP-CNT
           DISPLAY "CB107B WARN COUNT=" WS-ERR-CNT

           IF HARD-ERR
               IF RETURN-CODE = 0
                   MOVE 8 TO RETURN-CODE
               END-IF
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       CLOSE-FILES-RTN.
           IF OPEN-NOTIF
               CLOSE CDNOTIF
               IF FS-CDNOTIF NOT = "00"
                   DISPLAY "CDNOTIF CLOSE ERROR ST=" FS-CDNOTIF
                   MOVE "Y" TO SW-HARD-ERR
                   MOVE 8 TO RETURN-CODE
               END-IF
           END-IF

           IF OPEN-REV
               CLOSE CDREVF
               IF FS-CDREVF NOT = "00"
                   DISPLAY "CDREVF CLOSE ERROR ST=" FS-CDREVF
                   MOVE "Y" TO SW-HARD-ERR
                   MOVE 8 TO RETURN-CODE
               END-IF
           END-IF

           IF OPEN-MEM
               CLOSE CDMEMF
               IF FS-CDMEMF NOT = "00"
                   DISPLAY "CDMEMF CLOSE ERROR ST=" FS-CDMEMF
                   MOVE "Y" TO SW-HARD-ERR
                   MOVE 8 TO RETURN-CODE
               END-IF
           END-IF

           IF OPEN-STMT
               CLOSE CDSTMTF2
               IF FS-CDSTMTF2 NOT = "00"
                   DISPLAY "CDSTMTF2 CLOSE ERROR ST=" FS-CDSTMTF2
                   MOVE "Y" TO SW-HARD-ERR
                   MOVE 8 TO RETURN-CODE
               END-IF
           END-IF.
