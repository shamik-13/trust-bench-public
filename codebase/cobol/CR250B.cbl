       IDENTIFICATION DIVISION.
       PROGRAM-ID. CR250B.
       AUTHOR.     GYOMU-KAIHATSU.
      *================================================================*
      *  グループ送金送出バッチ
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CCXFRF
               ASSIGN       TO "CCXFRF"
               ORGANIZATION IS INDEXED
               ACCESS MODE  IS DYNAMIC
               RECORD KEY   IS XF-XFER-ID
               FILE STATUS  IS FS-CCXFRF.

           SELECT CCCALF
               ASSIGN       TO "CCCALF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS  IS FS-CCCALF.

           SELECT CCERRF
               ASSIGN       TO "CCERRF"
               ORGANIZATION IS INDEXED
               ACCESS MODE  IS DYNAMIC
               RECORD KEY   IS ER-ERROR-ID
               FILE STATUS  IS FS-CCERRF.

           SELECT SORTWK
               ASSIGN       TO "SORTWK".

       DATA DIVISION.
       FILE SECTION.
       FD  CCXFRF.
           COPY CCXFRC.

       FD  CCCALF.
           COPY CCCALFC.

       FD  CCERRF.
           COPY CCERRC.

       SD  SORTWK.
       01  SORT-REC.
           05  S-VALUE-DT              PIC 9(8).
           05  S-TO-ORG-CD             PIC X(10).
           05  S-XFER-AMT              PIC S9(13)V99 COMP-3.
           05  S-XFER-ID               PIC X(20).
           05  S-FROM-ORG-CD           PIC X(10).
           05  S-XFER-STATUS-KBN       PIC X(2).

       WORKING-STORAGE SECTION.
       01  FS-AREA.
           05  FS-CCXFRF               PIC XX VALUE SPACES.
           05  FS-CCCALF               PIC XX VALUE SPACES.
           05  FS-CCERRF               PIC XX VALUE SPACES.

       01  SW-AREA.
           05  EOF-CCXFRF              PIC X VALUE "N".
           05  EOF-CCCALF              PIC X VALUE "N".
           05  EOF-SORT                PIC X VALUE "N".
           05  ABEND-SW                PIC X VALUE "N".
           05  CAL-FOUND-SW            PIC X VALUE "N".
           05  VALID-DATE-SW           PIC X VALUE "N".
           05  SEND-ALLOW-SW           PIC X VALUE "N".

       01  CONST-AREA.
           05  C-PGM-ID                PIC X(8) VALUE "CR250B".
           05  C-ST-MISOSHIN           PIC X(2) VALUE "00".
           05  C-ST-SOSHINZUMI         PIC X(2) VALUE "20".
           05  C-HOLIDAY               PIC X VALUE "Y".
           05  C-BUSINESS              PIC X VALUE "N".
           05  C-MAX-SEND-CNT          PIC 9(7) VALUE 0500000.

       01  COUNT-AREA.
           05  IN-CNT                  PIC 9(9) VALUE ZERO.
           05  SORT-CNT                PIC 9(9) VALUE ZERO.
           05  SEND-CNT                PIC 9(9) VALUE ZERO.
           05  ERR-CNT                 PIC 9(9) VALUE ZERO.
           05  SKIP-CNT                PIC 9(9) VALUE ZERO.
           05  CAL-CNT                 PIC 9(5) VALUE ZERO.
           05  ERR-SEQ                 PIC 9(9) VALUE ZERO.

       01  HOLD-AREA.
           05  H-PREV-XFER-ID          PIC X(20) VALUE SPACES.
           05  H-ERR-KEY               PIC X(30) VALUE SPACES.
           05  H-ERR-KBN               PIC X(3) VALUE SPACES.
           05  H-ERR-TEXT              PIC X(80) VALUE SPACES.
           05  H-START-KEY             PIC X(20) VALUE LOW-VALUES.

       01  DATE-WORK.
           05  W-DATE                  PIC 9(8) VALUE ZERO.
           05  W-YYYY                  PIC 9(4) VALUE ZERO.
           05  W-MM                    PIC 9(2) VALUE ZERO.
           05  W-DD                    PIC 9(2) VALUE ZERO.
           05  W-LAST-DD               PIC 9(2) VALUE ZERO.
           05  W-REM                   PIC 9(4) VALUE ZERO.
           05  W-DUMMY                 PIC 9(4) VALUE ZERO.

       01  CAL-TABLE-AREA.
           05  CAL-MAX                 PIC 9(5) VALUE 20000.
           05  CAL-TBL OCCURS 20000 TIMES
                         ASCENDING KEY IS T-CAL-DT
                         INDEXED BY CAL-IDX.
               10 T-CAL-DT             PIC 9(8).
               10 T-HOLIDAY-FLAG       PIC X.

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE 0 TO RETURN-CODE
           PERFORM 1000-OPEN
           IF ABEND-SW = "N"
               PERFORM 2000-LOAD-CALENDAR
           END-IF
           IF ABEND-SW = "N"
               SORT SORTWK
                    ON ASCENDING KEY S-VALUE-DT
                                     S-TO-ORG-CD
                                     S-XFER-AMT
                                     S-XFER-ID
                    INPUT  PROCEDURE 3000-SORT-INPUT
                    OUTPUT PROCEDURE 4000-SORT-OUTPUT
           END-IF
           PERFORM 9000-CLOSE
           IF ABEND-SW = "Y"
               MOVE 12 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF
           DISPLAY "CR250B END IN=" IN-CNT
                   " SEND=" SEND-CNT
                   " ERR=" ERR-CNT
                   " SKIP=" SKIP-CNT
           GOBACK.

       1000-OPEN.
           OPEN I-O CCXFRF
           IF FS-CCXFRF NOT = "00"
               DISPLAY "CCXFRF OPEN ERROR ST=" FS-CCXFRF
               MOVE "Y" TO ABEND-SW
           END-IF

           IF ABEND-SW = "N"
               OPEN INPUT CCCALF
               IF FS-CCCALF NOT = "00"
                   DISPLAY "CCCALF OPEN ERROR ST=" FS-CCCALF
                   MOVE "Y" TO ABEND-SW
               END-IF
           END-IF

           IF ABEND-SW = "N"
               OPEN I-O CCERRF
               IF FS-CCERRF NOT = "00"
                   DISPLAY "CCERRF OPEN ERROR ST=" FS-CCERRF
                   MOVE "Y" TO ABEND-SW
               END-IF
           END-IF.

       2000-LOAD-CALENDAR.
           PERFORM UNTIL EOF-CCCALF = "Y" OR ABEND-SW = "Y"
               READ CCCALF
                   AT END
                       MOVE "Y" TO EOF-CCCALF
                   NOT AT END
                       IF FS-CCCALF = "00"
                           PERFORM 2100-STORE-CALENDAR
                       ELSE
                           DISPLAY "CCCALF READ ERROR ST=" FS-CCCALF
                           MOVE "Y" TO ABEND-SW
                       END-IF
               END-READ
           END-PERFORM.

       2100-STORE-CALENDAR.
           IF CAL-CNT >= CAL-MAX
               DISPLAY "CCCALF TABLE OVERFLOW"
               MOVE "Y" TO ABEND-SW
           ELSE
               ADD 1 TO CAL-CNT
               MOVE CL-CAL-DT       TO T-CAL-DT(CAL-CNT)
               MOVE CL-HOLIDAY-FLAG TO T-HOLIDAY-FLAG(CAL-CNT)
           END-IF.

       3000-SORT-INPUT.
           MOVE "N" TO EOF-CCXFRF
           MOVE H-START-KEY TO XF-XFER-ID
           START CCXFRF KEY IS NOT LESS THAN XF-XFER-ID
           IF FS-CCXFRF = "23"
               MOVE "Y" TO EOF-CCXFRF
           ELSE
               IF FS-CCXFRF NOT = "00"
                   DISPLAY "CCXFRF START ERROR ST=" FS-CCXFRF
                   MOVE "Y" TO ABEND-SW
               END-IF
           END-IF

           PERFORM UNTIL EOF-CCXFRF = "Y" OR ABEND-SW = "Y"
               READ CCXFRF NEXT RECORD
                   AT END
                       MOVE "Y" TO EOF-CCXFRF
                   NOT AT END
                       IF FS-CCXFRF = "00"
                           ADD 1 TO IN-CNT
                           PERFORM 3100-EDIT-AND-RELEASE
                       ELSE
                           DISPLAY "CCXFRF NEXT ERROR ST=" FS-CCXFRF
                           MOVE "Y" TO ABEND-SW
                       END-IF
               END-READ
           END-PERFORM.

       3100-EDIT-AND-RELEASE.
           MOVE "N" TO SEND-ALLOW-SW
           MOVE XF-XFER-ID TO H-ERR-KEY

           EVALUATE TRUE
               WHEN XF-XFER-ID = SPACES
                   MOVE "101" TO H-ERR-KBN
                   MOVE "XFER ID REQUIRED" TO H-ERR-TEXT
                   PERFORM 8200-WRITE-ERROR
               WHEN XF-FROM-ORG-CD = SPACES
                   MOVE "102" TO H-ERR-KBN
                   MOVE "FROM ORG REQUIRED" TO H-ERR-TEXT
                   PERFORM 8200-WRITE-ERROR
               WHEN XF-TO-ORG-CD = SPACES
                   MOVE "103" TO H-ERR-KBN
                   MOVE "TO ORG REQUIRED" TO H-ERR-TEXT
                   PERFORM 8200-WRITE-ERROR
               WHEN XF-FROM-ORG-CD = XF-TO-ORG-CD
                   MOVE "104" TO H-ERR-KBN
                   MOVE "SAME ORG TRANSFER" TO H-ERR-TEXT
                   PERFORM 8200-WRITE-ERROR
               WHEN XF-XFER-AMT <= ZERO
                   MOVE "105" TO H-ERR-KBN
                   MOVE "INVALID AMOUNT" TO H-ERR-TEXT
                   PERFORM 8200-WRITE-ERROR
               WHEN XF-XFER-STATUS-KBN NOT = C-ST-MISOSHIN
                   MOVE "106" TO H-ERR-KBN
                   MOVE "STATUS IS NOT UNSENT" TO H-ERR-TEXT
                   PERFORM 8200-WRITE-ERROR
               WHEN OTHER
                   PERFORM 3200-CHECK-VALUE-DATE
           END-EVALUATE

           IF SEND-ALLOW-SW = "Y"
               MOVE XF-VALUE-DT        TO S-VALUE-DT
               MOVE XF-TO-ORG-CD       TO S-TO-ORG-CD
               MOVE XF-XFER-AMT        TO S-XFER-AMT
               MOVE XF-XFER-ID         TO S-XFER-ID
               MOVE XF-FROM-ORG-CD     TO S-FROM-ORG-CD
               MOVE XF-XFER-STATUS-KBN TO S-XFER-STATUS-KBN
               RELEASE SORT-REC
               ADD 1 TO SORT-CNT
           END-IF.

       3200-CHECK-VALUE-DATE.
           MOVE XF-VALUE-DT TO W-DATE
           PERFORM 7000-CHECK-DATE
           IF VALID-DATE-SW NOT = "Y"
               MOVE "201" TO H-ERR-KBN
               MOVE "INVALID VALUE DATE" TO H-ERR-TEXT
               PERFORM 8200-WRITE-ERROR
           ELSE
               PERFORM 7100-FIND-CALENDAR
               IF CAL-FOUND-SW NOT = "Y"
                   MOVE "202" TO H-ERR-KBN
                   MOVE "CALENDAR NOT FOUND" TO H-ERR-TEXT
                   PERFORM 8200-WRITE-ERROR
               ELSE
                   IF T-HOLIDAY-FLAG(CAL-IDX) = C-HOLIDAY
                       MOVE "203" TO H-ERR-KBN
                       MOVE "VALUE DATE IS HOLIDAY" TO H-ERR-TEXT
                       PERFORM 8200-WRITE-ERROR
                   ELSE
                       IF T-HOLIDAY-FLAG(CAL-IDX) = C-BUSINESS
                           MOVE "Y" TO SEND-ALLOW-SW
                       ELSE
                           MOVE "204" TO H-ERR-KBN
                           MOVE "INVALID BUSINESS FLAG" TO H-ERR-TEXT
                           PERFORM 8200-WRITE-ERROR
                       END-IF
                   END-IF
               END-IF
           END-IF.

       4000-SORT-OUTPUT.
           MOVE "N" TO EOF-SORT
           MOVE SPACES TO H-PREV-XFER-ID
           PERFORM UNTIL EOF-SORT = "Y" OR ABEND-SW = "Y"
               RETURN SORTWK
                   AT END
                       MOVE "Y" TO EOF-SORT
                   NOT AT END
                       PERFORM 4100-DECIDE-SEND
               END-RETURN
           END-PERFORM.

       4100-DECIDE-SEND.
           MOVE S-XFER-ID TO H-ERR-KEY
           IF S-XFER-ID = H-PREV-XFER-ID
               MOVE "301" TO H-ERR-KBN
               MOVE "DUPLICATE XFER ID" TO H-ERR-TEXT
               PERFORM 8200-WRITE-ERROR
           ELSE
               IF SEND-CNT >= C-MAX-SEND-CNT
                   MOVE "302" TO H-ERR-KBN
                   MOVE "SEND LIMIT REACHED" TO H-ERR-TEXT
                   PERFORM 8200-WRITE-ERROR
               ELSE
                   PERFORM 4200-UPDATE-XFER
               END-IF
           END-IF
           MOVE S-XFER-ID TO H-PREV-XFER-ID.

       4200-UPDATE-XFER.
           MOVE S-XFER-ID TO XF-XFER-ID
           READ CCXFRF KEY IS XF-XFER-ID
           IF FS-CCXFRF = "00"
               IF XF-XFER-STATUS-KBN = C-ST-MISOSHIN
                   MOVE C-ST-SOSHINZUMI TO XF-XFER-STATUS-KBN
                   REWRITE CCXFRF-REC
                   IF FS-CCXFRF = "00"
                       ADD 1 TO SEND-CNT
                   ELSE
                       DISPLAY "CCXFRF REWRITE ERROR ST=" FS-CCXFRF
                       MOVE "Y" TO ABEND-SW
                   END-IF
               ELSE
                   MOVE "303" TO H-ERR-KBN
                   MOVE "STATUS CHANGED BEFORE UPDATE" TO H-ERR-TEXT
                   PERFORM 8200-WRITE-ERROR
               END-IF
           ELSE
               IF FS-CCXFRF = "23"
                   MOVE "304" TO H-ERR-KBN
                   MOVE "UPDATE TARGET NOT FOUND" TO H-ERR-TEXT
                   PERFORM 8200-WRITE-ERROR
               ELSE
                   DISPLAY "CCXFRF READ ERROR ST=" FS-CCXFRF
                   MOVE "Y" TO ABEND-SW
               END-IF
           END-IF.

       7000-CHECK-DATE.
           MOVE "N" TO VALID-DATE-SW
           MOVE W-DATE(1:4) TO W-YYYY
           MOVE W-DATE(5:2) TO W-MM
           MOVE W-DATE(7:2) TO W-DD

           IF W-YYYY < 1900 OR W-MM < 1 OR W-MM > 12
               EXIT PARAGRAPH
           END-IF

           EVALUATE W-MM
               WHEN 1
                   MOVE 31 TO W-LAST-DD
               WHEN 2
                   PERFORM 7010-SET-FEB-DD
               WHEN 3
                   MOVE 31 TO W-LAST-DD
               WHEN 4
                   MOVE 30 TO W-LAST-DD
               WHEN 5
                   MOVE 31 TO W-LAST-DD
               WHEN 6
                   MOVE 30 TO W-LAST-DD
               WHEN 7
                   MOVE 31 TO W-LAST-DD
               WHEN 8
                   MOVE 31 TO W-LAST-DD
               WHEN 9
                   MOVE 30 TO W-LAST-DD
               WHEN 10
                   MOVE 31 TO W-LAST-DD
               WHEN 11
                   MOVE 30 TO W-LAST-DD
               WHEN 12
                   MOVE 31 TO W-LAST-DD
           END-EVALUATE

           IF W-DD >= 1 AND W-DD <= W-LAST-DD
               MOVE "Y" TO VALID-DATE-SW
           END-IF.

       7010-SET-FEB-DD.
           MOVE 28 TO W-LAST-DD
           DIVIDE W-YYYY BY 400 GIVING W-DUMMY REMAINDER W-REM
           IF W-REM = ZERO
               MOVE 29 TO W-LAST-DD
           ELSE
               DIVIDE W-YYYY BY 4 GIVING W-DUMMY REMAINDER W-REM
               IF W-REM = ZERO
                   DIVIDE W-YYYY BY 100 GIVING W-DUMMY
                       REMAINDER W-REM
                   IF W-REM NOT = ZERO
                       MOVE 29 TO W-LAST-DD
                   END-IF
               END-IF
           END-IF.

       7100-FIND-CALENDAR.
           MOVE "N" TO CAL-FOUND-SW
           IF CAL-CNT = ZERO
               EXIT PARAGRAPH
           END-IF

           SET CAL-IDX TO 1
           SEARCH ALL CAL-TBL
               AT END
                   MOVE "N" TO CAL-FOUND-SW
               WHEN T-CAL-DT(CAL-IDX) = XF-VALUE-DT
                   MOVE "Y" TO CAL-FOUND-SW
           END-SEARCH.

       8200-WRITE-ERROR.
           ADD 1 TO ERR-CNT
           ADD 1 TO ERR-SEQ
           MOVE ERR-SEQ      TO ER-ERROR-ID
           MOVE C-PGM-ID     TO ER-PGM-ID
           MOVE XF-VALUE-DT  TO ER-BASE-DT
           MOVE H-ERR-KEY    TO ER-RECORD-KEY
           MOVE H-ERR-KBN    TO ER-ERROR-KBN
           MOVE H-ERR-TEXT   TO ER-ERROR-TEXT
           WRITE CCERRF-REC
           IF FS-CCERRF = "00"
               ADD 1 TO SKIP-CNT
           ELSE
               DISPLAY "CCERRF WRITE ERROR ST=" FS-CCERRF
               MOVE "Y" TO ABEND-SW
           END-IF.

       9000-CLOSE.
           IF FS-CCXFRF NOT = SPACES
               CLOSE CCXFRF
               IF FS-CCXFRF NOT = "00"
                   DISPLAY "CCXFRF CLOSE ERROR ST=" FS-CCXFRF
                   MOVE "Y" TO ABEND-SW
               END-IF
           END-IF

           IF FS-CCCALF NOT = SPACES
               CLOSE CCCALF
               IF FS-CCCALF NOT = "00"
                   DISPLAY "CCCALF CLOSE ERROR ST=" FS-CCCALF
                   MOVE "Y" TO ABEND-SW
               END-IF
           END-IF

           IF FS-CCERRF NOT = SPACES
               CLOSE CCERRF
               IF FS-CCERRF NOT = "00"
                   DISPLAY "CCERRF CLOSE ERROR ST=" FS-CCERRF
                   MOVE "Y" TO ABEND-SW
               END-IF
           END-IF.
