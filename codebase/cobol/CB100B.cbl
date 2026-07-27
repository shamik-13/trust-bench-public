       IDENTIFICATION DIVISION.
       PROGRAM-ID. CB100B.
       AUTHOR. CARD-BATCH.
      ******************************************************************
      * オーソリ保留期限切れバッチ
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CDAUTHF ASSIGN TO "CDAUTHF"
              ORGANIZATION IS SEQUENTIAL
              ACCESS MODE IS SEQUENTIAL
              FILE STATUS IS WS-CDAUTHF-ST.

       DATA DIVISION.
       FILE SECTION.
       FD  CDAUTHF
           RECORD CONTAINS 256 CHARACTERS
           DATA RECORD IS CDAUTHF-REC.
           COPY CDAUTHFC.

       WORKING-STORAGE SECTION.
       01  WS-CDAUTHF-ST              PIC XX VALUE "00".
       01  WS-END-FLG                 PIC X  VALUE "N".
           88  END-OF-CDAUTHF                VALUE "Y".
           88  NOT-END-OF-CDAUTHF            VALUE "N".

       01  WS-ABEND-FLG               PIC X  VALUE "N".
           88  ABEND-OCCURRED                VALUE "Y".
           88  NO-ABEND                      VALUE "N".

       01  WS-CURRENT-DATE.
           05  WS-CUR-YYYY            PIC 9(04).
           05  WS-CUR-MM              PIC 9(02).
           05  WS-CUR-DD              PIC 9(02).
       01  WS-PROCESS-DATE            PIC 9(08).

       01  WS-PREV-AUTH-ID            PIC X(20) VALUE SPACES.
       01  WS-PREV-CARD-NO            PIC X(19) VALUE SPACES.
       01  WS-PREV-MERCHANT-CODE      PIC X(15) VALUE SPACES.
       01  WS-PREV-AUTH-AMT           PIC S9(13)V99 COMP-3 VALUE 0.
       01  WS-HAVE-PREV-FLG           PIC X VALUE "N".
           88  HAVE-PREV-AUTH                VALUE "Y".
           88  NO-PREV-AUTH                  VALUE "N".

       01  WS-READ-CNT                PIC 9(09) VALUE 0.
       01  WS-CAND-CNT                PIC 9(09) VALUE 0.
       01  WS-EXPIRED-CNT             PIC 9(09) VALUE 0.
       01  WS-SKIP-CANCEL-CNT         PIC 9(09) VALUE 0.
       01  WS-SKIP-SALES-CNT          PIC 9(09) VALUE 0.
       01  WS-SKIP-ERR-CNT            PIC 9(09) VALUE 0.
       01  WS-REWRITE-CNT             PIC 9(09) VALUE 0.

       01  WS-WORK-RESULT             PIC XX VALUE SPACES.
       01  WS-VALID-FLG               PIC X  VALUE "N".
           88  RECORD-VALID                  VALUE "Y".
           88  RECORD-INVALID                VALUE "N".

       01  WS-DATE-WORK.
           05  WS-DATE-YYYY           PIC 9(04).
           05  WS-DATE-MM             PIC 9(02).
           05  WS-DATE-DD             PIC 9(02).
       01  WS-MAX-DAY                 PIC 9(02) VALUE 0.

       01  WS-DISPLAY-NUM             PIC Z(09).

       PROCEDURE DIVISION.
       MAIN-PROCESS.
           MOVE 8 TO RETURN-CODE
           PERFORM 1000-INITIALIZE
           IF NO-ABEND
              PERFORM 2000-MAIN-LOOP
           END-IF
           PERFORM 9000-TERMINATE
           GOBACK.

       1000-INITIALIZE.
           ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD
           MOVE WS-CURRENT-DATE TO WS-PROCESS-DATE
           DISPLAY "CB100B START DATE=" WS-PROCESS-DATE

           OPEN I-O CDAUTHF
           IF WS-CDAUTHF-ST NOT = "00"
              DISPLAY "CDAUTHF OPEN ERR ST=" WS-CDAUTHF-ST
              SET ABEND-OCCURRED TO TRUE
           ELSE
              SET NOT-END-OF-CDAUTHF TO TRUE
              SET NO-PREV-AUTH TO TRUE
           END-IF.

       2000-MAIN-LOOP.
           PERFORM UNTIL END-OF-CDAUTHF OR ABEND-OCCURRED
              READ CDAUTHF
                 AT END
                    SET END-OF-CDAUTHF TO TRUE
                 NOT AT END
                    ADD 1 TO WS-READ-CNT
                    PERFORM 3000-PROCESS-RECORD
              END-READ
              IF WS-CDAUTHF-ST NOT = "00" AND
                 WS-CDAUTHF-ST NOT = "10"
                 DISPLAY "CDAUTHF READ ERR ST=" WS-CDAUTHF-ST
                 SET ABEND-OCCURRED TO TRUE
              END-IF
           END-PERFORM.

       3000-PROCESS-RECORD.
           PERFORM 3100-CHECK-AUTH-UNIT

           IF RECORD-VALID
              EVALUATE AU-AUTH-RESULT
                 WHEN "20"
                    ADD 1 TO WS-SKIP-CANCEL-CNT
                 WHEN "30"
                    ADD 1 TO WS-SKIP-SALES-CNT
                 WHEN "00"
                    PERFORM 3200-CHECK-HOLD-EXPIRY
                 WHEN OTHER
                    DISPLAY "BAD RESULT"
                    DISPLAY "AUTH-ID=" AU-AUTH-ID
                    ADD 1 TO WS-SKIP-ERR-CNT
              END-EVALUATE
           ELSE
              ADD 1 TO WS-SKIP-ERR-CNT
           END-IF.

       3100-CHECK-AUTH-UNIT.
           SET RECORD-VALID TO TRUE

           IF AU-AUTH-ID = SPACES
              DISPLAY "AUTH-ID EMPTY"
              SET RECORD-INVALID TO TRUE
           END-IF

           IF AU-CARD-NO = SPACES
              DISPLAY "CARD-NO EMPTY"
              DISPLAY "AUTH-ID=" AU-AUTH-ID
              SET RECORD-INVALID TO TRUE
           END-IF

           IF AU-AUTH-AMT <= 0
              DISPLAY "AUTH-AMT BAD"
              DISPLAY "AUTH-ID=" AU-AUTH-ID
              SET RECORD-INVALID TO TRUE
           END-IF

           IF AU-CURRENCY-CD NOT = "JPY"
              DISPLAY "CURRENCY BAD"
              DISPLAY "AUTH-ID=" AU-AUTH-ID
              SET RECORD-INVALID TO TRUE
           END-IF

           IF AU-HOLD-EXP-DT NOT NUMERIC
              DISPLAY "HOLD-EXP-DT NONNUM"
              DISPLAY "AUTH-ID=" AU-AUTH-ID
              SET RECORD-INVALID TO TRUE
           ELSE
              MOVE AU-HOLD-EXP-DT TO WS-DATE-WORK
              PERFORM 3110-CHECK-DATE
              IF RECORD-INVALID
                 DISPLAY "HOLD-EXP-DT BAD"
                 DISPLAY "AUTH-ID=" AU-AUTH-ID
              END-IF
           END-IF

           IF RECORD-VALID
              IF HAVE-PREV-AUTH AND
                 AU-AUTH-ID = WS-PREV-AUTH-ID
                 IF AU-CARD-NO NOT = WS-PREV-CARD-NO OR
                    AU-MERCHANT-CODE NOT = WS-PREV-MERCHANT-CODE OR
                    AU-AUTH-AMT NOT = WS-PREV-AUTH-AMT
                    DISPLAY "AUTH-ID CONSISTENCY BAD"
                    DISPLAY "AUTH-ID=" AU-AUTH-ID
                    SET RECORD-INVALID TO TRUE
                 END-IF
              ELSE
                 MOVE AU-AUTH-ID TO WS-PREV-AUTH-ID
                 MOVE AU-CARD-NO TO WS-PREV-CARD-NO
                 MOVE AU-MERCHANT-CODE TO WS-PREV-MERCHANT-CODE
                 MOVE AU-AUTH-AMT TO WS-PREV-AUTH-AMT
                 SET HAVE-PREV-AUTH TO TRUE
              END-IF
           END-IF.

       3110-CHECK-DATE.
           IF WS-DATE-YYYY < 2000 OR
              WS-DATE-YYYY > 2099
              SET RECORD-INVALID TO TRUE
           END-IF

           EVALUATE WS-DATE-MM
              WHEN 01
              WHEN 03
              WHEN 05
              WHEN 07
              WHEN 08
              WHEN 10
              WHEN 12
                 MOVE 31 TO WS-MAX-DAY
              WHEN 04
              WHEN 06
              WHEN 09
              WHEN 11
                 MOVE 30 TO WS-MAX-DAY
              WHEN 02
                 PERFORM 3120-SET-FEB-DAY
              WHEN OTHER
                 MOVE 0 TO WS-MAX-DAY
                 SET RECORD-INVALID TO TRUE
           END-EVALUATE

           IF WS-DATE-DD < 1 OR
              WS-DATE-DD > WS-MAX-DAY
              SET RECORD-INVALID TO TRUE
           END-IF.

       3120-SET-FEB-DAY.
           IF FUNCTION MOD(WS-DATE-YYYY 400) = 0
              MOVE 29 TO WS-MAX-DAY
           ELSE
              IF FUNCTION MOD(WS-DATE-YYYY 100) = 0
                 MOVE 28 TO WS-MAX-DAY
              ELSE
                 IF FUNCTION MOD(WS-DATE-YYYY 4) = 0
                    MOVE 29 TO WS-MAX-DAY
                 ELSE
                    MOVE 28 TO WS-MAX-DAY
                 END-IF
              END-IF
           END-IF.

       3200-CHECK-HOLD-EXPIRY.
           IF AU-HOLD-EXP-DT <= WS-PROCESS-DATE
              IF AU-HOLD-EXP-DT = WS-PROCESS-DATE
                 MOVE "40" TO WS-WORK-RESULT
                 ADD 1 TO WS-CAND-CNT
              ELSE
                 MOVE "90" TO WS-WORK-RESULT
                 ADD 1 TO WS-EXPIRED-CNT
              END-IF
              PERFORM 3300-REWRITE-AUTH
           END-IF.

       3300-REWRITE-AUTH.
           MOVE WS-WORK-RESULT TO AU-AUTH-RESULT
           REWRITE CDAUTHF-REC
           IF WS-CDAUTHF-ST = "00"
              ADD 1 TO WS-REWRITE-CNT
           ELSE
              DISPLAY "CDAUTHF REWRITE ERR"
              DISPLAY "AUTH-ID=" AU-AUTH-ID
              DISPLAY "ST=" WS-CDAUTHF-ST
              SET ABEND-OCCURRED TO TRUE
           END-IF.

       9000-TERMINATE.
           IF WS-CDAUTHF-ST = "00" OR
              WS-CDAUTHF-ST = "10"
              CLOSE CDAUTHF
              IF WS-CDAUTHF-ST NOT = "00"
                 DISPLAY "CDAUTHF CLOSE ERR ST=" WS-CDAUTHF-ST
                 SET ABEND-OCCURRED TO TRUE
              END-IF
           END-IF

           MOVE WS-READ-CNT TO WS-DISPLAY-NUM
           DISPLAY "READ-CNT=" WS-DISPLAY-NUM
           MOVE WS-REWRITE-CNT TO WS-DISPLAY-NUM
           DISPLAY "REWRITE-CNT=" WS-DISPLAY-NUM
           MOVE WS-CAND-CNT TO WS-DISPLAY-NUM
           DISPLAY "CAND-CNT=" WS-DISPLAY-NUM
           MOVE WS-EXPIRED-CNT TO WS-DISPLAY-NUM
           DISPLAY "EXPIRED-CNT=" WS-DISPLAY-NUM
           MOVE WS-SKIP-CANCEL-CNT TO WS-DISPLAY-NUM
           DISPLAY "SKIP-CANCEL-CNT=" WS-DISPLAY-NUM
           MOVE WS-SKIP-SALES-CNT TO WS-DISPLAY-NUM
           DISPLAY "SKIP-SALES-CNT=" WS-DISPLAY-NUM
           MOVE WS-SKIP-ERR-CNT TO WS-DISPLAY-NUM
           DISPLAY "SKIP-ERR-CNT=" WS-DISPLAY-NUM

           IF ABEND-OCCURRED
              MOVE 8 TO RETURN-CODE
              DISPLAY "CB100B ABEND RC=8"
           ELSE
              MOVE 0 TO RETURN-CODE
              DISPLAY "CB100B END RC=0"
           END-IF.
