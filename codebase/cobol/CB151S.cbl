       IDENTIFICATION DIVISION.
       PROGRAM-ID. CB151S.
       AUTHOR. TRUST-BATCH.
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.
      *
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CDOSF
               ASSIGN       TO "CDOSF"
               ORGANIZATION IS INDEXED
               ACCESS MODE  IS DYNAMIC
               RECORD KEY   IS OS-CARD-NO
               FILE STATUS  IS WK-CDOSF-ST.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  CDOSF.
           COPY CDOSFC.
      *
       WORKING-STORAGE SECTION.
       01  WK-STATUS-AREA.
           05 WK-CDOSF-ST              PIC XX VALUE SPACE.
           05 WK-HARD-ERR-SW           PIC X  VALUE '0'.
              88 WK-HARD-ERR                 VALUE '1'.
      *
       01  WK-DATE-AREA.
           05 WK-CYCLE-DT              PIC 9(8) VALUE ZERO.
           05 WK-DUE-DT                PIC 9(8) VALUE ZERO.
           05 WK-BASE-DT               PIC 9(8) VALUE ZERO.
           05 WK-START-DT              PIC 9(8) VALUE ZERO.
           05 WK-CYCLE-INT             PIC S9(9) COMP VALUE ZERO.
           05 WK-DUE-INT               PIC S9(9) COMP VALUE ZERO.
           05 WK-BASE-INT              PIC S9(9) COMP VALUE ZERO.
           05 WK-START-INT             PIC S9(9) COMP VALUE ZERO.
           05 WK-DAYS                  PIC S9(9) COMP VALUE ZERO.
           05 WK-WEEK-REM              PIC S9(4) COMP VALUE ZERO.
           05 WK-CHECK-DT              PIC 9(8) VALUE ZERO.
           05 WK-DATE-OK-SW            PIC X VALUE '0'.
              88 WK-DATE-OK                  VALUE '1'.
      *
       01  WK-CALENDAR-AREA.
           05 WK-CAL-KBN               PIC X VALUE SPACE.
              88 WK-CAL-BANK                 VALUE '1'.
              88 WK-CAL-NONE                 VALUE '9'.
           05 WK-HOLIDAY-SW            PIC X VALUE '0'.
              88 WK-HOLIDAY                  VALUE '1'.
              88 WK-BUSINESS-DAY             VALUE '0'.
      *
       01  WK-EDIT-AREA.
           05 WK-MSG-CARD-NO           PIC X(16) VALUE SPACE.
           05 WK-MSG-DATE              PIC 9(8) VALUE ZERO.
      *
       LINKAGE SECTION.
       01  CB151S-PARM.
           05 LN-CARD-NO               PIC X(16).
           05 LN-CYCLE-DT              PIC 9(8).
           05 LN-DUE-DT                PIC 9(8).
           05 LN-CAL-KBN               PIC X.
           05 LN-DELAY-START-DT        PIC 9(8).
           05 LN-DELAY-DAYS            PIC 9(5).
           05 LN-RESULT-CD             PIC X.
           05 LN-REASON-CD             PIC X(4).
      *
       PROCEDURE DIVISION USING CB151S-PARM.
      *
       0000-MAIN.
           MOVE 8                      TO RETURN-CODE
           PERFORM 1000-INIT
           IF NOT WK-HARD-ERR
              PERFORM 2000-VALIDATE
           END-IF
           IF NOT WK-HARD-ERR
              PERFORM 3000-READ-CDOSF
           END-IF
           IF NOT WK-HARD-ERR
              PERFORM 4000-CALCULATE
           END-IF
           PERFORM 9000-FINAL
           GOBACK.
      *
       1000-INIT.
           MOVE ZERO                   TO LN-DELAY-START-DT
                                          LN-DELAY-DAYS
           MOVE SPACE                  TO LN-RESULT-CD
                                          LN-REASON-CD
           MOVE LN-CARD-NO             TO WK-MSG-CARD-NO
           MOVE LN-CYCLE-DT            TO WK-CYCLE-DT
           MOVE LN-DUE-DT              TO WK-DUE-DT
           MOVE LN-CAL-KBN             TO WK-CAL-KBN
      *
           OPEN INPUT CDOSF
           IF WK-CDOSF-ST NOT = '00'
              DISPLAY 'CB151S CDOSF OPEN ST=' WK-CDOSF-ST
              MOVE 'E'                 TO LN-RESULT-CD
              MOVE 'FOPN'              TO LN-REASON-CD
              SET WK-HARD-ERR          TO TRUE
           END-IF.
      *
       2000-VALIDATE.
           IF LN-CARD-NO = SPACE
              DISPLAY 'CB151S CARD NO SPACE'
              MOVE 'E'                 TO LN-RESULT-CD
              MOVE 'CARD'              TO LN-REASON-CD
              SET WK-HARD-ERR          TO TRUE
           END-IF
      *
           IF NOT WK-HARD-ERR
              PERFORM 2100-CHECK-CYCLE-DT
           END-IF
           IF NOT WK-HARD-ERR
              PERFORM 2200-CHECK-DUE-DT
           END-IF
           IF NOT WK-HARD-ERR
              IF NOT WK-CAL-BANK AND NOT WK-CAL-NONE
                 DISPLAY 'CB151S CAL KBN ERR=' WK-CAL-KBN
                 MOVE 'E'              TO LN-RESULT-CD
                 MOVE 'CALK'           TO LN-REASON-CD
                 SET WK-HARD-ERR       TO TRUE
              END-IF
           END-IF.
      *
       2100-CHECK-CYCLE-DT.
           MOVE LN-CYCLE-DT            TO WK-CHECK-DT
           PERFORM 2300-CHECK-DATE
           IF NOT WK-DATE-OK
              MOVE LN-CYCLE-DT         TO WK-MSG-DATE
              DISPLAY 'CB151S CYCLE DATE ERR=' WK-MSG-DATE
              MOVE 'E'                 TO LN-RESULT-CD
              MOVE 'CYDT'              TO LN-REASON-CD
              SET WK-HARD-ERR          TO TRUE
           ELSE
              COMPUTE WK-CYCLE-INT =
                  FUNCTION INTEGER-OF-DATE(LN-CYCLE-DT)
           END-IF.
      *
       2200-CHECK-DUE-DT.
           MOVE LN-DUE-DT              TO WK-CHECK-DT
           PERFORM 2300-CHECK-DATE
           IF NOT WK-DATE-OK
              MOVE LN-DUE-DT           TO WK-MSG-DATE
              DISPLAY 'CB151S DUE DATE ERR=' WK-MSG-DATE
              MOVE 'E'                 TO LN-RESULT-CD
              MOVE 'DUDT'              TO LN-REASON-CD
              SET WK-HARD-ERR          TO TRUE
           ELSE
              COMPUTE WK-DUE-INT =
                  FUNCTION INTEGER-OF-DATE(LN-DUE-DT)
           END-IF.
      *
       2300-CHECK-DATE.
           MOVE '0'                    TO WK-DATE-OK-SW
           IF WK-CHECK-DT >= 19000101
              AND WK-CHECK-DT <= 20991231
              IF FUNCTION TEST-DATE-YYYYMMDD(WK-CHECK-DT) = ZERO
                 MOVE '1'              TO WK-DATE-OK-SW
              END-IF
           END-IF.
      *
       3000-READ-CDOSF.
           MOVE LN-CARD-NO             TO OS-CARD-NO
           READ CDOSF
              KEY IS OS-CARD-NO
              INVALID KEY
                 IF WK-CDOSF-ST = '23'
                    DISPLAY 'CB151S CDOSF NF CARD='
                            WK-MSG-CARD-NO
                    MOVE 'E'           TO LN-RESULT-CD
                    MOVE 'NFND'        TO LN-REASON-CD
                    SET WK-HARD-ERR    TO TRUE
                 ELSE
                    DISPLAY 'CB151S CDOSF READ ST=' WK-CDOSF-ST
                    MOVE 'E'           TO LN-RESULT-CD
                    MOVE 'FRED'        TO LN-REASON-CD
                    SET WK-HARD-ERR    TO TRUE
                 END-IF
           END-READ
      *
           IF NOT WK-HARD-ERR
              IF OS-CYCLE-DT NOT = LN-CYCLE-DT
                 DISPLAY 'CB151S CYCLE UNMATCH CARD='
                         WK-MSG-CARD-NO
                 MOVE 'E'              TO LN-RESULT-CD
                 MOVE 'CYMM'           TO LN-REASON-CD
                 SET WK-HARD-ERR       TO TRUE
              END-IF
           END-IF
      *
           IF NOT WK-HARD-ERR
              IF OS-FEE-BAL-AMT < ZERO
                 OR OS-INTEREST-BAL-AMT < ZERO
                 OR OS-PRINCIPAL-BAL-AMT < ZERO
                 DISPLAY 'CB151S BALANCE ERR CARD='
                         WK-MSG-CARD-NO
                 MOVE 'E'              TO LN-RESULT-CD
                 MOVE 'BALN'           TO LN-REASON-CD
                 SET WK-HARD-ERR       TO TRUE
              END-IF
           END-IF.
      *
       4000-CALCULATE.
           MOVE WK-DUE-DT              TO WK-BASE-DT
           MOVE WK-DUE-INT             TO WK-BASE-INT
           IF WK-CAL-BANK
              PERFORM 4100-ADJUST-BUSINESS
           END-IF
      *
           COMPUTE WK-START-INT = WK-BASE-INT + 1
           COMPUTE WK-START-DT =
               FUNCTION DATE-OF-INTEGER(WK-START-INT)
           COMPUTE WK-DAYS = WK-CYCLE-INT - WK-BASE-INT
           IF WK-DAYS < ZERO
              MOVE ZERO                TO WK-DAYS
           END-IF
      *
           MOVE WK-START-DT            TO LN-DELAY-START-DT
           MOVE WK-DAYS                TO LN-DELAY-DAYS
           MOVE 'N'                    TO LN-RESULT-CD
           MOVE '0000'                 TO LN-REASON-CD.
      *
       4100-ADJUST-BUSINESS.
           PERFORM 4200-JUDGE-HOLIDAY
           PERFORM UNTIL WK-BUSINESS-DAY
              COMPUTE WK-BASE-INT = WK-BASE-INT + 1
              COMPUTE WK-BASE-DT =
                  FUNCTION DATE-OF-INTEGER(WK-BASE-INT)
              PERFORM 4200-JUDGE-HOLIDAY
           END-PERFORM.
      *
       4200-JUDGE-HOLIDAY.
           SET WK-BUSINESS-DAY         TO TRUE
           COMPUTE WK-WEEK-REM =
               FUNCTION MOD(WK-BASE-INT 7)
           IF WK-WEEK-REM = 0 OR WK-WEEK-REM = 6
              SET WK-HOLIDAY           TO TRUE
           END-IF
           IF WK-BASE-DT = 20240101
              OR WK-BASE-DT = 20240108
              OR WK-BASE-DT = 20240212
              OR WK-BASE-DT = 20240223
              OR WK-BASE-DT = 20240320
              OR WK-BASE-DT = 20240429
              OR WK-BASE-DT = 20240503
              OR WK-BASE-DT = 20240506
              OR WK-BASE-DT = 20240715
              OR WK-BASE-DT = 20240812
              OR WK-BASE-DT = 20240916
              OR WK-BASE-DT = 20240923
              OR WK-BASE-DT = 20241014
              OR WK-BASE-DT = 20241104
              OR WK-BASE-DT = 20250101
              OR WK-BASE-DT = 20250113
              OR WK-BASE-DT = 20250211
              OR WK-BASE-DT = 20250224
              OR WK-BASE-DT = 20250320
              OR WK-BASE-DT = 20250429
              OR WK-BASE-DT = 20250505
              OR WK-BASE-DT = 20250506
              OR WK-BASE-DT = 20250721
              OR WK-BASE-DT = 20250811
              OR WK-BASE-DT = 20250915
              OR WK-BASE-DT = 20250923
              OR WK-BASE-DT = 20251013
              OR WK-BASE-DT = 20251103
              OR WK-BASE-DT = 20251124
              SET WK-HOLIDAY           TO TRUE
           END-IF.
      *
       9000-FINAL.
           IF WK-CDOSF-ST NOT = SPACE
              CLOSE CDOSF
              IF WK-CDOSF-ST NOT = '00'
                 DISPLAY 'CB151S CDOSF CLOSE ST=' WK-CDOSF-ST
                 MOVE 'E'              TO LN-RESULT-CD
                 MOVE 'FCLS'           TO LN-REASON-CD
                 SET WK-HARD-ERR       TO TRUE
              END-IF
           END-IF
      *
           IF WK-HARD-ERR
              MOVE 8                   TO RETURN-CODE
           ELSE
              MOVE 0                   TO RETURN-CODE
           END-IF.
