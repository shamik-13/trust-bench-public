       IDENTIFICATION DIVISION.
       PROGRAM-ID. CB520S.
       AUTHOR. 大原 修.
       INSTALLATION. みらいカード.
       DATE-WRITTEN. 2025-03-12.
       DATE-COMPILED.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  WS-PGM-ID             PIC X(08) VALUE 'CB520S  '.
       77  WS-HARD-ERROR         PIC X     VALUE SPACE.
           88  HARD-ERROR                  VALUE '1'.
       77  WS-REASON             PIC X(40) VALUE SPACE.
       77  WS-AMT-LIMIT-SMALL    PIC 9(07) VALUE 1000.
       77  WS-AMT-LIMIT-CALL     PIC 9(07) VALUE 50000.
       77  WS-DAY-LIMIT-SMS      PIC 9(03) VALUE 7.
       77  WS-DAY-LIMIT-LETTER   PIC 9(03) VALUE 30.
       77  WS-DAY-LIMIT-CALL     PIC 9(03) VALUE 60.
       77  WS-NOTICE-LIMIT-CALL  PIC 9(02) VALUE 2.
       77  WS-NOTICE-LIMIT-STOP  PIC 9(02) VALUE 4.

       LINKAGE SECTION.
       01  LK-CB520S-PARM.
           05  LK520-DELAY-DAYS       PIC 9(03).
           05  LK520-UNPAID-AMOUNT    PIC 9(09).
           05  LK520-NOTICE-COUNT     PIC 9(02).
           05  LK520-MEMBER-STATUS    PIC X(01).
               88  LK520-STATUS-ACTIVE        VALUE '1'.
               88  LK520-STATUS-WATCH         VALUE '2'.
               88  LK520-STATUS-SUSPENDED     VALUE '3'.
               88  LK520-STATUS-CLOSED        VALUE '9'.
           05  LK520-DUNNING-KBN      PIC X(04).
               88  LK520-KBN-GRACE           VALUE 'YUYO'.
               88  LK520-KBN-SMS             VALUE 'SMS '.
               88  LK520-KBN-LETTER          VALUE 'SYMN'.
               88  LK520-KBN-CALL            VALUE 'CALL'.
               88  LK520-KBN-STOP-CAND       VALUE 'STOP'.
               88  LK520-KBN-NONE            VALUE 'NONE'.
               88  LK520-KBN-ERROR           VALUE 'ERR '.
           05  LK520-RESULT-CODE      PIC X(02).
           05  LK520-REASON-CODE      PIC X(04).

       PROCEDURE DIVISION USING LK-CB520S-PARM.

       0000-MAIN SECTION.
       0000-START.
           MOVE 0      TO RETURN-CODE
           MOVE SPACE  TO WS-HARD-ERROR
           MOVE SPACES TO WS-REASON
           MOVE '99'   TO LK520-RESULT-CODE
           MOVE 'INIT' TO LK520-REASON-CODE
           MOVE 'NONE' TO LK520-DUNNING-KBN

           PERFORM 1000-VALIDATE

           IF HARD-ERROR
              PERFORM 9000-HARD-ERROR
           ELSE
              PERFORM 2000-DECIDE-KBN
              MOVE 0 TO RETURN-CODE
           END-IF

           GOBACK
           .

       1000-VALIDATE SECTION.
       1000-START.
           IF LK520-DELAY-DAYS IS NOT NUMERIC
              MOVE '1' TO WS-HARD-ERROR
              MOVE 'DLAY' TO LK520-REASON-CODE
              MOVE '延滞日数不正' TO WS-REASON
           END-IF

           IF NOT HARD-ERROR
              IF LK520-UNPAID-AMOUNT IS NOT NUMERIC
                 MOVE '1' TO WS-HARD-ERROR
                 MOVE 'AMNT' TO LK520-REASON-CODE
                 MOVE '未収額不正' TO WS-REASON
              END-IF
           END-IF

           IF NOT HARD-ERROR
              IF LK520-NOTICE-COUNT IS NOT NUMERIC
                 MOVE '1' TO WS-HARD-ERROR
                 MOVE 'NTCE' TO LK520-REASON-CODE
                 MOVE '過去通知回数不正' TO WS-REASON
              END-IF
           END-IF

           IF NOT HARD-ERROR
              IF NOT LK520-STATUS-ACTIVE
                 AND NOT LK520-STATUS-WATCH
                 AND NOT LK520-STATUS-SUSPENDED
                 AND NOT LK520-STATUS-CLOSED
                 MOVE '1' TO WS-HARD-ERROR
                 MOVE 'STAT' TO LK520-REASON-CODE
                 MOVE '会員状態不正' TO WS-REASON
              END-IF
           END-IF
           .

       2000-DECIDE-KBN SECTION.
       2000-START.
           EVALUATE TRUE
              WHEN LK520-STATUS-CLOSED
                 MOVE 'NONE' TO LK520-DUNNING-KBN
                 MOVE '00'   TO LK520-RESULT-CODE
                 MOVE 'CLOS' TO LK520-REASON-CODE

              WHEN LK520-UNPAID-AMOUNT = 0
                 MOVE 'NONE' TO LK520-DUNNING-KBN
                 MOVE '00'   TO LK520-RESULT-CODE
                 MOVE 'ZERO' TO LK520-REASON-CODE

              WHEN LK520-DELAY-DAYS = 0
                 MOVE 'NONE' TO LK520-DUNNING-KBN
                 MOVE '00'   TO LK520-RESULT-CODE
                 MOVE 'NDLY' TO LK520-REASON-CODE

              WHEN LK520-UNPAID-AMOUNT <= WS-AMT-LIMIT-SMALL
                 AND LK520-DELAY-DAYS <= WS-DAY-LIMIT-LETTER
                 MOVE 'YUYO' TO LK520-DUNNING-KBN
                 MOVE '00'   TO LK520-RESULT-CODE
                 MOVE 'SMLA' TO LK520-REASON-CODE

              WHEN LK520-STATUS-SUSPENDED
                 MOVE 'CALL' TO LK520-DUNNING-KBN
                 MOVE '00'   TO LK520-RESULT-CODE
                 MOVE 'SUSD' TO LK520-REASON-CODE

              WHEN LK520-DELAY-DAYS > WS-DAY-LIMIT-CALL
                 AND LK520-NOTICE-COUNT >= WS-NOTICE-LIMIT-STOP
                 MOVE 'STOP' TO LK520-DUNNING-KBN
                 MOVE '00'   TO LK520-RESULT-CODE
                 MOVE 'STPC' TO LK520-REASON-CODE

              WHEN LK520-DELAY-DAYS > WS-DAY-LIMIT-CALL
                 OR LK520-UNPAID-AMOUNT >= WS-AMT-LIMIT-CALL
                 OR LK520-NOTICE-COUNT >= WS-NOTICE-LIMIT-CALL
                 MOVE 'CALL' TO LK520-DUNNING-KBN
                 MOVE '00'   TO LK520-RESULT-CODE
                 MOVE 'CALL' TO LK520-REASON-CODE

              WHEN LK520-DELAY-DAYS > WS-DAY-LIMIT-SMS
                 AND LK520-DELAY-DAYS <= WS-DAY-LIMIT-LETTER
                 MOVE 'SYMN' TO LK520-DUNNING-KBN
                 MOVE '00'   TO LK520-RESULT-CODE
                 MOVE 'LTR1' TO LK520-REASON-CODE

              WHEN OTHER
                 MOVE 'SMS ' TO LK520-DUNNING-KBN
                 MOVE '00'   TO LK520-RESULT-CODE
                 MOVE 'SMS1' TO LK520-REASON-CODE
           END-EVALUATE
           .

       9000-HARD-ERROR SECTION.
       9000-START.
           MOVE 'ERR ' TO LK520-DUNNING-KBN
           MOVE '08'   TO LK520-RESULT-CODE
           MOVE 8      TO RETURN-CODE
           DISPLAY WS-PGM-ID ' 入力検証エラー '
                   LK520-REASON-CODE ' ' WS-REASON
           .
