       IDENTIFICATION DIVISION.
       PROGRAM-ID. CB211S.
      *
      * オーソリ保留整合性チェック サブプログラム
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CDAUTHF ASSIGN TO "CDAUTHF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-CDAUTHF-ST.
           SELECT CDSALF ASSIGN TO "CDSALF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-CDSALF-ST.

       DATA DIVISION.
       FILE SECTION.
       FD  CDAUTHF.
           COPY CDAUTHFC.

       FD  CDSALF.
           COPY CDSALC.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05  WS-CDAUTHF-ST        PIC XX VALUE SPACES.
           05  WS-CDSALF-ST         PIC XX VALUE SPACES.

       01  WS-SWITCHES.
           05  WS-AUTH-EOF-SW       PIC X VALUE 'N'.
               88  WS-AUTH-EOF            VALUE 'Y'.
           05  WS-SALES-EOF-SW      PIC X VALUE 'N'.
               88  WS-SALES-EOF           VALUE 'Y'.
           05  WS-AUTH-FOUND-SW     PIC X VALUE 'N'.
               88  WS-AUTH-FOUND          VALUE 'Y'.
           05  WS-SALES-FOUND-SW    PIC X VALUE 'N'.
               88  WS-SALES-FOUND         VALUE 'Y'.
           05  WS-HARD-ERR-SW       PIC X VALUE 'N'.
               88  WS-HARD-ERR            VALUE 'Y'.

       01  WS-WORK.
           05  WS-AMT-DIFF          PIC S9(13) VALUE ZERO.
           05  WS-ABS-DIFF          PIC 9(13) VALUE ZERO.

       01  WS-CONSTANTS.
           05  WS-AUTH-OK           PIC XX VALUE '00'.
           05  WS-AUTH-CANCEL       PIC XX VALUE '20'.
           05  WS-AUTH-SOLD         PIC XX VALUE '30'.
           05  WS-BASE-CURRENCY     PIC XXX VALUE 'JPY'.

       LINKAGE SECTION.
       01  LK-CB211S-PARM.
           05  LK-AUTH-ID           PIC X(20).
           05  LK-RESULT-KBN        PIC X.
               88  LK-EXPIRABLE           VALUE '1'.
               88  LK-FIXED               VALUE '2'.
               88  LK-INVESTIGATE         VALUE '3'.
           05  LK-REASON-CD         PIC X(3).
           05  LK-DIFF-AMT          PIC S9(13).
           05  LK-STATUS-CD         PIC XX.

       PROCEDURE DIVISION USING LK-CB211S-PARM.
       0000-MAIN.
           MOVE 8 TO RETURN-CODE
           PERFORM 0100-INITIALIZE
           PERFORM 0200-OPEN-FILES
           IF NOT WS-HARD-ERR
               PERFORM 1000-FIND-AUTH
           END-IF
           IF NOT WS-HARD-ERR
               PERFORM 2000-FIND-SALES
           END-IF
           IF NOT WS-HARD-ERR
               PERFORM 3000-JUDGE-HOLD
               MOVE 0 TO RETURN-CODE
           END-IF
           PERFORM 9000-CLOSE-FILES
           GOBACK.

       0100-INITIALIZE.
           MOVE SPACE TO LK-RESULT-KBN
           MOVE SPACE TO LK-REASON-CD
           MOVE ZERO  TO LK-DIFF-AMT
           MOVE SPACE TO LK-STATUS-CD
           MOVE 'N' TO WS-AUTH-EOF-SW
           MOVE 'N' TO WS-SALES-EOF-SW
           MOVE 'N' TO WS-AUTH-FOUND-SW
           MOVE 'N' TO WS-SALES-FOUND-SW
           MOVE 'N' TO WS-HARD-ERR-SW
           MOVE ZERO TO WS-AMT-DIFF
           MOVE ZERO TO WS-ABS-DIFF.

       0200-OPEN-FILES.
           OPEN INPUT CDAUTHF
           IF WS-CDAUTHF-ST NOT = '00'
               DISPLAY 'CDAUTHF OPEN ERROR ST='
                   WS-CDAUTHF-ST
               MOVE 'Y' TO WS-HARD-ERR-SW
           END-IF

           IF NOT WS-HARD-ERR
               OPEN INPUT CDSALF
               IF WS-CDSALF-ST NOT = '00'
                   DISPLAY 'CDSALF OPEN ERROR ST='
                       WS-CDSALF-ST
                   MOVE 'Y' TO WS-HARD-ERR-SW
               END-IF
           END-IF.

       1000-FIND-AUTH.
           PERFORM UNTIL WS-AUTH-EOF
              OR WS-AUTH-FOUND
               READ CDAUTHF
                   AT END
                       MOVE 'Y' TO WS-AUTH-EOF-SW
                   NOT AT END
                       IF WS-CDAUTHF-ST = '00'
                           IF AU-AUTH-ID = LK-AUTH-ID
                               MOVE 'Y' TO WS-AUTH-FOUND-SW
                           END-IF
                       ELSE
                           DISPLAY 'CDAUTHF READ ERROR ST='
                               WS-CDAUTHF-ST
                           MOVE 'Y' TO WS-HARD-ERR-SW
                           MOVE 'Y' TO WS-AUTH-EOF-SW
                       END-IF
               END-READ
           END-PERFORM.

       2000-FIND-SALES.
           PERFORM UNTIL WS-SALES-EOF
              OR WS-SALES-FOUND
               READ CDSALF
                   AT END
                       MOVE 'Y' TO WS-SALES-EOF-SW
                   NOT AT END
                       IF WS-CDSALF-ST = '00'
                           IF SL-AUTH-ID = LK-AUTH-ID
                               MOVE 'Y' TO WS-SALES-FOUND-SW
                           END-IF
                       ELSE
                           DISPLAY 'CDSALF READ ERROR ST='
                               WS-CDSALF-ST
                           MOVE 'Y' TO WS-HARD-ERR-SW
                           MOVE 'Y' TO WS-SALES-EOF-SW
                       END-IF
               END-READ
           END-PERFORM.

       3000-JUDGE-HOLD.
           IF NOT WS-AUTH-FOUND
               SET LK-INVESTIGATE TO TRUE
               MOVE 'NAU' TO LK-REASON-CD
               MOVE '04'  TO LK-STATUS-CD
               DISPLAY 'AUTH NOT FOUND AUTH-ID='
                   LK-AUTH-ID
           ELSE
               EVALUATE TRUE
                   WHEN AU-AUTH-RESULT = WS-AUTH-CANCEL
                       SET LK-EXPIRABLE TO TRUE
                       MOVE 'CAN' TO LK-REASON-CD
                       MOVE '00'  TO LK-STATUS-CD
                   WHEN AU-AUTH-RESULT = WS-AUTH-SOLD
                       PERFORM 3100-JUDGE-SOLD
                   WHEN AU-AUTH-RESULT = WS-AUTH-OK
                       PERFORM 3200-JUDGE-AUTH-OK
                   WHEN OTHER
                       SET LK-INVESTIGATE TO TRUE
                       MOVE 'ARS' TO LK-REASON-CD
                       MOVE '03'  TO LK-STATUS-CD
                       DISPLAY 'AUTH RESULT ERROR AUTH-ID='
                           AU-AUTH-ID
               END-EVALUATE
           END-IF.

       3100-JUDGE-SOLD.
           IF NOT WS-SALES-FOUND
               SET LK-INVESTIGATE TO TRUE
               MOVE 'NSL' TO LK-REASON-CD
               MOVE '04'  TO LK-STATUS-CD
               DISPLAY 'SALES NOT FOUND AUTH-ID='
                   AU-AUTH-ID
           ELSE
               PERFORM 3300-CHECK-SALES-MATCH
           END-IF.

       3200-JUDGE-AUTH-OK.
           IF WS-SALES-FOUND
               PERFORM 3300-CHECK-SALES-MATCH
           ELSE
               SET LK-EXPIRABLE TO TRUE
               MOVE 'HLD' TO LK-REASON-CD
               MOVE '00'  TO LK-STATUS-CD
           END-IF.

       3300-CHECK-SALES-MATCH.
           COMPUTE WS-AMT-DIFF =
               SL-SALES-AMT - AU-AUTH-AMT
           MOVE WS-AMT-DIFF TO LK-DIFF-AMT

           IF WS-AMT-DIFF < ZERO
               COMPUTE WS-ABS-DIFF =
                   WS-AMT-DIFF * -1
           ELSE
               MOVE WS-AMT-DIFF TO WS-ABS-DIFF
           END-IF

           EVALUATE TRUE
               WHEN AU-CURRENCY-CD NOT = WS-BASE-CURRENCY
                   SET LK-INVESTIGATE TO TRUE
                   MOVE 'CUR' TO LK-REASON-CD
                   MOVE '03'  TO LK-STATUS-CD
                   DISPLAY 'CURRENCY ERROR AUTH-ID='
                       AU-AUTH-ID
               WHEN SL-CARD-NO NOT = AU-CARD-NO
                   SET LK-INVESTIGATE TO TRUE
                   MOVE 'CRD' TO LK-REASON-CD
                   MOVE '03'  TO LK-STATUS-CD
                   DISPLAY 'CARD ERROR AUTH-ID='
                       AU-AUTH-ID
               WHEN SL-MERCHANT-CODE NOT =
                    AU-MERCHANT-CODE
                   SET LK-INVESTIGATE TO TRUE
                   MOVE 'MER' TO LK-REASON-CD
                   MOVE '03'  TO LK-STATUS-CD
                   DISPLAY 'MERCHANT ERROR AUTH-ID='
                       AU-AUTH-ID
               WHEN WS-ABS-DIFF > ZERO
                   SET LK-INVESTIGATE TO TRUE
                   MOVE 'AMT' TO LK-REASON-CD
                   MOVE '03'  TO LK-STATUS-CD
                   DISPLAY 'AMOUNT ERROR AUTH-ID='
                       AU-AUTH-ID
               WHEN SL-SALES-TS < AU-AUTH-TS
                   SET LK-INVESTIGATE TO TRUE
                   MOVE 'TIM' TO LK-REASON-CD
                   MOVE '03'  TO LK-STATUS-CD
                   DISPLAY 'TIME ERROR AUTH-ID='
                       AU-AUTH-ID
               WHEN OTHER
                   SET LK-FIXED TO TRUE
                   MOVE 'FIX' TO LK-REASON-CD
                   MOVE '00'  TO LK-STATUS-CD
           END-EVALUATE.

       9000-CLOSE-FILES.
           IF WS-CDAUTHF-ST = '00'
               CLOSE CDAUTHF
               IF WS-CDAUTHF-ST NOT = '00'
                   DISPLAY 'CDAUTHF CLOSE ERROR ST='
                       WS-CDAUTHF-ST
                   MOVE 8 TO RETURN-CODE
               END-IF
           END-IF

           IF WS-CDSALF-ST = '00'
               CLOSE CDSALF
               IF WS-CDSALF-ST NOT = '00'
                   DISPLAY 'CDSALF CLOSE ERROR ST='
                       WS-CDSALF-ST
                   MOVE 8 TO RETURN-CODE
               END-IF
           END-IF.
