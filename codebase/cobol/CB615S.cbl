       IDENTIFICATION DIVISION.
       PROGRAM-ID. CB615S.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WK-AREA.
           05 WK-RESULT-CODE        PIC X(02) VALUE SPACE.
           05 WK-REASON-CODE        PIC X(04) VALUE SPACE.
           05 WK-REASON-TEXT        PIC X(40) VALUE SPACE.
           05 WK-EDIT-DATE.
              10 WK-EDIT-YYYY       PIC 9(04) VALUE ZERO.
              10 WK-EDIT-MM         PIC 9(02) VALUE ZERO.
              10 WK-EDIT-DD         PIC 9(02) VALUE ZERO.
           05 WK-MAX-DD             PIC 9(02) VALUE ZERO.
           05 WK-REM-4              PIC 9(04) VALUE ZERO.
           05 WK-REM-100            PIC 9(04) VALUE ZERO.
           05 WK-REM-400            PIC 9(04) VALUE ZERO.
           05 WK-LEAP-FLAG          PIC X(01) VALUE SPACE.
           05 WK-VALID-FLAG         PIC X(01) VALUE SPACE.

       LINKAGE SECTION.
       01  LS-CB615S-PARM.
           05 LS-SALES-DATE         PIC X(08).
           05 LS-MERCHANT-CODE      PIC X(10).
           05 LS-CURRENCY-CODE      PIC X(03).
           05 LS-SUMMARY-KEY        PIC X(21).
           05 LS-RESULT-CODE        PIC X(02).
           05 LS-REASON-CODE        PIC X(04).
           05 LS-REASON-TEXT        PIC X(40).

       PROCEDURE DIVISION USING LS-CB615S-PARM.
       MAIN-SECTION.
           PERFORM 1000-INIT
           PERFORM 2000-VALIDATE
           IF WK-VALID-FLAG = 'Y'
              PERFORM 3000-MAKE-KEY
           END-IF
           PERFORM 9000-FINAL
           GOBACK.

       1000-INIT.
           MOVE 0 TO RETURN-CODE
           MOVE SPACE TO LS-SUMMARY-KEY
           MOVE SPACE TO LS-RESULT-CODE
           MOVE SPACE TO LS-REASON-CODE
           MOVE SPACE TO LS-REASON-TEXT
           MOVE '00' TO WK-RESULT-CODE
           MOVE '0000' TO WK-REASON-CODE
           MOVE '正常終了' TO WK-REASON-TEXT
           MOVE 'Y' TO WK-VALID-FLAG
           MOVE SPACE TO WK-LEAP-FLAG.

       2000-VALIDATE.
           IF LS-SALES-DATE = SPACE
              MOVE '10' TO WK-RESULT-CODE
              MOVE 'D001' TO WK-REASON-CODE
              MOVE '売上日未設定' TO WK-REASON-TEXT
              MOVE 'N' TO WK-VALID-FLAG
           END-IF

           IF WK-VALID-FLAG = 'Y'
              IF LS-SALES-DATE IS NOT NUMERIC
                 MOVE '11' TO WK-RESULT-CODE
                 MOVE 'D002' TO WK-REASON-CODE
                 MOVE '売上日数字外' TO WK-REASON-TEXT
                 MOVE 'N' TO WK-VALID-FLAG
              END-IF
           END-IF

           IF WK-VALID-FLAG = 'Y'
              MOVE LS-SALES-DATE TO WK-EDIT-DATE
              PERFORM 2100-CHECK-DATE
           END-IF

           IF WK-VALID-FLAG = 'Y'
              IF LS-MERCHANT-CODE = SPACE
                 MOVE '20' TO WK-RESULT-CODE
                 MOVE 'M001' TO WK-REASON-CODE
                 MOVE '加盟店コード未設定' TO WK-REASON-TEXT
                 MOVE 'N' TO WK-VALID-FLAG
              END-IF
           END-IF

           IF WK-VALID-FLAG = 'Y'
              IF LS-CURRENCY-CODE = SPACE
                 MOVE '30' TO WK-RESULT-CODE
                 MOVE 'C001' TO WK-REASON-CODE
                 MOVE '通貨コード未設定' TO WK-REASON-TEXT
                 MOVE 'N' TO WK-VALID-FLAG
              END-IF
           END-IF

           IF WK-VALID-FLAG = 'Y'
              IF LS-CURRENCY-CODE IS NOT ALPHABETIC
                 MOVE '31' TO WK-RESULT-CODE
                 MOVE 'C002' TO WK-REASON-CODE
                 MOVE '通貨コード字種不正' TO WK-REASON-TEXT
                 MOVE 'N' TO WK-VALID-FLAG
              END-IF
           END-IF.

       2100-CHECK-DATE.
           IF WK-EDIT-YYYY < 1990 OR WK-EDIT-YYYY > 2099
              MOVE '12' TO WK-RESULT-CODE
              MOVE 'D003' TO WK-REASON-CODE
              MOVE '売上日年範囲不正' TO WK-REASON-TEXT
              MOVE 'N' TO WK-VALID-FLAG
           END-IF

           IF WK-VALID-FLAG = 'Y'
              IF WK-EDIT-MM < 1 OR WK-EDIT-MM > 12
                 MOVE '12' TO WK-RESULT-CODE
                 MOVE 'D004' TO WK-REASON-CODE
                 MOVE '売上日月不正' TO WK-REASON-TEXT
                 MOVE 'N' TO WK-VALID-FLAG
              END-IF
           END-IF

           IF WK-VALID-FLAG = 'Y'
              PERFORM 2110-SET-MAX-DAY
              IF WK-EDIT-DD < 1 OR WK-EDIT-DD > WK-MAX-DD
                 MOVE '12' TO WK-RESULT-CODE
                 MOVE 'D005' TO WK-REASON-CODE
                 MOVE '売上日日不正' TO WK-REASON-TEXT
                 MOVE 'N' TO WK-VALID-FLAG
              END-IF
           END-IF.

       2110-SET-MAX-DAY.
           MOVE 31 TO WK-MAX-DD
           EVALUATE WK-EDIT-MM
              WHEN 4
              WHEN 6
              WHEN 9
              WHEN 11
                 MOVE 30 TO WK-MAX-DD
              WHEN 2
                 PERFORM 2120-CHECK-LEAP
                 IF WK-LEAP-FLAG = 'Y'
                    MOVE 29 TO WK-MAX-DD
                 ELSE
                    MOVE 28 TO WK-MAX-DD
                 END-IF
              WHEN OTHER
                 MOVE 31 TO WK-MAX-DD
           END-EVALUATE.

       2120-CHECK-LEAP.
           MOVE 'N' TO WK-LEAP-FLAG
           DIVIDE WK-EDIT-YYYY BY 4 GIVING WK-REM-4
              REMAINDER WK-REM-4
           DIVIDE WK-EDIT-YYYY BY 100 GIVING WK-REM-100
              REMAINDER WK-REM-100
           DIVIDE WK-EDIT-YYYY BY 400 GIVING WK-REM-400
              REMAINDER WK-REM-400
           IF WK-REM-4 = 0
              IF WK-REM-100 NOT = 0 OR WK-REM-400 = 0
                 MOVE 'Y' TO WK-LEAP-FLAG
              END-IF
           END-IF.

       3000-MAKE-KEY.
           STRING LS-SALES-DATE
                  LS-MERCHANT-CODE
                  LS-CURRENCY-CODE
              DELIMITED BY SIZE
              INTO LS-SUMMARY-KEY
           END-STRING.

       9000-FINAL.
           MOVE WK-RESULT-CODE TO LS-RESULT-CODE
           MOVE WK-REASON-CODE TO LS-REASON-CODE
           MOVE WK-REASON-TEXT TO LS-REASON-TEXT
           IF WK-RESULT-CODE = '00'
              MOVE 0 TO RETURN-CODE
           ELSE
              MOVE 4 TO RETURN-CODE
              DISPLAY 'CB615S 入力不正 RC=' WK-RESULT-CODE
                      ' 理由=' WK-REASON-CODE
           END-IF.
