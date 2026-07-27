       IDENTIFICATION DIVISION.
       PROGRAM-ID. CB920S.
      ******************************************************************
      * 変更履歴
      * 版数  年月日    担当     概要
      * 1.00  20240415  開発一課 請求明細編集サブルーチン新規作成
      * 1.01  20240520  開発一課 支払期日算出の営業標準日数を定数化
      * 1.02  20240618  保守二課 金額・日付検証時の戻り値設定を明確化
      ******************************************************************
      * 請求明細編集サブルーチン
      * Ｄ－１７０２：
      *   最低支払額の算定は債権管理規程および関連稟議に従う。
      *   改定後の約定率・最低支払額の具体値は該当規程を参照のこと。
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  WS-STD-DUE-OFFSET       PIC S9(04) COMP VALUE 27.
       77  WS-CYCLE-INT            PIC S9(09) COMP VALUE ZERO.
       77  WS-DUE-INT              PIC S9(09) COMP VALUE ZERO.
       77  WS-CYCLE-DATE           PIC 9(08) VALUE ZERO.
       77  WS-CYCLE-YYYY           PIC 9(04) VALUE ZERO.
       77  WS-CYCLE-MM             PIC 9(02) VALUE ZERO.
       77  WS-CYCLE-DD             PIC 9(02) VALUE ZERO.
       77  WS-MAX-DD               PIC 9(02) VALUE ZERO.
       77  WS-LEAP-REM-4           PIC 9(04) VALUE ZERO.
       77  WS-LEAP-REM-100         PIC 9(04) VALUE ZERO.
       77  WS-LEAP-REM-400         PIC 9(04) VALUE ZERO.
       77  WS-DATE-OK              PIC X(01) VALUE SPACE.
           88  DATE-VALID                    VALUE 'Y'.
           88  DATE-INVALID                  VALUE 'N'.

       LINKAGE SECTION.
           COPY LK-BILLED-PARM.

       PROCEDURE DIVISION USING LK-BILLED-PARM.
       0000-MAIN SECTION.
       0000-START.
           MOVE 0 TO RETURN-CODE
           MOVE '99' TO LK-BE-RET

           PERFORM 1000-VALIDATE-PARM

           IF LK-BE-RET = '99'
              PERFORM 2000-EDIT-DUE-DATE
           END-IF

           IF LK-BE-RET = '99'
              MOVE '00' TO LK-BE-RET
           END-IF

           GOBACK
           .

       1000-VALIDATE-PARM SECTION.
       1000-START.
           IF LK-BE-CARD-NO = SPACES
              MOVE '11' TO LK-BE-RET
              DISPLAY 'カード番号未設定'
              EXIT SECTION
           END-IF

           IF LK-BE-CYCLE-DT IS NOT NUMERIC
              MOVE '12' TO LK-BE-RET
              DISPLAY '請求サイクル日数値不正'
              EXIT SECTION
           END-IF

           MOVE LK-BE-CYCLE-DT TO WS-CYCLE-DATE
           PERFORM 1100-CHECK-CYCLE-DATE

           IF DATE-INVALID
              MOVE '13' TO LK-BE-RET
              DISPLAY '請求サイクル日暦日不正'
              EXIT SECTION
           END-IF

           IF LK-BE-BILL-AMT < ZERO
              MOVE '21' TO LK-BE-RET
              DISPLAY '請求金額マイナス不正'
              EXIT SECTION
           END-IF
           .

       1100-CHECK-CYCLE-DATE SECTION.
       1100-START.
           SET DATE-INVALID TO TRUE
           MOVE WS-CYCLE-DATE(1:4) TO WS-CYCLE-YYYY
           MOVE WS-CYCLE-DATE(5:2) TO WS-CYCLE-MM
           MOVE WS-CYCLE-DATE(7:2) TO WS-CYCLE-DD

           IF WS-CYCLE-YYYY < 1900 OR WS-CYCLE-YYYY > 2099
              EXIT SECTION
           END-IF

           IF WS-CYCLE-MM < 1 OR WS-CYCLE-MM > 12
              EXIT SECTION
           END-IF

           EVALUATE WS-CYCLE-MM
              WHEN 1
              WHEN 3
              WHEN 5
              WHEN 7
              WHEN 8
              WHEN 10
              WHEN 12
                   MOVE 31 TO WS-MAX-DD
              WHEN 4
              WHEN 6
              WHEN 9
              WHEN 11
                   MOVE 30 TO WS-MAX-DD
              WHEN 2
                   PERFORM 1110-SET-FEB-DAYS
           END-EVALUATE

           IF WS-CYCLE-DD >= 1 AND WS-CYCLE-DD <= WS-MAX-DD
              SET DATE-VALID TO TRUE
           END-IF
           .

       1110-SET-FEB-DAYS SECTION.
       1110-START.
           COMPUTE WS-LEAP-REM-4 = FUNCTION MOD(WS-CYCLE-YYYY 4)
           COMPUTE WS-LEAP-REM-100 = FUNCTION MOD(WS-CYCLE-YYYY 100)
           COMPUTE WS-LEAP-REM-400 = FUNCTION MOD(WS-CYCLE-YYYY 400)

           IF WS-LEAP-REM-4 = ZERO
              AND (WS-LEAP-REM-100 NOT = ZERO
                   OR WS-LEAP-REM-400 = ZERO)
              MOVE 29 TO WS-MAX-DD
           ELSE
              MOVE 28 TO WS-MAX-DD
           END-IF
           .

       2000-EDIT-DUE-DATE SECTION.
       2000-START.
           COMPUTE WS-CYCLE-INT =
                   FUNCTION INTEGER-OF-DATE(WS-CYCLE-DATE)
           COMPUTE WS-DUE-INT = WS-CYCLE-INT + WS-STD-DUE-OFFSET
           COMPUTE LK-BE-DUE-DT =
                   FUNCTION DATE-OF-INTEGER(WS-DUE-INT)
           .
