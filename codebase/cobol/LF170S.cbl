       IDENTIFICATION DIVISION.
       PROGRAM-ID. LF170S.
      *
      * 返戻金明細整形サブルーチン
      * 返戻金明細の金額編集、日付編集、契約番号マスク、印字区分設定
      *
      * 変更履歴
      * 版数  年月日    担当      概要
      * 1.0   20200601  K.OKADA   初版作成
      * 1.1   20210615  K.OKADA   日付編集ロジック修正
      * 1.2   20230410  K.OKADA   契約番号マスク精度向上、印字区分設定強化
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           FUNCTION ALL INTRINSIC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
       01 WS-EDIT-WORK.
           05 WS-AMOUNT-NUM       PIC S9(12)V99 VALUE 0.
           05 WS-AMOUNT-ABS       PIC 9(12)V99 VALUE 0.
           05 WS-AMOUNT-EDIT      PIC ---,---,---,---9.99.
           05 WS-AMOUNT-DISPLAY   PIC X(18) VALUE SPACES.
           05 WS-AMT-SIGN         PIC X VALUE SPACE.
           05 WS-DATE-WORK        PIC 9(8) VALUE 0.
           05 WS-DATE-YYYY        PIC 9(4) VALUE 0.
           05 WS-DATE-MM          PIC 9(2) VALUE 0.
           05 WS-DATE-DD          PIC 9(2) VALUE 0.
           05 WS-DATE-EDIT        PIC X(10) VALUE SPACES.
           05 WS-CONTRACT-ORIG    PIC X(12) VALUE SPACES.
           05 WS-CONTRACT-MASKED  PIC X(12) VALUE SPACES.
           05 WS-CONTRACT-LEN     PIC 9(3) COMP VALUE 0.
           05 WS-PRINT-CLASS-SET  PIC X(2) VALUE '00'.
           05 WS-DETAIL-TYPE      PIC X VALUE SPACE.
           05 WS-IS-LEAP-YR       PIC X VALUE 'N'.
           05 WS-MAX-DAY          PIC 9(2) VALUE 0.

       01 WS-VALIDATION-FLAGS.
           05 WS-INPUT-VALID      PIC X VALUE 'Y'.
           05 WS-AMT-NEGATIVE     PIC X VALUE 'N'.
           05 WS-DATE-SUPPLIED    PIC X VALUE 'N'.
           05 WS-DATE-VALID-FLG   PIC X VALUE 'N'.

       LINKAGE SECTION.
       01 LS-INPUT-REC.
           05 LS-IN-AMOUNT        PIC S9(12)V99.
           05 LS-IN-DATE          PIC 9(8).
           05 LS-IN-CONTRACT      PIC X(12).
           05 LS-IN-PRINT-CLASS   PIC X(2).
           05 LS-IN-DETAIL-TYPE   PIC X VALUE SPACE.

       01 LS-OUTPUT-REC.
           05 LS-OUT-AMOUNT       PIC X(18).
           05 LS-OUT-DATE         PIC X(10).
           05 LS-OUT-CONTRACT     PIC X(12).
           05 LS-OUT-PRINT-CLASS  PIC X(2).

       PROCEDURE DIVISION USING LS-INPUT-REC LS-OUTPUT-REC.

       MAIN-PROCEDURE.
           MOVE 0 TO RETURN-CODE.
           PERFORM INIT-WORK-FIELDS.

           IF LS-INPUT-REC = LOW-VALUE
               MOVE 8 TO RETURN-CODE
               GOBACK
           END-IF.

           MOVE LS-IN-DETAIL-TYPE TO WS-DETAIL-TYPE.
           PERFORM PROCESS-AMOUNT-FIELD.
           IF RETURN-CODE NOT = 0
               GOBACK
           END-IF.

           PERFORM PROCESS-DATE-FIELD.
           IF RETURN-CODE NOT = 0
               GOBACK
           END-IF.

           PERFORM PROCESS-CONTRACT-FIELD.

           PERFORM DETERMINE-PRINT-CLASS.

           MOVE WS-AMOUNT-DISPLAY TO LS-OUT-AMOUNT.
           MOVE WS-DATE-EDIT TO LS-OUT-DATE.
           MOVE WS-CONTRACT-MASKED TO LS-OUT-CONTRACT.
           MOVE WS-PRINT-CLASS-SET TO LS-OUT-PRINT-CLASS.

           MOVE 0 TO RETURN-CODE.
           GOBACK.

       INIT-WORK-FIELDS.
           MOVE 'Y' TO WS-INPUT-VALID.
           MOVE 'N' TO WS-AMT-NEGATIVE.
           MOVE 'N' TO WS-DATE-SUPPLIED.
           MOVE 'N' TO WS-DATE-VALID-FLG.
           MOVE SPACES TO WS-AMOUNT-DISPLAY.
           MOVE SPACES TO WS-DATE-EDIT.
           MOVE SPACES TO WS-CONTRACT-MASKED.
           MOVE '00' TO WS-PRINT-CLASS-SET.

       PROCESS-AMOUNT-FIELD.
           MOVE LS-IN-AMOUNT TO WS-AMOUNT-NUM.

           IF WS-AMOUNT-NUM = 0
               MOVE '0.00' TO WS-AMOUNT-DISPLAY
               EXIT PARAGRAPH
           END-IF.

           IF WS-AMOUNT-NUM < 0
               MOVE 'Y' TO WS-AMT-NEGATIVE
               MOVE '-' TO WS-AMT-SIGN
               COMPUTE WS-AMOUNT-ABS =
                   FUNCTION ABS(WS-AMOUNT-NUM)
           ELSE
               MOVE 'N' TO WS-AMT-NEGATIVE
               MOVE SPACE TO WS-AMT-SIGN
               MOVE WS-AMOUNT-NUM TO WS-AMOUNT-ABS
           END-IF.

           IF WS-AMOUNT-ABS > 999999999999.99
               MOVE 8 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF.

           MOVE WS-AMOUNT-ABS TO WS-AMOUNT-EDIT.
           MOVE FUNCTION TRIM(WS-AMOUNT-EDIT)
               TO WS-AMOUNT-DISPLAY.

           IF WS-AMT-NEGATIVE = 'Y'
               STRING WS-AMT-SIGN DELIMITED BY SIZE
                   FUNCTION TRIM(WS-AMOUNT-DISPLAY)
                       DELIMITED BY SIZE
                   INTO WS-AMOUNT-DISPLAY
               END-STRING
           END-IF.

       PROCESS-DATE-FIELD.
           MOVE LS-IN-DATE TO WS-DATE-WORK.

           IF WS-DATE-WORK = 0 OR WS-DATE-WORK = SPACES
               MOVE SPACES TO WS-DATE-EDIT
               MOVE 'N' TO WS-DATE-SUPPLIED
               EXIT PARAGRAPH
           END-IF.

           MOVE 'Y' TO WS-DATE-SUPPLIED.
           MOVE WS-DATE-WORK(1:4) TO WS-DATE-YYYY.
           MOVE WS-DATE-WORK(5:2) TO WS-DATE-MM.
           MOVE WS-DATE-WORK(7:2) TO WS-DATE-DD.

           IF WS-DATE-YYYY < 1900 OR WS-DATE-YYYY > 2099
               MOVE 8 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF.

           IF WS-DATE-MM < 1 OR WS-DATE-MM > 12
               MOVE 8 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF.

           PERFORM VALIDATE-DAY-OF-MONTH.
           IF RETURN-CODE NOT = 0
               EXIT PARAGRAPH
           END-IF.

           PERFORM FORMAT-DATE-OUTPUT.
           MOVE 'Y' TO WS-DATE-VALID-FLG.

       VALIDATE-DAY-OF-MONTH.
           EVALUATE WS-DATE-MM
               WHEN 1
               WHEN 3
               WHEN 5
               WHEN 7
               WHEN 8
               WHEN 10
               WHEN 12
                   MOVE 31 TO WS-MAX-DAY
               WHEN 4
               WHEN 6
               WHEN 9
               WHEN 11
                   MOVE 30 TO WS-MAX-DAY
               WHEN 2
                   IF FUNCTION MOD(WS-DATE-YYYY, 400) = 0
                       MOVE 29 TO WS-MAX-DAY
                       MOVE 'Y' TO WS-IS-LEAP-YR
                   ELSE
                       IF FUNCTION MOD(WS-DATE-YYYY, 100) = 0
                           MOVE 28 TO WS-MAX-DAY
                           MOVE 'N' TO WS-IS-LEAP-YR
                       ELSE
                           IF FUNCTION MOD(
                               WS-DATE-YYYY, 4) = 0
                               MOVE 29 TO WS-MAX-DAY
                               MOVE 'Y' TO WS-IS-LEAP-YR
                           ELSE
                               MOVE 28 TO WS-MAX-DAY
                               MOVE 'N' TO WS-IS-LEAP-YR
                           END-IF
                       END-IF
                   END-IF
           END-EVALUATE.

           IF WS-DATE-DD < 1 OR WS-DATE-DD > WS-MAX-DAY
               MOVE 8 TO RETURN-CODE
           END-IF.

       FORMAT-DATE-OUTPUT.
           STRING WS-DATE-YYYY DELIMITED BY SIZE
               '/' DELIMITED BY SIZE
               WS-DATE-MM DELIMITED BY SIZE
               '/' DELIMITED BY SIZE
               WS-DATE-DD DELIMITED BY SIZE
               INTO WS-DATE-EDIT
           END-STRING.

       PROCESS-CONTRACT-FIELD.
           MOVE LS-IN-CONTRACT TO WS-CONTRACT-ORIG.

           IF WS-CONTRACT-ORIG = SPACES
               MOVE SPACES TO WS-CONTRACT-MASKED
               EXIT PARAGRAPH
           END-IF.

           MOVE FUNCTION LENGTH(
               FUNCTION TRIM(WS-CONTRACT-ORIG))
               TO WS-CONTRACT-LEN.

           IF WS-CONTRACT-LEN < 6
               MOVE WS-CONTRACT-ORIG TO WS-CONTRACT-MASKED
               EXIT PARAGRAPH
           END-IF.

           STRING WS-CONTRACT-ORIG(1:4) DELIMITED BY SIZE
               '****' DELIMITED BY SIZE
               WS-CONTRACT-ORIG(11:2) DELIMITED BY SIZE
               INTO WS-CONTRACT-MASKED
           END-STRING.

       DETERMINE-PRINT-CLASS.
           EVALUATE TRUE
               WHEN LS-IN-PRINT-CLASS NOT = SPACES
                   AND LS-IN-PRINT-CLASS NOT = '00'
                   MOVE LS-IN-PRINT-CLASS TO WS-PRINT-CLASS-SET
               WHEN WS-AMT-NEGATIVE = 'Y'
                   MOVE '02' TO WS-PRINT-CLASS-SET
               WHEN WS-DATE-VALID-FLG = 'Y'
                   MOVE '01' TO WS-PRINT-CLASS-SET
               WHEN OTHER
                   MOVE '00' TO WS-PRINT-CLASS-SET
           END-EVALUATE.

       END PROGRAM LF170S.
