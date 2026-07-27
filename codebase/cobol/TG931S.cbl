       IDENTIFICATION DIVISION.
       PROGRAM-ID. TG931S.
      ******************************************************************
      * 変更履歴
      * 版数  年月日      担当                         概要
      * 1.00  平成31年04月01日 システム部 為替・対外接続チーム 初版作成
      * 1.01  令和02年07月15日 システム部 為替・対外接続チーム 元号表示整備
      * 1.02  令和04年10月03日 システム部 為替・対外接続チーム 注記見直し
      ******************************************************************
       AUTHOR. システム部 為替・対外系チーム.
      ******************************************************************
      * 対外電文署名サブルーチン
      * 入力項目を規定順に編集し、検査文字を付加して署名を返却する。
      ******************************************************************

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-WORK.
           05  WS-BRANCH-EDIT        PIC X(04).
           05  WS-DT-EDIT            PIC X(08).
           05  WS-SEQ-EDIT           PIC X(10).
           05  WS-AMT-EDIT           PIC X(13).
           05  WS-SIGN-BODY          PIC X(35).
           05  WS-CHECK-CHAR         PIC X(03).
           05  WS-SIGNATURE          PIC X(38).
           05  WS-SRC                PIC X(40).
           05  WS-DIGITS             PIC X(40).
           05  WS-CHAR               PIC X(01).
           05  WS-MAP                PIC X(37)
               VALUE '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ*'.
       01  WS-NUMERIC-WORK.
           05  WS-AMT-N              PIC 9(13).
           05  WS-DT-N               PIC 9(08).
           05  WS-YYYY               PIC 9(04).
           05  WS-MM                 PIC 9(02).
           05  WS-DD                 PIC 9(02).
           05  WS-MAX-DD             PIC 9(02).
           05  WS-IDX                PIC 9(03) COMP-5.
           05  WS-SRC-LEN            PIC 9(03) COMP-5.
           05  WS-OUT-LEN            PIC 9(03) COMP-5.
           05  WS-DIG-CNT            PIC 9(03) COMP-5.
           05  WS-OUT-POS            PIC 9(03) COMP-5.
           05  WS-MAP-POS            PIC 9(03) COMP-5.
           05  WS-SUM-1              PIC 9(09) COMP-5.
           05  WS-SUM-2              PIC 9(09) COMP-5.
           05  WS-SUM-3              PIC 9(09) COMP-5.
           05  WS-WEIGHT             PIC 9(04) COMP-5.
           05  WS-VAL                PIC 9(04) COMP-5.
           05  WS-REM                PIC 9(04) COMP-5.
       01  WS-FLAGS.
           05  WS-ERR-SW             PIC X(01) VALUE SPACE.
               88  WS-ERR                    VALUE '1'.

       LINKAGE SECTION.
           COPY LK-SIG-PARM.

       PROCEDURE DIVISION USING LK-SIG-PARM.
       0000-MAIN SECTION.
       0000-START.
           MOVE 0 TO RETURN-CODE
           PERFORM 1000-INITIALIZE
           PERFORM 2000-VALIDATE
           IF WS-ERR
              MOVE 8 TO RETURN-CODE
              GOBACK
           END-IF
           PERFORM 3000-EDIT-INPUT
           PERFORM 4000-CALC-CHECK
           STRING WS-SIGN-BODY DELIMITED BY SIZE
                  WS-CHECK-CHAR DELIMITED BY SIZE
             INTO WS-SIGNATURE
           END-STRING
           MOVE WS-SIGNATURE TO LK-SIGNATURE
           GOBACK
           .

       1000-INITIALIZE SECTION.
       1000-START.
           MOVE SPACES TO WS-BRANCH-EDIT
                          WS-DT-EDIT
                          WS-SEQ-EDIT
                          WS-AMT-EDIT
                          WS-SIGN-BODY
                          WS-CHECK-CHAR
                          WS-SIGNATURE
                          WS-SRC
                          WS-DIGITS
                          LK-SIGNATURE
           MOVE ZERO TO WS-AMT-N
                        WS-DT-N
                        WS-YYYY
                        WS-MM
                        WS-DD
                        WS-MAX-DD
                        WS-SUM-1
                        WS-SUM-2
                        WS-SUM-3
           MOVE SPACE TO WS-ERR-SW
           .

       2000-VALIDATE SECTION.
       2000-START.
           MOVE LK-SIG-BRANCH TO WS-SRC
           PERFORM 2100-CHECK-DIGIT-SRC
           IF WS-DIG-CNT = ZERO
              DISPLAY 'TG931S 支店番号未設定'
              SET WS-ERR TO TRUE
           END-IF

           MOVE LK-SIG-ORIG-SEQ TO WS-SRC
           PERFORM 2100-CHECK-DIGIT-SRC
           IF WS-DIG-CNT = ZERO
              DISPLAY 'TG931S 原発信連番未設定'
              SET WS-ERR TO TRUE
           END-IF

           MOVE LK-SIG-DT TO WS-DT-N
           MOVE WS-DT-N TO WS-DT-EDIT
           IF WS-DT-EDIT NOT NUMERIC
              DISPLAY 'TG931S 署名日付数字不正'
              SET WS-ERR TO TRUE
           ELSE
              MOVE WS-DT-EDIT(1:4) TO WS-YYYY
              MOVE WS-DT-EDIT(5:2) TO WS-MM
              MOVE WS-DT-EDIT(7:2) TO WS-DD
              PERFORM 2200-CHECK-DATE
           END-IF

           MOVE LK-SIG-AMT TO WS-AMT-N
           IF WS-AMT-N = ZERO
              DISPLAY 'TG931S 署名金額ゼロ'
              SET WS-ERR TO TRUE
           END-IF
           .

       2100-CHECK-DIGIT-SRC SECTION.
       2100-START.
           MOVE ZERO TO WS-DIG-CNT
           PERFORM VARYING WS-IDX FROM 1 BY 1
             UNTIL WS-IDX > 40
              MOVE WS-SRC(WS-IDX:1) TO WS-CHAR
              IF WS-CHAR NOT = SPACE
                 IF WS-CHAR IS NUMERIC
                    ADD 1 TO WS-DIG-CNT
                 ELSE
                    DISPLAY 'TG931S 数字項目不正'
                    SET WS-ERR TO TRUE
                 END-IF
              END-IF
           END-PERFORM
           .

       2200-CHECK-DATE SECTION.
       2200-START.
           IF WS-YYYY < 1989 OR WS-YYYY > 2099
              DISPLAY 'TG931S 署名日付年不正'
              SET WS-ERR TO TRUE
           END-IF
           IF WS-MM < 1 OR WS-MM > 12
              DISPLAY 'TG931S 署名日付月不正'
              SET WS-ERR TO TRUE
           ELSE
              EVALUATE WS-MM
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
                 WHEN OTHER
                    MOVE 28 TO WS-MAX-DD
                    IF FUNCTION MOD(WS-YYYY 400) = ZERO
                       MOVE 29 TO WS-MAX-DD
                    ELSE
                       IF FUNCTION MOD(WS-YYYY 4) = ZERO
                          AND FUNCTION MOD(WS-YYYY 100) NOT = ZERO
                          MOVE 29 TO WS-MAX-DD
                       END-IF
                    END-IF
              END-EVALUATE
              IF WS-DD < 1 OR WS-DD > WS-MAX-DD
                 DISPLAY 'TG931S 署名日付日不正'
                 SET WS-ERR TO TRUE
              END-IF
           END-IF
           .

       3000-EDIT-INPUT SECTION.
       3000-START.
           MOVE LK-SIG-BRANCH TO WS-SRC
           MOVE 4 TO WS-OUT-LEN
           PERFORM 3100-ZERO-JUSTIFY
           MOVE WS-DIGITS(1:4) TO WS-BRANCH-EDIT

           MOVE WS-DT-N TO WS-DT-EDIT

           MOVE LK-SIG-ORIG-SEQ TO WS-SRC
           MOVE 10 TO WS-OUT-LEN
           PERFORM 3100-ZERO-JUSTIFY
           MOVE WS-DIGITS(1:10) TO WS-SEQ-EDIT

           MOVE WS-AMT-N TO WS-AMT-EDIT

           STRING WS-BRANCH-EDIT DELIMITED BY SIZE
                  WS-DT-EDIT     DELIMITED BY SIZE
                  WS-SEQ-EDIT    DELIMITED BY SIZE
                  WS-AMT-EDIT    DELIMITED BY SIZE
             INTO WS-SIGN-BODY
           END-STRING
           .

       3100-ZERO-JUSTIFY SECTION.
       3100-START.
           MOVE ALL '0' TO WS-DIGITS
           MOVE ZERO TO WS-DIG-CNT
           PERFORM VARYING WS-IDX FROM 1 BY 1
             UNTIL WS-IDX > 40
              IF WS-SRC(WS-IDX:1) IS NUMERIC
                 ADD 1 TO WS-DIG-CNT
              END-IF
           END-PERFORM
           IF WS-DIG-CNT > WS-OUT-LEN
              DISPLAY 'TG931S 編集桁数超過'
              SET WS-ERR TO TRUE
           ELSE
              COMPUTE WS-OUT-POS = WS-OUT-LEN - WS-DIG-CNT + 1
              PERFORM VARYING WS-IDX FROM 1 BY 1
                UNTIL WS-IDX > 40
                 IF WS-SRC(WS-IDX:1) IS NUMERIC
                    MOVE WS-SRC(WS-IDX:1)
                      TO WS-DIGITS(WS-OUT-POS:1)
                    ADD 1 TO WS-OUT-POS
                 END-IF
              END-PERFORM
           END-IF
           .

       4000-CALC-CHECK SECTION.
       4000-START.
           MOVE ZERO TO WS-SUM-1 WS-SUM-2 WS-SUM-3
           PERFORM VARYING WS-IDX FROM 1 BY 1
             UNTIL WS-IDX > 35
              MOVE WS-SIGN-BODY(WS-IDX:1) TO WS-CHAR
              COMPUTE WS-VAL = FUNCTION ORD(WS-CHAR) - 48
              COMPUTE WS-WEIGHT = FUNCTION MOD(WS-IDX 7) + 2
              COMPUTE WS-SUM-1 = WS-SUM-1 + (WS-VAL * WS-WEIGHT)
              COMPUTE WS-SUM-2 = WS-SUM-2 + (WS-VAL * WS-IDX)
              COMPUTE WS-SUM-3 =
                      WS-SUM-3 + (WS-VAL * (37 - WS-IDX))
           END-PERFORM

           COMPUTE WS-REM = FUNCTION MOD(WS-SUM-1 37) + 1
           MOVE WS-MAP(WS-REM:1) TO WS-CHECK-CHAR(1:1)

           COMPUTE WS-REM = FUNCTION MOD(WS-SUM-2 37) + 1
           MOVE WS-MAP(WS-REM:1) TO WS-CHECK-CHAR(2:1)

           COMPUTE WS-REM = FUNCTION MOD(WS-SUM-3 37) + 1
           MOVE WS-MAP(WS-REM:1) TO WS-CHECK-CHAR(3:1)
           .
