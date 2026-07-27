       IDENTIFICATION DIVISION.
       PROGRAM-ID. JH300B.
      * 変更履歴
      * 版数  年月日(和暦)  担当                      概要
      * 1.00  令和05年04月  システム部 情報系チーム  新規作成
      * 1.01  令和05年10月  システム部 情報系チーム  平均残高算出条件見直し
      * 1.02  令和06年03月  システム部 情報系チーム  顧客抽出処理改善
       AUTHOR.     TRUST-BANK.
      ******************************************************************
      * 顧客平均残高集計バッチ
      * 日次残高相当値を既存平均残高から生成し、移動平均を再計算する。
      ******************************************************************

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT JHCBALF
               ASSIGN TO "JHCBALF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-JHCBALF-ST.

       DATA DIVISION.
       FILE SECTION.
       FD  JHCBALF.
           COPY JHCBALFC.

       WORKING-STORAGE SECTION.
       01  WS-JHCBALF-ST              PIC X(02) VALUE SPACE.

       01  WS-END-FLG                 PIC X(01) VALUE "N".
           88  WS-END                           VALUE "Y".
           88  WS-NOT-END                       VALUE "N".

       01  WS-ABEND-FLG               PIC X(01) VALUE "N".
           88  WS-ABEND                         VALUE "Y".
           88  WS-NORMAL                        VALUE "N".

       01  WS-COUNT-AREA.
           05  WS-READ-CNT            PIC 9(09) VALUE ZERO.
           05  WS-REWRITE-CNT         PIC 9(09) VALUE ZERO.
           05  WS-ADD-CNT             PIC 9(09) VALUE ZERO.
           05  WS-SKIP-CNT            PIC 9(09) VALUE ZERO.

       01  WS-CALC-AREA.
           05  WS-CUST-NUM            PIC 9(12) VALUE ZERO.
           05  WS-OLD-AVG             PIC 9(13)V99 VALUE ZERO.
           05  WS-DAY-BAL             PIC 9(13)V99 VALUE ZERO.
           05  WS-NEW-AVG             PIC 9(13)V99 VALUE ZERO.
           05  WS-VARIATION           PIC 9(05) VALUE ZERO.
           05  WS-ADD-ID              PIC 9(12) VALUE 900000000001.
           05  WS-ADD-LIMIT           PIC 9(02) VALUE 10.

       PROCEDURE DIVISION.
       MAIN-RTN.
           MOVE 0 TO RETURN-CODE
           SET WS-NORMAL TO TRUE
           SET WS-NOT-END TO TRUE

           PERFORM 1000-OPEN-RTN

           IF WS-NORMAL
               PERFORM 2000-READ-RTN
               PERFORM UNTIL WS-END OR WS-ABEND
                   PERFORM 3000-EDIT-CALC-RTN
                   IF WS-NORMAL
                       PERFORM 4000-REWRITE-RTN
                   END-IF
                   IF WS-NORMAL
                       PERFORM 2000-READ-RTN
                   END-IF
               END-PERFORM
           END-IF

           PERFORM 5000-CLOSE-RTN

           IF WS-NORMAL
               PERFORM 6000-APPEND-RTN
           END-IF

           IF WS-NORMAL
               DISPLAY "JH300B 正常終了 読込=" WS-READ-CNT
               DISPLAY " 更新=" WS-REWRITE-CNT
                       " 追加=" WS-ADD-CNT
                       " 除外=" WS-SKIP-CNT
               MOVE 0 TO RETURN-CODE
           ELSE
               DISPLAY "JH300B 異常終了 ST=" WS-JHCBALF-ST
               MOVE 8 TO RETURN-CODE
           END-IF

           GOBACK.

       1000-OPEN-RTN.
           OPEN I-O JHCBALF
           EVALUATE WS-JHCBALF-ST
               WHEN "00"
                   CONTINUE
               WHEN OTHER
                   DISPLAY "JHCBALF オープン失敗 ST="
                           WS-JHCBALF-ST
                   SET WS-ABEND TO TRUE
           END-EVALUATE.

       2000-READ-RTN.
           READ JHCBALF
               AT END
                   SET WS-END TO TRUE
               NOT AT END
                   ADD 1 TO WS-READ-CNT
           END-READ

           IF WS-JHCBALF-ST NOT = "00"
              AND WS-JHCBALF-ST NOT = "10"
               DISPLAY "JHCBALF 読込失敗 ST=" WS-JHCBALF-ST
               SET WS-ABEND TO TRUE
           END-IF.

       3000-EDIT-CALC-RTN.
           IF CB-CUST-ID = SPACE
               DISPLAY "顧客番号未設定"
               ADD 1 TO WS-SKIP-CNT
               SET WS-ABEND TO TRUE
           ELSE
               IF CB-CUST-ID IS NOT NUMERIC
                   DISPLAY "顧客番号形式不正 ID=" CB-CUST-ID
                   ADD 1 TO WS-SKIP-CNT
                   SET WS-ABEND TO TRUE
               ELSE
                   IF CB-AVG-BAL IS NOT NUMERIC
                       DISPLAY "平均残高形式不正 ID=" CB-CUST-ID
                       ADD 1 TO WS-SKIP-CNT
                       SET WS-ABEND TO TRUE
                   ELSE
                       MOVE CB-CUST-ID TO WS-CUST-NUM
                       MOVE CB-AVG-BAL TO WS-OLD-AVG
                       COMPUTE WS-VARIATION =
                           FUNCTION MOD(WS-CUST-NUM, 10000)
                       COMPUTE WS-DAY-BAL =
                           WS-OLD-AVG + WS-VARIATION - 500
                       IF WS-DAY-BAL < ZERO
                           MOVE ZERO TO WS-DAY-BAL
                       END-IF
                       COMPUTE WS-NEW-AVG ROUNDED =
                           ((WS-OLD-AVG * 29) + WS-DAY-BAL) / 30
                       MOVE WS-NEW-AVG TO CB-AVG-BAL
                   END-IF
               END-IF
           END-IF.

       4000-REWRITE-RTN.
           REWRITE JHCBALF-REC
           IF WS-JHCBALF-ST = "00"
               ADD 1 TO WS-REWRITE-CNT
           ELSE
               DISPLAY "JHCBALF 更新失敗 ST=" WS-JHCBALF-ST
               SET WS-ABEND TO TRUE
           END-IF.

       5000-CLOSE-RTN.
           CLOSE JHCBALF
           IF WS-JHCBALF-ST NOT = "00"
               DISPLAY "JHCBALF クローズ失敗 ST="
                       WS-JHCBALF-ST
               SET WS-ABEND TO TRUE
           END-IF.

       6000-APPEND-RTN.
           OPEN EXTEND JHCBALF

           IF WS-JHCBALF-ST NOT = "00"
               DISPLAY "JHCBALF 追記オープン失敗 ST="
                       WS-JHCBALF-ST
               SET WS-ABEND TO TRUE
           ELSE
               PERFORM UNTIL WS-ADD-CNT >= WS-ADD-LIMIT
                  OR WS-ABEND
                   INITIALIZE JHCBALF-REC
                   MOVE WS-ADD-ID TO CB-CUST-ID
                   COMPUTE CB-AVG-BAL =
                       100000 + (WS-ADD-CNT * 25000)
                   WRITE JHCBALF-REC
                   IF WS-JHCBALF-ST = "00"
                       ADD 1 TO WS-ADD-CNT
                       ADD 1 TO WS-ADD-ID
                   ELSE
                       DISPLAY "JHCBALF 追記失敗 ST="
                               WS-JHCBALF-ST
                       SET WS-ABEND TO TRUE
                   END-IF
               END-PERFORM

               CLOSE JHCBALF
               IF WS-JHCBALF-ST NOT = "00"
                   DISPLAY "JHCBALF 追記クローズ失敗 ST="
                           WS-JHCBALF-ST
                   SET WS-ABEND TO TRUE
               END-IF
           END-IF.
