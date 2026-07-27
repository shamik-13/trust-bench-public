       IDENTIFICATION DIVISION.
       PROGRAM-ID. CB910S.
       AUTHOR.     和田 美咲.
      ******************************************************************
      * 変更履歴
      * 版数  年月日    担当       概要
      * 1.00  20160712  開発一課   最低支払額算定サブルーチン新規作成
      * 1.01  20190305  開発一課   約定率を定数化（約定率３％）
      ******************************************************************
      *  最低支払額算定サブルーチン
      *  旧約定率３％、最低支払額の下限適用なし。
      *  延滞区分０９も本処理では通常残高に対する率計算のみ行う。
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-WORK-AREA.
           05  WS-OLD-CONTRACT-RATE     PIC 9V9999 VALUE 0.0300.
           05  WS-CALC-AMT              PIC S9(15)V9999 VALUE ZERO.
           05  WS-TRUNC-AMT             PIC S9(15)      VALUE ZERO.
           05  WS-ERR-MSG               PIC X(80)       VALUE SPACE.
      *
       LINKAGE SECTION.
       COPY LK-MINPAY-PARM.
      *
       PROCEDURE DIVISION USING LK-MINPAY-PARM.
      *
       0000-MAIN SECTION.
       0000-MAIN-START.
           MOVE 0 TO RETURN-CODE
           MOVE SPACE TO WS-ERR-MSG
           MOVE '00' TO LK-MP-RET
      *
           PERFORM 1000-VALIDATE-PARM
      *
           IF RETURN-CODE NOT = 0
              PERFORM 9000-ERROR-END
           ELSE
              PERFORM 2000-CALCULATE-MINPAY
              PERFORM 8000-NORMAL-END
           END-IF
           .
      *
       1000-VALIDATE-PARM SECTION.
       1000-VALIDATE-PARM-START.
           IF LK-MP-CLOSING-AMT < ZERO
              MOVE 8 TO RETURN-CODE
              MOVE '91' TO LK-MP-RET
              MOVE '締日残高が負値' TO WS-ERR-MSG
           ELSE
              EVALUATE LK-MP-CARD-STATUS
                 WHEN '00'
                 WHEN '01'
                 WHEN '09'
                    CONTINUE
                 WHEN OTHER
                    MOVE 8 TO RETURN-CODE
                    MOVE '92' TO LK-MP-RET
                    MOVE 'カード状態不正' TO WS-ERR-MSG
              END-EVALUATE
           END-IF
           .
      *
       2000-CALCULATE-MINPAY SECTION.
       2000-CALCULATE-MINPAY-START.
           COMPUTE WS-CALC-AMT =
                   LK-MP-CLOSING-AMT * WS-OLD-CONTRACT-RATE
           COMPUTE WS-TRUNC-AMT = FUNCTION INTEGER(WS-CALC-AMT)
           MOVE WS-TRUNC-AMT TO LK-MP-MIN-PAY
           MOVE '00' TO LK-MP-RET
           .
      *
       8000-NORMAL-END SECTION.
       8000-NORMAL-END-START.
           MOVE 0 TO RETURN-CODE
           GOBACK
           .
      *
       9000-ERROR-END SECTION.
       9000-ERROR-END-START.
           DISPLAY 'CB910S 入力エラー 理由=' WS-ERR-MSG
           IF LK-MP-MIN-PAY IS NUMERIC
              MOVE ZERO TO LK-MP-MIN-PAY
           END-IF
           GOBACK
           .
