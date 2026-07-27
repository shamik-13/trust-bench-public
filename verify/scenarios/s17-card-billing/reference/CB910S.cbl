       IDENTIFICATION DIVISION.
       PROGRAM-ID. CB910S.
      *
      * 変更履歴
      * 版数  年月日    担当       概要
      * 1.00  20160712  開発一課   最低支払額算定サブルーチン新規作成
      * 1.01  20190305  開発一課   約定率を定数化（約定率３％）
      * 1.02  20240515  業務改革PT 改定後約定率と最低支払額を反映
      * 1.03  20240620  保守二課   延滞カードの算定経路を明確化
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-CALC-AREA.
           05  WS-CONTRACT-RATE        PIC 9V99 VALUE 0.05.
           05  WS-MINIMUM-AMT          PIC 9(09) VALUE 2000.
           05  WS-RATE-AMT             PIC 9(16)V99 VALUE ZERO.
           05  WS-TRUNC-AMT            PIC 9(16) VALUE ZERO.
           05  WS-RESULT-AMT           PIC 9(16) VALUE ZERO.
      *
       LINKAGE SECTION.
       COPY LK-MINPAY-PARM.
      *
       PROCEDURE DIVISION USING LK-MINPAY-PARM.
      *
       0000-MAIN SECTION.
       0000-MAIN-START.
           MOVE 0 TO RETURN-CODE
           PERFORM 1000-INIT
           PERFORM 2000-CHECK-PARM
           IF LK-MP-RET = '00'
              PERFORM 3000-CALC-MIN-PAY
           END-IF
           GOBACK
           .
      *
       1000-INIT SECTION.
       1000-INIT-START.
           MOVE ZERO TO LK-MP-MIN-PAY
           MOVE '00' TO LK-MP-RET
           MOVE ZERO TO WS-RATE-AMT
                        WS-TRUNC-AMT
                        WS-RESULT-AMT
           .
      *
       2000-CHECK-PARM SECTION.
       2000-CHECK-PARM-START.
           IF LK-MP-CLOSING-AMT IS NOT NUMERIC
              MOVE '91' TO LK-MP-RET
              MOVE 8 TO RETURN-CODE
              DISPLAY '最低支払額算定 入力金額不正'
           END-IF
      *
           IF LK-MP-RET = '00'
              IF LK-MP-CARD-STATUS IS NOT NUMERIC
                 MOVE '92' TO LK-MP-RET
                 MOVE 8 TO RETURN-CODE
                 DISPLAY '最低支払額算定 カード状態不正'
              END-IF
           END-IF
           .
      *
       3000-CALC-MIN-PAY SECTION.
       3000-CALC-MIN-PAY-START.
      *    債権管理規程: 締後残高が0円の場合は最低支払額も0円（下限¥2,000は適用しない）
           IF LK-MP-CLOSING-AMT = 0
              MOVE ZERO TO WS-RESULT-AMT
           ELSE
              COMPUTE WS-RATE-AMT =
                      LK-MP-CLOSING-AMT * WS-CONTRACT-RATE
              COMPUTE WS-TRUNC-AMT =
                      FUNCTION INTEGER-PART(WS-RATE-AMT)
      *
              IF WS-TRUNC-AMT < WS-MINIMUM-AMT
                 MOVE WS-MINIMUM-AMT TO WS-RESULT-AMT
              ELSE
                 MOVE WS-TRUNC-AMT TO WS-RESULT-AMT
              END-IF
           END-IF
      *
           MOVE WS-RESULT-AMT TO LK-MP-MIN-PAY
           MOVE '00' TO LK-MP-RET
           MOVE 0 TO RETURN-CODE
           .
