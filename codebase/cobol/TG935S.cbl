       IDENTIFICATION DIVISION.
       PROGRAM-ID. TG935S.
      *================================================================*
      * 変更履歴                                                       *
      * 版数  年月日     担当                         概要             *
      * 1.00  平成18.04  システム部 情報系チーム       新規作成         *
      * 1.01  平成21.10  システム部 対外系チーム       判定文言見直し   *
      * 1.02  令和02.06  システム部 情報系/対外系チーム 保守対応       *
      *================================================================*
       AUTHOR. TRUST-BANK-BATCH.
      *================================================================*
      * 相手行別交換尻算定サブルーチン                                 *
      * 純粋LINKAGE処理。ファイル入出力は行わない。                    *
      * 支払金額と受取金額から相手行別のネット額を算定する。          *
      *================================================================*

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-WORK-AREA.
           05  WS-CALC-AMT             PIC S9(14) COMP-3 VALUE ZERO.
           05  WS-MAX-AMT              PIC S9(14) COMP-3
                                                   VALUE +9999999999999.
           05  WS-MIN-AMT              PIC S9(14) COMP-3
                                                   VALUE -9999999999999.

       LINKAGE SECTION.
       COPY LK-NET-PARM.

       PROCEDURE DIVISION USING LK-NET-PARM.

       0000-MAIN SECTION.
       0000-START.
           MOVE 0 TO RETURN-CODE

           PERFORM 1000-VALIDATE

           IF RETURN-CODE = 0
              PERFORM 2000-CALCULATE
           END-IF

           GOBACK
           .

       1000-VALIDATE SECTION.
       1000-START.
           IF LK-NET-PAY-AMT NOT NUMERIC
              MOVE '91' TO LK-NET-RET
              DISPLAY 'TG935S 支払金額パック不正'
              MOVE 8 TO RETURN-CODE
           END-IF

           IF RETURN-CODE = 0
              IF LK-NET-RECV-AMT NOT NUMERIC
                 MOVE '92' TO LK-NET-RET
                 DISPLAY 'TG935S 受取金額パック不正'
                 MOVE 8 TO RETURN-CODE
              END-IF
           END-IF
           .

       2000-CALCULATE SECTION.
       2000-START.
      *    交換尻ネット額を算定する。
           COMPUTE WS-CALC-AMT =
                   LK-NET-PAY-AMT - LK-NET-RECV-AMT

           IF WS-CALC-AMT > WS-MAX-AMT
              MOVE '93' TO LK-NET-RET
              DISPLAY 'TG935S 交換尻上限超過'
              MOVE 8 TO RETURN-CODE
           ELSE
              IF WS-CALC-AMT < WS-MIN-AMT
                 MOVE '94' TO LK-NET-RET
                 DISPLAY 'TG935S 交換尻下限超過'
                 MOVE 8 TO RETURN-CODE
              ELSE
                 MOVE WS-CALC-AMT TO LK-NET-AMT
                 MOVE '00' TO LK-NET-RET
                 MOVE 0 TO RETURN-CODE
              END-IF
           END-IF
           .
