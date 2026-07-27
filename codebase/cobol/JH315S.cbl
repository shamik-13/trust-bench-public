       IDENTIFICATION DIVISION.
       PROGRAM-ID. JH315S.
      * 変更履歴
      * 版数  年月日(和暦)  担当                      概要
      * 1.00  令和04.04.01  システム部 情報系チーム  新規作成
      * 1.01  令和05.10.16  システム部 情報系チーム  判定条件追加
      * 1.02  令和06.07.22  システム部 情報系チーム  セグメント閾値見直し
       AUTHOR. TRUST-BATCH.
      ******************************************************************
      * 顧客平均残高から顧客セグメントを返却する純粋サブルーチン。
      * 五百万円以上を優良層、それ以外を一般層として区分する。
      ******************************************************************

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-CONSTANTS.
           05  WS-BAL-SG03-LIMIT       PIC S9(11)V99 COMP-3
                                       VALUE +5000000.00.
           05  WS-RET-NORMAL           PIC X(02) VALUE '00'.
           05  WS-RET-DATA-ERR         PIC X(02) VALUE '12'.
           05  WS-CD-GENERAL           PIC X(04) VALUE 'SG01'.
           05  WS-CD-GOOD              PIC X(04) VALUE 'SG03'.

       01  WS-WORK.
           05  WS-ABEND-MSG            PIC X(80).

       LINKAGE SECTION.
           COPY LK-SEG-PARM.

       PROCEDURE DIVISION USING LK-SEG-PARM.
       0000-MAIN SECTION.
       0000-START.
           PERFORM 1000-INIT
           PERFORM 2000-CHECK
           IF LK-SEG-RET = WS-RET-NORMAL
              PERFORM 3000-JUDGE
           END-IF
           PERFORM 9000-END
           GOBACK
           .

       1000-INIT SECTION.
       1000-START.
           MOVE 0                   TO RETURN-CODE
           MOVE WS-RET-NORMAL       TO LK-SEG-RET
           MOVE SPACES              TO LK-SEG-CD
           MOVE SPACES              TO LK-SEG-NAME
           MOVE SPACES              TO WS-ABEND-MSG
           .

       2000-CHECK SECTION.
       2000-START.
           IF LK-SEG-BAL IS NOT NUMERIC
              MOVE WS-RET-DATA-ERR  TO LK-SEG-RET
              MOVE '残高数値不正'   TO LK-SEG-NAME
              MOVE '残高数値不正'   TO WS-ABEND-MSG
              MOVE 8                TO RETURN-CODE
           ELSE
              IF LK-SEG-BAL < ZERO
                 MOVE WS-RET-DATA-ERR TO LK-SEG-RET
                 MOVE '残高マイナス不正' TO LK-SEG-NAME
                 MOVE '残高マイナス不正' TO WS-ABEND-MSG
                 MOVE 8             TO RETURN-CODE
              END-IF
           END-IF
           .

       3000-JUDGE SECTION.
       3000-START.
      *    平均残高を五百万円と比較してセグメントを判定する。
           IF LK-SEG-BAL >= WS-BAL-SG03-LIMIT
              MOVE WS-CD-GOOD       TO LK-SEG-CD
              MOVE '優良層'         TO LK-SEG-NAME
           ELSE
              MOVE WS-CD-GENERAL    TO LK-SEG-CD
              MOVE '一般層'         TO LK-SEG-NAME
           END-IF
           MOVE WS-RET-NORMAL       TO LK-SEG-RET
           .

       9000-END SECTION.
       9000-START.
           IF RETURN-CODE NOT = 0
              DISPLAY 'JH315S 判定異常 ' WS-ABEND-MSG
           END-IF
           .
