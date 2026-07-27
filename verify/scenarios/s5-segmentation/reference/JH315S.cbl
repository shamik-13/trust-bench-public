       IDENTIFICATION DIVISION.
       PROGRAM-ID. JH315S.
       AUTHOR. TRUST-BATCH.
      *
      * 顧客平均残高から顧客セグメントを判定する純リンケージサブ。
      * 境界値は下限を含み、同値は上位セグメントへ分類する。
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-CONST.
           05  WS-BAL-ZERO        PIC S9(11)V99 COMP-3 VALUE ZERO.
           05  WS-BAL-SG02        PIC S9(11)V99 COMP-3
                                   VALUE 1000000.00.
           05  WS-BAL-SG03        PIC S9(11)V99 COMP-3
                                   VALUE 5000000.00.
           05  WS-BAL-SG04        PIC S9(11)V99 COMP-3
                                   VALUE 20000000.00.
           05  WS-BAL-SG05        PIC S9(11)V99 COMP-3
                                   VALUE 50000000.00.
       01  WS-SEGMENT.
           05  WS-SEG-CD          PIC X(04).
           05  WS-SEG-NAME        PIC X(40).
       01  WS-RETURN.
           05  WS-RET-NORMAL      PIC X(02) VALUE '00'.
           05  WS-RET-DATA-ERR    PIC X(02) VALUE '08'.
      *
       LINKAGE SECTION.
           COPY LK-SEG-PARM.
      *
       PROCEDURE DIVISION USING LK-SEG-PARM.
      *
       0000-MAIN SECTION.
       0000-START.
           PERFORM 1000-INIT
           PERFORM 2000-CHECK-BALANCE
           IF LK-SEG-RET = WS-RET-NORMAL
              PERFORM 3000-DECIDE-SEGMENT
              PERFORM 4000-SET-RESULT
           END-IF
           PERFORM 9000-END
           GOBACK.
      *
       1000-INIT SECTION.
       1000-START.
           MOVE SPACE         TO LK-SEG-CD
           MOVE SPACE         TO LK-SEG-NAME
           MOVE WS-RET-NORMAL TO LK-SEG-RET
           MOVE SPACE         TO WS-SEG-CD
           MOVE SPACE         TO WS-SEG-NAME
           MOVE 0             TO RETURN-CODE
           EXIT.
      *
       2000-CHECK-BALANCE SECTION.
       2000-START.
           IF LK-SEG-BAL < WS-BAL-ZERO
              MOVE WS-RET-DATA-ERR TO LK-SEG-RET
              MOVE 8               TO RETURN-CODE
           END-IF
           EXIT.
      *
       3000-DECIDE-SEGMENT SECTION.
       3000-START.
           EVALUATE TRUE
             WHEN LK-SEG-BAL >= WS-BAL-SG05
               MOVE 'SG05'              TO WS-SEG-CD
               MOVE 'プレミアム層'      TO WS-SEG-NAME
             WHEN LK-SEG-BAL >= WS-BAL-SG04
               MOVE 'SG04'              TO WS-SEG-CD
               MOVE '上位層'            TO WS-SEG-NAME
             WHEN LK-SEG-BAL >= WS-BAL-SG03
               MOVE 'SG03'              TO WS-SEG-CD
               MOVE '優良層'            TO WS-SEG-NAME
             WHEN LK-SEG-BAL >= WS-BAL-SG02
               MOVE 'SG02'              TO WS-SEG-CD
               MOVE '準優良層'          TO WS-SEG-NAME
             WHEN OTHER
               MOVE 'SG01'              TO WS-SEG-CD
               MOVE '一般層'            TO WS-SEG-NAME
           END-EVALUATE
           EXIT.
      *
       4000-SET-RESULT SECTION.
       4000-START.
           MOVE WS-SEG-CD    TO LK-SEG-CD
           MOVE WS-SEG-NAME  TO LK-SEG-NAME
           MOVE WS-RET-NORMAL TO LK-SEG-RET
           EXIT.
      *
       9000-END SECTION.
       9000-START.
           EXIT.
