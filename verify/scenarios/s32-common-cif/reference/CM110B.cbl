       IDENTIFICATION DIVISION.
       PROGRAM-ID. CM110B.
       AUTHOR. 山岸 透.
      *
      * 顧客名寄せ統合キー生成バッチ (GOLDEN)。
      * 名寄せ統合キーの検査数字 = モジュラス11・ウェイト2〜7循環(右端から)。
      * 検査数字 = 11 - (重み付き合計 mod 11)。(11-剰余) が 10以上 のときは 0。
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CMCIFF ASSIGN TO "CMCIFF"
               ORGANIZATION IS SEQUENTIAL FILE STATUS IS WS-IN-ST.
           SELECT CMKEYF ASSIGN TO "CMKEYF"
               ORGANIZATION IS SEQUENTIAL FILE STATUS IS WS-OUT-ST.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  CMCIFF.
       COPY CMCIFFC.
       FD  CMKEYF.
       COPY CMKEYFC.
      *
       WORKING-STORAGE SECTION.
       01  WS-IN-ST              PIC X(02).
       01  WS-OUT-ST             PIC X(02).
       01  WS-EOF                PIC X(01) VALUE 'N'.
       01  WS-SEQ                PIC 9(08) VALUE ZERO.
      *
      * 統合キー本体(先頭10桁)と桁分解。
       01  WS-BODY-X             PIC X(10).
       01  WS-BODY-R REDEFINES WS-BODY-X.
           05  WS-DIG            PIC 9(01) OCCURS 10.
       01  WS-IDX                PIC 9(02).
       01  WS-POS                PIC 9(02).
       01  WS-WEIGHT             PIC 9(02).
       01  WS-SUM                PIC 9(09).
       01  WS-REM                PIC 9(02).
       01  WS-CD                 PIC 9(02).
      *
       PROCEDURE DIVISION.
       0000-MAIN SECTION.
       0000-START.
           OPEN INPUT CMCIFF
           OPEN OUTPUT CMKEYF
           IF WS-IN-ST NOT = "00"
              DISPLAY "CM110B CMCIFF OPEN ST=" WS-IN-ST
              MOVE 8 TO RETURN-CODE
              GOBACK
           END-IF
           PERFORM UNTIL WS-EOF = 'Y'
              READ CMCIFF AT END MOVE 'Y' TO WS-EOF
              NOT AT END
                 PERFORM 1000-PROCESS-CIF
              END-READ
           END-PERFORM
           CLOSE CMCIFF CMKEYF
           MOVE 0 TO RETURN-CODE
           GOBACK
           .
      *
       1000-PROCESS-CIF SECTION.
       1000-START.
           ADD 1 TO WS-SEQ
           INITIALIZE CMKEYF-REC
           STRING "CK" DELIMITED BY SIZE
                  WS-SEQ DELIMITED BY SIZE
                  INTO CK-KEY-ID
           END-STRING
           MOVE CF-CIF-NO TO CK-CIF-NO
      *
           IF CF-CIF-STATUS-KBN NOT = "01"
              MOVE ZERO TO CK-CHECK-DIGIT-CNT
              MOVE "S"  TO CK-KEY-STATUS-KBN
              WRITE CMKEYF-REC
              GO TO 1000-EXIT
           END-IF
      *
           PERFORM 2000-COMPUTE-CD
      *
           MOVE WS-CD TO CK-CHECK-DIGIT-CNT
           MOVE "K"   TO CK-KEY-STATUS-KBN
           WRITE CMKEYF-REC
           .
       1000-EXIT.
           EXIT.
      *
       2000-COMPUTE-CD SECTION.
       2000-START.
      *    名寄せ統合キー本体(先頭10桁)を取り出す
           MOVE CF-CIF-NO(1:10) TO WS-BODY-X
           MOVE ZERO TO WS-SUM
           MOVE ZERO TO WS-POS
      *    モジュラス11方式: 右端から ウェイト 2,3,4,5,6,7 を循環して付与
           PERFORM VARYING WS-IDX FROM 10 BY -1 UNTIL WS-IDX < 1
              ADD 1 TO WS-POS
              COMPUTE WS-WEIGHT = 2 + FUNCTION MOD(WS-POS - 1, 6)
              COMPUTE WS-SUM = WS-SUM + (WS-DIG(WS-IDX) * WS-WEIGHT)
           END-PERFORM
           COMPUTE WS-REM = FUNCTION MOD(WS-SUM, 11)
           COMPUTE WS-CD = 11 - WS-REM
           IF WS-CD >= 10
              MOVE ZERO TO WS-CD
           END-IF
           .
