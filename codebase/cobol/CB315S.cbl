       IDENTIFICATION DIVISION.
       PROGRAM-ID. CB315S.
       AUTHOR. TRUST-BATCH.
      ******************************************************************
      * 返品額検査サブ。
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CDCAPF-FILE ASSIGN TO "CDCAPF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-CDCAPF-ST.

       DATA DIVISION.
       FILE SECTION.
       FD  CDCAPF-FILE
           LABEL RECORDS ARE STANDARD.
           COPY CDCAPFC.

       WORKING-STORAGE SECTION.
       01  WS-CDCAPF-ST              PIC XX.
       01  WS-CDCAPF-OPEN-SW         PIC X VALUE SPACE.
           88  WS-CDCAPF-OPEN              VALUE '1'.
           88  WS-CDCAPF-CLOSED            VALUE SPACE.

       01  WS-EOF-SW                 PIC X VALUE SPACE.
           88  WS-EOF                      VALUE '1'.
           88  WS-NOT-EOF                  VALUE SPACE.

       01  WS-FOUND-SW               PIC X VALUE SPACE.
           88  WS-FOUND                    VALUE '1'.
           88  WS-NOT-FOUND                VALUE SPACE.

       01  WS-ABEND-SW               PIC X VALUE SPACE.
           88  WS-ABEND                    VALUE '1'.
           88  WS-NORMAL                   VALUE SPACE.

       01  WS-CALC-AREA.
           05  WS-REMAIN-AMT         PIC S9(13)V99 COMP-3 VALUE 0.
           05  WS-FILE-BILLED        PIC S9(13)V99 COMP-3 VALUE 0.
           05  WS-FILE-FEE           PIC S9(13)V99 COMP-3 VALUE 0.

       LINKAGE SECTION.
       01  LK-CB315S-PARM.
           05  LK-SALE-ID            PIC X(20).
           05  LK-RETURN-AMT         PIC S9(13)V99 COMP-3.
           05  LK-BILLED-AMT         PIC S9(13)V99 COMP-3.
           05  LK-ADJUST-AMT         PIC S9(13)V99 COMP-3.
           05  LK-RESULT-CD          PIC X.
               88  LK-RESULT-OK            VALUE '0'.
               88  LK-RESULT-NG            VALUE '1'.
               88  LK-RESULT-ERR           VALUE '9'.
           05  LK-FEE-RETURN-FLG     PIC X.
               88  LK-FEE-RETURN-NO        VALUE '0'.
               88  LK-FEE-RETURN-YES       VALUE '1'.
           05  LK-REASON-CD          PIC X(20).

       PROCEDURE DIVISION USING LK-CB315S-PARM.
       0000-MAIN SECTION.
       0000-START.
           PERFORM 1000-INIT
           IF WS-NORMAL
               PERFORM 2000-READ-CAPTURE
           END-IF
           IF WS-NORMAL
               PERFORM 3000-CHECK-RETURN
           END-IF
           PERFORM 9000-END
           GOBACK.

       1000-INIT SECTION.
       1000-START.
           SET WS-NORMAL TO TRUE
           SET WS-CDCAPF-CLOSED TO TRUE
           SET WS-NOT-EOF TO TRUE
           SET WS-NOT-FOUND TO TRUE
           MOVE '9'   TO LK-RESULT-CD
           MOVE '0'   TO LK-FEE-RETURN-FLG
           MOVE SPACE TO LK-REASON-CD
           MOVE 0     TO WS-REMAIN-AMT
           MOVE 0     TO WS-FILE-BILLED
           MOVE 0     TO WS-FILE-FEE
           MOVE SPACE TO WS-CDCAPF-ST

           IF LK-SALE-ID = SPACE
               MOVE '1' TO LK-RESULT-CD
               MOVE 'SALEID-MISIYOU' TO LK-REASON-CD
               DISPLAY 'CB315S 売上ID必須'
               SET WS-ABEND TO TRUE
               MOVE 8 TO RETURN-CODE
           END-IF

           IF WS-NORMAL
              AND LK-RETURN-AMT < 0
               MOVE '1' TO LK-RESULT-CD
               MOVE 'HENPIN-MINUS' TO LK-REASON-CD
               DISPLAY 'CB315S 返品額マイナス'
               SET WS-ABEND TO TRUE
               MOVE 8 TO RETURN-CODE
           END-IF

           IF WS-NORMAL
              AND LK-BILLED-AMT < 0
               MOVE '1' TO LK-RESULT-CD
               MOVE 'SEIKYU-MINUS' TO LK-REASON-CD
               DISPLAY 'CB315S 請求額マイナス'
               SET WS-ABEND TO TRUE
               MOVE 8 TO RETURN-CODE
           END-IF

           IF WS-NORMAL
              AND LK-ADJUST-AMT < 0
               MOVE '1' TO LK-RESULT-CD
               MOVE 'CHOSEI-MINUS' TO LK-REASON-CD
               DISPLAY 'CB315S 調整額マイナス'
               SET WS-ABEND TO TRUE
               MOVE 8 TO RETURN-CODE
           END-IF

           IF WS-NORMAL
               OPEN INPUT CDCAPF-FILE
               IF WS-CDCAPF-ST = '00'
                   SET WS-CDCAPF-OPEN TO TRUE
               ELSE
                   MOVE '9' TO LK-RESULT-CD
                   MOVE 'CDCAPF-OPEN' TO LK-REASON-CD
                   DISPLAY 'CB315S CDCAPFオープンST='
                           WS-CDCAPF-ST
                   SET WS-ABEND TO TRUE
                   MOVE 12 TO RETURN-CODE
               END-IF
           END-IF.

       2000-READ-CAPTURE SECTION.
       2000-START.
           PERFORM UNTIL WS-EOF OR WS-FOUND OR WS-ABEND
               READ CDCAPF-FILE
                   AT END
                       SET WS-EOF TO TRUE
                   NOT AT END
                       IF BC-SALE-ID = LK-SALE-ID
                           SET WS-FOUND TO TRUE
                       END-IF
               END-READ

               IF WS-CDCAPF-ST NOT = '00'
                  AND WS-CDCAPF-ST NOT = '10'
                   MOVE '9' TO LK-RESULT-CD
                   MOVE 'CDCAPF-READ' TO LK-REASON-CD
                   DISPLAY 'CB315S CDCAPF読込ST='
                           WS-CDCAPF-ST
                   SET WS-ABEND TO TRUE
                   MOVE 12 TO RETURN-CODE
               END-IF
           END-PERFORM

           IF WS-NORMAL
              AND WS-NOT-FOUND
               MOVE '1' TO LK-RESULT-CD
               MOVE 'CAP-NASHI' TO LK-REASON-CD
               DISPLAY 'CB315S 売上明細なし'
                       LK-SALE-ID
               SET WS-ABEND TO TRUE
               MOVE 8 TO RETURN-CODE
           END-IF.

       3000-CHECK-RETURN SECTION.
       3000-START.
           IF BC-CAP-STATUS NOT = 'C'
               MOVE '1' TO LK-RESULT-CD
               MOVE 'CAP-MIKAKUTEI' TO LK-REASON-CD
               DISPLAY 'CB315S 売上状態='
                       BC-CAP-STATUS
               SET WS-ABEND TO TRUE
               MOVE 8 TO RETURN-CODE
           END-IF

           IF WS-NORMAL
               MOVE BC-BILLED-AMT TO WS-FILE-BILLED
               MOVE BC-FEE-AMT    TO WS-FILE-FEE

               IF LK-BILLED-AMT NOT = WS-FILE-BILLED
                   MOVE '1' TO LK-RESULT-CD
                   MOVE 'SEIKYU-FUICHI' TO LK-REASON-CD
                   DISPLAY 'CB315S 請求額不一致'
                           LK-SALE-ID
                   SET WS-ABEND TO TRUE
                   MOVE 8 TO RETURN-CODE
               END-IF
           END-IF

           IF WS-NORMAL
               COMPUTE WS-REMAIN-AMT =
                   LK-BILLED-AMT - LK-ADJUST-AMT - LK-RETURN-AMT

               IF WS-REMAIN-AMT < 0
                   MOVE '1' TO LK-RESULT-CD
                   MOVE 'ZANDAKA-MINUS' TO LK-REASON-CD
                   DISPLAY 'CB315S 残高マイナス'
                           LK-SALE-ID
                   SET WS-ABEND TO TRUE
                   MOVE 8 TO RETURN-CODE
               ELSE
                   MOVE '0' TO LK-RESULT-CD
                   MOVE 'NORMAL' TO LK-REASON-CD
                   MOVE 0 TO RETURN-CODE
               END-IF
           END-IF

           IF WS-NORMAL
               IF BC-CURRENCY-CD NOT = 'JPY'
                  AND WS-FILE-FEE > 0
                  AND LK-RETURN-AMT > 0
                   MOVE '1' TO LK-FEE-RETURN-FLG
               ELSE
                   MOVE '0' TO LK-FEE-RETURN-FLG
               END-IF
           END-IF.

       9000-END SECTION.
       9000-START.
           IF WS-CDCAPF-OPEN
               CLOSE CDCAPF-FILE
               SET WS-CDCAPF-CLOSED TO TRUE
               IF WS-CDCAPF-ST NOT = '00'
                   MOVE '9' TO LK-RESULT-CD
                   MOVE 'CDCAPF-CLOSE' TO LK-REASON-CD
                   DISPLAY 'CB315S CDCAPFクローズST='
                           WS-CDCAPF-ST
                   MOVE 12 TO RETURN-CODE
               END-IF
           END-IF.
