       IDENTIFICATION DIVISION.
       PROGRAM-ID. LF290S.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LFCVPF
               ASSIGN TO "LFCVPF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-LFCVPF-ST.

       DATA DIVISION.
       FILE SECTION.
       FD  LFCVPF.
       COPY LFCVPFC.

       WORKING-STORAGE SECTION.
       01  WS-LFCVPF-ST              PIC XX.
       01  WS-END-SW                 PIC X VALUE SPACE.
           88  WS-END                VALUE '1'.
       01  WS-FOUND-SW               PIC X VALUE SPACE.
           88  WS-FOUND              VALUE '1'.
       01  WS-HARD-ERR-SW            PIC X VALUE SPACE.
           88  WS-HARD-ERR           VALUE '1'.

       01  WS-CV-STATUS.
           05  WS-CV-TAISHO          PIC XX VALUE '01'.
           05  WS-CV-JOGAI           PIC XX VALUE '08'.
           05  WS-CV-MUKO            PIC XX VALUE '09'.

       01  WS-WORK.
           05  WS-READ-CNT           PIC 9(9) VALUE ZERO.

       LINKAGE SECTION.
       01  LK-LF290S-AREA.
           05  LK-POL-NO             PIC X(12).
           05  LK-ELAPSED-MONTH-CNT  PIC 9(03).
           05  LK-CV-STATUS-KBN      PIC XX.
           05  LK-CALC-TARGET-FLG    PIC X.
           05  LK-RESULT-CD          PIC XX.
           05  LK-REASON-TEXT        PIC X(40).

       PROCEDURE DIVISION USING LK-LF290S-AREA.

       0000-MAIN.
           MOVE 8 TO RETURN-CODE
           PERFORM 1000-INIT
           IF NOT WS-HARD-ERR
               PERFORM 2000-READ-LFCVPF
           END-IF
           IF NOT WS-HARD-ERR
               PERFORM 3000-VALIDATE
           END-IF
           PERFORM 9000-FINAL
           GOBACK
           .

       1000-INIT.
           MOVE SPACE TO WS-END-SW
           MOVE SPACE TO WS-FOUND-SW
           MOVE SPACE TO WS-HARD-ERR-SW
           MOVE ZERO  TO WS-READ-CNT
           MOVE ZERO  TO LK-ELAPSED-MONTH-CNT
           MOVE SPACE TO LK-CV-STATUS-KBN
           MOVE '0'   TO LK-CALC-TARGET-FLG
           MOVE '00'  TO LK-RESULT-CD
           MOVE SPACE TO LK-REASON-TEXT

           IF LK-POL-NO = SPACE
               MOVE '1' TO WS-HARD-ERR-SW
               MOVE '12' TO LK-RESULT-CD
               MOVE '証券番号未設定' TO LK-REASON-TEXT
               DISPLAY 'LF290S 証券番号未設定'
           ELSE
               OPEN INPUT LFCVPF
               IF WS-LFCVPF-ST NOT = '00'
                   MOVE '1' TO WS-HARD-ERR-SW
                   MOVE '91' TO LK-RESULT-CD
                   STRING 'LFCVPF オープン失敗 ST='
                          WS-LFCVPF-ST
                       DELIMITED BY SIZE
                       INTO LK-REASON-TEXT
                   DISPLAY 'LF290S LFCVPF オープン失敗 ST='
                           WS-LFCVPF-ST
               END-IF
           END-IF
           .

       2000-READ-LFCVPF.
           PERFORM UNTIL WS-END OR WS-FOUND OR WS-HARD-ERR
               READ LFCVPF
                   AT END
                       MOVE '1' TO WS-END-SW
                   NOT AT END
                       ADD 1 TO WS-READ-CNT
                       IF CI-POL-NO = LK-POL-NO
                           MOVE '1' TO WS-FOUND-SW
                           MOVE CI-ELAPSED-MONTH-CNT
                               TO LK-ELAPSED-MONTH-CNT
                           MOVE CI-CV-STATUS-KBN
                               TO LK-CV-STATUS-KBN
                       END-IF
               END-READ

               IF WS-LFCVPF-ST NOT = '00'
                  AND WS-LFCVPF-ST NOT = '10'
                   MOVE '1' TO WS-HARD-ERR-SW
                   MOVE '92' TO LK-RESULT-CD
                   STRING 'LFCVPF 読込失敗 ST='
                          WS-LFCVPF-ST
                       DELIMITED BY SIZE
                       INTO LK-REASON-TEXT
                   DISPLAY 'LF290S LFCVPF 読込失敗 ST='
                           WS-LFCVPF-ST
               END-IF
           END-PERFORM

           IF NOT WS-HARD-ERR
              AND NOT WS-FOUND
               MOVE '04' TO LK-RESULT-CD
               MOVE '証券番号該当なし' TO LK-REASON-TEXT
               DISPLAY 'LF290S 証券番号該当なし '
                       LK-POL-NO
           END-IF
           .

       3000-VALIDATE.
           IF LK-RESULT-CD = '04'
               CONTINUE
           ELSE
               EVALUATE TRUE
                   WHEN LK-ELAPSED-MONTH-CNT IS NOT NUMERIC
                       MOVE '21' TO LK-RESULT-CD
                       MOVE '経過月数数値不正'
                           TO LK-REASON-TEXT
                       DISPLAY 'LF290S 経過月数数値不正 '
                               LK-POL-NO

                   WHEN LK-CV-STATUS-KBN = WS-CV-TAISHO
                       MOVE '1' TO LK-CALC-TARGET-FLG
                       MOVE '00' TO LK-RESULT-CD
                       MOVE '正常' TO LK-REASON-TEXT

                   WHEN LK-CV-STATUS-KBN = WS-CV-JOGAI
                       MOVE '0' TO LK-CALC-TARGET-FLG
                       MOVE '08' TO LK-RESULT-CD
                       MOVE '計算除外状態' TO LK-REASON-TEXT

                   WHEN LK-CV-STATUS-KBN = WS-CV-MUKO
                       MOVE '0' TO LK-CALC-TARGET-FLG
                       MOVE '09' TO LK-RESULT-CD
                       MOVE '無効状態' TO LK-REASON-TEXT

                   WHEN OTHER
                       MOVE '0' TO LK-CALC-TARGET-FLG
                       MOVE '22' TO LK-RESULT-CD
                       MOVE '返戻金状態区分不正'
                           TO LK-REASON-TEXT
                       DISPLAY 'LF290S 返戻金状態区分不正 '
                       DISPLAY 'POL=' LK-POL-NO
                               ' KBN=' LK-CV-STATUS-KBN
               END-EVALUATE
           END-IF
           .

       9000-FINAL.
           IF WS-LFCVPF-ST = '00'
              OR WS-LFCVPF-ST = '10'
               CLOSE LFCVPF
               IF WS-LFCVPF-ST NOT = '00'
                   MOVE '1' TO WS-HARD-ERR-SW
                   MOVE '93' TO LK-RESULT-CD
                   STRING 'LFCVPF クローズ失敗 ST='
                          WS-LFCVPF-ST
                       DELIMITED BY SIZE
                       INTO LK-REASON-TEXT
                   DISPLAY 'LF290S LFCVPF クローズ失敗 ST='
                           WS-LFCVPF-ST
               END-IF
           END-IF

           IF LK-RESULT-CD = '00'
              OR LK-RESULT-CD = '04'
              OR LK-RESULT-CD = '08'
              OR LK-RESULT-CD = '09'
              OR LK-RESULT-CD = '21'
              OR LK-RESULT-CD = '22'
               MOVE 0 TO RETURN-CODE
           ELSE
               MOVE 8 TO RETURN-CODE
           END-IF
           .
