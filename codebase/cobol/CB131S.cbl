       IDENTIFICATION DIVISION.
       PROGRAM-ID. CB131S.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CDTRRF ASSIGN TO "CDTRRF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-CDTRRF-ST.

           SELECT CDRTRYF ASSIGN TO "CDRTRYF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS RTY-RETRY-ID
               FILE STATUS IS WS-CDRTRYF-ST.

       DATA DIVISION.
       FILE SECTION.
       FD  CDTRRF.
           COPY CDTRRC.

       FD  CDRTRYF.
           COPY CDRTRYC.

       WORKING-STORAGE SECTION.
       01  WS-CDTRRF-ST                 PIC XX VALUE SPACES.
       01  WS-CDRTRYF-ST                PIC XX VALUE SPACES.

       01  WS-CDTRRF-OPEN-FLG           PIC X VALUE "N".
           88  WS-CDTRRF-OPEN                 VALUE "Y".
       01  WS-CDRTRYF-OPEN-FLG          PIC X VALUE "N".
           88  WS-CDRTRYF-OPEN                VALUE "Y".

       01  WS-END-FLG                   PIC X VALUE "N".
           88  WS-END                         VALUE "Y".
           88  WS-NOT-END                     VALUE "N".

       01  WS-ABEND-FLG                 PIC X VALUE "N".
           88  WS-ABEND                       VALUE "Y".
           88  WS-NORMAL                      VALUE "N".

       01  WS-RETRY-FLG                 PIC X VALUE "N".
           88  WS-RETRY-OK                    VALUE "Y".
           88  WS-RETRY-NG                    VALUE "N".

       01  WS-DUN-FLG                   PIC X VALUE "N".
           88  WS-DUN-ON                      VALUE "Y".
           88  WS-DUN-OFF                     VALUE "N".

       01  WS-INVALID-FLG               PIC X VALUE "N".
           88  WS-INVALID-ON                  VALUE "Y".
           88  WS-INVALID-OFF                 VALUE "N".

       01  WS-COUNT-AREA.
           05  WS-IN-CNT                PIC 9(9) VALUE ZERO.
           05  WS-RETRY-CNT             PIC 9(9) VALUE ZERO.
           05  WS-DUN-CNT               PIC 9(9) VALUE ZERO.
           05  WS-SKIP-CNT              PIC 9(9) VALUE ZERO.
           05  WS-ERR-CNT               PIC 9(9) VALUE ZERO.

       01  WS-DATE-AREA.
           05  WS-BASE-DT               PIC 9(8) VALUE ZERO.
           05  WS-NEXT-DT               PIC 9(8) VALUE ZERO.
           05  WS-INT-DATE              PIC 9(9) VALUE ZERO.
           05  WS-DAY-OF-WEEK           PIC 9 VALUE ZERO.
           05  WS-ADD-DAYS              PIC 9 VALUE ZERO.

       01  WS-CODE-AREA.
           05  WS-CLASS-CD              PIC X(2) VALUE SPACES.
           05  WS-REASON-TEXT           PIC X(40) VALUE SPACES.

       PROCEDURE DIVISION.
       MAIN-RTN.
           MOVE 0 TO RETURN-CODE
           PERFORM INIT-RTN
           IF WS-NORMAL
               PERFORM UNTIL WS-END OR WS-ABEND
                   PERFORM READ-CDTRRF-RTN
                   IF WS-NOT-END AND WS-NORMAL
                       PERFORM EDIT-INPUT-RTN
                       IF WS-INVALID-OFF
                           PERFORM CLASSIFY-RTN
                           IF WS-RETRY-OK
                               PERFORM MAKE-RETRY-RTN
                               PERFORM WRITE-CDRTRYF-RTN
                           ELSE
                               IF WS-DUN-ON
                                   ADD 1 TO WS-DUN-CNT
                                   DISPLAY "督促対象 結果ID="
                                           TRR-RESULT-ID
                                           " 理由="
                                           WS-REASON-TEXT
                               ELSE
                                   ADD 1 TO WS-SKIP-CNT
                               END-IF
                           END-IF
                       ELSE
                           ADD 1 TO WS-ERR-CNT
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
           PERFORM TERM-RTN
           GOBACK.

       INIT-RTN.
           SET WS-NOT-END TO TRUE
           SET WS-NORMAL TO TRUE

           OPEN INPUT CDTRRF
           IF WS-CDTRRF-ST = "00"
               MOVE "Y" TO WS-CDTRRF-OPEN-FLG
           ELSE
               DISPLAY "CDTRRF オープン失敗 ST=" WS-CDTRRF-ST
               SET WS-ABEND TO TRUE
               MOVE 8 TO RETURN-CODE
           END-IF

           IF WS-NORMAL
               OPEN OUTPUT CDRTRYF
               IF WS-CDRTRYF-ST = "00"
                   MOVE "Y" TO WS-CDRTRYF-OPEN-FLG
               ELSE
                   DISPLAY "CDRTRYF オープン失敗 ST="
                           WS-CDRTRYF-ST
                   SET WS-ABEND TO TRUE
                   MOVE 8 TO RETURN-CODE
               END-IF
           END-IF.

       READ-CDTRRF-RTN.
           READ CDTRRF
               AT END
                   SET WS-END TO TRUE
               NOT AT END
                   IF WS-CDTRRF-ST = "00"
                       ADD 1 TO WS-IN-CNT
                   ELSE
                       DISPLAY "CDTRRF 読込失敗 ST=" WS-CDTRRF-ST
                       SET WS-ABEND TO TRUE
                       MOVE 8 TO RETURN-CODE
                   END-IF
           END-READ.

       EDIT-INPUT-RTN.
           SET WS-INVALID-OFF TO TRUE

           IF TRR-RESULT-ID = SPACES
               DISPLAY "結果ID未設定 請求ID="
                       TRR-REQUEST-ID
               SET WS-INVALID-ON TO TRUE
           END-IF

           IF TRR-REQUEST-ID = SPACES
               DISPLAY "請求ID未設定 結果ID="
                       TRR-RESULT-ID
               SET WS-INVALID-ON TO TRUE
           END-IF

           IF TRR-CARD-NO = SPACES
               DISPLAY "カード番号未設定 結果ID="
                       TRR-RESULT-ID
               SET WS-INVALID-ON TO TRUE
           END-IF

           IF TRR-RESULT-CD = SPACES
               DISPLAY "結果コード未設定 結果ID="
                       TRR-RESULT-ID
               SET WS-INVALID-ON TO TRUE
           END-IF

           IF TRR-SETTLED-AMT <= ZERO
               DISPLAY "決済金額不正 結果ID="
                       TRR-RESULT-ID
               SET WS-INVALID-ON TO TRUE
           END-IF

           IF TRR-RESULT-DT < 19000101
              OR TRR-RESULT-DT > 20991231
               DISPLAY "結果日不正 結果ID="
                       TRR-RESULT-ID
               SET WS-INVALID-ON TO TRUE
           END-IF.

       CLASSIFY-RTN.
           SET WS-RETRY-NG TO TRUE
           SET WS-DUN-OFF TO TRUE
           MOVE SPACES TO WS-CLASS-CD
           MOVE SPACES TO WS-REASON-TEXT

           EVALUATE TRR-RESULT-CD
               WHEN "51"
               WHEN "Z1"
                   SET WS-RETRY-OK TO TRUE
                   MOVE "01" TO WS-CLASS-CD
                   MOVE "残高不足" TO WS-REASON-TEXT

               WHEN "91"
               WHEN "96"
                   SET WS-RETRY-OK TO TRUE
                   MOVE "02" TO WS-CLASS-CD
                   MOVE "金融機関一時障害" TO WS-REASON-TEXT

               WHEN "05"
               WHEN "14"
               WHEN "39"
                   SET WS-DUN-ON TO TRUE
                   MOVE "11" TO WS-CLASS-CD
                   MOVE "口座不備" TO WS-REASON-TEXT

               WHEN "54"
               WHEN "57"
               WHEN "62"
                   SET WS-DUN-ON TO TRUE
                   MOVE "12" TO WS-CLASS-CD
                   MOVE "取引停止" TO WS-REASON-TEXT

               WHEN "00"
                   MOVE "90" TO WS-CLASS-CD
                   MOVE "正常結果" TO WS-REASON-TEXT

               WHEN OTHER
                   SET WS-DUN-ON TO TRUE
                   MOVE "19" TO WS-CLASS-CD
                   MOVE "理由未定義" TO WS-REASON-TEXT
           END-EVALUATE

           IF WS-RETRY-OK
               IF TRR-RETURN-REASON = "R9"
                   SET WS-RETRY-NG TO TRUE
                   SET WS-DUN-ON TO TRUE
                   MOVE "13" TO WS-CLASS-CD
                   MOVE "再請求上限超過" TO WS-REASON-TEXT
               END-IF
           END-IF.

       MAKE-RETRY-RTN.
           MOVE LOW-VALUE TO CDRTRYF-REC

           STRING TRR-RESULT-ID DELIMITED BY SPACE
                  "-R1"         DELIMITED BY SIZE
             INTO RTY-RETRY-ID
           END-STRING

           MOVE TRR-CARD-NO TO RTY-CARD-NO
           MOVE TRR-REQUEST-ID TO RTY-ORIGINAL-REQUEST-ID
           MOVE 1 TO RTY-RETRY-COUNT
           MOVE TRR-SETTLED-AMT TO RTY-RETRY-AMT
           MOVE "0" TO RTY-RETRY-STATUS

           MOVE TRR-RESULT-DT TO WS-BASE-DT
           PERFORM CALC-NEXT-DATE-RTN
           MOVE WS-NEXT-DT TO RTY-NEXT-REQUEST-DT.

       CALC-NEXT-DATE-RTN.
           EVALUATE TRR-RESULT-CD
               WHEN "51"
               WHEN "Z1"
                   MOVE 3 TO WS-ADD-DAYS
               WHEN OTHER
                   MOVE 1 TO WS-ADD-DAYS
           END-EVALUATE

           COMPUTE WS-INT-DATE =
               FUNCTION INTEGER-OF-DATE(WS-BASE-DT) + WS-ADD-DAYS

           PERFORM UNTIL WS-DAY-OF-WEEK NOT = 6
                     AND WS-DAY-OF-WEEK NOT = 7
               COMPUTE WS-DAY-OF-WEEK =
                   FUNCTION MOD(WS-INT-DATE 7) + 1

               IF WS-DAY-OF-WEEK = 6
                  OR WS-DAY-OF-WEEK = 7
                   ADD 1 TO WS-INT-DATE
               END-IF
           END-PERFORM

           MOVE FUNCTION DATE-OF-INTEGER(WS-INT-DATE) TO WS-NEXT-DT.

       WRITE-CDRTRYF-RTN.
           WRITE CDRTRYF-REC

           EVALUATE WS-CDRTRYF-ST
               WHEN "00"
                   ADD 1 TO WS-RETRY-CNT
                   DISPLAY "再請求作成 結果ID="
                           TRR-RESULT-ID
                           " 次回請求日="
                           RTY-NEXT-REQUEST-DT

               WHEN "22"
                   ADD 1 TO WS-ERR-CNT
                   DISPLAY "再請求重複 結果ID="
                           TRR-RESULT-ID

               WHEN OTHER
                   DISPLAY "CDRTRYF 書込失敗 ST="
                           WS-CDRTRYF-ST
                           " 結果ID="
                           TRR-RESULT-ID
                   SET WS-ABEND TO TRUE
                   MOVE 8 TO RETURN-CODE
           END-EVALUATE.

       TERM-RTN.
           IF WS-CDTRRF-OPEN
               CLOSE CDTRRF
               IF WS-CDTRRF-ST NOT = "00"
                  AND WS-CDTRRF-ST NOT = "10"
                   DISPLAY "CDTRRF クローズ失敗 ST="
                           WS-CDTRRF-ST
                   MOVE 8 TO RETURN-CODE
               END-IF
           END-IF

           IF WS-CDRTRYF-OPEN
               CLOSE CDRTRYF
               IF WS-CDRTRYF-ST NOT = "00"
                   DISPLAY "CDRTRYF クローズ失敗 ST="
                           WS-CDRTRYF-ST
                   MOVE 8 TO RETURN-CODE
               END-IF
           END-IF

           DISPLAY "CB131S 終了 入力=" WS-IN-CNT
                   " 再請求=" WS-RETRY-CNT
                   " 督促=" WS-DUN-CNT
                   " 対象外=" WS-SKIP-CNT
                   " 異常=" WS-ERR-CNT.
