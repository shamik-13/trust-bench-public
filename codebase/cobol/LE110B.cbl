       IDENTIFICATION DIVISION.
       PROGRAM-ID. LE110B.
      ******************************************************************
      * 解約返戻金仕訳作成バッチ
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LFACJF ASSIGN TO "LFACJF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-LFACJF-ST.
           SELECT LFCVRF ASSIGN TO "LFCVRF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-LFCVRF-ST.
           SELECT LEJRNF ASSIGN TO "LEJRNF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-LEJRNF-ST.

       DATA DIVISION.
       FILE SECTION.
       FD  LFACJF.
           COPY LFACJC.
       FD  LFCVRF.
           COPY LFCVRFC.
       FD  LEJRNF.
           COPY LEJRNC.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05 WS-LFACJF-ST              PIC XX VALUE SPACES.
           05 WS-LFCVRF-ST              PIC XX VALUE SPACES.
           05 WS-LEJRNF-ST              PIC XX VALUE SPACES.

       01  WS-FLAGS.
           05 WS-LFACJF-EOF             PIC X VALUE "N".
              88 LFACJF-END                  VALUE "Y".
           05 WS-LFCVRF-EOF             PIC X VALUE "N".
              88 LFCVRF-END                  VALUE "Y".
           05 WS-HARD-ERROR             PIC X VALUE "N".
              88 HARD-ERROR                  VALUE "Y".
           05 WS-MATCH-FOUND            PIC X VALUE "N".
              88 MATCH-FOUND                 VALUE "Y".
           05 WS-VALID-JOURNAL          PIC X VALUE "N".
              88 VALID-JOURNAL               VALUE "Y".

       01  WS-CONSTANTS.
           05 WS-APPROVED-KBN           PIC XX VALUE "02".
           05 WS-CALC-OK-KBN            PIC XX VALUE "01".
           05 WS-EVENT-REFUND           PIC XX VALUE "01".
           05 WS-EVENT-LOAN-OFFSET      PIC XX VALUE "02".
           05 WS-EVENT-ROUNDING         PIC XX VALUE "03".
           05 WS-JR-READY-KBN           PIC XX VALUE "01".
           05 WS-MAX-CV-CNT             PIC 9(5) VALUE 20000.
           05 WS-ROUND-LIMIT            PIC S9(13)V99 VALUE 999.99.

       01  WS-WORK.
           05 WS-CV-CNT                 PIC 9(5) VALUE 0.
           05 WS-CV-IDX                 PIC 9(5) VALUE 0.
           05 WS-JOURNAL-SEQ            PIC 9(9) VALUE 0.
           05 WS-AJ-AMT                 PIC S9(13)V99 VALUE 0.
           05 WS-ABS-AMT                PIC S9(13)V99 VALUE 0.
           05 WS-ERR-MSG                PIC X(80) VALUE SPACES.
           05 WS-ACCEPT-DATE            PIC 9(8) VALUE 0.

       01  WS-COUNTERS.
           05 WS-LFACJF-READ-CNT        PIC 9(9) VALUE 0.
           05 WS-LFCVRF-READ-CNT        PIC 9(9) VALUE 0.
           05 WS-LEJRNF-WRITE-CNT       PIC 9(9) VALUE 0.
           05 WS-SKIP-CNT               PIC 9(9) VALUE 0.
           05 WS-ERR-CNT                PIC 9(9) VALUE 0.

       01  WS-CV-TABLE.
           05 WS-CV-ENT OCCURS 20000 TIMES.
              10 T-CO-CV-ID             PIC X(20).
              10 T-CO-POL-NO            PIC X(20).
              10 T-CO-RESERVE-AMT       PIC S9(13)V99.
              10 T-CO-SURR-CHARGE-AMT   PIC S9(13)V99.
              10 T-CO-CV-AMT            PIC S9(13)V99.
              10 T-CO-CALC-STATUS-KBN   PIC XX.

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE 0 TO RETURN-CODE
           ACCEPT WS-ACCEPT-DATE FROM DATE YYYYMMDD
           PERFORM 1000-OPEN-FILES
           IF NOT HARD-ERROR
              PERFORM 2000-LOAD-CV-FILE
           END-IF
           IF NOT HARD-ERROR
              PERFORM 3000-PROCESS-ADJ-FILE
           END-IF
           PERFORM 9000-CLOSE-FILES
           IF HARD-ERROR
              MOVE 12 TO RETURN-CODE
           ELSE
              DISPLAY "LE110B 正常終了 LFACJF読込="
                      WS-LFACJF-READ-CNT
              DISPLAY "LE110B 正常終了 LEJRNF作成="
                      WS-LEJRNF-WRITE-CNT
              DISPLAY "LE110B 読飛件数=" WS-SKIP-CNT
              MOVE 0 TO RETURN-CODE
           END-IF
           GOBACK.

       1000-OPEN-FILES.
           OPEN INPUT LFACJF
           IF WS-LFACJF-ST NOT = "00"
              DISPLAY "LFACJF オープン失敗 ST=" WS-LFACJF-ST
              SET HARD-ERROR TO TRUE
           END-IF

           IF NOT HARD-ERROR
              OPEN INPUT LFCVRF
              IF WS-LFCVRF-ST NOT = "00"
                 DISPLAY "LFCVRF オープン失敗 ST=" WS-LFCVRF-ST
                 SET HARD-ERROR TO TRUE
              END-IF
           END-IF

           IF NOT HARD-ERROR
              OPEN OUTPUT LEJRNF
              IF WS-LEJRNF-ST NOT = "00"
                 DISPLAY "LEJRNF オープン失敗 ST=" WS-LEJRNF-ST
                 SET HARD-ERROR TO TRUE
              END-IF
           END-IF.

       2000-LOAD-CV-FILE.
           PERFORM UNTIL LFCVRF-END OR HARD-ERROR
              READ LFCVRF
                 AT END
                    SET LFCVRF-END TO TRUE
                 NOT AT END
                    ADD 1 TO WS-LFCVRF-READ-CNT
                    PERFORM 2100-STORE-CV-REC
              END-READ
           END-PERFORM.

       2100-STORE-CV-REC.
           IF CO-CALC-STATUS-KBN NOT = WS-CALC-OK-KBN
              ADD 1 TO WS-SKIP-CNT
           ELSE
              IF CO-POL-NO = SPACES
                 DISPLAY "LFCVRF 証券番号未設定 CV-ID="
                         CO-CV-ID
                 ADD 1 TO WS-ERR-CNT
                 SET HARD-ERROR TO TRUE
              ELSE
                 IF WS-CV-CNT >= WS-MAX-CV-CNT
                    DISPLAY "LFCVRF テーブル件数超過"
                    SET HARD-ERROR TO TRUE
                 ELSE
                    ADD 1 TO WS-CV-CNT
                    MOVE CO-CV-ID
                      TO T-CO-CV-ID(WS-CV-CNT)
                    MOVE CO-POL-NO
                      TO T-CO-POL-NO(WS-CV-CNT)
                    MOVE CO-RESERVE-AMT
                      TO T-CO-RESERVE-AMT(WS-CV-CNT)
                    MOVE CO-SURR-CHARGE-AMT
                      TO T-CO-SURR-CHARGE-AMT(WS-CV-CNT)
                    MOVE CO-CV-AMT
                      TO T-CO-CV-AMT(WS-CV-CNT)
                    MOVE CO-CALC-STATUS-KBN
                      TO T-CO-CALC-STATUS-KBN(WS-CV-CNT)
                 END-IF
              END-IF
           END-IF.

       3000-PROCESS-ADJ-FILE.
           PERFORM UNTIL LFACJF-END OR HARD-ERROR
              READ LFACJF
                 AT END
                    SET LFACJF-END TO TRUE
                 NOT AT END
                    ADD 1 TO WS-LFACJF-READ-CNT
                    PERFORM 3100-PROCESS-ADJ-REC
              END-READ
           END-PERFORM.

       3100-PROCESS-ADJ-REC.
           IF AJ-POST-STATUS-KBN NOT = WS-APPROVED-KBN
              ADD 1 TO WS-SKIP-CNT
           ELSE
              PERFORM 3200-FIND-CV-REC
              PERFORM 3300-VALIDATE-ADJ
              IF VALID-JOURNAL
                 PERFORM 3400-WRITE-JOURNAL
              ELSE
                 ADD 1 TO WS-ERR-CNT
                 DISPLAY "LFACJF 仕訳作成対象外 ADJ-ID="
                         AJ-ADJ-ID
                         " 理由="
                         WS-ERR-MSG
              END-IF
           END-IF.

       3200-FIND-CV-REC.
           MOVE "N" TO WS-MATCH-FOUND
           MOVE 1 TO WS-CV-IDX
           PERFORM UNTIL WS-CV-IDX > WS-CV-CNT OR MATCH-FOUND
              IF T-CO-POL-NO(WS-CV-IDX) = AJ-POL-NO
                 SET MATCH-FOUND TO TRUE
              ELSE
                 ADD 1 TO WS-CV-IDX
              END-IF
           END-PERFORM.

       3300-VALIDATE-ADJ.
           MOVE "N" TO WS-VALID-JOURNAL
           MOVE SPACES TO WS-ERR-MSG
           MOVE 0 TO WS-AJ-AMT

           IF AJ-ADJ-ID = SPACES
              MOVE "調整ＩＤ未設定" TO WS-ERR-MSG
           ELSE
              IF AJ-POL-NO = SPACES
                 MOVE "証券番号未設定" TO WS-ERR-MSG
              ELSE
                 IF AJ-DR-ACCT-CD = SPACES
                    MOVE "借方科目未設定" TO WS-ERR-MSG
                 ELSE
                    IF AJ-CR-ACCT-CD = SPACES
                       MOVE "貸方科目未設定" TO WS-ERR-MSG
                    ELSE
                       COMPUTE WS-AJ-AMT = FUNCTION NUMVAL(AJ-AMT)
                       IF WS-AJ-AMT = ZERO
                          MOVE "金額ゼロ" TO WS-ERR-MSG
                       ELSE
                          IF NOT MATCH-FOUND
                             MOVE "計算結果なし" TO WS-ERR-MSG
                          ELSE
                             PERFORM 3310-CHECK-EVENT
                          END-IF
                       END-IF
                    END-IF
                 END-IF
              END-IF
           END-IF.

       3310-CHECK-EVENT.
           EVALUATE AJ-EVENT-KBN
              WHEN WS-EVENT-REFUND
                 IF WS-AJ-AMT = T-CO-CV-AMT(WS-CV-IDX)
                    SET VALID-JOURNAL TO TRUE
                 ELSE
                    MOVE "返戻金額不一致" TO WS-ERR-MSG
                 END-IF
              WHEN WS-EVENT-LOAN-OFFSET
                 IF WS-AJ-AMT > ZERO
                    IF WS-AJ-AMT <= T-CO-RESERVE-AMT(WS-CV-IDX)
                       SET VALID-JOURNAL TO TRUE
                    ELSE
                       MOVE "貸付相殺額過大" TO WS-ERR-MSG
                    END-IF
                 ELSE
                    MOVE "貸付相殺額不正" TO WS-ERR-MSG
                 END-IF
              WHEN WS-EVENT-ROUNDING
                 IF WS-AJ-AMT < ZERO
                    COMPUTE WS-ABS-AMT = WS-AJ-AMT * -1
                 ELSE
                    MOVE WS-AJ-AMT TO WS-ABS-AMT
                 END-IF
                 IF WS-ABS-AMT <= WS-ROUND-LIMIT
                    SET VALID-JOURNAL TO TRUE
                 ELSE
                    MOVE "端数調整額過大" TO WS-ERR-MSG
                 END-IF
              WHEN OTHER
                 MOVE "事由区分不正" TO WS-ERR-MSG
           END-EVALUATE.

       3400-WRITE-JOURNAL.
           INITIALIZE LEJRNF-REC
           ADD 1 TO WS-JOURNAL-SEQ
           MOVE WS-JOURNAL-SEQ TO JR-JOURNAL-ID
           MOVE WS-ACCEPT-DATE TO JR-POST-DATE
           MOVE AJ-POL-NO TO JR-POL-NO
           MOVE AJ-DR-ACCT-CD TO JR-DR-ACCT-CD
           MOVE AJ-CR-ACCT-CD TO JR-CR-ACCT-CD
           MOVE AJ-AMT TO JR-AMT
           MOVE WS-JR-READY-KBN TO JR-JOURNAL-STATUS-KBN

           WRITE LEJRNF-REC
           IF WS-LEJRNF-ST = "00"
              ADD 1 TO WS-LEJRNF-WRITE-CNT
           ELSE
              DISPLAY "LEJRNF 書込失敗 ST="
                      WS-LEJRNF-ST
                      " ADJ-ID="
                      AJ-ADJ-ID
              SET HARD-ERROR TO TRUE
           END-IF.

       9000-CLOSE-FILES.
           IF WS-LFACJF-ST NOT = SPACES
              CLOSE LFACJF
              IF WS-LFACJF-ST NOT = "00"
                 DISPLAY "LFACJF クローズ失敗 ST="
                         WS-LFACJF-ST
                 SET HARD-ERROR TO TRUE
              END-IF
           END-IF

           IF WS-LFCVRF-ST NOT = SPACES
              CLOSE LFCVRF
              IF WS-LFCVRF-ST NOT = "00"
                 DISPLAY "LFCVRF クローズ失敗 ST="
                         WS-LFCVRF-ST
                 SET HARD-ERROR TO TRUE
              END-IF
           END-IF

           IF WS-LEJRNF-ST NOT = SPACES
              CLOSE LEJRNF
              IF WS-LEJRNF-ST NOT = "00"
                 DISPLAY "LEJRNF クローズ失敗 ST="
                         WS-LEJRNF-ST
                 SET HARD-ERROR TO TRUE
              END-IF
           END-IF.
