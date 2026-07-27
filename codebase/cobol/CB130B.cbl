       IDENTIFICATION DIVISION.
       PROGRAM-ID. CB130B.
       AUTHOR.     TRUST-BATCH.
      ******************************************************************
      * 振替結果取込バッチ
      * 金融機関戻り結果を請求番号で突合し、正常入金を出力する。
      * 金額差異、未登録請求、結果コード不明は消込前例外に記録する。
      ******************************************************************

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CDTRRF ASSIGN TO "CDTRRF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS ST-CDTRRF.
           SELECT CDTRQF ASSIGN TO "CDTRQF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS ST-CDTRQF.
           SELECT CDPAYF ASSIGN TO "CDPAYF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS ST-CDPAYF.
           SELECT CDHISTF ASSIGN TO "CDHISTF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS HIS-CARD-NO
               FILE STATUS IS ST-CDHISTF.
           SELECT CDEXCPF2 ASSIGN TO "CDEXCPF2"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS ST-CDEXCPF2.

       DATA DIVISION.
       FILE SECTION.
       FD  CDTRRF.
           COPY CDTRRC.
       FD  CDTRQF.
           COPY CDTRQC.
       FD  CDPAYF.
           COPY CDPAYFC.
       FD  CDHISTF.
           COPY CDHISTC.
       FD  CDEXCPF2.
           COPY CDEXCPF2C.

       WORKING-STORAGE SECTION.
       01  FILE-STATUS-AREA.
           05 ST-CDTRRF             PIC XX VALUE SPACES.
           05 ST-CDTRQF             PIC XX VALUE SPACES.
           05 ST-CDPAYF             PIC XX VALUE SPACES.
           05 ST-CDHISTF            PIC XX VALUE SPACES.
           05 ST-CDEXCPF2           PIC XX VALUE SPACES.

       01  END-FLAGS.
           05 SW-TRR-END            PIC X VALUE "N".
              88 TRR-END                 VALUE "Y".
           05 SW-TRQ-END            PIC X VALUE "N".
              88 TRQ-END                 VALUE "Y".
           05 SW-HARD-ERROR         PIC X VALUE "N".
              88 HARD-ERROR              VALUE "Y".

       01  DATE-AREA.
           05 WS-CURRENT-DATE       PIC 9(8).
           05 WS-CURRENT-TIME       PIC 9(6).

       01  COUNTER-AREA.
           05 CNT-TRQ-READ          PIC 9(9) VALUE 0.
           05 CNT-TRR-READ          PIC 9(9) VALUE 0.
           05 CNT-PAY-WRITE         PIC 9(9) VALUE 0.
           05 CNT-HIS-WRITE         PIC 9(9) VALUE 0.
           05 CNT-EXP-WRITE         PIC 9(9) VALUE 0.
           05 CNT-REJECT            PIC 9(9) VALUE 0.
           05 SEQ-PAY               PIC 9(10) VALUE 0.
           05 SEQ-EXP               PIC 9(10) VALUE 0.
           05 SEQ-HIS               PIC 9(7) VALUE 0.

       01  REQUEST-TABLE-AREA.
           05 REQ-MAX               PIC 9(5) VALUE 10000.
           05 REQ-COUNT             PIC 9(5) VALUE 0.
           05 REQ-TABLE OCCURS 10000 TIMES
                         INDEXED BY REQ-IDX.
              10 T-REQUEST-ID       PIC X(20).
              10 T-CARD-NO          PIC X(20).
              10 T-CYCLE-DT         PIC 9(8).
              10 T-REQUEST-AMT      PIC S9(13)V99.
              10 T-DUE-DT           PIC 9(8).
              10 T-BANK-CD          PIC X(4).
              10 T-ACCOUNT-NO       PIC X(16).
              10 T-STATUS           PIC X.

       01  PROCESS-AREA.
           05 WS-FOUND              PIC X VALUE "N".
              88 REQUEST-FOUND           VALUE "Y".
           05 WS-EXCEPTION-CD       PIC X(4) VALUE SPACES.
           05 WS-PAY-ID             PIC X(20) VALUE SPACES.
           05 WS-EXP-ID             PIC X(20) VALUE SPACES.
           05 WS-HIS-KEY            PIC X(20) VALUE SPACES.
           05 WS-AMT-DIFF           PIC S9(13)V99 VALUE 0.
           05 WS-VALID-RESULT       PIC X VALUE "N".
              88 VALID-RESULT            VALUE "Y".
           05 WS-NORMAL-RESULT      PIC X VALUE "N".
              88 NORMAL-RESULT           VALUE "Y".

       01  DISPLAY-AREA.
           05 DSP-COUNT             PIC Z(8)9.

       PROCEDURE DIVISION.
       MAIN-SECTION.
           PERFORM 0000-INIT.
           IF NOT HARD-ERROR
              PERFORM 1000-LOAD-REQUEST
           END-IF.
           IF NOT HARD-ERROR
              PERFORM 2000-PROCESS-RESULT
           END-IF.
           PERFORM 9000-FINISH.
           GOBACK.

       0000-INIT.
           MOVE 0 TO RETURN-CODE.
           MOVE FUNCTION CURRENT-DATE(1:8) TO WS-CURRENT-DATE.
           MOVE FUNCTION CURRENT-DATE(9:6) TO WS-CURRENT-TIME.
           OPEN INPUT CDTRQF.
           IF ST-CDTRQF NOT = "00"
              DISPLAY "CDTRQF オープン失敗 ST=" ST-CDTRQF
              MOVE "Y" TO SW-HARD-ERROR
              MOVE 12 TO RETURN-CODE
           END-IF.
           IF NOT HARD-ERROR
              OPEN INPUT CDTRRF
              IF ST-CDTRRF NOT = "00"
                 DISPLAY "CDTRRF オープン失敗 ST=" ST-CDTRRF
                 MOVE "Y" TO SW-HARD-ERROR
                 MOVE 12 TO RETURN-CODE
              END-IF
           END-IF.
           IF NOT HARD-ERROR
              OPEN OUTPUT CDPAYF
              IF ST-CDPAYF NOT = "00"
                 DISPLAY "CDPAYF オープン失敗 ST=" ST-CDPAYF
                 MOVE "Y" TO SW-HARD-ERROR
                 MOVE 12 TO RETURN-CODE
              END-IF
           END-IF.
           IF NOT HARD-ERROR
              OPEN OUTPUT CDHISTF
              IF ST-CDHISTF NOT = "00"
                 DISPLAY "CDHISTF オープン失敗 ST=" ST-CDHISTF
                 MOVE "Y" TO SW-HARD-ERROR
                 MOVE 12 TO RETURN-CODE
              END-IF
           END-IF.
           IF NOT HARD-ERROR
              OPEN OUTPUT CDEXCPF2
              IF ST-CDEXCPF2 NOT = "00"
                 DISPLAY "CDEXCPF2 オープン失敗 ST=" ST-CDEXCPF2
                 MOVE "Y" TO SW-HARD-ERROR
                 MOVE 12 TO RETURN-CODE
              END-IF
           END-IF.

       1000-LOAD-REQUEST.
           PERFORM UNTIL TRQ-END OR HARD-ERROR
              READ CDTRQF
                 AT END
                    MOVE "Y" TO SW-TRQ-END
                 NOT AT END
                    ADD 1 TO CNT-TRQ-READ
                    IF REQ-COUNT >= REQ-MAX
                       DISPLAY "請求テーブル上限超過"
                       MOVE "Y" TO SW-HARD-ERROR
                       MOVE 12 TO RETURN-CODE
                    ELSE
                       ADD 1 TO REQ-COUNT
                       SET REQ-IDX TO REQ-COUNT
                       MOVE TRQ-REQUEST-ID TO T-REQUEST-ID(REQ-IDX)
                       MOVE TRQ-CARD-NO TO T-CARD-NO(REQ-IDX)
                       MOVE TRQ-BILLING-CYCLE-DT
                         TO T-CYCLE-DT(REQ-IDX)
                       MOVE TRQ-REQUEST-AMT TO T-REQUEST-AMT(REQ-IDX)
                       MOVE TRQ-DUE-DT TO T-DUE-DT(REQ-IDX)
                       MOVE TRQ-BANK-CD TO T-BANK-CD(REQ-IDX)
                       MOVE TRQ-ACCOUNT-NO TO T-ACCOUNT-NO(REQ-IDX)
                       MOVE TRQ-REQUEST-STATUS TO T-STATUS(REQ-IDX)
                    END-IF
              END-READ
              IF ST-CDTRQF NOT = "00" AND ST-CDTRQF NOT = "10"
                 DISPLAY "CDTRQF 読込失敗 ST=" ST-CDTRQF
                 MOVE "Y" TO SW-HARD-ERROR
                 MOVE 12 TO RETURN-CODE
              END-IF
           END-PERFORM.

       2000-PROCESS-RESULT.
           PERFORM UNTIL TRR-END OR HARD-ERROR
              READ CDTRRF
                 AT END
                    MOVE "Y" TO SW-TRR-END
                 NOT AT END
                    ADD 1 TO CNT-TRR-READ
                    PERFORM 2100-JUDGE-RESULT
                    IF NOT HARD-ERROR
                       IF NORMAL-RESULT
                          PERFORM 2200-WRITE-PAYMENT
                          IF NOT HARD-ERROR
                             PERFORM 2300-WRITE-HISTORY
                          END-IF
                       ELSE
                          PERFORM 2400-WRITE-EXCEPTION
                       END-IF
                    END-IF
              END-READ
              IF ST-CDTRRF NOT = "00" AND ST-CDTRRF NOT = "10"
                 DISPLAY "CDTRRF 読込失敗 ST=" ST-CDTRRF
                 MOVE "Y" TO SW-HARD-ERROR
                 MOVE 12 TO RETURN-CODE
              END-IF
           END-PERFORM.

       2100-JUDGE-RESULT.
           MOVE "N" TO WS-FOUND.
           MOVE "N" TO WS-VALID-RESULT.
           MOVE "N" TO WS-NORMAL-RESULT.
           MOVE SPACES TO WS-EXCEPTION-CD.
           MOVE 0 TO WS-AMT-DIFF.
           SET REQ-IDX TO 1.
           SEARCH REQ-TABLE
              AT END
                 MOVE "E101" TO WS-EXCEPTION-CD
              WHEN T-REQUEST-ID(REQ-IDX) = TRR-REQUEST-ID
                 MOVE "Y" TO WS-FOUND
           END-SEARCH.
           EVALUATE TRR-RESULT-CD
              WHEN "00"
                 MOVE "Y" TO WS-VALID-RESULT
              WHEN "01"
              WHEN "02"
              WHEN "03"
              WHEN "04"
              WHEN "05"
              WHEN "99"
                 MOVE "Y" TO WS-VALID-RESULT
              WHEN OTHER
                 MOVE "E103" TO WS-EXCEPTION-CD
           END-EVALUATE.
           IF REQUEST-FOUND AND VALID-RESULT
              IF TRR-RESULT-CD = "00"
                 COMPUTE WS-AMT-DIFF =
                    TRR-SETTLED-AMT - T-REQUEST-AMT(REQ-IDX)
                 IF WS-AMT-DIFF = 0
                    MOVE "Y" TO WS-NORMAL-RESULT
                 ELSE
                    MOVE "E102" TO WS-EXCEPTION-CD
                 END-IF
              ELSE
                 MOVE "E104" TO WS-EXCEPTION-CD
              END-IF
           END-IF.
           IF NOT NORMAL-RESULT
              ADD 1 TO CNT-REJECT
           END-IF.

       2200-WRITE-PAYMENT.
           ADD 1 TO SEQ-PAY.
           INITIALIZE CDPAYF-REC.
           STRING "PY" WS-CURRENT-DATE SEQ-PAY
              DELIMITED BY SIZE INTO WS-PAY-ID
           END-STRING.
           MOVE WS-PAY-ID TO PY-PAY-ID.
           MOVE TRR-CARD-NO TO PY-CARD-NO.
           MOVE TRR-SETTLED-AMT TO PY-PAY-AMT.
           MOVE TRR-RESULT-DT TO PY-PAY-DT.
           MOVE "10" TO PY-PAY-METHOD.
           WRITE CDPAYF-REC.
           IF ST-CDPAYF = "00"
              ADD 1 TO CNT-PAY-WRITE
           ELSE
              DISPLAY "CDPAYF 書込失敗 ST=" ST-CDPAYF
              MOVE "Y" TO SW-HARD-ERROR
              MOVE 12 TO RETURN-CODE
           END-IF.

       2300-WRITE-HISTORY.
           ADD 1 TO SEQ-HIS.
           INITIALIZE CDHISTF-REC.
           STRING TRR-CARD-NO SEQ-HIS
              DELIMITED BY SIZE INTO WS-HIS-KEY
           END-STRING.
           MOVE WS-HIS-KEY TO HIS-CARD-NO.
           MOVE WS-PAY-ID TO HIS-PAY-ID.
           MOVE SEQ-HIS TO HIS-EVENT-SEQ.
           MOVE "PAY" TO HIS-EVENT-TYPE.
           MOVE TRR-SETTLED-AMT TO HIS-EVENT-AMT.
           MOVE TRR-RESULT-DT TO HIS-EVENT-DT.
           MOVE "CB130B" TO HIS-SOURCE-PROGRAM.
           WRITE CDHISTF-REC.
           IF ST-CDHISTF = "00"
              ADD 1 TO CNT-HIS-WRITE
           ELSE
              DISPLAY "CDHISTF 書込失敗 ST=" ST-CDHISTF
              MOVE "Y" TO SW-HARD-ERROR
              MOVE 12 TO RETURN-CODE
           END-IF.

       2400-WRITE-EXCEPTION.
           ADD 1 TO SEQ-EXP.
           INITIALIZE CDEXCPF2-REC.
           STRING "EX" WS-CURRENT-DATE SEQ-EXP
              DELIMITED BY SIZE INTO WS-EXP-ID
           END-STRING.
           MOVE WS-EXP-ID TO EXP-EXCEPTION-ID.
           MOVE SPACES TO EXP-PAY-ID.
           IF REQUEST-FOUND
              MOVE T-CARD-NO(REQ-IDX) TO EXP-CARD-NO
           ELSE
              MOVE TRR-CARD-NO TO EXP-CARD-NO
           END-IF.
           MOVE WS-EXCEPTION-CD TO EXP-EXCEPTION-CD.
           IF WS-EXCEPTION-CD = "E102"
              MOVE WS-AMT-DIFF TO EXP-EXCEPTION-AMT
           ELSE
              MOVE TRR-SETTLED-AMT TO EXP-EXCEPTION-AMT
           END-IF.
           MOVE "CB130B" TO EXP-DETECTED-PROGRAM.
           MOVE WS-CURRENT-DATE TO EXP-DETECTED-DT.
           WRITE CDEXCPF2-REC.
           IF ST-CDEXCPF2 = "00"
              ADD 1 TO CNT-EXP-WRITE
           ELSE
              DISPLAY "CDEXCPF2 書込失敗 ST=" ST-CDEXCPF2
              MOVE "Y" TO SW-HARD-ERROR
              MOVE 12 TO RETURN-CODE
           END-IF.

       9000-FINISH.
           IF ST-CDTRQF NOT = SPACES
              CLOSE CDTRQF
           END-IF.
           IF ST-CDTRRF NOT = SPACES
              CLOSE CDTRRF
           END-IF.
           IF ST-CDPAYF NOT = SPACES
              CLOSE CDPAYF
           END-IF.
           IF ST-CDHISTF NOT = SPACES
              CLOSE CDHISTF
           END-IF.
           IF ST-CDEXCPF2 NOT = SPACES
              CLOSE CDEXCPF2
           END-IF.
           IF HARD-ERROR
              IF RETURN-CODE = 0
                 MOVE 12 TO RETURN-CODE
              END-IF
              DISPLAY "CB130B 異常終了"
           ELSE
              MOVE 0 TO RETURN-CODE
              DISPLAY "CB130B 正常終了"
           END-IF.
           MOVE CNT-TRQ-READ TO DSP-COUNT.
           DISPLAY "請求読込件数=" DSP-COUNT.
           MOVE CNT-TRR-READ TO DSP-COUNT.
           DISPLAY "結果読込件数=" DSP-COUNT.
           MOVE CNT-PAY-WRITE TO DSP-COUNT.
           DISPLAY "入金出力件数=" DSP-COUNT.
           MOVE CNT-HIS-WRITE TO DSP-COUNT.
           DISPLAY "履歴出力件数=" DSP-COUNT.
           MOVE CNT-EXP-WRITE TO DSP-COUNT.
           DISPLAY "例外出力件数=" DSP-COUNT.
           MOVE CNT-REJECT TO DSP-COUNT.
           DISPLAY "消込前例外件数=" DSP-COUNT.
