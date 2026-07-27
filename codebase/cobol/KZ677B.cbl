       IDENTIFICATION DIVISION.
       PROGRAM-ID. KZ677B.
      *----------------------------------------------------------------
      * 変更履歴
      * 版数  年月日(和暦)  担当                        概要
      * 1.00  H30.04.01  システム部 勘定系チーム  新規作成
      * 1.01  R02.10.15  システム部 勘定系チーム  税額訂正戻入条件見直し
      * 1.02  R05.06.20  システム部 勘定系チーム  戻入対象抽出処理改修
      *----------------------------------------------------------------
       AUTHOR. KZ-SYSTEM.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZCRIF
               ASSIGN TO "KZCRIF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-ST-KZCRIF.
           SELECT KZTXRF
               ASSIGN TO "KZTXRF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-ST-KZTXRF.
           SELECT KZGLPF
               ASSIGN TO "KZGLPF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-ST-KZGLPF.
           SELECT KZADLF
               ASSIGN TO "KZADLF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-ST-KZADLF.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  KZCRIF.
           COPY KZCRIFC.
      *
       FD  KZTXRF.
           COPY KZTXRFC.
      *
       FD  KZGLPF.
           COPY KZGLPFC2.
      *
       FD  KZADLF.
           COPY KZADLFC.
      *
       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05 WS-ST-KZCRIF          PIC X(02) VALUE SPACES.
           05 WS-ST-KZTXRF          PIC X(02) VALUE SPACES.
           05 WS-ST-KZGLPF          PIC X(02) VALUE SPACES.
           05 WS-ST-KZADLF          PIC X(02) VALUE SPACES.
      *
       01  WS-RUN-CONTROL.
           05 WS-RUN-DATE           PIC 9(08) VALUE ZERO.
           05 WS-EOF-KZCRIF         PIC X VALUE "N".
              88 EOF-KZCRIF               VALUE "Y".
           05 WS-EOF-KZTXRF         PIC X VALUE "N".
              88 EOF-KZTXRF               VALUE "Y".
           05 WS-HARD-ERROR         PIC X VALUE "N".
              88 HARD-ERROR               VALUE "Y".
           05 WS-AUDIT-SEQ          PIC 9(07) VALUE ZERO.
           05 WS-JOURNAL-SEQ        PIC 9(07) VALUE ZERO.
      *
       01  WS-COUNTERS.
           05 WS-TX-LOAD-CNT        PIC 9(09) VALUE ZERO.
           05 WS-CR-READ-CNT        PIC 9(09) VALUE ZERO.
           05 WS-REV-WRITE-CNT      PIC 9(09) VALUE ZERO.
           05 WS-GL-WRITE-CNT       PIC 9(09) VALUE ZERO.
           05 WS-AUDIT-WRITE-CNT    PIC 9(09) VALUE ZERO.
           05 WS-SKIP-CNT           PIC 9(09) VALUE ZERO.
      *
       01  WS-AMOUNTS.
           05 WS-CALC-TOTAL-TAX     PIC S9(13) VALUE ZERO.
           05 WS-CALC-NET-AMT       PIC S9(13) VALUE ZERO.
           05 WS-ABS-DEBIT          PIC S9(13) VALUE ZERO.
           05 WS-ABS-CREDIT         PIC S9(13) VALUE ZERO.
           05 WS-DIFF-AMT           PIC S9(13) VALUE ZERO.
      *
       01  WS-WORK.
           05 WS-IDX                PIC 9(05) VALUE ZERO.
           05 WS-MATCH-IDX          PIC 9(05) VALUE ZERO.
           05 WS-FOUND-SW           PIC X VALUE "N".
              88 TX-FOUND                 VALUE "Y".
           05 WS-DUP-SW             PIC X VALUE "N".
              88 DUP-FOUND                VALUE "Y".
           05 WS-VALID-SW           PIC X VALUE "N".
              88 VALID-CORRECTION         VALUE "Y".
           05 WS-EDIT-REASON        PIC X(30) VALUE SPACES.
           05 WS-AUDIT-ID-WK        PIC X(20) VALUE SPACES.
           05 WS-JOURNAL-NO-WK      PIC X(12) VALUE SPACES.
      *
       01  WS-TX-TABLE.
           05 WS-TX-ENTRY OCCURS 20000 TIMES.
              10 TB-ACCT-NO         PIC X(20).
              10 TB-ACCT-TYPE       PIC X(02).
              10 TB-GROSS-AMT       PIC S9(13).
              10 TB-NATIONAL-TAX    PIC S9(13).
              10 TB-LOCAL-TAX       PIC S9(13).
              10 TB-USED-SW         PIC X.
      *
       PROCEDURE DIVISION.
       MAIN-RTN.
           MOVE 0 TO RETURN-CODE
           ACCEPT WS-RUN-DATE FROM DATE YYYYMMDD
           PERFORM 1000-OPEN-INPUTS
           IF HARD-ERROR
              GO TO 9000-END
           END-IF
      *
           PERFORM 2000-LOAD-TAX-RESULT
           CLOSE KZTXRF
           IF HARD-ERROR
              GO TO 9000-END
           END-IF
      *
           PERFORM 3000-OPEN-OUTPUTS
           IF HARD-ERROR
              GO TO 9000-END
           END-IF
      *
           PERFORM 4000-PROCESS-CORRECTIONS
              UNTIL EOF-KZCRIF OR HARD-ERROR
           .
      *
       9000-END.
           PERFORM 8000-CLOSE-FILES
           IF HARD-ERROR
              MOVE 12 TO RETURN-CODE
           ELSE
              MOVE 0 TO RETURN-CODE
              DISPLAY "KZ677B 正常終了"
              DISPLAY "源泉結果読込件数=" WS-TX-LOAD-CNT
              DISPLAY "訂正依頼読込件数=" WS-CR-READ-CNT
              DISPLAY "訂正出力件数=" WS-REV-WRITE-CNT
              DISPLAY "仕訳出力件数=" WS-GL-WRITE-CNT
              DISPLAY "監査差額件数=" WS-AUDIT-WRITE-CNT
              DISPLAY "除外件数=" WS-SKIP-CNT
           END-IF
           GOBACK.
      *
       1000-OPEN-INPUTS.
           OPEN INPUT KZCRIF
           IF WS-ST-KZCRIF NOT = "00"
              DISPLAY "KZCRIF オープン失敗 ST=" WS-ST-KZCRIF
              SET HARD-ERROR TO TRUE
           END-IF
      *
           OPEN INPUT KZTXRF
           IF WS-ST-KZTXRF NOT = "00"
              DISPLAY "KZTXRF 入力オープン失敗 ST=" WS-ST-KZTXRF
              SET HARD-ERROR TO TRUE
           END-IF.
      *
       2000-LOAD-TAX-RESULT.
           PERFORM UNTIL EOF-KZTXRF OR HARD-ERROR
              READ KZTXRF
                 AT END
                    SET EOF-KZTXRF TO TRUE
                 NOT AT END
                    IF WS-TX-LOAD-CNT >= 20000
                       DISPLAY "源泉結果表上限超過"
                       SET HARD-ERROR TO TRUE
                    ELSE
                       ADD 1 TO WS-TX-LOAD-CNT
                       MOVE WS-TX-LOAD-CNT TO WS-IDX
                       MOVE TR-ACCT-NO TO TB-ACCT-NO(WS-IDX)
                       MOVE TR-ACCT-TYPE TO TB-ACCT-TYPE(WS-IDX)
                       MOVE TR-GROSS-INT-AMT TO TB-GROSS-AMT(WS-IDX)
                       MOVE TR-NATIONAL-TAX-AMT
                         TO TB-NATIONAL-TAX(WS-IDX)
                       MOVE TR-LOCAL-TAX-AMT TO TB-LOCAL-TAX(WS-IDX)
                       MOVE "N" TO TB-USED-SW(WS-IDX)
                    END-IF
              END-READ
              IF WS-ST-KZTXRF NOT = "00" AND WS-ST-KZTXRF NOT = "10"
                 DISPLAY "KZTXRF 読込失敗 ST=" WS-ST-KZTXRF
                 SET HARD-ERROR TO TRUE
              END-IF
           END-PERFORM.
      *
       3000-OPEN-OUTPUTS.
           OPEN EXTEND KZTXRF
           IF WS-ST-KZTXRF NOT = "00"
              DISPLAY "KZTXRF 追記オープン失敗 ST=" WS-ST-KZTXRF
              SET HARD-ERROR TO TRUE
           END-IF
      *
           OPEN OUTPUT KZGLPF
           IF WS-ST-KZGLPF NOT = "00"
              DISPLAY "KZGLPF オープン失敗 ST=" WS-ST-KZGLPF
              SET HARD-ERROR TO TRUE
           END-IF
      *
           OPEN OUTPUT KZADLF
           IF WS-ST-KZADLF NOT = "00"
              DISPLAY "KZADLF オープン失敗 ST=" WS-ST-KZADLF
              SET HARD-ERROR TO TRUE
           END-IF.
      *
       4000-PROCESS-CORRECTIONS.
           READ KZCRIF
              AT END
                 SET EOF-KZCRIF TO TRUE
              NOT AT END
                 ADD 1 TO WS-CR-READ-CNT
                 PERFORM 4100-VALIDATE-CORRECTION
                 IF VALID-CORRECTION
                    PERFORM 4200-FIND-TAX-RESULT
                    IF TX-FOUND
                       PERFORM 4300-CHECK-DUPLICATE
                       IF DUP-FOUND
                          MOVE "二重訂正" TO WS-EDIT-REASON
                          PERFORM 4700-WRITE-AUDIT
                       ELSE
                          PERFORM 4400-CHECK-AMOUNT
                          IF VALID-CORRECTION
                             PERFORM 4500-WRITE-REVERSAL
                             PERFORM 4600-WRITE-GL
                             MOVE "Y" TO TB-USED-SW(WS-MATCH-IDX)
                          ELSE
                             PERFORM 4700-WRITE-AUDIT
                          END-IF
                       END-IF
                    ELSE
                       MOVE "元明細不在" TO WS-EDIT-REASON
                       PERFORM 4700-WRITE-AUDIT
                    END-IF
                 ELSE
                    PERFORM 4700-WRITE-AUDIT
                 END-IF
           END-READ
           IF WS-ST-KZCRIF NOT = "00" AND WS-ST-KZCRIF NOT = "10"
              DISPLAY "KZCRIF 読込失敗 ST=" WS-ST-KZCRIF
              SET HARD-ERROR TO TRUE
           END-IF.
      *
       4100-VALIDATE-CORRECTION.
           MOVE "Y" TO WS-VALID-SW
           MOVE SPACES TO WS-EDIT-REASON
           IF CR-CORRECTION-NO = SPACES
              MOVE "N" TO WS-VALID-SW
              MOVE "訂正番号未設定" TO WS-EDIT-REASON
           END-IF
           IF CR-ORIG-ACCT-NO = SPACES
              MOVE "N" TO WS-VALID-SW
              MOVE "原口座番号未設定" TO WS-EDIT-REASON
           END-IF
           IF CR-ORIG-PAYMENT-DATE = ZERO
              MOVE "N" TO WS-VALID-SW
              MOVE "原支払日未設定" TO WS-EDIT-REASON
           END-IF
           IF CR-CORRECTION-REASON-CD = SPACES
              MOVE "N" TO WS-VALID-SW
              MOVE "訂正理由未設定" TO WS-EDIT-REASON
           END-IF
           IF CR-REV-GROSS-AMT < ZERO
              MOVE "N" TO WS-VALID-SW
              MOVE "戻入税込利息不正" TO WS-EDIT-REASON
           END-IF
           IF CR-REV-NATIONAL-TAX-AMT < ZERO
              MOVE "N" TO WS-VALID-SW
              MOVE "戻入国税不正" TO WS-EDIT-REASON
           END-IF
           IF CR-REV-LOCAL-TAX-AMT < ZERO
              MOVE "N" TO WS-VALID-SW
              MOVE "戻入地方税不正" TO WS-EDIT-REASON
           END-IF.
      *
       4200-FIND-TAX-RESULT.
           MOVE "N" TO WS-FOUND-SW
           MOVE ZERO TO WS-MATCH-IDX
           PERFORM VARYING WS-IDX FROM 1 BY 1
              UNTIL WS-IDX > WS-TX-LOAD-CNT OR TX-FOUND
              IF TB-ACCT-NO(WS-IDX) = CR-ORIG-ACCT-NO
                 MOVE "Y" TO WS-FOUND-SW
                 MOVE WS-IDX TO WS-MATCH-IDX
              END-IF
           END-PERFORM.
      *
       4300-CHECK-DUPLICATE.
           MOVE "N" TO WS-DUP-SW
           IF TB-USED-SW(WS-MATCH-IDX) = "Y"
              MOVE "Y" TO WS-DUP-SW
           END-IF.
      *
       4400-CHECK-AMOUNT.
           MOVE "Y" TO WS-VALID-SW
           IF CR-REV-GROSS-AMT NOT = TB-GROSS-AMT(WS-MATCH-IDX)
              MOVE "N" TO WS-VALID-SW
              MOVE "税込利息照合差異" TO WS-EDIT-REASON
           END-IF
           IF CR-REV-NATIONAL-TAX-AMT
              NOT = TB-NATIONAL-TAX(WS-MATCH-IDX)
              MOVE "N" TO WS-VALID-SW
              MOVE "国税照合差異" TO WS-EDIT-REASON
           END-IF
           IF CR-REV-LOCAL-TAX-AMT NOT = TB-LOCAL-TAX(WS-MATCH-IDX)
              MOVE "N" TO WS-VALID-SW
              MOVE "地方税照合差異" TO WS-EDIT-REASON
           END-IF.
      *
       4500-WRITE-REVERSAL.
           INITIALIZE KZTXRF-REC
           MOVE CR-ORIG-ACCT-NO TO TR-ACCT-NO
           MOVE TB-ACCT-TYPE(WS-MATCH-IDX) TO TR-ACCT-TYPE
           COMPUTE TR-GROSS-INT-AMT = CR-REV-GROSS-AMT * -1
           COMPUTE TR-NATIONAL-TAX-AMT =
                   CR-REV-NATIONAL-TAX-AMT * -1
           COMPUTE TR-LOCAL-TAX-AMT = CR-REV-LOCAL-TAX-AMT * -1
           COMPUTE TR-TOTAL-TAX-AMT =
                   TR-NATIONAL-TAX-AMT + TR-LOCAL-TAX-AMT
           COMPUTE TR-NET-AMT =
                   TR-GROSS-INT-AMT - TR-TOTAL-TAX-AMT
           WRITE KZTXRF-REC
           IF WS-ST-KZTXRF NOT = "00"
              DISPLAY "KZTXRF 訂正書込失敗 ST=" WS-ST-KZTXRF
              SET HARD-ERROR TO TRUE
           ELSE
              ADD 1 TO WS-REV-WRITE-CNT
           END-IF.
      *
       4600-WRITE-GL.
           INITIALIZE KZGLPF-REC
           ADD 1 TO WS-JOURNAL-SEQ
           MOVE WS-RUN-DATE TO GP-POSTING-DATE
           MOVE CR-ORIG-ACCT-NO TO GP-ACCT-NO
           MOVE TB-ACCT-TYPE(WS-MATCH-IDX) TO GP-ACCT-TYPE
           COMPUTE GP-WITHHOLDING-LIAB-AMT =
                   CR-REV-NATIONAL-TAX-AMT * -1
           COMPUTE GP-LOCAL-LIAB-AMT =
                   CR-REV-LOCAL-TAX-AMT * -1
           COMPUTE WS-CALC-TOTAL-TAX =
                   CR-REV-NATIONAL-TAX-AMT
                 + CR-REV-LOCAL-TAX-AMT
           COMPUTE WS-CALC-NET-AMT =
                   CR-REV-GROSS-AMT - WS-CALC-TOTAL-TAX
           COMPUTE GP-NET-PAYABLE-AMT = WS-CALC-NET-AMT * -1
           MOVE "0001" TO GP-GL-BRANCH-CD
           MOVE SPACES TO WS-JOURNAL-NO-WK
           MOVE "KZ677" TO WS-JOURNAL-NO-WK(1:5)
           MOVE WS-JOURNAL-SEQ TO WS-JOURNAL-NO-WK(6:7)
           MOVE WS-JOURNAL-NO-WK TO GP-JOURNAL-NO
           WRITE KZGLPF-REC
           IF WS-ST-KZGLPF NOT = "00"
              DISPLAY "KZGLPF 書込失敗 ST=" WS-ST-KZGLPF
              SET HARD-ERROR TO TRUE
           ELSE
              ADD 1 TO WS-GL-WRITE-CNT
           END-IF.
      *
       4700-WRITE-AUDIT.
           INITIALIZE KZADLF-REC
           ADD 1 TO WS-AUDIT-SEQ
           MOVE SPACES TO WS-AUDIT-ID-WK
           MOVE "KZ677B" TO WS-AUDIT-ID-WK(1:6)
           MOVE WS-AUDIT-SEQ TO WS-AUDIT-ID-WK(14:7)
           MOVE WS-AUDIT-ID-WK TO AL-AUDIT-ID
           MOVE WS-RUN-DATE TO AL-RUN-DATE
           MOVE "KZCRIF" TO AL-SOURCE-DSID
           COMPUTE WS-ABS-DEBIT =
                   CR-REV-GROSS-AMT
                 + CR-REV-NATIONAL-TAX-AMT
                 + CR-REV-LOCAL-TAX-AMT
           MOVE WS-ABS-DEBIT TO AL-DEBIT-AMT
           IF TX-FOUND
              COMPUTE WS-ABS-CREDIT =
                      TB-GROSS-AMT(WS-MATCH-IDX)
                    + TB-NATIONAL-TAX(WS-MATCH-IDX)
                    + TB-LOCAL-TAX(WS-MATCH-IDX)
           ELSE
              MOVE ZERO TO WS-ABS-CREDIT
           END-IF
           MOVE WS-ABS-CREDIT TO AL-CREDIT-AMT
           COMPUTE WS-DIFF-AMT = WS-ABS-DEBIT - WS-ABS-CREDIT
           MOVE WS-DIFF-AMT TO AL-DIFF-AMT
           EVALUATE WS-EDIT-REASON
              WHEN "元明細不在"
                 MOVE "01" TO AL-STATUS-CD
              WHEN "二重訂正"
                 MOVE "02" TO AL-STATUS-CD
              WHEN "税込利息照合差異"
                 MOVE "03" TO AL-STATUS-CD
              WHEN "国税照合差異"
                 MOVE "04" TO AL-STATUS-CD
              WHEN "地方税照合差異"
                 MOVE "05" TO AL-STATUS-CD
              WHEN OTHER
                 MOVE "09" TO AL-STATUS-CD
           END-EVALUATE
           WRITE KZADLF-REC
           IF WS-ST-KZADLF NOT = "00"
              DISPLAY "KZADLF 書込失敗 ST=" WS-ST-KZADLF
              SET HARD-ERROR TO TRUE
           ELSE
              ADD 1 TO WS-AUDIT-WRITE-CNT
              ADD 1 TO WS-SKIP-CNT
              DISPLAY "監査差額分類"
              DISPLAY "訂正番号=" CR-CORRECTION-NO
              DISPLAY "理由=" WS-EDIT-REASON
           END-IF.
      *
       8000-CLOSE-FILES.
           CLOSE KZCRIF
           CLOSE KZTXRF
           CLOSE KZGLPF
           CLOSE KZADLF.
