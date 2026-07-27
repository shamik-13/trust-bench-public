       IDENTIFICATION DIVISION.
       PROGRAM-ID. JH525S.
      * 変更履歴
      * 版数  年月日(和暦)  担当                         概要
      * 1.00  H26.04.01    システム部 情報系チーム      新規作成
      * 1.01  R02.10.15    システム部 情報系チーム      出力可否条件見直し
      * 1.02  R06.03.01    システム部 情報系チーム      マート出力判定追加

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT JHRECMF
               ASSIGN TO "JHRECMF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS REC-RECON-KEY
               FILE STATUS IS WS-JHRECMF-ST.

       DATA DIVISION.
       FILE SECTION.
       FD  JHRECMF.
           COPY JHRECC.

       WORKING-STORAGE SECTION.
       01  WS-JHRECMF-ST              PIC XX VALUE SPACES.
       01  WS-END-SW                  PIC X VALUE '0'.
           88  WS-END                       VALUE '1'.

       01  WS-COUNTERS.
           05  WS-READ-CNT            PIC 9(9) VALUE 0.
           05  WS-OK-CNT              PIC 9(9) VALUE 0.
           05  WS-WARN-CNT            PIC 9(9) VALUE 0.
           05  WS-STOP-CNT            PIC 9(9) VALUE 0.
           05  WS-CONT-DIFF-CNT       PIC 9(4) VALUE 0.

       01  WS-CALC.
           05  WS-CALC-DIFF-CNT       PIC S9(13) VALUE 0.
           05  WS-CALC-DIFF-AMT       PIC S9(15)V99 VALUE 0.
           05  WS-ABS-DIFF-AMT        PIC 9(15)V99 VALUE 0.
           05  WS-REC-LEVEL           PIC 9 VALUE 0.
           05  WS-MAX-LEVEL           PIC 9 VALUE 0.
           05  WS-DATE-N              PIC 9(8) VALUE 0.
           05  WS-YYYY                PIC 9(4) VALUE 0.
           05  WS-MM                  PIC 9(2) VALUE 0.
           05  WS-DD                  PIC 9(2) VALUE 0.

       01  WS-CONST.
           05  WS-AMT-WARN-LIMIT      PIC 9(15)V99
                                       VALUE 1000000.00.
           05  WS-AMT-STOP-LIMIT      PIC 9(15)V99
                                       VALUE 100000000.00.
           05  WS-CNT-WARN-LIMIT      PIC 9(9) VALUE 10.
           05  WS-CNT-STOP-LIMIT      PIC 9(9) VALUE 1000.
           05  WS-CONT-WARN-LIMIT     PIC 9(4) VALUE 3.

       01  WS-MESSAGE.
           05  WS-REASON              PIC X(60) VALUE SPACES.

       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-INIT
           PERFORM 2000-PROCESS UNTIL WS-END
           PERFORM 9000-FINAL
           GOBACK.

       1000-INIT.
           MOVE 0 TO RETURN-CODE
           OPEN INPUT JHRECMF
           IF WS-JHRECMF-ST NOT = '00'
              DISPLAY 'JHRECMF OPEN ERROR ST=' WS-JHRECMF-ST
              MOVE 12 TO RETURN-CODE
              GOBACK
           END-IF
           PERFORM 2100-READ.

       2000-PROCESS.
           ADD 1 TO WS-READ-CNT
           PERFORM 3000-VALIDATE-RECORD
           PERFORM 4000-JUDGE-RECORD
           PERFORM 5000-ACCUMULATE
           PERFORM 2100-READ.

       2100-READ.
           READ JHRECMF
              AT END
                 SET WS-END TO TRUE
              NOT AT END
                 IF WS-JHRECMF-ST NOT = '00'
                    DISPLAY 'JHRECMF READ ERROR ST=' WS-JHRECMF-ST
                    MOVE 12 TO RETURN-CODE
                    CLOSE JHRECMF
                    GOBACK
                 END-IF
           END-READ.

       3000-VALIDATE-RECORD.
           MOVE SPACES TO WS-REASON
           MOVE 0 TO WS-REC-LEVEL

           IF REC-RECON-KEY = SPACES
              MOVE 'RECON KEY IS BLANK' TO WS-REASON
              MOVE 2 TO WS-REC-LEVEL
           END-IF

           IF WS-REC-LEVEL < 2
              IF REC-BUSINESS-DT NOT NUMERIC
                 MOVE 'BUSINESS DATE IS NOT NUMERIC' TO WS-REASON
                 MOVE 2 TO WS-REC-LEVEL
              ELSE
                 MOVE REC-BUSINESS-DT TO WS-DATE-N
                 DIVIDE WS-DATE-N BY 10000
                    GIVING WS-YYYY
                 DIVIDE WS-DATE-N BY 100
                    GIVING WS-MM
                 COMPUTE WS-MM = FUNCTION MOD(WS-MM, 100)
                 COMPUTE WS-DD = FUNCTION MOD(WS-DATE-N, 100)

                 IF WS-YYYY < 2000 OR WS-YYYY > 2099
                    MOVE 'BUSINESS DATE YEAR ERROR' TO WS-REASON
                    MOVE 2 TO WS-REC-LEVEL
                 END-IF

                 IF WS-MM < 1 OR WS-MM > 12
                    MOVE 'BUSINESS DATE MONTH ERROR' TO WS-REASON
                    MOVE 2 TO WS-REC-LEVEL
                 END-IF

                 IF WS-DD < 1 OR WS-DD > 31
                    MOVE 'BUSINESS DATE DAY ERROR' TO WS-REASON
                    MOVE 2 TO WS-REC-LEVEL
                 END-IF
              END-IF
           END-IF

           IF WS-REC-LEVEL < 2
              IF REC-SOURCE-CNT < 0 OR REC-TARGET-CNT < 0
                 MOVE 'RECON COUNT IS NEGATIVE' TO WS-REASON
                 MOVE 2 TO WS-REC-LEVEL
              END-IF
           END-IF

           IF WS-REC-LEVEL < 2
              IF REC-JUDGE-CD NOT = '0'
                 AND REC-JUDGE-CD NOT = '1'
                 AND REC-JUDGE-CD NOT = '2'
                 MOVE 'JUDGE CODE IS INVALID' TO WS-REASON
                 MOVE 2 TO WS-REC-LEVEL
              END-IF
           END-IF.

       4000-JUDGE-RECORD.
           COMPUTE WS-CALC-DIFF-CNT =
                   REC-SOURCE-CNT - REC-TARGET-CNT
           COMPUTE WS-CALC-DIFF-AMT =
                   REC-SOURCE-AMT - REC-TARGET-AMT
           COMPUTE WS-ABS-DIFF-AMT =
                   FUNCTION ABS(WS-CALC-DIFF-AMT)

           IF WS-CALC-DIFF-CNT NOT = REC-DIFF-CNT
              MOVE 'DIFF COUNT MISMATCH' TO WS-REASON
              MOVE 2 TO WS-REC-LEVEL
           END-IF

           IF WS-CALC-DIFF-AMT NOT = REC-DIFF-AMT
              MOVE 'DIFF AMOUNT MISMATCH' TO WS-REASON
              MOVE 2 TO WS-REC-LEVEL
           END-IF

           IF REC-DIFF-CNT NOT = 0
              ADD 1 TO WS-CONT-DIFF-CNT
           ELSE
              MOVE 0 TO WS-CONT-DIFF-CNT
           END-IF

           IF WS-REC-LEVEL < 2
              EVALUATE REC-JUDGE-CD
                 WHEN '2'
                    MOVE 2 TO WS-REC-LEVEL
                    MOVE 'JUDGE CODE STOP' TO WS-REASON
                 WHEN '1'
                    IF WS-REC-LEVEL < 1
                       MOVE 1 TO WS-REC-LEVEL
                       MOVE 'JUDGE CODE WARNING' TO WS-REASON
                    END-IF
                 WHEN OTHER
                    CONTINUE
              END-EVALUATE
           END-IF

           IF WS-REC-LEVEL < 2
              IF FUNCTION ABS(REC-DIFF-CNT) >= WS-CNT-STOP-LIMIT
                 MOVE 2 TO WS-REC-LEVEL
                 MOVE 'DIFF COUNT STOP LIMIT' TO WS-REASON
              END-IF
           END-IF

           IF WS-REC-LEVEL < 2
              IF WS-ABS-DIFF-AMT >= WS-AMT-STOP-LIMIT
                 MOVE 2 TO WS-REC-LEVEL
                 MOVE 'DIFF AMOUNT STOP LIMIT' TO WS-REASON
              END-IF
           END-IF

           IF WS-REC-LEVEL < 1
              IF FUNCTION ABS(REC-DIFF-CNT) >= WS-CNT-WARN-LIMIT
                 MOVE 1 TO WS-REC-LEVEL
                 MOVE 'DIFF COUNT WARN LIMIT' TO WS-REASON
              END-IF
           END-IF

           IF WS-REC-LEVEL < 1
              IF WS-ABS-DIFF-AMT >= WS-AMT-WARN-LIMIT
                 MOVE 1 TO WS-REC-LEVEL
                 MOVE 'DIFF AMOUNT WARN LIMIT' TO WS-REASON
              END-IF
           END-IF

           IF WS-REC-LEVEL < 1
              IF REC-DIFF-CNT NOT = 0
                 AND WS-ABS-DIFF-AMT < WS-AMT-WARN-LIMIT
                 AND WS-CONT-DIFF-CNT >= WS-CONT-WARN-LIMIT
                 MOVE 1 TO WS-REC-LEVEL
                 MOVE 'CONTINUOUS SMALL DIFF' TO WS-REASON
              END-IF
           END-IF

           IF WS-REC-LEVEL = 0
              MOVE 'CONTINUE' TO WS-REASON
           END-IF.

       5000-ACCUMULATE.
           EVALUATE WS-REC-LEVEL
              WHEN 2
                 ADD 1 TO WS-STOP-CNT
                 DISPLAY 'STOP KEY=' REC-RECON-KEY
                         ' REASON=' WS-REASON
              WHEN 1
                 ADD 1 TO WS-WARN-CNT
                 DISPLAY 'WARN KEY=' REC-RECON-KEY
                         ' REASON=' WS-REASON
              WHEN OTHER
                 ADD 1 TO WS-OK-CNT
           END-EVALUATE

           IF WS-REC-LEVEL > WS-MAX-LEVEL
              MOVE WS-REC-LEVEL TO WS-MAX-LEVEL
           END-IF.

       9000-FINAL.
           CLOSE JHRECMF
           IF WS-JHRECMF-ST NOT = '00'
              DISPLAY 'JHRECMF CLOSE ERROR ST=' WS-JHRECMF-ST
              MOVE 12 TO RETURN-CODE
              GOBACK
           END-IF

           DISPLAY 'JH525S READ=' WS-READ-CNT
                   ' OK=' WS-OK-CNT
                   ' WARN=' WS-WARN-CNT
                   ' STOP=' WS-STOP-CNT

           EVALUATE WS-MAX-LEVEL
              WHEN 2
                 MOVE 8 TO RETURN-CODE
                 DISPLAY 'JH525S RESULT=STOP'
              WHEN 1
                 MOVE 4 TO RETURN-CODE
                 DISPLAY 'JH525S RESULT=WARN'
              WHEN OTHER
                 MOVE 0 TO RETURN-CODE
                 DISPLAY 'JH525S RESULT=OK'
           END-EVALUATE.
