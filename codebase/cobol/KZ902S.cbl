       IDENTIFICATION DIVISION.
       PROGRAM-ID. KZ902S.
      * 版数  年月日(和暦)  担当                       概要
      * 1.00  平成28.04.01  システム部 勘定系チーム   新規作成
      * 1.01  令和02.10.15  システム部 勘定系チーム   源泉税監査残高照合条件追加
      * 1.02  令和06.03.22  システム部 勘定系チーム   不一致明細抽出処理見直し
      ******************************************************************
      * 源泉税監査残高照合サブ
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZADLF ASSIGN TO "KZADLF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WK-KZADLF-ST.

       DATA DIVISION.
       FILE SECTION.
       FD  KZADLF.
           COPY KZADLFC.

       WORKING-STORAGE SECTION.
       01  WK-STATUS-AREA.
           05  WK-KZADLF-ST         PIC XX VALUE SPACES.
           05  WK-EOF-SW            PIC X  VALUE 'N'.
               88  WK-EOF           VALUE 'Y'.
               88  WK-NOT-EOF       VALUE 'N'.
           05  WK-HARD-ERR-SW       PIC X  VALUE 'N'.
               88  WK-HARD-ERR      VALUE 'Y'.
               88  WK-NORMAL        VALUE 'N'.

       01  WK-WORK-AREA.
           05  WK-ABS-DIFF          PIC S9(13)V99 COMP-3 VALUE 0.
           05  WK-RUN-DATE          PIC 9(8) VALUE 0.
           05  WK-EXIST-CNT         PIC 9(7) VALUE 0.
           05  WK-DUP-CNT           PIC 9(5) VALUE 0.
           05  WK-WRITE-CNT         PIC 9(5) VALUE 0.
           05  WK-CAUSE-CD          PIC X(2) VALUE SPACES.
           05  WK-ERR-MSG           PIC X(80) VALUE SPACES.

       01  WK-DATE-AREA.
           05  WK-CUR-DATE.
               10  WK-CUR-YYYY      PIC 9(4).
               10  WK-CUR-MM        PIC 9(2).
               10  WK-CUR-DD        PIC 9(2).

       LINKAGE SECTION.
       01  LK-KZ902S-PARM.
           05  LK-AUDIT-ID          PIC X(20).
           05  LK-SOURCE-DSID       PIC X(12).
           05  LK-DEBIT-AMT         PIC S9(13)V99 COMP-3.
           05  LK-CREDIT-AMT        PIC S9(13)V99 COMP-3.
           05  LK-RTN-CD            PIC 9(2).
           05  LK-CAUSE-CD          PIC X(2).
           05  LK-DIFF-AMT          PIC S9(13)V99 COMP-3.

       PROCEDURE DIVISION USING LK-KZ902S-PARM.
       0000-MAIN.
           MOVE 8 TO RETURN-CODE
           MOVE 8 TO LK-RTN-CD
           PERFORM 1000-INIT
           IF WK-NORMAL
               PERFORM 2000-VALIDATE
           END-IF
           IF WK-NORMAL
               PERFORM 3000-SCAN-EXIST
           END-IF
           IF WK-NORMAL
               PERFORM 4000-CALCULATE
           END-IF
           IF WK-NORMAL
               PERFORM 5000-WRITE-AUDIT
           END-IF
           PERFORM 9000-END
           GOBACK.

       1000-INIT.
           SET WK-NORMAL TO TRUE
           SET WK-NOT-EOF TO TRUE
           MOVE ZERO TO WK-EXIST-CNT
           MOVE ZERO TO WK-DUP-CNT
           MOVE ZERO TO WK-WRITE-CNT
           MOVE ZERO TO WK-ABS-DIFF
           MOVE ZERO TO LK-DIFF-AMT
           MOVE SPACES TO WK-CAUSE-CD
           MOVE SPACES TO LK-CAUSE-CD
           MOVE SPACES TO WK-ERR-MSG
           ACCEPT WK-CUR-DATE FROM DATE YYYYMMDD
           MOVE WK-CUR-DATE TO WK-RUN-DATE.

       2000-VALIDATE.
           IF LK-AUDIT-ID = SPACES
               MOVE 'AUDIT-ID REQUIRED' TO WK-ERR-MSG
               SET WK-HARD-ERR TO TRUE
           END-IF
           IF WK-NORMAL
              AND LK-SOURCE-DSID = SPACES
               MOVE 'SOURCE-DSID REQUIRED' TO WK-ERR-MSG
               SET WK-HARD-ERR TO TRUE
           END-IF
           IF WK-NORMAL
              AND LK-DEBIT-AMT = ZERO
              AND LK-CREDIT-AMT = ZERO
               MOVE 'DEBIT AND CREDIT ARE ZERO' TO WK-ERR-MSG
               SET WK-HARD-ERR TO TRUE
           END-IF
           IF WK-NORMAL
              AND LK-DEBIT-AMT < ZERO
               MOVE 'DEBIT AMOUNT IS NEGATIVE' TO WK-ERR-MSG
               SET WK-HARD-ERR TO TRUE
           END-IF
           IF WK-NORMAL
              AND LK-CREDIT-AMT < ZERO
               MOVE 'CREDIT AMOUNT IS NEGATIVE' TO WK-ERR-MSG
               SET WK-HARD-ERR TO TRUE
           END-IF.

       3000-SCAN-EXIST.
           OPEN INPUT KZADLF
           IF WK-KZADLF-ST NOT = '00'
              AND WK-KZADLF-ST NOT = '05'
               DISPLAY 'KZADLF INPUT OPEN ERROR ST='
                       WK-KZADLF-ST
               SET WK-HARD-ERR TO TRUE
           ELSE
               IF WK-KZADLF-ST = '05'
                   CONTINUE
               ELSE
                   PERFORM 3100-READ-LOOP
               END-IF
               CLOSE KZADLF
               IF WK-KZADLF-ST NOT = '00'
                   DISPLAY 'KZADLF INPUT CLOSE ERROR ST='
                           WK-KZADLF-ST
                   SET WK-HARD-ERR TO TRUE
               END-IF
           END-IF.

       3100-READ-LOOP.
           SET WK-NOT-EOF TO TRUE
           PERFORM UNTIL WK-EOF OR WK-HARD-ERR
               READ KZADLF
                   AT END
                       SET WK-EOF TO TRUE
                   NOT AT END
                       IF WK-KZADLF-ST = '00'
                           ADD 1 TO WK-EXIST-CNT
                           IF AL-AUDIT-ID = LK-AUDIT-ID
                              AND AL-SOURCE-DSID = LK-SOURCE-DSID
                               ADD 1 TO WK-DUP-CNT
                           END-IF
                       ELSE
                           DISPLAY 'KZADLF READ ERROR ST='
                                   WK-KZADLF-ST
                           SET WK-HARD-ERR TO TRUE
                       END-IF
               END-READ
           END-PERFORM.

       4000-CALCULATE.
           COMPUTE LK-DIFF-AMT = LK-DEBIT-AMT - LK-CREDIT-AMT
           IF LK-DIFF-AMT < ZERO
               COMPUTE WK-ABS-DIFF = LK-DIFF-AMT * -1
           ELSE
               MOVE LK-DIFF-AMT TO WK-ABS-DIFF
           END-IF
           IF LK-DIFF-AMT = ZERO
               MOVE 'OK' TO WK-CAUSE-CD
           ELSE
               PERFORM 4100-CLASSIFY
           END-IF
           MOVE WK-CAUSE-CD TO LK-CAUSE-CD.

       4100-CLASSIFY.
           IF LK-SOURCE-DSID(1:2) = 'GL'
               MOVE 'GL' TO WK-CAUSE-CD
           ELSE
               IF LK-SOURCE-DSID(1:2) = 'NO'
                  OR LK-SOURCE-DSID(1:3) = 'TAX'
                   MOVE 'NO' TO WK-CAUSE-CD
               ELSE
                   IF LK-SOURCE-DSID(1:2) = 'CH'
                      OR LK-SOURCE-DSID(1:3) = 'RPT'
                       MOVE 'CH' TO WK-CAUSE-CD
                   ELSE
                       IF LK-SOURCE-DSID(1:2) = 'TE'
                          OR WK-DUP-CNT > ZERO
                           MOVE 'TE' TO WK-CAUSE-CD
                       ELSE
                           IF WK-ABS-DIFF >= 1000000
                               MOVE 'GL' TO WK-CAUSE-CD
                           ELSE
                               IF LK-CREDIT-AMT > LK-DEBIT-AMT
                                   MOVE 'NO' TO WK-CAUSE-CD
                               ELSE
                                   MOVE 'CH' TO WK-CAUSE-CD
                               END-IF
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF.

       5000-WRITE-AUDIT.
           OPEN EXTEND KZADLF
           IF WK-KZADLF-ST NOT = '00'
              AND WK-KZADLF-ST NOT = '05'
               DISPLAY 'KZADLF EXTEND OPEN ERROR ST='
                       WK-KZADLF-ST
               SET WK-HARD-ERR TO TRUE
           ELSE
               PERFORM 5100-MAKE-REC
               WRITE KZADLF-REC
               IF WK-KZADLF-ST = '00'
                   ADD 1 TO WK-WRITE-CNT
               ELSE
                   DISPLAY 'KZADLF WRITE ERROR ST='
                           WK-KZADLF-ST
                   SET WK-HARD-ERR TO TRUE
               END-IF
               CLOSE KZADLF
               IF WK-KZADLF-ST NOT = '00'
                   DISPLAY 'KZADLF EXTEND CLOSE ERROR ST='
                           WK-KZADLF-ST
                   SET WK-HARD-ERR TO TRUE
               END-IF
           END-IF.

       5100-MAKE-REC.
           INITIALIZE KZADLF-REC
           MOVE LK-AUDIT-ID    TO AL-AUDIT-ID
           MOVE WK-RUN-DATE    TO AL-RUN-DATE
           MOVE LK-SOURCE-DSID TO AL-SOURCE-DSID
           MOVE LK-DEBIT-AMT   TO AL-DEBIT-AMT
           MOVE LK-CREDIT-AMT  TO AL-CREDIT-AMT
           MOVE LK-DIFF-AMT    TO AL-DIFF-AMT
           MOVE WK-CAUSE-CD    TO AL-STATUS-CD.

       9000-END.
           IF WK-HARD-ERR
               IF WK-ERR-MSG NOT = SPACES
                   DISPLAY WK-ERR-MSG
               END-IF
               MOVE 8 TO LK-RTN-CD
               MOVE 8 TO RETURN-CODE
           ELSE
               MOVE 0 TO LK-RTN-CD
               MOVE 0 TO RETURN-CODE
               DISPLAY 'KZ902S NORMAL END COUNT='
                       WK-WRITE-CNT
           END-IF.
