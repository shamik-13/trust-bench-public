       IDENTIFICATION DIVISION.
       PROGRAM-ID. KZ113B.
      * 変更履歴
      * 版数  年月日(和暦)  担当                      概要
      * 1.00  令和04.04.01  システム部 勘定系チーム  新規作成
      * 1.10  令和05.10.16  システム部 勘定系チーム  手数料転記条件追加
      * 1.20  令和06.07.08  システム部 勘定系チーム  利息再計算反映対応

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZACCTF
               ASSIGN       TO "KZACCTF"
               ORGANIZATION IS INDEXED
               ACCESS MODE  IS DYNAMIC
               RECORD KEY   IS AC-ACCT-NO
               FILE STATUS  IS WS-AC-ST.

           SELECT KZDLQF
               ASSIGN       TO "KZDLQF"
               ORGANIZATION IS INDEXED
               ACCESS MODE  IS DYNAMIC
               RECORD KEY   IS DL-ACCT-NO
               FILE STATUS  IS WS-DL-ST.

           SELECT KZFEEJF
               ASSIGN       TO "KZFEEJF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS  IS WS-FJ-ST.

       DATA DIVISION.
       FILE SECTION.
       FD  KZACCTF.
           COPY KZACCTC4.

       FD  KZDLQF.
           COPY KZDLQFC2.

       FD  KZFEEJF.
           COPY KZFEEJFC.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05 WS-AC-ST                 PIC XX VALUE SPACES.
           05 WS-DL-ST                 PIC XX VALUE SPACES.
           05 WS-FJ-ST                 PIC XX VALUE SPACES.

       01  WS-OPEN-SWITCHES.
           05 WS-AC-OPEN-SW            PIC X VALUE "N".
              88 WS-AC-OPEN                  VALUE "Y".
              88 WS-AC-CLOSED                VALUE "N".
           05 WS-DL-OPEN-SW            PIC X VALUE "N".
              88 WS-DL-OPEN                  VALUE "Y".
              88 WS-DL-CLOSED                VALUE "N".
           05 WS-FJ-OPEN-SW            PIC X VALUE "N".
              88 WS-FJ-OPEN                  VALUE "Y".
              88 WS-FJ-CLOSED                VALUE "N".

       01  WS-SWITCHES.
           05 WS-EOF-SW                PIC X VALUE "N".
              88 WS-EOF                      VALUE "Y".
              88 WS-NOT-EOF                  VALUE "N".
           05 WS-DLQ-FOUND-SW          PIC X VALUE "N".
              88 WS-DLQ-FOUND                VALUE "Y".
              88 WS-DLQ-NOT-FOUND            VALUE "N".
           05 WS-POST-OK-SW            PIC X VALUE "N".
              88 WS-POST-OK                  VALUE "Y".
              88 WS-POST-NG                  VALUE "N".
           05 WS-HARD-ERR-SW           PIC X VALUE "N".
              88 WS-HARD-ERR                 VALUE "Y".
              88 WS-NO-HARD-ERR              VALUE "N".

       01  WS-CONSTANTS.
           05 WS-POST-DATE             PIC 9(08) VALUE 20241130.
           05 WS-INTEREST-CODE         PIC X(03) VALUE "INT".
           05 WS-FEE-CODE              PIC X(03) VALUE "FEE".
           05 WS-DLQ-CODE              PIC X(03) VALUE "DLQ".
           05 WS-ERR-CODE              PIC X(03) VALUE "ERR".
           05 WS-RSN-NORMAL            PIC X(03) VALUE "000".
           05 WS-RSN-ACCT-TYPE         PIC X(03) VALUE "101".
           05 WS-RSN-BALANCE           PIC X(03) VALUE "102".
           05 WS-RSN-LIMIT             PIC X(03) VALUE "103".
           05 WS-RSN-REWRITE           PIC X(03) VALUE "201".
           05 WS-RSN-REREAD            PIC X(03) VALUE "202".
           05 WS-RSN-CHANGED           PIC X(03) VALUE "203".
           05 WS-MIN-AVG-BAL           PIC S9(13)V99 COMP-3 VALUE 0.
           05 WS-MAX-POST-AMT          PIC S9(11)V99 COMP-3
                                        VALUE 9999999.99.

       01  WS-RATES.
           05 WS-RATE-SAVING           PIC 9V9(07) COMP-3
                                        VALUE 0.0001200.
           05 WS-RATE-TIME             PIC 9V9(07) COMP-3
                                        VALUE 0.0003500.
           05 WS-RATE-CURRENT          PIC 9V9(07) COMP-3
                                        VALUE 0.0000000.
           05 WS-RATE-LOAN             PIC 9V9(07) COMP-3
                                        VALUE 0.0125000.

       01  WS-FEE-AMOUNTS.
           05 WS-MGMT-FEE-SAVING       PIC S9(7)V99 COMP-3
                                        VALUE 110.00.
           05 WS-MGMT-FEE-TIME         PIC S9(7)V99 COMP-3
                                        VALUE 0.00.
           05 WS-MGMT-FEE-CURRENT      PIC S9(7)V99 COMP-3
                                        VALUE 550.00.
           05 WS-MGMT-FEE-LOAN         PIC S9(7)V99 COMP-3
                                        VALUE 330.00.
           05 WS-DLQ-FEE-LOW           PIC S9(7)V99 COMP-3
                                        VALUE 220.00.
           05 WS-DLQ-FEE-MID           PIC S9(7)V99 COMP-3
                                        VALUE 550.00.
           05 WS-DLQ-FEE-HIGH          PIC S9(7)V99 COMP-3
                                        VALUE 1100.00.

       01  WS-WORK.
           05 WS-SAVED-ACCT-NO         PIC X(20) VALUE SPACES.
           05 WS-SAVED-BAL             PIC S9(13)V99 COMP-3 VALUE 0.
           05 WS-SAVED-AVG             PIC S9(13)V99 COMP-3 VALUE 0.
           05 WS-BASE-RATE             PIC 9V9(07) COMP-3 VALUE 0.
           05 WS-INTEREST-AMT          PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-MGMT-FEE-AMT          PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-DLQ-FEE-AMT           PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-POST-AMT              PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-NEW-BAL               PIC S9(13)V99 COMP-3 VALUE 0.
           05 WS-ABS-POST-AMT          PIC 9(11)V99 COMP-3 VALUE 0.
           05 WS-REASON                PIC X(03) VALUE SPACES.
           05 WS-TRAN-CODE             PIC X(03) VALUE SPACES.

       01  WS-COUNTERS.
           05 WS-READ-CNT              PIC 9(09) VALUE 0.
           05 WS-POST-CNT              PIC 9(09) VALUE 0.
           05 WS-SKIP-CNT              PIC 9(09) VALUE 0.
           05 WS-ERR-CNT               PIC 9(09) VALUE 0.

       PROCEDURE DIVISION.
       MAIN-RTN.
           PERFORM 1000-INIT
           IF WS-NO-HARD-ERR
              PERFORM 2000-MAIN UNTIL WS-EOF OR WS-HARD-ERR
           END-IF
           PERFORM 9000-END
           GOBACK.

       1000-INIT.
           MOVE 0 TO RETURN-CODE
           SET WS-NOT-EOF TO TRUE
           SET WS-NO-HARD-ERR TO TRUE
           SET WS-DLQ-NOT-FOUND TO TRUE

           OPEN I-O KZACCTF
           IF WS-AC-ST = "00"
              SET WS-AC-OPEN TO TRUE
           ELSE
              DISPLAY "KZACCTF OPEN ERROR ST=" WS-AC-ST
              SET WS-HARD-ERR TO TRUE
              MOVE 12 TO RETURN-CODE
           END-IF

           IF WS-NO-HARD-ERR
              OPEN INPUT KZDLQF
              IF WS-DL-ST = "00"
                 SET WS-DL-OPEN TO TRUE
              ELSE
                 DISPLAY "KZDLQF OPEN ERROR ST=" WS-DL-ST
                 SET WS-HARD-ERR TO TRUE
                 MOVE 12 TO RETURN-CODE
              END-IF
           END-IF

           IF WS-NO-HARD-ERR
              OPEN OUTPUT KZFEEJF
              IF WS-FJ-ST = "00"
                 SET WS-FJ-OPEN TO TRUE
              ELSE
                 DISPLAY "KZFEEJF OPEN ERROR ST=" WS-FJ-ST
                 SET WS-HARD-ERR TO TRUE
                 MOVE 12 TO RETURN-CODE
              END-IF
           END-IF

           IF WS-NO-HARD-ERR
              PERFORM 2100-READ-NEXT
           END-IF.

       2000-MAIN.
           ADD 1 TO WS-READ-CNT
           MOVE AC-ACCT-NO TO WS-SAVED-ACCT-NO
           MOVE AC-CUR-BAL TO WS-SAVED-BAL
           MOVE AC-AVG-BAL TO WS-SAVED-AVG

           PERFORM 3000-VALIDATE-ACCOUNT

           IF WS-POST-OK
              PERFORM 3100-READ-DLQ
              IF WS-NO-HARD-ERR
                 PERFORM 3200-CALCULATE-AMOUNT
                 PERFORM 3300-POST-ACCOUNT
              END-IF
           ELSE
              MOVE WS-ERR-CODE TO WS-TRAN-CODE
              MOVE 0 TO WS-POST-AMT
              MOVE AC-CUR-BAL TO WS-NEW-BAL
              PERFORM 3400-WRITE-JOURNAL
              ADD 1 TO WS-SKIP-CNT
           END-IF

           IF WS-NO-HARD-ERR
              PERFORM 2100-READ-NEXT
           END-IF.

       2100-READ-NEXT.
           READ KZACCTF NEXT RECORD
              AT END
                 SET WS-EOF TO TRUE
              NOT AT END
                 CONTINUE
           END-READ

           IF WS-AC-ST NOT = "00" AND WS-AC-ST NOT = "10"
              DISPLAY "KZACCTF READ ERROR ST=" WS-AC-ST
              SET WS-HARD-ERR TO TRUE
              MOVE 12 TO RETURN-CODE
           END-IF.

       3000-VALIDATE-ACCOUNT.
           SET WS-POST-OK TO TRUE
           MOVE WS-RSN-NORMAL TO WS-REASON

           EVALUATE AC-ACCT-TYPE
              WHEN "01"
              WHEN "02"
              WHEN "03"
              WHEN "04"
                 CONTINUE
              WHEN OTHER
                 SET WS-POST-NG TO TRUE
                 MOVE WS-RSN-ACCT-TYPE TO WS-REASON
                 DISPLAY "BAD ACCT TYPE " AC-ACCT-NO
           END-EVALUATE

           IF WS-POST-OK AND AC-AVG-BAL < WS-MIN-AVG-BAL
              SET WS-POST-NG TO TRUE
              MOVE WS-RSN-BALANCE TO WS-REASON
              DISPLAY "BAD AVG BAL " AC-ACCT-NO
           END-IF

           IF WS-POST-OK
              IF AC-ACCT-TYPE = "04"
                 IF AC-CREDIT-LIMIT <= 0
                    SET WS-POST-NG TO TRUE
                    MOVE WS-RSN-LIMIT TO WS-REASON
                    DISPLAY "BAD CREDIT LIMIT " AC-ACCT-NO
                 END-IF
              END-IF
           END-IF.

       3100-READ-DLQ.
           SET WS-DLQ-NOT-FOUND TO TRUE
           INITIALIZE KZDLQF-REC
           MOVE WS-SAVED-ACCT-NO TO DL-ACCT-NO

           READ KZDLQF RECORD KEY IS DL-ACCT-NO
              INVALID KEY
                 SET WS-DLQ-NOT-FOUND TO TRUE
              NOT INVALID KEY
                 SET WS-DLQ-FOUND TO TRUE
           END-READ

           IF WS-DL-ST NOT = "00" AND WS-DL-ST NOT = "23"
              DISPLAY "KZDLQF READ ERROR ST=" WS-DL-ST
              SET WS-HARD-ERR TO TRUE
              MOVE 12 TO RETURN-CODE
           END-IF.

       3200-CALCULATE-AMOUNT.
           MOVE 0 TO WS-INTEREST-AMT
           MOVE 0 TO WS-MGMT-FEE-AMT
           MOVE 0 TO WS-DLQ-FEE-AMT
           MOVE 0 TO WS-POST-AMT
           MOVE 0 TO WS-BASE-RATE

           EVALUATE AC-ACCT-TYPE
              WHEN "01"
                 MOVE WS-RATE-SAVING TO WS-BASE-RATE
                 MOVE WS-MGMT-FEE-SAVING TO WS-MGMT-FEE-AMT
              WHEN "02"
                 MOVE WS-RATE-TIME TO WS-BASE-RATE
                 MOVE WS-MGMT-FEE-TIME TO WS-MGMT-FEE-AMT
              WHEN "03"
                 MOVE WS-RATE-CURRENT TO WS-BASE-RATE
                 MOVE WS-MGMT-FEE-CURRENT TO WS-MGMT-FEE-AMT
              WHEN "04"
                 MOVE WS-RATE-LOAN TO WS-BASE-RATE
                 MOVE WS-MGMT-FEE-LOAN TO WS-MGMT-FEE-AMT
           END-EVALUATE

           COMPUTE WS-INTEREST-AMT ROUNDED =
              AC-AVG-BAL * WS-BASE-RATE / 12

           IF AC-ACCT-TYPE = "04"
              COMPUTE WS-INTEREST-AMT = WS-INTEREST-AMT * -1
           END-IF

           IF WS-DLQ-FOUND
              EVALUATE TRUE
                 WHEN DL-PAST-DUE-DAYS >= 90
                    MOVE WS-DLQ-FEE-HIGH TO WS-DLQ-FEE-AMT
                 WHEN DL-PAST-DUE-DAYS >= 30
                    MOVE WS-DLQ-FEE-MID TO WS-DLQ-FEE-AMT
                 WHEN DL-PAST-DUE-DAYS > 0
                    MOVE WS-DLQ-FEE-LOW TO WS-DLQ-FEE-AMT
                 WHEN OTHER
                    MOVE 0 TO WS-DLQ-FEE-AMT
              END-EVALUATE
           END-IF

           COMPUTE WS-POST-AMT =
              WS-INTEREST-AMT - WS-MGMT-FEE-AMT - WS-DLQ-FEE-AMT

           IF WS-POST-AMT < 0
              COMPUTE WS-ABS-POST-AMT = WS-POST-AMT * -1
           ELSE
              MOVE WS-POST-AMT TO WS-ABS-POST-AMT
           END-IF

           IF WS-ABS-POST-AMT > WS-MAX-POST-AMT
              SET WS-POST-NG TO TRUE
              MOVE WS-RSN-BALANCE TO WS-REASON
              DISPLAY "POST AMT TOO LARGE " AC-ACCT-NO
           END-IF.

       3300-POST-ACCOUNT.
           IF WS-POST-NG
              MOVE WS-ERR-CODE TO WS-TRAN-CODE
              MOVE AC-CUR-BAL TO WS-NEW-BAL
              PERFORM 3400-WRITE-JOURNAL
              ADD 1 TO WS-SKIP-CNT
           ELSE
              PERFORM 3310-REREAD-ACCOUNT
              IF WS-POST-OK
                 COMPUTE AC-CUR-BAL = AC-CUR-BAL + WS-POST-AMT
                 MOVE AC-CUR-BAL TO WS-NEW-BAL
                 REWRITE KZACCTF-REC
                 IF WS-AC-ST = "00"
                    PERFORM 3320-SET-TRAN-CODE
                    MOVE WS-RSN-NORMAL TO WS-REASON
                    PERFORM 3400-WRITE-JOURNAL
                    ADD 1 TO WS-POST-CNT
                 ELSE
                    MOVE WS-RSN-REWRITE TO WS-REASON
                    MOVE WS-ERR-CODE TO WS-TRAN-CODE
                    MOVE WS-SAVED-BAL TO WS-NEW-BAL
                    DISPLAY "KZACCTF REWRITE ERROR ST=" WS-AC-ST
                    PERFORM 3400-WRITE-JOURNAL
                    ADD 1 TO WS-ERR-CNT
                 END-IF
              ELSE
                 MOVE WS-ERR-CODE TO WS-TRAN-CODE
                 MOVE WS-SAVED-BAL TO WS-NEW-BAL
                 PERFORM 3400-WRITE-JOURNAL
                 ADD 1 TO WS-SKIP-CNT
              END-IF
           END-IF.

       3310-REREAD-ACCOUNT.
           MOVE WS-SAVED-ACCT-NO TO AC-ACCT-NO
           READ KZACCTF RECORD KEY IS AC-ACCT-NO
              INVALID KEY
                 SET WS-POST-NG TO TRUE
                 MOVE WS-RSN-REREAD TO WS-REASON
                 DISPLAY "REREAD MISS " WS-SAVED-ACCT-NO
              NOT INVALID KEY
                 IF AC-CUR-BAL NOT = WS-SAVED-BAL
                    SET WS-POST-NG TO TRUE
                    MOVE WS-RSN-CHANGED TO WS-REASON
                    DISPLAY "BAL CHANGED " WS-SAVED-ACCT-NO
                 END-IF
           END-READ

           IF WS-AC-ST NOT = "00" AND WS-AC-ST NOT = "23"
              SET WS-POST-NG TO TRUE
              MOVE WS-RSN-REREAD TO WS-REASON
              DISPLAY "KZACCTF REREAD ERROR ST=" WS-AC-ST
           END-IF.

       3320-SET-TRAN-CODE.
           IF WS-DLQ-FEE-AMT > 0
              MOVE WS-DLQ-CODE TO WS-TRAN-CODE
           ELSE
              IF WS-MGMT-FEE-AMT > 0
                 MOVE WS-FEE-CODE TO WS-TRAN-CODE
              ELSE
                 MOVE WS-INTEREST-CODE TO WS-TRAN-CODE
              END-IF
           END-IF.

       3400-WRITE-JOURNAL.
           INITIALIZE KZFEEJF-REC
           MOVE WS-SAVED-ACCT-NO TO FJ-ACCT-NO
           MOVE WS-POST-DATE TO FJ-POST-DATE
           MOVE WS-TRAN-CODE TO FJ-TRAN-CODE
           MOVE WS-POST-AMT TO FJ-POST-AMT
           MOVE WS-REASON TO FJ-FEE-REASON

           WRITE KZFEEJF-REC
           IF WS-FJ-ST NOT = "00"
              DISPLAY "KZFEEJF WRITE ERROR ST=" WS-FJ-ST
              SET WS-HARD-ERR TO TRUE
              MOVE 12 TO RETURN-CODE
           END-IF.

       9000-END.
           IF WS-AC-OPEN
              CLOSE KZACCTF
              SET WS-AC-CLOSED TO TRUE
              IF WS-AC-ST NOT = "00"
                 DISPLAY "KZACCTF CLOSE ERROR ST=" WS-AC-ST
                 MOVE 8 TO RETURN-CODE
              END-IF
           END-IF

           IF WS-DL-OPEN
              CLOSE KZDLQF
              SET WS-DL-CLOSED TO TRUE
              IF WS-DL-ST NOT = "00"
                 DISPLAY "KZDLQF CLOSE ERROR ST=" WS-DL-ST
                 MOVE 8 TO RETURN-CODE
              END-IF
           END-IF

           IF WS-FJ-OPEN
              CLOSE KZFEEJF
              SET WS-FJ-CLOSED TO TRUE
              IF WS-FJ-ST NOT = "00"
                 DISPLAY "KZFEEJF CLOSE ERROR ST=" WS-FJ-ST
                 MOVE 8 TO RETURN-CODE
              END-IF
           END-IF

           IF WS-HARD-ERR
              IF RETURN-CODE = 0
                 MOVE 12 TO RETURN-CODE
              END-IF
           ELSE
              IF RETURN-CODE = 0
                 DISPLAY "KZ190B NORMAL END READ=" WS-READ-CNT
                         " POST=" WS-POST-CNT
                         " SKIP=" WS-SKIP-CNT
                         " ERR=" WS-ERR-CNT
                 MOVE 0 TO RETURN-CODE
              END-IF
           END-IF.
