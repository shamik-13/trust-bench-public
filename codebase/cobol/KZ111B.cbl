       IDENTIFICATION DIVISION.
       PROGRAM-ID. KZ111B.
      * 変更履歴
      * 版数  年月日(和暦)  担当                       概要
      * 1.00  H30.04.01    システム部 勘定系チーム    新規作成
      * 1.01  R02.10.15    システム部 勘定系チーム    延滞抽出条件見直し
      * 1.02  R05.06.20    システム部 勘定系チーム    ウォッチ登録件数出力追加
       AUTHOR. KZBATCH.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZACCTF ASSIGN TO "KZACCTF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS AC-ACCT-NO
               FILE STATUS IS ST-KZACCTF.

           SELECT KZDLQF ASSIGN TO "KZDLQF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS DL-ACCT-NO
               FILE STATUS IS ST-KZDLQF.

           SELECT KZFEEJF ASSIGN TO "KZFEEJF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS ST-KZFEEJF.

           SELECT KZEXPF ASSIGN TO "KZEXPF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS ST-KZEXPF.

       DATA DIVISION.
       FILE SECTION.
       FD  KZACCTF.
           COPY KZACCTC4.

       FD  KZDLQF.
           COPY KZDLQFC2.

       FD  KZFEEJF.
           COPY KZFEEJFC.

       FD  KZEXPF.
           COPY KZEXPFC.

       WORKING-STORAGE SECTION.
       01  ST-KZACCTF                  PIC XX.
       01  ST-KZDLQF                   PIC XX.
       01  ST-KZFEEJF                  PIC XX.
       01  ST-KZEXPF                   PIC XX.

       01  WS-SW.
           05  WS-ACCT-EOF             PIC X VALUE "N".
               88  ACCT-EOF                 VALUE "Y".
           05  WS-FEE-EOF              PIC X VALUE "N".
               88  FEE-EOF                  VALUE "Y".
           05  WS-DLQ-FOUND            PIC X VALUE "N".
               88  DLQ-FOUND                VALUE "Y".
           05  WS-HARD-ERR             PIC X VALUE "N".
               88  HARD-ERR                 VALUE "Y".

       01  WS-PARM.
           05  WS-AS-OF-DATE           PIC 9(08) VALUE 20240628.
           05  WS-NOTICE-GAP           PIC 9(04) VALUE 30.
           05  WS-EXPOSURE-LIMIT       PIC S9(13)V99 COMP-3
                                        VALUE 1200000.00.
           05  WS-MAX-FEE-ROW          PIC 9(05) VALUE 20000.

       01  WS-CALC.
           05  WS-FEE-AMT              PIC S9(13)V99 COMP-3 VALUE 0.
           05  WS-BASE-DUE             PIC S9(13)V99 COMP-3 VALUE 0.
           05  WS-NEW-DUE              PIC S9(13)V99 COMP-3 VALUE 0.
           05  WS-OVER-LIMIT           PIC S9(13)V99 COMP-3 VALUE 0.
           05  WS-DAYS-ADD             PIC 9(04) VALUE 0.
           05  WS-DATE-DIFF            PIC S9(09) COMP VALUE 0.
           05  WS-DATE-FROM            PIC 9(08) VALUE 0.
           05  WS-DATE-TO              PIC 9(08) VALUE 0.
           05  WS-PAST-DUE-DAYS        PIC 9(05) VALUE 0.
           05  WS-RANK                 PIC 9 VALUE 0.

       01  WS-COUNT.
           05  CNT-ACCT-READ           PIC 9(09) VALUE 0.
           05  CNT-FEE-READ            PIC 9(09) VALUE 0.
           05  CNT-DLQ-WRITE           PIC 9(09) VALUE 0.
           05  CNT-EXP-WRITE           PIC 9(09) VALUE 0.
           05  CNT-ERR                 PIC 9(09) VALUE 0.

       01  WS-FEE-IDX                  PIC 9(05) COMP VALUE 0.
       01  WS-FEE-CNT                  PIC 9(05) COMP VALUE 0.

       01  WS-FEE-TABLE.
           05  WS-FEE-ENT OCCURS 20000 TIMES.
               10  TB-FEE-ACCT-NO      PIC X(20).
               10  TB-FEE-AMT          PIC S9(13)V99 COMP-3.

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE 0 TO RETURN-CODE
           PERFORM 1000-OPEN-FILES
           IF HARD-ERR
               PERFORM 9000-CLOSE-FILES
               MOVE 12 TO RETURN-CODE
               GOBACK
           END-IF

           PERFORM 2000-LOAD-FEE
           IF HARD-ERR
               PERFORM 9000-CLOSE-FILES
               MOVE 12 TO RETURN-CODE
               GOBACK
           END-IF

           PERFORM 3000-PROCESS-ACCT UNTIL ACCT-EOF OR HARD-ERR

           PERFORM 9000-CLOSE-FILES

           IF HARD-ERR
               MOVE 12 TO RETURN-CODE
           ELSE
               DISPLAY "KZ180B NORMAL END ACCT="
                   CNT-ACCT-READ " DLQ=" CNT-DLQ-WRITE
                   " EXP=" CNT-EXP-WRITE
               MOVE 0 TO RETURN-CODE
           END-IF
           GOBACK.

       1000-OPEN-FILES.
           OPEN INPUT KZACCTF
           IF ST-KZACCTF NOT = "00"
               DISPLAY "KZACCTF OPEN ERROR ST=" ST-KZACCTF
               SET HARD-ERR TO TRUE
           END-IF

           OPEN I-O KZDLQF
           IF ST-KZDLQF NOT = "00"
               DISPLAY "KZDLQF OPEN ERROR ST=" ST-KZDLQF
               SET HARD-ERR TO TRUE
           END-IF

           OPEN INPUT KZFEEJF
           IF ST-KZFEEJF NOT = "00"
               DISPLAY "KZFEEJF OPEN ERROR ST=" ST-KZFEEJF
               SET HARD-ERR TO TRUE
           END-IF

           OPEN OUTPUT KZEXPF
           IF ST-KZEXPF NOT = "00"
               DISPLAY "KZEXPF OPEN ERROR ST=" ST-KZEXPF
               SET HARD-ERR TO TRUE
           END-IF

           IF NOT HARD-ERR
               READ KZACCTF
                   AT END SET ACCT-EOF TO TRUE
                   NOT AT END ADD 1 TO CNT-ACCT-READ
               END-READ
               IF ST-KZACCTF NOT = "00" AND ST-KZACCTF NOT = "10"
                   DISPLAY "KZACCTF FIRST READ ERROR ST=" ST-KZACCTF
                   SET HARD-ERR TO TRUE
               END-IF
           END-IF.

       2000-LOAD-FEE.
           PERFORM UNTIL FEE-EOF OR HARD-ERR
               READ KZFEEJF
                   AT END SET FEE-EOF TO TRUE
                   NOT AT END
                       ADD 1 TO CNT-FEE-READ
                       PERFORM 2100-ACCUM-FEE
               END-READ
               IF ST-KZFEEJF NOT = "00" AND ST-KZFEEJF NOT = "10"
                   DISPLAY "KZFEEJF READ ERROR ST=" ST-KZFEEJF
                   SET HARD-ERR TO TRUE
               END-IF
           END-PERFORM.

       2100-ACCUM-FEE.
           IF FJ-ACCT-NO = SPACE
               DISPLAY "FEE JOURNAL BLANK ACCOUNT"
               ADD 1 TO CNT-ERR
           ELSE
               IF FJ-POST-DATE = WS-AS-OF-DATE
                   IF FJ-TRAN-CODE = "FEE" OR FJ-TRAN-CODE = "INT"
                       MOVE 1 TO WS-FEE-IDX
                       PERFORM UNTIL WS-FEE-IDX > WS-FEE-CNT
                          OR TB-FEE-ACCT-NO(WS-FEE-IDX) = FJ-ACCT-NO
                           ADD 1 TO WS-FEE-IDX
                       END-PERFORM
                       IF WS-FEE-IDX <= WS-FEE-CNT
                           ADD FJ-POST-AMT TO TB-FEE-AMT(WS-FEE-IDX)
                       ELSE
                           IF WS-FEE-CNT < WS-MAX-FEE-ROW
                               ADD 1 TO WS-FEE-CNT
                               MOVE FJ-ACCT-NO
                                 TO TB-FEE-ACCT-NO(WS-FEE-CNT)
                               MOVE FJ-POST-AMT
                                 TO TB-FEE-AMT(WS-FEE-CNT)
                           ELSE
                               DISPLAY "FEE TABLE OVERFLOW"
                               SET HARD-ERR TO TRUE
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF.

       3000-PROCESS-ACCT.
           PERFORM 3100-VALIDATE-ACCT

           IF NOT HARD-ERR
               PERFORM 3200-FIND-FEE
               PERFORM 3300-READ-DLQ
               PERFORM 3400-CALC-DLQ
               IF WS-NEW-DUE > 0
                   PERFORM 3500-WRITE-DLQ
                   IF WS-NEW-DUE >= WS-EXPOSURE-LIMIT
                       PERFORM 3600-WRITE-EXP
                   END-IF
               END-IF
           END-IF

           READ KZACCTF
               AT END SET ACCT-EOF TO TRUE
               NOT AT END ADD 1 TO CNT-ACCT-READ
           END-READ
           IF ST-KZACCTF NOT = "00" AND ST-KZACCTF NOT = "10"
               DISPLAY "KZACCTF READ ERROR ST=" ST-KZACCTF
               SET HARD-ERR TO TRUE
           END-IF.

       3100-VALIDATE-ACCT.
           IF AC-ACCT-NO = SPACE
               DISPLAY "BLANK ACCOUNT NUMBER"
               ADD 1 TO CNT-ERR
           END-IF
           IF AC-CUST-ID = SPACE
               DISPLAY "BLANK CUSTOMER ID ACCOUNT=" AC-ACCT-NO
               ADD 1 TO CNT-ERR
           END-IF
           IF AC-ACCT-TYPE NOT = "01"
              AND AC-ACCT-TYPE NOT = "02"
              AND AC-ACCT-TYPE NOT = "03"
               DISPLAY "BAD ACCOUNT TYPE ACCOUNT=" AC-ACCT-NO
               ADD 1 TO CNT-ERR
           END-IF.

       3200-FIND-FEE.
           MOVE 0 TO WS-FEE-AMT
           MOVE 1 TO WS-FEE-IDX
           PERFORM UNTIL WS-FEE-IDX > WS-FEE-CNT
              OR TB-FEE-ACCT-NO(WS-FEE-IDX) = AC-ACCT-NO
               ADD 1 TO WS-FEE-IDX
           END-PERFORM
           IF WS-FEE-IDX <= WS-FEE-CNT
               MOVE TB-FEE-AMT(WS-FEE-IDX) TO WS-FEE-AMT
           END-IF.

       3300-READ-DLQ.
           MOVE "N" TO WS-DLQ-FOUND
           MOVE AC-ACCT-NO TO DL-ACCT-NO
           READ KZDLQF KEY IS DL-ACCT-NO
               INVALID KEY
                   MOVE "N" TO WS-DLQ-FOUND
               NOT INVALID KEY
                   SET DLQ-FOUND TO TRUE
           END-READ
           IF ST-KZDLQF NOT = "00"
              AND ST-KZDLQF NOT = "23"
              AND ST-KZDLQF NOT = "10"
               DISPLAY "KZDLQF READ ERROR ST=" ST-KZDLQF
                   " ACCOUNT=" AC-ACCT-NO
               SET HARD-ERR TO TRUE
           END-IF.

       3400-CALC-DLQ.
           MOVE 0 TO WS-OVER-LIMIT
           MOVE 0 TO WS-BASE-DUE
           MOVE 0 TO WS-NEW-DUE
           MOVE 0 TO WS-DAYS-ADD
           MOVE 0 TO WS-DATE-DIFF
           MOVE 0 TO WS-PAST-DUE-DAYS
           MOVE 0 TO WS-RANK

           IF AC-CUR-BAL > AC-CREDIT-LIMIT
               COMPUTE WS-OVER-LIMIT = AC-CUR-BAL - AC-CREDIT-LIMIT
           END-IF

           IF DLQ-FOUND
               MOVE DL-DUE-AMT TO WS-BASE-DUE
               MOVE DL-PAST-DUE-DAYS TO WS-PAST-DUE-DAYS
               MOVE WS-AS-OF-DATE TO WS-DATE-TO
               MOVE DL-AS-OF-DATE TO WS-DATE-FROM
               IF WS-DATE-FROM > 0
                  AND WS-DATE-FROM < WS-DATE-TO
                   COMPUTE WS-DATE-DIFF =
                       FUNCTION INTEGER-OF-DATE(WS-DATE-TO)
                     - FUNCTION INTEGER-OF-DATE(WS-DATE-FROM)
                   IF WS-DATE-DIFF > 0 AND WS-DATE-DIFF < 366
                       MOVE WS-DATE-DIFF TO WS-DAYS-ADD
                   END-IF
               END-IF
           END-IF

           COMPUTE WS-NEW-DUE = WS-BASE-DUE + WS-FEE-AMT
           IF WS-OVER-LIMIT > WS-NEW-DUE
               MOVE WS-OVER-LIMIT TO WS-NEW-DUE
           END-IF

           IF WS-NEW-DUE > 0
               IF DLQ-FOUND
                   ADD WS-DAYS-ADD TO WS-PAST-DUE-DAYS
               ELSE
                   MOVE 1 TO WS-PAST-DUE-DAYS
               END-IF
               MOVE WS-PAST-DUE-DAYS TO DL-PAST-DUE-DAYS

               IF WS-PAST-DUE-DAYS >= 90
                  OR WS-NEW-DUE >= 5000000
                   MOVE 4 TO WS-RANK
               ELSE
                   IF WS-PAST-DUE-DAYS >= 60
                      OR WS-NEW-DUE >= 3500000
                       MOVE 3 TO WS-RANK
                   ELSE
                       IF WS-PAST-DUE-DAYS >= 30
                          OR WS-NEW-DUE >= 900000
                           MOVE 2 TO WS-RANK
                       ELSE
                           MOVE 1 TO WS-RANK
                       END-IF
                   END-IF
               END-IF

               IF DLQ-FOUND
                  AND DL-LAST-NOTICE-DATE > 0
                   MOVE WS-AS-OF-DATE TO WS-DATE-TO
                   MOVE DL-LAST-NOTICE-DATE TO WS-DATE-FROM
                   IF WS-DATE-FROM < WS-DATE-TO
                       COMPUTE WS-DATE-DIFF =
                           FUNCTION INTEGER-OF-DATE(WS-DATE-TO)
                         - FUNCTION INTEGER-OF-DATE(WS-DATE-FROM)
                       IF WS-DATE-DIFF >= WS-NOTICE-GAP
                          AND WS-RANK < 4
                           ADD 1 TO WS-RANK
                       END-IF
                   END-IF
               END-IF
           END-IF.

       3500-WRITE-DLQ.
           IF NOT DLQ-FOUND
               MOVE AC-ACCT-NO TO DL-ACCT-NO
               MOVE 0 TO DL-LAST-NOTICE-DATE
           END-IF
           MOVE WS-AS-OF-DATE TO DL-AS-OF-DATE
           MOVE WS-NEW-DUE TO DL-DUE-AMT
           MOVE WS-RANK TO DL-WATCH-RANK

           IF DLQ-FOUND
               REWRITE KZDLQF-REC
               IF ST-KZDLQF NOT = "00"
                   DISPLAY "KZDLQF REWRITE ERROR ST=" ST-KZDLQF
                       " ACCOUNT=" AC-ACCT-NO
                   SET HARD-ERR TO TRUE
               ELSE
                   ADD 1 TO CNT-DLQ-WRITE
               END-IF
           ELSE
               WRITE KZDLQF-REC
               IF ST-KZDLQF NOT = "00"
                   DISPLAY "KZDLQF WRITE ERROR ST=" ST-KZDLQF
                       " ACCOUNT=" AC-ACCT-NO
                   SET HARD-ERR TO TRUE
               ELSE
                   ADD 1 TO CNT-DLQ-WRITE
               END-IF
           END-IF.

       3600-WRITE-EXP.
           MOVE AC-CUST-ID TO EX-CUST-ID
           MOVE AC-ACCT-TYPE TO EX-PRODUCT-TYPE
           MOVE WS-NEW-DUE TO EX-EXPOSURE-AMT
           WRITE KZEXPF-REC
           IF ST-KZEXPF NOT = "00"
               DISPLAY "KZEXPF WRITE ERROR ST=" ST-KZEXPF
                   " CUSTOMER=" AC-CUST-ID
               SET HARD-ERR TO TRUE
           ELSE
               ADD 1 TO CNT-EXP-WRITE
           END-IF.

       9000-CLOSE-FILES.
           CLOSE KZACCTF
           IF ST-KZACCTF NOT = "00"
              AND ST-KZACCTF NOT = "42"
               DISPLAY "KZACCTF CLOSE ERROR ST=" ST-KZACCTF
               SET HARD-ERR TO TRUE
           END-IF

           CLOSE KZDLQF
           IF ST-KZDLQF NOT = "00"
              AND ST-KZDLQF NOT = "42"
               DISPLAY "KZDLQF CLOSE ERROR ST=" ST-KZDLQF
               SET HARD-ERR TO TRUE
           END-IF

           CLOSE KZFEEJF
           IF ST-KZFEEJF NOT = "00"
              AND ST-KZFEEJF NOT = "42"
               DISPLAY "KZFEEJF CLOSE ERROR ST=" ST-KZFEEJF
               SET HARD-ERR TO TRUE
           END-IF

           CLOSE KZEXPF
           IF ST-KZEXPF NOT = "00"
              AND ST-KZEXPF NOT = "42"
               DISPLAY "KZEXPF CLOSE ERROR ST=" ST-KZEXPF
               SET HARD-ERR TO TRUE
           END-IF.
