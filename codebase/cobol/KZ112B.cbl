       IDENTIFICATION DIVISION.
       PROGRAM-ID. KZ112B.
      * 変更履歴
      * 版数  年月日(和暦)  担当                      概要
      * 1.00  令和04.03.15  システム部 勘定系チーム  新規作成
      * 1.01  令和05.09.20  システム部 勘定系チーム  再日付受付判定を追加
      * 1.02  令和06.11.12  システム部 勘定系チーム  再処理結果ログ出力改善
      *---------------------------------------------------------------*
      *  再日付再処理受付バッチ                                      *
      *---------------------------------------------------------------*

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZRPRF
               ASSIGN       TO "KZRPRF"
               ORGANIZATION IS INDEXED
               ACCESS MODE  IS SEQUENTIAL
               RECORD KEY   IS RP-REPROCESS-ID
               FILE STATUS  IS FS-KZRPRF.

           SELECT KZCYCF
               ASSIGN       TO "KZCYCF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE  IS SEQUENTIAL
               FILE STATUS  IS FS-KZCYCF.

           SELECT KZCYRF
               ASSIGN       TO "KZCYRF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE  IS SEQUENTIAL
               FILE STATUS  IS FS-KZCYRF.

           SELECT KZCALF
               ASSIGN       TO "KZCALF"
               ORGANIZATION IS INDEXED
               ACCESS MODE  IS DYNAMIC
               RECORD KEY   IS CA-CAL-DT
               FILE STATUS  IS FS-KZCALF.

       DATA DIVISION.
       FILE SECTION.
       FD  KZRPRF.
           COPY KZRPRFC.

       FD  KZCYCF.
           COPY KZCYCFC.

       FD  KZCYRF.
           COPY KZCYRFC.

       FD  KZCALF.
           COPY KZCALFC.

       WORKING-STORAGE SECTION.
       01  FS-KZRPRF                 PIC X(02) VALUE SPACE.
           88  FS-RP-OK                        VALUE "00".
           88  FS-RP-EOF                       VALUE "10".
       01  FS-KZCYCF                 PIC X(02) VALUE SPACE.
           88  FS-CY-OK                        VALUE "00".
           88  FS-CY-EOF                       VALUE "10".
       01  FS-KZCYRF                 PIC X(02) VALUE SPACE.
           88  FS-CR-OK                        VALUE "00".
           88  FS-CR-EOF                       VALUE "10".
       01  FS-KZCALF                 PIC X(02) VALUE SPACE.
           88  FS-CA-OK                        VALUE "00".
           88  FS-CA-NF                        VALUE "23".

       01  WK-END-FLAG               PIC X(01) VALUE "N".
           88  WK-END                          VALUE "Y".
           88  WK-NOT-END                      VALUE "N".

       01  WK-HARD-ERROR             PIC X(01) VALUE "N".
           88  WK-ABEND                        VALUE "Y".
           88  WK-NORMAL                       VALUE "N".

       01  WK-APPLY-FLAG             PIC X(01) VALUE "N".
           88  WK-APPLY-OK                     VALUE "Y".
           88  WK-APPLY-NG                     VALUE "N".

       01  WK-CYCLE-FOUND            PIC X(01) VALUE "N".
           88  WK-CYCLE-YES                    VALUE "Y".
           88  WK-CYCLE-NO                     VALUE "N".

       01  WK-RESOLVE-FOUND          PIC X(01) VALUE "N".
           88  WK-RESOLVE-YES                  VALUE "Y".
           88  WK-RESOLVE-NO                   VALUE "N".

       01  WK-DATE-WORK.
           05 WK-SRC-DATE            PIC X(10) VALUE SPACE.
           05 WK-NORM-DATE           PIC 9(08) VALUE ZERO.
           05 WK-DIGIT-DATE          PIC X(08) VALUE SPACE.
           05 WK-DATE-OK-FLAG        PIC X(01) VALUE "N".
              88 WK-DATE-OK                    VALUE "Y".
              88 WK-DATE-NG                    VALUE "N".
           05 WK-YYYY                PIC 9(04) VALUE ZERO.
           05 WK-MM                  PIC 9(02) VALUE ZERO.
           05 WK-DD                  PIC 9(02) VALUE ZERO.
           05 WK-LEAP-FLAG           PIC X(01) VALUE "N".
              88 WK-LEAP                       VALUE "Y".
              88 WK-NON-LEAP                   VALUE "N".
           05 WK-MAX-DD              PIC 9(02) VALUE ZERO.

       01  WK-DATES.
           05 WK-ORG-DT              PIC 9(08) VALUE ZERO.
           05 WK-REDATE-DT           PIC 9(08) VALUE ZERO.
           05 WK-RESOLVED-DT         PIC 9(08) VALUE ZERO.
           05 WK-NEW-RESOLVED-DT     PIC 9(08) VALUE ZERO.

       01  WK-COUNTERS.
           05 WK-RD-RP-CNT           PIC 9(09) VALUE ZERO.
           05 WK-OK-CNT              PIC 9(09) VALUE ZERO.
           05 WK-SKIP-CNT            PIC 9(09) VALUE ZERO.
           05 WK-ERR-CNT             PIC 9(09) VALUE ZERO.

       01  WK-REASON                 PIC X(40) VALUE SPACE.
       01  WK-SAVE-CYCLE-ID          PIC X(20) VALUE SPACE.

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE 0 TO RETURN-CODE
           PERFORM 1000-OPEN
           IF WK-NORMAL
               PERFORM 2000-PROCESS UNTIL WK-END OR WK-ABEND
           END-IF
           PERFORM 9000-CLOSE
           IF WK-ABEND
               MOVE 8 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
               DISPLAY "KZ180B OK RD=" WK-RD-RP-CNT
                       " OK=" WK-OK-CNT
                       " SKIP=" WK-SKIP-CNT
                       " ERR=" WK-ERR-CNT
           END-IF
           GOBACK.

       1000-OPEN.
           OPEN I-O KZRPRF
           IF NOT FS-RP-OK
               DISPLAY "KZRPRF OPEN ST=" FS-KZRPRF
               SET WK-ABEND TO TRUE
           END-IF

           IF WK-NORMAL
               OPEN I-O KZCYCF
               IF NOT FS-CY-OK
                   DISPLAY "KZCYCF OPEN ST=" FS-KZCYCF
                   SET WK-ABEND TO TRUE
               END-IF
           END-IF

           IF WK-NORMAL
               OPEN INPUT KZCYRF
               IF NOT FS-CR-OK
                   DISPLAY "KZCYRF OPEN ST=" FS-KZCYRF
                   SET WK-ABEND TO TRUE
               END-IF
           END-IF

           IF WK-NORMAL
               OPEN INPUT KZCALF
               IF NOT FS-CA-OK
                   DISPLAY "KZCALF OPEN ST=" FS-KZCALF
                   SET WK-ABEND TO TRUE
               END-IF
           END-IF.

       2000-PROCESS.
           READ KZRPRF NEXT RECORD
               AT END
                   SET WK-END TO TRUE
               NOT AT END
                   IF NOT FS-RP-OK
                       DISPLAY "KZRPRF READ ST=" FS-KZRPRF
                       SET WK-ABEND TO TRUE
                   ELSE
                       ADD 1 TO WK-RD-RP-CNT
                       PERFORM 2100-EDIT-REQUEST
                       IF WK-APPLY-OK
                           PERFORM 3000-REFLECT-CYCLE
                       ELSE
                           ADD 1 TO WK-SKIP-CNT
                           DISPLAY "SKIP ID=" RP-REPROCESS-ID
                                   " REASON=" WK-REASON
                       END-IF
                   END-IF
           END-READ.

       2100-EDIT-REQUEST.
           SET WK-APPLY-NG TO TRUE
           MOVE SPACE TO WK-REASON
           MOVE ZERO TO WK-ORG-DT
           MOVE ZERO TO WK-REDATE-DT
           MOVE ZERO TO WK-RESOLVED-DT

           IF RP-REQUEST-STATUS NOT = "A"
               MOVE "NOT APPROVED" TO WK-REASON
           ELSE
               IF RP-APPROVER-ID = SPACE
                   MOVE "NO APPROVER" TO WK-REASON
               ELSE
                   MOVE RP-ORIGINAL-DT TO WK-SRC-DATE
                   PERFORM 2200-NORMALIZE-DATE
                   IF WK-DATE-NG
                       MOVE "BAD ORIGINAL DATE" TO WK-REASON
                   ELSE
                       MOVE WK-NORM-DATE TO WK-ORG-DT
                       MOVE RP-REDATE-DT TO WK-SRC-DATE
                       PERFORM 2200-NORMALIZE-DATE
                       IF WK-DATE-NG
                           MOVE "BAD REDATE" TO WK-REASON
                       ELSE
                           MOVE WK-NORM-DATE TO WK-REDATE-DT
                           PERFORM 2300-FIND-RESOLVE
                           IF WK-RESOLVE-NO
                               MOVE "NO RESOLVE DATE" TO WK-REASON
                           ELSE
                               PERFORM 2400-CHECK-RELATION
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF.

       2200-NORMALIZE-DATE.
           SET WK-DATE-NG TO TRUE
           MOVE SPACE TO WK-DIGIT-DATE
           MOVE ZERO TO WK-NORM-DATE
           MOVE ZERO TO WK-YYYY
           MOVE ZERO TO WK-MM
           MOVE ZERO TO WK-DD
           MOVE ZERO TO WK-MAX-DD
           MOVE "N" TO WK-LEAP-FLAG

           IF WK-SRC-DATE(1:8) IS NUMERIC
               MOVE WK-SRC-DATE(1:8) TO WK-DIGIT-DATE
           ELSE
               IF WK-SRC-DATE(1:4) IS NUMERIC
                  AND WK-SRC-DATE(5:1) = "-"
                  AND WK-SRC-DATE(6:2) IS NUMERIC
                  AND WK-SRC-DATE(8:1) = "-"
                  AND WK-SRC-DATE(9:2) IS NUMERIC
                   STRING WK-SRC-DATE(1:4)
                          WK-SRC-DATE(6:2)
                          WK-SRC-DATE(9:2)
                       DELIMITED BY SIZE
                       INTO WK-DIGIT-DATE
                   END-STRING
               END-IF
           END-IF

           IF WK-DIGIT-DATE IS NUMERIC
               MOVE WK-DIGIT-DATE(1:4) TO WK-YYYY
               MOVE WK-DIGIT-DATE(5:2) TO WK-MM
               MOVE WK-DIGIT-DATE(7:2) TO WK-DD
               PERFORM 2250-CHECK-CALENDAR-DATE
           END-IF.

       2250-CHECK-CALENDAR-DATE.
           IF FUNCTION MOD(WK-YYYY 400) = 0
               MOVE "Y" TO WK-LEAP-FLAG
           ELSE
               IF FUNCTION MOD(WK-YYYY 100) = 0
                   MOVE "N" TO WK-LEAP-FLAG
               ELSE
                   IF FUNCTION MOD(WK-YYYY 4) = 0
                       MOVE "Y" TO WK-LEAP-FLAG
                   END-IF
               END-IF
           END-IF

           EVALUATE WK-MM
               WHEN 1
               WHEN 3
               WHEN 5
               WHEN 7
               WHEN 8
               WHEN 10
               WHEN 12
                   MOVE 31 TO WK-MAX-DD
               WHEN 4
               WHEN 6
               WHEN 9
               WHEN 11
                   MOVE 30 TO WK-MAX-DD
               WHEN 2
                   IF WK-LEAP
                       MOVE 29 TO WK-MAX-DD
                   ELSE
                       MOVE 28 TO WK-MAX-DD
                   END-IF
               WHEN OTHER
                   MOVE ZERO TO WK-MAX-DD
           END-EVALUATE

           IF WK-YYYY >= 2000
              AND WK-YYYY <= 2099
              AND WK-DD >= 1
              AND WK-DD <= WK-MAX-DD
               MOVE WK-DIGIT-DATE TO WK-NORM-DATE
               SET WK-DATE-OK TO TRUE
           END-IF.

       2300-FIND-RESOLVE.
           SET WK-RESOLVE-NO TO TRUE
           MOVE ZERO TO WK-RESOLVED-DT

           CLOSE KZCYRF
           IF FS-KZCYRF NOT = "00"
               DISPLAY "KZCYRF CLOSE ST=" FS-KZCYRF
               SET WK-ABEND TO TRUE
           ELSE
               OPEN INPUT KZCYRF
               IF NOT FS-CR-OK
                   DISPLAY "KZCYRF REOPEN ST=" FS-KZCYRF
                   SET WK-ABEND TO TRUE
               END-IF
           END-IF

           PERFORM UNTIL WK-RESOLVE-YES OR FS-CR-EOF OR WK-ABEND
               READ KZCYRF
                   AT END
                       SET FS-CR-EOF TO TRUE
                   NOT AT END
                       IF NOT FS-CR-OK
                           DISPLAY "KZCYRF READ ST=" FS-KZCYRF
                           SET WK-ABEND TO TRUE
                       ELSE
                           IF CR-CYCLE-ID = RP-CYCLE-ID
                               SET WK-RESOLVE-YES TO TRUE
                               MOVE CR-RESOLVED-DT TO WK-RESOLVED-DT
                               IF CR-ROLLED-FLAG = "Y"
                                   MOVE "CYCLE CLOSED" TO WK-REASON
                                   SET WK-APPLY-NG TO TRUE
                               END-IF
                           END-IF
                       END-IF
               END-READ
           END-PERFORM.

       2400-CHECK-RELATION.
           IF WK-REASON NOT = SPACE
               CONTINUE
           ELSE
               IF WK-REDATE-DT < WK-ORG-DT
                   MOVE "REDATE BEFORE ORIGINAL" TO WK-REASON
               ELSE
                   IF WK-REDATE-DT > WK-RESOLVED-DT
                       MOVE "REDATE AFTER RESOLVE" TO WK-REASON
                   ELSE
                       MOVE WK-REDATE-DT TO CA-CAL-DT
                       READ KZCALF KEY IS CA-CAL-DT
                           INVALID KEY
                               MOVE "NO CALENDAR" TO WK-REASON
                           NOT INVALID KEY
                               IF NOT FS-CA-OK
                                   DISPLAY "KZCALF READ ST="
                                           FS-KZCALF
                                   SET WK-ABEND TO TRUE
                               ELSE
                                   IF CA-HOLIDAY-FLAG = "Y"
                                       MOVE "BANK HOLIDAY"
                                           TO WK-REASON
                                   ELSE
                                       IF CA-HOLIDAY-FLAG NOT = "N"
                                           MOVE "BAD HOLIDAY FLAG"
                                               TO WK-REASON
                                       ELSE
                                           SET WK-APPLY-OK TO TRUE
                                       END-IF
                                   END-IF
                               END-IF
                       END-READ
                   END-IF
               END-IF
           END-IF.

       3000-REFLECT-CYCLE.
           SET WK-CYCLE-NO TO TRUE
           MOVE RP-CYCLE-ID TO WK-SAVE-CYCLE-ID

           CLOSE KZCYCF
           IF FS-KZCYCF NOT = "00"
               DISPLAY "KZCYCF CLOSE ST=" FS-KZCYCF
               SET WK-ABEND TO TRUE
           ELSE
               OPEN I-O KZCYCF
               IF NOT FS-CY-OK
                   DISPLAY "KZCYCF REOPEN ST=" FS-KZCYCF
                   SET WK-ABEND TO TRUE
               END-IF
           END-IF

           PERFORM UNTIL WK-CYCLE-YES OR FS-CY-EOF OR WK-ABEND
               READ KZCYCF
                   AT END
                       SET FS-CY-EOF TO TRUE
                   NOT AT END
                       IF NOT FS-CY-OK
                           DISPLAY "KZCYCF READ ST=" FS-KZCYCF
                           SET WK-ABEND TO TRUE
                       ELSE
                           IF CY-CYCLE-ID = WK-SAVE-CYCLE-ID
                               SET WK-CYCLE-YES TO TRUE
                               MOVE WK-REDATE-DT TO CY-NOMINAL-DT
                               REWRITE KZCYCF-REC
                               IF NOT FS-CY-OK
                                   DISPLAY "KZCYCF REWRITE ST="
                                           FS-KZCYCF
                                   SET WK-ABEND TO TRUE
                               ELSE
                                   PERFORM 3100-UPDATE-REQUEST
                               END-IF
                           END-IF
                       END-IF
               END-READ
           END-PERFORM

           IF WK-NORMAL AND WK-CYCLE-NO
               ADD 1 TO WK-ERR-CNT
               DISPLAY "NO CYCLE ID=" RP-REPROCESS-ID
                       " CY=" RP-CYCLE-ID
           END-IF.

       3100-UPDATE-REQUEST.
           MOVE "R" TO RP-REQUEST-STATUS
           REWRITE KZRPRF-REC
           IF NOT FS-RP-OK
               DISPLAY "KZRPRF REWRITE ST=" FS-KZRPRF
               SET WK-ABEND TO TRUE
           ELSE
               PERFORM 3200-RECALC-RESOLVED
               IF WK-NORMAL
                   ADD 1 TO WK-OK-CNT
                   DISPLAY "APPLIED ID=" RP-REPROCESS-ID
                           " CY=" RP-CYCLE-ID
                           " REDATE=" WK-REDATE-DT
                           " RESOLVE=" WK-NEW-RESOLVED-DT
               END-IF
           END-IF.

       3200-RECALC-RESOLVED.
           MOVE WK-REDATE-DT TO WK-NEW-RESOLVED-DT
           PERFORM UNTIL WK-ABEND
               MOVE WK-NEW-RESOLVED-DT TO CA-CAL-DT
               READ KZCALF KEY IS CA-CAL-DT
                   INVALID KEY
                       DISPLAY "KZCALF NO RESOLVE DT="
                               WK-NEW-RESOLVED-DT
                       SET WK-ABEND TO TRUE
                   NOT INVALID KEY
                       IF NOT FS-CA-OK
                           DISPLAY "KZCALF RESOLVE READ ST="
                                   FS-KZCALF
                           SET WK-ABEND TO TRUE
                       ELSE
                           IF CA-HOLIDAY-FLAG = "N"
                               EXIT PERFORM
                           ELSE
                               ADD 1 TO WK-NEW-RESOLVED-DT
                               PERFORM 3300-ADJUST-SIMPLE-DATE
                           END-IF
                       END-IF
               END-READ
           END-PERFORM.

       3300-ADJUST-SIMPLE-DATE.
           SET WK-DATE-NG TO TRUE
           MOVE WK-NEW-RESOLVED-DT TO WK-DIGIT-DATE
           MOVE WK-DIGIT-DATE(1:4) TO WK-YYYY
           MOVE WK-DIGIT-DATE(5:2) TO WK-MM
           MOVE WK-DIGIT-DATE(7:2) TO WK-DD
           MOVE ZERO TO WK-MAX-DD
           MOVE "N" TO WK-LEAP-FLAG
           PERFORM 2250-CHECK-CALENDAR-DATE
           IF WK-DATE-NG
               ADD 1 TO WK-MM
               MOVE 1 TO WK-DD
               IF WK-MM > 12
                   ADD 1 TO WK-YYYY
                   MOVE 1 TO WK-MM
               END-IF
               COMPUTE WK-NEW-RESOLVED-DT =
                   (WK-YYYY * 10000) + (WK-MM * 100) + WK-DD
           END-IF
           SET WK-DATE-NG TO TRUE.

       9000-CLOSE.
           CLOSE KZRPRF
           IF FS-KZRPRF NOT = "00" AND FS-KZRPRF NOT = SPACE
               DISPLAY "KZRPRF CLOSE ST=" FS-KZRPRF
               SET WK-ABEND TO TRUE
           END-IF

           CLOSE KZCYCF
           IF FS-KZCYCF NOT = "00" AND FS-KZCYCF NOT = SPACE
               DISPLAY "KZCYCF CLOSE ST=" FS-KZCYCF
               SET WK-ABEND TO TRUE
           END-IF

           CLOSE KZCYRF
           IF FS-KZCYRF NOT = "00" AND FS-KZCYRF NOT = SPACE
               DISPLAY "KZCYRF CLOSE ST=" FS-KZCYRF
               SET WK-ABEND TO TRUE
           END-IF

           CLOSE KZCALF
           IF FS-KZCALF NOT = "00" AND FS-KZCALF NOT = SPACE
               DISPLAY "KZCALF CLOSE ST=" FS-KZCALF
               SET WK-ABEND TO TRUE
           END-IF.
