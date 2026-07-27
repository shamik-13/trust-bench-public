       IDENTIFICATION DIVISION.
       PROGRAM-ID. CT230B.
      *
      * 指図異動受付バッチ
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CCINSF-FILE ASSIGN TO "CCINSF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS IN-INS-ID
               FILE STATUS IS WS-CCINSF-ST.
           SELECT CCFCTF-FILE ASSIGN TO "CCFCTF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-CCFCTF-ST.
           SELECT CCVALF-FILE ASSIGN TO "CCVALF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-CCVALF-ST.
           SELECT CCCHGF-FILE ASSIGN TO "CCCHGF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-CCCHGF-ST.
           SELECT CCERRF-FILE ASSIGN TO "CCERRF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-CCERRF-ST.

       DATA DIVISION.
       FILE SECTION.
       FD  CCINSF-FILE.
           COPY CCINSC.
       FD  CCFCTF-FILE.
           COPY CCFCTFC.
       FD  CCVALF-FILE.
           COPY CCVALFC.
       FD  CCCHGF-FILE.
           COPY CCCHGC.
       FD  CCERRF-FILE.
           COPY CCERRC.

       WORKING-STORAGE SECTION.
       01  WS-PGM-ID                  PIC X(08) VALUE "CT230B".
       01  WS-CCINSF-ST               PIC X(02) VALUE SPACE.
       01  WS-CCFCTF-ST               PIC X(02) VALUE SPACE.
       01  WS-CCVALF-ST               PIC X(02) VALUE SPACE.
       01  WS-CCCHGF-ST               PIC X(02) VALUE SPACE.
       01  WS-CCERRF-ST               PIC X(02) VALUE SPACE.

       01  WS-END-FLAG                PIC X(01) VALUE "N".
           88  WS-END                           VALUE "Y".
           88  WS-NOT-END                       VALUE "N".
       01  WS-FCT-END-FLAG            PIC X(01) VALUE "N".
           88  WS-FCT-END                       VALUE "Y".
           88  WS-FCT-NOT-END                   VALUE "N".
       01  WS-VAL-END-FLAG            PIC X(01) VALUE "N".
           88  WS-VAL-END                       VALUE "Y".
           88  WS-VAL-NOT-END                   VALUE "N".
       01  WS-FCT-FOUND-FLAG          PIC X(01) VALUE "N".
           88  WS-FCT-FOUND                     VALUE "Y".
           88  WS-FCT-NOT-FOUND                 VALUE "N".
       01  WS-VAL-FIXED-FLAG          PIC X(01) VALUE "N".
           88  WS-VAL-FIXED                     VALUE "Y".
           88  WS-VAL-NOT-FIXED                 VALUE "N".
       01  WS-HARD-ERROR-FLAG         PIC X(01) VALUE "N".
           88  WS-HARD-ERROR                   VALUE "Y".
           88  WS-NO-HARD-ERROR                VALUE "N".

       01  WS-CHANGE-SEQ              PIC 9(09) VALUE ZERO.
       01  WS-ERROR-SEQ               PIC 9(09) VALUE ZERO.
       01  WS-ACCEPT-CNT              PIC 9(09) VALUE ZERO.
       01  WS-REJECT-CNT              PIC 9(09) VALUE ZERO.
       01  WS-READ-CNT                PIC 9(09) VALUE ZERO.
       01  WS-CHANGE-ID               PIC 9(09) VALUE ZERO.
       01  WS-ERROR-ID                PIC 9(09) VALUE ZERO.
       01  WS-SYS-DATE.
           05  WS-SYS-DATE-YYYY       PIC 9(04).
           05  WS-SYS-DATE-MM         PIC 9(02).
           05  WS-SYS-DATE-DD         PIC 9(02).
       01  WS-BASE-DT                 PIC 9(08) VALUE ZERO.
       01  WS-AFTER-STATUS            PIC X(02) VALUE SPACE.
       01  WS-ERROR-KBN               PIC X(02) VALUE SPACE.
       01  WS-ERROR-TEXT              PIC X(80) VALUE SPACE.

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE 8 TO RETURN-CODE
           ACCEPT WS-SYS-DATE FROM DATE YYYYMMDD
           MOVE WS-SYS-DATE TO WS-BASE-DT
           PERFORM 1000-OPEN-FILES
           IF WS-NO-HARD-ERROR
               PERFORM 2000-PROCESS UNTIL WS-END OR WS-HARD-ERROR
           END-IF
           PERFORM 9000-CLOSE-FILES
           IF WS-HARD-ERROR
               MOVE 8 TO RETURN-CODE
               DISPLAY "CT230B 異常終了"
           ELSE
               MOVE 0 TO RETURN-CODE
               DISPLAY "CT230B 正常終了 読込=" WS-READ-CNT
                       " 受付=" WS-ACCEPT-CNT
                       " 不可=" WS-REJECT-CNT
           END-IF
           GOBACK.

       1000-OPEN-FILES.
           SET WS-NO-HARD-ERROR TO TRUE
           OPEN INPUT CCINSF-FILE
           IF WS-CCINSF-ST NOT = "00"
               DISPLAY "CCINSF オープン失敗 ST=" WS-CCINSF-ST
               SET WS-HARD-ERROR TO TRUE
           END-IF
           IF WS-NO-HARD-ERROR
               OPEN OUTPUT CCCHGF-FILE
               IF WS-CCCHGF-ST NOT = "00"
                   DISPLAY "CCCHGF オープン失敗 ST=" WS-CCCHGF-ST
                   SET WS-HARD-ERROR TO TRUE
               END-IF
           END-IF
           IF WS-NO-HARD-ERROR
               OPEN OUTPUT CCERRF-FILE
               IF WS-CCERRF-ST NOT = "00"
                   DISPLAY "CCERRF オープン失敗 ST=" WS-CCERRF-ST
                   SET WS-HARD-ERROR TO TRUE
               END-IF
           END-IF
           .

       2000-PROCESS.
           READ CCINSF-FILE NEXT RECORD
               AT END
                   SET WS-END TO TRUE
               NOT AT END
                   ADD 1 TO WS-READ-CNT
                   PERFORM 3000-JUDGE-REQUEST
           END-READ
           .

       3000-JUDGE-REQUEST.
           SET WS-FCT-NOT-FOUND TO TRUE
           SET WS-VAL-NOT-FIXED TO TRUE
           MOVE SPACE TO WS-ERROR-TEXT
           MOVE SPACE TO WS-ERROR-KBN
           IF IN-INSTR-STATUS-KBN NOT = "01"
               MOVE "11" TO WS-ERROR-KBN
               MOVE "依頼状態が受付対象外"
                 TO WS-ERROR-TEXT
               PERFORM 7000-WRITE-ERROR
           ELSE
               PERFORM 4000-FIND-FCT
               IF WS-HARD-ERROR
                   CONTINUE
               ELSE
                   IF WS-FCT-NOT-FOUND
                       MOVE "12" TO WS-ERROR-KBN
                       MOVE "資金集中指図が存在しない"
                         TO WS-ERROR-TEXT
                       PERFORM 7000-WRITE-ERROR
                   ELSE
                       PERFORM 5000-FIND-FIXED-VALUE
                       IF WS-NO-HARD-ERROR
                           PERFORM 6000-APPLY-RULE
                       END-IF
                   END-IF
               END-IF
           END-IF
           .

       4000-FIND-FCT.
           SET WS-FCT-NOT-FOUND TO TRUE
           SET WS-FCT-NOT-END TO TRUE
           OPEN INPUT CCFCTF-FILE
           IF WS-CCFCTF-ST NOT = "00"
               DISPLAY "CCFCTF オープン失敗 ST=" WS-CCFCTF-ST
               SET WS-HARD-ERROR TO TRUE
           ELSE
               PERFORM UNTIL WS-FCT-END OR WS-FCT-FOUND
                   READ CCFCTF-FILE
                       AT END
                           SET WS-FCT-END TO TRUE
                       NOT AT END
                           IF FC-FCT-ID = IN-FCT-ID
                               SET WS-FCT-FOUND TO TRUE
                           END-IF
                   END-READ
               END-PERFORM
               CLOSE CCFCTF-FILE
               IF WS-CCFCTF-ST NOT = "00"
                   DISPLAY "CCFCTF クローズ失敗 ST=" WS-CCFCTF-ST
                   SET WS-HARD-ERROR TO TRUE
               END-IF
           END-IF
           .

       5000-FIND-FIXED-VALUE.
           SET WS-VAL-NOT-FIXED TO TRUE
           SET WS-VAL-NOT-END TO TRUE
           OPEN INPUT CCVALF-FILE
           IF WS-CCVALF-ST NOT = "00"
               DISPLAY "CCVALF オープン失敗 ST=" WS-CCVALF-ST
               SET WS-HARD-ERROR TO TRUE
           ELSE
               PERFORM UNTIL WS-VAL-END OR WS-VAL-FIXED
                   READ CCVALF-FILE
                       AT END
                           SET WS-VAL-END TO TRUE
                       NOT AT END
                           IF VL-FCT-ID = IN-FCT-ID
                              AND VL-VAL-STATUS-KBN = "01"
                               SET WS-VAL-FIXED TO TRUE
                           END-IF
                   END-READ
               END-PERFORM
               CLOSE CCVALF-FILE
               IF WS-CCVALF-ST NOT = "00"
                   DISPLAY "CCVALF クローズ失敗 ST=" WS-CCVALF-ST
                   SET WS-HARD-ERROR TO TRUE
               END-IF
           END-IF
           .

       6000-APPLY-RULE.
           EVALUATE TRUE
               WHEN FC-FCT-STATUS-KBN = "09"
                   MOVE "21" TO WS-ERROR-KBN
                   MOVE "資金集中指図は取消済"
                     TO WS-ERROR-TEXT
                   PERFORM 7000-WRITE-ERROR
               WHEN FC-FCT-STATUS-KBN = "08"
                   MOVE "22" TO WS-ERROR-KBN
                   MOVE "資金集中指図は保留中"
                     TO WS-ERROR-TEXT
                   PERFORM 7000-WRITE-ERROR
               WHEN FC-FCT-STATUS-KBN NOT = "01"
                   MOVE "23" TO WS-ERROR-KBN
                   MOVE "資金集中指図状態不正"
                     TO WS-ERROR-TEXT
                   PERFORM 7000-WRITE-ERROR
               WHEN IN-INSTR-KBN = "2"
                   MOVE "09" TO WS-AFTER-STATUS
                   PERFORM 6500-WRITE-CHANGE
               WHEN IN-INSTR-KBN = "1" AND WS-VAL-FIXED
                   MOVE "31" TO WS-ERROR-KBN
                   MOVE "確定受渡日あり変更不可"
                     TO WS-ERROR-TEXT
                   PERFORM 7000-WRITE-ERROR
               WHEN IN-INSTR-KBN = "1"
                   MOVE "01" TO WS-AFTER-STATUS
                   PERFORM 6500-WRITE-CHANGE
               WHEN OTHER
                   MOVE "24" TO WS-ERROR-KBN
                   MOVE "異動区分不正"
                     TO WS-ERROR-TEXT
                   PERFORM 7000-WRITE-ERROR
           END-EVALUATE
           .

       6500-WRITE-CHANGE.
           ADD 1 TO WS-CHANGE-SEQ
           MOVE WS-CHANGE-SEQ TO WS-CHANGE-ID
           INITIALIZE CCCHGF-REC
           MOVE WS-CHANGE-ID TO CH-CHANGE-ID
           MOVE IN-FCT-ID TO CH-FCT-ID
           MOVE WS-BASE-DT TO CH-CHANGE-DT
           MOVE IN-INSTR-KBN TO CH-CHANGE-KBN
           MOVE FC-FCT-STATUS-KBN TO CH-BEFORE-STATUS-KBN
           MOVE WS-AFTER-STATUS TO CH-AFTER-STATUS-KBN
           WRITE CCCHGF-REC
           IF WS-CCCHGF-ST = "00"
               ADD 1 TO WS-ACCEPT-CNT
           ELSE
               DISPLAY "CCCHGF ライト失敗 ST=" WS-CCCHGF-ST
               SET WS-HARD-ERROR TO TRUE
           END-IF
           .

       7000-WRITE-ERROR.
           ADD 1 TO WS-ERROR-SEQ
           MOVE WS-ERROR-SEQ TO WS-ERROR-ID
           INITIALIZE CCERRF-REC
           MOVE WS-ERROR-ID TO ER-ERROR-ID
           MOVE WS-PGM-ID TO ER-PGM-ID
           MOVE WS-BASE-DT TO ER-BASE-DT
           MOVE IN-INS-ID TO ER-RECORD-KEY
           MOVE WS-ERROR-KBN TO ER-ERROR-KBN
           MOVE WS-ERROR-TEXT TO ER-ERROR-TEXT
           WRITE CCERRF-REC
           IF WS-CCERRF-ST = "00"
               ADD 1 TO WS-REJECT-CNT
           ELSE
               DISPLAY "CCERRF ライト失敗 ST=" WS-CCERRF-ST
               SET WS-HARD-ERROR TO TRUE
           END-IF
           .

       9000-CLOSE-FILES.
           CLOSE CCINSF-FILE
           IF WS-CCINSF-ST NOT = "00"
              AND WS-CCINSF-ST NOT = "42"
               DISPLAY "CCINSF クローズ失敗 ST=" WS-CCINSF-ST
               SET WS-HARD-ERROR TO TRUE
           END-IF
           CLOSE CCCHGF-FILE
           IF WS-CCCHGF-ST NOT = "00"
              AND WS-CCCHGF-ST NOT = "42"
               DISPLAY "CCCHGF クローズ失敗 ST=" WS-CCCHGF-ST
               SET WS-HARD-ERROR TO TRUE
           END-IF
           CLOSE CCERRF-FILE
           IF WS-CCERRF-ST NOT = "00"
              AND WS-CCERRF-ST NOT = "42"
               DISPLAY "CCERRF クローズ失敗 ST=" WS-CCERRF-ST
               SET WS-HARD-ERROR TO TRUE
           END-IF
           .
