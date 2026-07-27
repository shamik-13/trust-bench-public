       IDENTIFICATION DIVISION.
       PROGRAM-ID. CB218B.
      *--------------------------------------------------------------*
      * 一時増枠期限管理バッチ
      * 変更履歴
      * 1.00  20240315  CARD-DEV  初版作成
      * 1.01  20240920  CARD-DEV  取消済み検査追加
      * 1.02  20250110  CARD-DEV  期限切れ通知ログ追加
      *--------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CDLIMF ASSIGN TO "CDLIMF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS LM-CARD-NO
               FILE STATUS IS FS-CDLIMF.
           SELECT CDCARDF ASSIGN TO "CDCARDF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CF-CARD-NO
               FILE STATUS IS FS-CDCARDF.

       DATA DIVISION.
       FILE SECTION.
       FD  CDLIMF.
           COPY CDLIMC.
       FD  CDCARDF.
           COPY CDCARD02.

       WORKING-STORAGE SECTION.
       01  FS-CDLIMF                  PIC XX VALUE SPACE.
       01  FS-CDCARDF                 PIC XX VALUE SPACE.
       01  WS-END-FLG                 PIC X VALUE SPACE.
           88  END-OF-CDLIMF          VALUE '1'.
       01  WS-HARD-ERR-FLG            PIC X VALUE SPACE.
           88  HARD-ERROR             VALUE '1'.
       01  WS-SYS-DATE                PIC 9(8) VALUE ZERO.
       01  WS-CURR-DATE               PIC X(21) VALUE SPACE.
       01  WS-READ-CNT                PIC 9(9) VALUE ZERO.
       01  WS-UPD-CNT                 PIC 9(9) VALUE ZERO.
       01  WS-SKIP-CNT                PIC 9(9) VALUE ZERO.
       01  WS-ERR-CNT                 PIC 9(9) VALUE ZERO.
       01  WS-NOTIFY-CNT              PIC 9(9) VALUE ZERO.
       01  WS-REASON-CD               PIC X(3) VALUE SPACE.
           88  RSN-CARD-STATUS        VALUE 'STS'.
       01  WS-BASE-CURRENCY           PIC X(3) VALUE 'JPY'.
       01  WS-AUTH-STATUS             PIC X(2) VALUE '01'.
       01  WS-ST-BEFORE               PIC X(2) VALUE '01'.
       01  WS-ST-ACTIVE               PIC X(2) VALUE '02'.
       01  WS-ST-APPROVING            PIC X(2) VALUE '03'.
       01  WS-ST-CANCEL               PIC X(2) VALUE '04'.
       01  WS-ST-EXPIRED              PIC X(2) VALUE '09'.

       PROCEDURE DIVISION.
       MAIN-RTN.
           MOVE 0 TO RETURN-CODE
           PERFORM INIT-RTN
           IF NOT HARD-ERROR
               PERFORM PROCESS-RTN
           END-IF
           PERFORM FINAL-RTN
           GOBACK.

       INIT-RTN.
           MOVE FUNCTION CURRENT-DATE TO WS-CURR-DATE
           MOVE WS-CURR-DATE(1:8) TO WS-SYS-DATE

           OPEN I-O CDLIMF
           IF FS-CDLIMF NOT = '00'
               DISPLAY 'CDLIMF OPEN ERR ST=' FS-CDLIMF
               MOVE '1' TO WS-HARD-ERR-FLG
               MOVE 12 TO RETURN-CODE
           END-IF

           IF NOT HARD-ERROR
               OPEN INPUT CDCARDF
               IF FS-CDCARDF NOT = '00'
                   DISPLAY 'CDCARDF OPEN ERR ST=' FS-CDCARDF
                   MOVE '1' TO WS-HARD-ERR-FLG
                   MOVE 12 TO RETURN-CODE
               END-IF
           END-IF.

       PROCESS-RTN.
           MOVE LOW-VALUE TO LM-CARD-NO
           START CDLIMF KEY IS >= LM-CARD-NO
               INVALID KEY
                   IF FS-CDLIMF = '23'
                       SET END-OF-CDLIMF TO TRUE
                   ELSE
                       DISPLAY 'CDLIMF START ERR ST=' FS-CDLIMF
                       MOVE '1' TO WS-HARD-ERR-FLG
                       MOVE 12 TO RETURN-CODE
                   END-IF
           END-START

           PERFORM UNTIL END-OF-CDLIMF OR HARD-ERROR
               READ CDLIMF NEXT RECORD
                   AT END
                       SET END-OF-CDLIMF TO TRUE
                   NOT AT END
                       IF FS-CDLIMF = '00'
                           ADD 1 TO WS-READ-CNT
                           PERFORM EDIT-LIMIT-RTN
                       ELSE
                           DISPLAY 'CDLIMF READ ERR ST=' FS-CDLIMF
                           MOVE '1' TO WS-HARD-ERR-FLG
                           MOVE 12 TO RETURN-CODE
                       END-IF
               END-READ
           END-PERFORM.

       EDIT-LIMIT-RTN.
           MOVE LM-CARD-NO TO CF-CARD-NO
           READ CDCARDF
               INVALID KEY
                   DISPLAY 'NO CARD MASTER CARD=' LM-CARD-NO
                   ADD 1 TO WS-ERR-CNT
                   ADD 1 TO WS-SKIP-CNT
               NOT INVALID KEY
                   IF FS-CDCARDF = '00'
                       PERFORM JUDGE-LIMIT-RTN
                   ELSE
                       DISPLAY 'CDCARDF READ ERR ST=' FS-CDCARDF
                       MOVE '1' TO WS-HARD-ERR-FLG
                       MOVE 12 TO RETURN-CODE
                   END-IF
           END-READ.

       JUDGE-LIMIT-RTN.
           EVALUATE TRUE
               WHEN LM-START-DT = ZERO OR LM-END-DT = ZERO
                   DISPLAY 'LIMIT DATE ERR CARD=' LM-CARD-NO
                   ADD 1 TO WS-ERR-CNT
                   ADD 1 TO WS-SKIP-CNT
               WHEN LM-START-DT > LM-END-DT
                   DISPLAY 'LIMIT DATE REVERSE CARD=' LM-CARD-NO
                   ADD 1 TO WS-ERR-CNT
                   ADD 1 TO WS-SKIP-CNT
               WHEN CF-CARD-STATUS NOT = WS-AUTH-STATUS
                   SET RSN-CARD-STATUS TO TRUE
                   PERFORM WRITE-SKIP-LOG-RTN
               WHEN LM-STATUS = WS-ST-ACTIVE
                   PERFORM ACTIVE-LIMIT-RTN
               WHEN LM-STATUS = WS-ST-BEFORE
                   PERFORM BEFORE-LIMIT-RTN
               WHEN LM-STATUS = WS-ST-APPROVING
                   PERFORM APPROVING-LIMIT-RTN
               WHEN LM-STATUS = WS-ST-CANCEL
                   PERFORM CANCEL-LIMIT-RTN
               WHEN LM-STATUS = WS-ST-EXPIRED
                   ADD 1 TO WS-SKIP-CNT
               WHEN OTHER
                   DISPLAY 'LIMIT STATUS ERR CARD=' LM-CARD-NO
                   ADD 1 TO WS-ERR-CNT
                   ADD 1 TO WS-SKIP-CNT
           END-EVALUATE.

       ACTIVE-LIMIT-RTN.
           IF LM-END-DT < WS-SYS-DATE
               MOVE WS-ST-EXPIRED TO LM-STATUS
               PERFORM UPDATE-LIMIT-RTN
               IF NOT HARD-ERROR
                   PERFORM NOTIFY-RTN
               END-IF
           ELSE
               ADD 1 TO WS-SKIP-CNT
           END-IF.

       BEFORE-LIMIT-RTN.
           IF LM-END-DT < WS-SYS-DATE
               MOVE WS-ST-EXPIRED TO LM-STATUS
               PERFORM UPDATE-LIMIT-RTN
               IF NOT HARD-ERROR
                   PERFORM NOTIFY-RTN
               END-IF
           ELSE
               IF LM-START-DT <= WS-SYS-DATE
                   IF LM-APPROVAL-ID NOT = SPACE
                       MOVE WS-ST-ACTIVE TO LM-STATUS
                       PERFORM UPDATE-LIMIT-RTN
                   ELSE
                       DISPLAY 'NO APPROVAL CARD=' LM-CARD-NO
                       ADD 1 TO WS-ERR-CNT
                       ADD 1 TO WS-SKIP-CNT
                   END-IF
               ELSE
                   ADD 1 TO WS-SKIP-CNT
               END-IF
           END-IF.

       APPROVING-LIMIT-RTN.
           IF LM-END-DT < WS-SYS-DATE
               MOVE WS-ST-EXPIRED TO LM-STATUS
               PERFORM UPDATE-LIMIT-RTN
               IF NOT HARD-ERROR
                   PERFORM NOTIFY-RTN
               END-IF
           ELSE
               IF LM-APPROVAL-ID NOT = SPACE
                   MOVE WS-ST-ACTIVE TO LM-STATUS
                   PERFORM UPDATE-LIMIT-RTN
               ELSE
                   ADD 1 TO WS-SKIP-CNT
               END-IF
           END-IF.

       CANCEL-LIMIT-RTN.
           IF LM-END-DT < WS-SYS-DATE
               DISPLAY 'CANCEL EXPIRED CARD=' LM-CARD-NO
           END-IF
           ADD 1 TO WS-SKIP-CNT.

       WRITE-SKIP-LOG-RTN.
           DISPLAY 'SKIP CARD=' LM-CARD-NO
                   ' ST=' CF-CARD-STATUS
                   ' RSN=' WS-REASON-CD
           ADD 1 TO WS-ERR-CNT
           ADD 1 TO WS-SKIP-CNT.

       UPDATE-LIMIT-RTN.
           REWRITE CDLIMF-REC
               INVALID KEY
                   DISPLAY 'CDLIMF UPDATE ERR CARD=' LM-CARD-NO
                           ' ST=' FS-CDLIMF
                   MOVE '1' TO WS-HARD-ERR-FLG
                   MOVE 12 TO RETURN-CODE
               NOT INVALID KEY
                   IF FS-CDLIMF = '00'
                       ADD 1 TO WS-UPD-CNT
                   ELSE
                       DISPLAY 'CDLIMF UPDATE NG CARD=' LM-CARD-NO
                               ' ST=' FS-CDLIMF
                       MOVE '1' TO WS-HARD-ERR-FLG
                       MOVE 12 TO RETURN-CODE
                   END-IF
           END-REWRITE.

       NOTIFY-RTN.
           DISPLAY '増枠期限切れ通知 CARD=' LM-CARD-NO
                   ' MEMBER=' CF-MEMBER-ID
                   ' CUR=' WS-BASE-CURRENCY
           ADD 1 TO WS-NOTIFY-CNT.

       FINAL-RTN.
           IF FS-CDCARDF = '00'
               CLOSE CDCARDF
               IF FS-CDCARDF NOT = '00'
                   DISPLAY 'CDCARDF CLOSE ERR ST=' FS-CDCARDF
                   MOVE 8 TO RETURN-CODE
               END-IF
           END-IF

           IF FS-CDLIMF = '00'
               CLOSE CDLIMF
               IF FS-CDLIMF NOT = '00'
                   DISPLAY 'CDLIMF CLOSE ERR ST=' FS-CDLIMF
                   MOVE 8 TO RETURN-CODE
               END-IF
           END-IF

           DISPLAY 'CB218B END READ=' WS-READ-CNT
                   ' UPD=' WS-UPD-CNT
                   ' NOTICE=' WS-NOTIFY-CNT
                   ' SKIP=' WS-SKIP-CNT
                   ' ERR=' WS-ERR-CNT

           IF HARD-ERROR
               IF RETURN-CODE = 0
                   MOVE 12 TO RETURN-CODE
               END-IF
           END-IF.
