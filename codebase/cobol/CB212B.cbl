       IDENTIFICATION DIVISION.
       PROGRAM-ID. CB212B.
      *
      * 日次ベロシティ集計バッチ
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CDAUTHF ASSIGN TO "CDAUTHF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-CDAUTHF-ST.
           SELECT CDVELF ASSIGN TO "CDVELF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS VL-CARD-NO
               FILE STATUS IS WS-CDVELF-ST.

       DATA DIVISION.
       FILE SECTION.
       FD  CDAUTHF.
           COPY CDAUTHFC.
       FD  CDVELF.
           COPY CDVELC.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05 WS-CDAUTHF-ST          PIC XX VALUE SPACES.
           05 WS-CDVELF-ST           PIC XX VALUE SPACES.

       01  WS-CONTROL.
           05 WS-EOF-SW              PIC X VALUE 'N'.
              88 WS-EOF                    VALUE 'Y'.
              88 WS-NOT-EOF                VALUE 'N'.
           05 WS-ABEND-SW            PIC X VALUE 'N'.
              88 WS-ABEND                  VALUE 'Y'.
              88 WS-NORMAL                 VALUE 'N'.
           05 WS-FIRST-SW            PIC X VALUE 'Y'.
              88 WS-FIRST-REC              VALUE 'Y'.
              88 WS-NOT-FIRST-REC          VALUE 'N'.
           05 WS-VEL-FOUND-SW        PIC X VALUE 'N'.
              88 WS-VEL-FOUND              VALUE 'Y'.
              88 WS-VEL-NOT-FOUND          VALUE 'N'.
           05 WS-CDAUTHF-OPEN-SW     PIC X VALUE 'N'.
              88 WS-CDAUTHF-OPEN           VALUE 'Y'.
              88 WS-CDAUTHF-CLOSED         VALUE 'N'.
           05 WS-CDVELF-OPEN-SW      PIC X VALUE 'N'.
              88 WS-CDVELF-OPEN            VALUE 'Y'.
              88 WS-CDVELF-CLOSED          VALUE 'N'.

       01  WS-KEY-AREA.
           05 WS-CUR-CARD-NO         PIC X(19) VALUE SPACES.
           05 WS-PREV-CARD-NO        PIC X(19) VALUE SPACES.
           05 WS-PREV-AUTH-TS        PIC X(14) VALUE SPACES.

       01  WS-COUNT-AREA.
           05 WS-READ-CNT            PIC 9(9) VALUE ZERO.
           05 WS-APPROVED-CNT        PIC 9(9) VALUE ZERO.
           05 WS-UPDATE-CNT          PIC 9(9) VALUE ZERO.

       01  WS-ROLLING-AREA.
           05 WS-ROLL-COUNT-10M      PIC 9(5) VALUE ZERO.
           05 WS-ROLL-AMT-1H         PIC S9(13)V99 VALUE ZERO.
           05 WS-CUR-AUTH-SEC        PIC 9(6) VALUE ZERO.
           05 WS-OLD-AUTH-SEC        PIC 9(6) VALUE ZERO.
           05 WS-IX                  PIC 9(3) VALUE ZERO.
           05 WS-KEEP-IX             PIC 9(3) VALUE ZERO.
           05 WS-ENTRY-CNT           PIC 9(3) VALUE ZERO.

       01  WS-THRESHOLD.
           05 WS-THRESH-COUNT-10M    PIC 9(5) VALUE 5.
           05 WS-THRESH-AMT-1H       PIC S9(13)V99 VALUE 300000.

       01  WS-TIME-PARTS.
           05 WS-HH                  PIC 99 VALUE ZERO.
           05 WS-MM                  PIC 99 VALUE ZERO.
           05 WS-SS                  PIC 99 VALUE ZERO.

       01  WS-EDIT-AREA.
           05 WS-EDIT-COUNT-10M      PIC 9(10) VALUE ZERO.
           05 WS-EDIT-AMT-1H         PIC 9(10) VALUE ZERO.

       01  WS-ROLLING-TABLE.
           05 WS-AUTH-ENTRY OCCURS 120 TIMES.
              10 WS-ENT-TS           PIC X(14) VALUE SPACES.
              10 WS-ENT-SEC          PIC 9(6) VALUE ZERO.
              10 WS-ENT-AMT          PIC S9(13)V99 VALUE ZERO.

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE 0 TO RETURN-CODE
           PERFORM 1000-OPEN-FILES
           IF WS-NORMAL
               PERFORM 2000-READ-AUTH
               PERFORM UNTIL WS-EOF OR WS-ABEND
                   PERFORM 3000-PROCESS-AUTH
                   IF WS-NORMAL
                       PERFORM 2000-READ-AUTH
                   END-IF
               END-PERFORM
           END-IF
           PERFORM 9000-CLOSE-FILES
           IF WS-ABEND
               MOVE 12 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
               DISPLAY 'CB212B NORMAL END READ=' WS-READ-CNT
               DISPLAY 'APPROVED=' WS-APPROVED-CNT
                       ' UPDATE=' WS-UPDATE-CNT
           END-IF
           GOBACK.

       1000-OPEN-FILES.
           SET WS-NORMAL TO TRUE
           SET WS-NOT-EOF TO TRUE
           OPEN INPUT CDAUTHF
           IF WS-CDAUTHF-ST = '00'
               SET WS-CDAUTHF-OPEN TO TRUE
           ELSE
               DISPLAY 'CDAUTHF OPEN ERROR ST=' WS-CDAUTHF-ST
               SET WS-ABEND TO TRUE
           END-IF
           IF WS-NORMAL
               OPEN I-O CDVELF
               IF WS-CDVELF-ST = '00'
                   SET WS-CDVELF-OPEN TO TRUE
               ELSE
                   DISPLAY 'CDVELF OPEN ERROR ST=' WS-CDVELF-ST
                   SET WS-ABEND TO TRUE
               END-IF
           END-IF.

       2000-READ-AUTH.
           READ CDAUTHF
               AT END
                   SET WS-EOF TO TRUE
               NOT AT END
                   ADD 1 TO WS-READ-CNT
           END-READ
           IF WS-CDAUTHF-ST NOT = '00' AND
              WS-CDAUTHF-ST NOT = '10'
               DISPLAY 'CDAUTHF READ ERROR ST=' WS-CDAUTHF-ST
               SET WS-ABEND TO TRUE
           END-IF.

       3000-PROCESS-AUTH.
           PERFORM 3100-VALIDATE-ORDER
           IF WS-NORMAL
               MOVE AU-CARD-NO TO WS-PREV-CARD-NO
               MOVE AU-AUTH-TS TO WS-PREV-AUTH-TS
               SET WS-NOT-FIRST-REC TO TRUE
               IF AU-AUTH-RESULT = '00'
                   PERFORM 3200-RESET-ON-NEW-CARD
                   PERFORM 3300-CALC-AUTH-SECOND
                   PERFORM 3400-ADD-ROLLING
                   PERFORM 3500-TRIM-ROLLING
                   ADD 1 TO WS-APPROVED-CNT
                   IF WS-ROLL-COUNT-10M > WS-THRESH-COUNT-10M OR
                      WS-ROLL-AMT-1H > WS-THRESH-AMT-1H
                       PERFORM 4000-UPDATE-VELOCITY
                   END-IF
               END-IF
           END-IF.

       3100-VALIDATE-ORDER.
           IF WS-FIRST-REC
               CONTINUE
           ELSE
               IF AU-CARD-NO < WS-PREV-CARD-NO
                   DISPLAY 'CDAUTHF SEQUENCE ERROR'
                   DISPLAY 'CARD=' AU-CARD-NO
                   SET WS-ABEND TO TRUE
               ELSE
                   IF AU-CARD-NO = WS-PREV-CARD-NO AND
                      AU-AUTH-TS < WS-PREV-AUTH-TS
                       DISPLAY 'CDAUTHF TIME ORDER ERROR'
                       DISPLAY 'CARD=' AU-CARD-NO
                       SET WS-ABEND TO TRUE
                   END-IF
               END-IF
           END-IF.

       3200-RESET-ON-NEW-CARD.
           IF AU-CARD-NO NOT = WS-CUR-CARD-NO
               MOVE AU-CARD-NO TO WS-CUR-CARD-NO
               MOVE ZERO TO WS-ROLL-COUNT-10M
               MOVE ZERO TO WS-ROLL-AMT-1H
               MOVE ZERO TO WS-ENTRY-CNT
               PERFORM VARYING WS-IX FROM 1 BY 1
                   UNTIL WS-IX > 120
                   MOVE SPACES TO WS-ENT-TS(WS-IX)
                   MOVE ZERO TO WS-ENT-SEC(WS-IX)
                   MOVE ZERO TO WS-ENT-AMT(WS-IX)
               END-PERFORM
           END-IF.

       3300-CALC-AUTH-SECOND.
           MOVE AU-AUTH-TS(9:2) TO WS-HH
           MOVE AU-AUTH-TS(11:2) TO WS-MM
           MOVE AU-AUTH-TS(13:2) TO WS-SS
           COMPUTE WS-CUR-AUTH-SEC =
               (WS-HH * 3600) + (WS-MM * 60) + WS-SS.

       3400-ADD-ROLLING.
           IF WS-ENTRY-CNT < 120
               ADD 1 TO WS-ENTRY-CNT
           ELSE
               PERFORM 3450-DROP-OLDEST
           END-IF
           MOVE AU-AUTH-TS TO WS-ENT-TS(WS-ENTRY-CNT)
           MOVE WS-CUR-AUTH-SEC TO WS-ENT-SEC(WS-ENTRY-CNT)
           MOVE AU-AUTH-AMT TO WS-ENT-AMT(WS-ENTRY-CNT).

       3450-DROP-OLDEST.
           PERFORM VARYING WS-IX FROM 1 BY 1
               UNTIL WS-IX >= 120
               COMPUTE WS-KEEP-IX = WS-IX + 1
               MOVE WS-ENT-TS(WS-KEEP-IX) TO WS-ENT-TS(WS-IX)
               MOVE WS-ENT-SEC(WS-KEEP-IX) TO WS-ENT-SEC(WS-IX)
               MOVE WS-ENT-AMT(WS-KEEP-IX) TO WS-ENT-AMT(WS-IX)
           END-PERFORM.

       3500-TRIM-ROLLING.
           MOVE ZERO TO WS-ROLL-COUNT-10M
           MOVE ZERO TO WS-ROLL-AMT-1H
           PERFORM VARYING WS-IX FROM 1 BY 1
               UNTIL WS-IX > WS-ENTRY-CNT
               MOVE WS-ENT-SEC(WS-IX) TO WS-OLD-AUTH-SEC
               IF WS-CUR-AUTH-SEC >= WS-OLD-AUTH-SEC
                   IF WS-CUR-AUTH-SEC - WS-OLD-AUTH-SEC <= 3600
                       ADD WS-ENT-AMT(WS-IX) TO WS-ROLL-AMT-1H
                   END-IF
                   IF WS-CUR-AUTH-SEC - WS-OLD-AUTH-SEC <= 600
                       ADD 1 TO WS-ROLL-COUNT-10M
                   END-IF
               END-IF
           END-PERFORM.

       4000-UPDATE-VELOCITY.
           MOVE AU-CARD-NO TO VL-CARD-NO
           SET WS-VEL-NOT-FOUND TO TRUE
           READ CDVELF KEY IS VL-CARD-NO
               INVALID KEY
                   SET WS-VEL-NOT-FOUND TO TRUE
               NOT INVALID KEY
                   SET WS-VEL-FOUND TO TRUE
           END-READ
           IF WS-CDVELF-ST NOT = '00' AND
              WS-CDVELF-ST NOT = '23'
               DISPLAY 'CDVELF READ ERROR ST=' WS-CDVELF-ST
               DISPLAY 'CARD=' AU-CARD-NO
               SET WS-ABEND TO TRUE
           ELSE
               PERFORM 4100-SET-VELOCITY-REC
               IF WS-VEL-FOUND
                   REWRITE CDVELF-REC
                   IF WS-CDVELF-ST NOT = '00'
                       DISPLAY 'CDVELF REWRITE ERROR ST=' WS-CDVELF-ST
                       DISPLAY 'CARD=' AU-CARD-NO
                       SET WS-ABEND TO TRUE
                   ELSE
                       ADD 1 TO WS-UPDATE-CNT
                   END-IF
               ELSE
                   WRITE CDVELF-REC
                   IF WS-CDVELF-ST NOT = '00'
                       DISPLAY 'CDVELF WRITE ERROR ST=' WS-CDVELF-ST
                       DISPLAY 'CARD=' AU-CARD-NO
                       SET WS-ABEND TO TRUE
                   ELSE
                       ADD 1 TO WS-UPDATE-CNT
                   END-IF
               END-IF
           END-IF.

       4100-SET-VELOCITY-REC.
           MOVE AU-CARD-NO TO VL-CARD-NO
           MOVE AU-AUTH-TS TO VL-WINDOW-START-TS
           MOVE WS-ROLL-COUNT-10M TO WS-EDIT-COUNT-10M
           MOVE WS-EDIT-COUNT-10M TO VL-AUTH-COUNT-10M
           MOVE WS-ROLL-AMT-1H TO WS-EDIT-AMT-1H
           MOVE WS-EDIT-AMT-1H TO VL-AUTH-AMT-1H
           MOVE AU-AUTH-TS TO VL-LAST-AUTH-TS
           MOVE '1' TO VL-VELOCITY-FLAG.

       9000-CLOSE-FILES.
           IF WS-CDAUTHF-OPEN
               CLOSE CDAUTHF
               SET WS-CDAUTHF-CLOSED TO TRUE
               IF WS-CDAUTHF-ST NOT = '00'
                   DISPLAY 'CDAUTHF CLOSE ERROR ST=' WS-CDAUTHF-ST
                   SET WS-ABEND TO TRUE
               END-IF
           END-IF
           IF WS-CDVELF-OPEN
               CLOSE CDVELF
               SET WS-CDVELF-CLOSED TO TRUE
               IF WS-CDVELF-ST NOT = '00'
                   DISPLAY 'CDVELF CLOSE ERROR ST=' WS-CDVELF-ST
                   SET WS-ABEND TO TRUE
               END-IF
           END-IF.
