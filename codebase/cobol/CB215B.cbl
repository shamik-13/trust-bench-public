       IDENTIFICATION DIVISION.
       PROGRAM-ID. CB215B.
       AUTHOR. CARD-BATCH.
      * カード状態一括反映バッチ
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CDSTSF ASSIGN TO "CDSTSF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-ST-FS.
           SELECT CDCARDF ASSIGN TO "CDCARDF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS CF-CARD-NO
               FILE STATUS IS WS-CF-FS.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  CDSTSF.
           COPY CDSTSC.
       FD  CDCARDF.
           COPY CDCARD02.
      *
       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05  WS-ST-FS                 PIC XX.
           05  WS-CF-FS                 PIC XX.
      *
       01  WS-FLAGS.
           05  WS-EOF-SW                PIC X VALUE "N".
               88  ST-EOF                     VALUE "Y".
           05  WS-ABEND-SW              PIC X VALUE "N".
               88  ABEND-ON                   VALUE "Y".
           05  WS-FOUND-SW              PIC X VALUE "N".
               88  ENTRY-FOUND                VALUE "Y".
           05  WS-VALID-SW              PIC X VALUE "N".
               88  ST-VALID                   VALUE "Y".
           05  WS-APPLY-SW              PIC X VALUE "N".
               88  APPLY-OK                   VALUE "Y".
      *
       01  WS-COUNTERS.
           05  WS-READ-CNT              PIC 9(9) VALUE ZERO.
           05  WS-UPDATE-CNT            PIC 9(9) VALUE ZERO.
           05  WS-SKIP-CNT              PIC 9(9) VALUE ZERO.
           05  WS-AUDIT-CNT             PIC 9(9) VALUE ZERO.
           05  WS-ERROR-CNT             PIC 9(9) VALUE ZERO.
           05  WS-TABLE-CNT             PIC 9(5) VALUE ZERO.
           05  WS-IDX                   PIC 9(5) VALUE ZERO.
           05  WS-FREE-IDX              PIC 9(5) VALUE ZERO.
      *
       01  WS-WORK.
           05  WS-IN-STATUS             PIC X(2).
           05  WS-IN-PRIORITY           PIC 9 VALUE ZERO.
           05  WS-DECISION              PIC X VALUE SPACE.
           05  WS-APPLY-STATUS          PIC X(2) VALUE SPACE.
           05  WS-MSG-CARD              PIC X(19) VALUE SPACE.
           05  WS-MSG-TS                PIC X(14) VALUE SPACE.
      *
       01  WS-LATEST-TABLE.
           05  WS-LATEST-ENTRY OCCURS 5000 TIMES.
               10  LT-CARD-NO           PIC X(19).
               10  LT-STATUS            PIC X(2).
               10  LT-STATUS-TS         PIC X(14).
               10  LT-REASON-CD         PIC X(4).
               10  LT-PRIORITY          PIC 9.
      *
       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE 0 TO RETURN-CODE
           PERFORM 1000-OPEN-FILES
           IF ABEND-ON
               PERFORM 9000-ABEND-END
               GOBACK
           END-IF
      *
           PERFORM 2000-READ-CDSTSF
           PERFORM UNTIL ST-EOF OR ABEND-ON
               ADD 1 TO WS-READ-CNT
               PERFORM 3000-VALIDATE-STATUS
               IF ST-VALID
                   PERFORM 4000-STORE-LATEST
               ELSE
                   ADD 1 TO WS-SKIP-CNT
               END-IF
               PERFORM 2000-READ-CDSTSF
           END-PERFORM
      *
           IF NOT ABEND-ON
               PERFORM 6000-APPLY-CARDF
           END-IF
      *
           PERFORM 8000-CLOSE-FILES
           IF ABEND-ON
               PERFORM 9000-ABEND-END
           ELSE
               PERFORM 9100-NORMAL-END
           END-IF
           GOBACK.
      *
       1000-OPEN-FILES.
           OPEN INPUT CDSTSF
           IF WS-ST-FS NOT = "00"
               DISPLAY "CDSTSF OPEN ERROR ST=" WS-ST-FS
               SET ABEND-ON TO TRUE
           END-IF
      *
           OPEN I-O CDCARDF
           IF WS-CF-FS NOT = "00"
               DISPLAY "CDCARDF OPEN ERROR ST=" WS-CF-FS
               SET ABEND-ON TO TRUE
           END-IF.
      *
       2000-READ-CDSTSF.
           READ CDSTSF
               AT END
                   SET ST-EOF TO TRUE
               NOT AT END
                   IF WS-ST-FS NOT = "00"
                       DISPLAY "CDSTSF READ ERROR ST=" WS-ST-FS
                       SET ABEND-ON TO TRUE
                   END-IF
           END-READ.
      *
       3000-VALIDATE-STATUS.
           MOVE "N" TO WS-VALID-SW
           IF ST-CARD-NO = SPACE
               DISPLAY "STATUS SKIP BLANK CARD"
               EXIT PARAGRAPH
           END-IF
      *
           IF ST-STATUS-TS = SPACE OR ST-STATUS-TS NOT NUMERIC
               MOVE ST-CARD-NO TO WS-MSG-CARD
               DISPLAY "BAD STATUS TS CARD=" WS-MSG-CARD
               EXIT PARAGRAPH
           END-IF
      *
           IF ST-SOURCE-SYS = SPACE
               MOVE ST-CARD-NO TO WS-MSG-CARD
               DISPLAY "BLANK SOURCE CARD=" WS-MSG-CARD
               EXIT PARAGRAPH
           END-IF
      *
           PERFORM 5000-DECIDE-STATUS
           IF WS-DECISION = "A"
               SET ST-VALID TO TRUE
           ELSE
               MOVE ST-CARD-NO TO WS-MSG-CARD
               DISPLAY "STATUS NOT TARGET CARD=" WS-MSG-CARD
               DISPLAY "REASON=" ST-REASON-CD
           END-IF.
      *
       4000-STORE-LATEST.
           MOVE "N" TO WS-FOUND-SW
           MOVE ZERO TO WS-IDX
           PERFORM VARYING WS-IDX FROM 1 BY 1
               UNTIL WS-IDX > WS-TABLE-CNT OR ENTRY-FOUND
               IF LT-CARD-NO(WS-IDX) = ST-CARD-NO
                   SET ENTRY-FOUND TO TRUE
               END-IF
           END-PERFORM
      *
           IF ENTRY-FOUND
               PERFORM 4100-COMPARE-LATEST
           ELSE
               IF WS-TABLE-CNT >= 5000
                   DISPLAY "LATEST TABLE FULL"
                   SET ABEND-ON TO TRUE
               ELSE
                   ADD 1 TO WS-TABLE-CNT
                   MOVE WS-TABLE-CNT TO WS-FREE-IDX
                   MOVE ST-CARD-NO TO LT-CARD-NO(WS-FREE-IDX)
                   MOVE WS-IN-STATUS TO LT-STATUS(WS-FREE-IDX)
                   MOVE ST-STATUS-TS TO LT-STATUS-TS(WS-FREE-IDX)
                   MOVE ST-REASON-CD TO LT-REASON-CD(WS-FREE-IDX)
                   MOVE WS-IN-PRIORITY TO LT-PRIORITY(WS-FREE-IDX)
               END-IF
           END-IF.
      *
       4100-COMPARE-LATEST.
           IF ST-STATUS-TS > LT-STATUS-TS(WS-IDX)
               MOVE WS-IN-STATUS TO LT-STATUS(WS-IDX)
               MOVE ST-STATUS-TS TO LT-STATUS-TS(WS-IDX)
               MOVE ST-REASON-CD TO LT-REASON-CD(WS-IDX)
               MOVE WS-IN-PRIORITY TO LT-PRIORITY(WS-IDX)
           ELSE
               IF ST-STATUS-TS = LT-STATUS-TS(WS-IDX)
                   IF WS-IN-PRIORITY > LT-PRIORITY(WS-IDX)
                       MOVE WS-IN-STATUS TO LT-STATUS(WS-IDX)
                       MOVE ST-REASON-CD TO LT-REASON-CD(WS-IDX)
                       MOVE WS-IN-PRIORITY TO LT-PRIORITY(WS-IDX)
                   ELSE
                       ADD 1 TO WS-AUDIT-CNT
                       MOVE ST-CARD-NO TO WS-MSG-CARD
                       MOVE ST-STATUS-TS TO WS-MSG-TS
                       DISPLAY "SAME TS LOW PRIORITY CARD="
                               WS-MSG-CARD
                       DISPLAY "TS=" WS-MSG-TS
                   END-IF
               ELSE
                   ADD 1 TO WS-AUDIT-CNT
                   MOVE ST-CARD-NO TO WS-MSG-CARD
                   MOVE ST-STATUS-TS TO WS-MSG-TS
                   DISPLAY "OLD STATUS AUDIT CARD=" WS-MSG-CARD
                   DISPLAY "TS=" WS-MSG-TS
               END-IF
           END-IF.
      *
       5000-DECIDE-STATUS.
           MOVE "D" TO WS-DECISION
           MOVE SPACE TO WS-IN-STATUS
           MOVE ZERO TO WS-IN-PRIORITY
      *
           EVALUATE TRUE
               WHEN ST-NEW-STATUS = "03"
                   MOVE "03" TO WS-IN-STATUS
                   MOVE 3 TO WS-IN-PRIORITY
                   MOVE "A" TO WS-DECISION
               WHEN ST-NEW-STATUS = "02"
                   MOVE "02" TO WS-IN-STATUS
                   MOVE 2 TO WS-IN-PRIORITY
                   MOVE "A" TO WS-DECISION
               WHEN ST-NEW-STATUS = "01"
                   IF ST-REASON-CD = "REAC"
                       MOVE "01" TO WS-IN-STATUS
                       MOVE 1 TO WS-IN-PRIORITY
                       MOVE "A" TO WS-DECISION
                   END-IF
               WHEN OTHER
                   MOVE "D" TO WS-DECISION
           END-EVALUATE
      *
           IF ST-REASON-CD = "FRCN"
               MOVE "03" TO WS-IN-STATUS
               MOVE 4 TO WS-IN-PRIORITY
               MOVE "A" TO WS-DECISION
           END-IF
      *
           IF ST-REASON-CD = "LOST" OR ST-REASON-CD = "STLN"
               MOVE "02" TO WS-IN-STATUS
               MOVE 5 TO WS-IN-PRIORITY
               MOVE "A" TO WS-DECISION
           END-IF.
      *
       6000-APPLY-CARDF.
           PERFORM VARYING WS-IDX FROM 1 BY 1
               UNTIL WS-IDX > WS-TABLE-CNT OR ABEND-ON
               MOVE LT-CARD-NO(WS-IDX) TO CF-CARD-NO
               READ CDCARDF
                   INVALID KEY
                       ADD 1 TO WS-ERROR-CNT
                       DISPLAY "CDCARDF NOT FOUND CARD=" CF-CARD-NO
                   NOT INVALID KEY
                       IF WS-CF-FS = "00"
                           PERFORM 6100-REWRITE-CARDF
                       ELSE
                           DISPLAY "CDCARDF READ ERROR ST=" WS-CF-FS
                           SET ABEND-ON TO TRUE
                       END-IF
               END-READ
           END-PERFORM.
      *
       6100-REWRITE-CARDF.
           MOVE "N" TO WS-APPLY-SW
           MOVE LT-STATUS(WS-IDX) TO WS-APPLY-STATUS
      *
           IF CF-CARD-STATUS = WS-APPLY-STATUS
               ADD 1 TO WS-SKIP-CNT
           ELSE
               SET APPLY-OK TO TRUE
           END-IF
      *
           IF APPLY-OK
               MOVE WS-APPLY-STATUS TO CF-CARD-STATUS
               REWRITE CDCARDF-REC
               IF WS-CF-FS = "00"
                   ADD 1 TO WS-UPDATE-CNT
               ELSE
                   DISPLAY "CDCARDF REWRITE ERROR ST=" WS-CF-FS
                   DISPLAY "CARD=" CF-CARD-NO
                   SET ABEND-ON TO TRUE
               END-IF
           END-IF.
      *
       8000-CLOSE-FILES.
           CLOSE CDSTSF
           IF WS-ST-FS NOT = "00"
               DISPLAY "CDSTSF CLOSE ERROR ST=" WS-ST-FS
               SET ABEND-ON TO TRUE
           END-IF
      *
           CLOSE CDCARDF
           IF WS-CF-FS NOT = "00"
               DISPLAY "CDCARDF CLOSE ERROR ST=" WS-CF-FS
               SET ABEND-ON TO TRUE
           END-IF.
      *
       9000-ABEND-END.
           MOVE 12 TO RETURN-CODE
           DISPLAY "CB215B ABEND"
           DISPLAY "READ COUNT=" WS-READ-CNT
           DISPLAY "UPDATE COUNT=" WS-UPDATE-CNT
           DISPLAY "AUDIT COUNT=" WS-AUDIT-CNT
           DISPLAY "ERROR COUNT=" WS-ERROR-CNT.
      *
       9100-NORMAL-END.
           MOVE 0 TO RETURN-CODE
           DISPLAY "CB215B NORMAL END"
           DISPLAY "READ COUNT=" WS-READ-CNT
           DISPLAY "UPDATE COUNT=" WS-UPDATE-CNT
           DISPLAY "SKIP COUNT=" WS-SKIP-CNT
           DISPLAY "AUDIT COUNT=" WS-AUDIT-CNT
           DISPLAY "ERROR COUNT=" WS-ERROR-CNT.
