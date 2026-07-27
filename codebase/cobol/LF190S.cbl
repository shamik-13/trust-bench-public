       IDENTIFICATION DIVISION.
       PROGRAM-ID. LF190S.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LFPOLF
               ASSIGN TO "LFPOLF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-LFPOLF-ST.

       DATA DIVISION.
       FILE SECTION.
       FD  LFPOLF.
       COPY LFPOLFC.

       WORKING-STORAGE SECTION.
       01  WS-LFPOLF-ST              PIC X(02) VALUE SPACE.
       01  WS-EOF-SW                 PIC X(01) VALUE 'N'.
           88  WS-EOF                          VALUE 'Y'.
           88  WS-NOT-EOF                      VALUE 'N'.

       01  WS-WORK.
           05  WS-HIT-SW             PIC X(01) VALUE 'N'.
               88  WS-HIT                      VALUE 'Y'.
               88  WS-NOT-HIT                  VALUE 'N'.
           05  WS-DISPLAY-AGE        PIC ZZZ9.
           05  WS-DISPLAY-ST         PIC X(02).

       LINKAGE SECTION.
       01  LK-LF190S-PARM.
           05  LK-POL-NO             PIC X(12).
           05  LK-AGE-BAND-KBN       PIC X(02).
           05  LK-RESULT-CD          PIC X(02).
           05  LK-REASON-CD          PIC X(04).

       PROCEDURE DIVISION USING LK-LF190S-PARM.
       0000-MAIN.
           PERFORM 1000-INIT
           PERFORM 2000-PROCESS
           PERFORM 9000-FINAL
           GOBACK.

       1000-INIT.
           MOVE 0 TO RETURN-CODE
           MOVE SPACE TO LK-AGE-BAND-KBN
           MOVE '00' TO LK-RESULT-CD
           MOVE SPACE TO LK-REASON-CD
           SET WS-NOT-EOF TO TRUE
           SET WS-NOT-HIT TO TRUE

           OPEN INPUT LFPOLF
           IF WS-LFPOLF-ST NOT = '00'
              MOVE WS-LFPOLF-ST TO WS-DISPLAY-ST
              DISPLAY 'LFPOLF オープン失敗 ST=' WS-DISPLAY-ST
              MOVE '90' TO LK-RESULT-CD
              MOVE 'FOPN' TO LK-REASON-CD
              MOVE 8 TO RETURN-CODE
           END-IF.

       2000-PROCESS.
           IF RETURN-CODE NOT = 0
              EXIT PARAGRAPH
           END-IF

           PERFORM UNTIL WS-EOF OR WS-HIT
              READ LFPOLF
                 AT END
                    SET WS-EOF TO TRUE
                 NOT AT END
                    IF PO-POL-NO = LK-POL-NO
                       SET WS-HIT TO TRUE
                       PERFORM 3000-CHECK-AND-MAP
                    END-IF
              END-READ

              IF WS-LFPOLF-ST NOT = '00'
                 AND WS-LFPOLF-ST NOT = '10'
                 MOVE WS-LFPOLF-ST TO WS-DISPLAY-ST
                 DISPLAY 'LFPOLF 読込失敗 ST=' WS-DISPLAY-ST
                 MOVE '91' TO LK-RESULT-CD
                 MOVE 'FRED' TO LK-REASON-CD
                 MOVE 8 TO RETURN-CODE
                 SET WS-EOF TO TRUE
              END-IF
           END-PERFORM

           IF RETURN-CODE = 0 AND WS-NOT-HIT
              DISPLAY '証券番号該当なし PO=' LK-POL-NO
              MOVE '04' TO LK-RESULT-CD
              MOVE 'PNFD' TO LK-REASON-CD
           END-IF.

       3000-CHECK-AND-MAP.
           IF PO-SEX-KBN NOT = '1'
              AND PO-SEX-KBN NOT = '2'
              DISPLAY '性別区分不正 PO=' PO-POL-NO
                      ' SEX=' PO-SEX-KBN
              MOVE '12' TO LK-RESULT-CD
              MOVE 'SINV' TO LK-REASON-CD
              EXIT PARAGRAPH
           END-IF

           IF PO-POL-STATUS-KBN NOT = '01'
              DISPLAY '契約状態対象外 PO=' PO-POL-NO
                      ' ST=' PO-POL-STATUS-KBN
              MOVE '08' TO LK-RESULT-CD
              MOVE 'STAT' TO LK-REASON-CD
              EXIT PARAGRAPH
           END-IF

           IF PO-ENTRY-AGE-CNT < 0
              MOVE PO-ENTRY-AGE-CNT TO WS-DISPLAY-AGE
              DISPLAY '加入年齢不正 PO=' PO-POL-NO
                      ' AGE=' WS-DISPLAY-AGE
              MOVE '12' TO LK-RESULT-CD
              MOVE 'AINV' TO LK-REASON-CD
              EXIT PARAGRAPH
           END-IF

           EVALUATE TRUE
              WHEN PO-ENTRY-AGE-CNT <= 29
                 MOVE 'A1' TO LK-AGE-BAND-KBN
              WHEN PO-ENTRY-AGE-CNT <= 39
                 MOVE 'A2' TO LK-AGE-BAND-KBN
              WHEN PO-ENTRY-AGE-CNT <= 49
                 MOVE 'A3' TO LK-AGE-BAND-KBN
              WHEN PO-ENTRY-AGE-CNT <= 59
                 MOVE 'A4' TO LK-AGE-BAND-KBN
              WHEN OTHER
                 MOVE 'A5' TO LK-AGE-BAND-KBN
           END-EVALUATE

           MOVE '00' TO LK-RESULT-CD
           MOVE 'NORM' TO LK-REASON-CD.

       9000-FINAL.
           IF WS-LFPOLF-ST NOT = SPACE
              CLOSE LFPOLF
              IF WS-LFPOLF-ST NOT = '00'
                 MOVE WS-LFPOLF-ST TO WS-DISPLAY-ST
                 DISPLAY 'LFPOLF クローズ失敗 ST=' WS-DISPLAY-ST
                 MOVE '92' TO LK-RESULT-CD
                 MOVE 'FCLS' TO LK-REASON-CD
                 MOVE 8 TO RETURN-CODE
              END-IF
           END-IF.
