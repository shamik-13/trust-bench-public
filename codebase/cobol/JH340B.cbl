       IDENTIFICATION DIVISION.
       PROGRAM-ID. JH340B.
       AUTHOR. TRUST-BANK-SYSTEM.
       INSTALLATION. JH-BATCH-CENTER.
       DATE-WRITTEN. 2025-03-31.
       DATE-COMPILED.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT JHSEGDST-FILE
               ASSIGN TO "JHSEGDST"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-SD-STATUS.

           SELECT JHREFMST-FILE
               ASSIGN TO "JHREFMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS RF-REF-TYPE-CD
               FILE STATUS IS WS-RF-STATUS.

       DATA DIVISION.
       FILE SECTION.

       FD  JHSEGDST-FILE.
       COPY JHSEGDC.

       FD  JHREFMST-FILE.
       COPY JHREFMSTC.

       WORKING-STORAGE SECTION.
       01  WS-STATUS-AREA.
           05  WS-SD-STATUS             PIC XX VALUE SPACE.
           05  WS-RF-STATUS             PIC XX VALUE SPACE.
           05  WS-CALL-STATUS           PIC S9(4) COMP VALUE ZERO.

       01  WS-RUN-CONTROL.
           05  WS-PROCESS-DATE          PIC 9(8) VALUE ZERO.
           05  WS-EOF-SD                PIC X VALUE 'N'.
               88  SD-EOF                     VALUE 'Y'.
           05  WS-EOF-RF                PIC X VALUE 'N'.
               88  RF-EOF                     VALUE 'Y'.
           05  WS-HARD-ERROR            PIC X VALUE 'N'.
               88  HARD-ERROR                 VALUE 'Y'.
           05  WS-REPLACE-FOUND         PIC X VALUE 'N'.
               88  REPLACE-FOUND              VALUE 'Y'.
           05  WS-VALID-ROW             PIC X VALUE 'N'.
               88  VALID-ROW                  VALUE 'Y'.
           05  WS-FOUND-REF             PIC X VALUE 'N'.
               88  FOUND-REF                  VALUE 'Y'.

       01  WS-COUNTERS.
           05  WS-IN-CNT                PIC 9(9) VALUE ZERO.
           05  WS-SKIP-CNT              PIC 9(9) VALUE ZERO.
           05  WS-OUT-CNT               PIC 9(9) VALUE ZERO.
           05  WS-REPL-CNT              PIC 9(9) VALUE ZERO.
           05  WS-ERR-CNT               PIC 9(9) VALUE ZERO.
           05  WS-TBL-IDX               PIC 9(5) VALUE ZERO.
           05  WS-TBL-MAX               PIC 9(5) VALUE 20000.
           05  WS-I                     PIC 9(5) VALUE ZERO.
           05  WS-J                     PIC 9(5) VALUE ZERO.
           05  WS-ROW-CNT               PIC 9(5) VALUE ZERO.

       01  WS-ACCUMULATORS.
           05  WS-TOTAL-COUNT           PIC 9(13) VALUE ZERO.
           05  WS-TOTAL-BAL             PIC S9(15)V99 COMP-3
               VALUE ZERO.
           05  WS-PREV-COUNT            PIC 9(13) VALUE ZERO.
           05  WS-PREV-BAL              PIC S9(15)V99 COMP-3
               VALUE ZERO.
           05  WS-WORK-MEAN             PIC S9(13)V99 COMP-3
               VALUE ZERO.

       01  WS-REF-KEYS.
           05  WS-REF-TYPE-SEG          PIC X(2) VALUE 'SG'.
           05  WS-REF-TYPE-AGE          PIC X(2) VALUE 'AG'.
           05  WS-REF-TYPE-PREF         PIC X(2) VALUE 'PF'.
           05  WS-REF-TYPE-SAVE         PIC X(2) VALUE SPACE.
           05  WS-REF-CD-SAVE           PIC X(4) VALUE SPACE.
           05  WS-REF-NAME              PIC X(40) VALUE SPACE.

       01  WS-DISPLAY-MSG.
           05  WS-DISP-PGM              PIC X(8) VALUE 'JH340B'.
           05  FILLER                   PIC X VALUE SPACE.
           05  WS-DISP-TEXT             PIC X(80) VALUE SPACE.

       01  LK-JH342S-PARM.
           05  LK342-RETURN-CD          PIC 9(2) VALUE ZERO.
           05  LK342-COUNT              PIC 9(13) VALUE ZERO.
           05  LK342-TOTAL-COUNT        PIC 9(13) VALUE ZERO.
           05  LK342-BAL-SUM            PIC S9(15)V99 COMP-3
               VALUE ZERO.
           05  LK342-PREV-BAL-SUM       PIC S9(15)V99 COMP-3
               VALUE ZERO.
           05  LK342-RATIO-EDIT         PIC X(7) VALUE SPACE.
           05  LK342-MEAN-BAL-EDIT      PIC X(15) VALUE SPACE.
           05  LK342-DIFF-SIGN          PIC X VALUE SPACE.
           05  LK342-REASON-CD          PIC X(4) VALUE SPACE.

       01  WS-SORT-TABLE.
           05  WS-SEG-ROW OCCURS 20000 TIMES.
               10  T-REPORT-DATE        PIC 9(8).
               10  T-SEG-CD             PIC X(4).
               10  T-AGE-BAND-CD        PIC X(2).
               10  T-PREF-CD            PIC X(2).
               10  T-CUSTOMER-COUNT     PIC 9(13).
               10  T-AVG-BAL-SUM        PIC S9(15)V99 COMP-3.
               10  T-AVG-BAL-MEAN       PIC S9(13)V99 COMP-3.
               10  T-SEG-NAME           PIC X(40).
               10  T-AGE-NAME           PIC X(40).
               10  T-PREF-NAME          PIC X(40).
               10  T-RATIO-EDIT         PIC X(7).
               10  T-MEAN-EDIT          PIC X(15).
               10  T-DIFF-SIGN          PIC X.

       01  WS-SWAP-ROW.
           05  S-REPORT-DATE            PIC 9(8).
           05  S-SEG-CD                 PIC X(4).
           05  S-AGE-BAND-CD            PIC X(2).
           05  S-PREF-CD                PIC X(2).
           05  S-CUSTOMER-COUNT         PIC 9(13).
           05  S-AVG-BAL-SUM            PIC S9(15)V99 COMP-3.
           05  S-AVG-BAL-MEAN           PIC S9(13)V99 COMP-3.
           05  S-SEG-NAME               PIC X(40).
           05  S-AGE-NAME               PIC X(40).
           05  S-PREF-NAME              PIC X(40).
           05  S-RATIO-EDIT             PIC X(7).
           05  S-MEAN-EDIT              PIC X(15).
           05  S-DIFF-SIGN              PIC X.

       PROCEDURE DIVISION.
       MAIN-SECTION.
           PERFORM 0000-INIT
           IF NOT HARD-ERROR
               PERFORM 1000-LOAD-SOURCE
           END-IF
           IF NOT HARD-ERROR
               PERFORM 2000-SORT-ROWS
           END-IF
           IF NOT HARD-ERROR
               PERFORM 3000-EDIT-ROWS
           END-IF
           IF NOT HARD-ERROR
               PERFORM 4000-WRITE-OUTPUT
           END-IF
           PERFORM 9000-FINAL
           GOBACK
           .

       0000-INIT.
           MOVE 0 TO RETURN-CODE
           ACCEPT WS-PROCESS-DATE FROM DATE YYYYMMDD
           OPEN INPUT JHSEGDST-FILE
           IF WS-SD-STATUS NOT = '00'
               MOVE 12 TO RETURN-CODE
               SET HARD-ERROR TO TRUE
               DISPLAY 'JHSEGDST INPUT OPEN ERR ST='
                       WS-SD-STATUS
           END-IF
           IF NOT HARD-ERROR
               OPEN INPUT JHREFMST-FILE
               IF WS-RF-STATUS NOT = '00'
                   MOVE 12 TO RETURN-CODE
                   SET HARD-ERROR TO TRUE
                   DISPLAY 'JHREFMST OPEN ERR ST='
                           WS-RF-STATUS
               END-IF
           END-IF
           .

       1000-LOAD-SOURCE.
           PERFORM UNTIL SD-EOF OR HARD-ERROR
               READ JHSEGDST-FILE
                   AT END
                       SET SD-EOF TO TRUE
                   NOT AT END
                       ADD 1 TO WS-IN-CNT
                       PERFORM 1100-CHECK-SOURCE
                       IF VALID-ROW
                           PERFORM 1200-STORE-ROW
                       ELSE
                           ADD 1 TO WS-SKIP-CNT
                       END-IF
               END-READ
           END-PERFORM
           IF REPLACE-FOUND
               DISPLAY 'JH340B REPLACE COUNT=' WS-REPL-CNT
           END-IF
           .

       1100-CHECK-SOURCE.
           MOVE 'N' TO WS-VALID-ROW
           IF SD-REPORT-DATE = WS-PROCESS-DATE
               SET REPLACE-FOUND TO TRUE
               ADD 1 TO WS-REPL-CNT
           ELSE
               IF SD-REPORT-DATE > ZERO
                  AND SD-SEG-CD NOT = SPACE
                  AND SD-AGE-BAND-CD NOT = SPACE
                  AND SD-PREF-CD NOT = SPACE
                  AND SD-CUSTOMER-COUNT > ZERO
                   SET VALID-ROW TO TRUE
               ELSE
                   DISPLAY 'JHSEGDST BAD ROW DATE='
                           SD-REPORT-DATE
                           ' SEG=' SD-SEG-CD
               END-IF
           END-IF
           .

       1200-STORE-ROW.
           IF WS-ROW-CNT >= WS-TBL-MAX
               MOVE 12 TO RETURN-CODE
               SET HARD-ERROR TO TRUE
               DISPLAY 'TABLE OVERFLOW JHSEGDST COUNT='
                       WS-IN-CNT
           ELSE
               ADD 1 TO WS-ROW-CNT
               MOVE SD-REPORT-DATE     TO T-REPORT-DATE(WS-ROW-CNT)
               MOVE SD-SEG-CD          TO T-SEG-CD(WS-ROW-CNT)
               MOVE SD-AGE-BAND-CD     TO T-AGE-BAND-CD(WS-ROW-CNT)
               MOVE SD-PREF-CD         TO T-PREF-CD(WS-ROW-CNT)
               MOVE SD-CUSTOMER-COUNT  TO T-CUSTOMER-COUNT
                   (WS-ROW-CNT)
               MOVE SD-AVG-BAL-SUM     TO T-AVG-BAL-SUM(WS-ROW-CNT)
               MOVE SD-AVG-BAL-MEAN    TO T-AVG-BAL-MEAN
                   (WS-ROW-CNT)
               ADD SD-CUSTOMER-COUNT   TO WS-TOTAL-COUNT
               ADD SD-AVG-BAL-SUM      TO WS-TOTAL-BAL
           END-IF
           .

       2000-SORT-ROWS.
           IF WS-ROW-CNT > 1
               PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I >= WS-ROW-CNT
                   COMPUTE WS-J = WS-I + 1
                   PERFORM VARYING WS-J FROM WS-J BY 1
                       UNTIL WS-J > WS-ROW-CNT
                       IF T-SEG-CD(WS-I) > T-SEG-CD(WS-J)
                          OR (T-SEG-CD(WS-I) = T-SEG-CD(WS-J)
                          AND T-AGE-BAND-CD(WS-I)
                              > T-AGE-BAND-CD(WS-J))
                          OR (T-SEG-CD(WS-I) = T-SEG-CD(WS-J)
                          AND T-AGE-BAND-CD(WS-I)
                              = T-AGE-BAND-CD(WS-J)
                          AND T-PREF-CD(WS-I) > T-PREF-CD(WS-J))
                           PERFORM 2100-SWAP-ROWS
                       END-IF
                   END-PERFORM
               END-PERFORM
           END-IF
           .

       2100-SWAP-ROWS.
           MOVE T-REPORT-DATE(WS-I)    TO S-REPORT-DATE
           MOVE T-SEG-CD(WS-I)         TO S-SEG-CD
           MOVE T-AGE-BAND-CD(WS-I)    TO S-AGE-BAND-CD
           MOVE T-PREF-CD(WS-I)        TO S-PREF-CD
           MOVE T-CUSTOMER-COUNT(WS-I) TO S-CUSTOMER-COUNT
           MOVE T-AVG-BAL-SUM(WS-I)    TO S-AVG-BAL-SUM
           MOVE T-AVG-BAL-MEAN(WS-I)   TO S-AVG-BAL-MEAN
           MOVE T-SEG-NAME(WS-I)       TO S-SEG-NAME
           MOVE T-AGE-NAME(WS-I)       TO S-AGE-NAME
           MOVE T-PREF-NAME(WS-I)      TO S-PREF-NAME
           MOVE T-RATIO-EDIT(WS-I)     TO S-RATIO-EDIT
           MOVE T-MEAN-EDIT(WS-I)      TO S-MEAN-EDIT
           MOVE T-DIFF-SIGN(WS-I)      TO S-DIFF-SIGN

           MOVE T-REPORT-DATE(WS-J)    TO T-REPORT-DATE(WS-I)
           MOVE T-SEG-CD(WS-J)         TO T-SEG-CD(WS-I)
           MOVE T-AGE-BAND-CD(WS-J)    TO T-AGE-BAND-CD(WS-I)
           MOVE T-PREF-CD(WS-J)        TO T-PREF-CD(WS-I)
           MOVE T-CUSTOMER-COUNT(WS-J) TO T-CUSTOMER-COUNT(WS-I)
           MOVE T-AVG-BAL-SUM(WS-J)    TO T-AVG-BAL-SUM(WS-I)
           MOVE T-AVG-BAL-MEAN(WS-J)   TO T-AVG-BAL-MEAN(WS-I)
           MOVE T-SEG-NAME(WS-J)       TO T-SEG-NAME(WS-I)
           MOVE T-AGE-NAME(WS-J)       TO T-AGE-NAME(WS-I)
           MOVE T-PREF-NAME(WS-J)      TO T-PREF-NAME(WS-I)
           MOVE T-RATIO-EDIT(WS-J)     TO T-RATIO-EDIT(WS-I)
           MOVE T-MEAN-EDIT(WS-J)      TO T-MEAN-EDIT(WS-I)
           MOVE T-DIFF-SIGN(WS-J)      TO T-DIFF-SIGN(WS-I)

           MOVE S-REPORT-DATE          TO T-REPORT-DATE(WS-J)
           MOVE S-SEG-CD               TO T-SEG-CD(WS-J)
           MOVE S-AGE-BAND-CD          TO T-AGE-BAND-CD(WS-J)
           MOVE S-PREF-CD              TO T-PREF-CD(WS-J)
           MOVE S-CUSTOMER-COUNT       TO T-CUSTOMER-COUNT(WS-J)
           MOVE S-AVG-BAL-SUM          TO T-AVG-BAL-SUM(WS-J)
           MOVE S-AVG-BAL-MEAN         TO T-AVG-BAL-MEAN(WS-J)
           MOVE S-SEG-NAME             TO T-SEG-NAME(WS-J)
           MOVE S-AGE-NAME             TO T-AGE-NAME(WS-J)
           MOVE S-PREF-NAME            TO T-PREF-NAME(WS-J)
           MOVE S-RATIO-EDIT           TO T-RATIO-EDIT(WS-J)
           MOVE S-MEAN-EDIT            TO T-MEAN-EDIT(WS-J)
           MOVE S-DIFF-SIGN            TO T-DIFF-SIGN(WS-J)
           .

       3000-EDIT-ROWS.
           MOVE ZERO TO WS-PREV-COUNT WS-PREV-BAL
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > WS-ROW-CNT OR HARD-ERROR
               PERFORM 3100-LOAD-REFS
               IF NOT HARD-ERROR
                   PERFORM 3200-CALL-JH342S
               END-IF
               MOVE T-CUSTOMER-COUNT(WS-I) TO WS-PREV-COUNT
               MOVE T-AVG-BAL-SUM(WS-I)    TO WS-PREV-BAL
           END-PERFORM
           .

       3100-LOAD-REFS.
           MOVE WS-REF-TYPE-SEG TO WS-REF-TYPE-SAVE
           MOVE T-SEG-CD(WS-I)  TO WS-REF-CD-SAVE
           PERFORM 3150-READ-REF
           MOVE WS-REF-NAME     TO T-SEG-NAME(WS-I)

           MOVE WS-REF-TYPE-AGE     TO WS-REF-TYPE-SAVE
           MOVE T-AGE-BAND-CD(WS-I) TO WS-REF-CD-SAVE
           PERFORM 3150-READ-REF
           MOVE WS-REF-NAME         TO T-AGE-NAME(WS-I)

           MOVE WS-REF-TYPE-PREF TO WS-REF-TYPE-SAVE
           MOVE T-PREF-CD(WS-I)  TO WS-REF-CD-SAVE
           PERFORM 3150-READ-REF
           MOVE WS-REF-NAME      TO T-PREF-NAME(WS-I)
           .

       3150-READ-REF.
           MOVE SPACE TO WS-REF-NAME
           MOVE 'N' TO WS-FOUND-REF WS-EOF-RF
           MOVE WS-REF-TYPE-SAVE TO RF-REF-TYPE-CD
           MOVE WS-REF-CD-SAVE   TO RF-REF-CD
           START JHREFMST-FILE KEY IS >= RF-REF-TYPE-CD
               INVALID KEY
                   MOVE 'NAME NOT FOUND' TO WS-REF-NAME
               NOT INVALID KEY
                   PERFORM UNTIL RF-EOF OR FOUND-REF
                       READ JHREFMST-FILE NEXT RECORD
                           AT END
                               SET RF-EOF TO TRUE
                           NOT AT END
                               IF RF-REF-TYPE-CD NOT =
                                  WS-REF-TYPE-SAVE
                                   SET RF-EOF TO TRUE
                               ELSE
                                   IF RF-REF-CD = WS-REF-CD-SAVE
                                      AND RF-ACTIVE-FLAG = '1'
                                      AND RF-VALID-FROM-DATE
                                          <= WS-PROCESS-DATE
                                      AND RF-VALID-TO-DATE
                                          >= WS-PROCESS-DATE
                                       MOVE RF-REF-NAME TO WS-REF-NAME
                                       SET FOUND-REF TO TRUE
                                   END-IF
                               END-IF
                       END-READ
                   END-PERFORM
           END-START
           IF WS-RF-STATUS NOT = '00' AND WS-RF-STATUS NOT = '10'
              AND WS-RF-STATUS NOT = '23'
               MOVE 12 TO RETURN-CODE
               SET HARD-ERROR TO TRUE
               DISPLAY 'JHREFMST READ ERR ST=' WS-RF-STATUS
           END-IF
           IF NOT FOUND-REF AND NOT HARD-ERROR
               MOVE 'NAME NOT FOUND' TO WS-REF-NAME
           END-IF
           .

       3200-CALL-JH342S.
           MOVE ZERO TO LK342-RETURN-CD
           MOVE T-CUSTOMER-COUNT(WS-I) TO LK342-COUNT
           MOVE WS-TOTAL-COUNT         TO LK342-TOTAL-COUNT
           MOVE T-AVG-BAL-SUM(WS-I)    TO LK342-BAL-SUM
           MOVE WS-PREV-BAL            TO LK342-PREV-BAL-SUM
           MOVE SPACE TO LK342-RATIO-EDIT
                         LK342-MEAN-BAL-EDIT
                         LK342-DIFF-SIGN
                         LK342-REASON-CD
           CALL 'JH342S' USING LK-JH342S-PARM
           IF LK342-RETURN-CD NOT = ZERO
               MOVE 8 TO RETURN-CODE
               SET HARD-ERROR TO TRUE
               DISPLAY 'JH342S EDIT ERR REASON=' LK342-REASON-CD
           ELSE
               MOVE LK342-RATIO-EDIT    TO T-RATIO-EDIT(WS-I)
               MOVE LK342-MEAN-BAL-EDIT TO T-MEAN-EDIT(WS-I)
               MOVE LK342-DIFF-SIGN     TO T-DIFF-SIGN(WS-I)
               MOVE FUNCTION NUMVAL(LK342-MEAN-BAL-EDIT)
                 TO T-AVG-BAL-MEAN(WS-I)
           END-IF
           .

       4000-WRITE-OUTPUT.
           CLOSE JHSEGDST-FILE
           OPEN OUTPUT JHSEGDST-FILE
           IF WS-SD-STATUS NOT = '00'
               MOVE 12 TO RETURN-CODE
               SET HARD-ERROR TO TRUE
               DISPLAY 'JHSEGDST OUTPUT OPEN ERR ST='
                       WS-SD-STATUS
           END-IF
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > WS-ROW-CNT OR HARD-ERROR
               MOVE WS-PROCESS-DATE        TO SD-REPORT-DATE
               MOVE T-SEG-CD(WS-I)         TO SD-SEG-CD
               MOVE T-AGE-BAND-CD(WS-I)    TO SD-AGE-BAND-CD
               MOVE T-PREF-CD(WS-I)        TO SD-PREF-CD
               MOVE T-CUSTOMER-COUNT(WS-I) TO SD-CUSTOMER-COUNT
               MOVE T-AVG-BAL-SUM(WS-I)    TO SD-AVG-BAL-SUM
               MOVE T-AVG-BAL-MEAN(WS-I)   TO SD-AVG-BAL-MEAN
               WRITE JHSEGDST-REC
               IF WS-SD-STATUS NOT = '00'
                   MOVE 12 TO RETURN-CODE
                   SET HARD-ERROR TO TRUE
                   DISPLAY 'JHSEGDST WRITE ERR ST='
                           WS-SD-STATUS
               ELSE
                   ADD 1 TO WS-OUT-CNT
               END-IF
           END-PERFORM
           .

       9000-FINAL.
           IF WS-SD-STATUS NOT = '42'
               CLOSE JHSEGDST-FILE
           END-IF
           IF WS-RF-STATUS NOT = '42'
               CLOSE JHREFMST-FILE
           END-IF
           IF HARD-ERROR
               IF RETURN-CODE = 0
                   MOVE 8 TO RETURN-CODE
               END-IF
               DISPLAY 'JH340B ABEND IN=' WS-IN-CNT
                       ' OUT=' WS-OUT-CNT
           ELSE
               MOVE 0 TO RETURN-CODE
               DISPLAY 'JH340B NORMAL END IN=' WS-IN-CNT
                       ' SKIP=' WS-SKIP-CNT
                       ' OUT=' WS-OUT-CNT
           END-IF
           .
