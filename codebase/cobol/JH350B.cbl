       IDENTIFICATION DIVISION.
       PROGRAM-ID. JH350B.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT JHCBALF
               ASSIGN TO "JHCBALF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-JHCBALF-ST.
           SELECT JHBANDRF
               ASSIGN TO "JHBANDRF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-JHBANDRF-ST.

       DATA DIVISION.
       FILE SECTION.

       FD  JHCBALF.
           COPY JHCBALFC.

       FD  JHBANDRF.
           COPY JHBANDC.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05 WS-JHCBALF-ST             PIC X(02) VALUE SPACES.
           05 WS-JHBANDRF-ST            PIC X(02) VALUE SPACES.

       01  WS-FLAGS.
           05 WS-EOF-FLG                PIC X(01) VALUE "N".
              88 WS-EOF                           VALUE "Y".
              88 WS-NOT-EOF                       VALUE "N".
           05 WS-ABEND-FLG              PIC X(01) VALUE "N".
              88 WS-ABEND                         VALUE "Y".
              88 WS-NORMAL                        VALUE "N".
           05 WS-FOUND-FLG              PIC X(01) VALUE "N".
              88 WS-BAND-FOUND                    VALUE "Y".
              88 WS-BAND-NOT-FOUND                VALUE "N".

       01  WS-COUNTERS.
           05 WS-READ-CNT               PIC 9(09) VALUE ZERO.
           05 WS-WRITE-CNT              PIC 9(09) VALUE ZERO.
           05 WS-SKIP-CNT               PIC 9(09) VALUE ZERO.
           05 WS-WARN-CNT               PIC 9(09) VALUE ZERO.
           05 WS-ERR-CNT                PIC 9(09) VALUE ZERO.

       01  WS-WORK.
           05 WS-IDX                    PIC 9(02) VALUE ZERO.
           05 WS-BAND-IDX               PIC 9(02) VALUE ZERO.
           05 WS-REPORT-DATE            PIC 9(08) VALUE ZERO.
           05 WS-DATE-WORK              PIC 9(08) VALUE ZERO.
           05 WS-DATE-X REDEFINES WS-DATE-WORK PIC X(08).

       01  WS-BAND-TABLE.
           05 WS-BAND-ENTRY OCCURS 10 TIMES.
              10 WS-BAND-CD             PIC X(03).
              10 WS-BAND-LOWER          PIC S9(13) COMP-3.
              10 WS-BAND-COUNT          PIC 9(09).

       01  WS-BAND-INIT.
           05 WS-BAND-INIT-ENTRY OCCURS 10 TIMES.
              10 WS-INIT-CD             PIC X(03).
              10 WS-INIT-LOWER          PIC S9(13) COMP-3.
              10 WS-INIT-COUNT          PIC 9(09).

       01  LK-JH352S-PARM.
           05 LK-AVG-BALANCE            PIC S9(13) COMP-3.
           05 LK-BAND-CD                PIC X(03).
           05 LK-WARN-FLAG              PIC X(01).
           05 LK-RETURN-STATUS          PIC 9(04).

       PROCEDURE DIVISION.
       0000-MAIN SECTION.
       0000-START.
           PERFORM 1000-INIT
           IF WS-NORMAL
              PERFORM 2000-PROCESS UNTIL WS-EOF OR WS-ABEND
           END-IF
           IF WS-NORMAL
              PERFORM 3000-WRITE-REPORT
           END-IF
           PERFORM 9000-FINAL
           GOBACK.

       1000-INIT SECTION.
       1000-START.
           MOVE ZERO TO RETURN-CODE
           SET WS-NOT-EOF TO TRUE
           SET WS-NORMAL TO TRUE

           ACCEPT WS-DATE-X FROM DATE YYYYMMDD
           MOVE WS-DATE-WORK TO WS-REPORT-DATE

           MOVE "H01" TO WS-INIT-CD(1)
           MOVE ZERO TO WS-INIT-LOWER(1)
           MOVE "H02" TO WS-INIT-CD(2)
           MOVE 300000 TO WS-INIT-LOWER(2)
           MOVE "H03" TO WS-INIT-CD(3)
           MOVE 700000 TO WS-INIT-LOWER(3)
           MOVE "H04" TO WS-INIT-CD(4)
           MOVE 1500000 TO WS-INIT-LOWER(4)
           MOVE "H05" TO WS-INIT-CD(5)
           MOVE 3000000 TO WS-INIT-LOWER(5)
           MOVE "H06" TO WS-INIT-CD(6)
           MOVE 7000000 TO WS-INIT-LOWER(6)
           MOVE "H07" TO WS-INIT-CD(7)
           MOVE 12000000 TO WS-INIT-LOWER(7)
           MOVE "H08" TO WS-INIT-CD(8)
           MOVE 30000000 TO WS-INIT-LOWER(8)
           MOVE "H09" TO WS-INIT-CD(9)
           MOVE 70000000 TO WS-INIT-LOWER(9)
           MOVE "H10" TO WS-INIT-CD(10)
           MOVE 999999999 TO WS-INIT-LOWER(10)

           PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > 10
              MOVE WS-INIT-CD(WS-IDX) TO WS-BAND-CD(WS-IDX)
              MOVE WS-INIT-LOWER(WS-IDX) TO WS-BAND-LOWER(WS-IDX)
              MOVE ZERO TO WS-BAND-COUNT(WS-IDX)
              MOVE ZERO TO WS-INIT-COUNT(WS-IDX)
           END-PERFORM

           OPEN INPUT JHCBALF
           IF WS-JHCBALF-ST NOT = "00"
              DISPLAY "JHCBALF OPEN ERROR ST=" WS-JHCBALF-ST
              SET WS-ABEND TO TRUE
              MOVE 8 TO RETURN-CODE
           END-IF

           IF WS-NORMAL
              OPEN OUTPUT JHBANDRF
              IF WS-JHBANDRF-ST NOT = "00"
                 DISPLAY "JHBANDRF OPEN ERROR ST=" WS-JHBANDRF-ST
                 SET WS-ABEND TO TRUE
                 MOVE 8 TO RETURN-CODE
              END-IF
           END-IF.

       2000-PROCESS SECTION.
       2000-START.
           READ JHCBALF
              AT END
                 SET WS-EOF TO TRUE
              NOT AT END
                 IF WS-JHCBALF-ST = "00"
                    ADD 1 TO WS-READ-CNT
                    PERFORM 2100-EDIT-AND-CALL
                 ELSE
                    DISPLAY "JHCBALF READ ERROR ST=" WS-JHCBALF-ST
                    SET WS-ABEND TO TRUE
                    MOVE 8 TO RETURN-CODE
                 END-IF
           END-READ.

       2100-EDIT-AND-CALL SECTION.
       2100-START.
           IF CB-CUST-ID = SPACES
              ADD 1 TO WS-SKIP-CNT
              DISPLAY "CUSTOMER ID IS SPACE"
           ELSE
              MOVE CB-AVG-BAL TO LK-AVG-BALANCE
              MOVE SPACES TO LK-BAND-CD
              MOVE SPACES TO LK-WARN-FLAG
              MOVE ZERO TO LK-RETURN-STATUS

              CALL "JH352S" USING LK-JH352S-PARM
                 ON EXCEPTION
                    DISPLAY "JH352S CALL ERROR CUST=" CB-CUST-ID
                    SET WS-ABEND TO TRUE
                    MOVE 12 TO RETURN-CODE
              END-CALL

              IF WS-NORMAL
                 PERFORM 2200-APPLY-BAND
              END-IF
           END-IF.

       2200-APPLY-BAND SECTION.
       2200-START.
           IF LK-RETURN-STATUS NOT = ZERO
              ADD 1 TO WS-SKIP-CNT
              DISPLAY "BAND JUDGE ERROR CUST=" CB-CUST-ID
                      " ST=" LK-RETURN-STATUS
           ELSE
              IF LK-WARN-FLAG NOT = SPACES
              AND LK-WARN-FLAG NOT = "0"
                 ADD 1 TO WS-WARN-CNT
                 ADD 1 TO WS-SKIP-CNT
                 DISPLAY "BAND WARNING CUST=" CB-CUST-ID
                         " BAND=" LK-BAND-CD
              ELSE
                 SET WS-BAND-NOT-FOUND TO TRUE
                 PERFORM VARYING WS-BAND-IDX FROM 1 BY 1
                    UNTIL WS-BAND-IDX > 10 OR WS-BAND-FOUND
                    IF LK-BAND-CD = WS-BAND-CD(WS-BAND-IDX)
                       ADD 1 TO WS-BAND-COUNT(WS-BAND-IDX)
                       SET WS-BAND-FOUND TO TRUE
                    END-IF
                 END-PERFORM

                 IF WS-BAND-NOT-FOUND
                    ADD 1 TO WS-ERR-CNT
                    ADD 1 TO WS-SKIP-CNT
                    DISPLAY "INVALID BAND CUST=" CB-CUST-ID
                            " BAND=" LK-BAND-CD
                 END-IF
              END-IF
           END-IF.

       3000-WRITE-REPORT SECTION.
       3000-START.
           PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > 10
              INITIALIZE JHBANDRF-REC
              MOVE WS-REPORT-DATE TO BD-REPORT-DATE
              MOVE WS-BAND-CD(WS-IDX) TO BD-BAND-CD
              MOVE WS-BAND-COUNT(WS-IDX) TO BD-CUSTOMER-COUNT
              MOVE WS-BAND-LOWER(WS-IDX) TO BD-BALANCE-SUM

              WRITE JHBANDRF-REC
              IF WS-JHBANDRF-ST = "00"
                 ADD 1 TO WS-WRITE-CNT
              ELSE
                 DISPLAY "JHBANDRF WRITE ERROR ST=" WS-JHBANDRF-ST
                         " BAND=" WS-BAND-CD(WS-IDX)
                 SET WS-ABEND TO TRUE
                 MOVE 8 TO RETURN-CODE
                 EXIT PERFORM
              END-IF
           END-PERFORM.

       9000-FINAL SECTION.
       9000-START.
           IF WS-JHCBALF-ST NOT = SPACES
              CLOSE JHCBALF
           END-IF

           IF WS-JHBANDRF-ST NOT = SPACES
              CLOSE JHBANDRF
           END-IF

           DISPLAY "JH350B END READ=" WS-READ-CNT
                   " WRITE=" WS-WRITE-CNT
                   " WARN=" WS-WARN-CNT
                   " SKIP=" WS-SKIP-CNT
                   " ERROR=" WS-ERR-CNT

           IF RETURN-CODE = ZERO
              DISPLAY "JH350B NORMAL END"
           ELSE
              DISPLAY "JH350B ABNORMAL END RC=" RETURN-CODE
           END-IF.
