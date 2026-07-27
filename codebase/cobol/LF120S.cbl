       IDENTIFICATION DIVISION.
       PROGRAM-ID. LF120S.
      *================================================================*
      * RESERVE VALIDATION SUBROUTINE                                  *
      * COMPARES LFRSVF RESERVE WITH LFCVPF INPUT RESERVE.             *
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LFRSVF ASSIGN TO "LFRSVF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS RS-POL-NO
               FILE STATUS IS WS-LFRSVF-ST.
           SELECT LFCVPF ASSIGN TO "LFCVPF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-LFCVPF-ST.

       DATA DIVISION.
       FILE SECTION.
       FD  LFRSVF.
           COPY LFRSVC.

       FD  LFCVPF.
           COPY LFCVPFC.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05  WS-LFRSVF-ST        PIC XX VALUE SPACES.
           05  WS-LFCVPF-ST        PIC XX VALUE SPACES.

       01  WS-CONTROL.
           05  WS-EOF-SW           PIC X VALUE 'N'.
               88  WS-EOF                VALUE 'Y'.
               88  WS-NOT-EOF            VALUE 'N'.
           05  WS-HARD-ERROR-SW    PIC X VALUE 'N'.
               88  WS-HARD-ERROR        VALUE 'Y'.
               88  WS-NO-HARD-ERROR     VALUE 'N'.
           05  WS-WARN-FOUND-SW    PIC X VALUE 'N'.
               88  WS-WARN-FOUND        VALUE 'Y'.
               88  WS-NO-WARN-FOUND     VALUE 'N'.
           05  WS-READ-CNT         PIC 9(9) VALUE ZERO.
           05  WS-ERR-CNT          PIC 9(9) VALUE ZERO.

       01  WS-CALC-AREA.
           05  WS-SHOHIN-CD        PIC XX VALUE SPACES.
           05  WS-TOL-AMT          PIC S9(13)V99 COMP-3 VALUE ZERO.
           05  WS-DIFF-AMT         PIC S9(13)V99 COMP-3 VALUE ZERO.
           05  WS-ABS-DIFF-AMT     PIC S9(13)V99 COMP-3 VALUE ZERO.
           05  WS-RS-AMT           PIC S9(13)V99 COMP-3 VALUE ZERO.
           05  WS-CI-AMT           PIC S9(13)V99 COMP-3 VALUE ZERO.
           05  WS-MAX-AMT          PIC S9(13)V99 COMP-3
               VALUE 9999999999999.99.

       01  WS-REASON.
           05  WS-MSG-POL-NO       PIC X(20) VALUE SPACES.
           05  WS-MSG-REASON       PIC X(40) VALUE SPACES.

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE ZERO TO RETURN-CODE
           SET WS-NOT-EOF TO TRUE
           SET WS-NO-HARD-ERROR TO TRUE
           SET WS-NO-WARN-FOUND TO TRUE

           PERFORM 1000-OPEN-FILES

           PERFORM 2000-PROCESS
               UNTIL WS-EOF OR WS-HARD-ERROR

           PERFORM 9000-CLOSE-FILES

           IF WS-HARD-ERROR
               MOVE 8 TO RETURN-CODE
           ELSE
               IF WS-WARN-FOUND
                   MOVE 4 TO RETURN-CODE
               ELSE
                   MOVE ZERO TO RETURN-CODE
               END-IF
           END-IF

           DISPLAY 'LF120S COUNT=' WS-READ-CNT
                   ' ERROR=' WS-ERR-CNT
                   ' RC=' RETURN-CODE
           GOBACK.

       1000-OPEN-FILES.
           OPEN INPUT LFRSVF
           IF WS-LFRSVF-ST NOT = '00'
               DISPLAY 'LFRSVF OPEN ERROR ST=' WS-LFRSVF-ST
               SET WS-HARD-ERROR TO TRUE
           END-IF

           OPEN INPUT LFCVPF
           IF WS-LFCVPF-ST NOT = '00'
               DISPLAY 'LFCVPF OPEN ERROR ST=' WS-LFCVPF-ST
               SET WS-HARD-ERROR TO TRUE
           END-IF.

       2000-PROCESS.
           READ LFCVPF
               AT END
                   SET WS-EOF TO TRUE
               NOT AT END
                   ADD 1 TO WS-READ-CNT
                   PERFORM 2100-VALIDATE-CURRENT
           END-READ

           IF WS-LFCVPF-ST NOT = '00'
              AND WS-LFCVPF-ST NOT = '10'
               DISPLAY 'LFCVPF READ ERROR ST=' WS-LFCVPF-ST
               SET WS-HARD-ERROR TO TRUE
           END-IF.

       2100-VALIDATE-CURRENT.
           MOVE CI-POL-NO TO WS-MSG-POL-NO
           MOVE SPACES TO WS-MSG-REASON

           IF CI-CV-STATUS-KBN NOT = '01'
              AND CI-CV-STATUS-KBN NOT = '08'
              AND CI-CV-STATUS-KBN NOT = '09'
               MOVE 'BAD CV STATUS' TO WS-MSG-REASON
               PERFORM 8000-RECORD-WARNING
           END-IF

           IF CI-RESERVE-AMT IS NOT NUMERIC
               MOVE 'BAD INPUT RESERVE' TO WS-MSG-REASON
               PERFORM 8000-RECORD-WARNING
           ELSE
               MOVE CI-RESERVE-AMT TO WS-CI-AMT
               IF WS-CI-AMT < ZERO
                   MOVE 'NEG INPUT RESERVE' TO WS-MSG-REASON
                   PERFORM 8000-RECORD-WARNING
               END-IF
               IF WS-CI-AMT > WS-MAX-AMT
                   MOVE 'BIG INPUT RESERVE' TO WS-MSG-REASON
                   PERFORM 8000-RECORD-WARNING
               END-IF
           END-IF

           MOVE CI-POL-NO TO RS-POL-NO
           READ LFRSVF KEY IS RS-POL-NO
               INVALID KEY
                   MOVE 'LFRSVF NOT FOUND' TO WS-MSG-REASON
                   PERFORM 8000-RECORD-WARNING
               NOT INVALID KEY
                   PERFORM 2200-VALIDATE-PAIR
           END-READ

           IF WS-LFRSVF-ST NOT = '00'
              AND WS-LFRSVF-ST NOT = '23'
               DISPLAY 'LFRSVF READ ERROR ST=' WS-LFRSVF-ST
                       ' POL=' CI-POL-NO
               SET WS-HARD-ERROR TO TRUE
           END-IF.

       2200-VALIDATE-PAIR.
           IF RS-RESERVE-AMT IS NOT NUMERIC
               MOVE 'BAD FILE RESERVE' TO WS-MSG-REASON
               PERFORM 8000-RECORD-WARNING
           ELSE
               MOVE RS-RESERVE-AMT TO WS-RS-AMT
               IF WS-RS-AMT < ZERO
                   MOVE 'NEG FILE RESERVE' TO WS-MSG-REASON
                   PERFORM 8000-RECORD-WARNING
               END-IF
               IF WS-RS-AMT > WS-MAX-AMT
                   MOVE 'BIG FILE RESERVE' TO WS-MSG-REASON
                   PERFORM 8000-RECORD-WARNING
               END-IF
           END-IF

           IF CI-CV-STATUS-KBN = '01'
              AND RS-CALC-STATUS-KBN NOT = '01'
               MOVE 'STATUS MISMATCH 1' TO WS-MSG-REASON
               PERFORM 8000-RECORD-WARNING
           END-IF

           IF CI-CV-STATUS-KBN NOT = '01'
              AND RS-CALC-STATUS-KBN = '01'
               MOVE 'STATUS MISMATCH 2' TO WS-MSG-REASON
               PERFORM 8000-RECORD-WARNING
           END-IF

           IF CI-RESERVE-AMT IS NUMERIC
              AND RS-RESERVE-AMT IS NUMERIC
               PERFORM 2300-COMPARE-AMOUNT
           END-IF.

       2300-COMPARE-AMOUNT.
           MOVE CI-POL-NO(1:2) TO WS-SHOHIN-CD

           EVALUATE WS-SHOHIN-CD
               WHEN '01'
                   MOVE ZERO TO WS-TOL-AMT
               WHEN '02'
                   MOVE 1.00 TO WS-TOL-AMT
               WHEN '03'
                   MOVE 1.00 TO WS-TOL-AMT
               WHEN OTHER
                   MOVE 10.00 TO WS-TOL-AMT
           END-EVALUATE

           MOVE CI-RESERVE-AMT TO WS-CI-AMT
           MOVE RS-RESERVE-AMT TO WS-RS-AMT

           SUBTRACT WS-CI-AMT FROM WS-RS-AMT
               GIVING WS-DIFF-AMT

           IF WS-DIFF-AMT < ZERO
               COMPUTE WS-ABS-DIFF-AMT = WS-DIFF-AMT * -1
           ELSE
               MOVE WS-DIFF-AMT TO WS-ABS-DIFF-AMT
           END-IF

           IF WS-ABS-DIFF-AMT > WS-TOL-AMT
               MOVE 'RESERVE DIFF OVER' TO WS-MSG-REASON
               PERFORM 8000-RECORD-WARNING
           END-IF.

       8000-RECORD-WARNING.
           SET WS-WARN-FOUND TO TRUE
           ADD 1 TO WS-ERR-CNT
           DISPLAY 'LF120S NG POL=' WS-MSG-POL-NO
                   ' REASON=' WS-MSG-REASON.

       9000-CLOSE-FILES.
           IF WS-LFRSVF-ST = '00'
               CLOSE LFRSVF
               IF WS-LFRSVF-ST NOT = '00'
                   DISPLAY 'LFRSVF CLOSE ERROR ST=' WS-LFRSVF-ST
                   SET WS-HARD-ERROR TO TRUE
               END-IF
           END-IF

           IF WS-LFCVPF-ST = '00'
              OR WS-LFCVPF-ST = '10'
               CLOSE LFCVPF
               IF WS-LFCVPF-ST NOT = '00'
                   DISPLAY 'LFCVPF CLOSE ERROR ST=' WS-LFCVPF-ST
                   SET WS-HARD-ERROR TO TRUE
               END-IF
           END-IF.
