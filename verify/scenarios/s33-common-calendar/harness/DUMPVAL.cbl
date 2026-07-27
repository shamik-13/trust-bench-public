       IDENTIFICATION DIVISION.
       PROGRAM-ID. DUMPVAL.
      *================================================================
      * Read CCVALF (the value-date file the engine wrote) and DISPLAY
      * each record as: VAL|fct-id|value-dt|val-status (grader parses these).
      * Harness-only.
      *================================================================
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CCVALF ASSIGN TO "CCVALF"
               ORGANIZATION IS SEQUENTIAL FILE STATUS IS WS-ST.
       DATA DIVISION.
       FILE SECTION.
       FD  CCVALF.
       COPY CCVALFC.
       WORKING-STORAGE SECTION.
       01  WS-ST       PIC X(02).
       01  WS-EOF      PIC X(01) VALUE 'N'.
       01  WS-DT-OUT   PIC 9(08).
       PROCEDURE DIVISION.
       000-MAIN.
           OPEN INPUT CCVALF
           IF WS-ST NOT = "00"
               DISPLAY "DUMPVAL OPEN ST=" WS-ST
               STOP RUN
           END-IF
           PERFORM UNTIL WS-EOF = 'Y'
               READ CCVALF AT END MOVE 'Y' TO WS-EOF
               NOT AT END
                   MOVE VL-VALUE-DT TO WS-DT-OUT
                   DISPLAY "VAL|" FUNCTION TRIM(VL-FCT-ID)
                           "|" WS-DT-OUT
                           "|" FUNCTION TRIM(VL-VAL-STATUS-KBN)
               END-READ
           END-PERFORM
           CLOSE CCVALF
           STOP RUN.
