       IDENTIFICATION DIVISION.
       PROGRAM-ID. DUMPCYR.
      *================================================================
      * Read KZCYRF (resolved cycle basis dates) and DISPLAY each as:
      * CYR|cycle|resolved-dt|rolled-flag  (grader parses these).
      *================================================================
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZCYRF ASSIGN TO "KZCYRF"
               ORGANIZATION IS SEQUENTIAL FILE STATUS IS WS-ST.
       DATA DIVISION.
       FILE SECTION.
       FD  KZCYRF.
       COPY KZCYRFC.
       WORKING-STORAGE SECTION.
       01  WS-ST   PIC X(02).
       01  WS-EOF  PIC X(01) VALUE 'N'.
       PROCEDURE DIVISION.
       000-MAIN.
           OPEN INPUT KZCYRF
           IF WS-ST NOT = "00"
               DISPLAY "DUMPCYR OPEN ST=" WS-ST
               STOP RUN
           END-IF
           PERFORM UNTIL WS-EOF = 'Y'
               READ KZCYRF AT END MOVE 'Y' TO WS-EOF
               NOT AT END
                   DISPLAY "CYR|" FUNCTION TRIM(CR-CYCLE-ID)
                           "|" CR-RESOLVED-DT
                           "|" FUNCTION TRIM(CR-ROLLED-FLAG)
               END-READ
           END-PERFORM
           CLOSE KZCYRF
           STOP RUN.
