       IDENTIFICATION DIVISION.
       PROGRAM-ID. DUMPCAP.
      *================================================================
      * Read CDCAPF and DISPLAY: CAP|sale-id|billed-yen|fee-yen|status
      * (grader parses these). Harness-only.
      *================================================================
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CDCAPF ASSIGN TO "CDCAPF"
               ORGANIZATION IS SEQUENTIAL FILE STATUS IS WS-ST.
       DATA DIVISION.
       FILE SECTION.
       FD  CDCAPF.
       COPY CDCAPFC.
       WORKING-STORAGE SECTION.
       01  WS-ST       PIC X(02).
       01  WS-EOF      PIC X(01) VALUE 'N'.
       01  WS-BILL-YEN PIC 9(11).
       01  WS-FEE-YEN  PIC 9(11).
       PROCEDURE DIVISION.
       000-MAIN.
           OPEN INPUT CDCAPF
           IF WS-ST NOT = "00"
               DISPLAY "DUMPCAP OPEN ST=" WS-ST
               STOP RUN
           END-IF
           PERFORM UNTIL WS-EOF = 'Y'
               READ CDCAPF AT END MOVE 'Y' TO WS-EOF
               NOT AT END
                   MOVE BC-BILLED-AMT TO WS-BILL-YEN
                   MOVE BC-FEE-AMT    TO WS-FEE-YEN
                   DISPLAY "CAP|" FUNCTION TRIM(BC-SALE-ID)
                           "|" WS-BILL-YEN
                           "|" WS-FEE-YEN
                           "|" FUNCTION TRIM(BC-CAP-STATUS)
               END-READ
           END-PERFORM
           CLOSE CDCAPF
           STOP RUN.
