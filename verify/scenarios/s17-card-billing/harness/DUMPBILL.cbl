       IDENTIFICATION DIVISION.
       PROGRAM-ID. DUMPBILL.
      *================================================================
      * Read CDBILLF (the statement file the billing engine wrote) and
      * DISPLAY each record as: BILL|card-no|min-pay-yen|bill-status
      * (grader parses these). min-pay is shown as integer yen. Harness-only.
      *================================================================
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CDBILLF ASSIGN TO "CDBILLF"
               ORGANIZATION IS SEQUENTIAL FILE STATUS IS WS-ST.
       DATA DIVISION.
       FILE SECTION.
       FD  CDBILLF.
       COPY CDBILLFC.
       WORKING-STORAGE SECTION.
       01  WS-ST       PIC X(02).
       01  WS-EOF      PIC X(01) VALUE 'N'.
       01  WS-MIN-YEN  PIC 9(11).
       PROCEDURE DIVISION.
       000-MAIN.
           OPEN INPUT CDBILLF
           IF WS-ST NOT = "00"
               DISPLAY "DUMPBILL OPEN ST=" WS-ST
               STOP RUN
           END-IF
           PERFORM UNTIL WS-EOF = 'Y'
               READ CDBILLF AT END MOVE 'Y' TO WS-EOF
               NOT AT END
                   MOVE BI-MIN-PAY-AMT TO WS-MIN-YEN
                   DISPLAY "BILL|" FUNCTION TRIM(BI-CARD-NO)
                           "|" WS-MIN-YEN
                           "|" FUNCTION TRIM(BI-BILL-STATUS)
               END-READ
           END-PERFORM
           CLOSE CDBILLF
           STOP RUN.
