       IDENTIFICATION DIVISION.
       PROGRAM-ID. DUMPSLD.
      *================================================================
      * Read CDRSLDF (the revolving statement file the engine wrote) and
      * DISPLAY each record as: SLDE|card-no|prin-yen|pay-yen|status|tier
      * (grader parses these). prin/pay shown as integer yen. Harness-only.
      *================================================================
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CDRSLDF ASSIGN TO "CDRSLDF"
               ORGANIZATION IS SEQUENTIAL FILE STATUS IS WS-ST.
       DATA DIVISION.
       FILE SECTION.
       FD  CDRSLDF.
       COPY CDRSLDFC.
       WORKING-STORAGE SECTION.
       01  WS-ST        PIC X(02).
       01  WS-EOF       PIC X(01) VALUE 'N'.
       01  WS-PRIN-YEN  PIC 9(11).
       01  WS-PAY-YEN   PIC 9(11).
       PROCEDURE DIVISION.
       000-MAIN.
           OPEN INPUT CDRSLDF
           IF WS-ST NOT = "00"
               DISPLAY "DUMPSLD OPEN ST=" WS-ST
               STOP RUN
           END-IF
           PERFORM UNTIL WS-EOF = 'Y'
               READ CDRSLDF AT END MOVE 'Y' TO WS-EOF
               NOT AT END
                   MOVE RS-PRIN-AMT TO WS-PRIN-YEN
                   MOVE RS-PAY-AMT  TO WS-PAY-YEN
                   DISPLAY "SLDE|" FUNCTION TRIM(RS-CARD-NO)
                           "|" WS-PRIN-YEN
                           "|" WS-PAY-YEN
                           "|" FUNCTION TRIM(RS-RSLD-STATUS)
                           "|" FUNCTION TRIM(RS-SLIDE-TIER)
               END-READ
           END-PERFORM
           CLOSE CDRSLDF
           STOP RUN.
