       IDENTIFICATION DIVISION.
       PROGRAM-ID. DUMPFEE.
      *================================================================
      * Read the KZFEEHF output the fee engine produced and DISPLAY each
      * record as a parseable line:  FEE|acct|fee_amt|fee_ytd|cap|exempt
      * (amounts in sen → grader divides by 100). Harness-only.
      *================================================================
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZFEEHF ASSIGN TO "KZFEEHF"
               ORGANIZATION IS INDEXED ACCESS IS SEQUENTIAL
               RECORD KEY IS FE-ACCT-ID FILE STATUS IS WS-ST.
       DATA DIVISION.
       FILE SECTION.
       FD  KZFEEHF.
       COPY KZFEEHC.
       WORKING-STORAGE SECTION.
       01  WS-ST    PIC X(02).
       01  WS-EOF   PIC X(01) VALUE 'N'.
       01  WS-AMT   PIC -(11)9.99.
       01  WS-YTD   PIC -(11)9.99.
       PROCEDURE DIVISION.
       000-MAIN.
           OPEN INPUT KZFEEHF
           IF WS-ST NOT = "00"
               DISPLAY "DUMPFEE OPEN FAIL ST=" WS-ST
               STOP RUN
           END-IF
           PERFORM UNTIL WS-EOF = 'Y'
               READ KZFEEHF NEXT AT END MOVE 'Y' TO WS-EOF
               NOT AT END
                   MOVE FE-FEE-AMT TO WS-AMT
                   MOVE FE-FEE-YTD TO WS-YTD
                   DISPLAY "FEE|" FE-ACCT-ID "|"
                           FUNCTION TRIM(WS-AMT) "|"
                           FUNCTION TRIM(WS-YTD) "|"
                           FE-CAP-FLAG "|" FE-EXEMPT-FLAG
               END-READ
           END-PERFORM
           CLOSE KZFEEHF
           STOP RUN.
