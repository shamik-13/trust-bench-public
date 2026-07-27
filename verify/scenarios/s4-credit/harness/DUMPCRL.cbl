       IDENTIFICATION DIVISION.
       PROGRAM-ID. DUMPCRL.
      *================================================================
      * Read KZCRLF (credit-review result) and DISPLAY each record as:
      * CRL|acct|raise-flag|kyc|new-limit  (grader parses these).
      *================================================================
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZCRLF ASSIGN TO "KZCRLF"
               ORGANIZATION IS SEQUENTIAL FILE STATUS IS WS-ST.
       DATA DIVISION.
       FILE SECTION.
       FD  KZCRLF.
       COPY KZCRLFC.
       WORKING-STORAGE SECTION.
       01  WS-ST   PIC X(02).
       01  WS-EOF  PIC X(01) VALUE 'N'.
       01  WS-LIM-DISP PIC 9(09).
       PROCEDURE DIVISION.
       000-MAIN.
           OPEN INPUT KZCRLF
           IF WS-ST NOT = "00"
               DISPLAY "DUMPCRL OPEN ST=" WS-ST
               STOP RUN
           END-IF
           PERFORM UNTIL WS-EOF = 'Y'
               READ KZCRLF AT END MOVE 'Y' TO WS-EOF
               NOT AT END
                   MOVE CR-NEW-LIMIT TO WS-LIM-DISP
                   DISPLAY "CRL|" FUNCTION TRIM(CR-ACCT-NO)
                           "|" FUNCTION TRIM(CR-RAISE-FLAG)
                           "|" FUNCTION TRIM(CR-KYC-STATUS)
                           "|" FUNCTION TRIM(WS-LIM-DISP)
               END-READ
           END-PERFORM
           CLOSE KZCRLF
           STOP RUN.
