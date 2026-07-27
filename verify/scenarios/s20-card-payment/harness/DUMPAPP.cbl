       IDENTIFICATION DIVISION.
       PROGRAM-ID. DUMPAPP.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CDAPPF ASSIGN TO "CDAPPF" ORGANIZATION IS SEQUENTIAL FILE STATUS IS WS-ST.
       DATA DIVISION.
       FILE SECTION.
       FD  CDAPPF.
       COPY CDAPPFC.
       WORKING-STORAGE SECTION.
       01  WS-ST PIC X(02).
       01  WS-EOF PIC X(01) VALUE 'N'.
       01  WS-FEE PIC 9(11).
       01  WS-INT PIC 9(11).
       01  WS-PRIN PIC 9(11).
       PROCEDURE DIVISION.
       000-MAIN.
           OPEN INPUT CDAPPF
           IF WS-ST NOT = "00"
               DISPLAY "DUMPAPP OPEN ST=" WS-ST
               STOP RUN
           END-IF
           PERFORM UNTIL WS-EOF = 'Y'
               READ CDAPPF AT END MOVE 'Y' TO WS-EOF
               NOT AT END
                   MOVE AP-APPLIED-FEE-AMT  TO WS-FEE
                   MOVE AP-APPLIED-INT-AMT  TO WS-INT
                   MOVE AP-APPLIED-PRIN-AMT TO WS-PRIN
                   DISPLAY "APP|" FUNCTION TRIM(AP-PAY-ID)
                           "|" WS-FEE "|" WS-INT "|" WS-PRIN
                           "|" FUNCTION TRIM(AP-APP-STATUS)
               END-READ
           END-PERFORM
           CLOSE CDAPPF
           STOP RUN.
