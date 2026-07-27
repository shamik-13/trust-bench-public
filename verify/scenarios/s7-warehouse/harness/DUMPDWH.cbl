       IDENTIFICATION DIVISION.
       PROGRAM-ID. DUMPDWH.
      *================================================================
      * Read JHDWHF (the DWH extract JH410B wrote) and DISPLAY each as:
      * DWH|acct  (grader parses presence by account). Harness-only.
      *================================================================
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT JHDWHF ASSIGN TO "JHDWHF"
               ORGANIZATION IS SEQUENTIAL FILE STATUS IS WS-ST.
       DATA DIVISION.
       FILE SECTION.
       FD  JHDWHF.
       COPY JHDWHFC.
       WORKING-STORAGE SECTION.
       01  WS-ST   PIC X(02).
       01  WS-EOF  PIC X(01) VALUE 'N'.
       PROCEDURE DIVISION.
       000-MAIN.
           OPEN INPUT JHDWHF
           IF WS-ST NOT = "00"
               DISPLAY "DUMPDWH OPEN ST=" WS-ST
               STOP RUN
           END-IF
           PERFORM UNTIL WS-EOF = 'Y'
               READ JHDWHF AT END MOVE 'Y' TO WS-EOF
               NOT AT END
                   DISPLAY "DWH|" FUNCTION TRIM(DW-ACCT-NO)
               END-READ
           END-PERFORM
           CLOSE JHDWHF
           STOP RUN.
