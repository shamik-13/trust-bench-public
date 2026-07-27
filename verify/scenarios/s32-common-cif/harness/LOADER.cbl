       IDENTIFICATION DIVISION.
       PROGRAM-ID. LOADER.
      *================================================================
      * s32 harness: load fixed-width text fixture into the real CMCIFF
      * (sequential) customer-master file the engine reads.
      *================================================================
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CIF-TXT ASSIGN TO "cif.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT CMCIFF ASSIGN TO "CMCIFF"
               ORGANIZATION IS SEQUENTIAL FILE STATUS IS WS-ST.
       DATA DIVISION.
       FILE SECTION.
       FD  CIF-TXT.
       01  IC-REC.
           05  IC-CIF-NO      PIC X(16).
           05  IC-BIRTH       PIC 9(08).
           05  IC-SEX         PIC X(02).
           05  IC-STATUS      PIC X(02).
       FD  CMCIFF.
       COPY CMCIFFC.
       WORKING-STORAGE SECTION.
       01  WS-ST   PIC X(02).
       01  WS-EOF  PIC X(01) VALUE 'N'.
       PROCEDURE DIVISION.
       000-MAIN.
           OPEN OUTPUT CMCIFF
           OPEN INPUT CIF-TXT
           MOVE 'N' TO WS-EOF
           PERFORM UNTIL WS-EOF = 'Y'
               READ CIF-TXT AT END MOVE 'Y' TO WS-EOF
               NOT AT END
                   INITIALIZE CMCIFF-REC
                   MOVE IC-CIF-NO   TO CF-CIF-NO
                   MOVE IC-BIRTH    TO CF-BIRTH-DT
                   MOVE IC-SEX      TO CF-SEX-KBN
                   MOVE IC-STATUS   TO CF-CIF-STATUS-KBN
                   WRITE CMCIFF-REC
               END-READ
           END-PERFORM
           CLOSE CIF-TXT CMCIFF
           DISPLAY "LOADER DONE"
           STOP RUN.
