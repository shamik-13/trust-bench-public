       IDENTIFICATION DIVISION.
       PROGRAM-ID. LOADER.
      *================================================================
      * s33 harness: load fixed-width text fixtures into the real CCCALF
      * (calendar) and CCFCTF (instructions) sequential files the engine reads.
      *================================================================
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CAL-TXT ASSIGN TO "cal.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT FCT-TXT ASSIGN TO "fct.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT CCCALF ASSIGN TO "CCCALF"
               ORGANIZATION IS SEQUENTIAL FILE STATUS IS WS-ST.
           SELECT CCFCTF ASSIGN TO "CCFCTF"
               ORGANIZATION IS SEQUENTIAL FILE STATUS IS WS-ST2.
       DATA DIVISION.
       FILE SECTION.
       FD  CAL-TXT.
       01  IL-REC.
           05  IL-CAL-DT      PIC 9(08).
           05  IL-FLAG        PIC X(01).
       FD  FCT-TXT.
       01  IF-REC.
           05  IF-FCT-ID      PIC X(10).
           05  IF-TRIGGER     PIC 9(08).
           05  IF-AMT         PIC 9(11)V99.
           05  IF-STATUS      PIC X(02).
       FD  CCCALF.
       COPY CCCALFC.
       FD  CCFCTF.
       COPY CCFCTFC.
       WORKING-STORAGE SECTION.
       01  WS-ST   PIC X(02).
       01  WS-ST2  PIC X(02).
       01  WS-EOF  PIC X(01) VALUE 'N'.
       PROCEDURE DIVISION.
       000-MAIN.
           OPEN OUTPUT CCCALF
           OPEN INPUT CAL-TXT
           MOVE 'N' TO WS-EOF
           PERFORM UNTIL WS-EOF = 'Y'
               READ CAL-TXT AT END MOVE 'Y' TO WS-EOF
               NOT AT END
                   INITIALIZE CCCALF-REC
                   MOVE IL-CAL-DT TO CL-CAL-DT
                   MOVE IL-FLAG   TO CL-HOLIDAY-FLAG
                   WRITE CCCALF-REC
               END-READ
           END-PERFORM
           CLOSE CAL-TXT CCCALF
      *
           OPEN OUTPUT CCFCTF
           OPEN INPUT FCT-TXT
           MOVE 'N' TO WS-EOF
           PERFORM UNTIL WS-EOF = 'Y'
               READ FCT-TXT AT END MOVE 'Y' TO WS-EOF
               NOT AT END
                   INITIALIZE CCFCTF-REC
                   MOVE IF-FCT-ID  TO FC-FCT-ID
                   MOVE IF-TRIGGER TO FC-TRIGGER-DT
                   MOVE IF-AMT     TO FC-CONC-AMT
                   MOVE IF-STATUS  TO FC-FCT-STATUS-KBN
                   WRITE CCFCTF-REC
               END-READ
           END-PERFORM
           CLOSE FCT-TXT CCFCTF
           DISPLAY "LOADER DONE"
           STOP RUN.
