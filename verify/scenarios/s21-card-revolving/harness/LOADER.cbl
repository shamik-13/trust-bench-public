       IDENTIFICATION DIVISION.
       PROGRAM-ID. LOADER.
      *================================================================
      * s21 harness: load fixed-width text fixtures into the real CDREVF
      * (indexed) + CDRBALF (sequential) files the revolving engine reads.
      *================================================================
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT REV-TXT ASSIGN TO "rev.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT BAL-TXT ASSIGN TO "bal.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT CDREVF ASSIGN TO "CDREVF"
               ORGANIZATION IS INDEXED ACCESS IS DYNAMIC
               RECORD KEY IS RV-CARD-NO FILE STATUS IS WS-ST.
           SELECT CDRBALF ASSIGN TO "CDRBALF"
               ORGANIZATION IS SEQUENTIAL FILE STATUS IS WS-ST.
       DATA DIVISION.
       FILE SECTION.
       FD  REV-TXT.
       01  IR-REC.
           05  IR-CARD-NO     PIC X(16).
           05  IR-MEMBER-ID   PIC X(10).
           05  IR-STATUS      PIC X(02).
           05  IR-COURSE-CD   PIC X(10).
           05  IR-NAME        PIC X(40).
           05  IR-START-DT    PIC 9(08).
       FD  BAL-TXT.
       01  IB-REC.
           05  IB-CARD-NO     PIC X(16).
           05  IB-CYCLE-DT    PIC 9(08).
           05  IB-REV-BAL     PIC 9(11)V99.
           05  IB-CARRIED-FEE PIC 9(11)V99.
           05  IB-NEW-REV     PIC 9(11)V99.
       FD  CDREVF.
       COPY CDREVFC.
       FD  CDRBALF.
       COPY CDRBALFC.
       WORKING-STORAGE SECTION.
       01  WS-ST   PIC X(02).
       01  WS-EOF  PIC X(01) VALUE 'N'.
       PROCEDURE DIVISION.
       000-MAIN.
           OPEN OUTPUT CDREVF CDRBALF
           PERFORM 100-LOAD-REV
           PERFORM 200-LOAD-BAL
           CLOSE CDREVF CDRBALF
           DISPLAY "LOADER DONE"
           STOP RUN.
       100-LOAD-REV.
           OPEN INPUT REV-TXT
           MOVE 'N' TO WS-EOF
           PERFORM UNTIL WS-EOF = 'Y'
               READ REV-TXT AT END MOVE 'Y' TO WS-EOF
               NOT AT END
                   INITIALIZE CDREVF-REC
                   MOVE IR-CARD-NO    TO RV-CARD-NO
                   MOVE IR-MEMBER-ID  TO RV-MEMBER-ID
                   MOVE IR-STATUS     TO RV-REV-STATUS
                   MOVE IR-COURSE-CD  TO RV-REV-COURSE-CD
                   MOVE IR-NAME       TO RV-MEMBER-NAME-KANA
                   MOVE IR-START-DT   TO RV-REV-START-DT
                   WRITE CDREVF-REC
               END-READ
           END-PERFORM
           CLOSE REV-TXT.
       200-LOAD-BAL.
           OPEN INPUT BAL-TXT
           MOVE 'N' TO WS-EOF
           PERFORM UNTIL WS-EOF = 'Y'
               READ BAL-TXT AT END MOVE 'Y' TO WS-EOF
               NOT AT END
                   INITIALIZE CDRBALF-REC
                   MOVE IB-CARD-NO     TO RB-CARD-NO
                   MOVE IB-CYCLE-DT    TO RB-CYCLE-DT
                   MOVE IB-REV-BAL     TO RB-REV-BAL-AMT
                   MOVE IB-CARRIED-FEE TO RB-CARRIED-FEE-AMT
                   MOVE IB-NEW-REV     TO RB-NEW-REV-AMT
                   WRITE CDRBALF-REC
               END-READ
           END-PERFORM
           CLOSE BAL-TXT.
