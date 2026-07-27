       IDENTIFICATION DIVISION.
       PROGRAM-ID. LOADER.
      *================================================================
      * s17 harness: load fixed-width text fixtures into the real CDCARDF
      * (indexed) + CDBALF (sequential) files the billing engine reads.
      *================================================================
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CARD-TXT ASSIGN TO "card.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT BAL-TXT ASSIGN TO "bal.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT CDCARDF ASSIGN TO "CDCARDF"
               ORGANIZATION IS INDEXED ACCESS IS DYNAMIC
               RECORD KEY IS CF-CARD-NO FILE STATUS IS WS-ST.
           SELECT CDBALF ASSIGN TO "CDBALF"
               ORGANIZATION IS SEQUENTIAL FILE STATUS IS WS-ST.
       DATA DIVISION.
       FILE SECTION.
       FD  CARD-TXT.
       01  IC-REC.
           05  IC-CARD-NO     PIC X(16).
           05  IC-MEMBER-ID   PIC X(10).
           05  IC-STATUS      PIC X(02).
           05  IC-LIMIT       PIC 9(11)V99.
           05  IC-CYCLE-CD    PIC X(10).
           05  IC-NAME        PIC X(40).
           05  IC-OPEN-DT     PIC 9(08).
       FD  BAL-TXT.
       01  IB-REC.
           05  IB-CARD-NO     PIC X(16).
           05  IB-CYCLE-DT    PIC 9(08).
           05  IB-CLOSING     PIC 9(11)V99.
           05  IB-REVOLVING   PIC 9(11)V99.
           05  IB-NEW-CHARGE  PIC 9(11)V99.
           05  IB-CASH-ADV    PIC 9(11)V99.
       FD  CDCARDF.
       COPY CDCARDFC.
       FD  CDBALF.
       COPY CDBALFC.
       WORKING-STORAGE SECTION.
       01  WS-ST   PIC X(02).
       01  WS-EOF  PIC X(01) VALUE 'N'.
       PROCEDURE DIVISION.
       000-MAIN.
           OPEN OUTPUT CDCARDF CDBALF
           PERFORM 100-LOAD-CARD
           PERFORM 200-LOAD-BAL
           CLOSE CDCARDF CDBALF
           DISPLAY "LOADER DONE"
           STOP RUN.
       100-LOAD-CARD.
           OPEN INPUT CARD-TXT
           MOVE 'N' TO WS-EOF
           PERFORM UNTIL WS-EOF = 'Y'
               READ CARD-TXT AT END MOVE 'Y' TO WS-EOF
               NOT AT END
                   INITIALIZE CDCARDF-REC
                   MOVE IC-CARD-NO    TO CF-CARD-NO
                   MOVE IC-MEMBER-ID  TO CF-MEMBER-ID
                   MOVE IC-STATUS     TO CF-CARD-STATUS
                   MOVE IC-LIMIT      TO CF-CREDIT-LIMIT
                   MOVE IC-CYCLE-CD   TO CF-BILL-CYCLE-CD
                   MOVE IC-NAME       TO CF-MEMBER-NAME-KANA
                   MOVE IC-OPEN-DT    TO CF-OPEN-DT
                   WRITE CDCARDF-REC
               END-READ
           END-PERFORM
           CLOSE CARD-TXT.
       200-LOAD-BAL.
           OPEN INPUT BAL-TXT
           MOVE 'N' TO WS-EOF
           PERFORM UNTIL WS-EOF = 'Y'
               READ BAL-TXT AT END MOVE 'Y' TO WS-EOF
               NOT AT END
                   INITIALIZE CDBALF-REC
                   MOVE IB-CARD-NO    TO BL-CARD-NO
                   MOVE IB-CYCLE-DT   TO BL-CYCLE-DT
                   MOVE IB-CLOSING    TO BL-CLOSING-BAL-AMT
                   MOVE IB-REVOLVING  TO BL-REVOLVING-BAL-AMT
                   MOVE IB-NEW-CHARGE TO BL-NEW-CHARGE-AMT
                   MOVE IB-CASH-ADV   TO BL-CASH-ADV-AMT
                   WRITE CDBALF-REC
               END-READ
           END-PERFORM
           CLOSE BAL-TXT.
