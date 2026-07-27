       IDENTIFICATION DIVISION.
       PROGRAM-ID. LOADER.
      *================================================================
      * s19 harness: load text fixtures into CDCARDF (indexed) +
      * CDSALEF (sequential) for the capture engine.
      *================================================================
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CARD-TXT ASSIGN TO "card.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT SALE-TXT ASSIGN TO "sale.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT CDCARDF ASSIGN TO "CDCARDF"
               ORGANIZATION IS INDEXED ACCESS IS DYNAMIC
               RECORD KEY IS CF-CARD-NO FILE STATUS IS WS-ST.
           SELECT CDSALEF ASSIGN TO "CDSALEF"
               ORGANIZATION IS SEQUENTIAL FILE STATUS IS WS-ST.
       DATA DIVISION.
       FILE SECTION.
       FD  CARD-TXT.
       01  IC-REC.
           05  IC-CARD-NO     PIC X(16).
           05  IC-MEMBER-ID   PIC X(10).
           05  IC-STATUS      PIC X(02).
           05  IC-LIMIT       PIC 9(11)V99.
           05  IC-NAME        PIC X(40).
       FD  SALE-TXT.
       01  IS-REC.
           05  IS-SALE-ID     PIC X(10).
           05  IS-CARD-NO     PIC X(16).
           05  IS-SALE-AMT    PIC 9(11)V99.
           05  IS-CURRENCY    PIC X(10).
           05  IS-MERCHANT    PIC X(04).
           05  IS-SALE-DT     PIC 9(08).
           05  IS-AUTH-ID     PIC X(10).
       FD  CDCARDF.
       COPY CDCARDFC.
       FD  CDSALEF.
       COPY CDSALEFC.
       WORKING-STORAGE SECTION.
       01  WS-ST   PIC X(02).
       01  WS-EOF  PIC X(01) VALUE 'N'.
       PROCEDURE DIVISION.
       000-MAIN.
           OPEN OUTPUT CDCARDF CDSALEF
           PERFORM 100-LOAD-CARD
           PERFORM 200-LOAD-SALE
           CLOSE CDCARDF CDSALEF
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
                   MOVE IC-NAME       TO CF-MEMBER-NAME-KANA
                   WRITE CDCARDF-REC
               END-READ
           END-PERFORM
           CLOSE CARD-TXT.
       200-LOAD-SALE.
           OPEN INPUT SALE-TXT
           MOVE 'N' TO WS-EOF
           PERFORM UNTIL WS-EOF = 'Y'
               READ SALE-TXT AT END MOVE 'Y' TO WS-EOF
               NOT AT END
                   INITIALIZE CDSALEF-REC
                   MOVE IS-SALE-ID    TO SL-SALE-ID
                   MOVE IS-CARD-NO    TO SL-CARD-NO
                   MOVE IS-SALE-AMT   TO SL-SALE-AMT
                   MOVE IS-CURRENCY   TO SL-CURRENCY-CD
                   MOVE IS-MERCHANT   TO SL-MERCHANT-CODE
                   MOVE IS-SALE-DT    TO SL-SALE-DT
                   MOVE IS-AUTH-ID    TO SL-AUTH-ID
                   WRITE CDSALEF-REC
               END-READ
           END-PERFORM
           CLOSE SALE-TXT.
