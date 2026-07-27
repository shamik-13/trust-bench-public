       IDENTIFICATION DIVISION.
       PROGRAM-ID. DUMPOUT.
      *================================================================
      * Read TGOUTCF (the outbound 訂正 corrections TG330B wrote) and
      * DISPLAY each record as: OUT|orig-center-seq|correction-type|
      * amt-yen|signature  (grader parses these). Harness-only.
      *================================================================
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TGOUTCF ASSIGN TO "TGOUTCF"
               ORGANIZATION IS SEQUENTIAL FILE STATUS IS WS-ST.
       DATA DIVISION.
       FILE SECTION.
       FD  TGOUTCF.
       COPY TGOUTCFC.
       WORKING-STORAGE SECTION.
       01  WS-ST   PIC X(02).
       01  WS-EOF  PIC X(01) VALUE 'N'.
       01  WS-AMT  PIC 9(11).
       PROCEDURE DIVISION.
       000-MAIN.
           OPEN INPUT TGOUTCF
           IF WS-ST NOT = "00"
               DISPLAY "DUMPOUT OPEN ST=" WS-ST
               STOP RUN
           END-IF
           PERFORM UNTIL WS-EOF = 'Y'
               READ TGOUTCF AT END MOVE 'Y' TO WS-EOF
               NOT AT END
                   MOVE OC-OUT-AMT TO WS-AMT
                   DISPLAY "OUT|" FUNCTION TRIM(OC-ORIG-CENTER-SEQ)
                           "|" FUNCTION TRIM(OC-CORRECTION-TYPE)
                           "|" WS-AMT
                           "|" FUNCTION TRIM(OC-OC-SIGNATURE)
               END-READ
           END-PERFORM
           CLOSE TGOUTCF
           STOP RUN.
