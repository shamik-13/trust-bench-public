       IDENTIFICATION DIVISION.
       PROGRAM-ID. DUMPKEY.
      *================================================================
      * Read CMKEYF (the integration-key file the engine wrote) and
      * DISPLAY each record as: KEY|cif-no|check-digit|key-status
      * (grader parses these). Harness-only.
      *================================================================
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CMKEYF ASSIGN TO "CMKEYF"
               ORGANIZATION IS SEQUENTIAL FILE STATUS IS WS-ST.
       DATA DIVISION.
       FILE SECTION.
       FD  CMKEYF.
       COPY CMKEYFC.
       WORKING-STORAGE SECTION.
       01  WS-ST       PIC X(02).
       01  WS-EOF      PIC X(01) VALUE 'N'.
       01  WS-CD-OUT   PIC 9(02).
       PROCEDURE DIVISION.
       000-MAIN.
           OPEN INPUT CMKEYF
           IF WS-ST NOT = "00"
               DISPLAY "DUMPKEY OPEN ST=" WS-ST
               STOP RUN
           END-IF
           PERFORM UNTIL WS-EOF = 'Y'
               READ CMKEYF AT END MOVE 'Y' TO WS-EOF
               NOT AT END
                   MOVE CK-CHECK-DIGIT-CNT TO WS-CD-OUT
                   DISPLAY "KEY|" FUNCTION TRIM(CK-CIF-NO)
                           "|" WS-CD-OUT
                           "|" FUNCTION TRIM(CK-KEY-STATUS-KBN)
               END-READ
           END-PERFORM
           CLOSE CMKEYF
           STOP RUN.
