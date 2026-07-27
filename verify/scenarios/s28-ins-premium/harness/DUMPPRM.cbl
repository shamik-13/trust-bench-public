       IDENTIFICATION DIVISION.
       PROGRAM-ID. DUMPPRM.
      *================================================================
      * Read LFPRMF (the premium-result file the engine wrote) and
      * DISPLAY each record as: PRMR|pol-no|prem-yen|calc-status|band
      * (grader parses these). prem shown as integer yen. Harness-only.
      *================================================================
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LFPRMF ASSIGN TO "LFPRMF"
               ORGANIZATION IS SEQUENTIAL FILE STATUS IS WS-ST.
       DATA DIVISION.
       FILE SECTION.
       FD  LFPRMF.
       COPY LFPRMFC.
       WORKING-STORAGE SECTION.
       01  WS-ST        PIC X(02).
       01  WS-EOF       PIC X(01) VALUE 'N'.
       01  WS-PREM-YEN  PIC 9(13).
       PROCEDURE DIVISION.
       000-MAIN.
           OPEN INPUT LFPRMF
           IF WS-ST NOT = "00"
               DISPLAY "DUMPPRM OPEN ST=" WS-ST
               STOP RUN
           END-IF
           PERFORM UNTIL WS-EOF = 'Y'
               READ LFPRMF AT END MOVE 'Y' TO WS-EOF
               NOT AT END
                   MOVE PR-PRM-AMT TO WS-PREM-YEN
                   DISPLAY "PRMR|" FUNCTION TRIM(PR-POL-NO)
                           "|" WS-PREM-YEN
                           "|" FUNCTION TRIM(PR-CALC-STATUS-KBN)
                           "|" FUNCTION TRIM(PR-BAND-KBN)
               END-READ
           END-PERFORM
           CLOSE LFPRMF
           STOP RUN.
