       IDENTIFICATION DIVISION.
       PROGRAM-ID. DUMPCLR.
      *================================================================
      * Read TGCLRF (the 交換尻 the engine wrote) and DISPLAY each record
      * as: CLR|counter-bank|net-amt|item-count|status (grader parses
      * these). NET-AMT is signed; emit it via SIGN LEADING SEPARATE so
      * the sign survives. Harness-only.
      *================================================================
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TGCLRF ASSIGN TO "TGCLRF"
               ORGANIZATION IS SEQUENTIAL FILE STATUS IS WS-ST.
       DATA DIVISION.
       FILE SECTION.
       FD  TGCLRF.
       COPY TGCLRFC.
       WORKING-STORAGE SECTION.
       01  WS-ST   PIC X(02).
       01  WS-EOF  PIC X(01) VALUE 'N'.
       01  WS-NET-DISP  PIC S9(13) SIGN IS LEADING SEPARATE.
       01  WS-CNT-DISP  PIC 9(08).
       PROCEDURE DIVISION.
       000-MAIN.
           OPEN INPUT TGCLRF
           IF WS-ST NOT = "00"
               DISPLAY "DUMPCLR OPEN ST=" WS-ST
               STOP RUN
           END-IF
           PERFORM UNTIL WS-EOF = 'Y'
               READ TGCLRF AT END MOVE 'Y' TO WS-EOF
               NOT AT END
                   MOVE CL-NET-AMT TO WS-NET-DISP
                   MOVE CL-ITEM-COUNT TO WS-CNT-DISP
                   DISPLAY "CLR|" FUNCTION TRIM(CL-COUNTER-BANK)
                           "|" FUNCTION TRIM(WS-NET-DISP)
                           "|" FUNCTION TRIM(WS-CNT-DISP)
                           "|" FUNCTION TRIM(CL-SETTLE-STATUS)
               END-READ
           END-PERFORM
           CLOSE TGCLRF
           STOP RUN.
