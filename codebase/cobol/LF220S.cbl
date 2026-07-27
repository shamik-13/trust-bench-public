       IDENTIFICATION DIVISION.
       PROGRAM-ID. LF220S.
      *---------------------------------------------------------------*
      * LF220S                                                        *
      *---------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LFPOLF ASSIGN TO "LFPOLF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-LFPOLF-ST.
           SELECT LFCNTF ASSIGN TO "LFCNTF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS CN-POL-NO
               FILE STATUS IS WS-LFCNTF-ST.

       DATA DIVISION.
       FILE SECTION.
       FD  LFPOLF.
           COPY LFPOLFC.

       FD  LFCNTF.
           COPY LFCNTFC.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05 WS-LFPOLF-ST             PIC X(02) VALUE SPACE.
           05 WS-LFCNTF-ST             PIC X(02) VALUE SPACE.

       01  WS-SWITCHES.
           05 WS-LFPOLF-EOF-SW         PIC X VALUE 'N'.
              88 LFPOLF-EOF                 VALUE 'Y'.
              88 LFPOLF-NOT-EOF             VALUE 'N'.
           05 WS-FOUND-SW              PIC X VALUE 'N'.
              88 CONTRACT-FOUND             VALUE 'Y'.
              88 CONTRACT-NOT-FOUND         VALUE 'N'.

       01  WS-WORK-AREA.
           05 WS-TODAY-DATE            PIC 9(08) VALUE ZERO.
           05 WS-TODAY-YM              PIC 9(06) VALUE ZERO.
           05 WS-DUE-YM                PIC 9(06) VALUE ZERO.
           05 WS-MATURITY-DATE         PIC 9(08) VALUE ZERO.

       01  WS-CONSTANTS.
           05 WS-ST-NORMAL             PIC X(02) VALUE '00'.
           05 WS-ST-EOF                PIC X(02) VALUE '10'.
           05 WS-ST-NOTFOUND           PIC X(02) VALUE '23'.
           05 WS-RC-NORMAL             PIC S9(04) COMP VALUE 0.
           05 WS-RC-ERROR              PIC S9(04) COMP VALUE 8.
           05 WS-RC-ABEND              PIC S9(04) COMP VALUE 12.

       LINKAGE SECTION.
       01  LK-LF220S-PARM.
           05 LK-POL-NO                PIC X(12).
           05 LK-VALID-FLG             PIC X.
              88 LK-CONTRACT-VALID          VALUE 'Y'.
              88 LK-CONTRACT-INVALID        VALUE 'N'.
           05 LK-RETURN-KBN            PIC X(02).
              88 LK-RTN-NORMAL              VALUE '00'.
              88 LK-RTN-NOT-FOUND           VALUE '04'.
              88 LK-RTN-LAPSED              VALUE '10'.
              88 LK-RTN-CANCELLED           VALUE '11'.
              88 LK-RTN-MATURED             VALUE '12'.
              88 LK-RTN-DUE-EXPIRED         VALUE '13'.
              88 LK-RTN-PENDING             VALUE '14'.
              88 LK-RTN-PARM-ERROR          VALUE '20'.
              88 LK-RTN-IO-ERROR            VALUE '90'.
           05 LK-AGE-BAND              PIC X(02).
           05 LK-REASON-TEXT           PIC X(40).

       PROCEDURE DIVISION USING LK-LF220S-PARM.
       0000-MAIN SECTION.
       0000-START.
           PERFORM 1000-INITIALIZE
           PERFORM 2000-VALIDATE-PARM
           IF LK-RTN-PARM-ERROR
               MOVE WS-RC-ERROR TO RETURN-CODE
               GOBACK
           END-IF

           PERFORM 3000-OPEN-FILES
           IF LK-RTN-IO-ERROR
               MOVE WS-RC-ABEND TO RETURN-CODE
               GOBACK
           END-IF

           PERFORM 4000-READ-POLICY
           IF LK-RTN-IO-ERROR
               PERFORM 8000-CLOSE-FILES
               MOVE WS-RC-ABEND TO RETURN-CODE
               GOBACK
           END-IF

           IF CONTRACT-FOUND
               PERFORM 5000-READ-CONTRACT
               IF NOT LK-RTN-IO-ERROR
                   PERFORM 6000-JUDGE-STATUS
               END-IF
           ELSE
               SET LK-RTN-NOT-FOUND TO TRUE
               SET LK-CONTRACT-INVALID TO TRUE
               MOVE 'POLICY NOT FOUND' TO LK-REASON-TEXT
           END-IF

           PERFORM 8000-CLOSE-FILES

           IF LK-RTN-IO-ERROR
               MOVE WS-RC-ABEND TO RETURN-CODE
           ELSE
               MOVE WS-RC-NORMAL TO RETURN-CODE
           END-IF
           GOBACK.

       1000-INITIALIZE SECTION.
       1000-START.
           SET LK-CONTRACT-INVALID TO TRUE
           MOVE '99' TO LK-RETURN-KBN
           MOVE SPACE TO LK-AGE-BAND
           MOVE SPACE TO LK-REASON-TEXT
           SET CONTRACT-NOT-FOUND TO TRUE
           SET LFPOLF-NOT-EOF TO TRUE
           MOVE FUNCTION CURRENT-DATE(1:8) TO WS-TODAY-DATE
           MOVE WS-TODAY-DATE(1:6) TO WS-TODAY-YM.

       2000-VALIDATE-PARM SECTION.
       2000-START.
           IF LK-POL-NO = SPACE
               SET LK-RTN-PARM-ERROR TO TRUE
               SET LK-CONTRACT-INVALID TO TRUE
               MOVE 'POLICY NUMBER REQUIRED' TO LK-REASON-TEXT
               DISPLAY 'LF220S PARAMETER ERROR'
           END-IF.

       3000-OPEN-FILES SECTION.
       3000-START.
           OPEN INPUT LFPOLF
           IF WS-LFPOLF-ST NOT = WS-ST-NORMAL
               SET LK-RTN-IO-ERROR TO TRUE
               MOVE 'LFPOLF OPEN ERROR' TO LK-REASON-TEXT
               DISPLAY 'LFPOLF OPEN ERROR'
               DISPLAY WS-LFPOLF-ST
               EXIT SECTION
           END-IF

           OPEN INPUT LFCNTF
           IF WS-LFCNTF-ST NOT = WS-ST-NORMAL
               SET LK-RTN-IO-ERROR TO TRUE
               MOVE 'LFCNTF OPEN ERROR' TO LK-REASON-TEXT
               DISPLAY 'LFCNTF OPEN ERROR'
               DISPLAY WS-LFCNTF-ST
               CLOSE LFPOLF
           END-IF.

       4000-READ-POLICY SECTION.
       4000-START.
           PERFORM UNTIL LFPOLF-EOF OR CONTRACT-FOUND
               READ LFPOLF
                   AT END
                       SET LFPOLF-EOF TO TRUE
                   NOT AT END
                       IF PO-POL-NO = LK-POL-NO
                           SET CONTRACT-FOUND TO TRUE
                       END-IF
               END-READ

               IF WS-LFPOLF-ST NOT = WS-ST-NORMAL
                  AND WS-LFPOLF-ST NOT = WS-ST-EOF
                   SET LK-RTN-IO-ERROR TO TRUE
                   MOVE 'LFPOLF READ ERROR' TO LK-REASON-TEXT
                   DISPLAY 'LFPOLF READ ERROR'
                   DISPLAY WS-LFPOLF-ST
                   SET LFPOLF-EOF TO TRUE
               END-IF
           END-PERFORM.

       5000-READ-CONTRACT SECTION.
       5000-START.
           MOVE LK-POL-NO TO CN-POL-NO
           READ LFCNTF
               INVALID KEY
                   SET LK-RTN-NOT-FOUND TO TRUE
                   SET LK-CONTRACT-INVALID TO TRUE
                   MOVE 'CONTRACT DETAIL NOT FOUND' TO LK-REASON-TEXT
               NOT INVALID KEY
                   CONTINUE
           END-READ

           IF WS-LFCNTF-ST NOT = WS-ST-NORMAL
              AND WS-LFCNTF-ST NOT = WS-ST-NOTFOUND
               SET LK-RTN-IO-ERROR TO TRUE
               MOVE 'LFCNTF READ ERROR' TO LK-REASON-TEXT
               DISPLAY 'LFCNTF READ ERROR'
               DISPLAY WS-LFCNTF-ST
           END-IF.

       6000-JUDGE-STATUS SECTION.
       6000-START.
           PERFORM 6100-SET-AGE-BAND
           MOVE CN-NEXT-DUE-YM TO WS-DUE-YM
           MOVE CN-MATURITY-DATE TO WS-MATURITY-DATE

           EVALUATE TRUE
               WHEN PO-POL-STATUS-KBN = '09'
                   SET LK-RTN-CANCELLED TO TRUE
                   SET LK-CONTRACT-INVALID TO TRUE
                   MOVE 'CANCELLED POLICY' TO LK-REASON-TEXT
               WHEN PO-POL-STATUS-KBN = '02'
                   SET LK-RTN-LAPSED TO TRUE
                   SET LK-CONTRACT-INVALID TO TRUE
                   MOVE 'LAPSED POLICY' TO LK-REASON-TEXT
               WHEN WS-MATURITY-DATE > ZERO
                AND WS-MATURITY-DATE <= WS-TODAY-DATE
                   SET LK-RTN-MATURED TO TRUE
                   SET LK-CONTRACT-INVALID TO TRUE
                   MOVE 'MATURED POLICY' TO LK-REASON-TEXT
               WHEN PO-POL-STATUS-KBN = '01'
                AND WS-DUE-YM > ZERO
                AND WS-DUE-YM < WS-TODAY-YM
                   SET LK-RTN-DUE-EXPIRED TO TRUE
                   SET LK-CONTRACT-INVALID TO TRUE
                   MOVE 'PAYMENT DUE EXPIRED' TO LK-REASON-TEXT
               WHEN PO-POL-STATUS-KBN = '01'
                   SET LK-RTN-NORMAL TO TRUE
                   SET LK-CONTRACT-VALID TO TRUE
                   MOVE 'VALID POLICY' TO LK-REASON-TEXT
               WHEN OTHER
                   SET LK-RTN-PENDING TO TRUE
                   SET LK-CONTRACT-INVALID TO TRUE
                   MOVE 'PENDING OR UNKNOWN STATUS' TO LK-REASON-TEXT
           END-EVALUATE.

       6100-SET-AGE-BAND SECTION.
       6100-START.
           EVALUATE TRUE
               WHEN PO-ENTRY-AGE-CNT <= 29
                   MOVE 'A1' TO LK-AGE-BAND
               WHEN PO-ENTRY-AGE-CNT <= 39
                   MOVE 'A2' TO LK-AGE-BAND
               WHEN PO-ENTRY-AGE-CNT <= 49
                   MOVE 'A3' TO LK-AGE-BAND
               WHEN PO-ENTRY-AGE-CNT <= 59
                   MOVE 'A4' TO LK-AGE-BAND
               WHEN OTHER
                   MOVE 'A5' TO LK-AGE-BAND
           END-EVALUATE.

       8000-CLOSE-FILES SECTION.
       8000-START.
           CLOSE LFPOLF
           IF WS-LFPOLF-ST NOT = WS-ST-NORMAL
               SET LK-RTN-IO-ERROR TO TRUE
               MOVE 'LFPOLF CLOSE ERROR' TO LK-REASON-TEXT
               DISPLAY 'LFPOLF CLOSE ERROR'
               DISPLAY WS-LFPOLF-ST
           END-IF

           CLOSE LFCNTF
           IF WS-LFCNTF-ST NOT = WS-ST-NORMAL
               SET LK-RTN-IO-ERROR TO TRUE
               MOVE 'LFCNTF CLOSE ERROR' TO LK-REASON-TEXT
               DISPLAY 'LFCNTF CLOSE ERROR'
               DISPLAY WS-LFCNTF-ST
           END-IF.
