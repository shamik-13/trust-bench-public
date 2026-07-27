       IDENTIFICATION DIVISION.
       PROGRAM-ID. KZ570B.
      ******************************************************************
      * CHANGE HISTORY
      * 1.00  20240115  KZDEV   INITIAL VERSION
      * 1.01  20240708  KZMAINT ADD MISMATCH REASONS
      * 1.02  20250220  KZMAINT ADD CONTROL TOTAL AND HASH TOTAL
      ******************************************************************
      * CREATE BALANCE CERTIFICATES BY MATCHING KZDLQF AND KZDLRF
      * ON ACCOUNT NUMBER, THEN WRITE THE CERTIFICATES TO SYSOUT.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZDLQF
               ASSIGN TO "KZDLQF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-DQ-STATUS.
           SELECT KZDLRF
               ASSIGN TO "KZDLRF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-DR-STATUS.
           SELECT SYSIN-FILE
               ASSIGN TO "SYSIN"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-SYSIN-STATUS.
           SELECT SYSOUT-FILE
               ASSIGN TO "SYSOUT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-SYSOUT-STATUS.
           SELECT SORT-FILE
               ASSIGN TO SORTWK.

       DATA DIVISION.
       FILE SECTION.
       FD  KZDLQF.
       COPY KZDLQFC.

       FD  KZDLRF.
       COPY KZDLRFC.

       FD  SYSIN-FILE.
       01  SYSIN-REC                 PIC X(80).

       FD  SYSOUT-FILE.
       01  SYSOUT-REC                PIC X(132).

       SD  SORT-FILE.
       01  SORT-REC.
           05 SR-BRANCH              PIC X(03).
           05 SR-ACCT-NO             PIC X(12).
           05 SR-ASOF-DT             PIC 9(08).
           05 SR-OVERDUE-AMT         PIC S9(13)V99.
           05 SR-LATE-CHARGE-AMT     PIC S9(11)V99.
           05 SR-DAYS-OVERDUE        PIC 9(05).
           05 SR-CURR-STATUS         PIC X(02).
           05 SR-NEW-STATUS          PIC X(02).
           05 SR-AGING-BUCKET        PIC X(02).

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05 WS-DQ-STATUS           PIC X(02) VALUE SPACE.
           05 WS-DR-STATUS           PIC X(02) VALUE SPACE.
           05 WS-SYSIN-STATUS        PIC X(02) VALUE SPACE.
           05 WS-SYSOUT-STATUS       PIC X(02) VALUE SPACE.

       01  WS-END-FLAGS.
           05 WS-DQ-EOF              PIC X VALUE 'N'.
              88 DQ-EOF                    VALUE 'Y'.
              88 DQ-NOT-EOF                VALUE 'N'.
           05 WS-DR-EOF              PIC X VALUE 'N'.
              88 DR-EOF                    VALUE 'Y'.
              88 DR-NOT-EOF                VALUE 'N'.

       01  WS-CONTROL.
           05 WS-CTL-TEXT            PIC X(80) VALUE SPACE.
           05 WS-CTL-TOTAL           PIC S9(15)V99 VALUE ZERO.
           05 WS-OUT-TOTAL           PIC S9(15)V99 VALUE ZERO.
           05 WS-HASH-TOTAL          PIC 9(18) VALUE ZERO.
           05 WS-REC-COUNT           PIC 9(09) VALUE ZERO.
           05 WS-DIFF-AMT            PIC S9(15)V99 VALUE ZERO.

       01  WS-WORK.
           05 WS-ACCT-NUM            PIC 9(12) VALUE ZERO.
           05 WS-HASH-MAX            PIC 9(18)
                                      VALUE 999999999999999999.
           05 WS-DATE-WORK           PIC X(08) VALUE SPACE.
           05 WS-VALID-SW            PIC X VALUE 'Y'.
              88 WS-VALID                  VALUE 'Y'.
              88 WS-INVALID                VALUE 'N'.

       01  WS-EDIT.
           05 WE-OVERDUE             PIC ZZZ,ZZZ,ZZZ,ZZ9.99.
           05 WE-LATE-CHARGE         PIC ZZZ,ZZZ,ZZ9.99.
           05 WE-DAYS                PIC ZZ,ZZ9.
           05 WE-TOTAL               PIC ZZZ,ZZZ,ZZZ,ZZ9.99.
           05 WE-COUNT               PIC ZZZ,ZZZ,ZZ9.
           05 WE-HASH                PIC ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.
           05 WE-DATE.
              10 WE-DATE-YYYY        PIC X(04).
              10 FILLER              PIC X VALUE '/'.
              10 WE-DATE-MM          PIC X(02).
              10 FILLER              PIC X VALUE '/'.
              10 WE-DATE-DD          PIC X(02).

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE 0 TO RETURN-CODE
           PERFORM 1000-OPEN-FILES
           IF RETURN-CODE NOT = 0
              GOBACK
           END-IF

           PERFORM 1100-READ-SYSIN
           IF RETURN-CODE NOT = 0
              PERFORM 9000-CLOSE-FILES
              GOBACK
           END-IF

           SORT SORT-FILE
               ON ASCENDING KEY SR-BRANCH SR-ACCT-NO
               INPUT PROCEDURE 2000-BUILD-SORT
               OUTPUT PROCEDURE 5000-WRITE-REPORT

           IF RETURN-CODE = 0
              PERFORM 7000-CHECK-CONTROL
           END-IF

           PERFORM 9000-CLOSE-FILES
           GOBACK.

       1000-OPEN-FILES.
           OPEN INPUT KZDLQF
           IF WS-DQ-STATUS NOT = '00'
              DISPLAY 'KZDLQF OPEN FAILED ST=' WS-DQ-STATUS
              MOVE 12 TO RETURN-CODE
              EXIT PARAGRAPH
           END-IF

           OPEN INPUT KZDLRF
           IF WS-DR-STATUS NOT = '00'
              DISPLAY 'KZDLRF OPEN FAILED ST=' WS-DR-STATUS
              MOVE 12 TO RETURN-CODE
              EXIT PARAGRAPH
           END-IF

           OPEN INPUT SYSIN-FILE
           IF WS-SYSIN-STATUS NOT = '00'
              DISPLAY 'SYSIN OPEN FAILED ST=' WS-SYSIN-STATUS
              MOVE 12 TO RETURN-CODE
              EXIT PARAGRAPH
           END-IF

           OPEN OUTPUT SYSOUT-FILE
           IF WS-SYSOUT-STATUS NOT = '00'
              DISPLAY 'SYSOUT OPEN FAILED ST=' WS-SYSOUT-STATUS
              MOVE 12 TO RETURN-CODE
           END-IF.

       1100-READ-SYSIN.
           READ SYSIN-FILE
              AT END
                 DISPLAY 'SYSIN CONTROL TOTAL MISSING'
                 MOVE 8 TO RETURN-CODE
              NOT AT END
                 IF WS-SYSIN-STATUS NOT = '00'
                    DISPLAY 'SYSIN READ FAILED ST=' WS-SYSIN-STATUS
                    MOVE 12 TO RETURN-CODE
                 ELSE
                    MOVE SYSIN-REC TO WS-CTL-TEXT
                    COMPUTE WS-CTL-TOTAL =
                        FUNCTION NUMVAL(WS-CTL-TEXT)
                    END-COMPUTE
                 END-IF
           END-READ.

       2000-BUILD-SORT.
           PERFORM 2100-READ-DQ
           PERFORM 2200-READ-DR

           PERFORM UNTIL DQ-EOF AND DR-EOF
              EVALUATE TRUE
                 WHEN DQ-EOF
                    DISPLAY 'KZDLRF UNMATCHED ACCOUNT=' DR-ACCT-NO
                    MOVE 8 TO RETURN-CODE
                    PERFORM 2200-READ-DR
                 WHEN DR-EOF
                    DISPLAY 'KZDLQF UNMATCHED ACCOUNT=' DQ-ACCT-NO
                    MOVE 8 TO RETURN-CODE
                    PERFORM 2100-READ-DQ
                 WHEN DQ-ACCT-NO = DR-ACCT-NO
                    PERFORM 3000-VALIDATE-PAIR
                    IF WS-VALID
                       PERFORM 3100-RELEASE-CERT
                    ELSE
                       MOVE 8 TO RETURN-CODE
                    END-IF
                    PERFORM 2100-READ-DQ
                    PERFORM 2200-READ-DR
                 WHEN DQ-ACCT-NO < DR-ACCT-NO
                    DISPLAY 'KZDLRF NOT FOUND ACCOUNT=' DQ-ACCT-NO
                    MOVE 8 TO RETURN-CODE
                    PERFORM 2100-READ-DQ
                 WHEN OTHER
                    DISPLAY 'KZDLQF NOT FOUND ACCOUNT=' DR-ACCT-NO
                    MOVE 8 TO RETURN-CODE
                    PERFORM 2200-READ-DR
              END-EVALUATE
           END-PERFORM.

       2100-READ-DQ.
           READ KZDLQF
              AT END
                 SET DQ-EOF TO TRUE
              NOT AT END
                 IF WS-DQ-STATUS NOT = '00'
                    DISPLAY 'KZDLQF READ FAILED ST=' WS-DQ-STATUS
                    MOVE 12 TO RETURN-CODE
                    SET DQ-EOF TO TRUE
                 END-IF
           END-READ.

       2200-READ-DR.
           READ KZDLRF
              AT END
                 SET DR-EOF TO TRUE
              NOT AT END
                 IF WS-DR-STATUS NOT = '00'
                    DISPLAY 'KZDLRF READ FAILED ST=' WS-DR-STATUS
                    MOVE 12 TO RETURN-CODE
                    SET DR-EOF TO TRUE
                 END-IF
           END-READ.

       3000-VALIDATE-PAIR.
           SET WS-VALID TO TRUE

           IF DQ-CURR-STATUS NOT = '00'
              AND DQ-CURR-STATUS NOT = '10'
              AND DQ-CURR-STATUS NOT = '30'
              DISPLAY 'INVALID CURRENT STATUS ACCOUNT=' DQ-ACCT-NO
              SET WS-INVALID TO TRUE
           END-IF

           IF DR-NEW-STATUS NOT = '00'
              AND DR-NEW-STATUS NOT = '10'
              AND DR-NEW-STATUS NOT = '30'
              DISPLAY 'INVALID NEW STATUS ACCOUNT=' DR-ACCT-NO
              SET WS-INVALID TO TRUE
           END-IF

           IF DR-AGING-BUCKET NOT = 'B1'
              AND DR-AGING-BUCKET NOT = 'B2'
              AND DR-AGING-BUCKET NOT = 'B3'
              AND DR-AGING-BUCKET NOT = 'B4'
              DISPLAY 'INVALID AGING BUCKET ACCOUNT=' DR-ACCT-NO
              SET WS-INVALID TO TRUE
           END-IF

           IF DQ-OVERDUE-AMT < ZERO
              DISPLAY 'INVALID OVERDUE AMOUNT ACCOUNT=' DQ-ACCT-NO
              SET WS-INVALID TO TRUE
           END-IF

           IF DR-LATE-CHARGE-AMT < ZERO
              DISPLAY 'INVALID LATE CHARGE ACCOUNT=' DR-ACCT-NO
              SET WS-INVALID TO TRUE
           END-IF

           IF DR-DAYS-OVERDUE = ZERO
              AND DQ-CURR-STATUS = '10'
              DISPLAY 'DAYS OVERDUE MISMATCH ACCOUNT=' DR-ACCT-NO
              SET WS-INVALID TO TRUE
           END-IF

           IF DR-DAYS-OVERDUE > 180
              AND DR-AGING-BUCKET NOT = 'B4'
              DISPLAY 'AGING DAYS MISMATCH ACCOUNT=' DR-ACCT-NO
              SET WS-INVALID TO TRUE
           END-IF

           IF DR-AGING-BUCKET = 'B4'
              AND DR-NEW-STATUS NOT = '30'
              DISPLAY 'COLLECTION STATUS MISMATCH ACCOUNT=' DR-ACCT-NO
              SET WS-INVALID TO TRUE
           END-IF.

       3100-RELEASE-CERT.
           MOVE SPACES TO SORT-REC
           MOVE DQ-ACCT-NO(1:3) TO SR-BRANCH
           MOVE DQ-ACCT-NO TO SR-ACCT-NO
           MOVE DQ-ASOF-DT TO SR-ASOF-DT
           MOVE DQ-OVERDUE-AMT TO SR-OVERDUE-AMT
           MOVE DR-LATE-CHARGE-AMT TO SR-LATE-CHARGE-AMT
           MOVE DR-DAYS-OVERDUE TO SR-DAYS-OVERDUE
           MOVE DQ-CURR-STATUS TO SR-CURR-STATUS
           MOVE DR-NEW-STATUS TO SR-NEW-STATUS
           MOVE DR-AGING-BUCKET TO SR-AGING-BUCKET

           ADD SR-OVERDUE-AMT TO WS-OUT-TOTAL
           ADD 1 TO WS-REC-COUNT

           COMPUTE WS-ACCT-NUM = FUNCTION NUMVAL(SR-ACCT-NO)
           END-COMPUTE
           ADD WS-ACCT-NUM TO WS-HASH-TOTAL
           IF WS-HASH-TOTAL > WS-HASH-MAX
              SUBTRACT WS-HASH-MAX FROM WS-HASH-TOTAL
           END-IF

           RELEASE SORT-REC.

       5000-WRITE-REPORT.
           PERFORM 5100-WRITE-TITLE

           RETURN SORT-FILE
              AT END
                 CONTINUE
              NOT AT END
                 PERFORM UNTIL RETURN-CODE NOT = 0
                    PERFORM 5200-WRITE-CERT
                    RETURN SORT-FILE
                       AT END
                          EXIT PERFORM
                       NOT AT END
                          CONTINUE
                    END-RETURN
                 END-PERFORM
           END-RETURN

           IF RETURN-CODE = 0
              PERFORM 5300-WRITE-TRAILER
           END-IF.

       5100-WRITE-TITLE.
           MOVE ALL '=' TO SYSOUT-REC
           PERFORM 8000-WRITE-LINE

           MOVE SPACES TO SYSOUT-REC
           STRING 'BALANCE CERTIFICATE BATCH KZ570B'
                  DELIMITED BY SIZE
             INTO SYSOUT-REC
           END-STRING
           PERFORM 8000-WRITE-LINE

           MOVE ALL '=' TO SYSOUT-REC
           PERFORM 8000-WRITE-LINE.

       5200-WRITE-CERT.
           MOVE SR-ASOF-DT TO WS-DATE-WORK
           MOVE WS-DATE-WORK(1:4) TO WE-DATE-YYYY
           MOVE WS-DATE-WORK(5:2) TO WE-DATE-MM
           MOVE WS-DATE-WORK(7:2) TO WE-DATE-DD
           MOVE SR-OVERDUE-AMT TO WE-OVERDUE
           MOVE SR-LATE-CHARGE-AMT TO WE-LATE-CHARGE
           MOVE SR-DAYS-OVERDUE TO WE-DAYS

           MOVE SPACES TO SYSOUT-REC
           STRING 'HEADER  BRANCH=' SR-BRANCH
                  '  ACCOUNT=' SR-ACCT-NO
                  '  CERT-DATE=' WE-DATE
                  DELIMITED BY SIZE
             INTO SYSOUT-REC
           END-STRING
           PERFORM 8000-WRITE-LINE

           MOVE SPACES TO SYSOUT-REC
           STRING 'DETAIL  OVERDUE=' WE-OVERDUE
                  '  LATE-CHARGE=' WE-LATE-CHARGE
                  '  DAYS=' WE-DAYS
                  '  STATUS=' SR-CURR-STATUS '/' SR-NEW-STATUS
                  '  AGING=' SR-AGING-BUCKET
                  DELIMITED BY SIZE
             INTO SYSOUT-REC
           END-STRING
           PERFORM 8000-WRITE-LINE

           MOVE SPACES TO SYSOUT-REC
           STRING 'SEAL    OFFICIAL SEAL POSITION'
                  DELIMITED BY SIZE
             INTO SYSOUT-REC
           END-STRING
           PERFORM 8000-WRITE-LINE

           MOVE SPACES TO SYSOUT-REC
           PERFORM 8000-WRITE-LINE.

       5300-WRITE-TRAILER.
           MOVE WS-REC-COUNT TO WE-COUNT
           MOVE WS-OUT-TOTAL TO WE-TOTAL
           MOVE WS-HASH-TOTAL TO WE-HASH

           MOVE ALL '-' TO SYSOUT-REC
           PERFORM 8000-WRITE-LINE

           MOVE SPACES TO SYSOUT-REC
           STRING 'TRAILER COUNT=' WE-COUNT
                  '  OVERDUE-TOTAL=' WE-TOTAL
                  '  HASH-TOTAL=' WE-HASH
                  DELIMITED BY SIZE
             INTO SYSOUT-REC
           END-STRING
           PERFORM 8000-WRITE-LINE.

       7000-CHECK-CONTROL.
           COMPUTE WS-DIFF-AMT = WS-OUT-TOTAL - WS-CTL-TOTAL
           END-COMPUTE

           IF WS-DIFF-AMT NOT = ZERO
              DISPLAY 'CONTROL TOTAL MISMATCH'
              DISPLAY 'SYSIN TOTAL=' WS-CTL-TOTAL
              DISPLAY 'OUTPUT TOTAL=' WS-OUT-TOTAL
              MOVE 16 TO RETURN-CODE
           ELSE
              MOVE 0 TO RETURN-CODE
           END-IF.

       8000-WRITE-LINE.
           WRITE SYSOUT-REC
           IF WS-SYSOUT-STATUS NOT = '00'
              DISPLAY 'SYSOUT WRITE FAILED ST=' WS-SYSOUT-STATUS
              MOVE 12 TO RETURN-CODE
           END-IF.

       9000-CLOSE-FILES.
           CLOSE KZDLQF
           IF WS-DQ-STATUS NOT = '00'
              DISPLAY 'KZDLQF CLOSE FAILED ST=' WS-DQ-STATUS
              IF RETURN-CODE = 0
                 MOVE 12 TO RETURN-CODE
              END-IF
           END-IF

           CLOSE KZDLRF
           IF WS-DR-STATUS NOT = '00'
              DISPLAY 'KZDLRF CLOSE FAILED ST=' WS-DR-STATUS
              IF RETURN-CODE = 0
                 MOVE 12 TO RETURN-CODE
              END-IF
           END-IF

           CLOSE SYSIN-FILE
           IF WS-SYSIN-STATUS NOT = '00'
              DISPLAY 'SYSIN CLOSE FAILED ST=' WS-SYSIN-STATUS
              IF RETURN-CODE = 0
                 MOVE 12 TO RETURN-CODE
              END-IF
           END-IF

           CLOSE SYSOUT-FILE
           IF WS-SYSOUT-STATUS NOT = '00'
              DISPLAY 'SYSOUT CLOSE FAILED ST=' WS-SYSOUT-STATUS
              IF RETURN-CODE = 0
                 MOVE 12 TO RETURN-CODE
              END-IF
           END-IF.
