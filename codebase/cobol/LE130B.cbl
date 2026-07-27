       IDENTIFICATION DIVISION.
       PROGRAM-ID. LE130B.
      ******************************************************************
      *  MONTHLY ACCOUNTING LINKAGE BATCH
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LEJRNF ASSIGN TO "LEJRNF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-LEJRNF-ST.
           SELECT LFMTHF ASSIGN TO "LFMTHF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-LFMTHF-ST.

       DATA DIVISION.
       FILE SECTION.
       FD  LEJRNF.
           COPY LEJRNC.
       FD  LFMTHF.
           COPY LFMTHC.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05  WS-LEJRNF-ST             PIC XX VALUE SPACE.
           05  WS-LFMTHF-ST             PIC XX VALUE SPACE.

       01  WS-CONTROL.
           05  WS-END-LEJRNF            PIC X VALUE "N".
           05  WS-END-LFMTHF            PIC X VALUE "N".
           05  WS-ERROR-SW              PIC X VALUE "N".
           05  WS-PARM-YM               PIC 9(6) VALUE ZERO.
           05  WS-WORK-YM               PIC X(6) VALUE SPACE.
           05  WS-POST-DATE             PIC 9(8) VALUE ZERO.
           05  WS-JR-POST-DATE          PIC 9(8) VALUE ZERO.
           05  WS-JR-POST-YM            PIC 9(6) VALUE ZERO.
           05  WS-RUN-DATE              PIC 9(8) VALUE ZERO.
           05  WS-RUN-TIME              PIC 9(8) VALUE ZERO.
           05  WS-JOURNAL-SEQ           PIC 9(6) VALUE ZERO.
           05  WS-ID-NUM                PIC 9(12) VALUE ZERO.
           05  WS-ID-CHAR               PIC X(12) VALUE SPACE.
           05  WS-FOUND-SW              PIC X VALUE "N".
           05  WS-I                     PIC 9(4) VALUE ZERO.
           05  WS-ENTRY-MAX             PIC 9(4) VALUE 200.
           05  WS-ENTRY-CNT             PIC 9(4) VALUE ZERO.
           05  WS-MTH-CNT               PIC 9(4) VALUE ZERO.
           05  WS-READ-JR-CNT           PIC 9(9) VALUE ZERO.
           05  WS-UPD-JR-CNT            PIC 9(9) VALUE ZERO.
           05  WS-WRITE-JR-CNT          PIC 9(9) VALUE ZERO.
           05  WS-READ-MT-CNT           PIC 9(9) VALUE ZERO.
           05  WS-SKIP-JR-CNT           PIC 9(9) VALUE ZERO.

       01  WS-WORK-AREA.
           05  WS-PRODUCT-CD            PIC X(3) VALUE SPACE.
           05  WS-DR-ACCT-CD            PIC X(10) VALUE SPACE.
           05  WS-CR-ACCT-CD            PIC X(10) VALUE SPACE.
           05  WS-JR-AMT                PIC S9(13) VALUE ZERO.
           05  WS-DIFF-AMT              PIC S9(13) VALUE ZERO.
           05  WS-ABS-AMT               PIC 9(13) VALUE ZERO.
           05  WS-EDIT-CNT              PIC ZZZ,ZZZ,ZZ9.

       01  WS-AGG-TABLE.
           05  WS-AGG-REC OCCURS 200 TIMES.
               10  AG-PRODUCT-CD        PIC X(3) VALUE SPACE.
               10  AG-DR-ACCT-CD        PIC X(10) VALUE SPACE.
               10  AG-CR-ACCT-CD        PIC X(10) VALUE SPACE.
               10  AG-JR-AMT            PIC S9(13) VALUE ZERO.
               10  AG-MT-CV-AMT         PIC S9(13) VALUE ZERO.
               10  AG-JR-CNT            PIC 9(9) VALUE ZERO.
               10  AG-MT-CNT            PIC 9(9) VALUE ZERO.

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE 0 TO RETURN-CODE
           PERFORM 1000-INITIALIZE
           IF WS-ERROR-SW = "N"
               PERFORM 2000-READ-MONTHLY
           END-IF
           IF WS-ERROR-SW = "N"
               PERFORM 3000-UPDATE-JOURNAL
           END-IF
           IF WS-ERROR-SW = "N"
               PERFORM 4000-WRITE-SUMMARY
           END-IF
           PERFORM 9000-FINALIZE
           GOBACK.

       1000-INITIALIZE.
           ACCEPT WS-WORK-YM FROM ENVIRONMENT "LE130B_YM"
           ACCEPT WS-RUN-DATE FROM DATE YYYYMMDD
           ACCEPT WS-RUN-TIME FROM TIME
           IF WS-WORK-YM NOT NUMERIC
               DISPLAY "BAD YM LE130B_YM=" WS-WORK-YM
               MOVE 8 TO RETURN-CODE
               MOVE "Y" TO WS-ERROR-SW
           ELSE
               MOVE WS-WORK-YM TO WS-PARM-YM
               IF WS-PARM-YM < 200001 OR WS-PARM-YM > 209912
                   DISPLAY "BAD YM RANGE LE130B_YM=" WS-WORK-YM
                   MOVE 8 TO RETURN-CODE
                   MOVE "Y" TO WS-ERROR-SW
               ELSE
                   COMPUTE WS-POST-DATE = WS-PARM-YM * 100 + 28
                   DISPLAY "LE130B START YM=" WS-PARM-YM
               END-IF
           END-IF.

       2000-READ-MONTHLY.
           OPEN INPUT LFMTHF
           IF WS-LFMTHF-ST NOT = "00"
               DISPLAY "LFMTHF OPEN ERROR ST=" WS-LFMTHF-ST
               MOVE 8 TO RETURN-CODE
               MOVE "Y" TO WS-ERROR-SW
           ELSE
               PERFORM UNTIL WS-END-LFMTHF = "Y"
                   READ LFMTHF
                       AT END
                           MOVE "Y" TO WS-END-LFMTHF
                       NOT AT END
                           ADD 1 TO WS-READ-MT-CNT
                           PERFORM 2100-EDIT-MONTHLY
                   END-READ
               END-PERFORM
               CLOSE LFMTHF
               IF WS-LFMTHF-ST NOT = "00"
                   DISPLAY "LFMTHF CLOSE ERROR ST=" WS-LFMTHF-ST
                   MOVE 8 TO RETURN-CODE
                   MOVE "Y" TO WS-ERROR-SW
               END-IF
           END-IF.

       2100-EDIT-MONTHLY.
           IF MT-SUMMARY-YM = WS-PARM-YM
               IF MT-PRODUCT-CD = SPACE
                   DISPLAY "MONTHLY PRODUCT ERROR"
                   MOVE 8 TO RETURN-CODE
                   MOVE "Y" TO WS-ERROR-SW
               ELSE
                   IF MT-CV-TOTAL-AMT < ZERO
                       DISPLAY "MONTHLY CV AMT ERROR PROD="
                               MT-PRODUCT-CD
                       MOVE 8 TO RETURN-CODE
                       MOVE "Y" TO WS-ERROR-SW
                   ELSE
                       MOVE MT-PRODUCT-CD TO WS-PRODUCT-CD
                       MOVE "2110000000" TO WS-DR-ACCT-CD
                       MOVE "1110000000" TO WS-CR-ACCT-CD
                       MOVE MT-CV-TOTAL-AMT TO WS-JR-AMT
                       PERFORM 2200-ADD-MONTHLY
                   END-IF
               END-IF
           END-IF.

       2200-ADD-MONTHLY.
           PERFORM 2300-FIND-AGG
           IF WS-FOUND-SW = "N"
               PERFORM 2400-NEW-AGG
           END-IF
           IF WS-ERROR-SW = "N"
               ADD WS-JR-AMT TO AG-MT-CV-AMT(WS-I)
               ADD 1 TO AG-MT-CNT(WS-I)
               ADD 1 TO WS-MTH-CNT
           END-IF.

       2300-FIND-AGG.
           MOVE "N" TO WS-FOUND-SW
           MOVE 1 TO WS-I
           PERFORM UNTIL WS-I > WS-ENTRY-CNT
                   OR WS-FOUND-SW = "Y"
               IF AG-PRODUCT-CD(WS-I) = WS-PRODUCT-CD
                  AND AG-DR-ACCT-CD(WS-I) = WS-DR-ACCT-CD
                  AND AG-CR-ACCT-CD(WS-I) = WS-CR-ACCT-CD
                   MOVE "Y" TO WS-FOUND-SW
               ELSE
                   ADD 1 TO WS-I
               END-IF
           END-PERFORM.

       2400-NEW-AGG.
           IF WS-ENTRY-CNT >= WS-ENTRY-MAX
               DISPLAY "AGG TABLE OVERFLOW"
               MOVE 12 TO RETURN-CODE
               MOVE "Y" TO WS-ERROR-SW
           ELSE
               ADD 1 TO WS-ENTRY-CNT
               MOVE WS-ENTRY-CNT TO WS-I
               MOVE WS-PRODUCT-CD TO AG-PRODUCT-CD(WS-I)
               MOVE WS-DR-ACCT-CD TO AG-DR-ACCT-CD(WS-I)
               MOVE WS-CR-ACCT-CD TO AG-CR-ACCT-CD(WS-I)
               MOVE ZERO TO AG-JR-AMT(WS-I)
               MOVE ZERO TO AG-MT-CV-AMT(WS-I)
               MOVE ZERO TO AG-JR-CNT(WS-I)
               MOVE ZERO TO AG-MT-CNT(WS-I)
           END-IF.

       3000-UPDATE-JOURNAL.
           OPEN I-O LEJRNF
           IF WS-LEJRNF-ST NOT = "00"
               DISPLAY "LEJRNF OPEN ERROR ST=" WS-LEJRNF-ST
               MOVE 8 TO RETURN-CODE
               MOVE "Y" TO WS-ERROR-SW
           ELSE
               PERFORM UNTIL WS-END-LEJRNF = "Y"
                   READ LEJRNF
                       AT END
                           MOVE "Y" TO WS-END-LEJRNF
                       NOT AT END
                           ADD 1 TO WS-READ-JR-CNT
                           PERFORM 3100-EDIT-JOURNAL
                   END-READ
               END-PERFORM
               CLOSE LEJRNF
               IF WS-LEJRNF-ST NOT = "00"
                   DISPLAY "LEJRNF CLOSE ERROR ST=" WS-LEJRNF-ST
                   MOVE 8 TO RETURN-CODE
                   MOVE "Y" TO WS-ERROR-SW
               END-IF
           END-IF.

       3100-EDIT-JOURNAL.
           IF JR-POST-DATE NUMERIC
               MOVE JR-POST-DATE TO WS-JR-POST-DATE
               COMPUTE WS-JR-POST-YM = WS-JR-POST-DATE / 100
               END-COMPUTE
               IF WS-JR-POST-YM = WS-PARM-YM
                   EVALUATE JR-JOURNAL-STATUS-KBN
                       WHEN "0"
                           PERFORM 3200-AGG-JOURNAL
                           IF WS-ERROR-SW = "N"
                               MOVE "1" TO JR-JOURNAL-STATUS-KBN
                               REWRITE LEJRNF-REC
                               IF WS-LEJRNF-ST = "00"
                                   ADD 1 TO WS-UPD-JR-CNT
                               ELSE
                                   DISPLAY "LEJRNF REWRITE ERROR ST="
                                           WS-LEJRNF-ST
                                   MOVE 8 TO RETURN-CODE
                                   MOVE "Y" TO WS-ERROR-SW
                               END-IF
                           END-IF
                       WHEN "1"
                           ADD 1 TO WS-SKIP-JR-CNT
                       WHEN "9"
                           ADD 1 TO WS-SKIP-JR-CNT
                       WHEN OTHER
                           DISPLAY "BAD JOURNAL STATUS ID="
                                   JR-JOURNAL-ID
                           MOVE 8 TO RETURN-CODE
                           MOVE "Y" TO WS-ERROR-SW
                   END-EVALUATE
               END-IF
           ELSE
               DISPLAY "BAD JOURNAL POST DATE ID=" JR-JOURNAL-ID
               MOVE 8 TO RETURN-CODE
               MOVE "Y" TO WS-ERROR-SW
           END-IF.

       3200-AGG-JOURNAL.
           IF JR-POL-NO = SPACE
               DISPLAY "BAD POLICY ID=" JR-JOURNAL-ID
               MOVE 8 TO RETURN-CODE
               MOVE "Y" TO WS-ERROR-SW
           ELSE
               IF JR-AMT <= ZERO
                   DISPLAY "BAD JOURNAL AMT ID=" JR-JOURNAL-ID
                   MOVE 8 TO RETURN-CODE
                   MOVE "Y" TO WS-ERROR-SW
               ELSE
                   MOVE JR-POL-NO(1:3) TO WS-PRODUCT-CD
                   MOVE JR-DR-ACCT-CD TO WS-DR-ACCT-CD
                   MOVE JR-CR-ACCT-CD TO WS-CR-ACCT-CD
                   MOVE JR-AMT TO WS-JR-AMT
                   PERFORM 2300-FIND-AGG
                   IF WS-FOUND-SW = "N"
                       PERFORM 2400-NEW-AGG
                   END-IF
                   IF WS-ERROR-SW = "N"
                       ADD WS-JR-AMT TO AG-JR-AMT(WS-I)
                       ADD 1 TO AG-JR-CNT(WS-I)
                   END-IF
               END-IF
           END-IF.

       4000-WRITE-SUMMARY.
           OPEN EXTEND LEJRNF
           IF WS-LEJRNF-ST NOT = "00"
               DISPLAY "LEJRNF EXTEND ERROR ST=" WS-LEJRNF-ST
               MOVE 8 TO RETURN-CODE
               MOVE "Y" TO WS-ERROR-SW
           ELSE
               MOVE 1 TO WS-I
               PERFORM UNTIL WS-I > WS-ENTRY-CNT
                   PERFORM 4100-WRITE-TOTAL
                   IF WS-ERROR-SW = "N"
                       PERFORM 4200-WRITE-ADJUST
                   END-IF
                   ADD 1 TO WS-I
               END-PERFORM
               CLOSE LEJRNF
               IF WS-LEJRNF-ST NOT = "00"
                   DISPLAY "LEJRNF EXT CLOSE ERROR ST="
                           WS-LEJRNF-ST
                   MOVE 8 TO RETURN-CODE
                   MOVE "Y" TO WS-ERROR-SW
               END-IF
           END-IF.

       4100-WRITE-TOTAL.
           IF AG-JR-CNT(WS-I) > ZERO
               INITIALIZE LEJRNF-REC
               PERFORM 4300-MAKE-JOURNAL-ID
               MOVE WS-ID-CHAR TO JR-JOURNAL-ID
               MOVE WS-POST-DATE TO JR-POST-DATE
               STRING AG-PRODUCT-CD(WS-I) "9999999"
                   DELIMITED BY SIZE
                   INTO JR-POL-NO
               END-STRING
               MOVE AG-DR-ACCT-CD(WS-I) TO JR-DR-ACCT-CD
               MOVE AG-CR-ACCT-CD(WS-I) TO JR-CR-ACCT-CD
               MOVE AG-JR-AMT(WS-I) TO JR-AMT
               MOVE "9" TO JR-JOURNAL-STATUS-KBN
               WRITE LEJRNF-REC
               IF WS-LEJRNF-ST = "00"
                   ADD 1 TO WS-WRITE-JR-CNT
               ELSE
                   DISPLAY "TOTAL WRITE ERROR ST=" WS-LEJRNF-ST
                   MOVE 8 TO RETURN-CODE
                   MOVE "Y" TO WS-ERROR-SW
               END-IF
           END-IF.

       4200-WRITE-ADJUST.
           COMPUTE WS-DIFF-AMT =
               AG-MT-CV-AMT(WS-I) - AG-JR-AMT(WS-I)
           END-COMPUTE
           IF WS-DIFF-AMT NOT = ZERO
               INITIALIZE LEJRNF-REC
               PERFORM 4300-MAKE-JOURNAL-ID
               MOVE WS-ID-CHAR TO JR-JOURNAL-ID
               MOVE WS-POST-DATE TO JR-POST-DATE
               STRING AG-PRODUCT-CD(WS-I) "8888888"
                   DELIMITED BY SIZE
                   INTO JR-POL-NO
               END-STRING
               IF WS-DIFF-AMT > ZERO
                   MOVE AG-DR-ACCT-CD(WS-I) TO JR-DR-ACCT-CD
                   MOVE AG-CR-ACCT-CD(WS-I) TO JR-CR-ACCT-CD
                   MOVE WS-DIFF-AMT TO JR-AMT
               ELSE
                   COMPUTE WS-ABS-AMT = WS-DIFF-AMT * -1
                   END-COMPUTE
                   MOVE AG-CR-ACCT-CD(WS-I) TO JR-DR-ACCT-CD
                   MOVE AG-DR-ACCT-CD(WS-I) TO JR-CR-ACCT-CD
                   MOVE WS-ABS-AMT TO JR-AMT
               END-IF
               MOVE "9" TO JR-JOURNAL-STATUS-KBN
               WRITE LEJRNF-REC
               IF WS-LEJRNF-ST = "00"
                   ADD 1 TO WS-WRITE-JR-CNT
               ELSE
                   DISPLAY "ADJUST WRITE ERROR ST=" WS-LEJRNF-ST
                   MOVE 8 TO RETURN-CODE
                   MOVE "Y" TO WS-ERROR-SW
               END-IF
           END-IF.

       4300-MAKE-JOURNAL-ID.
           ADD 1 TO WS-JOURNAL-SEQ
           COMPUTE WS-ID-NUM =
               WS-PARM-YM * 1000000 + WS-JOURNAL-SEQ
           END-COMPUTE
           MOVE WS-ID-NUM TO WS-ID-CHAR.

       9000-FINALIZE.
           MOVE WS-READ-JR-CNT TO WS-EDIT-CNT
           DISPLAY "LE130B READ JOURNAL COUNT=" WS-EDIT-CNT
           MOVE WS-UPD-JR-CNT TO WS-EDIT-CNT
           DISPLAY "LE130B UPDATE JOURNAL COUNT=" WS-EDIT-CNT
           MOVE WS-WRITE-JR-CNT TO WS-EDIT-CNT
           DISPLAY "LE130B WRITE JOURNAL COUNT=" WS-EDIT-CNT
           MOVE WS-READ-MT-CNT TO WS-EDIT-CNT
           DISPLAY "LE130B READ MONTHLY COUNT=" WS-EDIT-CNT
           IF WS-ERROR-SW = "N"
               MOVE 0 TO RETURN-CODE
               DISPLAY "LE130B NORMAL END"
           ELSE
               IF RETURN-CODE = 0
                   MOVE 8 TO RETURN-CODE
               END-IF
               DISPLAY "LE130B ABNORMAL END RC=" RETURN-CODE
           END-IF.
