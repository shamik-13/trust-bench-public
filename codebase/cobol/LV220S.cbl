       IDENTIFICATION DIVISION.
       PROGRAM-ID. LV220S.
       AUTHOR. BATCH-GRP.
      *
      * 契約異動履歴の前後状態、異動種別、承認状態を検証する。
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LVCHGF ASSIGN TO "LVCHGF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CH-CHANGE-ID
               FILE STATUS IS WS-LVCHGF-ST.
           SELECT LFPOLF2 ASSIGN TO "LFPOLF2"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS PO-POL-NO
               FILE STATUS IS WS-LFPOLF2-ST.

       DATA DIVISION.
       FILE SECTION.
       FD  LVCHGF.
           COPY LVCHGC.
       FD  LFPOLF2.
           COPY LFPOLF2C.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05  WS-LVCHGF-ST       PIC XX VALUE SPACES.
           05  WS-LFPOLF2-ST      PIC XX VALUE SPACES.

       01  WS-END-FLAGS.
           05  WS-LVCHGF-EOF      PIC X VALUE 'N'.
               88  LVCHGF-EOF           VALUE 'Y'.
               88  LVCHGF-NOT-EOF       VALUE 'N'.

       01  WS-PREVIOUS-AREA.
           05  WS-PREV-POL-NO     PIC X(12) VALUE SPACES.
           05  WS-PREV-DATE       PIC 9(8)  VALUE ZERO.
           05  WS-PREV-STATUS     PIC X     VALUE SPACE.
           05  WS-PREV-VALID      PIC X     VALUE 'N'.
               88  PREV-VALID           VALUE 'Y'.
               88  PREV-NOT-VALID       VALUE 'N'.

       01  WS-COUNTER-AREA.
           05  WS-READ-CNT        PIC 9(9) VALUE ZERO.
           05  WS-ERR-CNT         PIC 9(9) VALUE ZERO.
           05  WS-WARN-CNT        PIC 9(9) VALUE ZERO.

       01  WS-WORK-AREA.
           05  WS-HARD-ERROR      PIC X VALUE 'N'.
               88  HARD-ERROR           VALUE 'Y'.
               88  NO-HARD-ERROR        VALUE 'N'.
           05  WS-APPROVED-FLAG   PIC X VALUE 'N'.
               88  CHANGE-APPROVED      VALUE 'Y'.
               88  CHANGE-NOT-APPROVED  VALUE 'N'.

       PROCEDURE DIVISION.
       MAIN-RTN.
           PERFORM INIT-RTN
           IF NO-HARD-ERROR
               PERFORM MAIN-PROCESS
                   UNTIL LVCHGF-EOF OR HARD-ERROR
           END-IF
           PERFORM END-RTN
           GOBACK.

       INIT-RTN.
           MOVE 0 TO RETURN-CODE
           SET LVCHGF-NOT-EOF TO TRUE
           SET PREV-NOT-VALID TO TRUE
           SET NO-HARD-ERROR TO TRUE

           OPEN INPUT LVCHGF
           IF WS-LVCHGF-ST NOT = '00'
               DISPLAY 'LVCHGF OPEN ERROR ST=' WS-LVCHGF-ST
               MOVE 8 TO RETURN-CODE
               SET HARD-ERROR TO TRUE
           END-IF

           IF NO-HARD-ERROR
               OPEN INPUT LFPOLF2
               IF WS-LFPOLF2-ST NOT = '00'
                   DISPLAY 'LFPOLF2 OPEN ERROR ST=' WS-LFPOLF2-ST
                   MOVE 8 TO RETURN-CODE
                   SET HARD-ERROR TO TRUE
               END-IF
           END-IF.

       MAIN-PROCESS.
           READ LVCHGF NEXT RECORD
               AT END
                   SET LVCHGF-EOF TO TRUE
               NOT AT END
                   IF WS-LVCHGF-ST = '00'
                       ADD 1 TO WS-READ-CNT
                       PERFORM VALIDATE-CHANGE-RTN
                   ELSE
                       DISPLAY 'LVCHGF READ ERROR ST=' WS-LVCHGF-ST
                       MOVE 8 TO RETURN-CODE
                       SET HARD-ERROR TO TRUE
                   END-IF
           END-READ.

       VALIDATE-CHANGE-RTN.
           PERFORM CHECK-REQUIRED-RTN
           IF NO-HARD-ERROR
               PERFORM CHECK-POLICY-MASTER-RTN
           END-IF
           IF NO-HARD-ERROR
               PERFORM CHECK-TIMELINE-RTN
               PERFORM CHECK-CHANGE-TYPE-RTN
               PERFORM SAVE-PREVIOUS-RTN
           END-IF.

       CHECK-REQUIRED-RTN.
           IF CH-CHANGE-ID = SPACES
               ADD 1 TO WS-ERR-CNT
               DISPLAY 'CHANGE-ID REQUIRED POL=' CH-POL-NO
           END-IF

           IF CH-POL-NO = SPACES
               ADD 1 TO WS-ERR-CNT
               DISPLAY 'POL-NO REQUIRED CHG=' CH-CHANGE-ID
           END-IF

           IF CH-CHANGE-DATE = ZERO
               ADD 1 TO WS-ERR-CNT
               DISPLAY 'CHANGE-DATE REQUIRED CHG=' CH-CHANGE-ID
           END-IF

           IF CH-BEFORE-STATUS-KBN = SPACE
               ADD 1 TO WS-ERR-CNT
               DISPLAY 'BEFORE-STATUS REQUIRED CHG=' CH-CHANGE-ID
           END-IF

           IF CH-AFTER-STATUS-KBN = SPACE
               ADD 1 TO WS-ERR-CNT
               DISPLAY 'AFTER-STATUS REQUIRED CHG=' CH-CHANGE-ID
           END-IF.

       CHECK-POLICY-MASTER-RTN.
           MOVE CH-POL-NO TO PO-POL-NO
           READ LFPOLF2
               KEY IS PO-POL-NO
               INVALID KEY
                   ADD 1 TO WS-ERR-CNT
                   DISPLAY 'POLICY NOT FOUND POL=' CH-POL-NO
               NOT INVALID KEY
                   IF WS-LFPOLF2-ST NOT = '00'
                       DISPLAY 'LFPOLF2 READ ERROR ST='
                               WS-LFPOLF2-ST
                       MOVE 8 TO RETURN-CODE
                       SET HARD-ERROR TO TRUE
                   ELSE
                       PERFORM CHECK-MASTER-DATE-RTN
                   END-IF
           END-READ.

       CHECK-MASTER-DATE-RTN.
           IF CH-CHANGE-DATE < PO-ISSUE-DATE
               ADD 1 TO WS-ERR-CNT
               DISPLAY 'CHANGE-DATE BEFORE ISSUE CHG='
                       CH-CHANGE-ID
           END-IF

           IF PO-CONTRACT-STATUS-KBN NOT = CH-AFTER-STATUS-KBN
              AND CH-CHANGE-STATUS-KBN = '2'
               ADD 1 TO WS-WARN-CNT
               DISPLAY 'MASTER STATUS DIFF POL=' CH-POL-NO
           END-IF.

       CHECK-TIMELINE-RTN.
           IF PREV-VALID
              AND WS-PREV-POL-NO = CH-POL-NO
               IF CH-CHANGE-DATE < WS-PREV-DATE
                   ADD 1 TO WS-ERR-CNT
                   DISPLAY 'CHANGE-DATE REVERSED POL=' CH-POL-NO
               END-IF

               IF CH-BEFORE-STATUS-KBN NOT = WS-PREV-STATUS
                   ADD 1 TO WS-ERR-CNT
                   DISPLAY 'STATUS NOT CONTINUOUS CHG='
                           CH-CHANGE-ID
               END-IF
           END-IF.

       CHECK-CHANGE-TYPE-RTN.
           SET CHANGE-NOT-APPROVED TO TRUE
           IF CH-CHANGE-STATUS-KBN = '2'
               SET CHANGE-APPROVED TO TRUE
           END-IF

           IF CH-CHANGE-STATUS-KBN NOT = '1'
              AND CH-CHANGE-STATUS-KBN NOT = '2'
              AND CH-CHANGE-STATUS-KBN NOT = '9'
               ADD 1 TO WS-ERR-CNT
               DISPLAY 'BAD APPROVAL STATUS CHG='
                       CH-CHANGE-ID
           END-IF

           EVALUATE CH-CHANGE-TYPE-KBN
               WHEN '01'
                   PERFORM CHECK-CANCEL-RTN
               WHEN '02'
                   PERFORM CHECK-LAPSE-RTN
               WHEN '03'
                   PERFORM CHECK-REVIVE-RTN
               WHEN OTHER
                   ADD 1 TO WS-ERR-CNT
                   DISPLAY 'BAD CHANGE TYPE CHG=' CH-CHANGE-ID
           END-EVALUATE.

       CHECK-CANCEL-RTN.
           IF CH-BEFORE-STATUS-KBN NOT = '1'
               ADD 1 TO WS-ERR-CNT
               DISPLAY 'BAD CANCEL BEFORE CHG=' CH-CHANGE-ID
           END-IF

           IF CH-AFTER-STATUS-KBN NOT = '3'
               ADD 1 TO WS-ERR-CNT
               DISPLAY 'BAD CANCEL AFTER CHG=' CH-CHANGE-ID
           END-IF

           IF CHANGE-NOT-APPROVED
              AND CH-AFTER-STATUS-KBN = '3'
               ADD 1 TO WS-WARN-CNT
               DISPLAY 'UNAPPROVED CANCEL CHG=' CH-CHANGE-ID
           END-IF.

       CHECK-LAPSE-RTN.
           IF CH-BEFORE-STATUS-KBN NOT = '1'
               ADD 1 TO WS-ERR-CNT
               DISPLAY 'BAD LAPSE BEFORE CHG=' CH-CHANGE-ID
           END-IF

           IF CH-AFTER-STATUS-KBN NOT = '2'
               ADD 1 TO WS-ERR-CNT
               DISPLAY 'BAD LAPSE AFTER CHG=' CH-CHANGE-ID
           END-IF

           IF CH-CHANGE-DATE < PO-PAID-TO-DATE
               ADD 1 TO WS-WARN-CNT
               DISPLAY 'LAPSE BEFORE PAID-TO CHG=' CH-CHANGE-ID
           END-IF.

       CHECK-REVIVE-RTN.
           IF CH-BEFORE-STATUS-KBN NOT = '2'
               ADD 1 TO WS-ERR-CNT
               DISPLAY 'BAD REVIVE BEFORE CHG=' CH-CHANGE-ID
           END-IF

           IF CH-AFTER-STATUS-KBN NOT = '1'
               ADD 1 TO WS-ERR-CNT
               DISPLAY 'BAD REVIVE AFTER CHG=' CH-CHANGE-ID
           END-IF

           IF CHANGE-NOT-APPROVED
              AND CH-AFTER-STATUS-KBN = '1'
               ADD 1 TO WS-WARN-CNT
               DISPLAY 'UNAPPROVED REVIVE CHG=' CH-CHANGE-ID
           END-IF.

       SAVE-PREVIOUS-RTN.
           IF CH-POL-NO NOT = SPACES
               MOVE CH-POL-NO TO WS-PREV-POL-NO
               MOVE CH-CHANGE-DATE TO WS-PREV-DATE
               MOVE CH-AFTER-STATUS-KBN TO WS-PREV-STATUS
               SET PREV-VALID TO TRUE
           END-IF.

       END-RTN.
           IF WS-LVCHGF-ST NOT = SPACES
               CLOSE LVCHGF
           END-IF

           IF WS-LFPOLF2-ST NOT = SPACES
               CLOSE LFPOLF2
           END-IF

           DISPLAY 'LV220S CHECK COUNT=' WS-READ-CNT
           DISPLAY 'LV220S ERROR COUNT=' WS-ERR-CNT
           DISPLAY 'LV220S WARN COUNT=' WS-WARN-CNT

           IF HARD-ERROR
               MOVE 8 TO RETURN-CODE
           ELSE
               IF WS-ERR-CNT > ZERO
                   MOVE 4 TO RETURN-CODE
               ELSE
                   MOVE 0 TO RETURN-CODE
               END-IF
           END-IF.
