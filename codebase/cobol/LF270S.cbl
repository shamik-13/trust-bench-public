       IDENTIFICATION DIVISION.
       PROGRAM-ID. LF270S.
      ******************************************************************
      * 契約異動妥当性チェック
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LFCHGF ASSIGN TO "LFCHGF"
               ORGANIZATION IS INDEXED
               ACCESS MODE  IS DYNAMIC
               RECORD KEY   IS CG-CHANGE-ID
               FILE STATUS  IS WS-LFCHGF-ST.

           SELECT LFPOLF ASSIGN TO "LFPOLF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE  IS SEQUENTIAL
               FILE STATUS  IS WS-LFPOLF-ST.

       DATA DIVISION.
       FILE SECTION.
       FD  LFCHGF.
           COPY LFCHGFC.

       FD  LFPOLF.
           COPY LFPOLFC.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05 WS-LFCHGF-ST             PIC XX VALUE SPACE.
           05 WS-LFPOLF-ST             PIC XX VALUE SPACE.

       01  WS-WORK.
           05 WS-HARD-ERROR-SW         PIC X  VALUE '0'.
              88 HARD-ERROR                  VALUE '1'.
           05 WS-FOUND-POL-SW          PIC X  VALUE '0'.
              88 POL-FOUND                   VALUE '1'.
           05 WS-EOF-SW                PIC X  VALUE '0'.
              88 LFPOLF-EOF                  VALUE '1'.
           05 WS-OLD-AMT               PIC 9(11) VALUE ZERO.
           05 WS-NEW-AMT               PIC 9(11) VALUE ZERO.
           05 WS-OLD-AGE               PIC 9(03) VALUE ZERO.
           05 WS-NEW-AGE               PIC 9(03) VALUE ZERO.
           05 WS-CUR-BAND-KBN          PIC XX VALUE SPACE.
           05 WS-NEW-BAND-KBN          PIC XX VALUE SPACE.
           05 WS-TODAY                 PIC 9(08) VALUE ZERO.
           05 WS-LFCHGF-OPEN-SW        PIC X  VALUE '0'.
              88 LFCHGF-OPEN                 VALUE '1'.
           05 WS-LFPOLF-OPEN-SW        PIC X  VALUE '0'.
              88 LFPOLF-OPEN                 VALUE '1'.

       01  WS-CONST.
           05 C-OK                     PIC 99 VALUE 0.
           05 C-BUSINESS-ERR           PIC 99 VALUE 4.
           05 C-IO-ERR                 PIC 99 VALUE 8.
           05 C-PARM-ERR               PIC 99 VALUE 12.
           05 C-MIN-SUM-AMT            PIC 9(11) VALUE 00001000000.
           05 C-MAX-SUM-AMT            PIC 9(11) VALUE 01000000000.

       LINKAGE SECTION.
       01  LK-LF270S-PARM.
           05 LK-CHANGE-ID             PIC X(12).
           05 LK-POL-NO                PIC X(12).
           05 LK-RESULT-CD             PIC 99.
           05 LK-PREMIUM-IMPACT-FLG    PIC X.
           05 LK-BAND-KBN              PIC XX.
           05 LK-REASON-CD             PIC X(30).

       PROCEDURE DIVISION USING LK-LF270S-PARM.
       0000-MAIN SECTION.
           PERFORM 1000-INIT

           IF NOT HARD-ERROR
              AND LK-RESULT-CD = C-OK
              PERFORM 2000-READ-CHANGE
           END-IF

           IF NOT HARD-ERROR
              AND LK-RESULT-CD = C-OK
              PERFORM 3000-READ-POLICY
           END-IF

           IF NOT HARD-ERROR
              AND LK-RESULT-CD = C-OK
              IF POL-FOUND
                 PERFORM 4000-CHECK-CHANGE
              ELSE
                 MOVE C-BUSINESS-ERR TO LK-RESULT-CD
                 MOVE "POL-NO NOT FOUND" TO LK-REASON-CD
              END-IF
           END-IF

           PERFORM 9000-FINAL
           GOBACK
           .

       1000-INIT SECTION.
           MOVE C-OK       TO RETURN-CODE
           MOVE C-OK       TO LK-RESULT-CD
           MOVE '0'        TO LK-PREMIUM-IMPACT-FLG
           MOVE SPACE      TO LK-BAND-KBN
           MOVE SPACE      TO LK-REASON-CD
           MOVE '0'        TO WS-HARD-ERROR-SW
           MOVE '0'        TO WS-FOUND-POL-SW
           MOVE '0'        TO WS-EOF-SW
           MOVE '0'        TO WS-LFCHGF-OPEN-SW
           MOVE '0'        TO WS-LFPOLF-OPEN-SW
           ACCEPT WS-TODAY FROM DATE YYYYMMDD

           IF LK-CHANGE-ID = SPACE
              MOVE C-PARM-ERR TO LK-RESULT-CD
              MOVE C-PARM-ERR TO RETURN-CODE
              MOVE "CHANGE-ID REQUIRED" TO LK-REASON-CD
              MOVE '1' TO WS-HARD-ERROR-SW
              EXIT SECTION
           END-IF

           OPEN INPUT LFCHGF
           IF WS-LFCHGF-ST = '00'
              MOVE '1' TO WS-LFCHGF-OPEN-SW
           ELSE
              MOVE '1' TO WS-HARD-ERROR-SW
              MOVE C-IO-ERR TO LK-RESULT-CD
              MOVE C-IO-ERR TO RETURN-CODE
              MOVE "LFCHGF OPEN ERROR" TO LK-REASON-CD
              EXIT SECTION
           END-IF

           OPEN INPUT LFPOLF
           IF WS-LFPOLF-ST = '00'
              MOVE '1' TO WS-LFPOLF-OPEN-SW
           ELSE
              MOVE '1' TO WS-HARD-ERROR-SW
              MOVE C-IO-ERR TO LK-RESULT-CD
              MOVE C-IO-ERR TO RETURN-CODE
              MOVE "LFPOLF OPEN ERROR" TO LK-REASON-CD
           END-IF
           .

       2000-READ-CHANGE SECTION.
           MOVE LK-CHANGE-ID TO CG-CHANGE-ID

           READ LFCHGF KEY IS CG-CHANGE-ID
              INVALID KEY
                 MOVE C-BUSINESS-ERR TO LK-RESULT-CD
                 MOVE "CHANGE-ID NOT FOUND" TO LK-REASON-CD
              NOT INVALID KEY
                 IF LK-POL-NO NOT = SPACE
                    AND LK-POL-NO NOT = CG-POL-NO
                    MOVE C-BUSINESS-ERR TO LK-RESULT-CD
                    MOVE "POL-NO MISMATCH" TO LK-REASON-CD
                 ELSE
                    MOVE CG-POL-NO TO LK-POL-NO
                 END-IF
           END-READ

           IF WS-LFCHGF-ST NOT = '00'
              AND WS-LFCHGF-ST NOT = '23'
              MOVE '1' TO WS-HARD-ERROR-SW
              MOVE C-IO-ERR TO LK-RESULT-CD
              MOVE C-IO-ERR TO RETURN-CODE
              MOVE "LFCHGF READ ERROR" TO LK-REASON-CD
           END-IF
           .

       3000-READ-POLICY SECTION.
           PERFORM UNTIL LFPOLF-EOF OR POL-FOUND OR HARD-ERROR
              READ LFPOLF
                 AT END
                    MOVE '1' TO WS-EOF-SW
                 NOT AT END
                    IF PO-POL-NO = CG-POL-NO
                       MOVE '1' TO WS-FOUND-POL-SW
                    END-IF
              END-READ

              IF WS-LFPOLF-ST NOT = '00'
                 AND WS-LFPOLF-ST NOT = '10'
                 MOVE '1' TO WS-HARD-ERROR-SW
                 MOVE C-IO-ERR TO LK-RESULT-CD
                 MOVE C-IO-ERR TO RETURN-CODE
                 MOVE "LFPOLF READ ERROR" TO LK-REASON-CD
              END-IF
           END-PERFORM
           .

       4000-CHECK-CHANGE SECTION.
           EVALUATE TRUE
              WHEN CG-CHANGE-TYPE-KBN = '01'
                 PERFORM 4100-CHECK-SUM-ASSURED
              WHEN CG-CHANGE-TYPE-KBN = '02'
                 PERFORM 4200-CHECK-ENTRY-AGE
              WHEN CG-CHANGE-TYPE-KBN = '03'
                 PERFORM 4300-CHECK-POL-STATUS
              WHEN OTHER
                 MOVE C-BUSINESS-ERR TO LK-RESULT-CD
                 MOVE "CHANGE-TYPE INVALID" TO LK-REASON-CD
           END-EVALUATE

           IF LK-RESULT-CD = C-OK
              IF CG-APPROVAL-STATUS-KBN NOT = '02'
                 MOVE C-BUSINESS-ERR TO LK-RESULT-CD
                 MOVE "NOT APPROVED" TO LK-REASON-CD
              END-IF
           END-IF

           IF LK-RESULT-CD = C-OK
              IF CG-APPLY-DATE NOT NUMERIC
                 MOVE C-BUSINESS-ERR TO LK-RESULT-CD
                 MOVE "APPLY-DATE NOT NUMERIC" TO LK-REASON-CD
              ELSE
                 IF CG-APPLY-DATE < WS-TODAY
                    MOVE C-BUSINESS-ERR TO LK-RESULT-CD
                    MOVE "APPLY-DATE PAST" TO LK-REASON-CD
                 END-IF
              END-IF
           END-IF

           IF LK-RESULT-CD = C-OK
              MOVE "OK" TO LK-REASON-CD
           END-IF
           .

       4100-CHECK-SUM-ASSURED SECTION.
           MOVE '1' TO LK-PREMIUM-IMPACT-FLG

           IF PO-POL-STATUS-KBN NOT = '01'
              MOVE C-BUSINESS-ERR TO LK-RESULT-CD
              MOVE "PREMIUM NOT TARGET" TO LK-REASON-CD
              EXIT SECTION
           END-IF

           IF CG-OLD-VALUE NOT NUMERIC
              MOVE C-BUSINESS-ERR TO LK-RESULT-CD
              MOVE "OLD AMT NOT NUMERIC" TO LK-REASON-CD
              EXIT SECTION
           END-IF

           IF CG-NEW-VALUE NOT NUMERIC
              MOVE C-BUSINESS-ERR TO LK-RESULT-CD
              MOVE "NEW AMT NOT NUMERIC" TO LK-REASON-CD
              EXIT SECTION
           END-IF

           MOVE CG-OLD-VALUE TO WS-OLD-AMT
           MOVE CG-NEW-VALUE TO WS-NEW-AMT

           IF WS-OLD-AMT NOT = PO-SUM-ASSURED-AMT
              MOVE C-BUSINESS-ERR TO LK-RESULT-CD
              MOVE "OLD AMT MISMATCH" TO LK-REASON-CD
              EXIT SECTION
           END-IF

           IF WS-NEW-AMT < C-MIN-SUM-AMT
              OR WS-NEW-AMT > C-MAX-SUM-AMT
              MOVE C-BUSINESS-ERR TO LK-RESULT-CD
              MOVE "NEW AMT OUT OF RANGE" TO LK-REASON-CD
           END-IF
           .

       4200-CHECK-ENTRY-AGE SECTION.
           MOVE '1' TO LK-PREMIUM-IMPACT-FLG

           IF PO-POL-STATUS-KBN NOT = '01'
              MOVE C-BUSINESS-ERR TO LK-RESULT-CD
              MOVE "PREMIUM NOT TARGET" TO LK-REASON-CD
              EXIT SECTION
           END-IF

           IF CG-OLD-VALUE NOT NUMERIC
              MOVE C-BUSINESS-ERR TO LK-RESULT-CD
              MOVE "OLD AGE NOT NUMERIC" TO LK-REASON-CD
              EXIT SECTION
           END-IF

           IF CG-NEW-VALUE NOT NUMERIC
              MOVE C-BUSINESS-ERR TO LK-RESULT-CD
              MOVE "NEW AGE NOT NUMERIC" TO LK-REASON-CD
              EXIT SECTION
           END-IF

           MOVE CG-OLD-VALUE TO WS-OLD-AGE
           MOVE CG-NEW-VALUE TO WS-NEW-AGE

           IF WS-OLD-AGE NOT = PO-ENTRY-AGE-CNT
              MOVE C-BUSINESS-ERR TO LK-RESULT-CD
              MOVE "OLD AGE MISMATCH" TO LK-REASON-CD
              EXIT SECTION
           END-IF

           PERFORM 4210-MAKE-CURRENT-BAND
           PERFORM 4220-MAKE-NEW-BAND
           MOVE WS-NEW-BAND-KBN TO LK-BAND-KBN

           IF WS-CUR-BAND-KBN = WS-NEW-BAND-KBN
              MOVE C-BUSINESS-ERR TO LK-RESULT-CD
              MOVE "AGE BAND NO CHANGE" TO LK-REASON-CD
           END-IF
           .

       4210-MAKE-CURRENT-BAND SECTION.
           EVALUATE TRUE
              WHEN PO-ENTRY-AGE-CNT <= 29
                 MOVE 'A1' TO WS-CUR-BAND-KBN
              WHEN PO-ENTRY-AGE-CNT <= 39
                 MOVE 'A2' TO WS-CUR-BAND-KBN
              WHEN PO-ENTRY-AGE-CNT <= 49
                 MOVE 'A3' TO WS-CUR-BAND-KBN
              WHEN PO-ENTRY-AGE-CNT <= 59
                 MOVE 'A4' TO WS-CUR-BAND-KBN
              WHEN OTHER
                 MOVE 'A5' TO WS-CUR-BAND-KBN
           END-EVALUATE
           .

       4220-MAKE-NEW-BAND SECTION.
           EVALUATE TRUE
              WHEN WS-NEW-AGE <= 29
                 MOVE 'A1' TO WS-NEW-BAND-KBN
              WHEN WS-NEW-AGE <= 39
                 MOVE 'A2' TO WS-NEW-BAND-KBN
              WHEN WS-NEW-AGE <= 49
                 MOVE 'A3' TO WS-NEW-BAND-KBN
              WHEN WS-NEW-AGE <= 59
                 MOVE 'A4' TO WS-NEW-BAND-KBN
              WHEN OTHER
                 MOVE 'A5' TO WS-NEW-BAND-KBN
           END-EVALUATE
           .

       4300-CHECK-POL-STATUS SECTION.
           MOVE '0' TO LK-PREMIUM-IMPACT-FLG

           IF CG-OLD-VALUE(1:2) NOT = '01'
              AND CG-OLD-VALUE(1:2) NOT = '02'
              AND CG-OLD-VALUE(1:2) NOT = '09'
              MOVE C-BUSINESS-ERR TO LK-RESULT-CD
              MOVE "OLD STATUS INVALID" TO LK-REASON-CD
              EXIT SECTION
           END-IF

           IF CG-NEW-VALUE(1:2) NOT = '01'
              AND CG-NEW-VALUE(1:2) NOT = '02'
              AND CG-NEW-VALUE(1:2) NOT = '09'
              MOVE C-BUSINESS-ERR TO LK-RESULT-CD
              MOVE "NEW STATUS INVALID" TO LK-REASON-CD
              EXIT SECTION
           END-IF

           IF CG-OLD-VALUE(1:2) NOT = PO-POL-STATUS-KBN
              MOVE C-BUSINESS-ERR TO LK-RESULT-CD
              MOVE "OLD STATUS MISMATCH" TO LK-REASON-CD
           END-IF
           .

       9000-FINAL SECTION.
           IF LFCHGF-OPEN
              CLOSE LFCHGF
              IF WS-LFCHGF-ST NOT = '00'
                 AND NOT HARD-ERROR
                 MOVE C-IO-ERR TO LK-RESULT-CD
                 MOVE C-IO-ERR TO RETURN-CODE
                 MOVE "LFCHGF CLOSE ERROR" TO LK-REASON-CD
              END-IF
           END-IF

           IF LFPOLF-OPEN
              CLOSE LFPOLF
              IF WS-LFPOLF-ST NOT = '00'
                 AND NOT HARD-ERROR
                 MOVE C-IO-ERR TO LK-RESULT-CD
                 MOVE C-IO-ERR TO RETURN-CODE
                 MOVE "LFPOLF CLOSE ERROR" TO LK-REASON-CD
              END-IF
           END-IF

           IF RETURN-CODE = C-OK
              IF LK-RESULT-CD = C-OK
                 MOVE C-OK TO RETURN-CODE
              ELSE
                 MOVE C-BUSINESS-ERR TO RETURN-CODE
              END-IF
           END-IF
           .
