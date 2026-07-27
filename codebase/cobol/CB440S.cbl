       IDENTIFICATION DIVISION.
       PROGRAM-ID. CB440S.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CDMEMF ASSIGN TO "CDMEMF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS MM-MEMBER-ID
               FILE STATUS IS WS-CDMEMF-ST.

       DATA DIVISION.
       FILE SECTION.
       FD  CDMEMF.
           COPY CDMEMC.

       WORKING-STORAGE SECTION.
       01  WS-CDMEMF-ST               PIC XX VALUE SPACES.
       01  WS-FILE-OPEN-SW            PIC X VALUE '0'.
           88  WS-FILE-OPEN           VALUE '1'.
       01  WS-ABEND-SW                PIC X VALUE '0'.
           88  WS-ABEND               VALUE '1'.
       01  WS-FOUND-SW                PIC X VALUE '0'.
           88  WS-MEMBER-FOUND        VALUE '1'.
       01  WS-CHARGE-SW               PIC X VALUE '0'.
           88  WS-CHARGE              VALUE '1'.

       01  WS-BILL-DATE.
           05  WS-BILL-YYYY           PIC 9(04).
           05  WS-BILL-MM             PIC 9(02).

       01  WS-JOIN-DATE.
           05  WS-JOIN-YYYY           PIC 9(04).
           05  WS-JOIN-MM             PIC 9(02).
           05  WS-JOIN-DD             PIC 9(02).

       01  WS-STAT-DATE.
           05  WS-STAT-YYYY           PIC 9(04).
           05  WS-STAT-MM             PIC 9(02).
           05  WS-STAT-DD             PIC 9(02).

       01  WS-JOIN-YYYYMM             PIC 9(06).
       01  WS-STAT-YYYYMM             PIC 9(06).
       01  WS-DISPLAY-AMT             PIC Z,ZZZ,ZZ9.

       LINKAGE SECTION.
       01  LK-CB440S-PARM.
           05  LK-MEMBER-ID          PIC X(10).
           05  LK-BILL-YYYYMM        PIC 9(06).
           05  LK-ANNUAL-FEE-AMT     PIC S9(09) COMP-3.
           05  LK-RESULT-CD          PIC X(02).
           05  LK-CHARGE-REASON      PIC X(40).
           05  LK-NOCHARGE-REASON    PIC X(40).

       PROCEDURE DIVISION USING LK-CB440S-PARM.

       MAIN-SEC.
           PERFORM 1000-INIT
           IF NOT WS-ABEND
               PERFORM 2000-READ-MEMBER
           END-IF
           IF NOT WS-ABEND AND WS-MEMBER-FOUND
               PERFORM 3000-JUDGE-FEE
           END-IF
           PERFORM 9000-FINISH
           GOBACK
           .

       1000-INIT.
           MOVE 0              TO RETURN-CODE
           MOVE 0              TO LK-ANNUAL-FEE-AMT
           MOVE SPACES         TO LK-CHARGE-REASON
           MOVE SPACES         TO LK-NOCHARGE-REASON
           MOVE '00'           TO LK-RESULT-CD
           MOVE '0'            TO WS-ABEND-SW
           MOVE '0'            TO WS-FOUND-SW
           MOVE '0'            TO WS-CHARGE-SW
           MOVE '0'            TO WS-FILE-OPEN-SW
           MOVE SPACES         TO WS-CDMEMF-ST
           MOVE LK-BILL-YYYYMM TO WS-BILL-DATE

           IF LK-MEMBER-ID = SPACES
               MOVE '03' TO LK-RESULT-CD
               MOVE 'MEMBER ID IS SPACE' TO LK-NOCHARGE-REASON
           ELSE
               PERFORM 1100-VALIDATE-BILL
           END-IF

           IF LK-RESULT-CD = '00'
               OPEN INPUT CDMEMF
               IF WS-CDMEMF-ST = '00'
                   MOVE '1' TO WS-FILE-OPEN-SW
               ELSE
                   DISPLAY 'CDMEMF OPEN ERROR ST=' WS-CDMEMF-ST
                   MOVE 12   TO RETURN-CODE
                   MOVE '1'  TO WS-ABEND-SW
                   MOVE '99' TO LK-RESULT-CD
               END-IF
           END-IF
           .

       1100-VALIDATE-BILL.
           IF WS-BILL-YYYY < 1900 OR WS-BILL-YYYY > 2099
               MOVE '03' TO LK-RESULT-CD
               MOVE 'BILL YEAR ERROR' TO LK-NOCHARGE-REASON
           ELSE
               IF WS-BILL-MM < 1 OR WS-BILL-MM > 12
                   MOVE '03' TO LK-RESULT-CD
                   MOVE 'BILL MONTH ERROR' TO LK-NOCHARGE-REASON
               END-IF
           END-IF
           .

       2000-READ-MEMBER.
           MOVE LK-MEMBER-ID TO MM-MEMBER-ID
           READ CDMEMF KEY IS MM-MEMBER-ID
               INVALID KEY
                   IF WS-CDMEMF-ST = '23'
                       MOVE '01' TO LK-RESULT-CD
                       MOVE 'MEMBER NOT FOUND' TO LK-NOCHARGE-REASON
                   ELSE
                       DISPLAY 'CDMEMF READ ERROR ST=' WS-CDMEMF-ST
                       MOVE 12   TO RETURN-CODE
                       MOVE '1'  TO WS-ABEND-SW
                       MOVE '99' TO LK-RESULT-CD
                   END-IF
               NOT INVALID KEY
                   MOVE '1' TO WS-FOUND-SW
           END-READ
           .

       3000-JUDGE-FEE.
           PERFORM 3100-VALIDATE-MEMBER
           IF LK-RESULT-CD = '00'
               PERFORM 3200-JUDGE-STATUS
           END-IF
           IF LK-RESULT-CD = '00'
               PERFORM 3300-JUDGE-FEE-CODE
           END-IF
           IF LK-RESULT-CD = '00'
               PERFORM 3400-JUDGE-BILL-MONTH
           END-IF
           IF WS-CHARGE
               PERFORM 3500-SET-AMOUNT
           END-IF
           .

       3100-VALIDATE-MEMBER.
           MOVE MM-JOIN-DT TO WS-JOIN-DATE
           MOVE 0          TO WS-JOIN-YYYYMM
           MOVE 0          TO WS-STAT-YYYYMM

           IF WS-JOIN-YYYY < 1900 OR WS-JOIN-YYYY > 2099
               MOVE '03' TO LK-RESULT-CD
               MOVE 'JOIN YEAR ERROR' TO LK-NOCHARGE-REASON
           ELSE
               IF WS-JOIN-MM < 1 OR WS-JOIN-MM > 12
                   MOVE '03' TO LK-RESULT-CD
                   MOVE 'JOIN MONTH ERROR' TO LK-NOCHARGE-REASON
               ELSE
                   IF WS-JOIN-DD < 1 OR WS-JOIN-DD > 31
                       MOVE '03' TO LK-RESULT-CD
                       MOVE 'JOIN DAY ERROR' TO LK-NOCHARGE-REASON
                   ELSE
                       COMPUTE WS-JOIN-YYYYMM =
                           (WS-JOIN-YYYY * 100) + WS-JOIN-MM
                   END-IF
               END-IF
           END-IF

           IF LK-RESULT-CD = '00'
               MOVE MM-LAST-STATUS-DT TO WS-STAT-DATE
               IF MM-LAST-STATUS-DT = 0
                   MOVE 0 TO WS-STAT-YYYYMM
               ELSE
                   IF WS-STAT-YYYY < 1900 OR WS-STAT-YYYY > 2099
                       MOVE '03' TO LK-RESULT-CD
                       MOVE 'STATUS YEAR ERROR' TO LK-NOCHARGE-REASON
                   ELSE
                       IF WS-STAT-MM < 1 OR WS-STAT-MM > 12
                           MOVE '03' TO LK-RESULT-CD
                           MOVE 'STATUS MONTH ERROR'
                             TO LK-NOCHARGE-REASON
                       ELSE
                           IF WS-STAT-DD < 1 OR WS-STAT-DD > 31
                               MOVE '03' TO LK-RESULT-CD
                               MOVE 'STATUS DAY ERROR'
                                 TO LK-NOCHARGE-REASON
                           ELSE
                               COMPUTE WS-STAT-YYYYMM =
                                   (WS-STAT-YYYY * 100) + WS-STAT-MM
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF
           .

       3200-JUDGE-STATUS.
           EVALUATE MM-MEMBER-STATUS
               WHEN '00'
                   CONTINUE
               WHEN '01'
                   MOVE '02' TO LK-RESULT-CD
                   MOVE 'MEMBER SUSPENDED' TO LK-NOCHARGE-REASON
               WHEN '90'
                   MOVE '02' TO LK-RESULT-CD
                   MOVE 'MEMBER WITHDRAWN' TO LK-NOCHARGE-REASON
               WHEN '91'
                   MOVE '02' TO LK-RESULT-CD
                   MOVE 'MEMBER EXPELLED' TO LK-NOCHARGE-REASON
               WHEN OTHER
                   MOVE '03' TO LK-RESULT-CD
                   MOVE 'MEMBER STATUS ERROR' TO LK-NOCHARGE-REASON
           END-EVALUATE

           IF LK-RESULT-CD = '02'
              AND WS-STAT-YYYYMM > LK-BILL-YYYYMM
               MOVE '00'   TO LK-RESULT-CD
               MOVE SPACES TO LK-NOCHARGE-REASON
           END-IF
           .

       3300-JUDGE-FEE-CODE.
           EVALUATE MM-ANNUAL-FEE-CD
               WHEN '00'
                   MOVE '02' TO LK-RESULT-CD
                   MOVE 'ANNUAL FEE EXEMPT' TO LK-NOCHARGE-REASON
               WHEN '01'
                   CONTINUE
               WHEN '02'
                   CONTINUE
               WHEN '03'
                   CONTINUE
               WHEN '04'
                   IF WS-JOIN-YYYY = WS-BILL-YYYY
                       MOVE '02' TO LK-RESULT-CD
                       MOVE 'FIRST YEAR EXEMPT' TO LK-NOCHARGE-REASON
                   END-IF
               WHEN OTHER
                   MOVE '03' TO LK-RESULT-CD
                   MOVE 'ANNUAL FEE CODE ERROR' TO LK-NOCHARGE-REASON
           END-EVALUATE
           .

       3400-JUDGE-BILL-MONTH.
           IF WS-JOIN-YYYYMM > LK-BILL-YYYYMM
               MOVE '02' TO LK-RESULT-CD
               MOVE 'BEFORE JOIN MONTH' TO LK-NOCHARGE-REASON
           ELSE
               IF WS-JOIN-MM = WS-BILL-MM
                   MOVE '1' TO WS-CHARGE-SW
                   MOVE 'ANNUAL FEE MONTH' TO LK-CHARGE-REASON
               ELSE
                   MOVE '02' TO LK-RESULT-CD
                   MOVE 'NOT ANNUAL FEE MONTH' TO LK-NOCHARGE-REASON
               END-IF
           END-IF
           .

       3500-SET-AMOUNT.
           EVALUATE MM-ANNUAL-FEE-CD
               WHEN '01'
                   MOVE 1100  TO LK-ANNUAL-FEE-AMT
               WHEN '02'
                   MOVE 11000 TO LK-ANNUAL-FEE-AMT
               WHEN '03'
                   MOVE 33000 TO LK-ANNUAL-FEE-AMT
               WHEN '04'
                   MOVE 1100  TO LK-ANNUAL-FEE-AMT
               WHEN OTHER
                   MOVE 0     TO LK-ANNUAL-FEE-AMT
           END-EVALUATE

           IF LK-ANNUAL-FEE-AMT > 0
               MOVE '10' TO LK-RESULT-CD
               MOVE LK-ANNUAL-FEE-AMT TO WS-DISPLAY-AMT
               DISPLAY 'ANNUAL FEE MEMBER=' LK-MEMBER-ID
                       ' AMOUNT=' WS-DISPLAY-AMT
           ELSE
               MOVE '03' TO LK-RESULT-CD
               MOVE 'ANNUAL FEE AMOUNT ERROR' TO LK-NOCHARGE-REASON
           END-IF
           .

       9000-FINISH.
           IF WS-FILE-OPEN
               CLOSE CDMEMF
               IF WS-CDMEMF-ST NOT = '00'
                   DISPLAY 'CDMEMF CLOSE ERROR ST=' WS-CDMEMF-ST
                   MOVE 8    TO RETURN-CODE
                   MOVE '99' TO LK-RESULT-CD
               END-IF
           END-IF

           IF LK-RESULT-CD NOT = '99'
               IF RETURN-CODE NOT = 8 AND RETURN-CODE NOT = 12
                   MOVE 0 TO RETURN-CODE
               END-IF
           END-IF
           .
