       IDENTIFICATION DIVISION.
       PROGRAM-ID. CT240S.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WK-AREA.
           05  WK-HIT-SW               PIC X VALUE SPACE.
               88  WK-HIT              VALUE '1'.
           05  WK-BEFORE-ST            PIC X(02) VALUE SPACE.
           05  WK-CHANGE-KB            PIC X(02) VALUE SPACE.
           05  WK-APPROVAL-KB          PIC X(01) VALUE SPACE.
           05  WK-NEXT-ST              PIC X(02) VALUE SPACE.

       01  WK-TRANSITION-TABLE.
           05  WK-TRANSITION-ROW OCCURS 18 TIMES
               INDEXED BY TX-IDX.
               10  WK-T-BEFORE-ST      PIC X(02).
               10  WK-T-CHANGE-KB      PIC X(02).
               10  WK-T-APPROVAL-KB    PIC X(01).
               10  WK-T-NEXT-ST        PIC X(02).

       01  WK-TRANSITION-INIT.
           05  FILLER                  PIC X(07) VALUE '0101001'.
           05  FILLER                  PIC X(07) VALUE '0102102'.
           05  FILLER                  PIC X(07) VALUE '0202103'.
           05  FILLER                  PIC X(07) VALUE '0203103'.
           05  FILLER                  PIC X(07) VALUE '0304204'.
           05  FILLER                  PIC X(07) VALUE '0305301'.
           05  FILLER                  PIC X(07) VALUE '0406105'.
           05  FILLER                  PIC X(07) VALUE '0507106'.
           05  FILLER                  PIC X(07) VALUE '0103101'.
           05  FILLER                  PIC X(07) VALUE '0203102'.
           05  FILLER                  PIC X(07) VALUE '0303103'.
           05  FILLER                  PIC X(07) VALUE '0403104'.
           05  FILLER                  PIC X(07) VALUE '0100009'.
           05  FILLER                  PIC X(07) VALUE '0200009'.
           05  FILLER                  PIC X(07) VALUE '0300009'.
           05  FILLER                  PIC X(07) VALUE '0400009'.
           05  FILLER                  PIC X(07) VALUE '9008002'.
           05  FILLER                  PIC X(07) VALUE '9102103'.

       LINKAGE SECTION.
       01  LK-CT240S-PARM.
           05  LK-BEFORE-ST            PIC X(02).
           05  LK-CHANGE-KB            PIC X(02).
           05  LK-APPROVAL-KB          PIC X(01).
           05  LK-NEXT-ST              PIC X(02).
           05  LK-ERROR-CD             PIC X(04).
           05  LK-REASON-TEXT          PIC X(60).

       PROCEDURE DIVISION USING LK-CT240S-PARM.
       0000-MAIN.
           MOVE 0 TO RETURN-CODE
           PERFORM 0100-INIT
           PERFORM 0200-VALIDATE
           IF LK-ERROR-CD = '0000'
              PERFORM 0300-JUDGE-TRANSITION
           END-IF
           GOBACK
           .

       0100-INIT.
           MOVE WK-TRANSITION-INIT TO WK-TRANSITION-TABLE
           MOVE SPACE TO WK-HIT-SW
           MOVE LK-BEFORE-ST TO WK-BEFORE-ST
           MOVE LK-CHANGE-KB TO WK-CHANGE-KB
           MOVE LK-APPROVAL-KB TO WK-APPROVAL-KB
           MOVE SPACE TO WK-NEXT-ST
           MOVE SPACE TO LK-NEXT-ST
           MOVE '0000' TO LK-ERROR-CD
           MOVE SPACE TO LK-REASON-TEXT
           .

       0200-VALIDATE.
           EVALUATE WK-BEFORE-ST
             WHEN '01'
             WHEN '02'
             WHEN '03'
             WHEN '04'
             WHEN '05'
             WHEN '06'
             WHEN '90'
             WHEN '91'
             WHEN '99'
               CONTINUE
             WHEN OTHER
               MOVE 'E101' TO LK-ERROR-CD
               MOVE 'INVALID BEFORE STATUS' TO LK-REASON-TEXT
           END-EVALUATE

           IF LK-ERROR-CD NOT = '0000'
              EXIT PARAGRAPH
           END-IF

           EVALUATE WK-CHANGE-KB
             WHEN '00'
             WHEN '01'
             WHEN '02'
             WHEN '03'
             WHEN '04'
             WHEN '05'
             WHEN '06'
             WHEN '07'
             WHEN '08'
               CONTINUE
             WHEN OTHER
               MOVE 'E102' TO LK-ERROR-CD
               MOVE 'INVALID CHANGE TYPE' TO LK-REASON-TEXT
           END-EVALUATE

           IF LK-ERROR-CD NOT = '0000'
              EXIT PARAGRAPH
           END-IF

           EVALUATE WK-APPROVAL-KB
             WHEN '0'
             WHEN '1'
             WHEN '2'
             WHEN '3'
               CONTINUE
             WHEN OTHER
               MOVE 'E103' TO LK-ERROR-CD
               MOVE 'INVALID APPROVAL TYPE' TO LK-REASON-TEXT
           END-EVALUATE

           IF LK-ERROR-CD NOT = '0000'
              EXIT PARAGRAPH
           END-IF

           IF WK-BEFORE-ST = '99'
              MOVE 'E201' TO LK-ERROR-CD
              MOVE 'CANCELLED ORDER CANNOT BE RESTORED'
                TO LK-REASON-TEXT
              EXIT PARAGRAPH
           END-IF

           IF WK-BEFORE-ST = '06'
              MOVE 'E202' TO LK-ERROR-CD
              MOVE 'COMPLETED ORDER CANNOT BE CHANGED'
                TO LK-REASON-TEXT
              EXIT PARAGRAPH
           END-IF

           IF WK-CHANGE-KB = '04'
              AND WK-APPROVAL-KB NOT = '2'
              MOVE 'E301' TO LK-ERROR-CD
              MOVE 'APPROVAL TYPE MISMATCH FOR APPROVE'
                TO LK-REASON-TEXT
              EXIT PARAGRAPH
           END-IF

           IF WK-CHANGE-KB = '05'
              AND WK-APPROVAL-KB NOT = '3'
              MOVE 'E302' TO LK-ERROR-CD
              MOVE 'APPROVAL TYPE MISMATCH FOR REJECT'
                TO LK-REASON-TEXT
              EXIT PARAGRAPH
           END-IF

           IF WK-CHANGE-KB = '03'
              AND WK-APPROVAL-KB = '2'
              MOVE 'E303' TO LK-ERROR-CD
              MOVE 'APPROVED TYPE NOT ALLOWED FOR CANCEL'
                TO LK-REASON-TEXT
           END-IF
           .

       0300-JUDGE-TRANSITION.
           SET TX-IDX TO 1
           SEARCH WK-TRANSITION-ROW
             AT END
               MOVE 'E401' TO LK-ERROR-CD
               MOVE 'STATUS TRANSITION NOT ALLOWED'
                 TO LK-REASON-TEXT
             WHEN WK-T-BEFORE-ST(TX-IDX) = WK-BEFORE-ST
              AND WK-T-CHANGE-KB(TX-IDX) = WK-CHANGE-KB
              AND WK-T-APPROVAL-KB(TX-IDX) = WK-APPROVAL-KB
               MOVE WK-T-NEXT-ST(TX-IDX) TO WK-NEXT-ST
               SET WK-HIT TO TRUE
           END-SEARCH

           IF WK-HIT
              MOVE WK-NEXT-ST TO LK-NEXT-ST
              MOVE '0000' TO LK-ERROR-CD
              MOVE 'NORMAL' TO LK-REASON-TEXT
           END-IF
           .
