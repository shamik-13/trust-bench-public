       IDENTIFICATION DIVISION.
       PROGRAM-ID. CC280S.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CCVALF
               ASSIGN TO "CCVALF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-CCVALF-ST.
           SELECT CCCALF
               ASSIGN TO "CCCALF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-CCCALF-ST.

       DATA DIVISION.
       FILE SECTION.
       FD  CCVALF.
           COPY CCVALFC.
       FD  CCCALF.
           COPY CCCALFC.

       WORKING-STORAGE SECTION.
       01  WS-STATUS-AREA.
           05 WS-CCVALF-ST              PIC XX VALUE SPACE.
           05 WS-CCCALF-ST              PIC XX VALUE SPACE.

       01  WS-FLAG-AREA.
           05 WS-VAL-EOF                PIC X VALUE 'N'.
              88 VAL-EOF                      VALUE 'Y'.
           05 WS-CAL-EOF                PIC X VALUE 'N'.
              88 CAL-EOF                      VALUE 'Y'.
           05 WS-FOUND-FIXED            PIC X VALUE 'N'.
              88 FOUND-FIXED                  VALUE 'Y'.
           05 WS-FOUND-PENDING          PIC X VALUE 'N'.
              88 FOUND-PENDING                VALUE 'Y'.
           05 WS-CAL-FOUND              PIC X VALUE 'N'.
              88 CAL-FOUND                    VALUE 'Y'.

       01  WS-WORK-AREA.
           05 WS-TARGET-DT              PIC 9(8) VALUE ZERO.
           05 WS-OPENED-VAL             PIC X VALUE 'N'.
           05 WS-OPENED-CAL             PIC X VALUE 'N'.

       LINKAGE SECTION.
       01  LK-CC280S-PARM.
           05 LK-FCT-ID                 PIC X(20).
           05 LK-VALUE-DT               PIC 9(8).
           05 LK-STATUS-KBN             PIC X(2).
           05 LK-REASON-CD              PIC X(4).

       PROCEDURE DIVISION USING LK-CC280S-PARM.
       MAIN-RTN.
           PERFORM INIT-RTN
           PERFORM VALIDATE-RTN
           IF LK-STATUS-KBN NOT = '90'
              PERFORM OPEN-RTN
              IF LK-STATUS-KBN NOT = '99'
                 PERFORM SEARCH-VALUE-RTN
                 IF LK-STATUS-KBN NOT = '99'
                    IF FOUND-FIXED
                       PERFORM CHECK-CALENDAR-RTN
                    ELSE
                       IF FOUND-PENDING
                          MOVE '10' TO LK-STATUS-KBN
                          MOVE 'M001' TO LK-REASON-CD
                       ELSE
                          MOVE '20' TO LK-STATUS-KBN
                          MOVE 'N001' TO LK-REASON-CD
                       END-IF
                    END-IF
                 END-IF
              END-IF
           END-IF
           PERFORM CLOSE-RTN
           IF LK-STATUS-KBN = '99'
              MOVE 8 TO RETURN-CODE
           ELSE
              MOVE 0 TO RETURN-CODE
           END-IF
           GOBACK.

       INIT-RTN.
           MOVE ZERO  TO LK-VALUE-DT
           MOVE SPACE TO LK-STATUS-KBN
           MOVE SPACE TO LK-REASON-CD
           MOVE 'N'   TO WS-VAL-EOF
           MOVE 'N'   TO WS-CAL-EOF
           MOVE 'N'   TO WS-FOUND-FIXED
           MOVE 'N'   TO WS-FOUND-PENDING
           MOVE 'N'   TO WS-CAL-FOUND
           MOVE 'N'   TO WS-OPENED-VAL
           MOVE 'N'   TO WS-OPENED-CAL
           MOVE ZERO  TO WS-TARGET-DT.

       VALIDATE-RTN.
           IF LK-FCT-ID = SPACE
              MOVE '90' TO LK-STATUS-KBN
              MOVE 'I001' TO LK-REASON-CD
              DISPLAY 'CC280S FCT-ID REQUIRED'
           END-IF.

       OPEN-RTN.
           OPEN INPUT CCVALF
           IF WS-CCVALF-ST = '00'
              MOVE 'Y' TO WS-OPENED-VAL
           ELSE
              MOVE '99' TO LK-STATUS-KBN
              MOVE 'F001' TO LK-REASON-CD
              DISPLAY 'CC280S CCVALF OPEN ST='
                      WS-CCVALF-ST
           END-IF

           IF LK-STATUS-KBN NOT = '99'
              OPEN INPUT CCCALF
              IF WS-CCCALF-ST = '00'
                 MOVE 'Y' TO WS-OPENED-CAL
              ELSE
                 MOVE '99' TO LK-STATUS-KBN
                 MOVE 'F002' TO LK-REASON-CD
                 DISPLAY 'CC280S CCCALF OPEN ST='
                         WS-CCCALF-ST
              END-IF
           END-IF.

       SEARCH-VALUE-RTN.
           PERFORM UNTIL VAL-EOF OR FOUND-FIXED
              READ CCVALF
                 AT END
                    MOVE 'Y' TO WS-VAL-EOF
                 NOT AT END
                    IF VL-FCT-ID = LK-FCT-ID
                       EVALUATE VL-VAL-STATUS-KBN
                          WHEN '01'
                             IF VL-VALUE-DT > ZERO
                                MOVE VL-VALUE-DT TO WS-TARGET-DT
                                MOVE 'Y' TO WS-FOUND-FIXED
                             ELSE
                                MOVE 'Y' TO WS-FOUND-PENDING
                             END-IF
                          WHEN '08'
                             MOVE 'Y' TO WS-FOUND-PENDING
                          WHEN '09'
                             MOVE 'Y' TO WS-FOUND-PENDING
                          WHEN OTHER
                             MOVE '99' TO LK-STATUS-KBN
                             MOVE 'D001' TO LK-REASON-CD
                             DISPLAY 'CC280S CCVALF BAD KBN ID='
                                     VL-VAL-ID
                             MOVE 'Y' TO WS-VAL-EOF
                       END-EVALUATE
                    END-IF
              END-READ
              IF WS-CCVALF-ST NOT = '00'
                 AND WS-CCVALF-ST NOT = '10'
                 MOVE '99' TO LK-STATUS-KBN
                 MOVE 'F003' TO LK-REASON-CD
                 DISPLAY 'CC280S CCVALF READ ST='
                         WS-CCVALF-ST
                 MOVE 'Y' TO WS-VAL-EOF
              END-IF
           END-PERFORM.

       CHECK-CALENDAR-RTN.
           MOVE 'N' TO WS-CAL-EOF
           MOVE 'N' TO WS-CAL-FOUND
           PERFORM UNTIL CAL-EOF OR CAL-FOUND
              READ CCCALF
                 AT END
                    MOVE 'Y' TO WS-CAL-EOF
                 NOT AT END
                    IF CL-CAL-DT = WS-TARGET-DT
                       MOVE 'Y' TO WS-CAL-FOUND
                       EVALUATE CL-HOLIDAY-FLAG
                          WHEN 'N'
                             MOVE WS-TARGET-DT TO LK-VALUE-DT
                             MOVE '00' TO LK-STATUS-KBN
                             MOVE '0000' TO LK-REASON-CD
                          WHEN 'Y'
                             MOVE '30' TO LK-STATUS-KBN
                             MOVE 'C002' TO LK-REASON-CD
                          WHEN OTHER
                             MOVE '30' TO LK-STATUS-KBN
                             MOVE 'C003' TO LK-REASON-CD
                       END-EVALUATE
                    END-IF
              END-READ
              IF WS-CCCALF-ST NOT = '00'
                 AND WS-CCCALF-ST NOT = '10'
                 MOVE '99' TO LK-STATUS-KBN
                 MOVE 'F004' TO LK-REASON-CD
                 DISPLAY 'CC280S CCCALF READ ST='
                         WS-CCCALF-ST
                 MOVE 'Y' TO WS-CAL-EOF
              END-IF
           END-PERFORM

           IF LK-STATUS-KBN = SPACE
              MOVE '30' TO LK-STATUS-KBN
              MOVE 'C001' TO LK-REASON-CD
           END-IF.

       CLOSE-RTN.
           IF WS-OPENED-VAL = 'Y'
              CLOSE CCVALF
              IF WS-CCVALF-ST NOT = '00'
                 MOVE '99' TO LK-STATUS-KBN
                 MOVE 'F005' TO LK-REASON-CD
                 DISPLAY 'CC280S CCVALF CLOSE ST='
                         WS-CCVALF-ST
              END-IF
           END-IF

           IF WS-OPENED-CAL = 'Y'
              CLOSE CCCALF
              IF WS-CCCALF-ST NOT = '00'
                 MOVE '99' TO LK-STATUS-KBN
                 MOVE 'F006' TO LK-REASON-CD
                 DISPLAY 'CC280S CCCALF CLOSE ST='
                         WS-CCCALF-ST
              END-IF
           END-IF.
