       IDENTIFICATION DIVISION.
       PROGRAM-ID. CM120S.
       AUTHOR. MFG-KYOTSU-KIBAN.
      *================================================================*
      *  CIF BASIC VALIDATION SUBPROGRAM                               *
      *================================================================*

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CMCIFF ASSIGN TO "CMCIFF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-CMCIFF-ST.
           SELECT CGCODF ASSIGN TO "CGCODF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS GC-CODE-ID
               FILE STATUS IS WS-CGCODF-ST.

       DATA DIVISION.
       FILE SECTION.
       FD  CMCIFF.
           COPY CMCIFFC.
       FD  CGCODF.
           COPY CGCODC.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05  WS-CMCIFF-ST          PIC X(02) VALUE SPACES.
           05  WS-CGCODF-ST          PIC X(02) VALUE SPACES.

       01  WS-SWITCHES.
           05  WS-CMCIFF-EOF-SW      PIC X VALUE "N".
               88  CMCIFF-EOF              VALUE "Y".
               88  CMCIFF-NOT-EOF          VALUE "N".
           05  WS-CIF-FOUND-SW       PIC X VALUE "N".
               88  CIF-FOUND               VALUE "Y".
               88  CIF-NOT-FOUND           VALUE "N".
           05  WS-HARD-ERROR-SW      PIC X VALUE "N".
               88  HARD-ERROR              VALUE "Y".
               88  NO-HARD-ERROR           VALUE "N".
           05  WS-CODE-FOUND-SW      PIC X VALUE "N".
               88  CODE-FOUND              VALUE "Y".
               88  CODE-NOT-FOUND          VALUE "N".
           05  WS-CODE-VALID-SW      PIC X VALUE "N".
               88  CODE-VALID              VALUE "Y".
               88  CODE-NOT-VALID          VALUE "N".

       01  WS-CODE-WORK.
           05  WS-CODE-PREFIX        PIC X(03) VALUE SPACES.
           05  WS-CODE-VALUE         PIC X(10) VALUE SPACES.

       01  WS-DATE-WORK.
           05  WS-CURRENT-DATE-N     PIC 9(08) VALUE ZERO.
           05  WS-CURRENT-DATE-X     PIC X(08) VALUE SPACES.

       01  WS-CM190-AREA.
           05  WS-CM190-CIF-NO       PIC X(10) VALUE SPACES.
           05  WS-CM190-RC           PIC 9(02) VALUE ZERO.
           05  WS-CM190-REASON       PIC X(40) VALUE SPACES.

       01  WS-CONSTANTS.
           05  WS-SEX-KBN-ID         PIC X(03) VALUE "SEX".
           05  WS-CIF-STATUS-ID      PIC X(03) VALUE "CFS".
           05  WS-VALID-STATUS       PIC X(02) VALUE "01".
           05  WS-EXCLUDE-STATUS     PIC X(02) VALUE "08".
           05  WS-INVALID-STATUS     PIC X(02) VALUE "09".

       LINKAGE SECTION.
       01  LK-CM120-AREA.
           05  LK-CIF-NO             PIC X(10).
           05  LK-SEX-KBN            PIC X(02).
           05  LK-CIF-STATUS-KBN     PIC X(02).
           05  LK-BASE-DT            PIC 9(08).
           05  LK-RTN-CD             PIC 9(02).
           05  LK-ERR-CD             PIC X(08).
           05  LK-ERR-MSG            PIC X(80).

       PROCEDURE DIVISION USING LK-CM120-AREA.
       0000-MAIN SECTION.
       0000-MAIN-START.
           MOVE ZERO TO RETURN-CODE
           MOVE ZERO TO LK-RTN-CD
           MOVE SPACES TO LK-ERR-CD
           MOVE SPACES TO LK-ERR-MSG
           PERFORM 1000-INITIALIZE
           IF NO-HARD-ERROR
               PERFORM 2000-VALIDATE
           END-IF
           PERFORM 9000-FINALIZE
           GOBACK
           .

       1000-INITIALIZE SECTION.
       1000-INITIALIZE-START.
           SET NO-HARD-ERROR TO TRUE
           IF LK-BASE-DT = ZERO
               MOVE FUNCTION CURRENT-DATE(1:8)
                   TO WS-CURRENT-DATE-X
               MOVE FUNCTION NUMVAL(WS-CURRENT-DATE-X)
                   TO WS-CURRENT-DATE-N
           ELSE
               MOVE LK-BASE-DT TO WS-CURRENT-DATE-N
           END-IF

           OPEN INPUT CMCIFF
           IF WS-CMCIFF-ST NOT = "00"
               MOVE 12 TO LK-RTN-CD
               MOVE "CM120F01" TO LK-ERR-CD
               STRING "CMCIFF OPEN ERROR ST="
                      DELIMITED BY SIZE
                      WS-CMCIFF-ST
                      DELIMITED BY SIZE
                   INTO LK-ERR-MSG
               DISPLAY LK-ERR-MSG
               MOVE 12 TO RETURN-CODE
               SET HARD-ERROR TO TRUE
           END-IF

           IF NO-HARD-ERROR
               OPEN INPUT CGCODF
               IF WS-CGCODF-ST NOT = "00"
                   MOVE 12 TO LK-RTN-CD
                   MOVE "CM120F02" TO LK-ERR-CD
                   STRING "CGCODF OPEN ERROR ST="
                          DELIMITED BY SIZE
                          WS-CGCODF-ST
                          DELIMITED BY SIZE
                       INTO LK-ERR-MSG
                   DISPLAY LK-ERR-MSG
                   MOVE 12 TO RETURN-CODE
                   SET HARD-ERROR TO TRUE
               END-IF
           END-IF
           .

       2000-VALIDATE SECTION.
       2000-VALIDATE-START.
           PERFORM 2100-CHECK-CIF-FORMAT

           IF LK-RTN-CD = ZERO
               PERFORM 2200-SEARCH-CIF
           END-IF

           IF LK-RTN-CD = ZERO
               PERFORM 2300-CHECK-CIF-MATCH
           END-IF

           IF LK-RTN-CD = ZERO
               MOVE WS-SEX-KBN-ID TO WS-CODE-PREFIX
               MOVE LK-SEX-KBN TO WS-CODE-VALUE
               PERFORM 2400-CHECK-CODE
               IF CODE-NOT-FOUND
                   MOVE 8 TO LK-RTN-CD
                   MOVE "CM120E04" TO LK-ERR-CD
                   MOVE "SEX CODE NOT FOUND" TO LK-ERR-MSG
               ELSE
                   IF CODE-NOT-VALID
                       MOVE 8 TO LK-RTN-CD
                       MOVE "CM120E05" TO LK-ERR-CD
                       MOVE "SEX CODE OUT OF VALID DATE"
                           TO LK-ERR-MSG
                   END-IF
               END-IF
           END-IF

           IF LK-RTN-CD = ZERO
               PERFORM 2500-CHECK-STATUS-ENUM
           END-IF

           IF LK-RTN-CD = ZERO
               MOVE WS-CIF-STATUS-ID TO WS-CODE-PREFIX
               MOVE LK-CIF-STATUS-KBN TO WS-CODE-VALUE
               PERFORM 2400-CHECK-CODE
               IF CODE-NOT-FOUND
                   MOVE 8 TO LK-RTN-CD
                   MOVE "CM120E07" TO LK-ERR-CD
                   MOVE "CIF STATUS CODE NOT FOUND"
                       TO LK-ERR-MSG
               ELSE
                   IF CODE-NOT-VALID
                       MOVE 8 TO LK-RTN-CD
                       MOVE "CM120E08" TO LK-ERR-CD
                       MOVE "CIF STATUS OUT OF VALID DATE"
                           TO LK-ERR-MSG
                   END-IF
               END-IF
           END-IF
           .

       2100-CHECK-CIF-FORMAT SECTION.
       2100-CHECK-CIF-FORMAT-START.
           MOVE LK-CIF-NO TO WS-CM190-CIF-NO
           MOVE ZERO TO WS-CM190-RC
           MOVE SPACES TO WS-CM190-REASON

           CALL "CM190S" USING WS-CM190-CIF-NO
                               WS-CM190-RC
                               WS-CM190-REASON

           IF WS-CM190-RC NOT = ZERO
               MOVE 8 TO LK-RTN-CD
               MOVE "CM120E01" TO LK-ERR-CD
               STRING "INVALID CIF FORMAT "
                      DELIMITED BY SIZE
                      WS-CM190-REASON
                      DELIMITED BY SIZE
                   INTO LK-ERR-MSG
           END-IF
           .

       2200-SEARCH-CIF SECTION.
       2200-SEARCH-CIF-START.
           SET CMCIFF-NOT-EOF TO TRUE
           SET CIF-NOT-FOUND TO TRUE

           PERFORM UNTIL CMCIFF-EOF OR CIF-FOUND
               READ CMCIFF
                   AT END
                       SET CMCIFF-EOF TO TRUE
                   NOT AT END
                       IF CF-CIF-NO = LK-CIF-NO
                           SET CIF-FOUND TO TRUE
                       END-IF
               END-READ

               IF WS-CMCIFF-ST NOT = "00"
                  AND WS-CMCIFF-ST NOT = "10"
                   MOVE 12 TO LK-RTN-CD
                   MOVE "CM120F03" TO LK-ERR-CD
                   STRING "CMCIFF READ ERROR ST="
                          DELIMITED BY SIZE
                          WS-CMCIFF-ST
                          DELIMITED BY SIZE
                       INTO LK-ERR-MSG
                   DISPLAY LK-ERR-MSG
                   MOVE 12 TO RETURN-CODE
                   SET HARD-ERROR TO TRUE
                   SET CMCIFF-EOF TO TRUE
               END-IF
           END-PERFORM

           IF NO-HARD-ERROR
              AND CIF-NOT-FOUND
               MOVE 8 TO LK-RTN-CD
               MOVE "CM120E02" TO LK-ERR-CD
               MOVE "CIF NOT FOUND" TO LK-ERR-MSG
           END-IF
           .

       2300-CHECK-CIF-MATCH SECTION.
       2300-CHECK-CIF-MATCH-START.
           IF CF-SEX-KBN NOT = LK-SEX-KBN
               MOVE 8 TO LK-RTN-CD
               MOVE "CM120E03" TO LK-ERR-CD
               MOVE "SEX CODE DOES NOT MATCH CIF MASTER"
                   TO LK-ERR-MSG
           END-IF

           IF LK-RTN-CD = ZERO
               IF CF-CIF-STATUS-KBN NOT = LK-CIF-STATUS-KBN
                   MOVE 8 TO LK-RTN-CD
                   MOVE "CM120E06" TO LK-ERR-CD
                   MOVE "CIF STATUS DOES NOT MATCH MASTER"
                       TO LK-ERR-MSG
               END-IF
           END-IF
           .

       2400-CHECK-CODE SECTION.
       2400-CHECK-CODE-START.
           SET CODE-NOT-FOUND TO TRUE
           SET CODE-NOT-VALID TO TRUE

           MOVE SPACES TO GC-CODE-ID
           STRING WS-CODE-PREFIX
                  DELIMITED BY SIZE
                  WS-CODE-VALUE
                  DELIMITED BY SIZE
               INTO GC-CODE-ID

           READ CGCODF KEY IS GC-CODE-ID
               INVALID KEY
                   SET CODE-NOT-FOUND TO TRUE
               NOT INVALID KEY
                   IF GC-CODE-KBN = WS-CODE-PREFIX
                      AND GC-CODE-VALUE = WS-CODE-VALUE
                       SET CODE-FOUND TO TRUE
                       IF GC-VALID-FROM-DT <= WS-CURRENT-DATE-N
                          AND GC-VALID-TO-DT >= WS-CURRENT-DATE-N
                           SET CODE-VALID TO TRUE
                       END-IF
                   END-IF
           END-READ

           IF WS-CGCODF-ST NOT = "00"
              AND WS-CGCODF-ST NOT = "23"
               MOVE 12 TO LK-RTN-CD
               MOVE "CM120F04" TO LK-ERR-CD
               STRING "CGCODF READ ERROR ST="
                      DELIMITED BY SIZE
                      WS-CGCODF-ST
                      DELIMITED BY SIZE
                   INTO LK-ERR-MSG
               DISPLAY LK-ERR-MSG
               MOVE 12 TO RETURN-CODE
               SET HARD-ERROR TO TRUE
           END-IF
           .

       2500-CHECK-STATUS-ENUM SECTION.
       2500-CHECK-STATUS-ENUM-START.
           EVALUATE LK-CIF-STATUS-KBN
               WHEN WS-VALID-STATUS
                   CONTINUE
               WHEN WS-EXCLUDE-STATUS
                   CONTINUE
               WHEN WS-INVALID-STATUS
                   CONTINUE
               WHEN OTHER
                   MOVE 8 TO LK-RTN-CD
                   MOVE "CM120E09" TO LK-ERR-CD
                   MOVE "CIF STATUS IS OUT OF RANGE"
                       TO LK-ERR-MSG
           END-EVALUATE
           .

       9000-FINALIZE SECTION.
       9000-FINALIZE-START.
           IF WS-CMCIFF-ST NOT = SPACES
               CLOSE CMCIFF
           END-IF

           IF WS-CGCODF-ST NOT = SPACES
               CLOSE CGCODF
           END-IF

           IF LK-RTN-CD = ZERO
              AND NO-HARD-ERROR
               MOVE ZERO TO RETURN-CODE
           ELSE
               IF RETURN-CODE = ZERO
                   MOVE 8 TO RETURN-CODE
               END-IF
               IF LK-ERR-MSG NOT = SPACES
                   DISPLAY LK-ERR-MSG
               END-IF
           END-IF
           .
