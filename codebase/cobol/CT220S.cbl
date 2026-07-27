       IDENTIFICATION DIVISION.
       PROGRAM-ID. CT220S.
       AUTHOR. MFG-KYOTSU.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CCINSF ASSIGN TO "CCINSF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS IN-INS-ID
               FILE STATUS IS WS-CCINSF-ST.
           SELECT CCPOSF ASSIGN TO "CCPOSF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS PS-ORG-CD
               FILE STATUS IS WS-CCPOSF-ST.

       DATA DIVISION.
       FILE SECTION.
       FD  CCINSF.
           COPY CCINSC.
       FD  CCPOSF.
           COPY CCPOSC.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05 WS-CCINSF-ST             PIC XX VALUE SPACE.
           05 WS-CCPOSF-ST             PIC XX VALUE SPACE.

       01  WS-WORK-AREA.
           05 WS-REMAIN-AMT            PIC S9(13)V99 COMP-3 VALUE 0.
           05 WS-HARD-ERR-SW           PIC X VALUE '0'.
              88 WS-HARD-ERR          VALUE '1'.

       01  WS-CONST.
           05 WC-OK                    PIC X(2) VALUE '00'.
           05 WC-NOTFND                PIC X(2) VALUE '23'.
           05 WC-STAT-RECV             PIC X(2) VALUE '01'.
           05 WC-STAT-RETRY            PIC X(2) VALUE '02'.
           05 WC-POS-ACTIVE            PIC X VALUE '1'.
           05 WC-RC-NORMAL             PIC 9(4) VALUE 0.
           05 WC-RC-ERROR              PIC 9(4) VALUE 8.

       LINKAGE SECTION.
       01  LK-CT220S-PARM.
           05 LK-INS-ID                PIC X(16).
           05 LK-APPROVAL-KBN          PIC X.
              88 LK-APPROVAL-OK       VALUE '1'.
              88 LK-APPROVAL-NG       VALUE '0'.
           05 LK-REASON-CD             PIC X(4).
           05 LK-REASON-TEXT           PIC X(40).
           05 LK-REMAIN-AMT            PIC S9(13)V99 COMP-3.

       PROCEDURE DIVISION USING LK-CT220S-PARM.

       0000-MAIN.
           PERFORM 1000-INIT
           IF NOT WS-HARD-ERR
               PERFORM 2000-READ-INSTRUCTION
           END-IF
           IF NOT WS-HARD-ERR
               PERFORM 3000-CHECK-INSTRUCTION
           END-IF
           PERFORM 9000-END
           GOBACK.

       1000-INIT.
           MOVE WC-RC-NORMAL TO RETURN-CODE
           SET LK-APPROVAL-NG TO TRUE
           MOVE '0000' TO LK-REASON-CD
           MOVE SPACE TO LK-REASON-TEXT
           MOVE 0 TO LK-REMAIN-AMT
           OPEN INPUT CCINSF CCPOSF
           IF WS-CCINSF-ST NOT = WC-OK
               DISPLAY 'CCINSF OPEN ERR ST=' WS-CCINSF-ST
               MOVE '1' TO WS-HARD-ERR-SW
               MOVE WC-RC-ERROR TO RETURN-CODE
           END-IF
           IF WS-CCPOSF-ST NOT = WC-OK
               DISPLAY 'CCPOSF OPEN ERR ST=' WS-CCPOSF-ST
               MOVE '1' TO WS-HARD-ERR-SW
               MOVE WC-RC-ERROR TO RETURN-CODE
           END-IF.

       2000-READ-INSTRUCTION.
           IF LK-INS-ID = SPACE
               MOVE '1010' TO LK-REASON-CD
               MOVE 'INS-ID IS BLANK' TO LK-REASON-TEXT
               EXIT PARAGRAPH
           END-IF
           MOVE LK-INS-ID TO IN-INS-ID
           READ CCINSF KEY IS IN-INS-ID
               INVALID KEY
                   IF WS-CCINSF-ST = WC-NOTFND
                       MOVE '1020' TO LK-REASON-CD
                       MOVE 'INSTRUCTION NOT FOUND'
                           TO LK-REASON-TEXT
                   ELSE
                       DISPLAY 'CCINSF READ ERR ST=' WS-CCINSF-ST
                       MOVE '1' TO WS-HARD-ERR-SW
                       MOVE WC-RC-ERROR TO RETURN-CODE
                   END-IF
           END-READ.

       3000-CHECK-INSTRUCTION.
           IF LK-REASON-CD NOT = '0000'
               EXIT PARAGRAPH
           END-IF

           IF IN-INSTR-AMT <= 0
               MOVE '2010' TO LK-REASON-CD
               MOVE 'AMOUNT NOT POSITIVE'
                   TO LK-REASON-TEXT
               EXIT PARAGRAPH
           END-IF

           IF IN-ORG-CD = SPACE
               MOVE '2020' TO LK-REASON-CD
               MOVE 'ORG-CD IS BLANK' TO LK-REASON-TEXT
               EXIT PARAGRAPH
           END-IF

           IF IN-INSTR-STATUS-KBN NOT = WC-STAT-RECV
              AND IN-INSTR-STATUS-KBN NOT = WC-STAT-RETRY
               MOVE '2030' TO LK-REASON-CD
               MOVE 'STATUS NOT TARGET'
                   TO LK-REASON-TEXT
               EXIT PARAGRAPH
           END-IF

           PERFORM 3100-READ-POSITION
           IF LK-REASON-CD NOT = '0000' OR WS-HARD-ERR
               EXIT PARAGRAPH
           END-IF

           IF PS-POSITION-STATUS-KBN NOT = WC-POS-ACTIVE
               MOVE '3010' TO LK-REASON-CD
               MOVE 'POSITION STATUS ERROR'
                   TO LK-REASON-TEXT
               EXIT PARAGRAPH
           END-IF

           COMPUTE WS-REMAIN-AMT =
               PS-AVAILABLE-AMT - PS-RESERVED-AMT
           MOVE WS-REMAIN-AMT TO LK-REMAIN-AMT

           IF WS-REMAIN-AMT < IN-INSTR-AMT
               MOVE '3020' TO LK-REASON-CD
               MOVE 'INSUFFICIENT BALANCE'
                   TO LK-REASON-TEXT
           ELSE
               SET LK-APPROVAL-OK TO TRUE
               MOVE '0000' TO LK-REASON-CD
               MOVE 'APPROVED' TO LK-REASON-TEXT
           END-IF.

       3100-READ-POSITION.
           MOVE IN-ORG-CD TO PS-ORG-CD
           READ CCPOSF KEY IS PS-ORG-CD
               INVALID KEY
                   IF WS-CCPOSF-ST = WC-NOTFND
                       MOVE '2040' TO LK-REASON-CD
                       MOVE 'POSITION NOT FOUND'
                           TO LK-REASON-TEXT
                   ELSE
                       DISPLAY 'CCPOSF READ ERR ST=' WS-CCPOSF-ST
                       MOVE '1' TO WS-HARD-ERR-SW
                       MOVE WC-RC-ERROR TO RETURN-CODE
                   END-IF
           END-READ.

       9000-END.
           IF WS-CCINSF-ST NOT = SPACE
               CLOSE CCINSF
               IF WS-CCINSF-ST NOT = WC-OK
                   DISPLAY 'CCINSF CLOSE ERR ST=' WS-CCINSF-ST
                   MOVE WC-RC-ERROR TO RETURN-CODE
               END-IF
           END-IF
           IF WS-CCPOSF-ST NOT = SPACE
               CLOSE CCPOSF
               IF WS-CCPOSF-ST NOT = WC-OK
                   DISPLAY 'CCPOSF CLOSE ERR ST=' WS-CCPOSF-ST
                   MOVE WC-RC-ERROR TO RETURN-CODE
               END-IF
           END-IF.
