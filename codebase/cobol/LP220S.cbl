       IDENTIFICATION DIVISION.
       PROGRAM-ID. LP220S.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LPACCF ASSIGN TO "LPACCF"
             ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC
             RECORD KEY IS AC-POL-NO
             FILE STATUS IS WS-LPACCF-ST.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  LPACCF.
           COPY LPACCFC.
      *
       WORKING-STORAGE SECTION.
       01  WS-LPACCF-ST              PIC XX.
       01  WS-END-SW                 PIC X VALUE SPACE.
           88  WS-NORMAL-END         VALUE '0'.
           88  WS-HARD-ERR           VALUE '8'.
      *
       01  WS-ERR-MSG.
           05 WS-ERR-TEXT            PIC X(40).
           05 WS-ERR-ST              PIC XX.
      *
       01  WS-EDIT-WK.
           05 WS-BANK-NUM            PIC 9(04).
           05 WS-BRANCH-NUM          PIC 9(03).
           05 WS-ACCOUNT-NUM         PIC 9(07).
           05 WS-DAY-NUM             PIC 9(02).
           05 WS-KANA-LEN            PIC 9(03).
           05 WS-IDX                 PIC 9(03).
           05 WS-BAD-CNT             PIC 9(03).
      *
       LINKAGE SECTION.
       01  LK-LP220S-PARM.
           05 LK-POL-NO              PIC X(12).
           05 LK-RESULT-KBN          PIC X.
           05 LK-FURIKAE-FUKA-CD     PIC X(02).
           05 LK-REASON-CD           PIC X(04).
           05 LK-REASON-TEXT         PIC X(40).
      *
       PROCEDURE DIVISION USING LK-LP220S-PARM.
       MAIN-RTN.
           MOVE 0 TO RETURN-CODE
           MOVE '0' TO LK-RESULT-KBN
           MOVE '00' TO LK-FURIKAE-FUKA-CD
           MOVE '0000' TO LK-REASON-CD
           MOVE SPACE TO LK-REASON-TEXT
           SET WS-NORMAL-END TO TRUE
      *
           PERFORM OPEN-RTN
           IF WS-HARD-ERR
              GOBACK
           END-IF
      *
           PERFORM READ-RTN
           IF WS-HARD-ERR
              PERFORM CLOSE-RTN
              GOBACK
           END-IF
      *
           IF LK-RESULT-KBN = '0'
              PERFORM CHECK-ACCOUNT-RTN
           END-IF
      *
           PERFORM CLOSE-RTN
           GOBACK.
      *
       OPEN-RTN.
           OPEN INPUT LPACCF
           IF WS-LPACCF-ST NOT = '00'
              MOVE 12 TO RETURN-CODE
              SET WS-HARD-ERR TO TRUE
              MOVE 'LPACCF OPEN ERROR ST=' TO WS-ERR-TEXT
              MOVE WS-LPACCF-ST TO WS-ERR-ST
              DISPLAY WS-ERR-MSG
              MOVE '1' TO LK-RESULT-KBN
              MOVE '99' TO LK-FURIKAE-FUKA-CD
              MOVE 'E001' TO LK-REASON-CD
              MOVE 'LPACCF OPEN ERROR' TO LK-REASON-TEXT
           END-IF.
      *
       READ-RTN.
           MOVE LK-POL-NO TO AC-POL-NO
           READ LPACCF
             INVALID KEY
               IF WS-LPACCF-ST = '23'
                  MOVE '1' TO LK-RESULT-KBN
                  MOVE '11' TO LK-FURIKAE-FUKA-CD
                  MOVE 'N001' TO LK-REASON-CD
                  MOVE 'ACCOUNT NOT FOUND' TO LK-REASON-TEXT
               ELSE
                  MOVE 8 TO RETURN-CODE
                  SET WS-HARD-ERR TO TRUE
                  MOVE 'LPACCF READ ERROR ST=' TO WS-ERR-TEXT
                  MOVE WS-LPACCF-ST TO WS-ERR-ST
                  DISPLAY WS-ERR-MSG
                  MOVE '1' TO LK-RESULT-KBN
                  MOVE '99' TO LK-FURIKAE-FUKA-CD
                  MOVE 'E002' TO LK-REASON-CD
                  MOVE 'LPACCF READ ERROR' TO LK-REASON-TEXT
               END-IF
             NOT INVALID KEY
               CONTINUE
           END-READ.
      *
       CHECK-ACCOUNT-RTN.
           EVALUATE AC-ACCOUNT-STATUS-KBN
             WHEN '1'
               MOVE '1' TO LK-RESULT-KBN
               MOVE '21' TO LK-FURIKAE-FUKA-CD
               MOVE 'S001' TO LK-REASON-CD
               MOVE 'ACCOUNT STOPPED' TO LK-REASON-TEXT
             WHEN '2'
               MOVE '1' TO LK-RESULT-KBN
               MOVE '22' TO LK-FURIKAE-FUKA-CD
               MOVE 'S002' TO LK-REASON-CD
               MOVE 'ACCOUNT UNCONFIRMED' TO LK-REASON-TEXT
             WHEN '9'
               MOVE '1' TO LK-RESULT-KBN
               MOVE '23' TO LK-FURIKAE-FUKA-CD
               MOVE 'S003' TO LK-REASON-CD
               MOVE 'ACCOUNT CLOSED' TO LK-REASON-TEXT
             WHEN '0'
               PERFORM CHECK-FORMAT-RTN
             WHEN OTHER
               MOVE '1' TO LK-RESULT-KBN
               MOVE '24' TO LK-FURIKAE-FUKA-CD
               MOVE 'S009' TO LK-REASON-CD
               MOVE 'BAD ACCOUNT STATUS' TO LK-REASON-TEXT
           END-EVALUATE.
      *
       CHECK-FORMAT-RTN.
           IF AC-BANK-CD NOT NUMERIC
              MOVE '1' TO LK-RESULT-KBN
              MOVE '31' TO LK-FURIKAE-FUKA-CD
              MOVE 'F001' TO LK-REASON-CD
              MOVE 'BANK CODE NOT NUMERIC' TO LK-REASON-TEXT
           ELSE
              MOVE AC-BANK-CD TO WS-BANK-NUM
              IF WS-BANK-NUM = ZERO
                 MOVE '1' TO LK-RESULT-KBN
                 MOVE '31' TO LK-FURIKAE-FUKA-CD
                 MOVE 'F002' TO LK-REASON-CD
                 MOVE 'BANK CODE ZERO' TO LK-REASON-TEXT
              END-IF
           END-IF
      *
           IF LK-RESULT-KBN = '0'
              IF AC-BRANCH-CD NOT NUMERIC
                 MOVE '1' TO LK-RESULT-KBN
                 MOVE '32' TO LK-FURIKAE-FUKA-CD
                 MOVE 'F003' TO LK-REASON-CD
                 MOVE 'BRANCH CODE NOT NUMERIC' TO LK-REASON-TEXT
              ELSE
                 MOVE AC-BRANCH-CD TO WS-BRANCH-NUM
                 IF WS-BRANCH-NUM = ZERO
                    MOVE '1' TO LK-RESULT-KBN
                    MOVE '32' TO LK-FURIKAE-FUKA-CD
                    MOVE 'F004' TO LK-REASON-CD
                    MOVE 'BRANCH CODE ZERO' TO LK-REASON-TEXT
                 END-IF
              END-IF
           END-IF
      *
           IF LK-RESULT-KBN = '0'
              IF AC-ACCOUNT-NO NOT NUMERIC
                 MOVE '1' TO LK-RESULT-KBN
                 MOVE '33' TO LK-FURIKAE-FUKA-CD
                 MOVE 'F005' TO LK-REASON-CD
                 MOVE 'ACCOUNT NO NOT NUMERIC' TO LK-REASON-TEXT
              ELSE
                 MOVE AC-ACCOUNT-NO TO WS-ACCOUNT-NUM
                 IF WS-ACCOUNT-NUM = ZERO
                    MOVE '1' TO LK-RESULT-KBN
                    MOVE '33' TO LK-FURIKAE-FUKA-CD
                    MOVE 'F006' TO LK-REASON-CD
                    MOVE 'ACCOUNT NO ZERO' TO LK-REASON-TEXT
                 END-IF
              END-IF
           END-IF
      *
           IF LK-RESULT-KBN = '0'
              PERFORM CHECK-KANA-RTN
           END-IF
      *
           IF LK-RESULT-KBN = '0'
              IF AC-TRANSFER-DAY NOT NUMERIC
                 MOVE '1' TO LK-RESULT-KBN
                 MOVE '35' TO LK-FURIKAE-FUKA-CD
                 MOVE 'F010' TO LK-REASON-CD
                 MOVE 'TRANSFER DAY NOT NUMERIC' TO LK-REASON-TEXT
              ELSE
                 MOVE AC-TRANSFER-DAY TO WS-DAY-NUM
                 IF WS-DAY-NUM < 1 OR WS-DAY-NUM > 31
                    MOVE '1' TO LK-RESULT-KBN
                    MOVE '35' TO LK-FURIKAE-FUKA-CD
                    MOVE 'F011' TO LK-REASON-CD
                    MOVE 'TRANSFER DAY RANGE' TO LK-REASON-TEXT
                 END-IF
              END-IF
           END-IF.
      *
       CHECK-KANA-RTN.
           MOVE ZERO TO WS-KANA-LEN
           MOVE ZERO TO WS-BAD-CNT
           INSPECT AC-ACCOUNT-HOLDER-KANA
             TALLYING WS-KANA-LEN
             FOR CHARACTERS BEFORE INITIAL SPACE
      *
           IF WS-KANA-LEN = ZERO
              MOVE '1' TO LK-RESULT-KBN
              MOVE '34' TO LK-FURIKAE-FUKA-CD
              MOVE 'F007' TO LK-REASON-CD
              MOVE 'KANA NAME MISSING' TO LK-REASON-TEXT
           END-IF
      *
           IF LK-RESULT-KBN = '0'
              INSPECT AC-ACCOUNT-HOLDER-KANA
                TALLYING WS-BAD-CNT FOR ALL LOW-VALUE
              INSPECT AC-ACCOUNT-HOLDER-KANA
                TALLYING WS-BAD-CNT FOR ALL HIGH-VALUE
              IF WS-BAD-CNT > ZERO
                 MOVE '1' TO LK-RESULT-KBN
                 MOVE '34' TO LK-FURIKAE-FUKA-CD
                 MOVE 'F008' TO LK-REASON-CD
                 MOVE 'KANA NAME CONTROL CHAR' TO LK-REASON-TEXT
              END-IF
           END-IF
      *
           IF LK-RESULT-KBN = '0'
              PERFORM VARYING WS-IDX FROM 1 BY 1
                UNTIL WS-IDX > WS-KANA-LEN
                 IF AC-ACCOUNT-HOLDER-KANA(WS-IDX:1) = ','
                    OR AC-ACCOUNT-HOLDER-KANA(WS-IDX:1) = QUOTE
                    OR AC-ACCOUNT-HOLDER-KANA(WS-IDX:1) = "'"
                    MOVE '1' TO LK-RESULT-KBN
                    MOVE '34' TO LK-FURIKAE-FUKA-CD
                    MOVE 'F009' TO LK-REASON-CD
                    MOVE 'KANA NAME BAD CHAR' TO LK-REASON-TEXT
                    MOVE WS-KANA-LEN TO WS-IDX
                 END-IF
              END-PERFORM
           END-IF.
      *
       CLOSE-RTN.
           CLOSE LPACCF
           IF WS-LPACCF-ST NOT = '00'
              MOVE 8 TO RETURN-CODE
              MOVE 'LPACCF CLOSE ERROR ST=' TO WS-ERR-TEXT
              MOVE WS-LPACCF-ST TO WS-ERR-ST
              DISPLAY WS-ERR-MSG
              MOVE '1' TO LK-RESULT-KBN
              MOVE '99' TO LK-FURIKAE-FUKA-CD
              MOVE 'E003' TO LK-REASON-CD
              MOVE 'LPACCF CLOSE ERROR' TO LK-REASON-TEXT
           END-IF.
