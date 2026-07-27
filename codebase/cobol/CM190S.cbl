       IDENTIFICATION DIVISION.
       PROGRAM-ID. CM190S.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CMCIFF ASSIGN TO "CMCIFF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-CMCIFF-ST.

       DATA DIVISION.
       FILE SECTION.
       FD  CMCIFF.
           COPY CMCIFFC.

       WORKING-STORAGE SECTION.
       01  WS-CMCIFF-ST              PIC X(02) VALUE SPACES.
       01  WS-END-SW                 PIC X(01) VALUE "0".
           88  WS-END                         VALUE "1".
           88  WS-NOT-END                     VALUE "0".
       01  WS-FOUND-SW               PIC X(01) VALUE "0".
           88  WS-FOUND                       VALUE "1".
           88  WS-NOT-FOUND                   VALUE "0".
       01  WS-OPENED-SW              PIC X(01) VALUE "0".
           88  WS-OPENED                      VALUE "1".
           88  WS-NOT-OPENED                  VALUE "0".
       01  WS-HARD-ERR-SW            PIC X(01) VALUE "0".
           88  WS-HARD-ERR                    VALUE "1".
           88  WS-NORMAL                      VALUE "0".

       LINKAGE SECTION.
       01  LK-CIF-NO                 PIC X(10).
       01  LK-CIF-STATUS-KBN         PIC X(02).
       01  LK-VALID-KBN              PIC X(02).
       01  LK-REASON-CD              PIC X(08).

       PROCEDURE DIVISION USING
           LK-CIF-NO
           LK-CIF-STATUS-KBN
           LK-VALID-KBN
           LK-REASON-CD.

       0000-MAIN.
           MOVE 0        TO RETURN-CODE
           MOVE "00"     TO LK-VALID-KBN
           MOVE SPACES   TO LK-REASON-CD
           SET WS-NORMAL TO TRUE
           SET WS-NOT-END TO TRUE
           SET WS-NOT-FOUND TO TRUE
           SET WS-NOT-OPENED TO TRUE

           PERFORM 1000-CHECK-LINKAGE

           IF LK-VALID-KBN = "00"
               PERFORM 2000-OPEN-CMCIFF
           END-IF

           IF LK-VALID-KBN = "00"
           AND WS-NORMAL
               PERFORM 3000-SEARCH-CIF
                   UNTIL WS-END OR WS-FOUND OR WS-HARD-ERR
           END-IF

           IF LK-VALID-KBN = "00"
           AND WS-NORMAL
           AND WS-NOT-FOUND
               MOVE "04"       TO LK-VALID-KBN
               MOVE "CIFNONE"  TO LK-REASON-CD
               DISPLAY "ＣＩＦ番号未登録 CIF=" LK-CIF-NO
           END-IF

           PERFORM 9000-CLOSE-CMCIFF

           IF WS-HARD-ERR
               MOVE "08"      TO LK-VALID-KBN
               MOVE 8         TO RETURN-CODE
           ELSE
               MOVE 0         TO RETURN-CODE
           END-IF

           GOBACK.

       1000-CHECK-LINKAGE.
           IF LK-CIF-NO NOT NUMERIC
               MOVE "04"       TO LK-VALID-KBN
               MOVE "CIFDIGIT" TO LK-REASON-CD
               DISPLAY "ＣＩＦ番号数字不正 CIF=" LK-CIF-NO
           END-IF

           IF LK-VALID-KBN = "00"
           AND LK-CIF-STATUS-KBN NOT = "01"
           AND LK-CIF-STATUS-KBN NOT = "08"
           AND LK-CIF-STATUS-KBN NOT = "09"
               MOVE "04"       TO LK-VALID-KBN
               MOVE "STSINVAL" TO LK-REASON-CD
               DISPLAY "ＣＩＦ状態区分不正 STS="
                   LK-CIF-STATUS-KBN
           END-IF
           .

       2000-OPEN-CMCIFF.
           OPEN INPUT CMCIFF

           IF WS-CMCIFF-ST = "00"
               SET WS-OPENED TO TRUE
           ELSE
               SET WS-HARD-ERR TO TRUE
               MOVE "OPENERR" TO LK-REASON-CD
               DISPLAY "CMCIFF オープン失敗 ST=" WS-CMCIFF-ST
           END-IF
           .

       3000-SEARCH-CIF.
           READ CMCIFF
               AT END
                   SET WS-END TO TRUE
               NOT AT END
                   PERFORM 3100-CHECK-CIF-REC
           END-READ
           .

       3100-CHECK-CIF-REC.
           IF WS-CMCIFF-ST NOT = "00"
               SET WS-HARD-ERR TO TRUE
               MOVE "READERR" TO LK-REASON-CD
               DISPLAY "CMCIFF 読込失敗 ST=" WS-CMCIFF-ST
           ELSE
               IF CF-CIF-NO = LK-CIF-NO
                   SET WS-FOUND TO TRUE
                   PERFORM 3200-CHECK-CIF-STATUS
               END-IF
           END-IF
           .

       3200-CHECK-CIF-STATUS.
           IF CF-CIF-STATUS-KBN NOT = LK-CIF-STATUS-KBN
               MOVE "04"       TO LK-VALID-KBN
               MOVE "STSDIFF"  TO LK-REASON-CD
               DISPLAY "ＣＩＦ状態区分相違 CIF=" LK-CIF-NO
               DISPLAY " IN=" LK-CIF-STATUS-KBN
               DISPLAY " DB=" CF-CIF-STATUS-KBN
           ELSE
               IF CF-CIF-STATUS-KBN NOT = "01"
                   MOVE "04"       TO LK-VALID-KBN
                   MOVE "NOKEYGEN" TO LK-REASON-CD
                   DISPLAY "キー生成対象外 CIF=" LK-CIF-NO
                   DISPLAY " STS=" CF-CIF-STATUS-KBN
               END-IF
           END-IF
           .

       9000-CLOSE-CMCIFF.
           IF WS-OPENED
               CLOSE CMCIFF
               IF WS-CMCIFF-ST NOT = "00"
                   SET WS-HARD-ERR TO TRUE
                   MOVE "CLOSERR" TO LK-REASON-CD
                   DISPLAY "CMCIFF クローズ失敗 ST=" WS-CMCIFF-ST
               END-IF
           END-IF
           .
