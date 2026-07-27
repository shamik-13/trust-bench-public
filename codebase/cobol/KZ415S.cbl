       IDENTIFICATION DIVISION.
       PROGRAM-ID. KZ415S.
      * 変更履歴
      * 版数  年月日(和暦)  担当                      概要
      * 001   平成30.04.01  システム部 勘定系チーム  新規作成
      * 002   令和02.10.15  システム部 勘定系チーム  優遇利率判定追加
      * 003   令和05.06.01  システム部 勘定系チーム  適用開始日判定見直し
       AUTHOR. KZ-BATCH.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZIRMF
               ASSIGN TO "KZIRMF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS RM-RATE-KEY
               FILE STATUS IS WS-KZIRMF-ST.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  KZIRMF.
           COPY KZIRMFC.
      *
       WORKING-STORAGE SECTION.
       01  WS-KZIRMF-ST              PIC XX VALUE SPACE.
       01  WS-EOF-SW                 PIC X VALUE "0".
           88  WS-EOF                      VALUE "1".
           88  WS-NOT-EOF                  VALUE "0".
       01  WS-FOUND-SW               PIC X VALUE "0".
           88  WS-FOUND                    VALUE "1".
           88  WS-NOT-FOUND                VALUE "0".
       01  WS-FUTURE-SW              PIC X VALUE "0".
           88  WS-FUTURE-EXIST             VALUE "1".
           88  WS-FUTURE-NONE              VALUE "0".
       01  WS-UNAPP-SW               PIC X VALUE "0".
           88  WS-UNAPP-EXIST              VALUE "1".
           88  WS-UNAPP-NONE               VALUE "0".
       01  WS-ODONLY-SW              PIC X VALUE "0".
           88  WS-ODONLY-EXIST             VALUE "1".
           88  WS-ODONLY-NONE              VALUE "0".
       01  WS-OPENED-SW              PIC X VALUE "0".
           88  WS-OPENED                   VALUE "1".
           88  WS-NOT-OPENED               VALUE "0".
       01  WS-WORK.
           05  WS-SAVE-EFF-DT        PIC 9(8) VALUE ZERO.
           05  WS-SAVE-INT-RATE      PIC S9(3)V9(6) COMP-3 VALUE ZERO.
           05  WS-SAVE-OD-RATE       PIC S9(3)V9(6) COMP-3 VALUE ZERO.
           05  WS-ERR-MSG            PIC X(80) VALUE SPACE.
      *
       LINKAGE SECTION.
       01  LK-KZ415S-PARM.
           05  LK-IN-PRODUCT-CD      PIC X(6).
           05  LK-IN-BASE-DT         PIC 9(8).
           05  LK-IN-TAX-KBN         PIC X.
           05  LK-IN-USE-KBN         PIC X.
               88  LK-USE-NORMAL           VALUE "1".
               88  LK-USE-OD               VALUE "2".
           05  LK-OUT-RESULT-CD      PIC X(2).
           05  LK-OUT-EFFECTIVE-DT   PIC 9(8).
           05  LK-OUT-INT-RATE       PIC S9(3)V9(6) COMP-3.
           05  LK-OUT-OD-RATE        PIC S9(3)V9(6) COMP-3.
           05  LK-OUT-MSG            PIC X(80).
      *
       PROCEDURE DIVISION USING LK-KZ415S-PARM.
       0000-MAIN.
           MOVE 0 TO RETURN-CODE
           PERFORM 0100-INIT
           IF RETURN-CODE = 0
              AND LK-OUT-RESULT-CD = "99"
               PERFORM 1000-SEARCH-RATE
           END-IF
           PERFORM 9000-FINAL
           GOBACK.
      *
       0100-INIT.
           MOVE 0 TO RETURN-CODE
           MOVE "99" TO LK-OUT-RESULT-CD
           MOVE ZERO TO LK-OUT-EFFECTIVE-DT
           MOVE ZERO TO LK-OUT-INT-RATE
           MOVE ZERO TO LK-OUT-OD-RATE
           MOVE SPACE TO LK-OUT-MSG
           SET WS-NOT-EOF TO TRUE
           SET WS-NOT-FOUND TO TRUE
           SET WS-FUTURE-NONE TO TRUE
           SET WS-UNAPP-NONE TO TRUE
           SET WS-ODONLY-NONE TO TRUE
           SET WS-NOT-OPENED TO TRUE
           MOVE ZERO TO WS-SAVE-EFF-DT
           MOVE ZERO TO WS-SAVE-INT-RATE
           MOVE ZERO TO WS-SAVE-OD-RATE
           MOVE SPACE TO WS-ERR-MSG
      *
           IF LK-IN-PRODUCT-CD = SPACE
               MOVE "91" TO LK-OUT-RESULT-CD
               MOVE "PRODUCT CODE REQUIRED" TO LK-OUT-MSG
               EXIT PARAGRAPH
           END-IF
      *
           IF LK-IN-BASE-DT < 19000101
              OR LK-IN-BASE-DT > 20991231
               MOVE "92" TO LK-OUT-RESULT-CD
               MOVE "INVALID BASE DATE" TO LK-OUT-MSG
               EXIT PARAGRAPH
           END-IF
      *
           IF LK-IN-TAX-KBN NOT = "1"
              AND LK-IN-TAX-KBN NOT = "2"
               MOVE "93" TO LK-OUT-RESULT-CD
               MOVE "INVALID TAX KBN" TO LK-OUT-MSG
               EXIT PARAGRAPH
           END-IF
      *
           IF LK-IN-USE-KBN NOT = "1"
              AND LK-IN-USE-KBN NOT = "2"
               MOVE "94" TO LK-OUT-RESULT-CD
               MOVE "INVALID USE KBN" TO LK-OUT-MSG
               EXIT PARAGRAPH
           END-IF
      *
           OPEN INPUT KZIRMF
           IF WS-KZIRMF-ST NOT = "00"
               MOVE 8 TO RETURN-CODE
               STRING "KZIRMF OPEN ERROR ST="
                      WS-KZIRMF-ST
                      DELIMITED BY SIZE
                      INTO WS-ERR-MSG
               END-STRING
               MOVE WS-ERR-MSG TO LK-OUT-MSG
               DISPLAY WS-ERR-MSG
               EXIT PARAGRAPH
           END-IF
           SET WS-OPENED TO TRUE.
      *
       1000-SEARCH-RATE.
           MOVE LOW-VALUES TO RM-RATE-KEY
           MOVE LK-IN-PRODUCT-CD TO RM-PRODUCT-CD
           MOVE ZERO TO RM-EFFECTIVE-DT
      *
           START KZIRMF KEY IS >= RM-RATE-KEY
           IF WS-KZIRMF-ST = "23"
               MOVE "11" TO LK-OUT-RESULT-CD
               MOVE "PRODUCT RATE NOT FOUND" TO LK-OUT-MSG
               EXIT PARAGRAPH
           END-IF
      *
           IF WS-KZIRMF-ST NOT = "00"
               MOVE 8 TO RETURN-CODE
               STRING "KZIRMF START ERROR ST="
                      WS-KZIRMF-ST
                      DELIMITED BY SIZE
                      INTO WS-ERR-MSG
               END-STRING
               MOVE WS-ERR-MSG TO LK-OUT-MSG
               DISPLAY WS-ERR-MSG
               EXIT PARAGRAPH
           END-IF
      *
           PERFORM UNTIL WS-EOF
               READ KZIRMF NEXT RECORD
                   AT END
                       SET WS-EOF TO TRUE
                   NOT AT END
                       PERFORM 1100-JUDGE-ROW
               END-READ
      *
               IF WS-KZIRMF-ST NOT = "00"
                  AND WS-KZIRMF-ST NOT = "10"
                   MOVE 8 TO RETURN-CODE
                   STRING "KZIRMF READ ERROR ST="
                          WS-KZIRMF-ST
                          DELIMITED BY SIZE
                          INTO WS-ERR-MSG
                   END-STRING
                   MOVE WS-ERR-MSG TO LK-OUT-MSG
                   DISPLAY WS-ERR-MSG
                   SET WS-EOF TO TRUE
               END-IF
           END-PERFORM
      *
           IF RETURN-CODE = 0
               PERFORM 1200-SET-RESULT
           END-IF.
      *
       1100-JUDGE-ROW.
           IF RM-PRODUCT-CD NOT = LK-IN-PRODUCT-CD
               SET WS-EOF TO TRUE
               EXIT PARAGRAPH
           END-IF
      *
           IF RM-EFFECTIVE-DT > LK-IN-BASE-DT
               SET WS-FUTURE-EXIST TO TRUE
               EXIT PARAGRAPH
           END-IF
      *
           IF RM-APPROVAL-STAT NOT = "1"
               SET WS-UNAPP-EXIST TO TRUE
               EXIT PARAGRAPH
           END-IF
      *
           IF RM-WITHHOLD-TAX-KBN NOT = LK-IN-TAX-KBN
               EXIT PARAGRAPH
           END-IF
      *
           IF LK-USE-NORMAL
              AND RM-INT-RATE = ZERO
               IF RM-OD-RATE NOT = ZERO
                   SET WS-ODONLY-EXIST TO TRUE
               END-IF
               EXIT PARAGRAPH
           END-IF
      *
           IF LK-USE-OD
              AND RM-OD-RATE = ZERO
               EXIT PARAGRAPH
           END-IF
      *
           IF RM-EFFECTIVE-DT >= WS-SAVE-EFF-DT
               SET WS-FOUND TO TRUE
               MOVE RM-EFFECTIVE-DT TO WS-SAVE-EFF-DT
               MOVE RM-INT-RATE TO WS-SAVE-INT-RATE
               MOVE RM-OD-RATE TO WS-SAVE-OD-RATE
           END-IF.
      *
       1200-SET-RESULT.
           IF WS-FOUND
               MOVE "00" TO LK-OUT-RESULT-CD
               MOVE WS-SAVE-EFF-DT TO LK-OUT-EFFECTIVE-DT
               MOVE WS-SAVE-INT-RATE TO LK-OUT-INT-RATE
               MOVE WS-SAVE-OD-RATE TO LK-OUT-OD-RATE
               MOVE "OK" TO LK-OUT-MSG
               MOVE 0 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF
      *
           IF WS-UNAPP-EXIST
               MOVE "21" TO LK-OUT-RESULT-CD
               MOVE "UNAPPROVED RATE ROW" TO LK-OUT-MSG
               MOVE 0 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF
      *
           IF WS-FUTURE-EXIST
               MOVE "22" TO LK-OUT-RESULT-CD
               MOVE "FUTURE RATE ROW ONLY" TO LK-OUT-MSG
               MOVE 0 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF
      *
           IF WS-ODONLY-EXIST
               MOVE "23" TO LK-OUT-RESULT-CD
               MOVE "OD RATE ONLY" TO LK-OUT-MSG
               MOVE 0 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF
      *
           MOVE "12" TO LK-OUT-RESULT-CD
           MOVE "NO APPLICABLE RATE" TO LK-OUT-MSG
           MOVE 0 TO RETURN-CODE.
      *
       9000-FINAL.
           IF WS-OPENED
               CLOSE KZIRMF
               IF WS-KZIRMF-ST NOT = "00"
                   MOVE 8 TO RETURN-CODE
                   STRING "KZIRMF CLOSE ERROR ST="
                          WS-KZIRMF-ST
                          DELIMITED BY SIZE
                          INTO WS-ERR-MSG
                   END-STRING
                   MOVE WS-ERR-MSG TO LK-OUT-MSG
                   DISPLAY WS-ERR-MSG
               END-IF
           END-IF.
