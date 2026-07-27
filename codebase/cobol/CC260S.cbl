       IDENTIFICATION DIVISION.
       PROGRAM-ID. CC260S.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WK-WORK-AREA.
           05  WK-AMOUNT-ABS         PIC 9(13)V99 VALUE ZERO.
           05  WK-AMOUNT-EDIT        PIC ZZZZZZZZZZZZ9.99.
           05  WK-SIGN               PIC X VALUE SPACE.
           05  WK-STATE-NAME         PIC X(20) VALUE SPACE.
           05  WK-RESULT-CD          PIC X(2) VALUE SPACE.

       01  WK-CONSTANTS.
           05  WK-RESULT-NORMAL      PIC X(2) VALUE '00'.
           05  WK-RESULT-AMOUNT-ERR  PIC X(2) VALUE '02'.
           05  WK-RESULT-STATE-ERR   PIC X(2) VALUE '04'.
           05  WK-MAX-AMOUNT         PIC 9(13)V99
                                      VALUE 9999999999999.99.

       LINKAGE SECTION.
       01  CC260SLK-AMOUNT-IN     PIC S9(13)V99.
       01  CC260SLK-STATE-CD      PIC X(02).
       01  CC260SLK-AMOUNT-OUT    PIC X(20).
       01  CC260SLK-STATE-NAME    PIC X(20).
       01  CC260SLK-RESULT-CD     PIC X(02).

       PROCEDURE DIVISION USING
           CC260SLK-AMOUNT-IN
           CC260SLK-STATE-CD
           CC260SLK-AMOUNT-OUT
           CC260SLK-STATE-NAME
           CC260SLK-RESULT-CD.

       0000-MAIN.
           MOVE ZERO TO RETURN-CODE
           MOVE SPACES TO CC260SLK-AMOUNT-OUT
                          CC260SLK-STATE-NAME
           MOVE WK-RESULT-NORMAL TO CC260SLK-RESULT-CD

           PERFORM 1000-EDIT-AMOUNT

           IF CC260SLK-RESULT-CD = WK-RESULT-NORMAL
              PERFORM 2000-EDIT-STATE
           END-IF

           GOBACK.

       1000-EDIT-AMOUNT.
           MOVE WK-RESULT-NORMAL TO WK-RESULT-CD
           MOVE SPACES TO CC260SLK-AMOUNT-OUT
           MOVE ZERO TO WK-AMOUNT-ABS
           MOVE ZERO TO WK-AMOUNT-EDIT
           MOVE SPACE TO WK-SIGN

           IF CC260SLK-AMOUNT-IN < ZERO
              COMPUTE WK-AMOUNT-ABS = CC260SLK-AMOUNT-IN * -1
                 ON SIZE ERROR
                    MOVE WK-RESULT-AMOUNT-ERR TO WK-RESULT-CD
              END-COMPUTE
              MOVE '-' TO WK-SIGN
           ELSE
              MOVE CC260SLK-AMOUNT-IN TO WK-AMOUNT-ABS
              MOVE '+' TO WK-SIGN
           END-IF

           IF WK-RESULT-CD = WK-RESULT-NORMAL
              IF WK-AMOUNT-ABS > WK-MAX-AMOUNT
                 MOVE WK-RESULT-AMOUNT-ERR TO WK-RESULT-CD
              ELSE
                 MOVE WK-AMOUNT-ABS TO WK-AMOUNT-EDIT
                 STRING WK-SIGN
                        WK-AMOUNT-EDIT
                    DELIMITED BY SIZE
                    INTO CC260SLK-AMOUNT-OUT
                 END-STRING
              END-IF
           END-IF

           IF WK-RESULT-CD NOT = WK-RESULT-NORMAL
              MOVE WK-RESULT-CD TO CC260SLK-RESULT-CD
              MOVE SPACES TO CC260SLK-AMOUNT-OUT
           END-IF.

       2000-EDIT-STATE.
           MOVE SPACES TO WK-STATE-NAME

           EVALUATE CC260SLK-STATE-CD
             WHEN '00'
                MOVE '未判定' TO WK-STATE-NAME
             WHEN '01'
                MOVE '正常' TO WK-STATE-NAME
             WHEN '02'
                MOVE '要確認' TO WK-STATE-NAME
             WHEN '03'
                MOVE '承認待' TO WK-STATE-NAME
             WHEN '04'
                MOVE '差戻' TO WK-STATE-NAME
             WHEN '05'
                MOVE '停止中' TO WK-STATE-NAME
             WHEN '06'
                MOVE '解約済' TO WK-STATE-NAME
             WHEN '07'
                MOVE '更正済' TO WK-STATE-NAME
             WHEN '08'
                MOVE '取消済' TO WK-STATE-NAME
             WHEN '09'
                MOVE '保留' TO WK-STATE-NAME
             WHEN OTHER
                MOVE WK-RESULT-STATE-ERR TO CC260SLK-RESULT-CD
           END-EVALUATE

           IF CC260SLK-RESULT-CD = WK-RESULT-NORMAL
              MOVE WK-STATE-NAME TO CC260SLK-STATE-NAME
           ELSE
              MOVE SPACES TO CC260SLK-STATE-NAME
           END-IF.
