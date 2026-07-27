       IDENTIFICATION DIVISION.
       PROGRAM-ID. CB100S.
       AUTHOR. 大原 修.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CDAUTHF3
               ASSIGN TO "CDAUTHF3"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS AU-AUTH-NO
               FILE STATUS IS WS-CDAUTHF3-ST.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  CDAUTHF3.
           COPY CDAUTHF3C.
      *
       WORKING-STORAGE SECTION.
       01  WS-CDAUTHF3-ST             PIC XX VALUE SPACES.
           88  WS-AU-OK               VALUE "00".
           88  WS-AU-NOTFOUND         VALUE "23".
      *
       01  WS-WORK.
           05  WS-OPENED-SW           PIC X VALUE "0".
               88  WS-OPENED          VALUE "1".
               88  WS-NOT-OPENED      VALUE "0".
           05  WS-HARD-ERR-SW         PIC X VALUE "0".
               88  WS-HARD-ERR        VALUE "1".
               88  WS-NO-HARD-ERR     VALUE "0".
           05  WS-ABS-DIFF            PIC S9(13)V99 COMP-3 VALUE 0.
           05  WS-CALC-DIFF           PIC S9(13)V99 COMP-3 VALUE 0.
           05  WS-LIMIT-AMT           PIC S9(13)V99 COMP-3 VALUE 0.
           05  WS-LIMIT-PCT           PIC S9(3)V99 COMP-3 VALUE 0.50.
           05  WS-MIN-LIMIT           PIC S9(13)V99 COMP-3 VALUE 100.
      *
       01  WS-RESULT-CODES.
           05  WS-CD-NORMAL           PIC X(2) VALUE "00".
           05  WS-CD-AMT-DIFF         PIC X(2) VALUE "10".
           05  WS-CD-NO-AUTH          PIC X(2) VALUE "20".
           05  WS-CD-DUP-CAP          PIC X(2) VALUE "30".
           05  WS-CD-MISMATCH         PIC X(2) VALUE "40".
           05  WS-CD-SYSERR           PIC X(2) VALUE "99".
      *
       LINKAGE SECTION.
       01  LK-CB100S-PARM.
           05  LK-IN-AUTH-NO          PIC X(16).
           05  LK-IN-CARD-NO          PIC X(19).
           05  LK-IN-MERCHANT-ID      PIC X(15).
           05  LK-IN-CAPTURE-AMT      PIC S9(13)V99 COMP-3.
           05  LK-IN-CAPTURE-DT       PIC 9(8).
           05  LK-OUT-JUDGE-CD        PIC X(2).
           05  LK-OUT-REASON-CD       PIC X(4).
           05  LK-OUT-AUTH-AMT        PIC S9(13)V99 COMP-3.
           05  LK-OUT-DIFF-AMT        PIC S9(13)V99 COMP-3.
      *
       PROCEDURE DIVISION USING LK-CB100S-PARM.
       0000-MAIN.
           MOVE 0 TO RETURN-CODE
           PERFORM 1000-INIT
           IF WS-NO-HARD-ERR
               PERFORM 2000-READ-AUTH
           END-IF
           IF WS-NO-HARD-ERR
               PERFORM 3000-JUDGE
           END-IF
           PERFORM 9000-FINAL
           GOBACK.
      *
       1000-INIT.
           SET WS-NOT-OPENED TO TRUE
           SET WS-NO-HARD-ERR TO TRUE
           MOVE "00" TO WS-CDAUTHF3-ST
           MOVE WS-CD-SYSERR TO LK-OUT-JUDGE-CD
           MOVE "S000" TO LK-OUT-REASON-CD
           MOVE 0 TO LK-OUT-AUTH-AMT
           MOVE 0 TO LK-OUT-DIFF-AMT
           MOVE 0 TO WS-ABS-DIFF
           MOVE 0 TO WS-CALC-DIFF
           MOVE 0 TO WS-LIMIT-AMT
      *
           IF LK-IN-AUTH-NO = SPACES
               MOVE WS-CD-SYSERR TO LK-OUT-JUDGE-CD
               MOVE "S101" TO LK-OUT-REASON-CD
               MOVE 8 TO RETURN-CODE
               SET WS-HARD-ERR TO TRUE
           ELSE
               OPEN INPUT CDAUTHF3
               IF WS-AU-OK
                   SET WS-OPENED TO TRUE
               ELSE
                   MOVE WS-CD-SYSERR TO LK-OUT-JUDGE-CD
                   MOVE "S201" TO LK-OUT-REASON-CD
                   MOVE 12 TO RETURN-CODE
                   SET WS-HARD-ERR TO TRUE
               END-IF
           END-IF.
      *
       2000-READ-AUTH.
           MOVE LK-IN-AUTH-NO TO AU-AUTH-NO
           READ CDAUTHF3 KEY IS AU-AUTH-NO
               INVALID KEY
                   IF WS-AU-NOTFOUND
                       MOVE WS-CD-NO-AUTH TO LK-OUT-JUDGE-CD
                       MOVE "A001" TO LK-OUT-REASON-CD
                   ELSE
                       MOVE WS-CD-SYSERR TO LK-OUT-JUDGE-CD
                       MOVE "S301" TO LK-OUT-REASON-CD
                       MOVE 12 TO RETURN-CODE
                       SET WS-HARD-ERR TO TRUE
                   END-IF
               NOT INVALID KEY
                   MOVE AU-AUTH-AMT TO LK-OUT-AUTH-AMT
           END-READ.
      *
       3000-JUDGE.
           IF LK-OUT-JUDGE-CD = WS-CD-NO-AUTH
               CONTINUE
           ELSE
               PERFORM 3100-CHECK-AUTH
           END-IF.
      *
       3100-CHECK-AUTH.
           IF AU-AUTH-STATUS NOT = "1"
               MOVE WS-CD-NO-AUTH TO LK-OUT-JUDGE-CD
               MOVE "A002" TO LK-OUT-REASON-CD
           ELSE
               IF AU-REV-USE-FLG = "1"
                   MOVE WS-CD-DUP-CAP TO LK-OUT-JUDGE-CD
                   MOVE "D001" TO LK-OUT-REASON-CD
               ELSE
                   PERFORM 3200-CHECK-MATCH
               END-IF
           END-IF.
      *
       3200-CHECK-MATCH.
           IF AU-MERCHANT-ID NOT = LK-IN-MERCHANT-ID
               MOVE WS-CD-MISMATCH TO LK-OUT-JUDGE-CD
               MOVE "M001" TO LK-OUT-REASON-CD
           ELSE
               IF AU-CARD-NO NOT = LK-IN-CARD-NO
                   MOVE WS-CD-MISMATCH TO LK-OUT-JUDGE-CD
                   MOVE "M002" TO LK-OUT-REASON-CD
               ELSE
                   PERFORM 3300-CHECK-AMOUNT
               END-IF
           END-IF.
      *
       3300-CHECK-AMOUNT.
           COMPUTE WS-CALC-DIFF = LK-IN-CAPTURE-AMT - AU-AUTH-AMT
           MOVE WS-CALC-DIFF TO LK-OUT-DIFF-AMT
      *
           IF WS-CALC-DIFF < 0
               COMPUTE WS-ABS-DIFF = WS-CALC-DIFF * -1
           ELSE
               MOVE WS-CALC-DIFF TO WS-ABS-DIFF
           END-IF
      *
           COMPUTE WS-LIMIT-AMT ROUNDED =
               AU-AUTH-AMT * WS-LIMIT-PCT / 100
      *
           IF WS-LIMIT-AMT < WS-MIN-LIMIT
               MOVE WS-MIN-LIMIT TO WS-LIMIT-AMT
           END-IF
      *
           IF WS-ABS-DIFF > WS-LIMIT-AMT
               MOVE WS-CD-AMT-DIFF TO LK-OUT-JUDGE-CD
               MOVE "K001" TO LK-OUT-REASON-CD
           ELSE
               MOVE WS-CD-NORMAL TO LK-OUT-JUDGE-CD
               MOVE "0000" TO LK-OUT-REASON-CD
           END-IF.
      *
       9000-FINAL.
           IF WS-OPENED
               CLOSE CDAUTHF3
               IF NOT WS-AU-OK
                   MOVE WS-CD-SYSERR TO LK-OUT-JUDGE-CD
                   MOVE "S901" TO LK-OUT-REASON-CD
                   MOVE 8 TO RETURN-CODE
               END-IF
           END-IF.
      *
       END PROGRAM CB100S.
