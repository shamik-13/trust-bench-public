       IDENTIFICATION DIVISION.
       PROGRAM-ID. KZ151S.
      *
      * 変更履歴
      * 版数  年月日       担当                         概要
      * 1.00  平成30年04月 システム部 勘定系チーム     新規作成
      * 1.01  令和02年10月 システム部 勘定系チーム     利率判定見直し
      * 1.02  令和05年06月 システム部 勘定系チーム     例外区分追加
      *
      * 利率コード引当サブ。
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZACCTF
               ASSIGN TO "KZACCTF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS AC-ACCT-ID
               FILE STATUS IS WS-ACCT-STAT.
           SELECT KZINTRF
               ASSIGN TO "KZINTRF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS IR-RATE-CD
               FILE STATUS IS WS-INTR-STAT.

       DATA DIVISION.
       FILE SECTION.
       FD  KZACCTF.
           COPY KZACCTC.
       FD  KZINTRF.
           COPY KZINTRFC.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05 WS-ACCT-STAT              PIC XX VALUE SPACES.
           05 WS-INTR-STAT              PIC XX VALUE SPACES.
       01  WS-FLAGS.
           05 WS-FOUND-SW               PIC X VALUE "N".
              88 FOUND-RATE                   VALUE "Y".
              88 NOT-FOUND-RATE               VALUE "N".
           05 WS-FATAL-SW               PIC X VALUE "N".
              88 FATAL-ERROR                  VALUE "Y".
              88 NOT-FATAL-ERROR              VALUE "N".
           05 WS-EXEMPT-SW              PIC X VALUE "N".
              88 EXEMPT-GROUP                 VALUE "Y".
              88 NOT-EXEMPT-GROUP             VALUE "N".
       01  WS-WORK.
           05 WS-I                      PIC 9(02) COMP VALUE ZERO.
           05 WS-CAND-MAX               PIC 9(02) COMP VALUE ZERO.
           05 WS-BEST-DT                PIC 9(08) VALUE ZERO.
           05 WS-STD-APR                PIC 9V9(4) VALUE 0.0150.
           05 WS-ZERO-APR               PIC 9V9(4) VALUE 0.0000.
           05 WS-NORM-GROUP             PIC X(04) VALUE SPACES.
           05 WS-BEST-RATE-CD           PIC X(10) VALUE SPACES.
           05 WS-BEST-APR               PIC 9V9(4) VALUE ZERO.
           05 WS-BEST-ROUND             PIC X VALUE SPACE.
       01  WS-CANDIDATES.
           05 WS-CAND-TABLE OCCURS 5 TIMES.
              10 WS-CAND-CD             PIC X(10).

       LINKAGE SECTION.
       01  LK-KZ151S-PARM.
           05 LK-ACCT-ID                PIC X(12).
           05 LK-PROCESS-DT             PIC 9(08).
           05 LK-OUT-RATE-CD            PIC X(10).
           05 LK-OUT-APR                PIC 9V9(4).
           05 LK-OUT-ROUND-MODE         PIC X.
           05 LK-REASON-CD              PIC X(04).
           05 LK-RETURN-CD              PIC 9(02).

       PROCEDURE DIVISION USING LK-KZ151S-PARM.
       0000-MAIN.
           PERFORM 1000-INITIALIZE
           PERFORM 2000-OPEN-FILES
           IF NOT FATAL-ERROR
               PERFORM 3000-READ-ACCOUNT
           END-IF
           IF NOT FATAL-ERROR
               PERFORM 4000-VALIDATE-ACCOUNT
           END-IF
           IF NOT FATAL-ERROR AND LK-RETURN-CD = ZERO
               PERFORM 5000-BUILD-CANDIDATES
               IF EXEMPT-GROUP
                   MOVE ZERO             TO LK-RETURN-CD
                   MOVE "EXM0"           TO LK-OUT-RATE-CD
                   MOVE WS-ZERO-APR      TO LK-OUT-APR
                   MOVE "N"              TO LK-OUT-ROUND-MODE
                   MOVE "0000"           TO LK-REASON-CD
               ELSE
                   PERFORM 6000-SEARCH-RATE
                   PERFORM 7000-SET-RESULT
               END-IF
           END-IF
           PERFORM 9000-CLOSE-FILES
           GOBACK
           .

       1000-INITIALIZE.
           MOVE SPACES                  TO LK-OUT-RATE-CD
           MOVE ZERO                    TO LK-OUT-APR
           MOVE SPACE                   TO LK-OUT-ROUND-MODE
           MOVE "0000"                  TO LK-REASON-CD
           MOVE ZERO                    TO LK-RETURN-CD
           SET NOT-FOUND-RATE           TO TRUE
           SET NOT-FATAL-ERROR          TO TRUE
           SET NOT-EXEMPT-GROUP         TO TRUE
           MOVE ZERO                    TO WS-BEST-DT
           MOVE ZERO                    TO WS-BEST-APR
           MOVE SPACES                  TO WS-BEST-RATE-CD
           MOVE SPACE                   TO WS-BEST-ROUND
           MOVE ZERO                    TO WS-CAND-MAX
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 5
               MOVE SPACES              TO WS-CAND-CD(WS-I)
           END-PERFORM
           .

       2000-OPEN-FILES.
           OPEN INPUT KZACCTF KZINTRF
           IF WS-ACCT-STAT NOT = "00"
               MOVE "FOPN"              TO LK-REASON-CD
               MOVE 90                  TO LK-RETURN-CD
               SET FATAL-ERROR          TO TRUE
           END-IF
           IF WS-INTR-STAT NOT = "00"
               MOVE "FOPN"              TO LK-REASON-CD
               MOVE 91                  TO LK-RETURN-CD
               SET FATAL-ERROR          TO TRUE
           END-IF
           .

       3000-READ-ACCOUNT.
           MOVE LK-ACCT-ID              TO AC-ACCT-ID
           READ KZACCTF KEY IS AC-ACCT-ID
               INVALID KEY
                   MOVE "ACNF"          TO LK-REASON-CD
                   MOVE 10              TO LK-RETURN-CD
                   MOVE WS-ZERO-APR     TO LK-OUT-APR
               NOT INVALID KEY
                   CONTINUE
           END-READ
           IF WS-ACCT-STAT NOT = "00" AND WS-ACCT-STAT NOT = "23"
               MOVE "FRED"              TO LK-REASON-CD
               MOVE 92                  TO LK-RETURN-CD
               SET FATAL-ERROR          TO TRUE
           END-IF
           .

       4000-VALIDATE-ACCOUNT.
           IF LK-RETURN-CD NOT = ZERO
               EXIT PARAGRAPH
           END-IF
           EVALUATE AC-KYC-STATUS
               WHEN "OK"
               WHEN "01"
                   CONTINUE
               WHEN "PE"
               WHEN "02"
                   CONTINUE
               WHEN "HD"
               WHEN "03"
                   MOVE "KYCH"          TO LK-REASON-CD
                   MOVE 20              TO LK-RETURN-CD
               WHEN "NG"
               WHEN "99"
                   MOVE "KYCN"          TO LK-REASON-CD
                   MOVE 21              TO LK-RETURN-CD
               WHEN OTHER
                   MOVE "KYCU"          TO LK-REASON-CD
                   MOVE 22              TO LK-RETURN-CD
           END-EVALUATE
           IF LK-RETURN-CD NOT = ZERO
               MOVE WS-ZERO-APR         TO LK-OUT-APR
               EXIT PARAGRAPH
           END-IF
           EVALUATE AC-GROUP-CODE
               WHEN "STD0"
               WHEN "GLD1"
               WHEN "PLT2"
               WHEN "EXMP"
                   MOVE AC-GROUP-CODE   TO WS-NORM-GROUP
               WHEN OTHER
                   MOVE "GRPU"          TO LK-REASON-CD
                   MOVE 30              TO LK-RETURN-CD
                   MOVE WS-ZERO-APR     TO LK-OUT-APR
           END-EVALUATE
           .

       5000-BUILD-CANDIDATES.
           EVALUATE WS-NORM-GROUP
               WHEN "STD0"
                   MOVE 3               TO WS-CAND-MAX
                   MOVE "STD0A"         TO WS-CAND-CD(1)
                   MOVE "STD0B"         TO WS-CAND-CD(2)
                   MOVE "STD0C"         TO WS-CAND-CD(3)
               WHEN "GLD1"
                   MOVE 4               TO WS-CAND-MAX
                   MOVE "GLD1A"         TO WS-CAND-CD(1)
                   MOVE "GLD1B"         TO WS-CAND-CD(2)
                   MOVE "STD0A"         TO WS-CAND-CD(3)
                   MOVE "STD0B"         TO WS-CAND-CD(4)
               WHEN "PLT2"
                   MOVE 5               TO WS-CAND-MAX
                   MOVE "PLT2A"         TO WS-CAND-CD(1)
                   MOVE "PLT2B"         TO WS-CAND-CD(2)
                   MOVE "GLD1A"         TO WS-CAND-CD(3)
                   MOVE "STD0A"         TO WS-CAND-CD(4)
                   MOVE "STD0B"         TO WS-CAND-CD(5)
               WHEN "EXMP"
                   SET EXEMPT-GROUP     TO TRUE
           END-EVALUATE
           .

       6000-SEARCH-RATE.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-CAND-MAX
               MOVE WS-CAND-CD(WS-I)    TO IR-RATE-CD
               READ KZINTRF KEY IS IR-RATE-CD
                   INVALID KEY
                       CONTINUE
                   NOT INVALID KEY
                       PERFORM 6100-EVALUATE-RATE
               END-READ
               IF WS-INTR-STAT NOT = "00" AND WS-INTR-STAT NOT = "23"
                   MOVE "FRED"          TO LK-REASON-CD
                   MOVE 93              TO LK-RETURN-CD
                   SET FATAL-ERROR      TO TRUE
                   MOVE WS-CAND-MAX     TO WS-I
               END-IF
           END-PERFORM
           .

       6100-EVALUATE-RATE.
           IF IR-EFFECTIVE-DT > LK-PROCESS-DT
               EXIT PARAGRAPH
           END-IF
           IF IR-APR < ZERO
               EXIT PARAGRAPH
           END-IF
           IF IR-ROUND-MODE NOT = "N"
              AND IR-ROUND-MODE NOT = "U"
              AND IR-ROUND-MODE NOT = "D"
               EXIT PARAGRAPH
           END-IF
           IF IR-EFFECTIVE-DT > WS-BEST-DT
               MOVE IR-EFFECTIVE-DT     TO WS-BEST-DT
               MOVE WS-CAND-CD(WS-I)    TO WS-BEST-RATE-CD
               MOVE IR-APR              TO WS-BEST-APR
               MOVE IR-ROUND-MODE       TO WS-BEST-ROUND
               SET FOUND-RATE           TO TRUE
           END-IF
           .

       7000-SET-RESULT.
           IF FATAL-ERROR
               MOVE WS-ZERO-APR         TO LK-OUT-APR
               EXIT PARAGRAPH
           END-IF
           IF FOUND-RATE
               MOVE WS-BEST-RATE-CD     TO LK-OUT-RATE-CD
               MOVE WS-BEST-APR         TO LK-OUT-APR
               MOVE WS-BEST-ROUND       TO LK-OUT-ROUND-MODE
               MOVE "0000"              TO LK-REASON-CD
               MOVE ZERO                TO LK-RETURN-CD
           ELSE
               IF WS-NORM-GROUP = "STD0"
                   MOVE "STD0F"         TO LK-OUT-RATE-CD
                   MOVE WS-STD-APR      TO LK-OUT-APR
                   MOVE "N"             TO LK-OUT-ROUND-MODE
                   MOVE "RDEF"          TO LK-REASON-CD
                   MOVE 40              TO LK-RETURN-CD
               ELSE
                   MOVE WS-ZERO-APR     TO LK-OUT-APR
                   MOVE "RNFD"          TO LK-REASON-CD
                   MOVE 41              TO LK-RETURN-CD
               END-IF
           END-IF
           .

       9000-CLOSE-FILES.
           IF WS-ACCT-STAT NOT = SPACES
               CLOSE KZACCTF
           END-IF
           IF WS-INTR-STAT NOT = SPACES
               CLOSE KZINTRF
           END-IF
           .
