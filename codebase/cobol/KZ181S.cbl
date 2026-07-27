       IDENTIFICATION DIVISION.
       PROGRAM-ID. KZ181S.
      *変更履歴
      *版数  年月日       担当                         概要
      *1.00  平成30年04月 システム部 勘定系チーム     新規作成
      *1.01  令和02年10月 システム部 勘定系チーム     判定条件整理
      *1.02  令和05年06月 システム部 勘定系チーム     戻り値整備
       AUTHOR. BATCH-DEVELOPMENT.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-WORK-AREA.
           05  WS-ABS-DIFF              PIC 9(13)V99 COMP-3.
           05  WS-TRAN-GROUP            PIC X.
           05  WS-NORMAL-FLG            PIC X.
           05  WS-VALID-FLG             PIC X.
           05  WS-SOURCE-WORK           PIC X(08).
           05  WS-ERROR-WORK            PIC X(04).

       01  WS-CONSTANTS.
           05  WC-ZERO                  PIC S9(13)V99 COMP-3
               VALUE ZERO.
           05  WC-MINOR-LIMIT           PIC 9(13)V99 COMP-3
               VALUE 1000.00.
           05  WC-WARN-LIMIT            PIC 9(13)V99 COMP-3
               VALUE 1000000.00.
           05  WC-HIGH-LIMIT            PIC 9(13)V99 COMP-3
               VALUE 10000000.00.

       01  WS-STATUS-CODES.
           05  WC-STS-ACTIVE            PIC X(02) VALUE "01".
           05  WC-STS-PENDING           PIC X(02) VALUE "02".
           05  WC-STS-REVIEW            PIC X(02) VALUE "03".
           05  WC-STS-EXEMPT            PIC X(02) VALUE "10".
           05  WC-STS-CLOSED            PIC X(02) VALUE "90".
           05  WC-STS-FROZEN            PIC X(02) VALUE "91".

       LINKAGE SECTION.
       01  LK-KZ181S-PARM.
           05  LK-KZ181S-KYC-STATUS     PIC X(02).
           05  LK-KZ181S-TRAN-CD        PIC X(04).
           05  LK-KZ181S-SA-GAKU        PIC S9(13)V99 COMP-3.
           05  LK-KZ181S-ERROR-CD       PIC X(04).
           05  LK-KZ181S-SOURCE-PGM     PIC X(08).
           05  LK-KZ181S-SEVERITY       PIC X.
           05  LK-KZ181S-OUTPUT-SW      PIC X.

       PROCEDURE DIVISION USING LK-KZ181S-PARM.
       MAIN-RTN.
           PERFORM 1000-INIT
           PERFORM 2000-CHECK-INPUT

           IF WS-VALID-FLG = "Y"
               PERFORM 3000-EDIT-BALANCE
           ELSE
               PERFORM 8000-SET-PARAM-ERROR
           END-IF

           GOBACK
           .

       1000-INIT.
           MOVE SPACE                  TO LK-KZ181S-ERROR-CD
           MOVE SPACE                  TO LK-KZ181S-SOURCE-PGM
           MOVE "0"                    TO LK-KZ181S-SEVERITY
           MOVE "N"                    TO LK-KZ181S-OUTPUT-SW
           MOVE ZERO                   TO WS-ABS-DIFF
           MOVE SPACE                  TO WS-TRAN-GROUP
           MOVE "N"                    TO WS-NORMAL-FLG
           MOVE "Y"                    TO WS-VALID-FLG
           MOVE SPACE                  TO WS-SOURCE-WORK
           MOVE SPACE                  TO WS-ERROR-WORK
           .

       2000-CHECK-INPUT.
           EVALUATE LK-KZ181S-KYC-STATUS
               WHEN WC-STS-ACTIVE
               WHEN WC-STS-PENDING
               WHEN WC-STS-REVIEW
               WHEN WC-STS-EXEMPT
               WHEN WC-STS-CLOSED
               WHEN WC-STS-FROZEN
                   CONTINUE
               WHEN OTHER
                   MOVE "N"            TO WS-VALID-FLG
           END-EVALUATE

           IF LK-KZ181S-TRAN-CD = SPACE
               MOVE "N"                TO WS-VALID-FLG
           END-IF
           .

       3000-EDIT-BALANCE.
           PERFORM 3100-CALC-ABS-DIFF
           PERFORM 3200-CLASSIFY-TRAN

           EVALUATE TRUE
               WHEN WS-ABS-DIFF = ZERO
                   MOVE "Y"            TO WS-NORMAL-FLG
                   MOVE "NORM"         TO WS-ERROR-WORK
                   MOVE "KZ181S"       TO WS-SOURCE-WORK

               WHEN LK-KZ181S-KYC-STATUS = WC-STS-EXEMPT
                   MOVE "Y"            TO WS-NORMAL-FLG
                   MOVE "NCHK"         TO WS-ERROR-WORK
                   MOVE "KZ181S"       TO WS-SOURCE-WORK

               WHEN LK-KZ181S-KYC-STATUS = WC-STS-CLOSED
                   MOVE "Y"            TO WS-NORMAL-FLG
                   MOVE "CLSD"         TO WS-ERROR-WORK
                   MOVE "KZ181S"       TO WS-SOURCE-WORK

               WHEN LK-KZ181S-KYC-STATUS = WC-STS-FROZEN
                   MOVE "Y"            TO WS-NORMAL-FLG
                   MOVE "FRZN"         TO WS-ERROR-WORK
                   MOVE "KZ181S"       TO WS-SOURCE-WORK

               WHEN OTHER
                   PERFORM 4000-SET-DIFF-ERROR
           END-EVALUATE

           IF WS-NORMAL-FLG = "Y"
               PERFORM 5000-SET-NORMAL-RETURN
           ELSE
               PERFORM 6000-SET-ERROR-RETURN
           END-IF
           .

       3100-CALC-ABS-DIFF.
           IF LK-KZ181S-SA-GAKU < WC-ZERO
               COMPUTE WS-ABS-DIFF = LK-KZ181S-SA-GAKU * -1
           ELSE
               MOVE LK-KZ181S-SA-GAKU  TO WS-ABS-DIFF
           END-IF
           .

       3200-CLASSIFY-TRAN.
           EVALUATE LK-KZ181S-TRAN-CD(1:1)
               WHEN "1"
               WHEN "2"
                   MOVE "C"            TO WS-TRAN-GROUP
               WHEN "3"
               WHEN "4"
                   MOVE "D"            TO WS-TRAN-GROUP
               WHEN "5"
                   MOVE "F"            TO WS-TRAN-GROUP
               WHEN "7"
               WHEN "8"
                   MOVE "A"            TO WS-TRAN-GROUP
               WHEN OTHER
                   MOVE "U"            TO WS-TRAN-GROUP
           END-EVALUATE
           .

       4000-SET-DIFF-ERROR.
           EVALUATE TRUE
               WHEN LK-KZ181S-KYC-STATUS = WC-STS-PENDING
                   MOVE "KYCP"         TO WS-ERROR-WORK
                   MOVE "KZKYCHK"      TO WS-SOURCE-WORK

               WHEN LK-KZ181S-KYC-STATUS = WC-STS-REVIEW
                   MOVE "KYCR"         TO WS-ERROR-WORK
                   MOVE "KZKYCHK"      TO WS-SOURCE-WORK

               WHEN WS-TRAN-GROUP = "U"
                   MOVE "TRNU"         TO WS-ERROR-WORK
                   MOVE "KZTRNED"      TO WS-SOURCE-WORK

               WHEN WS-ABS-DIFF < WC-MINOR-LIMIT
                   MOVE "BAL1"         TO WS-ERROR-WORK
                   MOVE "KZBAL01"      TO WS-SOURCE-WORK

               WHEN WS-ABS-DIFF < WC-WARN-LIMIT
                   MOVE "BAL2"         TO WS-ERROR-WORK
                   MOVE "KZBAL02"      TO WS-SOURCE-WORK

               WHEN WS-ABS-DIFF < WC-HIGH-LIMIT
                   MOVE "BAL3"         TO WS-ERROR-WORK
                   MOVE "KZBAL03"      TO WS-SOURCE-WORK

               WHEN OTHER
                   MOVE "BAL9"         TO WS-ERROR-WORK
                   MOVE "KZBAL99"      TO WS-SOURCE-WORK
           END-EVALUATE

           IF WS-TRAN-GROUP = "C" OR WS-TRAN-GROUP = "D"
               IF WS-ERROR-WORK = "BAL1" OR WS-ERROR-WORK = "BAL2"
                   MOVE "KZDEPED"      TO WS-SOURCE-WORK
               END-IF
           END-IF

           IF WS-TRAN-GROUP = "F"
               IF WS-ABS-DIFF >= WC-WARN-LIMIT
                   MOVE "KZFURED"      TO WS-SOURCE-WORK
               END-IF
           END-IF
           .

       5000-SET-NORMAL-RETURN.
           MOVE WS-ERROR-WORK          TO LK-KZ181S-ERROR-CD
           MOVE WS-SOURCE-WORK         TO LK-KZ181S-SOURCE-PGM
           MOVE "0"                    TO LK-KZ181S-SEVERITY
           MOVE "N"                    TO LK-KZ181S-OUTPUT-SW
           .

       6000-SET-ERROR-RETURN.
           MOVE WS-ERROR-WORK          TO LK-KZ181S-ERROR-CD
           MOVE WS-SOURCE-WORK         TO LK-KZ181S-SOURCE-PGM

           EVALUATE WS-ERROR-WORK
               WHEN "BAL9"
               WHEN "KYCR"
                   MOVE "3"            TO LK-KZ181S-SEVERITY
               WHEN "BAL3"
               WHEN "KYCP"
               WHEN "TRNU"
                   MOVE "2"            TO LK-KZ181S-SEVERITY
               WHEN OTHER
                   MOVE "1"            TO LK-KZ181S-SEVERITY
           END-EVALUATE

           MOVE "Y"                    TO LK-KZ181S-OUTPUT-SW
           .

       8000-SET-PARAM-ERROR.
           MOVE "PRM1"                 TO LK-KZ181S-ERROR-CD
           MOVE "KZ181S"               TO LK-KZ181S-SOURCE-PGM
           MOVE "2"                    TO LK-KZ181S-SEVERITY
           MOVE "Y"                    TO LK-KZ181S-OUTPUT-SW
           .
