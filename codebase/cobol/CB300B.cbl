       IDENTIFICATION DIVISION.
       PROGRAM-ID. CB300B.
      ******************************************************************
      * 変更履歴
      * 版数  年月日    担当    概要
      * 1.00  20240210  ＣＤ運用 初版作成
      * 1.01  20240530  ＣＤ運用 年会費重複抽出判定を追加
      * 1.02  20241015  ＣＤ運用 限度額別年会費区分を整理
      ******************************************************************
      * 年会費対象カードを抽出し年会費明細を作成する
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CDCARDF ASSIGN TO "CDCARDF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS CF-CARD-NO
               FILE STATUS IS WS-CF-STATUS.

           SELECT CDMEMSTATF ASSIGN TO "CDMEMSTATF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS MS-MEMBER-ID
               FILE STATUS IS WS-MS-STATUS.

           SELECT CDFEEF-IN ASSIGN TO "CDFEEF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-FEI-STATUS.

           SELECT CDFEEF-OUT ASSIGN TO "CDFEEF.OUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-FEO-STATUS.

       DATA DIVISION.
       FILE SECTION.

       FD  CDCARDF.
       COPY CDCARDFC.

       FD  CDMEMSTATF.
       COPY CDMSTC.

       FD  CDFEEF-IN.
       COPY CDFEEC.

       FD  CDFEEF-OUT.
       COPY CDFEEC.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05 WS-CF-STATUS            PIC X(02) VALUE SPACE.
           05 WS-MS-STATUS            PIC X(02) VALUE SPACE.
           05 WS-FEI-STATUS           PIC X(02) VALUE SPACE.
           05 WS-FEO-STATUS           PIC X(02) VALUE SPACE.

       01  WS-SWITCHES.
           05 WS-CF-EOF-SW            PIC X VALUE 'N'.
              88 CF-EOF                    VALUE 'Y'.
           05 WS-FE-EOF-SW            PIC X VALUE 'N'.
              88 FE-EOF                    VALUE 'Y'.
           05 WS-HARD-ERR-SW          PIC X VALUE 'N'.
              88 HARD-ERROR                VALUE 'Y'.
           05 WS-DUP-FEE-SW           PIC X VALUE 'N'.
              88 DUP-FEE-FOUND             VALUE 'Y'.
           05 WS-MEMBER-OK-SW         PIC X VALUE 'N'.
              88 MEMBER-BILLABLE           VALUE 'Y'.
           05 WS-CARD-OK-SW           PIC X VALUE 'N'.
              88 CARD-BILLABLE             VALUE 'Y'.

       01  WS-RUN-CONTROL.
           05 WS-PROCESS-DT           PIC 9(08) VALUE ZERO.
           05 WS-PROCESS-YYYY         PIC 9(04) VALUE ZERO.
           05 WS-PROCESS-MM           PIC 9(02) VALUE ZERO.
           05 WS-CURRENT-YYYYMM       PIC 9(06) VALUE ZERO.
           05 WS-CURRENT-FY           PIC 9(04) VALUE ZERO.
           05 WS-OPEN-YYYY            PIC 9(04) VALUE ZERO.
           05 WS-OPEN-MM              PIC 9(02) VALUE ZERO.
           05 WS-OPEN-DD              PIC 9(02) VALUE ZERO.
           05 WS-FEE-YYYY             PIC 9(04) VALUE ZERO.
           05 WS-FEE-MM               PIC 9(02) VALUE ZERO.
           05 WS-FEE-DD               PIC 9(02) VALUE ZERO.
           05 WS-FEE-FY               PIC 9(04) VALUE ZERO.
           05 WS-FEE-ID-SEQ           PIC 9(06) VALUE ZERO.

       01  WS-COUNTERS.
           05 WS-CF-READ-CNT          PIC 9(09) VALUE ZERO.
           05 WS-MS-READ-CNT          PIC 9(09) VALUE ZERO.
           05 WS-FE-READ-CNT          PIC 9(09) VALUE ZERO.
           05 WS-FE-WRITE-CNT         PIC 9(09) VALUE ZERO.
           05 WS-SKIP-CNT             PIC 9(09) VALUE ZERO.
           05 WS-DUP-CNT              PIC 9(09) VALUE ZERO.
           05 WS-ERR-CNT              PIC 9(09) VALUE ZERO.

       01  WS-FEE-WORK.
           05 WS-NEW-FEE-ID           PIC X(16) VALUE SPACE.
           05 WS-NEW-FEE-AMT          PIC S9(09)V99 COMP-3 VALUE ZERO.
           05 WS-FEE-TYPE-ANNUAL      PIC X(02) VALUE 'AF'.
           05 WS-POST-STATUS-WAIT     PIC X(02) VALUE 'H '.

       01  WS-DATE-WORK.
           05 WS-ACCEPT-DATE          PIC 9(08) VALUE ZERO.
           05 WS-DATE-VALID-SW        PIC X VALUE 'N'.
              88 DATE-VALID                VALUE 'Y'.

       01  WS-MESSAGE.
           05 WS-DISP-CARD            PIC X(20) VALUE SPACE.
           05 WS-DISP-MEMBER          PIC X(20) VALUE SPACE.

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE 8 TO RETURN-CODE
           PERFORM 1000-INITIALIZE
           IF NOT HARD-ERROR
               PERFORM 2000-MAIN-PROCESS
           END-IF
           PERFORM 9000-FINALIZE
           GOBACK.

       1000-INITIALIZE.
           ACCEPT WS-ACCEPT-DATE FROM DATE YYYYMMDD
           MOVE WS-ACCEPT-DATE TO WS-PROCESS-DT
           MOVE WS-PROCESS-DT(1:4) TO WS-PROCESS-YYYY
           MOVE WS-PROCESS-DT(5:2) TO WS-PROCESS-MM
           MOVE WS-PROCESS-DT(1:6) TO WS-CURRENT-YYYYMM

           IF WS-PROCESS-MM >= 4
               MOVE WS-PROCESS-YYYY TO WS-CURRENT-FY
           ELSE
               COMPUTE WS-CURRENT-FY = WS-PROCESS-YYYY - 1
           END-IF

           OPEN INPUT CDCARDF
           IF WS-CF-STATUS NOT = '00'
               DISPLAY 'CDCARDF OPEN NG'
               DISPLAY 'ST=' WS-CF-STATUS
               SET HARD-ERROR TO TRUE
           END-IF

           IF NOT HARD-ERROR
               OPEN INPUT CDMEMSTATF
               IF WS-MS-STATUS NOT = '00'
                   DISPLAY 'CDMEMSTATF OPEN NG'
                   DISPLAY 'ST=' WS-MS-STATUS
                   SET HARD-ERROR TO TRUE
               END-IF
           END-IF

           IF NOT HARD-ERROR
               OPEN INPUT CDFEEF-IN
               IF WS-FEI-STATUS NOT = '00'
                   DISPLAY 'CDFEEF INPUT OPEN NG'
                   DISPLAY 'ST=' WS-FEI-STATUS
                   SET HARD-ERROR TO TRUE
               END-IF
           END-IF

           IF NOT HARD-ERROR
               OPEN OUTPUT CDFEEF-OUT
               IF WS-FEO-STATUS NOT = '00'
                   DISPLAY 'CDFEEF OUTPUT OPEN NG'
                   DISPLAY 'ST=' WS-FEO-STATUS
                   SET HARD-ERROR TO TRUE
               END-IF
           END-IF.

       2000-MAIN-PROCESS.
           PERFORM 2100-READ-CARD
           PERFORM UNTIL CF-EOF OR HARD-ERROR
               PERFORM 3000-EVALUATE-CARD
               IF NOT HARD-ERROR
                   PERFORM 2100-READ-CARD
               END-IF
           END-PERFORM.

       2100-READ-CARD.
           READ CDCARDF NEXT RECORD
               AT END
                   SET CF-EOF TO TRUE
               NOT AT END
                   ADD 1 TO WS-CF-READ-CNT
           END-READ
           IF WS-CF-STATUS NOT = '00'
              AND WS-CF-STATUS NOT = '10'
               DISPLAY 'CDCARDF READ NG'
               DISPLAY 'ST=' WS-CF-STATUS
               SET HARD-ERROR TO TRUE
           END-IF.

       3000-EVALUATE-CARD.
           MOVE 'N' TO WS-CARD-OK-SW
           MOVE 'N' TO WS-MEMBER-OK-SW
           MOVE 'N' TO WS-DUP-FEE-SW
           MOVE CF-CARD-NO TO WS-DISP-CARD
           MOVE CF-MEMBER-ID TO WS-DISP-MEMBER

           PERFORM 3100-VALIDATE-CARD
           IF CARD-BILLABLE
               PERFORM 3200-READ-MEMBER
           END-IF
           IF CARD-BILLABLE AND MEMBER-BILLABLE
               PERFORM 3300-CHECK-DUP-FEE
           END-IF
           IF CARD-BILLABLE
              AND MEMBER-BILLABLE
              AND NOT DUP-FEE-FOUND
               PERFORM 3400-WRITE-FEE
           END-IF.

       3100-VALIDATE-CARD.
           IF CF-CARD-NO = SPACE
               ADD 1 TO WS-ERR-CNT
               ADD 1 TO WS-SKIP-CNT
               DISPLAY 'カード番号未設定'
               DISPLAY 'CARD=' WS-DISP-CARD
               EXIT PARAGRAPH
           END-IF

           IF CF-MEMBER-ID = SPACE
               ADD 1 TO WS-ERR-CNT
               ADD 1 TO WS-SKIP-CNT
               DISPLAY '会員ID未設定'
               DISPLAY 'CARD=' WS-DISP-CARD
               EXIT PARAGRAPH
           END-IF

           IF CF-BILL-CYCLE-CD NOT = WS-PROCESS-MM
               ADD 1 TO WS-SKIP-CNT
               EXIT PARAGRAPH
           END-IF

           PERFORM 3110-VALIDATE-OPEN-DATE
           IF NOT DATE-VALID
               ADD 1 TO WS-ERR-CNT
               ADD 1 TO WS-SKIP-CNT
               DISPLAY '入会日不正'
               DISPLAY 'CARD=' WS-DISP-CARD
               EXIT PARAGRAPH
           END-IF

           IF CF-CARD-STATUS = '01'
              OR CF-CARD-STATUS = '09'
               CONTINUE
           ELSE
               ADD 1 TO WS-SKIP-CNT
               IF CF-CARD-STATUS NOT = '02'
                  AND CF-CARD-STATUS NOT = '03'
                   ADD 1 TO WS-ERR-CNT
                   DISPLAY 'カード状態不正'
                   DISPLAY 'CARD=' WS-DISP-CARD
                   DISPLAY '状態=' CF-CARD-STATUS
               END-IF
               EXIT PARAGRAPH
           END-IF

           IF CF-OPEN-DT(5:2) NOT = WS-PROCESS-MM
               ADD 1 TO WS-SKIP-CNT
               EXIT PARAGRAPH
           END-IF

           IF CF-OPEN-DT(1:6) >= WS-CURRENT-YYYYMM
               ADD 1 TO WS-SKIP-CNT
               EXIT PARAGRAPH
           END-IF

           SET CARD-BILLABLE TO TRUE.

       3110-VALIDATE-OPEN-DATE.
           MOVE 'N' TO WS-DATE-VALID-SW
           IF CF-OPEN-DT NUMERIC
               MOVE CF-OPEN-DT(1:4) TO WS-OPEN-YYYY
               MOVE CF-OPEN-DT(5:2) TO WS-OPEN-MM
               MOVE CF-OPEN-DT(7:2) TO WS-OPEN-DD
               IF WS-OPEN-YYYY >= 1980
                  AND WS-OPEN-YYYY <= WS-PROCESS-YYYY
                  AND WS-OPEN-MM >= 1
                  AND WS-OPEN-MM <= 12
                  AND WS-OPEN-DD >= 1
                  AND WS-OPEN-DD <= 31
                   IF WS-OPEN-MM = 4
                      OR WS-OPEN-MM = 6
                      OR WS-OPEN-MM = 9
                      OR WS-OPEN-MM = 11
                       IF WS-OPEN-DD <= 30
                           SET DATE-VALID TO TRUE
                       END-IF
                   ELSE
                       IF WS-OPEN-MM = 2
                           IF WS-OPEN-DD <= 29
                               SET DATE-VALID TO TRUE
                           END-IF
                       ELSE
                           SET DATE-VALID TO TRUE
                       END-IF
                   END-IF
               END-IF
           END-IF.

       3200-READ-MEMBER.
           MOVE CF-MEMBER-ID TO MS-MEMBER-ID
           READ CDMEMSTATF
               INVALID KEY
                   ADD 1 TO WS-SKIP-CNT
                   DISPLAY '会員ステータス未検出'
                   DISPLAY 'MEMBER=' WS-DISP-MEMBER
               NOT INVALID KEY
                   ADD 1 TO WS-MS-READ-CNT
                   PERFORM 3210-VALIDATE-MEMBER
           END-READ
           IF WS-MS-STATUS NOT = '00'
              AND WS-MS-STATUS NOT = '23'
               DISPLAY 'CDMEMSTATF READ NG'
               DISPLAY 'ST=' WS-MS-STATUS
               SET HARD-ERROR TO TRUE
           END-IF.

       3210-VALIDATE-MEMBER.
           IF MS-STATUS-CD = '01'
               SET MEMBER-BILLABLE TO TRUE
           ELSE
               ADD 1 TO WS-SKIP-CNT
               IF MS-STATUS-CD = SPACE
                   ADD 1 TO WS-ERR-CNT
                   DISPLAY '会員ステータス未設定'
                   DISPLAY 'MEMBER=' WS-DISP-MEMBER
               END-IF
           END-IF.

       3300-CHECK-DUP-FEE.
           CLOSE CDFEEF-IN
           IF WS-FEI-STATUS NOT = '00'
               DISPLAY 'CDFEEF INPUT CLOSE NG'
               DISPLAY 'ST=' WS-FEI-STATUS
               SET HARD-ERROR TO TRUE
               EXIT PARAGRAPH
           END-IF

           OPEN INPUT CDFEEF-IN
           IF WS-FEI-STATUS NOT = '00'
               DISPLAY 'CDFEEF INPUT REOPEN NG'
               DISPLAY 'ST=' WS-FEI-STATUS
               SET HARD-ERROR TO TRUE
               EXIT PARAGRAPH
           END-IF

           MOVE 'N' TO WS-FE-EOF-SW
           PERFORM 3310-READ-FEE
           PERFORM UNTIL FE-EOF
              OR HARD-ERROR
              OR DUP-FEE-FOUND
               IF FE-CARD-NO IN CDFEEF-REC OF CDFEEF-IN = CF-CARD-NO
                  AND FE-FEE-TYPE IN CDFEEF-REC OF CDFEEF-IN =
                      WS-FEE-TYPE-ANNUAL
                   PERFORM 3320-CHECK-FEE-YEAR
               END-IF
               IF NOT FE-EOF
                  AND NOT DUP-FEE-FOUND
                  AND NOT HARD-ERROR
                   PERFORM 3310-READ-FEE
               END-IF
           END-PERFORM

           IF DUP-FEE-FOUND
               ADD 1 TO WS-DUP-CNT
               ADD 1 TO WS-SKIP-CNT
               DISPLAY '年会費重複'
               DISPLAY 'CARD=' WS-DISP-CARD
           END-IF.

       3310-READ-FEE.
           READ CDFEEF-IN
               AT END
                   SET FE-EOF TO TRUE
               NOT AT END
                   ADD 1 TO WS-FE-READ-CNT
           END-READ
           IF WS-FEI-STATUS NOT = '00'
              AND WS-FEI-STATUS NOT = '10'
               DISPLAY 'CDFEEF READ NG'
               DISPLAY 'ST=' WS-FEI-STATUS
               SET HARD-ERROR TO TRUE
           END-IF.

       3320-CHECK-FEE-YEAR.
           IF FE-FEE-DT IN CDFEEF-REC OF CDFEEF-IN NUMERIC
               MOVE FE-FEE-DT IN CDFEEF-REC OF CDFEEF-IN(1:4)
                   TO WS-FEE-YYYY
               MOVE FE-FEE-DT IN CDFEEF-REC OF CDFEEF-IN(5:2)
                   TO WS-FEE-MM
               MOVE FE-FEE-DT IN CDFEEF-REC OF CDFEEF-IN(7:2)
                   TO WS-FEE-DD
               IF WS-FEE-MM >= 1
                  AND WS-FEE-MM <= 12
                  AND WS-FEE-DD >= 1
                  AND WS-FEE-DD <= 31
                   IF WS-FEE-MM >= 4
                       MOVE WS-FEE-YYYY TO WS-FEE-FY
                   ELSE
                       COMPUTE WS-FEE-FY = WS-FEE-YYYY - 1
                   END-IF
                   IF WS-FEE-FY = WS-CURRENT-FY
                      AND FE-POST-STATUS IN CDFEEF-REC OF CDFEEF-IN
                          NOT = 'S '
                       SET DUP-FEE-FOUND TO TRUE
                   END-IF
               END-IF
           END-IF.

       3400-WRITE-FEE.
           ADD 1 TO WS-FEE-ID-SEQ
           STRING 'AF'
                  WS-PROCESS-DT
                  WS-FEE-ID-SEQ
              DELIMITED BY SIZE
              INTO WS-NEW-FEE-ID
           END-STRING

           IF CF-CREDIT-LIMIT >= 1000000
               MOVE 12000 TO WS-NEW-FEE-AMT
           ELSE
               IF CF-CREDIT-LIMIT >= 500000
                   MOVE 6000 TO WS-NEW-FEE-AMT
               ELSE
                   MOVE 3000 TO WS-NEW-FEE-AMT
               END-IF
           END-IF

           INITIALIZE CDFEEF-REC OF CDFEEF-OUT
           MOVE WS-NEW-FEE-ID
               TO FE-FEE-ID IN CDFEEF-REC OF CDFEEF-OUT
           MOVE CF-CARD-NO
               TO FE-CARD-NO IN CDFEEF-REC OF CDFEEF-OUT
           MOVE WS-PROCESS-DT
               TO FE-FEE-DT IN CDFEEF-REC OF CDFEEF-OUT
           MOVE WS-NEW-FEE-AMT
               TO FE-FEE-AMT IN CDFEEF-REC OF CDFEEF-OUT
           MOVE WS-FEE-TYPE-ANNUAL
               TO FE-FEE-TYPE IN CDFEEF-REC OF CDFEEF-OUT
           MOVE CF-BILL-CYCLE-CD
               TO FE-BILL-CYCLE-CD IN CDFEEF-REC OF CDFEEF-OUT
           MOVE WS-POST-STATUS-WAIT
               TO FE-POST-STATUS IN CDFEEF-REC OF CDFEEF-OUT

           WRITE CDFEEF-REC OF CDFEEF-OUT
           IF WS-FEO-STATUS = '00'
               ADD 1 TO WS-FE-WRITE-CNT
           ELSE
               DISPLAY 'CDFEEF WRITE NG'
               DISPLAY 'ST=' WS-FEO-STATUS
               DISPLAY 'CARD=' WS-DISP-CARD
               SET HARD-ERROR TO TRUE
           END-IF.

       9000-FINALIZE.
           IF WS-CF-STATUS NOT = SPACE
               CLOSE CDCARDF
               IF WS-CF-STATUS NOT = '00'
                  AND WS-CF-STATUS NOT = '42'
                   DISPLAY 'CDCARDF CLOSE NG'
                   DISPLAY 'ST=' WS-CF-STATUS
                   SET HARD-ERROR TO TRUE
               END-IF
           END-IF

           IF WS-MS-STATUS NOT = SPACE
               CLOSE CDMEMSTATF
               IF WS-MS-STATUS NOT = '00'
                  AND WS-MS-STATUS NOT = '42'
                   DISPLAY 'CDMEMSTATF CLOSE NG'
                   DISPLAY 'ST=' WS-MS-STATUS
                   SET HARD-ERROR TO TRUE
               END-IF
           END-IF

           IF WS-FEI-STATUS NOT = SPACE
               CLOSE CDFEEF-IN
               IF WS-FEI-STATUS NOT = '00'
                  AND WS-FEI-STATUS NOT = '42'
                   DISPLAY 'CDFEEF INPUT CLOSE NG'
                   DISPLAY 'ST=' WS-FEI-STATUS
                   SET HARD-ERROR TO TRUE
               END-IF
           END-IF

           IF WS-FEO-STATUS NOT = SPACE
               CLOSE CDFEEF-OUT
               IF WS-FEO-STATUS NOT = '00'
                  AND WS-FEO-STATUS NOT = '42'
                   DISPLAY 'CDFEEF OUTPUT CLOSE NG'
                   DISPLAY 'ST=' WS-FEO-STATUS
                   SET HARD-ERROR TO TRUE
               END-IF
           END-IF

           DISPLAY 'CB300B 年会費抽出終了'
           DISPLAY 'カード読込件数=' WS-CF-READ-CNT
           DISPLAY '会員読込件数=' WS-MS-READ-CNT
           DISPLAY '年会費読込件数=' WS-FE-READ-CNT
           DISPLAY '年会費書込件数=' WS-FE-WRITE-CNT
           DISPLAY '対象外件数=' WS-SKIP-CNT
           DISPLAY '重複件数=' WS-DUP-CNT
           DISPLAY '警告件数=' WS-ERR-CNT

           IF HARD-ERROR
               MOVE 8 TO RETURN-CODE
               DISPLAY 'CB300B 異常終了 RC=8'
           ELSE
               MOVE 0 TO RETURN-CODE
               DISPLAY 'CB300B 正常終了 RC=0'
           END-IF.
