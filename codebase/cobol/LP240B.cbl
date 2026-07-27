       IDENTIFICATION DIVISION.
       PROGRAM-ID. LP240B.
       AUTHOR. BATCH-TEAM.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LPCLMF
               ASSIGN TO "LPCLMF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CL-CLAIM-ID
               FILE STATUS IS WS-LPCLMF-ST.

           SELECT LPPAYF
               ASSIGN TO "LPPAYF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-LPPAYF-ST.

           SELECT LFPRMF
               ASSIGN TO "LFPRMF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-LFPRMF-ST.

       DATA DIVISION.
       FILE SECTION.
       FD  LPCLMF.
           COPY LPCLMFC.

       FD  LPPAYF.
           COPY LPPAYFC.

       FD  LFPRMF.
           COPY LFPRMFC.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05 WS-LPCLMF-ST              PIC XX VALUE SPACES.
           05 WS-LPPAYF-ST              PIC XX VALUE SPACES.
           05 WS-LFPRMF-ST              PIC XX VALUE SPACES.

       01  WS-FLAGS.
           05 WS-EOF-LPPAYF             PIC X VALUE 'N'.
              88 LPPAYF-END                  VALUE 'Y'.
           05 WS-EOF-LFPRMF             PIC X VALUE 'N'.
              88 LFPRMF-END                  VALUE 'Y'.
           05 WS-EOF-LPCLMF             PIC X VALUE 'N'.
              88 LPCLMF-END                  VALUE 'Y'.
           05 WS-CLAIM-FOUND-SW         PIC X VALUE 'N'.
              88 CLAIM-FOUND                 VALUE 'Y'.
           05 WS-PRM-FOUND-SW           PIC X VALUE 'N'.
              88 PRM-FOUND                   VALUE 'Y'.
           05 WS-HARD-ERROR-SW          PIC X VALUE 'N'.
              88 HARD-ERROR                  VALUE 'Y'.

       01  WS-COUNTERS.
           05 WS-PAY-READ-CNT           PIC 9(9) VALUE ZERO.
           05 WS-CLAIM-MATCH-CNT        PIC 9(9) VALUE ZERO.
           05 WS-OVER-CNT               PIC 9(9) VALUE ZERO.
           05 WS-SHORT-CNT              PIC 9(9) VALUE ZERO.
           05 WS-NOCLAIM-CNT            PIC 9(9) VALUE ZERO.
           05 WS-HOLD-CNT               PIC 9(9) VALUE ZERO.
           05 WS-ERROR-CNT              PIC 9(9) VALUE ZERO.
           05 WS-PRM-LOAD-CNT           PIC 9(9) VALUE ZERO.

       01  WS-AMOUNTS.
           05 WS-DIFF-AMT               PIC S9(11)V99 VALUE ZERO.
           05 WS-BILL-AMT               PIC S9(11)V99 VALUE ZERO.
           05 WS-PAY-AMT                PIC S9(11)V99 VALUE ZERO.
           05 WS-NEW-RECEIPT-AMT        PIC S9(11)V99 VALUE ZERO.

       01  WS-WORK.
           05 WS-LOW-CLAIM-ID           PIC X(30) VALUE LOW-VALUES.
           05 WS-MSG-ST                 PIC XX VALUE SPACES.

       01  WS-PRM-TABLE.
           05 WS-PRM-MAX                PIC 9(5) VALUE 20000.
           05 WS-PRM-CNT                PIC 9(5) VALUE ZERO.
           05 WS-PRM-IDX                PIC 9(5) VALUE ZERO.
           05 WS-PRM-ENT OCCURS 20000 TIMES.
              10 WS-T-PRM-ID            PIC X(30).
              10 WS-T-POL-NO            PIC X(20).
              10 WS-T-SUM-ASSURED-AMT   PIC 9(11)V99.
              10 WS-T-PRM-AMT           PIC 9(11)V99.
              10 WS-T-BAND-KBN          PIC X(2).
              10 WS-T-CALC-STATUS-KBN   PIC X(2).

       01  WS-SAVE-PRM.
           05 WS-S-PRM-ID               PIC X(30) VALUE SPACES.
           05 WS-S-CALC-STATUS-KBN      PIC X(2)  VALUE SPACES.

       01  WS-CONSTANTS.
           05 CT-NORMAL                 PIC X VALUE '0'.
           05 CT-OVER                   PIC X VALUE '1'.
           05 CT-SHORT                  PIC X VALUE '2'.
           05 CT-NOCLAIM                PIC X VALUE '3'.
           05 CT-HOLD                   PIC X VALUE '4'.
           05 CT-ERROR                  PIC X VALUE '9'.
           05 CT-CLAIM-OPEN             PIC X(2) VALUE '01'.
           05 CT-CLAIM-PAID             PIC X(2) VALUE '02'.
           05 CT-CLAIM-PART             PIC X(2) VALUE '03'.
           05 CT-CLAIM-HOLD             PIC X(2) VALUE '04'.
           05 CT-CALC-OK                PIC X(2) VALUE '00'.

       PROCEDURE DIVISION.
       0000-MAIN SECTION.
           PERFORM 1000-INITIALIZE
           IF NOT HARD-ERROR
              PERFORM 2000-LOAD-LFPRMF
           END-IF
           IF NOT HARD-ERROR
              PERFORM 3000-PROCESS-PAYMENTS
           END-IF
           PERFORM 9000-FINALIZE
           GOBACK
           .

       1000-INITIALIZE SECTION.
           MOVE 0 TO RETURN-CODE
           OPEN I-O LPCLMF
           IF WS-LPCLMF-ST NOT = '00'
              MOVE WS-LPCLMF-ST TO WS-MSG-ST
              DISPLAY 'LPCLMF オープン失敗 ST=' WS-MSG-ST
              MOVE 'Y' TO WS-HARD-ERROR-SW
              MOVE 12 TO RETURN-CODE
           END-IF

           IF NOT HARD-ERROR
              OPEN I-O LPPAYF
              IF WS-LPPAYF-ST NOT = '00'
                 MOVE WS-LPPAYF-ST TO WS-MSG-ST
                 DISPLAY 'LPPAYF オープン失敗 ST=' WS-MSG-ST
                 MOVE 'Y' TO WS-HARD-ERROR-SW
                 MOVE 12 TO RETURN-CODE
              END-IF
           END-IF

           IF NOT HARD-ERROR
              OPEN INPUT LFPRMF
              IF WS-LFPRMF-ST NOT = '00'
                 MOVE WS-LFPRMF-ST TO WS-MSG-ST
                 DISPLAY 'LFPRMF オープン失敗 ST=' WS-MSG-ST
                 MOVE 'Y' TO WS-HARD-ERROR-SW
                 MOVE 12 TO RETURN-CODE
              END-IF
           END-IF
           .

       2000-LOAD-LFPRMF SECTION.
           PERFORM UNTIL LFPRMF-END OR HARD-ERROR
              READ LFPRMF
                 AT END
                    MOVE 'Y' TO WS-EOF-LFPRMF
                 NOT AT END
                    PERFORM 2100-STORE-PRM
              END-READ
           END-PERFORM
           .

       2100-STORE-PRM SECTION.
           IF WS-PRM-CNT >= WS-PRM-MAX
              DISPLAY 'LFPRMF 明細件数上限超過'
              MOVE 'Y' TO WS-HARD-ERROR-SW
              MOVE 12 TO RETURN-CODE
           ELSE
              ADD 1 TO WS-PRM-CNT
              MOVE PR-PRM-ID TO WS-T-PRM-ID(WS-PRM-CNT)
              MOVE PR-POL-NO TO WS-T-POL-NO(WS-PRM-CNT)
              MOVE PR-SUM-ASSURED-AMT
                   TO WS-T-SUM-ASSURED-AMT(WS-PRM-CNT)
              MOVE PR-PRM-AMT TO WS-T-PRM-AMT(WS-PRM-CNT)
              MOVE PR-BAND-KBN TO WS-T-BAND-KBN(WS-PRM-CNT)
              MOVE PR-CALC-STATUS-KBN
                   TO WS-T-CALC-STATUS-KBN(WS-PRM-CNT)
              ADD 1 TO WS-PRM-LOAD-CNT
           END-IF
           .

       3000-PROCESS-PAYMENTS SECTION.
           PERFORM UNTIL LPPAYF-END OR HARD-ERROR
              READ LPPAYF
                 AT END
                    MOVE 'Y' TO WS-EOF-LPPAYF
                 NOT AT END
                    ADD 1 TO WS-PAY-READ-CNT
                    PERFORM 3100-VALIDATE-PAYMENT
                    IF NOT HARD-ERROR
                       PERFORM 3200-MATCH-ONE-PAYMENT
                    END-IF
              END-READ
           END-PERFORM
           .

       3100-VALIDATE-PAYMENT SECTION.
           IF PY-PAY-ID = SPACES
              DISPLAY '入金番号未設定'
              MOVE CT-ERROR TO PY-MATCH-STATUS-KBN
              ADD 1 TO WS-ERROR-CNT
              PERFORM 3800-REWRITE-PAYMENT
           ELSE
              IF PY-POL-NO = SPACES
                 DISPLAY '契約番号未設定 入金番号='
                         PY-PAY-ID
                 MOVE CT-ERROR TO PY-MATCH-STATUS-KBN
                 ADD 1 TO WS-ERROR-CNT
                 PERFORM 3800-REWRITE-PAYMENT
              ELSE
                 IF PY-DUE-YM = ZERO
                    DISPLAY '請求年月不正 入金番号='
                            PY-PAY-ID
                    MOVE CT-ERROR TO PY-MATCH-STATUS-KBN
                    ADD 1 TO WS-ERROR-CNT
                    PERFORM 3800-REWRITE-PAYMENT
                 ELSE
                    IF PY-PAY-AMT <= ZERO
                       DISPLAY '入金金額不正 入金番号='
                               PY-PAY-ID
                       MOVE CT-ERROR TO PY-MATCH-STATUS-KBN
                       ADD 1 TO WS-ERROR-CNT
                       PERFORM 3800-REWRITE-PAYMENT
                    END-IF
                 END-IF
              END-IF
           END-IF
           .

       3200-MATCH-ONE-PAYMENT SECTION.
           IF PY-MATCH-STATUS-KBN = CT-ERROR
              EXIT SECTION
           END-IF

           MOVE 'N' TO WS-CLAIM-FOUND-SW
           MOVE 'N' TO WS-EOF-LPCLMF
           MOVE WS-LOW-CLAIM-ID TO CL-CLAIM-ID

           START LPCLMF KEY IS >= CL-CLAIM-ID
              INVALID KEY
                 MOVE 'Y' TO WS-EOF-LPCLMF
              NOT INVALID KEY
                 CONTINUE
           END-START

           IF WS-LPCLMF-ST NOT = '00' AND WS-LPCLMF-ST NOT = '23'
              MOVE WS-LPCLMF-ST TO WS-MSG-ST
              DISPLAY 'LPCLMF START 失敗 ST=' WS-MSG-ST
              MOVE 'Y' TO WS-HARD-ERROR-SW
              MOVE 12 TO RETURN-CODE
           END-IF

           PERFORM UNTIL LPCLMF-END OR CLAIM-FOUND OR HARD-ERROR
              READ LPCLMF NEXT RECORD
                 AT END
                    MOVE 'Y' TO WS-EOF-LPCLMF
                 NOT AT END
                    PERFORM 3300-CHECK-CLAIM
              END-READ
           END-PERFORM

           IF NOT HARD-ERROR
              IF CLAIM-FOUND
                 PERFORM 3500-DECIDE-MATCH
              ELSE
                 MOVE CT-NOCLAIM TO PY-MATCH-STATUS-KBN
                 ADD 1 TO WS-NOCLAIM-CNT
                 PERFORM 3800-REWRITE-PAYMENT
              END-IF
           END-IF
           .

       3300-CHECK-CLAIM SECTION.
           IF CL-POL-NO = PY-POL-NO
              IF CL-DUE-YM = PY-DUE-YM
                 MOVE 'Y' TO WS-CLAIM-FOUND-SW
              END-IF
           END-IF
           .

       3500-DECIDE-MATCH SECTION.
           PERFORM 3600-FIND-PRM
           MOVE CL-BILL-AMT TO WS-BILL-AMT
           MOVE PY-PAY-AMT TO WS-PAY-AMT
           COMPUTE WS-DIFF-AMT = WS-PAY-AMT - WS-BILL-AMT
           COMPUTE WS-NEW-RECEIPT-AMT =
                   CL-RECEIPT-AMT + PY-PAY-AMT

           IF PRM-FOUND
              IF WS-S-CALC-STATUS-KBN NOT = CT-CALC-OK
                 MOVE CT-HOLD TO PY-MATCH-STATUS-KBN
                 MOVE CT-CLAIM-HOLD TO CL-CLAIM-STATUS-KBN
                 ADD 1 TO WS-HOLD-CNT
                 PERFORM 3700-REWRITE-CLAIM
                 PERFORM 3800-REWRITE-PAYMENT
                 EXIT SECTION
              END-IF
           ELSE
              MOVE CT-HOLD TO PY-MATCH-STATUS-KBN
              MOVE CT-CLAIM-HOLD TO CL-CLAIM-STATUS-KBN
              ADD 1 TO WS-HOLD-CNT
              DISPLAY '保険料明細なし 入金番号='
                      PY-PAY-ID
              PERFORM 3700-REWRITE-CLAIM
              PERFORM 3800-REWRITE-PAYMENT
              EXIT SECTION
           END-IF

           MOVE WS-NEW-RECEIPT-AMT TO CL-RECEIPT-AMT

           IF WS-DIFF-AMT = ZERO
              MOVE CT-NORMAL TO PY-MATCH-STATUS-KBN
              MOVE CT-CLAIM-PAID TO CL-CLAIM-STATUS-KBN
              ADD 1 TO WS-CLAIM-MATCH-CNT
           ELSE
              IF WS-DIFF-AMT > ZERO
                 MOVE CT-OVER TO PY-MATCH-STATUS-KBN
                 MOVE CT-CLAIM-PAID TO CL-CLAIM-STATUS-KBN
                 ADD 1 TO WS-OVER-CNT
              ELSE
                 MOVE CT-SHORT TO PY-MATCH-STATUS-KBN
                 MOVE CT-CLAIM-PART TO CL-CLAIM-STATUS-KBN
                 ADD 1 TO WS-SHORT-CNT
              END-IF
           END-IF

           PERFORM 3700-REWRITE-CLAIM
           PERFORM 3800-REWRITE-PAYMENT
           .

       3600-FIND-PRM SECTION.
           MOVE 'N' TO WS-PRM-FOUND-SW
           MOVE SPACES TO WS-S-PRM-ID
           MOVE SPACES TO WS-S-CALC-STATUS-KBN
           MOVE 1 TO WS-PRM-IDX

           PERFORM UNTIL WS-PRM-IDX > WS-PRM-CNT OR PRM-FOUND
              IF WS-T-POL-NO(WS-PRM-IDX) = PY-POL-NO
                 MOVE 'Y' TO WS-PRM-FOUND-SW
                 MOVE WS-T-PRM-ID(WS-PRM-IDX) TO WS-S-PRM-ID
                 MOVE WS-T-CALC-STATUS-KBN(WS-PRM-IDX)
                      TO WS-S-CALC-STATUS-KBN
              ELSE
                 ADD 1 TO WS-PRM-IDX
              END-IF
           END-PERFORM
           .

       3700-REWRITE-CLAIM SECTION.
           REWRITE LPCLMF-REC
           IF WS-LPCLMF-ST NOT = '00'
              MOVE WS-LPCLMF-ST TO WS-MSG-ST
              DISPLAY 'LPCLMF 更新失敗 ST=' WS-MSG-ST
              MOVE 'Y' TO WS-HARD-ERROR-SW
              MOVE 12 TO RETURN-CODE
           END-IF
           .

       3800-REWRITE-PAYMENT SECTION.
           REWRITE LPPAYF-REC
           IF WS-LPPAYF-ST NOT = '00'
              MOVE WS-LPPAYF-ST TO WS-MSG-ST
              DISPLAY 'LPPAYF 更新失敗 ST=' WS-MSG-ST
              MOVE 'Y' TO WS-HARD-ERROR-SW
              MOVE 12 TO RETURN-CODE
           END-IF
           .

       9000-FINALIZE SECTION.
           IF WS-LPCLMF-ST NOT = SPACES
              CLOSE LPCLMF
              IF WS-LPCLMF-ST NOT = '00'
                 MOVE WS-LPCLMF-ST TO WS-MSG-ST
                 DISPLAY 'LPCLMF クローズ失敗 ST=' WS-MSG-ST
                 MOVE 'Y' TO WS-HARD-ERROR-SW
                 MOVE 8 TO RETURN-CODE
              END-IF
           END-IF

           IF WS-LPPAYF-ST NOT = SPACES
              CLOSE LPPAYF
              IF WS-LPPAYF-ST NOT = '00'
                 MOVE WS-LPPAYF-ST TO WS-MSG-ST
                 DISPLAY 'LPPAYF クローズ失敗 ST=' WS-MSG-ST
                 MOVE 'Y' TO WS-HARD-ERROR-SW
                 MOVE 8 TO RETURN-CODE
              END-IF
           END-IF

           IF WS-LFPRMF-ST NOT = SPACES
              CLOSE LFPRMF
              IF WS-LFPRMF-ST NOT = '00'
                 MOVE WS-LFPRMF-ST TO WS-MSG-ST
                 DISPLAY 'LFPRMF クローズ失敗 ST=' WS-MSG-ST
                 MOVE 'Y' TO WS-HARD-ERROR-SW
                 MOVE 8 TO RETURN-CODE
              END-IF
           END-IF

           DISPLAY 'LP240B 入金読込件数=' WS-PAY-READ-CNT
           DISPLAY 'LP240B 完全一致件数=' WS-CLAIM-MATCH-CNT
           DISPLAY 'LP240B 過入金件数=' WS-OVER-CNT
           DISPLAY 'LP240B 不足件数=' WS-SHORT-CNT
           DISPLAY 'LP240B 対象請求なし件数=' WS-NOCLAIM-CNT
           DISPLAY 'LP240B 保留消込件数=' WS-HOLD-CNT
           DISPLAY 'LP240B エラー件数=' WS-ERROR-CNT
           DISPLAY 'LP240B 保険料明細読込件数=' WS-PRM-LOAD-CNT

           IF HARD-ERROR
              IF RETURN-CODE = ZERO
                 MOVE 8 TO RETURN-CODE
              END-IF
           ELSE
              MOVE 0 TO RETURN-CODE
           END-IF
           .
