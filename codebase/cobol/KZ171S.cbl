       IDENTIFICATION DIVISION.
       PROGRAM-ID. KZ171S.
      * 変更履歴
      * 版数  年月日        担当                    概要
      * 1.00  平成28年04月  システム部 勘定系チーム 初版作成
      * 1.01  令和02年10月  システム部 勘定系チーム 判定条件整理
      * 1.02  令和05年06月  システム部 勘定系チーム 警告返却見直し
       AUTHOR. BATCH-SYSTEM.
      * 明細書取引分類サブ
      * 取引コードと履歴種別から明細分類を決定する。
      * 対象外、マスタ不整合、同一日重複候補は警告で返す。
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZTRANF
               ASSIGN TO "KZTRANF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-TR-STAT.
           SELECT KZFEEHF
               ASSIGN TO "KZFEEHF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FE-STAT.
           SELECT KZINTAF
               ASSIGN TO "KZINTAF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-IA-STAT.

       DATA DIVISION.
       FILE SECTION.
       FD  KZTRANF.
           COPY KZTRANC.
       FD  KZFEEHF.
           COPY KZFEEHC.
       FD  KZINTAF.
           COPY KZINTAFC.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05 WS-TR-STAT              PIC XX VALUE SPACES.
           05 WS-FE-STAT              PIC XX VALUE SPACES.
           05 WS-IA-STAT              PIC XX VALUE SPACES.

       01  WS-SWITCHES.
           05 WS-TR-EOF               PIC X VALUE "N".
              88 TR-EOF                     VALUE "Y".
           05 WS-FE-EOF               PIC X VALUE "N".
              88 FE-EOF                     VALUE "Y".
           05 WS-IA-EOF               PIC X VALUE "N".
              88 IA-EOF                     VALUE "Y".
           05 WS-TR-FOUND             PIC X VALUE "N".
              88 TR-FOUND                   VALUE "Y".
           05 WS-FE-HIT               PIC X VALUE "N".
              88 FE-HIT                     VALUE "Y".
           05 WS-IA-HIT               PIC X VALUE "N".
              88 IA-HIT                     VALUE "Y".
           05 WS-FILE-ERROR           PIC X VALUE "N".
              88 FILE-ERROR                 VALUE "Y".

       01  WS-WORK.
           05 WS-FE-DUP-CNT           PIC 9(03) VALUE ZERO.
           05 WS-IA-DUP-CNT           PIC 9(03) VALUE ZERO.
           05 WS-ABS-AMT              PIC S9(13)V99 COMP-3 VALUE ZERO.
           05 WS-EXPECTED-SIGN        PIC X VALUE SPACE.
           05 WS-OPENED               PIC X VALUE "N".
              88 FILES-OPENED               VALUE "Y".

       LINKAGE SECTION.
       01  LK-KZ171S-PARM.
           05 LK-ACCT-ID              PIC X(12).
           05 LK-TRAN-CD              PIC X(04).
           05 LK-HIST-TYPE            PIC X(02).
           05 LK-TRAN-DT              PIC 9(08).
           05 LK-TRAN-AMT             PIC S9(13)V99 COMP-3.
           05 LK-CLASS-CD             PIC X(02).
           05 LK-WARN-KBN             PIC X(01).
           05 LK-RETURN-CD            PIC X(02).

       PROCEDURE DIVISION USING LK-KZ171S-PARM.
       MAIN-SECTION.
           PERFORM 1000-INIT
           IF LK-RETURN-CD = "00"
               PERFORM 2000-VALIDATE
           END-IF
           IF LK-RETURN-CD = "00"
               PERFORM 3000-DECIDE-CLASS
           END-IF
           IF LK-RETURN-CD = "00"
               PERFORM 4000-CHECK-DUPLICATE
           END-IF
           PERFORM 9000-FINAL
           GOBACK.

       1000-INIT.
           MOVE SPACES TO LK-CLASS-CD
           MOVE "0"    TO LK-WARN-KBN
           MOVE "00"   TO LK-RETURN-CD
           MOVE "N"    TO WS-FILE-ERROR
           MOVE "N"    TO WS-TR-EOF
           MOVE "N"    TO WS-FE-EOF
           MOVE "N"    TO WS-IA-EOF
           MOVE "N"    TO WS-TR-FOUND
           MOVE "N"    TO WS-FE-HIT
           MOVE "N"    TO WS-IA-HIT
           MOVE ZERO   TO WS-FE-DUP-CNT
           MOVE ZERO   TO WS-IA-DUP-CNT
           MOVE SPACE  TO WS-EXPECTED-SIGN
           OPEN INPUT KZTRANF KZFEEHF KZINTAF
           MOVE "Y" TO WS-OPENED
           IF WS-TR-STAT NOT = "00"
               MOVE "91" TO LK-RETURN-CD
               MOVE "9"  TO LK-WARN-KBN
               SET FILE-ERROR TO TRUE
           END-IF
           IF WS-FE-STAT NOT = "00"
               MOVE "91" TO LK-RETURN-CD
               MOVE "9"  TO LK-WARN-KBN
               SET FILE-ERROR TO TRUE
           END-IF
           IF WS-IA-STAT NOT = "00"
               MOVE "91" TO LK-RETURN-CD
               MOVE "9"  TO LK-WARN-KBN
               SET FILE-ERROR TO TRUE
           END-IF.

       2000-VALIDATE.
      * 入力の形だけを先に確認し、分類判断の誤読を防ぐ。
           IF LK-ACCT-ID = SPACES
               MOVE "12" TO LK-RETURN-CD
               MOVE "2"  TO LK-WARN-KBN
           END-IF
           IF LK-RETURN-CD = "00"
              AND LK-TRAN-CD = SPACES
               MOVE "12" TO LK-RETURN-CD
               MOVE "2"  TO LK-WARN-KBN
           END-IF
           IF LK-RETURN-CD = "00"
              AND LK-HIST-TYPE NOT = "01"
              AND LK-HIST-TYPE NOT = "02"
              AND LK-HIST-TYPE NOT = "03"
              AND LK-HIST-TYPE NOT = "04"
               MOVE "04" TO LK-RETURN-CD
               MOVE "1"  TO LK-WARN-KBN
           END-IF
           IF LK-RETURN-CD = "00"
              AND LK-TRAN-DT < 19000101
               MOVE "12" TO LK-RETURN-CD
               MOVE "2"  TO LK-WARN-KBN
           END-IF
           IF LK-RETURN-CD = "00"
              AND LK-TRAN-AMT = ZERO
               MOVE "04" TO LK-RETURN-CD
               MOVE "1"  TO LK-WARN-KBN
           END-IF.

       3000-DECIDE-CLASS.
      * 旧勘定系の履歴種別を優先し、コード表で例外を補正する。
           EVALUATE LK-HIST-TYPE
               WHEN "01"
                   MOVE "PU" TO LK-CLASS-CD
                   MOVE "-"  TO WS-EXPECTED-SIGN
               WHEN "02"
                   MOVE "RP" TO LK-CLASS-CD
                   MOVE "+"  TO WS-EXPECTED-SIGN
               WHEN "03"
                   MOVE "FE" TO LK-CLASS-CD
                   MOVE "-"  TO WS-EXPECTED-SIGN
               WHEN "04"
                   MOVE "IN" TO LK-CLASS-CD
                   MOVE "-"  TO WS-EXPECTED-SIGN
           END-EVALUATE
           PERFORM 3100-SCAN-TRAN-MASTER
           IF NOT TR-FOUND
               MOVE "04" TO LK-RETURN-CD
               MOVE "1"  TO LK-WARN-KBN
           ELSE
               PERFORM 3200-CHECK-AMOUNT-SIGN
           END-IF.

       3100-SCAN-TRAN-MASTER.
           PERFORM UNTIL TR-EOF OR TR-FOUND OR FILE-ERROR
               READ KZTRANF
                   AT END
                       SET TR-EOF TO TRUE
                   NOT AT END
                       IF WS-TR-STAT = "00"
                           IF TR-TRAN-CD = LK-TRAN-CD
                               SET TR-FOUND TO TRUE
                               IF TR-TRAN-AMT NOT = ZERO
                                   MOVE TR-TRAN-AMT TO WS-ABS-AMT
                               ELSE
                                   MOVE LK-TRAN-AMT TO WS-ABS-AMT
                               END-IF
                           END-IF
                       ELSE
                           MOVE "92" TO LK-RETURN-CD
                           MOVE "9"  TO LK-WARN-KBN
                           SET FILE-ERROR TO TRUE
                       END-IF
               END-READ
           END-PERFORM.

       3200-CHECK-AMOUNT-SIGN.
           IF WS-EXPECTED-SIGN = "-"
              AND LK-TRAN-AMT > ZERO
               MOVE "1" TO LK-WARN-KBN
           END-IF
           IF WS-EXPECTED-SIGN = "+"
              AND LK-TRAN-AMT < ZERO
               MOVE "1" TO LK-WARN-KBN
           END-IF
           IF WS-ABS-AMT < ZERO
               COMPUTE WS-ABS-AMT = WS-ABS-AMT * -1
           END-IF
           IF WS-ABS-AMT > 99999999999.99
               MOVE "2" TO LK-WARN-KBN
           END-IF
           EVALUATE LK-TRAN-CD
               WHEN "1001"
               WHEN "1002"
               WHEN "1010"
                   IF LK-HIST-TYPE NOT = "01"
                       MOVE "1" TO LK-WARN-KBN
                   END-IF
               WHEN "2001"
               WHEN "2010"
                   IF LK-HIST-TYPE NOT = "02"
                       MOVE "1" TO LK-WARN-KBN
                   END-IF
               WHEN "3001"
               WHEN "3010"
                   IF LK-HIST-TYPE NOT = "03"
                       MOVE "1" TO LK-WARN-KBN
                   END-IF
               WHEN "4001"
               WHEN "4010"
                   IF LK-HIST-TYPE NOT = "04"
                       MOVE "1" TO LK-WARN-KBN
                   END-IF
               WHEN OTHER
                   MOVE "04" TO LK-RETURN-CD
                   MOVE "1"  TO LK-WARN-KBN
           END-EVALUATE.

       4000-CHECK-DUPLICATE.
      * 手数料・利息は同一日複数計上を呼出元で保留可能にする。
           IF LK-CLASS-CD = "FE"
               PERFORM 4100-SCAN-FEE-HISTORY
               IF WS-FE-DUP-CNT > 1
                   MOVE "3" TO LK-WARN-KBN
               END-IF
               IF NOT FE-HIT
                   MOVE "1" TO LK-WARN-KBN
               END-IF
           END-IF
           IF LK-CLASS-CD = "IN"
               PERFORM 4200-SCAN-INT-ACCRUAL
               IF WS-IA-DUP-CNT > 1
                   MOVE "3" TO LK-WARN-KBN
               END-IF
               IF NOT IA-HIT
                   MOVE "1" TO LK-WARN-KBN
               END-IF
           END-IF.

       4100-SCAN-FEE-HISTORY.
           PERFORM UNTIL FE-EOF OR FILE-ERROR
               READ KZFEEHF
                   AT END
                       SET FE-EOF TO TRUE
                   NOT AT END
                       IF WS-FE-STAT = "00"
                           IF FE-ACCT-ID = LK-ACCT-ID
                              AND FE-CYCLE-DT = LK-TRAN-DT
                               SET FE-HIT TO TRUE
                               ADD 1 TO WS-FE-DUP-CNT
                               IF FE-EXEMPT-FLAG = "Y"
                                   MOVE "1" TO LK-WARN-KBN
                               END-IF
                               IF FE-CAP-FLAG = "Y"
                                  AND FE-FEE-YTD > 500000
                                   MOVE "2" TO LK-WARN-KBN
                               END-IF
                           END-IF
                       ELSE
                           MOVE "92" TO LK-RETURN-CD
                           MOVE "9"  TO LK-WARN-KBN
                           SET FILE-ERROR TO TRUE
                       END-IF
               END-READ
           END-PERFORM.

       4200-SCAN-INT-ACCRUAL.
           PERFORM UNTIL IA-EOF OR FILE-ERROR
               READ KZINTAF
                   AT END
                       SET IA-EOF TO TRUE
                   NOT AT END
                       IF WS-IA-STAT = "00"
                           IF IA-ACCT-ID = LK-ACCT-ID
                              AND IA-ACCRUAL-DT = LK-TRAN-DT
                               SET IA-HIT TO TRUE
                               ADD 1 TO WS-IA-DUP-CNT
                               IF IA-RATE-CD = SPACES
                                   MOVE "2" TO LK-WARN-KBN
                               END-IF
                               IF IA-CYCLE-BAL <= ZERO
                                  AND IA-INT-AMT > ZERO
                                   MOVE "1" TO LK-WARN-KBN
                               END-IF
                           END-IF
                       ELSE
                           MOVE "92" TO LK-RETURN-CD
                           MOVE "9"  TO LK-WARN-KBN
                           SET FILE-ERROR TO TRUE
                       END-IF
               END-READ
           END-PERFORM.

       9000-FINAL.
           IF FILES-OPENED
               CLOSE KZTRANF KZFEEHF KZINTAF
           END-IF
           IF LK-RETURN-CD = "00"
              AND LK-WARN-KBN = "0"
              AND LK-CLASS-CD = SPACES
               MOVE "04" TO LK-RETURN-CD
               MOVE "1"  TO LK-WARN-KBN
           END-IF.
