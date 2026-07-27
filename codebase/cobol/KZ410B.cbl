       IDENTIFICATION DIVISION.
       PROGRAM-ID. KZ410B.
      ******************************************************************
      * 変更履歴
      * 版数  年月日    担当    概要
      * 01.00 20240401  KZ開発  初版作成
      * 01.01 20240515  KZ保守  入出力状態確認を追加
      * 01.02 20240620  KZ保守  日数計算検証を強化
      ******************************************************************
      * 利息計算バッチ
      * 日次平均残高と年利率から利息を算出し、利息明細を作成する。
      * 開始時点仕様として、基準日は名目月末日、日数は30/360で扱う。
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZDBALF
               ASSIGN TO "KZDBALF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-KZDBALF.
           SELECT KZCYRF
               ASSIGN TO "KZCYRF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-KZCYRF.
           SELECT KZINTF
               ASSIGN TO "KZINTF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-KZINTF.

       DATA DIVISION.
       FILE SECTION.
       FD  KZDBALF.
           COPY KZDBALFC.
       FD  KZCYRF.
           COPY KZCYRFC.
       FD  KZINTF.
           COPY KZINTFC.

       WORKING-STORAGE SECTION.
       01  FS-AREA.
           05 FS-KZDBALF              PIC XX VALUE SPACES.
           05 FS-KZCYRF              PIC XX VALUE SPACES.
           05 FS-KZINTF              PIC XX VALUE SPACES.

       01  SW-AREA.
           05 SW-DBAL-EOF            PIC X VALUE "N".
              88 DBAL-EOF                 VALUE "Y".
           05 SW-CYRF-EOF            PIC X VALUE "N".
              88 CYRF-EOF                 VALUE "Y".
           05 SW-HARD-ERROR          PIC X VALUE "N".
              88 HARD-ERROR               VALUE "Y".

       01  CNT-AREA.
           05 CNT-DBAL-READ          PIC 9(9) VALUE ZERO.
           05 CNT-CYRF-READ          PIC 9(9) VALUE ZERO.
           05 CNT-INT-WRITE          PIC 9(9) VALUE ZERO.
           05 CNT-CYCLE              PIC 9(3) VALUE ZERO.

       01  CYCLE-TABLE.
           05 CYCLE-ENTRY OCCURS 20 TIMES.
              10 TB-CYCLE-ID         PIC X(10) VALUE SPACES.
              10 TB-NOMINAL-DT       PIC 9(8)  VALUE ZERO.
              10 TB-RESOLVED-DT      PIC 9(8)  VALUE ZERO.

       01  WORK-AREA.
           05 WK-IDX                 PIC 9(3) COMP VALUE ZERO.
           05 WK-FOUND-IDX           PIC 9(3) COMP VALUE ZERO.
           05 WK-FOUND-SW            PIC X VALUE "N".
              88 CYCLE-FOUND              VALUE "Y".
           05 WK-END-DT              PIC 9(8) VALUE ZERO.
           05 WK-START-DT            PIC 9(8) VALUE ZERO.
           05 WK-Y1                  PIC 9(4) VALUE ZERO.
           05 WK-M1                  PIC 9(2) VALUE ZERO.
           05 WK-D1                  PIC 9(2) VALUE ZERO.
           05 WK-Y2                  PIC 9(4) VALUE ZERO.
           05 WK-M2                  PIC 9(2) VALUE ZERO.
           05 WK-D2                  PIC 9(2) VALUE ZERO.
           05 WK-REM1                PIC 9(4) VALUE ZERO.
           05 WK-REM2                PIC 9(4) VALUE ZERO.
           05 WK-ACCR-DAYS           PIC S9(5) COMP-3 VALUE ZERO.
           05 WK-INT-WORK            PIC S9(15)V9(6) COMP-3
                                      VALUE ZERO.

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE 0 TO RETURN-CODE
           PERFORM 1000-OPEN-FILES
           IF NOT HARD-ERROR
              PERFORM 2000-LOAD-CYCLES
           END-IF
           IF NOT HARD-ERROR
              PERFORM 3000-PROCESS-DBAL
           END-IF
           PERFORM 8000-CLOSE-FILES
           IF HARD-ERROR
              MOVE 12 TO RETURN-CODE
           ELSE
              MOVE 0 TO RETURN-CODE
              DISPLAY "KZ410B 正常終了"
              DISPLAY "KZCYRF 読込件数=" CNT-CYRF-READ
              DISPLAY "KZDBALF 読込件数=" CNT-DBAL-READ
              DISPLAY "KZINTF 書込件数=" CNT-INT-WRITE
           END-IF
           GOBACK.

       1000-OPEN-FILES.
           OPEN INPUT KZCYRF
           IF FS-KZCYRF NOT = "00"
              DISPLAY "KZCYRF オープン失敗 ST=" FS-KZCYRF
              SET HARD-ERROR TO TRUE
           END-IF
           IF NOT HARD-ERROR
              OPEN INPUT KZDBALF
              IF FS-KZDBALF NOT = "00"
                 DISPLAY "KZDBALF オープン失敗 ST=" FS-KZDBALF
                 SET HARD-ERROR TO TRUE
              END-IF
           END-IF
           IF NOT HARD-ERROR
              OPEN OUTPUT KZINTF
              IF FS-KZINTF NOT = "00"
                 DISPLAY "KZINTF オープン失敗 ST=" FS-KZINTF
                 SET HARD-ERROR TO TRUE
              END-IF
           END-IF.

       2000-LOAD-CYCLES.
           PERFORM UNTIL CYRF-EOF OR HARD-ERROR
              READ KZCYRF
                 AT END
                    SET CYRF-EOF TO TRUE
                 NOT AT END
                    IF FS-KZCYRF NOT = "00"
                       DISPLAY "KZCYRF 読込失敗 ST=" FS-KZCYRF
                       SET HARD-ERROR TO TRUE
                    ELSE
                       ADD 1 TO CNT-CYRF-READ
                       PERFORM 2100-STORE-CYCLE
                    END-IF
              END-READ
           END-PERFORM
           IF NOT HARD-ERROR AND CNT-CYCLE = ZERO
              DISPLAY "KZCYRF 基準日未登録"
              SET HARD-ERROR TO TRUE
           END-IF.

       2100-STORE-CYCLE.
           IF CR-CYCLE-ID = SPACES
              DISPLAY "KZCYRF サイクルID未設定"
              SET HARD-ERROR TO TRUE
           ELSE
              IF CR-NOMINAL-DT NOT NUMERIC
                 DISPLAY "KZCYRF 名目日不正 ID=" CR-CYCLE-ID
                 SET HARD-ERROR TO TRUE
              ELSE
                 IF CR-RESOLVED-DT NOT NUMERIC
                    DISPLAY "KZCYRF 解決日不正 ID=" CR-CYCLE-ID
                    SET HARD-ERROR TO TRUE
                 ELSE
                    IF CNT-CYCLE >= 20
                       DISPLAY "KZCYRF サイクル表超過"
                       SET HARD-ERROR TO TRUE
                    ELSE
                       ADD 1 TO CNT-CYCLE
                       MOVE CR-CYCLE-ID    TO TB-CYCLE-ID(CNT-CYCLE)
                       MOVE CR-NOMINAL-DT  TO TB-NOMINAL-DT(CNT-CYCLE)
                       MOVE CR-RESOLVED-DT TO TB-RESOLVED-DT(CNT-CYCLE)
                    END-IF
                 END-IF
              END-IF
           END-IF.

       3000-PROCESS-DBAL.
           PERFORM UNTIL DBAL-EOF OR HARD-ERROR
              READ KZDBALF
                 AT END
                    SET DBAL-EOF TO TRUE
                 NOT AT END
                    IF FS-KZDBALF NOT = "00"
                       DISPLAY "KZDBALF 読込失敗 ST=" FS-KZDBALF
                       SET HARD-ERROR TO TRUE
                    ELSE
                       ADD 1 TO CNT-DBAL-READ
                       PERFORM 3100-PROCESS-ACCOUNT
                    END-IF
              END-READ
           END-PERFORM.

       3100-PROCESS-ACCOUNT.
           IF DB-ACCT-NO = SPACES
              DISPLAY "KZDBALF 口座番号未設定"
              SET HARD-ERROR TO TRUE
           ELSE
              IF DB-CYCLE-ID = SPACES
                 DISPLAY "KZDBALF サイクルID未設定"
                 DISPLAY "口座=" DB-ACCT-NO
                 SET HARD-ERROR TO TRUE
              ELSE
                 IF DB-PERIOD-START-DT NOT NUMERIC
                    DISPLAY "KZDBALF 開始日不正"
                    DISPLAY "口座=" DB-ACCT-NO
                    SET HARD-ERROR TO TRUE
                 ELSE
                    PERFORM 3200-FIND-CYCLE
                    IF CYCLE-FOUND
                       PERFORM 3300-CALC-INTEREST
                       IF NOT HARD-ERROR
                          PERFORM 3400-WRITE-INT
                       END-IF
                    ELSE
                       DISPLAY "KZDBALF サイクル未登録"
                       DISPLAY "口座=" DB-ACCT-NO
                       SET HARD-ERROR TO TRUE
                    END-IF
                 END-IF
              END-IF
           END-IF.

       3200-FIND-CYCLE.
           MOVE "N" TO WK-FOUND-SW
           MOVE ZERO TO WK-FOUND-IDX
           PERFORM VARYING WK-IDX FROM 1 BY 1
              UNTIL WK-IDX > CNT-CYCLE OR CYCLE-FOUND
              IF DB-CYCLE-ID = TB-CYCLE-ID(WK-IDX)
                 MOVE WK-IDX TO WK-FOUND-IDX
                 SET CYCLE-FOUND TO TRUE
              END-IF
           END-PERFORM.

       3300-CALC-INTEREST.
           MOVE DB-PERIOD-START-DT TO WK-START-DT
           MOVE TB-NOMINAL-DT(WK-FOUND-IDX) TO WK-END-DT
           PERFORM 3310-SPLIT-DATES
           IF NOT HARD-ERROR
              COMPUTE WK-ACCR-DAYS =
                 ((WK-Y2 - WK-Y1) * 360)
               + ((WK-M2 - WK-M1) * 30)
               +  (WK-D2 - WK-D1)
              IF WK-ACCR-DAYS < ZERO
                 DISPLAY "KZDBALF 日数逆転"
                 DISPLAY "口座=" DB-ACCT-NO
                 SET HARD-ERROR TO TRUE
              ELSE
                 COMPUTE WK-INT-WORK =
                    DB-AVG-DAILY-BAL * DB-INT-RATE
                  * WK-ACCR-DAYS / 360
              END-IF
           END-IF.

       3310-SPLIT-DATES.
           DIVIDE WK-START-DT BY 10000
              GIVING WK-Y1 REMAINDER WK-REM1
           DIVIDE WK-REM1 BY 100
              GIVING WK-M1 REMAINDER WK-D1
           DIVIDE WK-END-DT BY 10000
              GIVING WK-Y2 REMAINDER WK-REM2
           DIVIDE WK-REM2 BY 100
              GIVING WK-M2 REMAINDER WK-D2
           IF WK-M1 < 1 OR WK-M1 > 12
              DISPLAY "KZDBALF 開始月不正"
              DISPLAY "口座=" DB-ACCT-NO
              SET HARD-ERROR TO TRUE
           END-IF
           IF WK-D1 < 1 OR WK-D1 > 31
              DISPLAY "KZDBALF 開始日不正"
              DISPLAY "口座=" DB-ACCT-NO
              SET HARD-ERROR TO TRUE
           END-IF
           IF WK-M2 < 1 OR WK-M2 > 12
              DISPLAY "KZCYRF 名目月不正"
              DISPLAY "ID=" DB-CYCLE-ID
              SET HARD-ERROR TO TRUE
           END-IF
           IF WK-D2 < 1 OR WK-D2 > 31
              DISPLAY "KZCYRF 名目日不正"
              DISPLAY "ID=" DB-CYCLE-ID
              SET HARD-ERROR TO TRUE
           END-IF.

       3400-WRITE-INT.
           MOVE SPACES TO KZINTF-REC
           MOVE DB-ACCT-NO     TO IN-ACCT-NO
           MOVE DB-CYCLE-ID    TO IN-CYCLE-ID
           MOVE WK-END-DT      TO IN-ACCRUAL-DT
           MOVE WK-ACCR-DAYS   TO IN-ACCR-DAYS
           MOVE WK-INT-WORK    TO IN-INT-AMT
           WRITE KZINTF-REC
           IF FS-KZINTF NOT = "00"
              DISPLAY "KZINTF 書込失敗 ST=" FS-KZINTF
              DISPLAY "KZINTF 書込口座=" DB-ACCT-NO
              SET HARD-ERROR TO TRUE
           ELSE
              ADD 1 TO CNT-INT-WRITE
           END-IF.

       8000-CLOSE-FILES.
           IF FS-KZCYRF = "00" OR FS-KZCYRF = "10"
              CLOSE KZCYRF
              IF FS-KZCYRF NOT = "00"
                 DISPLAY "KZCYRF クローズ失敗 ST=" FS-KZCYRF
                 SET HARD-ERROR TO TRUE
              END-IF
           END-IF
           IF FS-KZDBALF = "00" OR FS-KZDBALF = "10"
              CLOSE KZDBALF
              IF FS-KZDBALF NOT = "00"
                 DISPLAY "KZDBALF クローズ失敗 ST=" FS-KZDBALF
                 SET HARD-ERROR TO TRUE
              END-IF
           END-IF
           IF FS-KZINTF = "00"
              CLOSE KZINTF
              IF FS-KZINTF NOT = "00"
                 DISPLAY "KZINTF クローズ失敗 ST=" FS-KZINTF
                 SET HARD-ERROR TO TRUE
              END-IF
           END-IF.
