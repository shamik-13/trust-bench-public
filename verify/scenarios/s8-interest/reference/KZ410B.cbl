       IDENTIFICATION DIVISION.
       PROGRAM-ID. KZ410B.
      ******************************************************************
      * 変更履歴
      * 版数  年月日    担当      概要
      * 1.00  20260401  開発担当  新規作成
      * 1.01  20260515  保守担当  実日数３６５日計算を明確化
      * 1.02  20260619  保守担当  基準日に解決済日を使用
      ******************************************************************
      * 利息計算バッチ
      * 日次平均残高と年利率から、周期基準日までの利息を算出する。
      * 基準日は名目日ではなく、休日解決済日を使用する。
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
       01  FS-KZDBALF                  PIC X(02) VALUE SPACE.
       01  FS-KZCYRF                   PIC X(02) VALUE SPACE.
       01  FS-KZINTF                   PIC X(02) VALUE SPACE.

       01  SW-END.
           05  KZDBALF-END-SW          PIC X(01) VALUE "N".
               88  KZDBALF-END                   VALUE "Y".
           05  KZCYRF-END-SW           PIC X(01) VALUE "N".
               88  KZCYRF-END                    VALUE "Y".

       01  CT-AREA.
           05  CT-KZDBALF-READ         PIC 9(09) VALUE ZERO.
           05  CT-KZCYRF-READ          PIC 9(09) VALUE ZERO.
           05  CT-KZINTF-WRITE         PIC 9(09) VALUE ZERO.

       01  CR-TABLE-AREA.
           05  CR-TABLE-MAX            PIC 9(03) VALUE 050.
           05  CR-TABLE-CNT            PIC 9(03) VALUE ZERO.
           05  CR-TABLE                OCCURS 50 TIMES
                                       INDEXED BY CR-IDX.
               10  TB-CYCLE-ID         PIC X(10).
               10  TB-NOMINAL-DT       PIC 9(08).
               10  TB-RESOLVED-DT      PIC 9(08).

       01  WK-AREA.
           05  WK-CYCLE-FOUND-SW       PIC X(01) VALUE "N".
               88  WK-CYCLE-FOUND                VALUE "Y".
           05  WK-PERIOD-START-DT      PIC 9(08) VALUE ZERO.
           05  WK-ACCRUAL-DT           PIC 9(08) VALUE ZERO.
           05  WK-START-DAYS           PIC S9(09) VALUE ZERO.
           05  WK-END-DAYS             PIC S9(09) VALUE ZERO.
           05  WK-ACCR-DAYS            PIC S9(09) VALUE ZERO.
           05  WK-INT-AMT              PIC S9(13) VALUE ZERO.
           05  WK-ERR-TEXT             PIC X(80) VALUE SPACE.

       PROCEDURE DIVISION.
       MAIN-RTN.
           MOVE 0 TO RETURN-CODE
           PERFORM 1000-OPEN-FILES
           PERFORM 2000-LOAD-CYCLE
           PERFORM 3000-PROCESS-BALANCE
           PERFORM 9000-CLOSE-FILES
           DISPLAY "KZ410B 正常終了"
           DISPLAY "KZCYRF 読込件数=" CT-KZCYRF-READ
           DISPLAY "KZDBALF 読込件数=" CT-KZDBALF-READ
           DISPLAY "KZINTF 書込件数=" CT-KZINTF-WRITE
           GOBACK.

       1000-OPEN-FILES.
           OPEN INPUT KZCYRF
           IF FS-KZCYRF NOT = "00"
              DISPLAY "KZCYRF オープン失敗 ST=" FS-KZCYRF
              PERFORM 9900-ABEND
           END-IF

           OPEN INPUT KZDBALF
           IF FS-KZDBALF NOT = "00"
              DISPLAY "KZDBALF オープン失敗 ST=" FS-KZDBALF
              PERFORM 9900-ABEND
           END-IF

           OPEN OUTPUT KZINTF
           IF FS-KZINTF NOT = "00"
              DISPLAY "KZINTF オープン失敗 ST=" FS-KZINTF
              PERFORM 9900-ABEND
           END-IF.

       2000-LOAD-CYCLE.
           PERFORM UNTIL KZCYRF-END
              READ KZCYRF
                 AT END
                    SET KZCYRF-END TO TRUE
                 NOT AT END
                    IF FS-KZCYRF NOT = "00"
                       DISPLAY "KZCYRF 読込失敗 ST=" FS-KZCYRF
                       PERFORM 9900-ABEND
                    END-IF
                    ADD 1 TO CT-KZCYRF-READ
                    PERFORM 2100-STORE-CYCLE
              END-READ
           END-PERFORM

           IF CR-TABLE-CNT = ZERO
              DISPLAY "KZCYRF 周期基準日なし"
              PERFORM 9900-DATA-ABEND
           END-IF.

       2100-STORE-CYCLE.
           IF CR-CYCLE-ID = SPACE
              DISPLAY "KZCYRF 周期ＩＤ未設定"
              PERFORM 9900-DATA-ABEND
           END-IF

           IF CR-RESOLVED-DT = ZERO
              DISPLAY "KZCYRF 解決済日未設定 CYCLE=" CR-CYCLE-ID
              PERFORM 9900-DATA-ABEND
           END-IF

           IF CR-ROLLED-FLAG NOT = "Y"
              AND CR-ROLLED-FLAG NOT = "N"
              DISPLAY "KZCYRF 繰上フラグ不正 CYCLE=" CR-CYCLE-ID
              DISPLAY "KZCYRF 繰上フラグ=" CR-ROLLED-FLAG
              PERFORM 9900-DATA-ABEND
           END-IF

           IF CR-TABLE-CNT >= CR-TABLE-MAX
              DISPLAY "KZCYRF 周期表件数超過"
              PERFORM 9900-DATA-ABEND
           END-IF

           ADD 1 TO CR-TABLE-CNT
           MOVE CR-CYCLE-ID    TO TB-CYCLE-ID(CR-TABLE-CNT)
           MOVE CR-NOMINAL-DT  TO TB-NOMINAL-DT(CR-TABLE-CNT)
           MOVE CR-RESOLVED-DT TO TB-RESOLVED-DT(CR-TABLE-CNT).

       3000-PROCESS-BALANCE.
           PERFORM UNTIL KZDBALF-END
              READ KZDBALF
                 AT END
                    SET KZDBALF-END TO TRUE
                 NOT AT END
                    IF FS-KZDBALF NOT = "00"
                       DISPLAY "KZDBALF 読込失敗 ST=" FS-KZDBALF
                       PERFORM 9900-ABEND
                    END-IF
                    ADD 1 TO CT-KZDBALF-READ
                    PERFORM 3100-PROCESS-ACCOUNT
              END-READ
           END-PERFORM.

       3100-PROCESS-ACCOUNT.
           PERFORM 3200-VALIDATE-BALANCE
           PERFORM 3300-FIND-CYCLE
           PERFORM 3400-CALC-INTEREST
           PERFORM 3500-WRITE-INTEREST.

       3200-VALIDATE-BALANCE.
           IF DB-ACCT-NO = SPACE
              DISPLAY "KZDBALF 口座番号未設定"
              PERFORM 9900-DATA-ABEND
           END-IF

           IF DB-CYCLE-ID = SPACE
              DISPLAY "KZDBALF 周期ＩＤ未設定 ACCT=" DB-ACCT-NO
              PERFORM 9900-DATA-ABEND
           END-IF

           IF DB-PERIOD-START-DT = ZERO
              DISPLAY "KZDBALF 開始日未設定 ACCT=" DB-ACCT-NO
              PERFORM 9900-DATA-ABEND
           END-IF.

       3300-FIND-CYCLE.
           MOVE "N" TO WK-CYCLE-FOUND-SW
           SET CR-IDX TO 1

           SEARCH CR-TABLE
              AT END
                 CONTINUE
              WHEN TB-CYCLE-ID(CR-IDX) = DB-CYCLE-ID
                 MOVE "Y" TO WK-CYCLE-FOUND-SW
                 MOVE TB-RESOLVED-DT(CR-IDX) TO WK-ACCRUAL-DT
           END-SEARCH

           IF NOT WK-CYCLE-FOUND
              DISPLAY "周期基準日未登録 ACCT=" DB-ACCT-NO
              DISPLAY "周期ＩＤ=" DB-CYCLE-ID
              PERFORM 9900-DATA-ABEND
           END-IF.

       3400-CALC-INTEREST.
           MOVE DB-PERIOD-START-DT TO WK-PERIOD-START-DT
           COMPUTE WK-START-DAYS =
              FUNCTION INTEGER-OF-DATE(WK-PERIOD-START-DT)
           COMPUTE WK-END-DAYS =
              FUNCTION INTEGER-OF-DATE(WK-ACCRUAL-DT)
           COMPUTE WK-ACCR-DAYS = WK-END-DAYS - WK-START-DAYS

           IF WK-ACCR-DAYS < ZERO
              DISPLAY "利息計算日数不正 ACCT=" DB-ACCT-NO
              DISPLAY "開始日=" DB-PERIOD-START-DT
              DISPLAY "基準日=" WK-ACCRUAL-DT
              PERFORM 9900-DATA-ABEND
           END-IF

           COMPUTE WK-INT-AMT =
              DB-AVG-DAILY-BAL * DB-INT-RATE * WK-ACCR-DAYS / 365.

       3500-WRITE-INTEREST.
           INITIALIZE KZINTF-REC
           MOVE DB-ACCT-NO      TO IN-ACCT-NO
           MOVE DB-CYCLE-ID     TO IN-CYCLE-ID
           MOVE WK-ACCRUAL-DT   TO IN-ACCRUAL-DT
           MOVE WK-ACCR-DAYS    TO IN-ACCR-DAYS
           MOVE WK-INT-AMT      TO IN-INT-AMT

           WRITE KZINTF-REC
           IF FS-KZINTF NOT = "00"
              DISPLAY "KZINTF 書込失敗 ST=" FS-KZINTF
              DISPLAY "口座番号=" DB-ACCT-NO
              PERFORM 9900-ABEND
           END-IF
           ADD 1 TO CT-KZINTF-WRITE.

       9000-CLOSE-FILES.
           CLOSE KZCYRF
           IF FS-KZCYRF NOT = "00"
              DISPLAY "KZCYRF クローズ失敗 ST=" FS-KZCYRF
              PERFORM 9900-ABEND
           END-IF

           CLOSE KZDBALF
           IF FS-KZDBALF NOT = "00"
              DISPLAY "KZDBALF クローズ失敗 ST=" FS-KZDBALF
              PERFORM 9900-ABEND
           END-IF

           CLOSE KZINTF
           IF FS-KZINTF NOT = "00"
              DISPLAY "KZINTF クローズ失敗 ST=" FS-KZINTF
              PERFORM 9900-ABEND
           END-IF.

       9900-DATA-ABEND.
           MOVE "データ不正による異常終了" TO WK-ERR-TEXT
           DISPLAY WK-ERR-TEXT
           MOVE 12 TO RETURN-CODE
           GOBACK.

       9900-ABEND.
           MOVE "入出力異常による異常終了" TO WK-ERR-TEXT
           DISPLAY WK-ERR-TEXT
           MOVE 8 TO RETURN-CODE
           GOBACK.
