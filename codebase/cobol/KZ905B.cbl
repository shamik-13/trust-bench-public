       IDENTIFICATION DIVISION.
       PROGRAM-ID. KZ905B.
      * 変更履歴
      * 版数  年月日(和暦)  担当                 概要
      * 1.00  R05.04.01    鈴木 一郎 (E-004)   新規作成
      * 1.01  R05.10.16    加藤 さやか (E-016) サイクル基準日判定見直し
      * 1.02  R06.03.18    田中 美咲 (E-021)   休日跨り処理の補正
       AUTHOR. KZ-SYSTEM.
      ******************************************************************
      * サイクル基準日決定バッチ
      * KZCYCFを順次読込み、共通日付サブKZ900Sで基準日を解決し、
      * KZCYRFへ出力する。営業日判定は本プログラムでは行わない。
      ******************************************************************

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZCYCF ASSIGN TO "KZCYCF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-KZCYCF-ST.
           SELECT KZCYRF ASSIGN TO "KZCYRF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-KZCYRF-ST.

       DATA DIVISION.
       FILE SECTION.
       FD  KZCYCF
           RECORDING MODE IS F.
           COPY KZCYCFC.

       FD  KZCYRF
           RECORDING MODE IS F.
           COPY KZCYRFC.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05 WS-KZCYCF-ST             PIC X(02) VALUE SPACE.
           05 WS-KZCYRF-ST             PIC X(02) VALUE SPACE.

       01  WS-FLAGS.
           05 WS-EOF-SW                PIC X(01) VALUE "N".
              88 WS-EOF                         VALUE "Y".
           05 WS-CYCF-OPEN-SW          PIC X(01) VALUE "N".
              88 WS-CYCF-OPEN                    VALUE "Y".
           05 WS-CYRF-OPEN-SW          PIC X(01) VALUE "N".
              88 WS-CYRF-OPEN                    VALUE "Y".

       01  WS-COUNTERS.
           05 WS-READ-CNT              PIC 9(09) COMP-3 VALUE 0.
           05 WS-WRITE-CNT             PIC 9(09) COMP-3 VALUE 0.

       01  WS-DATE-WORK.
           05 WS-DATE-X                PIC X(08) VALUE SPACE.
           05 WS-YYYY-X                PIC X(04) VALUE SPACE.
           05 WS-MM-X                  PIC X(02) VALUE SPACE.
           05 WS-DD-X                  PIC X(02) VALUE SPACE.
           05 WS-YYYY                  PIC 9(04) VALUE 0.
           05 WS-MM                    PIC 9(02) VALUE 0.
           05 WS-DD                    PIC 9(02) VALUE 0.
           05 WS-MAX-DD                PIC 9(02) VALUE 0.
           05 WS-REM4                  PIC 9(04) VALUE 0.
           05 WS-REM100                PIC 9(04) VALUE 0.
           05 WS-REM400                PIC 9(04) VALUE 0.

       01  WS-ABEND-INFO.
           05 WS-ABEND-STEP            PIC X(16) VALUE SPACE.
           05 WS-ABEND-REASON          PIC X(60) VALUE SPACE.

           COPY LK-CAL-PARM.

       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-INIT
           PERFORM 2000-PROCESS UNTIL WS-EOF
           PERFORM 9000-NORMAL-END
           GOBACK.

       1000-INIT.
           MOVE 0 TO RETURN-CODE
           DISPLAY "KZ905B サイクル基準日決定バッチ 開始"
           OPEN INPUT KZCYCF
           IF WS-KZCYCF-ST NOT = "00"
              MOVE "OPEN-KZCYCF" TO WS-ABEND-STEP
              STRING "KZCYCF オープン失敗 ST=" WS-KZCYCF-ST
                 DELIMITED BY SIZE INTO WS-ABEND-REASON
              END-STRING
              PERFORM 8000-ABEND
           END-IF
           SET WS-CYCF-OPEN TO TRUE
           OPEN OUTPUT KZCYRF
           IF WS-KZCYRF-ST NOT = "00"
              MOVE "OPEN-KZCYRF" TO WS-ABEND-STEP
              STRING "KZCYRF オープン失敗 ST=" WS-KZCYRF-ST
                 DELIMITED BY SIZE INTO WS-ABEND-REASON
              END-STRING
              PERFORM 8000-ABEND
           END-IF
           SET WS-CYRF-OPEN TO TRUE
           PERFORM 2100-READ-CYCLE.

       2000-PROCESS.
           PERFORM 3000-VALIDATE-CYCLE
           PERFORM 4000-CALL-CALENDAR
           PERFORM 5000-WRITE-RESULT
           PERFORM 2100-READ-CYCLE.

       2100-READ-CYCLE.
           READ KZCYCF
              AT END
                 SET WS-EOF TO TRUE
              NOT AT END
                 IF WS-KZCYCF-ST = "00"
                    ADD 1 TO WS-READ-CNT
                 ELSE
                    MOVE "READ-KZCYCF" TO WS-ABEND-STEP
                    STRING "KZCYCF 読込失敗 ST=" WS-KZCYCF-ST
                       DELIMITED BY SIZE INTO WS-ABEND-REASON
                    END-STRING
                    PERFORM 8000-ABEND
                 END-IF
           END-READ.

       3000-VALIDATE-CYCLE.
           IF CY-CYCLE-ID = SPACE
              MOVE "VALIDATE-CYCLE" TO WS-ABEND-STEP
              MOVE "サイクルＩＤ未設定" TO WS-ABEND-REASON
              PERFORM 8000-ABEND
           END-IF
           MOVE CY-NOMINAL-DT TO WS-DATE-X
           IF WS-DATE-X NOT NUMERIC
              MOVE "VALIDATE-DATE" TO WS-ABEND-STEP
              STRING "名目日付数字外 ID=" CY-CYCLE-ID
                 DELIMITED BY SIZE INTO WS-ABEND-REASON
              END-STRING
              PERFORM 8000-ABEND
           END-IF
           MOVE WS-DATE-X(1:4) TO WS-YYYY-X
           MOVE WS-DATE-X(5:2) TO WS-MM-X
           MOVE WS-DATE-X(7:2) TO WS-DD-X
           MOVE WS-YYYY-X TO WS-YYYY
           MOVE WS-MM-X   TO WS-MM
           MOVE WS-DD-X   TO WS-DD
           IF WS-YYYY < 1900 OR WS-YYYY > 2099
              MOVE "VALIDATE-DATE" TO WS-ABEND-STEP
              STRING "名目日付年範囲外 ID=" CY-CYCLE-ID
                 DELIMITED BY SIZE INTO WS-ABEND-REASON
              END-STRING
              PERFORM 8000-ABEND
           END-IF
           IF WS-MM < 1 OR WS-MM > 12
              MOVE "VALIDATE-DATE" TO WS-ABEND-STEP
              STRING "名目日付月不正 ID=" CY-CYCLE-ID
                 DELIMITED BY SIZE INTO WS-ABEND-REASON
              END-STRING
              PERFORM 8000-ABEND
           END-IF
           PERFORM 3100-SET-MAX-DAY
           IF WS-DD < 1 OR WS-DD > WS-MAX-DD
              MOVE "VALIDATE-DATE" TO WS-ABEND-STEP
              STRING "名目日付日不正 ID=" CY-CYCLE-ID
                 DELIMITED BY SIZE INTO WS-ABEND-REASON
              END-STRING
              PERFORM 8000-ABEND
           END-IF.

       3100-SET-MAX-DAY.
           EVALUATE WS-MM
              WHEN 1  MOVE 31 TO WS-MAX-DD
              WHEN 3  MOVE 31 TO WS-MAX-DD
              WHEN 5  MOVE 31 TO WS-MAX-DD
              WHEN 7  MOVE 31 TO WS-MAX-DD
              WHEN 8  MOVE 31 TO WS-MAX-DD
              WHEN 10 MOVE 31 TO WS-MAX-DD
              WHEN 12 MOVE 31 TO WS-MAX-DD
              WHEN 4  MOVE 30 TO WS-MAX-DD
              WHEN 6  MOVE 30 TO WS-MAX-DD
              WHEN 9  MOVE 30 TO WS-MAX-DD
              WHEN 11 MOVE 30 TO WS-MAX-DD
              WHEN 2
                 DIVIDE WS-YYYY BY 4 GIVING WS-REM4
                    REMAINDER WS-REM4
                 DIVIDE WS-YYYY BY 100 GIVING WS-REM100
                    REMAINDER WS-REM100
                 DIVIDE WS-YYYY BY 400 GIVING WS-REM400
                    REMAINDER WS-REM400
                 IF (WS-REM4 = 0 AND WS-REM100 NOT = 0)
                    OR WS-REM400 = 0
                    MOVE 29 TO WS-MAX-DD
                 ELSE
                    MOVE 28 TO WS-MAX-DD
                 END-IF
           END-EVALUATE.

       4000-CALL-CALENDAR.
           INITIALIZE LK-CAL-PARM
           MOVE CY-NOMINAL-DT TO LK-CAL-NOMINAL-DT
           CALL "KZ900S" USING LK-CAL-PARM
           IF LK-CAL-RET NOT = "00"
              MOVE "CALL-KZ900S" TO WS-ABEND-STEP
              STRING "KZ900S 異常復帰 RET=" LK-CAL-RET
                     " ID=" CY-CYCLE-ID
                 DELIMITED BY SIZE INTO WS-ABEND-REASON
              END-STRING
              PERFORM 8000-ABEND
           END-IF.

       5000-WRITE-RESULT.
           INITIALIZE KZCYRF-REC
           MOVE CY-CYCLE-ID        TO CR-CYCLE-ID
           MOVE CY-NOMINAL-DT      TO CR-NOMINAL-DT
           MOVE LK-CAL-RESOLVED-DT TO CR-RESOLVED-DT
           MOVE LK-CAL-ROLLED-FLAG TO CR-ROLLED-FLAG
           WRITE KZCYRF-REC
           IF WS-KZCYRF-ST = "00"
              ADD 1 TO WS-WRITE-CNT
           ELSE
              MOVE "WRITE-KZCYRF" TO WS-ABEND-STEP
              STRING "KZCYRF 書込失敗 ST=" WS-KZCYRF-ST
                     " ID=" CY-CYCLE-ID
                 DELIMITED BY SIZE INTO WS-ABEND-REASON
              END-STRING
              PERFORM 8000-ABEND
           END-IF.

       8000-ABEND.
           DISPLAY "KZ905B 異常終了 STEP=" WS-ABEND-STEP
           DISPLAY "KZ905B 理由=" WS-ABEND-REASON
           DISPLAY "KZ905B 入力件数=" WS-READ-CNT
           DISPLAY "KZ905B 出力件数=" WS-WRITE-CNT
           MOVE 12 TO RETURN-CODE
           PERFORM 8900-CLOSE-FILES
           GOBACK.

       8900-CLOSE-FILES.
           IF WS-CYCF-OPEN
              CLOSE KZCYCF
              IF WS-KZCYCF-ST NOT = "00"
                 DISPLAY "KZCYCF クローズ失敗 ST=" WS-KZCYCF-ST
              END-IF
              MOVE "N" TO WS-CYCF-OPEN-SW
           END-IF
           IF WS-CYRF-OPEN
              CLOSE KZCYRF
              IF WS-KZCYRF-ST NOT = "00"
                 DISPLAY "KZCYRF クローズ失敗 ST=" WS-KZCYRF-ST
              END-IF
              MOVE "N" TO WS-CYRF-OPEN-SW
           END-IF.

       9000-NORMAL-END.
           PERFORM 8900-CLOSE-FILES
           DISPLAY "KZ905B 正常終了"
           DISPLAY "KZ905B 入力件数=" WS-READ-CNT
           DISPLAY "KZ905B 出力件数=" WS-WRITE-CNT
           MOVE 0 TO RETURN-CODE.
