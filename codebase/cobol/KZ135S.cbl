       IDENTIFICATION DIVISION.
       PROGRAM-ID. KZ135S.
      * 変更履歴
      * 版数  年月日(和暦)  担当                         概要
      * 1.00  H30.04.01    システム部 勘定系チーム      新規作成
      * 1.01  R02.10.15    システム部 勘定系チーム      元号判定処理見直し
      * 1.02  R05.06.20    システム部 勘定系チーム      期日形式変換対応追加
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZCALF ASSIGN TO "KZCALF"
              ORGANIZATION IS INDEXED
              ACCESS MODE  IS DYNAMIC
              RECORD KEY   IS CA-CAL-DT
              FILE STATUS  IS WK-CAL-ST.
       DATA DIVISION.
       FILE SECTION.
       FD  KZCALF.
           COPY KZCALFC.
       WORKING-STORAGE SECTION.
       01  WK-FILE-STATUS.
           05 WK-CAL-ST              PIC X(02) VALUE SPACE.
       01  WK-WORK.
           05 WK-IN-DATE             PIC X(16) VALUE SPACE.
           05 WK-FMT                 PIC X(01) VALUE SPACE.
           05 WK-ERA                 PIC X(01) VALUE SPACE.
           05 WK-YYYY-X              PIC X(04) VALUE SPACE.
           05 WK-MM-X                PIC X(02) VALUE SPACE.
           05 WK-DD-X                PIC X(02) VALUE SPACE.
           05 WK-YY-X                PIC X(02) VALUE SPACE.
           05 WK-YYYY                PIC 9(04) VALUE ZERO.
           05 WK-MM                  PIC 9(02) VALUE ZERO.
           05 WK-DD                  PIC 9(02) VALUE ZERO.
           05 WK-YY                  PIC 9(02) VALUE ZERO.
           05 WK-LAST-DD             PIC 9(02) VALUE ZERO.
           05 WK-REM4                PIC 9(04) VALUE ZERO.
           05 WK-REM100              PIC 9(04) VALUE ZERO.
           05 WK-REM400              PIC 9(04) VALUE ZERO.
           05 WK-VALID-SW            PIC X(01) VALUE SPACE.
              88 WK-VALID                    VALUE "Y".
              88 WK-INVALID                  VALUE "N".
           05 WK-HARD-ERR-SW         PIC X(01) VALUE SPACE.
              88 WK-HARD-ERR                 VALUE "Y".
              88 WK-NO-HARD-ERR              VALUE "N".
       01  WK-CONST.
           05 CN-RET-OK              PIC X(02) VALUE "00".
           05 CN-RET-FMT             PIC X(02) VALUE "10".
           05 CN-RET-DATE            PIC X(02) VALUE "20".
           05 CN-RET-CAL-NF          PIC X(02) VALUE "30".
           05 CN-RET-CAL-IO          PIC X(02) VALUE "90".
           05 CN-RET-SUB             PIC X(02) VALUE "91".
           05 CN-BLANK-DATE          PIC 9(08) VALUE ZERO.
       01  WK-NOMINAL-DT.
           05 WK-NOM-YYYY            PIC 9(04).
           05 WK-NOM-MM              PIC 9(02).
           05 WK-NOM-DD              PIC 9(02).
       01  WK-DATE-NUM REDEFINES WK-NOMINAL-DT PIC 9(08).
           COPY LK-CAL-PARM.
       LINKAGE SECTION.
       01  KZ135-PARM.
           05 KZ135-IN-DATE          PIC X(16).
           05 KZ135-OUT-DT           PIC 9(08).
           05 KZ135-HOLIDAY-FLAG     PIC X(01).
           05 KZ135-RESOLVED-DT      PIC 9(08).
           05 KZ135-ROLLED-FLAG      PIC X(01).
           05 KZ135-RET              PIC X(02).
           05 KZ135-REASON           PIC X(40).
       PROCEDURE DIVISION USING KZ135-PARM.
       0000-MAIN.
           PERFORM 1000-INIT.
           PERFORM 2000-OPEN-CAL.
           IF WK-NO-HARD-ERR
              PERFORM 3000-CONVERT-DATE
           END-IF
           IF WK-NO-HARD-ERR AND KZ135-RET = CN-RET-OK
              PERFORM 4000-CHECK-CALENDAR
           END-IF
           IF WK-NO-HARD-ERR AND KZ135-RET = CN-RET-OK
              PERFORM 5000-CALL-KZ900S
           END-IF
           PERFORM 9000-CLOSE-CAL
           IF WK-HARD-ERR
              MOVE 8 TO RETURN-CODE
           ELSE
              MOVE 0 TO RETURN-CODE
           END-IF
           GOBACK.
       1000-INIT.
           MOVE KZ135-IN-DATE TO WK-IN-DATE
           MOVE CN-BLANK-DATE TO KZ135-OUT-DT
           MOVE CN-BLANK-DATE TO KZ135-RESOLVED-DT
           MOVE SPACE TO KZ135-HOLIDAY-FLAG
           MOVE SPACE TO KZ135-ROLLED-FLAG
           MOVE CN-RET-OK TO KZ135-RET
           MOVE SPACE TO KZ135-REASON
           SET WK-NO-HARD-ERR TO TRUE
           SET WK-INVALID TO TRUE.
       2000-OPEN-CAL.
           OPEN INPUT KZCALF
           IF WK-CAL-ST NOT = "00"
              MOVE CN-RET-CAL-IO TO KZ135-RET
              STRING "暦ファイルオープン失敗 ST="
                     WK-CAL-ST
                DELIMITED BY SIZE INTO KZ135-REASON
              END-STRING
              DISPLAY "KZCALF オープン失敗 ST=" WK-CAL-ST
              SET WK-HARD-ERR TO TRUE
           END-IF.
       3000-CONVERT-DATE.
           PERFORM 3100-JUDGE-FORMAT
           IF KZ135-RET = CN-RET-OK
              PERFORM 3200-MOVE-NUMERIC
           END-IF
           IF KZ135-RET = CN-RET-OK
              PERFORM 3300-CHECK-EXISTENCE
           END-IF
           IF KZ135-RET = CN-RET-OK
              MOVE WK-YYYY TO WK-NOM-YYYY
              MOVE WK-MM   TO WK-NOM-MM
              MOVE WK-DD   TO WK-NOM-DD
              MOVE WK-DATE-NUM TO KZ135-OUT-DT
           END-IF.
       3100-JUDGE-FORMAT.
           EVALUATE TRUE
              WHEN WK-IN-DATE(1:8) IS NUMERIC
                   MOVE "1" TO WK-FMT
                   MOVE WK-IN-DATE(1:4) TO WK-YYYY-X
                   MOVE WK-IN-DATE(5:2) TO WK-MM-X
                   MOVE WK-IN-DATE(7:2) TO WK-DD-X
              WHEN WK-IN-DATE(1:4) IS NUMERIC
               AND WK-IN-DATE(5:1) = "/"
               AND WK-IN-DATE(6:2) IS NUMERIC
               AND WK-IN-DATE(8:1) = "/"
               AND WK-IN-DATE(9:2) IS NUMERIC
                   MOVE "2" TO WK-FMT
                   MOVE WK-IN-DATE(1:4) TO WK-YYYY-X
                   MOVE WK-IN-DATE(6:2) TO WK-MM-X
                   MOVE WK-IN-DATE(9:2) TO WK-DD-X
              WHEN WK-IN-DATE(1:4) IS NUMERIC
               AND WK-IN-DATE(5:1) = "-"
               AND WK-IN-DATE(6:2) IS NUMERIC
               AND WK-IN-DATE(8:1) = "-"
               AND WK-IN-DATE(9:2) IS NUMERIC
                   MOVE "3" TO WK-FMT
                   MOVE WK-IN-DATE(1:4) TO WK-YYYY-X
                   MOVE WK-IN-DATE(6:2) TO WK-MM-X
                   MOVE WK-IN-DATE(9:2) TO WK-DD-X
              WHEN WK-IN-DATE(1:1) = "R" OR WK-IN-DATE(1:1) = "H"
                OR WK-IN-DATE(1:1) = "S"
                   PERFORM 3110-JUDGE-ERA
              WHEN OTHER
                   MOVE CN-RET-FMT TO KZ135-RET
                   MOVE "入力日付形式不正" TO KZ135-REASON
           END-EVALUATE.
       3110-JUDGE-ERA.
           IF WK-IN-DATE(2:6) IS NUMERIC
              MOVE "4" TO WK-FMT
              MOVE WK-IN-DATE(1:1) TO WK-ERA
              MOVE WK-IN-DATE(2:2) TO WK-YY-X
              MOVE WK-IN-DATE(4:2) TO WK-MM-X
              MOVE WK-IN-DATE(6:2) TO WK-DD-X
           ELSE
              MOVE CN-RET-FMT TO KZ135-RET
              MOVE "和暦日付形式不正" TO KZ135-REASON
           END-IF.
       3200-MOVE-NUMERIC.
           IF WK-FMT = "4"
              MOVE WK-YY-X TO WK-YY
              EVALUATE WK-ERA
                 WHEN "R"
                    COMPUTE WK-YYYY = 2018 + WK-YY
                 WHEN "H"
                    COMPUTE WK-YYYY = 1988 + WK-YY
                 WHEN "S"
                    COMPUTE WK-YYYY = 1925 + WK-YY
              END-EVALUATE
           ELSE
              MOVE WK-YYYY-X TO WK-YYYY
           END-IF
           MOVE WK-MM-X TO WK-MM
           MOVE WK-DD-X TO WK-DD.
       3300-CHECK-EXISTENCE.
           IF WK-YYYY < 1900 OR WK-YYYY > 2099
              MOVE CN-RET-DATE TO KZ135-RET
              MOVE "西暦年範囲外" TO KZ135-REASON
           END-IF
           IF KZ135-RET = CN-RET-OK
              IF WK-MM < 1 OR WK-MM > 12
                 MOVE CN-RET-DATE TO KZ135-RET
                 MOVE "月不正" TO KZ135-REASON
              END-IF
           END-IF
           IF KZ135-RET = CN-RET-OK
              PERFORM 3310-SET-LAST-DAY
              IF WK-DD < 1 OR WK-DD > WK-LAST-DD
                 MOVE CN-RET-DATE TO KZ135-RET
                 MOVE "日不正" TO KZ135-REASON
              END-IF
           END-IF
           IF KZ135-RET = CN-RET-OK AND WK-FMT = "4"
              PERFORM 3320-CHECK-ERA-RANGE
           END-IF.
       3310-SET-LAST-DAY.
           EVALUATE WK-MM
              WHEN 1 WHEN 3 WHEN 5 WHEN 7 WHEN 8 WHEN 10 WHEN 12
                   MOVE 31 TO WK-LAST-DD
              WHEN 4 WHEN 6 WHEN 9 WHEN 11
                   MOVE 30 TO WK-LAST-DD
              WHEN 2
                   COMPUTE WK-REM4 = FUNCTION MOD(WK-YYYY 4)
                   COMPUTE WK-REM100 = FUNCTION MOD(WK-YYYY 100)
                   COMPUTE WK-REM400 = FUNCTION MOD(WK-YYYY 400)
                   IF WK-REM400 = 0
                      MOVE 29 TO WK-LAST-DD
                   ELSE
                      IF WK-REM4 = 0 AND WK-REM100 NOT = 0
                         MOVE 29 TO WK-LAST-DD
                      ELSE
                         MOVE 28 TO WK-LAST-DD
                      END-IF
                   END-IF
           END-EVALUATE.
       3320-CHECK-ERA-RANGE.
           EVALUATE WK-ERA
              WHEN "R"
                   IF WK-DATE-NUM < 20190501
                      MOVE CN-RET-DATE TO KZ135-RET
                      MOVE "令和開始日前" TO KZ135-REASON
                   END-IF
              WHEN "H"
                   IF WK-DATE-NUM < 19890108
                      MOVE CN-RET-DATE TO KZ135-RET
                      MOVE "平成開始日前" TO KZ135-REASON
                   END-IF
                   IF WK-DATE-NUM > 20190430
                      MOVE CN-RET-DATE TO KZ135-RET
                      MOVE "平成終了日後" TO KZ135-REASON
                   END-IF
              WHEN "S"
                   IF WK-DATE-NUM < 19261225
                      MOVE CN-RET-DATE TO KZ135-RET
                      MOVE "昭和開始日前" TO KZ135-REASON
                   END-IF
                   IF WK-DATE-NUM > 19890107
                      MOVE CN-RET-DATE TO KZ135-RET
                      MOVE "昭和終了日後" TO KZ135-REASON
                   END-IF
           END-EVALUATE.
       4000-CHECK-CALENDAR.
           MOVE KZ135-OUT-DT TO CA-CAL-DT
           READ KZCALF KEY IS CA-CAL-DT
              INVALID KEY
                 MOVE CN-RET-CAL-NF TO KZ135-RET
                 MOVE "暦ファイル未登録日" TO KZ135-REASON
              NOT INVALID KEY
                 MOVE CA-HOLIDAY-FLAG TO KZ135-HOLIDAY-FLAG
           END-READ
           IF WK-CAL-ST NOT = "00" AND WK-CAL-ST NOT = "23"
              MOVE CN-RET-CAL-IO TO KZ135-RET
              STRING "暦ファイル読込失敗 ST="
                     WK-CAL-ST
                DELIMITED BY SIZE INTO KZ135-REASON
              END-STRING
              DISPLAY "KZCALF 読込失敗 ST=" WK-CAL-ST
              SET WK-HARD-ERR TO TRUE
           END-IF.
       5000-CALL-KZ900S.
           MOVE KZ135-OUT-DT TO LK-CAL-NOMINAL-DT
           MOVE ZERO TO LK-CAL-RESOLVED-DT
           MOVE SPACE TO LK-CAL-ROLLED-FLAG
           MOVE CN-RET-OK TO LK-CAL-RET
           CALL "KZ900S" USING LK-CAL-PARM
           IF LK-CAL-RET = CN-RET-OK
              MOVE LK-CAL-RESOLVED-DT TO KZ135-RESOLVED-DT
              MOVE LK-CAL-ROLLED-FLAG TO KZ135-ROLLED-FLAG
           ELSE
              MOVE CN-RET-SUB TO KZ135-RET
              STRING "営業日判定エラー RC="
                     LK-CAL-RET
                DELIMITED BY SIZE INTO KZ135-REASON
              END-STRING
           END-IF.
       9000-CLOSE-CAL.
           IF WK-CAL-ST NOT = SPACE
              CLOSE KZCALF
              IF WK-CAL-ST NOT = "00"
                 MOVE CN-RET-CAL-IO TO KZ135-RET
                 STRING "暦ファイルクローズ失敗 ST="
                        WK-CAL-ST
                   DELIMITED BY SIZE INTO KZ135-REASON
                 END-STRING
                 DISPLAY "KZCALF クローズ失敗 ST=" WK-CAL-ST
                 SET WK-HARD-ERR TO TRUE
              END-IF
           END-IF.
