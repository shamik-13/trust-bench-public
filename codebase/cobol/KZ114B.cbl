       IDENTIFICATION DIVISION.
       PROGRAM-ID. KZ114B.
      * 変更履歴
      * 版数  年月日(和暦)  担当                      概要
      * 1.00  令和04.04.01  システム部 勘定系チーム  新規作成
      * 1.01  令和05.10.16  システム部 勘定系チーム  祝日取込処理見直し
      * 1.02  令和06.12.02  システム部 勘定系チーム  暦再構築統合対応
       AUTHOR. TRUST-BANK-SYSTEM.
       DATE-WRITTEN. 2025-03-31.
       DATE-COMPILED.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZIMPF ASSIGN TO "KZIMPF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS ST-KZIMPF.

           SELECT KZHOLF ASSIGN TO "KZHOLF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS HO-HOLIDAY-DT
               FILE STATUS IS ST-KZHOLF.

           SELECT KZCALF ASSIGN TO "KZCALF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CA-CAL-DT
               FILE STATUS IS ST-KZCALF.

           SELECT KZCYCF ASSIGN TO "KZCYCF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS ST-KZCYCF.

           SELECT KZFPRF ASSIGN TO "KZFPRF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS FP-FISCAL-PERIOD-ID
               FILE STATUS IS ST-KZFPRF.

           SELECT KZJNCF ASSIGN TO "KZJNCF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS JN-JOBNET-ID
               FILE STATUS IS ST-KZJNCF.

           SELECT KZRPRF ASSIGN TO "KZRPRF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS RP-REPROCESS-ID
               FILE STATUS IS ST-KZRPRF.

           SELECT KZSCHF ASSIGN TO "KZSCHF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS ST-KZSCHF.

           SELECT KZCYRF ASSIGN TO "KZCYRF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS ST-KZCYRF.

           SELECT KZAGEF ASSIGN TO "KZAGEF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS ST-KZAGEF.

       DATA DIVISION.
       FILE SECTION.
       FD  KZIMPF.
           COPY KZIMPFC.

       FD  KZHOLF.
           COPY KZHOLFC.

       FD  KZCALF.
           COPY KZCALFC.

       FD  KZCYCF.
           COPY KZCYCFC.

       FD  KZFPRF.
           COPY KZFPRFC.

       FD  KZJNCF.
           COPY KZJNCFC.

       FD  KZRPRF.
           COPY KZRPRFC.

       FD  KZSCHF.
           COPY KZSCHFC.

       FD  KZCYRF.
           COPY KZCYRFC.

       FD  KZAGEF.
           COPY KZAGEFC.

       WORKING-STORAGE SECTION.
           COPY LK-CAL-PARM.

       01  FS-AREA.
           05 ST-KZIMPF              PIC XX VALUE SPACE.
           05 ST-KZHOLF              PIC XX VALUE SPACE.
           05 ST-KZCALF              PIC XX VALUE SPACE.
           05 ST-KZCYCF              PIC XX VALUE SPACE.
           05 ST-KZFPRF              PIC XX VALUE SPACE.
           05 ST-KZJNCF              PIC XX VALUE SPACE.
           05 ST-KZRPRF              PIC XX VALUE SPACE.
           05 ST-KZSCHF              PIC XX VALUE SPACE.
           05 ST-KZCYRF              PIC XX VALUE SPACE.
           05 ST-KZAGEF              PIC XX VALUE SPACE.

       01  SW-AREA.
           05 SW-ABEND               PIC X VALUE "N".
              88 ABEND-ON            VALUE "Y".
           05 SW-STOP-STAGE          PIC X VALUE "N".
              88 STOP-STAGE          VALUE "Y".
           05 SW-END-IMP             PIC X VALUE "N".
              88 END-IMP             VALUE "Y".
           05 SW-END-CYC             PIC X VALUE "N".
              88 END-CYC             VALUE "Y".
           05 SW-END-JN              PIC X VALUE "N".
              88 END-JN              VALUE "Y".
           05 SW-END-RP              PIC X VALUE "N".
              88 END-RP              VALUE "Y".
           05 SW-HOL-FOUND           PIC X VALUE "N".
              88 HOL-FOUND           VALUE "Y".
           05 SW-CAL-FOUND           PIC X VALUE "N".
              88 CAL-FOUND           VALUE "Y".
           05 SW-PER-FOUND           PIC X VALUE "N".
              88 PER-FOUND           VALUE "Y".
           05 SW-RP-DUP              PIC X VALUE "N".
              88 RP-DUP              VALUE "Y".

       01  CNT-AREA.
           05 CNT-IMP-IN             PIC 9(9) VALUE ZERO.
           05 CNT-IMP-WR             PIC 9(9) VALUE ZERO.
           05 CNT-IMP-EX             PIC 9(9) VALUE ZERO.
           05 CNT-IMP-PD             PIC 9(9) VALUE ZERO.
           05 CNT-CAL-WR             PIC 9(9) VALUE ZERO.
           05 CNT-CAL-HOL            PIC 9(9) VALUE ZERO.
           05 CNT-CYC-IN             PIC 9(9) VALUE ZERO.
           05 CNT-CYC-WR             PIC 9(9) VALUE ZERO.
           05 CNT-CYC-PD             PIC 9(9) VALUE ZERO.
           05 CNT-SCH-WR             PIC 9(9) VALUE ZERO.
           05 CNT-JN-IN              PIC 9(9) VALUE ZERO.
           05 CNT-JN-EX              PIC 9(9) VALUE ZERO.
           05 CNT-RP-IN              PIC 9(9) VALUE ZERO.
           05 CNT-RP-WR              PIC 9(9) VALUE ZERO.
           05 CNT-RP-EX              PIC 9(9) VALUE ZERO.
           05 CNT-RP-PD              PIC 9(9) VALUE ZERO.
           05 CNT-AGE-WR             PIC 9(9) VALUE ZERO.

       01  DATE-AREA.
           05 WS-TODAY               PIC 9(8) VALUE ZERO.
           05 WS-FISCAL-YYYY         PIC 9(4) VALUE ZERO.
           05 WS-FY-START            PIC 9(8) VALUE ZERO.
           05 WS-FY-END              PIC 9(8) VALUE ZERO.
           05 WS-CUR-DATE            PIC 9(8) VALUE ZERO.
           05 WS-NEXT-DATE           PIC 9(8) VALUE ZERO.
           05 WS-PER-START           PIC 9(8) VALUE ZERO.
           05 WS-PER-END             PIC 9(8) VALUE ZERO.
           05 WS-CYCLE-DATE          PIC 9(8) VALUE ZERO.
           05 WS-BASE-DT             PIC 9(8) VALUE ZERO.
           05 WS-TARGET-DT           PIC 9(8) VALUE ZERO.
           05 WS-DAY-COUNT           PIC S9(9) COMP VALUE ZERO.
           05 WS-BUS-COUNT           PIC S9(9) COMP VALUE ZERO.
           05 WS-DATE-OK             PIC X VALUE "N".
              88 DATE-OK             VALUE "Y".

       01  WORK-AREA.
           05 WS-STAGE               PIC X(20) VALUE SPACE.
           05 WS-REASON              PIC X(60) VALUE SPACE.
           05 WS-YYYY                PIC 9(4) VALUE ZERO.
           05 WS-MM                  PIC 9(2) VALUE ZERO.
           05 WS-DD                  PIC 9(2) VALUE ZERO.
           05 WS-MAX-DD              PIC 9(2) VALUE ZERO.
           05 WS-MON-IDX             PIC 99 VALUE ZERO.
           05 WS-PERIOD-NO           PIC 99 VALUE ZERO.
           05 WS-PERIOD-ID           PIC X(10) VALUE SPACE.
           05 WS-AGING-SEQ           PIC 9(6) VALUE ZERO.
           05 WS-ACCEPT-YYYYMMDD     PIC 9(8) VALUE ZERO.

       01  RAW-DATE-AREA.
           05 RAW-YYYY               PIC 9(4).
           05 FILLER                 PIC X.
           05 RAW-MM                 PIC 9(2).
           05 FILLER                 PIC X.
           05 RAW-DD                 PIC 9(2).

       01  RAW-DATE-NODASH.
           05 RAWN-YYYY              PIC 9(4).
           05 RAWN-MM                PIC 9(2).
           05 RAWN-DD                PIC 9(2).

       01  HOLD-AREAS.
           05 HOLD-CYCLE-ID          PIC X(20) VALUE SPACE.
           05 HOLD-NOMINAL-DT        PIC 9(8) VALUE ZERO.
           05 HOLD-JOBNET-ID         PIC X(20) VALUE SPACE.
           05 HOLD-CALENDAR-CD       PIC X(10) VALUE SPACE.

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE 8 TO RETURN-CODE
           DISPLAY "KZ190B 暦再構築統合バッチ 開始"
           ACCEPT WS-ACCEPT-YYYYMMDD FROM DATE YYYYMMDD
           MOVE WS-ACCEPT-YYYYMMDD TO WS-TODAY
           PERFORM 0100-SET-FISCAL-YEAR
           PERFORM 0200-OPEN-FILES
           IF NOT ABEND-ON
              PERFORM 1000-IMPORT-HOLIDAY
           END-IF
           IF NOT ABEND-ON AND NOT STOP-STAGE
              PERFORM 2000-BUILD-CALENDAR
           END-IF
           IF NOT ABEND-ON AND NOT STOP-STAGE
              PERFORM 3000-BUILD-PERIODS
           END-IF
           IF NOT ABEND-ON AND NOT STOP-STAGE
              PERFORM 4000-BUILD-CYCLES
           END-IF
           IF NOT ABEND-ON AND NOT STOP-STAGE
              PERFORM 5000-EXPAND-JOBNET
           END-IF
           IF NOT ABEND-ON AND NOT STOP-STAGE
              PERFORM 6000-APPLY-REPROCESS
           END-IF
           IF NOT ABEND-ON AND NOT STOP-STAGE
              PERFORM 7000-WRITE-AGING
           END-IF
           PERFORM 9000-CLOSE-FILES
           IF ABEND-ON
              MOVE 12 TO RETURN-CODE
              DISPLAY "KZ190B 異常終了"
           ELSE
              IF STOP-STAGE
                 MOVE 8 TO RETURN-CODE
                 DISPLAY "KZ190B 後続段階停止"
              ELSE
                 MOVE 0 TO RETURN-CODE
                 DISPLAY "KZ190B 正常終了"
              END-IF
           END-IF
           GOBACK.

       0100-SET-FISCAL-YEAR.
           MOVE WS-TODAY(1:4) TO WS-YYYY
           MOVE WS-TODAY(5:2) TO WS-MM
           IF WS-MM < 4
              SUBTRACT 1 FROM WS-YYYY
           END-IF
           MOVE WS-YYYY TO WS-FISCAL-YYYY
           COMPUTE WS-FY-START = (WS-FISCAL-YYYY * 10000) + 401
           COMPUTE WS-FY-END =
              ((WS-FISCAL-YYYY + 1) * 10000) + 331
           DISPLAY "対象年度=" WS-FISCAL-YYYY
           DISPLAY "対象期間=" WS-FY-START " - " WS-FY-END.

       0200-OPEN-FILES.
           OPEN INPUT KZIMPF KZCYCF
                I-O KZHOLF KZCALF KZFPRF KZJNCF KZRPRF
                OUTPUT KZSCHF KZCYRF KZAGEF
           IF ST-KZIMPF NOT = "00"
              DISPLAY "KZIMPF オープン失敗 ST=" ST-KZIMPF
              SET ABEND-ON TO TRUE
           END-IF
           IF ST-KZCYCF NOT = "00"
              DISPLAY "KZCYCF オープン失敗 ST=" ST-KZCYCF
              SET ABEND-ON TO TRUE
           END-IF
           IF ST-KZHOLF NOT = "00" AND ST-KZHOLF NOT = "05"
              DISPLAY "KZHOLF オープン失敗 ST=" ST-KZHOLF
              SET ABEND-ON TO TRUE
           END-IF
           IF ST-KZCALF NOT = "00" AND ST-KZCALF NOT = "05"
              DISPLAY "KZCALF オープン失敗 ST=" ST-KZCALF
              SET ABEND-ON TO TRUE
           END-IF
           IF ST-KZFPRF NOT = "00" AND ST-KZFPRF NOT = "05"
              DISPLAY "KZFPRF オープン失敗 ST=" ST-KZFPRF
              SET ABEND-ON TO TRUE
           END-IF
           IF ST-KZJNCF NOT = "00"
              DISPLAY "KZJNCF オープン失敗 ST=" ST-KZJNCF
              SET ABEND-ON TO TRUE
           END-IF
           IF ST-KZRPRF NOT = "00" AND ST-KZRPRF NOT = "05"
              DISPLAY "KZRPRF オープン失敗 ST=" ST-KZRPRF
              SET ABEND-ON TO TRUE
           END-IF
           IF ST-KZSCHF NOT = "00"
              DISPLAY "KZSCHF オープン失敗 ST=" ST-KZSCHF
              SET ABEND-ON TO TRUE
           END-IF
           IF ST-KZCYRF NOT = "00"
              DISPLAY "KZCYRF オープン失敗 ST=" ST-KZCYRF
              SET ABEND-ON TO TRUE
           END-IF
           IF ST-KZAGEF NOT = "00"
              DISPLAY "KZAGEF オープン失敗 ST=" ST-KZAGEF
              SET ABEND-ON TO TRUE
           END-IF.

       1000-IMPORT-HOLIDAY.
           MOVE "休日取込" TO WS-STAGE
           DISPLAY "休日取込 開始"
           PERFORM UNTIL END-IMP OR ABEND-ON
              READ KZIMPF
                 AT END SET END-IMP TO TRUE
                 NOT AT END
                    ADD 1 TO CNT-IMP-IN
                    PERFORM 1100-EDIT-IMPORT
                    IF DATE-OK
                       PERFORM 1200-UPSERT-HOLIDAY
                    ELSE
                       ADD 1 TO CNT-IMP-EX
                    END-IF
              END-READ
           END-PERFORM
           IF NOT ABEND-ON
              IF CNT-IMP-IN = ZERO
                 DISPLAY "休日取込 入力なし"
                 SET STOP-STAGE TO TRUE
              END-IF
              IF CNT-IMP-IN NOT =
                 CNT-IMP-WR + CNT-IMP-EX + CNT-IMP-PD
                 DISPLAY "休日取込 件数不整合"
                 SET STOP-STAGE TO TRUE
              END-IF
           END-IF
           DISPLAY "休日取込 入力=" CNT-IMP-IN
           DISPLAY "休日取込 登録=" CNT-IMP-WR
           DISPLAY "休日取込 除外=" CNT-IMP-EX
           DISPLAY "休日取込 保留=" CNT-IMP-PD.

       1100-EDIT-IMPORT.
           MOVE "N" TO WS-DATE-OK
           IF IM-RAW-MARKET-CD = SPACE
              MOVE "市場コード未設定" TO WS-REASON
              EXIT PARAGRAPH
           END-IF
           IF IM-RAW-SOURCE-CD = SPACE
              ADD 1 TO CNT-IMP-PD
              MOVE "取込元未設定" TO WS-REASON
              EXIT PARAGRAPH
           END-IF
           IF IM-RAW-DATE-TEXT(5:1) = "-"
              MOVE IM-RAW-DATE-TEXT(1:10) TO RAW-DATE-AREA
              COMPUTE WS-CUR-DATE =
                 (RAW-YYYY * 10000) + (RAW-MM * 100) + RAW-DD
           ELSE
              MOVE IM-RAW-DATE-TEXT(1:8) TO RAW-DATE-NODASH
              COMPUTE WS-CUR-DATE =
                 (RAWN-YYYY * 10000) + (RAWN-MM * 100) + RAWN-DD
           END-IF
           PERFORM 8000-CHECK-DATE
           IF DATE-OK
              IF WS-CUR-DATE < WS-FY-START
                 OR WS-CUR-DATE > WS-FY-END
                 MOVE "年度範囲外" TO WS-REASON
                 MOVE "N" TO WS-DATE-OK
              END-IF
           END-IF.

       1200-UPSERT-HOLIDAY.
           MOVE WS-CUR-DATE TO HO-HOLIDAY-DT
           READ KZHOLF KEY IS HO-HOLIDAY-DT
              INVALID KEY
                 MOVE WS-CUR-DATE TO HO-HOLIDAY-DT
                 MOVE "JPBK" TO HO-HOLIDAY-CD
                 MOVE IM-RAW-MARKET-CD TO HO-MARKET-CD
                 MOVE IM-RAW-HOLIDAY-NAME TO HO-HOLIDAY-NAME
                 MOVE IM-RAW-SOURCE-CD TO HO-SOURCE-CD
                 MOVE "Y" TO HO-VALID-FLAG
                 WRITE KZHOLF-REC
                    INVALID KEY
                       DISPLAY "KZHOLF 登録失敗 ST=" ST-KZHOLF
                       SET ABEND-ON TO TRUE
                    NOT INVALID KEY
                       ADD 1 TO CNT-IMP-WR
                 END-WRITE
              NOT INVALID KEY
                 IF HO-VALID-FLAG = "Y"
                    ADD 1 TO CNT-IMP-PD
                 ELSE
                    MOVE IM-RAW-MARKET-CD TO HO-MARKET-CD
                    MOVE IM-RAW-HOLIDAY-NAME TO HO-HOLIDAY-NAME
                    MOVE IM-RAW-SOURCE-CD TO HO-SOURCE-CD
                    MOVE "Y" TO HO-VALID-FLAG
                    REWRITE KZHOLF-REC
                       INVALID KEY
                          DISPLAY "KZHOLF 更新失敗 ST=" ST-KZHOLF
                          SET ABEND-ON TO TRUE
                       NOT INVALID KEY
                          ADD 1 TO CNT-IMP-WR
                    END-REWRITE
                 END-IF
           END-READ.

       2000-BUILD-CALENDAR.
           MOVE "暦整合" TO WS-STAGE
           DISPLAY "暦整合 開始"
           MOVE WS-FY-START TO WS-CUR-DATE
           PERFORM UNTIL WS-CUR-DATE > WS-FY-END OR ABEND-ON
              PERFORM 2100-WRITE-CALENDAR-DAY
              PERFORM 8100-NEXT-DATE
              MOVE WS-NEXT-DATE TO WS-CUR-DATE
           END-PERFORM
           IF NOT ABEND-ON
              IF CNT-CAL-WR = ZERO
                 DISPLAY "暦整合 出力なし"
                 SET STOP-STAGE TO TRUE
              END-IF
           END-IF
           DISPLAY "暦整合 出力=" CNT-CAL-WR
           DISPLAY "暦整合 休日=" CNT-CAL-HOL.

       2100-WRITE-CALENDAR-DAY.
           MOVE "N" TO SW-HOL-FOUND
           MOVE WS-CUR-DATE TO HO-HOLIDAY-DT
           READ KZHOLF KEY IS HO-HOLIDAY-DT
              INVALID KEY
                 CONTINUE
              NOT INVALID KEY
                 IF HO-VALID-FLAG = "Y"
                    SET HOL-FOUND TO TRUE
                 END-IF
           END-READ
           IF ST-KZHOLF NOT = "00" AND ST-KZHOLF NOT = "23"
              DISPLAY "KZHOLF 読込失敗 ST=" ST-KZHOLF
              SET ABEND-ON TO TRUE
              EXIT PARAGRAPH
           END-IF
           MOVE WS-CUR-DATE TO CA-CAL-DT
           READ KZCALF KEY IS CA-CAL-DT
              INVALID KEY
                 MOVE WS-CUR-DATE TO CA-CAL-DT
                 IF HOL-FOUND
                    MOVE "Y" TO CA-HOLIDAY-FLAG
                 ELSE
                    MOVE "N" TO CA-HOLIDAY-FLAG
                 END-IF
                 WRITE KZCALF-REC
                    INVALID KEY
                       DISPLAY "KZCALF 登録失敗 ST=" ST-KZCALF
                       SET ABEND-ON TO TRUE
                    NOT INVALID KEY
                       ADD 1 TO CNT-CAL-WR
                       IF CA-HOLIDAY-FLAG = "Y"
                          ADD 1 TO CNT-CAL-HOL
                       END-IF
                 END-WRITE
              NOT INVALID KEY
                 IF HOL-FOUND
                    MOVE "Y" TO CA-HOLIDAY-FLAG
                 ELSE
                    MOVE "N" TO CA-HOLIDAY-FLAG
                 END-IF
                 REWRITE KZCALF-REC
                    INVALID KEY
                       DISPLAY "KZCALF 更新失敗 ST=" ST-KZCALF
                       SET ABEND-ON TO TRUE
                    NOT INVALID KEY
                       ADD 1 TO CNT-CAL-WR
                       IF CA-HOLIDAY-FLAG = "Y"
                          ADD 1 TO CNT-CAL-HOL
                       END-IF
                 END-REWRITE
           END-READ.

       3000-BUILD-PERIODS.
           MOVE "会計期間生成" TO WS-STAGE
           DISPLAY "会計期間生成 開始"
           PERFORM VARYING WS-PERIOD-NO FROM 1 BY 1
                   UNTIL WS-PERIOD-NO > 12 OR ABEND-ON
              COMPUTE WS-MON-IDX = WS-PERIOD-NO + 3
              MOVE WS-FISCAL-YYYY TO WS-YYYY
              IF WS-MON-IDX > 12
                 SUBTRACT 12 FROM WS-MON-IDX
                 ADD 1 TO WS-YYYY
              END-IF
              MOVE WS-MON-IDX TO WS-MM
              MOVE 1 TO WS-DD
              COMPUTE WS-PER-START =
                 (WS-YYYY * 10000) + (WS-MM * 100) + WS-DD
              PERFORM 8200-MAX-DAY
              MOVE WS-MAX-DD TO WS-DD
              COMPUTE WS-PER-END =
                 (WS-YYYY * 10000) + (WS-MM * 100) + WS-DD
              STRING WS-FISCAL-YYYY DELIMITED BY SIZE
                     WS-PERIOD-NO DELIMITED BY SIZE
                     INTO WS-PERIOD-ID
              END-STRING
              PERFORM 3100-UPSERT-PERIOD
           END-PERFORM.

       3100-UPSERT-PERIOD.
           MOVE WS-PERIOD-ID TO FP-FISCAL-PERIOD-ID
           READ KZFPRF KEY IS FP-FISCAL-PERIOD-ID
              INVALID KEY
                 MOVE WS-PERIOD-ID TO FP-FISCAL-PERIOD-ID
                 MOVE WS-PER-START TO FP-PERIOD-START-DT
                 MOVE WS-PER-END TO FP-PERIOD-END-DT
                 MOVE "O" TO FP-CLOSE-STATUS
                 MOVE SPACE TO FP-BASE-CYCLE-ID
                 WRITE KZFPRF-REC
                    INVALID KEY
                       DISPLAY "KZFPRF 登録失敗 ST=" ST-KZFPRF
                       SET ABEND-ON TO TRUE
                 END-WRITE
              NOT INVALID KEY
                 IF FP-CLOSE-STATUS NOT = "C"
                    MOVE WS-PER-START TO FP-PERIOD-START-DT
                    MOVE WS-PER-END TO FP-PERIOD-END-DT
                    REWRITE KZFPRF-REC
                       INVALID KEY
                          DISPLAY "KZFPRF 更新失敗 ST="
                                  ST-KZFPRF
                          SET ABEND-ON TO TRUE
                    END-REWRITE
                 END-IF
           END-READ.

       4000-BUILD-CYCLES.
           MOVE "サイクル予定生成" TO WS-STAGE
           DISPLAY "サイクル予定生成 開始"
           PERFORM UNTIL END-CYC OR ABEND-ON
              READ KZCYCF
                 AT END SET END-CYC TO TRUE
                 NOT AT END
                    ADD 1 TO CNT-CYC-IN
                    IF CY-NOMINAL-DT < WS-FY-START
                       OR CY-NOMINAL-DT > WS-FY-END
                       ADD 1 TO CNT-CYC-PD
                    ELSE
                       PERFORM 4100-RESOLVE-CYCLE
                    END-IF
              END-READ
           END-PERFORM
           IF NOT ABEND-ON
              IF CNT-CYC-IN NOT = CNT-CYC-WR + CNT-CYC-PD
                 DISPLAY "サイクル 件数不整合"
                 SET STOP-STAGE TO TRUE
              END-IF
           END-IF
           DISPLAY "サイクル 入力=" CNT-CYC-IN
           DISPLAY "サイクル 出力=" CNT-CYC-WR
           DISPLAY "サイクル 保留=" CNT-CYC-PD.

       4100-RESOLVE-CYCLE.
           MOVE CY-NOMINAL-DT TO LK-CAL-NOMINAL-DT
           MOVE ZERO TO LK-CAL-RESOLVED-DT
           MOVE SPACE TO LK-CAL-ROLLED-FLAG
           MOVE ZERO TO LK-CAL-RET
           CALL "KZ900S" USING LK-CAL-PARM
           IF LK-CAL-RET NOT = ZERO
              DISPLAY "KZ900S 暦解決失敗 RC=" LK-CAL-RET
              SET ABEND-ON TO TRUE
              EXIT PARAGRAPH
           END-IF
           MOVE CY-CYCLE-ID TO CR-CYCLE-ID
           MOVE CY-NOMINAL-DT TO CR-NOMINAL-DT
           MOVE LK-CAL-RESOLVED-DT TO CR-RESOLVED-DT
           MOVE LK-CAL-ROLLED-FLAG TO CR-ROLLED-FLAG
           WRITE KZCYRF-REC
           IF ST-KZCYRF NOT = "00"
              DISPLAY "KZCYRF 出力失敗 ST=" ST-KZCYRF
              SET ABEND-ON TO TRUE
           ELSE
              ADD 1 TO CNT-CYC-WR
              MOVE CY-CYCLE-ID TO HOLD-CYCLE-ID
              MOVE CY-NOMINAL-DT TO HOLD-NOMINAL-DT
           END-IF.

       5000-EXPAND-JOBNET.
           MOVE "ジョブネット暦展開" TO WS-STAGE
           DISPLAY "ジョブネット暦展開 開始"
           MOVE SPACE TO JN-JOBNET-ID
           START KZJNCF KEY IS NOT LESS THAN JN-JOBNET-ID
              INVALID KEY
                 DISPLAY "KZJNCF 有効定義なし"
                 SET STOP-STAGE TO TRUE
           END-START
           IF STOP-STAGE OR ABEND-ON
              EXIT PARAGRAPH
           END-IF
           PERFORM UNTIL END-JN OR ABEND-ON
              READ KZJNCF NEXT
                 AT END SET END-JN TO TRUE
                 NOT AT END
                    ADD 1 TO CNT-JN-IN
                    IF JN-ACTIVE-FLAG NOT = "Y"
                       ADD 1 TO CNT-JN-EX
                    ELSE
                       PERFORM 5100-MAKE-SCHEDULE
                    END-IF
              END-READ
           END-PERFORM
           IF NOT ABEND-ON
              IF CNT-JN-IN = CNT-JN-EX
                 DISPLAY "ジョブネット 有効定義なし"
                 SET STOP-STAGE TO TRUE
              END-IF
           END-IF
           DISPLAY "ジョブネット 入力=" CNT-JN-IN
           DISPLAY "ジョブネット 除外=" CNT-JN-EX
           DISPLAY "予定 出力=" CNT-SCH-WR.

       5100-MAKE-SCHEDULE.
           IF HOLD-CYCLE-ID = SPACE
              DISPLAY "予定生成 基準サイクル未生成"
              SET STOP-STAGE TO TRUE
              EXIT PARAGRAPH
           END-IF
           MOVE HOLD-NOMINAL-DT TO WS-CYCLE-DATE
           PERFORM 5200-FIND-PERIOD
           IF NOT PER-FOUND
              DISPLAY "予定生成 会計期間未決定 日付="
                      WS-CYCLE-DATE
              SET STOP-STAGE TO TRUE
              EXIT PARAGRAPH
           END-IF
           MOVE "N" TO SW-CAL-FOUND
           MOVE WS-CYCLE-DATE TO CA-CAL-DT
           READ KZCALF KEY IS CA-CAL-DT
              INVALID KEY
                 CONTINUE
              NOT INVALID KEY
                 SET CAL-FOUND TO TRUE
           END-READ
           IF NOT CAL-FOUND
              DISPLAY "予定生成 暦未登録 日付=" WS-CYCLE-DATE
              SET STOP-STAGE TO TRUE
              EXIT PARAGRAPH
           END-IF
           IF JN-SUPPRESS-HOLIDAY-FLAG = "Y"
              AND CA-HOLIDAY-FLAG = "Y"
              ADD 1 TO CNT-JN-EX
              EXIT PARAGRAPH
           END-IF
           MOVE HOLD-CYCLE-ID TO SC-CYCLE-ID
           MOVE WS-CYCLE-DATE TO SC-SCHEDULED-DT
           MOVE JN-JOBNET-ID TO SC-JOBNET-ID
           MOVE WS-PERIOD-ID TO SC-PERIOD-ID
           MOVE JN-CALENDAR-CD TO SC-CALENDAR-CD
           MOVE "N" TO SC-GENERATE-STATUS
           WRITE KZSCHF-REC
           IF ST-KZSCHF NOT = "00"
              DISPLAY "KZSCHF 出力失敗 ST=" ST-KZSCHF
              SET ABEND-ON TO TRUE
           ELSE
              ADD 1 TO CNT-SCH-WR
              MOVE JN-JOBNET-ID TO HOLD-JOBNET-ID
              MOVE JN-CALENDAR-CD TO HOLD-CALENDAR-CD
           END-IF.

       5200-FIND-PERIOD.
           MOVE "N" TO SW-PER-FOUND
           PERFORM VARYING WS-PERIOD-NO FROM 1 BY 1
                   UNTIL WS-PERIOD-NO > 12 OR PER-FOUND
              COMPUTE WS-MON-IDX = WS-PERIOD-NO + 3
              MOVE WS-FISCAL-YYYY TO WS-YYYY
              IF WS-MON-IDX > 12
                 SUBTRACT 12 FROM WS-MON-IDX
                 ADD 1 TO WS-YYYY
              END-IF
              MOVE WS-MON-IDX TO WS-MM
              MOVE 1 TO WS-DD
              COMPUTE WS-PER-START =
                 (WS-YYYY * 10000) + (WS-MM * 100) + WS-DD
              PERFORM 8200-MAX-DAY
              MOVE WS-MAX-DD TO WS-DD
              COMPUTE WS-PER-END =
                 (WS-YYYY * 10000) + (WS-MM * 100) + WS-DD
              IF WS-CYCLE-DATE >= WS-PER-START
                 AND WS-CYCLE-DATE <= WS-PER-END
                 STRING WS-FISCAL-YYYY DELIMITED BY SIZE
                        WS-PERIOD-NO DELIMITED BY SIZE
                        INTO WS-PERIOD-ID
                 END-STRING
                 SET PER-FOUND TO TRUE
              END-IF
           END-PERFORM.

       6000-APPLY-REPROCESS.
           MOVE "再処理反映" TO WS-STAGE
           DISPLAY "再処理反映 開始"
           MOVE SPACE TO RP-REPROCESS-ID
           START KZRPRF KEY IS NOT LESS THAN RP-REPROCESS-ID
              INVALID KEY
                 DISPLAY "再処理 指示なし"
                 EXIT PARAGRAPH
           END-START
           PERFORM UNTIL END-RP OR ABEND-ON
              READ KZRPRF NEXT
                 AT END SET END-RP TO TRUE
                 NOT AT END
                    ADD 1 TO CNT-RP-IN
                    IF RP-REQUEST-STATUS = "A"
                       PERFORM 6100-REPROCESS-ONE
                    ELSE
                       IF RP-REQUEST-STATUS = "P"
                          ADD 1 TO CNT-RP-PD
                       ELSE
                          ADD 1 TO CNT-RP-EX
                       END-IF
                    END-IF
              END-READ
           END-PERFORM
           IF NOT ABEND-ON
              IF CNT-RP-IN NOT =
                 CNT-RP-WR + CNT-RP-EX + CNT-RP-PD
                 DISPLAY "再処理 件数不整合"
                 SET STOP-STAGE TO TRUE
              END-IF
           END-IF
           DISPLAY "再処理 入力=" CNT-RP-IN
           DISPLAY "再処理 反映=" CNT-RP-WR
           DISPLAY "再処理 除外=" CNT-RP-EX
           DISPLAY "再処理 保留=" CNT-RP-PD.

       6100-REPROCESS-ONE.
           IF RP-APPROVER-ID = SPACE
              ADD 1 TO CNT-RP-PD
              EXIT PARAGRAPH
           END-IF
           IF RP-REDATE-DT < WS-FY-START
              OR RP-REDATE-DT > WS-FY-END
              ADD 1 TO CNT-RP-EX
              EXIT PARAGRAPH
           END-IF
           MOVE "N" TO SW-RP-DUP
           IF RP-CYCLE-ID = HOLD-CYCLE-ID
              AND RP-REDATE-DT = HOLD-NOMINAL-DT
              SET RP-DUP TO TRUE
           END-IF
           IF RP-DUP
              MOVE "D" TO RP-REQUEST-STATUS
              REWRITE KZRPRF-REC
                 INVALID KEY
                    DISPLAY "KZRPRF 二重更新失敗 ST="
                            ST-KZRPRF
                    SET ABEND-ON TO TRUE
                 NOT INVALID KEY
                    ADD 1 TO CNT-RP-EX
              END-REWRITE
              EXIT PARAGRAPH
           END-IF
           MOVE RP-CYCLE-ID TO SC-CYCLE-ID
           MOVE RP-REDATE-DT TO SC-SCHEDULED-DT
           IF HOLD-JOBNET-ID = SPACE
              MOVE "REPROCESS" TO SC-JOBNET-ID
           ELSE
              MOVE HOLD-JOBNET-ID TO SC-JOBNET-ID
           END-IF
           PERFORM 6200-FIND-REDATE-PERIOD
           MOVE WS-PERIOD-ID TO SC-PERIOD-ID
           IF HOLD-CALENDAR-CD = SPACE
              MOVE "JPBANK" TO SC-CALENDAR-CD
           ELSE
              MOVE HOLD-CALENDAR-CD TO SC-CALENDAR-CD
           END-IF
           MOVE "R" TO SC-GENERATE-STATUS
           WRITE KZSCHF-REC
           IF ST-KZSCHF NOT = "00"
              DISPLAY "KZSCHF 再処理出力失敗 ST=" ST-KZSCHF
              SET ABEND-ON TO TRUE
           ELSE
              MOVE "C" TO RP-REQUEST-STATUS
              REWRITE KZRPRF-REC
                 INVALID KEY
                    DISPLAY "KZRPRF 反映更新失敗 ST="
                            ST-KZRPRF
                    SET ABEND-ON TO TRUE
                 NOT INVALID KEY
                    ADD 1 TO CNT-RP-WR
                    ADD 1 TO CNT-SCH-WR
              END-REWRITE
           END-IF.

       6200-FIND-REDATE-PERIOD.
           MOVE RP-REDATE-DT TO WS-CYCLE-DATE
           PERFORM 5200-FIND-PERIOD
           IF NOT PER-FOUND
              MOVE "9999999999" TO WS-PERIOD-ID
           END-IF.

       7000-WRITE-AGING.
           MOVE "経過バケット出力" TO WS-STAGE
           DISPLAY "経過バケット出力 開始"
           MOVE WS-FY-START TO WS-BASE-DT
           MOVE WS-FY-END TO WS-TARGET-DT
           PERFORM 7100-WRITE-AGING-ONE
           MOVE WS-FY-START TO WS-BASE-DT
           MOVE WS-TODAY TO WS-TARGET-DT
           IF WS-TARGET-DT >= WS-FY-START
              AND WS-TARGET-DT <= WS-FY-END
              PERFORM 7100-WRITE-AGING-ONE
           END-IF
           IF HOLD-NOMINAL-DT NOT = ZERO
              MOVE WS-FY-START TO WS-BASE-DT
              MOVE HOLD-NOMINAL-DT TO WS-TARGET-DT
              PERFORM 7100-WRITE-AGING-ONE
           END-IF
           DISPLAY "経過バケット 出力=" CNT-AGE-WR.

       7100-WRITE-AGING-ONE.
           PERFORM 8300-COUNT-DAYS
           IF ABEND-ON
              EXIT PARAGRAPH
           END-IF
           ADD 1 TO WS-AGING-SEQ
           MOVE WS-AGING-SEQ TO AG-AGING-KEY
           MOVE WS-BASE-DT TO AG-BASE-DT
           MOVE WS-TARGET-DT TO AG-TARGET-DT
           MOVE WS-DAY-COUNT TO AG-ELAPSED-DAYS
           MOVE WS-BUS-COUNT TO AG-BUSINESS-DAYS
           IF WS-BUS-COUNT <= 5
              MOVE "A" TO AG-AGING-BUCKET
           ELSE
              IF WS-BUS-COUNT <= 20
                 MOVE "B" TO AG-AGING-BUCKET
              ELSE
                 IF WS-BUS-COUNT <= 60
                    MOVE "C" TO AG-AGING-BUCKET
                 ELSE
                    MOVE "D" TO AG-AGING-BUCKET
                 END-IF
              END-IF
           END-IF
           WRITE KZAGEF-REC
           IF ST-KZAGEF NOT = "00"
              DISPLAY "KZAGEF 出力失敗 ST=" ST-KZAGEF
              SET ABEND-ON TO TRUE
           ELSE
              ADD 1 TO CNT-AGE-WR
           END-IF.

       8000-CHECK-DATE.
           MOVE "N" TO WS-DATE-OK
           MOVE WS-CUR-DATE(1:4) TO WS-YYYY
           MOVE WS-CUR-DATE(5:2) TO WS-MM
           MOVE WS-CUR-DATE(7:2) TO WS-DD
           IF WS-MM < 1 OR WS-MM > 12
              EXIT PARAGRAPH
           END-IF
           PERFORM 8200-MAX-DAY
           IF WS-DD >= 1 AND WS-DD <= WS-MAX-DD
              MOVE "Y" TO WS-DATE-OK
           END-IF.

       8100-NEXT-DATE.
           MOVE WS-CUR-DATE(1:4) TO WS-YYYY
           MOVE WS-CUR-DATE(5:2) TO WS-MM
           MOVE WS-CUR-DATE(7:2) TO WS-DD
           PERFORM 8200-MAX-DAY
           ADD 1 TO WS-DD
           IF WS-DD > WS-MAX-DD
              MOVE 1 TO WS-DD
              ADD 1 TO WS-MM
              IF WS-MM > 12
                 MOVE 1 TO WS-MM
                 ADD 1 TO WS-YYYY
              END-IF
           END-IF
           COMPUTE WS-NEXT-DATE =
              (WS-YYYY * 10000) + (WS-MM * 100) + WS-DD.

       8200-MAX-DAY.
           EVALUATE WS-MM
              WHEN 1
              WHEN 3
              WHEN 5
              WHEN 7
              WHEN 8
              WHEN 10
              WHEN 12
                 MOVE 31 TO WS-MAX-DD
              WHEN 4
              WHEN 6
              WHEN 9
              WHEN 11
                 MOVE 30 TO WS-MAX-DD
              WHEN 2
                 IF FUNCTION MOD(WS-YYYY 400) = 0
                    MOVE 29 TO WS-MAX-DD
                 ELSE
                    IF FUNCTION MOD(WS-YYYY 100) = 0
                       MOVE 28 TO WS-MAX-DD
                    ELSE
                       IF FUNCTION MOD(WS-YYYY 4) = 0
                          MOVE 29 TO WS-MAX-DD
                       ELSE
                          MOVE 28 TO WS-MAX-DD
                       END-IF
                    END-IF
                 END-IF
              WHEN OTHER
                 MOVE 0 TO WS-MAX-DD
           END-EVALUATE.

       8300-COUNT-DAYS.
           MOVE ZERO TO WS-DAY-COUNT
           MOVE ZERO TO WS-BUS-COUNT
           MOVE WS-BASE-DT TO WS-CUR-DATE
           PERFORM UNTIL WS-CUR-DATE > WS-TARGET-DT OR ABEND-ON
              ADD 1 TO WS-DAY-COUNT
              MOVE WS-CUR-DATE TO CA-CAL-DT
              READ KZCALF KEY IS CA-CAL-DT
                 INVALID KEY
                    DISPLAY "KZCALF 経過計算未登録 日付="
                            WS-CUR-DATE
                    SET ABEND-ON TO TRUE
                 NOT INVALID KEY
                    IF CA-HOLIDAY-FLAG = "N"
                       ADD 1 TO WS-BUS-COUNT
                    END-IF
              END-READ
              IF NOT ABEND-ON
                 PERFORM 8100-NEXT-DATE
                 MOVE WS-NEXT-DATE TO WS-CUR-DATE
              END-IF
           END-PERFORM.

       9000-CLOSE-FILES.
           CLOSE KZIMPF KZHOLF KZCALF KZCYCF KZFPRF
                 KZJNCF KZRPRF KZSCHF KZCYRF KZAGEF
           IF ST-KZIMPF NOT = "00"
              DISPLAY "KZIMPF クローズ失敗 ST=" ST-KZIMPF
              SET ABEND-ON TO TRUE
           END-IF
           IF ST-KZHOLF NOT = "00"
              DISPLAY "KZHOLF クローズ失敗 ST=" ST-KZHOLF
              SET ABEND-ON TO TRUE
           END-IF
           IF ST-KZCALF NOT = "00"
              DISPLAY "KZCALF クローズ失敗 ST=" ST-KZCALF
              SET ABEND-ON TO TRUE
           END-IF
           IF ST-KZCYCF NOT = "00"
              DISPLAY "KZCYCF クローズ失敗 ST=" ST-KZCYCF
              SET ABEND-ON TO TRUE
           END-IF
           IF ST-KZFPRF NOT = "00"
              DISPLAY "KZFPRF クローズ失敗 ST=" ST-KZFPRF
              SET ABEND-ON TO TRUE
           END-IF
           IF ST-KZJNCF NOT = "00"
              DISPLAY "KZJNCF クローズ失敗 ST=" ST-KZJNCF
              SET ABEND-ON TO TRUE
           END-IF
           IF ST-KZRPRF NOT = "00"
              DISPLAY "KZRPRF クローズ失敗 ST=" ST-KZRPRF
              SET ABEND-ON TO TRUE
           END-IF
           IF ST-KZSCHF NOT = "00"
              DISPLAY "KZSCHF クローズ失敗 ST=" ST-KZSCHF
              SET ABEND-ON TO TRUE
           END-IF
           IF ST-KZCYRF NOT = "00"
              DISPLAY "KZCYRF クローズ失敗 ST=" ST-KZCYRF
              SET ABEND-ON TO TRUE
           END-IF
           IF ST-KZAGEF NOT = "00"
              DISPLAY "KZAGEF クローズ失敗 ST=" ST-KZAGEF
              SET ABEND-ON TO TRUE
           END-IF.
