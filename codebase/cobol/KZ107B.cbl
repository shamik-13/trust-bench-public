       IDENTIFICATION DIVISION.
       PROGRAM-ID. KZ107B.
      * 変更履歴
      * 版数  年月日(和暦)   担当                       概要
      * 1.00  令和03.03.31  システム部 勘定系チーム   新規作成
      * 1.01  令和04.09.30  システム部 勘定系チーム   期末締め判定を修正
      * 1.02  令和05.03.31  システム部 勘定系チーム   再実行時制御を追加
       AUTHOR. KZ-SYSTEM.
      * 期末締め制御バッチ。
      * 対象期間の予定、解決日、再処理状態を確認し締め状態を更新する。

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZFPRF
               ASSIGN TO "KZFPRF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS FP-FISCAL-PERIOD-ID
               FILE STATUS IS WS-FS-KZFPRF.

           SELECT KZSCHF
               ASSIGN TO "KZSCHF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-KZSCHF.

           SELECT KZCYRF
               ASSIGN TO "KZCYRF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-KZCYRF.

       DATA DIVISION.
       FILE SECTION.
       FD  KZFPRF.
           COPY KZFPRFC.

       FD  KZSCHF.
           COPY KZSCHFC.

       FD  KZCYRF.
           COPY KZCYRFC.

       WORKING-STORAGE SECTION.
           COPY LK-CAL-PARM.

       01  WS-FILE-STATUS.
           05 WS-FS-KZFPRF              PIC XX VALUE SPACES.
           05 WS-FS-KZSCHF              PIC XX VALUE SPACES.
           05 WS-FS-KZCYRF              PIC XX VALUE SPACES.

       01  WS-SWITCHES.
           05 WS-END-KZFPRF             PIC X VALUE "N".
              88 END-KZFPRF                  VALUE "Y".
           05 WS-END-KZSCHF             PIC X VALUE "N".
              88 END-KZSCHF                  VALUE "Y".
           05 WS-END-KZCYRF             PIC X VALUE "N".
              88 END-KZCYRF                  VALUE "Y".
           05 WS-HARD-ERROR             PIC X VALUE "N".
              88 HARD-ERROR                  VALUE "Y".
           05 WS-PERIOD-UPDATED         PIC X VALUE "N".
              88 PERIOD-UPDATED              VALUE "Y".

       01  WS-DATE-AREA.
           05 WS-CURRENT-DATE-RAW       PIC X(21).
           05 WS-TODAY                  PIC 9(08).
           05 WS-PERIOD-START           PIC 9(08).
           05 WS-PERIOD-END             PIC 9(08).
           05 WS-WORK-DATE              PIC 9(08).
           05 WS-WORK-INT               PIC S9(09) COMP.
           05 WS-END-INT                PIC S9(09) COMP.

       01  WS-COUNTERS.
           05 WS-PERIOD-READ-CNT        PIC 9(09) VALUE ZERO.
           05 WS-PERIOD-UPD-CNT         PIC 9(09) VALUE ZERO.
           05 WS-PERIOD-HOLD-CNT        PIC 9(09) VALUE ZERO.
           05 WS-SCH-READ-CNT           PIC 9(09) VALUE ZERO.
           05 WS-CYR-READ-CNT           PIC 9(09) VALUE ZERO.
           05 WS-SCH-IN-PERIOD-CNT      PIC 9(07) VALUE ZERO.
           05 WS-CYR-IN-PERIOD-CNT      PIC 9(07) VALUE ZERO.
           05 WS-BUSINESS-DAYS          PIC 9(05) VALUE ZERO.
           05 WS-ERR-CNT                PIC 9(05) VALUE ZERO.

       01  WS-DECISION.
           05 WS-NOT-ARRIVED-CNT        PIC 9(05) VALUE ZERO.
           05 WS-UNRESOLVED-CNT         PIC 9(05) VALUE ZERO.
           05 WS-REPROCESS-CNT          PIC 9(05) VALUE ZERO.
           05 WS-DATE-ERROR-CNT         PIC 9(05) VALUE ZERO.
           05 WS-CALENDAR-ERROR-CNT     PIC 9(05) VALUE ZERO.
           05 WS-NEW-CLOSE-STATUS       PIC X(01) VALUE SPACE.
           05 WS-REASON-TEXT            PIC X(40) VALUE SPACES.

       01  WS-DISPLAY-AREA.
           05 WS-DISP-PERIOD            PIC X(20) VALUE SPACES.
           05 WS-DISP-ST                PIC XX VALUE SPACES.

       PROCEDURE DIVISION.
       MAIN-SECTION.
           PERFORM 1000-INIT
           IF NOT HARD-ERROR
              PERFORM 2000-PROCESS
           END-IF
           PERFORM 9000-END
           GOBACK.

       1000-INIT.
           MOVE 0 TO RETURN-CODE
           ACCEPT WS-CURRENT-DATE-RAW FROM DATE YYYYMMDD
           MOVE WS-CURRENT-DATE-RAW(1:8) TO WS-TODAY

           OPEN I-O KZFPRF
           IF WS-FS-KZFPRF NOT = "00"
              DISPLAY "KZFPRF オープン失敗 ST=" WS-FS-KZFPRF
              SET HARD-ERROR TO TRUE
              MOVE 12 TO RETURN-CODE
           END-IF.

       2000-PROCESS.
           PERFORM UNTIL END-KZFPRF OR HARD-ERROR
              READ KZFPRF NEXT RECORD
                 AT END
                    SET END-KZFPRF TO TRUE
                 NOT AT END
                    ADD 1 TO WS-PERIOD-READ-CNT
                    PERFORM 2100-CHECK-PERIOD
                    IF NOT HARD-ERROR
                       PERFORM 2600-UPDATE-PERIOD
                    END-IF
              END-READ
           END-PERFORM.

       2100-CHECK-PERIOD.
           MOVE ZERO TO WS-NOT-ARRIVED-CNT
                        WS-UNRESOLVED-CNT
                        WS-REPROCESS-CNT
                        WS-DATE-ERROR-CNT
                        WS-CALENDAR-ERROR-CNT
                        WS-SCH-IN-PERIOD-CNT
                        WS-CYR-IN-PERIOD-CNT
                        WS-BUSINESS-DAYS
           MOVE "N" TO WS-PERIOD-UPDATED
           MOVE SPACES TO WS-REASON-TEXT
           MOVE FP-FISCAL-PERIOD-ID TO WS-DISP-PERIOD

           IF FP-PERIOD-START-DT IS NUMERIC
              AND FP-PERIOD-END-DT IS NUMERIC
              MOVE FP-PERIOD-START-DT TO WS-PERIOD-START
              MOVE FP-PERIOD-END-DT   TO WS-PERIOD-END
              IF WS-PERIOD-START > WS-PERIOD-END
                 ADD 1 TO WS-DATE-ERROR-CNT
                 MOVE "期間開始終了逆転" TO WS-REASON-TEXT
              END-IF
           ELSE
              ADD 1 TO WS-DATE-ERROR-CNT
              MOVE "期間日付形式不正" TO WS-REASON-TEXT
           END-IF

           IF WS-DATE-ERROR-CNT = ZERO
              PERFORM 2200-SCAN-SCHEDULE
              IF NOT HARD-ERROR
                 PERFORM 2300-SCAN-CYCLE
              END-IF
              IF NOT HARD-ERROR
                 PERFORM 2400-COUNT-BUSINESS-DAYS
              END-IF
           END-IF

           IF WS-DATE-ERROR-CNT > ZERO
              ADD 1 TO WS-ERR-CNT
           END-IF.

       2200-SCAN-SCHEDULE.
           MOVE "N" TO WS-END-KZSCHF
           OPEN INPUT KZSCHF
           IF WS-FS-KZSCHF NOT = "00"
              DISPLAY "KZSCHF オープン失敗 ST=" WS-FS-KZSCHF
              SET HARD-ERROR TO TRUE
              MOVE 12 TO RETURN-CODE
           END-IF

           PERFORM UNTIL END-KZSCHF OR HARD-ERROR
              READ KZSCHF
                 AT END
                    SET END-KZSCHF TO TRUE
                 NOT AT END
                    ADD 1 TO WS-SCH-READ-CNT
                    IF SC-PERIOD-ID = FP-FISCAL-PERIOD-ID
                       ADD 1 TO WS-SCH-IN-PERIOD-CNT
                       PERFORM 2210-CHECK-SCHEDULE
                    END-IF
              END-READ
           END-PERFORM

           CLOSE KZSCHF
           IF WS-FS-KZSCHF NOT = "00"
              DISPLAY "KZSCHF クローズ失敗 ST=" WS-FS-KZSCHF
              SET HARD-ERROR TO TRUE
              MOVE 12 TO RETURN-CODE
           END-IF.

       2210-CHECK-SCHEDULE.
           IF SC-SCHEDULED-DT IS NUMERIC
              MOVE SC-SCHEDULED-DT TO WS-WORK-DATE
              IF WS-WORK-DATE < WS-PERIOD-START
                 OR WS-WORK-DATE > WS-PERIOD-END
                 ADD 1 TO WS-DATE-ERROR-CNT
                 MOVE "予定日が期間外" TO WS-REASON-TEXT
              END-IF
              IF WS-WORK-DATE > WS-TODAY
                 ADD 1 TO WS-NOT-ARRIVED-CNT
              END-IF
           ELSE
              ADD 1 TO WS-DATE-ERROR-CNT
              MOVE "予定日形式不正" TO WS-REASON-TEXT
           END-IF

           IF SC-GENERATE-STATUS NOT = "C"
              ADD 1 TO WS-UNRESOLVED-CNT
           END-IF.

       2300-SCAN-CYCLE.
           MOVE "N" TO WS-END-KZCYRF
           OPEN INPUT KZCYRF
           IF WS-FS-KZCYRF NOT = "00"
              DISPLAY "KZCYRF オープン失敗 ST=" WS-FS-KZCYRF
              SET HARD-ERROR TO TRUE
              MOVE 12 TO RETURN-CODE
           END-IF

           PERFORM UNTIL END-KZCYRF OR HARD-ERROR
              READ KZCYRF
                 AT END
                    SET END-KZCYRF TO TRUE
                 NOT AT END
                    ADD 1 TO WS-CYR-READ-CNT
                    PERFORM 2310-CHECK-CYCLE-RANGE
              END-READ
           END-PERFORM

           CLOSE KZCYRF
           IF WS-FS-KZCYRF NOT = "00"
              DISPLAY "KZCYRF クローズ失敗 ST=" WS-FS-KZCYRF
              SET HARD-ERROR TO TRUE
              MOVE 12 TO RETURN-CODE
           END-IF.

       2310-CHECK-CYCLE-RANGE.
           IF CR-NOMINAL-DT IS NUMERIC
              MOVE CR-NOMINAL-DT TO WS-WORK-DATE
              IF WS-WORK-DATE >= WS-PERIOD-START
                 AND WS-WORK-DATE <= WS-PERIOD-END
                 ADD 1 TO WS-CYR-IN-PERIOD-CNT
                 PERFORM 2320-CHECK-CYCLE-RESOLVE
              END-IF
           ELSE
              ADD 1 TO WS-DATE-ERROR-CNT
              MOVE "サイクル基準日形式不正" TO WS-REASON-TEXT
           END-IF.

       2320-CHECK-CYCLE-RESOLVE.
           IF CR-RESOLVED-DT IS NOT NUMERIC
              OR CR-RESOLVED-DT = ZERO
              ADD 1 TO WS-UNRESOLVED-CNT
              MOVE "サイクル未解決" TO WS-REASON-TEXT
           ELSE
              MOVE CR-NOMINAL-DT   TO LK-CAL-NOMINAL-DT
              MOVE ZERO            TO LK-CAL-RESOLVED-DT
              MOVE SPACE           TO LK-CAL-ROLLED-FLAG
              MOVE ZERO            TO LK-CAL-RET
              CALL "KZ900S" USING LK-CAL-PARM
              IF LK-CAL-RET NOT = ZERO
                 ADD 1 TO WS-CALENDAR-ERROR-CNT
                 MOVE "暦解決呼出異常" TO WS-REASON-TEXT
              ELSE
                 IF LK-CAL-RESOLVED-DT NOT = CR-RESOLVED-DT
                    ADD 1 TO WS-UNRESOLVED-CNT
                    MOVE "解決日不一致" TO WS-REASON-TEXT
                 END-IF
              END-IF
           END-IF

           IF CR-ROLLED-FLAG = "R" OR CR-ROLLED-FLAG = "P"
              ADD 1 TO WS-REPROCESS-CNT
              MOVE "再処理保留あり" TO WS-REASON-TEXT
           END-IF.

       2400-COUNT-BUSINESS-DAYS.
           MOVE FUNCTION INTEGER-OF-DATE(WS-PERIOD-START) TO WS-WORK-INT
           MOVE FUNCTION INTEGER-OF-DATE(WS-PERIOD-END)   TO WS-END-INT

           PERFORM UNTIL WS-WORK-INT > WS-END-INT
              MOVE FUNCTION DATE-OF-INTEGER(WS-WORK-INT)
                TO WS-WORK-DATE
              MOVE WS-WORK-DATE TO LK-CAL-NOMINAL-DT
              MOVE ZERO         TO LK-CAL-RESOLVED-DT
              MOVE SPACE        TO LK-CAL-ROLLED-FLAG
              MOVE ZERO         TO LK-CAL-RET
              CALL "KZ900S" USING LK-CAL-PARM
              IF LK-CAL-RET NOT = ZERO
                 ADD 1 TO WS-CALENDAR-ERROR-CNT
                 MOVE "営業日算出異常" TO WS-REASON-TEXT
              ELSE
                 IF LK-CAL-RESOLVED-DT = WS-WORK-DATE
                    AND LK-CAL-ROLLED-FLAG NOT = "Y"
                    ADD 1 TO WS-BUSINESS-DAYS
                 END-IF
              END-IF
              ADD 1 TO WS-WORK-INT
           END-PERFORM.

       2600-UPDATE-PERIOD.
           IF HARD-ERROR
              EXIT PARAGRAPH
           END-IF

           EVALUATE TRUE
              WHEN WS-DATE-ERROR-CNT > ZERO
                 MOVE "E" TO WS-NEW-CLOSE-STATUS
              WHEN WS-CALENDAR-ERROR-CNT > ZERO
                 MOVE "E" TO WS-NEW-CLOSE-STATUS
              WHEN WS-NOT-ARRIVED-CNT > ZERO
                 MOVE "H" TO WS-NEW-CLOSE-STATUS
                 MOVE "未到来日あり" TO WS-REASON-TEXT
              WHEN WS-UNRESOLVED-CNT > ZERO
                 MOVE "H" TO WS-NEW-CLOSE-STATUS
                 MOVE "未解決サイクルあり" TO WS-REASON-TEXT
              WHEN WS-REPROCESS-CNT > ZERO
                 MOVE "H" TO WS-NEW-CLOSE-STATUS
                 MOVE "再処理保留あり" TO WS-REASON-TEXT
              WHEN WS-SCH-IN-PERIOD-CNT = ZERO
                 MOVE "H" TO WS-NEW-CLOSE-STATUS
                 MOVE "対象予定なし" TO WS-REASON-TEXT
              WHEN WS-CYR-IN-PERIOD-CNT = ZERO
                 MOVE "H" TO WS-NEW-CLOSE-STATUS
                 MOVE "対象サイクルなし" TO WS-REASON-TEXT
              WHEN OTHER
                 MOVE "R" TO WS-NEW-CLOSE-STATUS
                 MOVE "締め可能" TO WS-REASON-TEXT
           END-EVALUATE

           IF FP-CLOSE-STATUS NOT = WS-NEW-CLOSE-STATUS
              MOVE WS-NEW-CLOSE-STATUS TO FP-CLOSE-STATUS
              REWRITE KZFPRF-REC
              IF WS-FS-KZFPRF = "00"
                 SET PERIOD-UPDATED TO TRUE
                 ADD 1 TO WS-PERIOD-UPD-CNT
              ELSE
                 DISPLAY "KZFPRF 更新失敗 期間=" WS-DISP-PERIOD
                         " ST=" WS-FS-KZFPRF
                 SET HARD-ERROR TO TRUE
                 MOVE 12 TO RETURN-CODE
              END-IF
           END-IF

           IF WS-NEW-CLOSE-STATUS = "H"
              ADD 1 TO WS-PERIOD-HOLD-CNT
           END-IF

           IF NOT HARD-ERROR
              DISPLAY "締め判定 期間=" WS-DISP-PERIOD
                      " 状態=" WS-NEW-CLOSE-STATUS
                      " 営業日=" WS-BUSINESS-DAYS
                      " 理由=" WS-REASON-TEXT
           END-IF.

       9000-END.
           IF WS-FS-KZFPRF = "00"
              CLOSE KZFPRF
              IF WS-FS-KZFPRF NOT = "00"
                 DISPLAY "KZFPRF クローズ失敗 ST=" WS-FS-KZFPRF
                 MOVE 12 TO RETURN-CODE
              END-IF
           END-IF

           IF RETURN-CODE = 0
              DISPLAY "KZ160B 正常終了"
                      " 読込=" WS-PERIOD-READ-CNT
                      " 更新=" WS-PERIOD-UPD-CNT
                      " 保留=" WS-PERIOD-HOLD-CNT
           ELSE
              DISPLAY "KZ160B 異常終了 RC=" RETURN-CODE
                      " 読込=" WS-PERIOD-READ-CNT
                      " 更新=" WS-PERIOD-UPD-CNT
                      " エラー=" WS-ERR-CNT
           END-IF.
