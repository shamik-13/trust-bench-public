       IDENTIFICATION DIVISION.
       PROGRAM-ID. KZ117B.
      ******************************************************************
      * 変更履歴
      * 版数  年月日    担当      概要
      * 1.00  20240401  KZB001    初版作成
      * 1.01  20240918  KZB002    休日原簿照合を追加
      * 1.02  20250210  KZB003    連休前後候補の出力単位を整理
      ******************************************************************
      * 営業日長休判定バッチ
      * 指定範囲の暦日を走査し、休日連続区間の前後営業日候補を
      * KZCYRFへ記録する。調整方向は本処理で固定しない。
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZCALF ASSIGN TO "KZCALF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CA-CAL-DT
               FILE STATUS IS FS-KZCALF.
           SELECT KZHOLF ASSIGN TO "KZHOLF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS HO-HOLIDAY-DT
               FILE STATUS IS FS-KZHOLF.
           SELECT KZCYRF ASSIGN TO "KZCYRF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-KZCYRF.

       DATA DIVISION.
       FILE SECTION.
       FD  KZCALF.
           COPY KZCALFC.
       FD  KZHOLF.
           COPY KZHOLFC.
       FD  KZCYRF.
           COPY KZCYRFC.

       WORKING-STORAGE SECTION.
       01  FS-KZCALF                  PIC XX VALUE SPACE.
       01  FS-KZHOLF                  PIC XX VALUE SPACE.
       01  FS-KZCYRF                  PIC XX VALUE SPACE.

       01  SW-END                     PIC X VALUE "N".
           88  END-REQUEST                  VALUE "Y".
       01  SW-IN-RUN                  PIC X VALUE "N".
           88  IN-HOLIDAY-RUN              VALUE "Y".
           88  OUT-HOLIDAY-RUN             VALUE "N".
       01  SW-HOLIDAY                 PIC X VALUE "N".
           88  TARGET-HOLIDAY              VALUE "Y".
           88  TARGET-BUSINESS             VALUE "N".

       01  WS-RANGE.
           05  WS-FROM-DT             PIC 9(8).
           05  WS-TO-DT               PIC 9(8).
           05  WS-CURR-DT             PIC 9(8).
           05  WS-RUN-FROM-DT         PIC 9(8).
           05  WS-RUN-TO-DT           PIC 9(8).
           05  WS-PREV-BIZ-DT         PIC 9(8).
           05  WS-NEXT-BIZ-DT         PIC 9(8).
           05  WS-NOMINAL-DT          PIC 9(8).

       01  WS-INTEGER-DATE.
           05  WS-FROM-INT            PIC S9(9) COMP-5.
           05  WS-TO-INT              PIC S9(9) COMP-5.
           05  WS-CURR-INT            PIC S9(9) COMP-5.
           05  WS-SAVE-CURR-INT       PIC S9(9) COMP-5.
           05  WS-WORK-INT            PIC S9(9) COMP-5.
           05  WS-WEEK-REM            PIC S9(9) COMP-5.

       01  WS-COUNT.
           05  WS-CAL-READ-CNT        PIC 9(9) VALUE ZERO.
           05  WS-HOL-READ-CNT        PIC 9(9) VALUE ZERO.
           05  WS-CYR-WRITE-CNT       PIC 9(9) VALUE ZERO.
           05  WS-RUN-CNT             PIC 9(9) VALUE ZERO.

       01  WS-CYCLE-ID.
           05  WS-CYCLE-PREFIX        PIC X(04) VALUE "CYR-".
           05  WS-CYCLE-DT            PIC 9(8).
           05  WS-CYCLE-SIDE          PIC X(01).
           05  WS-CYCLE-SEQ           PIC 9(5).

       01  WS-DISPLAY.
           05  WS-DISP-NUM            PIC Z(9).

           COPY LK-CAL-PARM.

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE 8 TO RETURN-CODE
           PERFORM 1000-INITIALIZE
           PERFORM 2000-PROCESS
           PERFORM 9000-FINALIZE
           MOVE 0 TO RETURN-CODE
           GOBACK.

       1000-INITIALIZE.
           ACCEPT WS-FROM-DT
           ACCEPT WS-TO-DT

           PERFORM 1100-VALIDATE-RANGE

           OPEN INPUT KZCALF
           IF FS-KZCALF NOT = "00"
               DISPLAY "KZCALF オープン失敗 ST=" FS-KZCALF
               PERFORM 9900-ABEND
           END-IF

           OPEN INPUT KZHOLF
           IF FS-KZHOLF NOT = "00"
               DISPLAY "KZHOLF オープン失敗 ST=" FS-KZHOLF
               PERFORM 9900-ABEND
           END-IF

           OPEN OUTPUT KZCYRF
           IF FS-KZCYRF NOT = "00"
               DISPLAY "KZCYRF オープン失敗 ST=" FS-KZCYRF
               PERFORM 9900-ABEND
           END-IF.

       1100-VALIDATE-RANGE.
           IF WS-FROM-DT = ZERO OR WS-TO-DT = ZERO
               DISPLAY "処理日範囲未指定"
               PERFORM 9900-ABEND
           END-IF

           COMPUTE WS-FROM-INT =
               FUNCTION INTEGER-OF-DATE(WS-FROM-DT)
           COMPUTE WS-TO-INT =
               FUNCTION INTEGER-OF-DATE(WS-TO-DT)

           IF WS-FROM-INT = ZERO OR WS-TO-INT = ZERO
               DISPLAY "処理日範囲日付不正"
               PERFORM 9900-ABEND
           END-IF

           IF WS-FROM-INT > WS-TO-INT
               DISPLAY "処理日範囲大小不正 FROM=" WS-FROM-DT
                       " TO=" WS-TO-DT
               PERFORM 9900-ABEND
           END-IF

           COMPUTE WS-WORK-INT = WS-TO-INT - WS-FROM-INT + 1
           IF WS-WORK-INT > 370
               DISPLAY "処理日範囲過大 日数=" WS-WORK-INT
               PERFORM 9900-ABEND
           END-IF.

       2000-PROCESS.
           MOVE WS-FROM-INT TO WS-CURR-INT
           PERFORM UNTIL WS-CURR-INT > WS-TO-INT
               COMPUTE WS-CURR-DT =
                   FUNCTION DATE-OF-INTEGER(WS-CURR-INT)
               PERFORM 2100-JUDGE-DAY
               IF TARGET-HOLIDAY
                   IF OUT-HOLIDAY-RUN
                       MOVE "Y" TO SW-IN-RUN
                       MOVE WS-CURR-DT TO WS-RUN-FROM-DT
                       MOVE WS-CURR-DT TO WS-RUN-TO-DT
                   ELSE
                       MOVE WS-CURR-DT TO WS-RUN-TO-DT
                   END-IF
               ELSE
                   IF IN-HOLIDAY-RUN
                       PERFORM 3000-CLOSE-RUN
                       MOVE "N" TO SW-IN-RUN
                   END-IF
               END-IF
               ADD 1 TO WS-CURR-INT
           END-PERFORM

           IF IN-HOLIDAY-RUN
               PERFORM 3000-CLOSE-RUN
               MOVE "N" TO SW-IN-RUN
           END-IF.

       2100-JUDGE-DAY.
           MOVE "N" TO SW-HOLIDAY
           MOVE WS-CURR-DT TO CA-CAL-DT

           READ KZCALF KEY IS CA-CAL-DT
               INVALID KEY
                   PERFORM 2200-JUDGE-WEEKEND
               NOT INVALID KEY
                   ADD 1 TO WS-CAL-READ-CNT
                   EVALUATE CA-HOLIDAY-FLAG
                       WHEN "Y"
                           MOVE "Y" TO SW-HOLIDAY
                           PERFORM 2300-CHECK-HOLIDAY-MASTER
                       WHEN "N"
                           MOVE "N" TO SW-HOLIDAY
                       WHEN OTHER
                           DISPLAY "暦日区分不正 日付="
                                   WS-CURR-DT
                                   " 区分="
                                   CA-HOLIDAY-FLAG
                           PERFORM 9900-ABEND
                   END-EVALUATE
           END-READ

           IF FS-KZCALF NOT = "00" AND FS-KZCALF NOT = "23"
               DISPLAY "KZCALF 読込失敗 ST="
                       FS-KZCALF
                       " 日付="
                       WS-CURR-DT
               PERFORM 9900-ABEND
           END-IF.

       2200-JUDGE-WEEKEND.
           COMPUTE WS-WEEK-REM =
               FUNCTION MOD(WS-CURR-INT, 7)

           IF WS-WEEK-REM = 6 OR WS-WEEK-REM = 0
               MOVE "Y" TO SW-HOLIDAY
           ELSE
               DISPLAY "暦日未登録 日付=" WS-CURR-DT
               PERFORM 9900-ABEND
           END-IF.

       2300-CHECK-HOLIDAY-MASTER.
           MOVE WS-CURR-DT TO HO-HOLIDAY-DT

           READ KZHOLF KEY IS HO-HOLIDAY-DT
               INVALID KEY
                   CONTINUE
               NOT INVALID KEY
                   ADD 1 TO WS-HOL-READ-CNT
                   IF HO-VALID-FLAG NOT = "Y"
                       DISPLAY "休日原簿無効 日付="
                               WS-CURR-DT
                               " CD="
                               HO-HOLIDAY-CD
                       PERFORM 9900-ABEND
                   END-IF
           END-READ

           IF FS-KZHOLF NOT = "00" AND FS-KZHOLF NOT = "23"
               DISPLAY "KZHOLF 読込失敗 ST="
                       FS-KZHOLF
                       " 日付="
                       WS-CURR-DT
               PERFORM 9900-ABEND
           END-IF.

       3000-CLOSE-RUN.
           ADD 1 TO WS-RUN-CNT

           MOVE WS-CURR-INT TO WS-SAVE-CURR-INT

           PERFORM 3100-FIND-PREV-BUSINESS
           PERFORM 3200-FIND-NEXT-BUSINESS

           MOVE WS-SAVE-CURR-INT TO WS-CURR-INT
           COMPUTE WS-CURR-DT =
               FUNCTION DATE-OF-INTEGER(WS-CURR-INT)

           MOVE WS-PREV-BIZ-DT TO WS-NOMINAL-DT
           MOVE "P" TO WS-CYCLE-SIDE
           PERFORM 4000-WRITE-CYCLE

           MOVE WS-NEXT-BIZ-DT TO WS-NOMINAL-DT
           MOVE "N" TO WS-CYCLE-SIDE
           PERFORM 4000-WRITE-CYCLE.

       3100-FIND-PREV-BUSINESS.
           COMPUTE WS-WORK-INT =
               FUNCTION INTEGER-OF-DATE(WS-RUN-FROM-DT) - 1

           PERFORM UNTIL WS-WORK-INT < WS-FROM-INT
               COMPUTE WS-CURR-DT =
                   FUNCTION DATE-OF-INTEGER(WS-WORK-INT)
               PERFORM 2100-JUDGE-DAY
               IF TARGET-BUSINESS
                   MOVE WS-CURR-DT TO WS-PREV-BIZ-DT
                   EXIT PERFORM
               END-IF
               SUBTRACT 1 FROM WS-WORK-INT
           END-PERFORM

           IF WS-WORK-INT < WS-FROM-INT
               DISPLAY "前営業日候補なし 休日開始="
                       WS-RUN-FROM-DT
               PERFORM 9900-ABEND
           END-IF.

       3200-FIND-NEXT-BUSINESS.
           COMPUTE WS-WORK-INT =
               FUNCTION INTEGER-OF-DATE(WS-RUN-TO-DT) + 1

           PERFORM UNTIL WS-WORK-INT > WS-TO-INT
               COMPUTE WS-CURR-DT =
                   FUNCTION DATE-OF-INTEGER(WS-WORK-INT)
               PERFORM 2100-JUDGE-DAY
               IF TARGET-BUSINESS
                   MOVE WS-CURR-DT TO WS-NEXT-BIZ-DT
                   EXIT PERFORM
               END-IF
               ADD 1 TO WS-WORK-INT
           END-PERFORM

           IF WS-WORK-INT > WS-TO-INT
               DISPLAY "翌営業日候補なし 休日終了="
                       WS-RUN-TO-DT
               PERFORM 9900-ABEND
           END-IF.

       4000-WRITE-CYCLE.
           INITIALIZE LK-CAL-PARM

           MOVE WS-NOMINAL-DT TO LK-CAL-NOMINAL-DT

           CALL "KZ900S" USING LK-CAL-PARM

           IF LK-CAL-RET NOT = ZERO
               DISPLAY "日付解決サブ異常 日付="
                       WS-NOMINAL-DT
                       " RET="
                       LK-CAL-RET
               PERFORM 9900-ABEND
           END-IF

           INITIALIZE KZCYRF-REC

           MOVE WS-RUN-FROM-DT TO WS-CYCLE-DT
           MOVE WS-RUN-CNT TO WS-CYCLE-SEQ
           MOVE WS-CYCLE-ID TO CR-CYCLE-ID
           MOVE WS-NOMINAL-DT TO CR-NOMINAL-DT
           MOVE LK-CAL-RESOLVED-DT TO CR-RESOLVED-DT

           IF LK-CAL-ROLLED-FLAG = "Y"
              OR LK-CAL-RESOLVED-DT NOT = WS-NOMINAL-DT
               MOVE "Y" TO CR-ROLLED-FLAG
           ELSE
               MOVE "N" TO CR-ROLLED-FLAG
           END-IF

           WRITE KZCYRF-REC

           IF FS-KZCYRF NOT = "00"
               DISPLAY "KZCYRF 書込失敗 ST="
                       FS-KZCYRF
                       " 周期="
                       CR-CYCLE-ID
               PERFORM 9900-ABEND
           END-IF

           ADD 1 TO WS-CYR-WRITE-CNT.

       9000-FINALIZE.
           CLOSE KZCALF
           IF FS-KZCALF NOT = "00"
               DISPLAY "KZCALF クローズ失敗 ST=" FS-KZCALF
               PERFORM 9900-ABEND
           END-IF

           CLOSE KZHOLF
           IF FS-KZHOLF NOT = "00"
               DISPLAY "KZHOLF クローズ失敗 ST=" FS-KZHOLF
               PERFORM 9900-ABEND
           END-IF

           CLOSE KZCYRF
           IF FS-KZCYRF NOT = "00"
               DISPLAY "KZCYRF クローズ失敗 ST=" FS-KZCYRF
               PERFORM 9900-ABEND
           END-IF

           MOVE WS-RUN-CNT TO WS-DISP-NUM
           DISPLAY "長休区間件数=" WS-DISP-NUM

           MOVE WS-CYR-WRITE-CNT TO WS-DISP-NUM
           DISPLAY "KZCYRF 出力件数=" WS-DISP-NUM.

       9900-ABEND.
           MOVE 12 TO RETURN-CODE
           GOBACK.
