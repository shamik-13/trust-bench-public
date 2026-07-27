       IDENTIFICATION DIVISION.
       PROGRAM-ID. KZ140S.
      * 変更履歴
      * 版数  年月日(和暦)  担当                      概要
      * 1.00  平成28.04.01  システム部 勘定系チーム  新規作成
      * 1.01  令和02.01.06  システム部 勘定系チーム  祝日判定見直し
      * 1.02  令和06.04.01  システム部 勘定系チーム  営業日経過日数計算改善
       AUTHOR. KZBATCH.
      *
      * 営業日経過日数計算サブルーチン。
      * 開始日、終了日、算入区分を受け取り、暦日数と営業日数を返す。
      * 現状は日付妥当性と営業日判定を単純に直列処理する実装であり、
      * 月次大量呼出時の索引参照回数削減は未判断のまま残している。
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZCALF
               ASSIGN TO "KZCALF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CA-CAL-DT
               FILE STATUS IS FS-KZCALF.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  KZCALF.
           COPY KZCALFC.
      *
       WORKING-STORAGE SECTION.
       01  FS-KZCALF              PIC XX VALUE SPACE.
       01  WS-RETURN              PIC S9(04) COMP VALUE ZERO.
       01  WS-ABEND-FLAG          PIC X VALUE SPACE.
           88  WS-ABEND           VALUE "Y".
       01  WS-EOF-FLAG            PIC X VALUE SPACE.
           88  WS-EOF             VALUE "Y".
       01  WS-FOUND-FLAG          PIC X VALUE SPACE.
           88  WS-CAL-FOUND       VALUE "Y".
       01  WS-OPEN-FLAG           PIC X VALUE SPACE.
           88  WS-OPENED          VALUE "Y".
       01  WS-DATE-WORK.
           05  WS-START-DT        PIC 9(08) VALUE ZERO.
           05  WS-END-DT          PIC 9(08) VALUE ZERO.
           05  WS-FROM-DT         PIC 9(08) VALUE ZERO.
           05  WS-TO-DT           PIC 9(08) VALUE ZERO.
           05  WS-CUR-DT          PIC 9(08) VALUE ZERO.
           05  WS-INT-START       PIC 9(08) VALUE ZERO.
           05  WS-INT-END         PIC 9(08) VALUE ZERO.
           05  WS-INT-FROM        PIC 9(08) VALUE ZERO.
           05  WS-INT-TO          PIC 9(08) VALUE ZERO.
           05  WS-INT-CUR         PIC 9(08) VALUE ZERO.
       01  WS-COUNT-WORK.
           05  WS-CALENDAR-CNT    PIC 9(09) VALUE ZERO.
           05  WS-BUSINESS-CNT    PIC 9(09) VALUE ZERO.
       01  WS-INCLUDE-WORK.
           05  WS-INCLUDE-START   PIC X VALUE SPACE.
               88  WS-COUNT-START VALUE "Y".
           05  WS-INCLUDE-END     PIC X VALUE SPACE.
               88  WS-COUNT-END   VALUE "Y".
       01  WS-WARN-WORK.
           05  WS-WARN-CODE       PIC X(02) VALUE "00".
           05  WS-WARN-TEMP       PIC X(02) VALUE "00".
       01  WS-CAL-JUDGE.
           05  WS-HOLIDAY-FLAG    PIC X VALUE SPACE.
           05  WS-BOUNDARY-FLAG   PIC X VALUE SPACE.
               88  WS-BOUNDARY    VALUE "Y".
       01  WS-MSG.
           05  WS-MSG-PGM         PIC X(08) VALUE "KZ140S ".
           05  WS-MSG-TEXT        PIC X(60) VALUE SPACE.
      *
           COPY LK-CAL-PARM.
      *
       LINKAGE SECTION.
       01  LK-KZ140-PARM.
           05  LK-KZ140-START-DT  PIC 9(08).
           05  LK-KZ140-END-DT    PIC 9(08).
           05  LK-KZ140-SAN-KBN   PIC X(01).
               88  LK-SAN-BOTH    VALUE "1".
               88  LK-SAN-START   VALUE "2".
               88  LK-SAN-END     VALUE "3".
               88  LK-SAN-NONE    VALUE "4".
           05  LK-KZ140-CAL-DAYS  PIC 9(09).
           05  LK-KZ140-BUS-DAYS  PIC 9(09).
           05  LK-KZ140-WARN-CD   PIC X(02).
           05  LK-KZ140-RET       PIC X(02).
      *
       PROCEDURE DIVISION USING LK-KZ140-PARM.
      *
       0000-MAIN SECTION.
       0000-START.
           PERFORM 1000-INIT.
           IF NOT WS-ABEND
               PERFORM 2000-VALIDATE
           END-IF.
           IF NOT WS-ABEND
               PERFORM 3000-OPEN-FILE
           END-IF.
           IF NOT WS-ABEND
               PERFORM 4000-CALCULATE
           END-IF.
           IF WS-OPENED
               PERFORM 8000-CLOSE-FILE
           END-IF.
           PERFORM 9000-FINAL.
           GOBACK.
      *
       1000-INIT SECTION.
       1000-START.
           MOVE 0 TO RETURN-CODE
           MOVE SPACE TO WS-ABEND-FLAG
           MOVE SPACE TO WS-EOF-FLAG
           MOVE SPACE TO WS-OPEN-FLAG
           MOVE ZERO TO WS-CALENDAR-CNT
           MOVE ZERO TO WS-BUSINESS-CNT
           MOVE "00" TO WS-WARN-CODE
           MOVE "00" TO LK-KZ140-RET
           MOVE "00" TO LK-KZ140-WARN-CD
           MOVE ZERO TO LK-KZ140-CAL-DAYS
           MOVE ZERO TO LK-KZ140-BUS-DAYS
           MOVE LK-KZ140-START-DT TO WS-START-DT
           MOVE LK-KZ140-END-DT TO WS-END-DT.
      *
       2000-VALIDATE SECTION.
       2000-START.
           IF WS-START-DT = ZERO OR WS-END-DT = ZERO
               MOVE "日付未設定" TO WS-MSG-TEXT
               DISPLAY WS-MSG-PGM WS-MSG-TEXT
               MOVE "91" TO LK-KZ140-RET
               MOVE 8 TO WS-RETURN
               SET WS-ABEND TO TRUE
           END-IF
           IF NOT WS-ABEND
               COMPUTE WS-INT-START =
                   FUNCTION INTEGER-OF-DATE(WS-START-DT)
               COMPUTE WS-INT-END =
                   FUNCTION INTEGER-OF-DATE(WS-END-DT)
               IF WS-INT-START = ZERO OR WS-INT-END = ZERO
                   MOVE "日付形式不正" TO WS-MSG-TEXT
                   DISPLAY WS-MSG-PGM WS-MSG-TEXT
                   MOVE "92" TO LK-KZ140-RET
                   MOVE 8 TO WS-RETURN
                   SET WS-ABEND TO TRUE
               END-IF
           END-IF
           IF NOT WS-ABEND AND WS-INT-START > WS-INT-END
               MOVE "開始日終了日逆転" TO WS-MSG-TEXT
               DISPLAY WS-MSG-PGM WS-MSG-TEXT
               MOVE "93" TO LK-KZ140-RET
               MOVE 8 TO WS-RETURN
               SET WS-ABEND TO TRUE
           END-IF
           IF NOT WS-ABEND
               EVALUATE TRUE
                   WHEN LK-SAN-BOTH
                       MOVE "Y" TO WS-INCLUDE-START
                       MOVE "Y" TO WS-INCLUDE-END
                   WHEN LK-SAN-START
                       MOVE "Y" TO WS-INCLUDE-START
                       MOVE "N" TO WS-INCLUDE-END
                   WHEN LK-SAN-END
                       MOVE "N" TO WS-INCLUDE-START
                       MOVE "Y" TO WS-INCLUDE-END
                   WHEN LK-SAN-NONE
                       MOVE "N" TO WS-INCLUDE-START
                       MOVE "N" TO WS-INCLUDE-END
                   WHEN OTHER
                       MOVE "算入区分不正" TO WS-MSG-TEXT
                       DISPLAY WS-MSG-PGM WS-MSG-TEXT
                       MOVE "94" TO LK-KZ140-RET
                       MOVE 8 TO WS-RETURN
                       SET WS-ABEND TO TRUE
               END-EVALUATE
           END-IF
           IF NOT WS-ABEND
               MOVE WS-INT-START TO WS-INT-FROM
               MOVE WS-INT-END TO WS-INT-TO
               IF NOT WS-COUNT-START
                   ADD 1 TO WS-INT-FROM
               END-IF
               IF NOT WS-COUNT-END
                   SUBTRACT 1 FROM WS-INT-TO
               END-IF
               IF WS-INT-FROM > WS-INT-TO
                   MOVE ZERO TO WS-CALENDAR-CNT
                   MOVE ZERO TO WS-BUSINESS-CNT
               END-IF
           END-IF.
      *
       3000-OPEN-FILE SECTION.
       3000-START.
           OPEN INPUT KZCALF
           IF FS-KZCALF NOT = "00"
               MOVE "KZCALF オープン失敗 ST=" TO WS-MSG-TEXT
               DISPLAY WS-MSG-PGM WS-MSG-TEXT FS-KZCALF
               MOVE "95" TO LK-KZ140-RET
               MOVE 12 TO WS-RETURN
               SET WS-ABEND TO TRUE
           ELSE
               MOVE "Y" TO WS-OPEN-FLAG
           END-IF.
      *
       4000-CALCULATE SECTION.
       4000-START.
           IF WS-INT-FROM > WS-INT-TO
               CONTINUE
           ELSE
               COMPUTE WS-CALENDAR-CNT = WS-INT-TO - WS-INT-FROM + 1
               MOVE WS-INT-FROM TO WS-INT-CUR
               PERFORM UNTIL WS-INT-CUR > WS-INT-TO OR WS-ABEND
                   COMPUTE WS-CUR-DT =
                       FUNCTION DATE-OF-INTEGER(WS-INT-CUR)
                   PERFORM 4100-JUDGE-DAY
                   IF NOT WS-ABEND
                       IF WS-HOLIDAY-FLAG = "N"
                           ADD 1 TO WS-BUSINESS-CNT
                       END-IF
                   END-IF
                   ADD 1 TO WS-INT-CUR
               END-PERFORM
           END-IF.
      *
       4100-JUDGE-DAY SECTION.
       4100-START.
           MOVE SPACE TO WS-FOUND-FLAG
           MOVE SPACE TO WS-BOUNDARY-FLAG
           MOVE SPACE TO WS-HOLIDAY-FLAG
           IF WS-CUR-DT = WS-START-DT OR WS-CUR-DT = WS-END-DT
               SET WS-BOUNDARY TO TRUE
           END-IF
           MOVE WS-CUR-DT TO CA-CAL-DT
           READ KZCALF
               INVALID KEY
                   MOVE SPACE TO WS-FOUND-FLAG
               NOT INVALID KEY
                   SET WS-CAL-FOUND TO TRUE
           END-READ
           IF FS-KZCALF = "00"
               IF CA-HOLIDAY-FLAG = "Y" OR CA-HOLIDAY-FLAG = "N"
                   MOVE CA-HOLIDAY-FLAG TO WS-HOLIDAY-FLAG
               ELSE
                   MOVE "休日区分不正" TO WS-MSG-TEXT
                   DISPLAY WS-MSG-PGM WS-MSG-TEXT CA-CAL-DT
                   MOVE "96" TO LK-KZ140-RET
                   MOVE 12 TO WS-RETURN
                   SET WS-ABEND TO TRUE
               END-IF
           ELSE
               IF FS-KZCALF = "23"
                   IF WS-BOUNDARY
                       PERFORM 4200-CALL-KZ900S
                       IF WS-WARN-CODE = "00"
                           MOVE "01" TO WS-WARN-CODE
                       END-IF
                   ELSE
                       MOVE "KZCALF 日付未登録" TO WS-MSG-TEXT
                       DISPLAY WS-MSG-PGM WS-MSG-TEXT WS-CUR-DT
                       MOVE "97" TO LK-KZ140-RET
                       MOVE 12 TO WS-RETURN
                       SET WS-ABEND TO TRUE
                   END-IF
               ELSE
                   MOVE "KZCALF 読込失敗 ST=" TO WS-MSG-TEXT
                   DISPLAY WS-MSG-PGM WS-MSG-TEXT FS-KZCALF
                   MOVE "98" TO LK-KZ140-RET
                   MOVE 12 TO WS-RETURN
                   SET WS-ABEND TO TRUE
               END-IF
           END-IF.
      *
       4200-CALL-KZ900S SECTION.
       4200-START.
           MOVE LOW-VALUE TO LK-CAL-PARM
           MOVE WS-CUR-DT TO LK-CAL-NOMINAL-DT
           CALL "KZ900S" USING LK-CAL-PARM
           IF LK-CAL-RET NOT = "00"
               MOVE "KZ900S 判定失敗 RC=" TO WS-MSG-TEXT
               DISPLAY WS-MSG-PGM WS-MSG-TEXT LK-CAL-RET
               MOVE "99" TO LK-KZ140-RET
               MOVE 12 TO WS-RETURN
               SET WS-ABEND TO TRUE
           ELSE
               IF LK-CAL-ROLLED-FLAG = "Y"
                   MOVE "Y" TO WS-HOLIDAY-FLAG
               ELSE
                   MOVE "N" TO WS-HOLIDAY-FLAG
               END-IF
           END-IF.
      *
       8000-CLOSE-FILE SECTION.
       8000-START.
           CLOSE KZCALF
           IF FS-KZCALF NOT = "00"
               MOVE "KZCALF クローズ失敗 ST=" TO WS-MSG-TEXT
               DISPLAY WS-MSG-PGM WS-MSG-TEXT FS-KZCALF
               MOVE "98" TO LK-KZ140-RET
               MOVE 12 TO WS-RETURN
               SET WS-ABEND TO TRUE
           END-IF
           MOVE SPACE TO WS-OPEN-FLAG.
      *
       9000-FINAL SECTION.
       9000-START.
           IF WS-ABEND
               MOVE WS-RETURN TO RETURN-CODE
           ELSE
               MOVE WS-CALENDAR-CNT TO LK-KZ140-CAL-DAYS
               MOVE WS-BUSINESS-CNT TO LK-KZ140-BUS-DAYS
               MOVE WS-WARN-CODE TO LK-KZ140-WARN-CD
               IF LK-KZ140-RET = "00"
                   MOVE "00" TO LK-KZ140-RET
               END-IF
               MOVE 0 TO RETURN-CODE
           END-IF.
