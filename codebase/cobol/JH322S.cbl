       IDENTIFICATION DIVISION.
       PROGRAM-ID. JH322S.
      ******************************************************************
      * 変更履歴
      * 版数  年月日    担当    概要
      * 0.1   20250318  JH01    初版作成
      * 0.2   20250409  JH02    空値許容属性の警告判定追加
      * 0.3   20250522  JH03    参照マスタ期限判定の整理
      ******************************************************************
      * 属性コード妥当性判定サブルーチン
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT JHREFMST
               ASSIGN TO "JHREFMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS RF-REF-TYPE-CD
               FILE STATUS IS WS-RF-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  JHREFMST.
           COPY JHREFMSTC.

       WORKING-STORAGE SECTION.
       01  WS-RF-STATUS             PIC XX.
       01  WS-END-FLAG              PIC X VALUE SPACE.
           88  WS-END                         VALUE "1".
           88  WS-NOT-END                     VALUE SPACE.
       01  WS-FOUND-FLAG            PIC X VALUE SPACE.
           88  WS-FOUND                       VALUE "1".
           88  WS-NOT-FOUND                   VALUE SPACE.
       01  WS-OPEN-FLAG             PIC X VALUE SPACE.
           88  WS-RF-OPENED                   VALUE "1".
           88  WS-RF-CLOSED                   VALUE SPACE.
       01  WS-NULL-ALLOW-FLAG       PIC X VALUE SPACE.
           88  WS-NULL-ALLOWED                VALUE "1".
           88  WS-NULL-DENIED                 VALUE SPACE.
       01  WS-WORK-DATE             PIC 9(8).
       01  WS-RESULT-CD             PIC X.
           88  WS-RESULT-NORMAL               VALUE "0".
           88  WS-RESULT-WARNING              VALUE "1".
           88  WS-RESULT-EXPIRED              VALUE "2".
           88  WS-RESULT-NOT-REG              VALUE "3".
           88  WS-RESULT-SYSERR               VALUE "9".
       01  WS-SEVERITY              PIC 9.
       01  WS-LOG-MSG.
           05  WS-LOG-TEXT          PIC X(60).
           05  WS-LOG-ST            PIC XX.

       LINKAGE SECTION.
       01  LK-JH322S-PARM.
           05  LK-ATTR-TYPE-CD      PIC X(10).
           05  LK-ATTR-CD           PIC X(20).
           05  LK-BASE-DATE         PIC 9(8).
           05  LK-RESULT-CD         PIC X.
           05  LK-SEVERITY          PIC 9.
           05  LK-REASON-CD         PIC X(8).
           05  LK-REASON-TEXT       PIC X(60).

       PROCEDURE DIVISION USING LK-JH322S-PARM.
       0000-MAIN.
           MOVE 0 TO RETURN-CODE
           PERFORM 1000-INITIALIZE
           PERFORM 2000-VALIDATE-INPUT
           IF WS-RESULT-SYSERR
              PERFORM 9000-RETURN-RESULT
              GOBACK
           END-IF
           IF WS-RESULT-WARNING
              PERFORM 9000-RETURN-RESULT
              GOBACK
           END-IF
           PERFORM 3000-OPEN-FILE
           IF WS-RESULT-SYSERR
              PERFORM 9000-RETURN-RESULT
              GOBACK
           END-IF
           PERFORM 4000-SEARCH-REFMST
           IF WS-FOUND
              PERFORM 5000-JUDGE-VALIDITY
           ELSE
              SET WS-RESULT-NOT-REG TO TRUE
              MOVE 2 TO WS-SEVERITY
              MOVE "JH322003" TO LK-REASON-CD
              MOVE "属性コード未登録" TO LK-REASON-TEXT
           END-IF
           PERFORM 8000-CLOSE-FILE
           PERFORM 9000-RETURN-RESULT
           GOBACK.

       1000-INITIALIZE.
           SET WS-NOT-END TO TRUE
           SET WS-NOT-FOUND TO TRUE
           SET WS-RF-CLOSED TO TRUE
           SET WS-NULL-DENIED TO TRUE
           SET WS-RESULT-NORMAL TO TRUE
           MOVE 0 TO WS-SEVERITY
           MOVE SPACES TO LK-RESULT-CD
           MOVE ZERO TO LK-SEVERITY
           MOVE SPACES TO LK-REASON-CD
           MOVE SPACES TO LK-REASON-TEXT
           MOVE SPACES TO WS-LOG-MSG
           IF LK-BASE-DATE = ZERO
              ACCEPT WS-WORK-DATE FROM DATE YYYYMMDD
           ELSE
              MOVE LK-BASE-DATE TO WS-WORK-DATE
           END-IF.

       2000-VALIDATE-INPUT.
           IF LK-ATTR-TYPE-CD = SPACES
              SET WS-RESULT-SYSERR TO TRUE
              MOVE 8 TO WS-SEVERITY
              MOVE "JH322001" TO LK-REASON-CD
              MOVE "属性種別未設定" TO LK-REASON-TEXT
              MOVE 8 TO RETURN-CODE
              EXIT PARAGRAPH
           END-IF
           PERFORM 2100-CHECK-NULL-ALLOW
           IF LK-ATTR-CD = SPACES
              IF WS-NULL-ALLOWED
                 SET WS-RESULT-WARNING TO TRUE
                 MOVE 1 TO WS-SEVERITY
                 MOVE "JH322002" TO LK-REASON-CD
                 MOVE "属性コード空値許容" TO LK-REASON-TEXT
              ELSE
                 SET WS-RESULT-NOT-REG TO TRUE
                 MOVE 2 TO WS-SEVERITY
                 MOVE "JH322004" TO LK-REASON-CD
                 MOVE "属性コード未設定" TO LK-REASON-TEXT
              END-IF
           END-IF.

       2100-CHECK-NULL-ALLOW.
           SET WS-NULL-DENIED TO TRUE
           EVALUATE LK-ATTR-TYPE-CD
             WHEN "DMKYOKA"
             WHEN "SHOKUGYO"
             WHEN "GYOSYU"
                SET WS-NULL-ALLOWED TO TRUE
             WHEN OTHER
                CONTINUE
           END-EVALUATE.

       3000-OPEN-FILE.
           OPEN INPUT JHREFMST
           IF WS-RF-STATUS = "00"
              SET WS-RF-OPENED TO TRUE
           ELSE
              SET WS-RESULT-SYSERR TO TRUE
              MOVE 8 TO WS-SEVERITY
              MOVE "JH322901" TO LK-REASON-CD
              MOVE "JHREFMST オープン失敗" TO LK-REASON-TEXT
              MOVE "JHREFMST オープン失敗 ST=" TO WS-LOG-TEXT
              MOVE WS-RF-STATUS TO WS-LOG-ST
              DISPLAY WS-LOG-MSG
              MOVE 8 TO RETURN-CODE
           END-IF.

       4000-SEARCH-REFMST.
           MOVE LK-ATTR-TYPE-CD TO RF-REF-TYPE-CD
           START JHREFMST KEY IS NOT LESS THAN RF-REF-TYPE-CD
           IF WS-RF-STATUS = "00"
              PERFORM 4100-READ-LOOP UNTIL WS-END OR WS-FOUND
           ELSE
              IF WS-RF-STATUS = "23"
                 SET WS-NOT-FOUND TO TRUE
              ELSE
                 SET WS-RESULT-SYSERR TO TRUE
                 MOVE 8 TO WS-SEVERITY
                 MOVE "JH322902" TO LK-REASON-CD
                 MOVE "JHREFMST 検索失敗" TO LK-REASON-TEXT
                 MOVE "JHREFMST 検索失敗 ST=" TO WS-LOG-TEXT
                 MOVE WS-RF-STATUS TO WS-LOG-ST
                 DISPLAY WS-LOG-MSG
                 MOVE 8 TO RETURN-CODE
              END-IF
           END-IF.

       4100-READ-LOOP.
           READ JHREFMST NEXT RECORD
             AT END
                SET WS-END TO TRUE
             NOT AT END
                IF WS-RF-STATUS NOT = "00"
                   SET WS-RESULT-SYSERR TO TRUE
                   MOVE 8 TO WS-SEVERITY
                   MOVE "JH322903" TO LK-REASON-CD
                   MOVE "JHREFMST 読込失敗" TO LK-REASON-TEXT
                   MOVE "JHREFMST 読込失敗 ST=" TO WS-LOG-TEXT
                   MOVE WS-RF-STATUS TO WS-LOG-ST
                   DISPLAY WS-LOG-MSG
                   MOVE 8 TO RETURN-CODE
                   SET WS-END TO TRUE
                ELSE
                   IF RF-REF-TYPE-CD NOT = LK-ATTR-TYPE-CD
                      SET WS-END TO TRUE
                   ELSE
                      IF RF-REF-CD = LK-ATTR-CD
                         SET WS-FOUND TO TRUE
                      END-IF
                   END-IF
                END-IF
           END-READ.

       5000-JUDGE-VALIDITY.
           IF RF-ACTIVE-FLAG NOT = "1"
              SET WS-RESULT-EXPIRED TO TRUE
              MOVE 2 TO WS-SEVERITY
              MOVE "JH322005" TO LK-REASON-CD
              MOVE "属性コード非アクティブ" TO LK-REASON-TEXT
              EXIT PARAGRAPH
           END-IF
           IF WS-WORK-DATE < RF-VALID-FROM-DATE
              SET WS-RESULT-EXPIRED TO TRUE
              MOVE 2 TO WS-SEVERITY
              MOVE "JH322006" TO LK-REASON-CD
              MOVE "属性コード適用開始前" TO LK-REASON-TEXT
              EXIT PARAGRAPH
           END-IF
           IF WS-WORK-DATE > RF-VALID-TO-DATE
              SET WS-RESULT-EXPIRED TO TRUE
              MOVE 2 TO WS-SEVERITY
              MOVE "JH322007" TO LK-REASON-CD
              MOVE "属性コード有効期限切れ" TO LK-REASON-TEXT
              EXIT PARAGRAPH
           END-IF
           SET WS-RESULT-NORMAL TO TRUE
           MOVE 0 TO WS-SEVERITY
           MOVE "JH322000" TO LK-REASON-CD
           MOVE "属性コード正常" TO LK-REASON-TEXT.

       8000-CLOSE-FILE.
           IF WS-RF-OPENED
              CLOSE JHREFMST
              IF WS-RF-STATUS = "00"
                 SET WS-RF-CLOSED TO TRUE
              ELSE
                 SET WS-RESULT-SYSERR TO TRUE
                 MOVE 8 TO WS-SEVERITY
                 MOVE "JH322904" TO LK-REASON-CD
                 MOVE "JHREFMST クローズ失敗" TO LK-REASON-TEXT
                 MOVE "JHREFMST クローズ失敗 ST=" TO WS-LOG-TEXT
                 MOVE WS-RF-STATUS TO WS-LOG-ST
                 DISPLAY WS-LOG-MSG
                 MOVE 8 TO RETURN-CODE
              END-IF
           END-IF.

       9000-RETURN-RESULT.
           MOVE WS-RESULT-CD TO LK-RESULT-CD
           MOVE WS-SEVERITY TO LK-SEVERITY
           IF WS-RESULT-SYSERR
              MOVE 8 TO RETURN-CODE
           ELSE
              MOVE 0 TO RETURN-CODE
           END-IF.
