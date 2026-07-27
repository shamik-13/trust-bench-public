       IDENTIFICATION DIVISION.
       PROGRAM-ID. KP211S.
      *
      * 変更履歴
      * 版数  年月日       担当                         概要
      * 01    平成28年04月  システム部 勘定系チーム     新規作成
      * 02    令和02年10月  システム部 勘定系チーム     日付検証追加
      * 03    令和05年06月  システム部 勘定系チーム     状態返却見直し
      *
      * 遅延損害金率引当サブ。
      * 指定された遅延損害金率コードと基準日から、基準日以前で
      * 最も新しい有効日の利率を返す。該当なしは異常区分で返す。
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KPPENRF ASSIGN TO "KPPENRF"
             ORGANIZATION IS INDEXED
             ACCESS MODE IS SEQUENTIAL
             RECORD KEY IS PR-PENALTY-RATE-CD
             FILE STATUS IS WS-KPPENRF-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  KPPENRF
           LABEL RECORDS ARE STANDARD.
           COPY KPPENRFC.

       WORKING-STORAGE SECTION.
       01  WS-KPPENRF-STATUS       PIC XX VALUE SPACES.
           88  KPPENRF-OK                 VALUE "00".
           88  KPPENRF-EOF                VALUE "10".

       01  WS-SWITCHES.
           05  WS-FOUND-SW         PIC X VALUE "N".
               88  RATE-FOUND             VALUE "Y".
               88  RATE-NOT-FOUND         VALUE "N".
           05  WS-VALID-SW         PIC X VALUE "Y".
               88  INPUT-VALID            VALUE "Y".
               88  INPUT-INVALID          VALUE "N".

       01  WS-BEST-RATE.
           05  WS-BEST-EFFECTIVE-DT PIC 9(8) VALUE ZERO.
           05  WS-BEST-APR          PIC 9(3)V9(6) VALUE ZERO.
           05  WS-BEST-ROUND-MODE   PIC X VALUE SPACE.

       01  WS-WORK.
           05  WS-BASE-DT-N         PIC 9(8) VALUE ZERO.
           05  WS-YYYY              PIC 9(4) VALUE ZERO.
           05  WS-MM                PIC 9(2) VALUE ZERO.
           05  WS-DD                PIC 9(2) VALUE ZERO.

       LINKAGE SECTION.
       01  LK-KP211S-PARM.
           05  LK-PENALTY-RATE-CD   PIC X(10).
           05  LK-BASE-DT           PIC 9(8).
           05  LK-APR               PIC 9(3)V9(6).
           05  LK-ROUND-MODE        PIC X.
           05  LK-ABEND-KBN         PIC X.
           05  LK-FILE-STATUS       PIC XX.

       PROCEDURE DIVISION USING LK-KP211S-PARM.
       0000-MAIN.
           PERFORM 1000-INITIALIZE
           PERFORM 2000-VALIDATE-INPUT
           IF INPUT-VALID
              PERFORM 3000-SEARCH-RATE
           END-IF
           PERFORM 9000-RETURN
           GOBACK.

       1000-INITIALIZE.
           MOVE ZERO  TO LK-APR
           MOVE SPACE TO LK-ROUND-MODE
           MOVE "0"   TO LK-ABEND-KBN
           MOVE SPACE TO LK-FILE-STATUS
           MOVE ZERO  TO WS-BEST-EFFECTIVE-DT
           MOVE ZERO  TO WS-BEST-APR
           MOVE SPACE TO WS-BEST-ROUND-MODE
           SET RATE-NOT-FOUND TO TRUE
           SET INPUT-VALID TO TRUE.

       2000-VALIDATE-INPUT.
      *    コード未設定、または基準日が暦日として不正なら保留対象。
           IF LK-PENALTY-RATE-CD = SPACES
              SET INPUT-INVALID TO TRUE
              MOVE "1" TO LK-ABEND-KBN
           END-IF

           MOVE LK-BASE-DT TO WS-BASE-DT-N
           MOVE WS-BASE-DT-N(1:4) TO WS-YYYY
           MOVE WS-BASE-DT-N(5:2) TO WS-MM
           MOVE WS-BASE-DT-N(7:2) TO WS-DD

           IF WS-BASE-DT-N = ZERO
              SET INPUT-INVALID TO TRUE
              MOVE "1" TO LK-ABEND-KBN
           END-IF

           IF INPUT-VALID
              IF WS-MM < 1 OR WS-MM > 12
                 SET INPUT-INVALID TO TRUE
                 MOVE "1" TO LK-ABEND-KBN
              END-IF
           END-IF

           IF INPUT-VALID
              EVALUATE WS-MM
                 WHEN 1
                 WHEN 3
                 WHEN 5
                 WHEN 7
                 WHEN 8
                 WHEN 10
                 WHEN 12
                    IF WS-DD < 1 OR WS-DD > 31
                       SET INPUT-INVALID TO TRUE
                       MOVE "1" TO LK-ABEND-KBN
                    END-IF
                 WHEN 4
                 WHEN 6
                 WHEN 9
                 WHEN 11
                    IF WS-DD < 1 OR WS-DD > 30
                       SET INPUT-INVALID TO TRUE
                       MOVE "1" TO LK-ABEND-KBN
                    END-IF
                 WHEN 2
                    IF FUNCTION MOD(WS-YYYY 400) = 0
                       IF WS-DD < 1 OR WS-DD > 29
                          SET INPUT-INVALID TO TRUE
                          MOVE "1" TO LK-ABEND-KBN
                       END-IF
                    ELSE
                       IF FUNCTION MOD(WS-YYYY 100) = 0
                          IF WS-DD < 1 OR WS-DD > 28
                             SET INPUT-INVALID TO TRUE
                             MOVE "1" TO LK-ABEND-KBN
                          END-IF
                       ELSE
                          IF FUNCTION MOD(WS-YYYY 4) = 0
                             IF WS-DD < 1 OR WS-DD > 29
                                SET INPUT-INVALID TO TRUE
                                MOVE "1" TO LK-ABEND-KBN
                             END-IF
                          ELSE
                             IF WS-DD < 1 OR WS-DD > 28
                                SET INPUT-INVALID TO TRUE
                                MOVE "1" TO LK-ABEND-KBN
                             END-IF
                          END-IF
                       END-IF
                    END-IF
              END-EVALUATE
           END-IF.

       3000-SEARCH-RATE.
      *    単純に全件を走査し、対象コード内の最新有効日を採用する。
           OPEN INPUT KPPENRF
           IF NOT KPPENRF-OK
              MOVE "2" TO LK-ABEND-KBN
              MOVE WS-KPPENRF-STATUS TO LK-FILE-STATUS
           ELSE
              PERFORM UNTIL KPPENRF-EOF
                 READ KPPENRF NEXT RECORD
                    AT END
                       CONTINUE
                    NOT AT END
                       IF WS-KPPENRF-STATUS = "00"
                          PERFORM 3100-EVALUATE-RECORD
                       ELSE
                          IF NOT KPPENRF-EOF
                             MOVE "2" TO LK-ABEND-KBN
                             MOVE WS-KPPENRF-STATUS TO LK-FILE-STATUS
                             EXIT PERFORM
                          END-IF
                       END-IF
                 END-READ
              END-PERFORM

              CLOSE KPPENRF
              IF LK-ABEND-KBN = "0"
                 IF RATE-FOUND
                    MOVE WS-BEST-APR        TO LK-APR
                    MOVE WS-BEST-ROUND-MODE TO LK-ROUND-MODE
                    MOVE WS-KPPENRF-STATUS  TO LK-FILE-STATUS
                 ELSE
                    MOVE "1" TO LK-ABEND-KBN
                    MOVE WS-KPPENRF-STATUS TO LK-FILE-STATUS
                 END-IF
              END-IF
           END-IF.

       3100-EVALUATE-RECORD.
           IF PR-PENALTY-RATE-CD = LK-PENALTY-RATE-CD
              IF PR-EFFECTIVE-DT <= LK-BASE-DT
                 IF PR-EFFECTIVE-DT > WS-BEST-EFFECTIVE-DT
                    MOVE PR-EFFECTIVE-DT TO WS-BEST-EFFECTIVE-DT
                    MOVE PR-APR          TO WS-BEST-APR
                    MOVE PR-ROUND-MODE   TO WS-BEST-ROUND-MODE
                    SET RATE-FOUND TO TRUE
                 END-IF
              END-IF
           END-IF.

       9000-RETURN.
           IF LK-FILE-STATUS = SPACES
              MOVE WS-KPPENRF-STATUS TO LK-FILE-STATUS
           END-IF.
