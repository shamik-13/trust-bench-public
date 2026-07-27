       IDENTIFICATION DIVISION.
       PROGRAM-ID. CB216S.
      *--------------------------------------------------------------*
      * カード状態更新 採否判定サブプログラム
      * 変更履歴
      * 1.00  20240620  CARD-DEV  初版作成
      * 1.01  20250218  CARD-DEV  理由コード優先度見直し
      *--------------------------------------------------------------*

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  WK-CNTL.
           05  WK-ERR-SW              PIC X     VALUE SPACE.
               88  WK-ERR-ON                   VALUE '1'.
               88  WK-ERR-OFF                  VALUE SPACE.
           05  WK-CUR-STAT-PRI        PIC 9(03) VALUE ZERO.
           05  WK-NEW-STAT-PRI        PIC 9(03) VALUE ZERO.
           05  WK-CUR-SRC-PRI         PIC 9(03) VALUE ZERO.
           05  WK-NEW-SRC-PRI         PIC 9(03) VALUE ZERO.
           05  WK-REASON-PRI          PIC 9(03) VALUE ZERO.

       LINKAGE SECTION.
       01  CB216I.
           05  CB216I-CUR-STATUS     PIC X(02).
           05  CB216I-NEW-STATUS     PIC X(02).
           05  CB216I-CUR-SOURCE     PIC X(03).
           05  CB216I-NEW-SOURCE     PIC X(03).
           05  CB216I-CUR-TIME       PIC 9(14).
           05  CB216I-NEW-TIME       PIC 9(14).
           05  CB216I-REASON-CD      PIC X(03).

       01  CB216O.
           05  CB216O-UPDATE-FLG     PIC X(01).
           05  CB216O-PRIORITY       PIC 9(03).
           05  CB216O-JUDGE-CD       PIC X(02).
           05  CB216O-REASON-TEXT    PIC X(40).

       PROCEDURE DIVISION USING CB216I CB216O.

       0000-MAIN.
           SET WK-ERR-OFF TO TRUE
           PERFORM 1000-JUDGE-STATUS
           GOBACK
           .

       1000-JUDGE-STATUS.
           MOVE SPACE TO CB216O-UPDATE-FLG
           MOVE ZERO  TO CB216O-PRIORITY
           MOVE SPACE TO CB216O-JUDGE-CD
           MOVE SPACE TO CB216O-REASON-TEXT

           MOVE ZERO TO WK-CUR-STAT-PRI
           MOVE ZERO TO WK-NEW-STAT-PRI
           MOVE ZERO TO WK-CUR-SRC-PRI
           MOVE ZERO TO WK-NEW-SRC-PRI
           MOVE ZERO TO WK-REASON-PRI

           PERFORM 1100-VALIDATE-INPUT

           IF WK-ERR-ON
              MOVE 'N' TO CB216O-UPDATE-FLG
              MOVE ZERO TO CB216O-PRIORITY
              MOVE '99' TO CB216O-JUDGE-CD
              MOVE '入力不正' TO CB216O-REASON-TEXT
              EXIT PARAGRAPH
           END-IF

           PERFORM 1200-SET-CURRENT-PRIORITY
           PERFORM 1300-SET-NEW-PRIORITY
           PERFORM 1400-SET-SOURCE-PRIORITY
           PERFORM 1500-DECIDE-UPDATE
           .

       1100-VALIDATE-INPUT.
           IF CB216I-CUR-TIME = ZERO
              SET WK-ERR-ON TO TRUE
              EXIT PARAGRAPH
           END-IF

           IF CB216I-NEW-TIME = ZERO
              SET WK-ERR-ON TO TRUE
              EXIT PARAGRAPH
           END-IF

           IF CB216I-CUR-STATUS NOT = 'AC'
              AND CB216I-CUR-STATUS NOT = 'SP'
              AND CB216I-CUR-STATUS NOT = 'IN'
              AND CB216I-CUR-STATUS NOT = 'CL'
              SET WK-ERR-ON TO TRUE
              EXIT PARAGRAPH
           END-IF

           IF CB216I-NEW-STATUS NOT = 'AC'
              AND CB216I-NEW-STATUS NOT = 'SP'
              AND CB216I-NEW-STATUS NOT = 'IN'
              AND CB216I-NEW-STATUS NOT = 'CL'
              SET WK-ERR-ON TO TRUE
              EXIT PARAGRAPH
           END-IF

           IF CB216I-CUR-SOURCE NOT = 'HST'
              AND CB216I-CUR-SOURCE NOT = 'BAT'
              AND CB216I-CUR-SOURCE NOT = 'ATM'
              AND CB216I-CUR-SOURCE NOT = 'WEB'
              SET WK-ERR-ON TO TRUE
              EXIT PARAGRAPH
           END-IF

           IF CB216I-NEW-SOURCE NOT = 'HST'
              AND CB216I-NEW-SOURCE NOT = 'BAT'
              AND CB216I-NEW-SOURCE NOT = 'ATM'
              AND CB216I-NEW-SOURCE NOT = 'WEB'
              SET WK-ERR-ON TO TRUE
              EXIT PARAGRAPH
           END-IF

           IF CB216I-REASON-CD = SPACE
              SET WK-ERR-ON TO TRUE
              EXIT PARAGRAPH
           END-IF
           .

       1200-SET-CURRENT-PRIORITY.
           EVALUATE CB216I-CUR-STATUS
              WHEN 'CL'
                 MOVE 900 TO WK-CUR-STAT-PRI
              WHEN 'SP'
                 MOVE 800 TO WK-CUR-STAT-PRI
              WHEN 'IN'
                 MOVE 400 TO WK-CUR-STAT-PRI
              WHEN 'AC'
                 MOVE 100 TO WK-CUR-STAT-PRI
           END-EVALUATE
           .

       1300-SET-NEW-PRIORITY.
           EVALUATE CB216I-NEW-STATUS
              WHEN 'CL'
                 MOVE 900 TO WK-NEW-STAT-PRI
              WHEN 'SP'
                 MOVE 700 TO WK-NEW-STAT-PRI
              WHEN 'IN'
                 MOVE 400 TO WK-NEW-STAT-PRI
              WHEN 'AC'
                 MOVE 100 TO WK-NEW-STAT-PRI
           END-EVALUATE

           EVALUATE CB216I-REASON-CD
              WHEN 'F01'
              WHEN 'F02'
              WHEN 'L01'
                 MOVE 950 TO WK-REASON-PRI
              WHEN 'C99'
                 MOVE 900 TO WK-REASON-PRI
              WHEN 'C01'
                 MOVE 760 TO WK-REASON-PRI
              WHEN 'R01'
              WHEN 'R02'
                 MOVE 120 TO WK-REASON-PRI
              WHEN OTHER
                 MOVE 300 TO WK-REASON-PRI
           END-EVALUATE

           IF WK-REASON-PRI > WK-NEW-STAT-PRI
              MOVE WK-REASON-PRI TO WK-NEW-STAT-PRI
           END-IF
           .

       1400-SET-SOURCE-PRIORITY.
           EVALUATE CB216I-CUR-SOURCE
              WHEN 'HST'
                 MOVE 400 TO WK-CUR-SRC-PRI
              WHEN 'BAT'
                 MOVE 300 TO WK-CUR-SRC-PRI
              WHEN 'ATM'
                 MOVE 200 TO WK-CUR-SRC-PRI
              WHEN 'WEB'
                 MOVE 100 TO WK-CUR-SRC-PRI
           END-EVALUATE

           EVALUATE CB216I-NEW-SOURCE
              WHEN 'HST'
                 MOVE 400 TO WK-NEW-SRC-PRI
              WHEN 'BAT'
                 MOVE 300 TO WK-NEW-SRC-PRI
              WHEN 'ATM'
                 MOVE 200 TO WK-NEW-SRC-PRI
              WHEN 'WEB'
                 MOVE 100 TO WK-NEW-SRC-PRI
           END-EVALUATE
           .

       1500-DECIDE-UPDATE.
           IF CB216I-CUR-STATUS = 'CL'
              AND CB216I-NEW-STATUS NOT = 'CL'
              MOVE 'N' TO CB216O-UPDATE-FLG
              MOVE WK-CUR-STAT-PRI TO CB216O-PRIORITY
              MOVE '10' TO CB216O-JUDGE-CD
              MOVE '解約済' TO CB216O-REASON-TEXT
              EXIT PARAGRAPH
           END-IF

           IF CB216I-NEW-TIME > CB216I-CUR-TIME
              PERFORM 1510-DECIDE-BY-PRIORITY
              EXIT PARAGRAPH
           END-IF

           IF CB216I-NEW-TIME < CB216I-CUR-TIME
              IF WK-NEW-STAT-PRI > WK-CUR-STAT-PRI
                 PERFORM 1520-ACCEPT-UPDATE
              ELSE
                 MOVE 'N' TO CB216O-UPDATE-FLG
                 MOVE WK-CUR-STAT-PRI TO CB216O-PRIORITY
                 MOVE '20' TO CB216O-JUDGE-CD
                 MOVE '旧時刻優先度低' TO CB216O-REASON-TEXT
              END-IF
              EXIT PARAGRAPH
           END-IF

           IF WK-NEW-SRC-PRI >= WK-CUR-SRC-PRI
              PERFORM 1510-DECIDE-BY-PRIORITY
           ELSE
              MOVE 'N' TO CB216O-UPDATE-FLG
              MOVE WK-CUR-STAT-PRI TO CB216O-PRIORITY
              MOVE '30' TO CB216O-JUDGE-CD
              MOVE '連携元優先度低' TO CB216O-REASON-TEXT
           END-IF
           .

       1510-DECIDE-BY-PRIORITY.
           IF WK-NEW-STAT-PRI >= WK-CUR-STAT-PRI
              PERFORM 1520-ACCEPT-UPDATE
           ELSE
              MOVE 'N' TO CB216O-UPDATE-FLG
              MOVE WK-CUR-STAT-PRI TO CB216O-PRIORITY
              MOVE '40' TO CB216O-JUDGE-CD
              MOVE '状態優先度低' TO CB216O-REASON-TEXT
           END-IF
           .

       1520-ACCEPT-UPDATE.
           MOVE 'Y' TO CB216O-UPDATE-FLG
           MOVE WK-NEW-STAT-PRI TO CB216O-PRIORITY
           MOVE '00' TO CB216O-JUDGE-CD

           EVALUATE CB216I-REASON-CD
              WHEN 'F01'
                 MOVE '不正利用停止' TO CB216O-REASON-TEXT
              WHEN 'F02'
                 MOVE '紛失盗難停止' TO CB216O-REASON-TEXT
              WHEN 'L01'
                 MOVE '法的停止' TO CB216O-REASON-TEXT
              WHEN 'R01'
                 MOVE '本人確認後再開' TO CB216O-REASON-TEXT
              WHEN 'R02'
                 MOVE '停止解除' TO CB216O-REASON-TEXT
              WHEN 'C01'
                 MOVE '延滞停止' TO CB216O-REASON-TEXT
              WHEN 'C99'
                 MOVE '契約解約' TO CB216O-REASON-TEXT
              WHEN OTHER
                 MOVE '通常更新' TO CB216O-REASON-TEXT
           END-EVALUATE
           .
