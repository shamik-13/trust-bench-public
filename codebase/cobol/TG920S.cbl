       IDENTIFICATION DIVISION.
       PROGRAM-ID. TG920S.
      *---------------------------------------------------------------
      *  変更履歴
      *  版数  年月日      担当                          概要
      *  1.0   平成30.04.01 システム部 為替・対外接続チーム 新規作成
      *  1.1   令和02.10.15 システム部 為替・対外接続チーム 文言整備
      *  1.2   令和05.06.20 システム部 為替・対外接続チーム 保守対応
      *---------------------------------------------------------------
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
       01  WS-CURRENT-DATE.
           05  WS-CUR-YYYYMMDD        PIC X(08).
           05  WS-CUR-HHMMSS          PIC X(06).
           05  WS-CUR-HUNDRED         PIC X(02).
           05  WS-CUR-GMT-SIGN        PIC X(01).
           05  WS-CUR-GMT-HH          PIC X(02).
           05  WS-CUR-GMT-MM          PIC X(02).
      *
       01  WS-EDIT-AREA.
           05  WS-REASON-WORK         PIC X(32).
           05  WS-REASON-VALID        PIC X(01).
               88  REASON-OK          VALUE 'Y'.
               88  REASON-NG          VALUE 'N'.
      *
       01  WS-CONSTANTS.
           05  WC-CALL-PROGRAM        PIC X(06) VALUE 'TG214B'.
           05  WC-RET-NORMAL          PIC X(02) VALUE '00'.
           05  WC-RET-REASON-ERR      PIC X(02) VALUE '12'.
      *
       LINKAGE SECTION.
       COPY LK-REJLOG-PARM.
      *
       PROCEDURE DIVISION USING LK-REJLOG-PARM.
      *
       0000-MAIN SECTION.
      *
           PERFORM 1000-INIT
           PERFORM 2000-CHECK-REASON
      *
           IF REASON-OK
              PERFORM 3000-EDIT-LOG-PARM
              MOVE WC-RET-NORMAL     TO LK-RL-RET
              MOVE 0                 TO RETURN-CODE
           ELSE
              MOVE WC-CALL-PROGRAM   TO LK-RL-PROGRAM-ID
              MOVE WC-RET-REASON-ERR TO LK-RL-RET
              DISPLAY '被仕向拒否理由未設定'
              MOVE 8                 TO RETURN-CODE
           END-IF
      *
           GOBACK.
      *
       1000-INIT SECTION.
      *
           MOVE SPACE TO WS-REASON-WORK
           SET REASON-NG TO TRUE
           MOVE SPACE TO LK-RL-PROGRAM-ID
           MOVE SPACE TO LK-RL-LOG-TS
           MOVE SPACE TO LK-RL-RET
      *
           EXIT.
      *
       2000-CHECK-REASON SECTION.
      *
      *    否認理由コードの妥当性を検証する。
           MOVE LK-RL-REASON TO WS-REASON-WORK
      *
           IF WS-REASON-WORK NOT = SPACE
              SET REASON-OK TO TRUE
           ELSE
              SET REASON-NG TO TRUE
           END-IF
      *
           EXIT.
      *
       3000-EDIT-LOG-PARM SECTION.
      *
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE
      *
           MOVE WC-CALL-PROGRAM TO LK-RL-PROGRAM-ID
           STRING WS-CUR-YYYYMMDD
                  WS-CUR-HHMMSS
             DELIMITED BY SIZE
             INTO LK-RL-LOG-TS
           END-STRING
      *
           EXIT.
