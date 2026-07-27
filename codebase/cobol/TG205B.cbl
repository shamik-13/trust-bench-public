       IDENTIFICATION DIVISION.
       PROGRAM-ID. TG205B.
      ******************************************************************
      * 変更履歴
      * 版数  年月日    担当     概要
      * 1.00  20240401  信託基盤  初版作成
      * 1.01  20240920  信託基盤  受付拒否ログ編集追加
      * 1.02  20250214  信託基盤  TG182S結果による受付制御明確化
      ******************************************************************
      * 被仕向受信ファイル受付形式検査バッチ
      * 受信ファイルを受付前に形式検査し、受付結果と拒否ログを残す。
      * TG182Sの判定結果だけを後続処理への引渡可否の根拠とする。
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TGINRMF
               ASSIGN TO "TGINRMF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-ST-TGINRMF.

           SELECT TGACPTF
               ASSIGN TO "TGACPTF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-ST-TGACPTF.

           SELECT TGREJLF
               ASSIGN TO "TGREJLF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-ST-TGREJLF.

       DATA DIVISION.
       FILE SECTION.
       FD  TGINRMF
           RECORD CONTAINS 256 CHARACTERS.
           COPY TGINRMFC.

       FD  TGACPTF
           RECORD CONTAINS 128 CHARACTERS.
           COPY TGACPTC.

       FD  TGREJLF
           RECORD CONTAINS 256 CHARACTERS.
           COPY TGREJLFC.

       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID              PIC X(08) VALUE "TG205B".

       01  WS-FILE-STATUS.
           05 WS-ST-TGINRMF           PIC XX VALUE SPACES.
           05 WS-ST-TGACPTF           PIC XX VALUE SPACES.
           05 WS-ST-TGREJLF           PIC XX VALUE SPACES.

       01  WS-OPEN-SWITCHES.
           05 WS-TGINRMF-OPEN-SW      PIC X VALUE "N".
              88 WS-TGINRMF-OPEN            VALUE "Y".
           05 WS-TGACPTF-OPEN-SW      PIC X VALUE "N".
              88 WS-TGACPTF-OPEN            VALUE "Y".
           05 WS-TGREJLF-OPEN-SW      PIC X VALUE "N".
              88 WS-TGREJLF-OPEN            VALUE "Y".

       01  WS-SWITCHES.
           05 WS-EOF-SW               PIC X VALUE "N".
              88 WS-EOF                     VALUE "Y".
              88 WS-NOT-EOF                 VALUE "N".
           05 WS-ABEND-SW             PIC X VALUE "N".
              88 WS-ABEND                   VALUE "Y".
              88 WS-NORMAL                  VALUE "N".

       01  WS-COUNTERS.
           05 WS-IN-CNT               PIC 9(9) VALUE ZERO.
           05 WS-ACPT-CNT             PIC 9(9) VALUE ZERO.
           05 WS-REJ-CNT              PIC 9(9) VALUE ZERO.

       01  WS-DATE-TIME.
           05 WS-CUR-DATE             PIC 9(8) VALUE ZERO.
           05 WS-CUR-TIME             PIC 9(8) VALUE ZERO.
           05 WS-TIMESTAMP            PIC X(14) VALUE SPACES.
           05 WS-TS-NUM REDEFINES WS-TIMESTAMP.
              10 WS-TS-DATE           PIC 9(8).
              10 WS-TS-TIME           PIC 9(6).

       01  WS-REJECT-WORK.
           05 WS-REJ-REASON           PIC X(120) VALUE SPACES.
           05 WS-POSITION-TEXT        PIC X(06) VALUE SPACES.
           05 WS-POSITION-NUM REDEFINES WS-POSITION-TEXT
                                      PIC 9(06).

       01  TG182S-PARM.
           05 TG182S-IN-LENGTH        PIC 9(04) COMP VALUE ZERO.
           05 TG182S-IN-RECORD        PIC X(256) VALUE SPACES.
           05 TG182S-OUT-STATUS       PIC X(02) VALUE SPACES.
              88 TG182S-STATUS-OK           VALUE "OK".
           05 TG182S-OUT-ITEM-NAME    PIC X(30) VALUE SPACES.
           05 TG182S-OUT-POSITION     PIC 9(04) COMP VALUE ZERO.
           05 TG182S-OUT-MESSAGE      PIC X(80) VALUE SPACES.

           COPY LK-REJLOG-PARM.

       PROCEDURE DIVISION.
       MAIN-SECTION.
       0000-MAIN.
           PERFORM 1000-INITIALIZE
           IF WS-NORMAL
               PERFORM 2000-MAIN-PROCESS
                   UNTIL WS-EOF OR WS-ABEND
           END-IF
           PERFORM 9000-FINALIZE
           GOBACK.

       1000-INITIALIZE.
           MOVE 0 TO RETURN-CODE
           SET WS-NOT-EOF TO TRUE
           SET WS-NORMAL TO TRUE
           PERFORM 1100-GET-TIMESTAMP

           OPEN INPUT TGINRMF
           IF WS-ST-TGINRMF NOT = "00"
               DISPLAY "TGINRMF オープン失敗 ST="
                       WS-ST-TGINRMF
               MOVE 12 TO RETURN-CODE
               SET WS-ABEND TO TRUE
           ELSE
               SET WS-TGINRMF-OPEN TO TRUE
           END-IF

           IF WS-NORMAL
               OPEN OUTPUT TGACPTF
               IF WS-ST-TGACPTF NOT = "00"
                   DISPLAY "TGACPTF オープン失敗 ST="
                           WS-ST-TGACPTF
                   MOVE 12 TO RETURN-CODE
                   SET WS-ABEND TO TRUE
               ELSE
                   SET WS-TGACPTF-OPEN TO TRUE
               END-IF
           END-IF

           IF WS-NORMAL
               OPEN OUTPUT TGREJLF
               IF WS-ST-TGREJLF NOT = "00"
                   DISPLAY "TGREJLF オープン失敗 ST="
                           WS-ST-TGREJLF
                   MOVE 12 TO RETURN-CODE
                   SET WS-ABEND TO TRUE
               ELSE
                   SET WS-TGREJLF-OPEN TO TRUE
               END-IF
           END-IF.

       2000-MAIN-PROCESS.
           READ TGINRMF
               AT END
                   SET WS-EOF TO TRUE
               NOT AT END
                   IF WS-ST-TGINRMF = "00"
                       ADD 1 TO WS-IN-CNT
                       PERFORM 3000-CHECK-AND-OUTPUT
                   ELSE
                       DISPLAY "TGINRMF 読込失敗 ST="
                               WS-ST-TGINRMF
                       MOVE 12 TO RETURN-CODE
                       SET WS-ABEND TO TRUE
                   END-IF
           END-READ.

       3000-CHECK-AND-OUTPUT.
           INITIALIZE TG182S-PARM
           MOVE LENGTH OF TGINRMF-REC TO TG182S-IN-LENGTH
           MOVE TGINRMF-REC TO TG182S-IN-RECORD

           CALL "TG182S" USING TG182S-PARM

           IF TG182S-STATUS-OK
               PERFORM 4000-WRITE-ACCEPT
           ELSE
               PERFORM 5000-WRITE-REJECT
           END-IF.

       4000-WRITE-ACCEPT.
           INITIALIZE TGACPTF-REC
           MOVE IR-REMIT-DT       TO AP-ACCEPT-DT
           MOVE IR-CENTER-SEQ     TO AP-CENTER-SEQ
           MOVE "TGINRMF"         TO AP-FILE-ID
           MOVE IR-REMIT-TYPE     TO AP-RECORD-TYPE
           MOVE IR-SENDER-BANK    TO AP-BANK-CD
           MOVE IR-SENDER-BRANCH  TO AP-BRANCH-CD
           MOVE TG182S-OUT-STATUS TO AP-FORMAT-STATUS
           MOVE IR-REMIT-AMT      TO AP-ITEM-AMT
           MOVE WS-TIMESTAMP      TO AP-ACCEPT-TS

           WRITE TGACPTF-REC
           IF WS-ST-TGACPTF = "00"
               ADD 1 TO WS-ACPT-CNT
           ELSE
               DISPLAY "TGACPTF 書込失敗 ST="
                       WS-ST-TGACPTF
               MOVE 12 TO RETURN-CODE
               SET WS-ABEND TO TRUE
           END-IF.

       5000-WRITE-REJECT.
           PERFORM 5100-MAKE-REJECT-REASON
           PERFORM 5200-CALL-REJECT-LOGGER

           IF WS-NORMAL
               INITIALIZE TGREJLF-REC
               MOVE IR-REMIT-DT          TO RJ-REMIT-DT
               MOVE IR-CENTER-SEQ        TO RJ-CENTER-SEQ
               MOVE LK-RL-REASON         TO RJ-REJ-REASON
               MOVE WS-PROGRAM-ID        TO RJ-PROGRAM-ID
               MOVE IR-PAYEE-ACCT-NO     TO RJ-PAYEE-ACCT-NO
               MOVE IR-PAYEE-NAME-KANA   TO RJ-PAYEE-NAME-KANA
               MOVE SPACES               TO RJ-MASTER-NAME-KANA
               MOVE IR-REMIT-AMT         TO RJ-REJ-AMT
               MOVE LK-RL-LOG-TS         TO RJ-LOG-TS

               WRITE TGREJLF-REC
               IF WS-ST-TGREJLF = "00"
                   ADD 1 TO WS-REJ-CNT
               ELSE
                   DISPLAY "TGREJLF 書込失敗 ST="
                           WS-ST-TGREJLF
                   MOVE 12 TO RETURN-CODE
                   SET WS-ABEND TO TRUE
               END-IF
           END-IF.

       5100-MAKE-REJECT-REASON.
           MOVE SPACES TO WS-REJ-REASON
           MOVE ZERO TO WS-POSITION-NUM
           MOVE TG182S-OUT-POSITION TO WS-POSITION-NUM

           STRING
               "受付形式不備 項目="
                   DELIMITED BY SIZE
               TG182S-OUT-ITEM-NAME
                   DELIMITED BY SIZE
               " 位置="
                   DELIMITED BY SIZE
               WS-POSITION-TEXT
                   DELIMITED BY SIZE
               " 内容="
                   DELIMITED BY SIZE
               TG182S-OUT-MESSAGE
                   DELIMITED BY SIZE
               INTO WS-REJ-REASON
           END-STRING.

       5200-CALL-REJECT-LOGGER.
           INITIALIZE LK-REJLOG-PARM
           MOVE WS-PROGRAM-ID  TO LK-RL-PROGRAM-ID
           MOVE WS-REJ-REASON  TO LK-RL-REASON
           MOVE WS-TIMESTAMP   TO LK-RL-LOG-TS

           CALL "TG920S" USING LK-REJLOG-PARM

           IF LK-RL-RET NOT = ZERO
               DISPLAY "TG920S ログ文面編集失敗 RC="
                       LK-RL-RET
               MOVE 8 TO RETURN-CODE
               SET WS-ABEND TO TRUE
           END-IF.

       9000-FINALIZE.
           IF WS-TGINRMF-OPEN
               CLOSE TGINRMF
               IF WS-ST-TGINRMF NOT = "00"
                   DISPLAY "TGINRMF クローズ失敗 ST="
                           WS-ST-TGINRMF
                   MOVE 8 TO RETURN-CODE
               END-IF
           END-IF

           IF WS-TGACPTF-OPEN
               CLOSE TGACPTF
               IF WS-ST-TGACPTF NOT = "00"
                   DISPLAY "TGACPTF クローズ失敗 ST="
                           WS-ST-TGACPTF
                   MOVE 8 TO RETURN-CODE
               END-IF
           END-IF

           IF WS-TGREJLF-OPEN
               CLOSE TGREJLF
               IF WS-ST-TGREJLF NOT = "00"
                   DISPLAY "TGREJLF クローズ失敗 ST="
                           WS-ST-TGREJLF
                   MOVE 8 TO RETURN-CODE
               END-IF
           END-IF

           IF RETURN-CODE = 0
               DISPLAY "TG205B 正常終了 入力=" WS-IN-CNT
                       " 受付=" WS-ACPT-CNT
                       " 拒否=" WS-REJ-CNT
           ELSE
               DISPLAY "TG205B 異常終了 RC=" RETURN-CODE
                       " 入力=" WS-IN-CNT
                       " 受付=" WS-ACPT-CNT
                       " 拒否=" WS-REJ-CNT
           END-IF.

       1100-GET-TIMESTAMP.
           ACCEPT WS-CUR-DATE FROM DATE YYYYMMDD
           ACCEPT WS-CUR-TIME FROM TIME
           MOVE WS-CUR-DATE TO WS-TS-DATE
           MOVE WS-CUR-TIME(1:6) TO WS-TS-TIME.
