       IDENTIFICATION DIVISION.
       PROGRAM-ID. CT280B.
      *
      *---------------------------------------------------------------*
      *  変更履歴                                                     *
      *  版数  年月日    担当      概要                               *
      *  0.1   20250214  開発一課  初版作成                           *
      *  0.2   20250303  開発一課  再送対象抽出条件追加               *
      *  0.3   20250318  運用保守  履歴出力項目見直し                 *
      *---------------------------------------------------------------*
      *  資金集中トリガ再送バッチ                                     *
      *  前回エラー分を抽出し、指図状態と照合して再受付へ戻す。       *
      *---------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CCFCTF ASSIGN TO "CCFCTF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-CCFCTF.
           SELECT CCINSF ASSIGN TO "CCINSF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS IN-INS-ID
               FILE STATUS IS WS-FS-CCINSF.
           SELECT CCERRF ASSIGN TO "CCERRF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-CCERRF.
           SELECT CCCHGF ASSIGN TO "CCCHGF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-CCCHGF.

       DATA DIVISION.
       FILE SECTION.
       FD  CCFCTF.
           COPY CCFCTFC.
       FD  CCINSF.
           COPY CCINSC.
       FD  CCERRF.
           COPY CCERRC.
       FD  CCCHGF.
           COPY CCCHGC.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05 WS-FS-CCFCTF          PIC XX VALUE SPACES.
           05 WS-FS-CCINSF          PIC XX VALUE SPACES.
           05 WS-FS-CCERRF          PIC XX VALUE SPACES.
           05 WS-FS-CCCHGF          PIC XX VALUE SPACES.

       01  WS-CONSTANTS.
           05 WS-PGM-ID             PIC X(08) VALUE "CT280B".
           05 WS-ERR-RETRY-KBN      PIC X(02) VALUE "RT".
           05 WS-ST-KAKUTEI         PIC X(02) VALUE "01".
           05 WS-ST-HORYU           PIC X(02) VALUE "08".
           05 WS-INS-ST-UKETSUKE    PIC X(02) VALUE "01".
           05 WS-CHG-KBN-SAISOU     PIC X(02) VALUE "RS".
           05 WS-ZERO-AMT           PIC S9(15)V99 VALUE ZERO.
           05 WS-MAX-ERR            PIC 9(05) VALUE 05000.
           05 WS-MAX-INS            PIC 9(05) VALUE 05000.

       01  WS-SWITCHES.
           05 WS-EOF-ERR            PIC X VALUE "N".
              88 EOF-ERR                 VALUE "Y".
           05 WS-EOF-INS            PIC X VALUE "N".
              88 EOF-INS                 VALUE "Y".
           05 WS-EOF-FCT            PIC X VALUE "N".
              88 EOF-FCT                 VALUE "Y".
           05 WS-HARD-ERROR         PIC X VALUE "N".
              88 HARD-ERROR              VALUE "Y".

       01  WS-COUNTERS.
           05 WS-ERR-CNT            PIC 9(05) VALUE ZERO.
           05 WS-INS-CNT            PIC 9(05) VALUE ZERO.
           05 WS-FCT-READ-CNT       PIC 9(07) VALUE ZERO.
           05 WS-UPD-CNT            PIC 9(07) VALUE ZERO.
           05 WS-SKIP-CNT           PIC 9(07) VALUE ZERO.
           05 WS-IDX                PIC 9(05) VALUE ZERO.
           05 WS-IDX2               PIC 9(05) VALUE ZERO.

       01  WS-WORK.
           05 WS-CURRENT-DATE.
              10 WS-CUR-YYYY        PIC 9(04).
              10 WS-CUR-MM          PIC 9(02).
              10 WS-CUR-DD          PIC 9(02).
           05 WS-CHANGE-ID-NUM      PIC 9(10) VALUE ZERO.
           05 WS-CHANGE-ID-WK       PIC X(20) VALUE SPACES.
           05 WS-FOUND-ERR          PIC X VALUE "N".
              88 FOUND-ERR               VALUE "Y".
           05 WS-FOUND-INS          PIC X VALUE "N".
              88 FOUND-INS               VALUE "Y".
           05 WS-ALLOW-UPDATE       PIC X VALUE "N".
              88 ALLOW-UPDATE            VALUE "Y".
           05 WS-BEFORE-STATUS      PIC X(02) VALUE SPACES.

       01  WS-ERR-TABLE.
           05 WS-ERR-ENTRY OCCURS 5000 TIMES.
              10 WT-ERR-FCT-ID      PIC X(20).
              10 WT-ERR-BASE-DT     PIC X(08).
              10 WT-ERR-KBN         PIC X(02).

       01  WS-INS-TABLE.
           05 WS-INS-ENTRY OCCURS 5000 TIMES.
              10 WT-INS-FCT-ID      PIC X(20).
              10 WT-INS-STATUS      PIC X(02).
              10 WT-INS-AMT         PIC S9(15)V99.

       PROCEDURE DIVISION.
       MAIN-RTN.
           MOVE 0 TO RETURN-CODE
           ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD
           PERFORM OPEN-FILES
           IF NOT HARD-ERROR
              PERFORM LOAD-ERROR-FILE
              PERFORM LOAD-INSTRUCTION-FILE
              PERFORM UPDATE-FCT-FILE
           END-IF
           PERFORM CLOSE-FILES
           IF HARD-ERROR
              MOVE 8 TO RETURN-CODE
           ELSE
              DISPLAY "CT280B 正常終了 読込="
                      WS-FCT-READ-CNT " 更新=" WS-UPD-CNT
                      " 見送り=" WS-SKIP-CNT
              MOVE 0 TO RETURN-CODE
           END-IF
           GOBACK.

       OPEN-FILES.
           OPEN INPUT CCERRF
           IF WS-FS-CCERRF NOT = "00"
              DISPLAY "CCERRF オープン失敗 ST=" WS-FS-CCERRF
              SET HARD-ERROR TO TRUE
           END-IF
           OPEN INPUT CCINSF
           IF WS-FS-CCINSF NOT = "00"
              DISPLAY "CCINSF オープン失敗 ST=" WS-FS-CCINSF
              SET HARD-ERROR TO TRUE
           END-IF
           OPEN I-O CCFCTF
           IF WS-FS-CCFCTF NOT = "00"
              DISPLAY "CCFCTF オープン失敗 ST=" WS-FS-CCFCTF
              SET HARD-ERROR TO TRUE
           END-IF
           OPEN EXTEND CCCHGF
           IF WS-FS-CCCHGF NOT = "00"
              DISPLAY "CCCHGF オープン失敗 ST=" WS-FS-CCCHGF
              SET HARD-ERROR TO TRUE
           END-IF.

       LOAD-ERROR-FILE.
           PERFORM UNTIL EOF-ERR OR HARD-ERROR
              READ CCERRF
                 AT END
                    SET EOF-ERR TO TRUE
                 NOT AT END
                    IF ER-PGM-ID = WS-PGM-ID
                       AND ER-ERROR-KBN = WS-ERR-RETRY-KBN
                       IF WS-ERR-CNT < WS-MAX-ERR
                          ADD 1 TO WS-ERR-CNT
                          MOVE ER-RECORD-KEY
                            TO WT-ERR-FCT-ID(WS-ERR-CNT)
                          MOVE ER-BASE-DT
                            TO WT-ERR-BASE-DT(WS-ERR-CNT)
                          MOVE ER-ERROR-KBN
                            TO WT-ERR-KBN(WS-ERR-CNT)
                       ELSE
                          DISPLAY "CCERRF 対象件数上限超過"
                          SET HARD-ERROR TO TRUE
                       END-IF
                    END-IF
              END-READ
              IF WS-FS-CCERRF NOT = "00" AND WS-FS-CCERRF NOT = "10"
                 DISPLAY "CCERRF 読込失敗 ST=" WS-FS-CCERRF
                 SET HARD-ERROR TO TRUE
              END-IF
           END-PERFORM.

       LOAD-INSTRUCTION-FILE.
           MOVE LOW-VALUE TO IN-INS-ID
           START CCINSF KEY IS >= IN-INS-ID
              INVALID KEY
                 SET EOF-INS TO TRUE
              NOT INVALID KEY
                 CONTINUE
           END-START
           IF WS-FS-CCINSF NOT = "00" AND WS-FS-CCINSF NOT = "23"
              DISPLAY "CCINSF 開始位置設定失敗 ST=" WS-FS-CCINSF
              SET HARD-ERROR TO TRUE
           END-IF
           PERFORM UNTIL EOF-INS OR HARD-ERROR
              READ CCINSF NEXT RECORD
                 AT END
                    SET EOF-INS TO TRUE
                 NOT AT END
                    IF WS-INS-CNT < WS-MAX-INS
                       ADD 1 TO WS-INS-CNT
                       MOVE IN-FCT-ID
                         TO WT-INS-FCT-ID(WS-INS-CNT)
                       MOVE IN-INSTR-STATUS-KBN
                         TO WT-INS-STATUS(WS-INS-CNT)
                       MOVE IN-INSTR-AMT
                         TO WT-INS-AMT(WS-INS-CNT)
                    ELSE
                       DISPLAY "CCINSF 指図件数上限超過"
                       SET HARD-ERROR TO TRUE
                    END-IF
              END-READ
              IF WS-FS-CCINSF NOT = "00" AND WS-FS-CCINSF NOT = "10"
                 DISPLAY "CCINSF 読込失敗 ST=" WS-FS-CCINSF
                 SET HARD-ERROR TO TRUE
              END-IF
           END-PERFORM.

       UPDATE-FCT-FILE.
           PERFORM UNTIL EOF-FCT OR HARD-ERROR
              READ CCFCTF
                 AT END
                    SET EOF-FCT TO TRUE
                 NOT AT END
                    ADD 1 TO WS-FCT-READ-CNT
                    PERFORM JUDGE-FCT
                    IF ALLOW-UPDATE
                       PERFORM WRITE-CHANGE-HISTORY
                       IF NOT HARD-ERROR
                          MOVE WS-ST-KAKUTEI TO FC-FCT-STATUS-KBN
                          REWRITE CCFCTF-REC
                          IF WS-FS-CCFCTF = "00"
                             ADD 1 TO WS-UPD-CNT
                          ELSE
                             DISPLAY "CCFCTF 更新失敗 ST="
                                     WS-FS-CCFCTF
                             SET HARD-ERROR TO TRUE
                          END-IF
                       END-IF
                    ELSE
                       ADD 1 TO WS-SKIP-CNT
                    END-IF
              END-READ
              IF WS-FS-CCFCTF NOT = "00" AND WS-FS-CCFCTF NOT = "10"
                 DISPLAY "CCFCTF 読込失敗 ST=" WS-FS-CCFCTF
                 SET HARD-ERROR TO TRUE
              END-IF
           END-PERFORM.

       JUDGE-FCT.
           MOVE "N" TO WS-FOUND-ERR
           MOVE "N" TO WS-FOUND-INS
           MOVE "N" TO WS-ALLOW-UPDATE
           MOVE FC-FCT-STATUS-KBN TO WS-BEFORE-STATUS
           PERFORM VARYING WS-IDX FROM 1 BY 1
             UNTIL WS-IDX > WS-ERR-CNT OR FOUND-ERR
              IF WT-ERR-FCT-ID(WS-IDX) = FC-FCT-ID
                 SET FOUND-ERR TO TRUE
              END-IF
           END-PERFORM
           IF FOUND-ERR
              PERFORM VARYING WS-IDX2 FROM 1 BY 1
                UNTIL WS-IDX2 > WS-INS-CNT OR FOUND-INS
                 IF WT-INS-FCT-ID(WS-IDX2) = FC-FCT-ID
                    SET FOUND-INS TO TRUE
                 END-IF
              END-PERFORM
           END-IF
           IF FOUND-ERR AND FOUND-INS
              IF FC-CONC-AMT > WS-ZERO-AMT
                 AND FC-FCT-STATUS-KBN = WS-ST-HORYU
                 AND WT-INS-STATUS(WS-IDX2) = WS-INS-ST-UKETSUKE
                 AND WT-INS-AMT(WS-IDX2) = FC-CONC-AMT
                    SET ALLOW-UPDATE TO TRUE
              END-IF
           END-IF.

       WRITE-CHANGE-HISTORY.
           ADD 1 TO WS-CHANGE-ID-NUM
           MOVE SPACES TO CCCHGF-REC
           MOVE WS-CHANGE-ID-NUM TO WS-CHANGE-ID-WK
           STRING WS-PGM-ID DELIMITED BY SIZE
                  WS-CURRENT-DATE DELIMITED BY SIZE
                  WS-CHANGE-ID-WK DELIMITED BY SIZE
             INTO CH-CHANGE-ID
           END-STRING
           MOVE FC-FCT-ID TO CH-FCT-ID
           MOVE WS-CURRENT-DATE TO CH-CHANGE-DT
           MOVE WS-CHG-KBN-SAISOU TO CH-CHANGE-KBN
           MOVE WS-BEFORE-STATUS TO CH-BEFORE-STATUS-KBN
           MOVE WS-ST-KAKUTEI TO CH-AFTER-STATUS-KBN
           WRITE CCCHGF-REC
           IF WS-FS-CCCHGF NOT = "00"
              DISPLAY "CCCHGF 履歴出力失敗 ST=" WS-FS-CCCHGF
              SET HARD-ERROR TO TRUE
           END-IF.

       CLOSE-FILES.
           CLOSE CCERRF
           IF WS-FS-CCERRF NOT = "00"
              DISPLAY "CCERRF クローズ失敗 ST=" WS-FS-CCERRF
              SET HARD-ERROR TO TRUE
           END-IF
           CLOSE CCINSF
           IF WS-FS-CCINSF NOT = "00"
              DISPLAY "CCINSF クローズ失敗 ST=" WS-FS-CCINSF
              SET HARD-ERROR TO TRUE
           END-IF
           CLOSE CCFCTF
           IF WS-FS-CCFCTF NOT = "00"
              DISPLAY "CCFCTF クローズ失敗 ST=" WS-FS-CCFCTF
              SET HARD-ERROR TO TRUE
           END-IF
           CLOSE CCCHGF
           IF WS-FS-CCCHGF NOT = "00"
              DISPLAY "CCCHGF クローズ失敗 ST=" WS-FS-CCCHGF
              SET HARD-ERROR TO TRUE
           END-IF.
