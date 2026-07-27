       IDENTIFICATION DIVISION.
       PROGRAM-ID. CB190B.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CDEXCPF2 ASSIGN TO "CDEXCPF2"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-ST-EXP.
           SELECT CDAPPF ASSIGN TO "CDAPPF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-ST-APP.
           SELECT CDPAYF ASSIGN TO "CDPAYF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-ST-PAY.
           SELECT CDHISTF ASSIGN TO "CDHISTF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS HIS-CARD-NO
               FILE STATUS IS WS-ST-HIS.
           SELECT SORTWK ASSIGN TO "SORTWK".

       DATA DIVISION.
       FILE SECTION.
       FD  CDEXCPF2.
           COPY CDEXCPF2C.
       FD  CDAPPF.
           COPY CDAPPFC.
       FD  CDPAYF.
           COPY CDPAYFC.
       FD  CDHISTF.
           COPY CDHISTC.

       SD  SORTWK.
       01  SORT-REC.
           05 SRT-PROGRAM-ID        PIC X(08).
           05 SRT-EXCEPTION-CD      PIC X(08).
           05 SRT-AMT-BAND          PIC 9(01).
           05 SRT-EXCEPTION-ID      PIC X(20).
           05 SRT-PAY-ID            PIC X(20).
           05 SRT-CARD-NO           PIC X(20).
           05 SRT-EXCEPTION-AMT     PIC S9(13)V99.
           05 SRT-DETECTED-DT       PIC 9(08).

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05 WS-ST-EXP             PIC X(02).
           05 WS-ST-APP             PIC X(02).
           05 WS-ST-PAY             PIC X(02).
           05 WS-ST-HIS             PIC X(02).

       01  WS-SWITCHES.
           05 WS-EOF-EXP            PIC X(01) VALUE "N".
           05 WS-EOF-APP            PIC X(01) VALUE "N".
           05 WS-EOF-PAY            PIC X(01) VALUE "N".
           05 WS-FOUND-APP          PIC X(01) VALUE "N".
           05 WS-FOUND-PAY          PIC X(01) VALUE "N".
           05 WS-HARD-ERR           PIC X(01) VALUE "N".

       01  WS-WORK.
           05 WS-APPLIED-TOTAL      PIC S9(13)V99 VALUE ZERO.
           05 WS-DIFF-AMT           PIC S9(13)V99 VALUE ZERO.
           05 WS-EVENT-TYPE         PIC X(08) VALUE SPACE.
           05 WS-EVENT-AMT          PIC S9(13)V99 VALUE ZERO.
           05 WS-TODAY              PIC 9(08) VALUE ZERO.
           05 WS-HIS-SEQ            PIC 9(09) VALUE ZERO.
           05 WS-EXP-CNT            PIC 9(09) VALUE ZERO.
           05 WS-HIS-CNT            PIC 9(09) VALUE ZERO.
           05 WS-ERR-CNT            PIC 9(09) VALUE ZERO.

       PROCEDURE DIVISION.
       MAIN-SECTION.
           PERFORM 0000-INIT
           IF WS-HARD-ERR = "N"
               SORT SORTWK
                   ON ASCENDING KEY SRT-PROGRAM-ID
                                    SRT-EXCEPTION-CD
                                    SRT-AMT-BAND
                                    SRT-PAY-ID
                   INPUT PROCEDURE  IS 1000-SORT-IN
                   OUTPUT PROCEDURE IS 2000-SORT-OUT
               IF SORT-RETURN NOT = ZERO
                   DISPLAY "CB190B ソート失敗 ST=" SORT-RETURN
                   MOVE 12 TO RETURN-CODE
                   MOVE "Y" TO WS-HARD-ERR
               END-IF
           END-IF
           PERFORM 9000-FINAL
           GOBACK.

       0000-INIT.
           MOVE 0 TO RETURN-CODE
           ACCEPT WS-TODAY FROM DATE YYYYMMDD
           DISPLAY "CB190B 消込例外リスト作成 開始".

       1000-SORT-IN.
           OPEN INPUT CDEXCPF2
           IF WS-ST-EXP NOT = "00"
               DISPLAY "CDEXCPF2 オープン失敗 ST=" WS-ST-EXP
               MOVE 12 TO RETURN-CODE
               MOVE "Y" TO WS-HARD-ERR
           ELSE
               PERFORM UNTIL WS-EOF-EXP = "Y"
                   READ CDEXCPF2
                       AT END
                           MOVE "Y" TO WS-EOF-EXP
                       NOT AT END
                           PERFORM 1100-RELEASE-EXP
                   END-READ
                   IF WS-ST-EXP NOT = "00" AND WS-ST-EXP NOT = "10"
                       DISPLAY "CDEXCPF2 読込失敗 ST=" WS-ST-EXP
                       MOVE 12 TO RETURN-CODE
                       MOVE "Y" TO WS-HARD-ERR
                       MOVE "Y" TO WS-EOF-EXP
                   END-IF
               END-PERFORM
               CLOSE CDEXCPF2
               IF WS-ST-EXP NOT = "00"
                   DISPLAY "CDEXCPF2 クローズ失敗 ST=" WS-ST-EXP
                   MOVE 8 TO RETURN-CODE
                   MOVE "Y" TO WS-HARD-ERR
               END-IF
           END-IF.

       1100-RELEASE-EXP.
           ADD 1 TO WS-EXP-CNT
           MOVE EXP-DETECTED-PROGRAM TO SRT-PROGRAM-ID
           MOVE EXP-EXCEPTION-CD     TO SRT-EXCEPTION-CD
           MOVE EXP-EXCEPTION-ID     TO SRT-EXCEPTION-ID
           MOVE EXP-PAY-ID           TO SRT-PAY-ID
           MOVE EXP-CARD-NO          TO SRT-CARD-NO
           MOVE EXP-EXCEPTION-AMT    TO SRT-EXCEPTION-AMT
           MOVE EXP-DETECTED-DT      TO SRT-DETECTED-DT
           EVALUATE TRUE
               WHEN EXP-EXCEPTION-AMT < 10000
                   MOVE 1 TO SRT-AMT-BAND
               WHEN EXP-EXCEPTION-AMT < 100000
                   MOVE 2 TO SRT-AMT-BAND
               WHEN EXP-EXCEPTION-AMT < 1000000
                   MOVE 3 TO SRT-AMT-BAND
               WHEN OTHER
                   MOVE 4 TO SRT-AMT-BAND
           END-EVALUATE
           RELEASE SORT-REC.

       2000-SORT-OUT.
           OPEN I-O CDHISTF
           IF WS-ST-HIS NOT = "00"
               DISPLAY "CDHISTF オープン失敗 ST=" WS-ST-HIS
               MOVE 12 TO RETURN-CODE
               MOVE "Y" TO WS-HARD-ERR
           ELSE
               RETURN SORTWK
                   AT END
                       MOVE "Y" TO WS-EOF-EXP
                   NOT AT END
                       MOVE "N" TO WS-EOF-EXP
               END-RETURN
               PERFORM UNTIL WS-EOF-EXP = "Y" OR WS-HARD-ERR = "Y"
                   PERFORM 2100-BUILD-EVENT
                   IF WS-HARD-ERR = "N"
                       PERFORM 2500-WRITE-HISTORY
                   END-IF
                   RETURN SORTWK
                       AT END
                           MOVE "Y" TO WS-EOF-EXP
                       NOT AT END
                           CONTINUE
                   END-RETURN
               END-PERFORM
               CLOSE CDHISTF
               IF WS-ST-HIS NOT = "00" AND WS-HARD-ERR = "N"
                   DISPLAY "CDHISTF クローズ失敗 ST=" WS-ST-HIS
                   MOVE 8 TO RETURN-CODE
                   MOVE "Y" TO WS-HARD-ERR
               END-IF
           END-IF.

       2100-BUILD-EVENT.
           MOVE "N" TO WS-FOUND-APP WS-FOUND-PAY
           MOVE ZERO TO WS-APPLIED-TOTAL WS-DIFF-AMT WS-EVENT-AMT
           PERFORM 2200-FIND-PAY
           IF WS-HARD-ERR = "N"
               PERFORM 2300-FIND-APP
           END-IF
           IF WS-HARD-ERR = "N"
               EVALUATE TRUE
                   WHEN WS-FOUND-PAY = "N"
                       MOVE "EX-NOPY" TO WS-EVENT-TYPE
                       MOVE SRT-EXCEPTION-AMT TO WS-EVENT-AMT
                       DISPLAY "入金明細なし PAY-ID=" SRT-PAY-ID
                   WHEN WS-FOUND-APP = "N"
                       MOVE "EX-UNAP" TO WS-EVENT-TYPE
                       MOVE PY-PAY-AMT TO WS-EVENT-AMT
                       DISPLAY "未処理入金 PAY-ID=" SRT-PAY-ID
                   WHEN AP-APP-STATUS = "O"
                       MOVE "EX-OVER" TO WS-EVENT-TYPE
                       MOVE AP-REMAIN-AMT TO WS-EVENT-AMT
                       DISPLAY "過入金検出 PAY-ID=" SRT-PAY-ID
                   WHEN WS-DIFF-AMT NOT = ZERO
                       MOVE "EX-DIFF" TO WS-EVENT-TYPE
                       MOVE WS-DIFF-AMT TO WS-EVENT-AMT
                       DISPLAY "金額不一致 PAY-ID=" SRT-PAY-ID
                   WHEN OTHER
                       MOVE "EX-CHK" TO WS-EVENT-TYPE
                       MOVE SRT-EXCEPTION-AMT TO WS-EVENT-AMT
                       DISPLAY "確認対象例外 PAY-ID=" SRT-PAY-ID
               END-EVALUATE
           END-IF.

       2200-FIND-PAY.
           MOVE "N" TO WS-EOF-PAY
           OPEN INPUT CDPAYF
           IF WS-ST-PAY NOT = "00"
               DISPLAY "CDPAYF オープン失敗 ST=" WS-ST-PAY
               MOVE 12 TO RETURN-CODE
               MOVE "Y" TO WS-HARD-ERR
           ELSE
               PERFORM UNTIL WS-EOF-PAY = "Y" OR WS-FOUND-PAY = "Y"
                   READ CDPAYF
                       AT END
                           MOVE "Y" TO WS-EOF-PAY
                       NOT AT END
                           IF PY-PAY-ID = SRT-PAY-ID
                               MOVE "Y" TO WS-FOUND-PAY
                               IF PY-PAY-METHOD NOT = "10"
                                  AND PY-PAY-METHOD NOT = "20"
                                  AND PY-PAY-METHOD NOT = "30"
                                   DISPLAY "入金方法不正 PAY-ID="
                                           PY-PAY-ID
                                   MOVE 8 TO RETURN-CODE
                                   MOVE "Y" TO WS-HARD-ERR
                               END-IF
                           END-IF
                   END-READ
               END-PERFORM
               CLOSE CDPAYF
               IF WS-ST-PAY NOT = "00" AND WS-HARD-ERR = "N"
                   DISPLAY "CDPAYF クローズ失敗 ST=" WS-ST-PAY
                   MOVE 8 TO RETURN-CODE
                   MOVE "Y" TO WS-HARD-ERR
               END-IF
           END-IF.

       2300-FIND-APP.
           MOVE "N" TO WS-EOF-APP
           OPEN INPUT CDAPPF
           IF WS-ST-APP NOT = "00"
               DISPLAY "CDAPPF オープン失敗 ST=" WS-ST-APP
               MOVE 12 TO RETURN-CODE
               MOVE "Y" TO WS-HARD-ERR
           ELSE
               PERFORM UNTIL WS-EOF-APP = "Y" OR WS-FOUND-APP = "Y"
                   READ CDAPPF
                       AT END
                           MOVE "Y" TO WS-EOF-APP
                       NOT AT END
                           IF AP-PAY-ID = SRT-PAY-ID
                               MOVE "Y" TO WS-FOUND-APP
                               PERFORM 2350-CHECK-APP
                           END-IF
                   END-READ
               END-PERFORM
               CLOSE CDAPPF
               IF WS-ST-APP NOT = "00" AND WS-HARD-ERR = "N"
                   DISPLAY "CDAPPF クローズ失敗 ST=" WS-ST-APP
                   MOVE 8 TO RETURN-CODE
                   MOVE "Y" TO WS-HARD-ERR
               END-IF
           END-IF.

       2350-CHECK-APP.
           IF AP-APP-STATUS NOT = "F"
              AND AP-APP-STATUS NOT = "P"
              AND AP-APP-STATUS NOT = "O"
              AND AP-APP-STATUS NOT = "S"
               DISPLAY "消込状態不正 PAY-ID=" AP-PAY-ID
               MOVE 8 TO RETURN-CODE
               MOVE "Y" TO WS-HARD-ERR
           ELSE
               COMPUTE WS-APPLIED-TOTAL =
                   AP-APPLIED-FEE-AMT
                 + AP-APPLIED-INT-AMT
                 + AP-APPLIED-PRIN-AMT
                 + AP-REMAIN-AMT
               IF WS-FOUND-PAY = "Y"
                   COMPUTE WS-DIFF-AMT =
                       PY-PAY-AMT - WS-APPLIED-TOTAL
               END-IF
           END-IF.

       2500-WRITE-HISTORY.
           MOVE SRT-CARD-NO TO HIS-CARD-NO
           READ CDHISTF
               INVALID KEY
                   MOVE ZERO TO WS-HIS-SEQ
                   MOVE SRT-CARD-NO TO HIS-CARD-NO
                   MOVE SRT-PAY-ID TO HIS-PAY-ID
                   MOVE 1 TO HIS-EVENT-SEQ
                   MOVE WS-EVENT-TYPE TO HIS-EVENT-TYPE
                   MOVE WS-EVENT-AMT TO HIS-EVENT-AMT
                   MOVE WS-TODAY TO HIS-EVENT-DT
                   MOVE "CB190B" TO HIS-SOURCE-PROGRAM
                   WRITE CDHISTF-REC
                       INVALID KEY
                           DISPLAY "CDHISTF 書込失敗 ST="
                                   WS-ST-HIS
                           MOVE 12 TO RETURN-CODE
                           MOVE "Y" TO WS-HARD-ERR
                       NOT INVALID KEY
                           ADD 1 TO WS-HIS-CNT
                   END-WRITE
               NOT INVALID KEY
                   MOVE HIS-EVENT-SEQ TO WS-HIS-SEQ
                   ADD 1 TO WS-HIS-SEQ
                   MOVE SRT-PAY-ID TO HIS-PAY-ID
                   MOVE WS-HIS-SEQ TO HIS-EVENT-SEQ
                   MOVE WS-EVENT-TYPE TO HIS-EVENT-TYPE
                   MOVE WS-EVENT-AMT TO HIS-EVENT-AMT
                   MOVE WS-TODAY TO HIS-EVENT-DT
                   MOVE "CB190B" TO HIS-SOURCE-PROGRAM
                   REWRITE CDHISTF-REC
                       INVALID KEY
                           DISPLAY "CDHISTF 更新失敗 ST="
                                   WS-ST-HIS
                           MOVE 12 TO RETURN-CODE
                           MOVE "Y" TO WS-HARD-ERR
                       NOT INVALID KEY
                           ADD 1 TO WS-HIS-CNT
                   END-REWRITE
           END-READ
           IF WS-ST-HIS NOT = "00" AND WS-ST-HIS NOT = "23"
              AND WS-HARD-ERR = "N"
               DISPLAY "CDHISTF 入出力異常 ST=" WS-ST-HIS
               MOVE 12 TO RETURN-CODE
               MOVE "Y" TO WS-HARD-ERR
           END-IF.

       9000-FINAL.
           IF WS-HARD-ERR = "N"
               DISPLAY "CB190B 正常終了 例外件数=" WS-EXP-CNT
                       " 履歴件数=" WS-HIS-CNT
               MOVE 0 TO RETURN-CODE
           ELSE
               ADD 1 TO WS-ERR-CNT
               DISPLAY "CB190B 異常終了 エラー件数=" WS-ERR-CNT
           END-IF.
