       IDENTIFICATION DIVISION.
       PROGRAM-ID. CB810B.
      *
      *===============================================================*
      * 変更履歴
      * 版数   年月日    担当    概要
      * 1.00   20260210  B01     初版作成
      * 1.01   20260418  B02     海外利用精算編集追加
      * 1.02   20260612  B03     C2利息起算日・手数料判定追加
      *===============================================================*
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CDTXNF ASSIGN TO "CDTXNF"
             ORGANIZATION IS INDEXED
             ACCESS MODE IS SEQUENTIAL
             RECORD KEY IS TX-TXN-ID
             FILE STATUS IS WS-CDTXNF-STATUS.
           SELECT CDOVSF ASSIGN TO "CDOVSF"
             ORGANIZATION IS SEQUENTIAL
             FILE STATUS IS WS-CDOVSF-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  CDTXNF.
           COPY CDTXNFC.
       FD  CDOVSF.
           COPY CDOVSFC.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05 WS-CDTXNF-STATUS        PIC XX VALUE SPACES.
           05 WS-CDOVSF-STATUS        PIC XX VALUE SPACES.

       01  WS-SWITCHES.
           05 WS-EOF-SW               PIC X VALUE 'N'.
              88 END-OF-CDTXNF              VALUE 'Y'.
              88 NOT-END-OF-CDTXNF          VALUE 'N'.
           05 WS-HARD-ERROR-SW        PIC X VALUE 'N'.
              88 HARD-ERROR                 VALUE 'Y'.
              88 NO-HARD-ERROR              VALUE 'N'.
           05 WS-WRITE-SW             PIC X VALUE 'N'.
              88 WRITE-REQUIRED             VALUE 'Y'.
              88 WRITE-SKIPPED              VALUE 'N'.

       01  WS-COUNTERS.
           05 WS-READ-CNT             PIC 9(9) COMP-5 VALUE 0.
           05 WS-WRITE-CNT            PIC 9(9) COMP-5 VALUE 0.
           05 WS-SKIP-CNT             PIC 9(9) COMP-5 VALUE 0.
           05 WS-ERR-CNT              PIC 9(9) COMP-5 VALUE 0.

       01  WS-WORK-AREA.
           05 WS-PROGRAM-ID           PIC X(8) VALUE 'CB810B'.
           05 WS-STD-GRACE-DAYS       PIC 9(3) VALUE 55.
           05 WS-FEE-AMT              PIC 9(9) VALUE 0.
           05 WS-GRACE-DATE           PIC 9(8) VALUE 0.
           05 WS-DATE-INT             PIC 9(8) VALUE 0.
           05 WS-DATE-INTEGER         PIC 9(9) VALUE 0.
           05 WS-DATE-YMD             PIC 9(8) VALUE 0.
           05 WS-TXN-AMT              PIC 9(11) VALUE 0.
           05 WS-SETL-AMT             PIC 9(11) VALUE 0.
           05 WS-REASON               PIC X(60) VALUE SPACES.

       01  WS-DATE-CHK.
           05 WS-CHK-YYYY             PIC 9(4) VALUE 0.
           05 WS-CHK-MM               PIC 9(2) VALUE 0.
           05 WS-CHK-DD               PIC 9(2) VALUE 0.

       PROCEDURE DIVISION.
       MAIN-SECTION.
           PERFORM 0000-INIT
           PERFORM 1000-MAIN UNTIL END-OF-CDTXNF OR HARD-ERROR
           PERFORM 9000-FINAL
           GOBACK
           .

       0000-INIT.
           MOVE 0 TO RETURN-CODE
           SET NOT-END-OF-CDTXNF TO TRUE
           SET NO-HARD-ERROR TO TRUE
           OPEN INPUT CDTXNF
           IF WS-CDTXNF-STATUS NOT = '00'
              MOVE 'CDTXNF オープン失敗' TO WS-REASON
              PERFORM 8000-HARD-ERROR
           END-IF
           IF NO-HARD-ERROR
              OPEN OUTPUT CDOVSF
              IF WS-CDOVSF-STATUS NOT = '00'
                 MOVE 'CDOVSF オープン失敗' TO WS-REASON
                 PERFORM 8000-HARD-ERROR
              END-IF
           END-IF
           IF NO-HARD-ERROR
              PERFORM 1100-READ-CDTXNF
           END-IF
           .

       1000-MAIN.
           SET WRITE-SKIPPED TO TRUE
           PERFORM 2000-VALIDATE-TXN
           IF WRITE-REQUIRED
              PERFORM 3000-EDIT-SETTLEMENT
              IF NO-HARD-ERROR
                 PERFORM 4000-WRITE-CDOVSF
              END-IF
           ELSE
              ADD 1 TO WS-SKIP-CNT
           END-IF
           IF NO-HARD-ERROR
              PERFORM 1100-READ-CDTXNF
           END-IF
           .

       1100-READ-CDTXNF.
           READ CDTXNF
              AT END
                 SET END-OF-CDTXNF TO TRUE
              NOT AT END
                 ADD 1 TO WS-READ-CNT
                 IF WS-CDTXNF-STATUS NOT = '00'
                    MOVE 'CDTXNF 読込失敗' TO WS-REASON
                    PERFORM 8000-HARD-ERROR
                 END-IF
           END-READ
           .

       2000-VALIDATE-TXN.
           IF TX-AUTH-CD = SPACES
              MOVE '承認番号なし' TO WS-REASON
              PERFORM 2100-SKIP-LOG
           ELSE
              IF TX-TXN-ID = SPACES
                 MOVE '取引番号なし' TO WS-REASON
                 PERFORM 2100-SKIP-LOG
              ELSE
                 IF TX-CARD-NO = SPACES
                    MOVE 'カード番号なし' TO WS-REASON
                    PERFORM 2100-SKIP-LOG
                 ELSE
                    IF TX-TXN-AMT = ZERO
                       MOVE '利用金額ゼロ' TO WS-REASON
                       PERFORM 2100-SKIP-LOG
                    ELSE
                       PERFORM 2200-CHECK-DOMAIN
                    END-IF
                 END-IF
              END-IF
           END-IF
           .

       2100-SKIP-LOG.
           ADD 1 TO WS-ERR-CNT
           DISPLAY 'CB810B 入力除外 TX=' TX-TXN-ID
                   ' 理由=' WS-REASON
           SET WRITE-SKIPPED TO TRUE
           .

       2200-CHECK-DOMAIN.
           EVALUATE TRUE
              WHEN TX-TXN-KBN = 'P1'
              WHEN TX-TXN-KBN = 'P2'
              WHEN TX-TXN-KBN = 'C2'
                 CONTINUE
              WHEN OTHER
                 MOVE '精算対象外取引区分' TO WS-REASON
                 PERFORM 2100-SKIP-LOG
           END-EVALUATE

           IF WS-REASON NOT = '精算対象外取引区分'
              EVALUATE TRUE
                 WHEN TX-CHANNEL-KBN = '01'
                 WHEN TX-CHANNEL-KBN = '02'
                 WHEN TX-CHANNEL-KBN = '03'
                 WHEN TX-CHANNEL-KBN = '04'
                 WHEN TX-CHANNEL-KBN = '05'
                    PERFORM 2300-CHECK-DATE
                 WHEN OTHER
                    MOVE '利用チャネル区分不正' TO WS-REASON
                    PERFORM 2100-SKIP-LOG
              END-EVALUATE
           END-IF
           .

       2300-CHECK-DATE.
           MOVE TX-TXN-DT TO WS-DATE-CHK
           IF WS-CHK-YYYY < 2000
              MOVE '取引日年不正' TO WS-REASON
              PERFORM 2100-SKIP-LOG
           ELSE
              IF WS-CHK-MM < 1 OR WS-CHK-MM > 12
                 MOVE '取引日月不正' TO WS-REASON
                 PERFORM 2100-SKIP-LOG
              ELSE
                 IF WS-CHK-DD < 1 OR WS-CHK-DD > 31
                    MOVE '取引日日不正' TO WS-REASON
                    PERFORM 2100-SKIP-LOG
                 ELSE
                    SET WRITE-REQUIRED TO TRUE
                 END-IF
              END-IF
           END-IF
           .

       3000-EDIT-SETTLEMENT.
           INITIALIZE CDOVSF-REC
           MOVE TX-TXN-ID       TO OV-TXN-ID
           MOVE TX-CARD-NO      TO OV-CARD-NO
           MOVE TX-TXN-KBN      TO OV-TXN-KBN
           MOVE TX-TXN-AMT      TO WS-TXN-AMT
           MOVE ZERO            TO WS-FEE-AMT

           EVALUATE TX-TXN-KBN
              WHEN 'C2'
                 MOVE TX-TXN-DT TO OV-INT-START-DT
                 MOVE 'FA'      TO OV-FEE-KBN
                 PERFORM 3100-CALC-CASH-FEE
              WHEN 'P1'
              WHEN 'P2'
                 MOVE '00'      TO OV-FEE-KBN
                 MOVE ZERO      TO WS-FEE-AMT
                 PERFORM 3200-CALC-GRACE-DATE
                 MOVE WS-GRACE-DATE TO OV-INT-START-DT
              WHEN OTHER
                 MOVE '取引区分編集不正' TO WS-REASON
                 PERFORM 8000-HARD-ERROR
           END-EVALUATE

           IF NO-HARD-ERROR
              COMPUTE WS-SETL-AMT = WS-TXN-AMT + WS-FEE-AMT
              MOVE WS-FEE-AMT    TO OV-FEE-AMT
              MOVE WS-SETL-AMT   TO OV-SETL-AMT
              MOVE 'D'           TO OV-SETL-KBN
              MOVE WS-PROGRAM-ID TO OV-PROGRAM-ID
           END-IF
           .

       3100-CALC-CASH-FEE.
           EVALUATE TRUE
              WHEN TX-TXN-AMT <= 10000
                 MOVE 110 TO WS-FEE-AMT
              WHEN TX-TXN-AMT <= 50000
                 MOVE 220 TO WS-FEE-AMT
              WHEN TX-TXN-AMT <= 100000
                 MOVE 330 TO WS-FEE-AMT
              WHEN OTHER
                 MOVE 550 TO WS-FEE-AMT
           END-EVALUATE
           .

       3200-CALC-GRACE-DATE.
           MOVE TX-TXN-DT TO WS-DATE-INT
           COMPUTE WS-DATE-INTEGER =
              FUNCTION INTEGER-OF-DATE(WS-DATE-INT)
              + WS-STD-GRACE-DAYS
           COMPUTE WS-DATE-YMD =
              FUNCTION DATE-OF-INTEGER(WS-DATE-INTEGER)
           MOVE WS-DATE-YMD TO WS-GRACE-DATE
           .

       4000-WRITE-CDOVSF.
           WRITE CDOVSF-REC
           IF WS-CDOVSF-STATUS = '00'
              ADD 1 TO WS-WRITE-CNT
           ELSE
              MOVE 'CDOVSF 書込失敗' TO WS-REASON
              PERFORM 8000-HARD-ERROR
           END-IF
           .

       8000-HARD-ERROR.
           SET HARD-ERROR TO TRUE
           MOVE 12 TO RETURN-CODE
           DISPLAY 'CB810B 異常終了 理由=' WS-REASON
                   ' 入力ST=' WS-CDTXNF-STATUS
                   ' 出力ST=' WS-CDOVSF-STATUS
                   ' TX=' TX-TXN-ID
           .

       9000-FINAL.
           IF WS-CDTXNF-STATUS NOT = SPACES
              CLOSE CDTXNF
              IF WS-CDTXNF-STATUS NOT = '00'
                 AND WS-CDTXNF-STATUS NOT = '10'
                 IF NO-HARD-ERROR
                    MOVE 'CDTXNF クローズ失敗' TO WS-REASON
                    PERFORM 8000-HARD-ERROR
                 END-IF
              END-IF
           END-IF

           IF WS-CDOVSF-STATUS NOT = SPACES
              CLOSE CDOVSF
              IF WS-CDOVSF-STATUS NOT = '00'
                 IF NO-HARD-ERROR
                    MOVE 'CDOVSF クローズ失敗' TO WS-REASON
                    PERFORM 8000-HARD-ERROR
                 END-IF
              END-IF
           END-IF

           IF NO-HARD-ERROR
              MOVE 0 TO RETURN-CODE
              DISPLAY 'CB810B 正常終了'
                      ' 読込=' WS-READ-CNT
                      ' 書込=' WS-WRITE-CNT
                      ' 除外=' WS-SKIP-CNT
                      ' 入力不備=' WS-ERR-CNT
           ELSE
              DISPLAY 'CB810B 異常終了'
                      ' 読込=' WS-READ-CNT
                      ' 書込=' WS-WRITE-CNT
                      ' 除外=' WS-SKIP-CNT
                      ' 入力不備=' WS-ERR-CNT
           END-IF
           .
