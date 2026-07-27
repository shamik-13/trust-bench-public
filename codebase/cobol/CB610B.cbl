       IDENTIFICATION DIVISION.
      ******************************************************************
      * 変更履歴
      * 版数  年月日    担当      概要
      * 1.00  20240115  TRUST01   初版作成
      * 1.10  20240520  TRUST02   入金方法検証を追加
      * 1.20  20240910  TRUST03   消込順序を本バッチに集約
      ******************************************************************
       PROGRAM-ID. CB610B.
       AUTHOR. TRUST-BATCH.
       INSTALLATION. みらいカード.
       DATE-WRITTEN. 20240910.
       DATE-COMPILED.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CDPAYF
               ASSIGN TO "CDPAYF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS FS-CDPAYF.

           SELECT CDOSF
               ASSIGN TO "CDOSF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS OS-CARD-NO
               FILE STATUS IS FS-CDOSF.

           SELECT CDAPPF
               ASSIGN TO "CDAPPF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS FS-CDAPPF.

       DATA DIVISION.
       FILE SECTION.
       FD  CDPAYF.
       COPY CDPAYFC.

       FD  CDOSF.
       COPY CDOSFC.

       FD  CDAPPF.
       COPY CDAPPFC.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05  FS-CDPAYF              PIC XX VALUE SPACES.
           05  FS-CDOSF               PIC XX VALUE SPACES.
           05  FS-CDAPPF              PIC XX VALUE SPACES.

       01  WS-SWITCHES.
           05  SW-END                 PIC X VALUE 'N'.
               88  END-OF-CDPAYF            VALUE 'Y'.
               88  NOT-END-OF-CDPAYF        VALUE 'N'.
           05  SW-OS-FOUND            PIC X VALUE 'N'.
               88  OS-FOUND                 VALUE 'Y'.
               88  OS-NOT-FOUND             VALUE 'N'.

       01  WS-COUNTERS.
           05  CNT-READ-PAY           PIC 9(9) VALUE ZERO.
           05  CNT-WRITE-APP          PIC 9(9) VALUE ZERO.
           05  CNT-SKIP-APP           PIC 9(9) VALUE ZERO.

       01  WS-AMOUNTS.
           05  WS-WORK-AMT            PIC S9(13) VALUE ZERO.
           05  WS-TOTAL-BAL-AMT       PIC S9(13) VALUE ZERO.
           05  WS-APPLIED-FEE-AMT     PIC S9(13) VALUE ZERO.
           05  WS-APPLIED-INT-AMT     PIC S9(13) VALUE ZERO.
           05  WS-APPLIED-PRIN-AMT    PIC S9(13) VALUE ZERO.
           05  WS-REMAIN-AMT          PIC S9(13) VALUE ZERO.

       01  WS-DATE-CHECK.
           05  WS-YYYY                PIC 9(4) VALUE ZERO.
           05  WS-MM                  PIC 9(2) VALUE ZERO.
           05  WS-DD                  PIC 9(2) VALUE ZERO.

       01  WS-MESSAGES.
           05  MSG-PROGRAM            PIC X(8) VALUE 'CB610B'.
           05  MSG-ABEND              PIC X(20) VALUE SPACES.

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE 0 TO RETURN-CODE
           PERFORM 1000-OPEN-FILES
           PERFORM 2000-PROCESS UNTIL END-OF-CDPAYF
           PERFORM 9000-CLOSE-FILES
           DISPLAY 'CB610B 正常終了 入力=' CNT-READ-PAY
                   ' 出力=' CNT-WRITE-APP
                   ' 対象外=' CNT-SKIP-APP
           MOVE 0 TO RETURN-CODE
           GOBACK.

       1000-OPEN-FILES.
           OPEN INPUT CDPAYF
           IF FS-CDPAYF NOT = '00'
               DISPLAY 'CDPAYF オープン失敗 ST=' FS-CDPAYF
               MOVE 'OPEN-CDPAYF' TO MSG-ABEND
               PERFORM 9900-ABEND
           END-IF

           OPEN INPUT CDOSF
           IF FS-CDOSF NOT = '00'
               DISPLAY 'CDOSF オープン失敗 ST=' FS-CDOSF
               MOVE 'OPEN-CDOSF' TO MSG-ABEND
               PERFORM 9900-ABEND
           END-IF

           OPEN OUTPUT CDAPPF
           IF FS-CDAPPF NOT = '00'
               DISPLAY 'CDAPPF オープン失敗 ST=' FS-CDAPPF
               MOVE 'OPEN-CDAPPF' TO MSG-ABEND
               PERFORM 9900-ABEND
           END-IF

           PERFORM 2100-READ-PAY.

       2000-PROCESS.
           ADD 1 TO CNT-READ-PAY
           PERFORM 2200-VALIDATE-PAYMENT
           PERFORM 2300-READ-OS
           PERFORM 3000-CLEAR-APP

           IF OS-FOUND
               PERFORM 2400-VALIDATE-OS
               PERFORM 4000-APPLY-PAYMENT
           ELSE
               PERFORM 4100-MAKE-SKIP-APP
           END-IF

           PERFORM 5000-WRITE-APP
           PERFORM 2100-READ-PAY.

       2100-READ-PAY.
           READ CDPAYF
               AT END
                   SET END-OF-CDPAYF TO TRUE
               NOT AT END
                   IF FS-CDPAYF NOT = '00'
                       DISPLAY 'CDPAYF 読込失敗 ST=' FS-CDPAYF
                       MOVE 'READ-CDPAYF' TO MSG-ABEND
                       PERFORM 9900-ABEND
                   END-IF
           END-READ.

       2200-VALIDATE-PAYMENT.
           IF PY-PAY-ID = SPACES
               DISPLAY '入金ＩＤ未設定'
               MOVE 'PAY-ID' TO MSG-ABEND
               PERFORM 9900-ABEND
           END-IF

           IF PY-CARD-NO = SPACES
               DISPLAY 'カード番号未設定 PAY-ID='
                       PY-PAY-ID
               MOVE 'PAY-CARD' TO MSG-ABEND
               PERFORM 9900-ABEND
           END-IF

           IF PY-PAY-AMT <= ZERO
               DISPLAY '入金額不正 PAY-ID='
                       PY-PAY-ID
                       ' AMT='
                       PY-PAY-AMT
               MOVE 'PAY-AMT' TO MSG-ABEND
               PERFORM 9900-ABEND
           END-IF

           IF PY-PAY-METHOD NOT = '10'
              AND PY-PAY-METHOD NOT = '20'
              AND PY-PAY-METHOD NOT = '30'
               DISPLAY '入金方法不正 PAY-ID='
                       PY-PAY-ID
                       ' METHOD='
                       PY-PAY-METHOD
               MOVE 'PAY-METHOD' TO MSG-ABEND
               PERFORM 9900-ABEND
           END-IF

           MOVE PY-PAY-DT(1:4) TO WS-YYYY
           MOVE PY-PAY-DT(5:2) TO WS-MM
           MOVE PY-PAY-DT(7:2) TO WS-DD
           IF WS-YYYY < 2000
              OR WS-MM < 1
              OR WS-MM > 12
              OR WS-DD < 1
              OR WS-DD > 31
               DISPLAY '入金日不正 PAY-ID='
                       PY-PAY-ID
                       ' DATE='
                       PY-PAY-DT
               MOVE 'PAY-DATE' TO MSG-ABEND
               PERFORM 9900-ABEND
           END-IF.

       2300-READ-OS.
           SET OS-NOT-FOUND TO TRUE
           MOVE PY-CARD-NO TO OS-CARD-NO
           READ CDOSF
               INVALID KEY
                   IF FS-CDOSF = '23'
                       SET OS-NOT-FOUND TO TRUE
                   ELSE
                       DISPLAY 'CDOSF 読込失敗 ST='
                               FS-CDOSF
                               ' CARD='
                               PY-CARD-NO
                       MOVE 'READ-CDOSF' TO MSG-ABEND
                       PERFORM 9900-ABEND
                   END-IF
               NOT INVALID KEY
                   SET OS-FOUND TO TRUE
           END-READ.

       2400-VALIDATE-OS.
           IF OS-CARD-NO NOT = PY-CARD-NO
               DISPLAY '残高カード番号不一致 PAY-ID='
                       PY-PAY-ID
               MOVE 'OS-CARD' TO MSG-ABEND
               PERFORM 9900-ABEND
           END-IF

           IF OS-FEE-BAL-AMT < ZERO
              OR OS-INTEREST-BAL-AMT < ZERO
              OR OS-PRINCIPAL-BAL-AMT < ZERO
               DISPLAY '残高金額不正 CARD=' OS-CARD-NO
               MOVE 'OS-AMT' TO MSG-ABEND
               PERFORM 9900-ABEND
           END-IF

           MOVE OS-CYCLE-DT(1:4) TO WS-YYYY
           MOVE OS-CYCLE-DT(5:2) TO WS-MM
           MOVE OS-CYCLE-DT(7:2) TO WS-DD
           IF WS-YYYY < 2000
              OR WS-MM < 1
              OR WS-MM > 12
              OR WS-DD < 1
              OR WS-DD > 31
               DISPLAY '請求サイクル日不正 CARD='
                       OS-CARD-NO
                       ' DATE='
                       OS-CYCLE-DT
               MOVE 'OS-DATE' TO MSG-ABEND
               PERFORM 9900-ABEND
           END-IF.

       3000-CLEAR-APP.
           INITIALIZE CDAPPF-REC
           MOVE PY-PAY-ID TO AP-PAY-ID
           MOVE PY-CARD-NO TO AP-CARD-NO
           MOVE ZERO TO AP-APPLIED-FEE-AMT
           MOVE ZERO TO AP-APPLIED-INT-AMT
           MOVE ZERO TO AP-APPLIED-PRIN-AMT
           MOVE ZERO TO AP-REMAIN-AMT
           MOVE MSG-PROGRAM TO AP-PROGRAM-ID.

       4000-APPLY-PAYMENT.
      *    現行仕様は元金、利息、手数料の順で消込する。
      *    消込順序の判定点は本バッチが保有する。
           MOVE PY-PAY-AMT TO WS-WORK-AMT
           MOVE ZERO TO WS-APPLIED-FEE-AMT
           MOVE ZERO TO WS-APPLIED-INT-AMT
           MOVE ZERO TO WS-APPLIED-PRIN-AMT
           MOVE ZERO TO WS-REMAIN-AMT

           PERFORM 4200-APPLY-PRINCIPAL
           PERFORM 4300-APPLY-INTEREST
           PERFORM 4400-APPLY-FEE

           MOVE WS-WORK-AMT TO WS-REMAIN-AMT
           MOVE WS-APPLIED-FEE-AMT TO AP-APPLIED-FEE-AMT
           MOVE WS-APPLIED-INT-AMT TO AP-APPLIED-INT-AMT
           MOVE WS-APPLIED-PRIN-AMT TO AP-APPLIED-PRIN-AMT
           MOVE WS-REMAIN-AMT TO AP-REMAIN-AMT

           COMPUTE WS-TOTAL-BAL-AMT =
                   OS-FEE-BAL-AMT
                 + OS-INTEREST-BAL-AMT
                 + OS-PRINCIPAL-BAL-AMT

           IF WS-REMAIN-AMT > ZERO
               MOVE 'O' TO AP-APP-STATUS
           ELSE
               IF PY-PAY-AMT >= WS-TOTAL-BAL-AMT
                   MOVE 'F' TO AP-APP-STATUS
               ELSE
                   MOVE 'P' TO AP-APP-STATUS
               END-IF
           END-IF.

       4100-MAKE-SKIP-APP.
           ADD 1 TO CNT-SKIP-APP
           MOVE ZERO TO AP-APPLIED-FEE-AMT
           MOVE ZERO TO AP-APPLIED-INT-AMT
           MOVE ZERO TO AP-APPLIED-PRIN-AMT
           MOVE PY-PAY-AMT TO AP-REMAIN-AMT
           MOVE 'S' TO AP-APP-STATUS
           DISPLAY '消込対象なし PAY-ID='
                   PY-PAY-ID
                   ' CARD='
                   PY-CARD-NO.

       4200-APPLY-PRINCIPAL.
           IF WS-WORK-AMT > ZERO
               IF WS-WORK-AMT >= OS-PRINCIPAL-BAL-AMT
                   MOVE OS-PRINCIPAL-BAL-AMT
                     TO WS-APPLIED-PRIN-AMT
                   SUBTRACT OS-PRINCIPAL-BAL-AMT
                     FROM WS-WORK-AMT
               ELSE
                   MOVE WS-WORK-AMT TO WS-APPLIED-PRIN-AMT
                   MOVE ZERO TO WS-WORK-AMT
               END-IF
           END-IF.

       4300-APPLY-INTEREST.
           IF WS-WORK-AMT > ZERO
               IF WS-WORK-AMT >= OS-INTEREST-BAL-AMT
                   MOVE OS-INTEREST-BAL-AMT
                     TO WS-APPLIED-INT-AMT
                   SUBTRACT OS-INTEREST-BAL-AMT
                     FROM WS-WORK-AMT
               ELSE
                   MOVE WS-WORK-AMT TO WS-APPLIED-INT-AMT
                   MOVE ZERO TO WS-WORK-AMT
               END-IF
           END-IF.

       4400-APPLY-FEE.
           IF WS-WORK-AMT > ZERO
               IF WS-WORK-AMT >= OS-FEE-BAL-AMT
                   MOVE OS-FEE-BAL-AMT TO WS-APPLIED-FEE-AMT
                   SUBTRACT OS-FEE-BAL-AMT FROM WS-WORK-AMT
               ELSE
                   MOVE WS-WORK-AMT TO WS-APPLIED-FEE-AMT
                   MOVE ZERO TO WS-WORK-AMT
               END-IF
           END-IF.

       5000-WRITE-APP.
           IF AP-APP-STATUS NOT = 'F'
              AND AP-APP-STATUS NOT = 'P'
              AND AP-APP-STATUS NOT = 'O'
              AND AP-APP-STATUS NOT = 'S'
               DISPLAY '消込状態不正 PAY-ID=' AP-PAY-ID
               MOVE 'APP-STAT' TO MSG-ABEND
               PERFORM 9900-ABEND
           END-IF

           WRITE CDAPPF-REC
           IF FS-CDAPPF NOT = '00'
               DISPLAY 'CDAPPF 書込失敗 ST='
                       FS-CDAPPF
                       ' PAY-ID='
                       AP-PAY-ID
               MOVE 'WRITE-CDAPPF' TO MSG-ABEND
               PERFORM 9900-ABEND
           END-IF
           ADD 1 TO CNT-WRITE-APP.

       9000-CLOSE-FILES.
           CLOSE CDPAYF
           IF FS-CDPAYF NOT = '00'
               DISPLAY 'CDPAYF クローズ失敗 ST=' FS-CDPAYF
               MOVE 'CLOSE-PAY' TO MSG-ABEND
               PERFORM 9900-ABEND
           END-IF

           CLOSE CDOSF
           IF FS-CDOSF NOT = '00'
               DISPLAY 'CDOSF クローズ失敗 ST=' FS-CDOSF
               MOVE 'CLOSE-OS' TO MSG-ABEND
               PERFORM 9900-ABEND
           END-IF

           CLOSE CDAPPF
           IF FS-CDAPPF NOT = '00'
               DISPLAY 'CDAPPF クローズ失敗 ST=' FS-CDAPPF
               MOVE 'CLOSE-APP' TO MSG-ABEND
               PERFORM 9900-ABEND
           END-IF.

       9900-ABEND.
           DISPLAY 'CB610B 異常終了 理由=' MSG-ABEND
           DISPLAY '処理件数 入力=' CNT-READ-PAY
                   ' 出力=' CNT-WRITE-APP
                   ' 対象外=' CNT-SKIP-APP
           MOVE 12 TO RETURN-CODE
           GOBACK.
