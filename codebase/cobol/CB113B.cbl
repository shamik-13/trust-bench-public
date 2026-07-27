       IDENTIFICATION DIVISION.
       PROGRAM-ID. CB113B.
      *================================================================*
      *  変更履歴                                                      *
      *  版数  年月日    担当    概要                                  *
      *  1.00  20240115  債権一課 初版作成                            *
      *  1.01  20240520  債権一課 直近入金による督促停止条件を追加    *
      *  1.02  20241010  債権一課 会員状態別の督促停止条件を追加      *
      *================================================================*
      *  延滞督促抽出バッチ                                            *
      *  請求期日超過の未消込明細をカード番号単位で集計し、督促情報    *
      *  と会員状態更新用イベントを出力する。                          *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CDACCF ASSIGN TO "CDACCF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS AC-CARD-NO
               FILE STATUS IS WS-ST-CDACCF.

           SELECT CDOVSF ASSIGN TO "CDOVSF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-ST-CDOVSF.

           SELECT CDPYMF ASSIGN TO "CDPYMF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS PY-PAYMENT-ID
               FILE STATUS IS WS-ST-CDPYMF.

           SELECT CDDELF ASSIGN TO "CDDELF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-ST-CDDELF.

           SELECT CDLOGF ASSIGN TO "CDLOGF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS LG-LOG-ID
               FILE STATUS IS WS-ST-CDLOGF.

       DATA DIVISION.
       FILE SECTION.
       FD  CDACCF.
           COPY CDACCFC.

       FD  CDOVSF.
           COPY CDOVSFC.

       FD  CDPYMF.
           COPY CDPYMFC.

       FD  CDDELF.
           COPY CDDELFC.

       FD  CDLOGF.
           COPY CDLOGFC.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05 WS-ST-CDACCF              PIC XX VALUE SPACE.
           05 WS-ST-CDOVSF              PIC XX VALUE SPACE.
           05 WS-ST-CDPYMF              PIC XX VALUE SPACE.
           05 WS-ST-CDDELF              PIC XX VALUE SPACE.
           05 WS-ST-CDLOGF              PIC XX VALUE SPACE.

       01  WS-SWITCHES.
           05 WS-EOF-OVSF               PIC X VALUE "N".
              88 OVEOF                  VALUE "Y".
           05 WS-EOF-PYMF               PIC X VALUE "N".
              88 PYEOF                  VALUE "Y".
           05 WS-HARD-ERROR             PIC X VALUE "N".
              88 HARD-ERROR             VALUE "Y".
           05 WS-ACCOUNT-FOUND          PIC X VALUE "N".
              88 ACCOUNT-FOUND          VALUE "Y".
           05 WS-RECENT-PAID            PIC X VALUE "N".
              88 RECENT-PAID            VALUE "Y".
           05 WS-SKIP-MEMBER            PIC X VALUE "N".
              88 SKIP-MEMBER            VALUE "Y".

       01  WS-CURRENT.
           05 WS-CUR-CARD-NO            PIC X(19) VALUE LOW-VALUE.
           05 WS-CUR-BILL-DT            PIC 9(8) VALUE ZERO.
           05 WS-CUR-DUE-AMT            PIC S9(13) VALUE ZERO.
           05 WS-CUR-MIN-DUE            PIC S9(13) VALUE ZERO.
           05 WS-CUR-DELAY-DAYS         PIC S9(5) VALUE ZERO.
           05 WS-CUR-NOTICE             PIC X VALUE SPACE.
           05 WS-CUR-HAS-DATA           PIC X VALUE "N".
              88 CUR-HAS-DATA           VALUE "Y".

       01  WS-DATE-AREA.
           05 WS-TODAY                  PIC 9(8) VALUE ZERO.
           05 WS-TODAY-INT              PIC 9(8) VALUE ZERO.
           05 WS-DUE-DATE               PIC 9(8) VALUE ZERO.
           05 WS-DUE-INT                PIC 9(8) VALUE ZERO.
           05 WS-PAY-DATE               PIC 9(8) VALUE ZERO.
           05 WS-PAY-INT                PIC 9(8) VALUE ZERO.
           05 WS-RECENT-LIMIT-DAYS      PIC 9(3) VALUE 3.
           05 WS-RECENT-FROM-INT        PIC 9(8) VALUE ZERO.

       01  WS-AMOUNT-AREA.
           05 WS-OPEN-AMT               PIC S9(13) VALUE ZERO.
           05 WS-NET-PAY-AMT            PIC S9(13) VALUE ZERO.
           05 WS-MIN-PCT-AMT            PIC S9(13) VALUE ZERO.
           05 WS-FEE-AMT                PIC S9(13) VALUE ZERO.
           05 WS-SETL-AMT               PIC S9(13) VALUE ZERO.

       01  WS-COUNTERS.
           05 WS-READ-OVSF-CNT          PIC 9(9) VALUE ZERO.
           05 WS-READ-PYMF-CNT          PIC 9(9) VALUE ZERO.
           05 WS-WRITE-DELF-CNT         PIC 9(9) VALUE ZERO.
           05 WS-WRITE-LOGF-CNT         PIC 9(9) VALUE ZERO.
           05 WS-SKIP-CNT               PIC 9(9) VALUE ZERO.
           05 WS-ERR-CNT                PIC 9(9) VALUE ZERO.
           05 WS-LOG-SEQ                PIC 9(9) VALUE ZERO.

       01  WS-CONSTANTS.
           05 WS-PROGRAM-ID             PIC X(8) VALUE "CB113B".
           05 WS-LOW-PAYMENT-ID         PIC X(20) VALUE LOW-VALUE.
           05 WS-EVENT-EXTRACT          PIC X(2) VALUE "DE".
           05 WS-EVENT-SKIP             PIC X(2) VALUE "DS".
           05 WS-DETAIL-RECENT-PAY      PIC X(4) VALUE "P001".
           05 WS-DETAIL-ACCOUNT-STOP    PIC X(4) VALUE "A001".
           05 WS-DETAIL-NOTICE          PIC X(4) VALUE "N001".

       01  WS-EDIT.
           05 WS-DISP-CNT               PIC ZZZ,ZZZ,ZZ9.
           05 WS-DISP-AMT               PIC ZZZ,ZZZ,ZZZ,ZZ9.
           05 WS-DISP-DAYS              PIC ZZZZ9.

       PROCEDURE DIVISION.
       MAIN-SECTION.
           PERFORM 0000-INIT
           IF NOT HARD-ERROR
              PERFORM 1000-MAIN
           END-IF
           PERFORM 9000-END
           GOBACK.

       0000-INIT.
           MOVE 0 TO RETURN-CODE
           ACCEPT WS-TODAY FROM DATE YYYYMMDD
           COMPUTE WS-TODAY-INT =
               FUNCTION INTEGER-OF-DATE(WS-TODAY)
           COMPUTE WS-RECENT-FROM-INT =
               WS-TODAY-INT - WS-RECENT-LIMIT-DAYS

           OPEN INPUT CDACCF
           IF WS-ST-CDACCF NOT = "00"
              DISPLAY "CDACCF オープン失敗 ST=" WS-ST-CDACCF
              PERFORM 9900-ABEND
           END-IF

           OPEN INPUT CDOVSF
           IF WS-ST-CDOVSF NOT = "00"
              DISPLAY "CDOVSF オープン失敗 ST=" WS-ST-CDOVSF
              PERFORM 9900-ABEND
           END-IF

           OPEN INPUT CDPYMF
           IF WS-ST-CDPYMF NOT = "00"
              DISPLAY "CDPYMF オープン失敗 ST=" WS-ST-CDPYMF
              PERFORM 9900-ABEND
           END-IF

           OPEN OUTPUT CDDELF
           IF WS-ST-CDDELF NOT = "00"
              DISPLAY "CDDELF オープン失敗 ST=" WS-ST-CDDELF
              PERFORM 9900-ABEND
           END-IF

           OPEN OUTPUT CDLOGF
           IF WS-ST-CDLOGF NOT = "00"
              DISPLAY "CDLOGF オープン失敗 ST=" WS-ST-CDLOGF
              PERFORM 9900-ABEND
           END-IF.

       1000-MAIN.
           PERFORM 1100-READ-OVSF
           PERFORM UNTIL OVEOF OR HARD-ERROR
              PERFORM 1200-EDIT-OVSF
              PERFORM 1100-READ-OVSF
           END-PERFORM

           IF CUR-HAS-DATA AND NOT HARD-ERROR
              PERFORM 3000-FLUSH-CARD
           END-IF.

       1100-READ-OVSF.
           READ CDOVSF
              AT END
                 SET OVEOF TO TRUE
              NOT AT END
                 ADD 1 TO WS-READ-OVSF-CNT
           END-READ
           IF WS-ST-CDOVSF NOT = "00" AND WS-ST-CDOVSF NOT = "10"
              DISPLAY "CDOVSF 読込失敗 ST=" WS-ST-CDOVSF
              PERFORM 9900-ABEND
           END-IF.

       1200-EDIT-OVSF.
           IF OV-SETL-KBN NOT = "D"
              ADD 1 TO WS-SKIP-CNT
              EXIT PARAGRAPH
           END-IF

           IF OV-TXN-KBN NOT = "P1" AND
              OV-TXN-KBN NOT = "P2" AND
              OV-TXN-KBN NOT = "C1" AND
              OV-TXN-KBN NOT = "C2" AND
              OV-TXN-KBN NOT = "A1"
              DISPLAY "取引区分不正 TXN-ID=" OV-TXN-ID
              ADD 1 TO WS-ERR-CNT
              EXIT PARAGRAPH
           END-IF

           IF OV-FEE-KBN NOT = "00" AND
              OV-FEE-KBN NOT = "FA" AND
              OV-FEE-KBN NOT = "FB"
              DISPLAY "手数料区分不正 TXN-ID=" OV-TXN-ID
              ADD 1 TO WS-ERR-CNT
              EXIT PARAGRAPH
           END-IF

           MOVE OV-FEE-AMT TO WS-FEE-AMT
           MOVE OV-SETL-AMT TO WS-SETL-AMT
           COMPUTE WS-OPEN-AMT = WS-FEE-AMT - WS-SETL-AMT
           IF WS-OPEN-AMT <= ZERO
              ADD 1 TO WS-SKIP-CNT
              EXIT PARAGRAPH
           END-IF

           MOVE OV-INT-START-DT TO WS-DUE-DATE
           IF WS-DUE-DATE = ZERO
              DISPLAY "請求期日不正 TXN-ID=" OV-TXN-ID
              ADD 1 TO WS-ERR-CNT
              EXIT PARAGRAPH
           END-IF

           COMPUTE WS-DUE-INT =
               FUNCTION INTEGER-OF-DATE(WS-DUE-DATE)
           IF WS-TODAY-INT <= WS-DUE-INT
              ADD 1 TO WS-SKIP-CNT
              EXIT PARAGRAPH
           END-IF

           IF NOT CUR-HAS-DATA
              PERFORM 2100-START-CARD
           ELSE
              IF OV-CARD-NO NOT = WS-CUR-CARD-NO
                 PERFORM 3000-FLUSH-CARD
                 IF NOT HARD-ERROR
                    PERFORM 2100-START-CARD
                 END-IF
              END-IF
           END-IF

           IF NOT HARD-ERROR
              ADD WS-OPEN-AMT TO WS-CUR-DUE-AMT
              IF WS-DUE-DATE < WS-CUR-BILL-DT OR WS-CUR-BILL-DT = ZERO
                 MOVE WS-DUE-DATE TO WS-CUR-BILL-DT
                 COMPUTE WS-CUR-DELAY-DAYS = WS-TODAY-INT - WS-DUE-INT
              END-IF
           END-IF.

       2100-START-CARD.
           MOVE OV-CARD-NO TO WS-CUR-CARD-NO
           MOVE ZERO       TO WS-CUR-DUE-AMT
           MOVE ZERO       TO WS-CUR-MIN-DUE
           MOVE ZERO       TO WS-CUR-DELAY-DAYS
           MOVE ZERO       TO WS-CUR-BILL-DT
           MOVE SPACE      TO WS-CUR-NOTICE
           MOVE "Y"        TO WS-CUR-HAS-DATA.

       3000-FLUSH-CARD.
           IF WS-CUR-DUE-AMT <= ZERO
              MOVE "N" TO WS-CUR-HAS-DATA
              EXIT PARAGRAPH
           END-IF

           PERFORM 3100-READ-ACCOUNT
           IF NOT ACCOUNT-FOUND
              DISPLAY "会員口座なし CARD=" WS-CUR-CARD-NO
              ADD 1 TO WS-ERR-CNT
              MOVE "N" TO WS-CUR-HAS-DATA
              EXIT PARAGRAPH
           END-IF

           PERFORM 3200-JUDGE-STOP
           IF SKIP-MEMBER
              PERFORM 5300-WRITE-SKIP-LOG
              MOVE "N" TO WS-CUR-HAS-DATA
              EXIT PARAGRAPH
           END-IF

           PERFORM 3300-JUDGE-NOTICE
           PERFORM 3400-CALC-MIN-DUE
           PERFORM 5100-WRITE-DELF
           PERFORM 5200-WRITE-NOTICE-LOG
           MOVE "N" TO WS-CUR-HAS-DATA.

       3100-READ-ACCOUNT.
           MOVE "N" TO WS-ACCOUNT-FOUND
           MOVE WS-CUR-CARD-NO TO AC-CARD-NO
           READ CDACCF KEY IS AC-CARD-NO
              INVALID KEY
                 MOVE "N" TO WS-ACCOUNT-FOUND
              NOT INVALID KEY
                 MOVE "Y" TO WS-ACCOUNT-FOUND
           END-READ
           IF WS-ST-CDACCF NOT = "00" AND WS-ST-CDACCF NOT = "23"
              DISPLAY "CDACCF 読込失敗 ST=" WS-ST-CDACCF
              PERFORM 9900-ABEND
           END-IF.

       3200-JUDGE-STOP.
           MOVE "N" TO WS-SKIP-MEMBER
           MOVE "N" TO WS-RECENT-PAID

           IF AC-STATUS-KBN = "9" OR AC-STATUS-KBN = "8"
              MOVE "Y" TO WS-SKIP-MEMBER
              EXIT PARAGRAPH
           END-IF

           IF AC-STATUS-KBN NOT = "0" AND AC-STATUS-KBN NOT = "1" AND
              AC-STATUS-KBN NOT = "2"
              DISPLAY "口座状態不正 CARD=" WS-CUR-CARD-NO
              ADD 1 TO WS-ERR-CNT
              MOVE "Y" TO WS-SKIP-MEMBER
              EXIT PARAGRAPH
           END-IF

           PERFORM 4100-CHECK-RECENT-PAYMENT
           IF RECENT-PAID
              MOVE "Y" TO WS-SKIP-MEMBER
           END-IF.

       3300-JUDGE-NOTICE.
           EVALUATE TRUE
              WHEN WS-CUR-DELAY-DAYS >= 60
                 MOVE "4" TO WS-CUR-NOTICE
              WHEN WS-CUR-DELAY-DAYS >= 30
                 MOVE "3" TO WS-CUR-NOTICE
              WHEN WS-CUR-DELAY-DAYS >= 15
                 MOVE "2" TO WS-CUR-NOTICE
              WHEN OTHER
                 MOVE "1" TO WS-CUR-NOTICE
           END-EVALUATE.

       3400-CALC-MIN-DUE.
           COMPUTE WS-MIN-PCT-AMT = WS-CUR-DUE-AMT / 10
           EVALUATE TRUE
              WHEN WS-CUR-DUE-AMT <= 10000
                 MOVE WS-CUR-DUE-AMT TO WS-CUR-MIN-DUE
              WHEN WS-MIN-PCT-AMT < 5000
                 MOVE 5000 TO WS-CUR-MIN-DUE
              WHEN OTHER
                 MOVE WS-MIN-PCT-AMT TO WS-CUR-MIN-DUE
           END-EVALUATE.

       4100-CHECK-RECENT-PAYMENT.
           MOVE "N" TO WS-RECENT-PAID
           MOVE "N" TO WS-EOF-PYMF
           MOVE WS-LOW-PAYMENT-ID TO PY-PAYMENT-ID
           START CDPYMF KEY IS >= PY-PAYMENT-ID
              INVALID KEY
                 SET PYEOF TO TRUE
           END-START
           IF WS-ST-CDPYMF NOT = "00" AND WS-ST-CDPYMF NOT = "23"
              DISPLAY "CDPYMF START失敗 ST=" WS-ST-CDPYMF
              PERFORM 9900-ABEND
              EXIT PARAGRAPH
           END-IF

           PERFORM UNTIL PYEOF OR RECENT-PAID OR HARD-ERROR
              READ CDPYMF NEXT RECORD
                 AT END
                    SET PYEOF TO TRUE
                 NOT AT END
                    ADD 1 TO WS-READ-PYMF-CNT
                    PERFORM 4200-TEST-PAYMENT
              END-READ
              IF WS-ST-CDPYMF NOT = "00" AND WS-ST-CDPYMF NOT = "10"
                 DISPLAY "CDPYMF 読込失敗 ST=" WS-ST-CDPYMF
                 PERFORM 9900-ABEND
              END-IF
           END-PERFORM.

       4200-TEST-PAYMENT.
           IF PY-CARD-NO NOT = WS-CUR-CARD-NO
              EXIT PARAGRAPH
           END-IF

           IF PY-ALLOC-KBN NOT = "1"
              EXIT PARAGRAPH
           END-IF

           MOVE PY-PAY-DT TO WS-PAY-DATE
           IF WS-PAY-DATE = ZERO
              EXIT PARAGRAPH
           END-IF

           COMPUTE WS-PAY-INT =
               FUNCTION INTEGER-OF-DATE(WS-PAY-DATE)
           IF WS-PAY-INT >= WS-RECENT-FROM-INT AND
              WS-PAY-INT <= WS-TODAY-INT
              COMPUTE WS-NET-PAY-AMT =
                 PY-PAY-AMT - PY-UNAPPLIED-AMT
              IF WS-NET-PAY-AMT >= WS-CUR-DUE-AMT
                 MOVE "Y" TO WS-RECENT-PAID
              END-IF
           END-IF.

       5100-WRITE-DELF.
           INITIALIZE CDDELF-REC
           MOVE WS-CUR-CARD-NO    TO DL-CARD-NO
           MOVE WS-CUR-BILL-DT    TO DL-BILLING-DT
           MOVE WS-CUR-DELAY-DAYS TO DL-DELAY-DAYS
           MOVE WS-CUR-MIN-DUE    TO DL-DUE-AMT
           MOVE WS-CUR-NOTICE     TO DL-NOTICE-KBN
           MOVE WS-TODAY          TO DL-EXTRACT-DT
           WRITE CDDELF-REC
           IF WS-ST-CDDELF NOT = "00"
              DISPLAY "CDDELF 書込失敗 ST=" WS-ST-CDDELF
              PERFORM 9900-ABEND
           ELSE
              ADD 1 TO WS-WRITE-DELF-CNT
           END-IF.

       5200-WRITE-NOTICE-LOG.
           ADD 1 TO WS-LOG-SEQ
           INITIALIZE CDLOGF-REC
           MOVE WS-LOG-SEQ        TO LG-LOG-ID
           MOVE WS-PROGRAM-ID     TO LG-PROGRAM-ID
           MOVE WS-CUR-CARD-NO    TO LG-CARD-NO
           MOVE WS-EVENT-EXTRACT  TO LG-EVENT-KBN
           MOVE WS-TODAY          TO LG-EVENT-DT
           MOVE WS-DETAIL-NOTICE  TO LG-DETAIL-CD
           WRITE CDLOGF-REC
           IF WS-ST-CDLOGF NOT = "00"
              DISPLAY "CDLOGF 書込失敗 ST=" WS-ST-CDLOGF
              PERFORM 9900-ABEND
           ELSE
              ADD 1 TO WS-WRITE-LOGF-CNT
           END-IF.

       5300-WRITE-SKIP-LOG.
           ADD 1 TO WS-LOG-SEQ
           INITIALIZE CDLOGF-REC
           MOVE WS-LOG-SEQ       TO LG-LOG-ID
           MOVE WS-PROGRAM-ID    TO LG-PROGRAM-ID
           MOVE WS-CUR-CARD-NO   TO LG-CARD-NO
           MOVE WS-EVENT-SKIP    TO LG-EVENT-KBN
           MOVE WS-TODAY         TO LG-EVENT-DT
           IF RECENT-PAID
              MOVE WS-DETAIL-RECENT-PAY TO LG-DETAIL-CD
           ELSE
              MOVE WS-DETAIL-ACCOUNT-STOP TO LG-DETAIL-CD
           END-IF
           WRITE CDLOGF-REC
           IF WS-ST-CDLOGF NOT = "00"
              DISPLAY "CDLOGF 書込失敗 ST=" WS-ST-CDLOGF
              PERFORM 9900-ABEND
           ELSE
              ADD 1 TO WS-WRITE-LOGF-CNT
           END-IF.

       9000-END.
           CLOSE CDACCF CDOVSF CDPYMF CDDELF CDLOGF

           MOVE WS-READ-OVSF-CNT TO WS-DISP-CNT
           DISPLAY "延滞明細読込件数=" WS-DISP-CNT
           MOVE WS-WRITE-DELF-CNT TO WS-DISP-CNT
           DISPLAY "督促抽出出力件数=" WS-DISP-CNT
           MOVE WS-WRITE-LOGF-CNT TO WS-DISP-CNT
           DISPLAY "イベントログ出力件数=" WS-DISP-CNT
           MOVE WS-SKIP-CNT TO WS-DISP-CNT
           DISPLAY "対象外件数=" WS-DISP-CNT
           MOVE WS-ERR-CNT TO WS-DISP-CNT
           DISPLAY "警告件数=" WS-DISP-CNT

           IF HARD-ERROR
              MOVE 12 TO RETURN-CODE
           ELSE
              MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE "Y" TO WS-HARD-ERROR
           MOVE 12 TO RETURN-CODE.
