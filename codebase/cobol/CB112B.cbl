       IDENTIFICATION DIVISION.
       PROGRAM-ID. CB112B.
       AUTHOR. MIRAI-CARD-KESSAI.
      ******************************************************************
      * 入金消込バッチ
      * 入金データをカード番号単位で請求残高、利息、手数料、元本の
      * 優先順に配賦し、未消込額は入金マスタへ保持する。
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CDPYMF ASSIGN TO "CDPYMF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS PY-PAYMENT-ID
               FILE STATUS IS FS-CDPYMF.

           SELECT CDOVSF ASSIGN TO "CDOVSF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-CDOVSF.

           SELECT CDACCF ASSIGN TO "CDACCF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS AC-CARD-NO
               FILE STATUS IS FS-CDACCF.

           SELECT CDLOGF ASSIGN TO "CDLOGF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS FS-CDLOGF.

       DATA DIVISION.
       FILE SECTION.
       FD  CDPYMF.
           COPY CDPYMFC.

       FD  CDOVSF.
           COPY CDOVSFC.

       FD  CDACCF.
           COPY CDACCFC.

       FD  CDLOGF.
           COPY CDLOGFC.

       WORKING-STORAGE SECTION.
       01  FS-AREA.
           05 FS-CDPYMF              PIC XX VALUE SPACE.
           05 FS-CDOVSF              PIC XX VALUE SPACE.
           05 FS-CDACCF              PIC XX VALUE SPACE.
           05 FS-CDLOGF              PIC XX VALUE SPACE.

       01  SW-AREA.
           05 SW-END-PY              PIC X VALUE "N".
              88 END-PY                    VALUE "Y".
           05 SW-END-OV              PIC X VALUE "N".
              88 END-OV                    VALUE "Y".
           05 SW-ABEND               PIC X VALUE "N".
              88 ABEND-ON                  VALUE "Y".
           05 SW-ACC-FOUND           PIC X VALUE "N".
              88 ACC-FOUND                 VALUE "Y".
           05 SW-OV-FOUND            PIC X VALUE "N".
              88 OV-FOUND                  VALUE "Y".

       01  WK-AREA.
           05 WK-PROGRAM-ID          PIC X(08) VALUE "CB112B".
           05 WK-BATCH-DT            PIC 9(08) VALUE ZERO.
           05 WK-LOG-SEQ             PIC 9(10) VALUE ZERO.
           05 WK-PAY-AMT             PIC S9(13)V99 VALUE ZERO.
           05 WK-REMAIN-AMT          PIC S9(13)V99 VALUE ZERO.
           05 WK-ALLOC-AMT           PIC S9(13)V99 VALUE ZERO.
           05 WK-BILL-BAL            PIC S9(13)V99 VALUE ZERO.
           05 WK-INT-BAL             PIC S9(13)V99 VALUE ZERO.
           05 WK-FEE-BAL             PIC S9(13)V99 VALUE ZERO.
           05 WK-PRIN-BAL            PIC S9(13)V99 VALUE ZERO.
           05 WK-OLD-USED-AMT        PIC S9(13)V99 VALUE ZERO.
           05 WK-NEW-USED-AMT        PIC S9(13)V99 VALUE ZERO.
           05 WK-APPLIED-TOTAL       PIC S9(13)V99 VALUE ZERO.
           05 WK-UNAPPLIED-AMT       PIC S9(13)V99 VALUE ZERO.
           05 WK-DIFF-AMT            PIC S9(13)V99 VALUE ZERO.

       01  CNT-AREA.
           05 CNT-PY-READ            PIC 9(09) VALUE ZERO.
           05 CNT-PY-REWRITE         PIC 9(09) VALUE ZERO.
           05 CNT-OV-READ            PIC 9(09) VALUE ZERO.
           05 CNT-AC-READ            PIC 9(09) VALUE ZERO.
           05 CNT-AC-REWRITE         PIC 9(09) VALUE ZERO.
           05 CNT-LOG-WRITE          PIC 9(09) VALUE ZERO.
           05 CNT-ERROR              PIC 9(09) VALUE ZERO.

       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-INITIALIZE
           IF NOT ABEND-ON
              PERFORM 2000-MAIN-PROCESS UNTIL END-PY OR ABEND-ON
           END-IF
           PERFORM 9000-FINALIZE
           GOBACK.

       1000-INITIALIZE.
           MOVE 0 TO RETURN-CODE
           ACCEPT WK-BATCH-DT FROM DATE YYYYMMDD

           OPEN I-O CDPYMF
           IF FS-CDPYMF NOT = "00"
              DISPLAY "CDPYMF オープン失敗 ST=" FS-CDPYMF
              PERFORM 9100-SET-ABEND
              EXIT PARAGRAPH
           END-IF

           OPEN INPUT CDOVSF
           IF FS-CDOVSF NOT = "00"
              DISPLAY "CDOVSF オープン失敗 ST=" FS-CDOVSF
              PERFORM 9100-SET-ABEND
              EXIT PARAGRAPH
           END-IF

           OPEN I-O CDACCF
           IF FS-CDACCF NOT = "00"
              DISPLAY "CDACCF オープン失敗 ST=" FS-CDACCF
              PERFORM 9100-SET-ABEND
              EXIT PARAGRAPH
           END-IF

           OPEN EXTEND CDLOGF
           IF FS-CDLOGF NOT = "00"
              DISPLAY "CDLOGF オープン失敗 ST=" FS-CDLOGF
              PERFORM 9100-SET-ABEND
              EXIT PARAGRAPH
           END-IF

           PERFORM 2100-READ-PY
           PERFORM 2200-READ-OV.

       2000-MAIN-PROCESS.
           PERFORM 3000-VALIDATE-PAYMENT
           IF ABEND-ON
              EXIT PARAGRAPH
           END-IF

           PERFORM 4000-READ-ACCOUNT
           IF NOT ACC-FOUND
              MOVE "AC-NF" TO LG-DETAIL-CD
              PERFORM 7000-WRITE-LOG
              PERFORM 6200-SET-PY-UNAPPLIED
              PERFORM 2100-READ-PY
              EXIT PARAGRAPH
           END-IF

           PERFORM 5000-CALC-OUTSTANDING
           PERFORM 6000-ALLOCATE-PAYMENT
           PERFORM 6100-UPDATE-PAYMENT
           PERFORM 6300-UPDATE-ACCOUNT

           IF WK-DIFF-AMT NOT = ZERO
              MOVE "DIFF" TO LG-DETAIL-CD
              PERFORM 7000-WRITE-LOG
           END-IF

           PERFORM 2100-READ-PY.

       2100-READ-PY.
           READ CDPYMF NEXT RECORD
              AT END
                 SET END-PY TO TRUE
              NOT AT END
                 ADD 1 TO CNT-PY-READ
           END-READ
           IF FS-CDPYMF NOT = "00" AND FS-CDPYMF NOT = "10"
              DISPLAY "CDPYMF 読込失敗 ST=" FS-CDPYMF
              PERFORM 9100-SET-ABEND
           END-IF.

       2200-READ-OV.
           READ CDOVSF
              AT END
                 SET END-OV TO TRUE
              NOT AT END
                 ADD 1 TO CNT-OV-READ
           END-READ
           IF FS-CDOVSF NOT = "00" AND FS-CDOVSF NOT = "10"
              DISPLAY "CDOVSF 読込失敗 ST=" FS-CDOVSF
              PERFORM 9100-SET-ABEND
           END-IF.

       3000-VALIDATE-PAYMENT.
           IF PY-PAYMENT-ID = SPACE
              MOVE "PY-ID" TO LG-DETAIL-CD
              PERFORM 7000-WRITE-LOG
           END-IF

           IF PY-CARD-NO = SPACE
              MOVE "PY-CRD" TO LG-DETAIL-CD
              PERFORM 7000-WRITE-LOG
           END-IF

           IF PY-PAY-AMT <= ZERO
              MOVE "PY-AMT" TO LG-DETAIL-CD
              PERFORM 7000-WRITE-LOG
           END-IF

           IF PY-PAY-DT = ZERO OR PY-PAY-DT > WK-BATCH-DT
              MOVE "PY-DT" TO LG-DETAIL-CD
              PERFORM 7000-WRITE-LOG
           END-IF.

       4000-READ-ACCOUNT.
           MOVE "N" TO SW-ACC-FOUND
           MOVE PY-CARD-NO TO AC-CARD-NO
           READ CDACCF KEY IS AC-CARD-NO
              INVALID KEY
                 MOVE "N" TO SW-ACC-FOUND
              NOT INVALID KEY
                 ADD 1 TO CNT-AC-READ
                 MOVE "Y" TO SW-ACC-FOUND
           END-READ
           IF FS-CDACCF NOT = "00" AND FS-CDACCF NOT = "23"
              DISPLAY "CDACCF 読込失敗 ST=" FS-CDACCF
              PERFORM 9100-SET-ABEND
           END-IF.

       5000-CALC-OUTSTANDING.
           MOVE ZERO TO WK-BILL-BAL
                        WK-INT-BAL
                        WK-FEE-BAL
                        WK-PRIN-BAL
           MOVE "N" TO SW-OV-FOUND

           PERFORM UNTIL END-OV OR OV-CARD-NO >= PY-CARD-NO
                         OR ABEND-ON
              PERFORM 2200-READ-OV
           END-PERFORM

           PERFORM UNTIL END-OV OR OV-CARD-NO NOT = PY-CARD-NO
                         OR ABEND-ON
              MOVE "Y" TO SW-OV-FOUND
              IF OV-SETL-KBN = "D"
                 EVALUATE TRUE
                    WHEN OV-TXN-KBN = "A1"
                       ADD OV-SETL-AMT TO WK-FEE-BAL
                    WHEN OV-FEE-KBN NOT = "00"
                       ADD OV-FEE-AMT TO WK-FEE-BAL
                       COMPUTE WK-PRIN-BAL =
                          WK-PRIN-BAL + OV-SETL-AMT - OV-FEE-AMT
                    WHEN OV-TXN-KBN = "C1" OR OV-TXN-KBN = "C2"
                       ADD OV-SETL-AMT TO WK-PRIN-BAL
                    WHEN OV-TXN-KBN = "P1" OR OV-TXN-KBN = "P2"
                       ADD OV-SETL-AMT TO WK-PRIN-BAL
                    WHEN OTHER
                       MOVE "OV-KBN" TO LG-DETAIL-CD
                       PERFORM 7000-WRITE-LOG
                 END-EVALUATE

                 IF OV-INT-START-DT NOT = ZERO
                    IF OV-INT-START-DT < PY-PAY-DT
                       COMPUTE WK-INT-BAL ROUNDED =
                          WK-INT-BAL + (OV-SETL-AMT * 0.0005)
                    END-IF
                 END-IF
              END-IF
              PERFORM 2200-READ-OV
           END-PERFORM

           COMPUTE WK-BILL-BAL =
              WK-INT-BAL + WK-FEE-BAL + WK-PRIN-BAL

           IF NOT OV-FOUND
              MOVE "OV-NF" TO LG-DETAIL-CD
              PERFORM 7000-WRITE-LOG
           END-IF.

       6000-ALLOCATE-PAYMENT.
           MOVE PY-PAY-AMT TO WK-PAY-AMT
           MOVE PY-PAY-AMT TO WK-REMAIN-AMT
           MOVE ZERO TO WK-APPLIED-TOTAL
                        WK-UNAPPLIED-AMT
                        WK-DIFF-AMT

           PERFORM 6010-ALLOC-BILL
           PERFORM 6020-ALLOC-INTEREST
           PERFORM 6030-ALLOC-FEE
           PERFORM 6040-ALLOC-PRINCIPAL

           IF WK-REMAIN-AMT > ZERO
              MOVE WK-REMAIN-AMT TO WK-UNAPPLIED-AMT
           ELSE
              MOVE ZERO TO WK-UNAPPLIED-AMT
           END-IF

           COMPUTE WK-DIFF-AMT =
              WK-PAY-AMT - WK-APPLIED-TOTAL - WK-UNAPPLIED-AMT.

       6010-ALLOC-BILL.
           IF WK-BILL-BAL <= ZERO OR WK-REMAIN-AMT <= ZERO
              EXIT PARAGRAPH
           END-IF
           IF WK-REMAIN-AMT >= WK-BILL-BAL
              MOVE WK-BILL-BAL TO WK-ALLOC-AMT
           ELSE
              MOVE WK-REMAIN-AMT TO WK-ALLOC-AMT
           END-IF
           SUBTRACT WK-ALLOC-AMT FROM WK-REMAIN-AMT
           ADD WK-ALLOC-AMT TO WK-APPLIED-TOTAL.

       6020-ALLOC-INTEREST.
           IF WK-INT-BAL <= ZERO OR WK-REMAIN-AMT <= ZERO
              EXIT PARAGRAPH
           END-IF
           IF WK-REMAIN-AMT >= WK-INT-BAL
              MOVE WK-INT-BAL TO WK-ALLOC-AMT
           ELSE
              MOVE WK-REMAIN-AMT TO WK-ALLOC-AMT
           END-IF
           SUBTRACT WK-ALLOC-AMT FROM WK-REMAIN-AMT
           ADD WK-ALLOC-AMT TO WK-APPLIED-TOTAL.

       6030-ALLOC-FEE.
           IF WK-FEE-BAL <= ZERO OR WK-REMAIN-AMT <= ZERO
              EXIT PARAGRAPH
           END-IF
           IF WK-REMAIN-AMT >= WK-FEE-BAL
              MOVE WK-FEE-BAL TO WK-ALLOC-AMT
           ELSE
              MOVE WK-REMAIN-AMT TO WK-ALLOC-AMT
           END-IF
           SUBTRACT WK-ALLOC-AMT FROM WK-REMAIN-AMT
           ADD WK-ALLOC-AMT TO WK-APPLIED-TOTAL.

       6040-ALLOC-PRINCIPAL.
           IF WK-PRIN-BAL <= ZERO OR WK-REMAIN-AMT <= ZERO
              EXIT PARAGRAPH
           END-IF
           IF WK-REMAIN-AMT >= WK-PRIN-BAL
              MOVE WK-PRIN-BAL TO WK-ALLOC-AMT
           ELSE
              MOVE WK-REMAIN-AMT TO WK-ALLOC-AMT
           END-IF
           SUBTRACT WK-ALLOC-AMT FROM WK-REMAIN-AMT
           ADD WK-ALLOC-AMT TO WK-APPLIED-TOTAL.

       6100-UPDATE-PAYMENT.
           IF WK-UNAPPLIED-AMT > ZERO
              MOVE "9" TO PY-ALLOC-KBN
           ELSE
              MOVE "1" TO PY-ALLOC-KBN
           END-IF
           MOVE WK-UNAPPLIED-AMT TO PY-UNAPPLIED-AMT

           REWRITE CDPYMF-REC
           IF FS-CDPYMF = "00"
              ADD 1 TO CNT-PY-REWRITE
           ELSE
              DISPLAY "CDPYMF 更新失敗 ST=" FS-CDPYMF
              PERFORM 9100-SET-ABEND
           END-IF.

       6200-SET-PY-UNAPPLIED.
           MOVE PY-PAY-AMT TO PY-UNAPPLIED-AMT
           MOVE "9" TO PY-ALLOC-KBN
           REWRITE CDPYMF-REC
           IF FS-CDPYMF = "00"
              ADD 1 TO CNT-PY-REWRITE
           ELSE
              DISPLAY "CDPYMF 未消込更新失敗 ST=" FS-CDPYMF
              PERFORM 9100-SET-ABEND
           END-IF.

       6300-UPDATE-ACCOUNT.
           MOVE AC-USED-AMT TO WK-OLD-USED-AMT
           COMPUTE WK-NEW-USED-AMT =
              AC-USED-AMT - WK-APPLIED-TOTAL
           IF WK-NEW-USED-AMT < ZERO
              MOVE ZERO TO WK-NEW-USED-AMT
              MOVE "USED" TO LG-DETAIL-CD
              PERFORM 7000-WRITE-LOG
           END-IF

           MOVE WK-NEW-USED-AMT TO AC-USED-AMT
           MOVE WK-BATCH-DT TO AC-LAST-UPD-DT

           IF AC-USED-AMT <= ZERO
              MOVE "0" TO AC-DELAY-KBN
           ELSE
              IF WK-UNAPPLIED-AMT > ZERO
                 MOVE "1" TO AC-DELAY-KBN
              ELSE
                 MOVE "0" TO AC-DELAY-KBN
              END-IF
           END-IF

           IF AC-STATUS-KBN NOT = "0" AND AC-STATUS-KBN NOT = "1"
              MOVE "AC-ST" TO LG-DETAIL-CD
              PERFORM 7000-WRITE-LOG
           END-IF

           REWRITE CDACCF-REC
           IF FS-CDACCF = "00"
              ADD 1 TO CNT-AC-REWRITE
           ELSE
              DISPLAY "CDACCF 更新失敗 ST=" FS-CDACCF
              PERFORM 9100-SET-ABEND
           END-IF.

       7000-WRITE-LOG.
           IF ABEND-ON
              EXIT PARAGRAPH
           END-IF
           ADD 1 TO WK-LOG-SEQ
           ADD 1 TO CNT-ERROR
           MOVE WK-LOG-SEQ TO LG-LOG-ID
           MOVE WK-PROGRAM-ID TO LG-PROGRAM-ID
           MOVE PY-CARD-NO TO LG-CARD-NO
           MOVE "E" TO LG-EVENT-KBN
           MOVE WK-BATCH-DT TO LG-EVENT-DT

           WRITE CDLOGF-REC
           IF FS-CDLOGF = "00"
              ADD 1 TO CNT-LOG-WRITE
           ELSE
              DISPLAY "CDLOGF 書込失敗 ST=" FS-CDLOGF
              PERFORM 9100-SET-ABEND
           END-IF.

       9000-FINALIZE.
           IF FS-CDPYMF NOT = SPACE
              CLOSE CDPYMF
              IF FS-CDPYMF NOT = "00"
                 DISPLAY "CDPYMF クローズ失敗 ST=" FS-CDPYMF
                 PERFORM 9100-SET-ABEND
              END-IF
           END-IF

           IF FS-CDOVSF NOT = SPACE
              CLOSE CDOVSF
              IF FS-CDOVSF NOT = "00"
                 DISPLAY "CDOVSF クローズ失敗 ST=" FS-CDOVSF
                 PERFORM 9100-SET-ABEND
              END-IF
           END-IF

           IF FS-CDACCF NOT = SPACE
              CLOSE CDACCF
              IF FS-CDACCF NOT = "00"
                 DISPLAY "CDACCF クローズ失敗 ST=" FS-CDACCF
                 PERFORM 9100-SET-ABEND
              END-IF
           END-IF

           IF FS-CDLOGF NOT = SPACE
              CLOSE CDLOGF
              IF FS-CDLOGF NOT = "00"
                 DISPLAY "CDLOGF クローズ失敗 ST=" FS-CDLOGF
                 PERFORM 9100-SET-ABEND
              END-IF
           END-IF

           DISPLAY "CB112B 入金読込件数=" CNT-PY-READ
           DISPLAY "CB112B 入金更新件数=" CNT-PY-REWRITE
           DISPLAY "CB112B 債権読込件数=" CNT-OV-READ
           DISPLAY "CB112B 口座読込件数=" CNT-AC-READ
           DISPLAY "CB112B 口座更新件数=" CNT-AC-REWRITE
           DISPLAY "CB112B ログ出力件数=" CNT-LOG-WRITE
           DISPLAY "CB112B 検出エラー件数=" CNT-ERROR

           IF ABEND-ON
              MOVE 8 TO RETURN-CODE
           ELSE
              MOVE 0 TO RETURN-CODE
           END-IF.

       9100-SET-ABEND.
           SET ABEND-ON TO TRUE
           MOVE 8 TO RETURN-CODE.
