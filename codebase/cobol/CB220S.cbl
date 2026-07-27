       IDENTIFICATION DIVISION.
       PROGRAM-ID. CB220S.
      ******************************************************************
      * 変更履歴
      * 版数  年月日    担当  概要
      * 01.00 20240201  開発  区分別手数料候補算定を新規作成
      * 01.01 20240520  保守  円未満切上げから一円丸めへ変更
      * 01.02 20240910  保守  丸め単位を十円単位から一円単位へ戻し
      ******************************************************************
      * 確定明細から汎用的な国際ブランド手数料候補を算定する。
      * 海外ＡＴＭ専用事務手数料および利息開始日は扱わない。
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CDFXRF ASSIGN TO "CDFXRF"
               ORGANIZATION IS INDEXED
               ACCESS MODE  IS DYNAMIC
               RECORD KEY   IS FX-RATE-DT
               FILE STATUS  IS CDFXRF-ST.

           SELECT CDOVSF ASSIGN TO "CDOVSF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE  IS SEQUENTIAL
               FILE STATUS  IS CDOVSF-ST.

           SELECT CDLOGF ASSIGN TO "CDLOGF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE  IS SEQUENTIAL
               FILE STATUS  IS CDLOGF-ST.

       DATA DIVISION.
       FILE SECTION.
       FD  CDFXRF.
           COPY CDFXRFC.

       FD  CDOVSF.
           COPY CDOVSFC.

       FD  CDLOGF.
           COPY CDLOGFC.

       WORKING-STORAGE SECTION.
       01  CDFXRF-ST             PIC XX.
       01  CDOVSF-ST             PIC XX.
       01  CDLOGF-ST             PIC XX.

       01  WS-PROGRAM-ID         PIC X(08) VALUE "CB220S".
       01  WS-EOF-SW             PIC X VALUE "0".
           88  WS-EOF                  VALUE "1".
           88  WS-NOT-EOF              VALUE "0".
       01  WS-FX-FOUND-SW        PIC X VALUE "0".
           88  WS-FX-FOUND             VALUE "1".
           88  WS-FX-NOT-FOUND         VALUE "0".
       01  WS-HARD-ERR-SW        PIC X VALUE "0".
           88  WS-HARD-ERR             VALUE "1".
           88  WS-NORMAL               VALUE "0".

       01  WS-CURRENT-DATE.
           05  WS-CUR-YYYY       PIC 9(04).
           05  WS-CUR-MM         PIC 9(02).
           05  WS-CUR-DD         PIC 9(02).
           05  FILLER            PIC X(13).
       01  WS-EVENT-DT           PIC 9(08).
       01  WS-RATE-DT            PIC 9(08).

       01  WS-READ-CNT           PIC 9(09) VALUE ZERO.
       01  WS-SKIP-CNT           PIC 9(09) VALUE ZERO.
       01  WS-CALC-CNT           PIC 9(09) VALUE ZERO.
       01  WS-ERR-CNT            PIC 9(09) VALUE ZERO.
       01  WS-LOG-SEQ            PIC 9(09) VALUE ZERO.

       01  WS-BASE-AMT           PIC S9(13)V99 COMP-3 VALUE ZERO.
       01  WS-RATE-AMT           PIC S9(13)V99 COMP-3 VALUE ZERO.
       01  WS-FEE-RAW            PIC S9(13)V9999 COMP-3 VALUE ZERO.
       01  WS-FEE-ROUNDED        PIC S9(13) COMP-3 VALUE ZERO.
       01  WS-ZERO-AMT           PIC S9(13)V99 COMP-3 VALUE ZERO.

       01  WS-TARGET-BRAND       PIC X(02).
       01  WS-TARGET-CCY         PIC X(03).
       01  WS-DETAIL-CD          PIC X(08).

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE 8 TO RETURN-CODE
           PERFORM 1000-INIT
           IF WS-NORMAL
               PERFORM 2000-PROCESS UNTIL WS-EOF OR WS-HARD-ERR
           END-IF
           PERFORM 9000-FINAL
           GOBACK.

       1000-INIT.
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE
           COMPUTE WS-EVENT-DT =
               WS-CUR-YYYY * 10000 + WS-CUR-MM * 100 + WS-CUR-DD
           MOVE WS-EVENT-DT TO WS-RATE-DT
           SET WS-NOT-EOF TO TRUE
           SET WS-NORMAL TO TRUE

           OPEN INPUT CDFXRF
           IF CDFXRF-ST NOT = "00"
               DISPLAY "CDFXRF オープン失敗 ST=" CDFXRF-ST
               SET WS-HARD-ERR TO TRUE
           END-IF

           IF WS-NORMAL
               OPEN INPUT CDOVSF
               IF CDOVSF-ST NOT = "00"
                   DISPLAY "CDOVSF オープン失敗 ST=" CDOVSF-ST
                   SET WS-HARD-ERR TO TRUE
               END-IF
           END-IF

           IF WS-NORMAL
               OPEN OUTPUT CDLOGF
               IF CDLOGF-ST NOT = "00"
                   DISPLAY "CDLOGF オープン失敗 ST=" CDLOGF-ST
                   SET WS-HARD-ERR TO TRUE
               END-IF
           END-IF

           IF WS-NORMAL
               PERFORM 2100-READ-OV
           END-IF.

       2000-PROCESS.
           ADD 1 TO WS-READ-CNT
           MOVE SPACE TO WS-DETAIL-CD

           EVALUATE TRUE
               WHEN OV-SETL-KBN NOT = "D"
                   MOVE "SKP-HORY" TO WS-DETAIL-CD
                   ADD 1 TO WS-SKIP-CNT
                   PERFORM 5000-WRITE-LOG

               WHEN OV-TXN-KBN NOT = "P2" AND OV-TXN-KBN NOT = "C2"
                   MOVE "SKP-KOKU" TO WS-DETAIL-CD
                   ADD 1 TO WS-SKIP-CNT
                   PERFORM 5000-WRITE-LOG

               WHEN OV-FEE-KBN = "FA"
                   MOVE "SKP-ATM " TO WS-DETAIL-CD
                   ADD 1 TO WS-SKIP-CNT
                   PERFORM 5000-WRITE-LOG

               WHEN OV-SETL-AMT <= WS-ZERO-AMT
                   MOVE "ERR-KING" TO WS-DETAIL-CD
                   ADD 1 TO WS-ERR-CNT
                   PERFORM 5000-WRITE-LOG

               WHEN OTHER
                   PERFORM 3000-CALC-FEE
           END-EVALUATE

           IF WS-NORMAL
               PERFORM 2100-READ-OV
           END-IF.

       2100-READ-OV.
           READ CDOVSF
               AT END
                   SET WS-EOF TO TRUE
               NOT AT END
                   IF CDOVSF-ST NOT = "00"
                       DISPLAY "CDOVSF 読込失敗 ST=" CDOVSF-ST
                       SET WS-HARD-ERR TO TRUE
                   END-IF
           END-READ.

       3000-CALC-FEE.
           PERFORM 3100-SET-FX-KEY
           PERFORM 3200-FIND-FX

           IF WS-FX-NOT-FOUND
               MOVE "ERR-RATE" TO WS-DETAIL-CD
               ADD 1 TO WS-ERR-CNT
               PERFORM 5000-WRITE-LOG
           ELSE
               MOVE OV-SETL-AMT TO WS-BASE-AMT
               COMPUTE WS-RATE-AMT ROUNDED =
                   WS-BASE-AMT * FX-FX-RATE
               COMPUTE WS-FEE-RAW =
                   WS-RATE-AMT * FX-MARKUP-RATE / 100
               COMPUTE WS-FEE-ROUNDED ROUNDED = WS-FEE-RAW
               MOVE "CALC-FB " TO WS-DETAIL-CD
               ADD 1 TO WS-CALC-CNT
               PERFORM 5000-WRITE-LOG
           END-IF.

       3100-SET-FX-KEY.
           EVALUATE OV-TXN-KBN
               WHEN "P2"
                   MOVE "05"  TO WS-TARGET-BRAND
                   MOVE "USD" TO WS-TARGET-CCY
               WHEN "C2"
                   MOVE "04"  TO WS-TARGET-BRAND
                   MOVE "USD" TO WS-TARGET-CCY
               WHEN OTHER
                   MOVE SPACE TO WS-TARGET-BRAND
                   MOVE SPACE TO WS-TARGET-CCY
           END-EVALUATE.

       3200-FIND-FX.
           SET WS-FX-NOT-FOUND TO TRUE
           MOVE WS-RATE-DT TO FX-RATE-DT

           START CDFXRF KEY IS EQUAL TO FX-RATE-DT
               INVALID KEY
                   SET WS-FX-NOT-FOUND TO TRUE
               NOT INVALID KEY
                   PERFORM 3210-READ-FX
                       UNTIL WS-FX-FOUND OR CDFXRF-ST NOT = "00"
           END-START.

       3210-READ-FX.
           READ CDFXRF NEXT RECORD
               AT END
                   MOVE "10" TO CDFXRF-ST
               NOT AT END
                   IF CDFXRF-ST = "00"
                       IF FX-RATE-DT = WS-RATE-DT
                          AND FX-BRAND-KBN = WS-TARGET-BRAND
                          AND FX-CCY-CD = WS-TARGET-CCY
                           SET WS-FX-FOUND TO TRUE
                       END-IF
                       IF FX-RATE-DT NOT = WS-RATE-DT
                           MOVE "10" TO CDFXRF-ST
                       END-IF
                   ELSE
                       DISPLAY "CDFXRF 読込失敗 ST=" CDFXRF-ST
                       SET WS-HARD-ERR TO TRUE
                   END-IF
           END-READ.

       5000-WRITE-LOG.
           ADD 1 TO WS-LOG-SEQ
           MOVE LOW-VALUE TO CDLOGF-REC
           MOVE WS-LOG-SEQ    TO LG-LOG-ID
           MOVE WS-PROGRAM-ID TO LG-PROGRAM-ID
           MOVE OV-CARD-NO    TO LG-CARD-NO
           MOVE "FEE"         TO LG-EVENT-KBN
           MOVE WS-EVENT-DT   TO LG-EVENT-DT
           MOVE WS-DETAIL-CD  TO LG-DETAIL-CD

           WRITE CDLOGF-REC
           IF CDLOGF-ST NOT = "00"
               DISPLAY "CDLOGF 書込失敗 ST=" CDLOGF-ST
               SET WS-HARD-ERR TO TRUE
           END-IF.

       9000-FINAL.
           IF CDFXRF-ST NOT = SPACE
               CLOSE CDFXRF
               IF CDFXRF-ST NOT = "00" AND CDFXRF-ST NOT = "42"
                   DISPLAY "CDFXRF クローズ失敗 ST=" CDFXRF-ST
                   SET WS-HARD-ERR TO TRUE
               END-IF
           END-IF

           IF CDOVSF-ST NOT = SPACE
               CLOSE CDOVSF
               IF CDOVSF-ST NOT = "00" AND CDOVSF-ST NOT = "42"
                   DISPLAY "CDOVSF クローズ失敗 ST=" CDOVSF-ST
                   SET WS-HARD-ERR TO TRUE
               END-IF
           END-IF

           IF CDLOGF-ST NOT = SPACE
               CLOSE CDLOGF
               IF CDLOGF-ST NOT = "00" AND CDLOGF-ST NOT = "42"
                   DISPLAY "CDLOGF クローズ失敗 ST=" CDLOGF-ST
                   SET WS-HARD-ERR TO TRUE
               END-IF
           END-IF

           DISPLAY "CB220S 読込件数=" WS-READ-CNT
           DISPLAY "CB220S 算定件数=" WS-CALC-CNT
           DISPLAY "CB220S 除外件数=" WS-SKIP-CNT
           DISPLAY "CB220S エラー件数=" WS-ERR-CNT

           IF WS-HARD-ERR
               MOVE 12 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.
