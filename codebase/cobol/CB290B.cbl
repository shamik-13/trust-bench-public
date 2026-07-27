       IDENTIFICATION DIVISION.
       PROGRAM-ID. CB290B.
      ******************************************************************
      * 変更履歴
      * 版数  年月日    担当    概要
      * 1.00  20240215  ＣＤ運用 初版作成
      * 1.01  20240520  ＣＤ運用 年会費明細索引対象を追加
      * 1.02  20240910  ＣＤ運用 公開ステータス判定を請求単位へ統一
      ******************************************************************
      * 請求確定済み請求情報を売上・年会費明細と突合し、照会索引を作成
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CDBILLF
               ASSIGN TO "CDBILLF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS FS-CDBILLF.

           SELECT CDSALESF
               ASSIGN TO "CDSALESF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS FS-CDSALESF.

           SELECT CDFEEF
               ASSIGN TO "CDFEEF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS FS-CDFEEF.

           SELECT CDSTMTIDXF
               ASSIGN TO "CDSTMTIDXF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS SI-CARD-NO
               FILE STATUS IS FS-CDSTMTIDXF.

       DATA DIVISION.
       FILE SECTION.
       FD  CDBILLF.
           COPY CDBILLFC.

       FD  CDSALESF.
           COPY CDSALEC.

       FD  CDFEEF.
           COPY CDFEEC.

       FD  CDSTMTIDXF.
           COPY CDSIDXC.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05 FS-CDBILLF       PIC X(02) VALUE SPACE.
           05 FS-CDSALESF      PIC X(02) VALUE SPACE.
           05 FS-CDFEEF        PIC X(02) VALUE SPACE.
           05 FS-CDSTMTIDXF    PIC X(02) VALUE SPACE.

       01  WS-FLAGS.
           05 WS-END-BILL      PIC X VALUE "N".
              88 BILL-END            VALUE "Y".
              88 BILL-NOT-END        VALUE "N".
           05 WS-END-SALES     PIC X VALUE "N".
              88 SALES-END           VALUE "Y".
              88 SALES-NOT-END       VALUE "N".
           05 WS-END-FEE       PIC X VALUE "N".
              88 FEE-END             VALUE "Y".
              88 FEE-NOT-END         VALUE "N".
           05 WS-HARD-ERROR    PIC X VALUE "N".
              88 HARD-ERROR          VALUE "Y".
              88 NO-HARD-ERROR       VALUE "N".
           05 WS-DETAIL-FOUND  PIC X VALUE "N".
              88 DETAIL-FOUND        VALUE "Y".
              88 DETAIL-NOT-FOUND    VALUE "N".

       01  WS-COUNTERS.
           05 WS-BILL-READ-CNT PIC 9(09) VALUE ZERO.
           05 WS-BILL-SKIP-CNT PIC 9(09) VALUE ZERO.
           05 WS-SALES-HIT-CNT PIC 9(09) VALUE ZERO.
           05 WS-FEE-HIT-CNT   PIC 9(09) VALUE ZERO.
           05 WS-IDX-WRITE-CNT PIC 9(09) VALUE ZERO.
           05 WS-IDX-DUP-CNT   PIC 9(09) VALUE ZERO.
           05 WS-ERR-CNT       PIC 9(09) VALUE ZERO.
           05 WS-STMT-SEQ      PIC 9(06) VALUE ZERO.

       01  WS-DATE-AREA.
           05 WS-BI-CYCLE-X    PIC X(08) VALUE SPACE.
           05 WS-BI-YYYYMM     PIC X(06) VALUE SPACE.
           05 WS-SL-POST-X     PIC X(08) VALUE SPACE.
           05 WS-SL-YYYYMM     PIC X(06) VALUE SPACE.
           05 WS-FE-DATE-X     PIC X(08) VALUE SPACE.
           05 WS-FE-YYYYMM     PIC X(06) VALUE SPACE.

       01  WS-AMOUNT-AREA.
           05 WS-DETAIL-AMT    PIC S9(13)V99 COMP-3 VALUE ZERO.
           05 WS-SALES-AMT     PIC S9(13)V99 COMP-3 VALUE ZERO.
           05 WS-FEE-AMT       PIC S9(13)V99 COMP-3 VALUE ZERO.

       01  WS-STMT-ID-AREA.
           05 WS-SID-CARD      PIC X(16) VALUE SPACE.
           05 WS-SID-CYCLE     PIC X(08) VALUE SPACE.
           05 WS-SID-SEQ       PIC 9(06) VALUE ZERO.
           05 WS-SID-TEXT.
              10 WS-SID-T-CARD PIC X(16).
              10 WS-SID-T-CYCL PIC X(08).
              10 WS-SID-T-SEQ  PIC 9(06).

           COPY LK-BILLED-PARM.

       PROCEDURE DIVISION.
       MAIN-SECTION.
           PERFORM 0000-INIT
           IF NO-HARD-ERROR
               PERFORM 1000-MAIN UNTIL BILL-END OR HARD-ERROR
           END-IF
           PERFORM 9000-FINAL
           GOBACK.

       0000-INIT.
           MOVE 0 TO RETURN-CODE
           SET NO-HARD-ERROR TO TRUE
           SET BILL-NOT-END TO TRUE

           OPEN INPUT CDBILLF
           IF FS-CDBILLF NOT = "00"
               DISPLAY "CB290B CDBILLF OPEN ST=" FS-CDBILLF
               SET HARD-ERROR TO TRUE
               ADD 1 TO WS-ERR-CNT
           END-IF

           IF NO-HARD-ERROR
               OPEN OUTPUT CDSTMTIDXF
               IF FS-CDSTMTIDXF NOT = "00"
                   DISPLAY "CB290B CDSTMTIDXF OPEN ST="
                           FS-CDSTMTIDXF
                   SET HARD-ERROR TO TRUE
                   ADD 1 TO WS-ERR-CNT
               END-IF
           END-IF

           IF NO-HARD-ERROR
               PERFORM 1100-READ-BILL
           END-IF.

       1000-MAIN.
           ADD 1 TO WS-BILL-READ-CNT

           IF BI-CARD-NO = SPACE
               DISPLAY "CB290B カード番号未設定"
               ADD 1 TO WS-BILL-SKIP-CNT
           ELSE
               EVALUATE BI-BILL-STATUS
                   WHEN "C "
                       PERFORM 2000-PROCESS-BILL
                   WHEN "H "
                       ADD 1 TO WS-BILL-SKIP-CNT
                   WHEN "S "
                       ADD 1 TO WS-BILL-SKIP-CNT
                   WHEN OTHER
                       DISPLAY "CB290B 請求状態不正 CARD="
                               BI-CARD-NO
                               " 状態=" BI-BILL-STATUS
                       ADD 1 TO WS-BILL-SKIP-CNT
                       ADD 1 TO WS-ERR-CNT
               END-EVALUATE
           END-IF

           IF NO-HARD-ERROR
               PERFORM 1100-READ-BILL
           END-IF.

       1100-READ-BILL.
           READ CDBILLF
               AT END
                   SET BILL-END TO TRUE
               NOT AT END
                   IF FS-CDBILLF NOT = "00"
                       DISPLAY "CB290B CDBILLF READ ST="
                               FS-CDBILLF
                       SET HARD-ERROR TO TRUE
                       ADD 1 TO WS-ERR-CNT
                   END-IF
           END-READ.

       2000-PROCESS-BILL.
           SET DETAIL-NOT-FOUND TO TRUE
           MOVE ZERO TO WS-DETAIL-AMT
           MOVE ZERO TO WS-SALES-AMT
           MOVE ZERO TO WS-FEE-AMT
           MOVE BI-CYCLE-DT TO WS-BI-CYCLE-X
           MOVE WS-BI-CYCLE-X(1:6) TO WS-BI-YYYYMM

           PERFORM 2100-SCAN-SALES
           IF NO-HARD-ERROR
               PERFORM 2200-SCAN-FEE
           END-IF

           IF NO-HARD-ERROR
               IF DETAIL-FOUND
                   PERFORM 3000-CALL-EDIT
                   IF NO-HARD-ERROR
                       PERFORM 4000-WRITE-INDEX
                   END-IF
               ELSE
                   DISPLAY "CB290B 明細なし CARD=" BI-CARD-NO
                           " CY=" BI-CYCLE-DT
                   ADD 1 TO WS-BILL-SKIP-CNT
               END-IF
           END-IF.

       2100-SCAN-SALES.
           SET SALES-NOT-END TO TRUE
           OPEN INPUT CDSALESF
           IF FS-CDSALESF NOT = "00"
               DISPLAY "CB290B CDSALESF OPEN ST=" FS-CDSALESF
               SET HARD-ERROR TO TRUE
               ADD 1 TO WS-ERR-CNT
           END-IF

           IF NO-HARD-ERROR
               PERFORM UNTIL SALES-END OR HARD-ERROR
                   READ CDSALESF
                       AT END
                           SET SALES-END TO TRUE
                       NOT AT END
                           IF FS-CDSALESF = "00"
                               PERFORM 2110-CHECK-SALES
                           ELSE
                               DISPLAY "CB290B CDSALESF READ ST="
                                       FS-CDSALESF
                               SET HARD-ERROR TO TRUE
                               ADD 1 TO WS-ERR-CNT
                           END-IF
                   END-READ
               END-PERFORM
           END-IF

           CLOSE CDSALESF
           IF FS-CDSALESF NOT = "00"
               DISPLAY "CB290B CDSALESF CLOSE ST=" FS-CDSALESF
               SET HARD-ERROR TO TRUE
               ADD 1 TO WS-ERR-CNT
           END-IF.

       2110-CHECK-SALES.
           IF SL-CARD-NO = BI-CARD-NO
               MOVE SL-POSTING-DT TO WS-SL-POST-X
               MOVE WS-SL-POST-X(1:6) TO WS-SL-YYYYMM
               IF WS-SL-YYYYMM = WS-BI-YYYYMM
                   IF SL-CAPTURE-STATUS = "C"
                       ADD SL-SALES-AMT TO WS-SALES-AMT
                       ADD SL-TAX-AMT TO WS-SALES-AMT
                       SET DETAIL-FOUND TO TRUE
                       ADD 1 TO WS-SALES-HIT-CNT
                   ELSE
                       DISPLAY "CB290B 売上状態対象外 ID="
                               SL-SALES-ID
                               " ST=" SL-CAPTURE-STATUS
                   END-IF
               END-IF
           END-IF.

       2200-SCAN-FEE.
           SET FEE-NOT-END TO TRUE
           OPEN INPUT CDFEEF
           IF FS-CDFEEF NOT = "00"
               DISPLAY "CB290B CDFEEF OPEN ST=" FS-CDFEEF
               SET HARD-ERROR TO TRUE
               ADD 1 TO WS-ERR-CNT
           END-IF

           IF NO-HARD-ERROR
               PERFORM UNTIL FEE-END OR HARD-ERROR
                   READ CDFEEF
                       AT END
                           SET FEE-END TO TRUE
                       NOT AT END
                           IF FS-CDFEEF = "00"
                               PERFORM 2210-CHECK-FEE
                           ELSE
                               DISPLAY "CB290B CDFEEF READ ST="
                                       FS-CDFEEF
                               SET HARD-ERROR TO TRUE
                               ADD 1 TO WS-ERR-CNT
                           END-IF
                   END-READ
               END-PERFORM
           END-IF

           CLOSE CDFEEF
           IF FS-CDFEEF NOT = "00"
               DISPLAY "CB290B CDFEEF CLOSE ST=" FS-CDFEEF
               SET HARD-ERROR TO TRUE
               ADD 1 TO WS-ERR-CNT
           END-IF.

       2210-CHECK-FEE.
           IF FE-CARD-NO = BI-CARD-NO
               MOVE FE-FEE-DT TO WS-FE-DATE-X
               MOVE WS-FE-DATE-X(1:6) TO WS-FE-YYYYMM
               IF WS-FE-YYYYMM = WS-BI-YYYYMM
                   IF FE-POST-STATUS = "C"
                       ADD FE-FEE-AMT TO WS-FEE-AMT
                       SET DETAIL-FOUND TO TRUE
                       ADD 1 TO WS-FEE-HIT-CNT
                   ELSE
                       DISPLAY "CB290B 年会費状態対象外 ID="
                               FE-FEE-ID
                               " ST=" FE-POST-STATUS
                   END-IF
               END-IF
           END-IF.

       3000-CALL-EDIT.
           INITIALIZE LK-BILLED-PARM
           MOVE BI-CARD-NO  TO LK-BE-CARD-NO
           MOVE BI-CYCLE-DT TO LK-BE-CYCLE-DT
           MOVE BI-BILL-AMT TO LK-BE-BILL-AMT
           MOVE BI-DUE-DT   TO LK-BE-DUE-DT

           CALL "CB920S" USING LK-BILLED-PARM

           IF LK-BE-RET NOT = ZERO
               DISPLAY "CB290B 明細編集エラー CARD=" BI-CARD-NO
                       " CY=" BI-CYCLE-DT
                       " RC=" LK-BE-RET
               ADD 1 TO WS-ERR-CNT
               SET HARD-ERROR TO TRUE
           END-IF.

       4000-WRITE-INDEX.
           INITIALIZE CDSTMTIDXF-REC
           ADD 1 TO WS-STMT-SEQ
           MOVE BI-CARD-NO TO WS-SID-CARD
           MOVE BI-CYCLE-DT TO WS-SID-CYCLE
           MOVE WS-STMT-SEQ TO WS-SID-SEQ
           MOVE WS-SID-CARD TO WS-SID-T-CARD
           MOVE WS-SID-CYCLE TO WS-SID-T-CYCL
           MOVE WS-SID-SEQ TO WS-SID-T-SEQ

           ADD WS-SALES-AMT TO WS-DETAIL-AMT
           ADD WS-FEE-AMT TO WS-DETAIL-AMT

           MOVE BI-CARD-NO TO SI-CARD-NO
           MOVE BI-CYCLE-DT TO SI-CYCLE-DT
           MOVE WS-SID-TEXT TO SI-STATEMENT-ID
           MOVE BI-BILL-AMT TO SI-BILL-AMT
           MOVE BI-DUE-DT TO SI-DUE-DT

           IF BI-BILL-AMT = WS-DETAIL-AMT
               MOVE "1" TO SI-PUBLISH-STATUS
           ELSE
               MOVE "0" TO SI-PUBLISH-STATUS
               DISPLAY "CB290B 金額不一致 CARD=" BI-CARD-NO
                       " CY=" BI-CYCLE-DT
           END-IF

           WRITE CDSTMTIDXF-REC
           EVALUATE FS-CDSTMTIDXF
               WHEN "00"
                   ADD 1 TO WS-IDX-WRITE-CNT
               WHEN "22"
                   DISPLAY "CB290B 索引重複 CARD=" SI-CARD-NO
                   ADD 1 TO WS-IDX-DUP-CNT
                   ADD 1 TO WS-ERR-CNT
               WHEN OTHER
                   DISPLAY "CB290B CDSTMTIDXF WRITE ST="
                           FS-CDSTMTIDXF
                           " CARD=" SI-CARD-NO
                   SET HARD-ERROR TO TRUE
                   ADD 1 TO WS-ERR-CNT
           END-EVALUATE.

       9000-FINAL.
           IF FS-CDBILLF = "00"
               CLOSE CDBILLF
               IF FS-CDBILLF NOT = "00"
                   DISPLAY "CB290B CDBILLF CLOSE ST="
                           FS-CDBILLF
                   SET HARD-ERROR TO TRUE
                   ADD 1 TO WS-ERR-CNT
               END-IF
           END-IF

           IF FS-CDSTMTIDXF = "00" OR FS-CDSTMTIDXF = "22"
               CLOSE CDSTMTIDXF
               IF FS-CDSTMTIDXF NOT = "00"
                   DISPLAY "CB290B CDSTMTIDXF CLOSE ST="
                           FS-CDSTMTIDXF
                   SET HARD-ERROR TO TRUE
                   ADD 1 TO WS-ERR-CNT
               END-IF
           END-IF

           DISPLAY "CB290B 終了 請求件数=" WS-BILL-READ-CNT
                   " 索引件数=" WS-IDX-WRITE-CNT
                   " 対象外件数=" WS-BILL-SKIP-CNT
                   " エラー件数=" WS-ERR-CNT

           IF HARD-ERROR
               MOVE 8 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.
