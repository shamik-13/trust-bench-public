       IDENTIFICATION DIVISION.
       PROGRAM-ID. CB320B.
      ******************************************************************
      * 変更履歴
      * 版数  年月日    担当    概要
      * 01.00 20240201  ＣＤ運用 初版作成
      * 01.01 20240515  ＣＤ運用 サイクル日突合と再実行判定を追加
      * 01.02 20240930  ＣＤ運用 延滞カード抽出件数と明細索引作成を追加
      * 01.03 20241205  ＣＤ運用 最低支払額算定をCB910S呼出へ変更
      ******************************************************************
      * プログラム名 : CB320B
      * 概要         : サイクル締め統合制御
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CDCARDF ASSIGN TO "CDCARDF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS CF-CARD-NO
               FILE STATUS IS FS-CDCARDF.
           SELECT CDBALF ASSIGN TO "CDBALF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS FS-CDBALF.
           SELECT CDFEEF ASSIGN TO "CDFEEF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS FS-CDFEEF.
           SELECT CDSALESF ASSIGN TO "CDSALESF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS FS-CDSALESF.
           SELECT CDBILLF ASSIGN TO "CDBILLF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS FS-CDBILLF.
           SELECT CDSTMTIDXF ASSIGN TO "CDSTMTIDXF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS SI-CARD-NO
               FILE STATUS IS FS-CDSTMTIDXF.

       DATA DIVISION.
       FILE SECTION.
       FD  CDCARDF.
           COPY CDCARDFC.
       FD  CDBALF.
           COPY CDBALFC.
       FD  CDFEEF.
           COPY CDFEEC.
       FD  CDSALESF.
           COPY CDSALEC.
       FD  CDBILLF.
           COPY CDBILLFC.
       FD  CDSTMTIDXF.
           COPY CDSIDXC.

       WORKING-STORAGE SECTION.
       01  FS-AREA.
           05 FS-CDCARDF             PIC XX VALUE SPACES.
           05 FS-CDBALF              PIC XX VALUE SPACES.
           05 FS-CDFEEF              PIC XX VALUE SPACES.
           05 FS-CDSALESF            PIC XX VALUE SPACES.
           05 FS-CDBILLF             PIC XX VALUE SPACES.
           05 FS-CDSTMTIDXF          PIC XX VALUE SPACES.

       01  EOF-AREA.
           05 EOF-CDCARDF            PIC X VALUE 'N'.
           05 EOF-CDBALF             PIC X VALUE 'N'.
           05 EOF-CDFEEF             PIC X VALUE 'N'.
           05 EOF-CDSALESF           PIC X VALUE 'N'.

       01  RUN-AREA.
           05 WS-PROGRAM-ID          PIC X(08) VALUE 'CB320B'.
           05 WS-RUN-DATE            PIC 9(08) VALUE ZERO.
           05 WS-CYCLE-DT            PIC 9(08) VALUE ZERO.
           05 WS-CYCLE-CD            PIC X(02) VALUE SPACES.
           05 WS-DUE-DT              PIC 9(08) VALUE ZERO.
           05 WS-CHECKPOINT          PIC X(20) VALUE SPACES.
           05 WS-HARD-ERROR          PIC X VALUE 'N'.
           05 WS-SKIP-CARD           PIC X VALUE 'N'.
           05 WS-BILL-HOLD           PIC X VALUE 'N'.
           05 WS-BALANCE-FOUND       PIC X VALUE 'N'.
           05 WS-STATEMENT-SEQ       PIC 9(09) VALUE ZERO.

       01  TOTAL-AREA.
           05 WS-CARD-IN-CNT         PIC 9(09) VALUE ZERO.
           05 WS-CARD-ACT-CNT        PIC 9(09) VALUE ZERO.
           05 WS-CARD-SKP-CNT        PIC 9(09) VALUE ZERO.
           05 WS-CARD-DELQ-CNT       PIC 9(09) VALUE ZERO.
           05 WS-BAL-IN-CNT          PIC 9(09) VALUE ZERO.
           05 WS-FEE-IN-CNT          PIC 9(09) VALUE ZERO.
           05 WS-SALES-IN-CNT        PIC 9(09) VALUE ZERO.
           05 WS-BILL-OUT-CNT        PIC 9(09) VALUE ZERO.
           05 WS-IDX-OUT-CNT         PIC 9(09) VALUE ZERO.
           05 WS-CYCLE-MIS-CNT       PIC 9(09) VALUE ZERO.
           05 WS-SALES-TOTAL         PIC S9(13)V99 VALUE ZERO.
           05 WS-TAX-TOTAL           PIC S9(13)V99 VALUE ZERO.
           05 WS-FEE-TOTAL           PIC S9(13)V99 VALUE ZERO.
           05 WS-BAL-TOTAL           PIC S9(13)V99 VALUE ZERO.
           05 WS-BILL-TOTAL          PIC S9(13)V99 VALUE ZERO.

       01  CARD-WORK.
           05 WS-CF-CARD-NO          PIC X(16) VALUE SPACES.
           05 WS-CF-STATUS           PIC X(02) VALUE SPACES.
           05 WS-CF-CYCLE-CD         PIC X(02) VALUE SPACES.
           05 WS-CF-LIMIT            PIC S9(13)V99 VALUE ZERO.

       01  CALC-AREA.
           05 WS-BILL-AMT            PIC S9(13)V99 VALUE ZERO.
           05 WS-MIN-PAY-AMT         PIC S9(13)V99 VALUE ZERO.

           COPY LK-MINPAY-PARM.

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE 8 TO RETURN-CODE
           PERFORM 1000-INIT
           IF WS-HARD-ERROR = 'N'
               PERFORM 2000-PRECHECK
           END-IF
           IF WS-HARD-ERROR = 'N'
               PERFORM 3000-CYCLE-CLOSE
           END-IF
           PERFORM 9000-FINAL
           GOBACK.

       1000-INIT.
           ACCEPT WS-RUN-DATE FROM DATE YYYYMMDD
           MOVE WS-RUN-DATE TO WS-CYCLE-DT
           MOVE WS-RUN-DATE(7:2) TO WS-CYCLE-CD
           COMPUTE WS-DUE-DT = WS-CYCLE-DT + 25
           MOVE '開始前' TO WS-CHECKPOINT

           OPEN INPUT CDCARDF CDFEEF CDSALESF
           IF FS-CDCARDF NOT = '00'
               DISPLAY 'CDCARDF オープン失敗 ST=' FS-CDCARDF
               MOVE 'CDCARDF-OPEN' TO WS-CHECKPOINT
               MOVE 'Y' TO WS-HARD-ERROR
           END-IF
           IF FS-CDFEEF NOT = '00'
               DISPLAY 'CDFEEF オープン失敗 ST=' FS-CDFEEF
               MOVE 'CDFEEF-OPEN' TO WS-CHECKPOINT
               MOVE 'Y' TO WS-HARD-ERROR
           END-IF
           IF FS-CDSALESF NOT = '00'
               DISPLAY 'CDSALESF オープン失敗 ST=' FS-CDSALESF
               MOVE 'CDSALESF-OPEN' TO WS-CHECKPOINT
               MOVE 'Y' TO WS-HARD-ERROR
           END-IF

           OPEN I-O CDBALF
           IF FS-CDBALF NOT = '00'
               DISPLAY 'CDBALF オープン失敗 ST=' FS-CDBALF
               MOVE 'CDBALF-OPEN' TO WS-CHECKPOINT
               MOVE 'Y' TO WS-HARD-ERROR
           END-IF

           OPEN OUTPUT CDBILLF
           IF FS-CDBILLF NOT = '00'
               DISPLAY 'CDBILLF オープン失敗 ST=' FS-CDBILLF
               MOVE 'CDBILLF-OPEN' TO WS-CHECKPOINT
               MOVE 'Y' TO WS-HARD-ERROR
           END-IF

           OPEN OUTPUT CDSTMTIDXF
           IF FS-CDSTMTIDXF NOT = '00'
               DISPLAY 'CDSTMTIDXF オープン失敗 ST=' FS-CDSTMTIDXF
               MOVE 'CDSTMTIDXF-OPEN' TO WS-CHECKPOINT
               MOVE 'Y' TO WS-HARD-ERROR
           END-IF.

       2000-PRECHECK.
           MOVE '売上事前検査' TO WS-CHECKPOINT
           PERFORM UNTIL EOF-CDSALESF = 'Y'
               READ CDSALESF
                   AT END
                       MOVE 'Y' TO EOF-CDSALESF
                   NOT AT END
                       ADD 1 TO WS-SALES-IN-CNT
                       IF SL-CAPTURE-STATUS = 'C'
                           ADD SL-SALES-AMT TO WS-SALES-TOTAL
                           ADD SL-TAX-AMT TO WS-TAX-TOTAL
                       END-IF
                       IF SL-CARD-NO = SPACES
                           DISPLAY '売上カード番号未設定'
                           DISPLAY '売上ID=' SL-SALES-ID
                           MOVE 'SALES-EDIT' TO WS-CHECKPOINT
                           MOVE 'Y' TO WS-HARD-ERROR
                       END-IF
               END-READ
           END-PERFORM

           MOVE '年会費事前検査' TO WS-CHECKPOINT
           PERFORM UNTIL EOF-CDFEEF = 'Y'
               READ CDFEEF
                   AT END
                       MOVE 'Y' TO EOF-CDFEEF
                   NOT AT END
                       ADD 1 TO WS-FEE-IN-CNT
                       IF FE-POST-STATUS = '0'
                           ADD FE-FEE-AMT TO WS-FEE-TOTAL
                       END-IF
                       IF FE-BILL-CYCLE-CD NOT = WS-CYCLE-CD
                           DISPLAY '年会費サイクル対象外'
                           DISPLAY '年会費ID=' FE-FEE-ID
                       END-IF
                       IF FE-CARD-NO = SPACES
                           DISPLAY '年会費カード番号未設定'
                           DISPLAY '年会費ID=' FE-FEE-ID
                           MOVE 'FEE-EDIT' TO WS-CHECKPOINT
                           MOVE 'Y' TO WS-HARD-ERROR
                       END-IF
               END-READ
           END-PERFORM

           MOVE '残高サイクル検査' TO WS-CHECKPOINT
           PERFORM 2100-READ-CDBALF
           PERFORM UNTIL EOF-CDBALF = 'Y'
               ADD 1 TO WS-BAL-IN-CNT
               ADD BL-CLOSING-BAL-AMT TO WS-BAL-TOTAL
               IF BL-CYCLE-DT NOT = WS-CYCLE-DT
                   ADD 1 TO WS-CYCLE-MIS-CNT
                   DISPLAY '残高サイクル日不一致'
                   DISPLAY 'CARD=' BL-CARD-NO
                   DISPLAY '残高日=' BL-CYCLE-DT
                   DISPLAY '実行日=' WS-CYCLE-DT
               END-IF
               PERFORM 2100-READ-CDBALF
           END-PERFORM

           IF WS-CYCLE-MIS-CNT > ZERO
               DISPLAY 'サイクル不整合のため後続停止'
               DISPLAY '不整合件数=' WS-CYCLE-MIS-CNT
               MOVE 'CYCLE-CHECK' TO WS-CHECKPOINT
               MOVE 'Y' TO WS-HARD-ERROR
           END-IF

           CLOSE CDBALF
           IF FS-CDBALF NOT = '00'
               DISPLAY 'CDBALF クローズ失敗 ST=' FS-CDBALF
               MOVE 'CDBALF-CLOSE' TO WS-CHECKPOINT
               MOVE 'Y' TO WS-HARD-ERROR
           END-IF

           IF WS-HARD-ERROR = 'N'
               OPEN I-O CDBALF
               IF FS-CDBALF NOT = '00'
                   DISPLAY 'CDBALF 再オープン失敗 ST=' FS-CDBALF
                   MOVE 'CDBALF-REOPEN' TO WS-CHECKPOINT
                   MOVE 'Y' TO WS-HARD-ERROR
               END-IF
           END-IF
           MOVE 'N' TO EOF-CDBALF.

       2100-READ-CDBALF.
           READ CDBALF
               AT END
                   MOVE 'Y' TO EOF-CDBALF
               NOT AT END
                   CONTINUE
           END-READ
           IF FS-CDBALF NOT = '00' AND FS-CDBALF NOT = '10'
               DISPLAY 'CDBALF 読込失敗 ST=' FS-CDBALF
               MOVE 'CDBALF-READ' TO WS-CHECKPOINT
               MOVE 'Y' TO WS-HARD-ERROR
               MOVE 'Y' TO EOF-CDBALF
           END-IF.

       3000-CYCLE-CLOSE.
           MOVE '請求確定' TO WS-CHECKPOINT
           PERFORM 3100-READ-CDCARDF
           PERFORM 3200-READ-CDBALF
           PERFORM UNTIL EOF-CDCARDF = 'Y' OR WS-HARD-ERROR = 'Y'
               ADD 1 TO WS-CARD-IN-CNT
               MOVE CF-CARD-NO TO WS-CF-CARD-NO
               MOVE CF-CARD-STATUS TO WS-CF-STATUS
               MOVE CF-BILL-CYCLE-CD TO WS-CF-CYCLE-CD
               MOVE CF-CREDIT-LIMIT TO WS-CF-LIMIT
               PERFORM 3300-LOCATE-BALANCE
               PERFORM 3400-DECIDE-BILL
               PERFORM 3500-WRITE-BILL
               IF WS-SKIP-CARD = 'N' AND WS-HARD-ERROR = 'N'
                   PERFORM 3600-WRITE-INDEX
                   IF WS-BALANCE-FOUND = 'Y'
                       PERFORM 3700-UPDATE-BALANCE
                   END-IF
               END-IF
               PERFORM 3100-READ-CDCARDF
           END-PERFORM.

       3100-READ-CDCARDF.
           READ CDCARDF
               AT END
                   MOVE 'Y' TO EOF-CDCARDF
               NOT AT END
                   CONTINUE
           END-READ
           IF FS-CDCARDF NOT = '00' AND FS-CDCARDF NOT = '10'
               DISPLAY 'CDCARDF 読込失敗 ST=' FS-CDCARDF
               MOVE 'CDCARDF-READ' TO WS-CHECKPOINT
               MOVE 'Y' TO WS-HARD-ERROR
               MOVE 'Y' TO EOF-CDCARDF
           END-IF.

       3200-READ-CDBALF.
           READ CDBALF
               AT END
                   MOVE 'Y' TO EOF-CDBALF
               NOT AT END
                   CONTINUE
           END-READ
           IF FS-CDBALF NOT = '00' AND FS-CDBALF NOT = '10'
               DISPLAY 'CDBALF 読込失敗 ST=' FS-CDBALF
               MOVE 'CDBALF-READ2' TO WS-CHECKPOINT
               MOVE 'Y' TO WS-HARD-ERROR
               MOVE 'Y' TO EOF-CDBALF
           END-IF.

       3300-LOCATE-BALANCE.
           MOVE 'N' TO WS-BALANCE-FOUND
           PERFORM UNTIL EOF-CDBALF = 'Y'
               OR BL-CARD-NO >= WS-CF-CARD-NO
               PERFORM 3200-READ-CDBALF
           END-PERFORM

           IF EOF-CDBALF NOT = 'Y'
               IF BL-CARD-NO = WS-CF-CARD-NO
                   MOVE 'Y' TO WS-BALANCE-FOUND
               END-IF
           END-IF

           IF WS-BALANCE-FOUND = 'N'
               MOVE ZERO TO BL-CLOSING-BAL-AMT
               MOVE ZERO TO BL-REVOLVING-BAL-AMT
               MOVE ZERO TO BL-NEW-CHARGE-AMT
               MOVE ZERO TO BL-CASH-ADV-AMT
               MOVE WS-CF-CARD-NO TO BL-CARD-NO
               MOVE WS-CYCLE-DT TO BL-CYCLE-DT
           END-IF.

       3400-DECIDE-BILL.
           MOVE 'N' TO WS-SKIP-CARD
           MOVE 'N' TO WS-BILL-HOLD
           MOVE ZERO TO WS-BILL-AMT
           MOVE ZERO TO WS-MIN-PAY-AMT

           IF WS-CF-CYCLE-CD NOT = WS-CYCLE-CD
               MOVE 'Y' TO WS-SKIP-CARD
           END-IF

           IF WS-CF-STATUS = '02' OR WS-CF-STATUS = '03'
               MOVE 'Y' TO WS-SKIP-CARD
           END-IF

           IF WS-CF-STATUS = '09'
               ADD 1 TO WS-CARD-DELQ-CNT
           END-IF

           IF WS-CF-STATUS NOT = '01'
               AND WS-CF-STATUS NOT = '02'
               AND WS-CF-STATUS NOT = '03'
               AND WS-CF-STATUS NOT = '09'
               DISPLAY 'カード状態不正'
               DISPLAY 'CARD=' WS-CF-CARD-NO
               MOVE 'CARD-STATUS' TO WS-CHECKPOINT
               MOVE 'Y' TO WS-HARD-ERROR
           END-IF

           IF WS-SKIP-CARD = 'N' AND WS-HARD-ERROR = 'N'
               ADD 1 TO WS-CARD-ACT-CNT
               COMPUTE WS-BILL-AMT =
                   BL-CLOSING-BAL-AMT
                 + BL-REVOLVING-BAL-AMT
                 + BL-NEW-CHARGE-AMT
                 + BL-CASH-ADV-AMT
               IF WS-BILL-AMT < ZERO
                   MOVE ZERO TO WS-BILL-AMT
               END-IF
      *        最低支払額は CB910S（債権管理規程）に従い算定する
               INITIALIZE LK-MINPAY-PARM
               MOVE WS-BILL-AMT TO LK-MP-CLOSING-AMT
               MOVE WS-CF-STATUS TO LK-MP-CARD-STATUS
               CALL "CB910S" USING LK-MINPAY-PARM
               IF LK-MP-RET NOT = "00"
                   DISPLAY '最低支払額算定エラー'
                   DISPLAY 'CARD=' WS-CF-CARD-NO
                   DISPLAY 'RC=' LK-MP-RET
                   MOVE 'MINPAY-CALC' TO WS-CHECKPOINT
                   MOVE 'Y' TO WS-HARD-ERROR
               ELSE
                   MOVE LK-MP-MIN-PAY TO WS-MIN-PAY-AMT
               END-IF
               IF WS-BILL-AMT > WS-CF-LIMIT
                   MOVE 'Y' TO WS-BILL-HOLD
                   DISPLAY '利用限度超過保留'
                   DISPLAY 'CARD=' WS-CF-CARD-NO
               END-IF
           ELSE
               ADD 1 TO WS-CARD-SKP-CNT
           END-IF.

       3500-WRITE-BILL.
           IF WS-HARD-ERROR = 'N'
               MOVE WS-CF-CARD-NO TO BI-CARD-NO
               MOVE WS-CYCLE-DT TO BI-CYCLE-DT
               MOVE WS-BILL-AMT TO BI-BILL-AMT
               MOVE WS-MIN-PAY-AMT TO BI-MIN-PAY-AMT
               MOVE WS-DUE-DT TO BI-DUE-DT
               MOVE WS-PROGRAM-ID TO BI-PROGRAM-ID
               IF WS-SKIP-CARD = 'Y'
                   MOVE 'S' TO BI-BILL-STATUS
               ELSE
                   IF WS-BILL-HOLD = 'Y'
                       MOVE 'H' TO BI-BILL-STATUS
                   ELSE
                       MOVE 'C' TO BI-BILL-STATUS
                   END-IF
               END-IF
               WRITE CDBILLF-REC
               IF FS-CDBILLF = '00'
                   ADD 1 TO WS-BILL-OUT-CNT
                   ADD BI-BILL-AMT TO WS-BILL-TOTAL
               ELSE
                   DISPLAY 'CDBILLF 書込失敗 ST=' FS-CDBILLF
                   MOVE 'CDBILLF-WRITE' TO WS-CHECKPOINT
                   MOVE 'Y' TO WS-HARD-ERROR
               END-IF
           END-IF.

       3600-WRITE-INDEX.
           ADD 1 TO WS-STATEMENT-SEQ
           MOVE WS-CF-CARD-NO TO SI-CARD-NO
           MOVE WS-CYCLE-DT TO SI-CYCLE-DT
           MOVE WS-BILL-AMT TO SI-BILL-AMT
           MOVE WS-DUE-DT TO SI-DUE-DT
           MOVE '0' TO SI-PUBLISH-STATUS
           MOVE SPACES TO SI-STATEMENT-ID
           STRING WS-CYCLE-DT DELIMITED BY SIZE
                  WS-STATEMENT-SEQ DELIMITED BY SIZE
               INTO SI-STATEMENT-ID
           END-STRING

           WRITE CDSTMTIDXF-REC
           IF FS-CDSTMTIDXF = '00'
               ADD 1 TO WS-IDX-OUT-CNT
           ELSE
               IF FS-CDSTMTIDXF = '22'
                   DISPLAY '明細索引重複'
                   DISPLAY 'CARD=' SI-CARD-NO
                   MOVE 'IDX-DUP' TO WS-CHECKPOINT
                   MOVE 'Y' TO WS-HARD-ERROR
               ELSE
                   DISPLAY 'CDSTMTIDXF 書込失敗 ST=' FS-CDSTMTIDXF
                   MOVE 'IDX-WRITE' TO WS-CHECKPOINT
                   MOVE 'Y' TO WS-HARD-ERROR
               END-IF
           END-IF.

       3700-UPDATE-BALANCE.
           MOVE WS-CYCLE-DT TO BL-CYCLE-DT
           MOVE WS-BILL-AMT TO BL-CLOSING-BAL-AMT
           MOVE ZERO TO BL-NEW-CHARGE-AMT
           REWRITE CDBALF-REC
           IF FS-CDBALF NOT = '00'
               DISPLAY 'CDBALF 更新失敗 ST=' FS-CDBALF
               MOVE 'CDBALF-REWRITE' TO WS-CHECKPOINT
               MOVE 'Y' TO WS-HARD-ERROR
           END-IF.

       9000-FINAL.
           DISPLAY 'CB320B チェックポイント=' WS-CHECKPOINT
           DISPLAY 'カード入力件数=' WS-CARD-IN-CNT
           DISPLAY '請求対象件数=' WS-CARD-ACT-CNT
           DISPLAY '対象外件数=' WS-CARD-SKP-CNT
           DISPLAY '延滞抽出件数=' WS-CARD-DELQ-CNT
           DISPLAY '売上入力件数=' WS-SALES-IN-CNT
           DISPLAY '年会費入力件数=' WS-FEE-IN-CNT
           DISPLAY '残高入力件数=' WS-BAL-IN-CNT
           DISPLAY '請求出力件数=' WS-BILL-OUT-CNT
           DISPLAY '索引出力件数=' WS-IDX-OUT-CNT
           DISPLAY '売上合計=' WS-SALES-TOTAL
           DISPLAY '税額合計=' WS-TAX-TOTAL
           DISPLAY '年会費合計=' WS-FEE-TOTAL
           DISPLAY '残高合計=' WS-BAL-TOTAL
           DISPLAY '請求合計=' WS-BILL-TOTAL

           CLOSE CDCARDF CDBALF CDFEEF CDSALESF CDBILLF CDSTMTIDXF

           IF WS-HARD-ERROR = 'Y'
               DISPLAY 'CB320B 異常終了 RC=8'
               MOVE 8 TO RETURN-CODE
           ELSE
               DISPLAY 'CB320B 正常終了 RC=0'
               MOVE 0 TO RETURN-CODE
           END-IF.
