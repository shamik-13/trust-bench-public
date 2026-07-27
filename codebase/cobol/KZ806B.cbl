       IDENTIFICATION DIVISION.
       PROGRAM-ID. KZ806B.
      * 変更履歴
      * 版数  年月日(和暦)   担当                       概要
      * 1.00  令和03年12月01日 システム部 勘定系チーム  新規作成
      * 1.01  令和04年12月15日 システム部 勘定系チーム  税制改正対応
      * 1.02  令和05年11月20日 システム部 勘定系チーム  印字項目追加
       AUTHOR. KZ-BATCH.
      ******************************************************************
      * 源泉徴収票印字データ作成バッチ
      * 口座別年間集計と支払調書情報を突合し、印字用明細を作成する。
      * 税額は KZTXRF または KZYAGF の集計済み値のみを使用する。
      ******************************************************************

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZTXRF
               ASSIGN TO "KZTXRF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-ST-KZTXRF.
           SELECT KZPRIF
               ASSIGN TO "KZPRIF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-ST-KZPRIF.
           SELECT KZYAGF
               ASSIGN TO "KZYAGF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-ST-KZYAGF.

       DATA DIVISION.
       FILE SECTION.

       FD  KZTXRF.
       COPY KZTXRFC.

       FD  KZPRIF.
       COPY KZPRIFC.

       FD  KZYAGF.
       COPY KZYAGFC.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05  WS-ST-KZTXRF              PIC X(02) VALUE SPACE.
           05  WS-ST-KZPRIF              PIC X(02) VALUE SPACE.
           05  WS-ST-KZYAGF              PIC X(02) VALUE SPACE.

       01  WS-EOF-FLAGS.
           05  WS-EOF-KZTXRF             PIC X VALUE "N".
               88  KZTXRF-EOF                 VALUE "Y".
           05  WS-EOF-KZPRIF             PIC X VALUE "N".
               88  KZPRIF-EOF                 VALUE "Y".
           05  WS-EOF-KZYAGF             PIC X VALUE "N".
               88  KZYAGF-EOF                 VALUE "Y".

       01  WS-WORK.
           05  WS-CURR-DATE              PIC 9(08) VALUE ZERO.
           05  WS-CURR-DATE-X.
               10  WS-CD-YYYY            PIC 9(04).
               10  WS-CD-MM              PIC 9(02).
               10  WS-CD-DD              PIC 9(02).
           05  WS-CURRENT-DATE-FUNC      PIC X(21) VALUE SPACE.
           05  WS-USE-YA-DATA            PIC X VALUE "N".
               88  USE-YA-DATA                 VALUE "Y".
           05  WS-MATCHED-COUNT          PIC 9(09) VALUE ZERO.
           05  WS-SKIP-COUNT             PIC 9(09) VALUE ZERO.
           05  WS-UPDATE-COUNT           PIC 9(09) VALUE ZERO.
           05  WS-ERROR-COUNT            PIC 9(09) VALUE ZERO.
           05  WS-ERROR-COUNT-SAVE       PIC 9(09) VALUE ZERO.

       01  WS-AMOUNT-WORK.
           05  WS-GROSS-AMT              PIC S9(13) VALUE ZERO.
           05  WS-NATIONAL-TAX           PIC S9(13) VALUE ZERO.
           05  WS-LOCAL-TAX              PIC S9(13) VALUE ZERO.
           05  WS-NET-AMT                PIC S9(13) VALUE ZERO.
           05  WS-TOTAL-TAX              PIC S9(13) VALUE ZERO.

       01  WS-DISPLAY-WORK.
           05  WS-DSP-ACCT               PIC X(20) VALUE SPACE.
           05  WS-DSP-COUNT              PIC ZZZ,ZZZ,ZZ9.

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE 0 TO RETURN-CODE
           PERFORM 1000-INITIALIZE
           PERFORM 2000-MAIN-PROCESS
           PERFORM 9000-FINALIZE
           GOBACK.

       1000-INITIALIZE.
           DISPLAY "KZ806B 開始"
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-FUNC
           MOVE WS-CURRENT-DATE-FUNC(1:8) TO WS-CURR-DATE-X
           MOVE WS-CURR-DATE-X TO WS-CURR-DATE

           OPEN INPUT KZTXRF
           IF WS-ST-KZTXRF NOT = "00"
              DISPLAY "KZTXRF オープン失敗 ST=" WS-ST-KZTXRF
              PERFORM 9900-ABEND
           END-IF

           OPEN I-O KZPRIF
           IF WS-ST-KZPRIF NOT = "00"
              DISPLAY "KZPRIF オープン失敗 ST=" WS-ST-KZPRIF
              PERFORM 9900-ABEND
           END-IF

           OPEN INPUT KZYAGF
           IF WS-ST-KZYAGF NOT = "00"
              DISPLAY "KZYAGF オープン失敗 ST=" WS-ST-KZYAGF
              PERFORM 9900-ABEND
           END-IF

           PERFORM 1100-READ-KZTXRF
           PERFORM 1200-READ-KZPRIF
           PERFORM 1300-READ-KZYAGF.

       1100-READ-KZTXRF.
           READ KZTXRF
               AT END
                   SET KZTXRF-EOF TO TRUE
               NOT AT END
                   IF WS-ST-KZTXRF NOT = "00"
                      DISPLAY "KZTXRF 読込失敗 ST=" WS-ST-KZTXRF
                      PERFORM 9900-ABEND
                   END-IF
           END-READ.

       1200-READ-KZPRIF.
           READ KZPRIF
               AT END
                   SET KZPRIF-EOF TO TRUE
               NOT AT END
                   IF WS-ST-KZPRIF NOT = "00"
                      DISPLAY "KZPRIF 読込失敗 ST=" WS-ST-KZPRIF
                      PERFORM 9900-ABEND
                   END-IF
           END-READ.

       1300-READ-KZYAGF.
           READ KZYAGF
               AT END
                   SET KZYAGF-EOF TO TRUE
               NOT AT END
                   IF WS-ST-KZYAGF NOT = "00"
                      DISPLAY "KZYAGF 読込失敗 ST=" WS-ST-KZYAGF
                      PERFORM 9900-ABEND
                   END-IF
           END-READ.

       2000-MAIN-PROCESS.
           PERFORM UNTIL KZTXRF-EOF OR KZPRIF-EOF
              EVALUATE TRUE
                 WHEN TR-ACCT-NO = PR-ACCT-NO
                    PERFORM 3000-PROCESS-MATCH
                    PERFORM 1100-READ-KZTXRF
                    PERFORM 1200-READ-KZPRIF
                 WHEN TR-ACCT-NO < PR-ACCT-NO
                    ADD 1 TO WS-SKIP-COUNT
                    MOVE TR-ACCT-NO TO WS-DSP-ACCT
                    DISPLAY "支払調書未登録 口座="
                    DISPLAY WS-DSP-ACCT
                    PERFORM 1100-READ-KZTXRF
                 WHEN OTHER
                    ADD 1 TO WS-SKIP-COUNT
                    MOVE PR-ACCT-NO TO WS-DSP-ACCT
                    DISPLAY "税額集計未登録 口座="
                    DISPLAY WS-DSP-ACCT
                    PERFORM 1200-READ-KZPRIF
              END-EVALUATE
           END-PERFORM

           PERFORM UNTIL KZTXRF-EOF
              ADD 1 TO WS-SKIP-COUNT
              MOVE TR-ACCT-NO TO WS-DSP-ACCT
              DISPLAY "支払調書未登録 口座="
              DISPLAY WS-DSP-ACCT
              PERFORM 1100-READ-KZTXRF
           END-PERFORM

           PERFORM UNTIL KZPRIF-EOF
              ADD 1 TO WS-SKIP-COUNT
              MOVE PR-ACCT-NO TO WS-DSP-ACCT
              DISPLAY "税額集計未登録 口座="
              DISPLAY WS-DSP-ACCT
              PERFORM 1200-READ-KZPRIF
           END-PERFORM.

       3000-PROCESS-MATCH.
           ADD 1 TO WS-MATCHED-COUNT
           MOVE "N" TO WS-USE-YA-DATA
           MOVE WS-ERROR-COUNT TO WS-ERROR-COUNT-SAVE

           PERFORM 3100-VALIDATE-TAX-REC
           PERFORM 3200-SYNC-YEARLY-REC

           IF USE-YA-DATA
              MOVE YA-TOTAL-GROSS-INT-AMT TO WS-GROSS-AMT
              MOVE YA-TOTAL-NATIONAL-TAX-AMT TO WS-NATIONAL-TAX
              MOVE YA-TOTAL-LOCAL-TAX-AMT TO WS-LOCAL-TAX
              MOVE YA-TOTAL-NET-AMT TO WS-NET-AMT
           ELSE
              MOVE TR-GROSS-INT-AMT TO WS-GROSS-AMT
              MOVE TR-NATIONAL-TAX-AMT TO WS-NATIONAL-TAX
              MOVE TR-LOCAL-TAX-AMT TO WS-LOCAL-TAX
              MOVE TR-NET-AMT TO WS-NET-AMT
           END-IF

           COMPUTE WS-TOTAL-TAX = WS-NATIONAL-TAX + WS-LOCAL-TAX

           IF WS-GROSS-AMT < ZERO
              MOVE TR-ACCT-NO TO WS-DSP-ACCT
              DISPLAY "総支払額不正 口座="
              DISPLAY WS-DSP-ACCT
              PERFORM 9800-RECORD-ERROR
           END-IF

           IF WS-NATIONAL-TAX < ZERO OR WS-LOCAL-TAX < ZERO
              MOVE TR-ACCT-NO TO WS-DSP-ACCT
              DISPLAY "税額不正 口座="
              DISPLAY WS-DSP-ACCT
              PERFORM 9800-RECORD-ERROR
           END-IF

           IF WS-NET-AMT < ZERO
              MOVE TR-ACCT-NO TO WS-DSP-ACCT
              DISPLAY "手取額不正 口座="
              DISPLAY WS-DSP-ACCT
              PERFORM 9800-RECORD-ERROR
           END-IF

           IF WS-GROSS-AMT < WS-TOTAL-TAX
              MOVE TR-ACCT-NO TO WS-DSP-ACCT
              DISPLAY "支払額税額関係不正 口座="
              DISPLAY WS-DSP-ACCT
              PERFORM 9800-RECORD-ERROR
           END-IF

           IF WS-ERROR-COUNT = WS-ERROR-COUNT-SAVE
              PERFORM 3300-UPDATE-PRINT-REC
           END-IF.

       3100-VALIDATE-TAX-REC.
           EVALUATE TR-ACCT-TYPE
              WHEN "01"
              WHEN "02"
              WHEN "03"
                 CONTINUE
              WHEN OTHER
                 MOVE TR-ACCT-NO TO WS-DSP-ACCT
                 DISPLAY "口座区分不正 口座="
                 DISPLAY WS-DSP-ACCT
                 PERFORM 9800-RECORD-ERROR
           END-EVALUATE

           IF TR-TOTAL-TAX-AMT NOT =
              TR-NATIONAL-TAX-AMT + TR-LOCAL-TAX-AMT
              MOVE TR-ACCT-NO TO WS-DSP-ACCT
              DISPLAY "税額合計不一致 口座="
              DISPLAY WS-DSP-ACCT
              PERFORM 9800-RECORD-ERROR
           END-IF

           IF TR-GROSS-INT-AMT < TR-TOTAL-TAX-AMT
              MOVE TR-ACCT-NO TO WS-DSP-ACCT
              DISPLAY "税引前額不正 口座="
              DISPLAY WS-DSP-ACCT
              PERFORM 9800-RECORD-ERROR
           END-IF.

       3200-SYNC-YEARLY-REC.
           PERFORM UNTIL KZYAGF-EOF OR YA-ACCT-NO >= TR-ACCT-NO
              PERFORM 1300-READ-KZYAGF
           END-PERFORM

           IF NOT KZYAGF-EOF AND YA-ACCT-NO = TR-ACCT-NO
              IF YA-YEAR = WS-CD-YYYY
                 IF YA-PAYMENT-COUNT > ZERO
                    MOVE "Y" TO WS-USE-YA-DATA
                 ELSE
                    MOVE TR-ACCT-NO TO WS-DSP-ACCT
                    DISPLAY "年間支払回数不正 口座="
                    DISPLAY WS-DSP-ACCT
                    PERFORM 9800-RECORD-ERROR
                 END-IF
              END-IF
           END-IF.

       3300-UPDATE-PRINT-REC.
           IF PR-PAYEE-NAME-KANA = SPACE
              MOVE PR-ACCT-NO TO WS-DSP-ACCT
              DISPLAY "受取人カナ未設定 口座="
              DISPLAY WS-DSP-ACCT
              PERFORM 9800-RECORD-ERROR
           END-IF

           IF PR-PAYEE-ADDR-CD = SPACE
              MOVE PR-ACCT-NO TO WS-DSP-ACCT
              DISPLAY "住所コード未設定 口座="
              DISPLAY WS-DSP-ACCT
              PERFORM 9800-RECORD-ERROR
           END-IF

           IF WS-ERROR-COUNT NOT = WS-ERROR-COUNT-SAVE
              EXIT PARAGRAPH
           END-IF

           MOVE WS-CURR-DATE TO PR-PAYMENT-DATE
           MOVE WS-GROSS-AMT TO PR-GROSS-INT-AMT
           MOVE WS-NATIONAL-TAX TO PR-NATIONAL-TAX-AMT
           MOVE WS-LOCAL-TAX TO PR-LOCAL-TAX-AMT

           EVALUATE TR-ACCT-TYPE
              WHEN "01"
                 MOVE "1" TO PR-STATEMENT-CLASS
              WHEN "02"
                 MOVE "2" TO PR-STATEMENT-CLASS
              WHEN "03"
                 MOVE "3" TO PR-STATEMENT-CLASS
           END-EVALUATE

           REWRITE KZPRIF-REC
           IF WS-ST-KZPRIF NOT = "00"
              DISPLAY "KZPRIF 更新失敗 ST=" WS-ST-KZPRIF
              PERFORM 9900-ABEND
           END-IF

           ADD 1 TO WS-UPDATE-COUNT.

       9000-FINALIZE.
           CLOSE KZTXRF
           IF WS-ST-KZTXRF NOT = "00"
              DISPLAY "KZTXRF クローズ失敗 ST=" WS-ST-KZTXRF
              MOVE 8 TO RETURN-CODE
           END-IF

           CLOSE KZPRIF
           IF WS-ST-KZPRIF NOT = "00"
              DISPLAY "KZPRIF クローズ失敗 ST=" WS-ST-KZPRIF
              MOVE 8 TO RETURN-CODE
           END-IF

           CLOSE KZYAGF
           IF WS-ST-KZYAGF NOT = "00"
              DISPLAY "KZYAGF クローズ失敗 ST=" WS-ST-KZYAGF
              MOVE 8 TO RETURN-CODE
           END-IF

           MOVE WS-MATCHED-COUNT TO WS-DSP-COUNT
           DISPLAY "突合件数=" WS-DSP-COUNT
           MOVE WS-UPDATE-COUNT TO WS-DSP-COUNT
           DISPLAY "更新件数=" WS-DSP-COUNT
           MOVE WS-SKIP-COUNT TO WS-DSP-COUNT
           DISPLAY "除外件数=" WS-DSP-COUNT
           MOVE WS-ERROR-COUNT TO WS-DSP-COUNT
           DISPLAY "エラー件数=" WS-DSP-COUNT

           IF WS-ERROR-COUNT > ZERO
              MOVE 8 TO RETURN-CODE
           END-IF

           IF RETURN-CODE = ZERO
              DISPLAY "KZ806B 正常終了"
           ELSE
              DISPLAY "KZ806B 警告終了 RC=" RETURN-CODE
           END-IF.

       9800-RECORD-ERROR.
           ADD 1 TO WS-ERROR-COUNT.

       9900-ABEND.
           MOVE 12 TO RETURN-CODE
           DISPLAY "KZ806B 異常終了 RC=" RETURN-CODE
           GOBACK.
