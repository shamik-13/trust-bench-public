       IDENTIFICATION DIVISION.
       PROGRAM-ID. KZ108B.
      *================================================================*
      * 変更履歴
      * 版数  年月日    担当   概要
      * 0.1   20240401  KZ01   初版作成
      * 0.2   20240510  KZ02   顧客ＫＹＣ判定追加
      * 0.3   20240618  KZ03   参考エクスポージャ出力追加
      *================================================================*
      * 担保評価更新バッチ
      * 担保の評価日、種別、評価額、掛目を検査し、有効担保のみ
      * 時価換算して担保ファイルを更新する。
      * 口座・顧客照合後、担保控除後の参考エクスポージャを出力する。
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZCOLLF ASSIGN TO "KZCOLLF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CL-COLLATERAL-ID
               FILE STATUS IS FS-KZCOLLF.
           SELECT KZACCTF ASSIGN TO "KZACCTF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS AC-ACCT-NO
               FILE STATUS IS FS-KZACCTF.
           SELECT KZCUSTF ASSIGN TO "KZCUSTF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CU-CUST-ID
               FILE STATUS IS FS-KZCUSTF.
           SELECT KZEXPF ASSIGN TO "KZEXPF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-KZEXPF.

       DATA DIVISION.
       FILE SECTION.
       FD  KZCOLLF.
           COPY KZCOLLFC.
       FD  KZACCTF.
           COPY KZACCTC4.
       FD  KZCUSTF.
           COPY KZCUSTC.
       FD  KZEXPF.
           COPY KZEXPFC.

       WORKING-STORAGE SECTION.
       01  FS-AREA.
           05 FS-KZCOLLF              PIC XX VALUE SPACES.
           05 FS-KZACCTF              PIC XX VALUE SPACES.
           05 FS-KZCUSTF              PIC XX VALUE SPACES.
           05 FS-KZEXPF               PIC XX VALUE SPACES.

       01  SW-AREA.
           05 SW-END                  PIC X VALUE "N".
              88 END-OF-KZCOLLF             VALUE "Y".
           05 SW-HARD-ERROR           PIC X VALUE "N".
              88 HARD-ERROR                 VALUE "Y".
           05 SW-VALID-COLL           PIC X VALUE "N".
              88 VALID-COLL                 VALUE "Y".

       01  DATE-AREA.
           05 WS-TODAY                PIC 9(08) VALUE ZERO.
           05 WS-VAL-DATE             PIC 9(08) VALUE ZERO.
           05 WS-VAL-YYYY             PIC 9(04) VALUE ZERO.
           05 WS-VAL-MM               PIC 9(02) VALUE ZERO.
           05 WS-VAL-DD               PIC 9(02) VALUE ZERO.
           05 WS-DAYS-IN-MM           PIC 9(02) VALUE ZERO.
           05 WS-LEAP-REM4            PIC 9(04) VALUE ZERO.
           05 WS-LEAP-REM100          PIC 9(04) VALUE ZERO.
           05 WS-LEAP-REM400          PIC 9(04) VALUE ZERO.

       01  CALC-AREA.
           05 WS-MARKET-AMT           PIC S9(15)V99 VALUE ZERO.
           05 WS-BASE-EXPOSURE        PIC S9(15)V99 VALUE ZERO.
           05 WS-EXPOSURE-AMT         PIC S9(15)V99 VALUE ZERO.
           05 WS-COLL-CEILING          PIC S9(15)V99 VALUE ZERO.
           05 WS-PRODUCT-TYPE         PIC X(02) VALUE SPACES.
           05 WS-REASON               PIC X(60) VALUE SPACES.

       01  COUNT-AREA.
           05 CNT-READ                PIC 9(09) VALUE ZERO.
           05 CNT-UPDATE              PIC 9(09) VALUE ZERO.
           05 CNT-EXP-WRITE           PIC 9(09) VALUE ZERO.
           05 CNT-SKIP                PIC 9(09) VALUE ZERO.
           05 CNT-ERROR               PIC 9(09) VALUE ZERO.

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE 0 TO RETURN-CODE
           ACCEPT WS-TODAY FROM DATE YYYYMMDD
           PERFORM 1000-OPEN-FILES
           IF HARD-ERROR
               PERFORM 9000-CLOSE-FILES
               MOVE 12 TO RETURN-CODE
               GOBACK
           END-IF

           PERFORM 2000-READ-FIRST
           PERFORM UNTIL END-OF-KZCOLLF OR HARD-ERROR
               ADD 1 TO CNT-READ
               PERFORM 3000-PROCESS-COLL
               PERFORM 2100-READ-NEXT
           END-PERFORM

           PERFORM 9000-CLOSE-FILES
           IF HARD-ERROR
               MOVE 12 TO RETURN-CODE
           ELSE
               DISPLAY "KZ170B 正常終了 読込=" CNT-READ
                       " 更新=" CNT-UPDATE
                       " 出力=" CNT-EXP-WRITE
                       " 除外=" CNT-SKIP
                       " 警告=" CNT-ERROR
               MOVE 0 TO RETURN-CODE
           END-IF
           GOBACK.

       1000-OPEN-FILES.
           OPEN I-O KZCOLLF
           IF FS-KZCOLLF NOT = "00"
               DISPLAY "KZCOLLF オープン失敗 ST=" FS-KZCOLLF
               SET HARD-ERROR TO TRUE
               EXIT PARAGRAPH
           END-IF

           OPEN INPUT KZACCTF
           IF FS-KZACCTF NOT = "00"
               DISPLAY "KZACCTF オープン失敗 ST=" FS-KZACCTF
               SET HARD-ERROR TO TRUE
               EXIT PARAGRAPH
           END-IF

           OPEN INPUT KZCUSTF
           IF FS-KZCUSTF NOT = "00"
               DISPLAY "KZCUSTF オープン失敗 ST=" FS-KZCUSTF
               SET HARD-ERROR TO TRUE
               EXIT PARAGRAPH
           END-IF

           OPEN OUTPUT KZEXPF
           IF FS-KZEXPF NOT = "00"
               DISPLAY "KZEXPF オープン失敗 ST=" FS-KZEXPF
               SET HARD-ERROR TO TRUE
           END-IF.

       2000-READ-FIRST.
           READ KZCOLLF NEXT RECORD
               AT END
                   SET END-OF-KZCOLLF TO TRUE
               NOT AT END
                   CONTINUE
           END-READ
           IF FS-KZCOLLF NOT = "00" AND FS-KZCOLLF NOT = "10"
               DISPLAY "KZCOLLF 初回読込失敗 ST=" FS-KZCOLLF
               SET HARD-ERROR TO TRUE
           END-IF.

       2100-READ-NEXT.
           READ KZCOLLF NEXT RECORD
               AT END
                   SET END-OF-KZCOLLF TO TRUE
               NOT AT END
                   CONTINUE
           END-READ
           IF FS-KZCOLLF NOT = "00" AND FS-KZCOLLF NOT = "10"
               DISPLAY "KZCOLLF 読込失敗 ST=" FS-KZCOLLF
               SET HARD-ERROR TO TRUE
           END-IF.

       3000-PROCESS-COLL.
           MOVE "N" TO SW-VALID-COLL
           MOVE SPACES TO WS-REASON
           PERFORM 3100-VALIDATE-COLL
           IF NOT VALID-COLL
               ADD 1 TO CNT-SKIP
               DISPLAY "担保除外 ID=" CL-COLLATERAL-ID
                       " 理由=" WS-REASON
               EXIT PARAGRAPH
           END-IF

           PERFORM 3200-READ-ACCOUNT
           IF NOT VALID-COLL
               ADD 1 TO CNT-SKIP
               DISPLAY "担保除外 ID=" CL-COLLATERAL-ID
                       " 理由=" WS-REASON
               EXIT PARAGRAPH
           END-IF

           PERFORM 3300-READ-CUSTOMER
           IF NOT VALID-COLL
               ADD 1 TO CNT-SKIP
               DISPLAY "担保除外 ID=" CL-COLLATERAL-ID
                       " 理由=" WS-REASON
               EXIT PARAGRAPH
           END-IF

           PERFORM 3400-CALCULATE-AMOUNTS
           PERFORM 3500-UPDATE-COLL
           IF NOT HARD-ERROR
               PERFORM 3600-WRITE-EXPOSURE
           END-IF.

       3100-VALIDATE-COLL.
           IF CL-COLLATERAL-ID = SPACES
               MOVE "担保番号未設定" TO WS-REASON
               EXIT PARAGRAPH
           END-IF
           IF CL-CUST-ID = SPACES
               MOVE "顧客番号未設定" TO WS-REASON
               EXIT PARAGRAPH
           END-IF
           IF CL-ACCT-NO = SPACES
               MOVE "口座番号未設定" TO WS-REASON
               EXIT PARAGRAPH
           END-IF
           EVALUATE CL-COLLATERAL-TYPE
               WHEN "01"
               WHEN "02"
               WHEN "03"
               WHEN "04"
                   CONTINUE
               WHEN OTHER
                   MOVE "担保種別不正" TO WS-REASON
                   EXIT PARAGRAPH
           END-EVALUATE
           IF CL-APPRAISAL-AMT <= ZERO
               MOVE "評価額不正" TO WS-REASON
               EXIT PARAGRAPH
           END-IF
           IF CL-HAIRCUT-RATE <= ZERO OR CL-HAIRCUT-RATE > 100
               MOVE "掛目不正" TO WS-REASON
               EXIT PARAGRAPH
           END-IF
           MOVE CL-VALUATION-DATE TO WS-VAL-DATE
           IF WS-VAL-DATE NOT NUMERIC
               MOVE "評価日数字不正" TO WS-REASON
               EXIT PARAGRAPH
           END-IF
           PERFORM 3150-CHECK-DATE
           IF WS-REASON NOT = SPACES
               EXIT PARAGRAPH
           END-IF
           IF WS-VAL-DATE > WS-TODAY
               MOVE "評価日未来日" TO WS-REASON
               EXIT PARAGRAPH
           END-IF
           SET VALID-COLL TO TRUE.

       3150-CHECK-DATE.
           MOVE ZERO TO WS-DAYS-IN-MM
           MOVE WS-VAL-DATE(1:4) TO WS-VAL-YYYY
           MOVE WS-VAL-DATE(5:2) TO WS-VAL-MM
           MOVE WS-VAL-DATE(7:2) TO WS-VAL-DD

           IF WS-VAL-YYYY < 2000
               MOVE "評価日年不正" TO WS-REASON
               EXIT PARAGRAPH
           END-IF
           IF WS-VAL-MM < 1 OR WS-VAL-MM > 12
               MOVE "評価日月不正" TO WS-REASON
               EXIT PARAGRAPH
           END-IF

           EVALUATE WS-VAL-MM
               WHEN 1
               WHEN 3
               WHEN 5
               WHEN 7
               WHEN 8
               WHEN 10
               WHEN 12
                   MOVE 31 TO WS-DAYS-IN-MM
               WHEN 4
               WHEN 6
               WHEN 9
               WHEN 11
                   MOVE 30 TO WS-DAYS-IN-MM
               WHEN 2
                   DIVIDE WS-VAL-YYYY BY 4
                       GIVING WS-LEAP-REM4 REMAINDER WS-LEAP-REM4
                   DIVIDE WS-VAL-YYYY BY 100
                       GIVING WS-LEAP-REM100 REMAINDER WS-LEAP-REM100
                   DIVIDE WS-VAL-YYYY BY 400
                       GIVING WS-LEAP-REM400 REMAINDER WS-LEAP-REM400
                   IF WS-LEAP-REM400 = 0
                      OR (WS-LEAP-REM4 = 0 AND WS-LEAP-REM100 NOT = 0)
                       MOVE 29 TO WS-DAYS-IN-MM
                   ELSE
                       MOVE 28 TO WS-DAYS-IN-MM
                   END-IF
           END-EVALUATE

           IF WS-VAL-DD < 1 OR WS-VAL-DD > WS-DAYS-IN-MM
               MOVE "評価日日不正" TO WS-REASON
           END-IF.

       3200-READ-ACCOUNT.
           MOVE CL-ACCT-NO TO AC-ACCT-NO
           READ KZACCTF KEY IS AC-ACCT-NO
           EVALUATE FS-KZACCTF
               WHEN "00"
                   IF AC-CUST-ID NOT = CL-CUST-ID
                       MOVE "口座顧客不一致" TO WS-REASON
                       MOVE "N" TO SW-VALID-COLL
                   END-IF
               WHEN "23"
                   MOVE "口座未登録" TO WS-REASON
                   MOVE "N" TO SW-VALID-COLL
               WHEN OTHER
                   DISPLAY "KZACCTF 読込失敗 ST=" FS-KZACCTF
                   SET HARD-ERROR TO TRUE
                   MOVE "N" TO SW-VALID-COLL
           END-EVALUATE.

       3300-READ-CUSTOMER.
           MOVE CL-CUST-ID TO CU-CUST-ID
           READ KZCUSTF KEY IS CU-CUST-ID
           EVALUATE FS-KZCUSTF
               WHEN "00"
                   EVALUATE CU-KYC-STATUS
                       WHEN "1"
                       WHEN "0"
                       WHEN "9"
                           CONTINUE
                       WHEN OTHER
                           MOVE "ＫＹＣ状態不正" TO WS-REASON
                           MOVE "N" TO SW-VALID-COLL
                   END-EVALUATE
               WHEN "23"
                   MOVE "顧客未登録" TO WS-REASON
                   MOVE "N" TO SW-VALID-COLL
               WHEN OTHER
                   DISPLAY "KZCUSTF 読込失敗 ST=" FS-KZCUSTF
                   SET HARD-ERROR TO TRUE
                   MOVE "N" TO SW-VALID-COLL
           END-EVALUATE.

       3400-CALCULATE-AMOUNTS.
           COMPUTE WS-MARKET-AMT ROUNDED =
               CL-APPRAISAL-AMT * CL-HAIRCUT-RATE / 100

           EVALUATE AC-ACCT-TYPE
               WHEN "1"
                   MOVE "01" TO WS-PRODUCT-TYPE
                   MOVE 8000000 TO WS-COLL-CEILING
                   MOVE AC-CREDIT-LIMIT TO WS-BASE-EXPOSURE
               WHEN "2"
                   MOVE "02" TO WS-PRODUCT-TYPE
                   MOVE 25000000 TO WS-COLL-CEILING
                   MOVE AC-CREDIT-LIMIT TO WS-BASE-EXPOSURE
               WHEN "3"
                   MOVE "03" TO WS-PRODUCT-TYPE
                   MOVE 80000000 TO WS-COLL-CEILING
                   MOVE AC-CUR-BAL TO WS-BASE-EXPOSURE
               WHEN OTHER
                   MOVE "03" TO WS-PRODUCT-TYPE
                   MOVE 80000000 TO WS-COLL-CEILING
                   MOVE AC-CUR-BAL TO WS-BASE-EXPOSURE
           END-EVALUATE

           IF WS-BASE-EXPOSURE < ZERO
               COMPUTE WS-BASE-EXPOSURE = WS-BASE-EXPOSURE * -1
           END-IF
           IF WS-BASE-EXPOSURE > WS-COLL-CEILING
               MOVE WS-COLL-CEILING TO WS-BASE-EXPOSURE
           END-IF
           COMPUTE WS-EXPOSURE-AMT =
               WS-BASE-EXPOSURE - WS-MARKET-AMT
           IF WS-EXPOSURE-AMT < ZERO
               MOVE ZERO TO WS-EXPOSURE-AMT
           END-IF.

       3500-UPDATE-COLL.
           MOVE WS-MARKET-AMT TO CL-APPRAISAL-AMT
           REWRITE KZCOLLF-REC
           IF FS-KZCOLLF = "00"
               ADD 1 TO CNT-UPDATE
           ELSE
               DISPLAY "KZCOLLF 更新失敗 ID=" CL-COLLATERAL-ID
                       " ST=" FS-KZCOLLF
               SET HARD-ERROR TO TRUE
           END-IF.

       3600-WRITE-EXPOSURE.
           INITIALIZE KZEXPF-REC
           MOVE CL-CUST-ID TO EX-CUST-ID
           MOVE WS-PRODUCT-TYPE TO EX-PRODUCT-TYPE
           MOVE WS-EXPOSURE-AMT TO EX-EXPOSURE-AMT
           WRITE KZEXPF-REC
           IF FS-KZEXPF = "00"
               ADD 1 TO CNT-EXP-WRITE
           ELSE
               DISPLAY "KZEXPF 書込失敗 顧客=" EX-CUST-ID
                       " ST=" FS-KZEXPF
               SET HARD-ERROR TO TRUE
           END-IF.

       9000-CLOSE-FILES.
           CLOSE KZCOLLF
           IF FS-KZCOLLF NOT = "00"
              AND FS-KZCOLLF NOT = "42"
               DISPLAY "KZCOLLF クローズ失敗 ST=" FS-KZCOLLF
               SET HARD-ERROR TO TRUE
           END-IF

           CLOSE KZACCTF
           IF FS-KZACCTF NOT = "00"
              AND FS-KZACCTF NOT = "42"
               DISPLAY "KZACCTF クローズ失敗 ST=" FS-KZACCTF
               SET HARD-ERROR TO TRUE
           END-IF

           CLOSE KZCUSTF
           IF FS-KZCUSTF NOT = "00"
              AND FS-KZCUSTF NOT = "42"
               DISPLAY "KZCUSTF クローズ失敗 ST=" FS-KZCUSTF
               SET HARD-ERROR TO TRUE
           END-IF

           CLOSE KZEXPF
           IF FS-KZEXPF NOT = "00"
              AND FS-KZEXPF NOT = "42"
               DISPLAY "KZEXPF クローズ失敗 ST=" FS-KZEXPF
               SET HARD-ERROR TO TRUE
           END-IF.
