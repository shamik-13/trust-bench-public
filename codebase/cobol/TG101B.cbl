       IDENTIFICATION DIVISION.
       PROGRAM-ID. TG101B.
      *--------------------------------------------------------------*
      *  変更履歴                                                   *
      *  版数  年月日      担当                         概要        *
      *  1.00  平成28.04  システム部 為替・対外接続チーム 初版作成  *
      *  1.10  令和02.10  システム部 為替・対外接続チーム 接続改定  *
      *  1.20  令和05.07  システム部 為替・対外接続チーム 表示整備  *
      *--------------------------------------------------------------*

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TGOUTCF ASSIGN TO "TGOUTCF".
           SELECT KZACCTF ASSIGN TO "KZACCTF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS AC-ACCT-NO.
           SELECT TGOUTSF ASSIGN TO "TGOUTSF".
           SELECT TGCLJNF ASSIGN TO "TGCLJNF".

       DATA DIVISION.
       FILE SECTION.
       FD TGOUTCF.
       COPY TGOUTCFC.
       FD KZACCTF.
       COPY KZACCTC2.
       FD TGOUTSF.
       COPY TGOUTSC.
       FD TGCLJNF.
       COPY TGCLJNC.

       WORKING-STORAGE SECTION.
       COPY LK-KANA-PARM.
       COPY LK-SIG-PARM.

        01  WS-PROGRAM-ID      PIC X(08).
       01  WS-RUN-DATE        PIC 9(08).
       01  WS-RUN-TIME        PIC 9(06).
       01  WS-SEND-DT         PIC 9(08).
       01  WS-EOF-SW          PIC X(01).
       01  WS-ABEND-SW        PIC X(01).
       01  WS-RETURN-CODE     PIC S9(04) COMP.
       01  WS-FILE-STATUS-TGOUTCF PIC X(02).
       01  WS-FILE-STATUS-KZACCTF PIC X(02).
       01  WS-FILE-STATUS-TGOUTSF PIC X(02).
       01  WS-FILE-STATUS-TGCLJNF PIC X(02).
       01  WS-READ-COUNT      PIC 9(09).
       01  WS-SELECT-COUNT    PIC 9(09).
       01  WS-SKIP-COUNT      PIC 9(09).
       01  WS-ERROR-COUNT     PIC 9(09).
       01  WS-WRITE-SEND-COUNT PIC 9(09).
       01  WS-WRITE-JNL-COUNT PIC 9(09).
       01  WS-SEND-SEQ        PIC 9(10).
       01  WS-CURRENT-GROUP-KEY PIC X(16).
       01  WS-PREV-GROUP-KEY  PIC X(16).
       01  WS-GROUP-FIRST-SW  PIC X(01).
       01  WS-GROUP-ITEM-COUNT PIC 9(07).
       01  WS-GROUP-TOTAL-AMT PIC S9(13)V99 COMP-3.
       01  WS-DAY-ITEM-COUNT  PIC 9(09).
       01  WS-DAY-TOTAL-AMT   PIC S9(15)V99 COMP-3.
       01  WS-VALID-SW        PIC X(01).
       01  WS-MATCH-STATUS    PIC X(02).
       01  WS-ERROR-CODE      PIC X(04).
       01  WS-ERROR-FIELD     PIC X(20).
       01  WS-ERROR-MESSAGE   PIC X(80).
       01  WS-PRODUCT-CLASS   PIC X(02).
       01  WS-ACCOUNT-VARIANT PIC X(02).
       01  WS-BASE-RATE       PIC S9(03)V9(06) COMP-3.
       01  WS-CALC-FEE        PIC S9(09)V99 COMP-3.
       01  WS-FLOOR-FEE       PIC S9(09)V99 COMP-3.
       01  WS-CAP-FEE         PIC S9(09)V99 COMP-3.
       01  WS-NET-SEND-AMT    PIC S9(11)V99 COMP-3.
        01  WS-TG181S-RET      PIC 9(02).


       PROCEDURE DIVISION.
      *    全体制御を処理順に実行する。
            PERFORM 010-INITIALIZE
           PERFORM 020-OPEN-FILES
           IF WS-ABEND-SW NOT = '1'
              PERFORM 100-READ-TGOUTCF
           END-IF
           IF WS-ABEND-SW NOT = '1'
              PERFORM 110-PROCESS-LOOP
           END-IF
           IF WS-ABEND-SW NOT = '1'
              PERFORM 290-WRITE-CLEARING-JOURNAL
           END-IF
           PERFORM 900-FINAL-REPORT
           PERFORM 030-CLOSE-FILES
           IF WS-ABEND-SW = '1'
              MOVE 12 TO WS-RETURN-CODE
           ELSE
              IF WS-ERROR-COUNT > ZERO
                 MOVE 04 TO WS-RETURN-CODE
              ELSE
                 MOVE ZERO TO WS-RETURN-CODE
              END-IF
           END-IF
           MOVE WS-RETURN-CODE TO RETURN-CODE
           EXIT PARAGRAPH.
       010-INITIALIZE.
      *    実行日付と時刻を取得する
           ACCEPT  WS-RUN-DATE  FROM DATE YYYYMMDD
           ACCEPT  WS-RUN-TIME  FROM TIME
           MOVE    WS-RUN-DATE   TO WS-SEND-DT
      *    処理スイッチと戻り値を初期化する
           MOVE    SPACE         TO WS-EOF-SW
           MOVE    SPACE         TO WS-ABEND-SW
           MOVE    ZERO          TO WS-RETURN-CODE
           MOVE    ZERO          TO WS-TG181S-RET
      *    ファイル状態を初期化する
           MOVE    SPACE         TO WS-FILE-STATUS-TGOUTCF
           MOVE    SPACE         TO WS-FILE-STATUS-KZACCTF
           MOVE    SPACE         TO WS-FILE-STATUS-TGOUTSF
           MOVE    SPACE         TO WS-FILE-STATUS-TGCLJNF
      *    件数カウンタを初期化する
           MOVE    ZERO          TO WS-READ-COUNT
           MOVE    ZERO          TO WS-SELECT-COUNT
           MOVE    ZERO          TO WS-SKIP-COUNT
           MOVE    ZERO          TO WS-ERROR-COUNT
           MOVE    ZERO          TO WS-WRITE-SEND-COUNT
           MOVE    ZERO          TO WS-WRITE-JNL-COUNT
           MOVE    ZERO          TO WS-SEND-SEQ
      *    集計キーと集計域を初期化する
           MOVE    SPACE         TO WS-CURRENT-GROUP-KEY
           MOVE    SPACE         TO WS-PREV-GROUP-KEY
           MOVE    SPACE         TO WS-GROUP-FIRST-SW
           MOVE    ZERO          TO WS-GROUP-ITEM-COUNT
           MOVE    ZERO          TO WS-GROUP-TOTAL-AMT
           MOVE    ZERO          TO WS-DAY-ITEM-COUNT
           MOVE    ZERO          TO WS-DAY-TOTAL-AMT
      *    判定域とエラー情報を初期化する
           MOVE    SPACE         TO WS-VALID-SW
           MOVE    SPACE         TO WS-MATCH-STATUS
           MOVE    SPACE         TO WS-ERROR-CODE
           MOVE    SPACE         TO WS-ERROR-FIELD
           MOVE    SPACE         TO WS-ERROR-MESSAGE
           MOVE    SPACE         TO WS-PRODUCT-CLASS
           MOVE    SPACE         TO WS-ACCOUNT-VARIANT
      *    手数料計算域を初期化する
           MOVE    ZERO          TO WS-BASE-RATE
           MOVE    ZERO          TO WS-CALC-FEE
           MOVE    ZERO          TO WS-FLOOR-FEE
           MOVE    ZERO          TO WS-CAP-FEE
           MOVE    ZERO          TO WS-NET-SEND-AMT
      *    CALLパラメータ域を初期化する
           MOVE    SPACE         TO LK-RAW-KANA
           MOVE    SPACE         TO LK-NORM-KANA
           MOVE    SPACE         TO LK-KANA-RET
           MOVE    SPACE         TO LK-SIG-BRANCH
           MOVE    ZERO          TO LK-SIG-DT
           MOVE    SPACE         TO LK-SIG-ORIG-SEQ
           MOVE    ZERO          TO LK-SIG-AMT
           MOVE    SPACE         TO LK-SIGNATURE
      *    サイクル終端環境を確認する
           CALL    'TG181S'
                   RETURNING WS-TG181S-RET
           END-CALL
           IF      WS-TG181S-RET NOT = ZERO
                   MOVE 'Y'      TO WS-ABEND-SW
                   MOVE WS-TG181S-RET
                                  TO WS-RETURN-CODE
                   MOVE 'TG181S' TO WS-ERROR-FIELD
                   MOVE 'サイクル終端環境確認エラー'
                                  TO WS-ERROR-MESSAGE
                   PERFORM 320-ABEND-ROUTINE
           END-IF
           EXIT PARAGRAPH.
       020-OPEN-FILES.
      *    入出力ファイルをオープンする
           OPEN INPUT  TGOUTCF
                       KZACCTF
                OUTPUT TGOUTSF
                EXTEND TGCLJNF
      *    訂正入力ファイルの状態を確認する
           IF WS-FILE-STATUS-TGOUTCF NOT = "00"
               MOVE "OPEN"             TO WS-ERROR-CODE
               MOVE "TGOUTCF"          TO WS-ERROR-FIELD
               MOVE "訂正入力ファイルＯＰＥＮエラー"
                                      TO WS-ERROR-MESSAGE
               PERFORM 310-HANDLE-IO-ERROR
           END-IF
      *    口座マスタファイルの状態を確認する
           IF WS-FILE-STATUS-KZACCTF NOT = "00"
               MOVE "OPEN"             TO WS-ERROR-CODE
               MOVE "KZACCTF"          TO WS-ERROR-FIELD
               MOVE "口座マスタファイルＯＰＥＮエラー"
                                      TO WS-ERROR-MESSAGE
               PERFORM 310-HANDLE-IO-ERROR
           END-IF
      *    送信ファイルの状態を確認する
           IF WS-FILE-STATUS-TGOUTSF NOT = "00"
               MOVE "OPEN"             TO WS-ERROR-CODE
               MOVE "TGOUTSF"          TO WS-ERROR-FIELD
               MOVE "送信ファイルＯＰＥＮエラー"
                                      TO WS-ERROR-MESSAGE
               PERFORM 310-HANDLE-IO-ERROR
           END-IF
      *    クリアリング仕訳ファイルの状態を確認する
           IF WS-FILE-STATUS-TGCLJNF NOT = "00"
               MOVE "OPEN"             TO WS-ERROR-CODE
               MOVE "TGCLJNF"          TO WS-ERROR-FIELD
               MOVE "仕訳ファイルＯＰＥＮエラー"
                                      TO WS-ERROR-MESSAGE
               PERFORM 310-HANDLE-IO-ERROR
           END-IF
           EXIT PARAGRAPH.
       030-CLOSE-FILES.
      * 全ファイルをクローズする。
           CLOSE TGOUTCF
                 KZACCTF
                 TGOUTSF
                 TGCLJNF
      * クローズ異常があれば終了コードへ反映する。
           EVALUATE TRUE
               WHEN WS-FILE-STATUS-TGOUTCF NOT = "00"
                   IF WS-RETURN-CODE < 12
                       MOVE 12 TO WS-RETURN-CODE
                   END-IF
               WHEN WS-FILE-STATUS-KZACCTF NOT = "00"
                   IF WS-RETURN-CODE < 12
                       MOVE 12 TO WS-RETURN-CODE
                   END-IF
               WHEN WS-FILE-STATUS-TGOUTSF NOT = "00"
                   IF WS-RETURN-CODE < 12
                       MOVE 12 TO WS-RETURN-CODE
                   END-IF
               WHEN WS-FILE-STATUS-TGCLJNF NOT = "00"
                   IF WS-RETURN-CODE < 12
                       MOVE 12 TO WS-RETURN-CODE
                   END-IF
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           EXIT PARAGRAPH.
       100-READ-TGOUTCF.
      *    TGOUTCFを原取引順に一件読み込む
           READ TGOUTCF NEXT RECORD
               AT END
                   MOVE "1"             TO WS-EOF-SW
           END-READ
           EVALUATE WS-FILE-STATUS-TGOUTCF
               WHEN "00"
                   ADD 1                TO WS-READ-COUNT
               WHEN "10"
                   MOVE "1"             TO WS-EOF-SW
               WHEN OTHER
                   PERFORM 310-HANDLE-IO-ERROR
           END-EVALUATE
           EXIT PARAGRAPH.
       110-PROCESS-LOOP.
      *    入力終了または異常検知まで訂正送金を順次処理する
           PERFORM UNTIL WS-EOF-SW = '1'
              OR WS-ABEND-SW = '1'
               PERFORM 100-READ-TGOUTCF
               IF WS-EOF-SW = '1'
                   CONTINUE
               ELSE
                   ADD 1 TO WS-READ-COUNT
                   PERFORM 120-SELECT-CORRECTION
                   IF WS-VALID-SW = '1'
                       ADD 1 TO WS-SELECT-COUNT
                       PERFORM 130-VALIDATE-PER-FIELD
                       IF WS-VALID-SW = '1'
                           PERFORM 140-NORMALIZE-KANA
                       END-IF
                       IF WS-VALID-SW = '1'
                           PERFORM 150-READ-KZACCTF
                       END-IF
                       IF WS-VALID-SW = '1'
                           PERFORM 160-MATCH-ACCOUNT
                       END-IF
                       IF WS-VALID-SW = '1'
                           PERFORM 170-DETERMINE-PRODUCT
                           EVALUATE WS-ACCOUNT-VARIANT
                               WHEN '1'
                                   PERFORM 180-HANDLE-ORDINARY-DEPOSIT
                               WHEN '2'
                                   PERFORM 190-HANDLE-CURRENT-DEPOSIT
                               WHEN '3'
                                   PERFORM 200-HANDLE-TRUST-ACCOUNT
                                WHEN OTHER
                                    MOVE '1'            TO WS-VALID-SW
                                    MOVE 'AC'           TO WS-ERROR-CODE
                                    MOVE 'ACCOUNT-VARIANT'
                                    TO WS-ERROR-FIELD
                                    MOVE '口座区分が処理対象
      -                             '外'
                                    TO WS-ERROR-MESSAGE
                            END-EVALUATE
                       END-IF
                       IF WS-VALID-SW = '1'
                           PERFORM 210-CALCULATE-RATE
                           PERFORM 220-CALCULATE-FEE
                           PERFORM 230-APPLY-FEE-FLOOR
                           PERFORM 240-APPLY-FEE-CAP
                           PERFORM 250-BUILD-SEND-RECORD
                           PERFORM 260-CREATE-SIGNATURE
                           PERFORM 270-WRITE-TGOUTSF
                           IF WS-ABEND-SW NOT = '1'
                               ADD 1 TO WS-WRITE-SEND-COUNT
                               PERFORM 280-ACCUMULATE-GROUP
                           END-IF
                       ELSE
                           ADD 1 TO WS-ERROR-COUNT
                           PERFORM 300-HANDLE-VALIDATION-ERROR
                       END-IF
                   ELSE
                       ADD 1 TO WS-SKIP-COUNT
                   END-IF
               END-IF
           END-PERFORM
      *    最終グループの精算ジャーナルを出力する
           IF WS-ABEND-SW NOT = '1'
              AND WS-GROUP-ITEM-COUNT > ZERO
               PERFORM 290-WRITE-CLEARING-JOURNAL
               IF WS-ABEND-SW NOT = '1'
                   ADD 1 TO WS-WRITE-JNL-COUNT
               END-IF
           END-IF
           EXIT PARAGRAPH.
       120-SELECT-CORRECTION.
      *    訂正種別が訂正送金の明細だけを後続処理へ渡す
           MOVE 'N'                    TO WS-VALID-SW
           EVALUATE OC-CORRECTION-TYPE
               WHEN '2'
                   MOVE 'Y'            TO WS-VALID-SW
                   ADD 1               TO WS-SELECT-COUNT
               WHEN OTHER
                   ADD 1               TO WS-SKIP-COUNT
           END-EVALUATE
           EXIT PARAGRAPH.
       130-VALIDATE-PER-FIELD.
      *    訂正日付は実在する取扱日だけを許容する。
           MOVE 'Y'                    TO WS-VALID-SW
           MOVE SPACE                  TO WS-ERROR-CODE
                                         WS-ERROR-FIELD
                                         WS-ERROR-MESSAGE
           EVALUATE TRUE
               WHEN OC-CORRECTION-DT NOT NUMERIC
                   MOVE 'N'            TO WS-VALID-SW
                   MOVE 'E13001'       TO WS-ERROR-CODE
                   MOVE 'OC-CORRECTION-DT'
                                         TO WS-ERROR-FIELD
                   MOVE '訂正日付の形式が不正'
                                         TO WS-ERROR-MESSAGE
                   PERFORM 300-HANDLE-VALIDATION-ERROR
                   EXIT PARAGRAPH
               WHEN OC-CORRECTION-DT < 19000101
                   MOVE 'N'            TO WS-VALID-SW
                   MOVE 'E13002'       TO WS-ERROR-CODE
                   MOVE 'OC-CORRECTION-DT'
                                         TO WS-ERROR-FIELD
                   MOVE '訂正日付が範囲外'
                                         TO WS-ERROR-MESSAGE
                   PERFORM 300-HANDLE-VALIDATION-ERROR
                   EXIT PARAGRAPH
               WHEN FUNCTION MOD(OC-CORRECTION-DT 10000) < 0101
                   MOVE 'N'            TO WS-VALID-SW
                   MOVE 'E13003'       TO WS-ERROR-CODE
                   MOVE 'OC-CORRECTION-DT'
                                         TO WS-ERROR-FIELD
                   MOVE '訂正日付の月日が不正'
                                         TO WS-ERROR-MESSAGE
                   PERFORM 300-HANDLE-VALIDATION-ERROR
                   EXIT PARAGRAPH
               WHEN FUNCTION MOD(OC-CORRECTION-DT 10000) > 1231
                   MOVE 'N'            TO WS-VALID-SW
                   MOVE 'E13004'       TO WS-ERROR-CODE
                   MOVE 'OC-CORRECTION-DT'
                                         TO WS-ERROR-FIELD
                   MOVE '訂正日付の月日が不正'
                                         TO WS-ERROR-MESSAGE
                   PERFORM 300-HANDLE-VALIDATION-ERROR
                   EXIT PARAGRAPH
               WHEN FUNCTION MOD(OC-CORRECTION-DT 100) = ZERO
                   MOVE 'N'            TO WS-VALID-SW
                   MOVE 'E13005'       TO WS-ERROR-CODE
                   MOVE 'OC-CORRECTION-DT'
                                         TO WS-ERROR-FIELD
                   MOVE '訂正日付の日が不正'
                                         TO WS-ERROR-MESSAGE
                   PERFORM 300-HANDLE-VALIDATION-ERROR
                   EXIT PARAGRAPH
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           EVALUATE FUNCTION MOD(OC-CORRECTION-DT 10000) / 100
               WHEN 01
               WHEN 03
               WHEN 05
               WHEN 07
               WHEN 08
               WHEN 10
               WHEN 12
                   IF FUNCTION MOD(OC-CORRECTION-DT 100) > 31
                       MOVE 'N'        TO WS-VALID-SW
                       MOVE 'E13006'   TO WS-ERROR-CODE
                       MOVE 'OC-CORRECTION-DT'
                                         TO WS-ERROR-FIELD
                       MOVE '訂正日付の日が不正'
                                         TO WS-ERROR-MESSAGE
                       PERFORM 300-HANDLE-VALIDATION-ERROR
                       EXIT PARAGRAPH
                   END-IF
               WHEN 04
               WHEN 06
               WHEN 09
               WHEN 11
                   IF FUNCTION MOD(OC-CORRECTION-DT 100) > 30
                       MOVE 'N'        TO WS-VALID-SW
                       MOVE 'E13007'   TO WS-ERROR-CODE
                       MOVE 'OC-CORRECTION-DT'
                                         TO WS-ERROR-FIELD
                       MOVE '訂正日付の日が不正'
                                         TO WS-ERROR-MESSAGE
                       PERFORM 300-HANDLE-VALIDATION-ERROR
                       EXIT PARAGRAPH
                   END-IF
               WHEN 02
                   IF FUNCTION MOD(OC-CORRECTION-DT 100) > 29
                       MOVE 'N'        TO WS-VALID-SW
                       MOVE 'E13008'   TO WS-ERROR-CODE
                       MOVE 'OC-CORRECTION-DT'
                                         TO WS-ERROR-FIELD
                       MOVE '訂正日付の日が不正'
                                         TO WS-ERROR-MESSAGE
                       PERFORM 300-HANDLE-VALIDATION-ERROR
                       EXIT PARAGRAPH
                   END-IF
                   IF FUNCTION MOD(OC-CORRECTION-DT 100) = 29
                      AND FUNCTION MOD(OC-CORRECTION-DT / 10000 4)
                           NOT = ZERO
                       MOVE 'N'        TO WS-VALID-SW
                       MOVE 'E13009'   TO WS-ERROR-CODE
                       MOVE 'OC-CORRECTION-DT'
                                         TO WS-ERROR-FIELD
                       MOVE '訂正日付の閏日が不正'
                                         TO WS-ERROR-MESSAGE
                       PERFORM 300-HANDLE-VALIDATION-ERROR
                       EXIT PARAGRAPH
                   END-IF
               WHEN OTHER
                   MOVE 'N'            TO WS-VALID-SW
                   MOVE 'E13010'       TO WS-ERROR-CODE
                   MOVE 'OC-CORRECTION-DT'
                                         TO WS-ERROR-FIELD
                   MOVE '訂正日付の月が不正'
                                         TO WS-ERROR-MESSAGE
                   PERFORM 300-HANDLE-VALIDATION-ERROR
                   EXIT PARAGRAPH
           END-EVALUATE
      *    センタ採番、銀行店番、口座属性を単項目で確認する。
           IF OC-ORIG-CENTER-SEQ = SPACE
               MOVE 'N'                TO WS-VALID-SW
               MOVE 'E13011'           TO WS-ERROR-CODE
               MOVE 'OC-ORIG-CENTER-SEQ'
                                         TO WS-ERROR-FIELD
               MOVE '元センタ連番が未設定'
                                         TO WS-ERROR-MESSAGE
               PERFORM 300-HANDLE-VALIDATION-ERROR
               EXIT PARAGRAPH
           END-IF
           IF OC-PAYEE-BANK NOT NUMERIC
              OR OC-PAYEE-BANK = ZERO
               MOVE 'N'                TO WS-VALID-SW
               MOVE 'E13012'           TO WS-ERROR-CODE
               MOVE 'OC-PAYEE-BANK'    TO WS-ERROR-FIELD
               MOVE '銀行番号が不正'   TO WS-ERROR-MESSAGE
               PERFORM 300-HANDLE-VALIDATION-ERROR
               EXIT PARAGRAPH
           END-IF
           IF OC-PAYEE-BRANCH NOT NUMERIC
              OR OC-PAYEE-BRANCH = ZERO
               MOVE 'N'                TO WS-VALID-SW
               MOVE 'E13013'           TO WS-ERROR-CODE
               MOVE 'OC-PAYEE-BRANCH'  TO WS-ERROR-FIELD
               MOVE '店番号が不正'     TO WS-ERROR-MESSAGE
               PERFORM 300-HANDLE-VALIDATION-ERROR
               EXIT PARAGRAPH
           END-IF
           EVALUATE OC-PAYEE-ACCT-TYPE
               WHEN '1'
               WHEN '2'
               WHEN '3'
                   CONTINUE
               WHEN OTHER
                   MOVE 'N'            TO WS-VALID-SW
                   MOVE 'E13014'       TO WS-ERROR-CODE
                   MOVE 'OC-PAYEE-ACCT-TYPE'
                                         TO WS-ERROR-FIELD
                   MOVE '科目が不正'   TO WS-ERROR-MESSAGE
                   PERFORM 300-HANDLE-VALIDATION-ERROR
                   EXIT PARAGRAPH
           END-EVALUATE
           IF OC-PAYEE-ACCT-NO NOT NUMERIC
              OR OC-PAYEE-ACCT-NO = ZERO
               MOVE 'N'                TO WS-VALID-SW
               MOVE 'E13015'           TO WS-ERROR-CODE
               MOVE 'OC-PAYEE-ACCT-NO' TO WS-ERROR-FIELD
               MOVE '口座番号が不正'   TO WS-ERROR-MESSAGE
               PERFORM 300-HANDLE-VALIDATION-ERROR
               EXIT PARAGRAPH
           END-IF
      *    受取人カナは正規化サブルーチンの結果で判定する。
           IF OC-PAYEE-NAME-KANA = SPACE
               MOVE 'N'                TO WS-VALID-SW
               MOVE 'E13016'           TO WS-ERROR-CODE
               MOVE 'OC-PAYEE-NAME-KANA'
                                         TO WS-ERROR-FIELD
               MOVE '受取人カナが未設定'
                                         TO WS-ERROR-MESSAGE
               PERFORM 300-HANDLE-VALIDATION-ERROR
               EXIT PARAGRAPH
           END-IF
           PERFORM 140-NORMALIZE-KANA
           IF LK-KANA-RET NOT = '00'
               MOVE 'N'                TO WS-VALID-SW
               MOVE 'E13017'           TO WS-ERROR-CODE
               MOVE 'OC-PAYEE-NAME-KANA'
                                         TO WS-ERROR-FIELD
               MOVE '受取人カナに使用不可文字あり'
                                         TO WS-ERROR-MESSAGE
               PERFORM 300-HANDLE-VALIDATION-ERROR
               EXIT PARAGRAPH
           END-IF
      *    金額と元署名の必須性を確認する。
           IF OC-OUT-AMT NOT NUMERIC
              OR OC-OUT-AMT <= ZERO
               MOVE 'N'                TO WS-VALID-SW
               MOVE 'E13018'           TO WS-ERROR-CODE
               MOVE 'OC-OUT-AMT'       TO WS-ERROR-FIELD
               MOVE '金額が不正'       TO WS-ERROR-MESSAGE
               PERFORM 300-HANDLE-VALIDATION-ERROR
               EXIT PARAGRAPH
           END-IF
           IF OC-OC-SIGNATURE = SPACE
               MOVE 'N'                TO WS-VALID-SW
               MOVE 'E13019'           TO WS-ERROR-CODE
               MOVE 'OC-OC-SIGNATURE'  TO WS-ERROR-FIELD
               MOVE '元署名が未設定'   TO WS-ERROR-MESSAGE
               PERFORM 300-HANDLE-VALIDATION-ERROR
               EXIT PARAGRAPH
           END-IF
           EXIT PARAGRAPH.
       140-NORMALIZE-KANA.
      *    受取人カナを送信用カナへ正規化する
           MOVE SPACES                 TO LK-RAW-KANA
                                          LK-NORM-KANA
                                          LK-KANA-RET
           MOVE OC-PAYEE-NAME-KANA     TO LK-RAW-KANA
           CALL 'TG912S' USING LK-KANA-PARM
           EVALUATE LK-KANA-RET
             WHEN '00'
               MOVE LK-NORM-KANA       TO OC-PAYEE-NAME-KANA
             WHEN OTHER
      *        正規化不可は項目エラーとして後続処理を止める
               MOVE 'N'                TO WS-VALID-SW
               MOVE 'KANA'             TO WS-ERROR-CODE
               MOVE 'OC-PAYEE-NAME-KANA'
                                        TO WS-ERROR-FIELD
               MOVE 'ｳｹﾄﾘﾆﾝｶﾅ ﾍﾝｶﾝ ｴﾗｰ'
                                        TO WS-ERROR-MESSAGE
               PERFORM 300-HANDLE-VALIDATION-ERROR
           END-EVALUATE
           EXIT PARAGRAPH.
       150-READ-KZACCTF.
      *    被仕向口座番号で口座マスタを直接参照する
           MOVE OC-PAYEE-ACCT-NO       TO AC-ACCT-NO
           READ KZACCTF
                KEY IS AC-ACCT-NO
           END-READ
           EVALUATE WS-FILE-STATUS-KZACCTF
               WHEN '00'
      *            口座マスタを正常に取得した
                   MOVE 'Y'            TO WS-MATCH-STATUS
                   MOVE SPACE          TO WS-ERROR-CODE
                   MOVE SPACE          TO WS-ERROR-FIELD
                   MOVE SPACE          TO WS-ERROR-MESSAGE
               WHEN '23'
      *            口座番号に該当する口座が存在しない
                   MOVE 'N'            TO WS-MATCH-STATUS
                   MOVE 'N'            TO WS-VALID-SW
                   MOVE 'KZ001'        TO WS-ERROR-CODE
                   MOVE 'OC-PAYEE-ACCT-NO'
                                       TO WS-ERROR-FIELD
                   MOVE 'ｺｳｻﾞﾏｽﾀﾐﾄｳﾛｸ'
                                       TO WS-ERROR-MESSAGE
               WHEN OTHER
      *            続行できない入出力エラーとして扱う
                   MOVE 'Y'            TO WS-ABEND-SW
                   MOVE 12             TO WS-RETURN-CODE
                   MOVE 'KZ901'        TO WS-ERROR-CODE
                   MOVE 'KZACCTF'      TO WS-ERROR-FIELD
                   MOVE 'ｺｳｻﾞﾏｽﾀREADｴﾗｰ'
                                       TO WS-ERROR-MESSAGE
                   PERFORM 310-HANDLE-IO-ERROR
           END-EVALUATE
           EXIT PARAGRAPH.
       160-MATCH-ACCOUNT.
      *    口座属性を訂正送金の入力内容と照合する。
           MOVE '0'                    TO WS-MATCH-STATUS
           EVALUATE TRUE
               WHEN AC-BRANCH NOT = OC-PAYEE-BRANCH
                   MOVE '1'            TO WS-MATCH-STATUS
               WHEN AC-ACCT-TYPE NOT = OC-PAYEE-ACCT-TYPE
                   MOVE '2'            TO WS-MATCH-STATUS
               WHEN AC-STATUS NOT = '0'
                   MOVE '3'            TO WS-MATCH-STATUS
               WHEN AC-ACCT-NAME-KANA NOT = LK-NORM-KANA
                   MOVE '4'            TO WS-MATCH-STATUS
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
            EXIT PARAGRAPH.
        170-DETERMINE-PRODUCT.
      * 商品分類と後続計算で使用する処理条件を初期化する
           MOVE SPACE                  TO WS-PRODUCT-CLASS
           MOVE SPACE                  TO WS-ACCOUNT-VARIANT
           MOVE ZERO                   TO WS-BASE-RATE
           MOVE ZERO                   TO WS-CALC-FEE
           MOVE ZERO                   TO WS-FLOOR-FEE
           MOVE ZERO                   TO WS-CAP-FEE
           MOVE OC-OUT-AMT             TO WS-NET-SEND-AMT
      * 科目別の商品分類を決定する
           EVALUATE OC-PAYEE-ACCT-TYPE
             WHEN '1'
               MOVE '01'               TO WS-PRODUCT-CLASS
               PERFORM 180-HANDLE-ORDINARY-DEPOSIT
             WHEN '2'
               MOVE '02'               TO WS-PRODUCT-CLASS
               PERFORM 190-HANDLE-CURRENT-DEPOSIT
             WHEN '4'
               MOVE '03'               TO WS-PRODUCT-CLASS
               PERFORM 200-HANDLE-TRUST-ACCOUNT
             WHEN OTHER
               MOVE '90'               TO WS-PRODUCT-CLASS
               MOVE 'ACCT-TYPE'        TO WS-ERROR-FIELD
               MOVE '1701'             TO WS-ERROR-CODE
               MOVE '対象外科目です'   TO WS-ERROR-MESSAGE
               MOVE 'N'                TO WS-VALID-SW
               EXIT PARAGRAPH
           END-EVALUATE
      * 銀行コードと店番から内為区分を決定する
           IF OC-SENDER-BANK = OC-PAYEE-BANK
             IF OC-SENDER-BRANCH = OC-PAYEE-BRANCH
               MOVE 'OWN '             TO WS-ACCOUNT-VARIANT
             ELSE
               MOVE 'INBK'             TO WS-ACCOUNT-VARIANT
             END-IF
           ELSE
             MOVE 'OTBK'               TO WS-ACCOUNT-VARIANT
           END-IF
      * 訂正種別により返戻・組戻の扱いを優先する
           EVALUATE OC-CORRECTION-TYPE
             WHEN '1'
               CONTINUE
             WHEN '2'
               MOVE 'CNCL'             TO WS-ACCOUNT-VARIANT
               MOVE ZERO               TO WS-BASE-RATE
               MOVE ZERO               TO WS-FLOOR-FEE
               MOVE ZERO               TO WS-CAP-FEE
               EXIT PARAGRAPH
             WHEN '3'
               MOVE 'RVSL'             TO WS-ACCOUNT-VARIANT
             WHEN OTHER
               MOVE 'CORR-TYPE'        TO WS-ERROR-FIELD
               MOVE '1702'             TO WS-ERROR-CODE
               MOVE '対象外訂正種別です'
                                       TO WS-ERROR-MESSAGE
               MOVE 'N'                TO WS-VALID-SW
               EXIT PARAGRAPH
           END-EVALUATE
      * 金額帯と内為区分から料率計算条件を決定する
           EVALUATE TRUE
             WHEN OC-OUT-AMT < 30000
               EVALUATE WS-ACCOUNT-VARIANT
                 WHEN 'OWN '
                   MOVE 0              TO WS-BASE-RATE
                   MOVE 0              TO WS-FLOOR-FEE
                   MOVE 0              TO WS-CAP-FEE
                 WHEN 'INBK'
                   MOVE 15             TO WS-BASE-RATE
                   MOVE 110            TO WS-FLOOR-FEE
                   MOVE 330            TO WS-CAP-FEE
                 WHEN OTHER
                   MOVE 25             TO WS-BASE-RATE
                   MOVE 220            TO WS-FLOOR-FEE
                   MOVE 660            TO WS-CAP-FEE
               END-EVALUATE
             WHEN OC-OUT-AMT <= 1000000
               EVALUATE WS-ACCOUNT-VARIANT
                 WHEN 'OWN '
                   MOVE 0              TO WS-BASE-RATE
                   MOVE 0              TO WS-FLOOR-FEE
                   MOVE 0              TO WS-CAP-FEE
                 WHEN 'INBK'
                   MOVE 20             TO WS-BASE-RATE
                   MOVE 220            TO WS-FLOOR-FEE
                   MOVE 550            TO WS-CAP-FEE
                 WHEN OTHER
                   MOVE 35             TO WS-BASE-RATE
                   MOVE 440            TO WS-FLOOR-FEE
                   MOVE 880            TO WS-CAP-FEE
               END-EVALUATE
             WHEN OTHER
               EVALUATE WS-ACCOUNT-VARIANT
                 WHEN 'OWN '
                   MOVE 5              TO WS-BASE-RATE
                   MOVE 110            TO WS-FLOOR-FEE
                   MOVE 550            TO WS-CAP-FEE
                 WHEN 'INBK'
                   MOVE 30             TO WS-BASE-RATE
                   MOVE 550            TO WS-FLOOR-FEE
                   MOVE 1100           TO WS-CAP-FEE
                 WHEN OTHER
                   MOVE 45             TO WS-BASE-RATE
                   MOVE 770            TO WS-FLOOR-FEE
                   MOVE 1650           TO WS-CAP-FEE
               END-EVALUATE
           END-EVALUATE
      * 信託口座は高額帯のみ送信対象とする
           IF WS-PRODUCT-CLASS = '03'
             IF OC-OUT-AMT < 1000000
               MOVE 'AMOUNT'           TO WS-ERROR-FIELD
               MOVE '1703'             TO WS-ERROR-CODE
               MOVE '信託口座の金額帯対象外です'
                                       TO WS-ERROR-MESSAGE
               MOVE 'N'                TO WS-VALID-SW
               EXIT PARAGRAPH
             END-IF
           END-IF
           PERFORM 210-CALCULATE-RATE
           PERFORM 220-CALCULATE-FEE
           PERFORM 230-APPLY-FEE-FLOOR
           PERFORM 240-APPLY-FEE-CAP
           .
       180-HANDLE-ORDINARY-DEPOSIT.
      *    普通預金の送信条件を確認する
           IF WS-VALID-SW NOT = '1'
               EXIT PARAGRAPH
           END-IF
           IF AC-ACCT-TYPE NOT = OC-PAYEE-ACCT-TYPE
               MOVE '0'              TO WS-VALID-SW
               MOVE '1811'           TO WS-ERROR-CODE
               MOVE 'PAYEE-ACCT-TYPE'
                                      TO WS-ERROR-FIELD
               MOVE '口座種別が普通預金と一致しません'
                                      TO WS-ERROR-MESSAGE
               PERFORM 300-HANDLE-VALIDATION-ERROR
               EXIT PARAGRAPH
           END-IF
           EVALUATE AC-STATUS
               WHEN '0'
                   CONTINUE
               WHEN '1'
                   MOVE '0'          TO WS-VALID-SW
                   MOVE '1812'       TO WS-ERROR-CODE
                   MOVE 'AC-STATUS'  TO WS-ERROR-FIELD
                   MOVE '普通預金口座が停止中です'
                                      TO WS-ERROR-MESSAGE
                   PERFORM 300-HANDLE-VALIDATION-ERROR
                   EXIT PARAGRAPH
               WHEN '9'
                   MOVE '0'          TO WS-VALID-SW
                   MOVE '1813'       TO WS-ERROR-CODE
                   MOVE 'AC-STATUS'  TO WS-ERROR-FIELD
                   MOVE '普通預金口座が解約済です'
                                      TO WS-ERROR-MESSAGE
                   PERFORM 300-HANDLE-VALIDATION-ERROR
                   EXIT PARAGRAPH
               WHEN OTHER
                   MOVE '0'          TO WS-VALID-SW
                   MOVE '1814'       TO WS-ERROR-CODE
                   MOVE 'AC-STATUS'  TO WS-ERROR-FIELD
                   MOVE '普通預金口座状態が不正です'
                                      TO WS-ERROR-MESSAGE
                   PERFORM 300-HANDLE-VALIDATION-ERROR
                   EXIT PARAGRAPH
           END-EVALUATE
      *    依頼人入力カナと口座カナの名寄せ許容差を確認する
           MOVE OC-PAYEE-NAME-KANA    TO LK-RAW-KANA
           CALL 'TG912S' USING LK-KANA-PARM
           IF LK-KANA-RET NOT = '00'
               MOVE '0'              TO WS-VALID-SW
               MOVE '1815'           TO WS-ERROR-CODE
               MOVE 'PAYEE-NAME-KANA'
                                      TO WS-ERROR-FIELD
               MOVE '受取人カナの正規化に失敗しました'
                                      TO WS-ERROR-MESSAGE
               PERFORM 300-HANDLE-VALIDATION-ERROR
               EXIT PARAGRAPH
           END-IF
           MOVE LK-NORM-KANA          TO OS-PAYEE-NAME-KANA
           MOVE AC-ACCT-NAME-KANA     TO LK-RAW-KANA
           CALL 'TG912S' USING LK-KANA-PARM
           IF LK-KANA-RET NOT = '00'
               MOVE '0'              TO WS-VALID-SW
               MOVE '1816'           TO WS-ERROR-CODE
               MOVE 'AC-ACCT-NAME-KANA'
                                      TO WS-ERROR-FIELD
               MOVE '口座カナの正規化に失敗しました'
                                      TO WS-ERROR-MESSAGE
               PERFORM 300-HANDLE-VALIDATION-ERROR
               EXIT PARAGRAPH
           END-IF
           IF OS-PAYEE-NAME-KANA (1:30) NOT =
              LK-NORM-KANA        (1:30)
               MOVE '0'              TO WS-VALID-SW
               MOVE '1817'           TO WS-ERROR-CODE
               MOVE 'PAYEE-NAME-KANA'
                                      TO WS-ERROR-FIELD
                MOVE '普通預金口座のカナ名寄せ不一致
      -         'です'
                                       TO WS-ERROR-MESSAGE
               PERFORM 300-HANDLE-VALIDATION-ERROR
               EXIT PARAGRAPH
           END-IF
      *    普通預金向けの送金金額制限を適用する
           IF OC-OUT-AMT <= ZERO
               MOVE '0'              TO WS-VALID-SW
               MOVE '1818'           TO WS-ERROR-CODE
               MOVE 'OUT-AMT'        TO WS-ERROR-FIELD
               MOVE '送金金額が普通預金の下限未満です'
                                      TO WS-ERROR-MESSAGE
               PERFORM 300-HANDLE-VALIDATION-ERROR
               EXIT PARAGRAPH
           END-IF
           IF OC-OUT-AMT > 10000000
               MOVE '0'              TO WS-VALID-SW
               MOVE '1819'           TO WS-ERROR-CODE
               MOVE 'OUT-AMT'        TO WS-ERROR-FIELD
               MOVE '送金金額が普通預金の上限超過です'
                                      TO WS-ERROR-MESSAGE
               PERFORM 300-HANDLE-VALIDATION-ERROR
               EXIT PARAGRAPH
           END-IF
           MOVE 'ORDINARY'            TO WS-PRODUCT-CLASS
           MOVE 'STD'                 TO WS-ACCOUNT-VARIANT
           MOVE 'A'                   TO WS-MATCH-STATUS
           EXIT PARAGRAPH.
       190-HANDLE-CURRENT-DEPOSIT.
      *    当座預金は店番、名義カナ、状態、金額上限を個別確認する。
           IF OC-PAYEE-BRANCH NOT = AC-BRANCH
               MOVE '1901' TO WS-ERROR-CODE
               MOVE 'OC-PAYEE-BRANCH' TO WS-ERROR-FIELD
                MOVE '当座預金の店番が口座店番と不
      -         '一致' TO WS-ERROR-MESSAGE
                ADD 1 TO WS-ERROR-COUNT
               PERFORM 300-HANDLE-VALIDATION-ERROR
               EXIT PARAGRAPH
           END-IF
           MOVE OC-PAYEE-NAME-KANA TO LK-RAW-KANA
           PERFORM 140-NORMALIZE-KANA
           IF LK-KANA-RET NOT = '00'
               MOVE '1902' TO WS-ERROR-CODE
               MOVE 'OC-PAYEE-NAME-KANA' TO WS-ERROR-FIELD
                MOVE '法人名カナの正規化エラー'
                TO WS-ERROR-MESSAGE
                ADD 1 TO WS-ERROR-COUNT
                PERFORM 300-HANDLE-VALIDATION-ERROR
                EXIT PARAGRAPH
            END-IF
            MOVE LK-NORM-KANA TO OC-PAYEE-NAME-KANA
            IF OC-PAYEE-NAME-KANA NOT = AC-ACCT-NAME-KANA
                MOVE '1903' TO WS-ERROR-CODE
                MOVE 'OC-PAYEE-NAME-KANA' TO WS-ERROR-FIELD
                MOVE '法人名カナが口座名義と不一致'
                TO WS-ERROR-MESSAGE
                ADD 1 TO WS-ERROR-COUNT
                PERFORM 300-HANDLE-VALIDATION-ERROR
                EXIT PARAGRAPH
            END-IF
           EVALUATE AC-STATUS
               WHEN '11'
                   CONTINUE
               WHEN '12'
                   CONTINUE
               WHEN OTHER
                   MOVE '1904' TO WS-ERROR-CODE
                   MOVE 'AC-STATUS' TO WS-ERROR-FIELD
                    MOVE '当座預金の状態コードが
      -             '送信対象外' TO WS-ERROR-MESSAGE
                    ADD 1 TO WS-ERROR-COUNT
                    PERFORM 300-HANDLE-VALIDATION-ERROR
                    EXIT PARAGRAPH
            END-EVALUATE
            IF OC-OUT-AMT > 50000000
                 MOVE '1905' TO WS-ERROR-CODE
                MOVE 'OC-OUT-AMT' TO WS-ERROR-FIELD
                MOVE '当座預金の送金上限金額超過'
                TO WS-ERROR-MESSAGE
                ADD 1 TO WS-ERROR-COUNT
               PERFORM 300-HANDLE-VALIDATION-ERROR
               EXIT PARAGRAPH
           END-IF
           MOVE 'CURRENT' TO WS-ACCOUNT-VARIANT
           MOVE 'Y' TO WS-VALID-SW
           EXIT PARAGRAPH.
       200-HANDLE-TRUST-ACCOUNT.
      *    信託口座は受益者の実口座内容で送信する。
           IF AC-STATUS = "0"
              MOVE AC-BRANCH         TO OC-PAYEE-BRANCH
              MOVE AC-ACCT-TYPE      TO OC-PAYEE-ACCT-TYPE
              MOVE AC-ACCT-NO        TO OC-PAYEE-ACCT-NO
              IF AC-ACCT-NAME-KANA NOT = SPACE
                 MOVE AC-ACCT-NAME-KANA TO LK-RAW-KANA
                 PERFORM 140-NORMALIZE-KANA
                 IF LK-KANA-RET = "00"
                    MOVE LK-NORM-KANA TO OC-PAYEE-NAME-KANA
                 ELSE
                    MOVE AC-ACCT-NAME-KANA TO OC-PAYEE-NAME-KANA
                 END-IF
              END-IF
      *       同一行内の送金は社内店番に寄せる。
              IF OC-SENDER-BANK = OC-PAYEE-BANK
                 MOVE AC-BRANCH      TO OC-PAYEE-BRANCH
              END-IF
      *       信託口座扱いは送金種別を内部振替扱いに補正する。
              EVALUATE TRUE
                 WHEN OC-SENDER-BANK = OC-PAYEE-BANK
                    MOVE "2"         TO WS-PRODUCT-CLASS
                 WHEN OC-OUT-AMT > ZERO
                    MOVE "1"         TO WS-PRODUCT-CLASS
                 WHEN OTHER
                    MOVE "9"         TO WS-PRODUCT-CLASS
              END-EVALUATE
      *       名義が取れた信託口座は照合済みとして補足する。
              IF WS-MATCH-STATUS NOT = "0"
                 IF OC-PAYEE-NAME-KANA = LK-NORM-KANA
                    MOVE "0"         TO WS-MATCH-STATUS
                 END-IF
              END-IF
           ELSE
              MOVE "1"               TO WS-VALID-SW
              MOVE "TR"              TO WS-ERROR-CODE
              MOVE "PAYEE-ACCT"      TO WS-ERROR-FIELD
              MOVE "シンタクコウザ ムコウ" TO WS-ERROR-MESSAGE
           END-IF
           EXIT PARAGRAPH.
       210-CALCULATE-RATE.
      *    商品分類、店番、金額帯から基準料率を決定する
           EVALUATE WS-PRODUCT-CLASS
               WHEN '01'
                   EVALUATE TRUE
                       WHEN OC-SENDER-BRANCH >= '0100'
                        AND OC-SENDER-BRANCH <= '0199'
                           EVALUATE TRUE
                               WHEN OC-OUT-AMT < 3000000
                                   MOVE 0.0010 TO WS-BASE-RATE
                               WHEN OC-OUT-AMT < 10000000
                                   MOVE 0.0008 TO WS-BASE-RATE
                               WHEN OTHER
                                   MOVE 0.0006 TO WS-BASE-RATE
                           END-EVALUATE
                       WHEN OTHER
                           EVALUATE TRUE
                               WHEN OC-OUT-AMT < 3000000
                                   MOVE 0.0012 TO WS-BASE-RATE
                               WHEN OC-OUT-AMT < 10000000
                                   MOVE 0.0010 TO WS-BASE-RATE
                               WHEN OTHER
                                   MOVE 0.0008 TO WS-BASE-RATE
                           END-EVALUATE
                   END-EVALUATE
               WHEN '02'
                   EVALUATE TRUE
                       WHEN OC-SENDER-BRANCH >= '0100'
                        AND OC-SENDER-BRANCH <= '0199'
                           EVALUATE TRUE
                               WHEN OC-OUT-AMT < 3000000
                                   MOVE 0.0014 TO WS-BASE-RATE
                               WHEN OC-OUT-AMT < 10000000
                                   MOVE 0.0011 TO WS-BASE-RATE
                               WHEN OTHER
                                   MOVE 0.0009 TO WS-BASE-RATE
                           END-EVALUATE
                       WHEN OTHER
                           EVALUATE TRUE
                               WHEN OC-OUT-AMT < 3000000
                                   MOVE 0.0016 TO WS-BASE-RATE
                               WHEN OC-OUT-AMT < 10000000
                                   MOVE 0.0013 TO WS-BASE-RATE
                               WHEN OTHER
                                   MOVE 0.0010 TO WS-BASE-RATE
                           END-EVALUATE
                   END-EVALUATE
               WHEN '03'
                   EVALUATE TRUE
                       WHEN OC-OUT-AMT < 3000000
                           MOVE 0.0020 TO WS-BASE-RATE
                       WHEN OC-OUT-AMT < 10000000
                           MOVE 0.0017 TO WS-BASE-RATE
                       WHEN OTHER
                           MOVE 0.0015 TO WS-BASE-RATE
                   END-EVALUATE
               WHEN OTHER
                   MOVE 0.0018 TO WS-BASE-RATE
           END-EVALUATE
            EXIT PARAGRAPH.
        220-CALCULATE-FEE.
      *    送金額と基準率から基礎手数料を算出する。
           MOVE ZERO TO WS-CALC-FEE
           IF OC-OUT-AMT > ZERO
              AND WS-BASE-RATE > ZERO
              COMPUTE WS-CALC-FEE ROUNDED =
                      OC-OUT-AMT * WS-BASE-RATE / 100
              END-COMPUTE
           END-IF
      *    手数料は円単位にそろえる。
           COMPUTE WS-CALC-FEE =
                   FUNCTION INTEGER(WS-CALC-FEE)
           END-COMPUTE
           EXIT PARAGRAPH
            .
        230-APPLY-FEE-FLOOR.
      *    商品と口座分類に応じた最低手数料を求める。
           EVALUATE WS-PRODUCT-CLASS ALSO WS-ACCOUNT-VARIANT
               WHEN 'ORDINARY' ALSO 'STANDARD'
                   MOVE 100 TO WS-FLOOR-FEE
               WHEN 'ORDINARY' ALSO 'PREFERRED'
                   MOVE 50 TO WS-FLOOR-FEE
               WHEN 'CURRENT' ALSO 'STANDARD'
                   MOVE 150 TO WS-FLOOR-FEE
               WHEN 'CURRENT' ALSO 'PREFERRED'
                   MOVE 100 TO WS-FLOOR-FEE
               WHEN 'TRUST' ALSO 'STANDARD'
                   MOVE 200 TO WS-FLOOR-FEE
               WHEN 'TRUST' ALSO 'PREFERRED'
                   MOVE 150 TO WS-FLOOR-FEE
               WHEN OTHER
                   MOVE 100 TO WS-FLOOR-FEE
           END-EVALUATE
      *    算出手数料が最低手数料を下回る場合は引き上げる。
           IF WS-CALC-FEE > WS-FLOOR-FEE
               MOVE WS-CALC-FEE TO WS-FLOOR-FEE
           END-IF
           EXIT PARAGRAPH.
       240-APPLY-FEE-CAP.
      *    商品と口座分類に応じた上限額を決定する
           EVALUATE WS-PRODUCT-CLASS ALSO WS-ACCOUNT-VARIANT
               WHEN '01' ALSO '1'
                   MOVE 1500 TO WS-CAP-FEE
               WHEN '01' ALSO '2'
                   MOVE 2500 TO WS-CAP-FEE
               WHEN '02' ALSO '1'
                   MOVE 2000 TO WS-CAP-FEE
               WHEN '02' ALSO '2'
                   MOVE 3000 TO WS-CAP-FEE
               WHEN '03' ALSO '1'
                   MOVE 1000 TO WS-CAP-FEE
               WHEN '03' ALSO '2'
                   MOVE 2000 TO WS-CAP-FEE
               WHEN OTHER
                   MOVE 3000 TO WS-CAP-FEE
           END-EVALUATE
      *    算出手数料が上限を超える場合は上限額に抑える
           IF WS-CALC-FEE < WS-CAP-FEE
               MOVE WS-CALC-FEE TO WS-CAP-FEE
           END-IF
      *    送信金額は出金額から確定手数料を差し引く
           IF OC-OUT-AMT > WS-CAP-FEE
               COMPUTE WS-NET-SEND-AMT =
                       OC-OUT-AMT - WS-CAP-FEE
           ELSE
               MOVE ZERO TO WS-NET-SEND-AMT
               MOVE OC-OUT-AMT TO WS-CAP-FEE
           END-IF
           EXIT PARAGRAPH.
       250-BUILD-SEND-RECORD.
      *    送信ファイル出力用の明細を組み立てる
           INITIALIZE TGOUTSF-REC
           ADD 1 TO WS-SEND-SEQ
           MOVE WS-SEND-DT          TO OS-SEND-DT
           MOVE WS-SEND-SEQ         TO OS-SEND-SEQ
           MOVE OC-CORRECTION-TYPE  TO OS-REMIT-TYPE
           MOVE OC-SENDER-BANK      TO OS-SENDER-BANK
           MOVE OC-SENDER-BRANCH    TO OS-SENDER-BRANCH
           MOVE OC-PAYEE-BANK       TO OS-PAYEE-BANK
           MOVE OC-PAYEE-BRANCH     TO OS-PAYEE-BRANCH
           MOVE OC-PAYEE-ACCT-TYPE  TO OS-PAYEE-ACCT-TYPE
           MOVE OC-PAYEE-ACCT-NO    TO OS-PAYEE-ACCT-NO
      *    受取人カナは送信前に標準表記へそろえる
           MOVE OC-PAYEE-NAME-KANA  TO LK-RAW-KANA
           PERFORM 140-NORMALIZE-KANA
           MOVE LK-NORM-KANA        TO OS-PAYEE-NAME-KANA
           MOVE WS-NET-SEND-AMT     TO OS-SEND-AMT
      *    明細の改ざん検知用署名を作成する
           MOVE OC-PAYEE-BRANCH     TO LK-SIG-BRANCH
           MOVE WS-SEND-DT          TO LK-SIG-DT
           MOVE OC-ORIG-CENTER-SEQ  TO LK-SIG-ORIG-SEQ
           MOVE WS-NET-SEND-AMT     TO LK-SIG-AMT
           PERFORM 260-CREATE-SIGNATURE
           MOVE LK-SIGNATURE        TO OS-SIGNATURE
           MOVE WS-PROGRAM-ID       TO OS-BUILD-PROGRAM
           EXIT PARAGRAPH.
       260-CREATE-SIGNATURE.
      *    署名作成用の項目を送信レコードから受け渡す
           MOVE OS-SENDER-BRANCH    TO LK-SIG-BRANCH
           MOVE OS-SEND-DT          TO LK-SIG-DT
           MOVE OC-ORIG-CENTER-SEQ  TO LK-SIG-ORIG-SEQ
           MOVE OS-SEND-AMT         TO LK-SIG-AMT
           CALL 'TG931S' USING LK-SIG-PARM
           MOVE LK-SIGNATURE        TO OS-SIGNATURE
           EXIT PARAGRAPH.
       270-WRITE-TGOUTSF.
      *    送信ファイルへ編成済レコードを書き込む
           WRITE TGOUTSF-REC
           END-WRITE
           EVALUATE WS-FILE-STATUS-TGOUTSF
               WHEN "00"
                   ADD 1 TO WS-SEND-SEQ
                   ADD 1 TO WS-WRITE-SEND-COUNT
               WHEN OTHER
                   MOVE "TGOUTSF"               TO WS-ERROR-FIELD
                   MOVE "WRITE"                 TO WS-ERROR-CODE
                    MOVE "送信ファイル書込異常"
                    TO WS-ERROR-MESSAGE
                   MOVE 12                      TO WS-RETURN-CODE
                   MOVE "1"                     TO WS-ABEND-SW
                   PERFORM 310-HANDLE-IO-ERROR
                   PERFORM 320-ABEND-ROUTINE
           END-EVALUATE
           EXIT PARAGRAPH.
       280-ACCUMULATE-GROUP.
      *    初回は現在キーを前回キーとして保持する
           IF WS-GROUP-FIRST-SW = '1'
              MOVE WS-CURRENT-GROUP-KEY TO WS-PREV-GROUP-KEY
              MOVE '0'                  TO WS-GROUP-FIRST-SW
           END-IF
      *    送信単位キーが変わった場合は前単位の精算仕訳を出力する
           IF WS-CURRENT-GROUP-KEY NOT = WS-PREV-GROUP-KEY
              IF WS-GROUP-ITEM-COUNT > ZERO
                 PERFORM 290-WRITE-CLEARING-JOURNAL
              END-IF
              MOVE ZERO                 TO WS-GROUP-ITEM-COUNT
              MOVE ZERO                 TO WS-GROUP-TOTAL-AMT
              MOVE WS-CURRENT-GROUP-KEY TO WS-PREV-GROUP-KEY
           END-IF
      *    現レコードを送信単位および日次集計へ加算する
           ADD 1              TO WS-GROUP-ITEM-COUNT
           ADD OS-SEND-AMT    TO WS-GROUP-TOTAL-AMT
           ADD 1              TO WS-DAY-ITEM-COUNT
           ADD OS-SEND-AMT    TO WS-DAY-TOTAL-AMT
           EXIT PARAGRAPH.
       290-WRITE-CLEARING-JOURNAL.
      *    清算単位の集計結果をジャーナルへ記録する
           INITIALIZE TGCLJNF-REC
           MOVE OC-CORRECTION-DT       TO CJ-CLEARING-DT
           MOVE OC-ORIG-CENTER-SEQ     TO CJ-CENTER-SEQ
           MOVE OC-CORRECTION-TYPE     TO CJ-DIRECTION
           MOVE OC-SENDER-BANK         TO CJ-BANK-CD
           MOVE OC-SENDER-BRANCH       TO CJ-BRANCH-CD
           MOVE WS-GROUP-ITEM-COUNT    TO CJ-ITEM-COUNT
           MOVE WS-GROUP-TOTAL-AMT     TO CJ-TOTAL-AMT
           MOVE WS-MATCH-STATUS        TO CJ-MATCH-STATUS
           MOVE WS-RUN-TIME            TO CJ-JOURNAL-TS
           WRITE TGCLJNF-REC
           IF WS-FILE-STATUS-TGCLJNF = '00'
              ADD 1                    TO WS-WRITE-JNL-COUNT
           ELSE
              MOVE 'TGCLJNF'           TO WS-ERROR-FIELD
              MOVE '清算ジャーナル書込エラー'
                                       TO WS-ERROR-MESSAGE
              PERFORM 310-HANDLE-IO-ERROR
           END-IF
           EXIT PARAGRAPH.
       300-HANDLE-VALIDATION-ERROR.
      *    検証エラーは送信対象外とし、件数と理由を記録する。
           EVALUATE TRUE
             WHEN WS-VALID-SW = 'N'
               ADD 1 TO WS-ERROR-COUNT
               ADD 1 TO WS-SKIP-COUNT
               IF WS-ERROR-CODE = SPACE
                 MOVE 'V001' TO WS-ERROR-CODE
               END-IF
               IF WS-ERROR-FIELD = SPACE
                 MOVE 'TGOUTCF' TO WS-ERROR-FIELD
               END-IF
                MOVE '検証エラーのため送信対象外'
                TO WS-ERROR-MESSAGE
      *      照合ＮＧは送信を継続し、後続で識別できる状態にする。
             WHEN WS-MATCH-STATUS = 'NG'
               ADD 1 TO WS-ERROR-COUNT
               MOVE 'M001' TO WS-ERROR-CODE
               MOVE 'KZACCTF' TO WS-ERROR-FIELD
                MOVE '口座照合ＮＧとして送信継続'
                TO WS-ERROR-MESSAGE
                MOVE 'Y' TO WS-VALID-SW
             WHEN OTHER
               CONTINUE
           END-EVALUATE
           EXIT PARAGRAPH.
       310-HANDLE-IO-ERROR.
      *    ファイル状態を確認し、継続可能な未検出を分離する
           EVALUATE TRUE
               WHEN WS-FILE-STATUS-KZACCTF = '23'
                   ADD 1 TO WS-SKIP-COUNT
                   MOVE 'N'        TO WS-VALID-SW
                   MOVE '2'        TO WS-MATCH-STATUS
                   MOVE 'KZACCTF'  TO WS-ERROR-FIELD
                   MOVE 'ACCT-NOT-FOUND'
                                   TO WS-ERROR-CODE
                    MOVE '口座マスタ未検出のため
      -             '送信対象外'
                                    TO WS-ERROR-MESSAGE
                   EXIT PARAGRAPH
               WHEN WS-FILE-STATUS-TGOUTCF = '10'
                   MOVE '1'        TO WS-EOF-SW
                   EXIT PARAGRAPH
               WHEN WS-FILE-STATUS-TGOUTCF NOT = '00'
                   MOVE 'TGOUTCF'  TO WS-ERROR-FIELD
                   MOVE WS-FILE-STATUS-TGOUTCF
                                   TO WS-ERROR-CODE
               WHEN WS-FILE-STATUS-KZACCTF NOT = '00'
                   MOVE 'KZACCTF'  TO WS-ERROR-FIELD
                   MOVE WS-FILE-STATUS-KZACCTF
                                   TO WS-ERROR-CODE
               WHEN WS-FILE-STATUS-TGOUTSF NOT = '00'
                   MOVE 'TGOUTSF'  TO WS-ERROR-FIELD
                   MOVE WS-FILE-STATUS-TGOUTSF
                                   TO WS-ERROR-CODE
               WHEN WS-FILE-STATUS-TGCLJNF NOT = '00'
                   MOVE 'TGCLJNF'  TO WS-ERROR-FIELD
                   MOVE WS-FILE-STATUS-TGCLJNF
                                   TO WS-ERROR-CODE
               WHEN OTHER
                   EXIT PARAGRAPH
           END-EVALUATE
           ADD 1 TO WS-ERROR-COUNT
           MOVE '1'                TO WS-ABEND-SW
           MOVE 12                 TO WS-RETURN-CODE
           MOVE 'FILE-STATUS-ERROR'
                                   TO WS-ERROR-MESSAGE
           PERFORM 320-ABEND-ROUTINE
           EXIT PARAGRAPH.
       320-ABEND-ROUTINE.
      *    異常終了情報を共通領域へ退避する。
           MOVE '1'                 TO WS-ABEND-SW
           MOVE 12                  TO WS-RETURN-CODE
           MOVE WS-RETURN-CODE      TO RETURN-CODE
           IF WS-ERROR-CODE = SPACE
              MOVE 'E320'           TO WS-ERROR-CODE
           END-IF
           MOVE WS-CURRENT-GROUP-KEY
                                      TO WS-ERROR-FIELD
           EVALUATE TRUE
              WHEN WS-FILE-STATUS-TGOUTCF NOT = '00'
                 MOVE WS-FILE-STATUS-TGOUTCF
                                      TO WS-ERROR-MESSAGE
              WHEN WS-FILE-STATUS-KZACCTF NOT = '00'
                 MOVE WS-FILE-STATUS-KZACCTF
                                      TO WS-ERROR-MESSAGE
              WHEN WS-FILE-STATUS-TGOUTSF NOT = '00'
                 MOVE WS-FILE-STATUS-TGOUTSF
                                      TO WS-ERROR-MESSAGE
              WHEN WS-FILE-STATUS-TGCLJNF NOT = '00'
                 MOVE WS-FILE-STATUS-TGCLJNF
                                      TO WS-ERROR-MESSAGE
              WHEN OTHER
                 MOVE 'ABEND'       TO WS-ERROR-MESSAGE
           END-EVALUATE
           EXIT PARAGRAPH.
       900-FINAL-REPORT.
      *    実行結果をログへ出力する
           DISPLAY 'TG101B 仕向送金送信ファイル編成 終了'
           DISPLAY 'プログラムID=' WS-PROGRAM-ID
           DISPLAY '実行日=' WS-RUN-DATE
                   ' 実行時刻=' WS-RUN-TIME
           DISPLAY '送信日=' WS-SEND-DT
           DISPLAY '入力件数=' WS-READ-COUNT
           DISPLAY '選別件数=' WS-SELECT-COUNT
           DISPLAY 'スキップ件数=' WS-SKIP-COUNT
           DISPLAY '送信出力件数=' WS-WRITE-SEND-COUNT
           DISPLAY 'ジャーナル件数=' WS-WRITE-JNL-COUNT
           DISPLAY 'エラー件数=' WS-ERROR-COUNT
           DISPLAY '日次合計件数=' WS-DAY-ITEM-COUNT
           DISPLAY '日次合計金額=' WS-DAY-TOTAL-AMT
           IF WS-ABEND-SW NOT = SPACE
              DISPLAY '異常終了区分=' WS-ABEND-SW
              DISPLAY '戻りコード=' WS-RETURN-CODE
              DISPLAY 'エラーコード=' WS-ERROR-CODE
              DISPLAY 'エラー項目=' WS-ERROR-FIELD
              DISPLAY 'エラー内容=' WS-ERROR-MESSAGE
           ELSE
              DISPLAY '戻りコード=' WS-RETURN-CODE
           END-IF
            DISPLAY 'TG101B 仕向送金送信ファイル編成 
      -      'ログ出力完了'
            EXIT PARAGRAPH.
