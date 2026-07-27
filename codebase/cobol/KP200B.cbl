       IDENTIFICATION DIVISION.
       PROGRAM-ID. KP200B.
      *---------------------------------------------------------------*
      * 変更履歴                                                      *
      * 版数  年月日      担当                         概要           *
      * 1.00  平成31.04.01 システム部 勘定系チーム     新規作成       *
      * 1.10  令和03.10.15 システム部 勘定系チーム     判定見直し     *
      * 1.20  令和06.06.01 システム部 勘定系チーム     表示文言整備   *
      *---------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZSTMTF ASSIGN TO "KZSTMTF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS-KZSTMTF.
           SELECT KZTRANF ASSIGN TO "KZTRANF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS-KZTRANF.
           SELECT KZACCTF ASSIGN TO "KZACCTF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS AC-ACCT-ID
               FILE STATUS IS WS-FILE-STATUS-KZACCTF.
           SELECT KPDELINF ASSIGN TO "KPDELINF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS DL-ACCT-ID
               FILE STATUS IS WS-FILE-STATUS-KPDELINF.
       DATA DIVISION.
       FILE SECTION.
       FD KZSTMTF.
       COPY KZSTMTFC.
       FD KZTRANF.
       COPY KZTRANC.
       FD KZACCTF.
       COPY KZACCTC.
       FD KPDELINF.
       COPY KPDELINFC.
       WORKING-STORAGE SECTION.
       01  WS-RUN-DATE        PIC 9(08).
       01  WS-BATCH-ID        PIC X(12).
       01  WS-EOF-STMT        PIC X VALUE 'N'.
       01  WS-EOF-TRAN        PIC X VALUE 'N'.
       01  WS-FATAL-SW        PIC X VALUE 'N'.
       01  WS-CURRENT-ACCT-ID PIC X(16).
       01  WS-CURRENT-STMT-DT PIC 9(08).
       01  WS-PREV-ACCT-ID    PIC X(16).
       01  WS-PREV-STMT-DT    PIC 9(08).
       01  WS-TRAN-HOLD-ACCT-ID PIC X(16).
       01  WS-TRAN-HOLD-CD    PIC X(04).
       01  WS-TRAN-HOLD-AMT   PIC S9(13)V99 COMP-3.
       01  WS-STMT-DUE-AMT    PIC S9(13)V99 COMP-3.
       01  WS-PAYMENT-AMT     PIC S9(13)V99 COMP-3.
       01  WS-UNPAID-AMT      PIC S9(13)V99 COMP-3.
       01  WS-DAYS-PAST-DUE   PIC S9(05) COMP-3.
       01  WS-GROUP-RATE-CD   PIC X(03).
       01  WS-PENALTY-RATE    PIC S9(03)V9(05) COMP-3.
       01  WS-RAW-FEE-AMT     PIC S9(13)V99 COMP-3.
       01  WS-FLOOR-FEE-AMT   PIC S9(13)V99 COMP-3.
       01  WS-CAPPED-FEE-AMT  PIC S9(13)V99 COMP-3.
       01  WS-CYCLE-DUE-COUNT PIC 9(04) COMP.
       01  WS-ACCT-DUE-TOTAL  PIC S9(15)V99 COMP-3.
       01  WS-ACCT-FEE-TOTAL  PIC S9(15)V99 COMP-3.
       01  WS-ACCT-FIRST-DUE-DT PIC 9(08).
       01  WS-ACCT-LAST-DUE-DT PIC 9(08).
       01  WS-READ-STMT-CNT   PIC 9(09) COMP-3.
       01  WS-READ-TRAN-CNT   PIC 9(09) COMP-3.
       01  WS-READ-ACCT-CNT   PIC 9(09) COMP-3.
       01  WS-WRITE-DLINF-CNT PIC 9(09) COMP-3.
       01  WS-UPDATE-DLINF-CNT PIC 9(09) COMP-3.
       01  WS-SKIP-CNT        PIC 9(09) COMP-3.
       01  WS-ERROR-CNT       PIC 9(09) COMP-3.
       01  WS-FILE-STATUS-KZACCTF PIC XX.
       01  WS-FILE-STATUS-KZSTMTF PIC XX.
       01  WS-FILE-STATUS-KZTRANF PIC XX.
       01  WS-FILE-STATUS-KPDELINF PIC XX.
       01  WS-ERROR-CODE      PIC X(08).
       01  WS-ERROR-MSG       PIC X(80).
       01  WS-KP201S-FUNC     PIC X(02).
       01  WS-KP201S-ACCT-ID  PIC X(16).
       01  WS-KP201S-STMT-DT  PIC 9(08).
       01  WS-KP201S-OLD-DAYS-PD PIC S9(05) COMP-3.
       01  WS-KP201S-OLD-DUE-AMT PIC S9(13)V99 COMP-3.
       01  WS-KP201S-NEW-DAYS-PD PIC S9(05) COMP-3.
       01  WS-KP201S-NEW-DUE-AMT PIC S9(13)V99 COMP-3.
       01  WS-KP201S-STATE-IN PIC X(01).
       01  WS-KP201S-STATE-OUT PIC X(01).
       01  WS-KP201S-RETURN-CD PIC X(02).

       PROCEDURE DIVISION.
       000-MAIN.
      *    主制御を実行し、重大エラー時は異常終了へ進む
           PERFORM 010-INITIALIZE
           IF WS-FATAL-SW = '1'
              PERFORM 990-ABEND-ROUTINE
              EXIT PARAGRAPH
           END-IF
           PERFORM 020-OPEN-FILES
           IF WS-FATAL-SW = '1'
              PERFORM 990-ABEND-ROUTINE
              EXIT PARAGRAPH
           END-IF
           PERFORM 030-PRIME-READS
           IF WS-FATAL-SW = '1'
              PERFORM 990-ABEND-ROUTINE
              EXIT PARAGRAPH
           END-IF
           PERFORM 100-STATEMENT-DRIVER
           IF WS-FATAL-SW = '1'
              PERFORM 990-ABEND-ROUTINE
              EXIT PARAGRAPH
           END-IF
           PERFORM 900-CLOSE-FILES
           IF WS-FATAL-SW = '1'
              PERFORM 990-ABEND-ROUTINE
              EXIT PARAGRAPH
           END-IF
           PERFORM 999-RETURN
           EXIT PARAGRAPH.
       010-INITIALIZE.
      *    実行管理項目を初期化する。
           ACCEPT WS-RUN-DATE FROM DATE
           MOVE 'KP200B' TO WS-BATCH-ID
      *    入力終端および異常状態を開始状態に戻す。
           MOVE 'N' TO WS-EOF-STMT
           MOVE 'N' TO WS-EOF-TRAN
           MOVE 'N' TO WS-FATAL-SW
      *    口座、明細、取引の処理中キーを初期化する。
           INITIALIZE WS-CURRENT-ACCT-ID
                      WS-CURRENT-STMT-DT
                      WS-PREV-ACCT-ID
                      WS-PREV-STMT-DT
                      WS-TRAN-HOLD-ACCT-ID
                      WS-TRAN-HOLD-CD
                      WS-TRAN-HOLD-AMT
      *    延滞判定用の金額、日数、料率ワークを初期化する。
           INITIALIZE WS-STMT-DUE-AMT
                      WS-PAYMENT-AMT
                      WS-UNPAID-AMT
                      WS-DAYS-PAST-DUE
                      WS-GROUP-RATE-CD
                      WS-PENALTY-RATE
                      WS-RAW-FEE-AMT
                      WS-FLOOR-FEE-AMT
                      WS-CAPPED-FEE-AMT
      *    口座単位の累積ワークを初期化する。
           INITIALIZE WS-CYCLE-DUE-COUNT
                      WS-ACCT-DUE-TOTAL
                      WS-ACCT-FEE-TOTAL
                      WS-ACCT-FIRST-DUE-DT
                      WS-ACCT-LAST-DUE-DT
      *    件数カウンタを初期化する。
           INITIALIZE WS-READ-STMT-CNT
                      WS-READ-TRAN-CNT
                      WS-READ-ACCT-CNT
                      WS-WRITE-DLINF-CNT
                      WS-UPDATE-DLINF-CNT
                      WS-SKIP-CNT
                      WS-ERROR-CNT
      *    ファイル状態およびエラー情報を初期化する。
           INITIALIZE WS-FILE-STATUS-KZACCTF
                      WS-FILE-STATUS-KZSTMTF
                      WS-FILE-STATUS-KZTRANF
                      WS-FILE-STATUS-KPDELINF
                      WS-ERROR-CODE
                      WS-ERROR-MSG
      *    KP201S連携領域を初期化する。
           INITIALIZE WS-KP201S-FUNC
                      WS-KP201S-ACCT-ID
                      WS-KP201S-STMT-DT
                      WS-KP201S-OLD-DAYS-PD
                      WS-KP201S-OLD-DUE-AMT
                      WS-KP201S-NEW-DAYS-PD
                      WS-KP201S-NEW-DUE-AMT
                      WS-KP201S-STATE-IN
                      WS-KP201S-STATE-OUT
                      WS-KP201S-RETURN-CD
           EXIT PARAGRAPH.
       020-OPEN-FILES.
      *    入力三ファイルと延滞出力を用途別に開く
           OPEN INPUT  KZSTMTF
                       KZTRANF
                       KZACCTF
                I-O    KPDELINF
      *    明細ファイルのオープン状態を確認する
           IF WS-FILE-STATUS-KZSTMTF NOT = "00"
              MOVE "Y"              TO WS-FATAL-SW
              MOVE "E0201"          TO WS-ERROR-CODE
              MOVE "明細ファイルOPEN失敗" TO WS-ERROR-MSG
              ADD 1                 TO WS-ERROR-CNT
           END-IF
      *    取引ファイルのオープン状態を確認する
           IF WS-FILE-STATUS-KZTRANF NOT = "00"
              MOVE "Y"              TO WS-FATAL-SW
              MOVE "E0202"          TO WS-ERROR-CODE
              MOVE "取引ファイルOPEN失敗" TO WS-ERROR-MSG
              ADD 1                 TO WS-ERROR-CNT
           END-IF
      *    口座ファイルのオープン状態を確認する
           IF WS-FILE-STATUS-KZACCTF NOT = "00"
              MOVE "Y"              TO WS-FATAL-SW
              MOVE "E0203"          TO WS-ERROR-CODE
              MOVE "口座ファイルOPEN失敗" TO WS-ERROR-MSG
              ADD 1                 TO WS-ERROR-CNT
           END-IF
      *    延滞ファイルのオープン状態を確認する
           IF WS-FILE-STATUS-KPDELINF NOT = "00"
              MOVE "Y"              TO WS-FATAL-SW
              MOVE "E0204"          TO WS-ERROR-CODE
              MOVE "延滞ファイルOPEN失敗" TO WS-ERROR-MSG
              ADD 1                 TO WS-ERROR-CNT
           END-IF
           EXIT PARAGRAPH.
       030-PRIME-READS.
      *    明細ファイルの先頭を読み、処理中の締日キーを保持する
           READ KZSTMTF
           END-READ
           EVALUATE WS-FILE-STATUS-KZSTMTF
               WHEN '00'
                   ADD 1 TO WS-READ-STMT-CNT
                   MOVE ST-ACCT-ID  TO WS-CURRENT-ACCT-ID
                   MOVE ST-STMT-DT   TO WS-CURRENT-STMT-DT
                   MOVE ST-ACCT-ID  TO WS-PREV-ACCT-ID
                   MOVE ST-STMT-DT   TO WS-PREV-STMT-DT
               WHEN '10'
                   MOVE 'Y' TO WS-EOF-STMT
               WHEN OTHER
                   MOVE 'Y' TO WS-FATAL-SW
                   MOVE '0301' TO WS-ERROR-CODE
                   MOVE 'KZSTMTF 初回読込エラー' TO WS-ERROR-MSG
                   PERFORM 990-ABEND-ROUTINE
           END-EVALUATE
      *    入金取引の先頭を読み、突合用の保留域へ退避する
           READ KZTRANF
           END-READ
           EVALUATE WS-FILE-STATUS-KZTRANF
               WHEN '00'
                   ADD 1 TO WS-READ-TRAN-CNT
                   MOVE TR-ACCT-ID  TO WS-TRAN-HOLD-ACCT-ID
                   MOVE TR-TRAN-CD  TO WS-TRAN-HOLD-CD
                   MOVE TR-TRAN-AMT TO WS-TRAN-HOLD-AMT
               WHEN '10'
                   MOVE 'Y' TO WS-EOF-TRAN
               WHEN OTHER
                   MOVE 'Y' TO WS-FATAL-SW
                   MOVE '0302' TO WS-ERROR-CODE
                   MOVE 'KZTRANF 初回読込エラー' TO WS-ERROR-MSG
                   PERFORM 990-ABEND-ROUTINE
           END-EVALUATE
           EXIT PARAGRAPH.
       100-STATEMENT-DRIVER.
      *    明細EOFまで口座単位に延滞候補を束ねる
           IF WS-EOF-STMT = 'Y'
              EXIT PARAGRAPH
           END-IF
           MOVE ST-ACCT-ID  TO WS-CURRENT-ACCT-ID
           MOVE ST-STMT-DT   TO WS-CURRENT-STMT-DT
           MOVE SPACES       TO WS-PREV-ACCT-ID
           MOVE ZERO         TO WS-PREV-STMT-DT
           PERFORM 110-START-ACCOUNT-GROUP
           PERFORM UNTIL WS-EOF-STMT = 'Y'
              OR WS-FATAL-SW = 'Y'
              MOVE ST-ACCT-ID TO WS-CURRENT-ACCT-ID
              MOVE ST-STMT-DT  TO WS-CURRENT-STMT-DT
              IF WS-PREV-ACCT-ID NOT = SPACES
                 IF ST-ACCT-ID NOT = WS-PREV-ACCT-ID
                    PERFORM 310-END-ACCOUNT-GROUP
                    IF WS-FATAL-SW NOT = 'Y'
                       PERFORM 110-START-ACCOUNT-GROUP
                    END-IF
                 END-IF
              END-IF
              IF WS-FATAL-SW NOT = 'Y'
                 PERFORM 120-VALIDATE-STATEMENT
              END-IF
              IF WS-FATAL-SW NOT = 'Y'
                 PERFORM 130-VALIDATE-ACCOUNT
              END-IF
              IF WS-FATAL-SW NOT = 'Y'
                 PERFORM 140-DISPATCH-PRODUCT-VARIANT
              END-IF
              IF WS-FATAL-SW NOT = 'Y'
                 MOVE ST-ACCT-ID TO WS-PREV-ACCT-ID
                 MOVE ST-STMT-DT  TO WS-PREV-STMT-DT
                 READ KZSTMTF NEXT RECORD
                    AT END
                       MOVE 'Y' TO WS-EOF-STMT
                    NOT AT END
                       ADD 1 TO WS-READ-STMT-CNT
                 END-READ
                 IF WS-EOF-STMT NOT = 'Y'
                    AND WS-FILE-STATUS-KZSTMTF NOT = '00'
                    MOVE 'KZSTMTF' TO WS-ERROR-CODE
                    MOVE '明細書読込で異常を検知' TO
                    WS-ERROR-MSG
                    PERFORM 800-HANDLE-NONFATAL-ERROR
                 END-IF
              END-IF
           END-PERFORM
           IF WS-PREV-ACCT-ID NOT = SPACES
              AND WS-FATAL-SW NOT = 'Y'
              PERFORM 310-END-ACCOUNT-GROUP
           END-IF
           EXIT PARAGRAPH.
       110-START-ACCOUNT-GROUP.
      *    新しい口座の累積ワークを初期化する。
           MOVE ST-ACCT-ID             TO WS-CURRENT-ACCT-ID
           MOVE ST-STMT-DT             TO WS-CURRENT-STMT-DT
           MOVE ZERO                   TO WS-STMT-DUE-AMT
           MOVE ZERO                   TO WS-PAYMENT-AMT
           MOVE ZERO                   TO WS-UNPAID-AMT
           MOVE ZERO                   TO WS-DAYS-PAST-DUE
           MOVE ZERO                   TO WS-RAW-FEE-AMT
           MOVE ZERO                   TO WS-FLOOR-FEE-AMT
           MOVE ZERO                   TO WS-CAPPED-FEE-AMT
           MOVE ZERO                   TO WS-CYCLE-DUE-COUNT
           MOVE ZERO                   TO WS-ACCT-DUE-TOTAL
           MOVE ZERO                   TO WS-ACCT-FEE-TOTAL
           MOVE ZERO                   TO WS-ACCT-FIRST-DUE-DT
           MOVE ZERO                   TO WS-ACCT-LAST-DUE-DT
           MOVE SPACE                  TO WS-GROUP-RATE-CD
           MOVE ZERO                   TO WS-PENALTY-RATE
      *    口座マスタを参照し、後続処理で使う属性を取得する。
           MOVE WS-CURRENT-ACCT-ID     TO AC-ACCT-ID
           READ KZACCTF
               KEY IS AC-ACCT-ID
               INVALID KEY
                   ADD 1               TO WS-SKIP-CNT
                   ADD 1               TO WS-ERROR-CNT
                   MOVE 'E1101'        TO WS-ERROR-CODE
                   MOVE '口座マスタ未登録' TO WS-ERROR-MSG
                   PERFORM 800-HANDLE-NONFATAL-ERROR
               NOT INVALID KEY
                   ADD 1               TO WS-READ-ACCT-CNT
           END-READ
           EXIT PARAGRAPH.
       120-VALIDATE-STATEMENT.
      *    明細の口座番号を検証する。
           IF ST-ACCT-ID = SPACES
              MOVE 'ST001' TO WS-ERROR-CODE
              MOVE '明細口座番号が未設定' TO WS-ERROR-MSG
              PERFORM 800-HANDLE-NONFATAL-ERROR
              EXIT PARAGRAPH
           END-IF
      *    明細日付を検証する。
           IF ST-STMT-DT NOT NUMERIC
              MOVE 'ST002' TO WS-ERROR-CODE
              MOVE '明細日付が数字でない' TO WS-ERROR-MSG
              PERFORM 800-HANDLE-NONFATAL-ERROR
              EXIT PARAGRAPH
           END-IF
           IF ST-STMT-DT = ZERO
              MOVE 'ST003' TO WS-ERROR-CODE
              MOVE '明細日付がゼロ' TO WS-ERROR-MSG
              PERFORM 800-HANDLE-NONFATAL-ERROR
              EXIT PARAGRAPH
           END-IF
           IF ST-STMT-DT > WS-RUN-DATE
              MOVE 'ST004' TO WS-ERROR-CODE
              MOVE '明細日付が未来日' TO WS-ERROR-MSG
              PERFORM 800-HANDLE-NONFATAL-ERROR
              EXIT PARAGRAPH
           END-IF
      *    残高項目を検証する。
           IF ST-OPEN-BAL NOT NUMERIC
              MOVE 'ST005' TO WS-ERROR-CODE
              MOVE '開始残高が数字でない' TO WS-ERROR-MSG
              PERFORM 800-HANDLE-NONFATAL-ERROR
              EXIT PARAGRAPH
           END-IF
           IF ST-CLOSE-BAL NOT NUMERIC
              MOVE 'ST006' TO WS-ERROR-CODE
              MOVE '終了残高が数字でない' TO WS-ERROR-MSG
              PERFORM 800-HANDLE-NONFATAL-ERROR
              EXIT PARAGRAPH
           END-IF
      *    手数料を検証する。
           IF ST-FEE-AMT NOT NUMERIC
              MOVE 'ST007' TO WS-ERROR-CODE
              MOVE '手数料金額が数字でない' TO WS-ERROR-MSG
              PERFORM 800-HANDLE-NONFATAL-ERROR
              EXIT PARAGRAPH
           END-IF
           IF ST-FEE-AMT < ZERO
              MOVE 'ST008' TO WS-ERROR-CODE
              MOVE '手数料金額がマイナス' TO WS-ERROR-MSG
              PERFORM 800-HANDLE-NONFATAL-ERROR
              EXIT PARAGRAPH
           END-IF
      *    利息を検証する。
           IF ST-INT-AMT NOT NUMERIC
              MOVE 'ST009' TO WS-ERROR-CODE
              MOVE '利息金額が数字でない' TO WS-ERROR-MSG
              PERFORM 800-HANDLE-NONFATAL-ERROR
              EXIT PARAGRAPH
           END-IF
           IF ST-INT-AMT < ZERO
              MOVE 'ST010' TO WS-ERROR-CODE
              MOVE '利息金額がマイナス' TO WS-ERROR-MSG
              PERFORM 800-HANDLE-NONFATAL-ERROR
              EXIT PARAGRAPH
           END-IF
      *    最低支払額を検証する。
           IF ST-MIN-PAY-AMT NOT NUMERIC
              MOVE 'ST011' TO WS-ERROR-CODE
              MOVE '最低支払額が数字でない' TO WS-ERROR-MSG
              PERFORM 800-HANDLE-NONFATAL-ERROR
              EXIT PARAGRAPH
           END-IF
           IF ST-MIN-PAY-AMT < ZERO
              MOVE 'ST012' TO WS-ERROR-CODE
              MOVE '最低支払額がマイナス' TO WS-ERROR-MSG
              PERFORM 800-HANDLE-NONFATAL-ERROR
              EXIT PARAGRAPH
           END-IF
      *    延滞抽出の対象外を判定する。
           IF ST-CLOSE-BAL <= ZERO
              ADD 1 TO WS-SKIP-CNT
              EXIT PARAGRAPH
           END-IF
           IF ST-MIN-PAY-AMT = ZERO
              ADD 1 TO WS-SKIP-CNT
              EXIT PARAGRAPH
           END-IF
      *    後続処理で使う明細キーを保持する。
           MOVE ST-ACCT-ID  TO WS-CURRENT-ACCT-ID
           MOVE ST-STMT-DT  TO WS-CURRENT-STMT-DT
           MOVE ZERO        TO WS-STMT-DUE-AMT
           MOVE ZERO        TO WS-PAYMENT-AMT
           MOVE ZERO        TO WS-UNPAID-AMT
           MOVE ZERO        TO WS-DAYS-PAST-DUE
           EXIT PARAGRAPH.
       130-VALIDATE-ACCOUNT.
      *    口座マスタの必須項目を確認する。
           IF AC-ACCT-ID = SPACES
              MOVE 'E13001' TO WS-ERROR-CODE
              MOVE '口座番号未設定' TO WS-ERROR-MSG
              ADD 1 TO WS-ERROR-CNT
              PERFORM 800-HANDLE-NONFATAL-ERROR
              EXIT PARAGRAPH
           END-IF
           IF AC-CARD-NO = SPACES
              MOVE 'E13002' TO WS-ERROR-CODE
              MOVE 'カード番号未設定' TO WS-ERROR-MSG
              ADD 1 TO WS-ERROR-CNT
              PERFORM 800-HANDLE-NONFATAL-ERROR
              EXIT PARAGRAPH
           END-IF
           IF AC-GROUP-CODE = SPACES
              MOVE 'E13003' TO WS-ERROR-CODE
              MOVE '商品グループ未設定' TO WS-ERROR-MSG
              ADD 1 TO WS-ERROR-CNT
              PERFORM 800-HANDLE-NONFATAL-ERROR
              EXIT PARAGRAPH
           END-IF
           IF AC-CYCLE-BAL NOT NUMERIC
              MOVE 'E13004' TO WS-ERROR-CODE
              MOVE 'サイクル残高不正' TO WS-ERROR-MSG
              ADD 1 TO WS-ERROR-CNT
              PERFORM 800-HANDLE-NONFATAL-ERROR
              EXIT PARAGRAPH
           END-IF
           IF AC-CREDIT-LIMIT NOT NUMERIC
              MOVE 'E13005' TO WS-ERROR-CODE
              MOVE '与信限度額不正' TO WS-ERROR-MSG
              ADD 1 TO WS-ERROR-CNT
              PERFORM 800-HANDLE-NONFATAL-ERROR
              EXIT PARAGRAPH
           END-IF
           IF AC-OVER-AMT NOT NUMERIC
              MOVE 'E13006' TO WS-ERROR-CODE
              MOVE '超過額不正' TO WS-ERROR-MSG
              ADD 1 TO WS-ERROR-CNT
              PERFORM 800-HANDLE-NONFATAL-ERROR
              EXIT PARAGRAPH
           END-IF
           IF AC-CREDIT-LIMIT < ZERO
              MOVE 'E13007' TO WS-ERROR-CODE
              MOVE '与信限度額マイナス' TO WS-ERROR-MSG
              ADD 1 TO WS-ERROR-CNT
              PERFORM 800-HANDLE-NONFATAL-ERROR
              EXIT PARAGRAPH
           END-IF
           IF AC-OVER-AMT < ZERO
              MOVE 'E13008' TO WS-ERROR-CODE
              MOVE '超過額マイナス' TO WS-ERROR-MSG
              ADD 1 TO WS-ERROR-CNT
              PERFORM 800-HANDLE-NONFATAL-ERROR
              EXIT PARAGRAPH
           END-IF
           IF AC-OVER-AMT > AC-CYCLE-BAL
              MOVE 'E13009' TO WS-ERROR-CODE
              MOVE '超過額が残高超過' TO WS-ERROR-MSG
              ADD 1 TO WS-ERROR-CNT
              PERFORM 800-HANDLE-NONFATAL-ERROR
              EXIT PARAGRAPH
           END-IF
      *    凍結口座および本人確認不備は抽出対象外とする。
           EVALUATE AC-KYC-STATUS
              WHEN '1'
                 CONTINUE
              WHEN '8'
                 MOVE 'W13010' TO WS-ERROR-CODE
                 MOVE '凍結口座対象外' TO WS-ERROR-MSG
                 ADD 1 TO WS-SKIP-CNT
                 EXIT PARAGRAPH
              WHEN '0'
                 MOVE 'W13011' TO WS-ERROR-CODE
                 MOVE '本人確認未完了' TO WS-ERROR-MSG
                 ADD 1 TO WS-SKIP-CNT
                 EXIT PARAGRAPH
              WHEN '9'
                 MOVE 'W13012' TO WS-ERROR-CODE
                 MOVE '本人確認不備' TO WS-ERROR-MSG
                 ADD 1 TO WS-SKIP-CNT
                 EXIT PARAGRAPH
              WHEN OTHER
                 MOVE 'E13013' TO WS-ERROR-CODE
                 MOVE '本人確認区分不正' TO WS-ERROR-MSG
                 ADD 1 TO WS-ERROR-CNT
                 PERFORM 800-HANDLE-NONFATAL-ERROR
                 EXIT PARAGRAPH
           END-EVALUATE
           IF AC-CYCLE-BAL <= ZERO
              MOVE 'W13014' TO WS-ERROR-CODE
              MOVE '残高なし対象外' TO WS-ERROR-MSG
              ADD 1 TO WS-SKIP-CNT
              EXIT PARAGRAPH
           END-IF
           IF AC-GROUP-CODE = '999'
              MOVE 'W13015' TO WS-ERROR-CODE
              MOVE '業務除外グループ' TO WS-ERROR-MSG
              ADD 1 TO WS-SKIP-CNT
              EXIT PARAGRAPH
           END-IF
           .
       140-DISPATCH-PRODUCT-VARIANT.
      *    商品区分ごとの延滞判定経路を選ぶ。
           EVALUATE AC-GROUP-CODE
             WHEN 'TB'
             WHEN 'BK'
               MOVE 'T' TO WS-GROUP-RATE-CD
               PERFORM 170-HANDLE-TRUST-PRODUCT-ACCOUNT
             WHEN 'CP'
             WHEN 'CB'
               MOVE 'C' TO WS-GROUP-RATE-CD
               PERFORM 160-HANDLE-CORPORATE-ACCOUNT
             WHEN 'RV'
               MOVE 'R' TO WS-GROUP-RATE-CD
               PERFORM 150-HANDLE-STANDARD-ACCOUNT
             WHEN 'GR'
             WHEN 'YD'
      *        猶予対象は抽出せず、監査用に件数のみ加算する。
               ADD 1 TO WS-SKIP-CNT
             WHEN OTHER
               EVALUATE AC-CARD-NO(1:1)
                 WHEN '8'
                   MOVE 'R' TO WS-GROUP-RATE-CD
                 WHEN OTHER
                   MOVE 'S' TO WS-GROUP-RATE-CD
               END-EVALUATE
               PERFORM 150-HANDLE-STANDARD-ACCOUNT
           END-EVALUATE
           EXIT PARAGRAPH.
       150-HANDLE-STANDARD-ACCOUNT.
      *    通常個人口座は明細単位で延滞判定の基準額を作る
           MOVE ZERO                  TO WS-STMT-DUE-AMT
                                         WS-PAYMENT-AMT
                                         WS-UNPAID-AMT
                                         WS-DAYS-PAST-DUE
                                         WS-PENALTY-RATE
                                         WS-RAW-FEE-AMT
                                         WS-FLOOR-FEE-AMT
                                         WS-CAPPED-FEE-AMT
           PERFORM 180-CALCULATE-STMT-DUE
      *    当月請求がない明細は延滞対象外として集計しない
           IF WS-STMT-DUE-AMT <= ZERO
              EXIT PARAGRAPH
           END-IF
           PERFORM 190-MATCH-PAYMENT-TRANS
           PERFORM 210-CALCULATE-UNPAID
      *    入金充当後の未収がなければ延滞対象外
           IF WS-UNPAID-AMT <= ZERO
              EXIT PARAGRAPH
           END-IF
           PERFORM 220-CALCULATE-DAYS-PAST-DUE
      *    支払期日前または当日は遅延損害金を発生させない
           IF WS-DAYS-PAST-DUE <= ZERO
              EXIT PARAGRAPH
           END-IF
           PERFORM 230-RATE-LOOKUP
           PERFORM 240-CALCULATE-FEE
           PERFORM 250-APPLY-FLOOR
           PERFORM 260-APPLY-CAP
           PERFORM 270-CALL-KP201S
           PERFORM 280-ACCUMULATE-ACCOUNT
           .
       160-HANDLE-CORPORATE-ACCOUNT.
      *    法人親口座は部門単位の残高を親口座へ寄せて判定する
            PERFORM 180-CALCULATE-STMT-DUE
            PERFORM 190-MATCH-PAYMENT-TRANS
            PERFORM 210-CALCULATE-UNPAID
            IF AC-OVER-AMT > ZERO
               ADD AC-OVER-AMT TO WS-UNPAID-AMT
            END-IF
      *    親口座の与信上限を超えた部分だけを延滞対象に残す
            IF AC-CREDIT-LIMIT > ZERO
               IF WS-UNPAID-AMT > AC-CREDIT-LIMIT
                  SUBTRACT AC-CREDIT-LIMIT FROM WS-UNPAID-AMT
               ELSE
                  MOVE ZERO TO WS-UNPAID-AMT
               END-IF
            END-IF
            IF WS-UNPAID-AMT <= ZERO
               ADD 1 TO WS-SKIP-CNT
               EXIT PARAGRAPH
            END-IF
            PERFORM 220-CALCULATE-DAYS-PAST-DUE
      *    特例審査中の法人は一回の締め期間だけ督促を猶予する
            IF AC-KYC-STATUS = 'R'
               IF WS-DAYS-PAST-DUE <= 30
                  MOVE ZERO TO WS-DAYS-PAST-DUE
                  ADD 1 TO WS-SKIP-CNT
                  EXIT PARAGRAPH
               END-IF
            END-IF
            PERFORM 230-RATE-LOOKUP
            PERFORM 240-CALCULATE-FEE
            PERFORM 250-APPLY-FLOOR
            PERFORM 260-APPLY-CAP
      *    部門コード別の未収額を親口座集計へ反映する
            PERFORM 280-ACCUMULATE-ACCOUNT
            PERFORM 270-CALL-KP201S
            PERFORM 290-BUILD-DELINQUENCY-RECORD
            PERFORM 300-WRITE-OR-UPDATE-KPDELINF
            EXIT PARAGRAPH.
       170-HANDLE-TRUST-PRODUCT-ACCOUNT.
      *    信託提携商品は優遇期間後の実延滞のみ対象とする
           PERFORM 180-CALCULATE-STMT-DUE
           PERFORM 190-MATCH-PAYMENT-TRANS
           PERFORM 210-CALCULATE-UNPAID
      *    信託口座固有の手数料費目は延滞元本から除外する
           IF WS-UNPAID-AMT > ZERO
              IF WS-TRAN-HOLD-ACCT-ID = WS-CURRENT-ACCT-ID
                 EVALUATE WS-TRAN-HOLD-CD
                    WHEN 'TG'
                    WHEN 'TS'
                       COMPUTE WS-UNPAID-AMT =
                               WS-UNPAID-AMT - WS-TRAN-HOLD-AMT
                       IF WS-UNPAID-AMT < ZERO
                          MOVE ZERO TO WS-UNPAID-AMT
                       END-IF
                 END-EVALUATE
              END-IF
           END-IF
           IF WS-UNPAID-AMT > ZERO
              PERFORM 220-CALCULATE-DAYS-PAST-DUE
      *       信託提携商品の優遇期間を延滞日数から控除する
              IF WS-DAYS-PAST-DUE > 10
                 SUBTRACT 10 FROM WS-DAYS-PAST-DUE
              ELSE
                 MOVE ZERO TO WS-DAYS-PAST-DUE
              END-IF
              IF WS-DAYS-PAST-DUE > ZERO
                 PERFORM 230-RATE-LOOKUP
                 PERFORM 240-CALCULATE-FEE
      *          保証料相当は信託商品の料率上限前に加算する
                 IF ST-FEE-AMT > ZERO
                    COMPUTE WS-RAW-FEE-AMT =
                            WS-RAW-FEE-AMT + ST-FEE-AMT
                 END-IF
                 PERFORM 250-APPLY-FLOOR
                 PERFORM 260-APPLY-CAP
                 PERFORM 270-CALL-KP201S
                 PERFORM 280-ACCUMULATE-ACCOUNT
              ELSE
                 ADD 1 TO WS-SKIP-CNT
              END-IF
           ELSE
              ADD 1 TO WS-SKIP-CNT
           END-IF
           EXIT PARAGRAPH.
       180-CALCULATE-STMT-DUE.
      *    締日時点で残高がなければ請求対象外とする。
           MOVE ZERO               TO WS-STMT-DUE-AMT
           IF ST-CLOSE-BAL > ZERO
      *        約定請求額に手数料、利息、超過額を加算する。
               ADD ST-MIN-PAY-AMT  TO WS-STMT-DUE-AMT
               ADD ST-FEE-AMT      TO WS-STMT-DUE-AMT
               ADD ST-INT-AMT      TO WS-STMT-DUE-AMT
               ADD AC-OVER-AMT     TO WS-STMT-DUE-AMT
      *        返金調整などでマイナスになった場合は請求しない。
               IF WS-STMT-DUE-AMT < ZERO
                   MOVE ZERO       TO WS-STMT-DUE-AMT
               END-IF
      *        請求対象額は締日時点残高を上限とする。
               IF WS-STMT-DUE-AMT > ST-CLOSE-BAL
                   MOVE ST-CLOSE-BAL
                                      TO WS-STMT-DUE-AMT
               END-IF
           END-IF
           EXIT PARAGRAPH.
       190-MATCH-PAYMENT-TRANS.
      *    対象口座まで取引ファイルを進める
           MOVE ZERO               TO WS-PAYMENT-AMT
           PERFORM UNTIL WS-EOF-TRAN = 'Y'
              OR WS-TRAN-HOLD-ACCT-ID >= WS-CURRENT-ACCT-ID
              ADD 1                 TO WS-SKIP-CNT
              READ KZTRANF
                 AT END
                    MOVE 'Y'        TO WS-EOF-TRAN
                 NOT AT END
                    ADD 1           TO WS-READ-TRAN-CNT
                    MOVE TR-ACCT-ID TO WS-TRAN-HOLD-ACCT-ID
                    MOVE TR-TRAN-CD TO WS-TRAN-HOLD-CD
                    MOVE TR-TRAN-AMT
                                      TO WS-TRAN-HOLD-AMT
              END-READ
           END-PERFORM
      *    未来口座は次明細側で処理するため保持する
           IF WS-EOF-TRAN = 'Y'
              EXIT PARAGRAPH
           END-IF
           IF WS-TRAN-HOLD-ACCT-ID > WS-CURRENT-ACCT-ID
              EXIT PARAGRAPH
           END-IF
      *    同一口座の入金取引のみを対象明細へ充当する
           PERFORM UNTIL WS-EOF-TRAN = 'Y'
              OR WS-TRAN-HOLD-ACCT-ID > WS-CURRENT-ACCT-ID
              IF WS-TRAN-HOLD-ACCT-ID < WS-CURRENT-ACCT-ID
                 ADD 1              TO WS-SKIP-CNT
              ELSE
                 EVALUATE WS-TRAN-HOLD-CD
                    WHEN '10'
                    WHEN '11'
                       ADD WS-TRAN-HOLD-AMT
                                      TO WS-PAYMENT-AMT
                    WHEN '20'
                    WHEN '30'
                       ADD 1         TO WS-SKIP-CNT
                    WHEN OTHER
                       MOVE 'E190'   TO WS-ERROR-CODE
                       MOVE 'TRAN-CD 不明' TO WS-ERROR-MSG
                       ADD 1         TO WS-ERROR-CNT
                       PERFORM 800-HANDLE-NONFATAL-ERROR
                 END-EVALUATE
              END-IF
              READ KZTRANF
                 AT END
                    MOVE 'Y'        TO WS-EOF-TRAN
                 NOT AT END
                    ADD 1           TO WS-READ-TRAN-CNT
                    MOVE TR-ACCT-ID TO WS-TRAN-HOLD-ACCT-ID
                    MOVE TR-TRAN-CD TO WS-TRAN-HOLD-CD
                    MOVE TR-TRAN-AMT
                                      TO WS-TRAN-HOLD-AMT
              END-READ
           END-PERFORM
           EXIT PARAGRAPH.
       200-VALIDATE-TRANSACTION.
      *    取引の必須項目を項目単位で確認する
           MOVE SPACES              TO WS-ERROR-CODE
           MOVE SPACES              TO WS-ERROR-MSG
           MOVE TR-ACCT-ID          TO WS-TRAN-HOLD-ACCT-ID
           MOVE TR-TRAN-CD          TO WS-TRAN-HOLD-CD
           MOVE ZERO                TO WS-TRAN-HOLD-AMT
           IF TR-ACCT-ID = SPACES
               MOVE 'TR001'         TO WS-ERROR-CODE
               MOVE '取引口座番号が未設定' TO WS-ERROR-MSG
               ADD 1                TO WS-SKIP-CNT
               ADD 1                TO WS-ERROR-CNT
               PERFORM 800-HANDLE-NONFATAL-ERROR
               EXIT PARAGRAPH
           END-IF
           IF TR-TRAN-CD = SPACES
               MOVE 'TR002'         TO WS-ERROR-CODE
               MOVE '取引種別が未設定' TO WS-ERROR-MSG
               ADD 1                TO WS-SKIP-CNT
               ADD 1                TO WS-ERROR-CNT
               PERFORM 800-HANDLE-NONFATAL-ERROR
               EXIT PARAGRAPH
           END-IF
           IF TR-TRAN-AMT NOT NUMERIC
               MOVE 'TR003'         TO WS-ERROR-CODE
               MOVE '取引金額が数字でない' TO WS-ERROR-MSG
               ADD 1                TO WS-SKIP-CNT
               ADD 1                TO WS-ERROR-CNT
               PERFORM 800-HANDLE-NONFATAL-ERROR
               EXIT PARAGRAPH
           END-IF
           IF TR-TRAN-AMT <= ZERO
               MOVE 'TR004'         TO WS-ERROR-CODE
               MOVE '取引金額が正数でない' TO WS-ERROR-MSG
               ADD 1                TO WS-SKIP-CNT
               ADD 1                TO WS-ERROR-CNT
               PERFORM 800-HANDLE-NONFATAL-ERROR
               EXIT PARAGRAPH
           END-IF
      *    後続処理が扱う業務符号へ正規化する
           EVALUATE TR-TRAN-CD
               WHEN '01'
               WHEN 'PAY'
               WHEN 'PMT'
               WHEN 'NYU'
                   MOVE 'P'         TO WS-TRAN-HOLD-CD
               WHEN '02'
               WHEN 'CAN'
               WHEN 'CXL'
                   MOVE 'C'         TO WS-TRAN-HOLD-CD
               WHEN '03'
               WHEN 'REV'
               WHEN 'RET'
                   MOVE 'R'         TO WS-TRAN-HOLD-CD
               WHEN '04'
               WHEN 'ADJ'
               WHEN 'APY'
                   MOVE 'A'         TO WS-TRAN-HOLD-CD
               WHEN OTHER
                   MOVE 'TR005'     TO WS-ERROR-CODE
                   MOVE '対象外の取引種別' TO WS-ERROR-MSG
                   ADD 1            TO WS-SKIP-CNT
                   ADD 1            TO WS-ERROR-CNT
                   PERFORM 800-HANDLE-NONFATAL-ERROR
                   EXIT PARAGRAPH
           END-EVALUATE
           MOVE TR-TRAN-AMT         TO WS-TRAN-HOLD-AMT
           .
       210-CALCULATE-UNPAID.
      *    請求額から入金合計を差し引き、未入金状態を判定する。
           MOVE WS-STMT-DUE-AMT          TO WS-UNPAID-AMT
           SUBTRACT WS-PAYMENT-AMT     FROM WS-UNPAID-AMT
           EVALUATE TRUE
               WHEN WS-STMT-DUE-AMT <= ZERO
      *            請求がない明細は解消候補として扱う。
                   MOVE 'CLEARED'        TO WS-KP201S-STATE-IN
               WHEN WS-UNPAID-AMT < ZERO
      *            入金超過は解消候補として後続判定へ渡す。
                   MOVE 'OVERPAID'       TO WS-KP201S-STATE-IN
               WHEN WS-UNPAID-AMT = ZERO
      *            請求額と入金額が一致したため全額解消とする。
                   MOVE 'CLEARED'        TO WS-KP201S-STATE-IN
               WHEN WS-PAYMENT-AMT > ZERO
      *            入金はあるが残額があるため部分入金とする。
                   MOVE 'PARTIAL'        TO WS-KP201S-STATE-IN
               WHEN OTHER
      *            入金がなく請求残があるため未入金とする。
                   MOVE 'UNPAID'         TO WS-KP201S-STATE-IN
           END-EVALUATE
           EXIT PARAGRAPH.
       220-CALCULATE-DAYS-PAST-DUE.
      *    締日から実行日までの日数を基礎日数として求める
           IF WS-RUN-DATE <= WS-CURRENT-STMT-DT
              MOVE ZERO TO WS-DAYS-PAST-DUE
              EXIT PARAGRAPH
           END-IF
           COMPUTE WS-DAYS-PAST-DUE =
               FUNCTION INTEGER-OF-DATE(WS-RUN-DATE)
             - FUNCTION INTEGER-OF-DATE(WS-CURRENT-STMT-DT)
           END-COMPUTE
      *    商品区分ごとの支払猶予日数を控除する
           EVALUATE AC-GROUP-CODE
              WHEN 'C'
                 SUBTRACT 15 FROM WS-DAYS-PAST-DUE
              WHEN 'T'
                 SUBTRACT 20 FROM WS-DAYS-PAST-DUE
              WHEN OTHER
                 SUBTRACT 10 FROM WS-DAYS-PAST-DUE
           END-EVALUATE
      *    支払期限が休日に当たる場合は翌営業日扱いに補正する
           EVALUATE AC-GROUP-CODE
              WHEN 'C'
                 EVALUATE FUNCTION MOD
                    (FUNCTION INTEGER-OF-DATE(WS-CURRENT-STMT-DT)
                     + 15 7)
                    WHEN 0
                       SUBTRACT 2 FROM WS-DAYS-PAST-DUE
                    WHEN 1
                       SUBTRACT 1 FROM WS-DAYS-PAST-DUE
                 END-EVALUATE
              WHEN 'T'
                 EVALUATE FUNCTION MOD
                    (FUNCTION INTEGER-OF-DATE(WS-CURRENT-STMT-DT)
                     + 20 7)
                    WHEN 0
                       SUBTRACT 2 FROM WS-DAYS-PAST-DUE
                    WHEN 1
                       SUBTRACT 1 FROM WS-DAYS-PAST-DUE
                 END-EVALUATE
              WHEN OTHER
                 EVALUATE FUNCTION MOD
                    (FUNCTION INTEGER-OF-DATE(WS-CURRENT-STMT-DT)
                     + 10 7)
                    WHEN 0
                       SUBTRACT 2 FROM WS-DAYS-PAST-DUE
                    WHEN 1
                       SUBTRACT 1 FROM WS-DAYS-PAST-DUE
                 END-EVALUATE
           END-EVALUATE
           IF WS-DAYS-PAST-DUE < ZERO
              MOVE ZERO TO WS-DAYS-PAST-DUE
           END-IF
           EXIT PARAGRAPH.
       230-RATE-LOOKUP.
      *    口座属性と延滞状態から適用する料率区分を決める。
           MOVE 'STD'  TO WS-GROUP-RATE-CD
           MOVE ZERO   TO WS-PENALTY-RATE
           IF WS-DAYS-PAST-DUE <= ZERO
               MOVE 'CUR' TO WS-GROUP-RATE-CD
               EXIT PARAGRAPH
           END-IF
           EVALUATE TRUE
               WHEN AC-KYC-STATUS NOT = 'A'
                    AND AC-KYC-STATUS NOT = 'V'
                   MOVE 'KYC' TO WS-GROUP-RATE-CD
                   MOVE 0.2400 TO WS-PENALTY-RATE
               WHEN AC-OVER-AMT > ZERO
                    AND WS-DAYS-PAST-DUE >= 90
                   MOVE 'O90' TO WS-GROUP-RATE-CD
                   MOVE 0.2350 TO WS-PENALTY-RATE
               WHEN WS-DAYS-PAST-DUE >= 90
                   MOVE 'D90' TO WS-GROUP-RATE-CD
                   MOVE 0.2200 TO WS-PENALTY-RATE
               WHEN AC-OVER-AMT > ZERO
                    AND WS-DAYS-PAST-DUE >= 60
                   MOVE 'O60' TO WS-GROUP-RATE-CD
                   MOVE 0.2100 TO WS-PENALTY-RATE
               WHEN WS-DAYS-PAST-DUE >= 60
                   MOVE 'D60' TO WS-GROUP-RATE-CD
                   MOVE 0.1950 TO WS-PENALTY-RATE
               WHEN AC-OVER-AMT > ZERO
                    AND WS-DAYS-PAST-DUE >= 30
                   MOVE 'O30' TO WS-GROUP-RATE-CD
                   MOVE 0.1850 TO WS-PENALTY-RATE
               WHEN WS-DAYS-PAST-DUE >= 30
                   MOVE 'D30' TO WS-GROUP-RATE-CD
                   MOVE 0.1700 TO WS-PENALTY-RATE
               WHEN OTHER
                   MOVE 'D01' TO WS-GROUP-RATE-CD
                   MOVE 0.1450 TO WS-PENALTY-RATE
           END-EVALUATE
      *    法人、信託系は基準料率を抑える。
           EVALUATE AC-GROUP-CODE(1:1)
               WHEN 'C'
                   IF WS-PENALTY-RATE > 0.1800
                       MOVE 0.1800 TO WS-PENALTY-RATE
                   END-IF
               WHEN 'T'
                   IF WS-PENALTY-RATE > 0.1600
                       MOVE 0.1600 TO WS-PENALTY-RATE
                   END-IF
           END-EVALUATE
           EXIT PARAGRAPH.
       240-CALCULATE-FEE.
      *    未入金額、経過日数、料率から日割り基礎額を算出する
           MOVE ZERO                   TO WS-RAW-FEE-AMT
           IF WS-UNPAID-AMT <= ZERO
              EXIT PARAGRAPH
           END-IF
           IF WS-DAYS-PAST-DUE <= ZERO
              EXIT PARAGRAPH
           END-IF
           IF WS-PENALTY-RATE <= ZERO
              EXIT PARAGRAPH
           END-IF
           COMPUTE WS-RAW-FEE-AMT ROUNDED =
                   WS-UNPAID-AMT
                 * WS-DAYS-PAST-DUE
                 * WS-PENALTY-RATE
                 / 36500
           END-COMPUTE
           IF WS-RAW-FEE-AMT < ZERO
              MOVE ZERO                TO WS-RAW-FEE-AMT
           END-IF
           EXIT PARAGRAPH.
       250-APPLY-FLOOR.
      *    商品区分と口座属性で免除条件と最低徴収額を決める。
           MOVE WS-RAW-FEE-AMT        TO WS-FLOOR-FEE-AMT
           IF WS-UNPAID-AMT <= ZERO
              MOVE ZERO               TO WS-FLOOR-FEE-AMT
              EXIT PARAGRAPH
           END-IF
           EVALUATE AC-GROUP-CODE
              WHEN 'CORP'
                 IF WS-UNPAID-AMT < 1000
                    MOVE ZERO          TO WS-FLOOR-FEE-AMT
                 ELSE
                    IF AC-OVER-AMT > ZERO
                       IF WS-FLOOR-FEE-AMT < 700
                          MOVE 700     TO WS-FLOOR-FEE-AMT
                       END-IF
                    ELSE
                       IF WS-FLOOR-FEE-AMT < 500
                          MOVE 500     TO WS-FLOOR-FEE-AMT
                       END-IF
                    END-IF
                 END-IF
              WHEN 'TRST'
                 IF WS-UNPAID-AMT < 500
                    MOVE ZERO          TO WS-FLOOR-FEE-AMT
                 ELSE
                    IF WS-FLOOR-FEE-AMT < 300
                       MOVE 300        TO WS-FLOOR-FEE-AMT
                    END-IF
                 END-IF
              WHEN OTHER
                 IF WS-UNPAID-AMT < 300
                    MOVE ZERO          TO WS-FLOOR-FEE-AMT
                 ELSE
                    IF WS-FLOOR-FEE-AMT < 200
                       MOVE 200        TO WS-FLOOR-FEE-AMT
                    END-IF
                 END-IF
           END-EVALUATE
      *    本人確認未完了の口座は最低額を加算せず実額だけにする。
           IF AC-KYC-STATUS NOT = 'OK'
              IF WS-RAW-FEE-AMT < WS-FLOOR-FEE-AMT
                 MOVE WS-RAW-FEE-AMT  TO WS-FLOOR-FEE-AMT
              END-IF
           END-IF
      *    少額端数は徴収せず、百円単位未満を切り捨てる。
           IF WS-FLOOR-FEE-AMT < 100
              MOVE ZERO               TO WS-FLOOR-FEE-AMT
           ELSE
              DIVIDE WS-FLOOR-FEE-AMT BY 100
                 GIVING WS-FLOOR-FEE-AMT
              MULTIPLY WS-FLOOR-FEE-AMT BY 100
                 GIVING WS-FLOOR-FEE-AMT
           END-IF
           .
       260-APPLY-CAP.
      *    床適用後の金額を基準に、上限を低い順に反映する。
           MOVE WS-FLOOR-FEE-AMT       TO WS-CAPPED-FEE-AMT
      *    利息制限上限は元本残に対する今回算定額を超えない。
           IF WS-CAPPED-FEE-AMT > WS-UNPAID-AMT
              MOVE WS-UNPAID-AMT       TO WS-CAPPED-FEE-AMT
           END-IF
      *    商品区分別の上限を反映する。
           EVALUATE AC-GROUP-CODE
              WHEN 'STD'
                 IF WS-CAPPED-FEE-AMT > ST-MIN-PAY-AMT
                    MOVE ST-MIN-PAY-AMT TO WS-CAPPED-FEE-AMT
                 END-IF
              WHEN 'TRS'
                 IF WS-CAPPED-FEE-AMT > ST-INT-AMT
                    MOVE ST-INT-AMT     TO WS-CAPPED-FEE-AMT
                 END-IF
              WHEN 'CRP'
                 IF WS-CAPPED-FEE-AMT > ST-FEE-AMT
                    MOVE ST-FEE-AMT     TO WS-CAPPED-FEE-AMT
                 END-IF
              WHEN OTHER
                 CONTINUE
           END-EVALUATE
      *    信用枠超過分がある場合は超過額を上限とする。
           IF AC-OVER-AMT > ZERO
              IF WS-CAPPED-FEE-AMT > AC-OVER-AMT
                 MOVE AC-OVER-AMT       TO WS-CAPPED-FEE-AMT
              END-IF
           END-IF
      *    法人契約は当回請求残高を超えて損害金を立てない。
           IF AC-GROUP-CODE = 'CRP'
              IF WS-CAPPED-FEE-AMT > ST-CLOSE-BAL
                 MOVE ST-CLOSE-BAL      TO WS-CAPPED-FEE-AMT
              END-IF
           END-IF
           IF WS-CAPPED-FEE-AMT < ZERO
              MOVE ZERO                 TO WS-CAPPED-FEE-AMT
           END-IF
           EXIT PARAGRAPH.
       270-CALL-KP201S.
      *    既存延滞情報と当回算出値を判定サービスへ渡す
           MOVE 'JUDGE'             TO WS-KP201S-FUNC
           MOVE WS-CURRENT-ACCT-ID  TO WS-KP201S-ACCT-ID
           MOVE WS-CURRENT-STMT-DT  TO WS-KP201S-STMT-DT
           MOVE WS-DAYS-PAST-DUE    TO WS-KP201S-NEW-DAYS-PD
           MOVE WS-UNPAID-AMT       TO WS-KP201S-NEW-DUE-AMT
           MOVE ZERO                TO WS-KP201S-RETURN-CD
           IF DL-ACCT-ID = WS-CURRENT-ACCT-ID
               MOVE DL-DAYS-PAST-DUE TO WS-KP201S-OLD-DAYS-PD
               MOVE DL-DUE-AMT       TO WS-KP201S-OLD-DUE-AMT
               IF DL-DAYS-PAST-DUE > ZERO
               OR DL-DUE-AMT       > ZERO
                   MOVE 'D'          TO WS-KP201S-STATE-IN
               ELSE
                   MOVE 'N'          TO WS-KP201S-STATE-IN
               END-IF
           ELSE
               MOVE ZERO             TO WS-KP201S-OLD-DAYS-PD
               MOVE ZERO             TO WS-KP201S-OLD-DUE-AMT
               MOVE 'N'              TO WS-KP201S-STATE-IN
           END-IF
           CALL 'KP201S' USING WS-KP201S-FUNC
                               WS-KP201S-ACCT-ID
                               WS-KP201S-STMT-DT
                               WS-KP201S-OLD-DAYS-PD
                               WS-KP201S-OLD-DUE-AMT
                               WS-KP201S-NEW-DAYS-PD
                               WS-KP201S-NEW-DUE-AMT
                               WS-KP201S-STATE-IN
                               WS-KP201S-STATE-OUT
                               WS-KP201S-RETURN-CD
           END-CALL
           IF WS-KP201S-RETURN-CD NOT = ZERO
               MOVE 'KP201S'         TO WS-ERROR-CODE
                MOVE '延滞状態判定サービスでエラー' TO
                WS-ERROR-MSG
               PERFORM 800-HANDLE-NONFATAL-ERROR
           END-IF
           EXIT PARAGRAPH.
       280-ACCUMULATE-ACCOUNT.
      *    延滞残がない締め分は口座累積の対象外とする
           IF WS-UNPAID-AMT NOT > ZERO
              EXIT PARAGRAPH
           END-IF
      *    口座内の最古延滞日を保持する
           IF WS-CYCLE-DUE-COUNT = ZERO
              MOVE WS-CURRENT-STMT-DT TO WS-ACCT-FIRST-DUE-DT
           ELSE
              IF WS-CURRENT-STMT-DT < WS-ACCT-FIRST-DUE-DT
                 MOVE WS-CURRENT-STMT-DT TO WS-ACCT-FIRST-DUE-DT
              END-IF
           END-IF
      *    最新締日は後続締めで上書きする
           IF WS-CYCLE-DUE-COUNT = ZERO
              MOVE WS-CURRENT-STMT-DT TO WS-ACCT-LAST-DUE-DT
           ELSE
              IF WS-CURRENT-STMT-DT > WS-ACCT-LAST-DUE-DT
                 MOVE WS-CURRENT-STMT-DT TO WS-ACCT-LAST-DUE-DT
              END-IF
           END-IF
      *    未入金額、損害金、締め件数を口座単位に積み上げる
           ADD WS-UNPAID-AMT       TO WS-ACCT-DUE-TOTAL
           ADD WS-CAPPED-FEE-AMT   TO WS-ACCT-FEE-TOTAL
           ADD 1                   TO WS-CYCLE-DUE-COUNT
           EXIT PARAGRAPH.
       290-BUILD-DELINQUENCY-RECORD.
      *    口座単位の延滞情報を出力レコードへ正規化する
           INITIALIZE KPDELINF-REC
           MOVE WS-CURRENT-ACCT-ID     TO DL-ACCT-ID
           IF WS-ACCT-DUE-TOTAL > ZERO
               MOVE WS-ACCT-DUE-TOTAL TO DL-DUE-AMT
           ELSE
               MOVE ZERO              TO DL-DUE-AMT
           END-IF
           IF WS-DAYS-PAST-DUE > ZERO
               MOVE WS-DAYS-PAST-DUE  TO DL-DAYS-PAST-DUE
           ELSE
               MOVE ZERO              TO DL-DAYS-PAST-DUE
           END-IF
           IF WS-ACCT-LAST-DUE-DT > ZERO
               MOVE WS-ACCT-LAST-DUE-DT
                                       TO DL-LAST-DUE-DT
           ELSE
               MOVE WS-CURRENT-STMT-DT
                                       TO DL-LAST-DUE-DT
           END-IF
           IF WS-GROUP-RATE-CD NOT = SPACE
               MOVE WS-GROUP-RATE-CD  TO DL-PENALTY-RATE-CD
           ELSE
               MOVE ZERO              TO DL-PENALTY-RATE-CD
           END-IF
           EXIT PARAGRAPH.
       300-WRITE-OR-UPDATE-KPDELINF.
      *    KPDELINFを現口座キーで参照し発生状態を反映する
             MOVE WS-CURRENT-ACCT-ID     TO DL-ACCT-ID
           READ KPDELINF
                KEY IS DL-ACCT-ID
           END-READ
           EVALUATE WS-FILE-STATUS-KPDELINF
               WHEN '00'
                   IF WS-ACCT-DUE-TOTAL > ZERO
                       MOVE WS-CURRENT-ACCT-ID
                                                TO DL-ACCT-ID
                       MOVE WS-DAYS-PAST-DUE    TO DL-DAYS-PAST-DUE
                       MOVE WS-ACCT-DUE-TOTAL   TO DL-DUE-AMT
                       MOVE WS-ACCT-FIRST-DUE-DT
                                                TO DL-LAST-DUE-DT
                       MOVE WS-GROUP-RATE-CD    TO DL-PENALTY-RATE-CD
                   ELSE
                       MOVE WS-CURRENT-ACCT-ID
                                                TO DL-ACCT-ID
                       MOVE ZERO                TO DL-DAYS-PAST-DUE
                       MOVE ZERO                TO DL-DUE-AMT
                       MOVE WS-RUN-DATE         TO DL-LAST-DUE-DT
                        MOVE ZERO                TO DL-PENALTY-RATE-CD
                   END-IF
                   REWRITE KPDELINF-REC
                   END-REWRITE
                   EVALUATE WS-FILE-STATUS-KPDELINF
                       WHEN '00'
                           ADD 1                TO WS-UPDATE-DLINF-CNT
                       WHEN '23'
                           MOVE 'DL-NF-RW'      TO WS-ERROR-CODE
                           MOVE '解消更新対象なし'
                                                TO WS-ERROR-MSG
                           ADD 1                TO WS-ERROR-CNT
                           PERFORM 800-HANDLE-NONFATAL-ERROR
                       WHEN '92'
                           MOVE 'DL-LOCK'       TO WS-ERROR-CODE
                           MOVE 'KPDELINF排他エラー'
                                                TO WS-ERROR-MSG
                           ADD 1                TO WS-ERROR-CNT
                           PERFORM 800-HANDLE-NONFATAL-ERROR
                       WHEN OTHER
                           MOVE 'DL-RW-ER'      TO WS-ERROR-CODE
                           MOVE 'KPDELINF更新エラー'
                                                TO WS-ERROR-MSG
                           ADD 1                TO WS-ERROR-CNT
                           PERFORM 800-HANDLE-NONFATAL-ERROR
                   END-EVALUATE
               WHEN '23'
                   IF WS-ACCT-DUE-TOTAL > ZERO
                       MOVE WS-CURRENT-ACCT-ID
                                                TO DL-ACCT-ID
                       MOVE WS-DAYS-PAST-DUE    TO DL-DAYS-PAST-DUE
                       MOVE WS-ACCT-DUE-TOTAL   TO DL-DUE-AMT
                       MOVE WS-ACCT-FIRST-DUE-DT
                                                TO DL-LAST-DUE-DT
                       MOVE WS-GROUP-RATE-CD    TO DL-PENALTY-RATE-CD
                       WRITE KPDELINF-REC
                       END-WRITE
                       EVALUATE WS-FILE-STATUS-KPDELINF
                           WHEN '00'
                               ADD 1            TO WS-WRITE-DLINF-CNT
                           WHEN '22'
                               MOVE 'DL-DUP'    TO WS-ERROR-CODE
                               MOVE 'KPDELINF重複エラー'
                                                TO WS-ERROR-MSG
                               ADD 1            TO WS-ERROR-CNT
                               PERFORM 800-HANDLE-NONFATAL-ERROR
                           WHEN '92'
                               MOVE 'DL-LOCK'   TO WS-ERROR-CODE
                               MOVE 'KPDELINF排他エラー'
                                                TO WS-ERROR-MSG
                               ADD 1            TO WS-ERROR-CNT
                               PERFORM 800-HANDLE-NONFATAL-ERROR
                           WHEN OTHER
                               MOVE 'DL-WR-ER'  TO WS-ERROR-CODE
                               MOVE 'KPDELINF登録エラー'
                                                TO WS-ERROR-MSG
                               ADD 1            TO WS-ERROR-CNT
                               PERFORM 800-HANDLE-NONFATAL-ERROR
                       END-EVALUATE
                   END-IF
               WHEN '92'
                   MOVE 'DL-LOCK'               TO WS-ERROR-CODE
                   MOVE 'KPDELINF排他エラー'     TO WS-ERROR-MSG
                   ADD 1                        TO WS-ERROR-CNT
                   PERFORM 800-HANDLE-NONFATAL-ERROR
               WHEN OTHER
                   MOVE 'DL-RD-ER'              TO WS-ERROR-CODE
                   MOVE 'KPDELINF参照エラー'     TO WS-ERROR-MSG
                   ADD 1                        TO WS-ERROR-CNT
                   PERFORM 800-HANDLE-NONFATAL-ERROR
           END-EVALUATE
           EXIT PARAGRAPH.
       310-END-ACCOUNT-GROUP.
      *口座内で延滞が残った場合だけ延滞情報へ反映する
           IF WS-CYCLE-DUE-COUNT > ZERO
              AND WS-ACCT-DUE-TOTAL > ZERO
              MOVE WS-ACCT-DUE-TOTAL    TO WS-UNPAID-AMT
              MOVE WS-ACCT-FIRST-DUE-DT TO WS-CURRENT-STMT-DT
              PERFORM 220-CALCULATE-DAYS-PAST-DUE
              PERFORM 230-RATE-LOOKUP
              PERFORM 240-CALCULATE-FEE
              PERFORM 250-APPLY-FLOOR
              PERFORM 260-APPLY-CAP
              PERFORM 270-CALL-KP201S
              IF WS-KP201S-RETURN-CD = ZERO
                 PERFORM 290-BUILD-DELINQUENCY-RECORD
                 PERFORM 300-WRITE-OR-UPDATE-KPDELINF
              ELSE
                 MOVE 'KP201S' TO WS-ERROR-CODE
                 MOVE '延滞状態判定でエラー' TO WS-ERROR-MSG
                 PERFORM 800-HANDLE-NONFATAL-ERROR
              END-IF
           ELSE
              ADD 1 TO WS-SKIP-CNT
           END-IF
           EXIT PARAGRAPH.
       800-HANDLE-NONFATAL-ERROR.
      *    継続可能な不正を記録し、当該明細を処理対象外にする。
           IF WS-ERROR-CODE = SPACES
              MOVE 'E800' TO WS-ERROR-CODE
           END-IF
           IF WS-ERROR-MSG = SPACES
              MOVE '継続可能エラー' TO WS-ERROR-MSG
           END-IF
           ADD 1 TO WS-SKIP-CNT
           ADD 1 TO WS-ERROR-CNT
           DISPLAY 'KP200B 警告 コード=' WS-ERROR-CODE
           DISPLAY 'KP200B 警告 メッセージ=' WS-ERROR-MSG
           DISPLAY 'KP200B 警告 バッチ=' WS-BATCH-ID
           DISPLAY 'KP200B 警告 日付=' WS-RUN-DATE
           DISPLAY 'KP200B 警告 口座=' WS-CURRENT-ACCT-ID
           DISPLAY 'KP200B 警告 明細=' WS-CURRENT-STMT-DT
           DISPLAY 'KP200B 警告 取引=' WS-TRAN-HOLD-CD
           MOVE SPACES TO WS-ERROR-CODE
           MOVE SPACES TO WS-ERROR-MSG
           EXIT PARAGRAPH.
       900-CLOSE-FILES.
      *    全ファイルを閉じ、個別にクローズ結果を確認する
           CLOSE KZACCTF
                 KZSTMTF
                 KZTRANF
                 KPDELINF
           IF WS-FILE-STATUS-KZACCTF NOT = '00'
              MOVE 'CLOSE-KZACCTF' TO WS-ERROR-CODE
              MOVE 'KZACCTF クローズエラー' TO WS-ERROR-MSG
              MOVE 'Y' TO WS-FATAL-SW
              ADD 1 TO WS-ERROR-CNT
           END-IF
           IF WS-FILE-STATUS-KZSTMTF NOT = '00'
              MOVE 'CLOSE-KZSTMTF' TO WS-ERROR-CODE
              MOVE 'KZSTMTF クローズエラー' TO WS-ERROR-MSG
              MOVE 'Y' TO WS-FATAL-SW
              ADD 1 TO WS-ERROR-CNT
           END-IF
           IF WS-FILE-STATUS-KZTRANF NOT = '00'
              MOVE 'CLOSE-KZTRANF' TO WS-ERROR-CODE
              MOVE 'KZTRANF クローズエラー' TO WS-ERROR-MSG
              MOVE 'Y' TO WS-FATAL-SW
              ADD 1 TO WS-ERROR-CNT
           END-IF
           IF WS-FILE-STATUS-KPDELINF NOT = '00'
              MOVE 'CLOSE-KPDELINF' TO WS-ERROR-CODE
              MOVE 'KPDELINF クローズエラー' TO WS-ERROR-MSG
              MOVE 'Y' TO WS-FATAL-SW
              ADD 1 TO WS-ERROR-CNT
           END-IF
           EXIT PARAGRAPH.
       990-ABEND-ROUTINE.
      *    重大エラー情報を編集し、異常終了を通知する
           MOVE 'Y'                    TO WS-FATAL-SW
           ADD  1                      TO WS-ERROR-CNT
           EVALUATE TRUE
               WHEN WS-FILE-STATUS-KZACCTF   NOT = '00'
                   MOVE 'KZACCTF'       TO WS-ERROR-CODE
                   STRING '口座ファイル異常 FS='
                          WS-FILE-STATUS-KZACCTF
                          ' 口座=' WS-CURRENT-ACCT-ID
                          ' 締日=' WS-CURRENT-STMT-DT
                       DELIMITED BY SIZE
                       INTO WS-ERROR-MSG
                   END-STRING
               WHEN WS-FILE-STATUS-KZSTMTF   NOT = '00'
                AND WS-FILE-STATUS-KZSTMTF   NOT = '10'
                   MOVE 'KZSTMTF'       TO WS-ERROR-CODE
                   STRING '明細ファイル異常 FS='
                          WS-FILE-STATUS-KZSTMTF
                          ' 口座=' WS-CURRENT-ACCT-ID
                          ' 締日=' WS-CURRENT-STMT-DT
                       DELIMITED BY SIZE
                       INTO WS-ERROR-MSG
                   END-STRING
               WHEN WS-FILE-STATUS-KZTRANF   NOT = '00'
                AND WS-FILE-STATUS-KZTRANF   NOT = '10'
                   MOVE 'KZTRANF'       TO WS-ERROR-CODE
                   STRING '取引ファイル異常 FS='
                          WS-FILE-STATUS-KZTRANF
                          ' 口座=' WS-CURRENT-ACCT-ID
                          ' 締日=' WS-CURRENT-STMT-DT
                       DELIMITED BY SIZE
                       INTO WS-ERROR-MSG
                   END-STRING
               WHEN WS-FILE-STATUS-KPDELINF  NOT = '00'
                AND WS-FILE-STATUS-KPDELINF  NOT = '02'
                   MOVE 'KPDELINF'      TO WS-ERROR-CODE
                   STRING '延滞ファイル異常 FS='
                          WS-FILE-STATUS-KPDELINF
                          ' 口座=' WS-CURRENT-ACCT-ID
                          ' 締日=' WS-CURRENT-STMT-DT
                       DELIMITED BY SIZE
                       INTO WS-ERROR-MSG
                   END-STRING
               WHEN OTHER
                   MOVE 'KP200B'        TO WS-ERROR-CODE
                   STRING '重大エラー 口座=' WS-CURRENT-ACCT-ID
                          ' 締日=' WS-CURRENT-STMT-DT
                       DELIMITED BY SIZE
                       INTO WS-ERROR-MSG
                   END-STRING
           END-EVALUATE
           MOVE 12                      TO RETURN-CODE
           EXIT PARAGRAPH.
       999-RETURN.
      *    処理件数を出力し、最終リターンコードを決定する
           DISPLAY 'KP200B 明細読込件数 = ' WS-READ-STMT-CNT
           DISPLAY 'KP200B 取引読込件数 = ' WS-READ-TRAN-CNT
           DISPLAY 'KP200B 口座読込件数 = ' WS-READ-ACCT-CNT
           DISPLAY 'KP200B 延滞登録件数 = ' WS-WRITE-DLINF-CNT
           DISPLAY 'KP200B 延滞更新件数 = ' WS-UPDATE-DLINF-CNT
           DISPLAY 'KP200B スキップ件数 = ' WS-SKIP-CNT
           DISPLAY 'KP200B エラー件数   = ' WS-ERROR-CNT
           EVALUATE TRUE
               WHEN WS-FATAL-SW = 'Y'
                   MOVE 16 TO RETURN-CODE
               WHEN WS-ERROR-CNT > ZERO
                   MOVE 8 TO RETURN-CODE
               WHEN OTHER
                   MOVE ZERO TO RETURN-CODE
           END-EVALUATE
           EXIT PROGRAM.
