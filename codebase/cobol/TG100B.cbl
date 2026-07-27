       IDENTIFICATION DIVISION.
       PROGRAM-ID. TG100B.
      *---------------------------------------------------------------
      * 変更履歴
      * 版数  年月日      担当                         概要
      * 1.00  R08.03.10  システム部 情報系チーム       初版作成
      * 1.01  R08.04.22  システム部 対外系チーム       口座照会判定追加
      * 1.02  R08.06.18  システム部 情報系/対外系チーム 種別設定明確化
      *---------------------------------------------------------------
       AUTHOR.     TRUST-BANK-SYSTEM.
      *---------------------------------------------------------------
      * 被仕向受信バッチ
      * 全銀被仕向受信電文を順次読み込み、受取人口座を検証し、
      * 正常分を全銀電文蓄積ファイルへ追記する。
      * 本処理は先行ネット完了を示す事実を保持しない。
      *---------------------------------------------------------------

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TG-IN-FEED
               ASSIGN TO "NETTGIN.DAT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS TG-IN-STATUS.

           SELECT KZACCTF
               ASSIGN TO "KZACCTF.DAT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS AC-ACCT-NO
               FILE STATUS IS KZACCTF-STATUS.

           SELECT OPTIONAL TGZENF
               ASSIGN TO "TGZENF.DAT"
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS TGZENF-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  TG-IN-FEED
           RECORD CONTAINS 120 CHARACTERS
           DATA RECORD IS TG-IN-REC.
       01  TG-IN-REC.
           05 IN-VALUE-DT             PIC 9(08).
           05 IN-CENTER-SEQ           PIC 9(10).
           05 IN-COUNTER-BANK         PIC X(04).
           05 IN-COUNTER-BRANCH       PIC X(03).
           05 IN-RECV-ACCT-NO         PIC X(10).
           05 IN-REMIT-AMT            PIC 9(11).
           05 IN-REMIT-NAME-KANA      PIC X(40).
           05 IN-RECV-ACCT-TYPE       PIC X(01).
           05 IN-FILLER               PIC X(33).

       FD  KZACCTF.
           COPY KZACCTC2.

       FD  TGZENF.
           COPY TGZENFC.

       WORKING-STORAGE SECTION.
       01  FILE-STATUS-AREA.
           05 TG-IN-STATUS            PIC X(02) VALUE SPACES.
           05 KZACCTF-STATUS          PIC X(02) VALUE SPACES.
           05 TGZENF-STATUS           PIC X(02) VALUE SPACES.

       01  END-FLAGS.
           05 TG-IN-EOF-SW            PIC X(01) VALUE 'N'.
              88 TG-IN-EOF                      VALUE 'Y'.
              88 TG-IN-NOT-EOF                  VALUE 'N'.
           05 ABEND-SW                PIC X(01) VALUE 'N'.
              88 ABEND-REQUEST                  VALUE 'Y'.
              88 NO-ABEND                       VALUE 'N'.

       01  COUNT-AREA.
           05 CNT-READ                PIC 9(09) VALUE ZERO.
           05 CNT-ACCEPT              PIC 9(09) VALUE ZERO.
           05 CNT-REJECT              PIC 9(09) VALUE ZERO.
           05 CNT-ACCT-NF             PIC 9(09) VALUE ZERO.
           05 CNT-IO-ERR              PIC 9(09) VALUE ZERO.

       01  EDIT-AREA.
           05 ED-CNT-READ             PIC ZZZ,ZZZ,ZZ9.
           05 ED-CNT-ACCEPT           PIC ZZZ,ZZZ,ZZ9.
           05 ED-CNT-REJECT           PIC ZZZ,ZZZ,ZZ9.
           05 ED-CNT-ACCT-NF          PIC ZZZ,ZZZ,ZZ9.
           05 ED-CNT-IO-ERR           PIC ZZZ,ZZZ,ZZ9.
           05 ED-RETURN-CODE          PIC ZZ9.

       01  VALIDATION-AREA.
           05 REJECT-SW               PIC X(01) VALUE 'N'.
              88 REJECT-REC                     VALUE 'Y'.
              88 ACCEPT-REC                     VALUE 'N'.
           05 REJECT-REASON           PIC X(40) VALUE SPACES.
           05 WS-AMT-NUM              PIC 9(11) VALUE ZERO.

       PROCEDURE DIVISION.
       MAIN-SECTION.
           PERFORM 1000-INITIALIZE
           IF NOT ABEND-REQUEST
               PERFORM 2000-MAIN-PROCESS
                   UNTIL TG-IN-EOF OR ABEND-REQUEST
           END-IF
           PERFORM 9000-TERMINATE
           GOBACK.

       1000-INITIALIZE.
           MOVE 0 TO RETURN-CODE
           SET TG-IN-NOT-EOF TO TRUE
           SET NO-ABEND TO TRUE
           DISPLAY 'TG520B 被仕向受信バッチ 開始'

           OPEN INPUT TG-IN-FEED
           IF TG-IN-STATUS NOT = '00'
               DISPLAY '受信フィード オープン失敗'
               DISPLAY TG-IN-STATUS
               MOVE 12 TO RETURN-CODE
               SET ABEND-REQUEST TO TRUE
           END-IF

           IF NOT ABEND-REQUEST
               OPEN INPUT KZACCTF
               IF KZACCTF-STATUS NOT = '00'
                   DISPLAY 'KZACCTF オープン失敗'
                   DISPLAY KZACCTF-STATUS
                   MOVE 12 TO RETURN-CODE
                   SET ABEND-REQUEST TO TRUE
               END-IF
           END-IF

           IF NOT ABEND-REQUEST
               OPEN EXTEND TGZENF
               IF TGZENF-STATUS NOT = '00'
                  AND TGZENF-STATUS NOT = '05'
                   DISPLAY 'TGZENF オープン失敗'
                   DISPLAY TGZENF-STATUS
                   MOVE 12 TO RETURN-CODE
                   SET ABEND-REQUEST TO TRUE
               END-IF
           END-IF.

       2000-MAIN-PROCESS.
           READ TG-IN-FEED
               AT END
                   SET TG-IN-EOF TO TRUE
               NOT AT END
                   ADD 1 TO CNT-READ
                   PERFORM 3000-VALIDATE-INPUT
                   IF ACCEPT-REC
                       PERFORM 4000-CHECK-ACCOUNT
                   END-IF
                   IF ACCEPT-REC
                       PERFORM 5000-WRITE-ZEN
                   ELSE
                       ADD 1 TO CNT-REJECT
                       DISPLAY '受信電文棄却'
                       DISPLAY IN-CENTER-SEQ
                       DISPLAY REJECT-REASON
                   END-IF
           END-READ

           IF TG-IN-STATUS NOT = '00'
              AND TG-IN-STATUS NOT = '10'
               DISPLAY '受信フィード 読込失敗'
               DISPLAY TG-IN-STATUS
               MOVE 12 TO RETURN-CODE
               SET ABEND-REQUEST TO TRUE
           END-IF.

       3000-VALIDATE-INPUT.
           SET ACCEPT-REC TO TRUE
           MOVE SPACES TO REJECT-REASON
           MOVE ZERO TO WS-AMT-NUM

           IF IN-VALUE-DT NOT NUMERIC
               SET REJECT-REC TO TRUE
               MOVE '決済日不正' TO REJECT-REASON
           END-IF

           IF ACCEPT-REC AND IN-CENTER-SEQ NOT NUMERIC
               SET REJECT-REC TO TRUE
               MOVE 'センタ通番不正' TO REJECT-REASON
           END-IF

           IF ACCEPT-REC AND IN-RECV-ACCT-NO = SPACES
               SET REJECT-REC TO TRUE
               MOVE '受取人口座番号未設定' TO REJECT-REASON
           END-IF

           IF ACCEPT-REC AND IN-REMIT-AMT NOT NUMERIC
               SET REJECT-REC TO TRUE
               MOVE '振込金額数字不正' TO REJECT-REASON
           END-IF

           IF ACCEPT-REC
               MOVE IN-REMIT-AMT TO WS-AMT-NUM
               IF WS-AMT-NUM = ZERO
                   SET REJECT-REC TO TRUE
                   MOVE '振込金額ゼロ' TO REJECT-REASON
               END-IF
           END-IF

           IF ACCEPT-REC AND IN-COUNTER-BANK = SPACES
               SET REJECT-REC TO TRUE
               MOVE '相手銀行未設定' TO REJECT-REASON
           END-IF

           IF ACCEPT-REC AND IN-COUNTER-BRANCH = SPACES
               SET REJECT-REC TO TRUE
               MOVE '相手支店未設定' TO REJECT-REASON
           END-IF.

       4000-CHECK-ACCOUNT.
           MOVE IN-RECV-ACCT-NO TO AC-ACCT-NO
           READ KZACCTF
               INVALID KEY
                   SET REJECT-REC TO TRUE
                   ADD 1 TO CNT-ACCT-NF
                   MOVE '受取人口座未登録' TO REJECT-REASON
           END-READ

           IF ACCEPT-REC
               IF KZACCTF-STATUS NOT = '00'
                   DISPLAY 'KZACCTF 読込失敗'
                   DISPLAY KZACCTF-STATUS
                   DISPLAY IN-RECV-ACCT-NO
                   ADD 1 TO CNT-IO-ERR
                   MOVE 12 TO RETURN-CODE
                   SET ABEND-REQUEST TO TRUE
               END-IF
           END-IF

           IF ACCEPT-REC AND AC-STATUS NOT = '0'
               SET REJECT-REC TO TRUE
               MOVE '口座状態不正' TO REJECT-REASON
           END-IF

           IF ACCEPT-REC AND AC-ACCT-TYPE NOT = IN-RECV-ACCT-TYPE
               SET REJECT-REC TO TRUE
               MOVE '口座種別不一致' TO REJECT-REASON
           END-IF.

       5000-WRITE-ZEN.
           INITIALIZE TGZENF-REC
           MOVE IN-VALUE-DT        TO ZE-VALUE-DT
           MOVE IN-CENTER-SEQ      TO ZE-CENTER-SEQ
           MOVE '02'               TO ZE-ZEN-TYPE
           MOVE IN-COUNTER-BANK    TO ZE-COUNTER-BANK
           MOVE IN-COUNTER-BRANCH  TO ZE-COUNTER-BRANCH
           MOVE IN-RECV-ACCT-NO    TO ZE-RECV-ACCT-NO
           MOVE IN-REMIT-AMT       TO ZE-REMIT-AMT
           MOVE IN-REMIT-NAME-KANA TO ZE-REMIT-NAME-KANA

           WRITE TGZENF-REC
           IF TGZENF-STATUS = '00'
               ADD 1 TO CNT-ACCEPT
           ELSE
               DISPLAY 'TGZENF 書込失敗'
               DISPLAY TGZENF-STATUS
               DISPLAY IN-CENTER-SEQ
               ADD 1 TO CNT-IO-ERR
               MOVE 12 TO RETURN-CODE
               SET ABEND-REQUEST TO TRUE
           END-IF.

       9000-TERMINATE.
           IF TG-IN-STATUS NOT = SPACES
               CLOSE TG-IN-FEED
           END-IF

           IF KZACCTF-STATUS NOT = SPACES
               CLOSE KZACCTF
           END-IF

           IF TGZENF-STATUS NOT = SPACES
               CLOSE TGZENF
           END-IF

           MOVE CNT-READ    TO ED-CNT-READ
           MOVE CNT-ACCEPT  TO ED-CNT-ACCEPT
           MOVE CNT-REJECT  TO ED-CNT-REJECT
           MOVE CNT-ACCT-NF TO ED-CNT-ACCT-NF
           MOVE CNT-IO-ERR  TO ED-CNT-IO-ERR

           DISPLAY 'TG520B 読込件数=' ED-CNT-READ
           DISPLAY 'TG520B 受入件数=' ED-CNT-ACCEPT
           DISPLAY 'TG520B 棄却件数=' ED-CNT-REJECT
           DISPLAY 'TG520B 口座無件数=' ED-CNT-ACCT-NF
           DISPLAY 'TG520B 入出力異常件数=' ED-CNT-IO-ERR

           IF ABEND-REQUEST
               IF RETURN-CODE = 0
                   MOVE 8 TO RETURN-CODE
               END-IF
               MOVE RETURN-CODE TO ED-RETURN-CODE
               DISPLAY 'TG520B 被仕向受信バッチ 異常終了'
               DISPLAY 'RC=' ED-RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
               DISPLAY 'TG520B 被仕向受信バッチ 正常終了'
           END-IF.
