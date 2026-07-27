       IDENTIFICATION DIVISION.
       PROGRAM-ID. TG330B.
      *--------------------------------------------------------------
      * 変更履歴
      *  版数 年月日     担当          概要
      *  1.0  令和7年4月 加藤 さやか   初版作成
      *  1.1  令和7年6月 加藤 さやか   訂正区分署名追加
      *--------------------------------------------------------------
       AUTHOR. TRUST-BATCH.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TGINRMF ASSIGN TO "TGINRMF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-IR-STATUS.
           SELECT KZACCTF ASSIGN TO "KZACCTF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS AC-ACCT-NO
               FILE STATUS IS WS-AC-STATUS.
           SELECT TGOUTCF ASSIGN TO "TGOUTCF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-OC-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  TGINRMF.
           COPY TGINRMFC.
      *
       FD  KZACCTF.
           COPY KZACCTC2.
      *
       FD  TGOUTCF.
           COPY TGOUTCFC.
      *
       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05  WS-IR-STATUS          PIC XX VALUE SPACES.
           05  WS-AC-STATUS          PIC XX VALUE SPACES.
           05  WS-OC-STATUS          PIC XX VALUE SPACES.
      *
       01  WS-SWITCHES.
           05  WS-EOF-SW             PIC X VALUE "N".
               88  IR-EOF                 VALUE "Y".
               88  IR-NOT-EOF             VALUE "N".
           05  WS-HARD-ERR-SW        PIC X VALUE "N".
               88  HARD-ERROR             VALUE "Y".
               88  NO-HARD-ERROR          VALUE "N".
      *
       01  WS-COUNTERS.
           05  WS-READ-CNT           PIC 9(9) COMP-3 VALUE 0.
           05  WS-NORMAL-CNT         PIC 9(9) COMP-3 VALUE 0.
           05  WS-CANCEL-CNT         PIC 9(9) COMP-3 VALUE 0.
           05  WS-CORR-SKIP-CNT      PIC 9(9) COMP-3 VALUE 0.
           05  WS-OTHER-SKIP-CNT     PIC 9(9) COMP-3 VALUE 0.
           05  WS-AC-NG-CNT          PIC 9(9) COMP-3 VALUE 0.
      *
       01  WS-WORK.
           05  WS-DISP-CNT           PIC Z(8)9.
      *
           COPY LK-SIG-PARM.
      *
       PROCEDURE DIVISION.
      *
       0000-MAIN SECTION.
       0000-START.
           MOVE 0 TO RETURN-CODE
           SET IR-NOT-EOF TO TRUE
           SET NO-HARD-ERROR TO TRUE
           PERFORM 1000-OPEN-FILES
           IF NO-HARD-ERROR
               PERFORM 2000-MAIN-LOOP
                   UNTIL IR-EOF OR HARD-ERROR
           END-IF
           PERFORM 9000-CLOSE-FILES
           PERFORM 9100-DISPLAY-SUMMARY
           IF HARD-ERROR
               MOVE 8 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF
           GOBACK.
      *
       1000-OPEN-FILES SECTION.
       1000-START.
           OPEN INPUT TGINRMF
           IF WS-IR-STATUS NOT = "00"
               DISPLAY "TGINRMF オープン異常 ST=" WS-IR-STATUS
               SET HARD-ERROR TO TRUE
           END-IF
      *
           IF NO-HARD-ERROR
               OPEN INPUT KZACCTF
               IF WS-AC-STATUS NOT = "00"
                   DISPLAY "KZACCTF オープン異常 ST=" WS-AC-STATUS
                   SET HARD-ERROR TO TRUE
               END-IF
           END-IF
      *
           IF NO-HARD-ERROR
               OPEN OUTPUT TGOUTCF
               IF WS-OC-STATUS NOT = "00"
                   DISPLAY "TGOUTCF オープン異常 ST=" WS-OC-STATUS
                   SET HARD-ERROR TO TRUE
               END-IF
           END-IF
      *
           IF NO-HARD-ERROR
               PERFORM 2100-READ-IR
           END-IF.
      *
       2000-MAIN-LOOP SECTION.
       2000-START.
           EVALUATE IR-REMIT-TYPE
               WHEN 10
                   ADD 1 TO WS-NORMAL-CNT
                   PERFORM 3000-VALIDATE-ACCOUNT
               WHEN 20
                   ADD 1 TO WS-CANCEL-CNT
                   PERFORM 3000-VALIDATE-ACCOUNT
               WHEN 90
                   ADD 1 TO WS-CORR-SKIP-CNT
                   DISPLAY "訂正不可スキップ SEQ=" IR-CENTER-SEQ
               WHEN OTHER
                   ADD 1 TO WS-OTHER-SKIP-CNT
                   DISPLAY "種別外 TYPE=" IR-REMIT-TYPE
                   DISPLAY "種別外 SEQ=" IR-CENTER-SEQ
           END-EVALUATE
           IF NO-HARD-ERROR
               PERFORM 2100-READ-IR
           END-IF.
      *
       2100-READ-IR SECTION.
       2100-START.
           READ TGINRMF
               AT END
                   SET IR-EOF TO TRUE
               NOT AT END
                   ADD 1 TO WS-READ-CNT
           END-READ
           IF WS-IR-STATUS NOT = "00"
              AND WS-IR-STATUS NOT = "10"
               DISPLAY "TGINRMF 読込異常 ST=" WS-IR-STATUS
               SET HARD-ERROR TO TRUE
           END-IF.
      *
       3000-VALIDATE-ACCOUNT SECTION.
       3000-START.
           MOVE IR-PAYEE-ACCT-NO TO AC-ACCT-NO
           READ KZACCTF
               INVALID KEY
                   ADD 1 TO WS-AC-NG-CNT
                   DISPLAY "該当口座なし ACCT=" IR-PAYEE-ACCT-NO
                   DISPLAY "該当口座なし SEQ=" IR-CENTER-SEQ
               NOT INVALID KEY
                   PERFORM 3100-CHECK-ACCOUNT
           END-READ
           IF WS-AC-STATUS NOT = "00"
              AND WS-AC-STATUS NOT = "23"
               DISPLAY "KZACCTF 読込異常 ST=" WS-AC-STATUS
               SET HARD-ERROR TO TRUE
           END-IF.
      *
       3100-CHECK-ACCOUNT SECTION.
       3100-START.
           IF AC-BRANCH NOT = IR-PAYEE-BRANCH
               ADD 1 TO WS-AC-NG-CNT
               DISPLAY "店番不一致 ACCT=" IR-PAYEE-ACCT-NO
               DISPLAY "店番不一致 SEQ=" IR-CENTER-SEQ
           ELSE
               IF AC-ACCT-TYPE NOT = IR-PAYEE-ACCT-TYPE
                   ADD 1 TO WS-AC-NG-CNT
                   DISPLAY "種別不正 ACCT=" IR-PAYEE-ACCT-NO
                   DISPLAY "種別不正 SEQ=" IR-CENTER-SEQ
               ELSE
                   IF AC-ACCT-NAME-KANA NOT = IR-PAYEE-NAME-KANA
                       ADD 1 TO WS-AC-NG-CNT
                       DISPLAY "名義不一致 ACCT=" IR-PAYEE-ACCT-NO
                       DISPLAY "名義不一致 SEQ=" IR-CENTER-SEQ
                   ELSE
                       IF AC-STATUS NOT = "0"
                           ADD 1 TO WS-AC-NG-CNT
                           DISPLAY "状態NG ACCT=" IR-PAYEE-ACCT-NO
                           DISPLAY "状態NG SEQ=" IR-CENTER-SEQ
                       END-IF
                   END-IF
               END-IF
           END-IF.
      *
       9000-CLOSE-FILES SECTION.
       9000-START.
           IF WS-IR-STATUS NOT = SPACES
               CLOSE TGINRMF
               IF WS-IR-STATUS NOT = "00"
                   DISPLAY "TGINRMF クローズ異常 ST=" WS-IR-STATUS
                   SET HARD-ERROR TO TRUE
               END-IF
           END-IF
      *
           IF WS-AC-STATUS NOT = SPACES
               CLOSE KZACCTF
               IF WS-AC-STATUS NOT = "00"
                   DISPLAY "KZACCTF クローズ異常 ST=" WS-AC-STATUS
                   SET HARD-ERROR TO TRUE
               END-IF
           END-IF
      *
           IF WS-OC-STATUS NOT = SPACES
               CLOSE TGOUTCF
               IF WS-OC-STATUS NOT = "00"
                   DISPLAY "TGOUTCF クローズ異常 ST=" WS-OC-STATUS
                   SET HARD-ERROR TO TRUE
               END-IF
           END-IF.
      *
       9100-DISPLAY-SUMMARY SECTION.
       9100-START.
           MOVE WS-READ-CNT TO WS-DISP-CNT
           DISPLAY "読込件数=" WS-DISP-CNT
           MOVE WS-NORMAL-CNT TO WS-DISP-CNT
           DISPLAY "通常件数=" WS-DISP-CNT
           MOVE WS-CANCEL-CNT TO WS-DISP-CNT
           DISPLAY "取消件数=" WS-DISP-CNT
           MOVE WS-CORR-SKIP-CNT TO WS-DISP-CNT
           DISPLAY "訂正不可件数=" WS-DISP-CNT
           MOVE WS-OTHER-SKIP-CNT TO WS-DISP-CNT
           DISPLAY "種別対象外件数=" WS-DISP-CNT
           MOVE WS-AC-NG-CNT TO WS-DISP-CNT
           DISPLAY "口座エラー件数=" WS-DISP-CNT.
