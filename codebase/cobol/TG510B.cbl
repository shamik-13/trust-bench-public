       IDENTIFICATION DIVISION.
       PROGRAM-ID. TG510B.
      * 変更履歴
      * 版数  年月日      担当                         概要
      * 1.00  平成30.04.01 システム部 情報系チーム     新規作成
      * 1.01  令和02.10.15 システム部 対外系チーム     仕様確認
      * 1.02  令和05.07.03 システム部 情報系/対外系チーム 表示整備
       AUTHOR.     TGBT01.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZACCTF ASSIGN TO "KZACCTF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS AC-ACCT-NO
               FILE STATUS IS FS-KZACCTF.

           SELECT TGZENF ASSIGN TO "TGZENF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-TGZENF.

       DATA DIVISION.
       FILE SECTION.
       FD  KZACCTF.
           COPY KZACCTC2.

       FD  TGZENF.
           COPY TGZENFC.

       WORKING-STORAGE SECTION.
       01  FS-AREA.
           05 FS-KZACCTF              PIC XX VALUE SPACE.
           05 FS-TGZENF               PIC XX VALUE SPACE.

       01  SW-AREA.
           05 SW-EOF                  PIC X VALUE "N".
              88 EOF-KZACCTF               VALUE "Y".
              88 NOT-EOF-KZACCTF           VALUE "N".
           05 SW-HARD-ERROR           PIC X VALUE "N".
              88 HARD-ERROR                VALUE "Y".

       01  CTL-AREA.
           05 WK-SYS-DATE             PIC 9(08) VALUE ZERO.
           05 WK-CENTER-SEQ           PIC 9(08) VALUE ZERO.
           05 WK-READ-CNT             PIC 9(09) VALUE ZERO.
           05 WK-WRITE-CNT            PIC 9(09) VALUE ZERO.
           05 WK-SKIP-CNT             PIC 9(09) VALUE ZERO.
           05 WK-AMT-WORK             PIC 9(10) VALUE ZERO.
           05 WK-ACCT-NUM             PIC 9(12) VALUE ZERO.
           05 WK-BRANCH-NUM           PIC 9(03) VALUE ZERO.
           05 WK-COUNTER-BANK         PIC X(04) VALUE SPACE.
           05 WK-COUNTER-BRANCH       PIC X(03) VALUE SPACE.
           05 WK-REASON               PIC X(40) VALUE SPACE.

       01  DISP-AREA.
           05 DP-READ-CNT             PIC Z(09).
           05 DP-WRITE-CNT            PIC Z(09).
           05 DP-SKIP-CNT             PIC Z(09).

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE 0 TO RETURN-CODE
           ACCEPT WK-SYS-DATE FROM DATE YYYYMMDD
           PERFORM 1000-OPEN-FILES
           IF HARD-ERROR
               PERFORM 9000-CLOSE-FILES
               GOBACK
           END-IF

           PERFORM 2000-READ-KZACCTF
           PERFORM UNTIL EOF-KZACCTF OR HARD-ERROR
               ADD 1 TO WK-READ-CNT
               PERFORM 3000-PROCESS-ACCOUNT
               PERFORM 2000-READ-KZACCTF
           END-PERFORM

           PERFORM 9000-CLOSE-FILES

           IF HARD-ERROR
               MOVE 8 TO RETURN-CODE
           ELSE
               MOVE WK-READ-CNT  TO DP-READ-CNT
               MOVE WK-WRITE-CNT TO DP-WRITE-CNT
               MOVE WK-SKIP-CNT  TO DP-SKIP-CNT
               DISPLAY "TG510B 正常終了"
               DISPLAY "読込件数=" DP-READ-CNT
               DISPLAY "作成件数=" DP-WRITE-CNT
               DISPLAY "除外件数=" DP-SKIP-CNT
               MOVE 0 TO RETURN-CODE
           END-IF

           GOBACK.

       1000-OPEN-FILES.
           OPEN INPUT KZACCTF
           IF FS-KZACCTF NOT = "00"
               DISPLAY "KZACCTF オープン失敗 ST=" FS-KZACCTF
               SET HARD-ERROR TO TRUE
               MOVE 8 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF

           OPEN EXTEND TGZENF
           IF FS-TGZENF NOT = "00"
               DISPLAY "TGZENF オープン失敗 ST=" FS-TGZENF
               SET HARD-ERROR TO TRUE
               MOVE 8 TO RETURN-CODE
           END-IF.

       2000-READ-KZACCTF.
           READ KZACCTF NEXT RECORD
               AT END
                   SET EOF-KZACCTF TO TRUE
               NOT AT END
                   IF FS-KZACCTF NOT = "00"
                       DISPLAY "KZACCTF 読込失敗 ST=" FS-KZACCTF
                       SET HARD-ERROR TO TRUE
                       MOVE 8 TO RETURN-CODE
                   END-IF
           END-READ.

       3000-PROCESS-ACCOUNT.
           MOVE SPACE TO WK-REASON
           PERFORM 3100-VALIDATE-ACCOUNT
           IF WK-REASON NOT = SPACE
               ADD 1 TO WK-SKIP-CNT
               DISPLAY "TG510B 除外 口座=" AC-ACCT-NO
                       " 理由=" WK-REASON
               EXIT PARAGRAPH
           END-IF

           PERFORM 3200-BUILD-ZEN-DENBUN
           PERFORM 3300-WRITE-TGZENF.

       3100-VALIDATE-ACCOUNT.
           IF AC-ACCT-NO = SPACE
               MOVE "口座番号未設定" TO WK-REASON
               EXIT PARAGRAPH
           END-IF

           IF AC-BRANCH = SPACE
               MOVE "店番未設定" TO WK-REASON
               EXIT PARAGRAPH
           END-IF

           IF AC-ACCT-TYPE NOT = "1"
              AND AC-ACCT-TYPE NOT = "2"
              AND AC-ACCT-TYPE NOT = "4"
               MOVE "科目不正" TO WK-REASON
               EXIT PARAGRAPH
           END-IF

           IF AC-CUSTOMER-ID = SPACE
               MOVE "顧客番号未設定" TO WK-REASON
               EXIT PARAGRAPH
           END-IF

           IF AC-ACCT-NAME-KANA = SPACE
               MOVE "名義カナ未設定" TO WK-REASON
               EXIT PARAGRAPH
           END-IF

           IF AC-STATUS NOT = "0"
               MOVE "口座状態不正" TO WK-REASON
           END-IF.

       3200-BUILD-ZEN-DENBUN.
           ADD 1 TO WK-CENTER-SEQ

           MOVE ZERO TO WK-ACCT-NUM
           IF AC-ACCT-NO NUMERIC
               MOVE AC-ACCT-NO TO WK-ACCT-NUM
           END-IF

           MOVE ZERO TO WK-BRANCH-NUM
           IF AC-BRANCH NUMERIC
               MOVE AC-BRANCH TO WK-BRANCH-NUM
           END-IF

           COMPUTE WK-AMT-WORK =
               FUNCTION MOD(WK-ACCT-NUM + WK-BRANCH-NUM, 9000000)
               + 1000

           MOVE FUNCTION MOD(WK-BRANCH-NUM + 1000, 9000)
             TO WK-COUNTER-BANK
           MOVE FUNCTION MOD(WK-BRANCH-NUM + 100, 900)
             TO WK-COUNTER-BRANCH

           INITIALIZE TGZENF-REC
           MOVE WK-SYS-DATE       TO ZE-VALUE-DT
           MOVE WK-CENTER-SEQ     TO ZE-CENTER-SEQ
           MOVE "01"              TO ZE-ZEN-TYPE
           MOVE WK-COUNTER-BANK   TO ZE-COUNTER-BANK
           MOVE WK-COUNTER-BRANCH TO ZE-COUNTER-BRANCH
           MOVE AC-ACCT-NO        TO ZE-RECV-ACCT-NO
           MOVE WK-AMT-WORK       TO ZE-REMIT-AMT
           MOVE AC-ACCT-NAME-KANA TO ZE-REMIT-NAME-KANA.

       3300-WRITE-TGZENF.
           WRITE TGZENF-REC
           IF FS-TGZENF = "00"
               ADD 1 TO WK-WRITE-CNT
           ELSE
               DISPLAY "TGZENF 書込失敗 ST=" FS-TGZENF
               DISPLAY "対象口座=" AC-ACCT-NO
               SET HARD-ERROR TO TRUE
               MOVE 8 TO RETURN-CODE
           END-IF.

       9000-CLOSE-FILES.
           IF FS-KZACCTF NOT = SPACE
               CLOSE KZACCTF
               IF FS-KZACCTF NOT = "00"
                  AND FS-KZACCTF NOT = "42"
                   DISPLAY "KZACCTF クローズ失敗 ST=" FS-KZACCTF
                   SET HARD-ERROR TO TRUE
                   MOVE 8 TO RETURN-CODE
               END-IF
           END-IF

           IF FS-TGZENF NOT = SPACE
               CLOSE TGZENF
               IF FS-TGZENF NOT = "00"
                  AND FS-TGZENF NOT = "42"
                   DISPLAY "TGZENF クローズ失敗 ST=" FS-TGZENF
                   SET HARD-ERROR TO TRUE
                   MOVE 8 TO RETURN-CODE
               END-IF
           END-IF.
