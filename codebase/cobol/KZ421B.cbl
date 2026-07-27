       IDENTIFICATION DIVISION.
       PROGRAM-ID. KZ421B.
      * 変更履歴
      * 版数  年月日(和暦)    担当                     概要
      * 1.00  令和04.04.01  システム部 勘定系チーム  新規作成
      * 1.01  令和05.10.16  システム部 勘定系チーム  税率判定処理見直し
      * 1.02  令和06.06.03  システム部 勘定系チーム  GL計上摘要編集を修正
      *---------------------------------------------------------------*
      * 源泉税ＧＬ預り金計上バッチ                                    *
      *---------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZTXRF-FILE
               ASSIGN TO "KZTXRF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS FS-KZTXRF.

           SELECT KZGLPF-FILE
               ASSIGN TO "KZGLPF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS FS-KZGLPF.

           SELECT KZADLF-FILE
               ASSIGN TO "KZADLF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS FS-KZADLF.

       DATA DIVISION.
       FILE SECTION.
       FD  KZTXRF-FILE.
           COPY KZTXRFC.

       FD  KZGLPF-FILE.
           COPY KZGLPFC2.

       FD  KZADLF-FILE.
           COPY KZADLFC.

       WORKING-STORAGE SECTION.
       01  FS-AREA.
           05 FS-KZTXRF              PIC XX VALUE SPACE.
           05 FS-KZGLPF              PIC XX VALUE SPACE.
           05 FS-KZADLF              PIC XX VALUE SPACE.

       01  SW-AREA.
           05 SW-EOF                 PIC X VALUE "N".
              88 EOF-KZTXRF               VALUE "Y".
              88 NOT-EOF-KZTXRF           VALUE "N".
           05 SW-HARD-ERR            PIC X VALUE "N".
              88 HARD-ERROR               VALUE "Y".
              88 NO-HARD-ERROR            VALUE "N".
           05 SW-REC-VALID           PIC X VALUE "N".
              88 REC-VALID                VALUE "Y".
              88 REC-INVALID              VALUE "N".

       01  COUNT-AREA.
           05 CNT-READ               PIC 9(9) VALUE ZERO.
           05 CNT-WRITE-GL           PIC 9(9) VALUE ZERO.
           05 CNT-WRITE-AUDIT        PIC 9(9) VALUE ZERO.
           05 CNT-SKIP               PIC 9(9) VALUE ZERO.
           05 WK-JOURNAL-SEQ         PIC 9(7) VALUE ZERO.
           05 WK-AUDIT-SEQ           PIC 9(7) VALUE ZERO.

       01  DATE-AREA.
           05 WK-RUN-DATE            PIC 9(8) VALUE ZERO.
           05 WK-RUN-DATE-X          REDEFINES WK-RUN-DATE PIC X(8).

       01  AMOUNT-AREA.
           05 WK-REC-DEBIT-AMT       PIC S9(13)V99 COMP-3 VALUE ZERO.
           05 WK-REC-CREDIT-AMT      PIC S9(13)V99 COMP-3 VALUE ZERO.
           05 WK-REC-DIFF-AMT        PIC S9(13)V99 COMP-3 VALUE ZERO.
           05 WK-TAX-SUM-AMT         PIC S9(13)V99 COMP-3 VALUE ZERO.
           05 WK-TOT-DEBIT-AMT       PIC S9(13)V99 COMP-3 VALUE ZERO.
           05 WK-TOT-CREDIT-AMT      PIC S9(13)V99 COMP-3 VALUE ZERO.

       01  EDIT-AREA.
           05 ED-AMT                 PIC Z,ZZZ,ZZZ,ZZZ,ZZ9.99.
           05 ED-CNT                 PIC Z,ZZZ,ZZ9.

       01  LK-KZ902S.
           05 LK-AUDIT-ID            PIC X(20).
           05 LK-SOURCE-DSID         PIC X(12).
           05 LK-DEBIT-AMT           PIC S9(13)V99 COMP-3.
           05 LK-CREDIT-AMT          PIC S9(13)V99 COMP-3.
           05 LK-RTN-CD              PIC 9(2).
           05 LK-CAUSE-CD            PIC X(2).
           05 LK-DIFF-AMT            PIC S9(13)V99 COMP-3.

       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-INIT
           IF NO-HARD-ERROR
               PERFORM 2000-MAIN-LOOP UNTIL EOF-KZTXRF
               PERFORM 3000-CALL-AUDIT
           END-IF
           PERFORM 9000-FINAL
           GOBACK.

       1000-INIT.
           MOVE 0 TO RETURN-CODE
           ACCEPT WK-RUN-DATE FROM DATE YYYYMMDD
           SET NOT-EOF-KZTXRF TO TRUE
           SET NO-HARD-ERROR TO TRUE

           OPEN INPUT KZTXRF-FILE
           IF FS-KZTXRF NOT = "00"
               DISPLAY "KZTXRF オープン失敗 ST=" FS-KZTXRF
               SET HARD-ERROR TO TRUE
               MOVE 8 TO RETURN-CODE
           END-IF

           IF NO-HARD-ERROR
               OPEN OUTPUT KZGLPF-FILE
               IF FS-KZGLPF NOT = "00"
                   DISPLAY "KZGLPF オープン失敗 ST=" FS-KZGLPF
                   SET HARD-ERROR TO TRUE
                   MOVE 8 TO RETURN-CODE
               END-IF
           END-IF

           IF NO-HARD-ERROR
               OPEN OUTPUT KZADLF-FILE
               IF FS-KZADLF NOT = "00"
                   DISPLAY "KZADLF オープン失敗 ST=" FS-KZADLF
                   SET HARD-ERROR TO TRUE
                   MOVE 8 TO RETURN-CODE
               END-IF
           END-IF

           IF NO-HARD-ERROR
               PERFORM 2100-READ-TAX
           END-IF.

       2000-MAIN-LOOP.
           ADD 1 TO CNT-READ
           PERFORM 2200-VALIDATE-TAX
           IF NO-HARD-ERROR AND REC-VALID
               PERFORM 2300-WRITE-GL
           END-IF
           IF NO-HARD-ERROR
               PERFORM 2100-READ-TAX
           ELSE
               SET EOF-KZTXRF TO TRUE
           END-IF.

       2100-READ-TAX.
           READ KZTXRF-FILE
               AT END
                   SET EOF-KZTXRF TO TRUE
               NOT AT END
                   CONTINUE
           END-READ

           IF FS-KZTXRF NOT = "00" AND FS-KZTXRF NOT = "10"
               DISPLAY "KZTXRF 読込失敗 ST=" FS-KZTXRF
               SET HARD-ERROR TO TRUE
               MOVE 8 TO RETURN-CODE
           END-IF.

       2200-VALIDATE-TAX.
           SET REC-VALID TO TRUE
           MOVE TR-GROSS-INT-AMT TO WK-REC-DEBIT-AMT
           COMPUTE WK-TAX-SUM-AMT =
                   TR-NATIONAL-TAX-AMT
                 + TR-LOCAL-TAX-AMT
           COMPUTE WK-REC-CREDIT-AMT =
                   WK-TAX-SUM-AMT
                 + TR-NET-AMT
           COMPUTE WK-REC-DIFF-AMT =
                   WK-REC-DEBIT-AMT - WK-REC-CREDIT-AMT

           IF TR-ACCT-NO = SPACE
               DISPLAY "口座番号不正"
               PERFORM 2400-WRITE-REJECT-AUDIT
               ADD 1 TO CNT-SKIP
               SET REC-INVALID TO TRUE
           ELSE
               IF TR-ACCT-TYPE NOT = "01"
                  AND TR-ACCT-TYPE NOT = "02"
                  AND TR-ACCT-TYPE NOT = "03"
                   DISPLAY "口座区分不正 口座=" TR-ACCT-NO
                   PERFORM 2400-WRITE-REJECT-AUDIT
                   ADD 1 TO CNT-SKIP
                   SET REC-INVALID TO TRUE
               ELSE
                   IF TR-NATIONAL-TAX-AMT < ZERO
                      OR TR-LOCAL-TAX-AMT < ZERO
                      OR TR-TOTAL-TAX-AMT < ZERO
                      OR TR-NET-AMT < ZERO
                      OR TR-GROSS-INT-AMT < ZERO
                       DISPLAY "税額項目負値 口座=" TR-ACCT-NO
                       PERFORM 2400-WRITE-REJECT-AUDIT
                       ADD 1 TO CNT-SKIP
                       SET REC-INVALID TO TRUE
                   ELSE
                       IF TR-TOTAL-TAX-AMT NOT = WK-TAX-SUM-AMT
                           DISPLAY "税額合計不一致 口座="
                                   TR-ACCT-NO
                           PERFORM 2400-WRITE-REJECT-AUDIT
                           ADD 1 TO CNT-SKIP
                           SET REC-INVALID TO TRUE
                       ELSE
                           IF WK-REC-DIFF-AMT NOT = ZERO
                               DISPLAY "元帳貸借不一致 口座="
                                       TR-ACCT-NO
                               PERFORM 2400-WRITE-REJECT-AUDIT
                               ADD 1 TO CNT-SKIP
                               SET REC-INVALID TO TRUE
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF.

       2300-WRITE-GL.
           ADD 1 TO WK-JOURNAL-SEQ
           INITIALIZE KZGLPF-REC
           MOVE WK-RUN-DATE          TO GP-POSTING-DATE
           MOVE TR-ACCT-NO           TO GP-ACCT-NO
           MOVE TR-ACCT-TYPE         TO GP-ACCT-TYPE
           MOVE TR-NATIONAL-TAX-AMT  TO GP-WITHHOLDING-LIAB-AMT
           MOVE TR-LOCAL-TAX-AMT     TO GP-LOCAL-LIAB-AMT
           MOVE TR-NET-AMT           TO GP-NET-PAYABLE-AMT
           MOVE TR-ACCT-NO(1:3)      TO GP-GL-BRANCH-CD
           MOVE WK-JOURNAL-SEQ       TO GP-JOURNAL-NO

           WRITE KZGLPF-REC
           IF FS-KZGLPF = "00"
               ADD 1 TO CNT-WRITE-GL
               ADD WK-REC-DEBIT-AMT  TO WK-TOT-DEBIT-AMT
               ADD WK-REC-CREDIT-AMT TO WK-TOT-CREDIT-AMT
           ELSE
               DISPLAY "KZGLPF 書込失敗 ST=" FS-KZGLPF
               SET HARD-ERROR TO TRUE
               MOVE 8 TO RETURN-CODE
           END-IF.

       2400-WRITE-REJECT-AUDIT.
           ADD 1 TO WK-AUDIT-SEQ
           INITIALIZE KZADLF-REC
           STRING "KZ421B-REJ-" WK-AUDIT-SEQ
               DELIMITED BY SIZE INTO AL-AUDIT-ID
           END-STRING
           MOVE WK-RUN-DATE       TO AL-RUN-DATE
           MOVE "KZTXRF"          TO AL-SOURCE-DSID
           MOVE WK-REC-DEBIT-AMT  TO AL-DEBIT-AMT
           MOVE WK-REC-CREDIT-AMT TO AL-CREDIT-AMT
           MOVE WK-REC-DIFF-AMT   TO AL-DIFF-AMT
           MOVE "ER"              TO AL-STATUS-CD
           WRITE KZADLF-REC
           IF FS-KZADLF = "00"
               ADD 1 TO CNT-WRITE-AUDIT
           ELSE
               DISPLAY "KZADLF 書込失敗 ST=" FS-KZADLF
               SET HARD-ERROR TO TRUE
               MOVE 8 TO RETURN-CODE
           END-IF.

       3000-CALL-AUDIT.
           INITIALIZE LK-KZ902S
           MOVE "KZ421B-GL-TOTAL" TO LK-AUDIT-ID
           MOVE "KZGLPF"          TO LK-SOURCE-DSID
           MOVE WK-TOT-DEBIT-AMT  TO LK-DEBIT-AMT
           MOVE WK-TOT-CREDIT-AMT TO LK-CREDIT-AMT

           CALL "KZ902S" USING LK-KZ902S
               ON EXCEPTION
                   DISPLAY "KZ902S 呼出失敗"
                   SET HARD-ERROR TO TRUE
                   MOVE 12 TO RETURN-CODE
           END-CALL

           IF NO-HARD-ERROR
               IF LK-RTN-CD NOT = ZERO OR LK-DIFF-AMT NOT = ZERO
                   DISPLAY "監査残高不一致 原因=" LK-CAUSE-CD
                   PERFORM 3100-WRITE-AUDIT-MISMATCH
                   MOVE 8 TO RETURN-CODE
               END-IF
           END-IF.

       3100-WRITE-AUDIT-MISMATCH.
           ADD 1 TO WK-AUDIT-SEQ
           INITIALIZE KZADLF-REC
           MOVE LK-AUDIT-ID       TO AL-AUDIT-ID
           MOVE WK-RUN-DATE       TO AL-RUN-DATE
           MOVE LK-SOURCE-DSID    TO AL-SOURCE-DSID
           MOVE LK-DEBIT-AMT      TO AL-DEBIT-AMT
           MOVE LK-CREDIT-AMT     TO AL-CREDIT-AMT
           MOVE LK-DIFF-AMT       TO AL-DIFF-AMT
           MOVE LK-CAUSE-CD       TO AL-STATUS-CD
           WRITE KZADLF-REC
           IF FS-KZADLF = "00"
               ADD 1 TO CNT-WRITE-AUDIT
           ELSE
               DISPLAY "KZADLF 監査不一致書込失敗 ST="
                       FS-KZADLF
               SET HARD-ERROR TO TRUE
               MOVE 8 TO RETURN-CODE
           END-IF.

       9000-FINAL.
           IF FS-KZTXRF = "00" OR FS-KZTXRF = "10"
               CLOSE KZTXRF-FILE
               IF FS-KZTXRF NOT = "00"
                   DISPLAY "KZTXRF クローズ失敗 ST=" FS-KZTXRF
                   MOVE 8 TO RETURN-CODE
               END-IF
           END-IF

           IF FS-KZGLPF = "00"
               CLOSE KZGLPF-FILE
               IF FS-KZGLPF NOT = "00"
                   DISPLAY "KZGLPF クローズ失敗 ST=" FS-KZGLPF
                   MOVE 8 TO RETURN-CODE
               END-IF
           END-IF

           IF FS-KZADLF = "00"
               CLOSE KZADLF-FILE
               IF FS-KZADLF NOT = "00"
                   DISPLAY "KZADLF クローズ失敗 ST=" FS-KZADLF
                   MOVE 8 TO RETURN-CODE
               END-IF
           END-IF

           MOVE CNT-READ        TO ED-CNT
           DISPLAY "読込件数=" ED-CNT
           MOVE CNT-WRITE-GL    TO ED-CNT
           DISPLAY "ＧＬ出力件数=" ED-CNT
           MOVE CNT-WRITE-AUDIT TO ED-CNT
           DISPLAY "監査ログ件数=" ED-CNT
           MOVE CNT-SKIP        TO ED-CNT
           DISPLAY "除外件数=" ED-CNT
           MOVE WK-TOT-DEBIT-AMT TO ED-AMT
           DISPLAY "借方合計=" ED-AMT
           MOVE WK-TOT-CREDIT-AMT TO ED-AMT
           DISPLAY "貸方合計=" ED-AMT

           IF RETURN-CODE = ZERO
               DISPLAY "KZ421B 正常終了"
           ELSE
               DISPLAY "KZ421B 異常終了 RC=" RETURN-CODE
           END-IF.
