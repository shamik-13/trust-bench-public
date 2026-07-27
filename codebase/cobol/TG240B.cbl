       IDENTIFICATION DIVISION.
       PROGRAM-ID. TG240B.
      ******************************************************************
      * 変更履歴
      * 版数  年月日      担当                           概要
      * 1.00  平成30年04月 システム部 為替・対外接続チーム 新規作成
      * 1.01  令和02年10月 システム部 為替・対外接続チーム 突合記録見直し
      * 1.02  令和05年06月 システム部 為替・対外接続チーム 運用表示整備
      ******************************************************************
       AUTHOR. TRUST-BANK-SYSTEM.
      ******************************************************************
      * 全銀清算受信突合ジャーナル作成バッチ
      ******************************************************************

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TGINRMF
               ASSIGN TO "TGINRMF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS FS-TGINRMF.

           SELECT TGOUTSF
               ASSIGN TO "TGOUTSF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS FS-TGOUTSF.

           SELECT TGCLJNF
               ASSIGN TO "TGCLJNF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS FS-TGCLJNF.

       DATA DIVISION.
       FILE SECTION.

       FD  TGINRMF
           RECORD CONTAINS 0 CHARACTERS.
           COPY TGINRMFC.

       FD  TGOUTSF
           RECORD CONTAINS 0 CHARACTERS.
           COPY TGOUTSC.

       FD  TGCLJNF
           RECORD CONTAINS 0 CHARACTERS.
           COPY TGCLJNC.

       WORKING-STORAGE SECTION.
       01  FS-AREA.
           05 FS-TGINRMF                 PIC XX VALUE SPACES.
           05 FS-TGOUTSF                 PIC XX VALUE SPACES.
           05 FS-TGCLJNF                 PIC XX VALUE SPACES.

       01  EOF-AREA.
           05 EOF-TGINRMF                PIC X VALUE "N".
              88 TGINRMF-END                  VALUE "Y".
              88 TGINRMF-NOT-END              VALUE "N".
           05 EOF-TGOUTSF                PIC X VALUE "N".
              88 TGOUTSF-END                  VALUE "Y".
              88 TGOUTSF-NOT-END              VALUE "N".

       01  RUN-AREA.
           05 WK-ABEND-SW                PIC X VALUE "N".
              88 WK-ABEND                     VALUE "Y".
              88 WK-NORMAL                    VALUE "N".
           05 WK-CURRENT-DATE            PIC X(21) VALUE SPACES.
           05 WK-JOURNAL-COUNT           PIC 9(9) COMP-3 VALUE 0.
           05 WK-INPUT-COUNT             PIC 9(9) COMP-3 VALUE 0.
           05 WK-REJECT-COUNT            PIC 9(9) COMP-3 VALUE 0.
           05 WK-DUP-COUNT               PIC 9(9) COMP-3 VALUE 0.

       01  IR-GROUP-AREA.
           05 IR-GRP-INIT-SW             PIC X VALUE "N".
              88 IR-GRP-INIT                  VALUE "Y".
              88 IR-GRP-NOT-INIT              VALUE "N".
           05 IR-GRP-DT                  PIC 9(8) VALUE 0.
           05 IR-GRP-BANK                PIC X(4) VALUE SPACES.
           05 IR-GRP-BRANCH              PIC X(3) VALUE SPACES.
           05 IR-GRP-COUNT               PIC 9(9) COMP-3 VALUE 0.
           05 IR-GRP-AMT                 PIC 9(15) COMP-3 VALUE 0.
           05 IR-GRP-STATUS              PIC X(3) VALUE "OK ".
           05 IR-PREV-CENTER-SEQ         PIC X(20) VALUE LOW-VALUE.

       01  OS-GROUP-AREA.
           05 OS-GRP-INIT-SW             PIC X VALUE "N".
              88 OS-GRP-INIT                  VALUE "Y".
              88 OS-GRP-NOT-INIT              VALUE "N".
           05 OS-GRP-DT                  PIC 9(8) VALUE 0.
           05 OS-GRP-BANK                PIC X(4) VALUE SPACES.
           05 OS-GRP-BRANCH              PIC X(3) VALUE SPACES.
           05 OS-GRP-COUNT               PIC 9(9) COMP-3 VALUE 0.
           05 OS-GRP-AMT                 PIC 9(15) COMP-3 VALUE 0.
           05 OS-GRP-STATUS              PIC X(3) VALUE "OK ".
           05 OS-PREV-SEND-SEQ           PIC X(20) VALUE LOW-VALUE.

       PROCEDURE DIVISION.
       MAIN-SECTION.
           PERFORM 1000-INIT

           IF NOT WK-ABEND
               PERFORM 2000-PROCESS-IN
           END-IF

           IF NOT WK-ABEND
               PERFORM 3000-PROCESS-OUT
           END-IF

           PERFORM 9000-FINAL
           GOBACK.

       1000-INIT.
           MOVE 0 TO RETURN-CODE
           MOVE FUNCTION CURRENT-DATE TO WK-CURRENT-DATE

           OPEN INPUT TGINRMF
           IF FS-TGINRMF NOT = "00"
               DISPLAY "TGINRMF オープン失敗 ST=" FS-TGINRMF
               SET WK-ABEND TO TRUE
               MOVE 12 TO RETURN-CODE
           END-IF

           IF NOT WK-ABEND
               OPEN INPUT TGOUTSF
               IF FS-TGOUTSF NOT = "00"
                   DISPLAY "TGOUTSF オープン失敗 ST=" FS-TGOUTSF
                   SET WK-ABEND TO TRUE
                   MOVE 12 TO RETURN-CODE
               END-IF
           END-IF

           IF NOT WK-ABEND
               OPEN EXTEND TGCLJNF
               IF FS-TGCLJNF NOT = "00"
                   DISPLAY "TGCLJNF オープン失敗 ST=" FS-TGCLJNF
                   SET WK-ABEND TO TRUE
                   MOVE 12 TO RETURN-CODE
               END-IF
           END-IF.

       2000-PROCESS-IN.
           PERFORM 2100-READ-IN

           PERFORM UNTIL TGINRMF-END OR WK-ABEND
               ADD 1 TO WK-INPUT-COUNT
               PERFORM 2200-CHECK-IN-RECORD

               IF NOT WK-ABEND
                   IF IR-GRP-NOT-INIT
                       PERFORM 2300-START-IN-GROUP
                   ELSE
                       IF IR-REMIT-DT NOT = IR-GRP-DT
                          OR IR-PAYEE-BANK NOT = IR-GRP-BANK
                          OR IR-PAYEE-BRANCH NOT = IR-GRP-BRANCH
                           PERFORM 2400-WRITE-IN-JOURNAL
                           IF NOT WK-ABEND
                               PERFORM 2300-START-IN-GROUP
                           END-IF
                       END-IF
                   END-IF
               END-IF

               IF NOT WK-ABEND
                   IF IR-CENTER-SEQ = IR-PREV-CENTER-SEQ
                       MOVE "DUP" TO IR-GRP-STATUS
                       ADD 1 TO WK-DUP-COUNT
                   ELSE
                       MOVE IR-CENTER-SEQ TO IR-PREV-CENTER-SEQ
                   END-IF

                   ADD 1 TO IR-GRP-COUNT
                   ADD IR-REMIT-AMT TO IR-GRP-AMT
                   PERFORM 2100-READ-IN
               END-IF
           END-PERFORM

           IF NOT WK-ABEND AND IR-GRP-INIT
               PERFORM 2400-WRITE-IN-JOURNAL
           END-IF.

       2100-READ-IN.
           READ TGINRMF
               AT END
                   SET TGINRMF-END TO TRUE
               NOT AT END
                   IF FS-TGINRMF NOT = "00"
                       DISPLAY "TGINRMF 読込失敗 ST=" FS-TGINRMF
                       SET WK-ABEND TO TRUE
                       MOVE 12 TO RETURN-CODE
                   END-IF
           END-READ.

       2200-CHECK-IN-RECORD.
           IF IR-REMIT-DT = ZERO
               DISPLAY "TGINRMF 営業日不正"
               ADD 1 TO WK-REJECT-COUNT
               SET WK-ABEND TO TRUE
               MOVE 8 TO RETURN-CODE
           END-IF

           IF IR-PAYEE-BANK = SPACES
               DISPLAY "TGINRMF 被仕向銀行不正"
               ADD 1 TO WK-REJECT-COUNT
               SET WK-ABEND TO TRUE
               MOVE 8 TO RETURN-CODE
           END-IF

           IF IR-PAYEE-BRANCH = SPACES
               DISPLAY "TGINRMF 被仕向店番不正"
               ADD 1 TO WK-REJECT-COUNT
               SET WK-ABEND TO TRUE
               MOVE 8 TO RETURN-CODE
           END-IF

           IF IR-REMIT-AMT = ZERO
               DISPLAY "TGINRMF 金額不正"
               ADD 1 TO WK-REJECT-COUNT
               SET WK-ABEND TO TRUE
               MOVE 8 TO RETURN-CODE
           END-IF.

       2300-START-IN-GROUP.
           SET IR-GRP-INIT TO TRUE
           MOVE IR-REMIT-DT TO IR-GRP-DT
           MOVE IR-PAYEE-BANK TO IR-GRP-BANK
           MOVE IR-PAYEE-BRANCH TO IR-GRP-BRANCH
           MOVE 0 TO IR-GRP-COUNT
           MOVE 0 TO IR-GRP-AMT
           MOVE "OK " TO IR-GRP-STATUS.

       2400-WRITE-IN-JOURNAL.
           INITIALIZE TGCLJNF-REC
           MOVE IR-GRP-DT TO CJ-CLEARING-DT
           MOVE IR-PREV-CENTER-SEQ TO CJ-CENTER-SEQ
           MOVE "I" TO CJ-DIRECTION
           MOVE IR-GRP-BANK TO CJ-BANK-CD
           MOVE IR-GRP-BRANCH TO CJ-BRANCH-CD
           MOVE IR-GRP-COUNT TO CJ-ITEM-COUNT
           MOVE IR-GRP-AMT TO CJ-TOTAL-AMT
           MOVE IR-GRP-STATUS TO CJ-MATCH-STATUS
           MOVE WK-CURRENT-DATE TO CJ-JOURNAL-TS

           WRITE TGCLJNF-REC
           IF FS-TGCLJNF NOT = "00"
               DISPLAY "TGCLJNF 受信ジャーナル書込失敗 ST="
                       FS-TGCLJNF
               SET WK-ABEND TO TRUE
               MOVE 12 TO RETURN-CODE
           ELSE
               ADD 1 TO WK-JOURNAL-COUNT
           END-IF.

       3000-PROCESS-OUT.
           PERFORM 3100-READ-OUT

           PERFORM UNTIL TGOUTSF-END OR WK-ABEND
               ADD 1 TO WK-INPUT-COUNT
               PERFORM 3200-CHECK-OUT-RECORD

               IF NOT WK-ABEND
                   IF OS-GRP-NOT-INIT
                       PERFORM 3300-START-OUT-GROUP
                   ELSE
                       IF OS-SEND-DT NOT = OS-GRP-DT
                          OR OS-SENDER-BANK NOT = OS-GRP-BANK
                          OR OS-SENDER-BRANCH NOT = OS-GRP-BRANCH
                           PERFORM 3400-WRITE-OUT-JOURNAL
                           IF NOT WK-ABEND
                               PERFORM 3300-START-OUT-GROUP
                           END-IF
                       END-IF
                   END-IF
               END-IF

               IF NOT WK-ABEND
                   IF OS-SEND-SEQ = OS-PREV-SEND-SEQ
                       MOVE "DUP" TO OS-GRP-STATUS
                       ADD 1 TO WK-DUP-COUNT
                   ELSE
                       MOVE OS-SEND-SEQ TO OS-PREV-SEND-SEQ
                   END-IF

                   ADD 1 TO OS-GRP-COUNT
                   ADD OS-SEND-AMT TO OS-GRP-AMT
                   PERFORM 3100-READ-OUT
               END-IF
           END-PERFORM

           IF NOT WK-ABEND AND OS-GRP-INIT
               PERFORM 3400-WRITE-OUT-JOURNAL
           END-IF.

       3100-READ-OUT.
           READ TGOUTSF
               AT END
                   SET TGOUTSF-END TO TRUE
               NOT AT END
                   IF FS-TGOUTSF NOT = "00"
                       DISPLAY "TGOUTSF 読込失敗 ST=" FS-TGOUTSF
                       SET WK-ABEND TO TRUE
                       MOVE 12 TO RETURN-CODE
                   END-IF
           END-READ.

       3200-CHECK-OUT-RECORD.
           IF OS-SEND-DT = ZERO
               DISPLAY "TGOUTSF 営業日不正"
               ADD 1 TO WK-REJECT-COUNT
               SET WK-ABEND TO TRUE
               MOVE 8 TO RETURN-CODE
           END-IF

           IF OS-SENDER-BANK = SPACES
               DISPLAY "TGOUTSF 仕向銀行不正"
               ADD 1 TO WK-REJECT-COUNT
               SET WK-ABEND TO TRUE
               MOVE 8 TO RETURN-CODE
           END-IF

           IF OS-SENDER-BRANCH = SPACES
               DISPLAY "TGOUTSF 仕向店番不正"
               ADD 1 TO WK-REJECT-COUNT
               SET WK-ABEND TO TRUE
               MOVE 8 TO RETURN-CODE
           END-IF

           IF OS-SEND-AMT = ZERO
               DISPLAY "TGOUTSF 金額不正"
               ADD 1 TO WK-REJECT-COUNT
               SET WK-ABEND TO TRUE
               MOVE 8 TO RETURN-CODE
           END-IF.

       3300-START-OUT-GROUP.
           SET OS-GRP-INIT TO TRUE
           MOVE OS-SEND-DT TO OS-GRP-DT
           MOVE OS-SENDER-BANK TO OS-GRP-BANK
           MOVE OS-SENDER-BRANCH TO OS-GRP-BRANCH
           MOVE 0 TO OS-GRP-COUNT
           MOVE 0 TO OS-GRP-AMT
           MOVE "OK " TO OS-GRP-STATUS.

       3400-WRITE-OUT-JOURNAL.
           INITIALIZE TGCLJNF-REC
           MOVE OS-GRP-DT TO CJ-CLEARING-DT
           MOVE OS-PREV-SEND-SEQ TO CJ-CENTER-SEQ
           MOVE "O" TO CJ-DIRECTION
           MOVE OS-GRP-BANK TO CJ-BANK-CD
           MOVE OS-GRP-BRANCH TO CJ-BRANCH-CD
           MOVE OS-GRP-COUNT TO CJ-ITEM-COUNT
           MOVE OS-GRP-AMT TO CJ-TOTAL-AMT
           MOVE OS-GRP-STATUS TO CJ-MATCH-STATUS
           MOVE WK-CURRENT-DATE TO CJ-JOURNAL-TS

           WRITE TGCLJNF-REC
           IF FS-TGCLJNF NOT = "00"
               DISPLAY "TGCLJNF 送信ジャーナル書込失敗 ST="
                       FS-TGCLJNF
               SET WK-ABEND TO TRUE
               MOVE 12 TO RETURN-CODE
           ELSE
               ADD 1 TO WK-JOURNAL-COUNT
           END-IF.

       9000-FINAL.
           IF FS-TGINRMF = "00"
               CLOSE TGINRMF
               IF FS-TGINRMF NOT = "00"
                   DISPLAY "TGINRMF クローズ失敗 ST=" FS-TGINRMF
                   MOVE 12 TO RETURN-CODE
               END-IF
           END-IF

           IF FS-TGOUTSF = "00"
               CLOSE TGOUTSF
               IF FS-TGOUTSF NOT = "00"
                   DISPLAY "TGOUTSF クローズ失敗 ST=" FS-TGOUTSF
                   MOVE 12 TO RETURN-CODE
               END-IF
           END-IF

           IF FS-TGCLJNF = "00"
               CLOSE TGCLJNF
               IF FS-TGCLJNF NOT = "00"
                   DISPLAY "TGCLJNF クローズ失敗 ST=" FS-TGCLJNF
                   MOVE 12 TO RETURN-CODE
               END-IF
           END-IF

           IF RETURN-CODE = 0
               DISPLAY "TG240B 正常終了 入力件数="
                       WK-INPUT-COUNT
               DISPLAY "TG240B 正常終了 出力件数="
                       WK-JOURNAL-COUNT
               DISPLAY "TG240B 正常終了 重複件数="
                       WK-DUP-COUNT
           ELSE
               DISPLAY "TG240B 異常終了 入力件数="
                       WK-INPUT-COUNT
               DISPLAY "TG240B 異常終了 出力件数="
                       WK-JOURNAL-COUNT
               DISPLAY "TG240B 異常終了 拒否件数="
                       WK-REJECT-COUNT
           END-IF.
