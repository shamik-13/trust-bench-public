       IDENTIFICATION DIVISION.
       PROGRAM-ID. TG214B.
      *---------------------------------------------------------------*
      * 変更履歴                                                      *
      * 版数    年月日        担当                  概要              *
      * 1.00    平成30年04月  山本 修一 (E-007)     新規作成          *
      * 1.10    令和02年10月  高橋 直樹 (E-013)     照合条件見直し    *
      * 1.20    令和05年06月  佐藤 真理 (E-021)     表示文言整備      *
      *---------------------------------------------------------------*

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TGINRMF ASSIGN TO "TGINRMF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-TGINRMF.
           SELECT KZACCTF ASSIGN TO "KZACCTF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS AC-ACCT-NO
               FILE STATUS IS FS-KZACCTF.
           SELECT TGREJLF ASSIGN TO "TGREJLF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-TGREJLF.

       DATA DIVISION.
       FILE SECTION.

       FD  TGINRMF.
           COPY TGINRMFC.

       FD  KZACCTF.
           COPY KZACCTC2.

       FD  TGREJLF.
           COPY TGREJLFC.

       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID              PIC X(08) VALUE "TG214B".
       01  WS-EOF-SW                  PIC X(01) VALUE "N".
           88  EOF-TGINRMF                      VALUE "Y".
           88  NOT-EOF-TGINRMF                  VALUE "N".

       01  FS-TGINRMF                 PIC X(02) VALUE "00".
       01  FS-KZACCTF                 PIC X(02) VALUE "00".
       01  FS-TGREJLF                 PIC X(02) VALUE "00".

       01  WS-REJ-REASON              PIC X(02) VALUE SPACES.
       01  WS-PAYEE-NORM-KANA         PIC X(60) VALUE SPACES.
       01  WS-MASTER-NORM-KANA        PIC X(60) VALUE SPACES.
       01  WS-HARD-ERROR-SW           PIC X(01) VALUE "N".
           88  HARD-ERROR                       VALUE "Y".
           88  NOT-HARD-ERROR                   VALUE "N".

       01  WS-MAX-REMIT-AMT           PIC 9(13)
           VALUE 9999999999999.

       01  WS-READ-CNT                PIC 9(09) VALUE ZERO.
       01  WS-ACCEPT-CNT              PIC 9(09) VALUE ZERO.
       01  WS-REJECT-CNT              PIC 9(09) VALUE ZERO.
       01  WS-SKIP-CNT                PIC 9(09) VALUE ZERO.

       01  WS-DISPLAY-AMT             PIC ZZZ,ZZZ,ZZZ,ZZ9.
       01  WS-DISPLAY-CNT             PIC ZZZ,ZZZ,ZZ9.

           COPY LK-KANA-PARM.
           COPY LK-REJLOG-PARM.

       PROCEDURE DIVISION.
       0000-MAIN SECTION.
       0000-START.
           MOVE 0 TO RETURN-CODE
           PERFORM 1000-OPEN-FILES
           IF NOT HARD-ERROR
               PERFORM 2000-PROCESS-FILE
                   UNTIL EOF-TGINRMF OR HARD-ERROR
           END-IF
           PERFORM 9000-CLOSE-FILES
           IF HARD-ERROR
               MOVE 8 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
               PERFORM 9100-DISPLAY-END
           END-IF
           GOBACK.

       1000-OPEN-FILES SECTION.
       1000-START.
           OPEN INPUT TGINRMF
           IF FS-TGINRMF NOT = "00"
               DISPLAY "TGINRMF オープン ST=" FS-TGINRMF
               SET HARD-ERROR TO TRUE
           END-IF

           IF NOT HARD-ERROR
               OPEN INPUT KZACCTF
               IF FS-KZACCTF NOT = "00"
                   DISPLAY "KZACCTF オープン ST=" FS-KZACCTF
                   SET HARD-ERROR TO TRUE
               END-IF
           END-IF

           IF NOT HARD-ERROR
               OPEN OUTPUT TGREJLF
               IF FS-TGREJLF NOT = "00"
                   DISPLAY "TGREJLF オープン ST=" FS-TGREJLF
                   SET HARD-ERROR TO TRUE
               END-IF
           END-IF.

       2000-PROCESS-FILE SECTION.
       2000-START.
           READ TGINRMF
               AT END
                   SET EOF-TGINRMF TO TRUE
               NOT AT END
                   IF FS-TGINRMF = "00"
                       ADD 1 TO WS-READ-CNT
                       PERFORM 3000-DISPATCH-REMIT
                   ELSE
                       DISPLAY "TGINRMF 読込 ST=" FS-TGINRMF
                       SET HARD-ERROR TO TRUE
                   END-IF
           END-READ.

       3000-DISPATCH-REMIT SECTION.
       3000-START.
           EVALUATE IR-REMIT-TYPE
               WHEN "10"
                   PERFORM 4000-VALIDATE-DEPOSIT
               WHEN OTHER
                   ADD 1 TO WS-SKIP-CNT
                   DISPLAY "スキップ 種別=" IR-REMIT-TYPE
           END-EVALUATE.

       4000-VALIDATE-DEPOSIT SECTION.
       4000-START.
           MOVE SPACES TO WS-REJ-REASON
           IF IR-PAYEE-ACCT-NO = SPACES
               ADD 1 TO WS-SKIP-CNT
               DISPLAY "受取人口座なし"
           ELSE
               MOVE IR-PAYEE-ACCT-NO TO AC-ACCT-NO
               READ KZACCTF KEY IS AC-ACCT-NO
               EVALUATE FS-KZACCTF
                   WHEN "00"
                       PERFORM 4100-CHECK-MASTER
                   WHEN "23"
                       ADD 1 TO WS-SKIP-CNT
                       DISPLAY "口座未検出"
                   WHEN OTHER
                       DISPLAY "KZACCTF 読込 ST=" FS-KZACCTF
                       SET HARD-ERROR TO TRUE
               END-EVALUATE
           END-IF.

       4100-CHECK-MASTER SECTION.
       4100-START.
           IF AC-STATUS NOT = "0"
               ADD 1 TO WS-SKIP-CNT
               DISPLAY "口座状態不正"
           ELSE
               IF AC-ACCT-NAME-KANA = SPACES
                   ADD 1 TO WS-SKIP-CNT
                   DISPLAY "マスタカナなし"
               ELSE
                   IF IR-PAYEE-NAME-KANA = SPACES
                       ADD 1 TO WS-SKIP-CNT
                       DISPLAY "受取人カナなし"
                   ELSE
                       IF IR-REMIT-AMT = ZERO
                          OR IR-REMIT-AMT > WS-MAX-REMIT-AMT
                           ADD 1 TO WS-SKIP-CNT
                           MOVE IR-REMIT-AMT TO WS-DISPLAY-AMT
                           DISPLAY "金額不正=" WS-DISPLAY-AMT
                       ELSE
                           PERFORM 4200-CHECK-KANA-NAME
                       END-IF
                   END-IF
               END-IF
           END-IF.

       4200-CHECK-KANA-NAME SECTION.
       4200-START.
           MOVE SPACES TO WS-PAYEE-NORM-KANA
           MOVE SPACES TO WS-MASTER-NORM-KANA
           MOVE SPACES TO LK-RAW-KANA
           MOVE SPACES TO LK-NORM-KANA
           MOVE SPACES TO LK-KANA-RET

           MOVE IR-PAYEE-NAME-KANA TO LK-RAW-KANA
           CALL "TG912S" USING LK-KANA-PARM
           IF LK-KANA-RET = "00"
               MOVE LK-NORM-KANA TO WS-PAYEE-NORM-KANA
           ELSE
               ADD 1 TO WS-SKIP-CNT
               DISPLAY "受取人カナ ST=" LK-KANA-RET
           END-IF

           IF LK-KANA-RET = "00"
               MOVE SPACES TO LK-RAW-KANA
               MOVE SPACES TO LK-NORM-KANA
               MOVE SPACES TO LK-KANA-RET
               MOVE AC-ACCT-NAME-KANA TO LK-RAW-KANA
               CALL "TG912S" USING LK-KANA-PARM
               IF LK-KANA-RET = "00"
                   MOVE LK-NORM-KANA TO WS-MASTER-NORM-KANA
                   IF WS-PAYEE-NORM-KANA NOT = WS-MASTER-NORM-KANA
                       MOVE "N1" TO WS-REJ-REASON
                       PERFORM 5000-WRITE-REJECT
                   ELSE
                       ADD 1 TO WS-ACCEPT-CNT
                   END-IF
               ELSE
                   ADD 1 TO WS-SKIP-CNT
                   DISPLAY "マスタカナ ST=" LK-KANA-RET
               END-IF
           END-IF.

       5000-WRITE-REJECT SECTION.
       5000-START.
           MOVE SPACES TO LK-RL-PROGRAM-ID
           MOVE SPACES TO LK-RL-REASON
           MOVE SPACES TO LK-RL-LOG-TS
           MOVE SPACES TO LK-RL-RET
           MOVE WS-PROGRAM-ID TO LK-RL-PROGRAM-ID
           MOVE WS-REJ-REASON TO LK-RL-REASON
           CALL "TG920S" USING LK-REJLOG-PARM
           IF LK-RL-RET NOT = "00"
               ADD 1 TO WS-SKIP-CNT
               DISPLAY "REJLOG ST=" LK-RL-RET
           ELSE
               INITIALIZE TGREJLF-REC
               MOVE IR-REMIT-DT        TO RJ-REMIT-DT
               MOVE IR-CENTER-SEQ      TO RJ-CENTER-SEQ
               MOVE WS-REJ-REASON      TO RJ-REJ-REASON
               MOVE WS-PROGRAM-ID      TO RJ-PROGRAM-ID
               MOVE IR-PAYEE-ACCT-NO   TO RJ-PAYEE-ACCT-NO
               MOVE IR-PAYEE-NAME-KANA TO RJ-PAYEE-NAME-KANA
               MOVE AC-ACCT-NAME-KANA  TO RJ-MASTER-NAME-KANA
               MOVE IR-REMIT-AMT       TO RJ-REJ-AMT
               MOVE LK-RL-LOG-TS       TO RJ-LOG-TS
               WRITE TGREJLF-REC
               IF FS-TGREJLF = "00"
                   ADD 1 TO WS-REJECT-CNT
               ELSE
                   DISPLAY "TGREJLF 書込 ST=" FS-TGREJLF
                   SET HARD-ERROR TO TRUE
               END-IF
           END-IF.

       9000-CLOSE-FILES SECTION.
       9000-START.
           CLOSE TGINRMF
           IF FS-TGINRMF NOT = "00"
              AND FS-TGINRMF NOT = "42"
               DISPLAY "TGINRMF クローズ ST=" FS-TGINRMF
               SET HARD-ERROR TO TRUE
           END-IF

           CLOSE KZACCTF
           IF FS-KZACCTF NOT = "00"
              AND FS-KZACCTF NOT = "42"
               DISPLAY "KZACCTF クローズ ST=" FS-KZACCTF
               SET HARD-ERROR TO TRUE
           END-IF

           CLOSE TGREJLF
           IF FS-TGREJLF NOT = "00"
              AND FS-TGREJLF NOT = "42"
               DISPLAY "TGREJLF クローズ ST=" FS-TGREJLF
               SET HARD-ERROR TO TRUE
           END-IF.

       9100-DISPLAY-END SECTION.
       9100-START.
           MOVE WS-READ-CNT TO WS-DISPLAY-CNT
           DISPLAY "読込件数=" WS-DISPLAY-CNT
           MOVE WS-ACCEPT-CNT TO WS-DISPLAY-CNT
           DISPLAY "受入件数=" WS-DISPLAY-CNT
           MOVE WS-REJECT-CNT TO WS-DISPLAY-CNT
           DISPLAY "拒否件数=" WS-DISPLAY-CNT
           MOVE WS-SKIP-CNT TO WS-DISPLAY-CNT
           DISPLAY "スキップ件数=" WS-DISPLAY-CNT.
