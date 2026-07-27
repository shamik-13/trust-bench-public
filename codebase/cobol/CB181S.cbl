       IDENTIFICATION DIVISION.
       PROGRAM-ID. CB181S.
      *================================================================*
      *  変更履歴                                                      *
      *  版数   年月日    担当   概要                                  *
      *  01.00  20240315  開発   初版作成                              *
      *  01.01  20240722  保守   少額残高抑制条件追加                  *
      *  01.02  20241108  保守   法定文面判定を延滞六十日へ変更        *
      *================================================================*
      *  督促ランク判定サブ                                            *
      *  延滞情報と残高情報を参照し、通知ランクと推奨チャネルを返す。  *
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CDLATEF ASSIGN TO "CDLATEF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS LAT-CARD-NO
               FILE STATUS IS FS-CDLATEF.
           SELECT CDOSF ASSIGN TO "CDOSF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS OS-CARD-NO
               FILE STATUS IS FS-CDOSF.

       DATA DIVISION.
       FILE SECTION.
       FD  CDLATEF.
           COPY CDLATEC.
       FD  CDOSF.
           COPY CDOSFC.

       WORKING-STORAGE SECTION.
       01  FS-AREA.
           05 FS-CDLATEF              PIC XX VALUE SPACE.
           05 FS-CDOSF                PIC XX VALUE SPACE.

       01  WK-FLAGS.
           05 WK-HARD-ERR-SW          PIC X VALUE "N".
              88 WK-HARD-ERR              VALUE "Y".
           05 WK-LATE-FOUND-SW        PIC X VALUE "N".
              88 WK-LATE-FOUND            VALUE "Y".
           05 WK-OS-FOUND-SW          PIC X VALUE "N".
              88 WK-OS-FOUND              VALUE "Y".
           05 WK-SUPPRESS-SW          PIC X VALUE "N".
              88 WK-SUPPRESS              VALUE "Y".
           05 WK-LEGAL-SW             PIC X VALUE "N".
              88 WK-LEGAL                 VALUE "Y".

       01  WK-CALC.
           05 WK-TOTAL-BAL-AMT        PIC S9(13)V99 COMP-3 VALUE 0.
           05 WK-DELINQ-DAYS          PIC S9(5) COMP-3 VALUE 0.
           05 WK-BASE-RANK            PIC X VALUE SPACE.
           05 WK-PLAN-DIFF            PIC S9(7) COMP-3 VALUE 0.
           05 WK-AS-OF-INT            PIC S9(9) COMP-5 VALUE 0.
           05 WK-PLAN-INT             PIC S9(9) COMP-5 VALUE 0.

       01  WK-MSG.
           05 WK-DISP-MSG             PIC X(80) VALUE SPACE.

       LINKAGE SECTION.
       01  LK-CB181S-PARM.
           05 LK-CARD-NO              PIC X(16).
           05 LK-AS-OF-DT             PIC 9(8).
           05 LK-PAY-PLAN-DT          PIC 9(8).
           05 LK-PAST-NOTICE-CNT      PIC 9(3).
           05 LK-NOTICE-RANK          PIC X.
           05 LK-RECOMMEND-CHANNEL    PIC X(10).
           05 LK-LEGAL-TEXT-REQ       PIC X.
           05 LK-RESULT-CD            PIC X(2).
           05 LK-REASON-TEXT          PIC X(60).

       PROCEDURE DIVISION USING LK-CB181S-PARM.
       0000-MAIN.
           PERFORM 1000-INIT
           IF NOT WK-HARD-ERR
               PERFORM 2000-VALIDATE
           END-IF
           IF NOT WK-HARD-ERR
               PERFORM 3000-OPEN-FILES
           END-IF
           IF NOT WK-HARD-ERR
               PERFORM 4000-READ-FILES
           END-IF
           IF NOT WK-HARD-ERR
               PERFORM 5000-JUDGE-RANK
           END-IF
           PERFORM 9000-CLOSE-FILES
           IF WK-HARD-ERR
               MOVE 8 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF
           GOBACK.

       1000-INIT.
           MOVE "N" TO WK-HARD-ERR-SW
           MOVE "N" TO WK-LATE-FOUND-SW
           MOVE "N" TO WK-OS-FOUND-SW
           MOVE "N" TO WK-SUPPRESS-SW
           MOVE "N" TO WK-LEGAL-SW
           MOVE ZERO TO WK-TOTAL-BAL-AMT
           MOVE ZERO TO WK-DELINQ-DAYS
           MOVE SPACE TO WK-BASE-RANK
           MOVE SPACE TO LK-NOTICE-RANK
           MOVE SPACE TO LK-RECOMMEND-CHANNEL
           MOVE "N" TO LK-LEGAL-TEXT-REQ
           MOVE "00" TO LK-RESULT-CD
           MOVE SPACE TO LK-REASON-TEXT.

       2000-VALIDATE.
           IF LK-CARD-NO = SPACE
               MOVE "Y" TO WK-HARD-ERR-SW
               MOVE "91" TO LK-RESULT-CD
               MOVE "カード番号未設定" TO LK-REASON-TEXT
               DISPLAY "CB181S カード番号未設定"
           END-IF
           IF NOT WK-HARD-ERR
               IF LK-AS-OF-DT < 19000101 OR LK-AS-OF-DT > 20991231
                   MOVE "Y" TO WK-HARD-ERR-SW
                   MOVE "91" TO LK-RESULT-CD
                   MOVE "基準日不正" TO LK-REASON-TEXT
                   DISPLAY "CB181S 基準日不正"
               END-IF
           END-IF
           IF NOT WK-HARD-ERR
               IF LK-PAY-PLAN-DT NOT = ZERO
                  AND (LK-PAY-PLAN-DT < 19000101
                   OR  LK-PAY-PLAN-DT > 20991231)
                   MOVE "Y" TO WK-HARD-ERR-SW
                   MOVE "91" TO LK-RESULT-CD
                   MOVE "入金予定日不正" TO LK-REASON-TEXT
                   DISPLAY "CB181S 入金予定日不正"
               END-IF
           END-IF.

       3000-OPEN-FILES.
           OPEN INPUT CDLATEF
           IF FS-CDLATEF NOT = "00"
               MOVE "Y" TO WK-HARD-ERR-SW
               MOVE "92" TO LK-RESULT-CD
               STRING "CDLATEF オープン失敗 ST="
                      FS-CDLATEF
                 DELIMITED BY SIZE INTO WK-DISP-MSG
               END-STRING
               DISPLAY WK-DISP-MSG
           END-IF
           IF NOT WK-HARD-ERR
               OPEN INPUT CDOSF
               IF FS-CDOSF NOT = "00"
                   MOVE "Y" TO WK-HARD-ERR-SW
                   MOVE "92" TO LK-RESULT-CD
                   STRING "CDOSF オープン失敗 ST="
                          FS-CDOSF
                     DELIMITED BY SIZE INTO WK-DISP-MSG
                   END-STRING
                   DISPLAY WK-DISP-MSG
               END-IF
           END-IF.

       4000-READ-FILES.
           MOVE LK-CARD-NO TO LAT-CARD-NO
           READ CDLATEF KEY IS LAT-CARD-NO
               INVALID KEY
                   IF FS-CDLATEF = "23"
                       MOVE "N" TO WK-LATE-FOUND-SW
                   ELSE
                       MOVE "Y" TO WK-HARD-ERR-SW
                       MOVE "93" TO LK-RESULT-CD
                       STRING "CDLATEF 読込失敗 ST="
                              FS-CDLATEF
                         DELIMITED BY SIZE INTO WK-DISP-MSG
                       END-STRING
                       DISPLAY WK-DISP-MSG
                   END-IF
               NOT INVALID KEY
                   MOVE "Y" TO WK-LATE-FOUND-SW
           END-READ
           IF NOT WK-HARD-ERR
               MOVE LK-CARD-NO TO OS-CARD-NO
               READ CDOSF KEY IS OS-CARD-NO
                   INVALID KEY
                       IF FS-CDOSF = "23"
                           MOVE "N" TO WK-OS-FOUND-SW
                       ELSE
                           MOVE "Y" TO WK-HARD-ERR-SW
                           MOVE "93" TO LK-RESULT-CD
                           STRING "CDOSF 読込失敗 ST="
                                  FS-CDOSF
                             DELIMITED BY SIZE INTO WK-DISP-MSG
                           END-STRING
                           DISPLAY WK-DISP-MSG
                       END-IF
                   NOT INVALID KEY
                       MOVE "Y" TO WK-OS-FOUND-SW
               END-READ
           END-IF
           IF NOT WK-HARD-ERR
               IF NOT WK-LATE-FOUND
                   MOVE "01" TO LK-RESULT-CD
                   MOVE "延滞情報なし" TO LK-REASON-TEXT
               END-IF
               IF WK-LATE-FOUND AND NOT WK-OS-FOUND
                   MOVE "01" TO LK-RESULT-CD
                   MOVE "残高情報なし" TO LK-REASON-TEXT
               END-IF
           END-IF.

       5000-JUDGE-RANK.
           IF LK-RESULT-CD = "01"
               MOVE "0" TO LK-NOTICE-RANK
               MOVE "不要" TO LK-RECOMMEND-CHANNEL
               MOVE "N" TO LK-LEGAL-TEXT-REQ
           ELSE
               PERFORM 5100-CALCULATE
               PERFORM 5200-BASE-RANK
               PERFORM 5300-SUPPRESS-RANK
               PERFORM 5400-SET-OUTPUT
           END-IF.

       5100-CALCULATE.
           COMPUTE WK-TOTAL-BAL-AMT =
                   OS-FEE-BAL-AMT
                 + OS-INTEREST-BAL-AMT
                 + OS-PRINCIPAL-BAL-AMT
           MOVE LAT-DELINQ-DAYS TO WK-DELINQ-DAYS
           IF WK-DELINQ-DAYS < ZERO
               MOVE ZERO TO WK-DELINQ-DAYS
           END-IF
           IF LK-PAY-PLAN-DT NOT = ZERO
               COMPUTE WK-AS-OF-INT =
                   FUNCTION INTEGER-OF-DATE(LK-AS-OF-DT)
               COMPUTE WK-PLAN-INT =
                   FUNCTION INTEGER-OF-DATE(LK-PAY-PLAN-DT)
               COMPUTE WK-PLAN-DIFF = WK-PLAN-INT - WK-AS-OF-INT
           ELSE
               MOVE 9999999 TO WK-PLAN-DIFF
           END-IF.

       5200-BASE-RANK.
           EVALUATE TRUE
               WHEN WK-DELINQ-DAYS >= 60
                   MOVE "D" TO WK-BASE-RANK
                   MOVE "Y" TO WK-LEGAL-SW
               WHEN WK-DELINQ-DAYS >= 30
                   MOVE "C" TO WK-BASE-RANK
               WHEN WK-DELINQ-DAYS >= 15
                   MOVE "B" TO WK-BASE-RANK
               WHEN WK-DELINQ-DAYS >= 1
                   MOVE "A" TO WK-BASE-RANK
               WHEN OTHER
                   MOVE "0" TO WK-BASE-RANK
           END-EVALUATE
           IF WK-BASE-RANK = "B"
              AND LK-PAST-NOTICE-CNT >= 3
               MOVE "C" TO WK-BASE-RANK
           END-IF
           IF WK-BASE-RANK = "C"
              AND LK-PAST-NOTICE-CNT >= 5
              AND WK-DELINQ-DAYS >= 45
               MOVE "D" TO WK-BASE-RANK
               MOVE "Y" TO WK-LEGAL-SW
           END-IF.

       5300-SUPPRESS-RANK.
           IF WK-TOTAL-BAL-AMT <= 1000
              AND WK-BASE-RANK NOT = "0"
               MOVE "Y" TO WK-SUPPRESS-SW
               MOVE "A" TO WK-BASE-RANK
               MOVE "N" TO WK-LEGAL-SW
           END-IF
           IF WK-PLAN-DIFF >= 0
              AND WK-PLAN-DIFF <= 3
              AND WK-BASE-RANK NOT = "0"
               MOVE "Y" TO WK-SUPPRESS-SW
               IF WK-BASE-RANK = "D"
                   MOVE "C" TO WK-BASE-RANK
               ELSE
                   IF WK-BASE-RANK = "C"
                       MOVE "B" TO WK-BASE-RANK
                   ELSE
                       MOVE "A" TO WK-BASE-RANK
                   END-IF
               END-IF
               IF WK-DELINQ-DAYS < 60
                   MOVE "N" TO WK-LEGAL-SW
               END-IF
           END-IF.

       5400-SET-OUTPUT.
           MOVE WK-BASE-RANK TO LK-NOTICE-RANK
           IF WK-LEGAL
               MOVE "Y" TO LK-LEGAL-TEXT-REQ
           ELSE
               MOVE "N" TO LK-LEGAL-TEXT-REQ
           END-IF
           EVALUATE WK-BASE-RANK
               WHEN "0"
                   MOVE "不要" TO LK-RECOMMEND-CHANNEL
                   MOVE "延滞なし" TO LK-REASON-TEXT
               WHEN "A"
                   MOVE "SMS" TO LK-RECOMMEND-CHANNEL
                   MOVE "軽度延滞" TO LK-REASON-TEXT
               WHEN "B"
                   MOVE "ハガキ" TO LK-RECOMMEND-CHANNEL
                   MOVE "通常督促" TO LK-REASON-TEXT
               WHEN "C"
                   MOVE "電話" TO LK-RECOMMEND-CHANNEL
                   MOVE "重点督促" TO LK-REASON-TEXT
               WHEN "D"
                   MOVE "法定文書" TO LK-RECOMMEND-CHANNEL
                   MOVE "法定文面対象" TO LK-REASON-TEXT
               WHEN OTHER
                   MOVE "不要" TO LK-RECOMMEND-CHANNEL
                   MOVE "判定対象外" TO LK-REASON-TEXT
           END-EVALUATE
           IF WK-SUPPRESS
               STRING LK-REASON-TEXT DELIMITED BY SPACE
                      "・抑制済" DELIMITED BY SIZE
                 INTO LK-REASON-TEXT
               END-STRING
           END-IF.

       9000-CLOSE-FILES.
           IF FS-CDLATEF = "00"
               CLOSE CDLATEF
               IF FS-CDLATEF NOT = "00"
                   MOVE "Y" TO WK-HARD-ERR-SW
                   MOVE "94" TO LK-RESULT-CD
                   STRING "CDLATEF クローズ失敗 ST="
                          FS-CDLATEF
                     DELIMITED BY SIZE INTO WK-DISP-MSG
                   END-STRING
                   DISPLAY WK-DISP-MSG
               END-IF
           END-IF
           IF FS-CDOSF = "00"
               CLOSE CDOSF
               IF FS-CDOSF NOT = "00"
                   MOVE "Y" TO WK-HARD-ERR-SW
                   MOVE "94" TO LK-RESULT-CD
                   STRING "CDOSF クローズ失敗 ST="
                          FS-CDOSF
                     DELIMITED BY SIZE INTO WK-DISP-MSG
                   END-STRING
                   DISPLAY WK-DISP-MSG
               END-IF
           END-IF.
