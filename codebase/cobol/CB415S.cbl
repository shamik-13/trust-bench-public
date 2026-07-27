       IDENTIFICATION DIVISION.
       PROGRAM-ID. CB415S.
       AUTHOR. TRUST-BATCH.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CDMERCF
               ASSIGN       TO "CDMERCF"
               ORGANIZATION IS INDEXED
               ACCESS MODE  IS DYNAMIC
               RECORD KEY   IS MC-MERCHANT-CODE
               FILE STATUS  IS WK-CDMERCF-ST.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  CDMERCF.
           COPY CDMERCC.
      *
       WORKING-STORAGE SECTION.
       01  WK-CDMERCF-ST              PIC XX.
       01  WK-OPENED-SW               PIC X VALUE "0".
           88  WK-OPENED                    VALUE "1".
       01  WK-HARD-ERROR-SW           PIC X VALUE "0".
           88  WK-HARD-ERROR                VALUE "1".
       01  WK-EDIT-ERROR-SW           PIC X VALUE "0".
           88  WK-EDIT-ERROR                VALUE "1".
       01  WK-ACCOUNT-NO-NUM          PIC 9(7).
       01  WK-AMOUNT-ABS              PIC S9(11) COMP-3.
      *
       01  CN-RESULT.
           05  CN-SETTLE              PIC X VALUE "1".
           05  CN-HOLD                PIC X VALUE "2".
           05  CN-EXCLUDE             PIC X VALUE "3".
      *
       LINKAGE SECTION.
       01  LK-CB415S-AREA.
           05  LK-MERCHANT-CODE       PIC X(10).
           05  LK-SALES-ATTR          PIC X.
           05  LK-SALES-AMOUNT        PIC S9(11) COMP-3.
           05  LK-RESULT-KBN          PIC X.
           05  LK-REASON-CD           PIC X(4).
           05  LK-REASON-TEXT         PIC X(40).
           05  LK-SETTLE-BANK-CD      PIC X(4).
           05  LK-SETTLE-ACCOUNT-NO   PIC X(7).
           05  LK-FEE-PLAN-CD         PIC X(6).
      *
       PROCEDURE DIVISION USING LK-CB415S-AREA.
      *
       0000-MAIN SECTION.
       0000-START.
           MOVE 0 TO RETURN-CODE
           PERFORM 1000-INIT
           PERFORM 2000-EDIT
           IF NOT WK-EDIT-ERROR
              PERFORM 3000-READ-MERCHANT
           END-IF
           IF NOT WK-EDIT-ERROR
              AND NOT WK-HARD-ERROR
              PERFORM 4000-JUDGE
           END-IF
           PERFORM 9000-FINAL
           GOBACK.
      *
       1000-INIT SECTION.
       1000-START.
           INITIALIZE LK-RESULT-KBN
                      LK-REASON-CD
                      LK-REASON-TEXT
                      LK-SETTLE-BANK-CD
                      LK-SETTLE-ACCOUNT-NO
                      LK-FEE-PLAN-CD
                      WK-CDMERCF-ST
                      WK-ACCOUNT-NO-NUM
                      WK-AMOUNT-ABS
           MOVE "0" TO WK-OPENED-SW
                       WK-HARD-ERROR-SW
                       WK-EDIT-ERROR-SW.
      *
       2000-EDIT SECTION.
       2000-START.
           IF LK-MERCHANT-CODE = SPACE
              MOVE CN-EXCLUDE TO LK-RESULT-KBN
              MOVE "E001" TO LK-REASON-CD
              MOVE "加盟店コード未設定" TO LK-REASON-TEXT
              SET WK-EDIT-ERROR TO TRUE
           END-IF
      *
           IF NOT WK-EDIT-ERROR
              IF LK-SALES-ATTR NOT = "1"
                 AND LK-SALES-ATTR NOT = "2"
                 AND LK-SALES-ATTR NOT = "3"
                 MOVE CN-EXCLUDE TO LK-RESULT-KBN
                 MOVE "E002" TO LK-REASON-CD
                 MOVE "売上属性不正" TO LK-REASON-TEXT
                 SET WK-EDIT-ERROR TO TRUE
              END-IF
           END-IF
      *
           IF NOT WK-EDIT-ERROR
              COMPUTE WK-AMOUNT-ABS = FUNCTION ABS(LK-SALES-AMOUNT)
              IF WK-AMOUNT-ABS = 0
                 MOVE CN-EXCLUDE TO LK-RESULT-KBN
                 MOVE "E003" TO LK-REASON-CD
                 MOVE "売上金額ゼロ" TO LK-REASON-TEXT
                 SET WK-EDIT-ERROR TO TRUE
              END-IF
           END-IF.
      *
       3000-READ-MERCHANT SECTION.
       3000-START.
           OPEN INPUT CDMERCF
           IF WK-CDMERCF-ST NOT = "00"
              DISPLAY "CDMERCF オープン失敗 ST=" WK-CDMERCF-ST
              MOVE 8 TO RETURN-CODE
              SET WK-HARD-ERROR TO TRUE
           ELSE
              MOVE "1" TO WK-OPENED-SW
              MOVE LK-MERCHANT-CODE TO MC-MERCHANT-CODE
              READ CDMERCF
                 INVALID KEY
                    MOVE CN-HOLD TO LK-RESULT-KBN
                    MOVE "H001" TO LK-REASON-CD
                    MOVE "加盟店未登録" TO LK-REASON-TEXT
                    SET WK-EDIT-ERROR TO TRUE
                 NOT INVALID KEY
                    CONTINUE
              END-READ
              IF WK-CDMERCF-ST NOT = "00"
                 AND WK-CDMERCF-ST NOT = "23"
                 DISPLAY "CDMERCF 読込失敗 ST=" WK-CDMERCF-ST
                 MOVE 8 TO RETURN-CODE
                 SET WK-HARD-ERROR TO TRUE
              END-IF
           END-IF.
      *
       4000-JUDGE SECTION.
       4000-START.
           MOVE MC-SETTLE-BANK-CD    TO LK-SETTLE-BANK-CD
           MOVE MC-SETTLE-ACCOUNT-NO TO LK-SETTLE-ACCOUNT-NO
           MOVE MC-FEE-PLAN-CD       TO LK-FEE-PLAN-CD
      *
           IF MC-MERCHANT-STATUS = "9"
              MOVE CN-HOLD TO LK-RESULT-KBN
              MOVE "H002" TO LK-REASON-CD
              MOVE "加盟店停止中" TO LK-REASON-TEXT
           ELSE
              IF MC-MERCHANT-STATUS NOT = "1"
                 MOVE CN-HOLD TO LK-RESULT-KBN
                 MOVE "H003" TO LK-REASON-CD
                 MOVE "加盟店状態不正" TO LK-REASON-TEXT
              ELSE
                 PERFORM 4100-JUDGE-ACCOUNT
              END-IF
           END-IF.
      *
       4100-JUDGE-ACCOUNT SECTION.
       4100-START.
           IF MC-SETTLE-BANK-CD = SPACE
              OR MC-SETTLE-ACCOUNT-NO = SPACE
              MOVE CN-HOLD TO LK-RESULT-KBN
              MOVE "H004" TO LK-REASON-CD
              MOVE "精算口座未登録" TO LK-REASON-TEXT
           ELSE
              MOVE MC-SETTLE-ACCOUNT-NO TO WK-ACCOUNT-NO-NUM
              IF WK-ACCOUNT-NO-NUM = 0
                 MOVE CN-HOLD TO LK-RESULT-KBN
                 MOVE "H005" TO LK-REASON-CD
                 MOVE "精算口座番号不正" TO LK-REASON-TEXT
              ELSE
                 PERFORM 4200-JUDGE-FEE
              END-IF
           END-IF.
      *
       4200-JUDGE-FEE SECTION.
       4200-START.
           IF MC-FEE-PLAN-CD = SPACE
              OR MC-FEE-PLAN-CD = ZERO
              MOVE CN-HOLD TO LK-RESULT-KBN
              MOVE "H006" TO LK-REASON-CD
              MOVE "手数料プラン未設定" TO LK-REASON-TEXT
           ELSE
              PERFORM 4300-JUDGE-SALES
           END-IF.
      *
       4300-JUDGE-SALES SECTION.
       4300-START.
           IF LK-SALES-ATTR = "3"
              MOVE CN-EXCLUDE TO LK-RESULT-KBN
              MOVE "X001" TO LK-REASON-CD
              MOVE "取消売上は控除対象外" TO LK-REASON-TEXT
           ELSE
              IF LK-SALES-AMOUNT < 0
                 MOVE CN-EXCLUDE TO LK-RESULT-KBN
                 MOVE "X002" TO LK-REASON-CD
                 MOVE "負数売上は控除対象外" TO LK-REASON-TEXT
              ELSE
                 MOVE CN-SETTLE TO LK-RESULT-KBN
                 MOVE "0000" TO LK-REASON-CD
                 MOVE "精算対象" TO LK-REASON-TEXT
              END-IF
           END-IF.
      *
       9000-FINAL SECTION.
       9000-START.
           IF WK-OPENED
              CLOSE CDMERCF
              IF WK-CDMERCF-ST NOT = "00"
                 DISPLAY "CDMERCF クローズ失敗 ST=" WK-CDMERCF-ST
                 MOVE 8 TO RETURN-CODE
              END-IF
           END-IF.
