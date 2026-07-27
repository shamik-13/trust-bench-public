       IDENTIFICATION DIVISION.
       PROGRAM-ID. TG541S.
      * 変更履歴
      * 版数  年月日      担当                         概要
      * 1.00  平成28年04月  システム部 情報系チーム      新規作成
      * 1.01  令和02年10月  システム部 対外系チーム      電文検査見直し
      * 1.02  令和05年06月  システム部 情報系/対外系チーム 保守対応
       AUTHOR. ST01.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZACCTF ASSIGN TO "KZACCTF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS AC-ACCT-NO
               FILE STATUS IS WS-KZACCTF-ST.
           SELECT TGBANKF ASSIGN TO "TGBANKF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS BK-COUNTER-BANK
               FILE STATUS IS WS-TGBANKF-ST.

       DATA DIVISION.
       FILE SECTION.
       FD  KZACCTF.
           COPY KZACCTC2.

       FD  TGBANKF.
           COPY TGBANKFC.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05 WS-KZACCTF-ST        PIC XX VALUE SPACE.
           05 WS-TGBANKF-ST        PIC XX VALUE SPACE.

       01  WS-SWITCHES.
           05 WS-HARD-ERROR-SW     PIC X VALUE '0'.
              88 HARD-ERROR        VALUE '1'.
           05 WS-DETAIL-ERROR-SW   PIC X VALUE '0'.
              88 DETAIL-ERROR      VALUE '1'.

       01  WS-EDIT-WORK.
           05 WS-VALUE-DT-N        PIC 9(8) VALUE ZERO.
           05 WS-CENTER-SEQ-N      PIC 9(10) VALUE ZERO.
           05 WS-AMOUNT-N          PIC 9(11) VALUE ZERO.
           05 WS-VALID-FROM-N      PIC 9(8) VALUE ZERO.
           05 WS-VALID-TO-N        PIC 9(8) VALUE ZERO.

       01  WS-DATE-WORK.
           05 WS-YY                PIC 9(4) VALUE ZERO.
           05 WS-MM                PIC 9(2) VALUE ZERO.
           05 WS-DD                PIC 9(2) VALUE ZERO.
           05 WS-LEAP-SW           PIC X VALUE '0'.
              88 LEAP-YEAR         VALUE '1'.
           05 WS-DAYS-IN-MONTH    PIC 9(2) VALUE ZERO.
           05 WS-QUOTIENT          PIC 9(4) VALUE ZERO.
           05 WS-REM4              PIC 9(4) VALUE ZERO.
           05 WS-REM100            PIC 9(4) VALUE ZERO.
           05 WS-REM400            PIC 9(4) VALUE ZERO.

       01  WS-OPEN-SW.
           05 WS-KZACCTF-OPEN-SW   PIC X VALUE '0'.
              88 KZACCTF-OPENED    VALUE '1'.
           05 WS-TGBANKF-OPEN-SW   PIC X VALUE '0'.
              88 TGBANKF-OPENED    VALUE '1'.

       LINKAGE SECTION.
       01  LK-TG541S-PARM.
           05 LK-RAW-REC.
              10 LK-RAW-VALUE-DT       PIC X(8).
              10 LK-RAW-CENTER-SEQ     PIC X(10).
              10 LK-RAW-COUNTER-BANK   PIC X(4).
              10 LK-RAW-ACCT-NO        PIC X(12).
              10 LK-RAW-AMOUNT         PIC X(11).
              10 LK-RAW-NAME-KANA      PIC X(40).
              10 LK-RAW-FILLER         PIC X(35).
           05 LK-EDIT-DETAIL.
              10 LK-ED-VALUE-DT        PIC 9(8).
              10 LK-ED-CENTER-SEQ      PIC 9(10).
              10 LK-ED-COUNTER-BANK    PIC X(4).
              10 LK-ED-ACCT-NO         PIC X(12).
              10 LK-ED-AMOUNT          PIC 9(11).
              10 LK-ED-NAME-KANA       PIC X(40).
              10 LK-ED-BRANCH          PIC X(3).
              10 LK-ED-ACCT-TYPE       PIC X(2).
              10 LK-ED-CUSTOMER-ID     PIC X(10).
              10 LK-ED-BANK-NAME       PIC X(40).
              10 LK-ED-CLEARING-GRP    PIC X(2).
           05 LK-ERROR-AREA.
              10 LK-ERR-COUNT          PIC 9(2).
              10 LK-ERR-VALUE-DT       PIC X(4).
              10 LK-ERR-CENTER-SEQ     PIC X(4).
              10 LK-ERR-COUNTER-BANK   PIC X(4).
              10 LK-ERR-ACCT-NO        PIC X(4).
              10 LK-ERR-AMOUNT         PIC X(4).
              10 LK-ERR-NAME-KANA      PIC X(4).
              10 LK-ERR-KZACCTF        PIC X(4).
              10 LK-ERR-TGBANKF        PIC X(4).
           05 LK-RETURN-STATUS          PIC X(2).
              88 LK-STATUS-NORMAL      VALUE '00'.
              88 LK-STATUS-ITEM-ERR    VALUE '04'.
              88 LK-STATUS-HARD-ERR    VALUE '08'.

       PROCEDURE DIVISION USING LK-TG541S-PARM.
       0000-MAIN SECTION.
           PERFORM 1000-INIT
           IF NOT HARD-ERROR
               PERFORM 2000-EDIT-RAW
           END-IF
           IF NOT HARD-ERROR AND NOT DETAIL-ERROR
               PERFORM 3000-CHECK-ACCOUNT
           END-IF
           IF NOT HARD-ERROR AND NOT DETAIL-ERROR
               PERFORM 4000-CHECK-BANK
           END-IF
           PERFORM 9000-FINAL
           GOBACK.

       1000-INIT SECTION.
           MOVE ZERO TO RETURN-CODE
           MOVE '0' TO WS-HARD-ERROR-SW
           MOVE '0' TO WS-DETAIL-ERROR-SW
           INITIALIZE LK-EDIT-DETAIL
           MOVE SPACES TO LK-ERROR-AREA
           MOVE ZERO TO LK-ERR-COUNT
           MOVE '00' TO LK-RETURN-STATUS

           OPEN INPUT KZACCTF
           IF WS-KZACCTF-ST = '00'
               MOVE '1' TO WS-KZACCTF-OPEN-SW
           ELSE
               MOVE '1' TO WS-HARD-ERROR-SW
               MOVE '08' TO LK-RETURN-STATUS
               MOVE 'E900' TO LK-ERR-KZACCTF
               MOVE 8 TO RETURN-CODE
               DISPLAY 'KZACCTF オープンエラー ST='
                       WS-KZACCTF-ST
           END-IF

           IF NOT HARD-ERROR
               OPEN INPUT TGBANKF
               IF WS-TGBANKF-ST = '00'
                   MOVE '1' TO WS-TGBANKF-OPEN-SW
               ELSE
                   MOVE '1' TO WS-HARD-ERROR-SW
                   MOVE '08' TO LK-RETURN-STATUS
                   MOVE 'E900' TO LK-ERR-TGBANKF
                   MOVE 8 TO RETURN-CODE
                   DISPLAY 'TGBANKF オープンエラー ST='
                           WS-TGBANKF-ST
               END-IF
           END-IF.

       2000-EDIT-RAW SECTION.
           IF LK-RAW-VALUE-DT = SPACES
               PERFORM 8100-SET-ERR-VALUE-DT
           ELSE
               IF LK-RAW-VALUE-DT NUMERIC
                   MOVE LK-RAW-VALUE-DT TO WS-VALUE-DT-N
                   PERFORM 2100-CHECK-VALUE-DATE
               ELSE
                   PERFORM 8100-SET-ERR-VALUE-DT
               END-IF
           END-IF

           IF LK-RAW-CENTER-SEQ = SPACES
               PERFORM 8200-SET-ERR-CENTER-SEQ
           ELSE
               IF LK-RAW-CENTER-SEQ NUMERIC
                   MOVE LK-RAW-CENTER-SEQ TO WS-CENTER-SEQ-N
                   IF WS-CENTER-SEQ-N = ZERO
                       PERFORM 8200-SET-ERR-CENTER-SEQ
                   END-IF
               ELSE
                   PERFORM 8200-SET-ERR-CENTER-SEQ
               END-IF
           END-IF

           IF LK-RAW-COUNTER-BANK = SPACES
               PERFORM 8300-SET-ERR-BANK
           ELSE
               IF NOT LK-RAW-COUNTER-BANK NUMERIC
                   PERFORM 8300-SET-ERR-BANK
               END-IF
           END-IF

           IF LK-RAW-ACCT-NO = SPACES
               PERFORM 8400-SET-ERR-ACCT
           ELSE
               IF NOT LK-RAW-ACCT-NO NUMERIC
                   PERFORM 8400-SET-ERR-ACCT
               END-IF
           END-IF

           IF LK-RAW-AMOUNT = SPACES
               PERFORM 8500-SET-ERR-AMOUNT
           ELSE
               IF LK-RAW-AMOUNT NUMERIC
                   MOVE LK-RAW-AMOUNT TO WS-AMOUNT-N
                   IF WS-AMOUNT-N = ZERO
                       PERFORM 8500-SET-ERR-AMOUNT
                   END-IF
               ELSE
                   PERFORM 8500-SET-ERR-AMOUNT
               END-IF
           END-IF

           IF LK-RAW-NAME-KANA = SPACES
               PERFORM 8600-SET-ERR-NAME
           END-IF

           IF NOT DETAIL-ERROR
               MOVE WS-VALUE-DT-N       TO LK-ED-VALUE-DT
               MOVE WS-CENTER-SEQ-N     TO LK-ED-CENTER-SEQ
               MOVE LK-RAW-COUNTER-BANK TO LK-ED-COUNTER-BANK
               MOVE LK-RAW-ACCT-NO      TO LK-ED-ACCT-NO
               MOVE WS-AMOUNT-N         TO LK-ED-AMOUNT
               MOVE LK-RAW-NAME-KANA    TO LK-ED-NAME-KANA
           END-IF.

       2100-CHECK-VALUE-DATE SECTION.
           MOVE LK-RAW-VALUE-DT(1:4) TO WS-YY
           MOVE LK-RAW-VALUE-DT(5:2) TO WS-MM
           MOVE LK-RAW-VALUE-DT(7:2) TO WS-DD
           MOVE '0' TO WS-LEAP-SW
           MOVE ZERO TO WS-DAYS-IN-MONTH

           DIVIDE WS-YY BY 4
               GIVING WS-QUOTIENT
               REMAINDER WS-REM4
           DIVIDE WS-YY BY 100
               GIVING WS-QUOTIENT
               REMAINDER WS-REM100
           DIVIDE WS-YY BY 400
               GIVING WS-QUOTIENT
               REMAINDER WS-REM400

           IF WS-REM4 = ZERO AND
              (WS-REM100 NOT = ZERO OR WS-REM400 = ZERO)
               MOVE '1' TO WS-LEAP-SW
           END-IF

           EVALUATE WS-MM
               WHEN 01
               WHEN 03
               WHEN 05
               WHEN 07
               WHEN 08
               WHEN 10
               WHEN 12
                   MOVE 31 TO WS-DAYS-IN-MONTH
               WHEN 04
               WHEN 06
               WHEN 09
               WHEN 11
                   MOVE 30 TO WS-DAYS-IN-MONTH
               WHEN 02
                   IF LEAP-YEAR
                       MOVE 29 TO WS-DAYS-IN-MONTH
                   ELSE
                       MOVE 28 TO WS-DAYS-IN-MONTH
                   END-IF
               WHEN OTHER
                   MOVE ZERO TO WS-DAYS-IN-MONTH
           END-EVALUATE

           IF WS-YY < 2000 OR WS-DAYS-IN-MONTH = ZERO
               PERFORM 8100-SET-ERR-VALUE-DT
           ELSE
               IF WS-DD < 1 OR WS-DD > WS-DAYS-IN-MONTH
                   PERFORM 8100-SET-ERR-VALUE-DT
               END-IF
           END-IF.

       3000-CHECK-ACCOUNT SECTION.
           MOVE LK-ED-ACCT-NO TO AC-ACCT-NO
           READ KZACCTF KEY IS AC-ACCT-NO
           EVALUATE WS-KZACCTF-ST
               WHEN '00'
                   IF AC-STATUS = '1'
                       MOVE AC-BRANCH      TO LK-ED-BRANCH
                       MOVE AC-ACCT-TYPE   TO LK-ED-ACCT-TYPE
                       MOVE AC-CUSTOMER-ID TO LK-ED-CUSTOMER-ID
                       IF LK-ED-NAME-KANA NOT = AC-ACCT-NAME-KANA
                           MOVE '1' TO WS-DETAIL-ERROR-SW
                           MOVE 'E142' TO LK-ERR-NAME-KANA
                           ADD 1 TO LK-ERR-COUNT
                           DISPLAY '名義カナ不一致 ACCT='
                                   AC-ACCT-NO
                       END-IF
                   ELSE
                       MOVE '1' TO WS-DETAIL-ERROR-SW
                       MOVE 'E141' TO LK-ERR-KZACCTF
                       ADD 1 TO LK-ERR-COUNT
                       DISPLAY '口座状態エラー ACCT='
                               AC-ACCT-NO
                   END-IF
               WHEN '23'
                   MOVE '1' TO WS-DETAIL-ERROR-SW
                   MOVE 'E140' TO LK-ERR-KZACCTF
                   ADD 1 TO LK-ERR-COUNT
                   DISPLAY '口座未登録 ACCT='
                           AC-ACCT-NO
               WHEN OTHER
                   MOVE '1' TO WS-HARD-ERROR-SW
                   MOVE '08' TO LK-RETURN-STATUS
                   MOVE 'E901' TO LK-ERR-KZACCTF
                   MOVE 8 TO RETURN-CODE
                   DISPLAY 'KZACCTF 読込エラー ST='
                           WS-KZACCTF-ST
           END-EVALUATE.

       4000-CHECK-BANK SECTION.
           MOVE LK-ED-COUNTER-BANK TO BK-COUNTER-BANK
           READ TGBANKF KEY IS BK-COUNTER-BANK
           EVALUATE WS-TGBANKF-ST
               WHEN '00'
                   MOVE BK-VALID-FROM TO WS-VALID-FROM-N
                   MOVE BK-VALID-TO   TO WS-VALID-TO-N
                   IF BK-STATUS NOT = '1'
                       MOVE '1' TO WS-DETAIL-ERROR-SW
                       MOVE 'E241' TO LK-ERR-TGBANKF
                       ADD 1 TO LK-ERR-COUNT
                       DISPLAY '銀行状態エラー BANK='
                               BK-COUNTER-BANK
                   ELSE
                       IF LK-ED-VALUE-DT < WS-VALID-FROM-N OR
                          LK-ED-VALUE-DT > WS-VALID-TO-N
                           MOVE '1' TO WS-DETAIL-ERROR-SW
                           MOVE 'E242' TO LK-ERR-TGBANKF
                           ADD 1 TO LK-ERR-COUNT
                           DISPLAY '銀行有効日エラー BANK='
                                   BK-COUNTER-BANK
                       ELSE
                           MOVE BK-BANK-NAME-KANA TO LK-ED-BANK-NAME
                           MOVE BK-CLEARING-GROUP TO
                                LK-ED-CLEARING-GRP
                       END-IF
                   END-IF
               WHEN '23'
                   MOVE '1' TO WS-DETAIL-ERROR-SW
                   MOVE 'E240' TO LK-ERR-TGBANKF
                   ADD 1 TO LK-ERR-COUNT
                   DISPLAY '銀行未登録 BANK='
                           BK-COUNTER-BANK
               WHEN OTHER
                   MOVE '1' TO WS-HARD-ERROR-SW
                   MOVE '08' TO LK-RETURN-STATUS
                   MOVE 'E901' TO LK-ERR-TGBANKF
                   MOVE 8 TO RETURN-CODE
                   DISPLAY 'TGBANKF 読込エラー ST='
                           WS-TGBANKF-ST
           END-EVALUATE.

       8100-SET-ERR-VALUE-DT SECTION.
           IF LK-ERR-VALUE-DT = SPACES
               MOVE '1' TO WS-DETAIL-ERROR-SW
               MOVE 'E101' TO LK-ERR-VALUE-DT
               ADD 1 TO LK-ERR-COUNT
               DISPLAY 'VALUE-DT エラー'
           END-IF.

       8200-SET-ERR-CENTER-SEQ SECTION.
           IF LK-ERR-CENTER-SEQ = SPACES
               MOVE '1' TO WS-DETAIL-ERROR-SW
               MOVE 'E102' TO LK-ERR-CENTER-SEQ
               ADD 1 TO LK-ERR-COUNT
               DISPLAY 'CENTER-SEQ エラー'
           END-IF.

       8300-SET-ERR-BANK SECTION.
           IF LK-ERR-COUNTER-BANK = SPACES
               MOVE '1' TO WS-DETAIL-ERROR-SW
               MOVE 'E103' TO LK-ERR-COUNTER-BANK
               ADD 1 TO LK-ERR-COUNT
               DISPLAY 'COUNTER-BANK エラー'
           END-IF.

       8400-SET-ERR-ACCT SECTION.
           IF LK-ERR-ACCT-NO = SPACES
               MOVE '1' TO WS-DETAIL-ERROR-SW
               MOVE 'E104' TO LK-ERR-ACCT-NO
               ADD 1 TO LK-ERR-COUNT
               DISPLAY 'ACCT-NO エラー'
           END-IF.

       8500-SET-ERR-AMOUNT SECTION.
           IF LK-ERR-AMOUNT = SPACES
               MOVE '1' TO WS-DETAIL-ERROR-SW
               MOVE 'E105' TO LK-ERR-AMOUNT
               ADD 1 TO LK-ERR-COUNT
               DISPLAY 'AMOUNT エラー'
           END-IF.

       8600-SET-ERR-NAME SECTION.
           IF LK-ERR-NAME-KANA = SPACES
               MOVE '1' TO WS-DETAIL-ERROR-SW
               MOVE 'E106' TO LK-ERR-NAME-KANA
               ADD 1 TO LK-ERR-COUNT
               DISPLAY 'NAME-KANA エラー'
           END-IF.

       9000-FINAL SECTION.
           IF KZACCTF-OPENED
               CLOSE KZACCTF
               IF WS-KZACCTF-ST NOT = '00'
                   MOVE '1' TO WS-HARD-ERROR-SW
                   MOVE '08' TO LK-RETURN-STATUS
                   MOVE 'E902' TO LK-ERR-KZACCTF
                   MOVE 8 TO RETURN-CODE
                   DISPLAY 'KZACCTF クローズエラー ST='
                           WS-KZACCTF-ST
               END-IF
           END-IF

           IF TGBANKF-OPENED
               CLOSE TGBANKF
               IF WS-TGBANKF-ST NOT = '00'
                   MOVE '1' TO WS-HARD-ERROR-SW
                   MOVE '08' TO LK-RETURN-STATUS
                   MOVE 'E902' TO LK-ERR-TGBANKF
                   MOVE 8 TO RETURN-CODE
                   DISPLAY 'TGBANKF クローズエラー ST='
                           WS-TGBANKF-ST
               END-IF
           END-IF

           IF HARD-ERROR
               MOVE '08' TO LK-RETURN-STATUS
               IF RETURN-CODE = ZERO
                   MOVE 8 TO RETURN-CODE
               END-IF
           ELSE
               IF DETAIL-ERROR
                   MOVE '04' TO LK-RETURN-STATUS
                   MOVE ZERO TO RETURN-CODE
               ELSE
                   MOVE '00' TO LK-RETURN-STATUS
                   MOVE ZERO TO RETURN-CODE
               END-IF
           END-IF.
