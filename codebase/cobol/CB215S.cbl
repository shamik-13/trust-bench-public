       IDENTIFICATION DIVISION.
       PROGRAM-ID. CB215S.
      *================================================================*
      * 売上入力共通検査サブ                                           *
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CDCARDF
               ASSIGN       TO "CDCARDF"
               ORGANIZATION IS INDEXED
               ACCESS MODE  IS DYNAMIC
               RECORD KEY   IS CF-CARD-NO
               FILE STATUS  IS WK-CF-STATUS.

           SELECT CDAUTHF2
               ASSIGN       TO "CDAUTHF2"
               ORGANIZATION IS INDEXED
               ACCESS MODE  IS DYNAMIC
               RECORD KEY   IS AU-AUTH-ID
               FILE STATUS  IS WK-AU-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  CDCARDF.
           COPY CDCARD03.

       FD  CDAUTHF2.
           COPY CDAUTHF2C.

       WORKING-STORAGE SECTION.
       01  WK-CF-STATUS              PIC XX VALUE SPACES.
       01  WK-AU-STATUS              PIC XX VALUE SPACES.

       01  WK-HARD-ERROR             PIC X VALUE SPACE.
           88  WK-HARD-ERROR-ON            VALUE "1".
           88  WK-HARD-ERROR-OFF           VALUE "0" SPACE.

       01  WK-CF-OPENED              PIC X VALUE SPACE.
           88  WK-CF-OPENED-ON             VALUE "1".
           88  WK-CF-OPENED-OFF            VALUE "0" SPACE.

       01  WK-AU-OPENED              PIC X VALUE SPACE.
           88  WK-AU-OPENED-ON             VALUE "1".
           88  WK-AU-OPENED-OFF            VALUE "0" SPACE.

       01  WK-IDX                    PIC 9(03) COMP VALUE ZERO.
       01  WK-DIGIT-COUNT            PIC 9(03) COMP VALUE ZERO.
       01  WK-AUTH-DIFF              PIC S9(11)V99 COMP-3 VALUE ZERO.
       01  WK-MAX-UNDER-AMT          PIC S9(11)V99 COMP-3 VALUE 100.
       01  WK-TODAY                  PIC 9(08) VALUE ZERO.

       LINKAGE SECTION.
       01  BC-SALES-CHECK-AREA.
           05  BC-CARD-NO            PIC X(16).
           05  BC-AUTH-ID            PIC X(12).
           05  BC-SALE-AMT           PIC S9(11)V99 COMP-3.
           05  BC-CURRENCY-CD        PIC X(03).
           05  BC-SALE-DT            PIC 9(08).
           05  BC-CAP-STATUS         PIC X.
           05  BC-OVERSEAS-FEE       PIC S9(09)V99 COMP-3.
           05  BC-RESULT-CD          PIC X.
           05  BC-CARD-REASON        PIC X(04).
           05  BC-AUTH-REASON        PIC X(04).
           05  BC-AMT-REASON         PIC X(04).
           05  BC-CAP-REASON         PIC X(04).
           05  BC-DETAIL-MSG         PIC X(80).

       PROCEDURE DIVISION USING BC-SALES-CHECK-AREA.
       0000-MAIN.
           PERFORM 1000-INIT
           IF WK-HARD-ERROR-OFF
               PERFORM 2000-CHECK-CARD
           END-IF
           IF WK-HARD-ERROR-OFF
               PERFORM 3000-CHECK-AUTH
           END-IF
           IF WK-HARD-ERROR-OFF
               PERFORM 4000-CHECK-AMOUNT
           END-IF
           PERFORM 5000-DECIDE-CAPTURE
           PERFORM 9000-FINAL
           GOBACK
           .

       1000-INIT.
           SET WK-HARD-ERROR-OFF TO TRUE
           SET WK-CF-OPENED-OFF  TO TRUE
           SET WK-AU-OPENED-OFF  TO TRUE

           MOVE "0"    TO BC-RESULT-CD
           MOVE SPACES TO BC-CARD-REASON
                          BC-AUTH-REASON
                          BC-AMT-REASON
                          BC-CAP-REASON
                          BC-DETAIL-MSG
           MOVE ZERO   TO BC-OVERSEAS-FEE
           ACCEPT WK-TODAY FROM DATE YYYYMMDD

           OPEN INPUT CDCARDF
           IF WK-CF-STATUS = "00"
               SET WK-CF-OPENED-ON TO TRUE
           ELSE
               SET WK-HARD-ERROR-ON TO TRUE
               MOVE "9"    TO BC-RESULT-CD
               MOVE "CFIO" TO BC-CARD-REASON
               STRING "CDCARDF オープン ST=" WK-CF-STATUS
                   DELIMITED BY SIZE INTO BC-DETAIL-MSG
               DISPLAY "CDCARDF オープン ST=" WK-CF-STATUS
               MOVE 12 TO RETURN-CODE
           END-IF

           IF WK-HARD-ERROR-OFF
               OPEN INPUT CDAUTHF2
               IF WK-AU-STATUS = "00"
                   SET WK-AU-OPENED-ON TO TRUE
               ELSE
                   SET WK-HARD-ERROR-ON TO TRUE
                   MOVE "9"    TO BC-RESULT-CD
                   MOVE "AUIO" TO BC-AUTH-REASON
                   STRING "CDAUTHF2 オープン ST=" WK-AU-STATUS
                       DELIMITED BY SIZE INTO BC-DETAIL-MSG
                   DISPLAY "CDAUTHF2 オープン ST=" WK-AU-STATUS
                   MOVE 12 TO RETURN-CODE
               END-IF
           END-IF
           .

       2000-CHECK-CARD.
           PERFORM 2100-CHECK-CARD-DIGIT

           IF BC-CARD-REASON = SPACES
               MOVE BC-CARD-NO TO CF-CARD-NO
               READ CDCARDF KEY IS CF-CARD-NO
                   INVALID KEY
                       IF WK-CF-STATUS = "23"
                           MOVE "CFNF" TO BC-CARD-REASON
                           MOVE "1"    TO BC-RESULT-CD
                           STRING "カード未登録 " BC-CARD-NO
                               DELIMITED BY SIZE INTO BC-DETAIL-MSG
                       ELSE
                           SET WK-HARD-ERROR-ON TO TRUE
                           MOVE "9"    TO BC-RESULT-CD
                           MOVE "CFIO" TO BC-CARD-REASON
                           STRING "CDCARDF 読込 ST=" WK-CF-STATUS
                               DELIMITED BY SIZE INTO BC-DETAIL-MSG
                           DISPLAY "CDCARDF 読込 ST=" WK-CF-STATUS
                           MOVE 12 TO RETURN-CODE
                       END-IF
                   NOT INVALID KEY
                       IF CF-CARD-STATUS NOT = "01"
                           MOVE "CFST" TO BC-CARD-REASON
                           MOVE "1"    TO BC-RESULT-CD
                           STRING "カード状態 " CF-CARD-STATUS
                               DELIMITED BY SIZE INTO BC-DETAIL-MSG
                       END-IF
               END-READ
           END-IF
           .

       2100-CHECK-CARD-DIGIT.
           MOVE ZERO TO WK-DIGIT-COUNT

           PERFORM VARYING WK-IDX FROM 1 BY 1 UNTIL WK-IDX > 16
               IF BC-CARD-NO(WK-IDX:1) >= "0"
                  AND BC-CARD-NO(WK-IDX:1) <= "9"
                   ADD 1 TO WK-DIGIT-COUNT
               END-IF
           END-PERFORM

           IF WK-DIGIT-COUNT NOT = 16
               MOVE "CFDG" TO BC-CARD-REASON
               MOVE "1"    TO BC-RESULT-CD
               MOVE "カード桁数異常" TO BC-DETAIL-MSG
           END-IF
           .

       3000-CHECK-AUTH.
           IF BC-AUTH-ID = SPACES
               MOVE "AUBL" TO BC-AUTH-REASON
               MOVE "1"    TO BC-RESULT-CD
               IF BC-DETAIL-MSG = SPACES
                   MOVE "オーソリID未設定" TO BC-DETAIL-MSG
               END-IF
           ELSE
               MOVE BC-AUTH-ID TO AU-AUTH-ID
               READ CDAUTHF2 KEY IS AU-AUTH-ID
                   INVALID KEY
                       IF WK-AU-STATUS = "23"
                           MOVE "AUNF" TO BC-AUTH-REASON
                           MOVE "1"    TO BC-RESULT-CD
                           IF BC-DETAIL-MSG = SPACES
                               STRING "オーソリ未登録"
                                   BC-AUTH-ID
                                   DELIMITED BY SIZE INTO BC-DETAIL-MSG
                           END-IF
                       ELSE
                           SET WK-HARD-ERROR-ON TO TRUE
                           MOVE "9"    TO BC-RESULT-CD
                           MOVE "AUIO" TO BC-AUTH-REASON
                           STRING "CDAUTHF2 読込 ST=" WK-AU-STATUS
                               DELIMITED BY SIZE INTO BC-DETAIL-MSG
                           DISPLAY "CDAUTHF2 読込 ST=" WK-AU-STATUS
                           MOVE 12 TO RETURN-CODE
                       END-IF
                   NOT INVALID KEY
                       PERFORM 3100-CHECK-AUTH-DETAIL
               END-READ
           END-IF
           .

       3100-CHECK-AUTH-DETAIL.
           IF AU-CARD-NO NOT = BC-CARD-NO
               MOVE "AUCD" TO BC-AUTH-REASON
               MOVE "1"    TO BC-RESULT-CD
               IF BC-DETAIL-MSG = SPACES
                   MOVE "オーソリカード相違" TO BC-DETAIL-MSG
               END-IF
           END-IF

           IF AU-AUTH-STATUS NOT = "C"
               MOVE "AUST" TO BC-AUTH-REASON
               MOVE "1"    TO BC-RESULT-CD
               IF BC-DETAIL-MSG = SPACES
                   STRING "オーソリ状態 " AU-AUTH-STATUS
                       DELIMITED BY SIZE INTO BC-DETAIL-MSG
               END-IF
           END-IF

           IF AU-CURRENCY-CD NOT = BC-CURRENCY-CD
               MOVE "AUCR" TO BC-AUTH-REASON
               MOVE "1"    TO BC-RESULT-CD
               IF BC-DETAIL-MSG = SPACES
                   MOVE "オーソリ通貨不一致" TO BC-DETAIL-MSG
               END-IF
           END-IF
           .

       4000-CHECK-AMOUNT.
           IF BC-SALE-AMT <= ZERO
               MOVE "AMZR" TO BC-AMT-REASON
               MOVE "1"    TO BC-RESULT-CD
               IF BC-DETAIL-MSG = SPACES
                   MOVE "売上金額異常" TO BC-DETAIL-MSG
               END-IF
           ELSE
               IF BC-AUTH-REASON = SPACES
                   COMPUTE WK-AUTH-DIFF = AU-AUTH-AMT - BC-SALE-AMT
                   IF WK-AUTH-DIFF < ZERO
                       MOVE "AMOV" TO BC-AMT-REASON
                       MOVE "1"    TO BC-RESULT-CD
                       IF BC-DETAIL-MSG = SPACES
                           MOVE "売上金額超過" TO BC-DETAIL-MSG
                       END-IF
                   ELSE
                       IF WK-AUTH-DIFF > WK-MAX-UNDER-AMT
                           MOVE "AMDF" TO BC-AMT-REASON
                           MOVE "1"    TO BC-RESULT-CD
                           IF BC-DETAIL-MSG = SPACES
                               MOVE "金額差異大" TO BC-DETAIL-MSG
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF
           .

       5000-DECIDE-CAPTURE.
           IF WK-HARD-ERROR-ON
               MOVE "H"   TO BC-CAP-STATUS
               MOVE "SYS" TO BC-CAP-REASON
           ELSE
               IF BC-RESULT-CD = "0"
                  AND BC-CARD-REASON = SPACES
                  AND BC-AUTH-REASON = SPACES
                  AND BC-AMT-REASON = SPACES
                   MOVE "C"    TO BC-CAP-STATUS
                   MOVE "OK00" TO BC-CAP-REASON
      * 海外利用事務手数料は当サブでは算出しない。
      * 料率適用・計算は売上確定エンジン側で行う。
                   MOVE ZERO   TO BC-OVERSEAS-FEE
                   MOVE "ケンサOK" TO BC-DETAIL-MSG
               ELSE
                   MOVE "H"   TO BC-CAP-STATUS
                   MOVE "CHK" TO BC-CAP-REASON
               END-IF
           END-IF
           .

       9000-FINAL.
           IF WK-CF-OPENED-ON
               CLOSE CDCARDF
               IF WK-CF-STATUS NOT = "00"
                   DISPLAY "CDCARDF クローズ ST=" WK-CF-STATUS
                   SET WK-HARD-ERROR-ON TO TRUE
                   MOVE "9"    TO BC-RESULT-CD
                   MOVE "CFIO" TO BC-CARD-REASON
                   MOVE 8 TO RETURN-CODE
               END-IF
           END-IF

           IF WK-AU-OPENED-ON
               CLOSE CDAUTHF2
               IF WK-AU-STATUS NOT = "00"
                   DISPLAY "CDAUTHF2 クローズ ST=" WK-AU-STATUS
                   SET WK-HARD-ERROR-ON TO TRUE
                   MOVE "9"    TO BC-RESULT-CD
                   MOVE "AUIO" TO BC-AUTH-REASON
                   MOVE 8 TO RETURN-CODE
               END-IF
           END-IF

           IF WK-HARD-ERROR-OFF
               MOVE 0 TO RETURN-CODE
           END-IF
           .

       END PROGRAM CB215S.
