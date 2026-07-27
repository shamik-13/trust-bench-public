       IDENTIFICATION DIVISION.
       PROGRAM-ID. CB102B.
      *---------------------------------------------------------------*
      * 売上事前マッチングバッチ。
      * CDSALEF を読み、確定前の例外を CDEXCPF へ出力。
      * CB510B の入力は更新しない。
      *---------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CDSALEF ASSIGN TO "CDSALEF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-CDSALEF.

           SELECT CDCARDF ASSIGN TO "CDCARDF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CF-CARD-NO
               FILE STATUS IS FS-CDCARDF.

           SELECT CDAUTHF2 ASSIGN TO "CDAUTHF2"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS AU-AUTH-ID
               FILE STATUS IS FS-CDAUTHF2.

           SELECT CDMERCF ASSIGN TO "CDMERCF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS MC-MERCHANT-CODE
               FILE STATUS IS FS-CDMERCF.

           SELECT CDEXCPF ASSIGN TO "CDEXCPF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-CDEXCPF.

       DATA DIVISION.
       FILE SECTION.
       FD  CDSALEF.
           COPY CDSALEFC.

       FD  CDCARDF.
           COPY CDCARD03.

       FD  CDAUTHF2.
           COPY CDAUTHF2C.

       FD  CDMERCF.
           COPY CDMERCC.

       FD  CDEXCPF.
           COPY CDEXCPC.

       WORKING-STORAGE SECTION.
       01  FILE-STATUS-AREA.
           05 FS-CDSALEF             PIC XX VALUE SPACE.
           05 FS-CDCARDF             PIC XX VALUE SPACE.
           05 FS-CDAUTHF2            PIC XX VALUE SPACE.
           05 FS-CDMERCF             PIC XX VALUE SPACE.
           05 FS-CDEXCPF             PIC XX VALUE SPACE.

       01  SWITCH-AREA.
           05 EOF-CDSALEF            PIC X VALUE "N".
              88 CDSALEF-END               VALUE "Y".
              88 CDSALEF-NOT-END           VALUE "N".
           05 ABEND-SW               PIC X VALUE "N".
              88 ABEND-OCCURRED            VALUE "Y".
              88 ABEND-NOT-OCCURRED        VALUE "N".
           05 EXCEPTION-WRITTEN-SW   PIC X VALUE "N".
              88 EXCEPTION-WRITTEN         VALUE "Y".
              88 EXCEPTION-NOT-WRITTEN     VALUE "N".

       01  COUNTER-AREA.
           05 CNT-SALE-READ          PIC 9(9) VALUE ZERO.
           05 CNT-EXCEPTION-WRITE    PIC 9(9) VALUE ZERO.
           05 CNT-HARD-ERROR         PIC 9(5) VALUE ZERO.
           05 WK-EXCEPTION-SEQ       PIC 9(12) VALUE ZERO.

       01  DATE-AREA.
           05 WK-TODAY               PIC 9(8) VALUE ZERO.
           05 WK-TODAY-INT           PIC 9(8) VALUE ZERO.
           05 WK-FROM-INT            PIC 9(8) VALUE ZERO.
           05 WK-FROM-DT             PIC 9(8) VALUE ZERO.
           05 WK-TO-DT               PIC 9(8) VALUE ZERO.
           05 WK-SALE-DT             PIC 9(8) VALUE ZERO.

       01  EDIT-AREA.
           05 ED-CNT-SALE            PIC ZZZ,ZZZ,ZZ9.
           05 ED-CNT-EXC             PIC ZZZ,ZZZ,ZZ9.
           05 ED-CNT-ERR             PIC ZZZZ9.
           05 ED-EXC-ID              PIC 9(12).

       01  REASON-AREA.
           05 WK-REASON-CD           PIC X(4) VALUE SPACE.
           05 WK-REASON-TEXT         PIC X(80) VALUE SPACE.

       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-INITIALIZE
           IF ABEND-NOT-OCCURRED
              PERFORM 2000-PROCESS UNTIL CDSALEF-END
                                    OR ABEND-OCCURRED
           END-IF
           PERFORM 9000-TERMINATE
           GOBACK.

       1000-INITIALIZE.
           MOVE 0 TO RETURN-CODE
           SET ABEND-NOT-OCCURRED TO TRUE
           SET CDSALEF-NOT-END TO TRUE
           SET EXCEPTION-NOT-WRITTEN TO TRUE

           ACCEPT WK-TODAY FROM DATE YYYYMMDD
           COMPUTE WK-TODAY-INT = FUNCTION INTEGER-OF-DATE(WK-TODAY)
           COMPUTE WK-FROM-INT = WK-TODAY-INT - 7
           MOVE FUNCTION DATE-OF-INTEGER(WK-FROM-INT) TO WK-FROM-DT
           MOVE WK-TODAY TO WK-TO-DT

           OPEN INPUT CDSALEF
           IF FS-CDSALEF NOT = "00"
              DISPLAY "CDSALEF オープン失敗 ST=" FS-CDSALEF
              PERFORM 8900-SET-ABEND
           END-IF

           IF ABEND-NOT-OCCURRED
              OPEN INPUT CDCARDF
              IF FS-CDCARDF NOT = "00"
                 DISPLAY "CDCARDF オープン失敗 ST=" FS-CDCARDF
                 PERFORM 8900-SET-ABEND
              END-IF
           END-IF

           IF ABEND-NOT-OCCURRED
              OPEN INPUT CDAUTHF2
              IF FS-CDAUTHF2 NOT = "00"
                 DISPLAY "CDAUTHF2 オープン失敗 ST=" FS-CDAUTHF2
                 PERFORM 8900-SET-ABEND
              END-IF
           END-IF

           IF ABEND-NOT-OCCURRED
              OPEN INPUT CDMERCF
              IF FS-CDMERCF NOT = "00"
                 DISPLAY "CDMERCF オープン失敗 ST=" FS-CDMERCF
                 PERFORM 8900-SET-ABEND
              END-IF
           END-IF

           IF ABEND-NOT-OCCURRED
              OPEN OUTPUT CDEXCPF
              IF FS-CDEXCPF NOT = "00"
                 DISPLAY "CDEXCPF オープン失敗 ST=" FS-CDEXCPF
                 PERFORM 8900-SET-ABEND
              END-IF
           END-IF

           IF ABEND-NOT-OCCURRED
              PERFORM 2100-READ-SALE
           END-IF.

       2000-PROCESS.
           SET EXCEPTION-NOT-WRITTEN TO TRUE

           PERFORM 3000-CHECK-CARD

           IF ABEND-NOT-OCCURRED
              PERFORM 4000-CHECK-AUTH
           END-IF

           IF ABEND-NOT-OCCURRED
              PERFORM 5000-CHECK-MERCHANT
           END-IF

           IF ABEND-NOT-OCCURRED
              PERFORM 6000-CHECK-SALE-DATE
           END-IF

           IF ABEND-NOT-OCCURRED
              PERFORM 7000-CHECK-ATTENTION
           END-IF

           IF ABEND-NOT-OCCURRED
              PERFORM 2100-READ-SALE
           END-IF.

       2100-READ-SALE.
           READ CDSALEF
              AT END
                 SET CDSALEF-END TO TRUE
              NOT AT END
                 ADD 1 TO CNT-SALE-READ
           END-READ

           IF FS-CDSALEF NOT = "00" AND FS-CDSALEF NOT = "10"
              DISPLAY "CDSALEF 読込失敗 ST=" FS-CDSALEF
              PERFORM 8900-SET-ABEND
           END-IF.

       3000-CHECK-CARD.
           MOVE SL-CARD-NO TO CF-CARD-NO

           READ CDCARDF KEY IS CF-CARD-NO
              INVALID KEY
                 MOVE "C001" TO WK-REASON-CD
                 MOVE "カード未登録" TO WK-REASON-TEXT
                 PERFORM 8000-WRITE-EXCEPTION
              NOT INVALID KEY
                 IF CF-CARD-STATUS NOT = "01"
                    EVALUATE CF-CARD-STATUS
                       WHEN "02"
                          MOVE "C002" TO WK-REASON-CD
                          MOVE "カード利用停止" TO WK-REASON-TEXT
                       WHEN "03"
                          MOVE "C003" TO WK-REASON-CD
                          MOVE "カード解約済" TO WK-REASON-TEXT
                       WHEN "09"
                          MOVE "C009" TO WK-REASON-CD
                          MOVE "カード延滞" TO WK-REASON-TEXT
                       WHEN OTHER
                          MOVE "C099" TO WK-REASON-CD
                          MOVE "カード状態不正" TO WK-REASON-TEXT
                    END-EVALUATE
                    PERFORM 8000-WRITE-EXCEPTION
                 END-IF
           END-READ

           IF FS-CDCARDF NOT = "00" AND FS-CDCARDF NOT = "23"
              DISPLAY "CDCARDF 読込失敗 ST=" FS-CDCARDF
              PERFORM 8900-SET-ABEND
           END-IF.

       4000-CHECK-AUTH.
           MOVE SL-AUTH-ID TO AU-AUTH-ID

           READ CDAUTHF2 KEY IS AU-AUTH-ID
              INVALID KEY
                 MOVE "A001" TO WK-REASON-CD
                 MOVE "オーソリ未登録" TO WK-REASON-TEXT
                 PERFORM 8000-WRITE-EXCEPTION
              NOT INVALID KEY
                 IF AU-CARD-NO NOT = SL-CARD-NO
                    MOVE "A002" TO WK-REASON-CD
                    MOVE "オーソリカード不一致"
                       TO WK-REASON-TEXT
                    PERFORM 8000-WRITE-EXCEPTION
                 END-IF

                 IF AU-AUTH-AMT NOT = SL-SALE-AMT
                    MOVE "A003" TO WK-REASON-CD
                    MOVE "オーソリ金額不一致" TO WK-REASON-TEXT
                    PERFORM 8000-WRITE-EXCEPTION
                 END-IF

                 IF AU-CURRENCY-CD NOT = SL-CURRENCY-CD
                    MOVE "A004" TO WK-REASON-CD
                    MOVE "オーソリ通貨不一致" TO WK-REASON-TEXT
                    PERFORM 8000-WRITE-EXCEPTION
                 END-IF

                 IF AU-MERCHANT-CODE NOT = SL-MERCHANT-CODE
                    MOVE "A005" TO WK-REASON-CD
                    MOVE "オーソリ加盟店不一致"
                       TO WK-REASON-TEXT
                    PERFORM 8000-WRITE-EXCEPTION
                 END-IF

                 IF AU-AUTH-STATUS NOT = "01"
                    MOVE "A006" TO WK-REASON-CD
                    MOVE "オーソリ状態不正" TO WK-REASON-TEXT
                    PERFORM 8000-WRITE-EXCEPTION
                 END-IF
           END-READ

           IF FS-CDAUTHF2 NOT = "00" AND FS-CDAUTHF2 NOT = "23"
              DISPLAY "CDAUTHF2 読込失敗 ST=" FS-CDAUTHF2
              PERFORM 8900-SET-ABEND
           END-IF.

       5000-CHECK-MERCHANT.
           MOVE SL-MERCHANT-CODE TO MC-MERCHANT-CODE

           READ CDMERCF KEY IS MC-MERCHANT-CODE
              INVALID KEY
                 MOVE "M001" TO WK-REASON-CD
                 MOVE "加盟店未登録" TO WK-REASON-TEXT
                 PERFORM 8000-WRITE-EXCEPTION
              NOT INVALID KEY
                 IF MC-MERCHANT-STATUS NOT = "01"
                    MOVE "M002" TO WK-REASON-CD
                    MOVE "加盟店状態不正" TO WK-REASON-TEXT
                    PERFORM 8000-WRITE-EXCEPTION
                 END-IF
           END-READ

           IF FS-CDMERCF NOT = "00" AND FS-CDMERCF NOT = "23"
              DISPLAY "CDMERCF 読込失敗 ST=" FS-CDMERCF
              PERFORM 8900-SET-ABEND
           END-IF.

       6000-CHECK-SALE-DATE.
           MOVE SL-SALE-DT TO WK-SALE-DT

           IF WK-SALE-DT < WK-FROM-DT OR WK-SALE-DT > WK-TO-DT
              MOVE "D001" TO WK-REASON-CD
              MOVE "売上日付が営業範囲外" TO WK-REASON-TEXT
              PERFORM 8000-WRITE-EXCEPTION
           END-IF.

       7000-CHECK-ATTENTION.
           IF SL-CURRENCY-CD NOT = "JPY"
              MOVE "W001" TO WK-REASON-CD
              MOVE "外貨売上" TO WK-REASON-TEXT
              PERFORM 8000-WRITE-EXCEPTION
           END-IF

           IF FS-CDMERCF = "00"
              IF MC-FEE-PLAN-CD = SPACE OR MC-FEE-PLAN-CD = ZERO
                 MOVE "W002" TO WK-REASON-CD
                 MOVE "加盟店手数料プラン未設定"
                    TO WK-REASON-TEXT
                 PERFORM 8000-WRITE-EXCEPTION
              END-IF
           END-IF.

       8000-WRITE-EXCEPTION.
           INITIALIZE CDEXCPF-REC
           ADD 1 TO WK-EXCEPTION-SEQ

           MOVE WK-EXCEPTION-SEQ TO ED-EXC-ID
           MOVE ED-EXC-ID TO EX-EXCEPTION-ID
           MOVE SL-SALE-ID TO EX-SALE-ID
           MOVE SL-CARD-NO TO EX-CARD-NO
           MOVE WK-REASON-CD TO EX-REASON-CD
           MOVE "CB102B" TO EX-DETECTED-PGM
           MOVE WK-TODAY TO EX-EXCEPTION-DT
           MOVE "H" TO EX-ACTION-STATUS

           WRITE CDEXCPF-REC

           IF FS-CDEXCPF = "00"
              ADD 1 TO CNT-EXCEPTION-WRITE
              SET EXCEPTION-WRITTEN TO TRUE
           ELSE
              DISPLAY "CDEXCPF 書込失敗 ST=" FS-CDEXCPF
              DISPLAY "理由=" WK-REASON-CD " " WK-REASON-TEXT
              PERFORM 8900-SET-ABEND
           END-IF.

       8900-SET-ABEND.
           ADD 1 TO CNT-HARD-ERROR
           SET ABEND-OCCURRED TO TRUE
           MOVE 8 TO RETURN-CODE.

       9000-TERMINATE.
           IF FS-CDSALEF = "00" OR FS-CDSALEF = "10"
              CLOSE CDSALEF
           END-IF

           IF FS-CDCARDF = "00" OR FS-CDCARDF = "23"
              CLOSE CDCARDF
           END-IF

           IF FS-CDAUTHF2 = "00" OR FS-CDAUTHF2 = "23"
              CLOSE CDAUTHF2
           END-IF

           IF FS-CDMERCF = "00" OR FS-CDMERCF = "23"
              CLOSE CDMERCF
           END-IF

           IF FS-CDEXCPF = "00"
              CLOSE CDEXCPF
           END-IF

           MOVE CNT-SALE-READ TO ED-CNT-SALE
           MOVE CNT-EXCEPTION-WRITE TO ED-CNT-EXC
           MOVE CNT-HARD-ERROR TO ED-CNT-ERR

           DISPLAY "CB102B 売上読込件数=" ED-CNT-SALE
           DISPLAY "CB102B 例外書込件数=" ED-CNT-EXC
           DISPLAY "CB102B エラー件数=" ED-CNT-ERR

           IF ABEND-OCCURRED
              MOVE 8 TO RETURN-CODE
              DISPLAY "CB102B 異常終了"
           ELSE
              MOVE 0 TO RETURN-CODE
              DISPLAY "CB102B 正常終了"
           END-IF.
