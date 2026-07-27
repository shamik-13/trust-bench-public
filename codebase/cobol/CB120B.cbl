       IDENTIFICATION DIVISION.
       PROGRAM-ID. CB120B.
      *
      * 変更履歴
      * 版数  年月日    担当  概要
      * 1.00  20240401  B01   初版作成
      * 1.01  20240715  B02   銀行別締切判定を追加
      * 1.02  20241020  B03   重複請求検査を追加
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CDOSF ASSIGN TO "CDOSF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS OS-CARD-NO
               FILE STATUS IS FS-CDOSF.
           SELECT CDACTF ASSIGN TO "CDACTF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS AC-ACCOUNT-ID
               FILE STATUS IS FS-CDACTF.
           SELECT CDTRQF ASSIGN TO "CDTRQF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-CDTRQF.

       DATA DIVISION.
       FILE SECTION.
       FD  CDOSF.
           COPY CDOSFC.
       FD  CDACTF.
           COPY CDACTC.
       FD  CDTRQF.
           COPY CDTRQC.

       WORKING-STORAGE SECTION.
       01  FS-CDOSF                    PIC XX.
       01  FS-CDACTF                   PIC XX.
       01  FS-CDTRQF                   PIC XX.

       01  SW-END-CDOSF                PIC X VALUE "N".
           88  END-CDOSF                     VALUE "Y".
       01  SW-END-CDACTF               PIC X VALUE "N".
           88  END-CDACTF                    VALUE "Y".
       01  SW-FOUND-ACCT               PIC X VALUE "N".
           88  FOUND-ACCT                    VALUE "Y".
       01  SW-DUPLICATE                PIC X VALUE "N".
           88  DUPLICATE-REQUEST             VALUE "Y".
       01  SW-HARD-ERROR               PIC X VALUE "N".
           88  HARD-ERROR                    VALUE "Y".
       01  SW-NEXT-BIZ                 PIC X VALUE "N".
           88  NEXT-BIZ-DAY                  VALUE "Y".

       01  WS-SYSTEM-DATE.
           05  WS-TODAY                PIC 9(8).
           05  WS-TODAY-R              REDEFINES WS-TODAY.
               10  WS-TODAY-YYYY       PIC 9(4).
               10  WS-TODAY-MM         PIC 9(2).
               10  WS-TODAY-DD         PIC 9(2).
       01  WS-SYSTEM-TIME.
           05  WS-NOW-HHMMSS           PIC 9(6).
           05  WS-NOW-CC               PIC 9(2).

       01  WS-RANGE.
           05  WS-CYCLE-FROM           PIC 9(8).
           05  WS-CYCLE-TO             PIC 9(8).

       01  WS-WORK.
           05  WS-BILL-AMT             PIC S9(11) COMP-3 VALUE ZERO.
           05  WS-MIN-BILL-AMT         PIC S9(7)  COMP-3 VALUE 3000.
           05  WS-DUE-DT               PIC 9(8) VALUE ZERO.
           05  WS-BASE-INT             PIC S9(9) COMP VALUE ZERO.
           05  WS-DUE-INT              PIC S9(9) COMP VALUE ZERO.
           05  WS-ID-NUM               PIC 9(9) VALUE ZERO.
           05  WS-CUTOFF-HHMM          PIC 9(4) VALUE ZERO.
           05  WS-NOW-HHMM             PIC 9(4) VALUE ZERO.
           05  WS-REQ-STATUS           PIC X VALUE SPACE.
           05  WS-REASON               PIC X(40) VALUE SPACE.

       01  WS-COUNTERS.
           05  CNT-OS-READ             PIC 9(9) VALUE ZERO.
           05  CNT-AC-READ             PIC 9(9) VALUE ZERO.
           05  CNT-TRQ-WRITE           PIC 9(9) VALUE ZERO.
           05  CNT-SKIP                PIC 9(9) VALUE ZERO.
           05  CNT-ERROR               PIC 9(9) VALUE ZERO.

       01  ACCT-TABLE.
           05  ACCT-ENTRY OCCURS 20000 TIMES.
               10  TB-ACCOUNT-ID       PIC X(20).
               10  TB-CARD-NO          PIC X(16).
               10  TB-BANK-CD          PIC X(4).
               10  TB-BRANCH-CD        PIC X(3).
               10  TB-DEPOSIT-TYPE     PIC X(1).
               10  TB-ACCOUNT-NO       PIC X(7).
               10  TB-HOLDER-KANA      PIC X(40).
               10  TB-TRANSFER-STATUS  PIC X(1).
       01  WS-ACCT-MAX                 PIC 9(5) VALUE ZERO.
       01  IX-ACCT                     PIC 9(5) VALUE ZERO.
       01  IX-FOUND                    PIC 9(5) VALUE ZERO.

       01  DUP-TABLE.
           05  DUP-ENTRY OCCURS 30000 TIMES.
               10  TB-DUP-CARD-NO      PIC X(16).
               10  TB-DUP-CYCLE-DT     PIC 9(8).
       01  WS-DUP-MAX                  PIC 9(5) VALUE ZERO.
       01  IX-DUP                      PIC 9(5) VALUE ZERO.

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE 0 TO RETURN-CODE
           PERFORM 0100-INITIALIZE
           IF NOT HARD-ERROR
               PERFORM 0200-LOAD-ACCOUNT
           END-IF
           IF NOT HARD-ERROR
               PERFORM 0300-PROCESS-OS
           END-IF
           PERFORM 9000-FINALIZE
           GOBACK.

       0100-INITIALIZE.
           ACCEPT WS-TODAY FROM DATE YYYYMMDD
           ACCEPT WS-SYSTEM-TIME FROM TIME
           MOVE WS-NOW-HHMMSS(1:4) TO WS-NOW-HHMM
           MOVE WS-TODAY TO WS-CYCLE-TO
           MOVE WS-TODAY TO WS-CYCLE-FROM
           MOVE 01 TO WS-CYCLE-FROM(7:2)

           OPEN INPUT CDOSF
           IF FS-CDOSF NOT = "00"
               DISPLAY "CDOSF オープン失敗 ST=" FS-CDOSF
               MOVE "Y" TO SW-HARD-ERROR
               MOVE 12 TO RETURN-CODE
           END-IF

           OPEN INPUT CDACTF
           IF FS-CDACTF NOT = "00"
               DISPLAY "CDACTF オープン失敗 ST=" FS-CDACTF
               MOVE "Y" TO SW-HARD-ERROR
               MOVE 12 TO RETURN-CODE
           END-IF

           OPEN OUTPUT CDTRQF
           IF FS-CDTRQF NOT = "00"
               DISPLAY "CDTRQF オープン失敗 ST=" FS-CDTRQF
               MOVE "Y" TO SW-HARD-ERROR
               MOVE 12 TO RETURN-CODE
           END-IF.

       0200-LOAD-ACCOUNT.
           PERFORM UNTIL END-CDACTF OR HARD-ERROR
               READ CDACTF NEXT RECORD
                   AT END
                       SET END-CDACTF TO TRUE
                   NOT AT END
                       IF FS-CDACTF = "00"
                           ADD 1 TO CNT-AC-READ
                           IF WS-ACCT-MAX < 20000
                               ADD 1 TO WS-ACCT-MAX
                               MOVE AC-ACCOUNT-ID
                                 TO TB-ACCOUNT-ID(WS-ACCT-MAX)
                               MOVE AC-CARD-NO
                                 TO TB-CARD-NO(WS-ACCT-MAX)
                               MOVE AC-BANK-CD
                                 TO TB-BANK-CD(WS-ACCT-MAX)
                               MOVE AC-BRANCH-CD
                                 TO TB-BRANCH-CD(WS-ACCT-MAX)
                               MOVE AC-DEPOSIT-TYPE
                                 TO TB-DEPOSIT-TYPE(WS-ACCT-MAX)
                               MOVE AC-ACCOUNT-NO
                                 TO TB-ACCOUNT-NO(WS-ACCT-MAX)
                               MOVE AC-HOLDER-KANA
                                 TO TB-HOLDER-KANA(WS-ACCT-MAX)
                               MOVE AC-TRANSFER-STATUS
                                 TO TB-TRANSFER-STATUS(WS-ACCT-MAX)
                           ELSE
                               DISPLAY "口座表上限超過"
                               MOVE "Y" TO SW-HARD-ERROR
                               MOVE 12 TO RETURN-CODE
                           END-IF
                       ELSE
                           DISPLAY "CDACTF 読込失敗 ST=" FS-CDACTF
                           MOVE "Y" TO SW-HARD-ERROR
                           MOVE 12 TO RETURN-CODE
                       END-IF
               END-READ
           END-PERFORM.

       0300-PROCESS-OS.
           PERFORM UNTIL END-CDOSF OR HARD-ERROR
               READ CDOSF NEXT RECORD
                   AT END
                       SET END-CDOSF TO TRUE
                   NOT AT END
                       IF FS-CDOSF = "00"
                           ADD 1 TO CNT-OS-READ
                           PERFORM 1000-PROCESS-ONE
                       ELSE
                           DISPLAY "CDOSF 読込失敗 ST=" FS-CDOSF
                           MOVE "Y" TO SW-HARD-ERROR
                           MOVE 12 TO RETURN-CODE
                       END-IF
               END-READ
           END-PERFORM.

       1000-PROCESS-ONE.
           MOVE SPACE TO WS-REASON
           MOVE "N" TO SW-FOUND-ACCT
           MOVE "N" TO SW-DUPLICATE
           MOVE "N" TO SW-NEXT-BIZ

           IF OS-CYCLE-DT < WS-CYCLE-FROM
              OR OS-CYCLE-DT > WS-CYCLE-TO
               ADD 1 TO CNT-SKIP
               EXIT PARAGRAPH
           END-IF

           PERFORM 1100-FIND-ACCOUNT
           IF NOT FOUND-ACCT
               MOVE "登録口座なし" TO WS-REASON
               PERFORM 1900-WRITE-EXCEPTION
               EXIT PARAGRAPH
           END-IF

           PERFORM 1200-CHECK-DUPLICATE
           IF DUPLICATE-REQUEST
               MOVE "重複請求" TO WS-REASON
               PERFORM 1900-WRITE-EXCEPTION
               EXIT PARAGRAPH
           END-IF

           PERFORM 1300-VALIDATE-ACCOUNT
           IF WS-REASON NOT = SPACE
               PERFORM 1900-WRITE-EXCEPTION
               EXIT PARAGRAPH
           END-IF

           COMPUTE WS-BILL-AMT =
               OS-FEE-BAL-AMT
             + OS-INTEREST-BAL-AMT
             + OS-PRINCIPAL-BAL-AMT

           IF WS-BILL-AMT <= ZERO
               MOVE "請求残高なし" TO WS-REASON
               PERFORM 1900-WRITE-EXCEPTION
               EXIT PARAGRAPH
           END-IF

           IF WS-BILL-AMT >= WS-MIN-BILL-AMT
               IF TB-TRANSFER-STATUS(IX-FOUND) = "8"
                   COMPUTE WS-BILL-AMT = WS-BILL-AMT + 1000
               END-IF
           END-IF

           PERFORM 1400-DECIDE-DUE-DATE
           MOVE "0" TO WS-REQ-STATUS
           PERFORM 1800-WRITE-REQUEST
           PERFORM 1500-ADD-DUPLICATE.

       1100-FIND-ACCOUNT.
           PERFORM VARYING IX-ACCT FROM 1 BY 1
             UNTIL IX-ACCT > WS-ACCT-MAX OR FOUND-ACCT
               IF TB-CARD-NO(IX-ACCT) = OS-CARD-NO
                   MOVE IX-ACCT TO IX-FOUND
                   SET FOUND-ACCT TO TRUE
               END-IF
           END-PERFORM.

       1200-CHECK-DUPLICATE.
           PERFORM VARYING IX-DUP FROM 1 BY 1
             UNTIL IX-DUP > WS-DUP-MAX OR DUPLICATE-REQUEST
               IF TB-DUP-CARD-NO(IX-DUP) = OS-CARD-NO
                  AND TB-DUP-CYCLE-DT(IX-DUP) = OS-CYCLE-DT
                   SET DUPLICATE-REQUEST TO TRUE
               END-IF
           END-PERFORM.

       1300-VALIDATE-ACCOUNT.
           IF TB-TRANSFER-STATUS(IX-FOUND) = "1"
               MOVE "請求停止" TO WS-REASON
           ELSE
               IF TB-TRANSFER-STATUS(IX-FOUND) = "2"
                   MOVE "口座状態不正" TO WS-REASON
               ELSE
                   IF TB-BANK-CD(IX-FOUND) = SPACE
                       MOVE "銀行コード未設定" TO WS-REASON
                   ELSE
                       IF TB-ACCOUNT-NO(IX-FOUND) = SPACE
                           MOVE "口座番号未設定" TO WS-REASON
                       END-IF
                   END-IF
               END-IF
           END-IF.

       1400-DECIDE-DUE-DATE.
           EVALUATE TB-BANK-CD(IX-FOUND)
               WHEN "0001"
                   MOVE 1500 TO WS-CUTOFF-HHMM
               WHEN "0005"
                   MOVE 1430 TO WS-CUTOFF-HHMM
               WHEN "0009"
                   MOVE 1600 TO WS-CUTOFF-HHMM
               WHEN OTHER
                   MOVE 1400 TO WS-CUTOFF-HHMM
           END-EVALUATE

           MOVE FUNCTION INTEGER-OF-DATE(WS-TODAY) TO WS-BASE-INT
           IF WS-NOW-HHMM > WS-CUTOFF-HHMM
               ADD 1 TO WS-BASE-INT
               SET NEXT-BIZ-DAY TO TRUE
           END-IF

           PERFORM 1410-ADJUST-BUSINESS-DAY
           MOVE FUNCTION DATE-OF-INTEGER(WS-DUE-INT) TO WS-DUE-DT.

       1410-ADJUST-BUSINESS-DAY.
           MOVE WS-BASE-INT TO WS-DUE-INT
           PERFORM UNTIL FUNCTION MOD(WS-DUE-INT 7) NOT = 0
                     AND FUNCTION MOD(WS-DUE-INT 7) NOT = 1
               ADD 1 TO WS-DUE-INT
           END-PERFORM.

       1500-ADD-DUPLICATE.
           IF WS-DUP-MAX < 30000
               ADD 1 TO WS-DUP-MAX
               MOVE OS-CARD-NO TO TB-DUP-CARD-NO(WS-DUP-MAX)
               MOVE OS-CYCLE-DT TO TB-DUP-CYCLE-DT(WS-DUP-MAX)
           ELSE
               DISPLAY "重複表上限超過"
               MOVE "Y" TO SW-HARD-ERROR
               MOVE 12 TO RETURN-CODE
           END-IF.

       1800-WRITE-REQUEST.
           ADD 1 TO WS-ID-NUM
           MOVE SPACE TO CDTRQF-REC
           MOVE WS-ID-NUM TO TRQ-REQUEST-ID
           MOVE OS-CARD-NO TO TRQ-CARD-NO
           MOVE OS-CYCLE-DT TO TRQ-BILLING-CYCLE-DT
           MOVE WS-BILL-AMT TO TRQ-REQUEST-AMT
           MOVE WS-DUE-DT TO TRQ-DUE-DT
           MOVE TB-BANK-CD(IX-FOUND) TO TRQ-BANK-CD
           MOVE TB-ACCOUNT-NO(IX-FOUND) TO TRQ-ACCOUNT-NO
           MOVE WS-REQ-STATUS TO TRQ-REQUEST-STATUS
           WRITE CDTRQF-REC
           IF FS-CDTRQF = "00"
               ADD 1 TO CNT-TRQ-WRITE
           ELSE
               DISPLAY "CDTRQF 書込失敗 ST=" FS-CDTRQF
               MOVE "Y" TO SW-HARD-ERROR
               MOVE 12 TO RETURN-CODE
           END-IF.

       1900-WRITE-EXCEPTION.
           ADD 1 TO CNT-ERROR
           MOVE 1 TO WS-BILL-AMT
           MOVE WS-TODAY TO WS-DUE-DT
           MOVE "9" TO WS-REQ-STATUS
           DISPLAY "請求例外 カード=" OS-CARD-NO
                   " 理由=" WS-REASON
           IF FOUND-ACCT
               PERFORM 1800-WRITE-REQUEST
           ELSE
               ADD 1 TO WS-ID-NUM
               MOVE SPACE TO CDTRQF-REC
               MOVE WS-ID-NUM TO TRQ-REQUEST-ID
               MOVE OS-CARD-NO TO TRQ-CARD-NO
               MOVE OS-CYCLE-DT TO TRQ-BILLING-CYCLE-DT
               MOVE ZERO TO TRQ-REQUEST-AMT
               MOVE WS-TODAY TO TRQ-DUE-DT
               MOVE SPACE TO TRQ-BANK-CD
               MOVE SPACE TO TRQ-ACCOUNT-NO
               MOVE "9" TO TRQ-REQUEST-STATUS
               WRITE CDTRQF-REC
               IF FS-CDTRQF = "00"
                   ADD 1 TO CNT-TRQ-WRITE
               ELSE
                   DISPLAY "CDTRQF 例外書込失敗 ST=" FS-CDTRQF
                   MOVE "Y" TO SW-HARD-ERROR
                   MOVE 12 TO RETURN-CODE
               END-IF
           END-IF.

       9000-FINALIZE.
           CLOSE CDOSF
           IF FS-CDOSF NOT = "00"
              AND FS-CDOSF NOT = "42"
               DISPLAY "CDOSF クローズ失敗 ST=" FS-CDOSF
               MOVE 8 TO RETURN-CODE
           END-IF

           CLOSE CDACTF
           IF FS-CDACTF NOT = "00"
              AND FS-CDACTF NOT = "42"
               DISPLAY "CDACTF クローズ失敗 ST=" FS-CDACTF
               MOVE 8 TO RETURN-CODE
           END-IF

           CLOSE CDTRQF
           IF FS-CDTRQF NOT = "00"
              AND FS-CDTRQF NOT = "42"
               DISPLAY "CDTRQF クローズ失敗 ST=" FS-CDTRQF
               MOVE 8 TO RETURN-CODE
           END-IF

           DISPLAY "CB120B 終了 残高読込=" CNT-OS-READ
                   " 口座読込=" CNT-AC-READ
                   " 請求出力=" CNT-TRQ-WRITE
                   " 対象外=" CNT-SKIP
                   " 例外=" CNT-ERROR

           IF HARD-ERROR
               IF RETURN-CODE = 0
                   MOVE 12 TO RETURN-CODE
               END-IF
           ELSE
               IF RETURN-CODE = 0
                   MOVE 0 TO RETURN-CODE
               END-IF
           END-IF.
