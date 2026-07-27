       IDENTIFICATION DIVISION.
       PROGRAM-ID. CB160B.
       AUTHOR. TRUST-BATCH.
      *----------------------------------------------------------------*
      * 過入金返金抽出バッチ                                           *
      * 消込残額が返金しきい値を超える入金を返金候補として抽出する。   *
      * 登録口座が有効な場合は返金明細を作成し、不備または保留対象は   *
      * 履歴へ返金保留として記録する。                                 *
      *----------------------------------------------------------------*

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CDAPPF ASSIGN TO "CDAPPF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-CDAPPF.
           SELECT CDPAYF ASSIGN TO "CDPAYF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-CDPAYF.
           SELECT CDACTF ASSIGN TO "CDACTF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS AC-ACCOUNT-ID
               FILE STATUS IS FS-CDACTF.
           SELECT CDREFDF ASSIGN TO "CDREFDF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS REF-REFUND-ID
               FILE STATUS IS FS-CDREFDF.
           SELECT CDHISTF ASSIGN TO "CDHISTF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS HIS-CARD-NO
               FILE STATUS IS FS-CDHISTF.

       DATA DIVISION.
       FILE SECTION.

       FD  CDAPPF.
           COPY CDAPPFC.

       FD  CDPAYF.
           COPY CDPAYFC.

       FD  CDACTF.
           COPY CDACTC.

       FD  CDREFDF.
           COPY CDREFDC.

       FD  CDHISTF.
           COPY CDHISTC.

       WORKING-STORAGE SECTION.
       77  FS-CDAPPF                 PIC XX VALUE SPACE.
       77  FS-CDPAYF                 PIC XX VALUE SPACE.
       77  FS-CDACTF                 PIC XX VALUE SPACE.
       77  FS-CDREFDF                PIC XX VALUE SPACE.
       77  FS-CDHISTF                PIC XX VALUE SPACE.

       77  EOF-CDAPPF                PIC X VALUE "N".
       77  EOF-CDPAYF                PIC X VALUE "N".
       77  EOF-CDACTF                PIC X VALUE "N".

       77  WK-ABEND-FLG              PIC X VALUE "N".
       77  WK-FOUND-PAY              PIC X VALUE "N".
       77  WK-FOUND-ACT              PIC X VALUE "N".
       77  WK-DUP-PAY-FLG            PIC X VALUE "N".
       77  WK-HOLD-FLG               PIC X VALUE "N".

       77  WK-APP-CNT                PIC 9(9) VALUE 0.
       77  WK-REF-CNT                PIC 9(9) VALUE 0.
       77  WK-HOLD-CNT               PIC 9(9) VALUE 0.
       77  WK-SKIP-CNT               PIC 9(9) VALUE 0.
       77  WK-ERR-CNT                PIC 9(9) VALUE 0.
       77  WK-HIS-SEQ                PIC 9(5) VALUE 0.

       77  WK-REFUND-THRESHOLD       PIC 9(11)V99 VALUE 1000.
       77  WK-MIN-REFUND-AMT         PIC 9(11)V99 VALUE 1.
       77  WK-TODAY                  PIC 9(8) VALUE ZERO.

       77  IX-PAY                    PIC 9(5) VALUE 0.
       77  IX-SRCH                   PIC 9(5) VALUE 0.
       77  PAY-MAX                   PIC 9(5) VALUE 5000.
       77  PAY-CNT                   PIC 9(5) VALUE 0.

       01  WK-REFUND-ID.
           05 WK-REF-PREFIX          PIC X(2) VALUE "RF".
           05 WK-REF-DATE            PIC 9(8) VALUE ZERO.
           05 WK-REF-SERIAL          PIC 9(8) VALUE ZERO.

       01  WK-HOLD-REASON            PIC X(20) VALUE SPACE.
       01  WK-DISPLAY-AMT            PIC ZZZ,ZZZ,ZZZ,ZZ9.99.
       01  WK-DISPLAY-CNT            PIC ZZZ,ZZZ,ZZ9.

       01  PAY-TABLE.
           05 PAY-ENTRY OCCURS 5000 TIMES.
              10 TB-PY-PAY-ID        PIC X(20).
              10 TB-PY-CARD-NO       PIC X(20).
              10 TB-PY-PAY-AMT       PIC 9(11)V99.
              10 TB-PY-PAY-DT        PIC 9(8).
              10 TB-PY-PAY-METHOD    PIC X(2).

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE 0 TO RETURN-CODE
           ACCEPT WK-TODAY FROM DATE YYYYMMDD
           PERFORM 1000-OPEN-FILES
           IF WK-ABEND-FLG = "N"
              PERFORM 2000-LOAD-PAY
           END-IF
           IF WK-ABEND-FLG = "N"
              PERFORM 3000-PROCESS-APP UNTIL EOF-CDAPPF = "Y"
           END-IF
           PERFORM 9000-CLOSE-FILES
           IF WK-ABEND-FLG = "Y"
              MOVE 8 TO RETURN-CODE
           ELSE
              MOVE 0 TO RETURN-CODE
              PERFORM 9100-DISPLAY-SUMMARY
           END-IF
           GOBACK.

       1000-OPEN-FILES.
           OPEN INPUT CDAPPF
           IF FS-CDAPPF NOT = "00"
              DISPLAY "CDAPPF オープン失敗 ST=" FS-CDAPPF
              MOVE "Y" TO WK-ABEND-FLG
           END-IF

           OPEN INPUT CDPAYF
           IF FS-CDPAYF NOT = "00"
              DISPLAY "CDPAYF オープン失敗 ST=" FS-CDPAYF
              MOVE "Y" TO WK-ABEND-FLG
           END-IF

           OPEN INPUT CDACTF
           IF FS-CDACTF NOT = "00"
              DISPLAY "CDACTF オープン失敗 ST=" FS-CDACTF
              MOVE "Y" TO WK-ABEND-FLG
           END-IF

           OPEN OUTPUT CDREFDF
           IF FS-CDREFDF NOT = "00"
              DISPLAY "CDREFDF オープン失敗 ST=" FS-CDREFDF
              MOVE "Y" TO WK-ABEND-FLG
           END-IF

           OPEN OUTPUT CDHISTF
           IF FS-CDHISTF NOT = "00"
              DISPLAY "CDHISTF オープン失敗 ST=" FS-CDHISTF
              MOVE "Y" TO WK-ABEND-FLG
           END-IF.

       2000-LOAD-PAY.
           PERFORM UNTIL EOF-CDPAYF = "Y" OR WK-ABEND-FLG = "Y"
              READ CDPAYF
                 AT END
                    MOVE "Y" TO EOF-CDPAYF
                 NOT AT END
                    IF PAY-CNT >= PAY-MAX
                       DISPLAY "CDPAYF 件数超過"
                       MOVE "Y" TO WK-ABEND-FLG
                    ELSE
                       ADD 1 TO PAY-CNT
                       MOVE PY-PAY-ID     TO TB-PY-PAY-ID(PAY-CNT)
                       MOVE PY-CARD-NO    TO TB-PY-CARD-NO(PAY-CNT)
                       MOVE PY-PAY-AMT    TO TB-PY-PAY-AMT(PAY-CNT)
                       MOVE PY-PAY-DT     TO TB-PY-PAY-DT(PAY-CNT)
                       MOVE PY-PAY-METHOD TO TB-PY-PAY-METHOD(PAY-CNT)
                    END-IF
              END-READ
              IF FS-CDPAYF NOT = "00" AND FS-CDPAYF NOT = "10"
                 DISPLAY "CDPAYF 読込失敗 ST=" FS-CDPAYF
                 MOVE "Y" TO WK-ABEND-FLG
              END-IF
           END-PERFORM.

       3000-PROCESS-APP.
           READ CDAPPF
              AT END
                 MOVE "Y" TO EOF-CDAPPF
              NOT AT END
                 ADD 1 TO WK-APP-CNT
                 PERFORM 3100-EVALUATE-APP
           END-READ
           IF FS-CDAPPF NOT = "00" AND FS-CDAPPF NOT = "10"
              DISPLAY "CDAPPF 読込失敗 ST=" FS-CDAPPF
              MOVE "Y" TO WK-ABEND-FLG
           END-IF.

       3100-EVALUATE-APP.
           MOVE "N" TO WK-FOUND-PAY
           MOVE "N" TO WK-DUP-PAY-FLG
           MOVE "N" TO WK-HOLD-FLG
           MOVE SPACE TO WK-HOLD-REASON

           IF AP-APP-STATUS NOT = "O"
              ADD 1 TO WK-SKIP-CNT
           ELSE
              IF AP-REMAIN-AMT <= WK-REFUND-THRESHOLD
                 ADD 1 TO WK-SKIP-CNT
              ELSE
                 PERFORM 3200-FIND-PAY
                 IF WK-FOUND-PAY = "N"
                    MOVE "入金情報なし" TO WK-HOLD-REASON
                    PERFORM 3600-WRITE-HOLD-HISTORY
                 ELSE
                    PERFORM 3300-CHECK-SAME-DAY-PAY
                    PERFORM 3400-FIND-ACT
                    IF WK-HOLD-FLG = "Y"
                       PERFORM 3600-WRITE-HOLD-HISTORY
                    ELSE
                       PERFORM 3500-WRITE-REFUND
                    END-IF
                 END-IF
              END-IF
           END-IF.

       3200-FIND-PAY.
           PERFORM VARYING IX-PAY FROM 1 BY 1
              UNTIL IX-PAY > PAY-CNT OR WK-FOUND-PAY = "Y"
              IF TB-PY-PAY-ID(IX-PAY) = AP-PAY-ID
                 MOVE "Y" TO WK-FOUND-PAY
              END-IF
           END-PERFORM
           IF WK-FOUND-PAY = "Y"
              IF TB-PY-CARD-NO(IX-PAY) NOT = AP-CARD-NO
                 MOVE "Y" TO WK-HOLD-FLG
                 MOVE "カード番号不一致" TO WK-HOLD-REASON
              END-IF
              IF TB-PY-PAY-AMT(IX-PAY) < AP-REMAIN-AMT
                 MOVE "Y" TO WK-HOLD-FLG
                 MOVE "残額が入金額超過" TO WK-HOLD-REASON
              END-IF
              IF TB-PY-PAY-METHOD(IX-PAY) NOT = "10"
                 AND TB-PY-PAY-METHOD(IX-PAY) NOT = "20"
                 AND TB-PY-PAY-METHOD(IX-PAY) NOT = "30"
                 MOVE "Y" TO WK-HOLD-FLG
                 MOVE "入金方法不正" TO WK-HOLD-REASON
              END-IF
           END-IF.

       3300-CHECK-SAME-DAY-PAY.
           IF WK-HOLD-FLG = "N"
              PERFORM VARYING IX-SRCH FROM 1 BY 1
                 UNTIL IX-SRCH > PAY-CNT
                 IF IX-SRCH NOT = IX-PAY
                    IF TB-PY-CARD-NO(IX-SRCH) = AP-CARD-NO
                       AND TB-PY-PAY-DT(IX-SRCH) =
                           TB-PY-PAY-DT(IX-PAY)
                       MOVE "Y" TO WK-DUP-PAY-FLG
                    END-IF
                 END-IF
              END-PERFORM
              IF WK-DUP-PAY-FLG = "Y"
                 MOVE "Y" TO WK-HOLD-FLG
                 MOVE "同日複数入金" TO WK-HOLD-REASON
              END-IF
           END-IF.

       3400-FIND-ACT.
           MOVE "N" TO WK-FOUND-ACT
           IF WK-HOLD-FLG = "N"
              MOVE LOW-VALUE TO AC-ACCOUNT-ID
              START CDACTF KEY IS >= AC-ACCOUNT-ID
                 INVALID KEY
                    MOVE "Y" TO WK-HOLD-FLG
                    MOVE "登録口座なし" TO WK-HOLD-REASON
                 NOT INVALID KEY
                    MOVE "N" TO EOF-CDACTF
              END-START
              IF FS-CDACTF NOT = "00" AND FS-CDACTF NOT = "23"
                 DISPLAY "CDACTF START失敗 ST=" FS-CDACTF
                 MOVE "Y" TO WK-ABEND-FLG
              END-IF
              PERFORM UNTIL EOF-CDACTF = "Y"
                 OR WK-FOUND-ACT = "Y"
                 OR WK-ABEND-FLG = "Y"
                 READ CDACTF NEXT RECORD
                    AT END
                       MOVE "Y" TO EOF-CDACTF
                    NOT AT END
                       IF AC-CARD-NO = AP-CARD-NO
                          MOVE "Y" TO WK-FOUND-ACT
                       END-IF
                 END-READ
                 IF FS-CDACTF NOT = "00" AND FS-CDACTF NOT = "10"
                    DISPLAY "CDACTF 読込失敗 ST=" FS-CDACTF
                    MOVE "Y" TO WK-ABEND-FLG
                 END-IF
              END-PERFORM
              IF WK-ABEND-FLG = "N"
                 IF WK-FOUND-ACT = "N"
                    MOVE "Y" TO WK-HOLD-FLG
                    MOVE "登録口座なし" TO WK-HOLD-REASON
                 ELSE
                    PERFORM 3410-VALIDATE-ACT
                 END-IF
              END-IF
           END-IF.

       3410-VALIDATE-ACT.
           IF AC-TRANSFER-STATUS NOT = "0"
              MOVE "Y" TO WK-HOLD-FLG
              MOVE "本人確認保留" TO WK-HOLD-REASON
           END-IF

           IF AC-BANK-CD = SPACE OR AC-BANK-CD = ZERO
              MOVE "Y" TO WK-HOLD-FLG
              MOVE "銀行コード不備" TO WK-HOLD-REASON
           END-IF

           IF AC-BRANCH-CD = SPACE OR AC-BRANCH-CD = ZERO
              MOVE "Y" TO WK-HOLD-FLG
              MOVE "支店コード不備" TO WK-HOLD-REASON
           END-IF

           IF AC-ACCOUNT-NO = SPACE OR AC-ACCOUNT-NO = ZERO
              MOVE "Y" TO WK-HOLD-FLG
              MOVE "口座番号不備" TO WK-HOLD-REASON
           END-IF

           IF AC-HOLDER-KANA = SPACE
              MOVE "Y" TO WK-HOLD-FLG
              MOVE "名義カナ不備" TO WK-HOLD-REASON
           END-IF

           IF AC-DEPOSIT-TYPE NOT = "1" AND AC-DEPOSIT-TYPE NOT = "2"
              MOVE "Y" TO WK-HOLD-FLG
              MOVE "預金種目不正" TO WK-HOLD-REASON
           END-IF.

       3500-WRITE-REFUND.
           IF AP-REMAIN-AMT >= WK-MIN-REFUND-AMT
              MOVE "RF" TO WK-REF-PREFIX
              MOVE WK-TODAY TO WK-REF-DATE
              ADD 1 TO WK-REF-SERIAL
              MOVE WK-REFUND-ID TO REF-REFUND-ID
              MOVE AP-CARD-NO TO REF-CARD-NO
              MOVE AP-PAY-ID TO REF-PAY-ID
              MOVE AP-REMAIN-AMT TO REF-REFUND-AMT
              MOVE AC-BANK-CD TO REF-BANK-CD
              MOVE AC-ACCOUNT-NO TO REF-ACCOUNT-NO
              MOVE "N" TO REF-REFUND-STATUS
              MOVE SPACE TO REF-APPROVAL-ID
              WRITE CDREFDF-REC
                 INVALID KEY
                    DISPLAY "CDREFDF 書込重複 ID=" REF-REFUND-ID
                    MOVE "Y" TO WK-ABEND-FLG
                 NOT INVALID KEY
                    ADD 1 TO WK-REF-CNT
                    PERFORM 3700-WRITE-REF-HISTORY
              END-WRITE
              IF FS-CDREFDF NOT = "00"
                 DISPLAY "CDREFDF 書込失敗 ST=" FS-CDREFDF
                 MOVE "Y" TO WK-ABEND-FLG
              END-IF
           ELSE
              ADD 1 TO WK-SKIP-CNT
           END-IF.

       3600-WRITE-HOLD-HISTORY.
           ADD 1 TO WK-HOLD-CNT
           ADD 1 TO WK-HIS-SEQ
           MOVE AP-CARD-NO TO HIS-CARD-NO
           MOVE AP-PAY-ID TO HIS-PAY-ID
           MOVE WK-HIS-SEQ TO HIS-EVENT-SEQ
           MOVE "HOLD" TO HIS-EVENT-TYPE
           MOVE AP-REMAIN-AMT TO HIS-EVENT-AMT
           MOVE WK-TODAY TO HIS-EVENT-DT
           MOVE "CB160B" TO HIS-SOURCE-PROGRAM
           WRITE CDHISTF-REC
              INVALID KEY
                 DISPLAY "CDHISTF 保留履歴重複 CARD=" HIS-CARD-NO
                 ADD 1 TO WK-ERR-CNT
              NOT INVALID KEY
                 DISPLAY "返金保留 CARD=" AP-CARD-NO
                         " 理由=" WK-HOLD-REASON
           END-WRITE
           IF FS-CDHISTF NOT = "00" AND FS-CDHISTF NOT = "22"
              DISPLAY "CDHISTF 保留履歴書込失敗 ST=" FS-CDHISTF
              MOVE "Y" TO WK-ABEND-FLG
           END-IF.

       3700-WRITE-REF-HISTORY.
           ADD 1 TO WK-HIS-SEQ
           MOVE AP-CARD-NO TO HIS-CARD-NO
           MOVE AP-PAY-ID TO HIS-PAY-ID
           MOVE WK-HIS-SEQ TO HIS-EVENT-SEQ
           MOVE "REF" TO HIS-EVENT-TYPE
           MOVE AP-REMAIN-AMT TO HIS-EVENT-AMT
           MOVE WK-TODAY TO HIS-EVENT-DT
           MOVE "CB160B" TO HIS-SOURCE-PROGRAM
           WRITE CDHISTF-REC
              INVALID KEY
                 DISPLAY "CDHISTF 返金履歴重複 CARD=" HIS-CARD-NO
                 ADD 1 TO WK-ERR-CNT
           END-WRITE
           IF FS-CDHISTF NOT = "00" AND FS-CDHISTF NOT = "22"
              DISPLAY "CDHISTF 返金履歴書込失敗 ST=" FS-CDHISTF
              MOVE "Y" TO WK-ABEND-FLG
           END-IF.

       9000-CLOSE-FILES.
           CLOSE CDAPPF
           IF FS-CDAPPF NOT = "00"
              DISPLAY "CDAPPF クローズ失敗 ST=" FS-CDAPPF
              MOVE "Y" TO WK-ABEND-FLG
           END-IF

           CLOSE CDPAYF
           IF FS-CDPAYF NOT = "00"
              DISPLAY "CDPAYF クローズ失敗 ST=" FS-CDPAYF
              MOVE "Y" TO WK-ABEND-FLG
           END-IF

           CLOSE CDACTF
           IF FS-CDACTF NOT = "00"
              DISPLAY "CDACTF クローズ失敗 ST=" FS-CDACTF
              MOVE "Y" TO WK-ABEND-FLG
           END-IF

           CLOSE CDREFDF
           IF FS-CDREFDF NOT = "00"
              DISPLAY "CDREFDF クローズ失敗 ST=" FS-CDREFDF
              MOVE "Y" TO WK-ABEND-FLG
           END-IF

           CLOSE CDHISTF
           IF FS-CDHISTF NOT = "00"
              DISPLAY "CDHISTF クローズ失敗 ST=" FS-CDHISTF
              MOVE "Y" TO WK-ABEND-FLG
           END-IF.

       9100-DISPLAY-SUMMARY.
           MOVE WK-APP-CNT TO WK-DISPLAY-CNT
           DISPLAY "CB160B 処理件数=" WK-DISPLAY-CNT
           MOVE WK-REF-CNT TO WK-DISPLAY-CNT
           DISPLAY "CB160B 返金候補=" WK-DISPLAY-CNT
           MOVE WK-HOLD-CNT TO WK-DISPLAY-CNT
           DISPLAY "CB160B 返金保留=" WK-DISPLAY-CNT
           MOVE WK-SKIP-CNT TO WK-DISPLAY-CNT
           DISPLAY "CB160B 対象外件数=" WK-DISPLAY-CNT
           MOVE WK-ERR-CNT TO WK-DISPLAY-CNT
           DISPLAY "CB160B 警告件数=" WK-DISPLAY-CNT.
