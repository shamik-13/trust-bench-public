       IDENTIFICATION DIVISION.
       PROGRAM-ID. TG570B.
      *===============================================================*
      * 変更履歴                                                      *
      * 版数  年月日      担当                         概要           *
      * 1.00  平成30年04月  システム部 情報系チーム     新規作成       *
      * 1.10  令和02年10月  システム部 対外系チーム     再振込対応     *
      * 1.20  令和05年06月  システム部 情報系/対外系チーム 表示見直し  *
      *===============================================================*

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TGRTRF ASSIGN TO "TGRTRF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS RT-RETURN-KEY
               FILE STATUS IS FS-TGRTRF.
           SELECT KZACCTF ASSIGN TO "KZACCTF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS AC-ACCT-NO
               FILE STATUS IS FS-KZACCTF.
           SELECT TGBANKF ASSIGN TO "TGBANKF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS BK-COUNTER-BANK
               FILE STATUS IS FS-TGBANKF.
           SELECT TGZENF ASSIGN TO "TGZENF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-TGZENF.
           SELECT TGERRF ASSIGN TO "TGERRF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-TGERRF.
           SELECT TGACKF ASSIGN TO "TGACKF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-TGACKF.

       DATA DIVISION.
       FILE SECTION.
       FD  TGRTRF.
       COPY TGRTRFC.
       FD  KZACCTF.
       COPY KZACCTC2.
       FD  TGBANKF.
       COPY TGBANKFC.
       FD  TGZENF.
       COPY TGZENFC.
       FD  TGERRF.
       COPY TGERRFC.
       FD  TGACKF.
       COPY TGACKFC.

       WORKING-STORAGE SECTION.
       01  FS-AREA.
           05 FS-TGRTRF              PIC XX.
           05 FS-KZACCTF             PIC XX.
           05 FS-TGBANKF             PIC XX.
           05 FS-TGZENF              PIC XX.
           05 FS-TGERRF              PIC XX.
           05 FS-TGACKF              PIC XX.

       01  SW-AREA.
           05 SW-END                 PIC X VALUE "N".
              88 END-OF-TGRTRF             VALUE "Y".
           05 SW-HARD-ERR            PIC X VALUE "N".
              88 HARD-ERROR                VALUE "Y".
           05 SW-ITEM-ERR            PIC X VALUE "N".
              88 ITEM-ERROR                VALUE "Y".

       01  CNT-AREA.
           05 CNT-READ               PIC 9(9) VALUE 0.
           05 CNT-ZEN                PIC 9(9) VALUE 0.
           05 CNT-ERR                PIC 9(9) VALUE 0.
           05 CNT-ACK                PIC 9(9) VALUE 0.
           05 CNT-SKIP               PIC 9(9) VALUE 0.

       01  KEY-AREA.
           05 SAVE-RETURN-KEY        PIC X(40) VALUE SPACE.
           05 FIRST-KEY-SW           PIC X VALUE "Y".
              88 FIRST-KEY                 VALUE "Y".

       01  DATE-AREA.
           05 SYS-DATE               PIC 9(8).
           05 WK-VALUE-DT            PIC 9(8).
           05 WK-VALID-FROM          PIC 9(8).
           05 WK-VALID-TO            PIC 9(8).
           05 WK-ELAPSED-DAYS        PIC S9(9) COMP-5.
           05 WK-REENTRY-LIMIT       PIC S9(9) COMP-5 VALUE 10.

       01  EDIT-AREA.
           05 WK-ERROR-CD            PIC X(04).
           05 WK-ERROR-FIELD         PIC X(20).
           05 WK-ERROR-TEXT          PIC X(60).
           05 WK-RESULT-CD           PIC X(04).
           05 WK-NOTICE-TEXT         PIC X(60).

       PROCEDURE DIVISION.
       MAIN-RTN.
           MOVE 0 TO RETURN-CODE
           ACCEPT SYS-DATE FROM DATE YYYYMMDD
           PERFORM OPEN-RTN
           IF NOT HARD-ERROR
              PERFORM READ-TGRTRF
              PERFORM UNTIL END-OF-TGRTRF OR HARD-ERROR
                 ADD 1 TO CNT-READ
                 PERFORM PROCESS-RTN
                 PERFORM READ-TGRTRF
              END-PERFORM
           END-IF
           PERFORM CLOSE-RTN
           IF HARD-ERROR
              MOVE 8 TO RETURN-CODE
           ELSE
              MOVE 0 TO RETURN-CODE
              DISPLAY "TG570B 正常 読込=" CNT-READ
                      " 全銀=" CNT-ZEN
                      " エラー=" CNT-ERR
                      " 通知=" CNT-ACK
                      " 対象外=" CNT-SKIP
           END-IF
           GOBACK.

       OPEN-RTN.
           OPEN INPUT TGRTRF KZACCTF TGBANKF
                OUTPUT TGZENF TGERRF TGACKF
           IF FS-TGRTRF NOT = "00"
              DISPLAY "TGRTRF オープンエラー ST=" FS-TGRTRF
              SET HARD-ERROR TO TRUE
           END-IF
           IF FS-KZACCTF NOT = "00"
              DISPLAY "KZACCTF オープンエラー ST=" FS-KZACCTF
              SET HARD-ERROR TO TRUE
           END-IF
           IF FS-TGBANKF NOT = "00"
              DISPLAY "TGBANKF オープンエラー ST=" FS-TGBANKF
              SET HARD-ERROR TO TRUE
           END-IF
           IF FS-TGZENF NOT = "00"
              DISPLAY "TGZENF オープンエラー ST=" FS-TGZENF
              SET HARD-ERROR TO TRUE
           END-IF
           IF FS-TGERRF NOT = "00"
              DISPLAY "TGERRF オープンエラー ST=" FS-TGERRF
              SET HARD-ERROR TO TRUE
           END-IF
           IF FS-TGACKF NOT = "00"
              DISPLAY "TGACKF オープンエラー ST=" FS-TGACKF
              SET HARD-ERROR TO TRUE
           END-IF.

       READ-TGRTRF.
           READ TGRTRF
              AT END
                 SET END-OF-TGRTRF TO TRUE
              NOT AT END
                 IF FS-TGRTRF NOT = "00"
                    DISPLAY "TGRTRF 読込エラー ST=" FS-TGRTRF
                    SET HARD-ERROR TO TRUE
                 END-IF
           END-READ.

       PROCESS-RTN.
           MOVE "N" TO SW-ITEM-ERR
           IF RT-REENTRY-FLAG NOT = "1"
              ADD 1 TO CNT-SKIP
              GO TO PROCESS-EXIT
           END-IF

           PERFORM DUP-CHECK-RTN
           IF ITEM-ERROR
              GO TO PROCESS-EXIT
           END-IF

           PERFORM DATE-CHECK-RTN
           IF ITEM-ERROR
              GO TO PROCESS-EXIT
           END-IF

           PERFORM BANK-CHECK-RTN
           IF ITEM-ERROR
              GO TO PROCESS-EXIT
           END-IF

           PERFORM ACCOUNT-CHECK-RTN
           IF ITEM-ERROR
              GO TO PROCESS-EXIT
           END-IF

           PERFORM TG541S-EDIT-RTN
           PERFORM WRITE-ZEN-RTN
           IF NOT HARD-ERROR
              PERFORM WRITE-ACK-OK-RTN
           END-IF
           .
       PROCESS-EXIT.
           EXIT.

       DUP-CHECK-RTN.
           IF FIRST-KEY
              MOVE "N" TO FIRST-KEY-SW
              MOVE RT-RETURN-KEY TO SAVE-RETURN-KEY
           ELSE
              IF RT-RETURN-KEY = SAVE-RETURN-KEY
                 MOVE "E101" TO WK-ERROR-CD
                 MOVE "RT-RETURN-KEY" TO WK-ERROR-FIELD
                 MOVE "返却キー重複" TO WK-ERROR-TEXT
                 MOVE "DUPL" TO WK-RESULT-CD
                 MOVE "再振込返却キー重複" TO WK-NOTICE-TEXT
                 PERFORM WRITE-ERR-RTN
                 PERFORM WRITE-ACK-NG-RTN
                 SET ITEM-ERROR TO TRUE
              ELSE
                 MOVE RT-RETURN-KEY TO SAVE-RETURN-KEY
              END-IF
           END-IF.

       DATE-CHECK-RTN.
           IF RT-VALUE-DT IS NOT NUMERIC
              MOVE "E201" TO WK-ERROR-CD
              MOVE "RT-VALUE-DT" TO WK-ERROR-FIELD
              MOVE "再振込日不正" TO WK-ERROR-TEXT
              MOVE "DATE" TO WK-RESULT-CD
              MOVE "再振込日不正" TO WK-NOTICE-TEXT
              PERFORM WRITE-ERR-RTN
              PERFORM WRITE-ACK-NG-RTN
              SET ITEM-ERROR TO TRUE
           ELSE
              MOVE RT-VALUE-DT TO WK-VALUE-DT
              COMPUTE WK-ELAPSED-DAYS =
                  FUNCTION INTEGER-OF-DATE(SYS-DATE)
                - FUNCTION INTEGER-OF-DATE(WK-VALUE-DT)
              IF WK-ELAPSED-DAYS < 0
                 MOVE "E202" TO WK-ERROR-CD
                 MOVE "RT-VALUE-DT" TO WK-ERROR-FIELD
                 MOVE "再振込日未来日" TO WK-ERROR-TEXT
                 MOVE "DATE" TO WK-RESULT-CD
                 MOVE "未来日は指定不可" TO WK-NOTICE-TEXT
                 PERFORM WRITE-ERR-RTN
                 PERFORM WRITE-ACK-NG-RTN
                 SET ITEM-ERROR TO TRUE
              END-IF
              IF NOT ITEM-ERROR
                 IF WK-ELAPSED-DAYS > WK-REENTRY-LIMIT
                    MOVE "E203" TO WK-ERROR-CD
                    MOVE "RT-VALUE-DT" TO WK-ERROR-FIELD
                    MOVE "再振込期限切れ" TO WK-ERROR-TEXT
                    MOVE "TERM" TO WK-RESULT-CD
                    MOVE "再振込期限切れ" TO WK-NOTICE-TEXT
                    PERFORM WRITE-ERR-RTN
                    PERFORM WRITE-ACK-NG-RTN
                    SET ITEM-ERROR TO TRUE
                 END-IF
              END-IF
           END-IF.

       BANK-CHECK-RTN.
           MOVE RT-COUNTER-BANK TO BK-COUNTER-BANK
           READ TGBANKF
              INVALID KEY
                 MOVE "E301" TO WK-ERROR-CD
                 MOVE "RT-COUNTER-BANK" TO WK-ERROR-FIELD
                 MOVE "相手銀行未登録" TO WK-ERROR-TEXT
                 MOVE "BANK" TO WK-RESULT-CD
                 MOVE "相手銀行未登録" TO WK-NOTICE-TEXT
                 PERFORM WRITE-ERR-RTN
                 PERFORM WRITE-ACK-NG-RTN
                 SET ITEM-ERROR TO TRUE
              NOT INVALID KEY
                 IF FS-TGBANKF NOT = "00"
                    DISPLAY "TGBANKF 読込エラー ST=" FS-TGBANKF
                    SET HARD-ERROR TO TRUE
                    SET ITEM-ERROR TO TRUE
                 END-IF
           END-READ

           IF NOT ITEM-ERROR
              IF BK-STATUS NOT = "1"
                 MOVE "E302" TO WK-ERROR-CD
                 MOVE "BK-STATUS" TO WK-ERROR-FIELD
                 MOVE "相手銀行停止中" TO WK-ERROR-TEXT
                 MOVE "BSTOP" TO WK-RESULT-CD
                 MOVE "相手銀行停止中" TO WK-NOTICE-TEXT
                 PERFORM WRITE-ERR-RTN
                 PERFORM WRITE-ACK-NG-RTN
                 SET ITEM-ERROR TO TRUE
              END-IF
           END-IF

           IF NOT ITEM-ERROR
              MOVE BK-VALID-FROM TO WK-VALID-FROM
              MOVE BK-VALID-TO TO WK-VALID-TO
              IF WK-VALUE-DT < WK-VALID-FROM
                 MOVE "E303" TO WK-ERROR-CD
                 MOVE "BK-VALID-FROM" TO WK-ERROR-FIELD
                 MOVE "銀行有効開始日前" TO WK-ERROR-TEXT
                 MOVE "BDAY" TO WK-RESULT-CD
                 MOVE "銀行有効期間外" TO WK-NOTICE-TEXT
                 PERFORM WRITE-ERR-RTN
                 PERFORM WRITE-ACK-NG-RTN
                 SET ITEM-ERROR TO TRUE
              END-IF
           END-IF

           IF NOT ITEM-ERROR
              IF WK-VALUE-DT > WK-VALID-TO
                 MOVE "E304" TO WK-ERROR-CD
                 MOVE "BK-VALID-TO" TO WK-ERROR-FIELD
                 MOVE "銀行有効終了日後" TO WK-ERROR-TEXT
                 MOVE "BDAY" TO WK-RESULT-CD
                 MOVE "銀行有効期間外" TO WK-NOTICE-TEXT
                 PERFORM WRITE-ERR-RTN
                 PERFORM WRITE-ACK-NG-RTN
                 SET ITEM-ERROR TO TRUE
              END-IF
           END-IF.

       ACCOUNT-CHECK-RTN.
           MOVE RT-RECV-ACCT-NO TO AC-ACCT-NO
           READ KZACCTF
              INVALID KEY
                 MOVE "E401" TO WK-ERROR-CD
                 MOVE "RT-RECV-ACCT-NO" TO WK-ERROR-FIELD
                 MOVE "受取口座未登録" TO WK-ERROR-TEXT
                 MOVE "ACCT" TO WK-RESULT-CD
                 MOVE "受取口座未登録" TO WK-NOTICE-TEXT
                 PERFORM WRITE-ERR-RTN
                 PERFORM WRITE-ACK-NG-RTN
                 SET ITEM-ERROR TO TRUE
              NOT INVALID KEY
                 IF FS-KZACCTF NOT = "00"
                    DISPLAY "KZACCTF 読込エラー ST=" FS-KZACCTF
                    SET HARD-ERROR TO TRUE
                    SET ITEM-ERROR TO TRUE
                 END-IF
           END-READ

           IF NOT ITEM-ERROR
              IF AC-STATUS NOT = "0"
                 MOVE "E402" TO WK-ERROR-CD
                 MOVE "AC-STATUS" TO WK-ERROR-FIELD
                 MOVE "口座状態有効外" TO WK-ERROR-TEXT
                 MOVE "ASTS" TO WK-RESULT-CD
                 MOVE "口座状態有効外" TO WK-NOTICE-TEXT
                 PERFORM WRITE-ERR-RTN
                 PERFORM WRITE-ACK-NG-RTN
                 SET ITEM-ERROR TO TRUE
              END-IF
           END-IF.

       TG541S-EDIT-RTN.
           INITIALIZE TGZENF-REC
           MOVE RT-VALUE-DT TO ZE-VALUE-DT
           MOVE RT-CENTER-SEQ TO ZE-CENTER-SEQ
           MOVE "02" TO ZE-ZEN-TYPE
           MOVE RT-COUNTER-BANK TO ZE-COUNTER-BANK
           MOVE AC-BRANCH TO ZE-COUNTER-BRANCH
           MOVE RT-RECV-ACCT-NO TO ZE-RECV-ACCT-NO
           MOVE RT-REMIT-AMT TO ZE-REMIT-AMT
           MOVE AC-ACCT-NAME-KANA TO ZE-REMIT-NAME-KANA.

       WRITE-ZEN-RTN.
           WRITE TGZENF-REC
           IF FS-TGZENF NOT = "00"
              DISPLAY "TGZENF 書込エラー ST=" FS-TGZENF
              SET HARD-ERROR TO TRUE
           ELSE
              ADD 1 TO CNT-ZEN
           END-IF.

       WRITE-ERR-RTN.
           INITIALIZE TGERRF-REC
           MOVE RT-VALUE-DT TO ER-VALUE-DT
           MOVE RT-CENTER-SEQ TO ER-CENTER-SEQ
           MOVE WK-ERROR-CD TO ER-ERROR-CD
           MOVE WK-ERROR-FIELD TO ER-ERROR-FIELD
           MOVE WK-ERROR-TEXT TO ER-ERROR-TEXT
           MOVE TGRTRF-REC TO ER-RAW-RECORD
           WRITE TGERRF-REC
           IF FS-TGERRF NOT = "00"
              DISPLAY "TGERRF 書込エラー ST=" FS-TGERRF
              SET HARD-ERROR TO TRUE
           ELSE
              ADD 1 TO CNT-ERR
           END-IF.

       WRITE-ACK-NG-RTN.
           INITIALIZE TGACKF-REC
           MOVE RT-VALUE-DT TO AK-VALUE-DT
           MOVE RT-CENTER-SEQ TO AK-CENTER-SEQ
           MOVE "NG" TO AK-NOTICE-TYPE
           MOVE RT-COUNTER-BANK TO AK-COUNTER-BANK
           MOVE WK-RESULT-CD TO AK-RESULT-CD
           MOVE 1 TO AK-ITEM-COUNT
           MOVE WK-NOTICE-TEXT TO AK-NOTICE-TEXT
           WRITE TGACKF-REC
           IF FS-TGACKF NOT = "00"
              DISPLAY "TGACKF 書込エラー ST=" FS-TGACKF
              SET HARD-ERROR TO TRUE
           ELSE
              ADD 1 TO CNT-ACK
           END-IF.

       WRITE-ACK-OK-RTN.
           INITIALIZE TGACKF-REC
           MOVE RT-VALUE-DT TO AK-VALUE-DT
           MOVE RT-CENTER-SEQ TO AK-CENTER-SEQ
           MOVE "OK" TO AK-NOTICE-TYPE
           MOVE RT-COUNTER-BANK TO AK-COUNTER-BANK
           MOVE "0000" TO AK-RESULT-CD
           MOVE 1 TO AK-ITEM-COUNT
           MOVE "再振込受付済" TO AK-NOTICE-TEXT
           WRITE TGACKF-REC
           IF FS-TGACKF NOT = "00"
              DISPLAY "TGACKF 書込エラー ST=" FS-TGACKF
              SET HARD-ERROR TO TRUE
           ELSE
              ADD 1 TO CNT-ACK
           END-IF.

       CLOSE-RTN.
           CLOSE TGRTRF KZACCTF TGBANKF TGZENF TGERRF TGACKF
           IF FS-TGRTRF NOT = "00"
              DISPLAY "TGRTRF クローズエラー ST=" FS-TGRTRF
              SET HARD-ERROR TO TRUE
           END-IF
           IF FS-KZACCTF NOT = "00"
              DISPLAY "KZACCTF クローズエラー ST=" FS-KZACCTF
              SET HARD-ERROR TO TRUE
           END-IF
           IF FS-TGBANKF NOT = "00"
              DISPLAY "TGBANKF クローズエラー ST=" FS-TGBANKF
              SET HARD-ERROR TO TRUE
           END-IF
           IF FS-TGZENF NOT = "00"
              DISPLAY "TGZENF クローズエラー ST=" FS-TGZENF
              SET HARD-ERROR TO TRUE
           END-IF
           IF FS-TGERRF NOT = "00"
              DISPLAY "TGERRF クローズエラー ST=" FS-TGERRF
              SET HARD-ERROR TO TRUE
           END-IF
           IF FS-TGACKF NOT = "00"
              DISPLAY "TGACKF クローズエラー ST=" FS-TGACKF
              SET HARD-ERROR TO TRUE
           END-IF.
