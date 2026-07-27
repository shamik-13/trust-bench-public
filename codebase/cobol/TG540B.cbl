       IDENTIFICATION DIVISION.
       PROGRAM-ID. TG540B.
      *
      *  変更履歴
      *  版数  年月日    担当    概要
      *  1.00  20240401  信託    新規作成
      *  1.01  20240920  信託    銀行マスタ照合追加
      *  1.02  20250214  信託    受付通知編集見直し
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TGRCVF ASSIGN TO "TGRCVF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS RV-CENTER-SEQ
               FILE STATUS IS FS-TGRCVF.
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
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-TGZENF.
           SELECT TGERRF ASSIGN TO "TGERRF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-TGERRF.
           SELECT TGACKF ASSIGN TO "TGACKF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-TGACKF.

       DATA DIVISION.
       FILE SECTION.
       FD  TGRCVF.
       COPY TGRCVFC.
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
           05 FS-TGRCVF              PIC X(02) VALUE SPACES.
           05 FS-KZACCTF             PIC X(02) VALUE SPACES.
           05 FS-TGBANKF             PIC X(02) VALUE SPACES.
           05 FS-TGZENF              PIC X(02) VALUE SPACES.
           05 FS-TGERRF              PIC X(02) VALUE SPACES.
           05 FS-TGACKF              PIC X(02) VALUE SPACES.

       01  SW-AREA.
           05 SW-EOF                 PIC X(01) VALUE "N".
              88 EOF-TGRCVF                    VALUE "Y".
              88 NOT-EOF-TGRCVF                VALUE "N".
           05 SW-HARD-ERROR          PIC X(01) VALUE "N".
              88 HARD-ERROR                    VALUE "Y".
              88 NO-HARD-ERROR                 VALUE "N".

       01  CTL-AREA.
           05 CT-READ                PIC 9(09) COMP-3 VALUE 0.
           05 CT-ZEN                 PIC 9(09) COMP-3 VALUE 0.
           05 CT-ERR                 PIC 9(09) COMP-3 VALUE 0.
           05 CT-ACK                 PIC 9(09) COMP-3 VALUE 0.

       01  WK-AREA.
           05 WK-RESULT-CD           PIC X(02) VALUE SPACES.
           05 WK-ERROR-CD            PIC X(04) VALUE SPACES.
           05 WK-ERROR-FIELD         PIC X(16) VALUE SPACES.
           05 WK-ERROR-TEXT          PIC X(80) VALUE SPACES.
           05 WK-NOTICE-TEXT         PIC X(80) VALUE SPACES.

       01  LK-TG541S-PARM.
           05 LK-RAW-REC             PIC X(256).
           05 LK-EDIT-DETAIL.
              10 LK-ZE-VALUE-DT      PIC 9(08).
              10 LK-ZE-CENTER-SEQ    PIC 9(12).
              10 LK-ZE-ZEN-TYPE      PIC X(02).
              10 LK-ZE-COUNTER-BANK  PIC X(04).
              10 LK-ZE-COUNTER-BRANCH PIC X(03).
              10 LK-ZE-RECV-ACCT-NO  PIC X(10).
              10 LK-ZE-REMIT-AMT     PIC 9(13).
              10 LK-ZE-REMIT-NAME-KANA PIC X(48).
           05 LK-ERROR-AREA.
              10 LK-ER-VALUE-DT      PIC 9(08).
              10 LK-ER-CENTER-SEQ    PIC 9(12).
              10 LK-ER-ERROR-CD      PIC X(04).
              10 LK-ER-ERROR-FIELD   PIC X(16).
              10 LK-ER-ERROR-TEXT    PIC X(80).
              10 LK-ER-RAW-RECORD    PIC X(256).
           05 LK-RETURN-STATUS       PIC X(02).

       PROCEDURE DIVISION.
       MAIN-RTN.
           MOVE 0 TO RETURN-CODE
           PERFORM 1000-OPEN
           IF NO-HARD-ERROR
              PERFORM 2000-PROCESS UNTIL EOF-TGRCVF OR HARD-ERROR
           END-IF
           PERFORM 9000-CLOSE
           IF HARD-ERROR
              MOVE 8 TO RETURN-CODE
           ELSE
              MOVE 0 TO RETURN-CODE
              DISPLAY "TG540B 正常終了 読込=" CT-READ
                      " 正常=" CT-ZEN " エラー=" CT-ERR
                      " 通知=" CT-ACK
           END-IF
           GOBACK.

       1000-OPEN.
           SET NOT-EOF-TGRCVF TO TRUE
           SET NO-HARD-ERROR TO TRUE
           OPEN INPUT TGRCVF KZACCTF TGBANKF
                OUTPUT TGZENF TGERRF TGACKF
           IF FS-TGRCVF NOT = "00"
              DISPLAY "TGRCVF オープン失敗 ST=" FS-TGRCVF
              SET HARD-ERROR TO TRUE
           END-IF
           IF FS-KZACCTF NOT = "00"
              DISPLAY "KZACCTF オープン失敗 ST=" FS-KZACCTF
              SET HARD-ERROR TO TRUE
           END-IF
           IF FS-TGBANKF NOT = "00"
              DISPLAY "TGBANKF オープン失敗 ST=" FS-TGBANKF
              SET HARD-ERROR TO TRUE
           END-IF
           IF FS-TGZENF NOT = "00"
              DISPLAY "TGZENF オープン失敗 ST=" FS-TGZENF
              SET HARD-ERROR TO TRUE
           END-IF
           IF FS-TGERRF NOT = "00"
              DISPLAY "TGERRF オープン失敗 ST=" FS-TGERRF
              SET HARD-ERROR TO TRUE
           END-IF
           IF FS-TGACKF NOT = "00"
              DISPLAY "TGACKF オープン失敗 ST=" FS-TGACKF
              SET HARD-ERROR TO TRUE
           END-IF.

       2000-PROCESS.
           READ TGRCVF NEXT RECORD
              AT END
                 SET EOF-TGRCVF TO TRUE
              NOT AT END
                 IF FS-TGRCVF = "00"
                    ADD 1 TO CT-READ
                    PERFORM 3000-VALIDATE-RAW
                 ELSE
                    DISPLAY "TGRCVF 読込失敗 ST=" FS-TGRCVF
                    SET HARD-ERROR TO TRUE
                 END-IF
           END-READ.

       3000-VALIDATE-RAW.
           INITIALIZE LK-TG541S-PARM
           INITIALIZE WK-AREA
           MOVE RV-RAW-RECORD TO LK-RAW-REC
           CALL "TG541S" USING LK-TG541S-PARM
           IF LK-RETURN-STATUS = "00"
              PERFORM 4000-CHECK-MASTER
           ELSE
              MOVE "E1" TO WK-RESULT-CD
              MOVE LK-ER-ERROR-CD TO WK-ERROR-CD
              MOVE LK-ER-ERROR-FIELD TO WK-ERROR-FIELD
              MOVE LK-ER-ERROR-TEXT TO WK-ERROR-TEXT
              PERFORM 5100-WRITE-SUBPGM-ERROR
              PERFORM 7000-WRITE-ACK
           END-IF.

       4000-CHECK-MASTER.
           MOVE LK-ZE-RECV-ACCT-NO TO AC-ACCT-NO
           READ KZACCTF
              INVALID KEY
                 MOVE "E2" TO WK-RESULT-CD
                 MOVE "AC01" TO WK-ERROR-CD
                 MOVE "AC-ACCT-NO" TO WK-ERROR-FIELD
                 MOVE "受取口座番号未登録" TO WK-ERROR-TEXT
                 PERFORM 5200-WRITE-MASTER-ERROR
                 PERFORM 7000-WRITE-ACK
              NOT INVALID KEY
                 IF FS-KZACCTF = "00"
                    PERFORM 4100-CHECK-ACCOUNT
                 ELSE
                    DISPLAY "KZACCTF 読込失敗 ST=" FS-KZACCTF
                    SET HARD-ERROR TO TRUE
                 END-IF
           END-READ.

       4100-CHECK-ACCOUNT.
           IF AC-STATUS NOT = "0"
              MOVE "E2" TO WK-RESULT-CD
              MOVE "AC02" TO WK-ERROR-CD
              MOVE "AC-STATUS" TO WK-ERROR-FIELD
              MOVE "受取口座状態不正" TO WK-ERROR-TEXT
              PERFORM 5200-WRITE-MASTER-ERROR
              PERFORM 7000-WRITE-ACK
           ELSE
              MOVE LK-ZE-COUNTER-BANK TO BK-COUNTER-BANK
              READ TGBANKF
                 INVALID KEY
                    MOVE "E3" TO WK-RESULT-CD
                    MOVE "BK01" TO WK-ERROR-CD
                    MOVE "BK-COUNTER-BANK" TO WK-ERROR-FIELD
                    MOVE "相手行コード未登録" TO WK-ERROR-TEXT
                    PERFORM 5200-WRITE-MASTER-ERROR
                    PERFORM 7000-WRITE-ACK
                 NOT INVALID KEY
                    IF FS-TGBANKF = "00"
                       PERFORM 4200-CHECK-BANK
                    ELSE
                       DISPLAY "TGBANKF 読込失敗 ST=" FS-TGBANKF
                       SET HARD-ERROR TO TRUE
                    END-IF
              END-READ
           END-IF.

       4200-CHECK-BANK.
           IF BK-STATUS NOT = "0"
              MOVE "E3" TO WK-RESULT-CD
              MOVE "BK02" TO WK-ERROR-CD
              MOVE "BK-STATUS" TO WK-ERROR-FIELD
              MOVE "相手銀行状態不正" TO WK-ERROR-TEXT
              PERFORM 5200-WRITE-MASTER-ERROR
              PERFORM 7000-WRITE-ACK
           ELSE
              IF LK-ZE-VALUE-DT < BK-VALID-FROM
                 OR LK-ZE-VALUE-DT > BK-VALID-TO
                 MOVE "E3" TO WK-RESULT-CD
                 MOVE "BK03" TO WK-ERROR-CD
                 MOVE "BK-VALID-FROM" TO WK-ERROR-FIELD
                 MOVE "相手銀行有効日範囲外" TO WK-ERROR-TEXT
                 PERFORM 5200-WRITE-MASTER-ERROR
                 PERFORM 7000-WRITE-ACK
              ELSE
                 PERFORM 6000-WRITE-ZEN
                 IF NO-HARD-ERROR
                    MOVE "00" TO WK-RESULT-CD
                    MOVE "受付正常" TO WK-NOTICE-TEXT
                    PERFORM 7000-WRITE-ACK
                 END-IF
              END-IF
           END-IF.

       5100-WRITE-SUBPGM-ERROR.
           INITIALIZE TGERRF-REC
           MOVE LK-ER-VALUE-DT TO ER-VALUE-DT
           MOVE LK-ER-CENTER-SEQ TO ER-CENTER-SEQ
           MOVE LK-ER-ERROR-CD TO ER-ERROR-CD
           MOVE LK-ER-ERROR-FIELD TO ER-ERROR-FIELD
           MOVE LK-ER-ERROR-TEXT TO ER-ERROR-TEXT
           MOVE LK-ER-RAW-RECORD TO ER-RAW-RECORD
           WRITE TGERRF-REC
           IF FS-TGERRF = "00"
              ADD 1 TO CT-ERR
              MOVE LK-ER-ERROR-TEXT TO WK-NOTICE-TEXT
           ELSE
              DISPLAY "TGERRF 書込失敗 ST=" FS-TGERRF
              SET HARD-ERROR TO TRUE
           END-IF.

       5200-WRITE-MASTER-ERROR.
           INITIALIZE TGERRF-REC
           MOVE LK-ZE-VALUE-DT TO ER-VALUE-DT
           MOVE LK-ZE-CENTER-SEQ TO ER-CENTER-SEQ
           MOVE WK-ERROR-CD TO ER-ERROR-CD
           MOVE WK-ERROR-FIELD TO ER-ERROR-FIELD
           MOVE WK-ERROR-TEXT TO ER-ERROR-TEXT
           MOVE RV-RAW-RECORD TO ER-RAW-RECORD
           WRITE TGERRF-REC
           IF FS-TGERRF = "00"
              ADD 1 TO CT-ERR
              MOVE WK-ERROR-TEXT TO WK-NOTICE-TEXT
           ELSE
              DISPLAY "TGERRF 書込失敗 ST=" FS-TGERRF
              SET HARD-ERROR TO TRUE
           END-IF.

       6000-WRITE-ZEN.
           INITIALIZE TGZENF-REC
           MOVE LK-ZE-VALUE-DT TO ZE-VALUE-DT
           MOVE LK-ZE-CENTER-SEQ TO ZE-CENTER-SEQ
           MOVE "02" TO ZE-ZEN-TYPE
           MOVE LK-ZE-COUNTER-BANK TO ZE-COUNTER-BANK
           MOVE LK-ZE-COUNTER-BRANCH TO ZE-COUNTER-BRANCH
           MOVE LK-ZE-RECV-ACCT-NO TO ZE-RECV-ACCT-NO
           MOVE LK-ZE-REMIT-AMT TO ZE-REMIT-AMT
           MOVE LK-ZE-REMIT-NAME-KANA TO ZE-REMIT-NAME-KANA
           WRITE TGZENF-REC
           IF FS-TGZENF = "00"
              ADD 1 TO CT-ZEN
           ELSE
              DISPLAY "TGZENF 書込失敗 ST=" FS-TGZENF
              SET HARD-ERROR TO TRUE
           END-IF.

       7000-WRITE-ACK.
           IF NO-HARD-ERROR
              INITIALIZE TGACKF-REC
              MOVE LK-ZE-VALUE-DT TO AK-VALUE-DT
              MOVE LK-ZE-CENTER-SEQ TO AK-CENTER-SEQ
              MOVE "02" TO AK-NOTICE-TYPE
              MOVE LK-ZE-COUNTER-BANK TO AK-COUNTER-BANK
              MOVE WK-RESULT-CD TO AK-RESULT-CD
              MOVE 1 TO AK-ITEM-COUNT
              MOVE WK-NOTICE-TEXT TO AK-NOTICE-TEXT
              WRITE TGACKF-REC
              IF FS-TGACKF = "00"
                 ADD 1 TO CT-ACK
              ELSE
                 DISPLAY "TGACKF 書込失敗 ST=" FS-TGACKF
                 SET HARD-ERROR TO TRUE
              END-IF
           END-IF.

       9000-CLOSE.
           CLOSE TGRCVF KZACCTF TGBANKF TGZENF TGERRF TGACKF
           IF FS-TGRCVF NOT = "00"
              DISPLAY "TGRCVF クローズ失敗 ST=" FS-TGRCVF
              SET HARD-ERROR TO TRUE
           END-IF
           IF FS-KZACCTF NOT = "00"
              DISPLAY "KZACCTF クローズ失敗 ST=" FS-KZACCTF
              SET HARD-ERROR TO TRUE
           END-IF
           IF FS-TGBANKF NOT = "00"
              DISPLAY "TGBANKF クローズ失敗 ST=" FS-TGBANKF
              SET HARD-ERROR TO TRUE
           END-IF
           IF FS-TGZENF NOT = "00"
              DISPLAY "TGZENF クローズ失敗 ST=" FS-TGZENF
              SET HARD-ERROR TO TRUE
           END-IF
           IF FS-TGERRF NOT = "00"
              DISPLAY "TGERRF クローズ失敗 ST=" FS-TGERRF
              SET HARD-ERROR TO TRUE
           END-IF
           IF FS-TGACKF NOT = "00"
              DISPLAY "TGACKF クローズ失敗 ST=" FS-TGACKF
              SET HARD-ERROR TO TRUE
           END-IF.
