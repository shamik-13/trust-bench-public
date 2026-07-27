       IDENTIFICATION DIVISION.
       PROGRAM-ID. TG360B.
      *================================================================
      *  変更履歴
      *  版数  年月日      担当                              概要
      *  1.00  平成30.04.01 システム部 為替・対外接続チーム  新規作成
      *  1.10  令和02.10.01 システム部 為替・対外接続チーム  返却再受付対応
      *  1.20  令和05.07.03 システム部 為替・対外接続チーム  運用表示見直し
      *================================================================
       AUTHOR. TRUST-BANK-SYSTEM.
      *
      *  TG360B RETURN QUEUE RE-ENTRY BATCH
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.
      *
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TGRTNQF ASSIGN TO "TGRTNQF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS RT-RETURN-KEY
               FILE STATUS IS FS-TGRTNQF.
      *
           SELECT KZACCTF ASSIGN TO "KZACCTF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS AC-ACCT-NO
               FILE STATUS IS FS-KZACCTF.
      *
           SELECT TGINRMF ASSIGN TO "TGINRMF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS FS-TGINRMF.
      *
           SELECT TGCLJNF ASSIGN TO "TGCLJNF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS FS-TGCLJNF.
      *
       DATA DIVISION.
       FILE SECTION.
      *
       FD  TGRTNQF.
           COPY TGRTNQC.
      *
       FD  KZACCTF.
           COPY KZACCTC2.
      *
       FD  TGINRMF.
           COPY TGINRMFC.
      *
       FD  TGCLJNF.
           COPY TGCLJNC.
      *
       WORKING-STORAGE SECTION.
      *
       01  WS-FILE-STATUS.
           05  FS-TGRTNQF              PIC XX VALUE SPACES.
           05  FS-KZACCTF              PIC XX VALUE SPACES.
           05  FS-TGINRMF              PIC XX VALUE SPACES.
           05  FS-TGCLJNF              PIC XX VALUE SPACES.
      *
       01  WS-SWITCHES.
           05  WS-EOF-SW               PIC X VALUE "N".
               88  RTNQ-EOF                 VALUE "Y".
           05  WS-HARD-ERROR-SW        PIC X VALUE "N".
               88  HARD-ERROR               VALUE "Y".
      *
       01  WS-COUNTERS.
           05  WS-READ-CNT             PIC 9(9) VALUE ZERO.
           05  WS-SKIP-CNT             PIC 9(9) VALUE ZERO.
           05  WS-REIN-CNT             PIC 9(9) VALUE ZERO.
           05  WS-ERR-CNT              PIC 9(9) VALUE ZERO.
           05  WS-KEY-IDX              PIC 9(5) VALUE ZERO.
      *
       01  WS-AMOUNTS.
           05  WS-REIN-AMT             PIC S9(13) VALUE ZERO.
      *
       01  WS-CURRENT.
           05  WS-CUR-DATE             PIC 9(8) VALUE ZERO.
           05  WS-CUR-TIME             PIC 9(8) VALUE ZERO.
           05  WS-TIMESTAMP            PIC 9(14) VALUE ZERO.
           05  WS-CENTER-SEQ           PIC 9(10) VALUE ZERO.
      *
       01  WS-CONSTANTS.
           05  CT-QUEUE-WAIT           PIC X VALUE "1".
           05  CT-ACCT-ACTIVE          PIC X VALUE "1".
           05  CT-REMIT-TYPE           PIC X VALUE "R".
           05  CT-DIRECTION-RETURN     PIC X VALUE "2".
           05  CT-MATCH-OK             PIC X VALUE "0".
           05  CT-SENDER-BANK          PIC X(4) VALUE "9900".
           05  CT-SENDER-BRANCH        PIC X(3) VALUE "001".
           05  CT-SENDER-NAME          PIC X(48)
               VALUE "シンタクギンコウヘンレイセンター".
           05  CT-REMIT-MSG            PIC X(40)
               VALUE "ヘンレイサイウケツケ".
      *
       01  WS-RETURN-KEY-TABLE.
           05  WS-RETURN-KEY-ENT OCCURS 5000 TIMES.
               10  WS-SAVE-RETURN-KEY  PIC X(32).
      *
       01  WS-DISPLAY.
           05  WS-DISP-READ            PIC Z(9).
           05  WS-DISP-SKIP            PIC Z(9).
           05  WS-DISP-REIN            PIC Z(9).
           05  WS-DISP-ERR             PIC Z(9).
           05  WS-DISP-AMT             PIC Z(13).
      *
           COPY LK-KANA-PARM.
      *
       PROCEDURE DIVISION.
      *
       MAIN-SECTION.
           PERFORM 0000-INIT
           IF NOT HARD-ERROR
               PERFORM 1000-MAIN UNTIL RTNQ-EOF OR HARD-ERROR
           END-IF
           IF NOT HARD-ERROR
               PERFORM 8000-WRITE-JOURNAL
           END-IF
           PERFORM 9000-FINAL
           GOBACK.
      *
       0000-INIT.
           MOVE 0 TO RETURN-CODE
           ACCEPT WS-CUR-DATE FROM DATE YYYYMMDD
           ACCEPT WS-CUR-TIME FROM TIME
           MOVE WS-CUR-DATE TO WS-TIMESTAMP(1:8)
           MOVE WS-CUR-TIME(1:6) TO WS-TIMESTAMP(9:6)
      *
           OPEN INPUT TGRTNQF
           IF FS-TGRTNQF NOT = "00"
               DISPLAY "TGRTNQF オープン ST=" FS-TGRTNQF
               MOVE "Y" TO WS-HARD-ERROR-SW
           END-IF
      *
           OPEN INPUT KZACCTF
           IF FS-KZACCTF NOT = "00"
               DISPLAY "KZACCTF オープン ST=" FS-KZACCTF
               MOVE "Y" TO WS-HARD-ERROR-SW
           END-IF
      *
           OPEN OUTPUT TGINRMF
           IF FS-TGINRMF NOT = "00"
               DISPLAY "TGINRMF オープン ST=" FS-TGINRMF
               MOVE "Y" TO WS-HARD-ERROR-SW
           END-IF
      *
           OPEN OUTPUT TGCLJNF
           IF FS-TGCLJNF NOT = "00"
               DISPLAY "TGCLJNF オープン ST=" FS-TGCLJNF
               MOVE "Y" TO WS-HARD-ERROR-SW
           END-IF
      *
           IF HARD-ERROR
               MOVE 12 TO RETURN-CODE
           END-IF.
      *
       1000-MAIN.
           READ TGRTNQF NEXT RECORD
               AT END
                   MOVE "Y" TO WS-EOF-SW
               NOT AT END
                   IF FS-TGRTNQF = "00"
                       ADD 1 TO WS-READ-CNT
                       PERFORM 2000-PROCESS-RETURN
                   ELSE
                       DISPLAY "TGRTNQF 読込 ST=" FS-TGRTNQF
                       MOVE "Y" TO WS-HARD-ERROR-SW
                       MOVE 12 TO RETURN-CODE
                   END-IF
           END-READ.
      *
       2000-PROCESS-RETURN.
           IF RT-QUEUE-STATUS NOT = CT-QUEUE-WAIT
               ADD 1 TO WS-SKIP-CNT
               EXIT PARAGRAPH
           END-IF
      *
           IF RT-RETURN-AMT <= ZERO
               ADD 1 TO WS-ERR-CNT
               DISPLAY "金額不正 KEY=" RT-RETURN-KEY
               EXIT PARAGRAPH
           END-IF
      *
           IF RT-PAYEE-BANK = SPACES
              OR RT-PAYEE-BRANCH = SPACES
              OR RT-PAYEE-ACCT-TYPE = SPACES
              OR RT-PAYEE-ACCT-NO = SPACES
               ADD 1 TO WS-ERR-CNT
               DISPLAY "口座情報不正 KEY=" RT-RETURN-KEY
               EXIT PARAGRAPH
           END-IF
      *
           MOVE RT-PAYEE-ACCT-NO TO AC-ACCT-NO
           READ KZACCTF KEY IS AC-ACCT-NO
               INVALID KEY
                   ADD 1 TO WS-ERR-CNT
                   DISPLAY "口座なし KEY=" RT-RETURN-KEY
               NOT INVALID KEY
                   IF FS-KZACCTF = "00"
                       PERFORM 2100-CHECK-ACCOUNT
                   ELSE
                       DISPLAY "KZACCTF 読込 ST=" FS-KZACCTF
                       MOVE "Y" TO WS-HARD-ERROR-SW
                       MOVE 12 TO RETURN-CODE
                   END-IF
           END-READ.
      *
       2100-CHECK-ACCOUNT.
           IF AC-BRANCH NOT = RT-PAYEE-BRANCH
               ADD 1 TO WS-ERR-CNT
               DISPLAY "支店不一致 KEY=" RT-RETURN-KEY
               EXIT PARAGRAPH
           END-IF
      *
           IF AC-ACCT-TYPE NOT = RT-PAYEE-ACCT-TYPE
               ADD 1 TO WS-ERR-CNT
               DISPLAY "種別不一致 KEY=" RT-RETURN-KEY
               EXIT PARAGRAPH
           END-IF
      *
           IF AC-STATUS NOT = CT-ACCT-ACTIVE
               ADD 1 TO WS-ERR-CNT
               DISPLAY "状態不一致 KEY=" RT-RETURN-KEY
               EXIT PARAGRAPH
           END-IF
      *
           PERFORM 2200-NORMALIZE-KANA
           IF LK-KANA-RET NOT = ZERO
               ADD 1 TO WS-ERR-CNT
               DISPLAY "カナ不正 KEY=" RT-RETURN-KEY
               EXIT PARAGRAPH
           END-IF
      *
           PERFORM 2300-WRITE-INRM.
      *
       2200-NORMALIZE-KANA.
           INITIALIZE LK-KANA-PARM
           MOVE RT-PAYEE-NAME-KANA TO LK-RAW-KANA
           CALL "TG912S" USING LK-KANA-PARM
               ON EXCEPTION
                   MOVE 9 TO LK-KANA-RET
                   DISPLAY "TG912S 呼出 KEY=" RT-RETURN-KEY
           END-CALL.
      *
       2300-WRITE-INRM.
           IF WS-KEY-IDX >= 5000
               DISPLAY "KEY テーブル満杯 KEY=" RT-RETURN-KEY
               MOVE "Y" TO WS-HARD-ERROR-SW
               MOVE 12 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF
      *
           ADD 1 TO WS-CENTER-SEQ
           INITIALIZE TGINRMF-REC
           MOVE WS-CUR-DATE         TO IR-REMIT-DT
           MOVE WS-CENTER-SEQ       TO IR-CENTER-SEQ
           MOVE CT-REMIT-TYPE       TO IR-REMIT-TYPE
           MOVE CT-SENDER-BANK      TO IR-SENDER-BANK
           MOVE CT-SENDER-BRANCH    TO IR-SENDER-BRANCH
           MOVE CT-SENDER-NAME      TO IR-SENDER-NAME-KANA
           MOVE RT-PAYEE-BANK       TO IR-PAYEE-BANK
           MOVE RT-PAYEE-BRANCH     TO IR-PAYEE-BRANCH
           MOVE RT-PAYEE-ACCT-TYPE  TO IR-PAYEE-ACCT-TYPE
           MOVE RT-PAYEE-ACCT-NO    TO IR-PAYEE-ACCT-NO
           MOVE LK-NORM-KANA        TO IR-PAYEE-NAME-KANA
           MOVE RT-RETURN-AMT       TO IR-REMIT-AMT
           MOVE CT-REMIT-MSG        TO IR-REMIT-MSG
      *
           WRITE TGINRMF-REC
           IF FS-TGINRMF NOT = "00"
               DISPLAY "TGINRMF 書込 ST=" FS-TGINRMF
               MOVE "Y" TO WS-HARD-ERROR-SW
               MOVE 12 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF
      *
           ADD 1 TO WS-REIN-CNT
           ADD RT-RETURN-AMT TO WS-REIN-AMT
           ADD 1 TO WS-KEY-IDX
           MOVE RT-RETURN-KEY TO WS-SAVE-RETURN-KEY(WS-KEY-IDX).
      *
       8000-WRITE-JOURNAL.
           INITIALIZE TGCLJNF-REC
           MOVE WS-CUR-DATE         TO CJ-CLEARING-DT
           MOVE WS-CENTER-SEQ       TO CJ-CENTER-SEQ
           MOVE CT-DIRECTION-RETURN TO CJ-DIRECTION
           MOVE CT-SENDER-BANK      TO CJ-BANK-CD
           MOVE CT-SENDER-BRANCH    TO CJ-BRANCH-CD
           MOVE WS-REIN-CNT         TO CJ-ITEM-COUNT
           MOVE WS-REIN-AMT         TO CJ-TOTAL-AMT
           MOVE CT-MATCH-OK         TO CJ-MATCH-STATUS
           MOVE WS-TIMESTAMP        TO CJ-JOURNAL-TS
      *
           WRITE TGCLJNF-REC
           IF FS-TGCLJNF NOT = "00"
               DISPLAY "TGCLJNF 書込 ST=" FS-TGCLJNF
               MOVE "Y" TO WS-HARD-ERROR-SW
               MOVE 12 TO RETURN-CODE
           END-IF.
      *
       9000-FINAL.
           CLOSE TGRTNQF
           IF FS-TGRTNQF NOT = "00"
              AND FS-TGRTNQF NOT = "42"
               DISPLAY "TGRTNQF クローズ ST=" FS-TGRTNQF
               MOVE "Y" TO WS-HARD-ERROR-SW
           END-IF
      *
           CLOSE KZACCTF
           IF FS-KZACCTF NOT = "00"
              AND FS-KZACCTF NOT = "42"
               DISPLAY "KZACCTF クローズ ST=" FS-KZACCTF
               MOVE "Y" TO WS-HARD-ERROR-SW
           END-IF
      *
           CLOSE TGINRMF
           IF FS-TGINRMF NOT = "00"
              AND FS-TGINRMF NOT = "42"
               DISPLAY "TGINRMF クローズ ST=" FS-TGINRMF
               MOVE "Y" TO WS-HARD-ERROR-SW
           END-IF
      *
           CLOSE TGCLJNF
           IF FS-TGCLJNF NOT = "00"
              AND FS-TGCLJNF NOT = "42"
               DISPLAY "TGCLJNF クローズ ST=" FS-TGCLJNF
               MOVE "Y" TO WS-HARD-ERROR-SW
           END-IF
      *
           MOVE WS-READ-CNT TO WS-DISP-READ
           MOVE WS-SKIP-CNT TO WS-DISP-SKIP
           MOVE WS-REIN-CNT TO WS-DISP-REIN
           MOVE WS-ERR-CNT  TO WS-DISP-ERR
           MOVE WS-REIN-AMT TO WS-DISP-AMT
      *
           DISPLAY "TG360B 読込=" WS-DISP-READ
           DISPLAY "TG360B スキップ=" WS-DISP-SKIP
           DISPLAY "TG360B 再受付=" WS-DISP-REIN
           DISPLAY "TG360B エラー=" WS-DISP-ERR
           DISPLAY "TG360B 金額=" WS-DISP-AMT
      *
           IF HARD-ERROR
               MOVE 12 TO RETURN-CODE
               DISPLAY "TG360B 異常終了"
           ELSE
               MOVE 0 TO RETURN-CODE
               DISPLAY "TG360B 正常終了"
           END-IF.
