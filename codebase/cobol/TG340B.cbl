       IDENTIFICATION DIVISION.
       PROGRAM-ID. TG340B.
      *--------------------------------------------------------------
      * 変更履歴
      *  版数 年月日     担当          概要
      *  1.0  令和7年3月 中村 亮介   初版作成(拒否明細返戻再依頼)
      *  1.1  令和7年6月 中村 亮介   返戻区分判定（R/C/H）の精緻化
      *--------------------------------------------------------------
      ******************************************************************
      * REJECT DETAIL RETURN QUEUE REGISTRATION.
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TGREJLF ASSIGN TO "TGREJLF"
             ORGANIZATION IS SEQUENTIAL
             ACCESS MODE IS SEQUENTIAL
             FILE STATUS IS WS-RJ-ST.

           SELECT KZACCTF ASSIGN TO "KZACCTF"
             ORGANIZATION IS INDEXED
             ACCESS MODE IS RANDOM
             RECORD KEY IS AC-ACCT-NO
             FILE STATUS IS WS-AC-ST.

           SELECT TGRTNQF ASSIGN TO "TGRTNQF"
             ORGANIZATION IS INDEXED
             ACCESS MODE IS RANDOM
             RECORD KEY IS RT-RETURN-KEY
             FILE STATUS IS WS-RT-ST.

       DATA DIVISION.
       FILE SECTION.
       FD  TGREJLF.
           COPY TGREJLFC.

       FD  KZACCTF.
           COPY KZACCTC2.

       FD  TGRTNQF.
           COPY TGRTNQC.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05 WS-RJ-ST              PIC XX VALUE SPACE.
           05 WS-AC-ST              PIC XX VALUE SPACE.
           05 WS-RT-ST              PIC XX VALUE SPACE.

       01  WS-SWITCHES.
           05 WS-EOF-SW             PIC X VALUE "N".
              88 RJ-EOF                   VALUE "Y".
           05 WS-ABEND-SW           PIC X VALUE "N".
              88 ABEND-ON                 VALUE "Y".
           05 WS-ACCT-HIT-SW        PIC X VALUE "N".
              88 ACCT-HIT                 VALUE "Y".

       01  WS-COUNTERS.
           05 WS-RJ-READ-CNT        PIC 9(9) VALUE ZERO.
           05 WS-RT-WRITE-CNT       PIC 9(9) VALUE ZERO.
           05 WS-SKIP-CNT           PIC 9(9) VALUE ZERO.
           05 WS-ERR-CNT            PIC 9(9) VALUE ZERO.

       01  WS-WORK.
           05 WS-RETURN-SEQ         PIC 9(6) VALUE ZERO.
           05 WS-RETURN-KEY.
              10 WS-KEY-DT          PIC 9(8).
              10 WS-KEY-SEQ         PIC 9(10).
              10 WS-KEY-SUB         PIC 9(6).
           05 WS-REASON-CODE        PIC X(2) VALUE SPACE.
           05 WS-RETURN-TYPE        PIC X VALUE SPACE.
           05 WS-TODAY              PIC 9(8) VALUE ZERO.
           05 WS-DATE-INT           PIC 9(8) VALUE ZERO.
           05 WS-DATE-OK-SW         PIC X VALUE "N".
              88 DATE-OK                  VALUE "Y".
           05 WS-NAME-MATCH-SW      PIC X VALUE "N".
              88 NAME-MATCH               VALUE "Y".
           05 WS-VALID-REJ-SW       PIC X VALUE "N".
              88 VALID-REJ                VALUE "Y".
           05 WS-BANK-CODE          PIC X(4) VALUE "0000".

       01  WS-STATUS-VALUES.
           05 WS-STATUS-ACTIVE      PIC X VALUE "1".
           05 WS-STATUS-CLOSED      PIC X VALUE "9".

       01  WS-RETURN-TYPES.
           05 WS-RTN-RETURN         PIC X VALUE "R".
           05 WS-RTN-CORRECT        PIC X VALUE "C".
           05 WS-RTN-HOLD           PIC X VALUE "H".

       01  WS-QUEUE-VALUES.
           05 WS-Q-NEW              PIC X VALUE "0".
           05 WS-Q-ERROR            PIC X VALUE "8".

           COPY LK-KANA-PARM.

       PROCEDURE DIVISION.
       MAIN-SECTION.
           PERFORM 1000-INIT
           PERFORM 2000-MAIN UNTIL RJ-EOF OR ABEND-ON
           PERFORM 9000-FINAL
           GOBACK.

       1000-INIT.
           MOVE 0 TO RETURN-CODE
           ACCEPT WS-TODAY FROM DATE YYYYMMDD

           OPEN INPUT TGREJLF
           IF WS-RJ-ST NOT = "00"
              DISPLAY "TGREJLF オープン異常 ST=" WS-RJ-ST
              MOVE 12 TO RETURN-CODE
              SET ABEND-ON TO TRUE
              EXIT PARAGRAPH
           END-IF

           OPEN INPUT KZACCTF
           IF WS-AC-ST NOT = "00"
              DISPLAY "KZACCTF オープン異常 ST=" WS-AC-ST
              MOVE 12 TO RETURN-CODE
              SET ABEND-ON TO TRUE
              EXIT PARAGRAPH
           END-IF

           OPEN I-O TGRTNQF
           IF WS-RT-ST NOT = "00"
              DISPLAY "TGRTNQF オープン異常 ST=" WS-RT-ST
              MOVE 12 TO RETURN-CODE
              SET ABEND-ON TO TRUE
              EXIT PARAGRAPH
           END-IF

           PERFORM 2100-READ-REJECT.

       2000-MAIN.
           ADD 1 TO WS-RJ-READ-CNT
           PERFORM 3000-VALIDATE-REJECT

           IF VALID-REJ
              PERFORM 4000-READ-ACCOUNT
              IF NOT ABEND-ON
                 PERFORM 5000-DECIDE-RETURN
              END-IF
              IF NOT ABEND-ON
                 PERFORM 6000-WRITE-RETURN-Q
              END-IF
           ELSE
              ADD 1 TO WS-SKIP-CNT
           END-IF

           IF NOT ABEND-ON
              PERFORM 2100-READ-REJECT
           END-IF.

       2100-READ-REJECT.
           READ TGREJLF
              AT END
                 SET RJ-EOF TO TRUE
              NOT AT END
                 CONTINUE
           END-READ

           IF WS-RJ-ST NOT = "00" AND WS-RJ-ST NOT = "10"
              DISPLAY "TGREJLF 読込異常 ST=" WS-RJ-ST
              MOVE 12 TO RETURN-CODE
              SET ABEND-ON TO TRUE
           END-IF.

       3000-VALIDATE-REJECT.
           MOVE "N" TO WS-VALID-REJ-SW
           MOVE RJ-REJ-REASON TO WS-REASON-CODE

           IF RJ-REMIT-DT = ZERO
              DISPLAY "拒否日付不正 SEQ=" RJ-CENTER-SEQ
              EXIT PARAGRAPH
           END-IF

           MOVE RJ-REMIT-DT TO WS-DATE-INT
           PERFORM 3100-CHECK-DATE
           IF NOT DATE-OK
              DISPLAY "暦日付不正 SEQ=" RJ-CENTER-SEQ
              EXIT PARAGRAPH
           END-IF

           IF RJ-CENTER-SEQ = ZERO
              DISPLAY "センタSEQ不正 DT=" RJ-REMIT-DT
              EXIT PARAGRAPH
           END-IF

           IF RJ-PAYEE-ACCT-NO = SPACE OR RJ-PAYEE-ACCT-NO = ZERO
              DISPLAY "受取口座不正 SEQ=" RJ-CENTER-SEQ
              EXIT PARAGRAPH
           END-IF

           IF RJ-REJ-AMT <= ZERO
              DISPLAY "拒否金額不正 SEQ=" RJ-CENTER-SEQ
              EXIT PARAGRAPH
           END-IF

           IF WS-REASON-CODE = "01"
              OR WS-REASON-CODE = "02"
              OR WS-REASON-CODE = "03"
              OR WS-REASON-CODE = "04"
              OR WS-REASON-CODE = "09"
              SET VALID-REJ TO TRUE
           ELSE
              DISPLAY "対象外理由コード SEQ=" RJ-CENTER-SEQ
           END-IF.

       3100-CHECK-DATE.
           MOVE "N" TO WS-DATE-OK-SW
           IF WS-DATE-INT >= 20000101
              AND WS-DATE-INT <= WS-TODAY
              SET DATE-OK TO TRUE
           END-IF.

       4000-READ-ACCOUNT.
           MOVE "N" TO WS-ACCT-HIT-SW
           MOVE RJ-PAYEE-ACCT-NO TO AC-ACCT-NO

           READ KZACCTF KEY IS AC-ACCT-NO
              INVALID KEY
                 MOVE "N" TO WS-ACCT-HIT-SW
              NOT INVALID KEY
                 SET ACCT-HIT TO TRUE
           END-READ

           IF WS-AC-ST NOT = "00" AND WS-AC-ST NOT = "23"
              DISPLAY "KZACCTF 読込異常 ST=" WS-AC-ST
              MOVE 12 TO RETURN-CODE
              SET ABEND-ON TO TRUE
           END-IF.

       5000-DECIDE-RETURN.
           MOVE WS-RTN-RETURN TO WS-RETURN-TYPE

           IF NOT ACCT-HIT
              MOVE WS-RTN-RETURN TO WS-RETURN-TYPE
              EXIT PARAGRAPH
           END-IF

           IF AC-STATUS = WS-STATUS-CLOSED
              MOVE WS-RTN-RETURN TO WS-RETURN-TYPE
              EXIT PARAGRAPH
           END-IF

           IF AC-STATUS NOT = WS-STATUS-ACTIVE
              MOVE WS-RTN-HOLD TO WS-RETURN-TYPE
              DISPLAY "口座状態不正 ACCT=" AC-ACCT-NO
              EXIT PARAGRAPH
           END-IF

           PERFORM 5100-CHECK-KANA

           IF NAME-MATCH
              MOVE WS-RTN-HOLD TO WS-RETURN-TYPE
           ELSE
              IF WS-REASON-CODE = "03" OR WS-REASON-CODE = "04"
                 MOVE WS-RTN-CORRECT TO WS-RETURN-TYPE
              ELSE
                 MOVE WS-RTN-RETURN TO WS-RETURN-TYPE
              END-IF
           END-IF.

       5100-CHECK-KANA.
           MOVE "N" TO WS-NAME-MATCH-SW

           INITIALIZE LK-KANA-PARM
           MOVE RJ-PAYEE-NAME-KANA TO LK-RAW-KANA
           CALL "TG912S" USING LK-KANA-PARM
           IF LK-KANA-RET NOT = ZERO
              DISPLAY "カナ正規化異常 入力 SEQ=" RJ-CENTER-SEQ
              EXIT PARAGRAPH
           END-IF
           MOVE LK-NORM-KANA TO RJ-PAYEE-NAME-KANA

           INITIALIZE LK-KANA-PARM
           MOVE AC-ACCT-NAME-KANA TO LK-RAW-KANA
           CALL "TG912S" USING LK-KANA-PARM
           IF LK-KANA-RET NOT = ZERO
              DISPLAY "カナ正規化異常 ACCT=" AC-ACCT-NO
              EXIT PARAGRAPH
           END-IF

           IF RJ-PAYEE-NAME-KANA = LK-NORM-KANA
              SET NAME-MATCH TO TRUE
           END-IF.

       6000-WRITE-RETURN-Q.
           ADD 1 TO WS-RETURN-SEQ
           MOVE RJ-REMIT-DT     TO WS-KEY-DT
           MOVE RJ-CENTER-SEQ   TO WS-KEY-SEQ
           MOVE WS-RETURN-SEQ   TO WS-KEY-SUB

           INITIALIZE TGRTNQF-REC
           MOVE WS-RETURN-KEY   TO RT-RETURN-KEY
           MOVE RJ-REMIT-DT     TO RT-ORIG-REMIT-DT
           MOVE RJ-CENTER-SEQ   TO RT-ORIG-CENTER-SEQ
           MOVE WS-RETURN-TYPE  TO RT-RETURN-TYPE
           MOVE WS-BANK-CODE    TO RT-PAYEE-BANK
           MOVE RJ-PAYEE-ACCT-NO TO RT-PAYEE-ACCT-NO
           MOVE RJ-PAYEE-NAME-KANA TO RT-PAYEE-NAME-KANA
           MOVE RJ-REJ-AMT      TO RT-RETURN-AMT
           MOVE WS-Q-NEW        TO RT-QUEUE-STATUS

           IF ACCT-HIT
              MOVE AC-BRANCH    TO RT-PAYEE-BRANCH
              MOVE AC-ACCT-TYPE TO RT-PAYEE-ACCT-TYPE
           ELSE
              MOVE SPACE        TO RT-PAYEE-BRANCH
              MOVE SPACE        TO RT-PAYEE-ACCT-TYPE
           END-IF

           WRITE TGRTNQF-REC
              INVALID KEY
                 MOVE WS-Q-ERROR TO RT-QUEUE-STATUS
                 REWRITE TGRTNQF-REC
                    INVALID KEY
                       DISPLAY "TGRTNQF 重複キー再書込"
                       MOVE 12 TO RETURN-CODE
                       SET ABEND-ON TO TRUE
                 END-REWRITE
           END-WRITE

           IF NOT ABEND-ON
              IF WS-RT-ST = "00"
                 ADD 1 TO WS-RT-WRITE-CNT
              ELSE
                 DISPLAY "TGRTNQF 書込異常 ST=" WS-RT-ST
                 MOVE 12 TO RETURN-CODE
                 SET ABEND-ON TO TRUE
              END-IF
           END-IF.

       9000-FINAL.
           IF WS-RJ-ST NOT = SPACE
              CLOSE TGREJLF
           END-IF
           IF WS-AC-ST NOT = SPACE
              CLOSE KZACCTF
           END-IF
           IF WS-RT-ST NOT = SPACE
              CLOSE TGRTNQF
           END-IF

           DISPLAY "TG340B 終了 読込=" WS-RJ-READ-CNT
           DISPLAY "TG340B 終了 返戻=" WS-RT-WRITE-CNT
           DISPLAY "TG340B 終了 スキップ=" WS-SKIP-CNT

           IF ABEND-ON
              ADD 1 TO WS-ERR-CNT
              DISPLAY "TG340B 異常終了件数=" WS-ERR-CNT
              IF RETURN-CODE = ZERO
                 MOVE 8 TO RETURN-CODE
              END-IF
           ELSE
              MOVE 0 TO RETURN-CODE
           END-IF.
