       IDENTIFICATION DIVISION.
       PROGRAM-ID. KZ604B.
      ******************************************************************
      * 変更履歴
      * 版数  年月日    担当  概要
      * 01.00 20240401  KZ01  初版作成
      * 01.01 20240920  KZ02  税率マスタ有効性確認を追加
      * 01.02 20250115  KZ03  支払日別集計差額出力を追加
      ******************************************************************
      * 復興特別所得税照合バッチ
      ******************************************************************

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZTXRF
               ASSIGN TO "KZTXRF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-KZTXRF.

           SELECT KZPMRF
               ASSIGN TO "KZPMRF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-KZPMRF.

           SELECT KZRTMF
               ASSIGN TO "KZRTMF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS RT-RATE-KEY
               FILE STATUS IS FS-KZRTMF.

           SELECT KZADLF
               ASSIGN TO "KZADLF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS AL-AUDIT-ID
               FILE STATUS IS FS-KZADLF.

       DATA DIVISION.
       FILE SECTION.

       FD  KZTXRF.
       COPY KZTXRFC.

       FD  KZPMRF.
       COPY KZPMRFC.

       FD  KZRTMF.
       COPY KZRTMFC.

       FD  KZADLF.
       COPY KZADLFC.

       WORKING-STORAGE SECTION.
       01  FS-KZTXRF                       PIC X(02) VALUE SPACE.
       01  FS-KZPMRF                       PIC X(02) VALUE SPACE.
       01  FS-KZRTMF                       PIC X(02) VALUE SPACE.
       01  FS-KZADLF                       PIC X(02) VALUE SPACE.

       01  WS-END-FLAGS.
           05  WS-TX-EOF                   PIC X VALUE "N".
           05  WS-PM-EOF                   PIC X VALUE "N".

       01  WS-CONTROL.
           05  WS-RUN-DATE                 PIC 9(08) VALUE ZERO.
           05  WS-TODAY                    PIC 9(08) VALUE ZERO.
           05  WS-ABEND-FLAG              PIC X VALUE "N".
           05  WS-AUDIT-SEQ               PIC 9(06) VALUE ZERO.
           05  WS-SOURCE-DSID             PIC X(08) VALUE "KZ604B  ".
           05  WS-MAX-ACCT                PIC 9(05) VALUE 5000.
           05  WS-MAX-AGGR                PIC 9(04) VALUE 1000.

       01  WS-COUNTERS.
           05  WS-TX-CNT                  PIC 9(07) VALUE ZERO.
           05  WS-PM-CNT                  PIC 9(07) VALUE ZERO.
           05  WS-ERR-CNT                 PIC 9(07) VALUE ZERO.
           05  WS-WRN-CNT                 PIC 9(07) VALUE ZERO.
           05  WS-ACCT-CNT                PIC 9(05) VALUE ZERO.
           05  WS-AGGR-CNT                PIC 9(04) VALUE ZERO.

       01  WS-WORK.
           05  WS-IDX                     PIC 9(05) VALUE ZERO.
           05  WS-IDY                     PIC 9(05) VALUE ZERO.
           05  WS-FOUND                   PIC X VALUE "N".
           05  WS-RATE-KEY                PIC X(10) VALUE SPACE.
           05  WS-DIFF-AMT                PIC S9(13) VALUE ZERO.
           05  WS-ABS-DIFF                PIC 9(13) VALUE ZERO.
           05  WS-STATUS-CD               PIC X(02) VALUE SPACE.
           05  WS-DATE-CHAR               PIC X(08) VALUE SPACE.
           05  WS-SEQ-CHAR                PIC X(06) VALUE SPACE.

       01  WS-ACCT-TABLE.
           05  WS-ACCT-ENT OCCURS 5000 TIMES.
               10  WS-ACCT-NO             PIC X(12).
               10  WS-ACCT-TYPE           PIC X(02).
               10  WS-TX-NAT-AMT          PIC 9(13).
               10  WS-TX-USED-FLG         PIC X.

       01  WS-AGGR-TABLE.
           05  WS-AGGR-ENT OCCURS 1000 TIMES.
               10  WS-AGGR-RATE-KEY       PIC X(10).
               10  WS-AGGR-PAY-DATE       PIC 9(08).
               10  WS-AGGR-DEBIT          PIC 9(13).
               10  WS-AGGR-CREDIT         PIC 9(13).
               10  WS-AGGR-ERR-FLG        PIC X.

       PROCEDURE DIVISION.
       MAIN-RTN.
           MOVE 0 TO RETURN-CODE
           PERFORM 0100-INIT
           IF WS-ABEND-FLAG = "N"
               PERFORM 1000-LOAD-TX
           END-IF
           IF WS-ABEND-FLAG = "N"
               PERFORM 2000-PROCESS-PM
           END-IF
           IF WS-ABEND-FLAG = "N"
               PERFORM 3000-WRITE-AUDIT
           END-IF
           PERFORM 9000-END
           GOBACK.

       0100-INIT.
           ACCEPT WS-TODAY FROM DATE YYYYMMDD
           MOVE WS-TODAY TO WS-RUN-DATE

           OPEN INPUT KZTXRF
           IF FS-KZTXRF NOT = "00"
               DISPLAY "KZTXRF オープン失敗 ST=" FS-KZTXRF
               MOVE "Y" TO WS-ABEND-FLAG
               MOVE 12 TO RETURN-CODE
           END-IF

           IF WS-ABEND-FLAG = "N"
               OPEN INPUT KZPMRF
               IF FS-KZPMRF NOT = "00"
                   DISPLAY "KZPMRF オープン失敗 ST=" FS-KZPMRF
                   MOVE "Y" TO WS-ABEND-FLAG
                   MOVE 12 TO RETURN-CODE
               END-IF
           END-IF

           IF WS-ABEND-FLAG = "N"
               OPEN INPUT KZRTMF
               IF FS-KZRTMF NOT = "00"
                   DISPLAY "KZRTMF オープン失敗 ST=" FS-KZRTMF
                   MOVE "Y" TO WS-ABEND-FLAG
                   MOVE 12 TO RETURN-CODE
               END-IF
           END-IF

           IF WS-ABEND-FLAG = "N"
               OPEN OUTPUT KZADLF
               IF FS-KZADLF NOT = "00"
                   DISPLAY "KZADLF オープン失敗 ST=" FS-KZADLF
                   MOVE "Y" TO WS-ABEND-FLAG
                   MOVE 12 TO RETURN-CODE
               END-IF
           END-IF.

       1000-LOAD-TX.
           PERFORM UNTIL WS-TX-EOF = "Y"
               READ KZTXRF
                   AT END
                       MOVE "Y" TO WS-TX-EOF
                   NOT AT END
                       PERFORM 1100-STORE-TX
               END-READ
               IF FS-KZTXRF NOT = "00" AND FS-KZTXRF NOT = "10"
                   DISPLAY "KZTXRF 読込失敗 ST=" FS-KZTXRF
                   MOVE "Y" TO WS-ABEND-FLAG
                   MOVE 12 TO RETURN-CODE
                   MOVE "Y" TO WS-TX-EOF
               END-IF
           END-PERFORM.

       1100-STORE-TX.
           ADD 1 TO WS-TX-CNT

           IF TR-ACCT-TYPE NOT = "01"
              AND TR-ACCT-TYPE NOT = "02"
              AND TR-ACCT-TYPE NOT = "03"
               DISPLAY "税額明細 口座区分不正"
               DISPLAY "口座=" TR-ACCT-NO
               ADD 1 TO WS-ERR-CNT
           ELSE
               IF WS-ACCT-CNT >= WS-MAX-ACCT
                   DISPLAY "税額明細 件数上限超過"
                   MOVE "Y" TO WS-ABEND-FLAG
                   MOVE 12 TO RETURN-CODE
                   MOVE "Y" TO WS-TX-EOF
               ELSE
                   ADD 1 TO WS-ACCT-CNT
                   MOVE TR-ACCT-NO TO WS-ACCT-NO(WS-ACCT-CNT)
                   MOVE TR-ACCT-TYPE TO WS-ACCT-TYPE(WS-ACCT-CNT)
                   MOVE TR-NATIONAL-TAX-AMT
                       TO WS-TX-NAT-AMT(WS-ACCT-CNT)
                   MOVE "N" TO WS-TX-USED-FLG(WS-ACCT-CNT)
               END-IF
           END-IF.

       2000-PROCESS-PM.
           PERFORM UNTIL WS-PM-EOF = "Y"
               READ KZPMRF
                   AT END
                       MOVE "Y" TO WS-PM-EOF
                   NOT AT END
                       PERFORM 2100-CHECK-PM
               END-READ
               IF FS-KZPMRF NOT = "00" AND FS-KZPMRF NOT = "10"
                   DISPLAY "KZPMRF 読込失敗 ST=" FS-KZPMRF
                   MOVE "Y" TO WS-ABEND-FLAG
                   MOVE 12 TO RETURN-CODE
                   MOVE "Y" TO WS-PM-EOF
               END-IF
           END-PERFORM.

       2100-CHECK-PM.
           ADD 1 TO WS-PM-CNT

           IF PM-ACCT-TYPE NOT = "01"
              AND PM-ACCT-TYPE NOT = "02"
              AND PM-ACCT-TYPE NOT = "03"
               DISPLAY "国税納付 口座区分不正"
               DISPLAY "参照=" PM-PAYMENT-REF-NO
               ADD 1 TO WS-ERR-CNT
               EXIT PARAGRAPH
           END-IF

           IF PM-PAYMENT-DATE = ZERO
               DISPLAY "国税納付 支払日不正"
               DISPLAY "参照=" PM-PAYMENT-REF-NO
               ADD 1 TO WS-ERR-CNT
               EXIT PARAGRAPH
           END-IF

           PERFORM 2200-FIND-ACCT

           IF WS-FOUND NOT = "Y"
               DISPLAY "税額明細未検出"
               DISPLAY "参照=" PM-PAYMENT-REF-NO
               ADD 1 TO WS-ERR-CNT
               EXIT PARAGRAPH
           END-IF

           IF PM-ACCT-TYPE NOT = WS-ACCT-TYPE(WS-IDX)
               DISPLAY "口座区分不一致"
               DISPLAY "参照=" PM-PAYMENT-REF-NO
               ADD 1 TO WS-ERR-CNT
               EXIT PARAGRAPH
           END-IF

           PERFORM 2300-MAKE-RATE-KEY
           PERFORM 2400-CHECK-RATE-MASTER
           IF WS-ABEND-FLAG = "N"
               PERFORM 2500-ADD-AGGR
           END-IF.

       2200-FIND-ACCT.
           MOVE "N" TO WS-FOUND
           MOVE 1 TO WS-IDX
           PERFORM UNTIL WS-IDX > WS-ACCT-CNT OR WS-FOUND = "Y"
               IF WS-ACCT-NO(WS-IDX) = PM-ACCT-NO
                   MOVE "Y" TO WS-FOUND
                   MOVE "Y" TO WS-TX-USED-FLG(WS-IDX)
               ELSE
                   ADD 1 TO WS-IDX
               END-IF
           END-PERFORM.

       2300-MAKE-RATE-KEY.
           MOVE SPACES TO WS-RATE-KEY
           MOVE PM-ACCT-TYPE TO WS-RATE-KEY(1:2)
           MOVE PM-PAYMENT-DATE TO WS-DATE-CHAR
           MOVE WS-DATE-CHAR TO WS-RATE-KEY(3:8).

       2400-CHECK-RATE-MASTER.
           MOVE WS-RATE-KEY TO RT-RATE-KEY
           READ KZRTMF KEY IS RT-RATE-KEY
               INVALID KEY
                   DISPLAY "税率マスタ未検出 KEY=" WS-RATE-KEY
                   ADD 1 TO WS-ERR-CNT
               NOT INVALID KEY
                   IF RT-EFFECTIVE-DATE > PM-PAYMENT-DATE
                       DISPLAY "税率マスタ適用日不正 KEY="
                           WS-RATE-KEY
                       ADD 1 TO WS-ERR-CNT
                   END-IF
                   IF RT-TAX-KIND NOT = "SP"
                       DISPLAY "税率マスタ税種不正 KEY="
                           WS-RATE-KEY
                       ADD 1 TO WS-ERR-CNT
                   END-IF
           END-READ

           IF FS-KZRTMF NOT = "00" AND FS-KZRTMF NOT = "23"
               DISPLAY "KZRTMF 読込失敗 ST=" FS-KZRTMF
               MOVE "Y" TO WS-ABEND-FLAG
               MOVE 12 TO RETURN-CODE
               MOVE "Y" TO WS-PM-EOF
           END-IF.

       2500-ADD-AGGR.
           MOVE "N" TO WS-FOUND
           MOVE 1 TO WS-IDY
           PERFORM UNTIL WS-IDY > WS-AGGR-CNT OR WS-FOUND = "Y"
               IF WS-AGGR-RATE-KEY(WS-IDY) = WS-RATE-KEY
                  AND WS-AGGR-PAY-DATE(WS-IDY) = PM-PAYMENT-DATE
                   MOVE "Y" TO WS-FOUND
               ELSE
                   ADD 1 TO WS-IDY
               END-IF
           END-PERFORM

           IF WS-FOUND NOT = "Y"
               IF WS-AGGR-CNT >= WS-MAX-AGGR
                   DISPLAY "照合集計 件数上限超過"
                   MOVE "Y" TO WS-ABEND-FLAG
                   MOVE 12 TO RETURN-CODE
                   MOVE "Y" TO WS-PM-EOF
                   EXIT PARAGRAPH
               END-IF
               ADD 1 TO WS-AGGR-CNT
               MOVE WS-AGGR-CNT TO WS-IDY
               MOVE WS-RATE-KEY TO WS-AGGR-RATE-KEY(WS-IDY)
               MOVE PM-PAYMENT-DATE TO WS-AGGR-PAY-DATE(WS-IDY)
               MOVE ZERO TO WS-AGGR-DEBIT(WS-IDY)
               MOVE ZERO TO WS-AGGR-CREDIT(WS-IDY)
               MOVE "N" TO WS-AGGR-ERR-FLG(WS-IDY)
           END-IF

           ADD PM-SPECIAL-RECON-AMT TO WS-AGGR-DEBIT(WS-IDY)
           ADD WS-TX-NAT-AMT(WS-IDX) TO WS-AGGR-CREDIT(WS-IDY).

       3000-WRITE-AUDIT.
           MOVE 1 TO WS-IDY
           PERFORM UNTIL WS-IDY > WS-AGGR-CNT
               COMPUTE WS-DIFF-AMT =
                   WS-AGGR-DEBIT(WS-IDY) - WS-AGGR-CREDIT(WS-IDY)

               IF WS-DIFF-AMT < ZERO
                   COMPUTE WS-ABS-DIFF = WS-DIFF-AMT * -1
               ELSE
                   MOVE WS-DIFF-AMT TO WS-ABS-DIFF
               END-IF

               IF WS-ABS-DIFF = ZERO
                   MOVE "00" TO WS-STATUS-CD
               ELSE
                   MOVE "10" TO WS-STATUS-CD
                   DISPLAY "集計差額あり KEY="
                       WS-AGGR-RATE-KEY(WS-IDY)
                   DISPLAY "支払日=" WS-AGGR-PAY-DATE(WS-IDY)
                   ADD 1 TO WS-WRN-CNT
               END-IF

               PERFORM 3100-WRITE-ONE-AUDIT
               ADD 1 TO WS-IDY
           END-PERFORM.

       3100-WRITE-ONE-AUDIT.
           ADD 1 TO WS-AUDIT-SEQ
           MOVE WS-AUDIT-SEQ TO WS-SEQ-CHAR
           MOVE SPACES TO KZADLF-REC
           STRING "KZ604B" WS-SEQ-CHAR
               DELIMITED BY SIZE
               INTO AL-AUDIT-ID
           END-STRING
           MOVE WS-RUN-DATE TO AL-RUN-DATE
           MOVE WS-SOURCE-DSID TO AL-SOURCE-DSID
           MOVE WS-AGGR-DEBIT(WS-IDY) TO AL-DEBIT-AMT
           MOVE WS-AGGR-CREDIT(WS-IDY) TO AL-CREDIT-AMT
           MOVE WS-ABS-DIFF TO AL-DIFF-AMT
           MOVE WS-STATUS-CD TO AL-STATUS-CD

           WRITE KZADLF-REC
               INVALID KEY
                   DISPLAY "KZADLF 書込キー不正 ID="
                       AL-AUDIT-ID
                   MOVE "Y" TO WS-ABEND-FLAG
                   MOVE 12 TO RETURN-CODE
           END-WRITE

           IF FS-KZADLF NOT = "00"
               DISPLAY "KZADLF 書込失敗 ST=" FS-KZADLF
               MOVE "Y" TO WS-ABEND-FLAG
               MOVE 12 TO RETURN-CODE
           END-IF.

       9000-END.
           IF FS-KZTXRF = "00" OR FS-KZTXRF = "10"
               CLOSE KZTXRF
               IF FS-KZTXRF NOT = "00"
                   DISPLAY "KZTXRF クローズ失敗 ST=" FS-KZTXRF
                   MOVE 12 TO RETURN-CODE
               END-IF
           END-IF

           IF FS-KZPMRF = "00" OR FS-KZPMRF = "10"
               CLOSE KZPMRF
               IF FS-KZPMRF NOT = "00"
                   DISPLAY "KZPMRF クローズ失敗 ST=" FS-KZPMRF
                   MOVE 12 TO RETURN-CODE
               END-IF
           END-IF

           IF FS-KZRTMF = "00" OR FS-KZRTMF = "23"
               CLOSE KZRTMF
               IF FS-KZRTMF NOT = "00"
                   DISPLAY "KZRTMF クローズ失敗 ST=" FS-KZRTMF
                   MOVE 12 TO RETURN-CODE
               END-IF
           END-IF

           IF FS-KZADLF = "00" OR FS-KZADLF = "22"
               CLOSE KZADLF
               IF FS-KZADLF NOT = "00"
                   DISPLAY "KZADLF クローズ失敗 ST=" FS-KZADLF
                   MOVE 12 TO RETURN-CODE
               END-IF
           END-IF

           DISPLAY "KZ604B 終了 明細=" WS-TX-CNT
           DISPLAY "納付=" WS-PM-CNT
           DISPLAY "異常=" WS-ERR-CNT
           DISPLAY "警告=" WS-WRN-CNT

           IF RETURN-CODE = 0 AND WS-ERR-CNT > ZERO
               MOVE 8 TO RETURN-CODE
           END-IF.
