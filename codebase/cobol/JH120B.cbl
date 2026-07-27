       IDENTIFICATION DIVISION.
       PROGRAM-ID. JH120B.
       AUTHOR. JH-BATCH.
      *---------------------------------------------------------------*
      * 情報系抽出起動制御バッチ                                      *
      * JHCTLKFを基準にJH日次抽出の起動可否を判定し、                 *
      * JH121Sの業務戻りコードで実行を制御する。                      *
      *---------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT JHCTLKF ASSIGN TO "JHCTLKF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CT-JOB-ID
               FILE STATUS IS FS-JHCTLKF.

           SELECT JHAUDTF ASSIGN TO "JHAUDTF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS FS-JHAUDTF.

       DATA DIVISION.
       FILE SECTION.
       FD  JHCTLKF.
       COPY JHCTLC.

       FD  JHAUDTF.
       COPY JHAUDC.

       WORKING-STORAGE SECTION.
       01  FS-JHCTLKF                 PIC X(02) VALUE SPACE.
       01  FS-JHAUDTF                 PIC X(02) VALUE SPACE.

       01  WK-FLAGS.
           05  WK-END-SW              PIC X VALUE SPACE.
               88  WK-END                 VALUE '1'.
           05  WK-CTL-FOUND-SW        PIC X VALUE SPACE.
               88  WK-CTL-FOUND           VALUE '1'.
           05  WK-HARD-ERROR-SW       PIC X VALUE SPACE.
               88  WK-HARD-ERROR          VALUE '1'.
           05  WK-SKIP-SW             PIC X VALUE SPACE.
               88  WK-SKIP-RUN            VALUE '1'.

       01  WK-CONSTANTS.
           05  WK-JOB-ID              PIC X(08) VALUE 'JH410B  '.
           05  WK-REQ-NORMAL          PIC X(01) VALUE 'N'.
           05  WK-REQ-RESTART         PIC X(01) VALUE 'R'.
           05  WK-STS-IDLE            PIC X(01) VALUE '0'.
           05  WK-STS-RUNNING         PIC X(01) VALUE '1'.
           05  WK-STS-END             PIC X(01) VALUE '2'.
           05  WK-STS-ERROR           PIC X(01) VALUE '9'.

       01  WK-TIME-AREA.
           05  WK-DATE-YYYYMMDD       PIC X(08) VALUE SPACE.
           05  WK-TIME-HHMMSSCC       PIC X(08) VALUE SPACE.
           05  WK-TIMESTAMP           PIC X(14) VALUE SPACE.

       01  WK-WORK-AREA.
           05  WK-RUN-SEQ             PIC 9(12) VALUE ZERO.

       01  WK-AUDIT-AREA.
           05  WK-AUDIT-SEQ           PIC 9(12) VALUE ZERO.
           05  WK-ZERO-AMT            PIC S9(15)V99 COMP-3 VALUE ZERO.

       01  LK-JH121S-PARM.
           05  LK-JOB-ID              PIC X(08).
           05  LK-REQUEST-CD          PIC X(01).
           05  LK-BUSINESS-DT         PIC 9(08).
           05  LK-INPUT-CNT           PIC 9(12).
           05  LK-OUTPUT-CNT          PIC 9(12).
           05  LK-BUSINESS-RC         PIC S9(04) COMP.
           05  LK-EXIST-STATUS-CD     PIC X(01).
           05  LK-RESTART-POS         PIC 9(12).

      *---------------------------------------------------------------*
      * 開始時点判断資料                                              *
      * JHCTLKFは前回夜間の状態を保持している想定であり、             *
      * RUNNING残存、ERROR終了、END終了のいずれも実在し得る。         *
      * このPGMでは固定の理想値に補正せず、JH121Sの判定結果を         *
      * 正として起動可否を決める。KZFEEHFおよび確認フラグは参照しない。*
      *---------------------------------------------------------------*
       PROCEDURE DIVISION.
       MAIN-RTN.
           PERFORM 1000-INIT
           IF NOT WK-HARD-ERROR
              PERFORM 2000-CALL-JH121S
           END-IF
           IF NOT WK-HARD-ERROR
              AND NOT WK-SKIP-RUN
              PERFORM 3000-START-CONTROL
           END-IF
           IF NOT WK-HARD-ERROR
              AND NOT WK-SKIP-RUN
              PERFORM 4000-CALL-JH410B
           END-IF
           IF NOT WK-HARD-ERROR
              PERFORM 5000-FINAL-AUDIT
           END-IF
           PERFORM 9000-FINAL
           GOBACK.

       1000-INIT.
           MOVE 0 TO RETURN-CODE
           ACCEPT WK-DATE-YYYYMMDD FROM DATE YYYYMMDD
           ACCEPT WK-TIME-HHMMSSCC FROM TIME
           PERFORM 1100-MAKE-TIMESTAMP
           MOVE FUNCTION NUMVAL(WK-DATE-YYYYMMDD) TO LK-BUSINESS-DT
           MOVE WK-JOB-ID TO LK-JOB-ID
           MOVE WK-REQ-NORMAL TO LK-REQUEST-CD
           MOVE ZERO TO LK-INPUT-CNT
           MOVE ZERO TO LK-OUTPUT-CNT
           MOVE ZERO TO LK-BUSINESS-RC
           MOVE SPACE TO LK-EXIST-STATUS-CD
           MOVE ZERO TO LK-RESTART-POS
           OPEN I-O JHCTLKF
           IF FS-JHCTLKF NOT = '00'
              DISPLAY 'JHCTLKF オープン失敗 ST=' FS-JHCTLKF
              MOVE '1' TO WK-HARD-ERROR-SW
              MOVE 12 TO RETURN-CODE
           END-IF
           IF NOT WK-HARD-ERROR
              OPEN EXTEND JHAUDTF
              IF FS-JHAUDTF NOT = '00'
                 DISPLAY 'JHAUDTF オープン失敗 ST=' FS-JHAUDTF
                 MOVE '1' TO WK-HARD-ERROR-SW
                 MOVE 12 TO RETURN-CODE
              END-IF
           END-IF.

       1100-MAKE-TIMESTAMP.
           STRING WK-DATE-YYYYMMDD DELIMITED BY SIZE
                  WK-TIME-HHMMSSCC(1:6) DELIMITED BY SIZE
             INTO WK-TIMESTAMP
           END-STRING.

       2000-CALL-JH121S.
           MOVE WK-JOB-ID TO CT-JOB-ID
           READ JHCTLKF KEY IS CT-JOB-ID
              INVALID KEY
                 MOVE SPACE TO WK-CTL-FOUND-SW
              NOT INVALID KEY
                 MOVE '1' TO WK-CTL-FOUND-SW
           END-READ
           EVALUATE FS-JHCTLKF
              WHEN '00'
                 IF CT-STATUS-CD = WK-STS-ERROR
                    MOVE WK-REQ-RESTART TO LK-REQUEST-CD
                 ELSE
                    MOVE WK-REQ-NORMAL TO LK-REQUEST-CD
                 END-IF
                 IF FUNCTION NUMVAL(CT-BUSINESS-DT) NOT = ZERO
                    MOVE FUNCTION NUMVAL(CT-BUSINESS-DT)
                      TO LK-BUSINESS-DT
                 END-IF
              WHEN '23'
                 MOVE WK-REQ-NORMAL TO LK-REQUEST-CD
              WHEN OTHER
                 DISPLAY 'JHCTLKF 読込失敗 ST=' FS-JHCTLKF
                 MOVE '1' TO WK-HARD-ERROR-SW
                 MOVE 12 TO RETURN-CODE
           END-EVALUATE
           IF NOT WK-HARD-ERROR
              CALL 'JH121S' USING LK-JH121S-PARM
              END-CALL
              IF LK-BUSINESS-RC NOT = ZERO
                 DISPLAY 'JH121S 判定により起動抑止 RC='
                         LK-BUSINESS-RC
                 PERFORM 5100-WRITE-SKIP-AUDIT
                 MOVE '1' TO WK-SKIP-SW
              END-IF
           END-IF
           IF NOT WK-HARD-ERROR
              AND NOT WK-SKIP-RUN
              IF LK-EXIST-STATUS-CD = WK-STS-RUNNING
                 DISPLAY '同一営業日の実行中制御あり'
                 PERFORM 5100-WRITE-SKIP-AUDIT
                 MOVE '1' TO WK-SKIP-SW
              END-IF
           END-IF.

       3000-START-CONTROL.
           IF WK-CTL-FOUND
              MOVE LK-BUSINESS-DT TO CT-BUSINESS-DT
              MOVE FUNCTION NUMVAL(CT-RUN-SEQ) TO WK-RUN-SEQ
              ADD 1 TO WK-RUN-SEQ
              MOVE WK-RUN-SEQ TO CT-RUN-SEQ
              MOVE WK-STS-RUNNING TO CT-STATUS-CD
              MOVE WK-TIMESTAMP TO CT-START-TS
              MOVE SPACE TO CT-END-TS
              MOVE LK-RESTART-POS TO CT-RESTART-POS
              MOVE ZERO TO CT-INPUT-CNT
              MOVE ZERO TO CT-OUTPUT-CNT
              REWRITE JHCTLKF-REC
              END-REWRITE
              IF FS-JHCTLKF NOT = '00'
                 DISPLAY 'JHCTLKF RUNNING更新失敗 ST=' FS-JHCTLKF
                 MOVE '1' TO WK-HARD-ERROR-SW
                 MOVE 12 TO RETURN-CODE
              END-IF
           ELSE
              MOVE WK-JOB-ID TO CT-JOB-ID
              MOVE LK-BUSINESS-DT TO CT-BUSINESS-DT
              MOVE 1 TO CT-RUN-SEQ
              MOVE WK-STS-RUNNING TO CT-STATUS-CD
              MOVE WK-TIMESTAMP TO CT-START-TS
              MOVE SPACE TO CT-END-TS
              MOVE LK-RESTART-POS TO CT-RESTART-POS
              MOVE ZERO TO CT-INPUT-CNT
              MOVE ZERO TO CT-OUTPUT-CNT
              WRITE JHCTLKF-REC
              END-WRITE
              IF FS-JHCTLKF NOT = '00'
                 DISPLAY 'JHCTLKF RUNNING作成失敗 ST=' FS-JHCTLKF
                 MOVE '1' TO WK-HARD-ERROR-SW
                 MOVE 12 TO RETURN-CODE
              END-IF
           END-IF
           IF NOT WK-HARD-ERROR
              PERFORM 5200-WRITE-START-AUDIT
           END-IF.

       4000-CALL-JH410B.
           CALL 'JH410B' USING LK-JH121S-PARM
           END-CALL
           MOVE RETURN-CODE TO LK-BUSINESS-RC
           ACCEPT WK-DATE-YYYYMMDD FROM DATE YYYYMMDD
           ACCEPT WK-TIME-HHMMSSCC FROM TIME
           PERFORM 1100-MAKE-TIMESTAMP
           MOVE WK-JOB-ID TO CT-JOB-ID
           READ JHCTLKF KEY IS CT-JOB-ID
              INVALID KEY
                 DISPLAY 'JHCTLKF 完了更新対象なし'
                 MOVE '1' TO WK-HARD-ERROR-SW
                 MOVE 12 TO RETURN-CODE
              NOT INVALID KEY
                 IF LK-BUSINESS-RC = ZERO
                    MOVE WK-STS-END TO CT-STATUS-CD
                 ELSE
                    MOVE WK-STS-ERROR TO CT-STATUS-CD
                 END-IF
                 MOVE WK-TIMESTAMP TO CT-END-TS
                 MOVE LK-RESTART-POS TO CT-RESTART-POS
                 MOVE LK-INPUT-CNT TO CT-INPUT-CNT
                 MOVE LK-OUTPUT-CNT TO CT-OUTPUT-CNT
                 REWRITE JHCTLKF-REC
                 END-REWRITE
                 IF FS-JHCTLKF NOT = '00'
                    DISPLAY 'JHCTLKF 完了更新失敗 ST=' FS-JHCTLKF
                    MOVE '1' TO WK-HARD-ERROR-SW
                    MOVE 12 TO RETURN-CODE
                 END-IF
           END-READ
           IF NOT WK-HARD-ERROR
              IF LK-BUSINESS-RC NOT = ZERO
                 DISPLAY 'JH410B 異常終了 RC=' LK-BUSINESS-RC
                 MOVE 8 TO RETURN-CODE
              END-IF
           END-IF.

       5000-FINAL-AUDIT.
           IF LK-BUSINESS-RC = ZERO
              MOVE 'END ' TO AUD-EVENT-CD
           ELSE
              MOVE 'ABND' TO AUD-EVENT-CD
           END-IF
           MOVE WK-JOB-ID TO AUD-JOB-ID
           MOVE LK-BUSINESS-DT TO AUD-BUSINESS-DT
           MOVE 'JH410B' TO AUD-DATASET-ID
           MOVE LK-OUTPUT-CNT TO AUD-REC-CNT
           MOVE WK-ZERO-AMT TO AUD-AMT-TOTAL
           MOVE WK-TIMESTAMP TO AUD-EVENT-TS
           PERFORM 5300-WRITE-AUDIT.

       5100-WRITE-SKIP-AUDIT.
           ACCEPT WK-DATE-YYYYMMDD FROM DATE YYYYMMDD
           ACCEPT WK-TIME-HHMMSSCC FROM TIME
           PERFORM 1100-MAKE-TIMESTAMP
           MOVE 'SKIP' TO AUD-EVENT-CD
           MOVE WK-JOB-ID TO AUD-JOB-ID
           MOVE LK-BUSINESS-DT TO AUD-BUSINESS-DT
           MOVE 'JH121S' TO AUD-DATASET-ID
           MOVE ZERO TO AUD-REC-CNT
           MOVE WK-ZERO-AMT TO AUD-AMT-TOTAL
           MOVE WK-TIMESTAMP TO AUD-EVENT-TS
           PERFORM 5300-WRITE-AUDIT.

       5200-WRITE-START-AUDIT.
           MOVE 'STRT' TO AUD-EVENT-CD
           MOVE WK-JOB-ID TO AUD-JOB-ID
           MOVE LK-BUSINESS-DT TO AUD-BUSINESS-DT
           MOVE 'JHCTLK' TO AUD-DATASET-ID
           MOVE LK-RESTART-POS TO AUD-REC-CNT
           MOVE WK-ZERO-AMT TO AUD-AMT-TOTAL
           MOVE WK-TIMESTAMP TO AUD-EVENT-TS
           PERFORM 5300-WRITE-AUDIT.

       5300-WRITE-AUDIT.
           ADD 1 TO WK-AUDIT-SEQ
           MOVE WK-AUDIT-SEQ TO AUD-AUDIT-SEQ
           WRITE JHAUDTF-REC
           END-WRITE
           IF FS-JHAUDTF NOT = '00'
              DISPLAY 'JHAUDTF 書込失敗 ST=' FS-JHAUDTF
              MOVE '1' TO WK-HARD-ERROR-SW
              MOVE 12 TO RETURN-CODE
           END-IF.

       9000-FINAL.
           IF FS-JHCTLKF NOT = SPACE
              CLOSE JHCTLKF
              IF FS-JHCTLKF NOT = '00'
                 DISPLAY 'JHCTLKF クローズ失敗 ST=' FS-JHCTLKF
                 IF RETURN-CODE = ZERO
                    MOVE 8 TO RETURN-CODE
                 END-IF
              END-IF
           END-IF
           IF FS-JHAUDTF NOT = SPACE
              CLOSE JHAUDTF
              IF FS-JHAUDTF NOT = '00'
                 DISPLAY 'JHAUDTF クローズ失敗 ST=' FS-JHAUDTF
                 IF RETURN-CODE = ZERO
                    MOVE 8 TO RETURN-CODE
                 END-IF
              END-IF
           END-IF
           IF WK-SKIP-RUN
              AND RETURN-CODE = ZERO
              MOVE 4 TO RETURN-CODE
           END-IF.
