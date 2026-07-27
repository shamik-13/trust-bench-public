      * LR240B — 月次保険料集計帳票
      * 版数  年月日      担当         概要
      * V1.0  20200101    保険計理部  初版作成
      * V1.1  20210615    保険計理部  CALC-STATUS-KBN異常分離対応
      *
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LR240B.
       AUTHOR. みらい生命 システム部.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LFPRMF ASSIGN TO WS-LFPRMF-PATH
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-LFPRMF.
           SELECT LPCLMF ASSIGN TO WS-LPCLMF-PATH
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CL-POL-NO
               FILE STATUS IS FS-LPCLMF.
           SELECT LPPAYF ASSIGN TO WS-LPPAYF-PATH
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-LPPAYF.
           SELECT LRRPTF ASSIGN TO WS-LRRPTF-PATH
               ORGANIZATION IS INDEXED
               RECORD KEY IS RP-REPORT-ID
               FILE STATUS IS FS-LRRPTF.
       
       DATA DIVISION.
       FILE SECTION.
       FD  LFPRMF.
       COPY LFPRMFC.
       
       FD  LPCLMF.
       COPY LPCLMFC.
       
       FD  LPPAYF.
       COPY LPPAYFC.
       
       FD  LRRPTF.
       COPY LRRPTFC.
       
       WORKING-STORAGE SECTION.
       01  WS-FILE-PATHS.
           05  WS-LFPRMF-PATH         PIC X(256).
           05  WS-LPCLMF-PATH         PIC X(256).
           05  WS-LPPAYF-PATH         PIC X(256).
           05  WS-LRRPTF-PATH         PIC X(256).
       
       01  WS-FILE-STATUS.
           05  FS-LFPRMF              PIC XX.
           05  FS-LPCLMF              PIC XX.
           05  FS-LPPAYF              PIC XX.
           05  FS-LRRPTF              PIC XX.
       
       01  WS-PROCESSING-FLAGS.
           05  WS-EOF-LFPRMF          PIC X VALUE 'N'.
               88 LFPRMF-END           VALUE 'Y'.
           05  WS-EOF-LPPAYF          PIC X VALUE 'N'.
               88 LPPAYF-END           VALUE 'Y'.
           05  WS-ERROR-FLAG          PIC X VALUE 'N'.
               88 ERROR-OCCURRED       VALUE 'Y'.
       
       01  WS-AGGREGATION-TABLES.
           05  WS-NORMAL-PREM-TOT     PIC S9(13)V99 COMP-3 VALUE 0.
           05  WS-ABNORMAL-PREM-TOT   PIC S9(13)V99 COMP-3 VALUE 0.
           05  WS-BILL-AMT-TOT        PIC S9(13)V99 COMP-3 VALUE 0.
           05  WS-RECEIPT-AMT-TOT     PIC S9(13)V99 COMP-3 VALUE 0.
           05  WS-UNCOLLECTED-TOT     PIC S9(13)V99 COMP-3 VALUE 0.
           05  WS-OVERPAID-TOT        PIC S9(13)V99 COMP-3 VALUE 0.
       
       01  WS-AGGREGATION-BY-CHANNEL.
           05  WS-CHANNEL-TABLE OCCURS 9 TIMES INDEXED BY CH-IDX.
               10  WS-CH-CODE         PIC 9.
               10  WS-CH-PREM-NORMAL  PIC S9(13)V99 COMP-3.
               10  WS-CH-PREM-ABNORM  PIC S9(13)V99 COMP-3.
               10  WS-CH-BILL-AMT     PIC S9(13)V99 COMP-3.
               10  WS-CH-RECEIPT-AMT  PIC S9(13)V99 COMP-3.
               10  WS-CH-DIFF-AMT     PIC S9(13)V99 COMP-3.
       
       01  WS-COUNTERS.
           05  WS-LINE-NUMBER         PIC 9(5) VALUE 1.
           05  WS-REPORT-COUNT        PIC 9(7) VALUE 0.
           05  WS-ERROR-COUNT         PIC 9(5) VALUE 0.
       
       01  WS-WORKING-FIELDS.
           05  WS-CALC-DIFF           PIC S9(13)V99 COMP-3.
           05  WS-POL-STATUS          PIC 99.
           05  WS-CHANNEL-KBN         PIC 9.
           05  WS-REPORT-ID           PIC 9(11).
           05  WS-REPORT-YM           PIC 9(6).
           05  WS-CURRENT-POL         PIC X(10).
           05  WS-PREV-POL            PIC X(10).
           05  WS-PRINT-AMT           PIC S9(13)V99 COMP-3.
       
       01  WS-DATE-WORK.
           05  WS-CURRENT-DATE        PIC 9(8).
           05  WS-REPORT-YEAR         PIC 9(4).
           05  WS-REPORT-MONTH        PIC 9(2).
       
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM INITIALIZE-PROGRAM.
           IF ERROR-OCCURRED
               GO TO MAIN-ABNORMAL-END.
           
           PERFORM PROCESS-PREMIUM-FILE.
           IF ERROR-OCCURRED
               GO TO MAIN-ABNORMAL-END.
           
           PERFORM PROCESS-CLAIM-PAYMENT-MATCHING.
           IF ERROR-OCCURRED
               GO TO MAIN-ABNORMAL-END.
           
           PERFORM OUTPUT-AGGREGATION-REPORT.
           IF ERROR-OCCURRED
               GO TO MAIN-ABNORMAL-END.
           
           PERFORM FINAL-AUDIT.
           IF ERROR-OCCURRED
               GO TO MAIN-ABNORMAL-END.
           
           MOVE 0 TO RETURN-CODE.
           GOBACK.
       
       MAIN-ABNORMAL-END.
           MOVE 8 TO RETURN-CODE.
           GOBACK.
       
       INITIALIZE-PROGRAM.
           ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD.
           MOVE WS-CURRENT-DATE(1:4) TO WS-REPORT-YEAR.
           MOVE WS-CURRENT-DATE(5:2) TO WS-REPORT-MONTH.
           
           STRING WS-REPORT-YEAR DELIMITED BY SIZE
                  WS-REPORT-MONTH DELIMITED BY SIZE
               INTO WS-REPORT-YM.
           
           PERFORM INITIALIZE-AGGREGATION-TABLE.
           
           OPEN INPUT LFPRMF.
           IF FS-LFPRMF NOT = '00'
               PERFORM LOG-FILE-OPEN-ERROR
               SET ERROR-OCCURRED TO TRUE
               GO TO INITIALIZE-EXIT.
           
           OPEN INPUT LPPAYF.
           IF FS-LPPAYF NOT = '00'
               PERFORM LOG-FILE-OPEN-ERROR
               SET ERROR-OCCURRED TO TRUE
               GO TO INITIALIZE-EXIT.
           
           OPEN I-O LPCLMF.
           IF FS-LPCLMF NOT = '00'
               PERFORM LOG-FILE-OPEN-ERROR
               SET ERROR-OCCURRED TO TRUE
               GO TO INITIALIZE-EXIT.
           
           OPEN OUTPUT LRRPTF.
           IF FS-LRRPTF NOT = '00'
               PERFORM LOG-FILE-OPEN-ERROR
               SET ERROR-OCCURRED TO TRUE
               GO TO INITIALIZE-EXIT.
           
       INITIALIZE-EXIT.
           EXIT.
       
       INITIALIZE-AGGREGATION-TABLE.
           PERFORM VARYING CH-IDX FROM 1 BY 1
               UNTIL CH-IDX > 9
               MOVE CH-IDX TO WS-CH-CODE(CH-IDX)
               MOVE 0 TO WS-CH-PREM-NORMAL(CH-IDX)
               MOVE 0 TO WS-CH-PREM-ABNORM(CH-IDX)
               MOVE 0 TO WS-CH-BILL-AMT(CH-IDX)
               MOVE 0 TO WS-CH-RECEIPT-AMT(CH-IDX)
               MOVE 0 TO WS-CH-DIFF-AMT(CH-IDX)
           END-PERFORM.
       
       PROCESS-PREMIUM-FILE.
           MOVE 'N' TO WS-EOF-LFPRMF.
           MOVE 0 TO WS-NORMAL-PREM-TOT.
           MOVE 0 TO WS-ABNORMAL-PREM-TOT.
           
           PERFORM UNTIL LFPRMF-END
               READ LFPRMF
                   AT END
                       SET LFPRMF-END TO TRUE
                   NOT AT END
                       PERFORM PROCESS-PREMIUM-RECORD
               END-READ
           END-PERFORM.
       
       PROCESS-PREMIUM-RECORD.
           IF PR-BAND-KBN NOT NUMERIC
               MOVE 0 TO WS-CHANNEL-KBN
           ELSE
               MOVE PR-BAND-KBN TO WS-CHANNEL-KBN
               IF WS-CHANNEL-KBN < 1 OR WS-CHANNEL-KBN > 9
                   MOVE 9 TO WS-CHANNEL-KBN
               END-IF
           END-IF.
           
           SET CH-IDX TO WS-CHANNEL-KBN.
           
           IF PR-CALC-STATUS-KBN = '0'
               ADD PR-PRM-AMT TO WS-NORMAL-PREM-TOT
               ADD PR-PRM-AMT 
                   TO WS-CH-PREM-NORMAL(CH-IDX)
           ELSE
               ADD PR-PRM-AMT TO WS-ABNORMAL-PREM-TOT
               ADD PR-PRM-AMT
                   TO WS-CH-PREM-ABNORM(CH-IDX)
           END-IF.
       
       PROCESS-CLAIM-PAYMENT-MATCHING.
           MOVE 'N' TO WS-EOF-LPPAYF.
           MOVE 0 TO WS-BILL-AMT-TOT.
           MOVE 0 TO WS-RECEIPT-AMT-TOT.
           MOVE 0 TO WS-UNCOLLECTED-TOT.
           MOVE 0 TO WS-OVERPAID-TOT.
           
           PERFORM UNTIL LPPAYF-END
               READ LPPAYF
                   AT END
                       SET LPPAYF-END TO TRUE
                   NOT AT END
                       PERFORM MATCH-PAYMENT-TO-CLAIM
               END-READ
           END-PERFORM.
       
       MATCH-PAYMENT-TO-CLAIM.
           IF PY-PAY-CHANNEL-KBN NOT NUMERIC
               MOVE 0 TO WS-CHANNEL-KBN
           ELSE
               MOVE PY-PAY-CHANNEL-KBN TO WS-CHANNEL-KBN
               IF WS-CHANNEL-KBN < 1 OR WS-CHANNEL-KBN > 9
                   MOVE 9 TO WS-CHANNEL-KBN
               END-IF
           END-IF.
           
           SET CH-IDX TO WS-CHANNEL-KBN.
           MOVE PY-POL-NO TO CL-POL-NO.
           
           READ LPCLMF
               KEY IS CL-POL-NO
               INVALID KEY
                   ADD PY-PAY-AMT TO WS-OVERPAID-TOT
                   ADD PY-PAY-AMT
                       TO WS-CH-DIFF-AMT(CH-IDX)
           END-READ.
           
           IF FS-LPCLMF = '00'
               ADD CL-BILL-AMT TO WS-BILL-AMT-TOT
               ADD CL-RECEIPT-AMT TO WS-RECEIPT-AMT-TOT
               ADD CL-BILL-AMT
                   TO WS-CH-BILL-AMT(CH-IDX)
               ADD CL-RECEIPT-AMT
                   TO WS-CH-RECEIPT-AMT(CH-IDX)
               
               COMPUTE WS-CALC-DIFF =
                   CL-BILL-AMT - CL-RECEIPT-AMT
               
               IF WS-CALC-DIFF > 0
                   ADD WS-CALC-DIFF TO WS-UNCOLLECTED-TOT
               ELSE IF WS-CALC-DIFF < 0
                   SUBTRACT WS-CALC-DIFF
                       FROM WS-OVERPAID-TOT
               END-IF
               END-IF
           END-IF.
       
       OUTPUT-AGGREGATION-REPORT.
           MOVE 1 TO WS-LINE-NUMBER.
           
           PERFORM VARYING CH-IDX FROM 1 BY 1
               UNTIL CH-IDX > 9
               IF WS-CH-PREM-NORMAL(CH-IDX) > 0
                   OR WS-CH-PREM-ABNORM(CH-IDX) > 0
                   OR WS-CH-BILL-AMT(CH-IDX) > 0
                   PERFORM OUTPUT-CHANNEL-DETAIL
               END-IF
           END-PERFORM.
           
           PERFORM OUTPUT-TOTAL-LINE.
       
       OUTPUT-CHANNEL-DETAIL.
           ADD 1 TO WS-REPORT-COUNT.
           COMPUTE WS-REPORT-ID = FUNCTION INTEGER(
               FUNCTION RANDOM * 99999999999).
           
           MOVE WS-REPORT-YM TO RP-REPORT-YM.
           MOVE '01' TO RP-REPORT-TYPE-KBN.
           MOVE WS-CHANNEL-KBN TO RP-POL-NO.
           MOVE WS-LINE-NUMBER TO RP-LINE-NO.
           MOVE WS-REPORT-ID TO RP-REPORT-ID.
           
           IF WS-CH-PREM-NORMAL(CH-IDX) > 0
               MOVE WS-CH-PREM-NORMAL(CH-IDX) TO RP-PRINT-AMT
           ELSE
               MOVE WS-CH-PREM-ABNORM(CH-IDX) TO RP-PRINT-AMT
           END-IF.
           
           MOVE '01' TO RP-OUTPUT-STATUS-KBN.
           
           WRITE LRRPTF-REC.
           IF FS-LRRPTF NOT = '00'
               ADD 1 TO WS-ERROR-COUNT
               PERFORM LOG-FILE-IO-ERROR
           ELSE
               ADD 1 TO WS-LINE-NUMBER
           END-IF.
       
       OUTPUT-TOTAL-LINE.
           ADD 1 TO WS-REPORT-COUNT.
           COMPUTE WS-REPORT-ID = FUNCTION INTEGER(
               FUNCTION RANDOM * 99999999999).
           
           MOVE WS-REPORT-YM TO RP-REPORT-YM.
           MOVE '99' TO RP-REPORT-TYPE-KBN.
           MOVE WS-NORMAL-PREM-TOT TO RP-PRINT-AMT.
           MOVE '01' TO RP-OUTPUT-STATUS-KBN.
           MOVE WS-LINE-NUMBER TO RP-LINE-NO.
           MOVE WS-REPORT-ID TO RP-REPORT-ID.
           
           WRITE LRRPTF-REC.
           IF FS-LRRPTF NOT = '00'
               ADD 1 TO WS-ERROR-COUNT
               PERFORM LOG-FILE-IO-ERROR
           END-IF.
       
       FINAL-AUDIT.
           CLOSE LFPRMF.
           CLOSE LPPAYF.
           CLOSE LPCLMF.
           CLOSE LRRPTF.
           
           IF WS-ERROR-COUNT > 0
               SET ERROR-OCCURRED TO TRUE
           END-IF.
       
       LOG-FILE-OPEN-ERROR.
           DISPLAY '集計処理エラー ファイルOPEN失敗'.
       
       LOG-FILE-IO-ERROR.
           DISPLAY 'ファイルI/O エラー 発生'.
