       IDENTIFICATION DIVISION.
       PROGRAM-ID. JH470B.
      * 変更履歴
      * 版数  年月日(和暦)  担当                       概要
      * 1.00  平成28年04月01日 システム部 情報系チーム 初版作成
      * 1.10  令和02年10月15日 システム部 情報系チーム DWH増分抽出条件見直し
      * 1.20  令和05年07月03日 システム部 情報系チーム 変更捕捉ログ出力追加
      *---------------------------------------------------------------*
      * DWH INCREMENTAL CHANGE CAPTURE BATCH                           *
      *---------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT JHSTGF ASSIGN TO "JHSTGF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-STG-ST.

           SELECT JHDWHF ASSIGN TO "JHDWHF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-DW-ST.

           SELECT JHCTLKF ASSIGN TO "JHCTLKF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CT-JOB-ID
               FILE STATUS IS WS-CT-ST.

           SELECT JHCHGEF ASSIGN TO "JHCHGEF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-CHG-ST.

           SELECT JHAUDTF ASSIGN TO "JHAUDTF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-AUD-ST.

       DATA DIVISION.
       FILE SECTION.

       FD  JHSTGF.
           COPY JHSTGC.

       FD  JHDWHF.
           COPY JHDWHFC.

       FD  JHCTLKF.
           COPY JHCTLC.

       FD  JHCHGEF.
           COPY JHCHGC.

       FD  JHAUDTF.
           COPY JHAUDC.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05 WS-STG-ST              PIC XX VALUE SPACES.
           05 WS-DW-ST               PIC XX VALUE SPACES.
           05 WS-CT-ST               PIC XX VALUE SPACES.
           05 WS-CHG-ST              PIC XX VALUE SPACES.
           05 WS-AUD-ST              PIC XX VALUE SPACES.

       01  WS-CONSTANTS.
           05 WS-JOB-ID              PIC X(08) VALUE "JH470B  ".
           05 WS-DATASET-STG         PIC X(08) VALUE "JHSTGF  ".
           05 WS-DATASET-CHG         PIC X(08) VALUE "JHCHGEF ".
           05 WS-EV-START            PIC X(04) VALUE "STRT".
           05 WS-EV-END              PIC X(04) VALUE "NEND".
           05 WS-EV-ERR              PIC X(04) VALUE "ERR ".
           05 WS-TYPE-ADD            PIC X(01) VALUE "A".
           05 WS-TYPE-COR            PIC X(01) VALUE "C".
           05 WS-TYPE-DEL            PIC X(01) VALUE "D".
           05 WS-STAT-PEND           PIC X(01) VALUE "0".
           05 WS-CTL-RUN             PIC X(01) VALUE "R".
           05 WS-CTL-END             PIC X(01) VALUE "E".
           05 WS-CTL-ERR             PIC X(01) VALUE "9".

       01  WS-SWITCHES.
           05 WS-STG-EOF             PIC X VALUE "N".
              88 STG-EOF             VALUE "Y".
              88 STG-NOT-EOF         VALUE "N".
           05 WS-DW-EOF              PIC X VALUE "N".
              88 DW-EOF              VALUE "Y".
              88 DW-NOT-EOF          VALUE "N".
           05 WS-HARD-ERROR          PIC X VALUE "N".
              88 HARD-ERROR          VALUE "Y".
              88 NO-HARD-ERROR       VALUE "N".
           05 WS-FIRST-EVENT         PIC X VALUE "Y".
              88 FIRST-EVENT         VALUE "Y".
              88 NOT-FIRST-EVENT     VALUE "N".

       01  WS-COUNTERS.
           05 WS-STG-IN-CNT          PIC 9(11) VALUE ZERO.
           05 WS-DW-IN-CNT           PIC 9(11) VALUE ZERO.
           05 WS-CHG-OUT-CNT         PIC 9(11) VALUE ZERO.
           05 WS-SKIP-CNT            PIC 9(11) VALUE ZERO.
           05 WS-AUD-SEQ             PIC 9(11) VALUE ZERO.
           05 WS-CHG-SEQ             PIC 9(11) VALUE ZERO.
           05 WS-ERR-CNT             PIC 9(07) VALUE ZERO.

       01  WS-AMOUNTS.
           05 WS-STG-AMT-TOTAL       PIC S9(15)V99 VALUE ZERO.
           05 WS-CHG-AMT-TOTAL       PIC S9(15)V99 VALUE ZERO.

       01  WS-CURRENT.
           05 WS-CUR-ACCT            PIC X(20) VALUE HIGH-VALUES.
           05 WS-CUR-DATE            PIC 9(08) VALUE 99999999.
           05 WS-CUR-TS              PIC X(20) VALUE SPACES.
           05 WS-BUSINESS-DT         PIC 9(08) VALUE ZERO.

       01  WS-HASH-WORK.
           05 WS-HASH-SOURCE.
              10 WS-HASH-AMT         PIC S9(15)V99 VALUE ZERO.
              10 WS-HASH-YTD         PIC S9(15)V99 VALUE ZERO.
              10 WS-HASH-DATE        PIC 9(08) VALUE ZERO.
           05 WS-HASH-DISP.
              10 WS-HD-AMT           PIC -9(15).99.
              10 WS-HD-YTD           PIC -9(15).99.
              10 WS-HD-DATE          PIC 9(08).
           05 WS-HASH-IDX            PIC 9(03) COMP VALUE ZERO.
           05 WS-HASH-LEN            PIC 9(03) COMP VALUE 42.
           05 WS-HASH-CHAR           PIC X VALUE SPACE.
           05 WS-HASH-ORD            PIC 9(05) COMP VALUE ZERO.
           05 WS-HASH-SUM            PIC 9(09) COMP VALUE ZERO.
           05 WS-HASH-RESULT         PIC X(16) VALUE SPACES.
           05 WS-BEFORE-HASH         PIC X(16) VALUE SPACES.
           05 WS-AFTER-HASH          PIC X(16) VALUE SPACES.
           05 WS-LAST-BEFORE-HASH    PIC X(16) VALUE LOW-VALUES.
           05 WS-LAST-AFTER-HASH     PIC X(16) VALUE LOW-VALUES.
           05 WS-LAST-ACCT           PIC X(20) VALUE LOW-VALUES.

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE 8 TO RETURN-CODE
           PERFORM 1000-INITIALIZE
           IF NO-HARD-ERROR
              PERFORM 2000-PROCESS
           END-IF
           PERFORM 9000-FINALIZE
           GOBACK
           .

       1000-INITIALIZE.
           SET NO-HARD-ERROR TO TRUE
           SET STG-NOT-EOF TO TRUE
           SET DW-NOT-EOF TO TRUE
           MOVE FUNCTION CURRENT-DATE(1:14) TO WS-CUR-TS

           OPEN INPUT JHSTGF JHDWHF
           IF WS-STG-ST NOT = "00"
              DISPLAY "JHSTGF OPEN ERR ST=" WS-STG-ST
              SET HARD-ERROR TO TRUE
           END-IF
           IF WS-DW-ST NOT = "00"
              DISPLAY "JHDWHF OPEN ERR ST=" WS-DW-ST
              SET HARD-ERROR TO TRUE
           END-IF

           OPEN I-O JHCTLKF
           IF WS-CT-ST NOT = "00"
              DISPLAY "JHCTLKF OPEN ERR ST=" WS-CT-ST
              SET HARD-ERROR TO TRUE
           END-IF

           OPEN OUTPUT JHCHGEF JHAUDTF
           IF WS-CHG-ST NOT = "00"
              DISPLAY "JHCHGEF OPEN ERR ST=" WS-CHG-ST
              SET HARD-ERROR TO TRUE
           END-IF
           IF WS-AUD-ST NOT = "00"
              DISPLAY "JHAUDTF OPEN ERR ST=" WS-AUD-ST
              SET HARD-ERROR TO TRUE
           END-IF

           IF NO-HARD-ERROR
              MOVE WS-JOB-ID TO CT-JOB-ID
              READ JHCTLKF KEY IS CT-JOB-ID
                 INVALID KEY
                    DISPLAY "CTL NOT FOUND JOB=" WS-JOB-ID
                    SET HARD-ERROR TO TRUE
                 NOT INVALID KEY
                    MOVE CT-BUSINESS-DT TO WS-BUSINESS-DT
                    MOVE WS-CTL-RUN TO CT-STATUS-CD
                    MOVE WS-CUR-TS TO CT-START-TS
                    REWRITE JHCTLKF-REC
                    IF WS-CT-ST NOT = "00"
                       DISPLAY "CTL START ERR ST=" WS-CT-ST
                       SET HARD-ERROR TO TRUE
                    END-IF
              END-READ
           END-IF

           IF NO-HARD-ERROR
              PERFORM 7100-WRITE-START-AUDIT
              PERFORM 3100-READ-STG
              PERFORM 3200-READ-DW
           END-IF
           .

       2000-PROCESS.
           PERFORM UNTIL HARD-ERROR OR (STG-EOF AND DW-EOF)
              EVALUATE TRUE
                 WHEN STG-EOF
                    PERFORM 4300-BUILD-DELETE
                    PERFORM 3200-READ-DW
                 WHEN DW-EOF
                    PERFORM 4100-BUILD-ADD
                    PERFORM 3100-READ-STG
                 WHEN STG-ACCT-NO < DW-ACCT-NO
                    PERFORM 4100-BUILD-ADD
                    PERFORM 3100-READ-STG
                 WHEN STG-ACCT-NO > DW-ACCT-NO
                    PERFORM 4300-BUILD-DELETE
                    PERFORM 3200-READ-DW
                 WHEN STG-CYCLE-DT < DW-CYCLE-DT
                    PERFORM 4100-BUILD-ADD
                    PERFORM 3100-READ-STG
                 WHEN STG-CYCLE-DT > DW-CYCLE-DT
                    PERFORM 4300-BUILD-DELETE
                    PERFORM 3200-READ-DW
                 WHEN OTHER
                    PERFORM 4200-BUILD-CORRECT
                    PERFORM 3100-READ-STG
                    PERFORM 3200-READ-DW
              END-EVALUATE
           END-PERFORM
           .

       3100-READ-STG.
           READ JHSTGF
              AT END
                 SET STG-EOF TO TRUE
                 MOVE HIGH-VALUES TO STG-ACCT-NO
                 MOVE 99999999 TO STG-CYCLE-DT
              NOT AT END
                 ADD 1 TO WS-STG-IN-CNT
                 ADD STG-FEE-AMT TO WS-STG-AMT-TOTAL
                 PERFORM 5100-VALIDATE-STG
           END-READ
           IF WS-STG-ST NOT = "00" AND WS-STG-ST NOT = "10"
              DISPLAY "JHSTGF READ ERR ST=" WS-STG-ST
              SET HARD-ERROR TO TRUE
           END-IF
           .

       3200-READ-DW.
           READ JHDWHF
              AT END
                 SET DW-EOF TO TRUE
                 MOVE HIGH-VALUES TO DW-ACCT-NO
                 MOVE 99999999 TO DW-CYCLE-DT
              NOT AT END
                 ADD 1 TO WS-DW-IN-CNT
                 PERFORM 5200-VALIDATE-DW
           END-READ
           IF WS-DW-ST NOT = "00" AND WS-DW-ST NOT = "10"
              DISPLAY "JHDWHF READ ERR ST=" WS-DW-ST
              SET HARD-ERROR TO TRUE
           END-IF
           .

       4100-BUILD-ADD.
           IF STG-EDIT-STATUS NOT = "0"
              ADD 1 TO WS-SKIP-CNT
              DISPLAY "ADD SKIP ACCT=" STG-ACCT-NO
           ELSE
              MOVE SPACES TO WS-BEFORE-HASH
              MOVE STG-FEE-AMT TO WS-HASH-AMT
              MOVE STG-FEE-YTD TO WS-HASH-YTD
              MOVE STG-CYCLE-DT TO WS-HASH-DATE
              PERFORM 6100-MAKE-HASH
              MOVE WS-HASH-RESULT TO WS-AFTER-HASH
              MOVE STG-ACCT-NO TO WS-CUR-ACCT
              MOVE STG-CYCLE-DT TO WS-CUR-DATE
              PERFORM 6200-WRITE-CHANGE-ADD
           END-IF
           .

       4200-BUILD-CORRECT.
           MOVE DW-FEE-AMT TO WS-HASH-AMT
           MOVE DW-FEE-YTD TO WS-HASH-YTD
           MOVE DW-CYCLE-DT TO WS-HASH-DATE
           PERFORM 6100-MAKE-HASH
           MOVE WS-HASH-RESULT TO WS-BEFORE-HASH

           MOVE STG-FEE-AMT TO WS-HASH-AMT
           MOVE STG-FEE-YTD TO WS-HASH-YTD
           MOVE STG-CYCLE-DT TO WS-HASH-DATE
           PERFORM 6100-MAKE-HASH
           MOVE WS-HASH-RESULT TO WS-AFTER-HASH

           IF WS-BEFORE-HASH = WS-AFTER-HASH
              ADD 1 TO WS-SKIP-CNT
           ELSE
              MOVE STG-ACCT-NO TO WS-CUR-ACCT
              MOVE STG-CYCLE-DT TO WS-CUR-DATE
              PERFORM 6300-WRITE-CHANGE-COR
           END-IF
           .

       4300-BUILD-DELETE.
           MOVE DW-FEE-AMT TO WS-HASH-AMT
           MOVE DW-FEE-YTD TO WS-HASH-YTD
           MOVE DW-CYCLE-DT TO WS-HASH-DATE
           PERFORM 6100-MAKE-HASH
           MOVE WS-HASH-RESULT TO WS-BEFORE-HASH
           MOVE SPACES TO WS-AFTER-HASH
           MOVE DW-ACCT-NO TO WS-CUR-ACCT
           MOVE DW-CYCLE-DT TO WS-CUR-DATE
           PERFORM 6400-WRITE-CHANGE-DEL
           .

       5100-VALIDATE-STG.
           IF STG-ACCT-NO = SPACES
              DISPLAY "STG ACCT BLANK"
              SET HARD-ERROR TO TRUE
           END-IF
           IF STG-CYCLE-DT = ZERO
              DISPLAY "STG DATE ZERO ACCT=" STG-ACCT-NO
              SET HARD-ERROR TO TRUE
           END-IF
           IF STG-FEE-AMT < ZERO
              DISPLAY "STG AMT NEG ACCT=" STG-ACCT-NO
              SET HARD-ERROR TO TRUE
           END-IF
           IF STG-FEE-YTD < ZERO
              DISPLAY "STG YTD NEG ACCT=" STG-ACCT-NO
              SET HARD-ERROR TO TRUE
           END-IF
           IF STG-SOURCE-CD = SPACES
              DISPLAY "STG SRC BLANK ACCT=" STG-ACCT-NO
              SET HARD-ERROR TO TRUE
           END-IF
           .

       5200-VALIDATE-DW.
           IF DW-ACCT-NO = SPACES
              DISPLAY "DW ACCT BLANK"
              SET HARD-ERROR TO TRUE
           END-IF
           IF DW-CYCLE-DT = ZERO
              DISPLAY "DW DATE ZERO ACCT=" DW-ACCT-NO
              SET HARD-ERROR TO TRUE
           END-IF
           IF DW-FEE-AMT < ZERO
              DISPLAY "DW AMT NEG ACCT=" DW-ACCT-NO
              SET HARD-ERROR TO TRUE
           END-IF
           IF DW-FEE-YTD < ZERO
              DISPLAY "DW YTD NEG ACCT=" DW-ACCT-NO
              SET HARD-ERROR TO TRUE
           END-IF
           .

       6100-MAKE-HASH.
           MOVE WS-HASH-AMT TO WS-HD-AMT
           MOVE WS-HASH-YTD TO WS-HD-YTD
           MOVE WS-HASH-DATE TO WS-HD-DATE
           MOVE ZERO TO WS-HASH-SUM
           PERFORM VARYING WS-HASH-IDX FROM 1 BY 1
              UNTIL WS-HASH-IDX > WS-HASH-LEN
              MOVE WS-HASH-DISP(WS-HASH-IDX:1) TO WS-HASH-CHAR
              COMPUTE WS-HASH-ORD =
                 FUNCTION ORD(WS-HASH-CHAR) * WS-HASH-IDX
              ADD WS-HASH-ORD TO WS-HASH-SUM
           END-PERFORM
           COMPUTE WS-HASH-SUM =
              FUNCTION MOD(WS-HASH-SUM 999999937)
           MOVE SPACES TO WS-HASH-RESULT
           MOVE WS-HASH-SUM TO WS-HASH-RESULT(1:9)
           MOVE WS-HD-DATE TO WS-HASH-RESULT(10:7)
           .

       6200-WRITE-CHANGE-ADD.
           PERFORM 6500-CHECK-DUPLICATE
           IF NOT-FIRST-EVENT
              ADD 1 TO WS-CHG-SEQ
              MOVE WS-CHG-SEQ TO CHG-CHANGE-SEQ
              MOVE WS-CUR-ACCT TO CHG-ACCT-NO
              MOVE FUNCTION CURRENT-DATE(1:14) TO CHG-CHANGE-TS
              MOVE WS-TYPE-ADD TO CHG-CHANGE-TYPE
              MOVE WS-BEFORE-HASH TO CHG-BEFORE-HASH
              MOVE WS-AFTER-HASH TO CHG-AFTER-HASH
              MOVE WS-STAT-PEND TO CHG-APPLY-STATUS
              WRITE JHCHGEF-REC
              PERFORM 6600-CHECK-CHG-WRITE
           END-IF
           .

       6300-WRITE-CHANGE-COR.
           PERFORM 6500-CHECK-DUPLICATE
           IF NOT-FIRST-EVENT
              ADD 1 TO WS-CHG-SEQ
              MOVE WS-CHG-SEQ TO CHG-CHANGE-SEQ
              MOVE WS-CUR-ACCT TO CHG-ACCT-NO
              MOVE FUNCTION CURRENT-DATE(1:14) TO CHG-CHANGE-TS
              MOVE WS-TYPE-COR TO CHG-CHANGE-TYPE
              MOVE WS-BEFORE-HASH TO CHG-BEFORE-HASH
              MOVE WS-AFTER-HASH TO CHG-AFTER-HASH
              MOVE WS-STAT-PEND TO CHG-APPLY-STATUS
              WRITE JHCHGEF-REC
              PERFORM 6600-CHECK-CHG-WRITE
           END-IF
           .

       6400-WRITE-CHANGE-DEL.
           PERFORM 6500-CHECK-DUPLICATE
           IF NOT-FIRST-EVENT
              ADD 1 TO WS-CHG-SEQ
              MOVE WS-CHG-SEQ TO CHG-CHANGE-SEQ
              MOVE WS-CUR-ACCT TO CHG-ACCT-NO
              MOVE FUNCTION CURRENT-DATE(1:14) TO CHG-CHANGE-TS
              MOVE WS-TYPE-DEL TO CHG-CHANGE-TYPE
              MOVE WS-BEFORE-HASH TO CHG-BEFORE-HASH
              MOVE WS-AFTER-HASH TO CHG-AFTER-HASH
              MOVE WS-STAT-PEND TO CHG-APPLY-STATUS
              WRITE JHCHGEF-REC
              PERFORM 6600-CHECK-CHG-WRITE
           END-IF
           .

       6500-CHECK-DUPLICATE.
           IF WS-CUR-ACCT = WS-LAST-ACCT
              AND WS-BEFORE-HASH = WS-LAST-BEFORE-HASH
              AND WS-AFTER-HASH = WS-LAST-AFTER-HASH
              ADD 1 TO WS-SKIP-CNT
              SET FIRST-EVENT TO TRUE
           ELSE
              MOVE WS-CUR-ACCT TO WS-LAST-ACCT
              MOVE WS-BEFORE-HASH TO WS-LAST-BEFORE-HASH
              MOVE WS-AFTER-HASH TO WS-LAST-AFTER-HASH
              SET NOT-FIRST-EVENT TO TRUE
           END-IF
           .

       6600-CHECK-CHG-WRITE.
           IF WS-CHG-ST = "00"
              ADD 1 TO WS-CHG-OUT-CNT
              ADD 1 TO WS-CHG-AMT-TOTAL
           ELSE
              DISPLAY "JHCHGEF WRITE ERR ST=" WS-CHG-ST
              SET HARD-ERROR TO TRUE
           END-IF
           .

       7100-WRITE-START-AUDIT.
           ADD 1 TO WS-AUD-SEQ
           MOVE WS-AUD-SEQ TO AUD-AUDIT-SEQ
           MOVE WS-JOB-ID TO AUD-JOB-ID
           MOVE WS-BUSINESS-DT TO AUD-BUSINESS-DT
           MOVE WS-EV-START TO AUD-EVENT-CD
           MOVE WS-DATASET-STG TO AUD-DATASET-ID
           MOVE ZERO TO AUD-REC-CNT
           MOVE ZERO TO AUD-AMT-TOTAL
           MOVE FUNCTION CURRENT-DATE(1:14) TO AUD-EVENT-TS
           WRITE JHAUDTF-REC
           IF WS-AUD-ST NOT = "00"
              DISPLAY "JHAUDTF START ERR ST=" WS-AUD-ST
              SET HARD-ERROR TO TRUE
           END-IF
           .

       7200-WRITE-END-AUDIT.
           ADD 1 TO WS-AUD-SEQ
           MOVE WS-AUD-SEQ TO AUD-AUDIT-SEQ
           MOVE WS-JOB-ID TO AUD-JOB-ID
           MOVE WS-BUSINESS-DT TO AUD-BUSINESS-DT
           MOVE WS-EV-END TO AUD-EVENT-CD
           MOVE WS-DATASET-CHG TO AUD-DATASET-ID
           MOVE WS-CHG-OUT-CNT TO AUD-REC-CNT
           MOVE WS-CHG-AMT-TOTAL TO AUD-AMT-TOTAL
           MOVE FUNCTION CURRENT-DATE(1:14) TO AUD-EVENT-TS
           WRITE JHAUDTF-REC
           IF WS-AUD-ST NOT = "00"
              DISPLAY "JHAUDTF END ERR ST=" WS-AUD-ST
              SET HARD-ERROR TO TRUE
           END-IF
           .

       7300-WRITE-ERR-AUDIT.
           ADD 1 TO WS-AUD-SEQ
           ADD 1 TO WS-ERR-CNT
           MOVE WS-AUD-SEQ TO AUD-AUDIT-SEQ
           MOVE WS-JOB-ID TO AUD-JOB-ID
           MOVE WS-BUSINESS-DT TO AUD-BUSINESS-DT
           MOVE WS-EV-ERR TO AUD-EVENT-CD
           MOVE WS-DATASET-CHG TO AUD-DATASET-ID
           MOVE WS-ERR-CNT TO AUD-REC-CNT
           MOVE ZERO TO AUD-AMT-TOTAL
           MOVE FUNCTION CURRENT-DATE(1:14) TO AUD-EVENT-TS
           WRITE JHAUDTF-REC
           IF WS-AUD-ST NOT = "00"
              DISPLAY "JHAUDTF ERR AUD ST=" WS-AUD-ST
           END-IF
           .

       9000-FINALIZE.
           IF HARD-ERROR
              PERFORM 7300-WRITE-ERR-AUDIT
              MOVE WS-CTL-ERR TO CT-STATUS-CD
              MOVE FUNCTION CURRENT-DATE(1:14) TO CT-END-TS
              MOVE WS-STG-IN-CNT TO CT-INPUT-CNT
              MOVE WS-CHG-OUT-CNT TO CT-OUTPUT-CNT
              REWRITE JHCTLKF-REC
              MOVE 12 TO RETURN-CODE
              DISPLAY "JH470B ABEND IN=" WS-STG-IN-CNT
              DISPLAY "JH470B ABEND OUT=" WS-CHG-OUT-CNT
           ELSE
              PERFORM 7200-WRITE-END-AUDIT
              MOVE WS-CTL-END TO CT-STATUS-CD
              MOVE FUNCTION CURRENT-DATE(1:14) TO CT-END-TS
              MOVE WS-STG-IN-CNT TO CT-INPUT-CNT
              MOVE WS-CHG-OUT-CNT TO CT-OUTPUT-CNT
              REWRITE JHCTLKF-REC
              IF WS-CT-ST = "00"
                 MOVE 0 TO RETURN-CODE
                 DISPLAY "JH470B NORMAL IN=" WS-STG-IN-CNT
                 DISPLAY "JH470B NORMAL OUT=" WS-CHG-OUT-CNT
                 DISPLAY "JH470B SKIP=" WS-SKIP-CNT
              ELSE
                 DISPLAY "CTL END ERR ST=" WS-CT-ST
                 MOVE 8 TO RETURN-CODE
              END-IF
           END-IF

           CLOSE JHSTGF JHDWHF JHCTLKF JHCHGEF JHAUDTF
           .
