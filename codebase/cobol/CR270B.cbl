       IDENTIFICATION DIVISION.
       PROGRAM-ID. CR270B.
       AUTHOR. MFG-SHIKIN-BATCH.
      *================================================================*
      * 確定受渡・明細・ポジション・送金ファイルの突合バッチ          *
      *================================================================*

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CCVALF ASSIGN TO "CCVALF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-CCVALF-ST.
           SELECT CCDTLF ASSIGN TO "CCDTLF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-CCDTLF-ST.
           SELECT CCPOSF ASSIGN TO "CCPOSF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS PS-ORG-CD
               FILE STATUS IS WS-CCPOSF-ST.
           SELECT CCXFRF ASSIGN TO "CCXFRF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS XF-XFER-ID
               FILE STATUS IS WS-CCXFRF-ST.
           SELECT CCERRF ASSIGN TO "CCERRF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS ER-ERROR-ID
               FILE STATUS IS WS-CCERRF-ST.
           SELECT CCRPTF ASSIGN TO "CCRPTF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-CCRPTF-ST.

       DATA DIVISION.
       FILE SECTION.

       FD  CCVALF.
           COPY CCVALFC.

       FD  CCDTLF.
           COPY CCDTLC.

       FD  CCPOSF.
           COPY CCPOSC.

       FD  CCXFRF.
           COPY CCXFRC.

       FD  CCERRF.
           COPY CCERRC.

       FD  CCRPTF.
           COPY CCRPTC.

       WORKING-STORAGE SECTION.
       01  WS-PGM-ID                 PIC X(08) VALUE "CR270B".
       01  WS-BASE-DT                PIC 9(08) VALUE ZERO.
       01  WS-ABEND-FLG              PIC X VALUE "N".
           88  WS-ABEND              VALUE "Y".

       01  WS-EOF-FLG.
           05  WS-CCVALF-EOF         PIC X VALUE "N".
           05  WS-CCDTLF-EOF         PIC X VALUE "N".
           05  WS-CCPOSF-EOF         PIC X VALUE "N".
           05  WS-CCXFRF-EOF         PIC X VALUE "N".

       01  WS-FILE-STATUS.
           05  WS-CCVALF-ST          PIC XX VALUE SPACES.
           05  WS-CCDTLF-ST          PIC XX VALUE SPACES.
           05  WS-CCPOSF-ST          PIC XX VALUE SPACES.
           05  WS-CCXFRF-ST          PIC XX VALUE SPACES.
           05  WS-CCERRF-ST          PIC XX VALUE SPACES.
           05  WS-CCRPTF-ST          PIC XX VALUE SPACES.

       01  WS-COUNTERS.
           05  WS-VAL-CNT            PIC 9(07) VALUE ZERO.
           05  WS-DTL-CNT            PIC 9(07) VALUE ZERO.
           05  WS-POS-CNT            PIC 9(07) VALUE ZERO.
           05  WS-XFR-CNT            PIC 9(07) VALUE ZERO.
           05  WS-ERR-CNT            PIC 9(07) VALUE ZERO.
           05  WS-RPT-CNT            PIC 9(07) VALUE ZERO.
           05  WS-ERR-SEQ            PIC 9(09) VALUE ZERO.
           05  WS-RPT-SEQ            PIC 9(09) VALUE ZERO.
           05  WS-IDX                PIC 9(04) VALUE ZERO.
           05  WS-JDX                PIC 9(04) VALUE ZERO.

       01  WS-WORK.
           05  WS-FOUND-FLG          PIC X VALUE "N".
           05  WS-REASON-KBN         PIC X(02) VALUE SPACES.
           05  WS-DIFF-AMT           PIC S9(15)V99 COMP-3 VALUE ZERO.
           05  WS-FCT-TOTAL          PIC S9(15)V99 COMP-3 VALUE ZERO.
           05  WS-XFER-TOTAL         PIC S9(15)V99 COMP-3 VALUE ZERO.
           05  WS-POS-RESV           PIC S9(15)V99 COMP-3 VALUE ZERO.
           05  WS-POS-AVAIL          PIC S9(15)V99 COMP-3 VALUE ZERO.
           05  WS-FCT-TOTAL-DISP     PIC -ZZZZZZZZZZZZZZ9.99.
           05  WS-XFER-TOTAL-DISP    PIC -ZZZZZZZZZZZZZZ9.99.
           05  WS-KEY-TEXT           PIC X(40) VALUE SPACES.
           05  WS-RPT-TEXT           PIC X(120) VALUE SPACES.
           05  WS-ERR-TEXT           PIC X(120) VALUE SPACES.

       01  WS-FCT-TABLE.
           05  WS-FCT-ENTRY OCCURS 500 TIMES.
               10  WS-FCT-USED       PIC X VALUE "N".
               10  WS-FCT-ID         PIC X(20) VALUE SPACES.
               10  WS-FCT-VALUE-DT   PIC 9(08) VALUE ZERO.
               10  WS-FCT-STATUS     PIC X(02) VALUE SPACES.
               10  WS-FCT-AMT        PIC S9(15)V99 COMP-3 VALUE ZERO.
               10  WS-FCT-XFER-AMT   PIC S9(15)V99 COMP-3 VALUE ZERO.
               10  WS-FCT-ORG-CD     PIC X(10) VALUE SPACES.
               10  WS-FCT-DTL-CNT    PIC 9(05) VALUE ZERO.
               10  WS-FCT-XFR-CNT    PIC 9(05) VALUE ZERO.
               10  WS-FCT-BAD-FLG    PIC X VALUE "N".

       01  WS-POS-TABLE.
           05  WS-POS-ENTRY OCCURS 500 TIMES.
               10  WS-POS-USED       PIC X VALUE "N".
               10  WS-POS-ORG-CD     PIC X(10) VALUE SPACES.
               10  WS-POS-BASE-DT    PIC 9(08) VALUE ZERO.
               10  WS-POS-AVAIL-AMT  PIC S9(15)V99 COMP-3 VALUE ZERO.
               10  WS-POS-RESV-AMT   PIC S9(15)V99 COMP-3 VALUE ZERO.
               10  WS-POS-STATUS     PIC X(02) VALUE SPACES.

       PROCEDURE DIVISION.
       MAIN-RTN.
           MOVE 0 TO RETURN-CODE
           PERFORM 1000-OPEN
           IF NOT WS-ABEND
               PERFORM 2000-LOAD-VAL
               PERFORM 2100-LOAD-POS
               PERFORM 2200-APPLY-DTL
               PERFORM 2300-APPLY-XFR
               PERFORM 3000-JUDGE
           END-IF
           PERFORM 9000-CLOSE
           IF WS-ABEND
               MOVE 8 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF
           GOBACK.

       1000-OPEN.
           OPEN INPUT CCVALF
           IF WS-CCVALF-ST NOT = "00"
               DISPLAY "CCVALF OPEN ERROR ST=" WS-CCVALF-ST
               MOVE "Y" TO WS-ABEND-FLG
           END-IF
           OPEN INPUT CCDTLF
           IF WS-CCDTLF-ST NOT = "00"
               DISPLAY "CCDTLF OPEN ERROR ST=" WS-CCDTLF-ST
               MOVE "Y" TO WS-ABEND-FLG
           END-IF
           OPEN INPUT CCPOSF
           IF WS-CCPOSF-ST NOT = "00"
               DISPLAY "CCPOSF OPEN ERROR ST=" WS-CCPOSF-ST
               MOVE "Y" TO WS-ABEND-FLG
           END-IF
           OPEN INPUT CCXFRF
           IF WS-CCXFRF-ST NOT = "00"
               DISPLAY "CCXFRF OPEN ERROR ST=" WS-CCXFRF-ST
               MOVE "Y" TO WS-ABEND-FLG
           END-IF
           OPEN OUTPUT CCERRF
           IF WS-CCERRF-ST NOT = "00"
               DISPLAY "CCERRF OPEN ERROR ST=" WS-CCERRF-ST
               MOVE "Y" TO WS-ABEND-FLG
           END-IF
           OPEN OUTPUT CCRPTF
           IF WS-CCRPTF-ST NOT = "00"
               DISPLAY "CCRPTF OPEN ERROR ST=" WS-CCRPTF-ST
               MOVE "Y" TO WS-ABEND-FLG
           END-IF.

       2000-LOAD-VAL.
           PERFORM UNTIL WS-CCVALF-EOF = "Y" OR WS-ABEND
               READ CCVALF
                   AT END
                       MOVE "Y" TO WS-CCVALF-EOF
                   NOT AT END
                       ADD 1 TO WS-VAL-CNT
                       IF WS-BASE-DT = ZERO
                           MOVE VL-VALUE-DT TO WS-BASE-DT
                       END-IF
                       IF VL-VAL-STATUS-KBN = "01"
                           PERFORM 2010-ADD-FCT
                       ELSE
                           IF VL-VAL-STATUS-KBN NOT = "08"
                              AND VL-VAL-STATUS-KBN NOT = "09"
                               MOVE VL-VAL-ID TO WS-KEY-TEXT
                               MOVE "INVALID VALUE STATUS"
                                   TO WS-ERR-TEXT
                               PERFORM 8100-WRITE-ERROR
                           END-IF
                       END-IF
               END-READ
               IF WS-CCVALF-ST NOT = "00" AND WS-CCVALF-ST NOT = "10"
                   DISPLAY "CCVALF READ ERROR ST=" WS-CCVALF-ST
                   MOVE "Y" TO WS-ABEND-FLG
               END-IF
           END-PERFORM.

       2010-ADD-FCT.
           MOVE "N" TO WS-FOUND-FLG
           PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > 500
              OR WS-FOUND-FLG = "Y"
               IF WS-FCT-USED(WS-IDX) = "N"
                   MOVE "Y" TO WS-FCT-USED(WS-IDX)
                   MOVE VL-FCT-ID TO WS-FCT-ID(WS-IDX)
                   MOVE VL-VALUE-DT TO WS-FCT-VALUE-DT(WS-IDX)
                   MOVE VL-VAL-STATUS-KBN TO WS-FCT-STATUS(WS-IDX)
                   MOVE "Y" TO WS-FOUND-FLG
               ELSE
                   IF WS-FCT-ID(WS-IDX) = VL-FCT-ID
                       MOVE "Y" TO WS-FCT-BAD-FLG(WS-IDX)
                       MOVE VL-VAL-ID TO WS-KEY-TEXT
                       MOVE "DUPLICATE FCT-ID" TO WS-ERR-TEXT
                       PERFORM 8100-WRITE-ERROR
                       MOVE "Y" TO WS-FOUND-FLG
                   END-IF
               END-IF
           END-PERFORM
           IF WS-FOUND-FLG NOT = "Y"
               MOVE VL-VAL-ID TO WS-KEY-TEXT
               MOVE "FCT TABLE OVERFLOW" TO WS-ERR-TEXT
               PERFORM 8100-WRITE-ERROR
               MOVE "Y" TO WS-ABEND-FLG
           END-IF.

       2100-LOAD-POS.
           PERFORM UNTIL WS-CCPOSF-EOF = "Y" OR WS-ABEND
               READ CCPOSF NEXT RECORD
                   AT END
                       MOVE "Y" TO WS-CCPOSF-EOF
                   NOT AT END
                       ADD 1 TO WS-POS-CNT
                       PERFORM 2110-ADD-POS
               END-READ
               IF WS-CCPOSF-ST NOT = "00" AND WS-CCPOSF-ST NOT = "10"
                   DISPLAY "CCPOSF READ ERROR ST=" WS-CCPOSF-ST
                   MOVE "Y" TO WS-ABEND-FLG
               END-IF
           END-PERFORM.

       2110-ADD-POS.
           MOVE "N" TO WS-FOUND-FLG
           PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > 500
              OR WS-FOUND-FLG = "Y"
               IF WS-POS-USED(WS-IDX) = "N"
                   MOVE "Y" TO WS-POS-USED(WS-IDX)
                   MOVE PS-ORG-CD TO WS-POS-ORG-CD(WS-IDX)
                   MOVE PS-BASE-DT TO WS-POS-BASE-DT(WS-IDX)
                   MOVE PS-AVAILABLE-AMT TO WS-POS-AVAIL-AMT(WS-IDX)
                   MOVE PS-RESERVED-AMT TO WS-POS-RESV-AMT(WS-IDX)
                   MOVE PS-POSITION-STATUS-KBN TO WS-POS-STATUS(WS-IDX)
                   MOVE "Y" TO WS-FOUND-FLG
               END-IF
           END-PERFORM
           IF WS-FOUND-FLG NOT = "Y"
               MOVE PS-ORG-CD TO WS-KEY-TEXT
               MOVE "POSITION TABLE OVERFLOW" TO WS-ERR-TEXT
               PERFORM 8100-WRITE-ERROR
               MOVE "Y" TO WS-ABEND-FLG
           END-IF.

       2200-APPLY-DTL.
           PERFORM UNTIL WS-CCDTLF-EOF = "Y" OR WS-ABEND
               READ CCDTLF
                   AT END
                       MOVE "Y" TO WS-CCDTLF-EOF
                   NOT AT END
                       ADD 1 TO WS-DTL-CNT
                       PERFORM 2210-MATCH-DTL
               END-READ
               IF WS-CCDTLF-ST NOT = "00" AND WS-CCDTLF-ST NOT = "10"
                   DISPLAY "CCDTLF READ ERROR ST=" WS-CCDTLF-ST
                   MOVE "Y" TO WS-ABEND-FLG
               END-IF
           END-PERFORM.

       2210-MATCH-DTL.
           IF DL-DETAIL-STATUS-KBN NOT = "01"
               IF DL-DETAIL-STATUS-KBN NOT = "08"
                  AND DL-DETAIL-STATUS-KBN NOT = "09"
                   MOVE DL-VAL-ID TO WS-KEY-TEXT
                   MOVE "INVALID DETAIL STATUS" TO WS-ERR-TEXT
                   PERFORM 8100-WRITE-ERROR
               END-IF
           ELSE
               MOVE "N" TO WS-FOUND-FLG
               PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > 500
                  OR WS-FOUND-FLG = "Y"
                   IF WS-FCT-USED(WS-IDX) = "Y"
                      AND WS-FCT-ID(WS-IDX) = DL-FCT-ID
                       ADD DL-DETAIL-AMT TO WS-FCT-AMT(WS-IDX)
                       ADD 1 TO WS-FCT-DTL-CNT(WS-IDX)
                       IF WS-FCT-ORG-CD(WS-IDX) = SPACES
                           MOVE DL-ORG-CD TO WS-FCT-ORG-CD(WS-IDX)
                       ELSE
                           IF WS-FCT-ORG-CD(WS-IDX) NOT = DL-ORG-CD
                               MOVE "Y" TO WS-FCT-BAD-FLG(WS-IDX)
                               MOVE DL-VAL-ID TO WS-KEY-TEXT
                               MOVE "ORG MISMATCH IN FCT"
                                   TO WS-ERR-TEXT
                               PERFORM 8100-WRITE-ERROR
                           END-IF
                       END-IF
                       IF WS-FCT-VALUE-DT(WS-IDX) NOT = DL-VALUE-DT
                           MOVE "Y" TO WS-FCT-BAD-FLG(WS-IDX)
                           MOVE DL-VAL-ID TO WS-KEY-TEXT
                           MOVE "VALUE DETAIL DATE MISMATCH"
                               TO WS-ERR-TEXT
                           PERFORM 8100-WRITE-ERROR
                       END-IF
                       MOVE "Y" TO WS-FOUND-FLG
                   END-IF
               END-PERFORM
               IF WS-FOUND-FLG NOT = "Y"
                   MOVE DL-VAL-ID TO WS-KEY-TEXT
                   MOVE "DETAIL WITHOUT VALUE" TO WS-ERR-TEXT
                   PERFORM 8100-WRITE-ERROR
               END-IF
           END-IF.

       2300-APPLY-XFR.
           PERFORM UNTIL WS-CCXFRF-EOF = "Y" OR WS-ABEND
               READ CCXFRF NEXT RECORD
                   AT END
                       MOVE "Y" TO WS-CCXFRF-EOF
                   NOT AT END
                       ADD 1 TO WS-XFR-CNT
                       PERFORM 2310-MATCH-XFR
               END-READ
               IF WS-CCXFRF-ST NOT = "00" AND WS-CCXFRF-ST NOT = "10"
                   DISPLAY "CCXFRF READ ERROR ST=" WS-CCXFRF-ST
                   MOVE "Y" TO WS-ABEND-FLG
               END-IF
           END-PERFORM.

       2310-MATCH-XFR.
           IF XF-XFER-STATUS-KBN NOT = "01"
               IF XF-XFER-STATUS-KBN NOT = "08"
                  AND XF-XFER-STATUS-KBN NOT = "09"
                   MOVE XF-XFER-ID TO WS-KEY-TEXT
                   MOVE "INVALID XFER STATUS" TO WS-ERR-TEXT
                   PERFORM 8100-WRITE-ERROR
               END-IF
           ELSE
               MOVE "N" TO WS-FOUND-FLG
               PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > 500
                  OR WS-FOUND-FLG = "Y"
                   IF WS-FCT-USED(WS-IDX) = "Y"
                      AND WS-FCT-ID(WS-IDX) = XF-FCT-ID
                       ADD XF-XFER-AMT TO WS-FCT-XFER-AMT(WS-IDX)
                       ADD 1 TO WS-FCT-XFR-CNT(WS-IDX)
                       IF WS-FCT-VALUE-DT(WS-IDX) NOT = XF-VALUE-DT
                           MOVE "Y" TO WS-FCT-BAD-FLG(WS-IDX)
                           MOVE XF-XFER-ID TO WS-KEY-TEXT
                           MOVE "VALUE XFER DATE MISMATCH"
                               TO WS-ERR-TEXT
                           PERFORM 8100-WRITE-ERROR
                       END-IF
                       MOVE "Y" TO WS-FOUND-FLG
                   END-IF
               END-PERFORM
               IF WS-FOUND-FLG NOT = "Y"
                   MOVE XF-XFER-ID TO WS-KEY-TEXT
                   MOVE "XFER WITHOUT VALUE" TO WS-ERR-TEXT
                   PERFORM 8100-WRITE-ERROR
               END-IF
           END-IF.

       3000-JUDGE.
           PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > 500
               IF WS-FCT-USED(WS-IDX) = "Y"
                   PERFORM 3010-JUDGE-ONE
               END-IF
           END-PERFORM
           DISPLAY "CR270B NORMAL END VALUE=" WS-VAL-CNT
           DISPLAY "CR270B DETAIL=" WS-DTL-CNT " XFER=" WS-XFR-CNT
           DISPLAY "CR270B REPORT=" WS-RPT-CNT " ERROR=" WS-ERR-CNT.

       3010-JUDGE-ONE.
           MOVE ZERO TO WS-POS-RESV WS-POS-AVAIL
           PERFORM 3020-GET-POSITION
           MOVE WS-FCT-AMT(WS-IDX) TO WS-FCT-TOTAL
           MOVE WS-FCT-XFER-AMT(WS-IDX) TO WS-XFER-TOTAL
           COMPUTE WS-DIFF-AMT = WS-FCT-TOTAL - WS-XFER-TOTAL
           IF WS-FCT-DTL-CNT(WS-IDX) = ZERO
               MOVE "01" TO WS-REASON-KBN
               MOVE "NO DETAIL FOR VALUE" TO WS-RPT-TEXT
               PERFORM 8200-WRITE-REPORT
           ELSE
               IF WS-FCT-XFR-CNT(WS-IDX) = ZERO
                   MOVE "02" TO WS-REASON-KBN
                   MOVE "VALUE NOT IN CASHFLOW" TO WS-RPT-TEXT
                   PERFORM 8200-WRITE-REPORT
               ELSE
                   IF WS-DIFF-AMT NOT = ZERO
                       MOVE "03" TO WS-REASON-KBN
                       MOVE "DETAIL XFER AMOUNT MISMATCH"
                           TO WS-RPT-TEXT
                       PERFORM 8200-WRITE-REPORT
                   END-IF
               END-IF
           END-IF
           IF WS-FCT-ORG-CD(WS-IDX) NOT = SPACES
               IF WS-POS-RESV > WS-POS-AVAIL
                   MOVE "04" TO WS-REASON-KBN
                   MOVE "RESERVED AMOUNT EXCEEDS AVAILABLE"
                       TO WS-RPT-TEXT
                   PERFORM 8200-WRITE-REPORT
               END-IF
           END-IF
           IF WS-FCT-BAD-FLG(WS-IDX) = "Y"
               MOVE "05" TO WS-REASON-KBN
               MOVE "FCT CONSISTENCY ERROR" TO WS-RPT-TEXT
               PERFORM 8200-WRITE-REPORT
           END-IF.

       3020-GET-POSITION.
           MOVE "N" TO WS-FOUND-FLG
           PERFORM VARYING WS-JDX FROM 1 BY 1 UNTIL WS-JDX > 500
              OR WS-FOUND-FLG = "Y"
               IF WS-POS-USED(WS-JDX) = "Y"
                  AND WS-POS-ORG-CD(WS-JDX) = WS-FCT-ORG-CD(WS-IDX)
                   MOVE WS-POS-AVAIL-AMT(WS-JDX) TO WS-POS-AVAIL
                   MOVE WS-POS-RESV-AMT(WS-JDX) TO WS-POS-RESV
                   IF WS-POS-STATUS(WS-JDX) NOT = "01"
                       MOVE WS-FCT-ORG-CD(WS-IDX) TO WS-KEY-TEXT
                       MOVE "POSITION NOT ACTIVE" TO WS-ERR-TEXT
                       PERFORM 8100-WRITE-ERROR
                   END-IF
                   MOVE "Y" TO WS-FOUND-FLG
               END-IF
           END-PERFORM
           IF WS-FOUND-FLG NOT = "Y"
              AND WS-FCT-ORG-CD(WS-IDX) NOT = SPACES
               MOVE WS-FCT-ORG-CD(WS-IDX) TO WS-KEY-TEXT
               MOVE "POSITION NOT FOUND" TO WS-ERR-TEXT
               PERFORM 8100-WRITE-ERROR
           END-IF.

       8100-WRITE-ERROR.
           ADD 1 TO WS-ERR-SEQ
           ADD 1 TO WS-ERR-CNT
           INITIALIZE CCERRF-REC
           MOVE WS-ERR-SEQ TO ER-ERROR-ID
           MOVE WS-PGM-ID TO ER-PGM-ID
           MOVE WS-BASE-DT TO ER-BASE-DT
           MOVE WS-KEY-TEXT TO ER-RECORD-KEY
           MOVE "CR" TO ER-ERROR-KBN
           MOVE WS-ERR-TEXT TO ER-ERROR-TEXT
           WRITE CCERRF-REC
           IF WS-CCERRF-ST NOT = "00"
               DISPLAY "CCERRF WRITE ERROR ST=" WS-CCERRF-ST
               MOVE "Y" TO WS-ABEND-FLG
           END-IF
           MOVE SPACES TO WS-KEY-TEXT WS-ERR-TEXT.

       8200-WRITE-REPORT.
           ADD 1 TO WS-RPT-SEQ
           ADD 1 TO WS-RPT-CNT
           INITIALIZE CCRPTF-REC
           MOVE WS-RPT-SEQ TO RP-REPORT-ID
           MOVE WS-BASE-DT TO RP-BASE-DT
           MOVE WS-REASON-KBN TO RP-REPORT-KBN
           MOVE WS-RPT-SEQ TO RP-LINE-NO
           MOVE WS-FCT-TOTAL TO WS-FCT-TOTAL-DISP
           MOVE WS-XFER-TOTAL TO WS-XFER-TOTAL-DISP
           STRING
               "FCT-ID=" DELIMITED BY SIZE
               WS-FCT-ID(WS-IDX) DELIMITED BY SPACE
               " REASON=" DELIMITED BY SIZE
               WS-RPT-TEXT DELIMITED BY SPACE
               " DETAIL=" DELIMITED BY SIZE
               WS-FCT-TOTAL-DISP DELIMITED BY SIZE
               " XFER=" DELIMITED BY SIZE
               WS-XFER-TOTAL-DISP DELIMITED BY SIZE
               INTO RP-REPORT-TEXT
           END-STRING
           WRITE CCRPTF-REC
           IF WS-CCRPTF-ST NOT = "00"
               DISPLAY "CCRPTF WRITE ERROR ST=" WS-CCRPTF-ST
               MOVE "Y" TO WS-ABEND-FLG
           END-IF
           MOVE SPACES TO WS-RPT-TEXT WS-REASON-KBN.

       9000-CLOSE.
           CLOSE CCVALF
           IF WS-CCVALF-ST NOT = "00"
              AND WS-CCVALF-ST NOT = "42"
               DISPLAY "CCVALF CLOSE ERROR ST=" WS-CCVALF-ST
               MOVE "Y" TO WS-ABEND-FLG
           END-IF
           CLOSE CCDTLF
           IF WS-CCDTLF-ST NOT = "00"
              AND WS-CCDTLF-ST NOT = "42"
               DISPLAY "CCDTLF CLOSE ERROR ST=" WS-CCDTLF-ST
               MOVE "Y" TO WS-ABEND-FLG
           END-IF
           CLOSE CCPOSF
           IF WS-CCPOSF-ST NOT = "00"
              AND WS-CCPOSF-ST NOT = "42"
               DISPLAY "CCPOSF CLOSE ERROR ST=" WS-CCPOSF-ST
               MOVE "Y" TO WS-ABEND-FLG
           END-IF
           CLOSE CCXFRF
           IF WS-CCXFRF-ST NOT = "00"
              AND WS-CCXFRF-ST NOT = "42"
               DISPLAY "CCXFRF CLOSE ERROR ST=" WS-CCXFRF-ST
               MOVE "Y" TO WS-ABEND-FLG
           END-IF
           CLOSE CCERRF
           IF WS-CCERRF-ST NOT = "00"
              AND WS-CCERRF-ST NOT = "42"
               DISPLAY "CCERRF CLOSE ERROR ST=" WS-CCERRF-ST
               MOVE "Y" TO WS-ABEND-FLG
           END-IF
           CLOSE CCRPTF
           IF WS-CCRPTF-ST NOT = "00"
              AND WS-CCRPTF-ST NOT = "42"
               DISPLAY "CCRPTF CLOSE ERROR ST=" WS-CCRPTF-ST
               MOVE "Y" TO WS-ABEND-FLG
           END-IF.
