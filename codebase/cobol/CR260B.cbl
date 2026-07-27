       IDENTIFICATION DIVISION.
       PROGRAM-ID. CR260B.
       AUTHOR. BATCH-TEAM.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CCMONF ASSIGN TO "CCMONF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-ST-CCMONF.
           SELECT CCXFRF ASSIGN TO "CCXFRF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-ST-CCXFRF.
           SELECT CCPOSF ASSIGN TO "CCPOSF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS PS-ORG-CD
               FILE STATUS IS WS-ST-CCPOSF.
           SELECT CCRPTF ASSIGN TO "CCRPTF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-ST-CCRPTF.
           SELECT CCERRF ASSIGN TO "CCERRF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-ST-CCERRF.

       DATA DIVISION.
       FILE SECTION.
       FD  CCMONF.
           COPY CCMONC.
       FD  CCXFRF.
           COPY CCXFRC.
       FD  CCPOSF.
           COPY CCPOSC.
       FD  CCRPTF.
           COPY CCRPTC.
       FD  CCERRF.
           COPY CCERRC.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05 WS-ST-CCMONF              PIC XX VALUE SPACE.
           05 WS-ST-CCXFRF              PIC XX VALUE SPACE.
           05 WS-ST-CCPOSF              PIC XX VALUE SPACE.
           05 WS-ST-CCRPTF              PIC XX VALUE SPACE.
           05 WS-ST-CCERRF              PIC XX VALUE SPACE.

       01  WS-CONTROL.
           05 WS-END-CCMONF             PIC X VALUE "N".
              88 CCMONF-END                  VALUE "Y".
           05 WS-END-CCXFRF             PIC X VALUE "N".
              88 CCXFRF-END                  VALUE "Y".
           05 WS-HARD-ERROR             PIC X VALUE "N".
              88 HARD-ERROR                  VALUE "Y".
           05 WS-XFR-OPENED             PIC X VALUE "N".
              88 XFR-OPENED                  VALUE "Y".

       01  WS-DATE-AREA.
           05 WS-TODAY                  PIC 9(8) VALUE ZERO.
           05 WS-BASE-YYYYMM            PIC 9(6) VALUE ZERO.
           05 WS-BASE-DT                PIC 9(8) VALUE ZERO.
           05 WS-XF-YYYYMM              PIC 9(6) VALUE ZERO.
           05 WS-BASE-MM                PIC 99 VALUE ZERO.
           05 WS-BASE-YYYY              PIC 9(4) VALUE ZERO.
           05 WS-MONTH-DAYS             PIC 99 VALUE ZERO.
           05 WS-LEAP-REM               PIC 9 VALUE ZERO.

       01  WS-AMOUNT-AREA.
           05 WS-INSTR-AMT              PIC S9(15)V99 VALUE ZERO.
           05 WS-VALUE-AMT              PIC S9(15)V99 VALUE ZERO.
           05 WS-XFR-IN-AMT             PIC S9(15)V99 VALUE ZERO.
           05 WS-XFR-OUT-AMT            PIC S9(15)V99 VALUE ZERO.
           05 WS-NET-XFR-AMT            PIC S9(15)V99 VALUE ZERO.
           05 WS-POS-AVAILABLE          PIC S9(15)V99 VALUE ZERO.
           05 WS-POS-RESERVED           PIC S9(15)V99 VALUE ZERO.
           05 WS-POS-USABLE             PIC S9(15)V99 VALUE ZERO.
           05 WS-SHORT-AMT              PIC S9(15)V99 VALUE ZERO.

       01  WS-COUNT-AREA.
           05 WS-MON-COUNT              PIC 9(9) VALUE ZERO.
           05 WS-XFR-IN-COUNT           PIC 9(9) VALUE ZERO.
           05 WS-XFR-OUT-COUNT          PIC 9(9) VALUE ZERO.
           05 WS-XFR-COUNT              PIC 9(9) VALUE ZERO.
           05 WS-DIFF-COUNT             PIC S9(9) VALUE ZERO.
           05 WS-ABS-DIFF-COUNT         PIC 9(9) VALUE ZERO.
           05 WS-LINE-NO                PIC 9(7) VALUE ZERO.
           05 WS-RPT-SEQ                PIC 9(9) VALUE ZERO.
           05 WS-ERR-SEQ                PIC 9(9) VALUE ZERO.

       01  WS-EDIT-AREA.
           05 WS-RPT-ID                 PIC X(20) VALUE SPACE.
           05 WS-ERR-ID                 PIC X(20) VALUE SPACE.
           05 WS-REC-KEY                PIC X(40) VALUE SPACE.
           05 WS-TEXT                   PIC X(120) VALUE SPACE.
           05 WS-AMT-EDIT               PIC ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.
           05 WS-AMT-EDIT2              PIC ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.
           05 WS-CNT-EDIT               PIC ZZZ,ZZZ,ZZ9.
           05 WS-CNT-EDIT2              PIC ZZZ,ZZZ,ZZ9.

       01  WS-CONSTANTS.
           05 WS-PGM-ID                 PIC X(8) VALUE "CR260B".
           05 WS-REPORT-KBN             PIC X(2) VALUE "26".
           05 WS-ERR-KBN-IO             PIC X(2) VALUE "IO".
           05 WS-ERR-KBN-CNT            PIC X(2) VALUE "CT".
           05 WS-ERR-KBN-POS            PIC X(2) VALUE "PS".
           05 WS-STATUS-VALID           PIC X VALUE "1".
           05 WS-XFR-DONE               PIC X VALUE "9".
           05 WS-DIFF-LIMIT             PIC 9(9) VALUE 100.

       PROCEDURE DIVISION.
       MAIN-SECTION.
           PERFORM 0000-INIT
           IF NOT HARD-ERROR
               PERFORM 1000-MAIN
           END-IF
           PERFORM 9000-FINAL
           GOBACK.

       0000-INIT.
           MOVE 0 TO RETURN-CODE
           ACCEPT WS-TODAY FROM DATE YYYYMMDD
           MOVE WS-TODAY(1:6) TO WS-BASE-YYYYMM
           PERFORM 0100-CALC-MONTH-END
           OPEN INPUT CCMONF
           IF WS-ST-CCMONF NOT = "00"
               DISPLAY "CCMONF OPEN ERROR ST=" WS-ST-CCMONF
               MOVE 12 TO RETURN-CODE
               SET HARD-ERROR TO TRUE
           END-IF
           IF NOT HARD-ERROR
               OPEN INPUT CCPOSF
               IF WS-ST-CCPOSF NOT = "00"
                   DISPLAY "CCPOSF OPEN ERROR ST=" WS-ST-CCPOSF
                   MOVE 12 TO RETURN-CODE
                   SET HARD-ERROR TO TRUE
               END-IF
           END-IF
           IF NOT HARD-ERROR
               OPEN OUTPUT CCRPTF
               IF WS-ST-CCRPTF NOT = "00"
                   DISPLAY "CCRPTF OPEN ERROR ST=" WS-ST-CCRPTF
                   MOVE 12 TO RETURN-CODE
                   SET HARD-ERROR TO TRUE
               END-IF
           END-IF
           IF NOT HARD-ERROR
               OPEN OUTPUT CCERRF
               IF WS-ST-CCERRF NOT = "00"
                   DISPLAY "CCERRF OPEN ERROR ST=" WS-ST-CCERRF
                   MOVE 12 TO RETURN-CODE
                   SET HARD-ERROR TO TRUE
               END-IF
           END-IF.

       0100-CALC-MONTH-END.
           MOVE WS-BASE-YYYYMM(1:4) TO WS-BASE-YYYY
           MOVE WS-BASE-YYYYMM(5:2) TO WS-BASE-MM
           EVALUATE WS-BASE-MM
               WHEN 01 WHEN 03 WHEN 05 WHEN 07 WHEN 08 WHEN 10 WHEN 12
                   MOVE 31 TO WS-MONTH-DAYS
               WHEN 04 WHEN 06 WHEN 09 WHEN 11
                   MOVE 30 TO WS-MONTH-DAYS
               WHEN 02
                   DIVIDE WS-BASE-YYYY BY 4
                       GIVING WS-BASE-YYYY REMAINDER WS-LEAP-REM
                   IF WS-LEAP-REM = 0
                       MOVE 29 TO WS-MONTH-DAYS
                   ELSE
                       MOVE 28 TO WS-MONTH-DAYS
                   END-IF
               WHEN OTHER
                   MOVE 31 TO WS-MONTH-DAYS
           END-EVALUATE
           MOVE WS-BASE-YYYYMM TO WS-BASE-DT(1:6)
           MOVE WS-MONTH-DAYS TO WS-BASE-DT(7:2).

       1000-MAIN.
           PERFORM 1100-READ-MONTH
           PERFORM UNTIL CCMONF-END OR HARD-ERROR
               IF MN-YYYYMM = WS-BASE-YYYYMM
                   PERFORM 2000-PROCESS-ORG
               END-IF
               PERFORM 1100-READ-MONTH
           END-PERFORM.

       1100-READ-MONTH.
           READ CCMONF
               AT END
                   SET CCMONF-END TO TRUE
               NOT AT END
                   IF WS-ST-CCMONF NOT = "00"
                       DISPLAY "CCMONF READ ERROR ST=" WS-ST-CCMONF
                       MOVE 12 TO RETURN-CODE
                       SET HARD-ERROR TO TRUE
                   END-IF
           END-READ.

       2000-PROCESS-ORG.
           MOVE ZERO TO WS-XFR-IN-AMT
                        WS-XFR-OUT-AMT
                        WS-XFR-IN-COUNT
                        WS-XFR-OUT-COUNT
                        WS-XFR-COUNT
                        WS-NET-XFR-AMT
                        WS-SHORT-AMT
           MOVE MN-TOTAL-INSTR-AMT TO WS-INSTR-AMT
           MOVE MN-TOTAL-VALUE-AMT TO WS-VALUE-AMT
           COMPUTE WS-MON-COUNT =
               FUNCTION NUMVAL(MN-COUNT-INSTR) +
               FUNCTION NUMVAL(MN-COUNT-VALUE)
           PERFORM 2100-READ-POSITION
           IF NOT HARD-ERROR
               PERFORM 2200-SCAN-XFER
           END-IF
           IF NOT HARD-ERROR
               COMPUTE WS-NET-XFR-AMT = WS-XFR-IN-AMT - WS-XFR-OUT-AMT
               COMPUTE WS-POS-USABLE =
                   WS-POS-AVAILABLE - WS-POS-RESERVED
               PERFORM 2300-WRITE-REPORT
               PERFORM 2400-CHECK-DIFF
               PERFORM 2500-CHECK-SHORT
           END-IF.

       2100-READ-POSITION.
           INITIALIZE CCPOSF-REC
           MOVE MN-ORG-CD TO PS-ORG-CD
           READ CCPOSF KEY IS PS-ORG-CD
               INVALID KEY
                   MOVE ZERO TO WS-POS-AVAILABLE WS-POS-RESERVED
                   PERFORM 8100-WRITE-POS-ERROR
               NOT INVALID KEY
                   IF WS-ST-CCPOSF = "00"
                       MOVE PS-AVAILABLE-AMT TO WS-POS-AVAILABLE
                       MOVE PS-RESERVED-AMT TO WS-POS-RESERVED
                       IF PS-POSITION-STATUS-KBN NOT = WS-STATUS-VALID
                           PERFORM 8100-WRITE-POS-ERROR
                       END-IF
                   ELSE
                       DISPLAY "CCPOSF READ ERROR ST=" WS-ST-CCPOSF
                       MOVE 12 TO RETURN-CODE
                       SET HARD-ERROR TO TRUE
                   END-IF
           END-READ.

       2200-SCAN-XFER.
           MOVE "N" TO WS-END-CCXFRF
           OPEN INPUT CCXFRF
           IF WS-ST-CCXFRF NOT = "00"
               DISPLAY "CCXFRF OPEN ERROR ST=" WS-ST-CCXFRF
               MOVE 12 TO RETURN-CODE
               SET HARD-ERROR TO TRUE
           ELSE
               SET XFR-OPENED TO TRUE
           END-IF
           PERFORM UNTIL CCXFRF-END OR HARD-ERROR
               READ CCXFRF
                   AT END
                       SET CCXFRF-END TO TRUE
                   NOT AT END
                       IF WS-ST-CCXFRF NOT = "00"
                           DISPLAY "CCXFRF READ ERROR ST=" WS-ST-CCXFRF
                           MOVE 12 TO RETURN-CODE
                           SET HARD-ERROR TO TRUE
                       ELSE
                           PERFORM 2210-ACCUM-XFER
                       END-IF
               END-READ
           END-PERFORM
           IF XFR-OPENED
               CLOSE CCXFRF
               IF WS-ST-CCXFRF NOT = "00"
                   DISPLAY "CCXFRF CLOSE ERROR ST=" WS-ST-CCXFRF
                   MOVE 12 TO RETURN-CODE
                   SET HARD-ERROR TO TRUE
               END-IF
               MOVE "N" TO WS-XFR-OPENED
           END-IF.

       2210-ACCUM-XFER.
           MOVE XF-VALUE-DT(1:6) TO WS-XF-YYYYMM
           IF WS-XF-YYYYMM = WS-BASE-YYYYMM
              AND XF-XFER-STATUS-KBN = WS-XFR-DONE
               IF XF-TO-ORG-CD = MN-ORG-CD
                   ADD XF-XFER-AMT TO WS-XFR-IN-AMT
                   ADD 1 TO WS-XFR-IN-COUNT
               END-IF
               IF XF-FROM-ORG-CD = MN-ORG-CD
                   ADD XF-XFER-AMT TO WS-XFR-OUT-AMT
                   ADD 1 TO WS-XFR-OUT-COUNT
               END-IF
           END-IF.

       2300-WRITE-REPORT.
           ADD 1 TO WS-RPT-SEQ
           ADD 1 TO WS-LINE-NO
           INITIALIZE CCRPTF-REC
           MOVE "RP-CR260B-" TO WS-RPT-ID(1:10)
           MOVE WS-RPT-SEQ TO WS-RPT-ID(12:9)
           MOVE WS-RPT-ID TO RP-REPORT-ID
           MOVE WS-BASE-DT TO RP-BASE-DT
           MOVE WS-REPORT-KBN TO RP-REPORT-KBN
           MOVE WS-LINE-NO TO RP-LINE-NO
           MOVE WS-VALUE-AMT TO WS-AMT-EDIT
           MOVE WS-NET-XFR-AMT TO WS-AMT-EDIT2
           MOVE WS-MON-COUNT TO WS-CNT-EDIT
           ADD WS-XFR-IN-COUNT WS-XFR-OUT-COUNT GIVING WS-XFR-COUNT
           MOVE WS-XFR-COUNT TO WS-CNT-EDIT2
           STRING
               "ORG=" DELIMITED BY SIZE
               MN-ORG-CD DELIMITED BY SIZE
               " MON-CNT=" DELIMITED BY SIZE
               WS-CNT-EDIT DELIMITED BY SIZE
               " XFR-CNT=" DELIMITED BY SIZE
               WS-CNT-EDIT2 DELIMITED BY SIZE
               " VALUE-AMT=" DELIMITED BY SIZE
               WS-AMT-EDIT DELIMITED BY SIZE
               " NET-XFR=" DELIMITED BY SIZE
               WS-AMT-EDIT2 DELIMITED BY SIZE
               INTO WS-TEXT
           END-STRING
           MOVE WS-TEXT TO RP-REPORT-TEXT
           WRITE CCRPTF-REC
           IF WS-ST-CCRPTF NOT = "00"
               DISPLAY "CCRPTF WRITE ERROR ST=" WS-ST-CCRPTF
               MOVE 12 TO RETURN-CODE
               SET HARD-ERROR TO TRUE
           END-IF.

       2400-CHECK-DIFF.
           ADD WS-XFR-IN-COUNT WS-XFR-OUT-COUNT GIVING WS-XFR-COUNT
           COMPUTE WS-DIFF-COUNT = WS-MON-COUNT - WS-XFR-COUNT
           IF WS-DIFF-COUNT < 0
               COMPUTE WS-ABS-DIFF-COUNT = WS-DIFF-COUNT * -1
           ELSE
               MOVE WS-DIFF-COUNT TO WS-ABS-DIFF-COUNT
           END-IF
           IF WS-ABS-DIFF-COUNT > WS-DIFF-LIMIT
               PERFORM 8200-WRITE-COUNT-ERROR
           END-IF.

       2500-CHECK-SHORT.
           COMPUTE WS-SHORT-AMT = WS-VALUE-AMT + WS-NET-XFR-AMT
           IF WS-POS-USABLE < WS-SHORT-AMT
               PERFORM 8300-WRITE-SHORT-ERROR
           END-IF.

       8100-WRITE-POS-ERROR.
           INITIALIZE WS-TEXT WS-REC-KEY
           MOVE MN-ORG-CD TO WS-REC-KEY
           STRING
               "POSITION ERROR ORG=" DELIMITED BY SIZE
               MN-ORG-CD DELIMITED BY SIZE
               INTO WS-TEXT
           END-STRING
           PERFORM 8900-WRITE-ERROR.

       8200-WRITE-COUNT-ERROR.
           INITIALIZE WS-TEXT WS-REC-KEY
           MOVE MN-ORG-CD TO WS-REC-KEY
           MOVE WS-MON-COUNT TO WS-CNT-EDIT
           MOVE WS-XFR-COUNT TO WS-CNT-EDIT2
           STRING
               "COUNT DIFF ORG=" DELIMITED BY SIZE
               MN-ORG-CD DELIMITED BY SIZE
               " MON=" DELIMITED BY SIZE
               WS-CNT-EDIT DELIMITED BY SIZE
               " XFR=" DELIMITED BY SIZE
               WS-CNT-EDIT2 DELIMITED BY SIZE
               INTO WS-TEXT
           END-STRING
           PERFORM 8910-WRITE-COUNT-ERROR.

       8300-WRITE-SHORT-ERROR.
           INITIALIZE WS-TEXT WS-REC-KEY
           MOVE MN-ORG-CD TO WS-REC-KEY
           MOVE WS-POS-USABLE TO WS-AMT-EDIT
           MOVE WS-SHORT-AMT TO WS-AMT-EDIT2
           STRING
               "SHORT WARNING ORG=" DELIMITED BY SIZE
               MN-ORG-CD DELIMITED BY SIZE
               " USABLE=" DELIMITED BY SIZE
               WS-AMT-EDIT DELIMITED BY SIZE
               " REQUIRED=" DELIMITED BY SIZE
               WS-AMT-EDIT2 DELIMITED BY SIZE
               INTO WS-TEXT
           END-STRING
           PERFORM 8920-WRITE-SHORT-ERROR.

       8900-WRITE-ERROR.
           MOVE WS-ERR-KBN-POS TO ER-ERROR-KBN
           PERFORM 8990-WRITE-ERROR-COMMON.

       8910-WRITE-COUNT-ERROR.
           MOVE WS-ERR-KBN-CNT TO ER-ERROR-KBN
           PERFORM 8990-WRITE-ERROR-COMMON.

       8920-WRITE-SHORT-ERROR.
           MOVE WS-ERR-KBN-POS TO ER-ERROR-KBN
           PERFORM 8990-WRITE-ERROR-COMMON.

       8990-WRITE-ERROR-COMMON.
           ADD 1 TO WS-ERR-SEQ
           INITIALIZE CCERRF-REC
           MOVE "ER-CR260B-" TO WS-ERR-ID(1:10)
           MOVE WS-ERR-SEQ TO WS-ERR-ID(12:9)
           MOVE WS-ERR-ID TO ER-ERROR-ID
           MOVE WS-PGM-ID TO ER-PGM-ID
           MOVE WS-BASE-DT TO ER-BASE-DT
           MOVE WS-REC-KEY TO ER-RECORD-KEY
           MOVE WS-TEXT TO ER-ERROR-TEXT
           WRITE CCERRF-REC
           IF WS-ST-CCERRF NOT = "00"
               DISPLAY "CCERRF WRITE ERROR ST=" WS-ST-CCERRF
               MOVE 12 TO RETURN-CODE
               SET HARD-ERROR TO TRUE
           END-IF.

       9000-FINAL.
           IF XFR-OPENED
               CLOSE CCXFRF
           END-IF
           CLOSE CCMONF
           IF WS-ST-CCMONF NOT = "00"
              AND WS-ST-CCMONF NOT = "42"
               DISPLAY "CCMONF CLOSE ERROR ST=" WS-ST-CCMONF
               MOVE 8 TO RETURN-CODE
           END-IF
           CLOSE CCPOSF
           IF WS-ST-CCPOSF NOT = "00"
              AND WS-ST-CCPOSF NOT = "42"
               DISPLAY "CCPOSF CLOSE ERROR ST=" WS-ST-CCPOSF
               MOVE 8 TO RETURN-CODE
           END-IF
           CLOSE CCRPTF
           IF WS-ST-CCRPTF NOT = "00"
              AND WS-ST-CCRPTF NOT = "42"
               DISPLAY "CCRPTF CLOSE ERROR ST=" WS-ST-CCRPTF
               MOVE 8 TO RETURN-CODE
           END-IF
           CLOSE CCERRF
           IF WS-ST-CCERRF NOT = "00"
              AND WS-ST-CCERRF NOT = "42"
               DISPLAY "CCERRF CLOSE ERROR ST=" WS-ST-CCERRF
               MOVE 8 TO RETURN-CODE
           END-IF
           IF RETURN-CODE = 0
               DISPLAY "CR260B NORMAL END YYYYMM=" WS-BASE-YYYYMM
           ELSE
               DISPLAY "CR260B ABNORMAL END RC=" RETURN-CODE
           END-IF.
