       IDENTIFICATION DIVISION.
       PROGRAM-ID. JH410B.
      * 版数  年月日(和暦)   担当              概要
      * 1.00  平成30.04.01  石井 香織 (E-072) 分析用DWH抽出初版作成
      * 1.01  令和02.10.15  田中 正人 (E-118) 抽出条件見直し対応
      * 1.02  令和05.06.30  佐藤 美咲 (E-203) DWH連携項目追加対応
       AUTHOR. JH-SYSTEM.
      *
      * ANALYSIS DWH EXTRACT BATCH.
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.
      *
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZFEEHF
               ASSIGN TO "KZFEEHF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-KZFEEHF-ST.
      *
           SELECT JHDWHF
               ASSIGN TO "JHDWHF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-JHDWHF-ST.
      *
       DATA DIVISION.
       FILE SECTION.
      *
       FD  KZFEEHF
           RECORDING MODE IS F.
       COPY KZFEEHFC.
      *
       FD  JHDWHF
           RECORDING MODE IS F.
       COPY JHDWHFC.
      *
       WORKING-STORAGE SECTION.
      *
       01  WS-FILE-STATUS.
           05  WS-KZFEEHF-ST          PIC X(02) VALUE SPACES.
           05  WS-JHDWHF-ST           PIC X(02) VALUE SPACES.
      *
       01  WS-SWITCHES.
           05  WS-EOF-SW              PIC X(01) VALUE 'N'.
               88  EOF-KZFEEHF                  VALUE 'Y'.
               88  NOT-EOF-KZFEEHF              VALUE 'N'.
           05  WS-ABEND-SW            PIC X(01) VALUE 'N'.
               88  ABEND-ON                     VALUE 'Y'.
               88  ABEND-OFF                    VALUE 'N'.
           05  WS-KZFEEHF-OPEN-SW     PIC X(01) VALUE 'N'.
               88  KZFEEHF-OPEN                 VALUE 'Y'.
               88  KZFEEHF-CLOSED               VALUE 'N'.
           05  WS-JHDWHF-OPEN-SW      PIC X(01) VALUE 'N'.
               88  JHDWHF-OPEN                  VALUE 'Y'.
               88  JHDWHF-CLOSED                VALUE 'N'.
      *
       01  WS-COUNTERS.
           05  WS-READ-CNT            PIC 9(09) COMP-3 VALUE 0.
           05  WS-WRITE-CNT           PIC 9(09) COMP-3 VALUE 0.
           05  WS-SKIP-CNT            PIC 9(09) COMP-3 VALUE 0.
      *
       01  WS-DATE-AREA.
           05  WS-CURRENT-DATE        PIC X(21) VALUE SPACES.
           05  WS-RUN-DATE            PIC 9(08) VALUE 0.
      *
       PROCEDURE DIVISION.
      *
       0000-MAIN SECTION.
      *
           PERFORM 1000-INITIALIZE
      *
           IF ABEND-OFF
               PERFORM 2000-PROCESS
                   UNTIL EOF-KZFEEHF OR ABEND-ON
           END-IF
      *
           PERFORM 9000-FINALIZE
      *
           GOBACK
           .
      *
       1000-INITIALIZE SECTION.
      *
           MOVE 0 TO RETURN-CODE
           SET NOT-EOF-KZFEEHF TO TRUE
           SET ABEND-OFF TO TRUE
           SET KZFEEHF-CLOSED TO TRUE
           SET JHDWHF-CLOSED TO TRUE
      *
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE
           MOVE WS-CURRENT-DATE(1:8) TO WS-RUN-DATE
      *
           DISPLAY 'JH410B START DATE=' WS-RUN-DATE
      *
           OPEN INPUT KZFEEHF
           IF WS-KZFEEHF-ST = '00'
               SET KZFEEHF-OPEN TO TRUE
           ELSE
               DISPLAY 'KZFEEHF OPEN ERROR ST=' WS-KZFEEHF-ST
               MOVE 12 TO RETURN-CODE
               SET ABEND-ON TO TRUE
           END-IF
      *
           IF ABEND-OFF
               OPEN OUTPUT JHDWHF
               IF WS-JHDWHF-ST = '00'
                   SET JHDWHF-OPEN TO TRUE
               ELSE
                   DISPLAY 'JHDWHF OPEN ERROR ST=' WS-JHDWHF-ST
                   MOVE 12 TO RETURN-CODE
                   SET ABEND-ON TO TRUE
               END-IF
           END-IF
      *
           IF ABEND-OFF
               PERFORM 2100-READ-KZFEEHF
           END-IF
           .
      *
       2000-PROCESS SECTION.
      *
      *    OUTPUT ALL FEE HISTORY RECORDS TO DWH EXTRACT.
      *    EACH KZFEEHF RECORD IS WRITTEN TO JHDWHF IN SEQUENCE.
           PERFORM 2200-WRITE-JHDWHF
      *
           IF ABEND-OFF
               PERFORM 2100-READ-KZFEEHF
           END-IF
           .
      *
       2100-READ-KZFEEHF SECTION.
      *
           READ KZFEEHF
               AT END
                   SET EOF-KZFEEHF TO TRUE
               NOT AT END
                   ADD 1 TO WS-READ-CNT
           END-READ
      *
           IF WS-KZFEEHF-ST NOT = '00'
              AND WS-KZFEEHF-ST NOT = '10'
               DISPLAY 'KZFEEHF READ ERROR ST=' WS-KZFEEHF-ST
               MOVE 12 TO RETURN-CODE
               SET ABEND-ON TO TRUE
           END-IF
           .
      *
       2200-WRITE-JHDWHF SECTION.
      *
           MOVE FH-ACCT-NO   TO DW-ACCT-NO
           MOVE FH-CYCLE-DT  TO DW-CYCLE-DT
           MOVE FH-FEE-AMT   TO DW-FEE-AMT
           MOVE FH-FEE-YTD   TO DW-FEE-YTD
           MOVE WS-RUN-DATE  TO DW-EXTRACT-DT
      *
           WRITE JHDWHF-REC
      *
           IF WS-JHDWHF-ST = '00'
               ADD 1 TO WS-WRITE-CNT
           ELSE
               DISPLAY 'JHDWHF WRITE ERROR ST=' WS-JHDWHF-ST
               DISPLAY 'ACCOUNT=' FH-ACCT-NO
               MOVE 12 TO RETURN-CODE
               SET ABEND-ON TO TRUE
           END-IF
           .
      *
       9000-FINALIZE SECTION.
      *
           IF KZFEEHF-OPEN
               CLOSE KZFEEHF
               SET KZFEEHF-CLOSED TO TRUE
               IF WS-KZFEEHF-ST NOT = '00'
                   DISPLAY 'KZFEEHF CLOSE ERROR ST=' WS-KZFEEHF-ST
                   IF RETURN-CODE = 0
                       MOVE 8 TO RETURN-CODE
                   END-IF
               END-IF
           END-IF
      *
           IF JHDWHF-OPEN
               CLOSE JHDWHF
               SET JHDWHF-CLOSED TO TRUE
               IF WS-JHDWHF-ST NOT = '00'
                   DISPLAY 'JHDWHF CLOSE ERROR ST=' WS-JHDWHF-ST
                   IF RETURN-CODE = 0
                       MOVE 8 TO RETURN-CODE
                   END-IF
               END-IF
           END-IF
      *
           DISPLAY 'JH410B END READ=' WS-READ-CNT
           DISPLAY 'JH410B END WRITE=' WS-WRITE-CNT
           DISPLAY 'JH410B END SKIP=' WS-SKIP-CNT
      *
           IF RETURN-CODE = 0
               DISPLAY 'JH410B NORMAL END'
           ELSE
               DISPLAY 'JH410B ABNORMAL END RC=' RETURN-CODE
           END-IF
           .
