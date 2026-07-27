       IDENTIFICATION DIVISION.
       PROGRAM-ID. LF210B.
       AUTHOR. 守屋 健司.
      *
      * 解約返戻金計算バッチ。
      * 解約控除 = 未償却新契約費 = 新契約費 × max(0, 償却月数 - 経過月数) / 償却月数。
      * 解約返戻金 = max(0, 責任準備金 - 解約控除)。円未満切捨て。
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LFCVPF ASSIGN TO "LFCVPF"
               ORGANIZATION IS SEQUENTIAL FILE STATUS IS WS-IN-ST.
           SELECT LFCVRF ASSIGN TO "LFCVRF"
               ORGANIZATION IS SEQUENTIAL FILE STATUS IS WS-OUT-ST.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  LFCVPF.
       COPY LFCVPFC.
       FD  LFCVRF.
       COPY LFCVRFC.
      *
       WORKING-STORAGE SECTION.
       01  WS-IN-ST              PIC X(02).
       01  WS-OUT-ST             PIC X(02).
       01  WS-EOF                PIC X(01) VALUE 'N'.
       01  WS-SEQ                PIC 9(08) VALUE ZERO.
      *
      * 新契約費の償却月数 (直線償却の基準)。
       01  WS-AMORT-MONTHS       PIC 9(05) VALUE 60.
      *
       01  WS-WORK.
           05  WS-REMAIN         PIC S9(05).
           05  WS-CHARGE         PIC 9(13).
           05  WS-CALC           PIC 9(18).
           05  WS-CV             PIC S9(13).
      *
       PROCEDURE DIVISION.
       0000-MAIN SECTION.
       0000-START.
           OPEN INPUT LFCVPF
           OPEN OUTPUT LFCVRF
           IF WS-IN-ST NOT = "00"
              DISPLAY "LF210B LFCVPF OPEN ST=" WS-IN-ST
              MOVE 8 TO RETURN-CODE
              GOBACK
           END-IF
           PERFORM UNTIL WS-EOF = 'Y'
              READ LFCVPF AT END MOVE 'Y' TO WS-EOF
              NOT AT END
                 PERFORM 1000-PROCESS-POLICY
              END-READ
           END-PERFORM
           CLOSE LFCVPF LFCVRF
           MOVE 0 TO RETURN-CODE
           GOBACK
           .
      *
       1000-PROCESS-POLICY SECTION.
       1000-START.
           ADD 1 TO WS-SEQ
           INITIALIZE LFCVRF-REC
           STRING "CV" DELIMITED BY SIZE
                  WS-SEQ DELIMITED BY SIZE
                  INTO CO-CV-ID
           END-STRING
           MOVE CI-POL-NO      TO CO-POL-NO
           MOVE CI-RESERVE-AMT TO CO-RESERVE-AMT
      *
           IF CI-CV-STATUS-KBN NOT = "01"
              MOVE ZERO TO CO-SURR-CHARGE-AMT
              MOVE ZERO TO CO-CV-AMT
              MOVE "S"  TO CO-CALC-STATUS-KBN
              WRITE LFCVRF-REC
              GO TO 1000-EXIT
           END-IF
      *
           PERFORM 2000-COMPUTE-CHARGE
           PERFORM 3000-COMPUTE-CV
      *
           MOVE WS-CHARGE TO CO-SURR-CHARGE-AMT
           MOVE WS-CV     TO CO-CV-AMT
           MOVE "C"       TO CO-CALC-STATUS-KBN
           WRITE LFCVRF-REC
           .
       1000-EXIT.
           EXIT.
      *
       2000-COMPUTE-CHARGE SECTION.
       2000-START.
      *    未償却残月 = max(0, 償却月数 - 経過月数)
           COMPUTE WS-REMAIN = WS-AMORT-MONTHS - CI-ELAPSED-MONTH-CNT
           IF WS-REMAIN < ZERO
              MOVE ZERO TO WS-REMAIN
           END-IF
      *    解約控除 = 新契約費 × 未償却残月 / 償却月数 (円未満切捨て)
           COMPUTE WS-CALC = CI-NEWBIZ-COST-AMT * WS-REMAIN
           DIVIDE WS-CALC BY WS-AMORT-MONTHS GIVING WS-CHARGE
           .
      *
       3000-COMPUTE-CV SECTION.
       3000-START.
      *    解約返戻金 = max(0, 責任準備金 - 解約控除)
           COMPUTE WS-CV = CI-RESERVE-AMT - WS-CHARGE
           IF WS-CV < ZERO
              MOVE ZERO TO WS-CV
           END-IF
           .
