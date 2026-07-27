       IDENTIFICATION DIVISION.
       PROGRAM-ID. LF110B.
       AUTHOR. 葛西 由紀.
      *
      * 月額保険料計算バッチ。
      * 契約マスタ(LFPOLF)を読み、有効契約の月額保険料を
      * 加入年齢帯×性別の月額保険料率表により算定し LFPRMF へ出力する。
      * 月額保険料 = 切捨て( 保険金額 × 月額保険料率(×100) / 100000 )。
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LFPOLF ASSIGN TO "LFPOLF"
               ORGANIZATION IS SEQUENTIAL FILE STATUS IS WS-IN-ST.
           SELECT LFPRMF ASSIGN TO "LFPRMF"
               ORGANIZATION IS SEQUENTIAL FILE STATUS IS WS-OUT-ST.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  LFPOLF.
       COPY LFPOLFC.
       FD  LFPRMF.
       COPY LFPRMFC.
      *
       WORKING-STORAGE SECTION.
       01  WS-IN-ST              PIC X(02).
       01  WS-OUT-ST             PIC X(02).
       01  WS-EOF                PIC X(01) VALUE 'N'.
       01  WS-SEQ                PIC 9(08) VALUE ZERO.
      *
       01  WS-WORK.
           05  WS-BAND           PIC X(02).
           05  WS-RATE-X100      PIC 9(05).
           05  WS-PREM           PIC 9(13).
           05  WS-CALC           PIC 9(18).
      *
      * 月額保険料率表 (保険金額¥1,000あたり月額, ×100整数 RATE-X100)。
      * 加入年齢帯×性別。料率値は保険料率規程による。
      * --- 旧料率表 ---
       01  WS-RATE-TABLE.
           05  WS-RT-A1-M        PIC 9(05) VALUE 120.
           05  WS-RT-A1-F        PIC 9(05) VALUE 100.
           05  WS-RT-A2-M        PIC 9(05) VALUE 160.
           05  WS-RT-A2-F        PIC 9(05) VALUE 145.
           05  WS-RT-A3-M        PIC 9(05) VALUE 300.
           05  WS-RT-A3-F        PIC 9(05) VALUE 265.
           05  WS-RT-A4-M        PIC 9(05) VALUE 600.
           05  WS-RT-A4-F        PIC 9(05) VALUE 540.
           05  WS-RT-A5-M        PIC 9(05) VALUE 1250.
           05  WS-RT-A5-F        PIC 9(05) VALUE 1100.
      *
       PROCEDURE DIVISION.
       0000-MAIN SECTION.
       0000-START.
           OPEN INPUT LFPOLF
           OPEN OUTPUT LFPRMF
           IF WS-IN-ST NOT = "00"
              DISPLAY "LF110B LFPOLF OPEN ST=" WS-IN-ST
              MOVE 8 TO RETURN-CODE
              GOBACK
           END-IF
           PERFORM UNTIL WS-EOF = 'Y'
              READ LFPOLF AT END MOVE 'Y' TO WS-EOF
              NOT AT END
                 PERFORM 1000-PROCESS-POLICY
              END-READ
           END-PERFORM
           CLOSE LFPOLF LFPRMF
           MOVE 0 TO RETURN-CODE
           GOBACK
           .
      *
       1000-PROCESS-POLICY SECTION.
       1000-START.
           ADD 1 TO WS-SEQ
           INITIALIZE LFPRMF-REC
           STRING "PR" DELIMITED BY SIZE
                  WS-SEQ DELIMITED BY SIZE
                  INTO PR-PRM-ID
           END-STRING
           MOVE PO-POL-NO          TO PR-POL-NO
           MOVE PO-SUM-ASSURED-AMT TO PR-SUM-ASSURED-AMT
      *
           IF PO-POL-STATUS-KBN NOT = "01"
              MOVE ZERO   TO PR-PRM-AMT
              MOVE SPACES TO PR-BAND-KBN
              MOVE "S"    TO PR-CALC-STATUS-KBN
              WRITE LFPRMF-REC
              GO TO 1000-EXIT
           END-IF
      *
           PERFORM 2000-RESOLVE-BAND
           PERFORM 3000-LOOKUP-RATE
           PERFORM 4000-COMPUTE-PREMIUM
      *
           MOVE WS-PREM TO PR-PRM-AMT
           MOVE WS-BAND TO PR-BAND-KBN
           MOVE "C"     TO PR-CALC-STATUS-KBN
           WRITE LFPRMF-REC
           .
       1000-EXIT.
           EXIT.
      *
       2000-RESOLVE-BAND SECTION.
       2000-START.
           EVALUATE TRUE
              WHEN PO-ENTRY-AGE-CNT <= 29
                 MOVE "A1" TO WS-BAND
              WHEN PO-ENTRY-AGE-CNT <= 39
                 MOVE "A2" TO WS-BAND
              WHEN PO-ENTRY-AGE-CNT <= 49
                 MOVE "A3" TO WS-BAND
              WHEN PO-ENTRY-AGE-CNT <= 59
                 MOVE "A4" TO WS-BAND
              WHEN OTHER
                 MOVE "A5" TO WS-BAND
           END-EVALUATE
           .
      *
       3000-LOOKUP-RATE SECTION.
       3000-START.
           MOVE ZERO TO WS-RATE-X100
           EVALUATE WS-BAND ALSO PO-SEX-KBN
              WHEN "A1" ALSO "1"  MOVE WS-RT-A1-M TO WS-RATE-X100
              WHEN "A1" ALSO "2"  MOVE WS-RT-A1-F TO WS-RATE-X100
              WHEN "A2" ALSO "1"  MOVE WS-RT-A2-M TO WS-RATE-X100
              WHEN "A2" ALSO "2"  MOVE WS-RT-A2-F TO WS-RATE-X100
              WHEN "A3" ALSO "1"  MOVE WS-RT-A3-M TO WS-RATE-X100
              WHEN "A3" ALSO "2"  MOVE WS-RT-A3-F TO WS-RATE-X100
              WHEN "A4" ALSO "1"  MOVE WS-RT-A4-M TO WS-RATE-X100
              WHEN "A4" ALSO "2"  MOVE WS-RT-A4-F TO WS-RATE-X100
              WHEN "A5" ALSO "1"  MOVE WS-RT-A5-M TO WS-RATE-X100
              WHEN "A5" ALSO "2"  MOVE WS-RT-A5-F TO WS-RATE-X100
              WHEN OTHER          MOVE ZERO       TO WS-RATE-X100
           END-EVALUATE
           .
      *
       4000-COMPUTE-PREMIUM SECTION.
       4000-START.
      *    月額保険料 = 切捨て( 保険金額 × 月額保険料率(×100) / 100000 )
           COMPUTE WS-CALC = PO-SUM-ASSURED-AMT * WS-RATE-X100
           DIVIDE WS-CALC BY 100000 GIVING WS-PREM
           .
