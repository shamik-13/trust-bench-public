       IDENTIFICATION DIVISION.
       PROGRAM-ID. KZ580B.
      * 変更履歴
      * 版数  年月日(和暦)   担当                         概要
      * 1.00  令和03.04.01  システム部 勘定系チーム      新規作成
      * 1.01  令和04.10.01  システム部 勘定系チーム      引当率判定条件変更
      * 1.02  令和06.04.01  システム部 勘定系チーム      貸倒引当金端数処理見直し
      ******************************************************************
      * 貸倒引当金計算バッチ
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZDLRF ASSIGN TO "KZDLRF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-KZDLRF.
           SELECT KZDLQF ASSIGN TO "KZDLQF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-KZDLQF.
           SELECT KZRSCF ASSIGN TO "KZRSCF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-KZRSCF.
           SELECT KZCLCF ASSIGN TO "KZCLCF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS CLC-ACCT-NO
               FILE STATUS IS FS-KZCLCF.
           SELECT KZLLAF ASSIGN TO "KZLLAF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS LA-ACCT-NO
               FILE STATUS IS FS-KZLLAF.
           SELECT KZGLPF ASSIGN TO "KZGLPF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-KZGLPF.
           SELECT KZAUDF ASSIGN TO "KZAUDF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-KZAUDF.
           SELECT SYSIN-F ASSIGN TO "SYSIN"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-SYSIN.
           SELECT SYSOUT-F ASSIGN TO "SYSOUT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-SYSOUT.

       DATA DIVISION.
       FILE SECTION.
       FD  KZDLRF.
           COPY KZDLRFC.
       FD  KZDLQF.
           COPY KZDLQFC.
       FD  KZRSCF.
           COPY KZRSCCF.
       FD  KZCLCF.
           COPY KZCLCCF.
       FD  KZLLAF.
           COPY KZLLACF.
       FD  KZGLPF.
           COPY KZGLPCF.
       FD  KZAUDF.
           COPY KZAUDCF.
       FD  SYSIN-F.
       01  SYSIN-REC.
           05 SI-PROD-TYPE        PIC X(02).
           05 SI-B1-RATE          PIC 9V9999.
           05 SI-B2-RATE          PIC 9V9999.
           05 SI-B3-RATE          PIC 9V9999.
           05 SI-B4-RATE          PIC 9V9999.
           05 SI-SIZE-THRESHOLD   PIC 9(13)V99.
       FD  SYSOUT-F.
       01  SYSOUT-REC             PIC X(132).

       WORKING-STORAGE SECTION.
       01  FILE-STATUS-AREA.
           05 FS-KZDLRF           PIC XX.
           05 FS-KZDLQF           PIC XX.
           05 FS-KZRSCF           PIC XX.
           05 FS-KZCLCF           PIC XX.
           05 FS-KZLLAF           PIC XX.
           05 FS-KZGLPF           PIC XX.
           05 FS-KZAUDF           PIC XX.
           05 FS-SYSIN            PIC XX.
           05 FS-SYSOUT           PIC XX.

       01  SWITCH-AREA.
           05 EOF-KZDLRF          PIC X VALUE "N".
           05 EOF-KZDLQF          PIC X VALUE "N".
           05 EOF-KZRSCF          PIC X VALUE "N".
           05 EOF-SYSIN           PIC X VALUE "N".
           05 WS-HARD-ERROR       PIC X VALUE "N".
           05 WS-FOUND-SW         PIC X VALUE "N".

       01  CONSTANT-AREA.
           05 MAX-ACCOUNTS        PIC 9(05) VALUE 20000.
           05 MAX-RATES           PIC 9(03) VALUE 50.
           05 WS-TODAY            PIC 9(08).
           05 WS-DISC-RATE        PIC 9V9999 VALUE 0.0300.
           05 WS-RSC-RATE         PIC 9V9999 VALUE 0.3500.
           05 WS-COLL-RECOV-RATE  PIC 9V9999 VALUE 0.1500.

       01  ACCOUNT-MATRIX.
           05 ACCT-COUNT          PIC 9(05) VALUE ZERO.
           05 AM-ROW OCCURS 20000 TIMES.
              10 AM-ACCT-NO      PIC X(16).
              10 AM-PROD-TYPE    PIC X(02).
              10 AM-DAYS-OD      PIC 9(05).
              10 AM-BUCKET       PIC X(02).
              10 AM-LATE-AMT     PIC S9(13)V99.
              10 AM-NEW-STATUS   PIC X(02).
              10 AM-OD-AMT       PIC S9(13)V99.
              10 AM-DUE-DT       PIC 9(08).
              10 AM-ASOF-DT      PIC 9(08).
              10 AM-CURR-STATUS  PIC X(02).
              10 AM-CLC-STATUS   PIC X(02).
              10 AM-COLL-EST     PIC S9(13)V99.
              10 AM-RSC-STATUS   PIC X(02).
              10 AM-RSC-PAYMENT  PIC S9(13)V99.
              10 AM-PRIOR-ALLOW  PIC S9(13)V99.
              10 AM-ALLOW-AMT    PIC S9(13)V99.
              10 AM-ALLOW-RATE   PIC 9V9999.
              10 AM-TIER         PIC X(01).

       01  RATE-TABLE.
           05 RATE-COUNT          PIC 9(03) VALUE ZERO.
           05 RT-ROW OCCURS 50 TIMES.
              10 RT-PROD-TYPE    PIC X(02).
              10 RT-B1-RATE      PIC 9V9999.
              10 RT-B2-RATE      PIC 9V9999.
              10 RT-B3-RATE      PIC 9V9999.
              10 RT-B4-RATE      PIC 9V9999.
              10 RT-THRESHOLD    PIC 9(13)V99.

       01  TOTAL-AREA.
           05 TOT-CURR-ALLOW      PIC S9(15)V99 VALUE ZERO.
           05 TOT-PRIOR-ALLOW     PIC S9(15)V99 VALUE ZERO.
           05 TOT-PROVISION       PIC S9(15)V99 VALUE ZERO.
           05 TOT-REVERSAL        PIC S9(15)V99 VALUE ZERO.
           05 TOT-WRITEOFF        PIC S9(15)V99 VALUE ZERO.
           05 TOT-EXPOSURE        PIC S9(15)V99 VALUE ZERO.
           05 TOT-COUNT           PIC 9(07) VALUE ZERO.

       01  PRODUCT-SUMMARY.
           05 PS-ROW OCCURS 50 TIMES.
              10 PS-PROD-TYPE     PIC X(02).
              10 PS-CURR-ALLOW    PIC S9(15)V99.
              10 PS-PRIOR-ALLOW   PIC S9(15)V99.
              10 PS-PROVISION     PIC S9(15)V99.
              10 PS-REVERSAL      PIC S9(15)V99.
              10 PS-EXPOSURE      PIC S9(15)V99.
              10 PS-COUNT         PIC 9(07).

       01  BUCKET-SUMMARY.
           05 BS-ROW OCCURS 4 TIMES.
              10 BS-BUCKET        PIC X(02).
              10 BS-EXPOSURE      PIC S9(15)V99.
              10 BS-ALLOWANCE     PIC S9(15)V99.
              10 BS-COUNT         PIC 9(07).

       01  WORK-AREA.
           05 IX                  PIC 9(05).
           05 JX                  PIC 9(05).
           05 RX                  PIC 9(03).
           05 PX                  PIC 9(03).
           05 BX                  PIC 9(01).
           05 WS-RATE             PIC 9V9999.
           05 WS-THRESHOLD        PIC 9(13)V99.
           05 WS-EXPOSURE         PIC S9(13)V99.
           05 WS-PV-CASH          PIC S9(13)V99.
           05 WS-DELTA            PIC S9(13)V99.
           05 WS-ABS-AMT          PIC 9(13)V99.
           05 WS-EXP-GL           PIC X(10).
           05 WS-ALLOW-GL         PIC X(10).
           05 WS-CALL-RC          PIC 9(04).
           05 WS-REPORT-AMT       PIC ---,---,---,---,--9.99.
           05 WS-REPORT-RATE      PIC ZZ9.99.

       PROCEDURE DIVISION.
       MAIN-SECTION.
           MOVE 0 TO RETURN-CODE
           ACCEPT WS-TODAY FROM DATE YYYYMMDD
           PERFORM INIT-SUMMARY
           PERFORM OPEN-FILES
           IF WS-HARD-ERROR = "N"
              PERFORM LOAD-RATES
              PERFORM LOAD-DELINQUENCY
              PERFORM MERGE-DUE-FILE
              PERFORM MERGE-RESTRUCTURE
              PERFORM PROCESS-ACCOUNTS
              PERFORM WRITE-REPORT
           END-IF
           PERFORM CLOSE-FILES
           IF WS-HARD-ERROR = "Y"
              MOVE 8 TO RETURN-CODE
           ELSE
              MOVE 0 TO RETURN-CODE
           END-IF
           GOBACK.

       INIT-SUMMARY.
           PERFORM VARYING BX FROM 1 BY 1 UNTIL BX > 4
              MOVE ZERO TO BS-EXPOSURE(BX)
              MOVE ZERO TO BS-ALLOWANCE(BX)
              MOVE ZERO TO BS-COUNT(BX)
           END-PERFORM
           MOVE "B1" TO BS-BUCKET(1)
           MOVE "B2" TO BS-BUCKET(2)
           MOVE "B3" TO BS-BUCKET(3)
           MOVE "B4" TO BS-BUCKET(4)
           PERFORM VARYING PX FROM 1 BY 1 UNTIL PX > 50
              MOVE SPACES TO PS-PROD-TYPE(PX)
              MOVE ZERO TO PS-CURR-ALLOW(PX)
              MOVE ZERO TO PS-PRIOR-ALLOW(PX)
              MOVE ZERO TO PS-PROVISION(PX)
              MOVE ZERO TO PS-REVERSAL(PX)
              MOVE ZERO TO PS-EXPOSURE(PX)
              MOVE ZERO TO PS-COUNT(PX)
           END-PERFORM.

       OPEN-FILES.
           OPEN INPUT KZDLRF KZDLQF KZRSCF KZCLCF SYSIN-F
           IF FS-KZDLRF NOT = "00"
              DISPLAY "KZDLRF OPEN ERROR ST=" FS-KZDLRF
              MOVE "Y" TO WS-HARD-ERROR
           END-IF
           IF FS-KZDLQF NOT = "00"
              DISPLAY "KZDLQF OPEN ERROR ST=" FS-KZDLQF
              MOVE "Y" TO WS-HARD-ERROR
           END-IF
           IF FS-KZRSCF NOT = "00"
              DISPLAY "KZRSCF OPEN ERROR ST=" FS-KZRSCF
              MOVE "Y" TO WS-HARD-ERROR
           END-IF
           IF FS-KZCLCF NOT = "00"
              DISPLAY "KZCLCF OPEN ERROR ST=" FS-KZCLCF
              MOVE "Y" TO WS-HARD-ERROR
           END-IF
           IF FS-SYSIN NOT = "00"
              DISPLAY "SYSIN OPEN ERROR ST=" FS-SYSIN
              MOVE "Y" TO WS-HARD-ERROR
           END-IF
           OPEN I-O KZLLAF
           IF FS-KZLLAF NOT = "00" AND FS-KZLLAF NOT = "05"
              DISPLAY "KZLLAF OPEN ERROR ST=" FS-KZLLAF
              MOVE "Y" TO WS-HARD-ERROR
           END-IF
           OPEN OUTPUT KZGLPF KZAUDF SYSOUT-F
           IF FS-KZGLPF NOT = "00"
              DISPLAY "KZGLPF OPEN ERROR ST=" FS-KZGLPF
              MOVE "Y" TO WS-HARD-ERROR
           END-IF
           IF FS-KZAUDF NOT = "00"
              DISPLAY "KZAUDF OPEN ERROR ST=" FS-KZAUDF
              MOVE "Y" TO WS-HARD-ERROR
           END-IF
           IF FS-SYSOUT NOT = "00"
              DISPLAY "SYSOUT OPEN ERROR ST=" FS-SYSOUT
              MOVE "Y" TO WS-HARD-ERROR
           END-IF.

       LOAD-RATES.
           PERFORM UNTIL EOF-SYSIN = "Y" OR WS-HARD-ERROR = "Y"
              READ SYSIN-F
                 AT END
                    MOVE "Y" TO EOF-SYSIN
                 NOT AT END
                    IF RATE-COUNT >= MAX-RATES
                       DISPLAY "SYSIN RATE COUNT OVER"
                       MOVE "Y" TO WS-HARD-ERROR
                    ELSE
                       ADD 1 TO RATE-COUNT
                       MOVE SI-PROD-TYPE TO RT-PROD-TYPE(RATE-COUNT)
                       MOVE SI-B1-RATE TO RT-B1-RATE(RATE-COUNT)
                       MOVE SI-B2-RATE TO RT-B2-RATE(RATE-COUNT)
                       MOVE SI-B3-RATE TO RT-B3-RATE(RATE-COUNT)
                       MOVE SI-B4-RATE TO RT-B4-RATE(RATE-COUNT)
                       MOVE SI-SIZE-THRESHOLD
                         TO RT-THRESHOLD(RATE-COUNT)
                    END-IF
              END-READ
              IF FS-SYSIN NOT = "00" AND FS-SYSIN NOT = "10"
                 DISPLAY "SYSIN READ ERROR ST=" FS-SYSIN
                 MOVE "Y" TO WS-HARD-ERROR
              END-IF
           END-PERFORM
           IF RATE-COUNT = ZERO AND WS-HARD-ERROR = "N"
              DISPLAY "SYSIN RATE NOT FOUND"
              MOVE "Y" TO WS-HARD-ERROR
           END-IF.

       LOAD-DELINQUENCY.
           PERFORM UNTIL EOF-KZDLRF = "Y" OR WS-HARD-ERROR = "Y"
              READ KZDLRF
                 AT END
                    MOVE "Y" TO EOF-KZDLRF
                 NOT AT END
                    PERFORM ADD-DELINQUENCY
              END-READ
              IF FS-KZDLRF NOT = "00" AND FS-KZDLRF NOT = "10"
                 DISPLAY "KZDLRF READ ERROR ST=" FS-KZDLRF
                 MOVE "Y" TO WS-HARD-ERROR
              END-IF
           END-PERFORM.

       ADD-DELINQUENCY.
           IF DR-AGING-BUCKET NOT = "B1" AND
              DR-AGING-BUCKET NOT = "B2" AND
              DR-AGING-BUCKET NOT = "B3" AND
              DR-AGING-BUCKET NOT = "B4"
              DISPLAY "BAD BUCKET ACCT=" DR-ACCT-NO
              MOVE "Y" TO WS-HARD-ERROR
           ELSE
              IF DR-NEW-STATUS NOT = "00" AND
                 DR-NEW-STATUS NOT = "10" AND
                 DR-NEW-STATUS NOT = "30"
                 DISPLAY "BAD NEW STATUS ACCT=" DR-ACCT-NO
                 MOVE "Y" TO WS-HARD-ERROR
              ELSE
                 IF ACCT-COUNT >= MAX-ACCOUNTS
                    DISPLAY "ACCOUNT MATRIX COUNT OVER"
                    MOVE "Y" TO WS-HARD-ERROR
                 ELSE
                    ADD 1 TO ACCT-COUNT
                    MOVE DR-ACCT-NO TO AM-ACCT-NO(ACCT-COUNT)
                    MOVE DR-ACCT-NO(1:2)
                      TO AM-PROD-TYPE(ACCT-COUNT)
                    MOVE DR-DAYS-OVERDUE
                      TO AM-DAYS-OD(ACCT-COUNT)
                    MOVE DR-AGING-BUCKET
                      TO AM-BUCKET(ACCT-COUNT)
                    MOVE DR-LATE-CHARGE-AMT
                      TO AM-LATE-AMT(ACCT-COUNT)
                    MOVE DR-NEW-STATUS
                      TO AM-NEW-STATUS(ACCT-COUNT)
                    MOVE ZERO TO AM-OD-AMT(ACCT-COUNT)
                    MOVE ZERO TO AM-DUE-DT(ACCT-COUNT)
                    MOVE ZERO TO AM-ASOF-DT(ACCT-COUNT)
                    MOVE "00" TO AM-CURR-STATUS(ACCT-COUNT)
                    MOVE SPACES TO AM-CLC-STATUS(ACCT-COUNT)
                    MOVE ZERO TO AM-COLL-EST(ACCT-COUNT)
                    MOVE SPACES TO AM-RSC-STATUS(ACCT-COUNT)
                    MOVE ZERO TO AM-RSC-PAYMENT(ACCT-COUNT)
                    MOVE ZERO TO AM-PRIOR-ALLOW(ACCT-COUNT)
                    MOVE ZERO TO AM-ALLOW-AMT(ACCT-COUNT)
                    MOVE ZERO TO AM-ALLOW-RATE(ACCT-COUNT)
                    MOVE "G" TO AM-TIER(ACCT-COUNT)
                 END-IF
              END-IF
           END-IF.

       MERGE-DUE-FILE.
           PERFORM UNTIL EOF-KZDLQF = "Y" OR WS-HARD-ERROR = "Y"
              READ KZDLQF
                 AT END
                    MOVE "Y" TO EOF-KZDLQF
                 NOT AT END
                    PERFORM FIND-ACCOUNT-DQ
              END-READ
              IF FS-KZDLQF NOT = "00" AND FS-KZDLQF NOT = "10"
                 DISPLAY "KZDLQF READ ERROR ST=" FS-KZDLQF
                 MOVE "Y" TO WS-HARD-ERROR
              END-IF
           END-PERFORM.

       FIND-ACCOUNT-DQ.
           MOVE "N" TO WS-FOUND-SW
           PERFORM VARYING IX FROM 1 BY 1
             UNTIL IX > ACCT-COUNT OR WS-FOUND-SW = "Y"
              IF AM-ACCT-NO(IX) = DQ-ACCT-NO
                 MOVE DQ-OVERDUE-AMT TO AM-OD-AMT(IX)
                 MOVE DQ-DUE-DT TO AM-DUE-DT(IX)
                 MOVE DQ-ASOF-DT TO AM-ASOF-DT(IX)
                 MOVE DQ-CURR-STATUS TO AM-CURR-STATUS(IX)
                 MOVE "Y" TO WS-FOUND-SW
              END-IF
           END-PERFORM
           IF DQ-CURR-STATUS NOT = "00" AND
              DQ-CURR-STATUS NOT = "10" AND
              DQ-CURR-STATUS NOT = "30"
              DISPLAY "BAD CURR STATUS ACCT=" DQ-ACCT-NO
              MOVE "Y" TO WS-HARD-ERROR
           END-IF.

       MERGE-RESTRUCTURE.
           PERFORM UNTIL EOF-KZRSCF = "Y" OR WS-HARD-ERROR = "Y"
              READ KZRSCF
                 AT END
                    MOVE "Y" TO EOF-KZRSCF
                 NOT AT END
                    PERFORM FIND-ACCOUNT-RSC
              END-READ
              IF FS-KZRSCF NOT = "00" AND FS-KZRSCF NOT = "10"
                 DISPLAY "KZRSCF READ ERROR ST=" FS-KZRSCF
                 MOVE "Y" TO WS-HARD-ERROR
              END-IF
           END-PERFORM.

       FIND-ACCOUNT-RSC.
           MOVE "N" TO WS-FOUND-SW
           PERFORM VARYING IX FROM 1 BY 1
             UNTIL IX > ACCT-COUNT OR WS-FOUND-SW = "Y"
              IF AM-ACCT-NO(IX) = RSC-ACCT-NO
                 MOVE RSC-APPROVAL-STATUS TO AM-RSC-STATUS(IX)
                 MOVE RSC-NEW-PAYMENT-AMT TO AM-RSC-PAYMENT(IX)
                 MOVE "Y" TO WS-FOUND-SW
              END-IF
           END-PERFORM.

       PROCESS-ACCOUNTS.
           PERFORM VARYING IX FROM 1 BY 1
             UNTIL IX > ACCT-COUNT OR WS-HARD-ERROR = "Y"
              PERFORM ENRICH-CASE
              PERFORM READ-PRIOR-ALLOWANCE
              PERFORM CALCULATE-ALLOWANCE
              PERFORM UPDATE-SUMMARY
              PERFORM WRITE-ALLOWANCE
              PERFORM WRITE-JOURNAL
              PERFORM WRITE-AUDIT
           END-PERFORM.

       ENRICH-CASE.
           MOVE AM-ACCT-NO(IX) TO CLC-ACCT-NO
           READ KZCLCF
              INVALID KEY
                 MOVE SPACES TO AM-CLC-STATUS(IX)
                 MOVE ZERO TO AM-COLL-EST(IX)
              NOT INVALID KEY
                 MOVE CLC-CASE-STATUS TO AM-CLC-STATUS(IX)
                 COMPUTE AM-COLL-EST(IX) ROUNDED =
                    AM-OD-AMT(IX) * WS-COLL-RECOV-RATE
           END-READ
           IF FS-KZCLCF NOT = "00" AND FS-KZCLCF NOT = "23"
              DISPLAY "KZCLCF READ ERROR ACCT=" AM-ACCT-NO(IX)
              DISPLAY "KZCLCF ST=" FS-KZCLCF
              MOVE "Y" TO WS-HARD-ERROR
           END-IF.

       READ-PRIOR-ALLOWANCE.
           MOVE AM-ACCT-NO(IX) TO LA-ACCT-NO
           READ KZLLAF
              INVALID KEY
                 MOVE ZERO TO AM-PRIOR-ALLOW(IX)
              NOT INVALID KEY
                 MOVE LA-ALLOWANCE-AMT TO AM-PRIOR-ALLOW(IX)
           END-READ
           IF FS-KZLLAF NOT = "00" AND FS-KZLLAF NOT = "23"
              DISPLAY "KZLLAF READ ERROR ACCT=" AM-ACCT-NO(IX)
              DISPLAY "KZLLAF ST=" FS-KZLLAF
              MOVE "Y" TO WS-HARD-ERROR
           END-IF.

       CALCULATE-ALLOWANCE.
           MOVE ZERO TO WS-RATE
           MOVE ZERO TO WS-THRESHOLD
           PERFORM FIND-RATE
           MOVE AM-OD-AMT(IX) TO WS-EXPOSURE
           IF WS-EXPOSURE < ZERO
              MOVE ZERO TO WS-EXPOSURE
           END-IF
           IF AM-RSC-STATUS(IX) = "AP"
              MOVE "R" TO AM-TIER(IX)
              COMPUTE AM-ALLOW-AMT(IX) ROUNDED =
                 WS-EXPOSURE * WS-RSC-RATE
              MOVE WS-RSC-RATE TO AM-ALLOW-RATE(IX)
           ELSE
              IF WS-EXPOSURE > WS-THRESHOLD
                 MOVE "S" TO AM-TIER(IX)
                 COMPUTE WS-PV-CASH ROUNDED =
                    AM-RSC-PAYMENT(IX) / (1 + WS-DISC-RATE)
                 COMPUTE AM-ALLOW-AMT(IX) ROUNDED =
                    WS-EXPOSURE - AM-COLL-EST(IX) - WS-PV-CASH
                 IF AM-ALLOW-AMT(IX) < ZERO
                    MOVE ZERO TO AM-ALLOW-AMT(IX)
                 END-IF
                 IF WS-EXPOSURE > ZERO
                    COMPUTE AM-ALLOW-RATE(IX) ROUNDED =
                       AM-ALLOW-AMT(IX) / WS-EXPOSURE
                 ELSE
                    MOVE ZERO TO AM-ALLOW-RATE(IX)
                 END-IF
              ELSE
                 MOVE "G" TO AM-TIER(IX)
                 COMPUTE AM-ALLOW-AMT(IX) ROUNDED =
                    WS-EXPOSURE * WS-RATE
                 MOVE WS-RATE TO AM-ALLOW-RATE(IX)
              END-IF
           END-IF.

       FIND-RATE.
           MOVE ZERO TO WS-RATE
           MOVE ZERO TO WS-THRESHOLD
           PERFORM VARYING RX FROM 1 BY 1
             UNTIL RX > RATE-COUNT OR WS-THRESHOLD > ZERO
              IF RT-PROD-TYPE(RX) = AM-PROD-TYPE(IX)
                 MOVE RT-THRESHOLD(RX) TO WS-THRESHOLD
                 EVALUATE AM-BUCKET(IX)
                    WHEN "B1"
                       MOVE RT-B1-RATE(RX) TO WS-RATE
                    WHEN "B2"
                       MOVE RT-B2-RATE(RX) TO WS-RATE
                    WHEN "B3"
                       MOVE RT-B3-RATE(RX) TO WS-RATE
                    WHEN "B4"
                       MOVE RT-B4-RATE(RX) TO WS-RATE
                    WHEN OTHER
                       DISPLAY "UNDEFINED BUCKET ACCT="
                               AM-ACCT-NO(IX)
                       MOVE "Y" TO WS-HARD-ERROR
                 END-EVALUATE
              END-IF
           END-PERFORM
           IF WS-THRESHOLD = ZERO AND WS-HARD-ERROR = "N"
              DISPLAY "RATE NOT FOUND ACCT=" AM-ACCT-NO(IX)
              MOVE "Y" TO WS-HARD-ERROR
           END-IF.

       UPDATE-SUMMARY.
           ADD 1 TO TOT-COUNT
           ADD AM-OD-AMT(IX) TO TOT-EXPOSURE
           ADD AM-ALLOW-AMT(IX) TO TOT-CURR-ALLOW
           ADD AM-PRIOR-ALLOW(IX) TO TOT-PRIOR-ALLOW
           COMPUTE WS-DELTA =
              AM-ALLOW-AMT(IX) - AM-PRIOR-ALLOW(IX)
           IF WS-DELTA >= ZERO
              ADD WS-DELTA TO TOT-PROVISION
           ELSE
              COMPUTE WS-ABS-AMT = WS-DELTA * -1
              ADD WS-ABS-AMT TO TOT-REVERSAL
           END-IF
           IF AM-NEW-STATUS(IX) = "30"
              ADD AM-OD-AMT(IX) TO TOT-WRITEOFF
           END-IF
           PERFORM UPDATE-PRODUCT-SUMMARY
           PERFORM UPDATE-BUCKET-SUMMARY.

       UPDATE-PRODUCT-SUMMARY.
           MOVE ZERO TO PX
           PERFORM VARYING JX FROM 1 BY 1
             UNTIL JX > 50 OR PX > ZERO
              IF PS-PROD-TYPE(JX) = AM-PROD-TYPE(IX)
                 MOVE JX TO PX
              ELSE
                 IF PS-PROD-TYPE(JX) = SPACES
                    MOVE JX TO PX
                    MOVE AM-PROD-TYPE(IX) TO PS-PROD-TYPE(PX)
                 END-IF
              END-IF
           END-PERFORM
           IF PX > ZERO
              ADD 1 TO PS-COUNT(PX)
              ADD AM-OD-AMT(IX) TO PS-EXPOSURE(PX)
              ADD AM-ALLOW-AMT(IX) TO PS-CURR-ALLOW(PX)
              ADD AM-PRIOR-ALLOW(IX) TO PS-PRIOR-ALLOW(PX)
              IF WS-DELTA >= ZERO
                 ADD WS-DELTA TO PS-PROVISION(PX)
              ELSE
                 COMPUTE WS-ABS-AMT = WS-DELTA * -1
                 ADD WS-ABS-AMT TO PS-REVERSAL(PX)
              END-IF
           END-IF.

       UPDATE-BUCKET-SUMMARY.
           EVALUATE AM-BUCKET(IX)
              WHEN "B1" MOVE 1 TO BX
              WHEN "B2" MOVE 2 TO BX
              WHEN "B3" MOVE 3 TO BX
              WHEN "B4" MOVE 4 TO BX
              WHEN OTHER MOVE 1 TO BX
           END-EVALUATE
           ADD 1 TO BS-COUNT(BX)
           ADD AM-OD-AMT(IX) TO BS-EXPOSURE(BX)
           ADD AM-ALLOW-AMT(IX) TO BS-ALLOWANCE(BX).

       WRITE-ALLOWANCE.
           INITIALIZE KZLLAF-REC
           MOVE AM-ACCT-NO(IX) TO LA-ACCT-NO
           MOVE AM-ALLOW-AMT(IX) TO LA-ALLOWANCE-AMT
           MOVE AM-ALLOW-RATE(IX) TO LA-ALLOWANCE-RATE
           MOVE WS-TODAY TO LA-CALC-DT
           MOVE AM-TIER(IX) TO LA-ALLOWANCE-TIER
           MOVE AM-PRIOR-ALLOW(IX) TO LA-PRIOR-ALLOWANCE-AMT
           WRITE KZLLAF-REC
              INVALID KEY
                 REWRITE KZLLAF-REC
                    INVALID KEY
                       DISPLAY "KZLLAF WRITE ERROR ACCT="
                               AM-ACCT-NO(IX)
                       DISPLAY "KZLLAF ST=" FS-KZLLAF
                       MOVE "Y" TO WS-HARD-ERROR
                 END-REWRITE
           END-WRITE
           IF FS-KZLLAF NOT = "00" AND FS-KZLLAF NOT = "02"
              DISPLAY "KZLLAF WRITE ERROR ACCT=" AM-ACCT-NO(IX)
              DISPLAY "KZLLAF ST=" FS-KZLLAF
              MOVE "Y" TO WS-HARD-ERROR
           END-IF.

       WRITE-JOURNAL.
           COMPUTE WS-DELTA =
              AM-ALLOW-AMT(IX) - AM-PRIOR-ALLOW(IX)
           IF WS-DELTA NOT = ZERO
              MOVE SPACES TO WS-EXP-GL
              MOVE SPACES TO WS-ALLOW-GL
              MOVE ZERO TO WS-CALL-RC
              CALL "KZ531S" USING AM-PROD-TYPE(IX)
                                  WS-EXP-GL
                                  WS-ALLOW-GL
                                  WS-CALL-RC
              END-CALL
              IF WS-CALL-RC NOT = ZERO
                 DISPLAY "KZ531S ERROR PROD=" AM-PROD-TYPE(IX)
                 MOVE "Y" TO WS-HARD-ERROR
              ELSE
                 INITIALIZE KZGLPF-REC
                 MOVE AM-ACCT-NO(IX) TO GP-ACCT-NO
                 MOVE WS-TODAY TO GP-GL-DATE
                 MOVE WS-EXP-GL TO GP-DEBIT-ACCT-CD
                 MOVE WS-ALLOW-GL TO GP-CREDIT-ACCT-CD
                 IF WS-DELTA < ZERO
                    COMPUTE WS-ABS-AMT = WS-DELTA * -1
                    MOVE WS-ALLOW-GL TO GP-DEBIT-ACCT-CD
                    MOVE WS-EXP-GL TO GP-CREDIT-ACCT-CD
                    MOVE WS-ABS-AMT TO GP-JRNL-AMT
                    MOVE "RV" TO GP-JRNL-TYPE
                 ELSE
                    MOVE WS-DELTA TO GP-JRNL-AMT
                    MOVE "PV" TO GP-JRNL-TYPE
                 END-IF
                 MOVE "LLA" TO GP-COST-CENTER-CD
                 WRITE KZGLPF-REC
                 IF FS-KZGLPF NOT = "00"
                    DISPLAY "KZGLPF WRITE ERROR ACCT="
                            AM-ACCT-NO(IX)
                    DISPLAY "KZGLPF ST=" FS-KZGLPF
                    MOVE "Y" TO WS-HARD-ERROR
                 END-IF
              END-IF
           END-IF.

       WRITE-AUDIT.
           INITIALIZE KZAUDF-REC
           MOVE AM-ACCT-NO(IX) TO AUD-ACCT-NO
           MOVE WS-TODAY TO AUD-EVENT-DT
           MOVE "LLA" TO AUD-EVENT-TYPE
           MOVE AM-CURR-STATUS(IX) TO AUD-OLD-STATUS
           MOVE AM-NEW-STATUS(IX) TO AUD-NEW-STATUS
           COMPUTE AUD-CHANGE-AMT =
              AM-ALLOW-AMT(IX) - AM-PRIOR-ALLOW(IX)
           WRITE KZAUDF-REC
           IF FS-KZAUDF NOT = "00"
              DISPLAY "KZAUDF WRITE ERROR ACCT=" AM-ACCT-NO(IX)
              DISPLAY "KZAUDF ST=" FS-KZAUDF
              MOVE "Y" TO WS-HARD-ERROR
           END-IF.

       WRITE-REPORT.
           MOVE ALL "=" TO SYSOUT-REC
           WRITE SYSOUT-REC
           MOVE "KZ580B LOAN LOSS ALLOWANCE REPORT" TO SYSOUT-REC
           WRITE SYSOUT-REC
           MOVE SPACES TO SYSOUT-REC
           STRING "DATE=" WS-TODAY
                  " COUNT=" TOT-COUNT
                  DELIMITED BY SIZE INTO SYSOUT-REC
           WRITE SYSOUT-REC
           MOVE ALL "-" TO SYSOUT-REC
           WRITE SYSOUT-REC
           MOVE "PRODUCT SUMMARY" TO SYSOUT-REC
           WRITE SYSOUT-REC
           PERFORM VARYING PX FROM 1 BY 1 UNTIL PX > 50
              IF PS-PROD-TYPE(PX) NOT = SPACES
                 MOVE PS-CURR-ALLOW(PX) TO WS-REPORT-AMT
                 MOVE SPACES TO SYSOUT-REC
                 STRING "PROD=" PS-PROD-TYPE(PX)
                        " COUNT=" PS-COUNT(PX)
                        " ALLOW=" WS-REPORT-AMT
                        DELIMITED BY SIZE INTO SYSOUT-REC
                 WRITE SYSOUT-REC
                 MOVE PS-PROVISION(PX) TO WS-REPORT-AMT
                 MOVE SPACES TO SYSOUT-REC
                 STRING "  PROVISION=" WS-REPORT-AMT
                        DELIMITED BY SIZE INTO SYSOUT-REC
                 WRITE SYSOUT-REC
                 MOVE PS-REVERSAL(PX) TO WS-REPORT-AMT
                 MOVE SPACES TO SYSOUT-REC
                 STRING "  REVERSAL=" WS-REPORT-AMT
                        DELIMITED BY SIZE INTO SYSOUT-REC
                 WRITE SYSOUT-REC
              END-IF
           END-PERFORM
           MOVE ALL "-" TO SYSOUT-REC
           WRITE SYSOUT-REC
           MOVE "BUCKET SUMMARY" TO SYSOUT-REC
           WRITE SYSOUT-REC
           PERFORM VARYING BX FROM 1 BY 1 UNTIL BX > 4
              MOVE ZERO TO WS-REPORT-RATE
              IF BS-EXPOSURE(BX) > ZERO
                 COMPUTE WS-REPORT-RATE ROUNDED =
                    BS-ALLOWANCE(BX) / BS-EXPOSURE(BX) * 100
              END-IF
              MOVE BS-ALLOWANCE(BX) TO WS-REPORT-AMT
              MOVE SPACES TO SYSOUT-REC
              STRING "BUCKET=" BS-BUCKET(BX)
                     " COUNT=" BS-COUNT(BX)
                     " ALLOW=" WS-REPORT-AMT
                     " RATE=" WS-REPORT-RATE "%"
                     DELIMITED BY SIZE INTO SYSOUT-REC
              WRITE SYSOUT-REC
           END-PERFORM
           MOVE ALL "-" TO SYSOUT-REC
           WRITE SYSOUT-REC
           MOVE TOT-WRITEOFF TO WS-REPORT-AMT
           MOVE SPACES TO SYSOUT-REC
           STRING "WRITEOFF BALANCE=" WS-REPORT-AMT
                  DELIMITED BY SIZE INTO SYSOUT-REC
           WRITE SYSOUT-REC
           MOVE TOT-CURR-ALLOW TO WS-REPORT-AMT
           MOVE SPACES TO SYSOUT-REC
           STRING "CURRENT ALLOWANCE=" WS-REPORT-AMT
                  DELIMITED BY SIZE INTO SYSOUT-REC
           WRITE SYSOUT-REC
           MOVE TOT-PRIOR-ALLOW TO WS-REPORT-AMT
           MOVE SPACES TO SYSOUT-REC
           STRING "PRIOR ALLOWANCE=" WS-REPORT-AMT
                  DELIMITED BY SIZE INTO SYSOUT-REC
           WRITE SYSOUT-REC
           MOVE TOT-PROVISION TO WS-REPORT-AMT
           MOVE SPACES TO SYSOUT-REC
           STRING "TOTAL PROVISION=" WS-REPORT-AMT
                  DELIMITED BY SIZE INTO SYSOUT-REC
           WRITE SYSOUT-REC
           MOVE TOT-REVERSAL TO WS-REPORT-AMT
           MOVE SPACES TO SYSOUT-REC
           STRING "TOTAL REVERSAL=" WS-REPORT-AMT
                  DELIMITED BY SIZE INTO SYSOUT-REC
           WRITE SYSOUT-REC
           MOVE ALL "=" TO SYSOUT-REC
           WRITE SYSOUT-REC.

       CLOSE-FILES.
           CLOSE KZDLRF KZDLQF KZRSCF KZCLCF KZLLAF
                 KZGLPF KZAUDF SYSIN-F SYSOUT-F.
