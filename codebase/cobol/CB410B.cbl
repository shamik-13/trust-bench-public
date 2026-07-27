       IDENTIFICATION DIVISION.
       PROGRAM-ID. CB410B.
       AUTHOR. TRUST-BATCH.
      *================================================================*
      * 加盟店精算計算                                                 *
      * 確定済み売上を加盟店単位に集約し精算明細を作成する。           *
      *================================================================*

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CDCAPF
               ASSIGN TO "CDCAPF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-CDCAPF.

           SELECT CDMERCF
               ASSIGN TO "CDMERCF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS MC-MERCHANT-CODE
               FILE STATUS IS FS-CDMERCF.

           SELECT CDRTNF
               ASSIGN TO "CDRTNF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS RT-RETURN-ID
               FILE STATUS IS FS-CDRTNF.

           SELECT CDCBKPF
               ASSIGN TO "CDCBKPF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CBK-CHARGEBACK-ID
               FILE STATUS IS FS-CDCBKPF.

           SELECT CDSETLF
               ASSIGN TO "CDSETLF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-CDSETLF.

           SELECT CDEXCPF
               ASSIGN TO "CDEXCPF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-CDEXCPF.

       DATA DIVISION.
       FILE SECTION.
       FD  CDCAPF.
           COPY CDCAPFC.

       FD  CDMERCF.
           COPY CDMERCC.

       FD  CDRTNF.
           COPY CDRTNC.

       FD  CDCBKPF.
           COPY CDCBKPC.

       FD  CDSETLF.
           COPY CDSETLC.

       FD  CDEXCPF.
           COPY CDEXCPC.

       WORKING-STORAGE SECTION.
       01  FS-AREA.
           05 FS-CDCAPF              PIC XX VALUE SPACES.
           05 FS-CDMERCF             PIC XX VALUE SPACES.
           05 FS-CDRTNF              PIC XX VALUE SPACES.
           05 FS-CDCBKPF             PIC XX VALUE SPACES.
           05 FS-CDSETLF             PIC XX VALUE SPACES.
           05 FS-CDEXCPF             PIC XX VALUE SPACES.

       01  SW-AREA.
           05 SW-EOF-CAP             PIC X VALUE "N".
              88 EOF-CAP                   VALUE "Y".
           05 SW-EOF-RTN             PIC X VALUE "N".
              88 EOF-RTN                   VALUE "Y".
           05 SW-EOF-CBK             PIC X VALUE "N".
              88 EOF-CBK                   VALUE "Y".
           05 SW-HARD-ERROR          PIC X VALUE "N".
              88 HARD-ERROR                VALUE "Y".
           05 SW-GROUP-OPEN          PIC X VALUE "N".
              88 GROUP-OPEN                VALUE "Y".

       01  CONST-AREA.
           05 CN-PGM-ID              PIC X(08) VALUE "CB410B".
           05 CN-CAP-FIXED           PIC X VALUE "C".
           05 CN-MERCHANT-ACTIVE     PIC X VALUE "1".
           05 CN-CBK-PENDING         PIC X VALUE "P".
           05 CN-RTN-APPROVED        PIC X VALUE "A".
           05 CN-ST-NORMAL           PIC X VALUE "1".
           05 CN-EX-UNSET-PLAN       PIC X(04) VALUE "E101".
           05 CN-EX-STOP-MERCHANT    PIC X(04) VALUE "E102".
           05 CN-EX-NO-MERCHANT      PIC X(04) VALUE "E103".
           05 CN-ACT-WAIT            PIC X VALUE "0".

       01  DATE-AREA.
           05 WS-CURRENT-DATE.
              10 WS-CUR-YYYY         PIC 9(04).
              10 WS-CUR-MM           PIC 9(02).
              10 WS-CUR-DD           PIC 9(02).
           05 WS-SETTLE-DT           PIC 9(08).

       01  KEY-AREA.
           05 WS-MERCHANT-CODE       PIC X(10) VALUE SPACES.
           05 WS-PREV-MERCHANT       PIC X(10) VALUE SPACES.
           05 WS-CURRENCY-CD         PIC X(03) VALUE SPACES.
           05 WS-PREV-CURRENCY       PIC X(03) VALUE SPACES.
           05 WS-SALE-ID             PIC X(20) VALUE SPACES.
           05 WS-SETTLEMENT-ID       PIC X(24) VALUE SPACES.
           05 WS-EXCEPTION-ID        PIC X(24) VALUE SPACES.

       01  AMOUNT-AREA.
           05 WS-SALE-AMT            PIC S9(13)V99 VALUE ZERO.
           05 WS-RETURN-AMT          PIC S9(13)V99 VALUE ZERO.
           05 WS-CBK-AMT             PIC S9(13)V99 VALUE ZERO.
           05 WS-ADJ-AMT             PIC S9(13)V99 VALUE ZERO.
           05 WS-NET-AMT             PIC S9(13)V99 VALUE ZERO.
           05 WS-GRP-GROSS-AMT       PIC S9(15)V99 VALUE ZERO.
           05 WS-GRP-ADJ-AMT         PIC S9(15)V99 VALUE ZERO.
           05 WS-GRP-NET-AMT         PIC S9(15)V99 VALUE ZERO.

       01  COUNT-AREA.
           05 WS-CAP-READ-CNT        PIC 9(09) VALUE ZERO.
           05 WS-SET-WRITE-CNT       PIC 9(09) VALUE ZERO.
           05 WS-EXC-WRITE-CNT       PIC 9(09) VALUE ZERO.
           05 WS-SEQ-NO              PIC 9(09) VALUE ZERO.
           05 WS-EXC-SEQ-NO          PIC 9(09) VALUE ZERO.

       PROCEDURE DIVISION.
       MAIN-SECTION.
           PERFORM 0000-INIT
           PERFORM 1000-MAIN UNTIL EOF-CAP OR HARD-ERROR
           IF NOT HARD-ERROR
              PERFORM 3000-FLUSH-GROUP
           END-IF
           PERFORM 9000-FINAL
           GOBACK.

       0000-INIT.
           MOVE 0 TO RETURN-CODE
           ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD
           COMPUTE WS-SETTLE-DT =
                   WS-CUR-YYYY * 10000
                 + WS-CUR-MM   * 100
                 + WS-CUR-DD

           OPEN INPUT  CDCAPF
                INPUT  CDMERCF
                INPUT  CDRTNF
                INPUT  CDCBKPF
                OUTPUT CDSETLF
                OUTPUT CDEXCPF

           IF FS-CDCAPF NOT = "00"
              DISPLAY "CDCAPF OPEN ST=" FS-CDCAPF
              SET HARD-ERROR TO TRUE
           END-IF
           IF FS-CDMERCF NOT = "00"
              DISPLAY "CDMERCF OPEN ST=" FS-CDMERCF
              SET HARD-ERROR TO TRUE
           END-IF
           IF FS-CDRTNF NOT = "00"
              DISPLAY "CDRTNF OPEN ST=" FS-CDRTNF
              SET HARD-ERROR TO TRUE
           END-IF
           IF FS-CDCBKPF NOT = "00"
              DISPLAY "CDCBKPF OPEN ST=" FS-CDCBKPF
              SET HARD-ERROR TO TRUE
           END-IF
           IF FS-CDSETLF NOT = "00"
              DISPLAY "CDSETLF OPEN ST=" FS-CDSETLF
              SET HARD-ERROR TO TRUE
           END-IF
           IF FS-CDEXCPF NOT = "00"
              DISPLAY "CDEXCPF OPEN ST=" FS-CDEXCPF
              SET HARD-ERROR TO TRUE
           END-IF

           IF HARD-ERROR
              MOVE 12 TO RETURN-CODE
           ELSE
              PERFORM 1100-READ-CAP
           END-IF.

       1000-MAIN.
           IF BC-CAP-STATUS = CN-CAP-FIXED
              PERFORM 1200-PROCESS-CAPTURE
           END-IF
           IF NOT HARD-ERROR
              PERFORM 1100-READ-CAP
           END-IF.

       1100-READ-CAP.
           READ CDCAPF
              AT END
                 SET EOF-CAP TO TRUE
              NOT AT END
                 ADD 1 TO WS-CAP-READ-CNT
           END-READ
           IF FS-CDCAPF NOT = "00" AND FS-CDCAPF NOT = "10"
              DISPLAY "CDCAPF READ ST=" FS-CDCAPF
              SET HARD-ERROR TO TRUE
              MOVE 12 TO RETURN-CODE
           END-IF.

       1200-PROCESS-CAPTURE.
           MOVE BC-SALE-ID           TO WS-SALE-ID
           MOVE BC-SALE-ID(1:10)     TO WS-MERCHANT-CODE
           MOVE BC-CURRENCY-CD       TO WS-CURRENCY-CD
           MOVE WS-MERCHANT-CODE     TO MC-MERCHANT-CODE

           READ CDMERCF KEY IS MC-MERCHANT-CODE
              INVALID KEY
                 PERFORM 4100-WRITE-EXCEPTION-NOMERC
              NOT INVALID KEY
                 PERFORM 1300-VALIDATE-MERCHANT
           END-READ
           IF FS-CDMERCF NOT = "00" AND FS-CDMERCF NOT = "23"
              DISPLAY "CDMERCF READ ST=" FS-CDMERCF
              SET HARD-ERROR TO TRUE
              MOVE 12 TO RETURN-CODE
           END-IF.

       1300-VALIDATE-MERCHANT.
           IF MC-MERCHANT-STATUS NOT = CN-MERCHANT-ACTIVE
              PERFORM 4200-WRITE-EXCEPTION-STOP
           ELSE
              IF MC-FEE-PLAN-CD = SPACES
                 PERFORM 4300-WRITE-EXCEPTION-NOPLAN
              ELSE
                 PERFORM 1400-ACCUMULATE-SALE
              END-IF
           END-IF.

       1400-ACCUMULATE-SALE.
           IF GROUP-OPEN
              IF WS-MERCHANT-CODE NOT = WS-PREV-MERCHANT
                 OR WS-CURRENCY-CD NOT = WS-PREV-CURRENCY
                 PERFORM 3000-FLUSH-GROUP
              END-IF
           END-IF

           IF NOT GROUP-OPEN
              MOVE WS-MERCHANT-CODE TO WS-PREV-MERCHANT
              MOVE WS-CURRENCY-CD   TO WS-PREV-CURRENCY
              SET GROUP-OPEN TO TRUE
           END-IF

           MOVE BC-BILLED-AMT TO WS-SALE-AMT
           PERFORM 2100-CALC-RETURN
           IF NOT HARD-ERROR
              PERFORM 2200-CALC-CHARGEBACK
           END-IF

           IF NOT HARD-ERROR
              COMPUTE WS-ADJ-AMT = WS-RETURN-AMT + WS-CBK-AMT
              COMPUTE WS-NET-AMT = WS-SALE-AMT - WS-ADJ-AMT

              ADD WS-SALE-AMT TO WS-GRP-GROSS-AMT
              ADD WS-ADJ-AMT  TO WS-GRP-ADJ-AMT
              ADD WS-NET-AMT  TO WS-GRP-NET-AMT
           END-IF.

       2100-CALC-RETURN.
           MOVE ZERO TO WS-RETURN-AMT
           MOVE "N" TO SW-EOF-RTN

           START CDRTNF KEY IS NOT LESS THAN RT-RETURN-ID
              INVALID KEY
                 SET EOF-RTN TO TRUE
           END-START
           IF FS-CDRTNF NOT = "00" AND FS-CDRTNF NOT = "23"
              DISPLAY "CDRTNF START ST=" FS-CDRTNF
              SET HARD-ERROR TO TRUE
              MOVE 12 TO RETURN-CODE
           END-IF

           PERFORM UNTIL EOF-RTN OR HARD-ERROR
              READ CDRTNF NEXT RECORD
                 AT END
                    SET EOF-RTN TO TRUE
                 NOT AT END
                    IF RT-SALE-ID = WS-SALE-ID
                       AND RT-APPROVAL-STATUS = CN-RTN-APPROVED
                       ADD RT-RETURN-AMT TO WS-RETURN-AMT
                    END-IF
              END-READ
              IF FS-CDRTNF NOT = "00" AND FS-CDRTNF NOT = "10"
                 DISPLAY "CDRTNF READ ST=" FS-CDRTNF
                 SET HARD-ERROR TO TRUE
                 MOVE 12 TO RETURN-CODE
              END-IF
           END-PERFORM.

       2200-CALC-CHARGEBACK.
           MOVE ZERO TO WS-CBK-AMT
           MOVE "N" TO SW-EOF-CBK

           START CDCBKPF KEY IS NOT LESS THAN CBK-CHARGEBACK-ID
              INVALID KEY
                 SET EOF-CBK TO TRUE
           END-START
           IF FS-CDCBKPF NOT = "00" AND FS-CDCBKPF NOT = "23"
              DISPLAY "CDCBKPF START ST=" FS-CDCBKPF
              SET HARD-ERROR TO TRUE
              MOVE 12 TO RETURN-CODE
           END-IF

           PERFORM UNTIL EOF-CBK OR HARD-ERROR
              READ CDCBKPF NEXT RECORD
                 AT END
                    SET EOF-CBK TO TRUE
                 NOT AT END
                    IF CBK-SALE-ID = WS-SALE-ID
                       AND CBK-MERCHANT-CODE = WS-MERCHANT-CODE
                       AND CBK-CASE-STATUS = CN-CBK-PENDING
                       ADD CBK-CLAIM-AMT TO WS-CBK-AMT
                    END-IF
              END-READ
              IF FS-CDCBKPF NOT = "00" AND FS-CDCBKPF NOT = "10"
                 DISPLAY "CDCBKPF READ ST=" FS-CDCBKPF
                 SET HARD-ERROR TO TRUE
                 MOVE 12 TO RETURN-CODE
              END-IF
           END-PERFORM.

       3000-FLUSH-GROUP.
           IF GROUP-OPEN
              ADD 1 TO WS-SEQ-NO
              MOVE SPACES TO WS-SETTLEMENT-ID
              STRING CN-PGM-ID        DELIMITED BY SIZE
                     WS-SETTLE-DT     DELIMITED BY SIZE
                     WS-SEQ-NO        DELIMITED BY SIZE
                     INTO WS-SETTLEMENT-ID
              END-STRING
              INITIALIZE CDSETLF-REC
              MOVE WS-SETTLEMENT-ID  TO ST-SETTLEMENT-ID
              MOVE WS-PREV-MERCHANT  TO ST-MERCHANT-CODE
              MOVE WS-SETTLE-DT      TO ST-SETTLE-DT
              MOVE WS-GRP-GROSS-AMT  TO ST-GROSS-AMT
              MOVE WS-GRP-NET-AMT    TO ST-NET-AMT
              MOVE WS-GRP-ADJ-AMT    TO ST-ADJ-AMT
              MOVE CN-ST-NORMAL      TO ST-SETTLE-STATUS
              WRITE CDSETLF-REC
              IF FS-CDSETLF NOT = "00"
                 DISPLAY "CDSETLF WRITE ST=" FS-CDSETLF
                 SET HARD-ERROR TO TRUE
                 MOVE 12 TO RETURN-CODE
              ELSE
                 ADD 1 TO WS-SET-WRITE-CNT
              END-IF
              MOVE ZERO TO WS-GRP-GROSS-AMT
                           WS-GRP-ADJ-AMT
                           WS-GRP-NET-AMT
              MOVE SPACES TO WS-PREV-MERCHANT
                             WS-PREV-CURRENCY
              MOVE "N" TO SW-GROUP-OPEN
           END-IF.

       4100-WRITE-EXCEPTION-NOMERC.
           PERFORM 4900-BUILD-EXCEPTION-ID
           INITIALIZE CDEXCPF-REC
           MOVE WS-EXCEPTION-ID      TO EX-EXCEPTION-ID
           MOVE BC-SALE-ID           TO EX-SALE-ID
           MOVE BC-CARD-NO           TO EX-CARD-NO
           MOVE CN-EX-NO-MERCHANT    TO EX-REASON-CD
           MOVE CN-PGM-ID            TO EX-DETECTED-PGM
           MOVE WS-SETTLE-DT         TO EX-EXCEPTION-DT
           MOVE CN-ACT-WAIT          TO EX-ACTION-STATUS
           PERFORM 4950-WRITE-EXCEPTION.

       4200-WRITE-EXCEPTION-STOP.
           PERFORM 4900-BUILD-EXCEPTION-ID
           INITIALIZE CDEXCPF-REC
           MOVE WS-EXCEPTION-ID      TO EX-EXCEPTION-ID
           MOVE BC-SALE-ID           TO EX-SALE-ID
           MOVE BC-CARD-NO           TO EX-CARD-NO
           MOVE CN-EX-STOP-MERCHANT  TO EX-REASON-CD
           MOVE CN-PGM-ID            TO EX-DETECTED-PGM
           MOVE WS-SETTLE-DT         TO EX-EXCEPTION-DT
           MOVE CN-ACT-WAIT          TO EX-ACTION-STATUS
           PERFORM 4950-WRITE-EXCEPTION.

       4300-WRITE-EXCEPTION-NOPLAN.
           PERFORM 4900-BUILD-EXCEPTION-ID
           INITIALIZE CDEXCPF-REC
           MOVE WS-EXCEPTION-ID      TO EX-EXCEPTION-ID
           MOVE BC-SALE-ID           TO EX-SALE-ID
           MOVE BC-CARD-NO           TO EX-CARD-NO
           MOVE CN-EX-UNSET-PLAN     TO EX-REASON-CD
           MOVE CN-PGM-ID            TO EX-DETECTED-PGM
           MOVE WS-SETTLE-DT         TO EX-EXCEPTION-DT
           MOVE CN-ACT-WAIT          TO EX-ACTION-STATUS
           PERFORM 4950-WRITE-EXCEPTION.

       4900-BUILD-EXCEPTION-ID.
           ADD 1 TO WS-EXC-SEQ-NO
           MOVE SPACES TO WS-EXCEPTION-ID
           STRING CN-PGM-ID        DELIMITED BY SIZE
                  WS-SETTLE-DT     DELIMITED BY SIZE
                  WS-EXC-SEQ-NO    DELIMITED BY SIZE
                  INTO WS-EXCEPTION-ID
           END-STRING.

       4950-WRITE-EXCEPTION.
           WRITE CDEXCPF-REC
           IF FS-CDEXCPF NOT = "00"
              DISPLAY "CDEXCPF WRITE ST=" FS-CDEXCPF
              SET HARD-ERROR TO TRUE
              MOVE 12 TO RETURN-CODE
           ELSE
              ADD 1 TO WS-EXC-WRITE-CNT
           END-IF.

       9000-FINAL.
           IF HARD-ERROR
              DISPLAY "CB410B ABEND"
              DISPLAY "CAP READ=" WS-CAP-READ-CNT
              IF RETURN-CODE = 0
                 MOVE 8 TO RETURN-CODE
              END-IF
           ELSE
              DISPLAY "CB410B NORMAL END"
              DISPLAY "CAP READ=" WS-CAP-READ-CNT
              DISPLAY "SET WRITE=" WS-SET-WRITE-CNT
              DISPLAY "EXC WRITE=" WS-EXC-WRITE-CNT
              MOVE 0 TO RETURN-CODE
           END-IF

           CLOSE CDCAPF
                 CDMERCF
                 CDRTNF
                 CDCBKPF
                 CDSETLF
                 CDEXCPF.
