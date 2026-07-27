       IDENTIFICATION DIVISION.
       PROGRAM-ID. CB119B.
       AUTHOR. TRUST-BATCH.
      ******************************************************************
      * 不正検知バッチ.
      ******************************************************************

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CDSALEF ASSIGN TO "CDSALEF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-CDSALEF.
           SELECT CDCAPF ASSIGN TO "CDCAPF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-CDCAPF.
           SELECT CDCARDF ASSIGN TO "CDCARDF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CF-CARD-NO
               FILE STATUS IS FS-CDCARDF.
           SELECT CDMERCF ASSIGN TO "CDMERCF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS MC-MERCHANT-CODE
               FILE STATUS IS FS-CDMERCF.
           SELECT CDSUMF ASSIGN TO "CDSUMF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS SM-SUMMARY-KEY
               FILE STATUS IS FS-CDSUMF.
           SELECT CDFRDF2 ASSIGN TO "CDFRDF2"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS FR-FRAUD-CASE-ID
               FILE STATUS IS FS-CDFRDF2.
           SELECT CDEXCPF ASSIGN TO "CDEXCPF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-CDEXCPF.

       DATA DIVISION.
       FILE SECTION.

       FD  CDSALEF.
       COPY CDSALEFC.

       FD  CDCAPF.
       COPY CDCAPFC.

       FD  CDCARDF.
       COPY CDCARD03.

       FD  CDMERCF.
       COPY CDMERCC.

       FD  CDSUMF.
       COPY CDSUMC.

       FD  CDFRDF2.
       COPY CDFRDF2C.

       FD  CDEXCPF.
       COPY CDEXCPC.

       WORKING-STORAGE SECTION.
       01  FS-CDSALEF                 PIC XX VALUE SPACES.
       01  FS-CDCAPF                  PIC XX VALUE SPACES.
       01  FS-CDCARDF                 PIC XX VALUE SPACES.
       01  FS-CDMERCF                 PIC XX VALUE SPACES.
       01  FS-CDSUMF                  PIC XX VALUE SPACES.
       01  FS-CDFRDF2                 PIC XX VALUE SPACES.
       01  FS-CDEXCPF                 PIC XX VALUE SPACES.

       01  SW-END-CDSALEF             PIC X VALUE "N".
           88  END-CDSALEF            VALUE "Y".
       01  SW-END-CDCAPF              PIC X VALUE "N".
           88  END-CDCAPF             VALUE "Y".
       01  SW-END-CDSUMF              PIC X VALUE "N".
           88  END-CDSUMF             VALUE "Y".

       01  WS-CUR-DATE-TIME           PIC X(21).
       01  WS-BATCH-DT                PIC 9(8).
       01  WS-ABEND-SW                PIC X VALUE "N".
           88  ABEND-ON               VALUE "Y".

       01  WS-RISK-SCORE              PIC 9(4) VALUE 0.
       01  WS-RULE-HIT-CD             PIC X(6) VALUE SPACES.
       01  WS-CASE-STATUS             PIC XX VALUE "OP".
       01  WS-REASON-CD               PIC X(6) VALUE SPACES.
       01  WS-FEE-FLG                 PIC X VALUE "N".
       01  WS-CAP-FOUND               PIC X VALUE "N".
       01  WS-CARD-FOUND              PIC X VALUE "N".
       01  WS-MER-FOUND               PIC X VALUE "N".
       01  WS-SUM-FOUND               PIC X VALUE "N".
       01  WS-SKIP-SALE               PIC X VALUE "N".

       01  WS-FRAUD-SEQ               PIC 9(7) VALUE 0.
       01  WS-EXCEPT-SEQ              PIC 9(7) VALUE 0.
       01  WS-ID-WORK                 PIC X(24) VALUE SPACES.
       01  WS-ID-NUM                  PIC 9(7) VALUE 0.

       01  WS-MER-AVG-AMT             PIC S9(13)V99 VALUE 0.
       01  WS-SALE-AMT-N              PIC S9(13)V99 VALUE 0.
       01  WS-SUM-SALE-AMT-N          PIC S9(13)V99 VALUE 0.
       01  WS-SUM-RETURN-AMT-N        PIC S9(13)V99 VALUE 0.
       01  WS-SUM-SALE-COUNT-N        PIC 9(9) VALUE 0.
       01  WS-RETURN-RATE             PIC 9(3)V99 VALUE 0.
       01  WS-CARD-HIT-COUNT          PIC 9(4) VALUE 0.
       01  WS-FOREIGN-COUNT           PIC 9(4) VALUE 0.
       01  WS-IDX                     PIC 9(5) VALUE 0.
       01  WS-IDX2                    PIC 9(5) VALUE 0.

       01  WS-CAP-MAX                 PIC 9(5) VALUE 05000.
       01  WS-CAP-CNT                 PIC 9(5) VALUE 0.
       01  WS-CAP-TABLE.
           05  WS-CAP-ENTRY OCCURS 5000 TIMES.
               10  T-CAP-SALE-ID      PIC X(20).
               10  T-CAP-CARD-NO      PIC X(20).
               10  T-CAP-BILLED-AMT   PIC S9(13)V99.
               10  T-CAP-FEE-AMT      PIC S9(11)V99.
               10  T-CAP-CURRENCY-CD  PIC X(3).
               10  T-CAP-STATUS       PIC X.
               10  T-CAP-PROGRAM-ID   PIC X(8).

       01  WS-SUM-MAX                 PIC 9(5) VALUE 02000.
       01  WS-SUM-CNT                 PIC 9(5) VALUE 0.
       01  WS-SUM-TABLE.
           05  WS-SUM-ENTRY OCCURS 2000 TIMES.
               10  T-SUM-KEY          PIC X(40).
               10  T-SUM-DT           PIC 9(8).
               10  T-SUM-MERCHANT     PIC X(15).
               10  T-SUM-CURRENCY     PIC X(3).
               10  T-SUM-COUNT        PIC 9(9).
               10  T-SUM-AMT          PIC S9(13)V99.
               10  T-SUM-RETURN       PIC S9(13)V99.

       01  WS-CARD-MAX                PIC 9(5) VALUE 03000.
       01  WS-CARD-CNT                PIC 9(5) VALUE 0.
       01  WS-CARD-TABLE.
           05  WS-CARD-ENTRY OCCURS 3000 TIMES.
               10  T-CARD-NO          PIC X(20).
               10  T-CARD-DT          PIC 9(8).
               10  T-CARD-COUNT       PIC 9(4).
               10  T-CARD-FOREIGN     PIC 9(4).

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE 0 TO RETURN-CODE
           PERFORM 0100-INIT
           IF NOT ABEND-ON
               PERFORM 1000-LOAD-CAPTURE
           END-IF
           IF NOT ABEND-ON
               PERFORM 2000-LOAD-SUMMARY
           END-IF
           IF NOT ABEND-ON
               PERFORM 3000-PROCESS-SALE
           END-IF
           PERFORM 9000-FINAL
           GOBACK.

       0100-INIT.
           MOVE FUNCTION CURRENT-DATE TO WS-CUR-DATE-TIME
           MOVE WS-CUR-DATE-TIME(1:8) TO WS-BATCH-DT

           OPEN INPUT CDCAPF
           IF FS-CDCAPF NOT = "00"
               DISPLAY "CDCAPF オープン失敗 ST=" FS-CDCAPF
               MOVE "Y" TO WS-ABEND-SW
               MOVE 12 TO RETURN-CODE
           END-IF

           OPEN INPUT CDSUMF
           IF FS-CDSUMF NOT = "00"
               DISPLAY "CDSUMF オープン失敗 ST=" FS-CDSUMF
               MOVE "Y" TO WS-ABEND-SW
               MOVE 12 TO RETURN-CODE
           END-IF

           OPEN INPUT CDSALEF
           IF FS-CDSALEF NOT = "00"
               DISPLAY "CDSALEF オープン失敗 ST=" FS-CDSALEF
               MOVE "Y" TO WS-ABEND-SW
               MOVE 12 TO RETURN-CODE
           END-IF

           OPEN INPUT CDCARDF
           IF FS-CDCARDF NOT = "00"
               DISPLAY "CDCARDF オープン失敗 ST=" FS-CDCARDF
               MOVE "Y" TO WS-ABEND-SW
               MOVE 12 TO RETURN-CODE
           END-IF

           OPEN INPUT CDMERCF
           IF FS-CDMERCF NOT = "00"
               DISPLAY "CDMERCF オープン失敗 ST=" FS-CDMERCF
               MOVE "Y" TO WS-ABEND-SW
               MOVE 12 TO RETURN-CODE
           END-IF

           OPEN OUTPUT CDFRDF2
           IF FS-CDFRDF2 NOT = "00"
               DISPLAY "CDFRDF2 オープン失敗 ST=" FS-CDFRDF2
               MOVE "Y" TO WS-ABEND-SW
               MOVE 12 TO RETURN-CODE
           END-IF

           OPEN OUTPUT CDEXCPF
           IF FS-CDEXCPF NOT = "00"
               DISPLAY "CDEXCPF オープン失敗 ST=" FS-CDEXCPF
               MOVE "Y" TO WS-ABEND-SW
               MOVE 12 TO RETURN-CODE
           END-IF.

       1000-LOAD-CAPTURE.
           PERFORM UNTIL END-CDCAPF OR ABEND-ON
               READ CDCAPF
                   AT END
                       SET END-CDCAPF TO TRUE
                   NOT AT END
                       IF FS-CDCAPF = "00"
                           PERFORM 1100-KEEP-CAPTURE
                       ELSE
                           DISPLAY "CDCAPF 読込失敗 ST=" FS-CDCAPF
                           MOVE "Y" TO WS-ABEND-SW
                           MOVE 12 TO RETURN-CODE
                       END-IF
               END-READ
           END-PERFORM.

       1100-KEEP-CAPTURE.
           IF WS-CAP-CNT < WS-CAP-MAX
               ADD 1 TO WS-CAP-CNT
               MOVE BC-SALE-ID TO T-CAP-SALE-ID(WS-CAP-CNT)
               MOVE BC-CARD-NO TO T-CAP-CARD-NO(WS-CAP-CNT)
               MOVE BC-BILLED-AMT TO T-CAP-BILLED-AMT(WS-CAP-CNT)
               MOVE BC-FEE-AMT TO T-CAP-FEE-AMT(WS-CAP-CNT)
               MOVE BC-CURRENCY-CD TO T-CAP-CURRENCY-CD(WS-CAP-CNT)
               MOVE BC-CAP-STATUS TO T-CAP-STATUS(WS-CAP-CNT)
               MOVE BC-PROGRAM-ID TO T-CAP-PROGRAM-ID(WS-CAP-CNT)
           ELSE
               DISPLAY "CDCAPF テーブル溢れ"
               MOVE "Y" TO WS-ABEND-SW
               MOVE 12 TO RETURN-CODE
           END-IF.

       2000-LOAD-SUMMARY.
           PERFORM UNTIL END-CDSUMF OR ABEND-ON
               READ CDSUMF NEXT RECORD
                   AT END
                       SET END-CDSUMF TO TRUE
                   NOT AT END
                       IF FS-CDSUMF = "00"
                           PERFORM 2100-KEEP-SUMMARY
                       ELSE
                           DISPLAY "CDSUMF 読込失敗 ST=" FS-CDSUMF
                           MOVE "Y" TO WS-ABEND-SW
                           MOVE 12 TO RETURN-CODE
                       END-IF
               END-READ
           END-PERFORM.

       2100-KEEP-SUMMARY.
           IF WS-SUM-CNT < WS-SUM-MAX
               ADD 1 TO WS-SUM-CNT
               MOVE SM-SUMMARY-KEY TO T-SUM-KEY(WS-SUM-CNT)
               MOVE SM-SUMMARY-DT TO T-SUM-DT(WS-SUM-CNT)
               MOVE SM-MERCHANT-CODE TO T-SUM-MERCHANT(WS-SUM-CNT)
               MOVE SM-CURRENCY-CD TO T-SUM-CURRENCY(WS-SUM-CNT)
               MOVE SM-SALE-COUNT TO T-SUM-COUNT(WS-SUM-CNT)
               MOVE SM-SALE-AMT TO T-SUM-AMT(WS-SUM-CNT)
               MOVE SM-RETURN-AMT TO T-SUM-RETURN(WS-SUM-CNT)
           ELSE
               DISPLAY "CDSUMF テーブル溢れ"
               MOVE "Y" TO WS-ABEND-SW
               MOVE 12 TO RETURN-CODE
           END-IF.

       3000-PROCESS-SALE.
           PERFORM UNTIL END-CDSALEF OR ABEND-ON
               READ CDSALEF
                   AT END
                       SET END-CDSALEF TO TRUE
                   NOT AT END
                       IF FS-CDSALEF = "00"
                           PERFORM 3100-EVALUATE-SALE
                       ELSE
                           DISPLAY "CDSALEF 読込失敗 ST=" FS-CDSALEF
                           MOVE "Y" TO WS-ABEND-SW
                           MOVE 12 TO RETURN-CODE
                       END-IF
               END-READ
           END-PERFORM.

       3100-EVALUATE-SALE.
           MOVE "N" TO WS-SKIP-SALE
           MOVE ZERO TO WS-RISK-SCORE
           MOVE SPACES TO WS-RULE-HIT-CD
           MOVE SPACES TO WS-REASON-CD
           MOVE "N" TO WS-CAP-FOUND
           MOVE "N" TO WS-CARD-FOUND
           MOVE "N" TO WS-MER-FOUND
           MOVE "N" TO WS-SUM-FOUND
           MOVE "N" TO WS-FEE-FLG
           MOVE SL-SALE-AMT TO WS-SALE-AMT-N

           PERFORM 3200-FIND-CAPTURE
           PERFORM 3300-READ-CARD
           PERFORM 3400-READ-MERCHANT

           IF WS-SKIP-SALE = "N"
               PERFORM 3500-CARD-FREQUENCY
               PERFORM 3600-FIND-SUMMARY
               PERFORM 3700-SCORE-SALE
               IF WS-RISK-SCORE >= 40
                   PERFORM 4000-WRITE-FRAUD
               END-IF
               IF WS-RISK-SCORE >= 80
                   PERFORM 5000-WRITE-EXCEPTION
               END-IF
           END-IF.

       3200-FIND-CAPTURE.
           PERFORM VARYING WS-IDX FROM 1 BY 1
               UNTIL WS-IDX > WS-CAP-CNT OR WS-CAP-FOUND = "Y"
               IF T-CAP-SALE-ID(WS-IDX) = SL-SALE-ID
                   MOVE "Y" TO WS-CAP-FOUND
                   IF T-CAP-STATUS(WS-IDX) = "S"
                       MOVE "Y" TO WS-SKIP-SALE
                   END-IF
                   IF T-CAP-CURRENCY-CD(WS-IDX) NOT = "JPY"
                       IF T-CAP-FEE-AMT(WS-IDX) > ZERO
                           MOVE "Y" TO WS-FEE-FLG
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
           IF WS-CAP-FOUND = "N"
               ADD 10 TO WS-RISK-SCORE
               MOVE "CPMISS" TO WS-RULE-HIT-CD
           END-IF.

       3300-READ-CARD.
           MOVE SL-CARD-NO TO CF-CARD-NO
           READ CDCARDF KEY IS CF-CARD-NO
               INVALID KEY
                   MOVE "N" TO WS-CARD-FOUND
                   ADD 20 TO WS-RISK-SCORE
                   MOVE "CDMISS" TO WS-RULE-HIT-CD
               NOT INVALID KEY
                   MOVE "Y" TO WS-CARD-FOUND
                   IF CF-CARD-STATUS NOT = "01"
                       MOVE "Y" TO WS-SKIP-SALE
                       DISPLAY "カード状態スキップ CARD="
                       DISPLAY SL-CARD-NO
                   END-IF
           END-READ
           IF FS-CDCARDF NOT = "00"
              AND FS-CDCARDF NOT = "23"
               DISPLAY "CDCARDF 読込失敗 ST=" FS-CDCARDF
               MOVE "Y" TO WS-ABEND-SW
               MOVE 12 TO RETURN-CODE
           END-IF.

       3400-READ-MERCHANT.
           MOVE SL-MERCHANT-CODE TO MC-MERCHANT-CODE
           READ CDMERCF KEY IS MC-MERCHANT-CODE
               INVALID KEY
                   MOVE "N" TO WS-MER-FOUND
                   ADD 15 TO WS-RISK-SCORE
                   MOVE "MCMISS" TO WS-RULE-HIT-CD
               NOT INVALID KEY
                   MOVE "Y" TO WS-MER-FOUND
                   IF MC-MERCHANT-STATUS NOT = "01"
                       ADD 25 TO WS-RISK-SCORE
                       MOVE "MCSTAT" TO WS-RULE-HIT-CD
                   END-IF
           END-READ
           IF FS-CDMERCF NOT = "00"
              AND FS-CDMERCF NOT = "23"
               DISPLAY "CDMERCF 読込失敗 ST=" FS-CDMERCF
               MOVE "Y" TO WS-ABEND-SW
               MOVE 12 TO RETURN-CODE
           END-IF.

       3500-CARD-FREQUENCY.
           MOVE ZERO TO WS-CARD-HIT-COUNT
           MOVE ZERO TO WS-FOREIGN-COUNT
           PERFORM VARYING WS-IDX FROM 1 BY 1
               UNTIL WS-IDX > WS-CARD-CNT
               IF T-CARD-NO(WS-IDX) = SL-CARD-NO
                  AND T-CARD-DT(WS-IDX) = SL-SALE-DT
                   ADD 1 TO T-CARD-COUNT(WS-IDX)
                   MOVE T-CARD-COUNT(WS-IDX) TO WS-CARD-HIT-COUNT
                   IF SL-CURRENCY-CD NOT = "JPY"
                       ADD 1 TO T-CARD-FOREIGN(WS-IDX)
                   END-IF
                   MOVE T-CARD-FOREIGN(WS-IDX) TO WS-FOREIGN-COUNT
               END-IF
           END-PERFORM

           IF WS-CARD-HIT-COUNT = ZERO
               IF WS-CARD-CNT < WS-CARD-MAX
                   ADD 1 TO WS-CARD-CNT
                   MOVE SL-CARD-NO TO T-CARD-NO(WS-CARD-CNT)
                   MOVE SL-SALE-DT TO T-CARD-DT(WS-CARD-CNT)
                   MOVE 1 TO T-CARD-COUNT(WS-CARD-CNT)
                   IF SL-CURRENCY-CD NOT = "JPY"
                       MOVE 1 TO T-CARD-FOREIGN(WS-CARD-CNT)
                       MOVE 1 TO WS-FOREIGN-COUNT
                   ELSE
                       MOVE ZERO TO T-CARD-FOREIGN(WS-CARD-CNT)
                   END-IF
                   MOVE 1 TO WS-CARD-HIT-COUNT
               ELSE
                   DISPLAY "カードテーブル溢れ"
                   MOVE "Y" TO WS-ABEND-SW
                   MOVE 12 TO RETURN-CODE
               END-IF
           END-IF.

       3600-FIND-SUMMARY.
           MOVE ZERO TO WS-SUM-SALE-COUNT-N
           MOVE ZERO TO WS-SUM-SALE-AMT-N
           MOVE ZERO TO WS-SUM-RETURN-AMT-N
           MOVE ZERO TO WS-MER-AVG-AMT
           MOVE ZERO TO WS-RETURN-RATE
           PERFORM VARYING WS-IDX2 FROM 1 BY 1
               UNTIL WS-IDX2 > WS-SUM-CNT OR WS-SUM-FOUND = "Y"
               IF T-SUM-MERCHANT(WS-IDX2) = SL-MERCHANT-CODE
                  AND T-SUM-CURRENCY(WS-IDX2) = SL-CURRENCY-CD
                   MOVE "Y" TO WS-SUM-FOUND
                   MOVE T-SUM-COUNT(WS-IDX2)
                       TO WS-SUM-SALE-COUNT-N
                   MOVE T-SUM-AMT(WS-IDX2) TO WS-SUM-SALE-AMT-N
                   MOVE T-SUM-RETURN(WS-IDX2)
                       TO WS-SUM-RETURN-AMT-N
               END-IF
           END-PERFORM

           IF WS-SUM-SALE-COUNT-N > ZERO
               COMPUTE WS-MER-AVG-AMT ROUNDED =
                   WS-SUM-SALE-AMT-N / WS-SUM-SALE-COUNT-N
               IF WS-SUM-SALE-AMT-N > ZERO
                   COMPUTE WS-RETURN-RATE ROUNDED =
                       (WS-SUM-RETURN-AMT-N * 100)
                       / WS-SUM-SALE-AMT-N
               END-IF
           END-IF.

       3700-SCORE-SALE.
           IF WS-CARD-HIT-COUNT >= 5
               ADD 35 TO WS-RISK-SCORE
               MOVE "CRDFRQ" TO WS-RULE-HIT-CD
           END-IF

           IF WS-MER-AVG-AMT > ZERO
              AND WS-SALE-AMT-N > WS-MER-AVG-AMT * 3
               ADD 30 TO WS-RISK-SCORE
               MOVE "AVGDEV" TO WS-RULE-HIT-CD
           END-IF

           IF SL-CURRENCY-CD NOT = "JPY"
               ADD 15 TO WS-RISK-SCORE
               IF WS-FEE-FLG = "Y"
                   ADD 10 TO WS-RISK-SCORE
               END-IF
               IF WS-FOREIGN-COUNT >= 3
                   ADD 25 TO WS-RISK-SCORE
                   MOVE "FORINC" TO WS-RULE-HIT-CD
               END-IF
           END-IF

           IF WS-RETURN-RATE >= 25
              AND WS-CAP-FOUND = "Y"
               ADD 25 TO WS-RISK-SCORE
               MOVE "RTRATE" TO WS-RULE-HIT-CD
           END-IF

           IF WS-RULE-HIT-CD = SPACES
              AND WS-RISK-SCORE > ZERO
               MOVE "GENRUL" TO WS-RULE-HIT-CD
           END-IF.

       4000-WRITE-FRAUD.
           ADD 1 TO WS-FRAUD-SEQ
           MOVE WS-FRAUD-SEQ TO WS-ID-NUM
           INITIALIZE CDFRDF2-REC
           MOVE SPACES TO WS-ID-WORK
           STRING "FR" WS-BATCH-DT WS-ID-NUM
               DELIMITED BY SIZE INTO WS-ID-WORK
           END-STRING
           MOVE WS-ID-WORK TO FR-FRAUD-CASE-ID
           MOVE SL-SALE-ID TO FR-SALE-ID
           MOVE SL-CARD-NO TO FR-CARD-NO
           MOVE SL-MERCHANT-CODE TO FR-MERCHANT-CODE
           MOVE WS-RISK-SCORE TO FR-RISK-SCORE
           MOVE WS-RULE-HIT-CD TO FR-RULE-HIT-CD
           MOVE WS-CASE-STATUS TO FR-CASE-STATUS
           WRITE CDFRDF2-REC
               INVALID KEY
                   DISPLAY "CDFRDF2 重複 ST=" FS-CDFRDF2
                   MOVE "Y" TO WS-ABEND-SW
                   MOVE 12 TO RETURN-CODE
           END-WRITE
           IF FS-CDFRDF2 NOT = "00"
               DISPLAY "CDFRDF2 書込失敗 ST=" FS-CDFRDF2
               MOVE "Y" TO WS-ABEND-SW
               MOVE 12 TO RETURN-CODE
           END-IF.

       5000-WRITE-EXCEPTION.
           ADD 1 TO WS-EXCEPT-SEQ
           MOVE WS-EXCEPT-SEQ TO WS-ID-NUM
           INITIALIZE CDEXCPF-REC
           MOVE SPACES TO WS-ID-WORK
           STRING "EX" WS-BATCH-DT WS-ID-NUM
               DELIMITED BY SIZE INTO WS-ID-WORK
           END-STRING
           MOVE WS-ID-WORK TO EX-EXCEPTION-ID
           MOVE SL-SALE-ID TO EX-SALE-ID
           MOVE SL-CARD-NO TO EX-CARD-NO
           MOVE WS-RULE-HIT-CD TO EX-REASON-CD
           MOVE "CB810B" TO EX-DETECTED-PGM
           MOVE WS-BATCH-DT TO EX-EXCEPTION-DT
           MOVE "00" TO EX-ACTION-STATUS
           WRITE CDEXCPF-REC
           IF FS-CDEXCPF NOT = "00"
               DISPLAY "CDEXCPF 書込失敗 ST=" FS-CDEXCPF
               MOVE "Y" TO WS-ABEND-SW
               MOVE 12 TO RETURN-CODE
           END-IF.

       9000-FINAL.
           CLOSE CDSALEF
           CLOSE CDCAPF
           CLOSE CDCARDF
           CLOSE CDMERCF
           CLOSE CDSUMF
           CLOSE CDFRDF2
           CLOSE CDEXCPF

           IF ABEND-ON
               IF RETURN-CODE = 0
                   MOVE 8 TO RETURN-CODE
               END-IF
               DISPLAY "CB810B 異常終了 RC=" RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
               DISPLAY "CB810B 正常 件数=" WS-FRAUD-SEQ
           END-IF.
