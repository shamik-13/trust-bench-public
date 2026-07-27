       IDENTIFICATION DIVISION.
       PROGRAM-ID. KZ508B.
      * 変更履歴
      * 版数  年月日(和暦)  担当                    概要
      * 1.00  H30.12.20    システム部 勘定系チーム  新規作成
      * 1.01  R02.01.15    システム部 勘定系チーム  法定調書様式変更対応
      * 1.02  R05.10.02    システム部 勘定系チーム  出力対象判定見直し
       AUTHOR. KZ-BATCH.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZTXRF ASSIGN TO "KZTXRF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-ST-KZTXRF.
           SELECT KZNIMF ASSIGN TO "KZNIMF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS NI-ACCT-NO
               FILE STATUS IS WS-ST-KZNIMF.
           SELECT KZNRMF ASSIGN TO "KZNRMF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS NR-ACCT-NO
               FILE STATUS IS WS-ST-KZNRMF.
           SELECT KZPRIF ASSIGN TO "KZPRIF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-ST-KZPRIF.

       DATA DIVISION.
       FILE SECTION.
       FD  KZTXRF.
           COPY KZTXRFC.
       FD  KZNIMF.
           COPY KZNIMFC.
       FD  KZNRMF.
           COPY KZNRMFC.
       FD  KZPRIF.
           COPY KZPRIFC.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05 WS-ST-KZTXRF        PIC XX VALUE SPACES.
           05 WS-ST-KZNIMF        PIC XX VALUE SPACES.
           05 WS-ST-KZNRMF        PIC XX VALUE SPACES.
           05 WS-ST-KZPRIF        PIC XX VALUE SPACES.

       01  WS-CONTROL.
           05 WS-EOF-SW           PIC X VALUE "N".
              88 WS-EOF                 VALUE "Y".
           05 WS-HARD-ERR-SW      PIC X VALUE "N".
              88 WS-HARD-ERR            VALUE "Y".
           05 WS-NI-FOUND-SW      PIC X VALUE "N".
              88 WS-NI-FOUND            VALUE "Y".
           05 WS-NR-FOUND-SW      PIC X VALUE "N".
              88 WS-NR-FOUND            VALUE "Y".
           05 WS-NISA-ACTIVE-SW   PIC X VALUE "N".
              88 WS-NISA-ACTIVE         VALUE "Y".
           05 WS-NR-ACTIVE-SW     PIC X VALUE "N".
              88 WS-NR-ACTIVE           VALUE "Y".

       01  WS-DATE-AREA.
           05 WS-CURRENT-DATE.
              10 WS-CURRENT-YYYYMMDD PIC 9(8).
              10 WS-CURRENT-TIME     PIC 9(8).
              10 WS-CURRENT-DIFF     PIC S9(4).
           05 WS-PAYMENT-DATE      PIC 9(8).

       01  WS-COUNTER.
           05 WS-READ-CNT          PIC 9(9) VALUE 0.
           05 WS-WRITE-CNT         PIC 9(9) VALUE 0.
           05 WS-SKIP-CNT          PIC 9(9) VALUE 0.
           05 WS-ERR-CNT           PIC 9(9) VALUE 0.

       01  WS-JUDGE.
           05 WS-SUBMIT-SW         PIC X VALUE "N".
              88 WS-SUBMIT              VALUE "Y".
           05 WS-THRESHOLD-AMT     PIC S9(13) VALUE 0.
           05 WS-TAX-SUM           PIC S9(13) VALUE 0.
           05 WS-NET-CALC          PIC S9(13) VALUE 0.
           05 WS-VALID-GROSS-SW    PIC X VALUE "N".
              88 WS-VALID-GROSS         VALUE "Y".

       01  WS-CONSTANT.
           05 WC-ACCT-IPPAN        PIC XX VALUE "01".
           05 WC-ACCT-HOJIN        PIC XX VALUE "02".
           05 WC-ACCT-NISA         PIC XX VALUE "03".
           05 WC-NI-STAT-ACTIVE    PIC X  VALUE "1".
           05 WC-NI-STAT-CLOSED    PIC X  VALUE "9".
           05 WC-CERT-RECEIVED     PIC X  VALUE "1".
           05 WC-TH-IPPAN          PIC S9(13) VALUE 100000.
           05 WC-TH-HOJIN          PIC S9(13) VALUE 50000.
           05 WC-TH-NR             PIC S9(13) VALUE 1.
           05 WC-TH-NISA           PIC S9(13) VALUE 9999999999999.

       PROCEDURE DIVISION.
       MAIN-RTN.
           MOVE 0 TO RETURN-CODE
           PERFORM INIT-RTN
           IF NOT WS-HARD-ERR
              PERFORM UNTIL WS-EOF OR WS-HARD-ERR
                 PERFORM READ-TX-RTN
                 IF NOT WS-EOF AND NOT WS-HARD-ERR
                    PERFORM EDIT-TX-RTN
                    IF WS-VALID-GROSS
                       PERFORM READ-NI-RTN
                       IF NOT WS-HARD-ERR
                          PERFORM READ-NR-RTN
                       END-IF
                       IF NOT WS-HARD-ERR
                          PERFORM JUDGE-SUBMIT-RTN
                          IF WS-SUBMIT
                             PERFORM MAKE-PR-RTN
                             PERFORM WRITE-PR-RTN
                          END-IF
                       END-IF
                    ELSE
                       ADD 1 TO WS-ERR-CNT
                    END-IF
                 END-IF
              END-PERFORM
           END-IF
           PERFORM FINAL-RTN
           GOBACK.

       INIT-RTN.
           ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD
           MOVE WS-CURRENT-YYYYMMDD TO WS-PAYMENT-DATE

           OPEN INPUT KZTXRF
           IF WS-ST-KZTXRF NOT = "00"
              DISPLAY "KZTXRF OPEN ERROR ST=" WS-ST-KZTXRF
              SET WS-HARD-ERR TO TRUE
              MOVE 8 TO RETURN-CODE
           END-IF

           IF NOT WS-HARD-ERR
              OPEN INPUT KZNIMF
              IF WS-ST-KZNIMF NOT = "00"
                 DISPLAY "KZNIMF OPEN ERROR ST=" WS-ST-KZNIMF
                 SET WS-HARD-ERR TO TRUE
                 MOVE 8 TO RETURN-CODE
              END-IF
           END-IF

           IF NOT WS-HARD-ERR
              OPEN INPUT KZNRMF
              IF WS-ST-KZNRMF NOT = "00"
                 DISPLAY "KZNRMF OPEN ERROR ST=" WS-ST-KZNRMF
                 SET WS-HARD-ERR TO TRUE
                 MOVE 8 TO RETURN-CODE
              END-IF
           END-IF

           IF NOT WS-HARD-ERR
              OPEN OUTPUT KZPRIF
              IF WS-ST-KZPRIF NOT = "00"
                 DISPLAY "KZPRIF OPEN ERROR ST=" WS-ST-KZPRIF
                 SET WS-HARD-ERR TO TRUE
                 MOVE 8 TO RETURN-CODE
              END-IF
           END-IF.

       READ-TX-RTN.
           READ KZTXRF
              AT END
                 SET WS-EOF TO TRUE
              NOT AT END
                 ADD 1 TO WS-READ-CNT
                 IF WS-ST-KZTXRF NOT = "00"
                    DISPLAY "KZTXRF READ ERROR ST=" WS-ST-KZTXRF
                    SET WS-HARD-ERR TO TRUE
                    MOVE 8 TO RETURN-CODE
                 END-IF
           END-READ.

       EDIT-TX-RTN.
           MOVE "N" TO WS-VALID-GROSS-SW
           COMPUTE WS-TAX-SUM =
                   TR-NATIONAL-TAX-AMT + TR-LOCAL-TAX-AMT

           IF TR-ACCT-NO = SPACES
              DISPLAY "ACCT NO IS BLANK"
           ELSE
              IF TR-ACCT-TYPE NOT = WC-ACCT-IPPAN
                 AND TR-ACCT-TYPE NOT = WC-ACCT-HOJIN
                 AND TR-ACCT-TYPE NOT = WC-ACCT-NISA
                 DISPLAY "BAD ACCT TYPE ACCT=" TR-ACCT-NO
              ELSE
                 IF TR-GROSS-INT-AMT < 0
                    DISPLAY "BAD GROSS AMT ACCT=" TR-ACCT-NO
                 ELSE
                    IF TR-TOTAL-TAX-AMT NOT = WS-TAX-SUM
                       DISPLAY "BAD TAX SUM ACCT=" TR-ACCT-NO
                    ELSE
                       COMPUTE WS-NET-CALC =
                               TR-GROSS-INT-AMT
                             - TR-TOTAL-TAX-AMT
                       IF TR-NET-AMT NOT = WS-NET-CALC
                          DISPLAY "BAD NET AMT ACCT=" TR-ACCT-NO
                       ELSE
                          MOVE "Y" TO WS-VALID-GROSS-SW
                       END-IF
                    END-IF
                 END-IF
              END-IF
           END-IF.

       READ-NI-RTN.
           MOVE "N" TO WS-NI-FOUND-SW
           MOVE "N" TO WS-NISA-ACTIVE-SW
           MOVE TR-ACCT-NO TO NI-ACCT-NO
           READ KZNIMF
              INVALID KEY
                 IF WS-ST-KZNIMF = "23"
                    CONTINUE
                 ELSE
                    DISPLAY "KZNIMF READ ERROR ST=" WS-ST-KZNIMF
                    DISPLAY "ACCT=" TR-ACCT-NO
                    SET WS-HARD-ERR TO TRUE
                    MOVE 8 TO RETURN-CODE
                 END-IF
              NOT INVALID KEY
                 SET WS-NI-FOUND TO TRUE
           END-READ

           IF WS-NI-FOUND
              IF NI-STATUS-CD = WC-NI-STAT-ACTIVE
                 IF TR-ACCT-TYPE = WC-ACCT-NISA
                    IF WS-PAYMENT-DATE >= NI-NISA-APPLY-FROM
                       AND WS-PAYMENT-DATE <= NI-NISA-APPLY-TO
                       MOVE "Y" TO WS-NISA-ACTIVE-SW
                    END-IF
                 END-IF
              ELSE
                 IF NI-STATUS-CD NOT = WC-NI-STAT-CLOSED
                    DISPLAY "BAD ACCT STATUS ACCT=" TR-ACCT-NO
                 END-IF
              END-IF
           END-IF.

       READ-NR-RTN.
           MOVE "N" TO WS-NR-FOUND-SW
           MOVE "N" TO WS-NR-ACTIVE-SW
           MOVE TR-ACCT-NO TO NR-ACCT-NO
           READ KZNRMF
              INVALID KEY
                 IF WS-ST-KZNRMF = "23"
                    CONTINUE
                 ELSE
                    DISPLAY "KZNRMF READ ERROR ST=" WS-ST-KZNRMF
                    DISPLAY "ACCT=" TR-ACCT-NO
                    SET WS-HARD-ERR TO TRUE
                    MOVE 8 TO RETURN-CODE
                 END-IF
              NOT INVALID KEY
                 SET WS-NR-FOUND TO TRUE
           END-READ

           IF WS-NR-FOUND
              IF WS-PAYMENT-DATE >= NR-VALID-FROM
                 AND WS-PAYMENT-DATE <= NR-VALID-TO
                 AND NR-CERT-RECEIVED-FLG = WC-CERT-RECEIVED
                 MOVE "Y" TO WS-NR-ACTIVE-SW
              END-IF
           END-IF.

       JUDGE-SUBMIT-RTN.
           MOVE "N" TO WS-SUBMIT-SW
           MOVE ZERO TO WS-THRESHOLD-AMT

           EVALUATE TRUE
              WHEN WS-NISA-ACTIVE
                 MOVE WC-TH-NISA TO WS-THRESHOLD-AMT
              WHEN WS-NR-ACTIVE
                 MOVE WC-TH-NR TO WS-THRESHOLD-AMT
              WHEN TR-ACCT-TYPE = WC-ACCT-HOJIN
                 MOVE WC-TH-HOJIN TO WS-THRESHOLD-AMT
              WHEN OTHER
                 MOVE WC-TH-IPPAN TO WS-THRESHOLD-AMT
           END-EVALUATE

           IF TR-GROSS-INT-AMT >= WS-THRESHOLD-AMT
              IF WS-NI-FOUND
                 IF NI-STATUS-CD = WC-NI-STAT-ACTIVE
                    MOVE "Y" TO WS-SUBMIT-SW
                 ELSE
                    ADD 1 TO WS-SKIP-CNT
                    DISPLAY "CLOSED ACCT SKIP ACCT=" TR-ACCT-NO
                 END-IF
              ELSE
                 MOVE "Y" TO WS-SUBMIT-SW
              END-IF
           ELSE
              ADD 1 TO WS-SKIP-CNT
           END-IF.

       MAKE-PR-RTN.
           INITIALIZE KZPRIF-REC
           MOVE TR-ACCT-NO           TO PR-ACCT-NO
           MOVE WS-PAYMENT-DATE      TO PR-PAYMENT-DATE
           MOVE TR-GROSS-INT-AMT     TO PR-GROSS-INT-AMT
           MOVE TR-NATIONAL-TAX-AMT  TO PR-NATIONAL-TAX-AMT
           MOVE TR-LOCAL-TAX-AMT     TO PR-LOCAL-TAX-AMT

           STRING "JUKYUSHA" DELIMITED BY SIZE
                  TR-ACCT-NO  DELIMITED BY SIZE
             INTO PR-PAYEE-NAME-KANA
           END-STRING

           IF WS-NR-ACTIVE
              STRING "NR" DELIMITED BY SIZE
                     NR-RESIDENCE-COUNTRY-CD DELIMITED BY SIZE
                INTO PR-PAYEE-ADDR-CD
              END-STRING
              MOVE "3" TO PR-STATEMENT-CLASS
           ELSE
              IF TR-ACCT-TYPE = WC-ACCT-HOJIN
                 MOVE "JP13000" TO PR-PAYEE-ADDR-CD
                 MOVE "2" TO PR-STATEMENT-CLASS
              ELSE
                 MOVE "JP00000" TO PR-PAYEE-ADDR-CD
                 MOVE "1" TO PR-STATEMENT-CLASS
              END-IF
           END-IF.

       WRITE-PR-RTN.
           WRITE KZPRIF-REC
           IF WS-ST-KZPRIF NOT = "00"
              DISPLAY "KZPRIF WRITE ERROR ST=" WS-ST-KZPRIF
              DISPLAY "ACCT=" TR-ACCT-NO
              SET WS-HARD-ERR TO TRUE
              MOVE 8 TO RETURN-CODE
           ELSE
              ADD 1 TO WS-WRITE-CNT
           END-IF.

       FINAL-RTN.
           IF WS-ST-KZTXRF NOT = SPACES
              CLOSE KZTXRF
              IF WS-ST-KZTXRF NOT = "00"
                 DISPLAY "KZTXRF CLOSE ERROR ST=" WS-ST-KZTXRF
                 SET WS-HARD-ERR TO TRUE
                 MOVE 8 TO RETURN-CODE
              END-IF
           END-IF

           IF WS-ST-KZNIMF NOT = SPACES
              CLOSE KZNIMF
              IF WS-ST-KZNIMF NOT = "00"
                 DISPLAY "KZNIMF CLOSE ERROR ST=" WS-ST-KZNIMF
                 SET WS-HARD-ERR TO TRUE
                 MOVE 8 TO RETURN-CODE
              END-IF
           END-IF

           IF WS-ST-KZNRMF NOT = SPACES
              CLOSE KZNRMF
              IF WS-ST-KZNRMF NOT = "00"
                 DISPLAY "KZNRMF CLOSE ERROR ST=" WS-ST-KZNRMF
                 SET WS-HARD-ERR TO TRUE
                 MOVE 8 TO RETURN-CODE
              END-IF
           END-IF

           IF WS-ST-KZPRIF NOT = SPACES
              CLOSE KZPRIF
              IF WS-ST-KZPRIF NOT = "00"
                 DISPLAY "KZPRIF CLOSE ERROR ST=" WS-ST-KZPRIF
                 SET WS-HARD-ERR TO TRUE
                 MOVE 8 TO RETURN-CODE
              END-IF
           END-IF

           DISPLAY "KZ508B READ CNT=" WS-READ-CNT
           DISPLAY "KZ508B WRITE CNT=" WS-WRITE-CNT
           DISPLAY "KZ508B SKIP CNT=" WS-SKIP-CNT
           DISPLAY "KZ508B ERROR CNT=" WS-ERR-CNT

           IF WS-HARD-ERR
              DISPLAY "KZ508B ABEND"
              MOVE 8 TO RETURN-CODE
           ELSE
              DISPLAY "KZ508B NORMAL END"
              MOVE 0 TO RETURN-CODE
           END-IF.
