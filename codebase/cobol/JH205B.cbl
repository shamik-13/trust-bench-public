       IDENTIFICATION DIVISION.
       PROGRAM-ID. JH205B.
      * 変更履歴
      * 版数  年月日(和暦)  担当                    概要
      * 1.00  令和04.04.01  システム部 情報系チーム  新規作成
      * 1.01  令和05.10.16  システム部 情報系チーム  手数料DWH項目追加
      * 1.02  令和06.07.08  システム部 情報系チーム  ロード件数集計見直し
       AUTHOR. TRUST-BATCH.
      ******************************************************************
      * FEE DWH STAGING LOAD
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZFEEHF ASSIGN TO "KZFEEHF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS FS-KZFEEHF.
           SELECT JHCTLKF ASSIGN TO "JHCTLKF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CT-JOB-ID
               FILE STATUS IS FS-JHCTLKF.
           SELECT JHSTGF ASSIGN TO "JHSTGF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS FS-JHSTGF.
           SELECT JHAUDTF ASSIGN TO "JHAUDTF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS FS-JHAUDTF.

       DATA DIVISION.
       FILE SECTION.
       FD  KZFEEHF.
           COPY KZFEEHFC.
       FD  JHCTLKF.
           COPY JHCTLC.
       FD  JHSTGF.
           COPY JHSTGC.
       FD  JHAUDTF.
           COPY JHAUDC.

       WORKING-STORAGE SECTION.
       01  FS-AREA.
           05 FS-KZFEEHF        PIC XX VALUE SPACES.
           05 FS-JHCTLKF        PIC XX VALUE SPACES.
           05 FS-JHSTGF         PIC XX VALUE SPACES.
           05 FS-JHAUDTF        PIC XX VALUE SPACES.

       01  WK-CONTROL.
           05 WK-JOB-ID         PIC X(08) VALUE "JH205B  ".
           05 WK-EOF-SW         PIC X VALUE "N".
              88 WK-EOF         VALUE "Y".
           05 WK-HARD-ERR       PIC X VALUE "N".
              88 WK-ABEND       VALUE "Y".
           05 WK-INPUT-CNT      PIC 9(11) VALUE 0.
           05 WK-OUTPUT-CNT     PIC 9(11) VALUE 0.
           05 WK-SKIP-CNT       PIC 9(11) VALUE 0.
           05 WK-BAD-CNT        PIC 9(11) VALUE 0.
           05 WK-RESTART-POS    PIC 9(11) VALUE 0.
           05 WK-CKPT-INT       PIC 9(05) VALUE 10000.
           05 WK-CKPT-NEXT      PIC 9(11) VALUE 10000.
           05 WK-AUDIT-SEQ      PIC 9(11) VALUE 0.
           05 WK-AMT-TOTAL      PIC S9(15)V99 VALUE 0.
           05 WK-EDIT-STATUS    PIC X(02) VALUE SPACES.
           05 WK-BUSINESS-DT    PIC 9(08) VALUE 0.

       01  WK-DATE-TIME.
           05 WK-DATE-YYYYMMDD  PIC 9(08) VALUE 0.
           05 WK-TIME-HHMMSSCC  PIC 9(08) VALUE 0.
           05 WK-TIMESTAMP      PIC 9(16) VALUE 0.

       01  WK-DATE-CHECK.
           05 WK-YYYY           PIC 9(04) VALUE 0.
           05 WK-MM             PIC 9(02) VALUE 0.
           05 WK-DD             PIC 9(02) VALUE 0.
           05 WK-DATE-OK        PIC X VALUE "N".
              88 WK-VALID-DATE  VALUE "Y".

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE 0 TO RETURN-CODE
           PERFORM 1000-INITIALIZE
           IF NOT WK-ABEND
              PERFORM 2000-PROCESS UNTIL WK-EOF OR WK-ABEND
           END-IF
           PERFORM 9000-FINALIZE
           GOBACK.

       1000-INITIALIZE.
           ACCEPT WK-DATE-YYYYMMDD FROM DATE YYYYMMDD
           ACCEPT WK-TIME-HHMMSSCC FROM TIME
           COMPUTE WK-TIMESTAMP =
               (WK-DATE-YYYYMMDD * 100000000) + WK-TIME-HHMMSSCC

           OPEN INPUT KZFEEHF
           IF FS-KZFEEHF NOT = "00"
              DISPLAY "KZFEEHF OPEN ERR ST=" FS-KZFEEHF
              MOVE "Y" TO WK-HARD-ERR
              MOVE 12 TO RETURN-CODE
              EXIT PARAGRAPH
           END-IF

           OPEN I-O JHCTLKF
           IF FS-JHCTLKF NOT = "00"
              DISPLAY "JHCTLKF OPEN ERR ST=" FS-JHCTLKF
              MOVE "Y" TO WK-HARD-ERR
              MOVE 12 TO RETURN-CODE
              EXIT PARAGRAPH
           END-IF

           OPEN OUTPUT JHSTGF
           IF FS-JHSTGF NOT = "00"
              DISPLAY "JHSTGF OPEN ERR ST=" FS-JHSTGF
              MOVE "Y" TO WK-HARD-ERR
              MOVE 12 TO RETURN-CODE
              EXIT PARAGRAPH
           END-IF

           OPEN OUTPUT JHAUDTF
           IF FS-JHAUDTF NOT = "00"
              DISPLAY "JHAUDTF OPEN ERR ST=" FS-JHAUDTF
              MOVE "Y" TO WK-HARD-ERR
              MOVE 12 TO RETURN-CODE
              EXIT PARAGRAPH
           END-IF

           MOVE WK-JOB-ID TO CT-JOB-ID
           READ JHCTLKF
              INVALID KEY
                 DISPLAY "CTL REC NOT FOUND JOB=" WK-JOB-ID
                 MOVE "Y" TO WK-HARD-ERR
                 MOVE 8 TO RETURN-CODE
                 EXIT PARAGRAPH
           END-READ

           MOVE CT-RESTART-POS TO WK-RESTART-POS
           MOVE CT-BUSINESS-DT TO WK-BUSINESS-DT

           IF CT-STATUS-CD NOT = "R" AND CT-STATUS-CD NOT = "N"
              DISPLAY "CTL STATUS ERR STS=" CT-STATUS-CD
              MOVE "Y" TO WK-HARD-ERR
              MOVE 8 TO RETURN-CODE
              EXIT PARAGRAPH
           END-IF

           MOVE "S" TO CT-STATUS-CD
           MOVE WK-TIMESTAMP TO CT-START-TS
           REWRITE JHCTLKF-REC
           IF FS-JHCTLKF NOT = "00"
              DISPLAY "CTL START UPD ERR ST=" FS-JHCTLKF
              MOVE "Y" TO WK-HARD-ERR
              MOVE 12 TO RETURN-CODE
              EXIT PARAGRAPH
           END-IF

           PERFORM 8100-WRITE-AUDIT-START.

       2000-PROCESS.
           READ KZFEEHF
              AT END
                 MOVE "Y" TO WK-EOF-SW
                 EXIT PARAGRAPH
           END-READ

           IF FS-KZFEEHF NOT = "00"
              DISPLAY "KZFEEHF READ ERR ST=" FS-KZFEEHF
              MOVE "Y" TO WK-HARD-ERR
              MOVE 12 TO RETURN-CODE
              EXIT PARAGRAPH
           END-IF

           ADD 1 TO WK-INPUT-CNT

           IF WK-INPUT-CNT <= WK-RESTART-POS
              ADD 1 TO WK-SKIP-CNT
              EXIT PARAGRAPH
           END-IF

           PERFORM 3000-EDIT-RECORD
           PERFORM 4000-BUILD-STAGE
           PERFORM 5000-WRITE-STAGE

           IF WK-EDIT-STATUS NOT = "00"
              ADD 1 TO WK-BAD-CNT
           END-IF

           IF WK-INPUT-CNT >= WK-CKPT-NEXT
              PERFORM 7000-UPDATE-CHECKPOINT
              ADD WK-CKPT-INT TO WK-CKPT-NEXT
           END-IF.

       3000-EDIT-RECORD.
           MOVE "00" TO WK-EDIT-STATUS

           IF FH-ACCT-NO NOT NUMERIC
              MOVE "11" TO WK-EDIT-STATUS
           ELSE
              IF FH-ACCT-NO = ZERO
                 MOVE "12" TO WK-EDIT-STATUS
              END-IF
           END-IF

           IF WK-EDIT-STATUS = "00"
              PERFORM 3100-CHECK-CYCLE-DATE
              IF NOT WK-VALID-DATE
                 MOVE "21" TO WK-EDIT-STATUS
              END-IF
           END-IF

           IF WK-EDIT-STATUS = "00"
              IF FH-FEE-AMT NOT NUMERIC
                 MOVE "31" TO WK-EDIT-STATUS
              END-IF
           END-IF

           IF WK-EDIT-STATUS = "00"
              IF FH-FEE-YTD NOT NUMERIC
                 MOVE "32" TO WK-EDIT-STATUS
              END-IF
           END-IF

           IF WK-EDIT-STATUS = "00"
              IF FH-FEE-AMT < ZERO AND FH-FEE-YTD > ZERO
                 MOVE "41" TO WK-EDIT-STATUS
              END-IF
           END-IF

           IF WK-EDIT-STATUS = "00"
              IF FH-FEE-AMT > ZERO AND FH-FEE-YTD < ZERO
                 MOVE "42" TO WK-EDIT-STATUS
              END-IF
           END-IF

           IF WK-EDIT-STATUS = "00"
              CONTINUE
           END-IF.

       3100-CHECK-CYCLE-DATE.
           MOVE "N" TO WK-DATE-OK
           IF FH-CYCLE-DT NUMERIC
              MOVE FH-CYCLE-DT(1:4) TO WK-YYYY
              MOVE FH-CYCLE-DT(5:2) TO WK-MM
              MOVE FH-CYCLE-DT(7:2) TO WK-DD
              IF WK-YYYY >= 1990 AND WK-YYYY <= 2099
                 IF WK-MM >= 1 AND WK-MM <= 12
                    IF WK-DD >= 1 AND WK-DD <= 31
                       IF WK-MM = 4 OR WK-MM = 6 OR WK-MM = 9
                          OR WK-MM = 11
                          IF WK-DD <= 30
                             MOVE "Y" TO WK-DATE-OK
                          END-IF
                       ELSE
                          IF WK-MM = 2
                             IF WK-DD <= 29
                                MOVE "Y" TO WK-DATE-OK
                             END-IF
                          ELSE
                             MOVE "Y" TO WK-DATE-OK
                          END-IF
                       END-IF
                    END-IF
                 END-IF
              END-IF
           END-IF.

       4000-BUILD-STAGE.
           INITIALIZE JHSTGF-REC
           MOVE FH-ACCT-NO     TO STG-ACCT-NO
           MOVE FH-CYCLE-DT    TO STG-CYCLE-DT
           MOVE FH-FEE-AMT     TO STG-FEE-AMT
           MOVE FH-FEE-YTD     TO STG-FEE-YTD
           MOVE "KZF"          TO STG-SOURCE-CD
           MOVE WK-TIMESTAMP   TO STG-LOAD-TS
           MOVE WK-EDIT-STATUS TO STG-EDIT-STATUS.

       5000-WRITE-STAGE.
           WRITE JHSTGF-REC
           IF FS-JHSTGF NOT = "00"
              DISPLAY "JHSTGF WRITE ERR ST=" FS-JHSTGF
              MOVE "Y" TO WK-HARD-ERR
              MOVE 12 TO RETURN-CODE
              EXIT PARAGRAPH
           END-IF
           ADD 1 TO WK-OUTPUT-CNT
           IF WK-EDIT-STATUS = "00"
              ADD FH-FEE-AMT TO WK-AMT-TOTAL
           END-IF.

       7000-UPDATE-CHECKPOINT.
           MOVE WK-JOB-ID TO CT-JOB-ID
           READ JHCTLKF
              INVALID KEY
                 DISPLAY "CKPT CTL READ ERR JOB=" WK-JOB-ID
                 MOVE "Y" TO WK-HARD-ERR
                 MOVE 12 TO RETURN-CODE
                 EXIT PARAGRAPH
           END-READ

           MOVE WK-INPUT-CNT  TO CT-RESTART-POS
           MOVE WK-INPUT-CNT  TO CT-INPUT-CNT
           MOVE WK-OUTPUT-CNT TO CT-OUTPUT-CNT
           MOVE "S"           TO CT-STATUS-CD

           REWRITE JHCTLKF-REC
           IF FS-JHCTLKF NOT = "00"
              DISPLAY "CKPT CTL UPD ERR ST=" FS-JHCTLKF
              MOVE "Y" TO WK-HARD-ERR
              MOVE 12 TO RETURN-CODE
           END-IF.

       8100-WRITE-AUDIT-START.
           ADD 1 TO WK-AUDIT-SEQ
           INITIALIZE JHAUDTF-REC
           MOVE WK-AUDIT-SEQ   TO AUD-AUDIT-SEQ
           MOVE WK-JOB-ID      TO AUD-JOB-ID
           MOVE WK-BUSINESS-DT TO AUD-BUSINESS-DT
           MOVE "START"        TO AUD-EVENT-CD
           MOVE "KZFEEHF"      TO AUD-DATASET-ID
           MOVE 0              TO AUD-REC-CNT
           MOVE 0              TO AUD-AMT-TOTAL
           MOVE WK-TIMESTAMP   TO AUD-EVENT-TS

           WRITE JHAUDTF-REC
           IF FS-JHAUDTF NOT = "00"
              DISPLAY "AUD START WRITE ERR ST=" FS-JHAUDTF
              MOVE "Y" TO WK-HARD-ERR
              MOVE 12 TO RETURN-CODE
           END-IF.

       8200-WRITE-AUDIT-END.
           ADD 1 TO WK-AUDIT-SEQ
           INITIALIZE JHAUDTF-REC
           MOVE WK-AUDIT-SEQ   TO AUD-AUDIT-SEQ
           MOVE WK-JOB-ID      TO AUD-JOB-ID
           MOVE WK-BUSINESS-DT TO AUD-BUSINESS-DT

           IF WK-ABEND
              MOVE "ABEND"     TO AUD-EVENT-CD
           ELSE
              MOVE "END"       TO AUD-EVENT-CD
           END-IF

           MOVE "JHSTGF"       TO AUD-DATASET-ID
           MOVE WK-OUTPUT-CNT  TO AUD-REC-CNT
           MOVE WK-AMT-TOTAL   TO AUD-AMT-TOTAL
           MOVE WK-TIMESTAMP   TO AUD-EVENT-TS

           WRITE JHAUDTF-REC
           IF FS-JHAUDTF NOT = "00"
              DISPLAY "AUD END WRITE ERR ST=" FS-JHAUDTF
              MOVE "Y" TO WK-HARD-ERR
              MOVE 12 TO RETURN-CODE
           END-IF.

       9000-FINALIZE.
           ACCEPT WK-DATE-YYYYMMDD FROM DATE YYYYMMDD
           ACCEPT WK-TIME-HHMMSSCC FROM TIME
           COMPUTE WK-TIMESTAMP =
               (WK-DATE-YYYYMMDD * 100000000) + WK-TIME-HHMMSSCC

           IF FS-JHAUDTF = "00"
              PERFORM 8200-WRITE-AUDIT-END
           END-IF

           IF FS-JHCTLKF = "00"
              MOVE WK-JOB-ID TO CT-JOB-ID
              READ JHCTLKF
                 INVALID KEY
                    DISPLAY "END CTL READ ERR JOB=" WK-JOB-ID
                    MOVE "Y" TO WK-HARD-ERR
                    MOVE 12 TO RETURN-CODE
              END-READ

              IF FS-JHCTLKF = "00"
                 MOVE WK-INPUT-CNT  TO CT-RESTART-POS
                 MOVE WK-INPUT-CNT  TO CT-INPUT-CNT
                 MOVE WK-OUTPUT-CNT TO CT-OUTPUT-CNT
                 MOVE WK-TIMESTAMP  TO CT-END-TS

                 IF WK-ABEND
                    MOVE "A" TO CT-STATUS-CD
                 ELSE
                    MOVE "C" TO CT-STATUS-CD
                 END-IF

                 REWRITE JHCTLKF-REC
                 IF FS-JHCTLKF NOT = "00"
                    DISPLAY "END CTL UPD ERR ST=" FS-JHCTLKF
                    MOVE "Y" TO WK-HARD-ERR
                    MOVE 12 TO RETURN-CODE
                 END-IF
              END-IF
           END-IF

           DISPLAY "JH205B INPUT=" WK-INPUT-CNT
           DISPLAY "JH205B SKIP=" WK-SKIP-CNT
           DISPLAY "JH205B OUTPUT=" WK-OUTPUT-CNT
           DISPLAY "JH205B BAD=" WK-BAD-CNT

           IF FS-KZFEEHF NOT = SPACES
              CLOSE KZFEEHF
           END-IF
           IF FS-JHCTLKF NOT = SPACES
              CLOSE JHCTLKF
           END-IF
           IF FS-JHSTGF NOT = SPACES
              CLOSE JHSTGF
           END-IF
           IF FS-JHAUDTF NOT = SPACES
              CLOSE JHAUDTF
           END-IF

           IF WK-ABEND
              IF RETURN-CODE = 0
                 MOVE 12 TO RETURN-CODE
              END-IF
           ELSE
              MOVE 0 TO RETURN-CODE
           END-IF.
