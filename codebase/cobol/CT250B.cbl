       IDENTIFICATION DIVISION.
       PROGRAM-ID. CT250B.
       AUTHOR. MFG-SHIKIN-BATCH.
      *================================================================*
      *  指図照会索引作成バッチ                                        *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CCINSF ASSIGN TO "CCINSF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS IN-INS-ID
               FILE STATUS IS FS-CCINSF.

           SELECT CCFCTF ASSIGN TO "CCFCTF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS FS-CCFCTF.

           SELECT CCVALF ASSIGN TO "CCVALF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS FS-CCVALF.

           SELECT CCCHGF ASSIGN TO "CCCHGF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS FS-CCCHGF.

           SELECT CCRPTF ASSIGN TO "CCRPTF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS FS-CCRPTF.

       DATA DIVISION.
       FILE SECTION.
       FD  CCINSF.
           COPY CCINSC.

       FD  CCFCTF.
           COPY CCFCTFC.

       FD  CCVALF.
           COPY CCVALFC.

       FD  CCCHGF.
           COPY CCCHGC.

       FD  CCRPTF.
           COPY CCRPTC.

       WORKING-STORAGE SECTION.
       01  FS-AREA.
           05 FS-CCINSF              PIC XX VALUE SPACE.
           05 FS-CCFCTF              PIC XX VALUE SPACE.
           05 FS-CCVALF              PIC XX VALUE SPACE.
           05 FS-CCCHGF              PIC XX VALUE SPACE.
           05 FS-CCRPTF              PIC XX VALUE SPACE.

       01  SW-AREA.
           05 SW-END-INS             PIC X VALUE "N".
              88 END-INS                  VALUE "Y".
           05 SW-END-FCT             PIC X VALUE "N".
              88 END-FCT                  VALUE "Y".
           05 SW-END-VAL             PIC X VALUE "N".
              88 END-VAL                  VALUE "Y".
           05 SW-END-CHG             PIC X VALUE "N".
              88 END-CHG                  VALUE "Y".
           05 SW-FCT-FOUND           PIC X VALUE "N".
              88 FCT-FOUND                VALUE "Y".
           05 SW-VAL-FOUND           PIC X VALUE "N".
              88 VAL-FOUND                VALUE "Y".
           05 SW-CHG-FOUND           PIC X VALUE "N".
              88 CHG-FOUND                VALUE "Y".
           05 SW-HARD-ERR            PIC X VALUE "N".
              88 HARD-ERR                 VALUE "Y".

       01  CONST-AREA.
           05 CT-PGM-ID              PIC X(08) VALUE "CT250B".
           05 CT-RPT-KBN             PIC X(02) VALUE "25".
           05 CT-STATUS-KAKUTEI      PIC X(02) VALUE "01".
           05 CT-STATUS-HORYU        PIC X(02) VALUE "08".
           05 CT-STATUS-TORIKESHI    PIC X(02) VALUE "09".

       01  DATE-AREA.
           05 WS-CURRENT-DATE        PIC X(21).
           05 WS-BASE-DT             PIC 9(08).
           05 WS-DATE-NUM            PIC 9(08).
           05 WS-DATE-INT            PIC S9(09) COMP-5.
           05 WS-DATE-OK             PIC X VALUE "N".
              88 DATE-OK                  VALUE "Y".

       01  COUNT-AREA.
           05 CNT-INS-READ           PIC 9(09) VALUE ZERO.
           05 CNT-FCT-READ           PIC 9(09) VALUE ZERO.
           05 CNT-VAL-READ           PIC 9(09) VALUE ZERO.
           05 CNT-CHG-READ           PIC 9(09) VALUE ZERO.
           05 CNT-RPT-WRITE          PIC 9(09) VALUE ZERO.
           05 CNT-SKIP               PIC 9(09) VALUE ZERO.
           05 CNT-FCT-TBL            PIC 9(05) VALUE ZERO.
           05 CNT-VAL-TBL            PIC 9(05) VALUE ZERO.
           05 CNT-CHG-TBL            PIC 9(05) VALUE ZERO.
           05 IDX                    PIC 9(05) VALUE ZERO.
           05 WK-LINE-NO             PIC 9(07) VALUE ZERO.

       01  EDIT-AREA.
           05 ED-INSTR-AMT           PIC ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.
           05 ED-CONC-AMT            PIC ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.
           05 ED-STATUS              PIC X(12).
           05 ED-CHG-KBN             PIC X(10).
           05 ED-VALUE-DT            PIC X(08).
           05 ED-CHANGE-DT           PIC X(08).
           05 ED-TEXT                PIC X(120).
           05 ED-REASON              PIC X(60).

       01  CUR-AREA.
           05 CUR-FCT-ID             PIC X(20).
           05 CUR-VALUE-DT           PIC 9(08).
           05 CUR-CHANGE-DT          PIC 9(08).
           05 CUR-CHANGE-KBN         PIC X(02).
           05 CUR-FCT-STATUS         PIC X(02).
           05 CUR-CONC-AMT           PIC S9(15)V99 COMP-3.

       01  FCT-TABLE.
           05 FCT-ENTRY OCCURS 5000 TIMES.
              10 TB-FCT-ID           PIC X(20).
              10 TB-TRIGGER-DT       PIC 9(08).
              10 TB-CONC-AMT         PIC S9(15)V99 COMP-3.
              10 TB-FCT-STATUS       PIC X(02).

       01  VAL-TABLE.
           05 VAL-ENTRY OCCURS 5000 TIMES.
              10 TB-VAL-FCT-ID       PIC X(20).
              10 TB-VALUE-DT         PIC 9(08).
              10 TB-VAL-STATUS       PIC X(02).

       01  CHG-TABLE.
           05 CHG-ENTRY OCCURS 5000 TIMES.
              10 TB-CHG-FCT-ID       PIC X(20).
              10 TB-CHANGE-DT        PIC 9(08).
              10 TB-CHANGE-KBN       PIC X(02).
              10 TB-BEFORE-STATUS    PIC X(02).
              10 TB-AFTER-STATUS     PIC X(02).

       PROCEDURE DIVISION.
       MAIN-SECTION.
           PERFORM 0000-INIT
           IF NOT HARD-ERR
               PERFORM 1000-LOAD-FCT
               PERFORM 1100-LOAD-VAL
               PERFORM 1200-LOAD-CHG
           END-IF
           IF NOT HARD-ERR
               PERFORM 2000-PROCESS-INS UNTIL END-INS OR HARD-ERR
           END-IF
           PERFORM 9000-FINAL
           GOBACK
           .

       0000-INIT.
           MOVE 0 TO RETURN-CODE
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE
           MOVE WS-CURRENT-DATE(1:8) TO WS-BASE-DT

           OPEN INPUT CCINSF
           IF FS-CCINSF NOT = "00"
               DISPLAY "CCINSF OPEN ERROR ST=" FS-CCINSF
               SET HARD-ERR TO TRUE
               MOVE 8 TO RETURN-CODE
           END-IF

           OPEN INPUT CCFCTF
           IF FS-CCFCTF NOT = "00"
               DISPLAY "CCFCTF OPEN ERROR ST=" FS-CCFCTF
               SET HARD-ERR TO TRUE
               MOVE 8 TO RETURN-CODE
           END-IF

           OPEN INPUT CCVALF
           IF FS-CCVALF NOT = "00"
               DISPLAY "CCVALF OPEN ERROR ST=" FS-CCVALF
               SET HARD-ERR TO TRUE
               MOVE 8 TO RETURN-CODE
           END-IF

           OPEN INPUT CCCHGF
           IF FS-CCCHGF NOT = "00"
               DISPLAY "CCCHGF OPEN ERROR ST=" FS-CCCHGF
               SET HARD-ERR TO TRUE
               MOVE 8 TO RETURN-CODE
           END-IF

           OPEN OUTPUT CCRPTF
           IF FS-CCRPTF NOT = "00"
               DISPLAY "CCRPTF OPEN ERROR ST=" FS-CCRPTF
               SET HARD-ERR TO TRUE
               MOVE 8 TO RETURN-CODE
           END-IF

           IF NOT HARD-ERR
               PERFORM 2100-READ-INS
           END-IF
           .

       1000-LOAD-FCT.
           PERFORM UNTIL END-FCT OR HARD-ERR
               READ CCFCTF
                   AT END
                       SET END-FCT TO TRUE
                   NOT AT END
                       IF FS-CCFCTF = "00"
                           ADD 1 TO CNT-FCT-READ
                           IF CNT-FCT-TBL < 5000
                               ADD 1 TO CNT-FCT-TBL
                               MOVE FC-FCT-ID
                                 TO TB-FCT-ID(CNT-FCT-TBL)
                               MOVE FC-TRIGGER-DT
                                 TO TB-TRIGGER-DT(CNT-FCT-TBL)
                               MOVE FC-CONC-AMT
                                 TO TB-CONC-AMT(CNT-FCT-TBL)
                               MOVE FC-FCT-STATUS-KBN
                                 TO TB-FCT-STATUS(CNT-FCT-TBL)
                           ELSE
                               DISPLAY "CCFCTF TABLE FULL"
                               SET HARD-ERR TO TRUE
                               MOVE 12 TO RETURN-CODE
                           END-IF
                       ELSE
                           DISPLAY "CCFCTF READ ERROR ST=" FS-CCFCTF
                           SET HARD-ERR TO TRUE
                           MOVE 8 TO RETURN-CODE
                       END-IF
               END-READ
           END-PERFORM
           .

       1100-LOAD-VAL.
           PERFORM UNTIL END-VAL OR HARD-ERR
               READ CCVALF
                   AT END
                       SET END-VAL TO TRUE
                   NOT AT END
                       IF FS-CCVALF = "00"
                           ADD 1 TO CNT-VAL-READ
                           PERFORM 1110-VALIDATE-VALUE-DT
                           IF DATE-OK
                               IF CNT-VAL-TBL < 5000
                                   ADD 1 TO CNT-VAL-TBL
                                   MOVE VL-FCT-ID
                                     TO TB-VAL-FCT-ID(CNT-VAL-TBL)
                                   MOVE VL-VALUE-DT
                                     TO TB-VALUE-DT(CNT-VAL-TBL)
                                   MOVE VL-VAL-STATUS-KBN
                                     TO TB-VAL-STATUS(CNT-VAL-TBL)
                               ELSE
                                   DISPLAY "CCVALF TABLE FULL"
                                   SET HARD-ERR TO TRUE
                                   MOVE 12 TO RETURN-CODE
                               END-IF
                           ELSE
                               DISPLAY "CCVALF BAD DATE FCT=" VL-FCT-ID
                           END-IF
                       ELSE
                           DISPLAY "CCVALF READ ERROR ST=" FS-CCVALF
                           SET HARD-ERR TO TRUE
                           MOVE 8 TO RETURN-CODE
                       END-IF
               END-READ
           END-PERFORM
           .

       1110-VALIDATE-VALUE-DT.
           MOVE "N" TO WS-DATE-OK
           IF VL-VALUE-DT IS NUMERIC
               MOVE VL-VALUE-DT TO WS-DATE-NUM
               PERFORM 7000-CHECK-DATE
           END-IF
           .

       1200-LOAD-CHG.
           PERFORM UNTIL END-CHG OR HARD-ERR
               READ CCCHGF
                   AT END
                       SET END-CHG TO TRUE
                   NOT AT END
                       IF FS-CCCHGF = "00"
                           ADD 1 TO CNT-CHG-READ
                           IF CNT-CHG-TBL < 5000
                               ADD 1 TO CNT-CHG-TBL
                               MOVE CH-FCT-ID
                                 TO TB-CHG-FCT-ID(CNT-CHG-TBL)
                               MOVE CH-CHANGE-DT
                                 TO TB-CHANGE-DT(CNT-CHG-TBL)
                               MOVE CH-CHANGE-KBN
                                 TO TB-CHANGE-KBN(CNT-CHG-TBL)
                               MOVE CH-BEFORE-STATUS-KBN
                                 TO TB-BEFORE-STATUS(CNT-CHG-TBL)
                               MOVE CH-AFTER-STATUS-KBN
                                 TO TB-AFTER-STATUS(CNT-CHG-TBL)
                           ELSE
                               DISPLAY "CCCHGF TABLE FULL"
                               SET HARD-ERR TO TRUE
                               MOVE 12 TO RETURN-CODE
                           END-IF
                       ELSE
                           DISPLAY "CCCHGF READ ERROR ST=" FS-CCCHGF
                           SET HARD-ERR TO TRUE
                           MOVE 8 TO RETURN-CODE
                       END-IF
               END-READ
           END-PERFORM
           .

       2000-PROCESS-INS.
           ADD 1 TO CNT-INS-READ
           PERFORM 3000-VALIDATE-INS
           IF ED-REASON = SPACE
               MOVE IN-FCT-ID TO CUR-FCT-ID
               PERFORM 3100-FIND-FCT
               PERFORM 3200-FIND-VAL
               PERFORM 3300-FIND-CHG
               IF FCT-FOUND
                   PERFORM 4000-EDIT-REPORT
                   PERFORM 5000-WRITE-REPORT
               ELSE
                   ADD 1 TO CNT-SKIP
                   DISPLAY "FCT NOT FOUND INS=" IN-INS-ID
                           " FCT=" IN-FCT-ID
               END-IF
           ELSE
               ADD 1 TO CNT-SKIP
               DISPLAY ED-REASON " INS=" IN-INS-ID
           END-IF
           PERFORM 2100-READ-INS
           .

       2100-READ-INS.
           READ CCINSF
               AT END
                   SET END-INS TO TRUE
               NOT AT END
                   IF FS-CCINSF NOT = "00"
                       DISPLAY "CCINSF READ ERROR ST=" FS-CCINSF
                       SET HARD-ERR TO TRUE
                       MOVE 8 TO RETURN-CODE
                   END-IF
           END-READ
           .

       3000-VALIDATE-INS.
           MOVE SPACE TO ED-REASON
           IF IN-INS-ID = SPACE
               MOVE "INS-ID BLANK" TO ED-REASON
           END-IF
           IF ED-REASON = SPACE AND IN-FCT-ID = SPACE
               MOVE "FCT-ID BLANK" TO ED-REASON
           END-IF
           IF ED-REASON = SPACE
               IF IN-INSTR-STATUS-KBN NOT = CT-STATUS-KAKUTEI
                  AND IN-INSTR-STATUS-KBN NOT = CT-STATUS-HORYU
                  AND IN-INSTR-STATUS-KBN NOT = CT-STATUS-TORIKESHI
                   MOVE "BAD INS STATUS" TO ED-REASON
               END-IF
           END-IF
           IF ED-REASON = SPACE
               IF IN-RECV-DT IS NUMERIC
                   MOVE IN-RECV-DT TO WS-DATE-NUM
                   PERFORM 7000-CHECK-DATE
                   IF NOT DATE-OK
                       MOVE "BAD RECV DATE" TO ED-REASON
                   END-IF
               ELSE
                   MOVE "RECV DATE NONNUM" TO ED-REASON
               END-IF
           END-IF
           .

       3100-FIND-FCT.
           MOVE "N" TO SW-FCT-FOUND
           MOVE ZERO TO CUR-CONC-AMT
           MOVE SPACE TO CUR-FCT-STATUS
           PERFORM VARYING IDX FROM 1 BY 1
             UNTIL IDX > CNT-FCT-TBL
               IF TB-FCT-ID(IDX) = CUR-FCT-ID
                   MOVE TB-CONC-AMT(IDX) TO CUR-CONC-AMT
                   MOVE TB-FCT-STATUS(IDX) TO CUR-FCT-STATUS
                   MOVE "Y" TO SW-FCT-FOUND
                   MOVE CNT-FCT-TBL TO IDX
               END-IF
           END-PERFORM
           .

       3200-FIND-VAL.
           MOVE "N" TO SW-VAL-FOUND
           MOVE ZERO TO CUR-VALUE-DT
           PERFORM VARYING IDX FROM 1 BY 1
             UNTIL IDX > CNT-VAL-TBL
               IF TB-VAL-FCT-ID(IDX) = CUR-FCT-ID
                   IF NOT VAL-FOUND
                      OR TB-VALUE-DT(IDX) > CUR-VALUE-DT
                       MOVE TB-VALUE-DT(IDX) TO CUR-VALUE-DT
                       MOVE "Y" TO SW-VAL-FOUND
                   END-IF
               END-IF
           END-PERFORM
           .

       3300-FIND-CHG.
           MOVE "N" TO SW-CHG-FOUND
           MOVE ZERO TO CUR-CHANGE-DT
           MOVE SPACE TO CUR-CHANGE-KBN
           PERFORM VARYING IDX FROM 1 BY 1
             UNTIL IDX > CNT-CHG-TBL
               IF TB-CHG-FCT-ID(IDX) = CUR-FCT-ID
                   IF NOT CHG-FOUND
                      OR TB-CHANGE-DT(IDX) > CUR-CHANGE-DT
                       MOVE TB-CHANGE-DT(IDX) TO CUR-CHANGE-DT
                       MOVE TB-CHANGE-KBN(IDX) TO CUR-CHANGE-KBN
                       MOVE "Y" TO SW-CHG-FOUND
                   END-IF
               END-IF
           END-PERFORM
           .

       4000-EDIT-REPORT.
           EVALUATE CUR-FCT-STATUS
               WHEN CT-STATUS-KAKUTEI
                   MOVE "KAKUTEI" TO ED-STATUS
               WHEN CT-STATUS-HORYU
                   MOVE "HORYU" TO ED-STATUS
               WHEN CT-STATUS-TORIKESHI
                   MOVE "TORIKESHI" TO ED-STATUS
               WHEN OTHER
                   MOVE "BAD-STATUS" TO ED-STATUS
           END-EVALUATE

           EVALUATE CUR-CHANGE-KBN
               WHEN "01"
                   MOVE "TOROKU" TO ED-CHG-KBN
               WHEN "02"
                   MOVE "TEISEI" TO ED-CHG-KBN
               WHEN "03"
                   MOVE "TORIKESHI" TO ED-CHG-KBN
               WHEN SPACE
                   MOVE "NO-HIST" TO ED-CHG-KBN
               WHEN OTHER
                   MOVE "BAD-KBN" TO ED-CHG-KBN
           END-EVALUATE

           MOVE IN-INSTR-AMT TO ED-INSTR-AMT
           MOVE CUR-CONC-AMT TO ED-CONC-AMT

           IF VAL-FOUND
               MOVE CUR-VALUE-DT TO ED-VALUE-DT
           ELSE
               MOVE "00000000" TO ED-VALUE-DT
           END-IF

           IF CHG-FOUND
               MOVE CUR-CHANGE-DT TO ED-CHANGE-DT
           ELSE
               MOVE "00000000" TO ED-CHANGE-DT
           END-IF

           MOVE SPACE TO ED-TEXT
           STRING
               "INS="          DELIMITED BY SIZE
               IN-INS-ID       DELIMITED BY SPACE
               " FCT="         DELIMITED BY SIZE
               IN-FCT-ID       DELIMITED BY SPACE
               " ST="          DELIMITED BY SIZE
               ED-STATUS       DELIMITED BY SPACE
               " VALUE-DT="    DELIMITED BY SIZE
               ED-VALUE-DT     DELIMITED BY SIZE
               " INS-AMT="     DELIMITED BY SIZE
               ED-INSTR-AMT    DELIMITED BY SIZE
               " CONC-AMT="    DELIMITED BY SIZE
               ED-CONC-AMT     DELIMITED BY SIZE
               " CHG-DT="      DELIMITED BY SIZE
               ED-CHANGE-DT    DELIMITED BY SIZE
               " CHG="         DELIMITED BY SIZE
               ED-CHG-KBN      DELIMITED BY SPACE
               INTO ED-TEXT
           END-STRING
           .

       5000-WRITE-REPORT.
           ADD 1 TO WK-LINE-NO
           INITIALIZE CCRPTF-REC
           MOVE CT-PGM-ID TO RP-REPORT-ID
           MOVE WS-BASE-DT TO RP-BASE-DT
           MOVE CT-RPT-KBN TO RP-REPORT-KBN
           MOVE WK-LINE-NO TO RP-LINE-NO
           MOVE ED-TEXT TO RP-REPORT-TEXT
           WRITE CCRPTF-REC
           IF FS-CCRPTF = "00"
               ADD 1 TO CNT-RPT-WRITE
           ELSE
               DISPLAY "CCRPTF WRITE ERROR ST=" FS-CCRPTF
               SET HARD-ERR TO TRUE
               MOVE 8 TO RETURN-CODE
           END-IF
           .

       7000-CHECK-DATE.
           MOVE "N" TO WS-DATE-OK
           IF WS-DATE-NUM >= 19000101
              AND WS-DATE-NUM <= 20991231
               COMPUTE WS-DATE-INT =
                   FUNCTION INTEGER-OF-DATE(WS-DATE-NUM)
               IF FUNCTION DATE-OF-INTEGER(WS-DATE-INT)
                  = WS-DATE-NUM
                   MOVE "Y" TO WS-DATE-OK
               END-IF
           END-IF
           .

       9000-FINAL.
           IF FS-CCINSF NOT = SPACE
               CLOSE CCINSF
           END-IF
           IF FS-CCFCTF NOT = SPACE
               CLOSE CCFCTF
           END-IF
           IF FS-CCVALF NOT = SPACE
               CLOSE CCVALF
           END-IF
           IF FS-CCCHGF NOT = SPACE
               CLOSE CCCHGF
           END-IF
           IF FS-CCRPTF NOT = SPACE
               CLOSE CCRPTF
           END-IF

           DISPLAY "CT250B COUNT INS=" CNT-INS-READ
                   " FCT=" CNT-FCT-READ
                   " VAL=" CNT-VAL-READ
                   " CHG=" CNT-CHG-READ
                   " RPT=" CNT-RPT-WRITE
                   " SKIP=" CNT-SKIP

           IF HARD-ERR
               IF RETURN-CODE = 0
                   MOVE 8 TO RETURN-CODE
               END-IF
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF
           .
