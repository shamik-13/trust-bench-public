       IDENTIFICATION DIVISION.
       PROGRAM-ID. JH100B.
       AUTHOR. JHBT.
      *================================================================*
      * Monthly revenue aggregation batch.                             *
      * Reads JHDWHF for target month, enriches by account dimension,  *
      * calls JH315S validation, aggregates accepted records, and       *
      * writes audit records for logical monthly replacement.           *
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT JHDWHF
               ASSIGN       TO "JHDWHF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS  IS FS-JHDWHF.

           SELECT JHACDMF
               ASSIGN       TO "JHACDMF"
               ORGANIZATION IS INDEXED
               ACCESS MODE  IS DYNAMIC
               RECORD KEY   IS ACD-ACCT-NO
               FILE STATUS  IS FS-JHACDMF.

           SELECT JHCTLKF
               ASSIGN       TO "JHCTLKF"
               ORGANIZATION IS INDEXED
               ACCESS MODE  IS DYNAMIC
               RECORD KEY   IS CT-JOB-ID
               FILE STATUS  IS FS-JHCTLKF.

           SELECT JHMONRF
               ASSIGN       TO "JHMONRF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS  IS FS-JHMONRF.

           SELECT JHAUDTF
               ASSIGN       TO "JHAUDTF"
               ORGANIZATION IS INDEXED
               ACCESS MODE  IS DYNAMIC
               RECORD KEY   IS AUD-AUDIT-SEQ
               FILE STATUS  IS FS-JHAUDTF.

       DATA DIVISION.
       FILE SECTION.
       FD  JHDWHF.
           COPY JHDWHFC.

       FD  JHACDMF.
           COPY JHACDC.

       FD  JHCTLKF.
           COPY JHCTLC.

       FD  JHMONRF.
           COPY JHMONC.

       FD  JHAUDTF.
           COPY JHAUDC.

       WORKING-STORAGE SECTION.
       01  FS-AREA.
           05 FS-JHDWHF             PIC X(02) VALUE SPACE.
           05 FS-JHACDMF            PIC X(02) VALUE SPACE.
           05 FS-JHCTLKF            PIC X(02) VALUE SPACE.
           05 FS-JHMONRF            PIC X(02) VALUE SPACE.
           05 FS-JHAUDTF            PIC X(02) VALUE SPACE.

       01  SW-AREA.
           05 SW-END-JHDW           PIC X(01) VALUE "N".
              88 END-JHDW                     VALUE "Y".
           05 SW-ACD-FOUND          PIC X(01) VALUE "N".
              88 ACD-FOUND                    VALUE "Y".
           05 SW-CACHE-HIT          PIC X(01) VALUE "N".
              88 CACHE-HIT                    VALUE "Y".
           05 SW-AGG-FOUND          PIC X(01) VALUE "N".
              88 AGG-FOUND                    VALUE "Y".
           05 SW-HARD-ERR           PIC X(01) VALUE "N".
              88 HARD-ERR                     VALUE "Y".

       01  CTL-AREA.
           05 WS-JOB-ID             PIC X(08) VALUE "JH330B".
           05 WS-THRESH-JOB-ID      PIC X(08) VALUE "JH330BTH".
           05 WS-BUSINESS-DT        PIC 9(08) VALUE ZERO.
           05 WS-TARGET-YYYYMM      PIC 9(06) VALUE ZERO.
           05 WS-RUN-SEQ            PIC 9(05) VALUE ZERO.
           05 WS-CURRENT-TS         PIC 9(14) VALUE ZERO.
           05 WS-DATE8              PIC 9(08) VALUE ZERO.
           05 WS-TIME6              PIC 9(06) VALUE ZERO.
           05 WS-AUDIT-SEQ          PIC 9(12) VALUE ZERO.
           05 WS-SPILL-LIMIT        PIC 9(04) VALUE 4500.
           05 WS-SPILL-CNT          PIC 9(07) VALUE ZERO.
           05 WS-MAX-AGG            PIC 9(04) VALUE 5000.
           05 WS-MAX-CACHE          PIC 9(03) VALUE 200.
           05 WS-CACHE-POS          PIC 9(03) VALUE ZERO.

       01  COUNTER-AREA.
           05 CNT-INPUT             PIC 9(11) VALUE ZERO.
           05 CNT-STAGED            PIC 9(11) VALUE ZERO.
           05 CNT-REJECT            PIC 9(11) VALUE ZERO.
           05 CNT-AGG-OUT           PIC 9(11) VALUE ZERO.
           05 CNT-COMP-FAIL         PIC 9(11) VALUE ZERO.
           05 CNT-CLOSED            PIC 9(11) VALUE ZERO.
           05 CNT-ADJUST            PIC 9(11) VALUE ZERO.
           05 SUM-FEE-AMT           PIC S9(15)V99 COMP-3 VALUE ZERO.
           05 SUM-FEE-YTD           PIC S9(15)V99 COMP-3 VALUE ZERO.

       01  WORK-KEY-AREA.
           05 WK-YYYYMM             PIC 9(06) VALUE ZERO.
           05 WK-PRODUCT-CD         PIC X(06) VALUE SPACE.
           05 WK-BRANCH-CD          PIC X(04) VALUE SPACE.
           05 WK-EXCEPTION-LIMIT    PIC S9(13)V99 COMP-3 VALUE ZERO.

       01  CACHE-TABLE.
           05 CACHE-ENTRY OCCURS 200 TIMES.
              10 C-ACCT-NO          PIC X(16) VALUE SPACE.
              10 C-PRODUCT-CD       PIC X(06) VALUE SPACE.
              10 C-BRANCH-CD        PIC X(04) VALUE SPACE.
              10 C-STATUS-CD        PIC X(01) VALUE SPACE.
              10 C-CLOSE-DT         PIC 9(08) VALUE ZERO.
              10 C-LAST-CHG-TS      PIC 9(14) VALUE ZERO.

       01  AGG-TABLE.
           05 AGG-ENTRY OCCURS 5000 TIMES.
              10 A-USED             PIC X(01) VALUE "N".
              10 A-YYYYMM           PIC 9(06) VALUE ZERO.
              10 A-PRODUCT-CD       PIC X(06) VALUE SPACE.
              10 A-BRANCH-CD        PIC X(04) VALUE SPACE.
              10 A-FEE-AMT          PIC S9(15)V99 COMP-3 VALUE ZERO.
              10 A-FEE-YTD          PIC S9(15)V99 COMP-3 VALUE ZERO.
              10 A-ACCT-CNT         PIC 9(11) VALUE ZERO.
              10 A-ADJUST-CNT       PIC 9(11) VALUE ZERO.

       01  SUBSCRIPT-AREA.
           05 IX                    PIC 9(04) COMP VALUE ZERO.
           05 CX                    PIC 9(03) COMP VALUE ZERO.
           05 FREE-IX               PIC 9(04) COMP VALUE ZERO.

       01  MESSAGE-AREA.
           05 MSG-ST                PIC X(02) VALUE SPACE.

       01  JH315S-PARM.
           05 JH315S-IN-AREA        PIC X(512) VALUE SPACE.
           05 JH315S-STATUS         PIC X(02)  VALUE SPACE.

       PROCEDURE DIVISION.
       MAIN-RTN.
           PERFORM 1000-INIT
           IF NOT HARD-ERR
              PERFORM 2000-MAIN UNTIL END-JHDW OR HARD-ERR
           END-IF
           IF NOT HARD-ERR
              PERFORM 5000-FLUSH-AGG
              PERFORM 8000-WRITE-END-AUDIT
           END-IF
           PERFORM 9000-CLOSE
           IF HARD-ERR
              MOVE 8 TO RETURN-CODE
           ELSE
              MOVE 0 TO RETURN-CODE
           END-IF
           GOBACK.

       1000-INIT.
           PERFORM 1100-GET-TIMESTAMP
           OPEN INPUT  JHDWHF
                INPUT  JHACDMF
                INPUT  JHCTLKF
                OUTPUT JHMONRF
                I-O    JHAUDTF
           IF FS-JHDWHF NOT = "00"
              MOVE FS-JHDWHF TO MSG-ST
              DISPLAY "JHDWHF OPEN ERROR ST=" MSG-ST
              SET HARD-ERR TO TRUE
           END-IF
           IF FS-JHACDMF NOT = "00"
              MOVE FS-JHACDMF TO MSG-ST
              DISPLAY "JHACDMF OPEN ERROR ST=" MSG-ST
              SET HARD-ERR TO TRUE
           END-IF
           IF FS-JHCTLKF NOT = "00"
              MOVE FS-JHCTLKF TO MSG-ST
              DISPLAY "JHCTLKF OPEN ERROR ST=" MSG-ST
              SET HARD-ERR TO TRUE
           END-IF
           IF FS-JHMONRF NOT = "00"
              MOVE FS-JHMONRF TO MSG-ST
              DISPLAY "JHMONRF OPEN ERROR ST=" MSG-ST
              SET HARD-ERR TO TRUE
           END-IF
           IF FS-JHAUDTF NOT = "00"
              MOVE FS-JHAUDTF TO MSG-ST
              DISPLAY "JHAUDTF OPEN ERROR ST=" MSG-ST
              SET HARD-ERR TO TRUE
           END-IF
           IF NOT HARD-ERR
              PERFORM 1200-READ-CONTROL
              PERFORM 8100-WRITE-START-AUDIT
              PERFORM 2100-READ-JHDW
           END-IF.

       1100-GET-TIMESTAMP.
           ACCEPT WS-DATE8 FROM DATE YYYYMMDD
           ACCEPT WS-TIME6 FROM TIME
           COMPUTE WS-CURRENT-TS = WS-DATE8 * 1000000 + WS-TIME6.

       1200-READ-CONTROL.
           MOVE WS-JOB-ID TO CT-JOB-ID
           READ JHCTLKF KEY IS CT-JOB-ID
              INVALID KEY
                 MOVE FS-JHCTLKF TO MSG-ST
                 DISPLAY "JHCTLKF CONTROL MISSING ST=" MSG-ST
                 SET HARD-ERR TO TRUE
              NOT INVALID KEY
                 MOVE CT-BUSINESS-DT TO WS-BUSINESS-DT
                 MOVE CT-RUN-SEQ     TO WS-RUN-SEQ
                 COMPUTE WS-TARGET-YYYYMM =
                    FUNCTION INTEGER(CT-BUSINESS-DT / 100)
           END-READ
           IF NOT HARD-ERR
              IF CT-STATUS-CD NOT = "R" AND CT-STATUS-CD NOT = "N"
                 DISPLAY "JHCTLKF STATUS ERROR"
                 SET HARD-ERR TO TRUE
              END-IF
           END-IF
           IF NOT HARD-ERR
              MOVE WS-THRESH-JOB-ID TO CT-JOB-ID
              READ JHCTLKF KEY IS CT-JOB-ID
                 INVALID KEY
                    MOVE 9999999999999.99 TO WK-EXCEPTION-LIMIT
                 NOT INVALID KEY
                    COMPUTE WK-EXCEPTION-LIMIT =
                       CT-INPUT-CNT * 1000000.00
              END-READ
           END-IF.

       2000-MAIN.
           ADD 1 TO CNT-INPUT
           COMPUTE WK-YYYYMM = FUNCTION INTEGER(DW-CYCLE-DT / 100)
           IF WK-YYYYMM = WS-TARGET-YYYYMM
              PERFORM 2200-GET-ACCOUNT
              IF ACD-FOUND
                 PERFORM 2300-CALL-JH315S
              ELSE
                 ADD 1 TO CNT-COMP-FAIL
                 ADD 1 TO CNT-REJECT
              END-IF
           END-IF
           PERFORM 2100-READ-JHDW.

       2100-READ-JHDW.
           READ JHDWHF
              AT END
                 SET END-JHDW TO TRUE
              NOT AT END
                 IF FS-JHDWHF NOT = "00"
                    MOVE FS-JHDWHF TO MSG-ST
                    DISPLAY "JHDWHF READ ERROR ST=" MSG-ST
                    SET HARD-ERR TO TRUE
                 END-IF
           END-READ.

       2200-GET-ACCOUNT.
           MOVE "N" TO SW-ACD-FOUND
           MOVE "N" TO SW-CACHE-HIT
           PERFORM VARYING CX FROM 1 BY 1 UNTIL CX > WS-MAX-CACHE
              IF C-ACCT-NO(CX) = DW-ACCT-NO
                 MOVE C-PRODUCT-CD(CX)  TO ACD-TRUST-PRODUCT-CD
                 MOVE C-BRANCH-CD(CX)   TO ACD-BRANCH-CD
                 MOVE C-STATUS-CD(CX)   TO ACD-STATUS-CD
                 MOVE C-CLOSE-DT(CX)    TO ACD-CLOSE-DT
                 MOVE C-LAST-CHG-TS(CX) TO ACD-LAST-CHG-TS
                 SET ACD-FOUND TO TRUE
                 SET CACHE-HIT TO TRUE
                 MOVE WS-MAX-CACHE TO CX
              END-IF
           END-PERFORM
           IF NOT CACHE-HIT
              MOVE DW-ACCT-NO TO ACD-ACCT-NO
              READ JHACDMF KEY IS ACD-ACCT-NO
                 INVALID KEY
                    MOVE "N" TO SW-ACD-FOUND
                 NOT INVALID KEY
                    SET ACD-FOUND TO TRUE
                    PERFORM 2210-PUT-CACHE
              END-READ
              IF FS-JHACDMF NOT = "00" AND FS-JHACDMF NOT = "23"
                 MOVE FS-JHACDMF TO MSG-ST
                 DISPLAY "JHACDMF READ ERROR ST=" MSG-ST
                 SET HARD-ERR TO TRUE
              END-IF
           END-IF.

       2210-PUT-CACHE.
           ADD 1 TO WS-CACHE-POS
           IF WS-CACHE-POS > WS-MAX-CACHE
              MOVE 1 TO WS-CACHE-POS
           END-IF
           MOVE DW-ACCT-NO            TO C-ACCT-NO(WS-CACHE-POS)
           MOVE ACD-TRUST-PRODUCT-CD  TO C-PRODUCT-CD(WS-CACHE-POS)
           MOVE ACD-BRANCH-CD         TO C-BRANCH-CD(WS-CACHE-POS)
           MOVE ACD-STATUS-CD         TO C-STATUS-CD(WS-CACHE-POS)
           MOVE ACD-CLOSE-DT          TO C-CLOSE-DT(WS-CACHE-POS)
           MOVE ACD-LAST-CHG-TS       TO C-LAST-CHG-TS(WS-CACHE-POS).

       2300-CALL-JH315S.
           INITIALIZE JH315S-PARM
           MOVE JHDWHF-REC TO JH315S-IN-AREA
           CALL "JH100S" USING JH315S-PARM
           IF JH315S-STATUS = "00"
              IF ACD-STATUS-CD = "9"
                 ADD 1 TO CNT-CLOSED
                 ADD 1 TO CNT-ADJUST
              END-IF
              PERFORM 2400-CHECK-EXCEPTION
              PERFORM 3000-ADD-AGG
              ADD 1 TO CNT-STAGED
           ELSE
              IF JH315S-STATUS = "10" OR JH315S-STATUS = "20"
                 ADD 1 TO CNT-REJECT
              ELSE
                 DISPLAY "JH315S STATUS ERROR ST=" JH315S-STATUS
                 SET HARD-ERR TO TRUE
              END-IF
           END-IF.

       2400-CHECK-EXCEPTION.
           IF DW-FEE-AMT > WK-EXCEPTION-LIMIT
              ADD 1 TO CNT-ADJUST
              DISPLAY "EXCEPTION LIMIT ACCT=" DW-ACCT-NO
           END-IF.

       3000-ADD-AGG.
           MOVE ACD-TRUST-PRODUCT-CD TO WK-PRODUCT-CD
           MOVE ACD-BRANCH-CD        TO WK-BRANCH-CD
           MOVE "N" TO SW-AGG-FOUND
           MOVE ZERO TO FREE-IX
           PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > WS-MAX-AGG
              IF A-USED(IX) = "Y"
                 IF A-YYYYMM(IX) = WK-YYYYMM
                    AND A-PRODUCT-CD(IX) = WK-PRODUCT-CD
                    AND A-BRANCH-CD(IX) = WK-BRANCH-CD
                    PERFORM 3100-ACCUM-AGG
                    SET AGG-FOUND TO TRUE
                    MOVE WS-MAX-AGG TO IX
                 END-IF
              ELSE
                 IF FREE-IX = ZERO
                    MOVE IX TO FREE-IX
                 END-IF
              END-IF
           END-PERFORM
           IF NOT AGG-FOUND
              IF FREE-IX = ZERO
                 PERFORM 5000-FLUSH-AGG
                 ADD 1 TO WS-SPILL-CNT
                 MOVE 1 TO FREE-IX
              END-IF
              MOVE FREE-IX TO IX
              MOVE "Y" TO A-USED(IX)
              MOVE WK-YYYYMM      TO A-YYYYMM(IX)
              MOVE WK-PRODUCT-CD  TO A-PRODUCT-CD(IX)
              MOVE WK-BRANCH-CD   TO A-BRANCH-CD(IX)
              MOVE ZERO           TO A-FEE-AMT(IX)
              MOVE ZERO           TO A-FEE-YTD(IX)
              MOVE ZERO           TO A-ACCT-CNT(IX)
              MOVE ZERO           TO A-ADJUST-CNT(IX)
              PERFORM 3100-ACCUM-AGG
           END-IF
           IF FREE-IX > WS-SPILL-LIMIT
              PERFORM 5000-FLUSH-AGG
              ADD 1 TO WS-SPILL-CNT
           END-IF.

       3100-ACCUM-AGG.
           ADD DW-FEE-AMT TO A-FEE-AMT(IX)
           ADD DW-FEE-YTD TO A-FEE-YTD(IX)
           ADD 1          TO A-ACCT-CNT(IX)
           IF ACD-STATUS-CD = "9" OR DW-FEE-AMT < ZERO
              ADD 1 TO A-ADJUST-CNT(IX)
           END-IF
           ADD DW-FEE-AMT TO SUM-FEE-AMT
           ADD DW-FEE-YTD TO SUM-FEE-YTD.

       5000-FLUSH-AGG.
           PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > WS-MAX-AGG
              IF A-USED(IX) = "Y"
                 INITIALIZE JHMONRF-REC
                 MOVE A-YYYYMM(IX)     TO MON-YYYYMM
                 MOVE A-PRODUCT-CD(IX) TO MON-TRUST-PRODUCT-CD
                 MOVE A-BRANCH-CD(IX)  TO MON-BRANCH-CD
                 MOVE A-FEE-AMT(IX)    TO MON-FEE-AMT-SUM
                 MOVE A-FEE-YTD(IX)    TO MON-FEE-YTD-SUM
                 MOVE A-ACCT-CNT(IX)   TO MON-ACCT-CNT
                 MOVE A-ADJUST-CNT(IX) TO MON-ADJUST-CNT
                 WRITE JHMONRF-REC
                 IF FS-JHMONRF NOT = "00"
                    MOVE FS-JHMONRF TO MSG-ST
                    DISPLAY "JHMONRF WRITE ERROR ST=" MSG-ST
                    SET HARD-ERR TO TRUE
                    MOVE WS-MAX-AGG TO IX
                 ELSE
                    ADD 1 TO CNT-AGG-OUT
                    MOVE "N" TO A-USED(IX)
                 END-IF
              END-IF
           END-PERFORM.

       8000-WRITE-END-AUDIT.
           PERFORM 1100-GET-TIMESTAMP
           PERFORM 8200-NEXT-AUDIT-SEQ
           INITIALIZE JHAUDTF-REC
           MOVE WS-AUDIT-SEQ   TO AUD-AUDIT-SEQ
           MOVE WS-JOB-ID      TO AUD-JOB-ID
           MOVE WS-BUSINESS-DT TO AUD-BUSINESS-DT
           MOVE "END"          TO AUD-EVENT-CD
           MOVE "JHMONRF"      TO AUD-DATASET-ID
           MOVE CNT-AGG-OUT    TO AUD-REC-CNT
           MOVE SUM-FEE-AMT    TO AUD-AMT-TOTAL
           MOVE WS-CURRENT-TS  TO AUD-EVENT-TS
           WRITE JHAUDTF-REC
           IF FS-JHAUDTF NOT = "00"
              MOVE FS-JHAUDTF TO MSG-ST
              DISPLAY "JHAUDTF END AUDIT WRITE ERROR ST=" MSG-ST
              SET HARD-ERR TO TRUE
           END-IF
           DISPLAY "JH330B INPUT COUNT=" CNT-INPUT
           DISPLAY "JH330B AGG COUNT=" CNT-AGG-OUT
           DISPLAY "JH330B AMOUNT TOTAL=" SUM-FEE-AMT
           DISPLAY "JH330B ENRICH FAIL COUNT=" CNT-COMP-FAIL
           DISPLAY "JH330B SPILL COUNT=" WS-SPILL-CNT.

       8100-WRITE-START-AUDIT.
           PERFORM 8200-NEXT-AUDIT-SEQ
           INITIALIZE JHAUDTF-REC
           MOVE WS-AUDIT-SEQ   TO AUD-AUDIT-SEQ
           MOVE WS-JOB-ID      TO AUD-JOB-ID
           MOVE WS-BUSINESS-DT TO AUD-BUSINESS-DT
           MOVE "REPL"         TO AUD-EVENT-CD
           MOVE "JHMONRF"      TO AUD-DATASET-ID
           MOVE ZERO           TO AUD-REC-CNT
           MOVE ZERO           TO AUD-AMT-TOTAL
           MOVE WS-CURRENT-TS  TO AUD-EVENT-TS
           WRITE JHAUDTF-REC
           IF FS-JHAUDTF NOT = "00"
              MOVE FS-JHAUDTF TO MSG-ST
              DISPLAY "JHAUDTF START AUDIT WRITE ERROR ST=" MSG-ST
              SET HARD-ERR TO TRUE
           END-IF.

       8200-NEXT-AUDIT-SEQ.
           ADD 1 TO WS-AUDIT-SEQ
           IF WS-AUDIT-SEQ = 1
              COMPUTE WS-AUDIT-SEQ =
                 WS-RUN-SEQ * 1000000 + WS-AUDIT-SEQ
           END-IF.

       9000-CLOSE.
           CLOSE JHDWHF JHACDMF JHCTLKF JHMONRF JHAUDTF
           IF FS-JHDWHF NOT = "00"
              AND FS-JHDWHF NOT = "42"
              DISPLAY "JHDWHF CLOSE ERROR ST=" FS-JHDWHF
              SET HARD-ERR TO TRUE
           END-IF
           IF FS-JHACDMF NOT = "00"
              AND FS-JHACDMF NOT = "42"
              DISPLAY "JHACDMF CLOSE ERROR ST=" FS-JHACDMF
              SET HARD-ERR TO TRUE
           END-IF
           IF FS-JHCTLKF NOT = "00"
              AND FS-JHCTLKF NOT = "42"
              DISPLAY "JHCTLKF CLOSE ERROR ST=" FS-JHCTLKF
              SET HARD-ERR TO TRUE
           END-IF
           IF FS-JHMONRF NOT = "00"
              AND FS-JHMONRF NOT = "42"
              DISPLAY "JHMONRF CLOSE ERROR ST=" FS-JHMONRF
              SET HARD-ERR TO TRUE
           END-IF
           IF FS-JHAUDTF NOT = "00"
              AND FS-JHAUDTF NOT = "42"
              DISPLAY "JHAUDTF CLOSE ERROR ST=" FS-JHAUDTF
              SET HARD-ERR TO TRUE
           END-IF.
