       IDENTIFICATION DIVISION.
       PROGRAM-ID. KZ220B.
      *---------------------------------------------------------------*
      * 変更履歴
      * 版数  年月日    担当     概要
      * 1.00  20250203  KZB001   初版作成
      * 1.01  20250312  KZB002   限度額ゼロ判定追加
      * 1.02  20250418  KZB003   顧客別利用額連携追加
      *---------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZACCTF ASSIGN TO "KZACCTF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS AC-ACCT-NO
               FILE STATUS IS WS-ST-ACCT.
           SELECT KZCUSTF ASSIGN TO "KZCUSTF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS CU-CUST-ID
               FILE STATUS IS WS-ST-CUST.
           SELECT KZSCORF ASSIGN TO "KZSCORF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS SC-CUST-ID
               FILE STATUS IS WS-ST-SCOR.
           SELECT KZLUTLF ASSIGN TO "KZLUTLF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-ST-LUTL.
           SELECT KZEXPF ASSIGN TO "KZEXPF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-ST-EXPF.

       DATA DIVISION.
       FILE SECTION.
       FD  KZACCTF.
           COPY KZACCTC4.
       FD  KZCUSTF.
           COPY KZCUSTC.
       FD  KZSCORF.
           COPY KZSCORFC.
       FD  KZLUTLF.
           COPY KZLUTLFC.
       FD  KZEXPF.
           COPY KZEXPFC.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05 WS-ST-ACCT        PIC XX VALUE SPACES.
           05 WS-ST-CUST        PIC XX VALUE SPACES.
           05 WS-ST-SCOR        PIC XX VALUE SPACES.
           05 WS-ST-LUTL        PIC XX VALUE SPACES.
           05 WS-ST-EXPF        PIC XX VALUE SPACES.

       01  WS-FLAGS.
           05 WS-EOF-ACCT       PIC X  VALUE 'N'.
              88 ACCT-END              VALUE 'Y'.
           05 WS-HARD-ERR       PIC X  VALUE 'N'.
              88 HARD-ERROR            VALUE 'Y'.
           05 WS-CUST-FOUND     PIC X  VALUE 'N'.
              88 CUST-FOUND            VALUE 'Y'.
           05 WS-SCOR-FOUND     PIC X  VALUE 'N'.
              88 SCOR-FOUND            VALUE 'Y'.
           05 WS-WARN-FLAG      PIC X  VALUE SPACE.

       01  WS-WORK.
           05 WS-SYS-DATE.
              10 WS-SYS-YYYY    PIC 9(04).
              10 WS-SYS-MM      PIC 9(02).
              10 WS-SYS-DD      PIC 9(02).
           05 WS-AS-OF-DATE     PIC 9(08) VALUE ZERO.
           05 WS-UTIL-RATE      PIC S9(05)V99 COMP-3 VALUE ZERO.
           05 WS-EXPOSURE-AMT   PIC S9(13)V99 COMP-3 VALUE ZERO.
           05 WS-PRODUCT-TYPE   PIC X(02) VALUE SPACES.
           05 WS-SEARCH-ID      PIC X(12) VALUE SPACES.
           05 WS-CUR-INDEX      PIC 9(04) COMP VALUE ZERO.
           05 WS-FREE-INDEX     PIC 9(04) COMP VALUE ZERO.
           05 WS-TABLE-MAX      PIC 9(04) COMP VALUE 5000.

       01  WS-COUNTERS.
           05 WS-CNT-READ       PIC 9(09) VALUE ZERO.
           05 WS-CNT-LUTL       PIC 9(09) VALUE ZERO.
           05 WS-CNT-EXPF       PIC 9(09) VALUE ZERO.
           05 WS-CNT-ZERO-LIM   PIC 9(09) VALUE ZERO.
           05 WS-CNT-MINUS-BAL  PIC 9(09) VALUE ZERO.
           05 WS-CNT-NO-CUST    PIC 9(09) VALUE ZERO.
           05 WS-CNT-WARN       PIC 9(09) VALUE ZERO.
           05 WS-CNT-SCOR-MISS  PIC 9(09) VALUE ZERO.

       01  WS-EXPOSURE-TABLE.
           05 WS-EXPOSURE-ENTRY OCCURS 5000 TIMES.
              10 WT-CUST-ID     PIC X(12) VALUE SPACES.
              10 WT-PROD-TYPE   PIC X(02) VALUE SPACES.
              10 WT-AMOUNT      PIC S9(13)V99 COMP-3 VALUE ZERO.

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE FUNCTION CURRENT-DATE(1:8) TO WS-SYS-DATE
           MOVE FUNCTION NUMVAL(WS-SYS-DATE) TO WS-AS-OF-DATE

           PERFORM 1000-OPEN-FILES
           IF NOT HARD-ERROR
              PERFORM 2000-READ-ACCT
              PERFORM UNTIL ACCT-END OR HARD-ERROR
                 ADD 1 TO WS-CNT-READ
                 PERFORM 3000-PROCESS-ACCT
                 PERFORM 2000-READ-ACCT
              END-PERFORM
           END-IF

           IF NOT HARD-ERROR
              PERFORM 5000-WRITE-EXPOSURE
           END-IF

           PERFORM 9000-CLOSE-FILES

           IF HARD-ERROR
              MOVE 12 TO RETURN-CODE
           ELSE
              DISPLAY 'KZ220B NORMAL END INPUT=' WS-CNT-READ
              DISPLAY 'KZ220B LUTL OUT=' WS-CNT-LUTL
              DISPLAY 'KZ220B EXPF OUT=' WS-CNT-EXPF
              DISPLAY 'KZ220B ZERO LIMIT=' WS-CNT-ZERO-LIM
              DISPLAY 'KZ220B MINUS BAL=' WS-CNT-MINUS-BAL
              DISPLAY 'KZ220B NO CUSTOMER=' WS-CNT-NO-CUST
              DISPLAY 'KZ220B WARNING=' WS-CNT-WARN
              DISPLAY 'KZ220B SCORE MISS=' WS-CNT-SCOR-MISS
              MOVE 0 TO RETURN-CODE
           END-IF
           GOBACK.

       1000-OPEN-FILES.
           OPEN INPUT KZACCTF
           IF WS-ST-ACCT NOT = '00'
              DISPLAY 'KZACCTF OPEN ERROR ST=' WS-ST-ACCT
              SET HARD-ERROR TO TRUE
           END-IF

           OPEN INPUT KZCUSTF
           IF WS-ST-CUST NOT = '00'
              DISPLAY 'KZCUSTF OPEN ERROR ST=' WS-ST-CUST
              SET HARD-ERROR TO TRUE
           END-IF

           OPEN INPUT KZSCORF
           IF WS-ST-SCOR NOT = '00'
              DISPLAY 'KZSCORF OPEN ERROR ST=' WS-ST-SCOR
              SET HARD-ERROR TO TRUE
           END-IF

           OPEN OUTPUT KZLUTLF
           IF WS-ST-LUTL NOT = '00'
              DISPLAY 'KZLUTLF OPEN ERROR ST=' WS-ST-LUTL
              SET HARD-ERROR TO TRUE
           END-IF

           OPEN OUTPUT KZEXPF
           IF WS-ST-EXPF NOT = '00'
              DISPLAY 'KZEXPF OPEN ERROR ST=' WS-ST-EXPF
              SET HARD-ERROR TO TRUE
           END-IF.

       2000-READ-ACCT.
           READ KZACCTF NEXT RECORD
              AT END
                 SET ACCT-END TO TRUE
              NOT AT END
                 IF WS-ST-ACCT NOT = '00'
                    DISPLAY 'KZACCTF READ ERROR ST=' WS-ST-ACCT
                    SET HARD-ERROR TO TRUE
                 END-IF
           END-READ.

       3000-PROCESS-ACCT.
           MOVE 'N' TO WS-CUST-FOUND
           MOVE 'N' TO WS-SCOR-FOUND
           MOVE SPACE TO WS-WARN-FLAG
           MOVE ZERO TO WS-UTIL-RATE
           MOVE ZERO TO WS-EXPOSURE-AMT
           MOVE SPACES TO WS-PRODUCT-TYPE

           PERFORM 3100-READ-CUSTOMER
           IF NOT CUST-FOUND
              ADD 1 TO WS-CNT-NO-CUST
              DISPLAY 'NO CUSTOMER ACCT=' AC-ACCT-NO
              DISPLAY 'NO CUSTOMER CUST=' AC-CUST-ID
              EXIT PARAGRAPH
           END-IF

           IF AC-CREDIT-LIMIT = ZERO
              ADD 1 TO WS-CNT-ZERO-LIM
              DISPLAY 'ZERO LIMIT ACCT=' AC-ACCT-NO
              DISPLAY 'ZERO LIMIT CUST=' AC-CUST-ID
              EXIT PARAGRAPH
           END-IF

           IF AC-CUR-BAL < ZERO
              ADD 1 TO WS-CNT-MINUS-BAL
              DISPLAY 'MINUS BAL ACCT=' AC-ACCT-NO
              DISPLAY 'MINUS BAL AMT=' AC-CUR-BAL
              EXIT PARAGRAPH
           END-IF

           PERFORM 3200-READ-SCORE
           PERFORM 3300-DECIDE-WARNING
           PERFORM 3400-CALC-UTIL
           PERFORM 3500-MAP-PRODUCT

           IF WS-PRODUCT-TYPE = SPACES
              DISPLAY 'UNKNOWN ACCT TYPE ACCT=' AC-ACCT-NO
              DISPLAY 'UNKNOWN ACCT TYPE=' AC-ACCT-TYPE
              EXIT PARAGRAPH
           END-IF

           IF WS-UTIL-RATE >= 80
              PERFORM 3600-WRITE-LUTL
           END-IF

           PERFORM 3700-ACCUM-EXPOSURE.

       3100-READ-CUSTOMER.
           MOVE AC-CUST-ID TO CU-CUST-ID
           READ KZCUSTF
              INVALID KEY
                 MOVE 'N' TO WS-CUST-FOUND
              NOT INVALID KEY
                 MOVE 'Y' TO WS-CUST-FOUND
           END-READ

           IF WS-ST-CUST NOT = '00' AND WS-ST-CUST NOT = '23'
              DISPLAY 'KZCUSTF READ ERROR ST=' WS-ST-CUST
              DISPLAY 'KZCUSTF READ CUST=' AC-CUST-ID
              SET HARD-ERROR TO TRUE
           END-IF.

       3200-READ-SCORE.
           MOVE AC-CUST-ID TO SC-CUST-ID
           READ KZSCORF
              INVALID KEY
                 MOVE 'N' TO WS-SCOR-FOUND
                 ADD 1 TO WS-CNT-SCOR-MISS
              NOT INVALID KEY
                 MOVE 'Y' TO WS-SCOR-FOUND
           END-READ

           IF WS-ST-SCOR NOT = '00' AND WS-ST-SCOR NOT = '23'
              DISPLAY 'KZSCORF READ ERROR ST=' WS-ST-SCOR
              DISPLAY 'KZSCORF READ CUST=' AC-CUST-ID
              SET HARD-ERROR TO TRUE
           END-IF.

       3300-DECIDE-WARNING.
           IF CU-KYC-STATUS = '0'
              MOVE 'K' TO WS-WARN-FLAG
              ADD 1 TO WS-CNT-WARN
              DISPLAY 'KYC UNCONFIRMED CUST=' AC-CUST-ID
           ELSE
              IF CU-KYC-STATUS = '9'
                 MOVE 'H' TO WS-WARN-FLAG
                 ADD 1 TO WS-CNT-WARN
                 DISPLAY 'KYC HOLD CUST=' AC-CUST-ID
              END-IF
           END-IF

           IF SCOR-FOUND
              IF SC-SCORE-POINT < 300
                 MOVE 'S' TO WS-WARN-FLAG
                 ADD 1 TO WS-CNT-WARN
                 DISPLAY 'LOW SCORE CUST=' AC-CUST-ID
                 DISPLAY 'LOW SCORE POINT=' SC-SCORE-POINT
              END-IF
           ELSE
              IF WS-WARN-FLAG = SPACE
                 MOVE 'N' TO WS-WARN-FLAG
                 ADD 1 TO WS-CNT-WARN
              END-IF
           END-IF.

       3400-CALC-UTIL.
           COMPUTE WS-UTIL-RATE ROUNDED =
               (AC-CUR-BAL * 100) / AC-CREDIT-LIMIT

           IF AC-CUR-BAL > AC-CREDIT-LIMIT
              MOVE 'L' TO WS-WARN-FLAG
              ADD 1 TO WS-CNT-WARN
              DISPLAY 'OVER LIMIT ACCT=' AC-ACCT-NO
              DISPLAY 'OVER LIMIT RATE=' WS-UTIL-RATE
           END-IF.

       3500-MAP-PRODUCT.
           EVALUATE AC-ACCT-TYPE
              WHEN 'CL'
                 MOVE '01' TO WS-PRODUCT-TYPE
              WHEN 'OD'
                 MOVE '02' TO WS-PRODUCT-TYPE
              WHEN 'LN'
                 MOVE '03' TO WS-PRODUCT-TYPE
              WHEN OTHER
                 MOVE SPACES TO WS-PRODUCT-TYPE
           END-EVALUATE.

       3600-WRITE-LUTL.
           INITIALIZE KZLUTLF-REC
           MOVE AC-ACCT-NO      TO LU-ACCT-NO
           MOVE AC-CUST-ID      TO LU-CUST-ID
           MOVE WS-AS-OF-DATE   TO LU-AS-OF-DATE
           MOVE AC-CUR-BAL      TO LU-CUR-BAL
           MOVE AC-CREDIT-LIMIT TO LU-CREDIT-LIMIT
           MOVE WS-UTIL-RATE    TO LU-UTIL-RATE
           MOVE WS-WARN-FLAG    TO LU-WARN-FLAG

           WRITE KZLUTLF-REC
           IF WS-ST-LUTL = '00'
              ADD 1 TO WS-CNT-LUTL
           ELSE
              DISPLAY 'KZLUTLF WRITE ERROR ST=' WS-ST-LUTL
              DISPLAY 'KZLUTLF WRITE ACCT=' AC-ACCT-NO
              SET HARD-ERROR TO TRUE
           END-IF.

       3700-ACCUM-EXPOSURE.
           MOVE AC-CUR-BAL TO WS-EXPOSURE-AMT
           MOVE AC-CUST-ID TO WS-SEARCH-ID
           MOVE ZERO TO WS-CUR-INDEX

           PERFORM VARYING WS-FREE-INDEX FROM 1 BY 1
              UNTIL WS-FREE-INDEX > WS-TABLE-MAX
                 OR WS-CUR-INDEX NOT = ZERO
              IF WT-CUST-ID(WS-FREE-INDEX) = WS-SEARCH-ID
                 AND WT-PROD-TYPE(WS-FREE-INDEX) = WS-PRODUCT-TYPE
                 MOVE WS-FREE-INDEX TO WS-CUR-INDEX
              ELSE
                 IF WT-CUST-ID(WS-FREE-INDEX) = SPACES
                    MOVE WS-FREE-INDEX TO WS-CUR-INDEX
                    MOVE WS-SEARCH-ID TO WT-CUST-ID(WS-CUR-INDEX)
                    MOVE WS-PRODUCT-TYPE
                       TO WT-PROD-TYPE(WS-CUR-INDEX)
                 END-IF
              END-IF
           END-PERFORM

           IF WS-CUR-INDEX = ZERO
              DISPLAY 'EXPOSURE TABLE FULL CUST=' AC-CUST-ID
              SET HARD-ERROR TO TRUE
           ELSE
              ADD WS-EXPOSURE-AMT TO WT-AMOUNT(WS-CUR-INDEX)
           END-IF.

       5000-WRITE-EXPOSURE.
           PERFORM VARYING WS-CUR-INDEX FROM 1 BY 1
              UNTIL WS-CUR-INDEX > WS-TABLE-MAX OR HARD-ERROR
              IF WT-CUST-ID(WS-CUR-INDEX) NOT = SPACES
                 INITIALIZE KZEXPF-REC
                 MOVE WT-CUST-ID(WS-CUR-INDEX) TO EX-CUST-ID
                 MOVE WT-PROD-TYPE(WS-CUR-INDEX)
                    TO EX-PRODUCT-TYPE
                 MOVE WT-AMOUNT(WS-CUR-INDEX)
                    TO EX-EXPOSURE-AMT
                 WRITE KZEXPF-REC
                 IF WS-ST-EXPF = '00'
                    ADD 1 TO WS-CNT-EXPF
                 ELSE
                    DISPLAY 'KZEXPF WRITE ERROR ST=' WS-ST-EXPF
                    DISPLAY 'KZEXPF WRITE CUST='
                       WT-CUST-ID(WS-CUR-INDEX)
                    SET HARD-ERROR TO TRUE
                 END-IF
              END-IF
           END-PERFORM.

       9000-CLOSE-FILES.
           CLOSE KZACCTF
           IF WS-ST-ACCT NOT = '00'
              DISPLAY 'KZACCTF CLOSE ERROR ST=' WS-ST-ACCT
              SET HARD-ERROR TO TRUE
           END-IF

           CLOSE KZCUSTF
           IF WS-ST-CUST NOT = '00'
              DISPLAY 'KZCUSTF CLOSE ERROR ST=' WS-ST-CUST
              SET HARD-ERROR TO TRUE
           END-IF

           CLOSE KZSCORF
           IF WS-ST-SCOR NOT = '00'
              DISPLAY 'KZSCORF CLOSE ERROR ST=' WS-ST-SCOR
              SET HARD-ERROR TO TRUE
           END-IF

           CLOSE KZLUTLF
           IF WS-ST-LUTL NOT = '00'
              DISPLAY 'KZLUTLF CLOSE ERROR ST=' WS-ST-LUTL
              SET HARD-ERROR TO TRUE
           END-IF

           CLOSE KZEXPF
           IF WS-ST-EXPF NOT = '00'
              DISPLAY 'KZEXPF CLOSE ERROR ST=' WS-ST-EXPF
              SET HARD-ERROR TO TRUE
           END-IF.
