       IDENTIFICATION DIVISION.
       PROGRAM-ID. KZ110B.
      * 変更履歴
      * 版数  年月日(和暦)   担当              概要
      * 001   令和04.03.31   佐藤 真一 (E-007) 初版作成
      * 002   令和05.09.30   高橋 直樹 (E-013) 手数料率改定対応
      * 003   令和06.03.31   鈴木 健太 (E-021) 端数処理見直し

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZACCTF ASSIGN TO "KZACCTF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS AC-ACCT-ID
               FILE STATUS IS FS-KZACCTF.

           SELECT KZCARDF ASSIGN TO "KZCARDF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS CD-CARD-NO
               FILE STATUS IS FS-KZCARDF.

           SELECT KZDGRPF ASSIGN TO "KZDGRPF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS DG-GROUP-CODE
               FILE STATUS IS FS-KZDGRPF.

           SELECT KZTRANF ASSIGN TO "KZTRANF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS FS-KZTRANF.

           SELECT KZFEEHF ASSIGN TO "KZFEEHF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS FE-ACCT-ID
               FILE STATUS IS FS-KZFEEHF.

       DATA DIVISION.
       FILE SECTION.
       FD  KZACCTF.
       COPY KZACCTC.

       FD  KZCARDF.
       COPY KZCARDC.

       FD  KZDGRPF.
       COPY KZDGRPC.

       FD  KZTRANF.
       COPY KZTRANC.

       FD  KZFEEHF.
       COPY KZFEEHC.

       WORKING-STORAGE SECTION.
       01  FS-KZACCTF              PIC XX VALUE SPACES.
       01  FS-KZCARDF              PIC XX VALUE SPACES.
       01  FS-KZDGRPF              PIC XX VALUE SPACES.
       01  FS-KZTRANF              PIC XX VALUE SPACES.
       01  FS-KZFEEHF              PIC XX VALUE SPACES.

       01  WS-EOF-SW               PIC X VALUE "N".
           88  WS-EOF                    VALUE "Y".
           88  WS-NOT-EOF                VALUE "N".

       01  WS-VALID-SW             PIC X VALUE "N".
           88  WS-VALID                  VALUE "Y".
           88  WS-INVALID                VALUE "N".

       01  WS-ABEND-SW             PIC X VALUE "N".
           88  WS-ABEND                  VALUE "Y".
           88  WS-NO-ABEND               VALUE "N".

       01  WS-CYCLE-DT             PIC 9(8) VALUE ZERO.
       01  WS-FEE-YTD              PIC S9(11)V99 COMP-3 VALUE ZERO.
       01  WS-OVER-AMT             PIC S9(13)V99 COMP-3 VALUE ZERO.
       01  WS-DISPLAY-AMT          PIC ZZZ,ZZZ,ZZZ,ZZ9.99.

       COPY LK-GRP-PARM.
       COPY LK-RATE-PARM.
       COPY LK-RND-PARM.
       COPY LK-CAP-PARM.

       PROCEDURE DIVISION.
       0000-MAIN.
           ACCEPT WS-CYCLE-DT FROM DATE YYYYMMDD

           OPEN INPUT KZACCTF
           IF FS-KZACCTF NOT = "00"
              DISPLAY "KZ110B OPEN ERROR KZACCTF FS=" FS-KZACCTF
              SET WS-ABEND TO TRUE
           END-IF

           IF WS-NO-ABEND
              OPEN OUTPUT KZFEEHF
              IF FS-KZFEEHF NOT = "00"
                 DISPLAY "KZ110B OPEN ERROR KZFEEHF FS=" FS-KZFEEHF
                 SET WS-ABEND TO TRUE
              END-IF
           END-IF

           IF WS-NO-ABEND
              PERFORM 1000-PROCESS-ACCOUNTS
           END-IF

           PERFORM 9000-CLOSE-FILES

           IF WS-ABEND
              STOP RUN
           END-IF

           GOBACK.

       1000-PROCESS-ACCOUNTS.
           SET WS-NOT-EOF TO TRUE

           PERFORM UNTIL WS-EOF OR WS-ABEND
              READ KZACCTF NEXT RECORD
                 AT END
                    SET WS-EOF TO TRUE
                 NOT AT END
                    IF FS-KZACCTF = "00"
                       PERFORM 2000-PROCESS-ONE-ACCOUNT
                    ELSE
                       DISPLAY "KZ110B READ ERROR KZACCTF FS="
                               FS-KZACCTF
                       SET WS-ABEND TO TRUE
                    END-IF
              END-READ
           END-PERFORM.

       2000-PROCESS-ONE-ACCOUNT.
           PERFORM 2100-VALIDATE-ACCOUNT

           IF WS-VALID
              PERFORM 2200-CALCULATE-OVER-AMT
              PERFORM 2300-CALL-FEE-CHAIN
              IF WS-NO-ABEND
                 PERFORM 2400-WRITE-FEE-HISTORY
              END-IF
           END-IF.

       2100-VALIDATE-ACCOUNT.
           SET WS-VALID TO TRUE

           IF AC-ACCT-ID = SPACES
              DISPLAY "KZ110B SKIP BLANK ACCT-ID"
              SET WS-INVALID TO TRUE
           END-IF

           IF WS-VALID
              IF AC-CYCLE-BAL < ZERO
                 MOVE AC-CYCLE-BAL TO WS-DISPLAY-AMT
                 DISPLAY "KZ110B SKIP NEGATIVE CYCLE-BAL ACCT="
                         AC-ACCT-ID " BAL=" WS-DISPLAY-AMT
                 SET WS-INVALID TO TRUE
              END-IF
           END-IF

           IF WS-VALID
              IF AC-CREDIT-LIMIT < ZERO
                 MOVE AC-CREDIT-LIMIT TO WS-DISPLAY-AMT
                 DISPLAY "KZ110B SKIP NEGATIVE CREDIT-LIMIT ACCT="
                         AC-ACCT-ID " LIMIT=" WS-DISPLAY-AMT
                 SET WS-INVALID TO TRUE
              END-IF
           END-IF

           IF WS-VALID
              IF AC-KYC-STATUS NOT = "A" AND AC-KYC-STATUS NOT = "V"
                 DISPLAY "KZ110B SKIP INVALID KYC ACCT="
                         AC-ACCT-ID " KYC=" AC-KYC-STATUS
                 SET WS-INVALID TO TRUE
              END-IF
           END-IF

           IF WS-VALID
              IF AC-GROUP-CODE NOT = "STD0"
                 AND AC-GROUP-CODE NOT = "GLD1"
                 AND AC-GROUP-CODE NOT = "PLT2"
                 AND AC-GROUP-CODE NOT = "EXMP"
                 AND AC-GROUP-CODE NOT = "PREM"
                 DISPLAY "KZ110B SKIP INVALID GROUP ACCT="
                         AC-ACCT-ID " GROUP=" AC-GROUP-CODE
                 SET WS-INVALID TO TRUE
              END-IF
           END-IF.

       2200-CALCULATE-OVER-AMT.
           IF AC-CYCLE-BAL > AC-CREDIT-LIMIT
              COMPUTE WS-OVER-AMT = AC-CYCLE-BAL - AC-CREDIT-LIMIT
           ELSE
              MOVE ZERO TO WS-OVER-AMT
           END-IF

           MOVE WS-OVER-AMT TO AC-OVER-AMT.

       2300-CALL-FEE-CHAIN.
           MOVE ZERO TO WS-FEE-YTD

           INITIALIZE LK-GRP-PARM
           MOVE AC-GROUP-CODE TO LK-GRP-CODE OF LK-GRP-PARM
           MOVE WS-OVER-AMT   TO LK-OVER-AMT OF LK-GRP-PARM

           CALL "KZ120S" USING LK-GRP-PARM

           IF LK-RET-CD OF LK-GRP-PARM NOT = ZERO
              DISPLAY "KZ110B KZ120S ERROR ACCT=" AC-ACCT-ID
                      " GROUP=" AC-GROUP-CODE
                      " RC=" LK-RET-CD OF LK-GRP-PARM
              SET WS-ABEND TO TRUE
           END-IF

           IF WS-NO-ABEND
              INITIALIZE LK-RATE-PARM
              MOVE LK-GRP-CODE OF LK-GRP-PARM
                   TO LK-GRP-CODE OF LK-RATE-PARM
              MOVE WS-OVER-AMT
                   TO LK-OVER-AMT OF LK-RATE-PARM

              CALL "KZ125S" USING LK-RATE-PARM

              IF LK-RET-CD OF LK-RATE-PARM NOT = ZERO
                 DISPLAY "KZ110B KZ125S ERROR ACCT=" AC-ACCT-ID
                         " GROUP=" LK-GRP-CODE OF LK-RATE-PARM
                         " RC=" LK-RET-CD OF LK-RATE-PARM
                 SET WS-ABEND TO TRUE
              END-IF
           END-IF

           IF WS-NO-ABEND
              INITIALIZE LK-RND-PARM
              MOVE LK-FEE-AMT OF LK-RATE-PARM
                   TO LK-AMT-RAW OF LK-RND-PARM

              CALL "KZ130S" USING LK-RND-PARM

              ADD LK-AMT-FLOORED OF LK-RND-PARM TO WS-FEE-YTD
           END-IF

           IF WS-NO-ABEND
              INITIALIZE LK-CAP-PARM
              MOVE WS-FEE-YTD TO LK-FEE-YTD OF LK-CAP-PARM

              CALL "KZ115B" USING LK-CAP-PARM

              IF LK-CAP-FLAG OF LK-CAP-PARM = "E"
                 DISPLAY "KZ110B KZ115B ERROR ACCT=" AC-ACCT-ID
                 SET WS-ABEND TO TRUE
              END-IF
           END-IF.

       2400-WRITE-FEE-HISTORY.
           INITIALIZE KZFEEHF-REC

           MOVE AC-ACCT-ID TO FE-ACCT-ID
           MOVE LK-FEE-CAPPED OF LK-CAP-PARM TO FE-FEE-AMT
           MOVE WS-FEE-YTD TO FE-FEE-YTD
           MOVE LK-CAP-FLAG OF LK-CAP-PARM TO FE-CAP-FLAG
           MOVE LK-EXEMPT OF LK-GRP-PARM TO FE-EXEMPT-FLAG
           MOVE WS-CYCLE-DT TO FE-CYCLE-DT

           WRITE KZFEEHF-REC
              INVALID KEY
                 DISPLAY "KZ110B WRITE DUP/ERROR KZFEEHF ACCT="
                         FE-ACCT-ID " FS=" FS-KZFEEHF
                 SET WS-ABEND TO TRUE
              NOT INVALID KEY
                 IF FS-KZFEEHF NOT = "00"
                    DISPLAY "KZ110B WRITE ERROR KZFEEHF ACCT="
                            FE-ACCT-ID " FS=" FS-KZFEEHF
                    SET WS-ABEND TO TRUE
                 END-IF
           END-WRITE.

       9000-CLOSE-FILES.
           IF FS-KZACCTF = "00"
              CLOSE KZACCTF
              IF FS-KZACCTF NOT = "00"
                 DISPLAY "KZ110B CLOSE ERROR KZACCTF FS=" FS-KZACCTF
              END-IF
           END-IF

           IF FS-KZFEEHF = "00"
              CLOSE KZFEEHF
              IF FS-KZFEEHF NOT = "00"
                 DISPLAY "KZ110B CLOSE ERROR KZFEEHF FS=" FS-KZFEEHF
              END-IF
           END-IF.
