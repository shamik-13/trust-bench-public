       IDENTIFICATION DIVISION.
       PROGRAM-ID. JH101B.
      * 変更履歴
      * 版数  年月日(和暦)  担当                      概要
      * 1.00  令和04年04月01日  システム部 情報系チーム  新規作成
      * 1.01  令和05年10月16日  システム部 情報系チーム  手数料照合条件見直し
      * 1.02  令和06年06月03日  システム部 情報系チーム  不整合検知項目追加
       AUTHOR.     JH-BATCH.
      * 手数料データ品質照合バッチ
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CONSOLE IS SYSOUT.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZFEEHF ASSIGN TO "KZFEEHF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-KZFEEHF.
           SELECT JHDWHF ASSIGN TO "JHDWHF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-JHDWHF.
           SELECT JHSTGF ASSIGN TO "JHSTGF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-JHSTGF.
           SELECT JHRECMF ASSIGN TO "JHRECMF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS REC-RECON-KEY
               FILE STATUS IS FS-JHRECMF.
           SELECT JHAUDTF ASSIGN TO "JHAUDTF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-JHAUDTF.

       DATA DIVISION.
       FILE SECTION.
       FD  KZFEEHF.
           COPY KZFEEHFC.
       FD  JHDWHF.
           COPY JHDWHFC.
       FD  JHSTGF.
           COPY JHSTGC.
       FD  JHRECMF.
           COPY JHRECC.
       FD  JHAUDTF.
           COPY JHAUDC.

       WORKING-STORAGE SECTION.
       01  FS-AREA.
           05 FS-KZFEEHF              PIC XX VALUE SPACES.
           05 FS-JHDWHF               PIC XX VALUE SPACES.
           05 FS-JHSTGF               PIC XX VALUE SPACES.
           05 FS-JHRECMF              PIC XX VALUE SPACES.
           05 FS-JHAUDTF              PIC XX VALUE SPACES.

       01  SW-AREA.
           05 EOF-KZFEEHF             PIC X VALUE "N".
              88 KZFEEHF-END                VALUE "Y".
           05 EOF-JHDWHF              PIC X VALUE "N".
              88 JHDWHF-END                 VALUE "Y".
           05 EOF-JHSTGF              PIC X VALUE "N".
              88 JHSTGF-END                 VALUE "Y".
           05 WS-HARD-ERROR           PIC X VALUE "N".
              88 HARD-ERROR                 VALUE "Y".
           05 WS-STOP-MART            PIC X VALUE "N".

       01  WS-CONTROL.
           05 WS-JOB-ID               PIC X(08) VALUE "JH360B".
           05 WS-BUSINESS-DT          PIC 9(08) VALUE ZERO.
           05 WS-TIME-NOW             PIC 9(08) VALUE ZERO.
           05 WS-AUDIT-SEQ            PIC 9(09) VALUE ZERO.
           05 WS-TOL-CNT              PIC S9(09) COMP-3 VALUE 0.
           05 WS-TOL-AMT              PIC S9(13)V99 COMP-3 VALUE 0.
           05 WS-MAX-REC              PIC 9(05) VALUE 20000.
           05 IX                      PIC 9(05) COMP VALUE 0.
           05 JX                      PIC 9(05) COMP VALUE 0.
           05 WS-FOUND                PIC X VALUE "N".
           05 WS-KEY                  PIC X(32) VALUE SPACES.

       01  WS-SUMMARY.
           05 FH-CNT                  PIC S9(09) COMP-3 VALUE 0.
           05 FH-AMT                  PIC S9(13)V99 COMP-3 VALUE 0.
           05 STG-CNT                 PIC S9(09) COMP-3 VALUE 0.
           05 STG-AMT                 PIC S9(13)V99 COMP-3 VALUE 0.
           05 DW-CNT                  PIC S9(09) COMP-3 VALUE 0.
           05 DW-AMT                  PIC S9(13)V99 COMP-3 VALUE 0.
           05 STG-DIFF-CNT            PIC S9(09) COMP-3 VALUE 0.
           05 STG-DIFF-AMT            PIC S9(13)V99 COMP-3 VALUE 0.
           05 DW-DIFF-CNT             PIC S9(09) COMP-3 VALUE 0.
           05 DW-DIFF-AMT             PIC S9(13)V99 COMP-3 VALUE 0.
           05 DUP-CNT                 PIC S9(09) COMP-3 VALUE 0.
           05 MISS-CNT                PIC S9(09) COMP-3 VALUE 0.
           05 AMT-NG-CNT              PIC S9(09) COMP-3 VALUE 0.

       01  WS-JUDGE.
           05 STG-JUDGE-CD            PIC X(02) VALUE "00".
           05 DW-JUDGE-CD             PIC X(02) VALUE "00".
           05 JUDGE-OK                PIC X(02) VALUE "00".
           05 JUDGE-DUP               PIC X(02) VALUE "10".
           05 JUDGE-MISS              PIC X(02) VALUE "20".
           05 JUDGE-AMT               PIC X(02) VALUE "30".
           05 JUDGE-CNT               PIC X(02) VALUE "40".
           05 JUDGE-IO                PIC X(02) VALUE "90".

       01  FH-TABLE.
           05 FH-ENT OCCURS 20000 TIMES.
              10 T-FH-ACCT            PIC X(20).
              10 T-FH-AMT             PIC S9(13)V99 COMP-3.
              10 T-FH-USED            PIC X.
       01  STG-TABLE.
           05 STG-ENT OCCURS 20000 TIMES.
              10 T-STG-ACCT           PIC X(20).
              10 T-STG-AMT            PIC S9(13)V99 COMP-3.
              10 T-STG-USED           PIC X.
       01  DW-TABLE.
           05 DW-ENT OCCURS 20000 TIMES.
              10 T-DW-ACCT            PIC X(20).
              10 T-DW-AMT             PIC S9(13)V99 COMP-3.
              10 T-DW-USED            PIC X.

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE 0 TO RETURN-CODE
           PERFORM 0100-INIT
           IF NOT HARD-ERROR
              PERFORM 1000-LOAD-FH
           END-IF
           IF NOT HARD-ERROR
              PERFORM 2000-LOAD-STG
           END-IF
           IF NOT HARD-ERROR
              PERFORM 3000-LOAD-DW
           END-IF
           IF NOT HARD-ERROR
              PERFORM 4000-RECONCILE
           END-IF
           PERFORM 9000-CLOSE
           IF HARD-ERROR
              MOVE 12 TO RETURN-CODE
           ELSE
              IF WS-STOP-MART = "Y"
                 MOVE 8 TO RETURN-CODE
              ELSE
                 MOVE 0 TO RETURN-CODE
              END-IF
           END-IF
           GOBACK.

       0100-INIT.
           ACCEPT WS-BUSINESS-DT FROM DATE YYYYMMDD
           ACCEPT WS-TIME-NOW FROM TIME
           INITIALIZE FH-TABLE STG-TABLE DW-TABLE
           OPEN INPUT KZFEEHF JHSTGF JHDWHF
           IF FS-KZFEEHF NOT = "00"
              DISPLAY "KZFEEHF オープン失敗 ST=" FS-KZFEEHF
              SET HARD-ERROR TO TRUE
           END-IF
           IF FS-JHSTGF NOT = "00"
              DISPLAY "JHSTGF オープン失敗 ST=" FS-JHSTGF
              SET HARD-ERROR TO TRUE
           END-IF
           IF FS-JHDWHF NOT = "00"
              DISPLAY "JHDWHF オープン失敗 ST=" FS-JHDWHF
              SET HARD-ERROR TO TRUE
           END-IF
           OPEN I-O JHRECMF
           IF FS-JHRECMF = "35"
              OPEN OUTPUT JHRECMF
           END-IF
           IF FS-JHRECMF NOT = "00"
              DISPLAY "JHRECMF オープン失敗 ST=" FS-JHRECMF
              SET HARD-ERROR TO TRUE
           END-IF
           OPEN EXTEND JHAUDTF
           IF FS-JHAUDTF = "35"
              OPEN OUTPUT JHAUDTF
           END-IF
           IF FS-JHAUDTF NOT = "00"
              DISPLAY "JHAUDTF オープン失敗 ST=" FS-JHAUDTF
              SET HARD-ERROR TO TRUE
           END-IF.

       1000-LOAD-FH.
           PERFORM UNTIL KZFEEHF-END OR HARD-ERROR
              READ KZFEEHF
                 AT END
                    SET KZFEEHF-END TO TRUE
                 NOT AT END
                    IF FS-KZFEEHF NOT = "00"
                       DISPLAY "KZFEEHF 読込失敗 ST=" FS-KZFEEHF
                       SET HARD-ERROR TO TRUE
                    ELSE
                       PERFORM 1100-EDIT-FH
                    END-IF
              END-READ
           END-PERFORM
           PERFORM 8100-WRITE-AUDIT-FH.

       1100-EDIT-FH.
           IF FH-CYCLE-DT = WS-BUSINESS-DT
              IF FH-ACCT-NO = SPACES
                 DISPLAY "KZFEEHF 口座番号不正"
                 SET HARD-ERROR TO TRUE
              ELSE
                 ADD 1 TO FH-CNT
                 ADD FH-FEE-AMT TO FH-AMT
                 IF FH-CNT > WS-MAX-REC
                    DISPLAY "KZFEEHF 件数上限超過"
                    SET HARD-ERROR TO TRUE
                 ELSE
                    MOVE FH-ACCT-NO TO T-FH-ACCT(FH-CNT)
                    MOVE FH-FEE-AMT TO T-FH-AMT(FH-CNT)
                    MOVE "N" TO T-FH-USED(FH-CNT)
                    PERFORM 1200-CHECK-FH-DUP
                 END-IF
              END-IF
           END-IF.

       1200-CHECK-FH-DUP.
           PERFORM VARYING IX FROM 1 BY 1 UNTIL IX >= FH-CNT
              IF T-FH-ACCT(IX) = FH-ACCT-NO
                 ADD 1 TO DUP-CNT
                 MOVE JUDGE-DUP TO STG-JUDGE-CD DW-JUDGE-CD
                 DISPLAY "KZFEEHF キー重複 ACCT=" FH-ACCT-NO
              END-IF
           END-PERFORM.

       2000-LOAD-STG.
           PERFORM UNTIL JHSTGF-END OR HARD-ERROR
              READ JHSTGF
                 AT END
                    SET JHSTGF-END TO TRUE
                 NOT AT END
                    IF FS-JHSTGF NOT = "00"
                       DISPLAY "JHSTGF 読込失敗 ST=" FS-JHSTGF
                       SET HARD-ERROR TO TRUE
                    ELSE
                       PERFORM 2100-EDIT-STG
                    END-IF
              END-READ
           END-PERFORM
           PERFORM 8200-WRITE-AUDIT-STG.

       2100-EDIT-STG.
           IF STG-CYCLE-DT = WS-BUSINESS-DT
              AND STG-EDIT-STATUS = "0"
              IF STG-ACCT-NO = SPACES
                 DISPLAY "JHSTGF 口座番号不正"
                 SET HARD-ERROR TO TRUE
              ELSE
                 ADD 1 TO STG-CNT
                 ADD STG-FEE-AMT TO STG-AMT
                 IF STG-CNT > WS-MAX-REC
                    DISPLAY "JHSTGF 件数上限超過"
                    SET HARD-ERROR TO TRUE
                 ELSE
                    MOVE STG-ACCT-NO TO T-STG-ACCT(STG-CNT)
                    MOVE STG-FEE-AMT TO T-STG-AMT(STG-CNT)
                    MOVE "N" TO T-STG-USED(STG-CNT)
                    PERFORM 2200-CHECK-STG-DUP
                 END-IF
              END-IF
           END-IF.

       2200-CHECK-STG-DUP.
           PERFORM VARYING IX FROM 1 BY 1 UNTIL IX >= STG-CNT
              IF T-STG-ACCT(IX) = STG-ACCT-NO
                 ADD 1 TO DUP-CNT
                 MOVE JUDGE-DUP TO STG-JUDGE-CD
                 DISPLAY "JHSTGF キー重複 ACCT=" STG-ACCT-NO
              END-IF
           END-PERFORM.

       3000-LOAD-DW.
           PERFORM UNTIL JHDWHF-END OR HARD-ERROR
              READ JHDWHF
                 AT END
                    SET JHDWHF-END TO TRUE
                 NOT AT END
                    IF FS-JHDWHF NOT = "00"
                       DISPLAY "JHDWHF 読込失敗 ST=" FS-JHDWHF
                       SET HARD-ERROR TO TRUE
                    ELSE
                       PERFORM 3100-EDIT-DW
                    END-IF
              END-READ
           END-PERFORM
           PERFORM 8300-WRITE-AUDIT-DW.

       3100-EDIT-DW.
           IF DW-CYCLE-DT = WS-BUSINESS-DT
              IF DW-ACCT-NO = SPACES
                 DISPLAY "JHDWHF 口座番号不正"
                 SET HARD-ERROR TO TRUE
              ELSE
                 ADD 1 TO DW-CNT
                 ADD DW-FEE-AMT TO DW-AMT
                 IF DW-CNT > WS-MAX-REC
                    DISPLAY "JHDWHF 件数上限超過"
                    SET HARD-ERROR TO TRUE
                 ELSE
                    MOVE DW-ACCT-NO TO T-DW-ACCT(DW-CNT)
                    MOVE DW-FEE-AMT TO T-DW-AMT(DW-CNT)
                    MOVE "N" TO T-DW-USED(DW-CNT)
                    PERFORM 3200-CHECK-DW-DUP
                 END-IF
              END-IF
           END-IF.

       3200-CHECK-DW-DUP.
           PERFORM VARYING IX FROM 1 BY 1 UNTIL IX >= DW-CNT
              IF T-DW-ACCT(IX) = DW-ACCT-NO
                 ADD 1 TO DUP-CNT
                 MOVE JUDGE-DUP TO DW-JUDGE-CD
                 DISPLAY "JHDWHF キー重複 ACCT=" DW-ACCT-NO
              END-IF
           END-PERFORM.

       4000-RECONCILE.
           PERFORM 4100-COMPARE-STG
           PERFORM 4200-COMPARE-DW
           COMPUTE STG-DIFF-CNT = FH-CNT - STG-CNT
           COMPUTE STG-DIFF-AMT = FH-AMT - STG-AMT
           COMPUTE DW-DIFF-CNT = FH-CNT - DW-CNT
           COMPUTE DW-DIFF-AMT = FH-AMT - DW-AMT
           PERFORM 4300-DECIDE-STG
           PERFORM 4400-DECIDE-DW
           PERFORM 8500-WRITE-RECON-STG
           PERFORM 8600-WRITE-RECON-DW.

       4100-COMPARE-STG.
           PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > FH-CNT
              MOVE "N" TO WS-FOUND
              PERFORM VARYING JX FROM 1 BY 1
                 UNTIL JX > STG-CNT OR WS-FOUND = "Y"
                 IF T-FH-ACCT(IX) = T-STG-ACCT(JX)
                    MOVE "Y" TO WS-FOUND
                    MOVE "Y" TO T-FH-USED(IX)
                    MOVE "Y" TO T-STG-USED(JX)
                    IF T-FH-AMT(IX) NOT = T-STG-AMT(JX)
                       ADD 1 TO AMT-NG-CNT
                       MOVE JUDGE-AMT TO STG-JUDGE-CD
                       DISPLAY "STG 金額不一致 ACCT=" T-FH-ACCT(IX)
                    END-IF
                 END-IF
              END-PERFORM
              IF WS-FOUND NOT = "Y"
                 ADD 1 TO MISS-CNT
                 MOVE JUDGE-MISS TO STG-JUDGE-CD
                 DISPLAY "STG 欠落 ACCT=" T-FH-ACCT(IX)
              END-IF
           END-PERFORM
           PERFORM VARYING JX FROM 1 BY 1 UNTIL JX > STG-CNT
              IF T-STG-USED(JX) NOT = "Y"
                 ADD 1 TO MISS-CNT
                 MOVE JUDGE-MISS TO STG-JUDGE-CD
                 DISPLAY "STG 余剰 ACCT=" T-STG-ACCT(JX)
              END-IF
           END-PERFORM.

       4200-COMPARE-DW.
           PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > FH-CNT
              MOVE "N" TO WS-FOUND
              PERFORM VARYING JX FROM 1 BY 1
                 UNTIL JX > DW-CNT OR WS-FOUND = "Y"
                 IF T-FH-ACCT(IX) = T-DW-ACCT(JX)
                    MOVE "Y" TO WS-FOUND
                    MOVE "Y" TO T-DW-USED(JX)
                    IF T-FH-AMT(IX) NOT = T-DW-AMT(JX)
                       ADD 1 TO AMT-NG-CNT
                       MOVE JUDGE-AMT TO DW-JUDGE-CD
                       DISPLAY "DWH 金額不一致 ACCT=" T-FH-ACCT(IX)
                    END-IF
                 END-IF
              END-PERFORM
              IF WS-FOUND NOT = "Y"
                 ADD 1 TO MISS-CNT
                 MOVE JUDGE-MISS TO DW-JUDGE-CD
                 DISPLAY "DWH 欠落 ACCT=" T-FH-ACCT(IX)
              END-IF
           END-PERFORM
           PERFORM VARYING JX FROM 1 BY 1 UNTIL JX > DW-CNT
              IF T-DW-USED(JX) NOT = "Y"
                 ADD 1 TO MISS-CNT
                 MOVE JUDGE-MISS TO DW-JUDGE-CD
                 DISPLAY "DWH 余剰 ACCT=" T-DW-ACCT(JX)
              END-IF
           END-PERFORM.

       4300-DECIDE-STG.
           IF STG-JUDGE-CD = JUDGE-OK
              IF STG-DIFF-CNT NOT = 0
                 MOVE JUDGE-CNT TO STG-JUDGE-CD
              END-IF
              IF STG-DIFF-AMT NOT = 0
                 MOVE JUDGE-AMT TO STG-JUDGE-CD
              END-IF
           END-IF
           IF STG-DIFF-CNT > WS-TOL-CNT
              MOVE "Y" TO WS-STOP-MART
           END-IF
           IF STG-DIFF-CNT < ZERO
              IF STG-DIFF-CNT < WS-TOL-CNT
                 MOVE "Y" TO WS-STOP-MART
              END-IF
           END-IF
           IF STG-DIFF-AMT > WS-TOL-AMT
              MOVE "Y" TO WS-STOP-MART
           END-IF
           IF STG-DIFF-AMT < ZERO
              IF STG-DIFF-AMT < WS-TOL-AMT
                 MOVE "Y" TO WS-STOP-MART
              END-IF
           END-IF.

       4400-DECIDE-DW.
           IF DW-JUDGE-CD = JUDGE-OK
              IF DW-DIFF-CNT NOT = 0
                 MOVE JUDGE-CNT TO DW-JUDGE-CD
              END-IF
              IF DW-DIFF-AMT NOT = 0
                 MOVE JUDGE-AMT TO DW-JUDGE-CD
              END-IF
           END-IF
           IF DW-DIFF-CNT NOT = 0
              MOVE "Y" TO WS-STOP-MART
           END-IF
           IF DW-DIFF-AMT NOT = 0
              MOVE "Y" TO WS-STOP-MART
           END-IF.

       8100-WRITE-AUDIT-FH.
           IF NOT HARD-ERROR
              MOVE "KZFEEHF" TO AUD-DATASET-ID
              MOVE FH-CNT TO AUD-REC-CNT
              MOVE FH-AMT TO AUD-AMT-TOTAL
              MOVE "読込" TO AUD-EVENT-CD
              PERFORM 8900-WRITE-AUDIT
           END-IF.

       8200-WRITE-AUDIT-STG.
           IF NOT HARD-ERROR
              MOVE "JHSTGF" TO AUD-DATASET-ID
              MOVE STG-CNT TO AUD-REC-CNT
              MOVE STG-AMT TO AUD-AMT-TOTAL
              MOVE "読込" TO AUD-EVENT-CD
              PERFORM 8900-WRITE-AUDIT
           END-IF.

       8300-WRITE-AUDIT-DW.
           IF NOT HARD-ERROR
              MOVE "JHDWHF" TO AUD-DATASET-ID
              MOVE DW-CNT TO AUD-REC-CNT
              MOVE DW-AMT TO AUD-AMT-TOTAL
              MOVE "読込" TO AUD-EVENT-CD
              PERFORM 8900-WRITE-AUDIT
           END-IF.

       8500-WRITE-RECON-STG.
           MOVE SPACES TO JHRECMF-REC
           STRING WS-BUSINESS-DT "STG" DELIMITED BY SIZE
              INTO REC-RECON-KEY
           END-STRING
           MOVE WS-BUSINESS-DT TO REC-BUSINESS-DT
           MOVE FH-CNT TO REC-SOURCE-CNT
           MOVE STG-CNT TO REC-TARGET-CNT
           MOVE FH-AMT TO REC-SOURCE-AMT
           MOVE STG-AMT TO REC-TARGET-AMT
           MOVE STG-DIFF-CNT TO REC-DIFF-CNT
           MOVE STG-DIFF-AMT TO REC-DIFF-AMT
           MOVE STG-JUDGE-CD TO REC-JUDGE-CD
           WRITE JHRECMF-REC
              INVALID KEY
                 REWRITE JHRECMF-REC
                    INVALID KEY
                       DISPLAY "JHRECMF STG 書込失敗 ST=" FS-JHRECMF
                       SET HARD-ERROR TO TRUE
                 END-REWRITE
           END-WRITE.

       8600-WRITE-RECON-DW.
           MOVE SPACES TO JHRECMF-REC
           STRING WS-BUSINESS-DT "DWH" DELIMITED BY SIZE
              INTO REC-RECON-KEY
           END-STRING
           MOVE WS-BUSINESS-DT TO REC-BUSINESS-DT
           MOVE FH-CNT TO REC-SOURCE-CNT
           MOVE DW-CNT TO REC-TARGET-CNT
           MOVE FH-AMT TO REC-SOURCE-AMT
           MOVE DW-AMT TO REC-TARGET-AMT
           MOVE DW-DIFF-CNT TO REC-DIFF-CNT
           MOVE DW-DIFF-AMT TO REC-DIFF-AMT
           MOVE DW-JUDGE-CD TO REC-JUDGE-CD
           WRITE JHRECMF-REC
              INVALID KEY
                 REWRITE JHRECMF-REC
                    INVALID KEY
                       DISPLAY "JHRECMF DWH 書込失敗 ST=" FS-JHRECMF
                       SET HARD-ERROR TO TRUE
                 END-REWRITE
           END-WRITE.

       8900-WRITE-AUDIT.
           ADD 1 TO WS-AUDIT-SEQ
           MOVE WS-AUDIT-SEQ TO AUD-AUDIT-SEQ
           MOVE WS-JOB-ID TO AUD-JOB-ID
           MOVE WS-BUSINESS-DT TO AUD-BUSINESS-DT
           MOVE WS-TIME-NOW TO AUD-EVENT-TS
           WRITE JHAUDTF-REC
           IF FS-JHAUDTF NOT = "00"
              DISPLAY "JHAUDTF 書込失敗 ST=" FS-JHAUDTF
              SET HARD-ERROR TO TRUE
           END-IF.

       9000-CLOSE.
           CLOSE KZFEEHF JHSTGF JHDWHF JHRECMF JHAUDTF
           IF FS-KZFEEHF NOT = "00" AND FS-KZFEEHF NOT = "42"
              DISPLAY "KZFEEHF クローズ失敗 ST=" FS-KZFEEHF
              SET HARD-ERROR TO TRUE
           END-IF
           IF FS-JHSTGF NOT = "00" AND FS-JHSTGF NOT = "42"
              DISPLAY "JHSTGF クローズ失敗 ST=" FS-JHSTGF
              SET HARD-ERROR TO TRUE
           END-IF
           IF FS-JHDWHF NOT = "00" AND FS-JHDWHF NOT = "42"
              DISPLAY "JHDWHF クローズ失敗 ST=" FS-JHDWHF
              SET HARD-ERROR TO TRUE
           END-IF
           IF FS-JHRECMF NOT = "00" AND FS-JHRECMF NOT = "42"
              DISPLAY "JHRECMF クローズ失敗 ST=" FS-JHRECMF
              SET HARD-ERROR TO TRUE
           END-IF
           IF FS-JHAUDTF NOT = "00" AND FS-JHAUDTF NOT = "42"
              DISPLAY "JHAUDTF クローズ失敗 ST=" FS-JHAUDTF
              SET HARD-ERROR TO TRUE
           END-IF.
