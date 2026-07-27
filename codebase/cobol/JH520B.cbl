       IDENTIFICATION DIVISION.
       PROGRAM-ID. JH520B.
      ******************************************************************
      * 変更履歴
      * 版数  年月日    担当    概要
      * 01.00 20240115  JH開発  初版作成
      * 01.01 20240410  JH保守  照合差分時の監査出力追加
      * 01.02 20240620  JH保守  JH525S問合せによる抑止判定追加
      ******************************************************************
      * 収益レポーティングマート作成
      * DWH手数料明細へ口座属性と月次分類値を付加する。
      * 照合未判定または重大差分はJH525Sへ問合せ、
      * 抑止時は監査のみ残す。
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GnuCOBOL.
       OBJECT-COMPUTER. GnuCOBOL.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT JHDWHF ASSIGN TO "JHDWHF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-JHDWHF.
           SELECT JHACDMF ASSIGN TO "JHACDMF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS ACD-ACCT-NO
               FILE STATUS IS FS-JHACDMF.
           SELECT JHMONRF ASSIGN TO "JHMONRF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-JHMONRF.
           SELECT JHRECMF ASSIGN TO "JHRECMF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS REC-RECON-KEY
               FILE STATUS IS FS-JHRECMF.
           SELECT JHMARTF ASSIGN TO "JHMARTF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-JHMARTF.
           SELECT JHAUDTF ASSIGN TO "JHAUDTF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS AUD-AUDIT-SEQ
               FILE STATUS IS FS-JHAUDTF.

       DATA DIVISION.
       FILE SECTION.
       FD  JHDWHF.
       COPY JHDWHFC.
       FD  JHACDMF.
       COPY JHACDC.
       FD  JHMONRF.
       COPY JHMONC.
       FD  JHRECMF.
       COPY JHRECC.
       FD  JHMARTF.
       COPY JHMRTC.
       FD  JHAUDTF.
       COPY JHAUDC.

       WORKING-STORAGE SECTION.
       01  FS-AREA.
           05 FS-JHDWHF              PIC XX VALUE SPACE.
           05 FS-JHACDMF             PIC XX VALUE SPACE.
           05 FS-JHMONRF             PIC XX VALUE SPACE.
           05 FS-JHRECMF             PIC XX VALUE SPACE.
           05 FS-JHMARTF             PIC XX VALUE SPACE.
           05 FS-JHAUDTF             PIC XX VALUE SPACE.

       01  SW-AREA.
           05 SW-EOF-DWH             PIC X VALUE "N".
              88 EOF-DWH                  VALUE "Y".
           05 SW-EOF-MON             PIC X VALUE "N".
              88 EOF-MON                  VALUE "Y".
           05 SW-HARD-ERR            PIC X VALUE "N".
              88 HARD-ERR                 VALUE "Y".
           05 SW-MON-FOUND           PIC X VALUE "N".
              88 MON-FOUND                VALUE "Y".
           05 SW-ACD-FOUND           PIC X VALUE "N".
              88 ACD-FOUND                VALUE "Y".
           05 SW-REC-FOUND           PIC X VALUE "N".
              88 REC-FOUND                VALUE "Y".
           05 SW-SUPPRESS            PIC X VALUE "N".
              88 SUPPRESS-REC             VALUE "Y".
           05 SW-NEED-INQUIRY        PIC X VALUE "N".
              88 NEED-INQUIRY             VALUE "Y".

       01  CNT-AREA.
           05 CNT-DWH-READ           PIC 9(09) VALUE ZERO.
           05 CNT-MON-READ           PIC 9(09) VALUE ZERO.
           05 CNT-MART-WRITE         PIC 9(09) VALUE ZERO.
           05 CNT-AUD-WRITE          PIC 9(09) VALUE ZERO.
           05 CNT-SUPPRESS           PIC 9(09) VALUE ZERO.
           05 CNT-SKIP               PIC 9(09) VALUE ZERO.

       01  WK-AREA.
           05 WK-JOB-ID              PIC X(08) VALUE "JH520B".
           05 WK-DATE-8              PIC 9(08) VALUE ZERO.
           05 WK-TIME-8              PIC 9(08) VALUE ZERO.
           05 WK-BUSINESS-DT         PIC 9(08) VALUE ZERO.
           05 WK-YYYYMM              PIC 9(06) VALUE ZERO.
           05 WK-MON-KEY             PIC X(16) VALUE SPACE.
           05 WK-REC-KEY             PIC X(24) VALUE SPACE.
           05 WK-MART-KEY            PIC X(32) VALUE SPACE.
           05 WK-AUDIT-SEQ           PIC 9(12) VALUE ZERO.
           05 WK-ABS-DIFF-AMT        PIC S9(13)V99 VALUE ZERO.
           05 WK-ABS-DIFF-CNT        PIC S9(09) VALUE ZERO.
           05 WK-REASON-CD           PIC X(10) VALUE SPACE.
           05 WK-CALL-STATUS         PIC X(02) VALUE SPACE.

       01  MON-TABLE-AREA.
           05 MON-ENTRY OCCURS 500 TIMES.
              10 T-MON-YYYYMM        PIC 9(06).
              10 T-MON-PROD          PIC X(04).
              10 T-MON-BRANCH        PIC X(03).
              10 T-MON-AMT-SUM       PIC S9(13)V99.
              10 T-MON-YTD-SUM       PIC S9(13)V99.
              10 T-MON-ACCT-CNT      PIC 9(09).
              10 T-MON-ADJ-CNT       PIC 9(09).
           05 MON-IDX                PIC 9(04) VALUE ZERO.
           05 MON-MAX                PIC 9(04) VALUE ZERO.

       01  JH525S-AREA.
           05 JH525S-FUNC-CD         PIC X(02) VALUE SPACE.
           05 JH525S-ACCT-NO         PIC X(16) VALUE SPACE.
           05 JH525S-BUSINESS-DT     PIC 9(08) VALUE ZERO.
           05 JH525S-YYYYMM          PIC 9(06) VALUE ZERO.
           05 JH525S-PRODUCT-CD      PIC X(04) VALUE SPACE.
           05 JH525S-BRANCH-CD       PIC X(03) VALUE SPACE.
           05 JH525S-RECON-KEY       PIC X(24) VALUE SPACE.
           05 JH525S-JUDGE-CD        PIC X(01) VALUE SPACE.
           05 JH525S-DIFF-CNT        PIC S9(09) VALUE ZERO.
           05 JH525S-DIFF-AMT        PIC S9(13)V99 VALUE ZERO.
           05 JH525S-FEE-AMT         PIC S9(13)V99 VALUE ZERO.
           05 JH525S-FEE-YTD         PIC S9(13)V99 VALUE ZERO.
           05 JH525S-RESULT-CD       PIC X(01) VALUE SPACE.
           05 JH525S-STATUS-CD       PIC X(02) VALUE SPACE.
           05 JH525S-REASON-CD       PIC X(10) VALUE SPACE.

       PROCEDURE DIVISION.
       MAIN-RTN.
           MOVE 0 TO RETURN-CODE
           PERFORM 1000-INIT
           IF NOT HARD-ERR
              PERFORM 2000-LOAD-MON
              PERFORM 3000-PROCESS-DWH UNTIL EOF-DWH OR HARD-ERR
           END-IF
           PERFORM 9000-FINAL
           GOBACK.

       1000-INIT.
           ACCEPT WK-DATE-8 FROM DATE YYYYMMDD
           ACCEPT WK-TIME-8 FROM TIME
           MOVE WK-DATE-8 TO WK-BUSINESS-DT

           OPEN INPUT JHDWHF
           IF FS-JHDWHF NOT = "00"
              DISPLAY "JHDWHF オープン失敗 ST=" FS-JHDWHF
              SET HARD-ERR TO TRUE
              MOVE 12 TO RETURN-CODE
              EXIT PARAGRAPH
           END-IF

           OPEN INPUT JHACDMF
           IF FS-JHACDMF NOT = "00"
              DISPLAY "JHACDMF オープン失敗 ST=" FS-JHACDMF
              SET HARD-ERR TO TRUE
              MOVE 12 TO RETURN-CODE
              EXIT PARAGRAPH
           END-IF

           OPEN INPUT JHMONRF
           IF FS-JHMONRF NOT = "00"
              DISPLAY "JHMONRF オープン失敗 ST=" FS-JHMONRF
              SET HARD-ERR TO TRUE
              MOVE 12 TO RETURN-CODE
              EXIT PARAGRAPH
           END-IF

           OPEN INPUT JHRECMF
           IF FS-JHRECMF NOT = "00"
              DISPLAY "JHRECMF オープン失敗 ST=" FS-JHRECMF
              SET HARD-ERR TO TRUE
              MOVE 12 TO RETURN-CODE
              EXIT PARAGRAPH
           END-IF

           OPEN OUTPUT JHMARTF
           IF FS-JHMARTF NOT = "00"
              DISPLAY "JHMARTF オープン失敗 ST=" FS-JHMARTF
              SET HARD-ERR TO TRUE
              MOVE 12 TO RETURN-CODE
              EXIT PARAGRAPH
           END-IF

           OPEN OUTPUT JHAUDTF
           IF FS-JHAUDTF NOT = "00"
              DISPLAY "JHAUDTF オープン失敗 ST=" FS-JHAUDTF
              SET HARD-ERR TO TRUE
              MOVE 12 TO RETURN-CODE
              EXIT PARAGRAPH
           END-IF

           PERFORM 2100-READ-DWH.

       2000-LOAD-MON.
           PERFORM UNTIL EOF-MON OR HARD-ERR
              READ JHMONRF
                 AT END
                    SET EOF-MON TO TRUE
                 NOT AT END
                    ADD 1 TO CNT-MON-READ
                    IF MON-MAX >= 500
                       DISPLAY "月次分類表件数超過"
                       SET HARD-ERR TO TRUE
                       MOVE 12 TO RETURN-CODE
                    ELSE
                       ADD 1 TO MON-MAX
                       MOVE MON-YYYYMM
                         TO T-MON-YYYYMM(MON-MAX)
                       MOVE MON-TRUST-PRODUCT-CD
                         TO T-MON-PROD(MON-MAX)
                       MOVE MON-BRANCH-CD
                         TO T-MON-BRANCH(MON-MAX)
                       MOVE MON-FEE-AMT-SUM
                         TO T-MON-AMT-SUM(MON-MAX)
                       MOVE MON-FEE-YTD-SUM
                         TO T-MON-YTD-SUM(MON-MAX)
                       MOVE MON-ACCT-CNT
                         TO T-MON-ACCT-CNT(MON-MAX)
                       MOVE MON-ADJUST-CNT
                         TO T-MON-ADJ-CNT(MON-MAX)
                    END-IF
              END-READ
              IF FS-JHMONRF NOT = "00" AND FS-JHMONRF NOT = "10"
                 DISPLAY "JHMONRF 読込失敗 ST=" FS-JHMONRF
                 SET HARD-ERR TO TRUE
                 MOVE 12 TO RETURN-CODE
              END-IF
           END-PERFORM.

       2100-READ-DWH.
           READ JHDWHF
              AT END
                 SET EOF-DWH TO TRUE
              NOT AT END
                 ADD 1 TO CNT-DWH-READ
           END-READ
           IF FS-JHDWHF NOT = "00" AND FS-JHDWHF NOT = "10"
              DISPLAY "JHDWHF 読込失敗 ST=" FS-JHDWHF
              SET HARD-ERR TO TRUE
              MOVE 12 TO RETURN-CODE
           END-IF.

       3000-PROCESS-DWH.
           MOVE "N" TO SW-SUPPRESS SW-NEED-INQUIRY
           MOVE SPACE TO WK-REASON-CD

           IF DW-ACCT-NO = SPACE
              MOVE "ACCT-BLANK" TO WK-REASON-CD
              PERFORM 7600-WRITE-AUDIT
              ADD 1 TO CNT-SKIP
              PERFORM 2100-READ-DWH
              EXIT PARAGRAPH
           END-IF

           IF DW-FEE-AMT < ZERO OR DW-FEE-YTD < ZERO
              MOVE "FEE-MINUS" TO WK-REASON-CD
              PERFORM 7600-WRITE-AUDIT
              ADD 1 TO CNT-SKIP
              PERFORM 2100-READ-DWH
              EXIT PARAGRAPH
           END-IF

           COMPUTE WK-YYYYMM = FUNCTION INTEGER(DW-CYCLE-DT / 100)

           PERFORM 4100-READ-ACD
           IF NOT ACD-FOUND
              MOVE "ACD-NONE" TO WK-REASON-CD
              PERFORM 7600-WRITE-AUDIT
              ADD 1 TO CNT-SKIP
              PERFORM 2100-READ-DWH
              EXIT PARAGRAPH
           END-IF

           IF ACD-STATUS-CD NOT = "1" AND ACD-STATUS-CD NOT = "2"
              MOVE "ACD-STATUS" TO WK-REASON-CD
              PERFORM 7600-WRITE-AUDIT
              ADD 1 TO CNT-SKIP
              PERFORM 2100-READ-DWH
              EXIT PARAGRAPH
           END-IF

           IF ACD-CLOSE-DT NOT = ZERO
              AND ACD-CLOSE-DT < DW-CYCLE-DT
              MOVE "ACD-CLOSE" TO WK-REASON-CD
              PERFORM 7600-WRITE-AUDIT
              ADD 1 TO CNT-SKIP
              PERFORM 2100-READ-DWH
              EXIT PARAGRAPH
           END-IF

           PERFORM 4200-FIND-MON
           IF NOT MON-FOUND
              MOVE "MON-NONE" TO WK-REASON-CD
              PERFORM 7600-WRITE-AUDIT
              ADD 1 TO CNT-SKIP
              PERFORM 2100-READ-DWH
              EXIT PARAGRAPH
           END-IF

           PERFORM 4300-READ-REC
           IF NOT REC-FOUND
              MOVE "REC-NONE" TO WK-REASON-CD
              SET NEED-INQUIRY TO TRUE
           ELSE
              PERFORM 4400-CHECK-RECON
           END-IF

           IF NEED-INQUIRY
              PERFORM 5000-CALL-JH525S
           END-IF

           IF HARD-ERR
              EXIT PARAGRAPH
           END-IF

           IF SUPPRESS-REC
              ADD 1 TO CNT-SUPPRESS
              PERFORM 7600-WRITE-AUDIT
           ELSE
              PERFORM 7000-WRITE-MART
           END-IF

           PERFORM 2100-READ-DWH.

       4100-READ-ACD.
           MOVE "N" TO SW-ACD-FOUND
           MOVE DW-ACCT-NO TO ACD-ACCT-NO
           READ JHACDMF KEY IS ACD-ACCT-NO
              INVALID KEY
                 MOVE "N" TO SW-ACD-FOUND
              NOT INVALID KEY
                 SET ACD-FOUND TO TRUE
           END-READ
           IF FS-JHACDMF NOT = "00" AND FS-JHACDMF NOT = "23"
              DISPLAY "JHACDMF 読込失敗 ST=" FS-JHACDMF
              SET HARD-ERR TO TRUE
              MOVE 12 TO RETURN-CODE
           END-IF.

       4200-FIND-MON.
           MOVE "N" TO SW-MON-FOUND
           PERFORM VARYING MON-IDX FROM 1 BY 1
             UNTIL MON-IDX > MON-MAX OR MON-FOUND
              IF T-MON-YYYYMM(MON-IDX) = WK-YYYYMM
                 AND T-MON-PROD(MON-IDX) = ACD-TRUST-PRODUCT-CD
                 AND T-MON-BRANCH(MON-IDX) = ACD-BRANCH-CD
                    SET MON-FOUND TO TRUE
              END-IF
           END-PERFORM.

       4300-READ-REC.
           MOVE "N" TO SW-REC-FOUND
           MOVE SPACE TO WK-REC-KEY
           STRING WK-YYYYMM DELIMITED BY SIZE
                  ACD-TRUST-PRODUCT-CD DELIMITED BY SIZE
                  ACD-BRANCH-CD DELIMITED BY SIZE
             INTO WK-REC-KEY
           END-STRING
           MOVE WK-REC-KEY TO REC-RECON-KEY
           READ JHRECMF KEY IS REC-RECON-KEY
              INVALID KEY
                 MOVE "N" TO SW-REC-FOUND
              NOT INVALID KEY
                 SET REC-FOUND TO TRUE
           END-READ
           IF FS-JHRECMF NOT = "00" AND FS-JHRECMF NOT = "23"
              DISPLAY "JHRECMF 読込失敗 ST=" FS-JHRECMF
              SET HARD-ERR TO TRUE
              MOVE 12 TO RETURN-CODE
           END-IF.

       4400-CHECK-RECON.
           IF REC-JUDGE-CD = "0" OR REC-JUDGE-CD = "9"
              MOVE "REC-JUDGE" TO WK-REASON-CD
              SET NEED-INQUIRY TO TRUE
           END-IF

           COMPUTE WK-ABS-DIFF-CNT = FUNCTION ABS(REC-DIFF-CNT)
           COMPUTE WK-ABS-DIFF-AMT = FUNCTION ABS(REC-DIFF-AMT)

           IF WK-ABS-DIFF-CNT > 10
              OR WK-ABS-DIFF-AMT > 1000000
              MOVE "REC-DIFF" TO WK-REASON-CD
              SET NEED-INQUIRY TO TRUE
           END-IF.

       5000-CALL-JH525S.
           MOVE "01" TO JH525S-FUNC-CD
           MOVE DW-ACCT-NO TO JH525S-ACCT-NO
           MOVE DW-CYCLE-DT TO JH525S-BUSINESS-DT
           MOVE WK-YYYYMM TO JH525S-YYYYMM
           MOVE ACD-TRUST-PRODUCT-CD TO JH525S-PRODUCT-CD
           MOVE ACD-BRANCH-CD TO JH525S-BRANCH-CD
           MOVE WK-REC-KEY TO JH525S-RECON-KEY
           IF REC-FOUND
              MOVE REC-JUDGE-CD TO JH525S-JUDGE-CD
              MOVE REC-DIFF-CNT TO JH525S-DIFF-CNT
              MOVE REC-DIFF-AMT TO JH525S-DIFF-AMT
           ELSE
              MOVE "9" TO JH525S-JUDGE-CD
              MOVE ZERO TO JH525S-DIFF-CNT
              MOVE ZERO TO JH525S-DIFF-AMT
           END-IF
           MOVE DW-FEE-AMT TO JH525S-FEE-AMT
           MOVE DW-FEE-YTD TO JH525S-FEE-YTD
           MOVE SPACE TO JH525S-RESULT-CD
           MOVE SPACE TO JH525S-STATUS-CD
           MOVE WK-REASON-CD TO JH525S-REASON-CD

           CALL "JH525S" USING JH525S-AREA
           END-CALL

           MOVE JH525S-STATUS-CD TO WK-CALL-STATUS
           IF WK-CALL-STATUS NOT = "00"
              DISPLAY "JH525S 問合せ失敗 ST=" WK-CALL-STATUS
              MOVE "CALL-ERR" TO WK-REASON-CD
              SET HARD-ERR TO TRUE
              MOVE 12 TO RETURN-CODE
              EXIT PARAGRAPH
           END-IF

           EVALUATE JH525S-RESULT-CD
              WHEN "S"
                 SET SUPPRESS-REC TO TRUE
                 MOVE JH525S-REASON-CD TO WK-REASON-CD
              WHEN "O"
                 MOVE JH525S-REASON-CD TO WK-REASON-CD
              WHEN OTHER
                 DISPLAY "JH525S 応答区分不正 区分="
                         JH525S-RESULT-CD
                 MOVE "CALL-ANS" TO WK-REASON-CD
                 SET HARD-ERR TO TRUE
                 MOVE 12 TO RETURN-CODE
           END-EVALUATE.

       7000-WRITE-MART.
           INITIALIZE JHMARTF-REC
           MOVE SPACE TO WK-MART-KEY
           STRING WK-YYYYMM DELIMITED BY SIZE
                  DW-ACCT-NO DELIMITED BY SIZE
             INTO WK-MART-KEY
           END-STRING
           MOVE WK-MART-KEY TO MRT-MART-KEY
           MOVE WK-YYYYMM TO MRT-YYYYMM
           MOVE DW-ACCT-NO TO MRT-ACCT-NO
           MOVE ACD-CUSTOMER-ID TO MRT-CUSTOMER-ID
           MOVE ACD-TRUST-PRODUCT-CD TO MRT-TRUST-PRODUCT-CD
           MOVE ACD-BRANCH-CD TO MRT-BRANCH-CD
           MOVE DW-FEE-AMT TO MRT-FEE-AMT
           MOVE DW-FEE-YTD TO MRT-FEE-YTD
           MOVE WK-DATE-8 TO MRT-LOAD-DT

           WRITE JHMARTF-REC
           IF FS-JHMARTF NOT = "00"
              DISPLAY "JHMARTF 書込失敗 ST=" FS-JHMARTF
              SET HARD-ERR TO TRUE
              MOVE 12 TO RETURN-CODE
           ELSE
              ADD 1 TO CNT-MART-WRITE
           END-IF.

       7600-WRITE-AUDIT.
           ADD 1 TO WK-AUDIT-SEQ
           INITIALIZE JHAUDTF-REC
           MOVE WK-AUDIT-SEQ TO AUD-AUDIT-SEQ
           MOVE WK-JOB-ID TO AUD-JOB-ID
           MOVE DW-CYCLE-DT TO AUD-BUSINESS-DT
           IF SUPPRESS-REC
              MOVE "SUPPRESS" TO AUD-EVENT-CD
           ELSE
              MOVE WK-REASON-CD TO AUD-EVENT-CD
           END-IF
           MOVE "JHDWHF" TO AUD-DATASET-ID
           MOVE 1 TO AUD-REC-CNT
           MOVE DW-FEE-AMT TO AUD-AMT-TOTAL
           MOVE WK-TIME-8 TO AUD-EVENT-TS

           WRITE JHAUDTF-REC
           IF FS-JHAUDTF NOT = "00"
              DISPLAY "JHAUDTF 書込失敗 ST=" FS-JHAUDTF
              SET HARD-ERR TO TRUE
              MOVE 12 TO RETURN-CODE
           ELSE
              ADD 1 TO CNT-AUD-WRITE
           END-IF.

       9000-FINAL.
           IF FS-JHDWHF = "00" OR FS-JHDWHF = "10"
              CLOSE JHDWHF
           END-IF
           IF FS-JHACDMF = "00" OR FS-JHACDMF = "23"
              CLOSE JHACDMF
           END-IF
           IF FS-JHMONRF = "00" OR FS-JHMONRF = "10"
              CLOSE JHMONRF
           END-IF
           IF FS-JHRECMF = "00" OR FS-JHRECMF = "23"
              CLOSE JHRECMF
           END-IF
           IF FS-JHMARTF = "00"
              CLOSE JHMARTF
           END-IF
           IF FS-JHAUDTF = "00"
              CLOSE JHAUDTF
           END-IF

           IF HARD-ERR
              IF RETURN-CODE = 0
                 MOVE 8 TO RETURN-CODE
              END-IF
              DISPLAY "JH520B 異常終了 RC=" RETURN-CODE
           ELSE
              MOVE 0 TO RETURN-CODE
              DISPLAY "JH520B 正常終了"
              DISPLAY "DWH読込件数=" CNT-DWH-READ
              DISPLAY "月次読込件数=" CNT-MON-READ
              DISPLAY "マート出力件数=" CNT-MART-WRITE
              DISPLAY "監査出力件数=" CNT-AUD-WRITE
              DISPLAY "抑止件数=" CNT-SUPPRESS
              DISPLAY "除外件数=" CNT-SKIP
           END-IF.
