       IDENTIFICATION DIVISION.
       PROGRAM-ID. KZ590B.
      * 変更履歴
      * 版数  年月日(和暦)  担当                      概要
      * 1.00  H30.04.01    システム部 勘定系チーム  新規作成
      * 1.01  R02.10.15    システム部 勘定系チーム  延滞抽出条件見直し
      * 1.02  R05.06.01    システム部 勘定系チーム  報告書編集項目追加
       AUTHOR. KZ-BATCH.
      *延滞報告書作成バッチ
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZDLRF ASSIGN TO "KZDLRF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-KZDLRF.
           SELECT KZLLAF ASSIGN TO "KZLLAF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS LA-ACCT-NO
               FILE STATUS IS FS-KZLLAF.

       DATA DIVISION.
       FILE SECTION.
       FD  KZDLRF.
           COPY KZDLRFC.
       FD  KZLLAF.
           COPY KZLLACF.

       WORKING-STORAGE SECTION.
       01  FS-AREA.
           05 FS-KZDLRF              PIC X(02) VALUE SPACE.
           05 FS-KZLLAF              PIC X(02) VALUE SPACE.

       01  SW-AREA.
           05 SW-EOF                 PIC X VALUE "N".
              88 EOF-KZDLRF               VALUE "Y".
           05 SW-HARD-ERR            PIC X VALUE "N".
              88 HARD-ERR                 VALUE "Y".

       01  SYSIN-AREA.
           05 SYSIN-LINE             PIC X(80) VALUE SPACE.
           05 WS-THRESHOLD           PIC S9(15)V99 COMP-3 VALUE ZERO.

       01  CONST-AREA.
           05 CN-MILLION             PIC 9(09) VALUE 1000000.
           05 CN-PGM                 PIC X(08) VALUE "KZ590B".
           05 CN-RPT-ID              PIC X(08) VALUE "FSA-LATE".

       01  WORK-AREA.
           05 WS-BKT-IDX             PIC 9 VALUE ZERO.
           05 WS-PRD-IDX             PIC 9 VALUE ZERO.
           05 WS-AMT-WK              PIC S9(15)V99 COMP-3 VALUE ZERO.
           05 WS-MIL-WK              PIC S9(15) COMP-3 VALUE ZERO.
           05 WS-REM-WK              PIC S9(15)V99 COMP-3 VALUE ZERO.
           05 WS-COV-WK              PIC S9(07)V9999 COMP-3 VALUE ZERO.
           05 WS-HASH-TOTAL          PIC 9(18) COMP-3 VALUE ZERO.
           05 WS-READ-CNT            PIC 9(09) COMP-3 VALUE ZERO.
           05 WS-SKIP-CNT            PIC 9(09) COMP-3 VALUE ZERO.
           05 WS-OUT-CNT             PIC 9(09) COMP-3 VALUE ZERO.
           05 WS-ERR-CNT             PIC 9(09) COMP-3 VALUE ZERO.
           05 WS-PROD-CD             PIC X(02) VALUE SPACE.
           05 WS-AGING-CD            PIC X(02) VALUE SPACE.
           05 WS-REPORT-DATE         PIC 9(08) VALUE ZERO.

       01  LABEL-TABLE.
           05 BKT-LABEL-VALUES       PIC X(08) VALUE "B1B2B3B4".
           05 BKT-LABELS REDEFINES BKT-LABEL-VALUES.
              10 BKT-CODE            PIC X(02) OCCURS 4.
           05 PRD-LABEL-VALUES       PIC X(08) VALUE "LNTRBGOT".
           05 PRD-LABELS REDEFINES PRD-LABEL-VALUES.
              10 PRD-CODE            PIC X(02) OCCURS 4.

       01  SUM-TABLE.
           05 SUM-BKT OCCURS 4.
              10 SUM-PRD OCCURS 4.
                 15 SUM-LATE-AMT     PIC S9(15)V99 COMP-3
                                      VALUE ZERO.
                 15 SUM-ALLOW-AMT    PIC S9(15)V99 COMP-3
                                      VALUE ZERO.
                 15 SUM-ACCT-CNT     PIC 9(09) COMP-3 VALUE ZERO.

       01  OUT-REC.
           05 OUT-RPT-ID             PIC X(08).
           05 OUT-RPT-DATE           PIC 9(08).
           05 OUT-BUCKET             PIC X(02).
           05 OUT-PRODUCT            PIC X(02).
           05 OUT-ACCT-CNT           PIC 9(09).
           05 OUT-LATE-MIL           PIC 9(13).
           05 OUT-ALLOW-MIL          PIC 9(13).
           05 OUT-COVERAGE           PIC 9(05)V99.

       01  TRL-REC.
           05 TRL-ID                 PIC X(08).
           05 TRL-OUT-CNT            PIC 9(09).
           05 TRL-HASH-TOTAL         PIC 9(18).
           05 TRL-READ-CNT           PIC 9(09).
           05 TRL-SKIP-CNT           PIC 9(09).
           05 TRL-ERR-CNT            PIC 9(09).

       PROCEDURE DIVISION.
       MAIN-RTN.
           MOVE 0 TO RETURN-CODE
           PERFORM INIT-RTN
           IF NOT HARD-ERR
              PERFORM READ-DETAIL-RTN
                 UNTIL EOF-KZDLRF OR HARD-ERR
           END-IF
           IF NOT HARD-ERR
              PERFORM OUTPUT-REPORT-RTN
           END-IF
           PERFORM CLOSE-RTN
           IF HARD-ERR
              MOVE 8 TO RETURN-CODE
           ELSE
              MOVE 0 TO RETURN-CODE
           END-IF
           GOBACK.

       INIT-RTN.
           ACCEPT SYSIN-LINE
           IF SYSIN-LINE = SPACE
              DISPLAY CN-PGM " SYSINしきい値未指定"
              SET HARD-ERR TO TRUE
              EXIT PARAGRAPH
           END-IF
           COMPUTE WS-THRESHOLD = FUNCTION NUMVAL(SYSIN-LINE)
           IF WS-THRESHOLD < ZERO
              DISPLAY CN-PGM " SYSINしきい値不正"
              SET HARD-ERR TO TRUE
              EXIT PARAGRAPH
           END-IF
           ACCEPT WS-REPORT-DATE FROM DATE YYYYMMDD
           OPEN INPUT KZDLRF
           IF FS-KZDLRF NOT = "00"
              DISPLAY CN-PGM " KZDLRF オープン失敗 ST="
                      FS-KZDLRF
              SET HARD-ERR TO TRUE
              EXIT PARAGRAPH
           END-IF
           OPEN INPUT KZLLAF
           IF FS-KZLLAF NOT = "00"
              DISPLAY CN-PGM " KZLLAF オープン失敗 ST="
                      FS-KZLLAF
              SET HARD-ERR TO TRUE
           END-IF.

       READ-DETAIL-RTN.
           READ KZDLRF
              AT END
                 SET EOF-KZDLRF TO TRUE
              NOT AT END
                 ADD 1 TO WS-READ-CNT
                 PERFORM VALIDATE-DETAIL-RTN
                 IF NOT HARD-ERR
                    IF WS-BKT-IDX > ZERO AND WS-PRD-IDX > ZERO
                       PERFORM JOIN-ALLOWANCE-RTN
                       IF NOT HARD-ERR
                          PERFORM ACCUMULATE-RTN
                       END-IF
                    ELSE
                       ADD 1 TO WS-SKIP-CNT
                    END-IF
                 END-IF
           END-READ
           IF FS-KZDLRF NOT = "00" AND FS-KZDLRF NOT = "10"
              DISPLAY CN-PGM " KZDLRF 読込失敗 ST="
                      FS-KZDLRF
              SET HARD-ERR TO TRUE
           END-IF.

       VALIDATE-DETAIL-RTN.
           MOVE ZERO TO WS-BKT-IDX WS-PRD-IDX
           MOVE DR-AGING-BUCKET TO WS-AGING-CD
           EVALUATE DR-AGING-BUCKET
              WHEN "B1" MOVE 1 TO WS-BKT-IDX
              WHEN "B2" MOVE 2 TO WS-BKT-IDX
              WHEN "B3" MOVE 3 TO WS-BKT-IDX
              WHEN "B4" MOVE 4 TO WS-BKT-IDX
              WHEN OTHER
                 DISPLAY CN-PGM " 経過区分不正 口座="
                         DR-ACCT-NO
                 ADD 1 TO WS-ERR-CNT
                 EXIT PARAGRAPH
           END-EVALUATE
           IF DR-NEW-STATUS NOT = "00"
              AND DR-NEW-STATUS NOT = "10"
              AND DR-NEW-STATUS NOT = "30"
              DISPLAY CN-PGM " 延滞状態不正 口座="
                      DR-ACCT-NO
              ADD 1 TO WS-ERR-CNT
              MOVE ZERO TO WS-BKT-IDX
              EXIT PARAGRAPH
           END-IF
           IF DR-DAYS-OVERDUE < ZERO
              DISPLAY CN-PGM " 延滞日数不正 口座="
                      DR-ACCT-NO
              ADD 1 TO WS-ERR-CNT
              MOVE ZERO TO WS-BKT-IDX
              EXIT PARAGRAPH
           END-IF
           IF DR-LATE-CHARGE-AMT < ZERO
              DISPLAY CN-PGM " 延滞金額不正 口座="
                      DR-ACCT-NO
              ADD 1 TO WS-ERR-CNT
              MOVE ZERO TO WS-BKT-IDX
              EXIT PARAGRAPH
           END-IF
           EVALUATE DR-ACCT-NO(1:1)
              WHEN "1" MOVE "LN" TO WS-PROD-CD
              WHEN "2" MOVE "TR" TO WS-PROD-CD
              WHEN "3" MOVE "BG" TO WS-PROD-CD
              WHEN OTHER MOVE "OT" TO WS-PROD-CD
           END-EVALUATE
           EVALUATE WS-PROD-CD
              WHEN "LN" MOVE 1 TO WS-PRD-IDX
              WHEN "TR" MOVE 2 TO WS-PRD-IDX
              WHEN "BG" MOVE 3 TO WS-PRD-IDX
              WHEN "OT" MOVE 4 TO WS-PRD-IDX
           END-EVALUATE.

       JOIN-ALLOWANCE-RTN.
           MOVE DR-ACCT-NO TO LA-ACCT-NO
           READ KZLLAF KEY IS LA-ACCT-NO
              INVALID KEY
                 IF FS-KZLLAF = "23"
                    MOVE ZERO TO LA-ALLOWANCE-AMT
                    ADD 1 TO WS-SKIP-CNT
                 ELSE
                    DISPLAY CN-PGM " KZLLAF 照合失敗 ST="
                            FS-KZLLAF
                    SET HARD-ERR TO TRUE
                 END-IF
              NOT INVALID KEY
                 IF LA-ALLOWANCE-AMT < ZERO
                    DISPLAY CN-PGM " 引当金額不正"
                    DISPLAY "口座=" LA-ACCT-NO
                    ADD 1 TO WS-ERR-CNT
                    MOVE ZERO TO LA-ALLOWANCE-AMT
                 END-IF
           END-READ.

       ACCUMULATE-RTN.
           ADD DR-LATE-CHARGE-AMT
              TO SUM-LATE-AMT(WS-BKT-IDX WS-PRD-IDX)
           ADD LA-ALLOWANCE-AMT
              TO SUM-ALLOW-AMT(WS-BKT-IDX WS-PRD-IDX)
           ADD 1 TO SUM-ACCT-CNT(WS-BKT-IDX WS-PRD-IDX).

       OUTPUT-REPORT-RTN.
           PERFORM VARYING WS-BKT-IDX FROM 1 BY 1
              UNTIL WS-BKT-IDX > 4
              PERFORM VARYING WS-PRD-IDX FROM 1 BY 1
                 UNTIL WS-PRD-IDX > 4
                 IF SUM-LATE-AMT(WS-BKT-IDX WS-PRD-IDX)
                    >= WS-THRESHOLD
                    PERFORM BUILD-OUT-RTN
                    DISPLAY OUT-REC
                    ADD 1 TO WS-OUT-CNT
                    ADD OUT-ACCT-CNT TO WS-HASH-TOTAL
                    ADD OUT-LATE-MIL TO WS-HASH-TOTAL
                    ADD OUT-ALLOW-MIL TO WS-HASH-TOTAL
                 END-IF
              END-PERFORM
           END-PERFORM
           MOVE "HASH-TTL" TO TRL-ID
           MOVE WS-OUT-CNT TO TRL-OUT-CNT
           MOVE WS-HASH-TOTAL TO TRL-HASH-TOTAL
           MOVE WS-READ-CNT TO TRL-READ-CNT
           MOVE WS-SKIP-CNT TO TRL-SKIP-CNT
           MOVE WS-ERR-CNT TO TRL-ERR-CNT
           DISPLAY TRL-REC.

       BUILD-OUT-RTN.
           MOVE CN-RPT-ID TO OUT-RPT-ID
           MOVE WS-REPORT-DATE TO OUT-RPT-DATE
           MOVE BKT-CODE(WS-BKT-IDX) TO OUT-BUCKET
           MOVE PRD-CODE(WS-PRD-IDX) TO OUT-PRODUCT
           MOVE SUM-ACCT-CNT(WS-BKT-IDX WS-PRD-IDX)
              TO OUT-ACCT-CNT
           MOVE SUM-LATE-AMT(WS-BKT-IDX WS-PRD-IDX)
              TO WS-AMT-WK
           PERFORM ROUND-UP-MILLION-RTN
           MOVE WS-MIL-WK TO OUT-LATE-MIL
           MOVE SUM-ALLOW-AMT(WS-BKT-IDX WS-PRD-IDX)
              TO WS-AMT-WK
           PERFORM ROUND-UP-MILLION-RTN
           MOVE WS-MIL-WK TO OUT-ALLOW-MIL
           IF SUM-LATE-AMT(WS-BKT-IDX WS-PRD-IDX) > ZERO
              COMPUTE WS-COV-WK ROUNDED =
                 SUM-ALLOW-AMT(WS-BKT-IDX WS-PRD-IDX)
                 * 100 / SUM-LATE-AMT(WS-BKT-IDX WS-PRD-IDX)
           ELSE
              MOVE ZERO TO WS-COV-WK
           END-IF
           MOVE WS-COV-WK TO OUT-COVERAGE.

       ROUND-UP-MILLION-RTN.
           COMPUTE WS-MIL-WK = WS-AMT-WK / CN-MILLION
           COMPUTE WS-REM-WK = WS-AMT-WK
              - (WS-MIL-WK * CN-MILLION)
           IF WS-REM-WK > ZERO
              ADD 1 TO WS-MIL-WK
           END-IF.

       CLOSE-RTN.
           IF FS-KZDLRF = "00" OR FS-KZDLRF = "10"
              CLOSE KZDLRF
              IF FS-KZDLRF NOT = "00"
                 DISPLAY CN-PGM " KZDLRF クローズ失敗 ST="
                         FS-KZDLRF
                 SET HARD-ERR TO TRUE
              END-IF
           END-IF
           IF FS-KZLLAF = "00" OR FS-KZLLAF = "23"
              CLOSE KZLLAF
              IF FS-KZLLAF NOT = "00"
                 DISPLAY CN-PGM " KZLLAF クローズ失敗 ST="
                         FS-KZLLAF
                 SET HARD-ERR TO TRUE
              END-IF
           END-IF.
