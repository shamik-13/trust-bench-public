       IDENTIFICATION DIVISION.
       PROGRAM-ID. CG240B.
      *
      * 顧客月次推移作成バッチ
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CGSUMF ASSIGN TO "CGSUMF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-CGSUMF-ST.
           SELECT CMRSLF ASSIGN TO "CMRSLF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-CMRSLF-ST.

       DATA DIVISION.
       FILE SECTION.
       FD  CGSUMF.
           COPY CGSUMC.
       FD  CMRSLF.
           COPY CMRSLC.

       WORKING-STORAGE SECTION.
       01  WS-CGSUMF-ST             PIC X(02) VALUE SPACE.
       01  WS-CMRSLF-ST             PIC X(02) VALUE SPACE.

       01  WS-END-FLAGS.
           05  WS-CGSUMF-EOF        PIC X VALUE 'N'.
               88  CGSUMF-END             VALUE 'Y'.
           05  WS-CMRSLF-EOF        PIC X VALUE 'N'.
               88  CMRSLF-END             VALUE 'Y'.

       01  WS-RUN-AREA.
           05  WS-RUN-YYYYMM-IN     PIC X(10) VALUE SPACE.
           05  WS-RUN-YYYYMM        PIC 9(06) VALUE ZERO.
           05  WS-PREV-YYYYMM       PIC 9(06) VALUE ZERO.
           05  WS-RUN-YYYY          PIC 9(04) VALUE ZERO.
           05  WS-RUN-MM            PIC 9(02) VALUE ZERO.
           05  WS-PREV-YYYY         PIC 9(04) VALUE ZERO.
           05  WS-PREV-MM           PIC 9(02) VALUE ZERO.
           05  WS-GS-YYYYMM         PIC 9(06) VALUE ZERO.

       01  WS-CGSUM-COUNTERS.
           05  WS-CGSUM-READ-CNT    PIC 9(09) VALUE ZERO.
           05  WS-CGSUM-WRITE-CNT   PIC 9(09) VALUE ZERO.
           05  WS-CGSUM-EXCL-CNT    PIC 9(09) VALUE ZERO.

       01  WS-CMRSL-COUNTERS.
           05  WS-CMRSL-READ-CNT    PIC 9(09) VALUE ZERO.
           05  WS-RS-VALID-CNT      PIC 9(09) VALUE ZERO.
           05  WS-RS-UNKNOWN-CNT    PIC 9(09) VALUE ZERO.
           05  WS-KEY-HOLD-CNT      PIC 9(09) VALUE ZERO.
           05  WS-NAYOSE-FIX-CNT    PIC 9(09) VALUE ZERO.
           05  WS-KEY-STOP-CNT      PIC 9(09) VALUE ZERO.
           05  WS-KEY-NEW-CNT       PIC 9(09) VALUE ZERO.

       01  WS-WORK-NUMBERS.
           05  WS-IDX               PIC 9(03) COMP VALUE ZERO.
           05  WS-FIND-IDX          PIC 9(03) COMP VALUE ZERO.
           05  WS-DIFF-CNT          PIC S9(11) COMP-3 VALUE ZERO.
           05  WS-PREV-CUST         PIC 9(09) VALUE ZERO.
           05  WS-PREV-ACT          PIC 9(09) VALUE ZERO.
           05  WS-CURR-CUST         PIC 9(09) VALUE ZERO.
           05  WS-CURR-ACT          PIC 9(09) VALUE ZERO.

       01  WS-SUM-TABLE.
           05  WS-SUM-ENTRY OCCURS 200 TIMES.
               10  WS-TBL-USED      PIC X VALUE 'N'.
               10  WS-TBL-SEG       PIC X(02) VALUE SPACE.
               10  WS-TBL-P-CUST    PIC 9(09) VALUE ZERO.
               10  WS-TBL-P-ACT     PIC 9(09) VALUE ZERO.
               10  WS-TBL-C-CUST    PIC 9(09) VALUE ZERO.
               10  WS-TBL-C-ACT     PIC 9(09) VALUE ZERO.
               10  WS-TBL-NEW       PIC 9(09) VALUE ZERO.
               10  WS-TBL-STOP      PIC 9(09) VALUE ZERO.

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE 0 TO RETURN-CODE
           PERFORM 1000-INIT
           IF RETURN-CODE = 0
              PERFORM 2000-LOAD-CMRSLF
           END-IF
           IF RETURN-CODE = 0
              PERFORM 3000-LOAD-CGSUMF
           END-IF
           IF RETURN-CODE = 0
              PERFORM 4000-CALC-TREND
           END-IF
           IF RETURN-CODE = 0
              PERFORM 5000-WRITE-CGSUMF
           END-IF
           IF RETURN-CODE = 0
              PERFORM 9000-END-MSG
           END-IF
           GOBACK.

       1000-INIT.
           ACCEPT WS-RUN-YYYYMM-IN FROM ENVIRONMENT "RUN_YYYYMM"

           IF WS-RUN-YYYYMM-IN = SPACE
              DISPLAY 'CG240B 実行年月未指定 RUN_YYYYMM'
              MOVE 8 TO RETURN-CODE
              EXIT PARAGRAPH
           END-IF

           IF WS-RUN-YYYYMM-IN(1:6) IS NOT NUMERIC
              DISPLAY 'CG240B 実行年月不正'
              DISPLAY 'YYYYMM=' WS-RUN-YYYYMM-IN
              MOVE 8 TO RETURN-CODE
              EXIT PARAGRAPH
           END-IF

           MOVE WS-RUN-YYYYMM-IN(1:6) TO WS-RUN-YYYYMM

           DIVIDE WS-RUN-YYYYMM BY 100
              GIVING WS-RUN-YYYY
              REMAINDER WS-RUN-MM
           END-DIVIDE

           IF WS-RUN-MM < 1 OR WS-RUN-MM > 12
              DISPLAY 'CG240B 実行年月不正'
              DISPLAY 'YYYYMM=' WS-RUN-YYYYMM
              MOVE 8 TO RETURN-CODE
              EXIT PARAGRAPH
           END-IF

           IF WS-RUN-MM = 1
              COMPUTE WS-PREV-YYYY = WS-RUN-YYYY - 1
              MOVE 12 TO WS-PREV-MM
           ELSE
              MOVE WS-RUN-YYYY TO WS-PREV-YYYY
              COMPUTE WS-PREV-MM = WS-RUN-MM - 1
           END-IF

           COMPUTE WS-PREV-YYYYMM =
                   WS-PREV-YYYY * 100 + WS-PREV-MM

           DISPLAY 'CG240B 開始'
           DISPLAY '当月=' WS-RUN-YYYYMM
           DISPLAY '前月=' WS-PREV-YYYYMM.

       2000-LOAD-CMRSLF.
           MOVE 'N' TO WS-CMRSLF-EOF
           OPEN INPUT CMRSLF
           IF WS-CMRSLF-ST NOT = '00'
              DISPLAY 'CMRSLF オープン失敗'
              DISPLAY 'ST=' WS-CMRSLF-ST
              MOVE 8 TO RETURN-CODE
              EXIT PARAGRAPH
           END-IF

           PERFORM UNTIL CMRSLF-END OR RETURN-CODE NOT = 0
              READ CMRSLF
                 AT END
                    SET CMRSLF-END TO TRUE
                 NOT AT END
                    ADD 1 TO WS-CMRSL-READ-CNT
                    PERFORM 2100-EDIT-CMRSLF
              END-READ
           END-PERFORM

           CLOSE CMRSLF
           IF WS-CMRSLF-ST NOT = '00'
              DISPLAY 'CMRSLF クローズ失敗'
              DISPLAY 'ST=' WS-CMRSLF-ST
              MOVE 8 TO RETURN-CODE
           END-IF.

       2100-EDIT-CMRSLF.
           IF RS-RESULT-ID = SPACE
              DISPLAY '名寄せ結果ＩＤ未設定'
              DISPLAY 'CIF=' RS-CIF-NO
              ADD 1 TO WS-RS-UNKNOWN-CNT
              EXIT PARAGRAPH
           END-IF

           EVALUATE RS-RESULT-KBN
              WHEN '01'
                 ADD 1 TO WS-KEY-NEW-CNT
                 ADD 1 TO WS-RS-VALID-CNT
              WHEN '02'
                 ADD 1 TO WS-KEY-STOP-CNT
                 ADD 1 TO WS-RS-VALID-CNT
              WHEN '03'
                 ADD 1 TO WS-KEY-HOLD-CNT
                 ADD 1 TO WS-RS-VALID-CNT
              WHEN '04'
                 ADD 1 TO WS-NAYOSE-FIX-CNT
                 ADD 1 TO WS-RS-VALID-CNT
              WHEN OTHER
                 ADD 1 TO WS-RS-UNKNOWN-CNT
                 DISPLAY '名寄せ結果区分不明'
                 DISPLAY 'ID=' RS-RESULT-ID
                 DISPLAY 'KBN=' RS-RESULT-KBN
           END-EVALUATE.

       3000-LOAD-CGSUMF.
           MOVE 'N' TO WS-CGSUMF-EOF
           OPEN INPUT CGSUMF
           IF WS-CGSUMF-ST NOT = '00'
              DISPLAY 'CGSUMF オープン失敗'
              DISPLAY 'ST=' WS-CGSUMF-ST
              MOVE 8 TO RETURN-CODE
              EXIT PARAGRAPH
           END-IF

           PERFORM UNTIL CGSUMF-END OR RETURN-CODE NOT = 0
              READ CGSUMF
                 AT END
                    SET CGSUMF-END TO TRUE
                 NOT AT END
                    ADD 1 TO WS-CGSUM-READ-CNT
                    PERFORM 3100-EDIT-CGSUMF
              END-READ
           END-PERFORM

           CLOSE CGSUMF
           IF WS-CGSUMF-ST NOT = '00'
              DISPLAY 'CGSUMF クローズ失敗'
              DISPLAY 'ST=' WS-CGSUMF-ST
              MOVE 8 TO RETURN-CODE
           END-IF.

       3100-EDIT-CGSUMF.
           IF GS-SEGMENT-KBN = SPACE
              DISPLAY '顧客区分未設定'
              DISPLAY '年月=' GS-SUMMARY-YYYYMM
              ADD 1 TO WS-CGSUM-EXCL-CNT
              EXIT PARAGRAPH
           END-IF

           IF GS-SUMMARY-YYYYMM(1:6) IS NOT NUMERIC
              DISPLAY '集計年月不正'
              DISPLAY '年月=' GS-SUMMARY-YYYYMM
              ADD 1 TO WS-CGSUM-EXCL-CNT
              EXIT PARAGRAPH
           END-IF

           MOVE GS-SUMMARY-YYYYMM(1:6) TO WS-GS-YYYYMM

           IF GS-CUSTOMER-CNT < GS-ACTIVE-CNT
              DISPLAY '顧客件数と稼働件数不整合'
              DISPLAY '年月=' GS-SUMMARY-YYYYMM
              DISPLAY '区分=' GS-SEGMENT-KBN
              MOVE 8 TO RETURN-CODE
              EXIT PARAGRAPH
           END-IF

           IF WS-GS-YYYYMM = WS-PREV-YYYYMM
              PERFORM 3200-FIND-SEGMENT
              IF RETURN-CODE = 0
                 ADD GS-CUSTOMER-CNT TO WS-TBL-P-CUST(WS-FIND-IDX)
                 ADD GS-ACTIVE-CNT TO WS-TBL-P-ACT(WS-FIND-IDX)
              END-IF
           ELSE
              IF WS-GS-YYYYMM = WS-RUN-YYYYMM
                 PERFORM 3200-FIND-SEGMENT
                 IF RETURN-CODE = 0
                    ADD GS-CUSTOMER-CNT
                        TO WS-TBL-C-CUST(WS-FIND-IDX)
                    ADD GS-ACTIVE-CNT
                        TO WS-TBL-C-ACT(WS-FIND-IDX)
                 END-IF
              ELSE
                 ADD 1 TO WS-CGSUM-EXCL-CNT
              END-IF
           END-IF.

       3200-FIND-SEGMENT.
           MOVE ZERO TO WS-FIND-IDX

           PERFORM VARYING WS-IDX FROM 1 BY 1
              UNTIL WS-IDX > 200 OR WS-FIND-IDX NOT = ZERO
              IF WS-TBL-USED(WS-IDX) = 'Y'
                 IF WS-TBL-SEG(WS-IDX) = GS-SEGMENT-KBN
                    MOVE WS-IDX TO WS-FIND-IDX
                 END-IF
              END-IF
           END-PERFORM

           IF WS-FIND-IDX = ZERO
              PERFORM VARYING WS-IDX FROM 1 BY 1
                 UNTIL WS-IDX > 200 OR WS-FIND-IDX NOT = ZERO
                 IF WS-TBL-USED(WS-IDX) NOT = 'Y'
                    MOVE WS-IDX TO WS-FIND-IDX
                    MOVE 'Y' TO WS-TBL-USED(WS-IDX)
                    MOVE GS-SEGMENT-KBN TO WS-TBL-SEG(WS-IDX)
                 END-IF
              END-PERFORM
           END-IF

           IF WS-FIND-IDX = ZERO
              DISPLAY '顧客区分表あふれ'
              DISPLAY '区分=' GS-SEGMENT-KBN
              MOVE 8 TO RETURN-CODE
           END-IF.

       4000-CALC-TREND.
           PERFORM VARYING WS-IDX FROM 1 BY 1
              UNTIL WS-IDX > 200
              IF WS-TBL-USED(WS-IDX) = 'Y'
                 MOVE WS-TBL-P-CUST(WS-IDX) TO WS-PREV-CUST
                 MOVE WS-TBL-C-CUST(WS-IDX) TO WS-CURR-CUST
                 COMPUTE WS-DIFF-CNT = WS-CURR-CUST - WS-PREV-CUST
                 IF WS-DIFF-CNT > 0
                    MOVE WS-DIFF-CNT TO WS-TBL-NEW(WS-IDX)
                 ELSE
                    MOVE ZERO TO WS-TBL-NEW(WS-IDX)
                 END-IF

                 MOVE WS-TBL-P-ACT(WS-IDX) TO WS-PREV-ACT
                 MOVE WS-TBL-C-ACT(WS-IDX) TO WS-CURR-ACT
                 COMPUTE WS-DIFF-CNT = WS-PREV-ACT - WS-CURR-ACT
                 IF WS-DIFF-CNT > 0
                    MOVE WS-DIFF-CNT TO WS-TBL-STOP(WS-IDX)
                 ELSE
                    MOVE ZERO TO WS-TBL-STOP(WS-IDX)
                 END-IF
              END-IF
           END-PERFORM.

       5000-WRITE-CGSUMF.
           OPEN OUTPUT CGSUMF
           IF WS-CGSUMF-ST NOT = '00'
              DISPLAY 'CGSUMF 出力オープン失敗'
              DISPLAY 'ST=' WS-CGSUMF-ST
              MOVE 8 TO RETURN-CODE
              EXIT PARAGRAPH
           END-IF

           PERFORM VARYING WS-IDX FROM 1 BY 1
              UNTIL WS-IDX > 200 OR RETURN-CODE NOT = 0
              IF WS-TBL-USED(WS-IDX) = 'Y'
                 INITIALIZE CGSUMF-REC
                 MOVE WS-RUN-YYYYMM TO GS-SUMMARY-YYYYMM
                 MOVE WS-TBL-SEG(WS-IDX) TO GS-SEGMENT-KBN
                 MOVE WS-TBL-C-CUST(WS-IDX) TO GS-CUSTOMER-CNT
                 MOVE WS-TBL-C-ACT(WS-IDX) TO GS-ACTIVE-CNT
                 MOVE WS-TBL-STOP(WS-IDX) TO GS-STOP-CNT
                 MOVE WS-TBL-NEW(WS-IDX) TO GS-NEW-CNT
                 WRITE CGSUMF-REC
                 IF WS-CGSUMF-ST NOT = '00'
                    DISPLAY 'CGSUMF 書込失敗'
                    DISPLAY 'ST=' WS-CGSUMF-ST
                    DISPLAY '区分=' GS-SEGMENT-KBN
                    MOVE 8 TO RETURN-CODE
                 ELSE
                    ADD 1 TO WS-CGSUM-WRITE-CNT
                 END-IF
              END-IF
           END-PERFORM

           CLOSE CGSUMF
           IF WS-CGSUMF-ST NOT = '00'
              DISPLAY 'CGSUMF 出力クローズ失敗'
              DISPLAY 'ST=' WS-CGSUMF-ST
              MOVE 8 TO RETURN-CODE
           END-IF.

       9000-END-MSG.
           DISPLAY 'CG240B 正常終了'
           DISPLAY 'CGSUMF 読込件数=' WS-CGSUM-READ-CNT
           DISPLAY 'CGSUMF 書込件数=' WS-CGSUM-WRITE-CNT
           DISPLAY 'CGSUMF 対象外件数=' WS-CGSUM-EXCL-CNT
           DISPLAY 'CMRSLF 読込件数=' WS-CMRSL-READ-CNT
           DISPLAY 'CMRSLF 有効件数=' WS-RS-VALID-CNT
           DISPLAY 'CMRSLF 不明件数=' WS-RS-UNKNOWN-CNT
           DISPLAY '統合キー保留件数=' WS-KEY-HOLD-CNT
           DISPLAY '名寄せ確定件数=' WS-NAYOSE-FIX-CNT
           MOVE 0 TO RETURN-CODE.
