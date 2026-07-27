       IDENTIFICATION DIVISION.
       PROGRAM-ID. LF103B.
      ******************************************************************
      * 解約明細帳票出力バッチ
      * LFREPFの解約明細を帳票種別別に整列し、見出し、合計行、
      * エラー行を付与した出力用レコードへ変換する。
      * 金額は既存明細の編集・集計のみを行い、再計算しない。
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LFREPF
               ASSIGN TO "LFREPF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-LFREPF-ST.

       DATA DIVISION.
       FILE SECTION.
       FD  LFREPF.
           COPY LFREPC.

       WORKING-STORAGE SECTION.
       01  WS-LFREPF-ST             PIC XX.
       01  WS-END-SW                PIC X VALUE SPACE.
           88  WS-END                     VALUE "1".
       01  WS-ABEND-SW              PIC X VALUE SPACE.
           88  WS-ABEND                   VALUE "1".

       01  WS-COUNTERS.
           05  WS-IN-CNT            PIC 9(7) VALUE ZERO.
           05  WS-OUT-CNT           PIC 9(7) VALUE ZERO.
           05  WS-ERR-CNT           PIC 9(7) VALUE ZERO.
           05  WS-TBL-CNT           PIC 9(5) VALUE ZERO.
           05  WS-IDX               PIC 9(5) VALUE ZERO.
           05  WS-JDX               PIC 9(5) VALUE ZERO.
           05  WS-PAGE-LINE         PIC 9(3) VALUE ZERO.
           05  WS-PAGE-NO           PIC 9(5) VALUE ZERO.

       01  WS-TOTALS.
           05  WS-KBN-TOTAL         PIC S9(13)V99 VALUE ZERO.
           05  WS-GRAND-TOTAL       PIC S9(13)V99 VALUE ZERO.
           05  WS-KBN-CNT           PIC 9(7) VALUE ZERO.
           05  WS-GRAND-CNT         PIC 9(7) VALUE ZERO.

       01  WS-CURRENT.
           05  WS-CUR-KBN           PIC X VALUE SPACE.
           05  WS-PRV-KBN           PIC X VALUE SPACE.
           05  WS-WORK-AMT          PIC S9(13)V99 VALUE ZERO.
           05  WS-OUT-LINE          PIC 9(7) VALUE ZERO.

       01  WS-SAVE-REC.
           05  SV-REPORT-ID         PIC X(12).
           05  SV-LINE-NO           PIC 9(7).
           05  SV-POL-NO            PIC X(20).
           05  SV-PRINT-KBN         PIC X.
           05  SV-PRINT-AMT         PIC S9(13)V99.
           05  SV-ERROR-KBN         PIC X.

       01  WS-TABLE.
           05  TB-REC OCCURS 5000 TIMES.
               10  TB-REPORT-ID     PIC X(12).
               10  TB-LINE-NO       PIC 9(7).
               10  TB-POL-NO        PIC X(20).
               10  TB-PRINT-KBN     PIC X.
               10  TB-PRINT-AMT     PIC S9(13)V99.
               10  TB-ERROR-KBN     PIC X.

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE 0 TO RETURN-CODE
           PERFORM 1000-READ-INPUT
           IF NOT WS-ABEND
              PERFORM 2000-SORT-TABLE
              PERFORM 3000-WRITE-OUTPUT
           END-IF
           IF WS-ABEND
              MOVE 12 TO RETURN-CODE
           END-IF
           GOBACK.

       1000-READ-INPUT.
           OPEN INPUT LFREPF
           IF WS-LFREPF-ST NOT = "00"
              DISPLAY "LFREPF オープン失敗 ST=" WS-LFREPF-ST
              SET WS-ABEND TO TRUE
              EXIT PARAGRAPH
           END-IF

           PERFORM UNTIL WS-END OR WS-ABEND
              READ LFREPF
                 AT END
                    SET WS-END TO TRUE
                 NOT AT END
                    ADD 1 TO WS-IN-CNT
                    PERFORM 1100-STORE-DETAIL
              END-READ
           END-PERFORM

           CLOSE LFREPF
           IF WS-LFREPF-ST NOT = "00"
              DISPLAY "LFREPF クローズ失敗 ST=" WS-LFREPF-ST
              SET WS-ABEND TO TRUE
           END-IF.

       1100-STORE-DETAIL.
           IF RP-PRINT-KBN NOT = "1"
              AND RP-PRINT-KBN NOT = "2"
              AND RP-PRINT-KBN NOT = "3"
              DISPLAY "帳票種別不正 証券番号=" RP-POL-NO
              MOVE "E" TO RP-ERROR-KBN
           END-IF

           IF RP-ERROR-KBN NOT = SPACE AND RP-ERROR-KBN NOT = "0"
              ADD 1 TO WS-ERR-CNT
           END-IF

           IF WS-TBL-CNT >= 5000
              DISPLAY "LFREPF 明細件数超過 件数=" WS-IN-CNT
              SET WS-ABEND TO TRUE
              EXIT PARAGRAPH
           END-IF

           ADD 1 TO WS-TBL-CNT
           MOVE RP-REPORT-ID TO TB-REPORT-ID(WS-TBL-CNT)
           MOVE RP-LINE-NO   TO TB-LINE-NO(WS-TBL-CNT)
           MOVE RP-POL-NO    TO TB-POL-NO(WS-TBL-CNT)
           MOVE RP-PRINT-KBN TO TB-PRINT-KBN(WS-TBL-CNT)
           MOVE RP-PRINT-AMT TO TB-PRINT-AMT(WS-TBL-CNT)
           MOVE RP-ERROR-KBN TO TB-ERROR-KBN(WS-TBL-CNT).

       2000-SORT-TABLE.
           PERFORM VARYING WS-IDX FROM 2 BY 1
             UNTIL WS-IDX > WS-TBL-CNT
              MOVE TB-REPORT-ID(WS-IDX) TO SV-REPORT-ID
              MOVE TB-LINE-NO(WS-IDX)   TO SV-LINE-NO
              MOVE TB-POL-NO(WS-IDX)    TO SV-POL-NO
              MOVE TB-PRINT-KBN(WS-IDX) TO SV-PRINT-KBN
              MOVE TB-PRINT-AMT(WS-IDX) TO SV-PRINT-AMT
              MOVE TB-ERROR-KBN(WS-IDX) TO SV-ERROR-KBN
              COMPUTE WS-JDX = WS-IDX - 1
              PERFORM UNTIL WS-JDX = 0
                 OR TB-PRINT-KBN(WS-JDX) < SV-PRINT-KBN
                 OR (TB-PRINT-KBN(WS-JDX) = SV-PRINT-KBN
                 AND TB-REPORT-ID(WS-JDX) <= SV-REPORT-ID)
                 MOVE TB-REPORT-ID(WS-JDX)
                   TO TB-REPORT-ID(WS-JDX + 1)
                 MOVE TB-LINE-NO(WS-JDX)
                   TO TB-LINE-NO(WS-JDX + 1)
                 MOVE TB-POL-NO(WS-JDX)
                   TO TB-POL-NO(WS-JDX + 1)
                 MOVE TB-PRINT-KBN(WS-JDX)
                   TO TB-PRINT-KBN(WS-JDX + 1)
                 MOVE TB-PRINT-AMT(WS-JDX)
                   TO TB-PRINT-AMT(WS-JDX + 1)
                 MOVE TB-ERROR-KBN(WS-JDX)
                   TO TB-ERROR-KBN(WS-JDX + 1)
                 SUBTRACT 1 FROM WS-JDX
              END-PERFORM
              MOVE SV-REPORT-ID TO TB-REPORT-ID(WS-JDX + 1)
              MOVE SV-LINE-NO   TO TB-LINE-NO(WS-JDX + 1)
              MOVE SV-POL-NO    TO TB-POL-NO(WS-JDX + 1)
              MOVE SV-PRINT-KBN TO TB-PRINT-KBN(WS-JDX + 1)
              MOVE SV-PRINT-AMT TO TB-PRINT-AMT(WS-JDX + 1)
              MOVE SV-ERROR-KBN TO TB-ERROR-KBN(WS-JDX + 1)
           END-PERFORM.

       3000-WRITE-OUTPUT.
           OPEN OUTPUT LFREPF
           IF WS-LFREPF-ST NOT = "00"
              DISPLAY "LFREPF 出力オープン失敗 ST=" WS-LFREPF-ST
              SET WS-ABEND TO TRUE
              EXIT PARAGRAPH
           END-IF

           MOVE SPACE TO WS-PRV-KBN
           PERFORM VARYING WS-IDX FROM 1 BY 1
             UNTIL WS-IDX > WS-TBL-CNT OR WS-ABEND
              MOVE TB-PRINT-KBN(WS-IDX) TO WS-CUR-KBN
              IF WS-PRV-KBN NOT = WS-CUR-KBN
                 IF WS-PRV-KBN NOT = SPACE
                    PERFORM 3300-WRITE-TOTAL
                 END-IF
                 MOVE ZERO TO WS-KBN-TOTAL WS-KBN-CNT
                 PERFORM 3100-WRITE-HEADER
                 MOVE WS-CUR-KBN TO WS-PRV-KBN
              END-IF
              PERFORM 3200-WRITE-DETAIL
           END-PERFORM

           IF NOT WS-ABEND AND WS-TBL-CNT > ZERO
              PERFORM 3300-WRITE-TOTAL
              PERFORM 3400-WRITE-GRAND
           END-IF

           CLOSE LFREPF
           IF WS-LFREPF-ST NOT = "00"
              DISPLAY "LFREPF 出力クローズ失敗 ST=" WS-LFREPF-ST
              SET WS-ABEND TO TRUE
           END-IF

           IF NOT WS-ABEND
              DISPLAY "LF103B 正常終了 入力=" WS-IN-CNT
                      " 出力=" WS-OUT-CNT " エラー=" WS-ERR-CNT
           END-IF.

       3100-WRITE-HEADER.
           ADD 1 TO WS-PAGE-NO
           MOVE ZERO TO WS-PAGE-LINE
           INITIALIZE LFREPF-REC
           ADD 1 TO WS-OUT-LINE
           MOVE "HEADER"     TO RP-REPORT-ID
           MOVE WS-OUT-LINE  TO RP-LINE-NO
           MOVE SPACE        TO RP-POL-NO
           MOVE WS-CUR-KBN   TO RP-PRINT-KBN
           MOVE ZERO         TO RP-PRINT-AMT
           MOVE SPACE        TO RP-ERROR-KBN
           WRITE LFREPF-REC
           PERFORM 9000-CHECK-WRITE
           ADD 1 TO WS-PAGE-LINE.

       3200-WRITE-DETAIL.
           IF WS-PAGE-LINE >= 55
              PERFORM 3100-WRITE-HEADER
           END-IF

           INITIALIZE LFREPF-REC
           ADD 1 TO WS-OUT-LINE
           MOVE TB-REPORT-ID(WS-IDX) TO RP-REPORT-ID
           MOVE WS-OUT-LINE          TO RP-LINE-NO
           MOVE TB-POL-NO(WS-IDX)    TO RP-POL-NO
           MOVE TB-PRINT-KBN(WS-IDX) TO RP-PRINT-KBN
           MOVE TB-PRINT-AMT(WS-IDX) TO RP-PRINT-AMT
           MOVE TB-ERROR-KBN(WS-IDX) TO RP-ERROR-KBN
           WRITE LFREPF-REC
           PERFORM 9000-CHECK-WRITE

           ADD TB-PRINT-AMT(WS-IDX) TO WS-KBN-TOTAL
           ADD TB-PRINT-AMT(WS-IDX) TO WS-GRAND-TOTAL
           ADD 1 TO WS-KBN-CNT WS-GRAND-CNT WS-PAGE-LINE

           IF TB-ERROR-KBN(WS-IDX) NOT = SPACE
              AND TB-ERROR-KBN(WS-IDX) NOT = "0"
              PERFORM 3250-WRITE-ERROR
           END-IF.

       3250-WRITE-ERROR.
           INITIALIZE LFREPF-REC
           ADD 1 TO WS-OUT-LINE
           MOVE "ERROR"             TO RP-REPORT-ID
           MOVE WS-OUT-LINE         TO RP-LINE-NO
           MOVE TB-POL-NO(WS-IDX)   TO RP-POL-NO
           MOVE TB-PRINT-KBN(WS-IDX) TO RP-PRINT-KBN
           MOVE ZERO                TO RP-PRINT-AMT
           MOVE TB-ERROR-KBN(WS-IDX) TO RP-ERROR-KBN
           WRITE LFREPF-REC
           PERFORM 9000-CHECK-WRITE
           ADD 1 TO WS-PAGE-LINE.

       3300-WRITE-TOTAL.
           INITIALIZE LFREPF-REC
           ADD 1 TO WS-OUT-LINE
           MOVE "TOTAL"       TO RP-REPORT-ID
           MOVE WS-OUT-LINE   TO RP-LINE-NO
           MOVE SPACE         TO RP-POL-NO
           MOVE WS-PRV-KBN    TO RP-PRINT-KBN
           MOVE WS-KBN-TOTAL  TO RP-PRINT-AMT
           MOVE SPACE         TO RP-ERROR-KBN
           WRITE LFREPF-REC
           PERFORM 9000-CHECK-WRITE
           ADD 1 TO WS-PAGE-LINE.

       3400-WRITE-GRAND.
           INITIALIZE LFREPF-REC
           ADD 1 TO WS-OUT-LINE
           MOVE "GRAND"        TO RP-REPORT-ID
           MOVE WS-OUT-LINE    TO RP-LINE-NO
           MOVE SPACE          TO RP-POL-NO
           MOVE "9"            TO RP-PRINT-KBN
           MOVE WS-GRAND-TOTAL TO RP-PRINT-AMT
           MOVE SPACE          TO RP-ERROR-KBN
           WRITE LFREPF-REC
           PERFORM 9000-CHECK-WRITE.

       9000-CHECK-WRITE.
           IF WS-LFREPF-ST = "00"
              ADD 1 TO WS-OUT-CNT
           ELSE
              DISPLAY "LFREPF 書込失敗 ST=" WS-LFREPF-ST
              SET WS-ABEND TO TRUE
           END-IF.
