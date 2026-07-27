       IDENTIFICATION DIVISION.
       PROGRAM-ID. KZ160B.
      * 変更履歴
      * 版数  年月日(和暦)   担当              概要
      * 1.00  令和04年04月01日  佐藤 健 (E-042)   新規作成
      * 1.01  令和05年10月16日  高橋 麻衣 (E-085) 与信残高集計条件追加
      * 1.02  令和06年07月01日  高橋 麻衣 (E-085) 業種別集計対応
       AUTHOR.     TRUST-BATCH.
      *----------------------------------------------------------------*
      * 与信エクスポージャ集計バッチ                                  *
      * KZEXPFを順読し、KZEXPRFへ集計結果を出力する。                  *
      * 現行開始版では商品別上限判定は未実装。                        *
      *----------------------------------------------------------------*

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZEXPF
               ASSIGN       TO "KZEXPF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS  IS FS-KZEXPF.
           SELECT KZEXPRF
               ASSIGN       TO "KZEXPRF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS  IS FS-KZEXPRF.

       DATA DIVISION.
       FILE SECTION.

       FD  KZEXPF.
       COPY KZEXPFC.

       FD  KZEXPRF.
       COPY KZEXPRC.

       WORKING-STORAGE SECTION.
       01  FS-KZEXPF                 PIC XX VALUE SPACES.
       01  FS-KZEXPRF                PIC XX VALUE SPACES.

       01  SW-EOF                    PIC X VALUE 'N'.
           88  EOF-KZEXPF                  VALUE 'Y'.
           88  NOT-EOF-KZEXPF              VALUE 'N'.

       01  WS-ABEND-SW               PIC X VALUE 'N'.
           88  ABEND-OCCURRED              VALUE 'Y'.
           88  NORMAL-PROCESS              VALUE 'N'.

       01  WS-ABEND-POINT            PIC X(16) VALUE SPACES.
       01  WS-ABEND-FILE             PIC X(08) VALUE SPACES.
       01  WS-ABEND-STATUS           PIC XX VALUE SPACES.
       01  WS-DATE                   PIC 9(08) VALUE ZERO.

       01  WS-READ-COUNT             PIC 9(09) VALUE ZERO.
       01  WS-WRITE-COUNT            PIC 9(09) VALUE ZERO.
       01  WS-ERROR-COUNT            PIC 9(09) VALUE ZERO.

       01  WS-DISPLAY-AREA.
           05  WS-DISP-READ          PIC ZZZ,ZZZ,ZZ9.
           05  WS-DISP-WRITE         PIC ZZZ,ZZZ,ZZ9.
           05  WS-DISP-ERROR         PIC ZZZ,ZZZ,ZZ9.

       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-INITIALIZE
           IF NORMAL-PROCESS
               PERFORM 2000-PROCESS UNTIL EOF-KZEXPF
           END-IF
           PERFORM 9000-FINALIZE
           GOBACK.

       1000-INITIALIZE.
           MOVE 0 TO RETURN-CODE
           SET NORMAL-PROCESS TO TRUE
           SET NOT-EOF-KZEXPF TO TRUE
           ACCEPT WS-DATE FROM DATE YYYYMMDD
           DISPLAY 'KZ160B 開始 日付=' WS-DATE

           OPEN INPUT KZEXPF
           IF FS-KZEXPF NOT = '00'
               MOVE 'OPEN'    TO WS-ABEND-POINT
               MOVE 'KZEXPF'  TO WS-ABEND-FILE
               MOVE FS-KZEXPF TO WS-ABEND-STATUS
               PERFORM 8000-ABEND
           END-IF

           IF NORMAL-PROCESS
               OPEN OUTPUT KZEXPRF
               IF FS-KZEXPRF NOT = '00'
                   MOVE 'OPEN'     TO WS-ABEND-POINT
                   MOVE 'KZEXPRF'  TO WS-ABEND-FILE
                   MOVE FS-KZEXPRF TO WS-ABEND-STATUS
                   PERFORM 8000-ABEND
               END-IF
           END-IF.

       2000-PROCESS.
           PERFORM 2100-READ-KZEXPF
           IF NOT-EOF-KZEXPF
              AND NORMAL-PROCESS
               PERFORM 3000-VALIDATE-INPUT
               IF NORMAL-PROCESS
                   PERFORM 4000-MAKE-OUTPUT
                   PERFORM 5000-WRITE-KZEXPRF
               END-IF
           END-IF.

       2100-READ-KZEXPF.
           READ KZEXPF
               AT END
                   SET EOF-KZEXPF TO TRUE
               NOT AT END
                   ADD 1 TO WS-READ-COUNT
           END-READ

           IF FS-KZEXPF NOT = '00'
              AND FS-KZEXPF NOT = '10'
               MOVE 'READ'    TO WS-ABEND-POINT
               MOVE 'KZEXPF'  TO WS-ABEND-FILE
               MOVE FS-KZEXPF TO WS-ABEND-STATUS
               PERFORM 8000-ABEND
           END-IF.

       3000-VALIDATE-INPUT.
           IF EX-CUST-ID = SPACES
               ADD 1 TO WS-ERROR-COUNT
               DISPLAY 'KZ160B 入力不正 顧客番号空白'
               MOVE 'VALIDATE' TO WS-ABEND-POINT
               MOVE 'KZEXPF'   TO WS-ABEND-FILE
               MOVE 'CU'       TO WS-ABEND-STATUS
               PERFORM 8100-DATA-ABEND
           ELSE
               IF EX-PRODUCT-TYPE NOT = '01'
                  AND EX-PRODUCT-TYPE NOT = '02'
                  AND EX-PRODUCT-TYPE NOT = '03'
                   ADD 1 TO WS-ERROR-COUNT
                   DISPLAY 'KZ160B 入力不正 商品区分='
                           EX-PRODUCT-TYPE
                   MOVE 'VALIDATE' TO WS-ABEND-POINT
                   MOVE 'KZEXPF'   TO WS-ABEND-FILE
                   MOVE 'PR'       TO WS-ABEND-STATUS
                   PERFORM 8100-DATA-ABEND
               END-IF
           END-IF.

       4000-MAKE-OUTPUT.
           MOVE EX-CUST-ID       TO XR-CUST-ID
           MOVE EX-PRODUCT-TYPE  TO XR-PRODUCT-TYPE
           MOVE EX-EXPOSURE-AMT  TO XR-EXPOSURE-AMT
           MOVE EX-EXPOSURE-AMT  TO XR-CAPPED-AMT
           MOVE 'N'              TO XR-OVER-FLAG.

       5000-WRITE-KZEXPRF.
           WRITE KZEXPRF-REC
           IF FS-KZEXPRF = '00'
               ADD 1 TO WS-WRITE-COUNT
           ELSE
               MOVE 'WRITE'    TO WS-ABEND-POINT
               MOVE 'KZEXPRF'  TO WS-ABEND-FILE
               MOVE FS-KZEXPRF TO WS-ABEND-STATUS
               PERFORM 8000-ABEND
           END-IF.

       8000-ABEND.
           SET ABEND-OCCURRED TO TRUE
           MOVE 8 TO RETURN-CODE
           DISPLAY 'KZ160B 入出力異常 処理=' WS-ABEND-POINT
                   ' ファイル=' WS-ABEND-FILE
                   ' ST=' WS-ABEND-STATUS.

       8100-DATA-ABEND.
           SET ABEND-OCCURRED TO TRUE
           MOVE 12 TO RETURN-CODE
           DISPLAY 'KZ160B データ異常 処理=' WS-ABEND-POINT
                   ' ファイル=' WS-ABEND-FILE
                   ' 理由=' WS-ABEND-STATUS.

       9000-FINALIZE.
           IF FS-KZEXPF = '00'
              OR FS-KZEXPF = '10'
               CLOSE KZEXPF
               IF FS-KZEXPF NOT = '00'
                   MOVE 'CLOSE'   TO WS-ABEND-POINT
                   MOVE 'KZEXPF'  TO WS-ABEND-FILE
                   MOVE FS-KZEXPF TO WS-ABEND-STATUS
                   PERFORM 8000-ABEND
               END-IF
           END-IF

           IF FS-KZEXPRF = '00'
               CLOSE KZEXPRF
               IF FS-KZEXPRF NOT = '00'
                   MOVE 'CLOSE'    TO WS-ABEND-POINT
                   MOVE 'KZEXPRF'  TO WS-ABEND-FILE
                   MOVE FS-KZEXPRF TO WS-ABEND-STATUS
                   PERFORM 8000-ABEND
               END-IF
           END-IF

           MOVE WS-READ-COUNT  TO WS-DISP-READ
           MOVE WS-WRITE-COUNT TO WS-DISP-WRITE
           MOVE WS-ERROR-COUNT TO WS-DISP-ERROR
           DISPLAY 'KZ160B 読込件数=' WS-DISP-READ
           DISPLAY 'KZ160B 出力件数=' WS-DISP-WRITE
           DISPLAY 'KZ160B 異常件数=' WS-DISP-ERROR

           IF NORMAL-PROCESS
               MOVE 0 TO RETURN-CODE
               DISPLAY 'KZ160B 正常終了'
           ELSE
               DISPLAY 'KZ160B 異常終了 RC=' RETURN-CODE
           END-IF.
