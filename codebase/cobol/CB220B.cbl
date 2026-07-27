       IDENTIFICATION DIVISION.
       PROGRAM-ID. CB220B.
      *
      * 変更履歴
      * 版数  年月日    担当    概要
      * 1.00  20240310  ＣＤ運用 初版作成
      * 1.01  20240705  ＣＤ運用 理由別集計区分を追加
      * 1.02  20241018  ＣＤ運用 加盟店照会対象判定を整理
      *
      * 売上例外リスト作成
      * 承認番号欠落、金額超過、期限切れ、カード番号不一致を抽出し、
      * 運用確認用の件数と金額合計を理由別に集計する。
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CDSALESF ASSIGN TO "CDSALESF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-SL-STATUS.
           SELECT CDAUTHF ASSIGN TO "CDAUTHF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS AU-AUTH-ID
               FILE STATUS IS WS-AU-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  CDSALESF.
           COPY CDSALEC.
       FD  CDAUTHF.
           COPY CDAUTHC.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05  WS-SL-STATUS          PIC XX VALUE SPACE.
           05  WS-AU-STATUS          PIC XX VALUE SPACE.

       01  WS-SWITCHES.
           05  WS-END-SALES          PIC X VALUE 'N'.
               88  END-SALES              VALUE 'Y'.
           05  WS-AUTH-FOUND         PIC X VALUE 'N'.
               88  AUTH-FOUND             VALUE 'Y'.
               88  AUTH-NOT-FOUND         VALUE 'N'.
           05  WS-EXCEPTION-SW       PIC X VALUE 'N'.
               88  EXCEPTION-FOUND        VALUE 'Y'.

       01  WS-DATE-AREA.
           05  WS-CURRENT-DATE       PIC 9(8) VALUE ZERO.
           05  WS-CURRENT-TS.
               10  WS-CURRENT-YYYY   PIC 9(4).
               10  WS-CURRENT-MM     PIC 9(2).
               10  WS-CURRENT-DD     PIC 9(2).
               10  FILLER            PIC X(13).

       01  WS-COUNTERS.
           05  WS-READ-CNT           PIC 9(9) VALUE ZERO.
           05  WS-EXC-CNT            PIC 9(9) VALUE ZERO.
           05  WS-RETRY-CNT          PIC 9(9) VALUE ZERO.
           05  WS-INQUIRY-CNT        PIC 9(9) VALUE ZERO.
           05  WS-MISS-CNT           PIC 9(9) VALUE ZERO.
           05  WS-OVER-CNT           PIC 9(9) VALUE ZERO.
           05  WS-EXP-CNT            PIC 9(9) VALUE ZERO.
           05  WS-CARD-CNT           PIC 9(9) VALUE ZERO.

       01  WS-AMOUNTS.
           05  WS-EXC-AMT            PIC S9(13)V99 VALUE ZERO.
           05  WS-RETRY-AMT          PIC S9(13)V99 VALUE ZERO.
           05  WS-INQUIRY-AMT        PIC S9(13)V99 VALUE ZERO.
           05  WS-MISS-AMT           PIC S9(13)V99 VALUE ZERO.
           05  WS-OVER-AMT           PIC S9(13)V99 VALUE ZERO.
           05  WS-EXP-AMT            PIC S9(13)V99 VALUE ZERO.
           05  WS-CARD-AMT           PIC S9(13)V99 VALUE ZERO.

       01  WS-DISPLAY-AREA.
           05  WS-DSP-CNT            PIC ZZZ,ZZZ,ZZ9.
           05  WS-DSP-AMT            PIC ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  WS-REASON-TEXT        PIC X(30) VALUE SPACE.
           05  WS-CLASS-TEXT         PIC X(20) VALUE SPACE.

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-TS
           MOVE WS-CURRENT-YYYY      TO WS-CURRENT-DATE(1:4)
           MOVE WS-CURRENT-MM        TO WS-CURRENT-DATE(5:2)
           MOVE WS-CURRENT-DD        TO WS-CURRENT-DATE(7:2)

           PERFORM 1000-OPEN-FILES
           PERFORM 2000-PROCESS-SALES UNTIL END-SALES
           PERFORM 8000-CLOSE-FILES
           PERFORM 9000-DISPLAY-SUMMARY

           MOVE 0 TO RETURN-CODE
           GOBACK.

       1000-OPEN-FILES.
           OPEN INPUT CDSALESF CDAUTHF

           IF WS-SL-STATUS NOT = '00'
               DISPLAY 'CDSALESF オープン失敗 ST=' WS-SL-STATUS
               MOVE 8 TO RETURN-CODE
               GOBACK
           END-IF

           IF WS-AU-STATUS NOT = '00'
               DISPLAY 'CDAUTHF オープン失敗 ST=' WS-AU-STATUS
               MOVE 8 TO RETURN-CODE
               GOBACK
           END-IF.

       2000-PROCESS-SALES.
           READ CDSALESF
               AT END
                   SET END-SALES TO TRUE
               NOT AT END
                   ADD 1 TO WS-READ-CNT
                   PERFORM 3000-EVALUATE-SALES
           END-READ

           IF WS-SL-STATUS NOT = '00' AND WS-SL-STATUS NOT = '10'
               DISPLAY 'CDSALESF 読込失敗 ST=' WS-SL-STATUS
               MOVE 8 TO RETURN-CODE
               GOBACK
           END-IF.

       3000-EVALUATE-SALES.
           MOVE 'N' TO WS-EXCEPTION-SW
           SET AUTH-NOT-FOUND TO TRUE

           IF SL-AUTH-ID = SPACE OR SL-AUTH-ID = ZERO
               MOVE '承認番号欠落' TO WS-REASON-TEXT
               PERFORM 4100-COUNT-MISSING
               PERFORM 5000-CLASSIFY-SALES
           ELSE
               PERFORM 3100-READ-AUTH
               IF AUTH-NOT-FOUND
                   MOVE '承認履歴なし' TO WS-REASON-TEXT
                   PERFORM 4100-COUNT-MISSING
                   PERFORM 5000-CLASSIFY-SALES
               ELSE
                   PERFORM 3200-CHECK-AUTH
               END-IF
           END-IF.

       3100-READ-AUTH.
           MOVE SL-AUTH-ID TO AU-AUTH-ID
           READ CDAUTHF
               INVALID KEY
                   SET AUTH-NOT-FOUND TO TRUE
               NOT INVALID KEY
                   SET AUTH-FOUND TO TRUE
           END-READ

           IF WS-AU-STATUS NOT = '00'
              AND WS-AU-STATUS NOT = '23'
              AND WS-AU-STATUS NOT = '10'
               DISPLAY 'CDAUTHF 読込失敗 ST=' WS-AU-STATUS
                       ' 承認番号=' SL-AUTH-ID
               MOVE 8 TO RETURN-CODE
               GOBACK
           END-IF.

       3200-CHECK-AUTH.
           IF AU-AUTH-RESULT NOT = '00'
               MOVE '承認結果不正' TO WS-REASON-TEXT
               PERFORM 4100-COUNT-MISSING
               PERFORM 5000-CLASSIFY-SALES
           END-IF

           IF SL-SALES-AMT > AU-AUTH-AMT
               MOVE '売上金額超過' TO WS-REASON-TEXT
               PERFORM 4200-COUNT-OVER
               PERFORM 5000-CLASSIFY-SALES
           END-IF

           IF AU-HOLD-EXP-DT < WS-CURRENT-DATE
               MOVE '承認期限切れ' TO WS-REASON-TEXT
               PERFORM 4300-COUNT-EXPIRED
               PERFORM 5000-CLASSIFY-SALES
           END-IF

           IF SL-CARD-NO NOT = AU-CARD-NO
               MOVE 'カード番号不一致' TO WS-REASON-TEXT
               PERFORM 4400-COUNT-CARD
               PERFORM 5000-CLASSIFY-SALES
           END-IF.

       4100-COUNT-MISSING.
           ADD 1 TO WS-MISS-CNT
           ADD SL-SALES-AMT TO WS-MISS-AMT
           SET EXCEPTION-FOUND TO TRUE.

       4200-COUNT-OVER.
           ADD 1 TO WS-OVER-CNT
           ADD SL-SALES-AMT TO WS-OVER-AMT
           SET EXCEPTION-FOUND TO TRUE.

       4300-COUNT-EXPIRED.
           ADD 1 TO WS-EXP-CNT
           ADD SL-SALES-AMT TO WS-EXP-AMT
           SET EXCEPTION-FOUND TO TRUE.

       4400-COUNT-CARD.
           ADD 1 TO WS-CARD-CNT
           ADD SL-SALES-AMT TO WS-CARD-AMT
           SET EXCEPTION-FOUND TO TRUE.

       5000-CLASSIFY-SALES.
           IF EXCEPTION-FOUND
               ADD 1 TO WS-EXC-CNT
               ADD SL-SALES-AMT TO WS-EXC-AMT
               EVALUATE TRUE
                   WHEN AUTH-NOT-FOUND
                    AND (SL-CAPTURE-STATUS = '0'
                     OR  SL-CAPTURE-STATUS = '1')
                       ADD 1 TO WS-RETRY-CNT
                       ADD SL-SALES-AMT TO WS-RETRY-AMT
                       MOVE '再投入対象' TO WS-CLASS-TEXT
                   WHEN AUTH-FOUND
                    AND SL-CAPTURE-STATUS = '2'
                       ADD 1 TO WS-INQUIRY-CNT
                       ADD SL-SALES-AMT TO WS-INQUIRY-AMT
                       MOVE '加盟店照会対象' TO WS-CLASS-TEXT
                   WHEN OTHER
                       ADD 1 TO WS-INQUIRY-CNT
                       ADD SL-SALES-AMT TO WS-INQUIRY-AMT
                       MOVE '運用確認対象' TO WS-CLASS-TEXT
               END-EVALUATE
               DISPLAY '売上例外 '
                       '売上ID=' SL-SALES-ID
                       ' 理由=' WS-REASON-TEXT
                       ' 分類=' WS-CLASS-TEXT
           END-IF.

       8000-CLOSE-FILES.
           CLOSE CDSALESF CDAUTHF

           IF WS-SL-STATUS NOT = '00'
               DISPLAY 'CDSALESF クローズ失敗 ST=' WS-SL-STATUS
               MOVE 8 TO RETURN-CODE
               GOBACK
           END-IF

           IF WS-AU-STATUS NOT = '00'
               DISPLAY 'CDAUTHF クローズ失敗 ST=' WS-AU-STATUS
               MOVE 8 TO RETURN-CODE
               GOBACK
           END-IF.

       9000-DISPLAY-SUMMARY.
           DISPLAY '売上例外リスト作成 集計開始'

           MOVE WS-READ-CNT TO WS-DSP-CNT
           DISPLAY '読込件数=' WS-DSP-CNT

           MOVE WS-EXC-CNT TO WS-DSP-CNT
           MOVE WS-EXC-AMT TO WS-DSP-AMT
           DISPLAY '例外合計 件数=' WS-DSP-CNT
                   ' 金額=' WS-DSP-AMT

           MOVE WS-MISS-CNT TO WS-DSP-CNT
           MOVE WS-MISS-AMT TO WS-DSP-AMT
           DISPLAY '承認番号欠落 件数=' WS-DSP-CNT
                   ' 金額=' WS-DSP-AMT

           MOVE WS-OVER-CNT TO WS-DSP-CNT
           MOVE WS-OVER-AMT TO WS-DSP-AMT
           DISPLAY '売上金額超過 件数=' WS-DSP-CNT
                   ' 金額=' WS-DSP-AMT

           MOVE WS-EXP-CNT TO WS-DSP-CNT
           MOVE WS-EXP-AMT TO WS-DSP-AMT
           DISPLAY '承認期限切れ 件数=' WS-DSP-CNT
                   ' 金額=' WS-DSP-AMT

           MOVE WS-CARD-CNT TO WS-DSP-CNT
           MOVE WS-CARD-AMT TO WS-DSP-AMT
           DISPLAY 'カード番号不一致 件数=' WS-DSP-CNT
                   ' 金額=' WS-DSP-AMT

           MOVE WS-RETRY-CNT TO WS-DSP-CNT
           MOVE WS-RETRY-AMT TO WS-DSP-AMT
           DISPLAY '再投入対象 件数=' WS-DSP-CNT
                   ' 金額=' WS-DSP-AMT

           MOVE WS-INQUIRY-CNT TO WS-DSP-CNT
           MOVE WS-INQUIRY-AMT TO WS-DSP-AMT
           DISPLAY '加盟店照会対象 件数=' WS-DSP-CNT
                   ' 金額=' WS-DSP-AMT

           DISPLAY '売上例外リスト作成 正常終了'.
