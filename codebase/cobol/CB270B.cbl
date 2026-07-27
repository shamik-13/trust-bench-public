       IDENTIFICATION DIVISION.
       PROGRAM-ID. CB270B.
      ******************************************************************
      * 変更履歴
      * 版数  年月日    担当    概要
      * 1.00  20240225  ＣＤ運用 初版作成
      * 1.01  20240612  ＣＤ運用 加算対象売上区分の除外条件を追加
      * 1.02  20241004  ＣＤ運用 会員別集計とポイント新規作成を整理
      ******************************************************************
      * 確定売上をもとに会員別ポイントを加算・更新する
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CDSALESF ASSIGN TO "CDSALESF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS SL-SALES-ID
               FILE STATUS IS WS-SL-STATUS.

           SELECT CDCARDF ASSIGN TO "CDCARDF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS CF-CARD-NO
               FILE STATUS IS WS-CF-STATUS.

           SELECT CDPOINTF ASSIGN TO "CDPOINTF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS PT-MEMBER-ID
               FILE STATUS IS WS-PT-STATUS.

       DATA DIVISION.
       FILE SECTION.

       FD  CDSALESF.
       COPY CDSALEC.

       FD  CDCARDF.
       COPY CDCARDFC.

       FD  CDPOINTF.
       COPY CDPNTC.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05  WS-SL-STATUS          PIC X(02).
           05  WS-CF-STATUS          PIC X(02).
           05  WS-PT-STATUS          PIC X(02).

       01  WS-FLAGS.
           05  WS-EOF-SW             PIC X VALUE "N".
               88  WS-EOF                 VALUE "Y".
               88  WS-NOT-EOF             VALUE "N".
           05  WS-ABEND-SW           PIC X VALUE "N".
               88  WS-ABEND               VALUE "Y".
               88  WS-NORMAL              VALUE "N".
           05  WS-TABLE-FOUND-SW     PIC X VALUE "N".
               88  WS-TABLE-FOUND         VALUE "Y".
               88  WS-TABLE-NOT-FOUND     VALUE "N".

       01  WS-CURRENT.
           05  WS-CUR-MEMBER-ID      PIC X(10).
           05  WS-EARN-AMT           PIC S9(13)V99 COMP-3.
           05  WS-EARN-POINT         PIC S9(11) COMP-3.

       01  WS-COUNTERS.
           05  WS-SL-READ-CNT        PIC 9(09) COMP-3 VALUE ZERO.
           05  WS-CF-READ-CNT        PIC 9(09) COMP-3 VALUE ZERO.
           05  WS-PT-READ-CNT        PIC 9(09) COMP-3 VALUE ZERO.
           05  WS-PT-WRITE-CNT       PIC 9(09) COMP-3 VALUE ZERO.
           05  WS-PT-REWRITE-CNT     PIC 9(09) COMP-3 VALUE ZERO.
           05  WS-SKIP-CNT           PIC 9(09) COMP-3 VALUE ZERO.
           05  WS-ERR-CNT            PIC 9(09) COMP-3 VALUE ZERO.
           05  WS-TABLE-CNT          PIC 9(05) COMP-3 VALUE ZERO.
           05  WS-IDX                PIC 9(05) COMP-3 VALUE ZERO.

       01  WS-MEMBER-TABLE.
           05  WS-MEMBER-ROW OCCURS 5000 TIMES.
               10  WS-TB-MEMBER-ID   PIC X(10).
               10  WS-TB-EARN-AMT    PIC S9(13)V99 COMP-3.
               10  WS-TB-LAST-DT     PIC 9(08).

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE 8 TO RETURN-CODE
           SET WS-NORMAL TO TRUE
           SET WS-NOT-EOF TO TRUE

           PERFORM 1000-OPEN-FILES
           IF WS-NORMAL
               PERFORM 2000-READ-SALES
               PERFORM UNTIL WS-EOF OR WS-ABEND
                   PERFORM 3000-PROCESS-SALES
                   IF WS-NORMAL
                       PERFORM 2000-READ-SALES
                   END-IF
               END-PERFORM
           END-IF

           IF WS-NORMAL
               PERFORM 5000-UPDATE-POINTS
           END-IF

           PERFORM 9000-CLOSE-FILES

           IF WS-NORMAL
               MOVE 0 TO RETURN-CODE
               DISPLAY "CB270B 正常終了"
               DISPLAY "売上読込件数=" WS-SL-READ-CNT
               DISPLAY "対象外件数=" WS-SKIP-CNT
               DISPLAY "エラー件数=" WS-ERR-CNT
               DISPLAY "ポイント更新件数=" WS-PT-REWRITE-CNT
               DISPLAY "ポイント新規件数=" WS-PT-WRITE-CNT
           ELSE
               MOVE 12 TO RETURN-CODE
               DISPLAY "CB270B 異常終了"
               DISPLAY "売上読込件数=" WS-SL-READ-CNT
               DISPLAY "エラー件数=" WS-ERR-CNT
           END-IF
           GOBACK.

       1000-OPEN-FILES.
           OPEN INPUT CDSALESF
           IF WS-SL-STATUS NOT = "00"
               DISPLAY "CDSALESF OPEN ERROR ST="
                       WS-SL-STATUS
               SET WS-ABEND TO TRUE
           END-IF

           IF WS-NORMAL
               OPEN INPUT CDCARDF
               IF WS-CF-STATUS NOT = "00"
                   DISPLAY "CDCARDF OPEN ERROR ST="
                           WS-CF-STATUS
                   SET WS-ABEND TO TRUE
               END-IF
           END-IF

           IF WS-NORMAL
               OPEN I-O CDPOINTF
               IF WS-PT-STATUS NOT = "00"
                   DISPLAY "CDPOINTF OPEN ERROR ST="
                           WS-PT-STATUS
                   SET WS-ABEND TO TRUE
               END-IF
           END-IF.

       2000-READ-SALES.
           READ CDSALESF NEXT RECORD
               AT END
                   SET WS-EOF TO TRUE
               NOT AT END
                   ADD 1 TO WS-SL-READ-CNT
           END-READ

           IF WS-SL-STATUS NOT = "00" AND WS-SL-STATUS NOT = "10"
               DISPLAY "CDSALESF READ ERROR ST="
                       WS-SL-STATUS
               SET WS-ABEND TO TRUE
           END-IF.

       3000-PROCESS-SALES.
           IF SL-CAPTURE-STATUS NOT = "C"
               ADD 1 TO WS-SKIP-CNT
           ELSE
               IF SL-MERCHANT-ID (1:3) = "CSH"
                  OR SL-MERCHANT-ID (1:3) = "FEE"
                  OR SL-MERCHANT-ID (1:3) = "ADJ"
                   ADD 1 TO WS-SKIP-CNT
               ELSE
                   PERFORM 3100-READ-CARD
                   IF WS-NORMAL
                       PERFORM 3200-EDIT-CARD
                   END-IF
                   IF WS-NORMAL
                       PERFORM 3300-ADD-MEMBER-TOTAL
                   END-IF
               END-IF
           END-IF.

       3100-READ-CARD.
           MOVE SL-CARD-NO TO CF-CARD-NO
           READ CDCARDF
               INVALID KEY
                   ADD 1 TO WS-ERR-CNT
                   DISPLAY "カード未検出 売上ID="
                           SL-SALES-ID
               NOT INVALID KEY
                   ADD 1 TO WS-CF-READ-CNT
           END-READ

           IF WS-CF-STATUS NOT = "00" AND WS-CF-STATUS NOT = "23"
               DISPLAY "CDCARDF READ ERROR ST="
                       WS-CF-STATUS
               SET WS-ABEND TO TRUE
           END-IF.

       3200-EDIT-CARD.
           IF WS-CF-STATUS = "23"
               ADD 1 TO WS-SKIP-CNT
           ELSE
               EVALUATE CF-CARD-STATUS
                   WHEN "01"
                   WHEN "09"
                       CONTINUE
                   WHEN "02"
                   WHEN "03"
                       ADD 1 TO WS-SKIP-CNT
                   WHEN OTHER
                       ADD 1 TO WS-ERR-CNT
                       ADD 1 TO WS-SKIP-CNT
                       DISPLAY "カード状態不正 CARD="
                               CF-CARD-NO
                       DISPLAY "カード状態="
                               CF-CARD-STATUS
               END-EVALUATE
           END-IF.

       3300-ADD-MEMBER-TOTAL.
           IF WS-CF-STATUS = "00"
              AND (CF-CARD-STATUS = "01" OR CF-CARD-STATUS = "09")
               MOVE CF-MEMBER-ID TO WS-CUR-MEMBER-ID
               MOVE SL-SALES-AMT TO WS-EARN-AMT
               PERFORM 3310-FIND-MEMBER
               IF WS-TABLE-FOUND
                   ADD WS-EARN-AMT TO WS-TB-EARN-AMT (WS-IDX)
                   IF SL-POSTING-DT > WS-TB-LAST-DT (WS-IDX)
                       MOVE SL-POSTING-DT
                         TO WS-TB-LAST-DT (WS-IDX)
                   END-IF
               ELSE
                   IF WS-TABLE-CNT < 5000
                       ADD 1 TO WS-TABLE-CNT
                       MOVE WS-CUR-MEMBER-ID
                         TO WS-TB-MEMBER-ID (WS-TABLE-CNT)
                       MOVE WS-EARN-AMT
                         TO WS-TB-EARN-AMT (WS-TABLE-CNT)
                       MOVE SL-POSTING-DT
                         TO WS-TB-LAST-DT (WS-TABLE-CNT)
                   ELSE
                       DISPLAY "会員集計表オーバーフロー"
                       SET WS-ABEND TO TRUE
                   END-IF
               END-IF
           END-IF.

       3310-FIND-MEMBER.
           SET WS-TABLE-NOT-FOUND TO TRUE
           MOVE 1 TO WS-IDX
           PERFORM UNTIL WS-IDX > WS-TABLE-CNT OR WS-TABLE-FOUND
               IF WS-TB-MEMBER-ID (WS-IDX) = WS-CUR-MEMBER-ID
                   SET WS-TABLE-FOUND TO TRUE
               ELSE
                   ADD 1 TO WS-IDX
               END-IF
           END-PERFORM.

       5000-UPDATE-POINTS.
           MOVE 1 TO WS-IDX
           PERFORM UNTIL WS-IDX > WS-TABLE-CNT OR WS-ABEND
               MOVE WS-TB-MEMBER-ID (WS-IDX) TO PT-MEMBER-ID
               READ CDPOINTF
                   INVALID KEY
                       PERFORM 5200-CREATE-POINT
                   NOT INVALID KEY
                       ADD 1 TO WS-PT-READ-CNT
                       PERFORM 5100-REWRITE-POINT
               END-READ

               IF WS-PT-STATUS NOT = "00"
                  AND WS-PT-STATUS NOT = "23"
                   DISPLAY "CDPOINTF READ ERROR ST="
                           WS-PT-STATUS
                   SET WS-ABEND TO TRUE
               END-IF

               ADD 1 TO WS-IDX
           END-PERFORM.

       5100-REWRITE-POINT.
           IF PT-POINT-STATUS NOT = "01"
               ADD 1 TO WS-ERR-CNT
               DISPLAY "ポイント状態不正 会員="
                       PT-MEMBER-ID
               DISPLAY "ポイント状態="
                       PT-POINT-STATUS
           ELSE
               COMPUTE WS-EARN-POINT =
                   WS-TB-EARN-AMT (WS-IDX) / 100
               ADD WS-EARN-POINT TO PT-POINT-BAL
               MOVE WS-TB-LAST-DT (WS-IDX) TO PT-LAST-EARN-DT
               REWRITE CDPOINTF-REC
               IF WS-PT-STATUS = "00"
                   ADD 1 TO WS-PT-REWRITE-CNT
               ELSE
                   DISPLAY "CDPOINTF REWRITE ERROR ST="
                           WS-PT-STATUS
                   SET WS-ABEND TO TRUE
               END-IF
           END-IF.

       5200-CREATE-POINT.
           INITIALIZE CDPOINTF-REC
           MOVE WS-TB-MEMBER-ID (WS-IDX) TO PT-MEMBER-ID
           COMPUTE WS-EARN-POINT =
               WS-TB-EARN-AMT (WS-IDX) / 100
           MOVE WS-EARN-POINT TO PT-POINT-BAL
           MOVE WS-TB-LAST-DT (WS-IDX) TO PT-LAST-EARN-DT
           MOVE ZERO TO PT-LAST-REDEEM-DT
           MOVE "01" TO PT-POINT-STATUS
           WRITE CDPOINTF-REC
           IF WS-PT-STATUS = "00"
               ADD 1 TO WS-PT-WRITE-CNT
           ELSE
               DISPLAY "CDPOINTF WRITE ERROR ST="
                       WS-PT-STATUS
               SET WS-ABEND TO TRUE
           END-IF.

       9000-CLOSE-FILES.
           CLOSE CDSALESF
           IF WS-SL-STATUS NOT = "00"
               DISPLAY "CDSALESF CLOSE ERROR ST="
                       WS-SL-STATUS
               SET WS-ABEND TO TRUE
           END-IF

           CLOSE CDCARDF
           IF WS-CF-STATUS NOT = "00"
               DISPLAY "CDCARDF CLOSE ERROR ST="
                       WS-CF-STATUS
               SET WS-ABEND TO TRUE
           END-IF

           CLOSE CDPOINTF
           IF WS-PT-STATUS NOT = "00"
               DISPLAY "CDPOINTF CLOSE ERROR ST="
                       WS-PT-STATUS
               SET WS-ABEND TO TRUE
           END-IF.
