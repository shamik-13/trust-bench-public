       IDENTIFICATION DIVISION.
       PROGRAM-ID. KZ116B.
      * 変更履歴
      * 版数  年月日(和暦)  担当                         概要
      * 1.00  令和03.04.01  システム部 勘定系チーム      新規作成
      * 1.01  令和04.10.15  システム部 勘定系チーム      判定係数見直し
      * 1.02  令和06.06.01  システム部 勘定系チーム      延滞情報反映追加
       AUTHOR. KZ-BATCH.
      *
      * 信用スコア算定バッチ。
      * 顧客別に口座、延滞、担保、集計済みエクスポージャを集約し、
      * 内部スコアとグレードを算定してスコアファイルへ格納する。
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZACCTF ASSIGN TO "KZACCTF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS AC-ACCT-NO
               FILE STATUS IS WS-ST-ACCT.
           SELECT KZCUSTF ASSIGN TO "KZCUSTF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CU-CUST-ID
               FILE STATUS IS WS-ST-CUST.
           SELECT KZDLQF ASSIGN TO "KZDLQF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS DL-ACCT-NO
               FILE STATUS IS WS-ST-DLQ.
           SELECT KZCOLLF ASSIGN TO "KZCOLLF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CL-COLLATERAL-ID
               FILE STATUS IS WS-ST-COLL.
           SELECT KZEXPRF ASSIGN TO "KZEXPRF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-ST-EXPR.
           SELECT KZSCORF ASSIGN TO "KZSCORF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS SC-CUST-ID
               FILE STATUS IS WS-ST-SCOR.

       DATA DIVISION.
       FILE SECTION.
       FD  KZACCTF.
           COPY KZACCTC4.
       FD  KZCUSTF.
           COPY KZCUSTC.
       FD  KZDLQF.
           COPY KZDLQFC2.
       FD  KZCOLLF.
           COPY KZCOLLFC.
       FD  KZEXPRF.
           COPY KZEXPRC.
       FD  KZSCORF.
           COPY KZSCORFC.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05 WS-ST-ACCT             PIC XX.
           05 WS-ST-CUST             PIC XX.
           05 WS-ST-DLQ              PIC XX.
           05 WS-ST-COLL             PIC XX.
           05 WS-ST-EXPR             PIC XX.
           05 WS-ST-SCOR             PIC XX.

       01  WS-SWITCHES.
           05 WS-EOF-CUST            PIC X VALUE "N".
              88 CUST-END           VALUE "Y".
           05 WS-EOF-ACCT            PIC X VALUE "N".
              88 ACCT-END           VALUE "Y".
           05 WS-EOF-COLL            PIC X VALUE "N".
              88 COLL-END           VALUE "Y".
           05 WS-EOF-EXPR            PIC X VALUE "N".
              88 EXPR-END           VALUE "Y".
           05 WS-HARD-ERROR          PIC X VALUE "N".
              88 HARD-ERROR         VALUE "Y".

       01  WS-CONSTANTS.
           05 WS-MAX-CUST            PIC 9(04) VALUE 2000.
           05 WS-TODAY               PIC 9(08).
           05 WS-RC-NORMAL           PIC 9(02) VALUE 0.
           05 WS-RC-ERROR            PIC 9(02) VALUE 8.
           05 WS-RC-ABEND            PIC 9(02) VALUE 12.

       01  WS-COUNTERS.
           05 WS-CUST-CNT            PIC 9(04) VALUE 0.
           05 WS-IDX                 PIC 9(04) VALUE 0.
           05 WS-OUT-CNT             PIC 9(06) VALUE 0.
           05 WS-HOLD-CNT            PIC 9(06) VALUE 0.
           05 WS-ERR-CNT             PIC 9(06) VALUE 0.

       01  WS-CALC.
           05 WS-SCORE               PIC S9(05) VALUE 0.
           05 WS-POINT               PIC S9(05) VALUE 0.
           05 WS-COLL-NET            PIC S9(13) VALUE 0.
           05 WS-HASH                PIC 9(10) VALUE 0.
           05 WS-HASH-WORK           PIC 9(10) VALUE 0.

       01  WS-CUST-TABLE.
           05 WS-CUST-ENTRY OCCURS 2000 TIMES.
              10 TB-CUST-ID          PIC X(10).
              10 TB-KYC-STATUS       PIC X.
              10 TB-BRANCH-CODE      PIC X(03).
              10 TB-ACCT-CNT         PIC 9(04).
              10 TB-BAL-TOTAL        PIC S9(13).
              10 TB-AVG-TOTAL        PIC S9(13).
              10 TB-LIMIT-TOTAL      PIC S9(13).
              10 TB-MAX-DAYS         PIC 9(04).
              10 TB-WATCH-RANK       PIC X.
              10 TB-DUE-TOTAL        PIC S9(13).
              10 TB-COLL-TOTAL       PIC S9(13).
              10 TB-EXPOSURE-TOTAL   PIC S9(13).
              10 TB-CAPPED-TOTAL     PIC S9(13).
              10 TB-OVER-FLAG        PIC X.
              10 TB-HAS-EXPR         PIC X.
              10 TB-HAS-COLL         PIC X.
              10 TB-HOLD-FLAG        PIC X.
              10 TB-REASON           PIC X(20).

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE WS-RC-ERROR TO RETURN-CODE
           ACCEPT WS-TODAY FROM DATE YYYYMMDD
           PERFORM 1000-OPEN-FILES
           IF NOT HARD-ERROR
              PERFORM 2000-LOAD-CUSTOMERS
           END-IF
           IF NOT HARD-ERROR
              PERFORM 3000-ACCUM-ACCOUNTS
           END-IF
           IF NOT HARD-ERROR
              PERFORM 4000-ACCUM-COLLATERAL
           END-IF
           IF NOT HARD-ERROR
              PERFORM 5000-ACCUM-EXPOSURE
           END-IF
           IF NOT HARD-ERROR
              PERFORM 6000-WRITE-SCORES
           END-IF
           PERFORM 9000-CLOSE-FILES
           IF HARD-ERROR
              MOVE WS-RC-ABEND TO RETURN-CODE
           ELSE
              MOVE WS-RC-NORMAL TO RETURN-CODE
              DISPLAY "KZ210B 正常終了 出力件数=" WS-OUT-CNT
              DISPLAY "KZ210B 保留件数=" WS-HOLD-CNT
           END-IF
           GOBACK.

       1000-OPEN-FILES.
           OPEN INPUT KZCUSTF
           IF WS-ST-CUST NOT = "00"
              DISPLAY "KZCUSTF オープン失敗 ST=" WS-ST-CUST
              SET HARD-ERROR TO TRUE
           END-IF
           IF NOT HARD-ERROR
              OPEN INPUT KZACCTF
              IF WS-ST-ACCT NOT = "00"
                 DISPLAY "KZACCTF オープン失敗 ST=" WS-ST-ACCT
                 SET HARD-ERROR TO TRUE
              END-IF
           END-IF
           IF NOT HARD-ERROR
              OPEN INPUT KZDLQF
              IF WS-ST-DLQ NOT = "00"
                 DISPLAY "KZDLQF オープン失敗 ST=" WS-ST-DLQ
                 SET HARD-ERROR TO TRUE
              END-IF
           END-IF
           IF NOT HARD-ERROR
              OPEN INPUT KZCOLLF
              IF WS-ST-COLL NOT = "00"
                 DISPLAY "KZCOLLF オープン失敗 ST=" WS-ST-COLL
                 SET HARD-ERROR TO TRUE
              END-IF
           END-IF
           IF NOT HARD-ERROR
              OPEN INPUT KZEXPRF
              IF WS-ST-EXPR NOT = "00"
                 DISPLAY "KZEXPRF オープン失敗 ST=" WS-ST-EXPR
                 SET HARD-ERROR TO TRUE
              END-IF
           END-IF
           IF NOT HARD-ERROR
              OPEN I-O KZSCORF
              IF WS-ST-SCOR NOT = "00"
                 DISPLAY "KZSCORF オープン失敗 ST=" WS-ST-SCOR
                 SET HARD-ERROR TO TRUE
              END-IF
           END-IF.

       2000-LOAD-CUSTOMERS.
           MOVE LOW-VALUE TO CU-CUST-ID
           START KZCUSTF KEY IS >= CU-CUST-ID
              INVALID KEY
                 SET CUST-END TO TRUE
           END-START
           PERFORM UNTIL CUST-END OR HARD-ERROR
              READ KZCUSTF NEXT RECORD
                 AT END
                    SET CUST-END TO TRUE
                 NOT AT END
                    IF WS-ST-CUST NOT = "00"
                       DISPLAY "KZCUSTF 読込失敗 ST=" WS-ST-CUST
                       SET HARD-ERROR TO TRUE
                    ELSE
                       PERFORM 2100-ADD-CUSTOMER
                    END-IF
              END-READ
           END-PERFORM.

       2100-ADD-CUSTOMER.
           IF WS-CUST-CNT >= WS-MAX-CUST
              DISPLAY "顧客表あふれ 件数=" WS-CUST-CNT
              SET HARD-ERROR TO TRUE
           ELSE
              ADD 1 TO WS-CUST-CNT
              MOVE CU-CUST-ID     TO TB-CUST-ID(WS-CUST-CNT)
              MOVE CU-KYC-STATUS  TO TB-KYC-STATUS(WS-CUST-CNT)
              MOVE CU-BRANCH-CODE TO TB-BRANCH-CODE(WS-CUST-CNT)
              MOVE ZERO           TO TB-ACCT-CNT(WS-CUST-CNT)
              MOVE ZERO           TO TB-BAL-TOTAL(WS-CUST-CNT)
              MOVE ZERO           TO TB-AVG-TOTAL(WS-CUST-CNT)
              MOVE ZERO           TO TB-LIMIT-TOTAL(WS-CUST-CNT)
              MOVE ZERO           TO TB-MAX-DAYS(WS-CUST-CNT)
              MOVE SPACE          TO TB-WATCH-RANK(WS-CUST-CNT)
              MOVE ZERO           TO TB-DUE-TOTAL(WS-CUST-CNT)
              MOVE ZERO           TO TB-COLL-TOTAL(WS-CUST-CNT)
              MOVE ZERO           TO TB-EXPOSURE-TOTAL(WS-CUST-CNT)
              MOVE ZERO           TO TB-CAPPED-TOTAL(WS-CUST-CNT)
              MOVE "0"            TO TB-OVER-FLAG(WS-CUST-CNT)
              MOVE "N"            TO TB-HAS-EXPR(WS-CUST-CNT)
              MOVE "N"            TO TB-HAS-COLL(WS-CUST-CNT)
              MOVE "N"            TO TB-HOLD-FLAG(WS-CUST-CNT)
              MOVE SPACE          TO TB-REASON(WS-CUST-CNT)
           END-IF.

       3000-ACCUM-ACCOUNTS.
           MOVE LOW-VALUE TO AC-ACCT-NO
           START KZACCTF KEY IS >= AC-ACCT-NO
              INVALID KEY
                 SET ACCT-END TO TRUE
           END-START
           PERFORM UNTIL ACCT-END OR HARD-ERROR
              READ KZACCTF NEXT RECORD
                 AT END
                    SET ACCT-END TO TRUE
                 NOT AT END
                    IF WS-ST-ACCT NOT = "00"
                       DISPLAY "KZACCTF 読込失敗 ST=" WS-ST-ACCT
                       SET HARD-ERROR TO TRUE
                    ELSE
                       PERFORM 3100-ACCUM-ONE-ACCOUNT
                    END-IF
              END-READ
           END-PERFORM.

       3100-ACCUM-ONE-ACCOUNT.
           PERFORM 8000-FIND-CUSTOMER
           IF WS-IDX = ZERO
              DISPLAY "顧客未登録 口座=" AC-ACCT-NO
              ADD 1 TO WS-ERR-CNT
           ELSE
              ADD 1               TO TB-ACCT-CNT(WS-IDX)
              ADD AC-CUR-BAL      TO TB-BAL-TOTAL(WS-IDX)
              ADD AC-AVG-BAL      TO TB-AVG-TOTAL(WS-IDX)
              ADD AC-CREDIT-LIMIT TO TB-LIMIT-TOTAL(WS-IDX)
              PERFORM 3200-READ-DELINQUENCY
           END-IF.

       3200-READ-DELINQUENCY.
           MOVE AC-ACCT-NO TO DL-ACCT-NO
           READ KZDLQF KEY IS DL-ACCT-NO
              INVALID KEY
                 CONTINUE
              NOT INVALID KEY
                 IF WS-ST-DLQ NOT = "00"
                    DISPLAY "KZDLQF 読込失敗 ST=" WS-ST-DLQ
                    SET HARD-ERROR TO TRUE
                 ELSE
                    IF DL-PAST-DUE-DAYS > TB-MAX-DAYS(WS-IDX)
                       MOVE DL-PAST-DUE-DAYS TO TB-MAX-DAYS(WS-IDX)
                       MOVE DL-WATCH-RANK TO TB-WATCH-RANK(WS-IDX)
                    END-IF
                    ADD DL-DUE-AMT TO TB-DUE-TOTAL(WS-IDX)
                 END-IF
           END-READ.

       4000-ACCUM-COLLATERAL.
           MOVE LOW-VALUE TO CL-COLLATERAL-ID
           START KZCOLLF KEY IS >= CL-COLLATERAL-ID
              INVALID KEY
                 SET COLL-END TO TRUE
           END-START
           PERFORM UNTIL COLL-END OR HARD-ERROR
              READ KZCOLLF NEXT RECORD
                 AT END
                    SET COLL-END TO TRUE
                 NOT AT END
                    IF WS-ST-COLL NOT = "00"
                       DISPLAY "KZCOLLF 読込失敗 ST=" WS-ST-COLL
                       SET HARD-ERROR TO TRUE
                    ELSE
                       PERFORM 4100-ACCUM-ONE-COLL
                    END-IF
              END-READ
           END-PERFORM.

       4100-ACCUM-ONE-COLL.
           PERFORM 8100-FIND-COLL-CUSTOMER
           IF WS-IDX = ZERO
              DISPLAY "担保顧客未登録 担保=" CL-COLLATERAL-ID
              ADD 1 TO WS-ERR-CNT
           ELSE
              COMPUTE WS-COLL-NET =
                 CL-APPRAISAL-AMT
                 - (CL-APPRAISAL-AMT * CL-HAIRCUT-RATE / 100)
              ADD WS-COLL-NET TO TB-COLL-TOTAL(WS-IDX)
              MOVE "Y" TO TB-HAS-COLL(WS-IDX)
           END-IF.

       5000-ACCUM-EXPOSURE.
           PERFORM UNTIL EXPR-END OR HARD-ERROR
              READ KZEXPRF
                 AT END
                    SET EXPR-END TO TRUE
                 NOT AT END
                    IF WS-ST-EXPR NOT = "00"
                       DISPLAY "KZEXPRF 読込失敗 ST=" WS-ST-EXPR
                       SET HARD-ERROR TO TRUE
                    ELSE
                       PERFORM 5100-ACCUM-ONE-EXPR
                    END-IF
              END-READ
           END-PERFORM.

       5100-ACCUM-ONE-EXPR.
           PERFORM 8200-FIND-EXPR-CUSTOMER
           IF WS-IDX = ZERO
              DISPLAY "与信集計顧客未登録 顧客=" XR-CUST-ID
              ADD 1 TO WS-ERR-CNT
           ELSE
              EVALUATE XR-PRODUCT-TYPE
                 WHEN "01"
                 WHEN "02"
                 WHEN "03"
                    ADD XR-EXPOSURE-AMT TO TB-EXPOSURE-TOTAL(WS-IDX)
                    ADD XR-CAPPED-AMT   TO TB-CAPPED-TOTAL(WS-IDX)
                    MOVE "Y" TO TB-HAS-EXPR(WS-IDX)
                    IF XR-OVER-FLAG = "1"
                       MOVE "1" TO TB-OVER-FLAG(WS-IDX)
                    END-IF
                 WHEN OTHER
                    DISPLAY "商品区分不正 顧客=" XR-CUST-ID
                    MOVE "Y" TO TB-HOLD-FLAG(WS-IDX)
                    MOVE "商品区分不正" TO TB-REASON(WS-IDX)
              END-EVALUATE
           END-IF.

       6000-WRITE-SCORES.
           PERFORM VARYING WS-IDX FROM 1 BY 1
              UNTIL WS-IDX > WS-CUST-CNT OR HARD-ERROR
              PERFORM 6100-CALC-SCORE
              PERFORM 6200-STORE-SCORE
           END-PERFORM.

       6100-CALC-SCORE.
           MOVE 50 TO WS-SCORE
           IF TB-KYC-STATUS(WS-IDX) NOT = "1"
              AND TB-KYC-STATUS(WS-IDX) NOT = "0"
              AND TB-KYC-STATUS(WS-IDX) NOT = "9"
              MOVE "Y" TO TB-HOLD-FLAG(WS-IDX)
              IF TB-REASON(WS-IDX) = SPACE
                 MOVE "本人確認区分不正" TO TB-REASON(WS-IDX)
              END-IF
           END-IF
           IF TB-ACCT-CNT(WS-IDX) = ZERO
              SUBTRACT 10 FROM WS-SCORE
              IF TB-REASON(WS-IDX) = SPACE
                 MOVE "口座情報欠落" TO TB-REASON(WS-IDX)
              END-IF
           END-IF
           IF TB-HAS-EXPR(WS-IDX) NOT = "Y"
              MOVE "Y" TO TB-HOLD-FLAG(WS-IDX)
              IF TB-REASON(WS-IDX) = SPACE
                 MOVE "与信集計欠落" TO TB-REASON(WS-IDX)
              END-IF
           END-IF
           IF TB-HAS-COLL(WS-IDX) NOT = "Y"
              SUBTRACT 5 FROM WS-SCORE
              IF TB-REASON(WS-IDX) = SPACE
                 MOVE "担保情報欠落" TO TB-REASON(WS-IDX)
              END-IF
           END-IF
           IF TB-AVG-TOTAL(WS-IDX) >= 5000000
              ADD 12 TO WS-SCORE
           ELSE
              IF TB-AVG-TOTAL(WS-IDX) >= 1200000
                 ADD 6 TO WS-SCORE
              ELSE
                 IF TB-AVG-TOTAL(WS-IDX) < 100000
                    SUBTRACT 4 FROM WS-SCORE
                 END-IF
              END-IF
           END-IF
           IF TB-MAX-DAYS(WS-IDX) >= 90
              SUBTRACT 35 FROM WS-SCORE
           ELSE
              IF TB-MAX-DAYS(WS-IDX) >= 30
                 SUBTRACT 20 FROM WS-SCORE
              ELSE
                 IF TB-MAX-DAYS(WS-IDX) > ZERO
                    SUBTRACT 8 FROM WS-SCORE
                 END-IF
              END-IF
           END-IF
           IF TB-COLL-TOTAL(WS-IDX) > TB-CAPPED-TOTAL(WS-IDX)
              ADD 10 TO WS-SCORE
           ELSE
              IF TB-COLL-TOTAL(WS-IDX) > ZERO
                 ADD 4 TO WS-SCORE
              END-IF
           END-IF
           IF TB-OVER-FLAG(WS-IDX) = "1"
              SUBTRACT 15 FROM WS-SCORE
              IF TB-REASON(WS-IDX) = SPACE
                 MOVE "限度超過あり" TO TB-REASON(WS-IDX)
              END-IF
           END-IF
           IF WS-SCORE > 99
              MOVE 99 TO WS-SCORE
           END-IF
           IF WS-SCORE < ZERO
              MOVE ZERO TO WS-SCORE
           END-IF.

       6200-STORE-SCORE.
           MOVE SPACES TO KZSCORF-REC
           MOVE TB-CUST-ID(WS-IDX) TO SC-CUST-ID
           MOVE WS-TODAY           TO SC-SCORE-DATE
           MOVE WS-SCORE           TO SC-SCORE-POINT
           IF TB-HOLD-FLAG(WS-IDX) = "Y"
              MOVE "H" TO SC-GRADE-CODE
              ADD 1 TO WS-HOLD-CNT
           ELSE
              EVALUATE TRUE
                 WHEN WS-SCORE >= 80
                    MOVE "A" TO SC-GRADE-CODE
                 WHEN WS-SCORE >= 65
                    MOVE "B" TO SC-GRADE-CODE
                 WHEN WS-SCORE >= 50
                    MOVE "C" TO SC-GRADE-CODE
                 WHEN WS-SCORE >= 35
                    MOVE "D" TO SC-GRADE-CODE
                 WHEN OTHER
                    MOVE "E" TO SC-GRADE-CODE
              END-EVALUATE
           END-IF
           IF TB-REASON(WS-IDX) = SPACE
              MOVE "通常算定" TO SC-GRADE-REASON
           ELSE
              MOVE TB-REASON(WS-IDX) TO SC-GRADE-REASON
           END-IF
           PERFORM 6300-MAKE-HASH
           MOVE WS-HASH TO SC-INPUT-HASH
           READ KZSCORF KEY IS SC-CUST-ID
              INVALID KEY
                 WRITE KZSCORF-REC
                 IF WS-ST-SCOR NOT = "00"
                    DISPLAY "KZSCORF 書込失敗 顧客="
                       TB-CUST-ID(WS-IDX) " ST=" WS-ST-SCOR
                    SET HARD-ERROR TO TRUE
                 ELSE
                    ADD 1 TO WS-OUT-CNT
                 END-IF
              NOT INVALID KEY
                 MOVE TB-CUST-ID(WS-IDX) TO SC-CUST-ID
                 MOVE WS-TODAY           TO SC-SCORE-DATE
                 MOVE WS-SCORE           TO SC-SCORE-POINT
                 IF TB-HOLD-FLAG(WS-IDX) = "Y"
                    MOVE "H" TO SC-GRADE-CODE
                 ELSE
                    EVALUATE TRUE
                       WHEN WS-SCORE >= 80
                          MOVE "A" TO SC-GRADE-CODE
                       WHEN WS-SCORE >= 65
                          MOVE "B" TO SC-GRADE-CODE
                       WHEN WS-SCORE >= 50
                          MOVE "C" TO SC-GRADE-CODE
                       WHEN WS-SCORE >= 35
                          MOVE "D" TO SC-GRADE-CODE
                       WHEN OTHER
                          MOVE "E" TO SC-GRADE-CODE
                    END-EVALUATE
                 END-IF
                 IF TB-REASON(WS-IDX) = SPACE
                    MOVE "通常算定" TO SC-GRADE-REASON
                 ELSE
                    MOVE TB-REASON(WS-IDX) TO SC-GRADE-REASON
                 END-IF
                 MOVE WS-HASH TO SC-INPUT-HASH
                 REWRITE KZSCORF-REC
                 IF WS-ST-SCOR NOT = "00"
                    DISPLAY "KZSCORF 更新失敗 顧客="
                       TB-CUST-ID(WS-IDX) " ST=" WS-ST-SCOR
                    SET HARD-ERROR TO TRUE
                 ELSE
                    ADD 1 TO WS-OUT-CNT
                 END-IF
           END-READ.

       6300-MAKE-HASH.
           COMPUTE WS-HASH-WORK =
              TB-BAL-TOTAL(WS-IDX)
              + TB-LIMIT-TOTAL(WS-IDX)
              + TB-COLL-TOTAL(WS-IDX)
              + TB-CAPPED-TOTAL(WS-IDX)
              + TB-DUE-TOTAL(WS-IDX)
              + WS-SCORE
           IF WS-HASH-WORK < ZERO
              COMPUTE WS-HASH-WORK = WS-HASH-WORK * -1
           END-IF
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH-WORK 999999937).

       8000-FIND-CUSTOMER.
           MOVE ZERO TO WS-IDX
           PERFORM VARYING WS-POINT FROM 1 BY 1
              UNTIL WS-POINT > WS-CUST-CNT OR WS-IDX NOT = ZERO
              IF TB-CUST-ID(WS-POINT) = AC-CUST-ID
                 MOVE WS-POINT TO WS-IDX
              END-IF
           END-PERFORM.

       8100-FIND-COLL-CUSTOMER.
           MOVE ZERO TO WS-IDX
           PERFORM VARYING WS-POINT FROM 1 BY 1
              UNTIL WS-POINT > WS-CUST-CNT OR WS-IDX NOT = ZERO
              IF TB-CUST-ID(WS-POINT) = CL-CUST-ID
                 MOVE WS-POINT TO WS-IDX
              END-IF
           END-PERFORM.

       8200-FIND-EXPR-CUSTOMER.
           MOVE ZERO TO WS-IDX
           PERFORM VARYING WS-POINT FROM 1 BY 1
              UNTIL WS-POINT > WS-CUST-CNT OR WS-IDX NOT = ZERO
              IF TB-CUST-ID(WS-POINT) = XR-CUST-ID
                 MOVE WS-POINT TO WS-IDX
              END-IF
           END-PERFORM.

       9000-CLOSE-FILES.
           CLOSE KZCUSTF
           CLOSE KZACCTF
           CLOSE KZDLQF
           CLOSE KZCOLLF
           CLOSE KZEXPRF
           CLOSE KZSCORF.
