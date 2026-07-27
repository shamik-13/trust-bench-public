       IDENTIFICATION DIVISION.
       PROGRAM-ID. LF270B.
      *配当金充当計算バッチ
      *有効契約を対象に商品別配当率コードと責任準備金を用いて配
      *当金を算出し、現金支払、積立、保険料充当の配当充当区分を
      *設定する。解約受付中の契約は状態判定により保留し、返戻金
      *計算項目には関与しない。
      *
      *版数  年月日      担当      概要
      *--- --------- ---------- ----------------------
      * 1.0 20210601 システム部 初版作成
      * 1.1 20230115 保険部課  有効契約フィルタ及び
      *                        配当率テーブル追加
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LFPOLF2 ASSIGN TO 'LFPOLF2'
               ORGANIZATION IS INDEXED
               RECORD KEY IS PO-POL-NO
               FILE STATUS IS LFPOLF2-STATUS.
           SELECT LFRSVF ASSIGN TO 'LFRSVF'
               ORGANIZATION IS INDEXED
               RECORD KEY IS RS-POL-NO
               FILE STATUS IS LFRSVF-STATUS.
           SELECT LFDIVF ASSIGN TO 'LFDIVF'
               ORGANIZATION IS INDEXED
               RECORD KEY IS DV-POL-NO
               FILE STATUS IS LFDIVF-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  LFPOLF2.
       COPY LFPOLF2C.
       FD  LFRSVF.
       COPY LFRSVC.
       FD  LFDIVF.
       COPY LFDIVC.
       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05  LFPOLF2-STATUS     PIC XX.
           05  LFRSVF-STATUS      PIC XX.
           05  LFDIVF-STATUS      PIC XX.
       01  WS-PROCESS-FLAGS.
           05  WS-EOF-POLF        PIC 9 VALUE 0.
           05  WS-ERROR-FLAG      PIC 9 VALUE 0.
       01  WS-COUNTERS.
           05  WS-POLICY-READ     PIC 9(8) VALUE 0.
           05  WS-POLICY-CALC     PIC 9(8) VALUE 0.
           05  WS-POLICY-HOLD     PIC 9(8) VALUE 0.
           05  WS-ERROR-COUNT     PIC 9(8) VALUE 0.
       01  WS-WORK-FIELDS.
           05  WS-DIVIDEND-RATE   PIC 9V9999 VALUE 0.03.
           05  WS-CALC-AMOUNT     PIC 9(13)V99.
           05  WS-RESERVE-AMOUNT  PIC 9(13)V99.
           05  WS-ALLOC-KBN       PIC 9.
           05  WS-INDEX-VAL       PIC 99.
           05  WS-TARGET-YEAR     PIC 9(4) VALUE 2026.
       01  WS-ALLOCATION-LIMITS.
           05  WS-HIGH-THRESHOLD  PIC 9(13)V99 VALUE 10000000.00.
           05  WS-MID-THRESHOLD   PIC 9(13)V99 VALUE 5000000.00.
       01  WS-PRODUCT-RATE-TABLE.
           05  FILLER PIC X(10) VALUE 'RATE-TBL-'.
           05  WS-RATE-ENTRY OCCURS 6 TIMES.
               10  WS-RATE-PRODUCT-CODE   PIC X(4).
               10  WS-RATE-DIVIDEND-PCT   PIC 9V9999.
       01  WS-MESSAGES.
           05  WS-LOG-MESSAGE     PIC X(70).
       PROCEDURE DIVISION.
       0000-MAIN-PROCESS.
           MOVE 0 TO RETURN-CODE.
           PERFORM 1000-INIT-RATE-TABLE.
           PERFORM 1100-OPEN-INPUT-FILES.
           IF WS-ERROR-FLAG = 1
               MOVE 8 TO RETURN-CODE
               PERFORM 9000-CLOSE-ALL-FILES
               GOBACK
           END-IF.
           PERFORM 2000-PROCESS-POLICIES UNTIL WS-EOF-POLF = 1.
           PERFORM 9000-CLOSE-ALL-FILES.
           PERFORM 9100-PRINT-SUMMARY.
           IF WS-ERROR-COUNT > 0
               MOVE 8 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.
           GOBACK.
       1000-INIT-RATE-TABLE.
           MOVE '0001' TO WS-RATE-PRODUCT-CODE(1).
           MOVE 0.04 TO WS-RATE-DIVIDEND-PCT(1).
           MOVE '0002' TO WS-RATE-PRODUCT-CODE(2).
           MOVE 0.035 TO WS-RATE-DIVIDEND-PCT(2).
           MOVE '0003' TO WS-RATE-PRODUCT-CODE(3).
           MOVE 0.03 TO WS-RATE-DIVIDEND-PCT(3).
           MOVE '0004' TO WS-RATE-PRODUCT-CODE(4).
           MOVE 0.025 TO WS-RATE-DIVIDEND-PCT(4).
           MOVE '0005' TO WS-RATE-PRODUCT-CODE(5).
           MOVE 0.02 TO WS-RATE-DIVIDEND-PCT(5).
           MOVE '0010' TO WS-RATE-PRODUCT-CODE(6).
           MOVE 0.015 TO WS-RATE-DIVIDEND-PCT(6).
       1100-OPEN-INPUT-FILES.
           MOVE 0 TO WS-ERROR-FLAG.
           OPEN INPUT LFPOLF2.
           IF LFPOLF2-STATUS NOT = '00'
               MOVE '契約ファイルOPEN失敗' TO WS-LOG-MESSAGE
               STRING WS-LOG-MESSAGE DELIMITED BY SIZE
                   ' ST=' DELIMITED BY SIZE
                   LFPOLF2-STATUS DELIMITED BY SIZE
                   INTO WS-LOG-MESSAGE
               DISPLAY WS-LOG-MESSAGE
               MOVE 1 TO WS-ERROR-FLAG
               EXIT PARAGRAPH
           END-IF.
           OPEN INPUT LFRSVF.
           IF LFRSVF-STATUS NOT = '00'
               MOVE '準備金ファイルOPEN失敗' TO WS-LOG-MESSAGE
               STRING WS-LOG-MESSAGE DELIMITED BY SIZE
                   ' ST=' DELIMITED BY SIZE
                   LFRSVF-STATUS DELIMITED BY SIZE
                   INTO WS-LOG-MESSAGE
               DISPLAY WS-LOG-MESSAGE
               CLOSE LFPOLF2
               MOVE 1 TO WS-ERROR-FLAG
               EXIT PARAGRAPH
           END-IF.
           OPEN OUTPUT LFDIVF.
           IF LFDIVF-STATUS NOT = '00'
               MOVE '配当ファイルOPEN失敗' TO WS-LOG-MESSAGE
               STRING WS-LOG-MESSAGE DELIMITED BY SIZE
                   ' ST=' DELIMITED BY SIZE
                   LFDIVF-STATUS DELIMITED BY SIZE
                   INTO WS-LOG-MESSAGE
               DISPLAY WS-LOG-MESSAGE
               CLOSE LFPOLF2
               CLOSE LFRSVF
               MOVE 1 TO WS-ERROR-FLAG
           END-IF.
       2000-PROCESS-POLICIES.
           READ LFPOLF2
               AT END
                   MOVE 1 TO WS-EOF-POLF
               NOT AT END
                   ADD 1 TO WS-POLICY-READ
                   EVALUATE PO-CONTRACT-STATUS-KBN
                       WHEN '01'
                           PERFORM 3000-CALC-FOR-VALID
                       WHEN '02'
                           PERFORM 3000-CALC-FOR-VALID
                       WHEN '03'
                           PERFORM 4000-HOLD-PENDING
                       WHEN OTHER
                           ADD 1 TO WS-ERROR-COUNT
                   END-EVALUATE
           END-READ.
       3000-CALC-FOR-VALID.
           MOVE PO-POL-NO TO RS-POL-NO.
           READ LFRSVF
               INVALID KEY
                   MOVE '準備金未発見 契約=' TO WS-LOG-MESSAGE
                   STRING WS-LOG-MESSAGE DELIMITED BY SIZE
                       PO-POL-NO DELIMITED BY SIZE
                       INTO WS-LOG-MESSAGE
                   DISPLAY WS-LOG-MESSAGE
                   ADD 1 TO WS-ERROR-COUNT
               NOT INVALID KEY
                   PERFORM 3100-LOOKUP-RATE
                   PERFORM 3200-CALCULATE-DIVIDEND
                   PERFORM 3300-SET-ALLOCATION-KBN
                   PERFORM 3400-WRITE-DIVIDEND
                   ADD 1 TO WS-POLICY-CALC
           END-READ.
       3100-LOOKUP-RATE.
           MOVE 0.03 TO WS-DIVIDEND-RATE.
           PERFORM VARYING WS-INDEX-VAL FROM 1 BY 1
               UNTIL WS-INDEX-VAL > 6
               IF PO-PRODUCT-CD = 
                   WS-RATE-PRODUCT-CODE(WS-INDEX-VAL)
                   MOVE WS-RATE-DIVIDEND-PCT(WS-INDEX-VAL)
                       TO WS-DIVIDEND-RATE
                   EXIT PERFORM
               END-IF
           END-PERFORM.
       3200-CALCULATE-DIVIDEND.
           MOVE RS-RESERVE-AMT TO WS-RESERVE-AMOUNT.
           COMPUTE WS-CALC-AMOUNT = 
               FUNCTION INTEGER(
                   WS-RESERVE-AMOUNT * WS-DIVIDEND-RATE
               ).
       3300-SET-ALLOCATION-KBN.
           EVALUATE TRUE
               WHEN WS-RESERVE-AMOUNT > WS-HIGH-THRESHOLD
                   MOVE 2 TO WS-ALLOC-KBN
               WHEN WS-RESERVE-AMOUNT > WS-MID-THRESHOLD
                   MOVE 3 TO WS-ALLOC-KBN
               WHEN OTHER
                   MOVE 1 TO WS-ALLOC-KBN
           END-EVALUATE.
       3400-WRITE-DIVIDEND.
           MOVE PO-POL-NO TO DV-POL-NO.
           MOVE WS-TARGET-YEAR TO DV-DIV-YEAR.
           MOVE WS-CALC-AMOUNT TO DV-DIV-AMT.
           MOVE WS-ALLOC-KBN TO DV-DIV-ALLOC-KBN.
           MOVE 0 TO DV-DIV-STATUS-KBN.
           WRITE LFDIVF-REC.
           IF LFDIVF-STATUS NOT = '00'
               MOVE '配当書込失敗 契約=' TO WS-LOG-MESSAGE
               STRING WS-LOG-MESSAGE DELIMITED BY SIZE
                   PO-POL-NO DELIMITED BY SIZE
                   ' ST=' DELIMITED BY SIZE
                   LFDIVF-STATUS DELIMITED BY SIZE
                   INTO WS-LOG-MESSAGE
               DISPLAY WS-LOG-MESSAGE
               ADD 1 TO WS-ERROR-COUNT
           END-IF.
       4000-HOLD-PENDING.
           MOVE PO-POL-NO TO DV-POL-NO.
           MOVE WS-TARGET-YEAR TO DV-DIV-YEAR.
           MOVE 0 TO DV-DIV-AMT.
           MOVE 0 TO DV-DIV-ALLOC-KBN.
           MOVE 1 TO DV-DIV-STATUS-KBN.
           WRITE LFDIVF-REC.
           IF LFDIVF-STATUS NOT = '00'
               MOVE '保留書込失敗' TO WS-LOG-MESSAGE
               ADD 1 TO WS-ERROR-COUNT
           ELSE
               ADD 1 TO WS-POLICY-HOLD
           END-IF.
       9000-CLOSE-ALL-FILES.
           CLOSE LFPOLF2.
           CLOSE LFRSVF.
           CLOSE LFDIVF.
       9100-PRINT-SUMMARY.
           MOVE 'LF270B処理完了' TO WS-LOG-MESSAGE.
           STRING WS-LOG-MESSAGE DELIMITED BY SIZE
               ' 読込=' DELIMITED BY SIZE
               WS-POLICY-READ DELIMITED BY SIZE
               ' 計算=' DELIMITED BY SIZE
               WS-POLICY-CALC DELIMITED BY SIZE
               ' 保留=' DELIMITED BY SIZE
               WS-POLICY-HOLD DELIMITED BY SIZE
               ' 誤=' DELIMITED BY SIZE
               WS-ERROR-COUNT DELIMITED BY SIZE
               INTO WS-LOG-MESSAGE
           DISPLAY WS-LOG-MESSAGE.
