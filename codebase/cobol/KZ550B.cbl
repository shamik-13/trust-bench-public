       IDENTIFICATION DIVISION.
       PROGRAM-ID. KZ550B.
      * 変更履歴
      * 版数  年月日(和暦)  担当                       概要
      * 1.00  令和04.04.01  システム部 勘定系チーム  新規作成
      * 1.01  令和05.10.16  システム部 勘定系チーム  入金照合条件追加
      * 1.02  令和06.07.08  システム部 勘定系チーム  消込結果出力修正
       AUTHOR. KZB.
      *
      * 入金消込バッチ。
      * 入金を手数料、遅延損害金、延滞元本の順で充当する。
      * 経過日数、経過区分、猶予日数、回収判定は本処理で扱わない。
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZPMTF ASSIGN TO 'KZPMTF'
              ORGANIZATION IS SEQUENTIAL
              ACCESS MODE  IS SEQUENTIAL
              FILE STATUS  IS WS-PM-STATUS.
           SELECT KZDLQF ASSIGN TO 'KZDLQF'
              ORGANIZATION IS SEQUENTIAL
              ACCESS MODE  IS SEQUENTIAL
              FILE STATUS  IS WS-DQ-STATUS.
           SELECT KZGLPF ASSIGN TO 'KZGLPF'
              ORGANIZATION IS SEQUENTIAL
              ACCESS MODE  IS SEQUENTIAL
              FILE STATUS  IS WS-GP-STATUS.
           SELECT KZAUDF ASSIGN TO 'KZAUDF'
              ORGANIZATION IS SEQUENTIAL
              ACCESS MODE  IS SEQUENTIAL
              FILE STATUS  IS WS-AU-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  KZPMTF.
           COPY KZPMTCF.
       FD  KZDLQF.
           COPY KZDLQFC.
       FD  KZGLPF.
           COPY KZGLPCF.
       FD  KZAUDF.
           COPY KZAUDCF.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05 WS-PM-STATUS              PIC X(02) VALUE SPACES.
           05 WS-DQ-STATUS              PIC X(02) VALUE SPACES.
           05 WS-GP-STATUS              PIC X(02) VALUE SPACES.
           05 WS-AU-STATUS              PIC X(02) VALUE SPACES.

       01  WS-EOF-FLAGS.
           05 WS-PM-EOF                 PIC X VALUE 'N'.
              88 PM-EOF                       VALUE 'Y'.
           05 WS-DQ-EOF                 PIC X VALUE 'N'.
              88 DQ-EOF                       VALUE 'Y'.

       01  WS-OPEN-FLAGS.
           05 WS-PM-OPEN                PIC X VALUE 'N'.
              88 PM-OPEN                      VALUE 'Y'.
           05 WS-DQ-OPEN                PIC X VALUE 'N'.
              88 DQ-OPEN                      VALUE 'Y'.
           05 WS-GP-OPEN                PIC X VALUE 'N'.
              88 GP-OPEN                      VALUE 'Y'.
           05 WS-AU-OPEN                PIC X VALUE 'N'.
              88 AU-OPEN                      VALUE 'Y'.

       01  WS-CURRENT-ACCOUNT.
           05 WS-GRP-ACCT-NO            PIC X(20) VALUE SPACES.
           05 WS-PREV-ACCT-NO           PIC X(20) VALUE SPACES.
           05 WS-PREV-PMT-DATE          PIC 9(08) VALUE ZERO.
           05 WS-PREV-APPLY-SEQ         PIC 9(09) VALUE ZERO.

       01  WS-AMOUNTS.
           05 WS-PAYMENT-LEFT           PIC S9(13)V99 COMP-3
                                           VALUE ZERO.
           05 WS-TRANCHE-AMT            PIC S9(13)V99 COMP-3
                                           VALUE ZERO.
           05 WS-ACCT-PMT-TOTAL         PIC S9(13)V99 COMP-3
                                           VALUE ZERO.
           05 WS-FEE-BAL                PIC S9(13)V99 COMP-3
                                           VALUE ZERO.
           05 WS-LATE-BAL               PIC S9(13)V99 COMP-3
                                           VALUE ZERO.
           05 WS-PRIN-BAL               PIC S9(13)V99 COMP-3
                                           VALUE ZERO.
           05 WS-OLD-OVERDUE            PIC S9(13)V99 COMP-3
                                           VALUE ZERO.
           05 WS-NEW-OVERDUE            PIC S9(13)V99 COMP-3
                                           VALUE ZERO.

       01  WS-COUNTERS.
           05 WS-PM-READ-CNT            PIC 9(09) VALUE ZERO.
           05 WS-DQ-READ-CNT            PIC 9(09) VALUE ZERO.
           05 WS-GP-WRITE-CNT           PIC 9(09) VALUE ZERO.
           05 WS-AU-WRITE-CNT           PIC 9(09) VALUE ZERO.
           05 WS-DQ-REWRITE-CNT         PIC 9(09) VALUE ZERO.

       01  WS-STATUS-WORK.
           05 WS-OLD-STATUS             PIC X(02) VALUE SPACES.
           05 WS-NEW-STATUS             PIC X(02) VALUE SPACES.
           05 WS-TRANCHE-TYPE           PIC X(01) VALUE SPACES.
              88 TR-FEE                       VALUE 'F'.
              88 TR-LATE                      VALUE 'L'.
              88 TR-PRIN                      VALUE 'P'.
           05 WS-ABEND-SW               PIC X VALUE 'N'.
              88 HARD-ERROR                   VALUE 'Y'.

       01  WS-KZ531S-PARM.
           05 K531-TRANCHE-TYPE         PIC X(01) VALUE SPACES.
           05 K531-PMT-TYPE             PIC X(02) VALUE SPACES.
           05 K531-DEBIT-ACCT-CD        PIC X(12) VALUE SPACES.
           05 K531-CREDIT-ACCT-CD       PIC X(12) VALUE SPACES.
           05 K531-COST-CENTER-CD       PIC X(06) VALUE SPACES.
           05 K531-RETURN-CD            PIC 9(02) VALUE ZERO.

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE 0 TO RETURN-CODE
           PERFORM 1000-OPEN-FILES
           IF NOT HARD-ERROR
              PERFORM 2000-READ-PAYMENT
              PERFORM 2100-READ-DELINQ
              PERFORM 3000-PROCESS UNTIL PM-EOF OR HARD-ERROR
           END-IF
           PERFORM 9000-CLOSE-FILES
           IF HARD-ERROR
              MOVE 8 TO RETURN-CODE
           ELSE
              MOVE 0 TO RETURN-CODE
              DISPLAY 'KZ550B 正常終了 入金=' WS-PM-READ-CNT
              DISPLAY 'GL=' WS-GP-WRITE-CNT
                      ' 監査=' WS-AU-WRITE-CNT
           END-IF
           GOBACK.

       1000-OPEN-FILES.
           OPEN INPUT KZPMTF
           IF WS-PM-STATUS NOT = '00'
              DISPLAY 'KZPMTF オープン失敗 ST=' WS-PM-STATUS
              SET HARD-ERROR TO TRUE
           ELSE
              SET PM-OPEN TO TRUE
           END-IF
           IF NOT HARD-ERROR
              OPEN I-O KZDLQF
              IF WS-DQ-STATUS NOT = '00'
                 DISPLAY 'KZDLQF オープン失敗 ST=' WS-DQ-STATUS
                 SET HARD-ERROR TO TRUE
              ELSE
                 SET DQ-OPEN TO TRUE
              END-IF
           END-IF
           IF NOT HARD-ERROR
              OPEN OUTPUT KZGLPF
              IF WS-GP-STATUS NOT = '00'
                 DISPLAY 'KZGLPF オープン失敗 ST=' WS-GP-STATUS
                 SET HARD-ERROR TO TRUE
              ELSE
                 SET GP-OPEN TO TRUE
              END-IF
           END-IF
           IF NOT HARD-ERROR
              OPEN OUTPUT KZAUDF
              IF WS-AU-STATUS NOT = '00'
                 DISPLAY 'KZAUDF オープン失敗 ST=' WS-AU-STATUS
                 SET HARD-ERROR TO TRUE
              ELSE
                 SET AU-OPEN TO TRUE
              END-IF
           END-IF.

       2000-READ-PAYMENT.
           READ KZPMTF
              AT END
                 SET PM-EOF TO TRUE
              NOT AT END
                 ADD 1 TO WS-PM-READ-CNT
                 PERFORM 2200-VALIDATE-PAYMENT
           END-READ.

       2100-READ-DELINQ.
           READ KZDLQF
              AT END
                 SET DQ-EOF TO TRUE
              NOT AT END
                 ADD 1 TO WS-DQ-READ-CNT
                 PERFORM 2300-VALIDATE-DELINQ
           END-READ.

       2200-VALIDATE-PAYMENT.
           IF PM-ACCT-NO = SPACES
              DISPLAY '入金口座番号不正'
              SET HARD-ERROR TO TRUE
           END-IF
           IF PM-PMT-DATE = ZERO
              DISPLAY '入金日不正'
              DISPLAY '口座=' PM-ACCT-NO
              SET HARD-ERROR TO TRUE
           END-IF
           IF PM-PMT-AMT <= ZERO
              DISPLAY '入金額不正'
              DISPLAY '口座=' PM-ACCT-NO
              SET HARD-ERROR TO TRUE
           END-IF
           IF WS-PREV-ACCT-NO NOT = SPACES
              IF PM-ACCT-NO < WS-PREV-ACCT-NO
                 DISPLAY '入金ファイル順序不正'
                 DISPLAY '口座=' PM-ACCT-NO
                 SET HARD-ERROR TO TRUE
              END-IF
              IF PM-ACCT-NO = WS-PREV-ACCT-NO
                 IF PM-PMT-DATE < WS-PREV-PMT-DATE
                    DISPLAY '入金日順序不正'
                    DISPLAY '口座=' PM-ACCT-NO
                    SET HARD-ERROR TO TRUE
                 END-IF
              END-IF
           END-IF
           IF NOT HARD-ERROR
              MOVE PM-ACCT-NO TO WS-PREV-ACCT-NO
              MOVE PM-PMT-DATE TO WS-PREV-PMT-DATE
              MOVE PM-APPLY-SEQ-NO TO WS-PREV-APPLY-SEQ
           END-IF.

       2300-VALIDATE-DELINQ.
           IF DQ-ACCT-NO = SPACES
              DISPLAY '延滞口座番号不正'
              SET HARD-ERROR TO TRUE
           END-IF
           IF DQ-OVERDUE-AMT < ZERO
              DISPLAY '延滞残高不正'
              DISPLAY '口座=' DQ-ACCT-NO
              SET HARD-ERROR TO TRUE
           END-IF
           IF DQ-CURR-STATUS NOT = '00'
              AND DQ-CURR-STATUS NOT = '10'
              AND DQ-CURR-STATUS NOT = '30'
              DISPLAY '口座状態不正'
              DISPLAY '口座=' DQ-ACCT-NO
              DISPLAY '状態=' DQ-CURR-STATUS
              SET HARD-ERROR TO TRUE
           END-IF.

       3000-PROCESS.
           MOVE PM-ACCT-NO TO WS-GRP-ACCT-NO
           PERFORM 3100-LOCATE-DELINQ
           IF NOT HARD-ERROR
              PERFORM 3200-INIT-ACCOUNT
              PERFORM 3300-PROCESS-PAYMENT-GRP
                 UNTIL PM-EOF
                    OR PM-ACCT-NO NOT = WS-GRP-ACCT-NO
                    OR HARD-ERROR
              PERFORM 3800-REWRITE-DELINQ
           END-IF.

       3100-LOCATE-DELINQ.
           PERFORM UNTIL DQ-EOF
                    OR DQ-ACCT-NO >= WS-GRP-ACCT-NO
                    OR HARD-ERROR
              PERFORM 2100-READ-DELINQ
           END-PERFORM
           IF DQ-EOF
              DISPLAY '延滞明細未検出'
              DISPLAY '口座=' WS-GRP-ACCT-NO
              SET HARD-ERROR TO TRUE
           ELSE
              IF DQ-ACCT-NO NOT = WS-GRP-ACCT-NO
                 DISPLAY '延滞明細未検出'
                 DISPLAY '口座=' WS-GRP-ACCT-NO
                 SET HARD-ERROR TO TRUE
              END-IF
           END-IF.

       3200-INIT-ACCOUNT.
           MOVE ZERO TO WS-ACCT-PMT-TOTAL
           MOVE DQ-CURR-STATUS TO WS-OLD-STATUS
           MOVE DQ-CURR-STATUS TO WS-NEW-STATUS
           MOVE DQ-OVERDUE-AMT TO WS-OLD-OVERDUE
           MOVE DQ-OVERDUE-AMT TO WS-NEW-OVERDUE
           COMPUTE WS-FEE-BAL ROUNDED = DQ-OVERDUE-AMT * 0.020
           IF WS-FEE-BAL > 5000
              MOVE 5000 TO WS-FEE-BAL
           END-IF
           COMPUTE WS-LATE-BAL ROUNDED = DQ-OVERDUE-AMT * 0.030
           COMPUTE WS-PRIN-BAL =
              DQ-OVERDUE-AMT - WS-FEE-BAL - WS-LATE-BAL
           IF WS-PRIN-BAL < ZERO
              MOVE ZERO TO WS-PRIN-BAL
           END-IF.

       3300-PROCESS-PAYMENT-GRP.
           MOVE PM-PMT-AMT TO WS-PAYMENT-LEFT
           ADD PM-PMT-AMT TO WS-ACCT-PMT-TOTAL
           IF WS-PAYMENT-LEFT > ZERO AND WS-FEE-BAL > ZERO
              SET TR-FEE TO TRUE
              PERFORM 3400-ALLOCATE-TRANCHE
           END-IF
           IF WS-PAYMENT-LEFT > ZERO AND WS-LATE-BAL > ZERO
              SET TR-LATE TO TRUE
              PERFORM 3400-ALLOCATE-TRANCHE
           END-IF
           IF WS-PAYMENT-LEFT > ZERO AND WS-PRIN-BAL > ZERO
              SET TR-PRIN TO TRUE
              PERFORM 3400-ALLOCATE-TRANCHE
           END-IF
           PERFORM 2000-READ-PAYMENT.

       3400-ALLOCATE-TRANCHE.
           MOVE ZERO TO WS-TRANCHE-AMT
           EVALUATE TRUE
              WHEN TR-FEE
                 IF WS-PAYMENT-LEFT < WS-FEE-BAL
                    MOVE WS-PAYMENT-LEFT TO WS-TRANCHE-AMT
                 ELSE
                    MOVE WS-FEE-BAL TO WS-TRANCHE-AMT
                 END-IF
                 SUBTRACT WS-TRANCHE-AMT FROM WS-FEE-BAL
              WHEN TR-LATE
                 IF WS-PAYMENT-LEFT < WS-LATE-BAL
                    MOVE WS-PAYMENT-LEFT TO WS-TRANCHE-AMT
                 ELSE
                    MOVE WS-LATE-BAL TO WS-TRANCHE-AMT
                 END-IF
                 SUBTRACT WS-TRANCHE-AMT FROM WS-LATE-BAL
              WHEN TR-PRIN
                 IF WS-PAYMENT-LEFT < WS-PRIN-BAL
                    MOVE WS-PAYMENT-LEFT TO WS-TRANCHE-AMT
                 ELSE
                    MOVE WS-PRIN-BAL TO WS-TRANCHE-AMT
                 END-IF
                 SUBTRACT WS-TRANCHE-AMT FROM WS-PRIN-BAL
           END-EVALUATE
           IF WS-TRANCHE-AMT > ZERO
              SUBTRACT WS-TRANCHE-AMT FROM WS-PAYMENT-LEFT
              SUBTRACT WS-TRANCHE-AMT FROM WS-NEW-OVERDUE
              PERFORM 3500-CALL-GL-SERVICE
              PERFORM 3600-WRITE-GL
              PERFORM 3700-WRITE-AUDIT
           END-IF.

       3500-CALL-GL-SERVICE.
           MOVE WS-TRANCHE-TYPE TO K531-TRANCHE-TYPE
           MOVE PM-PMT-TYPE TO K531-PMT-TYPE
           MOVE SPACES TO K531-DEBIT-ACCT-CD
           MOVE SPACES TO K531-CREDIT-ACCT-CD
           MOVE SPACES TO K531-COST-CENTER-CD
           MOVE ZERO TO K531-RETURN-CD
           CALL 'KZ531S' USING WS-KZ531S-PARM
           IF K531-RETURN-CD NOT = ZERO
              DISPLAY 'GL科目取得失敗'
              DISPLAY '口座=' PM-ACCT-NO
              DISPLAY '区分=' WS-TRANCHE-TYPE
              DISPLAY 'RC=' K531-RETURN-CD
              SET HARD-ERROR TO TRUE
           END-IF.

       3600-WRITE-GL.
           IF NOT HARD-ERROR
              INITIALIZE KZGLPF-REC
              MOVE PM-ACCT-NO TO GP-ACCT-NO
              MOVE PM-PMT-DATE TO GP-GL-DATE
              MOVE K531-DEBIT-ACCT-CD TO GP-DEBIT-ACCT-CD
              MOVE K531-CREDIT-ACCT-CD TO GP-CREDIT-ACCT-CD
              MOVE WS-TRANCHE-AMT TO GP-JRNL-AMT
              MOVE WS-TRANCHE-TYPE TO GP-JRNL-TYPE
              MOVE K531-COST-CENTER-CD TO GP-COST-CENTER-CD
              WRITE KZGLPF-REC
              IF WS-GP-STATUS NOT = '00'
                 DISPLAY 'KZGLPF 書込失敗 ST=' WS-GP-STATUS
                 DISPLAY '口座=' PM-ACCT-NO
                 SET HARD-ERROR TO TRUE
              ELSE
                 ADD 1 TO WS-GP-WRITE-CNT
              END-IF
           END-IF.

       3700-WRITE-AUDIT.
           IF NOT HARD-ERROR
              INITIALIZE KZAUDF-REC
              MOVE PM-ACCT-NO TO AUD-ACCT-NO
              MOVE PM-PMT-DATE TO AUD-EVENT-DT
              MOVE 'PMT' TO AUD-EVENT-TYPE
              MOVE WS-OLD-STATUS TO AUD-OLD-STATUS
              MOVE WS-NEW-STATUS TO AUD-NEW-STATUS
              MOVE WS-TRANCHE-AMT TO AUD-CHANGE-AMT
              WRITE KZAUDF-REC
              IF WS-AU-STATUS NOT = '00'
                 DISPLAY 'KZAUDF 書込失敗 ST=' WS-AU-STATUS
                 DISPLAY '口座=' PM-ACCT-NO
                 SET HARD-ERROR TO TRUE
              ELSE
                 ADD 1 TO WS-AU-WRITE-CNT
              END-IF
           END-IF.

       3800-REWRITE-DELINQ.
           IF NOT HARD-ERROR
              IF WS-NEW-OVERDUE < ZERO
                 MOVE ZERO TO WS-NEW-OVERDUE
              END-IF
              MOVE WS-NEW-OVERDUE TO DQ-OVERDUE-AMT
              IF DQ-OVERDUE-AMT = ZERO
                 MOVE '00' TO DQ-CURR-STATUS
                 MOVE '00' TO WS-NEW-STATUS
              END-IF
              REWRITE KZDLQF-REC
              IF WS-DQ-STATUS NOT = '00'
                 DISPLAY 'KZDLQF 更新失敗 ST=' WS-DQ-STATUS
                 DISPLAY '口座=' DQ-ACCT-NO
                 SET HARD-ERROR TO TRUE
              ELSE
                 ADD 1 TO WS-DQ-REWRITE-CNT
                 IF DQ-OVERDUE-AMT = ZERO
                    DISPLAY '完済 口座=' DQ-ACCT-NO
                    DISPLAY '累計入金額=' WS-ACCT-PMT-TOTAL
                 END-IF
              END-IF
           END-IF.

       9000-CLOSE-FILES.
           IF PM-OPEN
              CLOSE KZPMTF
              IF WS-PM-STATUS NOT = '00'
                 DISPLAY 'KZPMTF クローズ失敗 ST=' WS-PM-STATUS
                 SET HARD-ERROR TO TRUE
              END-IF
           END-IF
           IF DQ-OPEN
              CLOSE KZDLQF
              IF WS-DQ-STATUS NOT = '00'
                 DISPLAY 'KZDLQF クローズ失敗 ST=' WS-DQ-STATUS
                 SET HARD-ERROR TO TRUE
              END-IF
           END-IF
           IF GP-OPEN
              CLOSE KZGLPF
              IF WS-GP-STATUS NOT = '00'
                 DISPLAY 'KZGLPF クローズ失敗 ST=' WS-GP-STATUS
                 SET HARD-ERROR TO TRUE
              END-IF
           END-IF
           IF AU-OPEN
              CLOSE KZAUDF
              IF WS-AU-STATUS NOT = '00'
                 DISPLAY 'KZAUDF クローズ失敗 ST=' WS-AU-STATUS
                 SET HARD-ERROR TO TRUE
              END-IF
           END-IF.
