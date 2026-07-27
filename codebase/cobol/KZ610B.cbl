       IDENTIFICATION DIVISION.
       PROGRAM-ID. KZ610B.
      * 変更履歴
      * 版数  年月日(和暦)  担当                      概要
      * 1.00  令和04年04月01日 システム部 勘定系チーム 新規作成
      * 1.10  令和05年10月16日 システム部 勘定系チーム 延滞解消判定追加
      * 1.20  令和06年06月03日 システム部 勘定系チーム 法的措置フラグ更新見直し
       AUTHOR. KZBATCH.
      *
      * 延滞解消・法的措置フラグ更新バッチ。
      * KZDLQF と KZDLRF を口座番号で突合し、延滞解消または
      * 法的措置対象の督促制御、回収ケース、監査、GL 出力を行う。
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.
      *
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZDLQF ASSIGN TO "KZDLQF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-ST-DLQ.
           SELECT KZDLRF ASSIGN TO "KZDLRF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-ST-DLR.
           SELECT KZCLCF ASSIGN TO "KZCLCF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS CLC-ACCT-NO
               FILE STATUS IS WS-ST-CLC.
           SELECT KZDNNF ASSIGN TO "KZDNNF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS DNN-ACCT-NO
               FILE STATUS IS WS-ST-DNN.
           SELECT KZAUDF ASSIGN TO "KZAUDF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS AUD-ACCT-NO
               FILE STATUS IS WS-ST-AUD.
           SELECT KZGLPF ASSIGN TO "KZGLPF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-ST-GLP.
      *
       DATA DIVISION.
       FILE SECTION.
      *
       FD  KZDLQF.
           COPY KZDLQFC.
      *
       FD  KZDLRF.
           COPY KZDLRFC.
      *
       FD  KZCLCF.
           COPY KZCLCCF.
      *
       FD  KZDNNF.
           COPY KZDNNCF.
      *
       FD  KZAUDF.
           COPY KZAUDCF.
      *
       FD  KZGLPF.
           COPY KZGLPCF.
      *
       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05 WS-ST-DLQ                  PIC XX VALUE SPACES.
           05 WS-ST-DLR                  PIC XX VALUE SPACES.
           05 WS-ST-CLC                  PIC XX VALUE SPACES.
           05 WS-ST-DNN                  PIC XX VALUE SPACES.
           05 WS-ST-AUD                  PIC XX VALUE SPACES.
           05 WS-ST-GLP                  PIC XX VALUE SPACES.
      *
       01  WS-FLAGS.
           05 WS-DLQ-EOF                 PIC X VALUE 'N'.
              88 DLQ-EOF                       VALUE 'Y'.
              88 DLQ-ACTIVE                    VALUE 'N'.
           05 WS-DLR-EOF                 PIC X VALUE 'N'.
              88 DLR-EOF                       VALUE 'Y'.
              88 DLR-ACTIVE                    VALUE 'N'.
           05 WS-HARD-ERROR              PIC X VALUE 'N'.
              88 HARD-ERROR                    VALUE 'Y'.
      *
       01  WS-WORK.
           05 WS-RUN-DATE                PIC 9(08) VALUE ZERO.
           05 WS-CURR-DATE-TIME          PIC X(21) VALUE SPACES.
           05 WS-PROC-ACCT-NO            PIC X(20) VALUE SPACES.
           05 WS-OLD-CLC-STATUS          PIC X(02) VALUE SPACES.
           05 WS-OLD-DNN-STOP            PIC X VALUE SPACE.
           05 WS-OLD-DNN-LEGAL           PIC X VALUE SPACE.
      *
       01  WS-COUNTERS.
           05 WS-CNT-DLQ-READ            PIC 9(09) VALUE ZERO.
           05 WS-CNT-DLR-READ            PIC 9(09) VALUE ZERO.
           05 WS-CNT-MATCH               PIC 9(09) VALUE ZERO.
           05 WS-CNT-CURED               PIC 9(09) VALUE ZERO.
           05 WS-CNT-LEGAL               PIC 9(09) VALUE ZERO.
           05 WS-CNT-UNCHANGED           PIC 9(09) VALUE ZERO.
           05 WS-CNT-DLQ-ONLY            PIC 9(09) VALUE ZERO.
           05 WS-CNT-DLR-ONLY            PIC 9(09) VALUE ZERO.
           05 WS-CNT-NO-CLC              PIC 9(09) VALUE ZERO.
           05 WS-CNT-NO-DNN              PIC 9(09) VALUE ZERO.
      *
       01  WS-CONSTANTS.
           05 WS-STATUS-NORMAL           PIC X(02) VALUE '00'.
           05 WS-STATUS-DELINQ           PIC X(02) VALUE '10'.
           05 WS-STATUS-COLLECT          PIC X(02) VALUE '30'.
           05 WS-CASE-CLOSED             PIC X(02) VALUE 'CL'.
           05 WS-CASE-LEGAL              PIC X(02) VALUE 'LG'.
           05 WS-FLAG-YES                PIC X VALUE 'Y'.
           05 WS-FLAG-NO                 PIC X VALUE 'N'.
           05 WS-AUD-CURE                PIC X(03) VALUE 'CUR'.
           05 WS-AUD-LEGAL               PIC X(03) VALUE 'LGL'.
           05 WS-GL-TYPE-REV             PIC X(03) VALUE 'REV'.
           05 WS-GL-DR-CD                PIC X(10) VALUE '4199000000'.
           05 WS-GL-CR-CD                PIC X(10) VALUE '1299000000'.
           05 WS-COST-CENTER             PIC X(06) VALUE '610B01'.
      *
       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE 0 TO RETURN-CODE
           PERFORM 1000-INITIALIZE
           IF NOT HARD-ERROR
              PERFORM 2000-PROCESS UNTIL DLQ-EOF AND DLR-EOF
           END-IF
           PERFORM 9000-TERMINATE
           GOBACK.
      *
       1000-INITIALIZE.
           MOVE FUNCTION CURRENT-DATE TO WS-CURR-DATE-TIME
           MOVE WS-CURR-DATE-TIME(1:8) TO WS-RUN-DATE
      *
           OPEN INPUT KZDLQF KZDLRF
                I-O   KZCLCF KZDNNF
                EXTEND KZAUDF
                OUTPUT KZGLPF
      *
           IF WS-ST-DLQ NOT = '00'
              DISPLAY 'KZDLQF オープン失敗 ST=' WS-ST-DLQ
              PERFORM 9900-SET-HARD-ERROR
           END-IF
           IF WS-ST-DLR NOT = '00'
              DISPLAY 'KZDLRF オープン失敗 ST=' WS-ST-DLR
              PERFORM 9900-SET-HARD-ERROR
           END-IF
           IF WS-ST-CLC NOT = '00'
              DISPLAY 'KZCLCF オープン失敗 ST=' WS-ST-CLC
              PERFORM 9900-SET-HARD-ERROR
           END-IF
           IF WS-ST-DNN NOT = '00'
              DISPLAY 'KZDNNF オープン失敗 ST=' WS-ST-DNN
              PERFORM 9900-SET-HARD-ERROR
           END-IF
           IF WS-ST-AUD NOT = '00'
              DISPLAY 'KZAUDF オープン失敗 ST=' WS-ST-AUD
              PERFORM 9900-SET-HARD-ERROR
           END-IF
           IF WS-ST-GLP NOT = '00'
              DISPLAY 'KZGLPF オープン失敗 ST=' WS-ST-GLP
              PERFORM 9900-SET-HARD-ERROR
           END-IF
      *
           IF NOT HARD-ERROR
              PERFORM 1100-READ-DLQ
              PERFORM 1200-READ-DLR
           END-IF.
      *
       1100-READ-DLQ.
           READ KZDLQF
              AT END
                 SET DLQ-EOF TO TRUE
              NOT AT END
                 ADD 1 TO WS-CNT-DLQ-READ
                 IF DQ-ACCT-NO = SPACES
                    DISPLAY 'KZDLQF 口座番号未設定'
                    PERFORM 9900-SET-HARD-ERROR
                 END-IF
           END-READ
           IF WS-ST-DLQ NOT = '00' AND WS-ST-DLQ NOT = '10'
              DISPLAY 'KZDLQF 読込失敗 ST=' WS-ST-DLQ
              PERFORM 9900-SET-HARD-ERROR
           END-IF.
      *
       1200-READ-DLR.
           READ KZDLRF
              AT END
                 SET DLR-EOF TO TRUE
              NOT AT END
                 ADD 1 TO WS-CNT-DLR-READ
                 IF DR-ACCT-NO = SPACES
                    DISPLAY 'KZDLRF 口座番号未設定'
                    PERFORM 9900-SET-HARD-ERROR
                 END-IF
           END-READ
           IF WS-ST-DLR NOT = '00' AND WS-ST-DLR NOT = '10'
              DISPLAY 'KZDLRF 読込失敗 ST=' WS-ST-DLR
              PERFORM 9900-SET-HARD-ERROR
           END-IF.
      *
       2000-PROCESS.
           IF HARD-ERROR
              EXIT PARAGRAPH
           END-IF
      *
           IF DLQ-ACTIVE AND DLR-ACTIVE
              EVALUATE TRUE
                 WHEN DQ-ACCT-NO = DR-ACCT-NO
                    ADD 1 TO WS-CNT-MATCH
                    MOVE DQ-ACCT-NO TO WS-PROC-ACCT-NO
                    PERFORM 3000-PROCESS-MATCH
                    PERFORM 1100-READ-DLQ
                    PERFORM 1200-READ-DLR
                 WHEN DQ-ACCT-NO < DR-ACCT-NO
                    ADD 1 TO WS-CNT-DLQ-ONLY
                    DISPLAY 'KZDLRF 未突合 口座=' DQ-ACCT-NO
                    PERFORM 1100-READ-DLQ
                 WHEN OTHER
                    ADD 1 TO WS-CNT-DLR-ONLY
                    DISPLAY 'KZDLQF 未突合 口座=' DR-ACCT-NO
                    PERFORM 1200-READ-DLR
              END-EVALUATE
           ELSE
              IF DLQ-ACTIVE
                 ADD 1 TO WS-CNT-DLQ-ONLY
                 DISPLAY 'KZDLRF 未突合 口座=' DQ-ACCT-NO
                 PERFORM 1100-READ-DLQ
              END-IF
              IF DLR-ACTIVE
                 ADD 1 TO WS-CNT-DLR-ONLY
                 DISPLAY 'KZDLQF 未突合 口座=' DR-ACCT-NO
                 PERFORM 1200-READ-DLR
              END-IF
           END-IF.
      *
       3000-PROCESS-MATCH.
           PERFORM 3100-VALIDATE-MATCH
           IF HARD-ERROR
              EXIT PARAGRAPH
           END-IF
      *
           MOVE WS-PROC-ACCT-NO TO CLC-ACCT-NO
           READ KZCLCF
              INVALID KEY
                 ADD 1 TO WS-CNT-NO-CLC
                 DISPLAY 'KZCLCF 未登録 口座=' WS-PROC-ACCT-NO
                 ADD 1 TO WS-CNT-UNCHANGED
                 EXIT PARAGRAPH
           END-READ
           IF WS-ST-CLC NOT = '00'
              DISPLAY 'KZCLCF 読込失敗 ST=' WS-ST-CLC
              DISPLAY '対象口座=' WS-PROC-ACCT-NO
              PERFORM 9900-SET-HARD-ERROR
              EXIT PARAGRAPH
           END-IF
      *
           MOVE WS-PROC-ACCT-NO TO DNN-ACCT-NO
           READ KZDNNF
              INVALID KEY
                 ADD 1 TO WS-CNT-NO-DNN
                 DISPLAY 'KZDNNF 未登録 口座=' WS-PROC-ACCT-NO
                 ADD 1 TO WS-CNT-UNCHANGED
                 EXIT PARAGRAPH
           END-READ
           IF WS-ST-DNN NOT = '00'
              DISPLAY 'KZDNNF 読込失敗 ST=' WS-ST-DNN
              DISPLAY '対象口座=' WS-PROC-ACCT-NO
              PERFORM 9900-SET-HARD-ERROR
              EXIT PARAGRAPH
           END-IF
      *
           MOVE CLC-CASE-STATUS TO WS-OLD-CLC-STATUS
           MOVE DNN-STOP-FLAG   TO WS-OLD-DNN-STOP
           MOVE DNN-LEGAL-FLAG  TO WS-OLD-DNN-LEGAL
      *
           EVALUATE TRUE
              WHEN CLC-CASE-STATUS = WS-CASE-LEGAL
                 PERFORM 4000-APPLY-LEGAL
              WHEN DQ-OVERDUE-AMT = ZERO
               AND DR-LATE-CHARGE-AMT = ZERO
                 PERFORM 5000-APPLY-CURE
              WHEN OTHER
                 ADD 1 TO WS-CNT-UNCHANGED
           END-EVALUATE.
      *
       3100-VALIDATE-MATCH.
           IF DQ-CURR-STATUS NOT = WS-STATUS-NORMAL
              AND DQ-CURR-STATUS NOT = WS-STATUS-DELINQ
              AND DQ-CURR-STATUS NOT = WS-STATUS-COLLECT
              DISPLAY '延滞現状態不正 口座=' DQ-ACCT-NO
              DISPLAY '状態=' DQ-CURR-STATUS
              PERFORM 9900-SET-HARD-ERROR
           END-IF
           IF DR-NEW-STATUS NOT = WS-STATUS-NORMAL
              AND DR-NEW-STATUS NOT = WS-STATUS-DELINQ
              AND DR-NEW-STATUS NOT = WS-STATUS-COLLECT
              DISPLAY '延滞新状態不正 口座=' DR-ACCT-NO
              DISPLAY '状態=' DR-NEW-STATUS
              PERFORM 9900-SET-HARD-ERROR
           END-IF
           IF DR-AGING-BUCKET NOT = 'B1'
              AND DR-AGING-BUCKET NOT = 'B2'
              AND DR-AGING-BUCKET NOT = 'B3'
              AND DR-AGING-BUCKET NOT = 'B4'
              DISPLAY '経過区分不正 口座=' DR-ACCT-NO
              DISPLAY '区分=' DR-AGING-BUCKET
              PERFORM 9900-SET-HARD-ERROR
           END-IF
           IF DQ-ASOF-DT = ZERO OR DR-DAYS-OVERDUE < ZERO
              DISPLAY '延滞基準値不正 口座=' DQ-ACCT-NO
              PERFORM 9900-SET-HARD-ERROR
           END-IF.
      *
       4000-APPLY-LEGAL.
           MOVE WS-FLAG-YES TO DNN-LEGAL-FLAG
           MOVE WS-FLAG-YES TO DNN-STOP-FLAG
           REWRITE KZDNNF-REC
           IF WS-ST-DNN NOT = '00'
              DISPLAY 'KZDNNF 法的措置更新失敗 ST=' WS-ST-DNN
              DISPLAY '対象口座=' WS-PROC-ACCT-NO
              PERFORM 9900-SET-HARD-ERROR
              EXIT PARAGRAPH
           END-IF
      *
           IF DR-LATE-CHARGE-AMT > ZERO
              PERFORM 4100-WRITE-GL-REVERSAL
           END-IF
           IF NOT HARD-ERROR
              PERFORM 6100-WRITE-LEGAL-AUDIT
              ADD 1 TO WS-CNT-LEGAL
           END-IF.
      *
       4100-WRITE-GL-REVERSAL.
           INITIALIZE KZGLPF-REC
           MOVE WS-PROC-ACCT-NO     TO GP-ACCT-NO
           MOVE WS-RUN-DATE         TO GP-GL-DATE
           MOVE WS-GL-DR-CD         TO GP-DEBIT-ACCT-CD
           MOVE WS-GL-CR-CD         TO GP-CREDIT-ACCT-CD
           MOVE DR-LATE-CHARGE-AMT  TO GP-JRNL-AMT
           MOVE WS-GL-TYPE-REV      TO GP-JRNL-TYPE
           MOVE WS-COST-CENTER      TO GP-COST-CENTER-CD
           WRITE KZGLPF-REC
           IF WS-ST-GLP NOT = '00'
              DISPLAY 'KZGLPF 手数料戻入出力失敗 ST=' WS-ST-GLP
              DISPLAY '対象口座=' WS-PROC-ACCT-NO
              PERFORM 9900-SET-HARD-ERROR
           END-IF.
      *
       5000-APPLY-CURE.
           MOVE WS-CASE-CLOSED TO CLC-CASE-STATUS
           MOVE WS-RUN-DATE    TO CLC-LAST-ACTION-DT
           REWRITE KZCLCF-REC
           IF WS-ST-CLC NOT = '00'
              DISPLAY 'KZCLCF 解消更新失敗 ST=' WS-ST-CLC
              DISPLAY '対象口座=' WS-PROC-ACCT-NO
              PERFORM 9900-SET-HARD-ERROR
              EXIT PARAGRAPH
           END-IF
      *
           MOVE WS-FLAG-NO TO DNN-STOP-FLAG
           REWRITE KZDNNF-REC
           IF WS-ST-DNN NOT = '00'
              DISPLAY 'KZDNNF 督促停止解除失敗 ST=' WS-ST-DNN
              DISPLAY '対象口座=' WS-PROC-ACCT-NO
              PERFORM 9900-SET-HARD-ERROR
              EXIT PARAGRAPH
           END-IF
      *
           PERFORM 6000-WRITE-CURE-AUDIT
           IF NOT HARD-ERROR
              ADD 1 TO WS-CNT-CURED
           END-IF.
      *
       6000-WRITE-CURE-AUDIT.
           INITIALIZE KZAUDF-REC
           MOVE WS-PROC-ACCT-NO    TO AUD-ACCT-NO
           MOVE WS-RUN-DATE        TO AUD-EVENT-DT
           MOVE WS-AUD-CURE        TO AUD-EVENT-TYPE
           MOVE WS-OLD-CLC-STATUS  TO AUD-OLD-STATUS
           MOVE WS-CASE-CLOSED     TO AUD-NEW-STATUS
           MOVE ZERO               TO AUD-CHANGE-AMT
           WRITE KZAUDF-REC
           IF WS-ST-AUD NOT = '00'
              DISPLAY 'KZAUDF 解消監査出力失敗 ST=' WS-ST-AUD
              DISPLAY '対象口座=' WS-PROC-ACCT-NO
              PERFORM 9900-SET-HARD-ERROR
           END-IF.
      *
       6100-WRITE-LEGAL-AUDIT.
           INITIALIZE KZAUDF-REC
           MOVE WS-PROC-ACCT-NO    TO AUD-ACCT-NO
           MOVE WS-RUN-DATE        TO AUD-EVENT-DT
           MOVE WS-AUD-LEGAL       TO AUD-EVENT-TYPE
           MOVE WS-OLD-DNN-LEGAL   TO AUD-OLD-STATUS
           MOVE WS-FLAG-YES        TO AUD-NEW-STATUS
           MOVE DR-LATE-CHARGE-AMT TO AUD-CHANGE-AMT
           WRITE KZAUDF-REC
           IF WS-ST-AUD NOT = '00'
              DISPLAY 'KZAUDF 法的監査出力失敗 ST=' WS-ST-AUD
              DISPLAY '対象口座=' WS-PROC-ACCT-NO
              PERFORM 9900-SET-HARD-ERROR
           END-IF.
      *
       9000-TERMINATE.
           CLOSE KZDLQF KZDLRF KZCLCF KZDNNF KZAUDF KZGLPF
           DISPLAY 'KZ610B 処理件数 DLQ=' WS-CNT-DLQ-READ
           DISPLAY 'KZ610B 処理件数 DLR=' WS-CNT-DLR-READ
           DISPLAY 'KZ610B 突合件数=' WS-CNT-MATCH
           DISPLAY 'KZ610B 解消更新件数=' WS-CNT-CURED
           DISPLAY 'KZ610B 法的措置件数=' WS-CNT-LEGAL
           DISPLAY 'KZ610B 変更なし件数=' WS-CNT-UNCHANGED
           DISPLAY 'KZ610B DLQ片寄件数=' WS-CNT-DLQ-ONLY
           DISPLAY 'KZ610B DLR片寄件数=' WS-CNT-DLR-ONLY
           DISPLAY 'KZ610B CLC未登録件数=' WS-CNT-NO-CLC
           DISPLAY 'KZ610B DNN未登録件数=' WS-CNT-NO-DNN
           IF HARD-ERROR
              MOVE 8 TO RETURN-CODE
              DISPLAY 'KZ610B 異常終了 RC=' RETURN-CODE
           ELSE
              MOVE 0 TO RETURN-CODE
              DISPLAY 'KZ610B 正常終了 RC=' RETURN-CODE
           END-IF.
      *
       9900-SET-HARD-ERROR.
           SET HARD-ERROR TO TRUE
           MOVE 8 TO RETURN-CODE.
