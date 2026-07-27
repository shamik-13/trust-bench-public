       IDENTIFICATION DIVISION.
       PROGRAM-ID. KZ440B.
      * 変更履歴
      * 版数   年月日(和暦)   担当                     概要
      * 1.00   平成30年04月01日 システム部 勘定系チーム 利息源泉計算新規作成
      * 1.01   令和02年01月06日 システム部 勘定系チーム 復興特別所得税率対応
      * 1.02   令和05年10月02日 システム部 勘定系チーム 月次集計端数処理見直し
       AUTHOR. KZ-BATCH.
      *================================================================*
      * 利息源泉計算バッチ                                             *
      * KZINTFの既計上済み正利息に対して源泉税を算出しKZTAXFへ出力する。*
      * 税区分確認は必ずKZ415Sへ委譲する。                             *
      *================================================================*

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZINTF
               ASSIGN TO "KZINTF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-KZINTF.

           SELECT KZIRMF
               ASSIGN TO "KZIRMF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS RM-RATE-KEY
               FILE STATUS IS FS-KZIRMF.

           SELECT KZCTLF
               ASSIGN TO "KZCTLF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CL-CONTROL-KEY
               FILE STATUS IS FS-KZCTLF.

           SELECT KZTAXF
               ASSIGN TO "KZTAXF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-KZTAXF.

           SELECT KZRCNF
               ASSIGN TO "KZRCNF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-KZRCNF.

       DATA DIVISION.
       FILE SECTION.

       FD  KZINTF.
           COPY KZINTFC.

       FD  KZIRMF.
           COPY KZIRMFC.

       FD  KZCTLF.
           COPY KZCTLFC.

       FD  KZTAXF.
           COPY KZTAXFC.

       FD  KZRCNF.
           COPY KZRCNFC.

       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      * ファイル状態                                                   *
      *----------------------------------------------------------------*
       01  FILE-STATUS-AREA.
           05  FS-KZINTF              PIC X(02) VALUE SPACE.
           05  FS-KZIRMF              PIC X(02) VALUE SPACE.
           05  FS-KZCTLF              PIC X(02) VALUE SPACE.
           05  FS-KZTAXF              PIC X(02) VALUE SPACE.
           05  FS-KZRCNF              PIC X(02) VALUE SPACE.

      *----------------------------------------------------------------*
      * KZ415S連絡領域                                                 *
      *----------------------------------------------------------------*
       01  KZ415S-LINKAGE-AREA.
           05  LK-IN-PRODUCT-CD       PIC X(6).
           05  LK-IN-BASE-DT          PIC 9(8).
           05  LK-IN-TAX-KBN          PIC X.
           05  LK-IN-USE-KBN          PIC X.
           05  LK-OUT-RESULT-CD       PIC X(2).
           05  LK-OUT-EFFECTIVE-DT    PIC 9(8).
           05  LK-OUT-INT-RATE        PIC S9(3)V9(6) COMP-3.
           05  LK-OUT-OD-RATE         PIC S9(3)V9(6) COMP-3.
           05  LK-OUT-MSG             PIC X(80).

      *----------------------------------------------------------------*
      * 定数                                                           *
      *----------------------------------------------------------------*
       01  CONSTANT-AREA.
           05  CN-PGM-ID              PIC X(08) VALUE "KZ440B".
           05  CN-CTL-KEY             PIC X(16) VALUE
               "KZ440B-CONTROL".
           05  CN-RECON-KEY           PIC X(16) VALUE
               "KZ440B-RECON  ".
           05  CN-STAT-NORMAL         PIC X(01) VALUE "N".
           05  CN-STAT-RUN            PIC X(01) VALUE "R".
           05  CN-STAT-ABEND          PIC X(01) VALUE "A".
           05  CN-DISP-NORMAL         PIC X(01) VALUE "0".
           05  CN-DISP-DIFF           PIC X(01) VALUE "1".
           05  CN-USE-ACTIVE          PIC X(01) VALUE "1".
           05  CN-TAXABLE             PIC X(01) VALUE "1".
           05  CN-NONTAX              PIC X(01) VALUE "0".
           05  CN-CORRECTED           PIC X(01) VALUE "9".
           05  CN-CORP-SPECIAL        PIC X(01) VALUE "C".
           05  CN-APPROVED            PIC X(01) VALUE "A".
           05  CN-NAT-RATE            PIC 9V9(05) VALUE 0.15315.
           05  CN-LOC-RATE            PIC 9V9(05) VALUE 0.05000.
           05  CN-ALL-RATE            PIC 9V9(05) VALUE 0.20315.

      *----------------------------------------------------------------*
      * 作業領域                                                       *
      *----------------------------------------------------------------*
       01  WORK-AREA.
           05  WK-EOF-SW              PIC X(01) VALUE "N".
               88  END-OF-KZINTF                VALUE "Y".
           05  WK-HARD-ERROR-SW       PIC X(01) VALUE "N".
               88  HARD-ERROR                   VALUE "Y".
           05  WK-PRODUCT-CD          PIC X(06) VALUE SPACE.
           05  WK-RATE-KEY            PIC X(32) VALUE SPACE.
           05  WK-BASE-DT             PIC 9(08) VALUE ZERO.
           05  WK-MASTER-TAX-KBN      PIC X(01) VALUE SPACE.
           05  WK-EFFECTIVE-TAX-KBN   PIC X(01) VALUE SPACE.
           05  WK-ROUND-UNIT          PIC 9(03) VALUE 1.
           05  WK-GROSS-AMT           PIC S9(13) VALUE ZERO.
           05  WK-NATIONAL-TAX        PIC S9(13) VALUE ZERO.
           05  WK-LOCAL-TAX           PIC S9(13) VALUE ZERO.
           05  WK-TAX-TOTAL           PIC S9(13) VALUE ZERO.
           05  WK-NET-AMT             PIC S9(13) VALUE ZERO.
           05  WK-UNIT-WORK           PIC S9(13)V9(5) VALUE ZERO.
           05  WK-ROUNDED-WORK        PIC S9(13) VALUE ZERO.
           05  WK-REASON-CD           PIC X(02) VALUE SPACE.

      *----------------------------------------------------------------*
      * 監査集計                                                       *
      *----------------------------------------------------------------*
       01  AUDIT-AREA.
           05  AU-READ-CNT            PIC 9(09) VALUE ZERO.
           05  AU-WRITE-CNT           PIC 9(09) VALUE ZERO.
           05  AU-SKIP-ZERO-CNT       PIC 9(09) VALUE ZERO.
           05  AU-SKIP-NONTAX-CNT     PIC 9(09) VALUE ZERO.
           05  AU-SKIP-CORP-CNT       PIC 9(09) VALUE ZERO.
           05  AU-SKIP-CORR-CNT       PIC 9(09) VALUE ZERO.
           05  AU-SKIP-MAST-CNT       PIC 9(09) VALUE ZERO.
           05  AU-GROSS-TOTAL         PIC S9(15) VALUE ZERO.
           05  AU-NATIONAL-TOTAL      PIC S9(15) VALUE ZERO.
           05  AU-LOCAL-TOTAL         PIC S9(15) VALUE ZERO.
           05  AU-TAX-TOTAL           PIC S9(15) VALUE ZERO.
           05  AU-NET-TOTAL           PIC S9(15) VALUE ZERO.
           05  AU-SKIP-TOTAL-AMT      PIC S9(15) VALUE ZERO.

       PROCEDURE DIVISION.
       MAIN-SECTION.
           PERFORM 0000-INIT
           IF NOT HARD-ERROR
               PERFORM 1000-MAIN UNTIL END-OF-KZINTF OR HARD-ERROR
           END-IF
           PERFORM 9000-FINAL
           GOBACK.

      *----------------------------------------------------------------*
      * 初期処理                                                       *
      *----------------------------------------------------------------*
       0000-INIT.
           MOVE 0 TO RETURN-CODE
           OPEN INPUT  KZINTF
                INPUT  KZIRMF
                I-O    KZCTLF
                OUTPUT KZTAXF
                OUTPUT KZRCNF

           IF FS-KZINTF NOT = "00"
               MOVE 12 TO RETURN-CODE
               MOVE "Y" TO WK-HARD-ERROR-SW
               DISPLAY "KZINTF OPEN ERROR ST=" FS-KZINTF
           END-IF

           IF FS-KZIRMF NOT = "00"
               MOVE 12 TO RETURN-CODE
               MOVE "Y" TO WK-HARD-ERROR-SW
               DISPLAY "KZIRMF OPEN ERROR ST=" FS-KZIRMF
           END-IF

           IF FS-KZCTLF NOT = "00"
               MOVE 12 TO RETURN-CODE
               MOVE "Y" TO WK-HARD-ERROR-SW
               DISPLAY "KZCTLF OPEN ERROR ST=" FS-KZCTLF
           END-IF

           IF FS-KZTAXF NOT = "00"
               MOVE 12 TO RETURN-CODE
               MOVE "Y" TO WK-HARD-ERROR-SW
               DISPLAY "KZTAXF OPEN ERROR ST=" FS-KZTAXF
           END-IF

           IF FS-KZRCNF NOT = "00"
               MOVE 12 TO RETURN-CODE
               MOVE "Y" TO WK-HARD-ERROR-SW
               DISPLAY "KZRCNF OPEN ERROR ST=" FS-KZRCNF
           END-IF

           IF NOT HARD-ERROR
               PERFORM 0100-READ-CONTROL
               IF NOT HARD-ERROR
                   PERFORM 0110-MARK-CONTROL-RUN
                   PERFORM 0200-READ-INTEREST
               END-IF
           END-IF.

       0100-READ-CONTROL.
           INITIALIZE KZCTLF-REC
           MOVE CN-CTL-KEY TO CL-CONTROL-KEY
           READ KZCTLF
               INVALID KEY
                   MOVE 12 TO RETURN-CODE
                   MOVE "Y" TO WK-HARD-ERROR-SW
                   DISPLAY "CONTROL NOT FOUND KEY=" CN-CTL-KEY
               NOT INVALID KEY
                   IF CL-RUN-STATUS = CN-STAT-RUN
                       MOVE 12 TO RETURN-CODE
                       MOVE "Y" TO WK-HARD-ERROR-SW
                       DISPLAY "PREVIOUS RUN NOT CLOSED KEY="
                               CN-CTL-KEY
                   END-IF
           END-READ.

       0110-MARK-CONTROL-RUN.
           IF NOT HARD-ERROR
               MOVE CN-STAT-RUN TO CL-RUN-STATUS
               MOVE CN-PGM-ID   TO CL-LAST-PGM-ID
               REWRITE KZCTLF-REC
                   INVALID KEY
                       MOVE 12 TO RETURN-CODE
                       MOVE "Y" TO WK-HARD-ERROR-SW
                       DISPLAY "CONTROL UPDATE ERROR ST=" FS-KZCTLF
               END-REWRITE
           END-IF.

       0200-READ-INTEREST.
           READ KZINTF
               AT END
                   MOVE "Y" TO WK-EOF-SW
               NOT AT END
                   ADD 1 TO AU-READ-CNT
                   IF FS-KZINTF NOT = "00"
                       MOVE 12 TO RETURN-CODE
                       MOVE "Y" TO WK-HARD-ERROR-SW
                       DISPLAY "KZINTF READ ERROR ST=" FS-KZINTF
                   END-IF
           END-READ.

      *----------------------------------------------------------------*
      * 主処理                                                         *
      *----------------------------------------------------------------*
       1000-MAIN.
           PERFORM 1100-PROCESS-RECORD
           IF NOT HARD-ERROR
               PERFORM 0200-READ-INTEREST
           END-IF.

       1100-PROCESS-RECORD.
           MOVE SPACE TO WK-REASON-CD
           MOVE IN-INT-AMT TO WK-GROSS-AMT

           IF IN-INT-AMT <= ZERO
               MOVE "01" TO WK-REASON-CD
               ADD 1 TO AU-SKIP-ZERO-CNT
               ADD IN-INT-AMT TO AU-SKIP-TOTAL-AMT
           ELSE
               PERFORM 1200-SET-PRODUCT
               PERFORM 1300-READ-RATE-MASTER
               IF NOT HARD-ERROR
                   PERFORM 1400-CALL-KZ415S
                   IF NOT HARD-ERROR
                       PERFORM 1500-JUDGE-TAXABLE
                       IF WK-REASON-CD = SPACE
                           PERFORM 1600-CALC-TAX
                           PERFORM 1700-WRITE-TAX
                       ELSE
                           ADD IN-INT-AMT TO AU-SKIP-TOTAL-AMT
                       END-IF
                   END-IF
               END-IF
           END-IF.

       1200-SET-PRODUCT.
           MOVE IN-ACCT-NO(1:6) TO WK-PRODUCT-CD
           MOVE IN-ACCRUAL-DT   TO WK-BASE-DT.

       1300-READ-RATE-MASTER.
           INITIALIZE KZIRMF-REC
           MOVE SPACE TO WK-RATE-KEY
           STRING WK-PRODUCT-CD DELIMITED BY SIZE
                  WK-BASE-DT    DELIMITED BY SIZE
               INTO WK-RATE-KEY
           END-STRING
           MOVE WK-RATE-KEY TO RM-RATE-KEY

           READ KZIRMF
               INVALID KEY
                   MOVE "05" TO WK-REASON-CD
                   ADD 1 TO AU-SKIP-MAST-CNT
                   DISPLAY "RATE MASTER NOT FOUND PRODUCT="
                           WK-PRODUCT-CD
                   DISPLAY "BASE DATE=" WK-BASE-DT
               NOT INVALID KEY
                   IF RM-APPROVAL-STAT NOT = CN-APPROVED
                       MOVE "06" TO WK-REASON-CD
                       ADD 1 TO AU-SKIP-MAST-CNT
                       DISPLAY "RATE MASTER NOT APPROVED PRODUCT="
                               RM-PRODUCT-CD
                   ELSE
                       MOVE RM-WITHHOLD-TAX-KBN TO
                           WK-MASTER-TAX-KBN
                   END-IF
           END-READ.

       1400-CALL-KZ415S.
           IF WK-REASON-CD = SPACE
               INITIALIZE KZ415S-LINKAGE-AREA
               MOVE WK-PRODUCT-CD      TO LK-IN-PRODUCT-CD
               MOVE WK-BASE-DT         TO LK-IN-BASE-DT
               MOVE WK-MASTER-TAX-KBN  TO LK-IN-TAX-KBN
               MOVE CN-USE-ACTIVE      TO LK-IN-USE-KBN

               CALL "KZ415S" USING KZ415S-LINKAGE-AREA
                   ON EXCEPTION
                       MOVE 12 TO RETURN-CODE
                       MOVE "Y" TO WK-HARD-ERROR-SW
                       DISPLAY "KZ415S CALL ERROR PRODUCT="
                               WK-PRODUCT-CD
               END-CALL

               IF NOT HARD-ERROR
                   IF LK-OUT-RESULT-CD = "00"
                       MOVE LK-IN-TAX-KBN TO WK-EFFECTIVE-TAX-KBN
                   ELSE
                       MOVE "07" TO WK-REASON-CD
                       ADD 1 TO AU-SKIP-MAST-CNT
                       DISPLAY "KZ415S REJECT PRODUCT="
                               WK-PRODUCT-CD
                       DISPLAY "KZ415S RESULT="
                               LK-OUT-RESULT-CD
                   END-IF
               END-IF
           END-IF.

       1500-JUDGE-TAXABLE.
           IF WK-REASON-CD = SPACE
               EVALUATE WK-EFFECTIVE-TAX-KBN
                   WHEN CN-TAXABLE
                       CONTINUE
                   WHEN CN-NONTAX
                       MOVE "02" TO WK-REASON-CD
                       ADD 1 TO AU-SKIP-NONTAX-CNT
                   WHEN CN-CORP-SPECIAL
                       MOVE "03" TO WK-REASON-CD
                       ADD 1 TO AU-SKIP-CORP-CNT
                   WHEN CN-CORRECTED
                       MOVE "04" TO WK-REASON-CD
                       ADD 1 TO AU-SKIP-CORR-CNT
                   WHEN OTHER
                       MOVE "08" TO WK-REASON-CD
                       ADD 1 TO AU-SKIP-MAST-CNT
                       DISPLAY "INVALID TAX KBN PRODUCT="
                               WK-PRODUCT-CD
                       DISPLAY "TAX KBN=" WK-EFFECTIVE-TAX-KBN
               END-EVALUATE
           END-IF.

       1600-CALC-TAX.
           EVALUATE WK-PRODUCT-CD(1:1)
               WHEN "1"
                   MOVE 1 TO WK-ROUND-UNIT
               WHEN "2"
                   MOVE 10 TO WK-ROUND-UNIT
               WHEN "3"
                   MOVE 100 TO WK-ROUND-UNIT
               WHEN OTHER
                   MOVE 1 TO WK-ROUND-UNIT
           END-EVALUATE

      *    源泉税は国税・地方税を合算した税率で総額を算定し、総額側で
      *    端数処理する。地方税は総額から国税を差引いた残額として求める。
           COMPUTE WK-UNIT-WORK =
               (WK-GROSS-AMT * CN-ALL-RATE) / WK-ROUND-UNIT
           COMPUTE WK-ROUNDED-WORK =
               FUNCTION INTEGER-PART(WK-UNIT-WORK) * WK-ROUND-UNIT
           MOVE WK-ROUNDED-WORK TO WK-TAX-TOTAL

           COMPUTE WK-UNIT-WORK =
               (WK-GROSS-AMT * CN-NAT-RATE) / WK-ROUND-UNIT
           COMPUTE WK-ROUNDED-WORK =
               FUNCTION INTEGER-PART(WK-UNIT-WORK) * WK-ROUND-UNIT
           MOVE WK-ROUNDED-WORK TO WK-NATIONAL-TAX

           COMPUTE WK-LOCAL-TAX =
               WK-TAX-TOTAL - WK-NATIONAL-TAX
           COMPUTE WK-NET-AMT =
               WK-GROSS-AMT - WK-TAX-TOTAL.

       1700-WRITE-TAX.
           INITIALIZE KZTAXF-REC
           MOVE IN-ACCT-NO       TO TX-ACCT-NO
           MOVE IN-CYCLE-ID      TO TX-CYCLE-ID
           MOVE IN-ACCRUAL-DT    TO TX-TAX-DT
           MOVE WK-GROSS-AMT     TO TX-GROSS-INT-AMT
           MOVE WK-NATIONAL-TAX  TO TX-NATIONAL-TAX-AMT
           MOVE WK-LOCAL-TAX     TO TX-LOCAL-TAX-AMT
           MOVE WK-NET-AMT       TO TX-NET-INT-AMT

           WRITE KZTAXF-REC
           IF FS-KZTAXF NOT = "00"
               MOVE 12 TO RETURN-CODE
               MOVE "Y" TO WK-HARD-ERROR-SW
               DISPLAY "KZTAXF WRITE ERROR ST=" FS-KZTAXF
           ELSE
               ADD 1               TO AU-WRITE-CNT
               ADD WK-GROSS-AMT    TO AU-GROSS-TOTAL
               ADD WK-NATIONAL-TAX TO AU-NATIONAL-TOTAL
               ADD WK-LOCAL-TAX    TO AU-LOCAL-TOTAL
               ADD WK-TAX-TOTAL    TO AU-TAX-TOTAL
               ADD WK-NET-AMT      TO AU-NET-TOTAL
           END-IF.

      *----------------------------------------------------------------*
      * 終了処理                                                       *
      *----------------------------------------------------------------*
       9000-FINAL.
           IF FS-KZRCNF = "00"
               PERFORM 9100-WRITE-RECON
           END-IF

           IF FS-KZCTLF = "00"
               PERFORM 9200-UPDATE-CONTROL
           END-IF

           CLOSE KZINTF
                 KZIRMF
                 KZCTLF
                 KZTAXF
                 KZRCNF

           IF HARD-ERROR
               IF RETURN-CODE = 0
                   MOVE 8 TO RETURN-CODE
               END-IF
               DISPLAY "KZ440B ABEND RC=" RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
               DISPLAY "KZ440B NORMAL READ=" AU-READ-CNT
               DISPLAY "WRITE=" AU-WRITE-CNT
                       " TAX=" AU-TAX-TOTAL
           END-IF.

       9100-WRITE-RECON.
           INITIALIZE KZRCNF-REC
           MOVE CN-RECON-KEY      TO RC-RECON-KEY
           MOVE CL-CYCLE-ID       TO RC-CYCLE-ID
           MOVE AU-NET-TOTAL      TO RC-GL-TOTAL-AMT
           MOVE AU-GROSS-TOTAL    TO RC-ACCRUAL-TOTAL-AMT
           MOVE AU-TAX-TOTAL      TO RC-TAX-TOTAL-AMT
           COMPUTE RC-DIFF-AMT =
               AU-GROSS-TOTAL - AU-TAX-TOTAL - AU-NET-TOTAL

           IF RC-DIFF-AMT = ZERO
               MOVE CN-DISP-NORMAL TO RC-DISPOSITION-CD
           ELSE
               MOVE CN-DISP-DIFF   TO RC-DISPOSITION-CD
           END-IF

           WRITE KZRCNF-REC
           IF FS-KZRCNF NOT = "00"
               MOVE 8 TO RETURN-CODE
               MOVE "Y" TO WK-HARD-ERROR-SW
               DISPLAY "KZRCNF WRITE ERROR ST=" FS-KZRCNF
           END-IF.

       9200-UPDATE-CONTROL.
           MOVE CN-CTL-KEY TO CL-CONTROL-KEY
           READ KZCTLF
               INVALID KEY
                   MOVE 8 TO RETURN-CODE
                   MOVE "Y" TO WK-HARD-ERROR-SW
                   DISPLAY "CONTROL REREAD ERROR KEY=" CN-CTL-KEY
               NOT INVALID KEY
                   IF HARD-ERROR
                       MOVE CN-STAT-ABEND TO CL-RUN-STATUS
                   ELSE
                       MOVE CN-STAT-NORMAL TO CL-RUN-STATUS
                   END-IF
                   MOVE CN-PGM-ID TO CL-LAST-PGM-ID
                   REWRITE KZCTLF-REC
                       INVALID KEY
                           MOVE 8 TO RETURN-CODE
                           MOVE "Y" TO WK-HARD-ERROR-SW
                           DISPLAY "CONTROL FINAL UPDATE ERROR ST="
                                   FS-KZCTLF
                   END-REWRITE
           END-READ.
