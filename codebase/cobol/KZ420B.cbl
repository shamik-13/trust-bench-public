       IDENTIFICATION DIVISION.
       PROGRAM-ID. KZ420B.
      * 変更履歴
      * 版数  年月日(和暦)  担当                      概要
      * 1.00  平成28年04月01日 システム部 勘定系チーム 未収利息繰越初版
      * 1.10  令和02年10月15日 システム部 勘定系チーム 休日繰越判定見直し
      * 1.20  令和05年06月30日 システム部 勘定系チーム 月次締め処理対応
       AUTHOR. KZ-BATCH.
       INSTALLATION. みらい信託銀行.
       DATE-WRITTEN. 2026-06-12.
       DATE-COMPILED.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZINTF
               ASSIGN TO "KZINTF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS FS-KZINTF.

           SELECT KZUNIF
               ASSIGN TO "KZUNIF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS UI-ACCT-NO
               FILE STATUS IS FS-KZUNIF.

           SELECT KZCTLF
               ASSIGN TO "KZCTLF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CL-CONTROL-KEY
               FILE STATUS IS FS-KZCTLF.

           SELECT KZRCNF
               ASSIGN TO "KZRCNF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS FS-KZRCNF.

       DATA DIVISION.
       FILE SECTION.

       FD  KZINTF.
       COPY KZINTFC.

       FD  KZUNIF.
       COPY KZUNIFC.

       FD  KZCTLF.
       COPY KZCTLFC.

       FD  KZRCNF.
       COPY KZRCNFC.

       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID              PIC X(08) VALUE "KZ420B  ".

       01  WS-FILE-STATUS.
           05 FS-KZINTF                PIC X(02) VALUE SPACES.
           05 FS-KZUNIF                PIC X(02) VALUE SPACES.
           05 FS-KZCTLF                PIC X(02) VALUE SPACES.
           05 FS-KZRCNF                PIC X(02) VALUE SPACES.

       01  WS-SWITCHES.
           05 WS-EOF-SW                PIC X(01) VALUE "N".
              88 WS-EOF                         VALUE "Y".
              88 WS-NOT-EOF                     VALUE "N".
           05 WS-FIRST-REC-SW          PIC X(01) VALUE "Y".
              88 WS-FIRST-REC                   VALUE "Y".
              88 WS-NOT-FIRST-REC               VALUE "N".
           05 WS-HARD-ERROR-SW         PIC X(01) VALUE "N".
              88 WS-HARD-ERROR                  VALUE "Y".
              88 WS-NO-HARD-ERROR               VALUE "N".
           05 WS-CTL-FOUND-SW          PIC X(01) VALUE "N".
              88 WS-CTL-FOUND                   VALUE "Y".
              88 WS-CTL-NOT-FOUND               VALUE "N".
           05 WS-UNIF-FOUND-SW         PIC X(01) VALUE "N".
              88 WS-UNIF-FOUND                  VALUE "Y".
              88 WS-UNIF-NOT-FOUND              VALUE "N".

       01  WS-CONTROL.
           05 WS-CONTROL-KEY           PIC X(16)
              VALUE "KZ420B-CURRENT ".
           05 WS-CTL-RUN-DATE          PIC 9(08) VALUE ZERO.
           05 WS-CYCLE-ID              PIC X(08) VALUE SPACES.
           05 WS-RESTART-POINT         PIC X(20) VALUE SPACES.
           05 WS-RUN-STATUS            PIC X(01) VALUE SPACE.

       01  WS-CURRENT-GROUP.
           05 WS-CUR-ACCT-NO           PIC X(20) VALUE SPACES.
           05 WS-CUR-CYCLE-ID          PIC X(08) VALUE SPACES.
           05 WS-GRP-INT-AMT           PIC S9(13)V99 COMP-3
              VALUE ZERO.
           05 WS-GRP-DAYS              PIC 9(05) COMP-3 VALUE ZERO.
           05 WS-GRP-REC-CNT           PIC 9(07) COMP-3 VALUE ZERO.
           05 WS-GRP-CAUSE-CD          PIC X(02) VALUE SPACES.

       01  WS-TOTALS.
           05 WS-IN-REC-CNT            PIC 9(09) COMP-3 VALUE ZERO.
           05 WS-SKIP-REC-CNT          PIC 9(09) COMP-3 VALUE ZERO.
           05 WS-UPD-REC-CNT           PIC 9(09) COMP-3 VALUE ZERO.
           05 WS-ADD-REC-CNT           PIC 9(09) COMP-3 VALUE ZERO.
           05 WS-RCNF-REC-CNT          PIC 9(09) COMP-3 VALUE ZERO.
           05 WS-TOT-GL-AMT            PIC S9(13)V99 COMP-3
              VALUE ZERO.
           05 WS-TOT-ACCR-AMT          PIC S9(13)V99 COMP-3
              VALUE ZERO.
           05 WS-TOT-TAX-AMT           PIC S9(13)V99 COMP-3
              VALUE ZERO.

       01  WS-CALC.
           05 WS-BASE-ACCR-AMT         PIC S9(13)V99 COMP-3
              VALUE ZERO.
           05 WS-TAX-AMT               PIC S9(13)V99 COMP-3
              VALUE ZERO.
           05 WS-CARRY-AMT             PIC S9(13)V99 COMP-3
              VALUE ZERO.
           05 WS-DIFF-AMT              PIC S9(13)V99 COMP-3
              VALUE ZERO.
           05 WS-ABS-AMT               PIC S9(13)V99 COMP-3
              VALUE ZERO.

       01  WS-DISPLAY.
           05 WS-DISP-CNT              PIC Z(8)9.
           05 WS-DISP-AMT              PIC -(12)9.99.
           05 WS-DISP-ST               PIC X(02).

       01  WS-KZ475S-PARM.
           05 LK-CALLER-PGM            PIC X(08).
           05 LK-CYCLE-ID              PIC X(08).
           05 LK-AMT-KIND              PIC X(01).
           05 LK-AUDIT-KEY             PIC X(24).
           05 LK-GL-AMT                PIC S9(13)V99 COMP-3.
           05 LK-ACCRUAL-AMT           PIC S9(13)V99 COMP-3.
           05 LK-TAX-AMT               PIC S9(13)V99 COMP-3.
           05 LK-BASE-ACCRUAL-AMT      PIC S9(13)V99 COMP-3.
           05 LK-BASE-TAX-AMT          PIC S9(13)V99 COMP-3.
           05 LK-RETURN-STATUS         PIC X(02).

       PROCEDURE DIVISION.
       0000-MAIN SECTION.
       0000-MAIN-START.
           MOVE 8 TO RETURN-CODE
           PERFORM 1000-INITIALIZE
           IF WS-NO-HARD-ERROR
              PERFORM 2000-READ-INTF
              PERFORM UNTIL WS-EOF OR WS-HARD-ERROR
                 PERFORM 3000-PROCESS-INTF
                 PERFORM 2000-READ-INTF
              END-PERFORM
              IF WS-NO-HARD-ERROR
                 PERFORM 3400-FLUSH-GROUP
                 PERFORM 8000-FINALIZE
              END-IF
           END-IF

           IF WS-HARD-ERROR
              PERFORM 9000-ABEND-CLOSE
              MOVE 12 TO RETURN-CODE
           ELSE
              MOVE 0 TO RETURN-CODE
           END-IF
           GOBACK.

       1000-INITIALIZE SECTION.
       1000-START.
           SET WS-NO-HARD-ERROR TO TRUE
           SET WS-NOT-EOF TO TRUE
           SET WS-FIRST-REC TO TRUE
           MOVE ZERO TO WS-IN-REC-CNT
           MOVE ZERO TO WS-SKIP-REC-CNT
           MOVE ZERO TO WS-UPD-REC-CNT
           MOVE ZERO TO WS-ADD-REC-CNT
           MOVE ZERO TO WS-RCNF-REC-CNT
           MOVE ZERO TO WS-TOT-GL-AMT
           MOVE ZERO TO WS-TOT-ACCR-AMT
           MOVE ZERO TO WS-TOT-TAX-AMT

           OPEN INPUT KZINTF
           IF FS-KZINTF NOT = "00"
              MOVE FS-KZINTF TO WS-DISP-ST
              DISPLAY "KZINTF OPEN FAILED ST=" WS-DISP-ST
              SET WS-HARD-ERROR TO TRUE
           END-IF

           IF WS-NO-HARD-ERROR
              OPEN I-O KZUNIF
              IF FS-KZUNIF NOT = "00"
                 MOVE FS-KZUNIF TO WS-DISP-ST
                 DISPLAY "KZUNIF OPEN FAILED ST=" WS-DISP-ST
                 SET WS-HARD-ERROR TO TRUE
              END-IF
           END-IF

           IF WS-NO-HARD-ERROR
              OPEN I-O KZCTLF
              IF FS-KZCTLF NOT = "00"
                 MOVE FS-KZCTLF TO WS-DISP-ST
                 DISPLAY "KZCTLF OPEN FAILED ST=" WS-DISP-ST
                 SET WS-HARD-ERROR TO TRUE
              END-IF
           END-IF

           IF WS-NO-HARD-ERROR
              OPEN OUTPUT KZRCNF
              IF FS-KZRCNF NOT = "00"
                 MOVE FS-KZRCNF TO WS-DISP-ST
                 DISPLAY "KZRCNF OPEN FAILED ST=" WS-DISP-ST
                 SET WS-HARD-ERROR TO TRUE
              END-IF
           END-IF

           IF WS-NO-HARD-ERROR
              PERFORM 1100-LOAD-CONTROL
           END-IF.

       1100-LOAD-CONTROL SECTION.
       1100-START.
           SET WS-CTL-NOT-FOUND TO TRUE
           MOVE WS-CONTROL-KEY TO CL-CONTROL-KEY

           READ KZCTLF
              INVALID KEY
                 SET WS-CTL-NOT-FOUND TO TRUE
              NOT INVALID KEY
                 SET WS-CTL-FOUND TO TRUE
           END-READ

           IF FS-KZCTLF NOT = "00" AND FS-KZCTLF NOT = "23"
              MOVE FS-KZCTLF TO WS-DISP-ST
              DISPLAY "KZCTLF READ FAILED ST=" WS-DISP-ST
              SET WS-HARD-ERROR TO TRUE
           END-IF

           IF WS-NO-HARD-ERROR
              IF WS-CTL-FOUND
                 MOVE CL-RUN-DT TO WS-CTL-RUN-DATE
                 MOVE CL-CYCLE-ID TO WS-CYCLE-ID
                 MOVE CL-RESTART-POINT TO WS-RESTART-POINT
                 MOVE CL-RUN-STATUS TO WS-RUN-STATUS
                 IF CL-RUN-STATUS = "R" OR CL-RUN-STATUS = "A"
                    CONTINUE
                 ELSE
                    DISPLAY "BAD CONTROL STATUS="
                       CL-RUN-STATUS
                    SET WS-HARD-ERROR TO TRUE
                 END-IF
              ELSE
                 DISPLAY "KZCTLF CONTROL NOT FOUND"
                 SET WS-HARD-ERROR TO TRUE
              END-IF
           END-IF.

       2000-READ-INTF SECTION.
       2000-START.
           READ KZINTF
              AT END
                 SET WS-EOF TO TRUE
              NOT AT END
                 ADD 1 TO WS-IN-REC-CNT
           END-READ

           IF FS-KZINTF NOT = "00" AND FS-KZINTF NOT = "10"
              MOVE FS-KZINTF TO WS-DISP-ST
              DISPLAY "KZINTF READ FAILED ST=" WS-DISP-ST
              SET WS-HARD-ERROR TO TRUE
           END-IF.

       3000-PROCESS-INTF SECTION.
       3000-START.
           IF IN-ACCT-NO = SPACES
              DISPLAY "INPUT ACCOUNT MISSING"
              ADD 1 TO WS-SKIP-REC-CNT
           ELSE
              IF IN-CYCLE-ID NOT = WS-CYCLE-ID
                 DISPLAY "INPUT CYCLE MISMATCH"
                 ADD 1 TO WS-SKIP-REC-CNT
              ELSE
                 IF IN-INT-AMT = ZERO
                    ADD 1 TO WS-SKIP-REC-CNT
                 ELSE
                    PERFORM 3100-ACCEPT-DETAIL
                 END-IF
              END-IF
           END-IF.

       3100-ACCEPT-DETAIL SECTION.
       3100-START.
           IF WS-FIRST-REC
              PERFORM 3200-START-GROUP
              SET WS-NOT-FIRST-REC TO TRUE
           ELSE
              IF IN-ACCT-NO NOT = WS-CUR-ACCT-NO
                 OR IN-CYCLE-ID NOT = WS-CUR-CYCLE-ID
                 PERFORM 3400-FLUSH-GROUP
                 IF WS-NO-HARD-ERROR
                    PERFORM 3200-START-GROUP
                 END-IF
              END-IF
           END-IF

           IF WS-NO-HARD-ERROR
              ADD IN-INT-AMT TO WS-GRP-INT-AMT
              ADD IN-ACCR-DAYS TO WS-GRP-DAYS
              ADD 1 TO WS-GRP-REC-CNT
           END-IF.

       3200-START-GROUP SECTION.
       3200-START.
           MOVE IN-ACCT-NO TO WS-CUR-ACCT-NO
           MOVE IN-CYCLE-ID TO WS-CUR-CYCLE-ID
           MOVE ZERO TO WS-GRP-INT-AMT
           MOVE ZERO TO WS-GRP-DAYS
           MOVE ZERO TO WS-GRP-REC-CNT
           MOVE SPACES TO WS-GRP-CAUSE-CD.

       3400-FLUSH-GROUP SECTION.
       3400-START.
           IF WS-NOT-FIRST-REC AND WS-GRP-REC-CNT > ZERO
              PERFORM 3500-DECIDE-CAUSE
              IF WS-GRP-CAUSE-CD NOT = SPACES
                 PERFORM 4000-UPDATE-UNIF
                 IF WS-NO-HARD-ERROR
                    PERFORM 5000-CALL-AUDIT
                 END-IF
              END-IF
           END-IF.

       3500-DECIDE-CAUSE SECTION.
       3500-START.
           MOVE SPACES TO WS-GRP-CAUSE-CD

           IF WS-GRP-INT-AMT < ZERO
              MOVE "90" TO WS-GRP-CAUSE-CD
           ELSE
              IF WS-GRP-DAYS > 366
                 MOVE "10" TO WS-GRP-CAUSE-CD
              ELSE
                 IF WS-GRP-INT-AMT > 1000000
                    MOVE "20" TO WS-GRP-CAUSE-CD
                 ELSE
                    IF WS-GRP-DAYS > 180
                       MOVE "30" TO WS-GRP-CAUSE-CD
                    END-IF
                 END-IF
              END-IF
           END-IF.

       4000-UPDATE-UNIF SECTION.
       4000-START.
           SET WS-UNIF-NOT-FOUND TO TRUE
           MOVE WS-CUR-ACCT-NO TO UI-ACCT-NO

           READ KZUNIF
              INVALID KEY
                 SET WS-UNIF-NOT-FOUND TO TRUE
              NOT INVALID KEY
                 SET WS-UNIF-FOUND TO TRUE
           END-READ

           IF FS-KZUNIF NOT = "00" AND FS-KZUNIF NOT = "23"
              MOVE FS-KZUNIF TO WS-DISP-ST
              DISPLAY "KZUNIF READ FAILED ST=" WS-DISP-ST
              SET WS-HARD-ERROR TO TRUE
           END-IF

           IF WS-NO-HARD-ERROR
              IF WS-UNIF-FOUND
                 PERFORM 4100-REWRITE-UNIF
              ELSE
                 PERFORM 4200-WRITE-UNIF
              END-IF
           END-IF.

       4100-REWRITE-UNIF SECTION.
       4100-START.
           IF UI-CYCLE-ID = WS-CUR-CYCLE-ID
              IF UI-CAUSE-CD = WS-GRP-CAUSE-CD
                 ADD WS-GRP-INT-AMT TO UI-UNPAID-INT-AMT
              ELSE
                 SUBTRACT WS-GRP-INT-AMT
                    FROM UI-UNPAID-INT-AMT
                 MOVE WS-GRP-CAUSE-CD TO UI-CAUSE-CD
              END-IF
           ELSE
              MOVE WS-CUR-CYCLE-ID TO UI-CYCLE-ID
              MOVE WS-GRP-INT-AMT TO UI-UNPAID-INT-AMT
              MOVE WS-GRP-CAUSE-CD TO UI-CAUSE-CD
           END-IF

           IF UI-UNPAID-INT-AMT < ZERO
              COMPUTE WS-ABS-AMT = UI-UNPAID-INT-AMT * -1
              IF WS-ABS-AMT < 1
                 MOVE ZERO TO UI-UNPAID-INT-AMT
              END-IF
           END-IF

           MOVE WS-CTL-RUN-DATE TO UI-CARRY-FWD-DT
           MOVE WS-PROGRAM-ID TO UI-LAST-UPDATE-PGM

           REWRITE KZUNIF-REC
           IF FS-KZUNIF = "00"
              ADD 1 TO WS-UPD-REC-CNT
           ELSE
              MOVE FS-KZUNIF TO WS-DISP-ST
              DISPLAY "KZUNIF REWRITE FAILED ST=" WS-DISP-ST
              SET WS-HARD-ERROR TO TRUE
           END-IF.

       4200-WRITE-UNIF SECTION.
       4200-START.
           INITIALIZE KZUNIF-REC
           MOVE WS-CUR-ACCT-NO TO UI-ACCT-NO
           MOVE WS-CUR-CYCLE-ID TO UI-CYCLE-ID
           MOVE WS-GRP-INT-AMT TO UI-UNPAID-INT-AMT
           MOVE WS-CTL-RUN-DATE TO UI-CARRY-FWD-DT
           MOVE WS-GRP-CAUSE-CD TO UI-CAUSE-CD
           MOVE WS-PROGRAM-ID TO UI-LAST-UPDATE-PGM

           WRITE KZUNIF-REC
           IF FS-KZUNIF = "00"
              ADD 1 TO WS-ADD-REC-CNT
           ELSE
              MOVE FS-KZUNIF TO WS-DISP-ST
              DISPLAY "KZUNIF WRITE FAILED ST=" WS-DISP-ST
              SET WS-HARD-ERROR TO TRUE
           END-IF.

       5000-CALL-AUDIT SECTION.
       5000-START.
           COMPUTE WS-TAX-AMT ROUNDED =
              WS-GRP-INT-AMT * 0.20315
           COMPUTE WS-BASE-ACCR-AMT =
              WS-GRP-INT-AMT - WS-TAX-AMT
           MOVE WS-GRP-INT-AMT TO WS-CARRY-AMT

           INITIALIZE WS-KZ475S-PARM
           MOVE WS-PROGRAM-ID TO LK-CALLER-PGM
           MOVE WS-CUR-CYCLE-ID TO LK-CYCLE-ID
           MOVE "U" TO LK-AMT-KIND

           STRING WS-CUR-ACCT-NO DELIMITED BY SIZE
                  WS-GRP-CAUSE-CD DELIMITED BY SIZE
              INTO LK-AUDIT-KEY
           END-STRING

           MOVE WS-CARRY-AMT TO LK-GL-AMT
           MOVE WS-CARRY-AMT TO LK-ACCRUAL-AMT
           MOVE WS-TAX-AMT TO LK-TAX-AMT
           MOVE WS-BASE-ACCR-AMT TO LK-BASE-ACCRUAL-AMT
           MOVE WS-TAX-AMT TO LK-BASE-TAX-AMT

           CALL "KZ475S" USING WS-KZ475S-PARM
              ON EXCEPTION
                 DISPLAY "KZ475S CALL FAILED"
                 SET WS-HARD-ERROR TO TRUE
           END-CALL

           IF WS-NO-HARD-ERROR
              ADD LK-GL-AMT TO WS-TOT-GL-AMT
              ADD LK-ACCRUAL-AMT TO WS-TOT-ACCR-AMT
              ADD LK-TAX-AMT TO WS-TOT-TAX-AMT
              IF LK-RETURN-STATUS NOT = "00"
                 PERFORM 5100-WRITE-RECON
              END-IF
           END-IF.

       5100-WRITE-RECON SECTION.
       5100-START.
           COMPUTE WS-DIFF-AMT =
              LK-GL-AMT - LK-ACCRUAL-AMT

           INITIALIZE KZRCNF-REC
           MOVE LK-AUDIT-KEY TO RC-RECON-KEY
           MOVE LK-CYCLE-ID TO RC-CYCLE-ID
           MOVE LK-GL-AMT TO RC-GL-TOTAL-AMT
           MOVE LK-ACCRUAL-AMT TO RC-ACCRUAL-TOTAL-AMT
           MOVE LK-TAX-AMT TO RC-TAX-TOTAL-AMT
           MOVE WS-DIFF-AMT TO RC-DIFF-AMT

           IF LK-RETURN-STATUS = "01"
              MOVE "HORYU" TO RC-DISPOSITION-CD
           ELSE
              MOVE "SAKAK" TO RC-DISPOSITION-CD
           END-IF

           WRITE KZRCNF-REC
           IF FS-KZRCNF = "00"
              ADD 1 TO WS-RCNF-REC-CNT
           ELSE
              MOVE FS-KZRCNF TO WS-DISP-ST
              DISPLAY "KZRCNF WRITE FAILED ST=" WS-DISP-ST
              SET WS-HARD-ERROR TO TRUE
           END-IF.

       8000-FINALIZE SECTION.
       8000-START.
           MOVE WS-CONTROL-KEY TO CL-CONTROL-KEY

           READ KZCTLF
              INVALID KEY
                 DISPLAY "KZCTLF COMPLETE ROW MISSING"
                 SET WS-HARD-ERROR TO TRUE
              NOT INVALID KEY
                 MOVE "C" TO CL-RUN-STATUS
                 MOVE WS-PROGRAM-ID TO CL-LAST-PGM-ID
                 MOVE SPACES TO CL-RESTART-POINT
                 REWRITE KZCTLF-REC
           END-READ

           IF WS-NO-HARD-ERROR
              IF FS-KZCTLF NOT = "00"
                 MOVE FS-KZCTLF TO WS-DISP-ST
                 DISPLAY "KZCTLF REWRITE FAILED ST=" WS-DISP-ST
                 SET WS-HARD-ERROR TO TRUE
              END-IF
           END-IF

           IF WS-NO-HARD-ERROR
              MOVE WS-IN-REC-CNT TO WS-DISP-CNT
              DISPLAY "INPUT COUNT=" WS-DISP-CNT
              MOVE WS-SKIP-REC-CNT TO WS-DISP-CNT
              DISPLAY "SKIP COUNT=" WS-DISP-CNT
              MOVE WS-UPD-REC-CNT TO WS-DISP-CNT
              DISPLAY "UPDATE COUNT=" WS-DISP-CNT
              MOVE WS-ADD-REC-CNT TO WS-DISP-CNT
              DISPLAY "ADD COUNT=" WS-DISP-CNT
              MOVE WS-RCNF-REC-CNT TO WS-DISP-CNT
              DISPLAY "RECON COUNT=" WS-DISP-CNT
              MOVE WS-TOT-GL-AMT TO WS-DISP-AMT
              DISPLAY "AUDIT GL TOTAL=" WS-DISP-AMT
           END-IF

           CLOSE KZINTF KZUNIF KZCTLF KZRCNF.

       9000-ABEND-CLOSE SECTION.
       9000-START.
           IF FS-KZINTF = "00"
              CLOSE KZINTF
           END-IF
           IF FS-KZUNIF = "00"
              CLOSE KZUNIF
           END-IF
           IF FS-KZCTLF = "00"
              CLOSE KZCTLF
           END-IF
           IF FS-KZRCNF = "00"
              CLOSE KZRCNF
           END-IF
           DISPLAY "KZ420B ABEND".
