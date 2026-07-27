       IDENTIFICATION DIVISION.
       PROGRAM-ID. KZ480B.
      * 変更履歴
      * 版数  年月日(和暦)   担当                       概要
      * 1.00  平成30年04月01日 システム部 勘定系チーム 預貸利息分離初版
      * 1.10  令和02年10月15日 システム部 勘定系チーム 休日処理判定見直し
      * 1.20  令和05年06月01日 システム部 勘定系チーム 利息按分計算精度改善
       AUTHOR. KZBATCH.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZINTF ASSIGN TO "KZINTF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-KZINTF.
           SELECT KZODBF ASSIGN TO "KZODBF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-KZODBF.
           SELECT KZIRMF ASSIGN TO "KZIRMF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS RM-RATE-KEY
               FILE STATUS IS FS-KZIRMF.
           SELECT KZCTLF ASSIGN TO "KZCTLF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CL-CONTROL-KEY
               FILE STATUS IS FS-KZCTLF.
           SELECT KZRCNF ASSIGN TO "KZRCNF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-KZRCNF.
           SELECT KZGLPF ASSIGN TO "KZGLPF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-KZGLPF.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  KZINTF.
           COPY KZINTFC.
       FD  KZODBF.
           COPY KZODBFC.
       FD  KZIRMF.
           COPY KZIRMFC.
       FD  KZCTLF.
           COPY KZCTLFC.
       FD  KZRCNF.
           COPY KZRCNFC.
       FD  KZGLPF.
           COPY KZGLPFC.
      *
       WORKING-STORAGE SECTION.
       01  FS-AREA.
           05 FS-KZINTF              PIC XX VALUE SPACES.
           05 FS-KZODBF              PIC XX VALUE SPACES.
           05 FS-KZIRMF              PIC XX VALUE SPACES.
           05 FS-KZCTLF              PIC XX VALUE SPACES.
           05 FS-KZRCNF              PIC XX VALUE SPACES.
           05 FS-KZGLPF              PIC XX VALUE SPACES.
      *
       01  SW-AREA.
           05 SW-END-INTF            PIC X VALUE "N".
              88 END-INTF                 VALUE "Y".
           05 SW-END-ODBF            PIC X VALUE "N".
              88 END-ODBF                 VALUE "Y".
           05 SW-HARD-ERROR          PIC X VALUE "N".
              88 HARD-ERROR               VALUE "Y".
           05 SW-OD-MATCH            PIC X VALUE "N".
              88 OD-MATCH                 VALUE "Y".
           05 SW-EXCEPTION           PIC X VALUE "N".
              88 EXCEPTION-DETAIL         VALUE "Y".
      *
       01  CONST-AREA.
           05 CTL-KEY                PIC X(16) VALUE "KZ480B-CURRENT".
           05 PGM-ID                 PIC X(08) VALUE "KZ480B".
           05 ST-RUN                 PIC X(01) VALUE "R".
           05 ST-NORMAL              PIC X(01) VALUE "N".
           05 ST-ABEND               PIC X(01) VALUE "A".
           05 DISP-NORMAL            PIC X(02) VALUE "00".
           05 DISP-EXCEPTION         PIC X(02) VALUE "91".
      *
       01  WK-AREA.
           05 WK-JOURNAL-NO          PIC 9(12) VALUE ZERO.
           05 WK-RC-COUNT            PIC 9(09) VALUE ZERO.
           05 WK-PRODUCT-CD          PIC X(04) VALUE SPACES.
           05 WK-RATE-KEY            PIC X(24) VALUE SPACES.
           05 WK-CLASS-KBN           PIC X(01) VALUE SPACES.
              88 CLASS-DEPOSIT            VALUE "D".
              88 CLASS-LOAN               VALUE "L".
              88 CLASS-OVERDRAFT          VALUE "O".
           05 WK-DR-CR-KBN           PIC X(01) VALUE SPACES.
           05 WK-GL-ACCT-CD          PIC X(10) VALUE SPACES.
           05 WK-TAX-AMT             PIC S9(13)V99 COMP-3 VALUE ZERO.
           05 WK-POST-AMT            PIC S9(13)V99 COMP-3 VALUE ZERO.
           05 WK-ABS-AMT             PIC S9(13)V99 COMP-3 VALUE ZERO.
           05 WK-DEPOSIT-TOTAL       PIC S9(15)V99 COMP-3 VALUE ZERO.
           05 WK-LOAN-TOTAL          PIC S9(15)V99 COMP-3 VALUE ZERO.
           05 WK-OD-TOTAL            PIC S9(15)V99 COMP-3 VALUE ZERO.
           05 WK-ACCRUAL-TOTAL       PIC S9(15)V99 COMP-3 VALUE ZERO.
           05 WK-GL-TOTAL            PIC S9(15)V99 COMP-3 VALUE ZERO.
           05 WK-TAX-TOTAL           PIC S9(15)V99 COMP-3 VALUE ZERO.
           05 WK-DIFF-AMT            PIC S9(15)V99 COMP-3 VALUE ZERO.
      *
       01  OD-TABLE-AREA.
           05 OD-CNT                 PIC 9(05) VALUE ZERO.
           05 OD-MAX                 PIC 9(05) VALUE 10000.
           05 OD-TBL OCCURS 10000 TIMES.
              10 T-OD-ACCT-NO        PIC X(16).
              10 T-OD-CYCLE-ID       PIC X(08).
              10 T-OD-INT-AMT        PIC S9(13)V99 COMP-3.
              10 T-OD-NEG-AVG-BAL    PIC S9(15)V99 COMP-3.
      *
       01  IDX-AREA.
           05 IX                     PIC 9(05) COMP VALUE ZERO.
      *
       PROCEDURE DIVISION.
       MAIN-RTN.
           PERFORM 1000-INITIALIZE
           IF NOT HARD-ERROR
              PERFORM 2000-MAIN-PROCESS
           END-IF
           PERFORM 9000-TERMINATE
           GOBACK.
      *
       1000-INITIALIZE.
           MOVE 0 TO RETURN-CODE
           OPEN INPUT KZINTF KZODBF KZIRMF
                I-O KZCTLF
                OUTPUT KZRCNF KZGLPF
           IF FS-KZINTF NOT = "00"
              DISPLAY "KZINTF OPEN FAIL ST=" FS-KZINTF
              SET HARD-ERROR TO TRUE
           END-IF
           IF FS-KZODBF NOT = "00"
              DISPLAY "KZODBF OPEN FAIL ST=" FS-KZODBF
              SET HARD-ERROR TO TRUE
           END-IF
           IF FS-KZIRMF NOT = "00"
              DISPLAY "KZIRMF OPEN FAIL ST=" FS-KZIRMF
              SET HARD-ERROR TO TRUE
           END-IF
           IF FS-KZCTLF NOT = "00"
              DISPLAY "KZCTLF OPEN FAIL ST=" FS-KZCTLF
              SET HARD-ERROR TO TRUE
           END-IF
           IF FS-KZRCNF NOT = "00"
              DISPLAY "KZRCNF OPEN FAIL ST=" FS-KZRCNF
              SET HARD-ERROR TO TRUE
           END-IF
           IF FS-KZGLPF NOT = "00"
              DISPLAY "KZGLPF OPEN FAIL ST=" FS-KZGLPF
              SET HARD-ERROR TO TRUE
           END-IF
           IF HARD-ERROR
              MOVE 12 TO RETURN-CODE
              EXIT PARAGRAPH
           END-IF
           PERFORM 1100-READ-CONTROL
           IF NOT HARD-ERROR
              PERFORM 1200-LOAD-ODBF
           END-IF.
      *
       1100-READ-CONTROL.
           MOVE CTL-KEY TO CL-CONTROL-KEY
           READ KZCTLF KEY IS CL-CONTROL-KEY
              INVALID KEY
                 DISPLAY "CONTROL RECORD NOT FOUND KEY=" CTL-KEY
                 SET HARD-ERROR TO TRUE
              NOT INVALID KEY
                 IF CL-RUN-STATUS = ST-RUN
                    DISPLAY "PREVIOUS RUN IS INCOMPLETE"
                    SET HARD-ERROR TO TRUE
                 ELSE
                    MOVE ST-RUN TO CL-RUN-STATUS
                    MOVE PGM-ID TO CL-LAST-PGM-ID
                    REWRITE KZCTLF-REC
                    IF FS-KZCTLF NOT = "00"
                       DISPLAY "CONTROL UPDATE FAIL ST=" FS-KZCTLF
                       SET HARD-ERROR TO TRUE
                    END-IF
                 END-IF
           END-READ
           IF HARD-ERROR
              MOVE 12 TO RETURN-CODE
           END-IF.
      *
       1200-LOAD-ODBF.
           PERFORM UNTIL END-ODBF OR HARD-ERROR
              READ KZODBF
                 AT END
                    SET END-ODBF TO TRUE
                 NOT AT END
                    IF OD-CNT >= OD-MAX
                       DISPLAY "OD TABLE OVERFLOW"
                       SET HARD-ERROR TO TRUE
                       MOVE 12 TO RETURN-CODE
                    ELSE
                       ADD 1 TO OD-CNT
                       MOVE OD-ACCT-NO     TO T-OD-ACCT-NO(OD-CNT)
                       MOVE OD-CYCLE-ID    TO T-OD-CYCLE-ID(OD-CNT)
                       MOVE OD-OD-INT-AMT  TO T-OD-INT-AMT(OD-CNT)
                       MOVE OD-NEG-AVG-BAL TO T-OD-NEG-AVG-BAL(OD-CNT)
                    END-IF
              END-READ
              IF FS-KZODBF NOT = "00" AND FS-KZODBF NOT = "10"
                 DISPLAY "KZODBF READ FAIL ST=" FS-KZODBF
                 SET HARD-ERROR TO TRUE
                 MOVE 12 TO RETURN-CODE
              END-IF
           END-PERFORM.
      *
       2000-MAIN-PROCESS.
           PERFORM 2100-READ-INTF
           PERFORM UNTIL END-INTF OR HARD-ERROR
              PERFORM 2200-CLASSIFY-DETAIL
              IF EXCEPTION-DETAIL
                 PERFORM 2500-WRITE-RECON-EXCEPTION
              ELSE
                 PERFORM 2600-WRITE-GL
              END-IF
              PERFORM 2100-READ-INTF
           END-PERFORM
           IF NOT HARD-ERROR
              PERFORM 2700-WRITE-RECON-TOTAL
           END-IF.
      *
       2100-READ-INTF.
           READ KZINTF
              AT END
                 SET END-INTF TO TRUE
              NOT AT END
                 IF IN-ACCR-DAYS <= ZERO
                    DISPLAY "BAD ACCR DAYS ACCT=" IN-ACCT-NO
                    SET HARD-ERROR TO TRUE
                    MOVE 8 TO RETURN-CODE
                 END-IF
           END-READ
           IF FS-KZINTF NOT = "00" AND FS-KZINTF NOT = "10"
              DISPLAY "KZINTF READ FAIL ST=" FS-KZINTF
              SET HARD-ERROR TO TRUE
              MOVE 12 TO RETURN-CODE
           END-IF.
      *
       2200-CLASSIFY-DETAIL.
           MOVE "N" TO SW-EXCEPTION
           MOVE SPACES TO WK-PRODUCT-CD
           MOVE SPACES TO WK-RATE-KEY
           MOVE SPACES TO WK-CLASS-KBN
           MOVE IN-ACCT-NO(1:4) TO WK-PRODUCT-CD
           STRING WK-PRODUCT-CD IN-ACCRUAL-DT
              DELIMITED BY SIZE INTO WK-RATE-KEY
           END-STRING
           MOVE WK-RATE-KEY TO RM-RATE-KEY
           READ KZIRMF KEY IS RM-RATE-KEY
              INVALID KEY
                 DISPLAY "RATE MASTER NOT FOUND ACCT=" IN-ACCT-NO
                 SET EXCEPTION-DETAIL TO TRUE
              NOT INVALID KEY
                 IF RM-APPROVAL-STAT NOT = "1"
                    DISPLAY "RATE MASTER NOT APPROVED"
                    SET EXCEPTION-DETAIL TO TRUE
                 END-IF
           END-READ
           IF FS-KZIRMF NOT = "00" AND FS-KZIRMF NOT = "23"
              DISPLAY "KZIRMF READ FAIL ST=" FS-KZIRMF
              SET HARD-ERROR TO TRUE
              MOVE 12 TO RETURN-CODE
           END-IF
           IF HARD-ERROR OR EXCEPTION-DETAIL
              EXIT PARAGRAPH
           END-IF
           EVALUATE RM-PRODUCT-CD
              WHEN "D001"
              WHEN "D002"
              WHEN "T001"
                 SET CLASS-DEPOSIT TO TRUE
              WHEN "L001"
              WHEN "L002"
              WHEN "M001"
                 SET CLASS-LOAN TO TRUE
              WHEN "O001"
                 SET CLASS-OVERDRAFT TO TRUE
              WHEN OTHER
                 DISPLAY "BAD PRODUCT CD=" RM-PRODUCT-CD
                 SET EXCEPTION-DETAIL TO TRUE
           END-EVALUATE
           IF NOT EXCEPTION-DETAIL
              PERFORM 2300-CHECK-SIGN-AND-OD
           END-IF.
      *
       2300-CHECK-SIGN-AND-OD.
           EVALUATE TRUE
              WHEN CLASS-DEPOSIT
                 IF IN-INT-AMT <= ZERO
                    DISPLAY "BAD DEPOSIT SIGN ACCT=" IN-ACCT-NO
                    SET EXCEPTION-DETAIL TO TRUE
                 ELSE
                    MOVE "C"          TO WK-DR-CR-KBN
                    MOVE "2111010001" TO WK-GL-ACCT-CD
                 END-IF
              WHEN CLASS-LOAN
                 IF IN-INT-AMT >= ZERO
                    DISPLAY "BAD LOAN SIGN ACCT=" IN-ACCT-NO
                    SET EXCEPTION-DETAIL TO TRUE
                 ELSE
                    MOVE "D"          TO WK-DR-CR-KBN
                    MOVE "5112010001" TO WK-GL-ACCT-CD
                 END-IF
              WHEN CLASS-OVERDRAFT
                 PERFORM 2400-FIND-OD
                 IF NOT OD-MATCH
                    DISPLAY "OD DETAIL NOT FOUND ACCT=" IN-ACCT-NO
                    SET EXCEPTION-DETAIL TO TRUE
                 ELSE
                    IF IN-INT-AMT >= ZERO
                       DISPLAY "BAD OD SIGN ACCT=" IN-ACCT-NO
                       SET EXCEPTION-DETAIL TO TRUE
                    ELSE
                       MOVE "D"          TO WK-DR-CR-KBN
                       MOVE "5113010001" TO WK-GL-ACCT-CD
                    END-IF
                 END-IF
           END-EVALUATE.
      *
       2400-FIND-OD.
           MOVE "N" TO SW-OD-MATCH
           PERFORM VARYING IX FROM 1 BY 1
              UNTIL IX > OD-CNT OR OD-MATCH
              IF T-OD-ACCT-NO(IX) = IN-ACCT-NO
                 AND T-OD-CYCLE-ID(IX) = IN-CYCLE-ID
                 AND T-OD-NEG-AVG-BAL(IX) < ZERO
                 SET OD-MATCH TO TRUE
              END-IF
           END-PERFORM.
      *
       2500-WRITE-RECON-EXCEPTION.
           ADD 1 TO WK-RC-COUNT
           MOVE SPACES TO KZRCNF-REC
           STRING "EX" IN-CYCLE-ID WK-RC-COUNT
              DELIMITED BY SIZE INTO RC-RECON-KEY
           END-STRING
           MOVE IN-CYCLE-ID       TO RC-CYCLE-ID
           MOVE ZERO              TO RC-GL-TOTAL-AMT
           MOVE IN-INT-AMT        TO RC-ACCRUAL-TOTAL-AMT
           MOVE ZERO              TO RC-TAX-TOTAL-AMT
           MOVE IN-INT-AMT        TO RC-DIFF-AMT
           MOVE DISP-EXCEPTION    TO RC-DISPOSITION-CD
           WRITE KZRCNF-REC
           IF FS-KZRCNF NOT = "00"
              DISPLAY "KZRCNF EXCEPTION WRITE FAIL ST=" FS-KZRCNF
              SET HARD-ERROR TO TRUE
              MOVE 12 TO RETURN-CODE
           END-IF.
      *
       2600-WRITE-GL.
           ADD 1 TO WK-JOURNAL-NO
           COMPUTE WK-ABS-AMT = FUNCTION ABS(IN-INT-AMT)
           IF RM-WITHHOLD-TAX-KBN = "1" AND CLASS-DEPOSIT
              COMPUTE WK-TAX-AMT ROUNDED = WK-ABS-AMT * 0.20315
           ELSE
              MOVE ZERO TO WK-TAX-AMT
           END-IF
           COMPUTE WK-POST-AMT = WK-ABS-AMT - WK-TAX-AMT
           MOVE SPACES TO KZGLPF-REC
           MOVE WK-JOURNAL-NO TO GL-JOURNAL-NO
           MOVE IN-ACCRUAL-DT TO GL-POST-DT
           MOVE IN-ACCT-NO    TO GL-ACCT-NO
           MOVE IN-CYCLE-ID   TO GL-CYCLE-ID
           MOVE WK-DR-CR-KBN  TO GL-DR-CR-KBN
           MOVE WK-GL-ACCT-CD TO GL-GL-ACCT-CD
           MOVE WK-POST-AMT   TO GL-POST-AMT
           MOVE RM-WITHHOLD-TAX-KBN TO GL-TAX-KBN
           MOVE PGM-ID        TO GL-SRC-PGM-ID
           WRITE KZGLPF-REC
           IF FS-KZGLPF NOT = "00"
              DISPLAY "KZGLPF WRITE FAIL ST=" FS-KZGLPF
              SET HARD-ERROR TO TRUE
              MOVE 12 TO RETURN-CODE
           ELSE
              ADD WK-POST-AMT TO WK-GL-TOTAL
              ADD WK-TAX-AMT  TO WK-TAX-TOTAL
              ADD WK-ABS-AMT  TO WK-ACCRUAL-TOTAL
              EVALUATE TRUE
                 WHEN CLASS-DEPOSIT
                    ADD WK-POST-AMT TO WK-DEPOSIT-TOTAL
                 WHEN CLASS-LOAN
                    ADD WK-POST-AMT TO WK-LOAN-TOTAL
                 WHEN CLASS-OVERDRAFT
                    ADD WK-POST-AMT TO WK-OD-TOTAL
              END-EVALUATE
           END-IF.
      *
       2700-WRITE-RECON-TOTAL.
           COMPUTE WK-DIFF-AMT = WK-ACCRUAL-TOTAL
                              - WK-GL-TOTAL
                              - WK-TAX-TOTAL
           ADD 1 TO WK-RC-COUNT
           MOVE SPACES TO KZRCNF-REC
           STRING "TT" CL-CYCLE-ID WK-RC-COUNT
              DELIMITED BY SIZE INTO RC-RECON-KEY
           END-STRING
           MOVE CL-CYCLE-ID       TO RC-CYCLE-ID
           MOVE WK-GL-TOTAL       TO RC-GL-TOTAL-AMT
           MOVE WK-ACCRUAL-TOTAL  TO RC-ACCRUAL-TOTAL-AMT
           MOVE WK-TAX-TOTAL      TO RC-TAX-TOTAL-AMT
           MOVE WK-DIFF-AMT       TO RC-DIFF-AMT
           IF WK-DIFF-AMT = ZERO
              MOVE DISP-NORMAL TO RC-DISPOSITION-CD
           ELSE
              MOVE DISP-EXCEPTION TO RC-DISPOSITION-CD
           END-IF
           WRITE KZRCNF-REC
           IF FS-KZRCNF NOT = "00"
              DISPLAY "KZRCNF TOTAL WRITE FAIL ST=" FS-KZRCNF
              SET HARD-ERROR TO TRUE
              MOVE 12 TO RETURN-CODE
           ELSE
              DISPLAY "DEPOSIT TOTAL=" WK-DEPOSIT-TOTAL
              DISPLAY "LOAN TOTAL=" WK-LOAN-TOTAL
              DISPLAY "OD TOTAL=" WK-OD-TOTAL
           END-IF.
      *
       9000-TERMINATE.
           IF FS-KZCTLF = "00" AND NOT HARD-ERROR
              MOVE ST-NORMAL TO CL-RUN-STATUS
              MOVE PGM-ID    TO CL-LAST-PGM-ID
              REWRITE KZCTLF-REC
              IF FS-KZCTLF NOT = "00"
                 DISPLAY "CONTROL COMPLETE FAIL ST=" FS-KZCTLF
                 MOVE 12 TO RETURN-CODE
                 SET HARD-ERROR TO TRUE
              END-IF
           END-IF
           IF FS-KZCTLF = "00" AND HARD-ERROR
              MOVE ST-ABEND TO CL-RUN-STATUS
              MOVE PGM-ID   TO CL-LAST-PGM-ID
              REWRITE KZCTLF-REC
              IF FS-KZCTLF NOT = "00"
                 DISPLAY "CONTROL ABEND FAIL ST=" FS-KZCTLF
              END-IF
           END-IF
           CLOSE KZINTF KZODBF KZIRMF KZCTLF KZRCNF KZGLPF
           IF HARD-ERROR AND RETURN-CODE = 0
              MOVE 8 TO RETURN-CODE
           END-IF.
       END PROGRAM KZ480B.
