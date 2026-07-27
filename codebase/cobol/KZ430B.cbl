       IDENTIFICATION DIVISION.
       PROGRAM-ID. KZ430B.
      * 変更履歴
      * 版数  年月日(和暦)  担当                    概要
      * 1.00  平成28.04.01  システム部 勘定系チーム  新規作成
      * 1.01  平成31.02.15  システム部 勘定系チーム  利息元帳抽出条件見直し
      * 1.02  令和03.10.01  システム部 勘定系チーム  仕訳計上日判定を改修
       AUTHOR. KZBATCH.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZINTF ASSIGN TO "KZINTF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-ST-KZINTF.
           SELECT KZTAXF ASSIGN TO "KZTAXF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-ST-KZTAXF.
           SELECT KZUNIF ASSIGN TO "KZUNIF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS UI-ACCT-NO
               FILE STATUS IS WS-ST-KZUNIF.
           SELECT KZCTLF ASSIGN TO "KZCTLF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CL-CONTROL-KEY
               FILE STATUS IS WS-ST-KZCTLF.
           SELECT KZGLPF ASSIGN TO "KZGLPF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-ST-KZGLPF.
           SELECT KZRCNF ASSIGN TO "KZRCNF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-ST-KZRCNF.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  KZINTF.
           COPY KZINTFC.
       FD  KZTAXF.
           COPY KZTAXFC.
       FD  KZUNIF.
           COPY KZUNIFC.
       FD  KZCTLF.
           COPY KZCTLFC.
       FD  KZGLPF.
           COPY KZGLPFC.
       FD  KZRCNF.
           COPY KZRCNFC.
      *
       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05 WS-ST-KZINTF        PIC XX.
           05 WS-ST-KZTAXF        PIC XX.
           05 WS-ST-KZUNIF        PIC XX.
           05 WS-ST-KZCTLF        PIC XX.
           05 WS-ST-KZGLPF        PIC XX.
           05 WS-ST-KZRCNF        PIC XX.
      *
       01  WS-CONSTANT.
           05 WS-PGM-ID           PIC X(08) VALUE "KZ430B  ".
           05 WS-CTL-KEY          PIC X(24) VALUE "KZ430B-CYCLE-CTL".
           05 WS-STAT-RUN         PIC X(01) VALUE "R".
           05 WS-STAT-END         PIC X(01) VALUE "E".
           05 WS-STAT-ERR         PIC X(01) VALUE "A".
      *
       01  WS-FLAG.
           05 WS-EOF-IN           PIC X VALUE "N".
              88 EOF-IN                 VALUE "Y".
           05 WS-EOF-TX           PIC X VALUE "N".
              88 EOF-TX                 VALUE "Y".
           05 WS-HARD-ERR         PIC X VALUE "N".
              88 HARD-ERR               VALUE "Y".
      *
       01  WS-RUN.
           05 WS-CYCLE-ID         PIC X(08).
           05 WS-RUN-DT           PIC 9(08).
           05 WS-RESTART-POINT    PIC X(20).
           05 WS-LAST-ACCT        PIC X(20).
           05 WS-JOURNAL-SEQ      PIC 9(09) VALUE ZERO.
           05 WS-RCN-SEQ          PIC 9(09) VALUE ZERO.
           05 WS-SKIP-CNT         PIC 9(09) VALUE ZERO.
           05 WS-IN-CNT           PIC 9(09) VALUE ZERO.
           05 WS-GL-CNT           PIC 9(09) VALUE ZERO.
      *
       01  WS-AMOUNT.
           05 WS-INT-TOTAL        PIC S9(13)V99 COMP-3 VALUE ZERO.
           05 WS-TAX-TOTAL        PIC S9(13)V99 COMP-3 VALUE ZERO.
           05 WS-NET-TOTAL        PIC S9(13)V99 COMP-3 VALUE ZERO.
           05 WS-UNPAID-TOTAL     PIC S9(13)V99 COMP-3 VALUE ZERO.
           05 WS-GL-TOTAL         PIC S9(13)V99 COMP-3 VALUE ZERO.
           05 WS-DIFF-AMT         PIC S9(13)V99 COMP-3 VALUE ZERO.
           05 WS-TAX-AMT          PIC S9(13)V99 COMP-3 VALUE ZERO.
      *
       01  WS-EDIT.
           05 WS-DISP-AMT         PIC -ZZZ,ZZZ,ZZZ,ZZ9.99.
           05 WS-DISP-CNT         PIC ZZZ,ZZZ,ZZ9.
      *
       01  LK-KZ475S.
           05 LK-CALLER-PGM       PIC X(08).
           05 LK-CYCLE-ID         PIC X(08).
           05 LK-AMT-KIND         PIC X(01).
           05 LK-AUDIT-KEY        PIC X(24).
           05 LK-GL-AMT           PIC S9(13)V99 COMP-3.
           05 LK-ACCRUAL-AMT      PIC S9(13)V99 COMP-3.
           05 LK-TAX-AMT          PIC S9(13)V99 COMP-3.
           05 LK-BASE-ACCRUAL-AMT PIC S9(13)V99 COMP-3.
           05 LK-BASE-TAX-AMT     PIC S9(13)V99 COMP-3.
           05 LK-RETURN-STATUS    PIC X(02).
      *
       PROCEDURE DIVISION.
       MAIN-SECTION.
           PERFORM 1000-INIT.
           IF NOT HARD-ERR
              PERFORM 2000-PROCESS
           END-IF.
           PERFORM 8000-FINAL.
           GOBACK.
      *
       1000-INIT.
           MOVE 0 TO RETURN-CODE.
           OPEN INPUT KZINTF KZTAXF
                I-O KZUNIF KZCTLF
                OUTPUT KZGLPF KZRCNF.
           IF WS-ST-KZINTF NOT = "00"
              DISPLAY "KZINTF OPEN ST=" WS-ST-KZINTF
              PERFORM 9000-ABEND
           END-IF.
           IF WS-ST-KZTAXF NOT = "00"
              DISPLAY "KZTAXF OPEN ST=" WS-ST-KZTAXF
              PERFORM 9000-ABEND
           END-IF.
           IF WS-ST-KZUNIF NOT = "00"
              DISPLAY "KZUNIF OPEN ST=" WS-ST-KZUNIF
              PERFORM 9000-ABEND
           END-IF.
           IF WS-ST-KZCTLF NOT = "00"
              DISPLAY "KZCTLF OPEN ST=" WS-ST-KZCTLF
              PERFORM 9000-ABEND
           END-IF.
           IF WS-ST-KZGLPF NOT = "00"
              DISPLAY "KZGLPF OPEN ST=" WS-ST-KZGLPF
              PERFORM 9000-ABEND
           END-IF.
           IF WS-ST-KZRCNF NOT = "00"
              DISPLAY "KZRCNF OPEN ST=" WS-ST-KZRCNF
              PERFORM 9000-ABEND
           END-IF.
           IF NOT HARD-ERR
              MOVE WS-CTL-KEY TO CL-CONTROL-KEY
              READ KZCTLF
                 INVALID KEY
                    DISPLAY "KZCTLF CTL REC NOT FOUND"
                    PERFORM 9000-ABEND
                 NOT INVALID KEY
                    MOVE CL-CYCLE-ID TO WS-CYCLE-ID
                    MOVE CL-RUN-DT TO WS-RUN-DT
                    MOVE CL-RESTART-POINT TO WS-RESTART-POINT
                    MOVE WS-STAT-RUN TO CL-RUN-STATUS
                    MOVE WS-PGM-ID TO CL-LAST-PGM-ID
                    REWRITE KZCTLF-REC
                    IF WS-ST-KZCTLF NOT = "00"
                       DISPLAY "KZCTLF START ST=" WS-ST-KZCTLF
                       PERFORM 9000-ABEND
                    END-IF
              END-READ
           END-IF.
      *
       2000-PROCESS.
           PERFORM 2100-READ-TAX.
           PERFORM 2200-READ-INT.
           PERFORM UNTIL EOF-IN OR HARD-ERR
              IF IN-CYCLE-ID NOT = WS-CYCLE-ID
                 PERFORM 2200-READ-INT
              ELSE
                 IF IN-ACCT-NO <= WS-RESTART-POINT
                    ADD 1 TO WS-SKIP-CNT
                    PERFORM 2200-READ-INT
                 ELSE
                    PERFORM 3000-ACCOUNT
                    PERFORM 2200-READ-INT
                 END-IF
              END-IF
           END-PERFORM.
           IF NOT HARD-ERR
              PERFORM 5000-AUDIT-CALL
           END-IF.
      *
       2100-READ-TAX.
           READ KZTAXF
              AT END
                 SET EOF-TX TO TRUE
              NOT AT END
                 IF TX-CYCLE-ID = WS-CYCLE-ID
                    COMPUTE WS-TAX-AMT =
                       TX-NATIONAL-TAX-AMT + TX-LOCAL-TAX-AMT
                    ADD WS-TAX-AMT TO WS-TAX-TOTAL
                    ADD TX-NET-INT-AMT TO WS-NET-TOTAL
                 END-IF
           END-READ.
           IF WS-ST-KZTAXF NOT = "00" AND WS-ST-KZTAXF NOT = "10"
              DISPLAY "KZTAXF READ ST=" WS-ST-KZTAXF
              PERFORM 9000-ABEND
           END-IF.
           IF NOT EOF-TX AND NOT HARD-ERR
              PERFORM 2100-READ-TAX
           END-IF.
      *
       2200-READ-INT.
           READ KZINTF
              AT END
                 SET EOF-IN TO TRUE
              NOT AT END
                 ADD 1 TO WS-IN-CNT
           END-READ.
           IF WS-ST-KZINTF NOT = "00" AND WS-ST-KZINTF NOT = "10"
              DISPLAY "KZINTF READ ST=" WS-ST-KZINTF
              PERFORM 9000-ABEND
           END-IF.
      *
       3000-ACCOUNT.
           IF IN-ACCT-NO = SPACE
              DISPLAY "ACCT NO MISSING"
              PERFORM 6000-RECON-DETAIL
           ELSE
              MOVE IN-ACCT-NO TO UI-ACCT-NO
              READ KZUNIF
                 INVALID KEY
                    DISPLAY "KZUNIF ACCT NOT FOUND " IN-ACCT-NO
                    PERFORM 6000-RECON-DETAIL
                 NOT INVALID KEY
                    PERFORM 3100-POST-INTEREST
                    PERFORM 3200-POST-UNPAID
              END-READ
           END-IF.
      *
       3100-POST-INTEREST.
           IF IN-INT-AMT NOT = ZERO
              ADD IN-INT-AMT TO WS-INT-TOTAL
              PERFORM 4100-WRITE-GL-DR
              PERFORM 4200-WRITE-GL-CR
              MOVE IN-ACCT-NO TO WS-LAST-ACCT
              MOVE WS-LAST-ACCT TO CL-RESTART-POINT
              REWRITE KZCTLF-REC
              IF WS-ST-KZCTLF NOT = "00"
                 DISPLAY "KZCTLF RESTART ST=" WS-ST-KZCTLF
                 PERFORM 9000-ABEND
              END-IF
           END-IF.
      *
       3200-POST-UNPAID.
           IF UI-CYCLE-ID = WS-CYCLE-ID
              IF UI-UNPAID-INT-AMT NOT = ZERO
                 ADD UI-UNPAID-INT-AMT TO WS-UNPAID-TOTAL
                 PERFORM 4300-WRITE-GL-UNPAID
              END-IF
           END-IF.
      *
       4100-WRITE-GL-DR.
           INITIALIZE KZGLPF-REC.
           ADD 1 TO WS-JOURNAL-SEQ.
           MOVE WS-JOURNAL-SEQ TO GL-JOURNAL-NO.
           MOVE WS-RUN-DT TO GL-POST-DT.
           MOVE IN-ACCT-NO TO GL-ACCT-NO.
           MOVE WS-CYCLE-ID TO GL-CYCLE-ID.
           MOVE "D" TO GL-DR-CR-KBN.
           MOVE "801010" TO GL-GL-ACCT-CD.
           MOVE IN-INT-AMT TO GL-POST-AMT.
           MOVE "0" TO GL-TAX-KBN.
           MOVE WS-PGM-ID TO GL-SRC-PGM-ID.
           WRITE KZGLPF-REC.
           IF WS-ST-KZGLPF NOT = "00"
              DISPLAY "KZGLPF DR WRITE ST=" WS-ST-KZGLPF
              PERFORM 9000-ABEND
           ELSE
              ADD IN-INT-AMT TO WS-GL-TOTAL
              ADD 1 TO WS-GL-CNT
           END-IF.
      *
       4200-WRITE-GL-CR.
           INITIALIZE KZGLPF-REC.
           ADD 1 TO WS-JOURNAL-SEQ.
           MOVE WS-JOURNAL-SEQ TO GL-JOURNAL-NO.
           MOVE WS-RUN-DT TO GL-POST-DT.
           MOVE IN-ACCT-NO TO GL-ACCT-NO.
           MOVE WS-CYCLE-ID TO GL-CYCLE-ID.
           MOVE "C" TO GL-DR-CR-KBN.
           MOVE "221010" TO GL-GL-ACCT-CD.
           MOVE IN-INT-AMT TO GL-POST-AMT.
           MOVE "1" TO GL-TAX-KBN.
           MOVE WS-PGM-ID TO GL-SRC-PGM-ID.
           WRITE KZGLPF-REC.
           IF WS-ST-KZGLPF NOT = "00"
              DISPLAY "KZGLPF CR WRITE ST=" WS-ST-KZGLPF
              PERFORM 9000-ABEND
           ELSE
              SUBTRACT IN-INT-AMT FROM WS-GL-TOTAL
              ADD 1 TO WS-GL-CNT
           END-IF.
      *
       4300-WRITE-GL-UNPAID.
           INITIALIZE KZGLPF-REC.
           ADD 1 TO WS-JOURNAL-SEQ.
           MOVE WS-JOURNAL-SEQ TO GL-JOURNAL-NO.
           MOVE WS-RUN-DT TO GL-POST-DT.
           MOVE UI-ACCT-NO TO GL-ACCT-NO.
           MOVE WS-CYCLE-ID TO GL-CYCLE-ID.
           MOVE "D" TO GL-DR-CR-KBN.
           MOVE "116020" TO GL-GL-ACCT-CD.
           MOVE UI-UNPAID-INT-AMT TO GL-POST-AMT.
           MOVE "9" TO GL-TAX-KBN.
           MOVE WS-PGM-ID TO GL-SRC-PGM-ID.
           WRITE KZGLPF-REC.
           IF WS-ST-KZGLPF NOT = "00"
              DISPLAY "KZGLPF UNPAID ST=" WS-ST-KZGLPF
              PERFORM 9000-ABEND
           ELSE
              ADD UI-UNPAID-INT-AMT TO WS-GL-TOTAL
              ADD 1 TO WS-GL-CNT
           END-IF.
      *
       5000-AUDIT-CALL.
           MOVE WS-PGM-ID TO LK-CALLER-PGM.
           MOVE WS-CYCLE-ID TO LK-CYCLE-ID.
           MOVE "I" TO LK-AMT-KIND.
           MOVE WS-CTL-KEY TO LK-AUDIT-KEY.
           MOVE WS-GL-TOTAL TO LK-GL-AMT.
           MOVE WS-INT-TOTAL TO LK-ACCRUAL-AMT.
           MOVE WS-TAX-TOTAL TO LK-TAX-AMT.
           MOVE WS-UNPAID-TOTAL TO LK-BASE-ACCRUAL-AMT.
           MOVE WS-NET-TOTAL TO LK-BASE-TAX-AMT.
           MOVE SPACE TO LK-RETURN-STATUS.
           CALL "KZ475S" USING LK-KZ475S.
           IF LK-RETURN-STATUS NOT = "00"
              COMPUTE WS-DIFF-AMT =
                 LK-GL-AMT - LK-ACCRUAL-AMT + LK-TAX-AMT
              DISPLAY "AUDIT NG STATUS=" LK-RETURN-STATUS
              PERFORM 6100-RECON-AUDIT
           END-IF.
      *
       6000-RECON-DETAIL.
           INITIALIZE KZRCNF-REC.
           ADD 1 TO WS-RCN-SEQ.
           MOVE WS-RCN-SEQ TO RC-RECON-KEY.
           MOVE WS-CYCLE-ID TO RC-CYCLE-ID.
           MOVE WS-GL-TOTAL TO RC-GL-TOTAL-AMT.
           MOVE IN-INT-AMT TO RC-ACCRUAL-TOTAL-AMT.
           MOVE ZERO TO RC-TAX-TOTAL-AMT.
           MOVE IN-INT-AMT TO RC-DIFF-AMT.
           MOVE "ACCT" TO RC-DISPOSITION-CD.
           WRITE KZRCNF-REC.
           IF WS-ST-KZRCNF NOT = "00"
              DISPLAY "KZRCNF DETAIL ST=" WS-ST-KZRCNF
              PERFORM 9000-ABEND
           END-IF.
      *
       6100-RECON-AUDIT.
           INITIALIZE KZRCNF-REC.
           ADD 1 TO WS-RCN-SEQ.
           MOVE WS-RCN-SEQ TO RC-RECON-KEY.
           MOVE WS-CYCLE-ID TO RC-CYCLE-ID.
           MOVE LK-GL-AMT TO RC-GL-TOTAL-AMT.
           MOVE LK-ACCRUAL-AMT TO RC-ACCRUAL-TOTAL-AMT.
           MOVE LK-TAX-AMT TO RC-TAX-TOTAL-AMT.
           MOVE WS-DIFF-AMT TO RC-DIFF-AMT.
           MOVE "AUDT" TO RC-DISPOSITION-CD.
           WRITE KZRCNF-REC.
           IF WS-ST-KZRCNF NOT = "00"
              DISPLAY "KZRCNF AUDIT ST=" WS-ST-KZRCNF
              PERFORM 9000-ABEND
           END-IF.
      *
       8000-FINAL.
           IF HARD-ERR
              MOVE WS-STAT-ERR TO CL-RUN-STATUS
              MOVE WS-PGM-ID TO CL-LAST-PGM-ID
              REWRITE KZCTLF-REC
              MOVE 12 TO RETURN-CODE
           ELSE
              MOVE WS-STAT-END TO CL-RUN-STATUS
              MOVE WS-PGM-ID TO CL-LAST-PGM-ID
              REWRITE KZCTLF-REC
              IF WS-ST-KZCTLF NOT = "00"
                 DISPLAY "KZCTLF END ST=" WS-ST-KZCTLF
                 MOVE 8 TO RETURN-CODE
              ELSE
                 MOVE 0 TO RETURN-CODE
              END-IF
           END-IF.
           MOVE WS-IN-CNT TO WS-DISP-CNT.
           DISPLAY "INPUT COUNT=" WS-DISP-CNT.
           MOVE WS-GL-CNT TO WS-DISP-CNT.
           DISPLAY "GL COUNT=" WS-DISP-CNT.
           MOVE WS-SKIP-CNT TO WS-DISP-CNT.
           DISPLAY "SKIP COUNT=" WS-DISP-CNT.
           MOVE WS-GL-TOTAL TO WS-DISP-AMT.
           DISPLAY "GL BALANCE=" WS-DISP-AMT.
           CLOSE KZINTF KZTAXF KZUNIF KZCTLF KZGLPF KZRCNF.
      *
       9000-ABEND.
           SET HARD-ERR TO TRUE.
           MOVE 12 TO RETURN-CODE.
