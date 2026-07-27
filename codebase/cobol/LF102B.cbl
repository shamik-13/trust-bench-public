       IDENTIFICATION DIVISION.
       PROGRAM-ID. LF102B.
       AUTHOR. KEIRI-BATCH.
      ******************************************************************
      * 解約関連総合月次バッチ
      * LF210Bの計算結果を正として扱い、解約受付、保全、責任準備金、
      * 貸付、配当、契約異動を契約単位に照合する。
      * 本処理では未償却新契約費控除および解約控除額の再計算を行わない。
      ******************************************************************

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LFREQF ASSIGN TO "LFREQF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS RQ-REQ-ID
               FILE STATUS IS FS-LFREQF.
           SELECT LFCVPF ASSIGN TO "LFCVPF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-LFCVPF.
           SELECT LFCVRF ASSIGN TO "LFCVRF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-LFCVRF.
           SELECT LFPOLF2 ASSIGN TO "LFPOLF2"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS PO-POL-NO
               FILE STATUS IS FS-LFPOLF2.
           SELECT LFRSVF ASSIGN TO "LFRSVF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS RS-POL-NO
               FILE STATUS IS FS-LFRSVF.
           SELECT LFLOANF ASSIGN TO "LFLOANF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS LN-POL-NO
               FILE STATUS IS FS-LFLOANF.
           SELECT LFDIVF ASSIGN TO "LFDIVF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS DV-POL-NO
               FILE STATUS IS FS-LFDIVF.
           SELECT LVCHGF ASSIGN TO "LVCHGF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS CH-CHANGE-ID
               FILE STATUS IS FS-LVCHGF.
           SELECT LFMTHF ASSIGN TO "LFMTHF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-LFMTHF.
           SELECT LFREPF ASSIGN TO "LFREPF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-LFREPF.
           SELECT LFACJF ASSIGN TO "LFACJF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-LFACJF.

       DATA DIVISION.
       FILE SECTION.
       FD  LFREQF.
           COPY LFREQC.
       FD  LFCVPF.
           COPY LFCVPFC.
       FD  LFCVRF.
           COPY LFCVRFC.
       FD  LFPOLF2.
           COPY LFPOLF2C.
       FD  LFRSVF.
           COPY LFRSVC.
       FD  LFLOANF.
           COPY LFLOANC.
       FD  LFDIVF.
           COPY LFDIVC.
       FD  LVCHGF.
           COPY LVCHGC.
       FD  LFMTHF.
           COPY LFMTHC.
       FD  LFREPF.
           COPY LFREPC.
       FD  LFACJF.
           COPY LFACJC.

       WORKING-STORAGE SECTION.
       01  FS-AREA.
           05 FS-LFREQF              PIC XX.
           05 FS-LFCVPF              PIC XX.
           05 FS-LFCVRF              PIC XX.
           05 FS-LFPOLF2             PIC XX.
           05 FS-LFRSVF              PIC XX.
           05 FS-LFLOANF             PIC XX.
           05 FS-LFDIVF              PIC XX.
           05 FS-LVCHGF              PIC XX.
           05 FS-LFMTHF              PIC XX.
           05 FS-LFREPF              PIC XX.
           05 FS-LFACJF              PIC XX.

       01  SW-AREA.
           05 SW-REQ-END             PIC X VALUE SPACE.
              88 REQ-END                   VALUE "Y".
           05 SW-CI-END              PIC X VALUE SPACE.
              88 CI-END                    VALUE "Y".
           05 SW-CO-END              PIC X VALUE SPACE.
              88 CO-END                    VALUE "Y".
           05 SW-CH-END              PIC X VALUE SPACE.
              88 CH-END                    VALUE "Y".
           05 SW-HARD-ERR            PIC X VALUE SPACE.
              88 HARD-ERR                  VALUE "Y".
           05 SW-HOLD                PIC X VALUE SPACE.
              88 HOLD-ON                   VALUE "Y".
           05 SW-POL-FOUND           PIC X VALUE SPACE.
              88 POL-FOUND                 VALUE "Y".
           05 SW-RS-FOUND            PIC X VALUE SPACE.
              88 RS-FOUND                  VALUE "Y".
           05 SW-LN-FOUND            PIC X VALUE SPACE.
              88 LN-FOUND                  VALUE "Y".
           05 SW-DV-FOUND            PIC X VALUE SPACE.
              88 DV-FOUND                  VALUE "Y".
           05 SW-CI-FOUND            PIC X VALUE SPACE.
              88 CI-FOUND                  VALUE "Y".
           05 SW-CO-FOUND            PIC X VALUE SPACE.
              88 CO-FOUND                  VALUE "Y".
           05 SW-CH-FOUND            PIC X VALUE SPACE.
              88 CH-FOUND                  VALUE "Y".

       01  WK-AREA.
           05 WK-SUMMARY-YM          PIC 9(6) VALUE ZERO.
           05 WK-REQ-DATE-N          PIC 9(8) VALUE ZERO.
           05 WK-PREV-PRODUCT-CD     PIC X(10) VALUE LOW-VALUE.
           05 WK-PREV-STATUS-KBN     PIC X(02) VALUE LOW-VALUE.
           05 WK-REPORT-SEQ          PIC 9(9) VALUE ZERO.
           05 WK-ADJ-SEQ             PIC 9(9) VALUE ZERO.
           05 WK-LINE-NO             PIC 9(7) VALUE ZERO.
           05 WK-READ-CNT            PIC 9(9) VALUE ZERO.
           05 WK-OK-CNT              PIC 9(9) VALUE ZERO.
           05 WK-HOLD-CNT            PIC 9(9) VALUE ZERO.
           05 WK-ERR-CNT             PIC 9(9) VALUE ZERO.
           05 WK-MT-POL-CNT          PIC 9(9) VALUE ZERO.
           05 WK-MT-RSV-AMT          PIC S9(13)V99 VALUE ZERO.
           05 WK-MT-CV-AMT           PIC S9(13)V99 VALUE ZERO.
           05 WK-NET-PAY-AMT         PIC S9(13)V99 VALUE ZERO.
           05 WK-LOAN-DEDUCT-AMT     PIC S9(13)V99 VALUE ZERO.
           05 WK-DIV-ADD-AMT         PIC S9(13)V99 VALUE ZERO.
           05 WK-DIFF-AMT            PIC S9(13)V99 VALUE ZERO.
           05 WK-ABS-DIFF-AMT        PIC S9(13)V99 VALUE ZERO.
           05 WK-ERR-KBN             PIC X(02) VALUE SPACE.
           05 WK-PRINT-KBN           PIC X(02) VALUE SPACE.
           05 WK-EVENT-KBN           PIC X(02) VALUE SPACE.
           05 WK-POST-KBN            PIC X(02) VALUE SPACE.
           05 WK-DR-ACCT             PIC X(10) VALUE SPACE.
           05 WK-CR-ACCT             PIC X(10) VALUE SPACE.
           05 WK-AJ-AMT              PIC -9(13).99 VALUE ZERO.

       01  HOLD-CI-REC.
           05 HOLD-CI-DATA           PIC X(3000).
       01  HOLD-CO-REC.
           05 HOLD-CO-DATA           PIC X(3000).
       01  HOLD-CH-REC.
           05 HOLD-CH-DATA           PIC X(3000).

       01  CONST-AREA.
           05 CN-CV-TARGET           PIC X(02) VALUE "01".
           05 CN-REQ-CANCEL          PIC X(02) VALUE "20".
           05 CN-REQ-ACCEPT          PIC X(02) VALUE "01".
           05 CN-CALC-OK             PIC X(02) VALUE "00".
           05 CN-RS-OK               PIC X(02) VALUE "00".
           05 CN-POL-ACTIVE          PIC X(02) VALUE "01".
           05 CN-POL-LAPSE           PIC X(02) VALUE "03".
           05 CN-LOAN-ACTIVE         PIC X(02) VALUE "01".
           05 CN-DIV-PAYABLE         PIC X(02) VALUE "01".
           05 CN-DIV-CASH            PIC X(02) VALUE "01".
           05 CN-CH-SURRENDER        PIC X(02) VALUE "20".
           05 CN-CH-COMPLETE         PIC X(02) VALUE "09".
           05 CN-PRINT-APPROVE       PIC X(02) VALUE "01".
           05 CN-PRINT-HOLD          PIC X(02) VALUE "02".
           05 CN-POST-READY          PIC X(02) VALUE "01".
           05 CN-POST-HOLD           PIC X(02) VALUE "08".
           05 CN-ERR-NONE            PIC X(02) VALUE "00".
           05 CN-ERR-POL             PIC X(02) VALUE "11".
           05 CN-ERR-CI              PIC X(02) VALUE "12".
           05 CN-ERR-CO              PIC X(02) VALUE "13".
           05 CN-ERR-RSV             PIC X(02) VALUE "14".
           05 CN-ERR-LOAN            PIC X(02) VALUE "15".
           05 CN-ERR-DIV             PIC X(02) VALUE "16".
           05 CN-ERR-CHG             PIC X(02) VALUE "17".
           05 CN-ERR-DIFF            PIC X(02) VALUE "21".
           05 CN-ERR-STATE           PIC X(02) VALUE "22".
           05 CN-ACCT-CASH           PIC X(10) VALUE "1110100001".
           05 CN-ACCT-CV             PIC X(10) VALUE "2240200001".
           05 CN-ACCT-LOAN           PIC X(10) VALUE "1320300001".
           05 CN-ACCT-DIV            PIC X(10) VALUE "2250400001".

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE 0 TO RETURN-CODE
           PERFORM 1000-OPEN-FILES
           IF HARD-ERR
              PERFORM 9000-CLOSE-FILES
              MOVE 12 TO RETURN-CODE
              GOBACK
           END-IF

           PERFORM 1100-PRIME-MASTER-FILES
           IF HARD-ERR
              PERFORM 9000-CLOSE-FILES
              MOVE 12 TO RETURN-CODE
              GOBACK
           END-IF

           PERFORM 2000-PROCESS-REQUESTS UNTIL REQ-END OR HARD-ERR

           IF NOT HARD-ERR
              PERFORM 7000-WRITE-SUMMARY
           END-IF

           PERFORM 9000-CLOSE-FILES

           IF HARD-ERR
              MOVE 12 TO RETURN-CODE
           ELSE
              MOVE 0 TO RETURN-CODE
              DISPLAY "LF102B 正常終了 件数=" WK-READ-CNT
                      " 承認=" WK-OK-CNT " 保留=" WK-HOLD-CNT
           END-IF
           GOBACK.

       1000-OPEN-FILES.
           OPEN INPUT  LFREQF LFCVPF LFCVRF LFPOLF2 LFRSVF
                       LFLOANF LFDIVF LVCHGF
                OUTPUT LFMTHF LFREPF LFACJF

           IF FS-LFREQF NOT = "00"
              DISPLAY "LFREQF オープン失敗 ST=" FS-LFREQF
              SET HARD-ERR TO TRUE
           END-IF
           IF FS-LFCVPF NOT = "00"
              DISPLAY "LFCVPF オープン失敗 ST=" FS-LFCVPF
              SET HARD-ERR TO TRUE
           END-IF
           IF FS-LFCVRF NOT = "00"
              DISPLAY "LFCVRF オープン失敗 ST=" FS-LFCVRF
              SET HARD-ERR TO TRUE
           END-IF
           IF FS-LFPOLF2 NOT = "00"
              DISPLAY "LFPOLF2 オープン失敗 ST=" FS-LFPOLF2
              SET HARD-ERR TO TRUE
           END-IF
           IF FS-LFRSVF NOT = "00"
              DISPLAY "LFRSVF オープン失敗 ST=" FS-LFRSVF
              SET HARD-ERR TO TRUE
           END-IF
           IF FS-LFLOANF NOT = "00"
              DISPLAY "LFLOANF オープン失敗 ST=" FS-LFLOANF
              SET HARD-ERR TO TRUE
           END-IF
           IF FS-LFDIVF NOT = "00"
              DISPLAY "LFDIVF オープン失敗 ST=" FS-LFDIVF
              SET HARD-ERR TO TRUE
           END-IF
           IF FS-LVCHGF NOT = "00"
              DISPLAY "LVCHGF オープン失敗 ST=" FS-LVCHGF
              SET HARD-ERR TO TRUE
           END-IF
           IF FS-LFMTHF NOT = "00"
              DISPLAY "LFMTHF オープン失敗 ST=" FS-LFMTHF
              SET HARD-ERR TO TRUE
           END-IF
           IF FS-LFREPF NOT = "00"
              DISPLAY "LFREPF オープン失敗 ST=" FS-LFREPF
              SET HARD-ERR TO TRUE
           END-IF
           IF FS-LFACJF NOT = "00"
              DISPLAY "LFACJF オープン失敗 ST=" FS-LFACJF
              SET HARD-ERR TO TRUE
           END-IF.

       1100-PRIME-MASTER-FILES.
           PERFORM 1110-READ-CI
           PERFORM 1120-READ-CO
           PERFORM 1130-READ-CH
           PERFORM 1200-READ-REQ.

       1110-READ-CI.
           READ LFCVPF
              AT END
                 SET CI-END TO TRUE
              NOT AT END
                 MOVE LFCVPF-REC TO HOLD-CI-DATA
           END-READ
           IF FS-LFCVPF NOT = "00" AND FS-LFCVPF NOT = "10"
              DISPLAY "LFCVPF 読込失敗 ST=" FS-LFCVPF
              SET HARD-ERR TO TRUE
           END-IF.

       1120-READ-CO.
           READ LFCVRF
              AT END
                 SET CO-END TO TRUE
              NOT AT END
                 MOVE LFCVRF-REC TO HOLD-CO-DATA
           END-READ
           IF FS-LFCVRF NOT = "00" AND FS-LFCVRF NOT = "10"
              DISPLAY "LFCVRF 読込失敗 ST=" FS-LFCVRF
              SET HARD-ERR TO TRUE
           END-IF.

       1130-READ-CH.
           READ LVCHGF
              AT END
                 SET CH-END TO TRUE
              NOT AT END
                 MOVE LVCHGF-REC TO HOLD-CH-DATA
           END-READ
           IF FS-LVCHGF NOT = "00" AND FS-LVCHGF NOT = "10"
              DISPLAY "LVCHGF 読込失敗 ST=" FS-LVCHGF
              SET HARD-ERR TO TRUE
           END-IF.

       1200-READ-REQ.
           READ LFREQF
              AT END
                 SET REQ-END TO TRUE
              NOT AT END
                 ADD 1 TO WK-READ-CNT
           END-READ
           IF FS-LFREQF NOT = "00" AND FS-LFREQF NOT = "10"
              DISPLAY "LFREQF 読込失敗 ST=" FS-LFREQF
              SET HARD-ERR TO TRUE
           END-IF.

       2000-PROCESS-REQUESTS.
           MOVE SPACE TO SW-HOLD
           MOVE CN-ERR-NONE TO WK-ERR-KBN
           MOVE ZERO TO WK-NET-PAY-AMT WK-LOAN-DEDUCT-AMT
                        WK-DIV-ADD-AMT WK-DIFF-AMT WK-ABS-DIFF-AMT

           IF RQ-REQ-TYPE-KBN NOT = CN-REQ-CANCEL
              PERFORM 1200-READ-REQ
              EXIT PARAGRAPH
           END-IF

           IF RQ-REQ-STATUS-KBN NOT = CN-REQ-ACCEPT
              MOVE CN-ERR-STATE TO WK-ERR-KBN
              SET HOLD-ON TO TRUE
           END-IF

           PERFORM 2100-READ-POLICY
           PERFORM 2200-READ-RESERVE
           PERFORM 2300-READ-LOAN
           PERFORM 2400-READ-DIVIDEND
           PERFORM 2500-FIND-CI
           PERFORM 2600-FIND-CO
           PERFORM 2700-FIND-CHANGE
           PERFORM 3000-VALIDATE-SET
           PERFORM 4000-CALCULATE-PAYMENT
           PERFORM 5000-WRITE-DETAILS
           PERFORM 6000-ACCUMULATE-SUMMARY
           PERFORM 1200-READ-REQ.

       2100-READ-POLICY.
           MOVE SPACE TO SW-POL-FOUND
           MOVE RQ-POL-NO TO PO-POL-NO
           READ LFPOLF2 KEY IS PO-POL-NO
              INVALID KEY
                 CONTINUE
              NOT INVALID KEY
                 SET POL-FOUND TO TRUE
           END-READ
           IF FS-LFPOLF2 NOT = "00" AND FS-LFPOLF2 NOT = "23"
              DISPLAY "LFPOLF2 読込失敗 ST=" FS-LFPOLF2
              SET HARD-ERR TO TRUE
           END-IF.

       2200-READ-RESERVE.
           MOVE SPACE TO SW-RS-FOUND
           MOVE RQ-POL-NO TO RS-POL-NO
           READ LFRSVF KEY IS RS-POL-NO
              INVALID KEY
                 CONTINUE
              NOT INVALID KEY
                 SET RS-FOUND TO TRUE
           END-READ
           IF FS-LFRSVF NOT = "00" AND FS-LFRSVF NOT = "23"
              DISPLAY "LFRSVF 読込失敗 ST=" FS-LFRSVF
              SET HARD-ERR TO TRUE
           END-IF.

       2300-READ-LOAN.
           MOVE SPACE TO SW-LN-FOUND
           MOVE RQ-POL-NO TO LN-POL-NO
           READ LFLOANF KEY IS LN-POL-NO
              INVALID KEY
                 CONTINUE
              NOT INVALID KEY
                 SET LN-FOUND TO TRUE
           END-READ
           IF FS-LFLOANF NOT = "00" AND FS-LFLOANF NOT = "23"
              DISPLAY "LFLOANF 読込失敗 ST=" FS-LFLOANF
              SET HARD-ERR TO TRUE
           END-IF.

       2400-READ-DIVIDEND.
           MOVE SPACE TO SW-DV-FOUND
           MOVE RQ-POL-NO TO DV-POL-NO
           READ LFDIVF KEY IS DV-POL-NO
              INVALID KEY
                 CONTINUE
              NOT INVALID KEY
                 SET DV-FOUND TO TRUE
           END-READ
           IF FS-LFDIVF NOT = "00" AND FS-LFDIVF NOT = "23"
              DISPLAY "LFDIVF 読込失敗 ST=" FS-LFDIVF
              SET HARD-ERR TO TRUE
           END-IF.

       2500-FIND-CI.
           MOVE SPACE TO SW-CI-FOUND
           PERFORM UNTIL CI-END OR CI-FOUND OR HARD-ERR
              MOVE HOLD-CI-DATA TO LFCVPF-REC
              IF CI-POL-NO < RQ-POL-NO
                 PERFORM 1110-READ-CI
              ELSE
                 IF CI-POL-NO = RQ-POL-NO
                    SET CI-FOUND TO TRUE
                 ELSE
                    EXIT PERFORM
                 END-IF
              END-IF
           END-PERFORM.

       2600-FIND-CO.
           MOVE SPACE TO SW-CO-FOUND
           PERFORM UNTIL CO-END OR CO-FOUND OR HARD-ERR
              MOVE HOLD-CO-DATA TO LFCVRF-REC
              IF CO-POL-NO < RQ-POL-NO
                 PERFORM 1120-READ-CO
              ELSE
                 IF CO-POL-NO = RQ-POL-NO
                    SET CO-FOUND TO TRUE
                 ELSE
                    EXIT PERFORM
                 END-IF
              END-IF
           END-PERFORM.

       2700-FIND-CHANGE.
           MOVE SPACE TO SW-CH-FOUND
           PERFORM UNTIL CH-END OR CH-FOUND OR HARD-ERR
              MOVE HOLD-CH-DATA TO LVCHGF-REC
              IF CH-POL-NO < RQ-POL-NO
                 PERFORM 1130-READ-CH
              ELSE
                 IF CH-POL-NO = RQ-POL-NO
                    SET CH-FOUND TO TRUE
                 ELSE
                    EXIT PERFORM
                 END-IF
              END-IF
           END-PERFORM.

       3000-VALIDATE-SET.
           IF NOT POL-FOUND
              MOVE CN-ERR-POL TO WK-ERR-KBN
              SET HOLD-ON TO TRUE
           ELSE
              IF PO-CONTRACT-STATUS-KBN NOT = CN-POL-ACTIVE
                 AND PO-CONTRACT-STATUS-KBN NOT = CN-POL-LAPSE
                 MOVE CN-ERR-STATE TO WK-ERR-KBN
                 SET HOLD-ON TO TRUE
              END-IF
           END-IF

           IF NOT CI-FOUND
              MOVE CN-ERR-CI TO WK-ERR-KBN
              SET HOLD-ON TO TRUE
           ELSE
              IF CI-CV-STATUS-KBN NOT = CN-CV-TARGET
                 MOVE CN-ERR-CI TO WK-ERR-KBN
                 SET HOLD-ON TO TRUE
              END-IF
           END-IF

           IF NOT CO-FOUND
              MOVE CN-ERR-CO TO WK-ERR-KBN
              SET HOLD-ON TO TRUE
           ELSE
              IF CO-CALC-STATUS-KBN NOT = CN-CALC-OK
                 MOVE CN-ERR-CO TO WK-ERR-KBN
                 SET HOLD-ON TO TRUE
              END-IF
           END-IF

           IF NOT RS-FOUND
              MOVE CN-ERR-RSV TO WK-ERR-KBN
              SET HOLD-ON TO TRUE
           ELSE
              IF RS-CALC-STATUS-KBN NOT = CN-RS-OK
                 MOVE CN-ERR-RSV TO WK-ERR-KBN
                 SET HOLD-ON TO TRUE
              END-IF
           END-IF

           IF LN-FOUND
              IF LN-LOAN-STATUS-KBN = CN-LOAN-ACTIVE
                 COMPUTE WK-LOAN-DEDUCT-AMT =
                         LN-LOAN-BAL-AMT + LN-ACCRUED-INT-AMT
              ELSE
                 MOVE CN-ERR-LOAN TO WK-ERR-KBN
                 SET HOLD-ON TO TRUE
              END-IF
           END-IF

           IF DV-FOUND
              IF DV-DIV-STATUS-KBN = CN-DIV-PAYABLE
                 AND DV-DIV-ALLOC-KBN = CN-DIV-CASH
                 MOVE DV-DIV-AMT TO WK-DIV-ADD-AMT
              ELSE
                 MOVE CN-ERR-DIV TO WK-ERR-KBN
                 SET HOLD-ON TO TRUE
              END-IF
           END-IF

           IF NOT CH-FOUND
              MOVE CN-ERR-CHG TO WK-ERR-KBN
              SET HOLD-ON TO TRUE
           ELSE
              IF CH-CHANGE-TYPE-KBN NOT = CN-CH-SURRENDER
                 MOVE CN-ERR-CHG TO WK-ERR-KBN
                 SET HOLD-ON TO TRUE
              END-IF
              IF CH-CHANGE-STATUS-KBN NOT = CN-CH-COMPLETE
                 MOVE CN-ERR-STATE TO WK-ERR-KBN
                 SET HOLD-ON TO TRUE
              END-IF
           END-IF.

       4000-CALCULATE-PAYMENT.
           IF CO-FOUND
              COMPUTE WK-NET-PAY-AMT =
                      CO-CV-AMT - WK-LOAN-DEDUCT-AMT + WK-DIV-ADD-AMT
           END-IF

           IF CI-FOUND AND CO-FOUND
              COMPUTE WK-DIFF-AMT = CO-RESERVE-AMT - CI-RESERVE-AMT
              IF WK-DIFF-AMT < ZERO
                 COMPUTE WK-ABS-DIFF-AMT = WK-DIFF-AMT * -1
              ELSE
                 MOVE WK-DIFF-AMT TO WK-ABS-DIFF-AMT
              END-IF
              IF WK-ABS-DIFF-AMT > 100
                 MOVE CN-ERR-DIFF TO WK-ERR-KBN
                 SET HOLD-ON TO TRUE
              END-IF
           END-IF

           IF RS-FOUND AND CO-FOUND
              COMPUTE WK-DIFF-AMT = CO-RESERVE-AMT - RS-RESERVE-AMT
              IF WK-DIFF-AMT < ZERO
                 COMPUTE WK-ABS-DIFF-AMT = WK-DIFF-AMT * -1
              ELSE
                 MOVE WK-DIFF-AMT TO WK-ABS-DIFF-AMT
              END-IF
              IF WK-ABS-DIFF-AMT > 1000
                 MOVE CN-ERR-RSV TO WK-ERR-KBN
                 SET HOLD-ON TO TRUE
              END-IF
           END-IF

           IF WK-NET-PAY-AMT < ZERO
              MOVE CN-ERR-DIFF TO WK-ERR-KBN
              SET HOLD-ON TO TRUE
           END-IF.

       5000-WRITE-DETAILS.
           ADD 1 TO WK-REPORT-SEQ
           ADD 1 TO WK-LINE-NO
           MOVE WK-REPORT-SEQ TO RP-REPORT-ID
           MOVE WK-LINE-NO TO RP-LINE-NO
           MOVE RQ-POL-NO TO RP-POL-NO
           IF HOLD-ON
              MOVE CN-PRINT-HOLD TO RP-PRINT-KBN
              MOVE WK-ERR-KBN TO RP-ERROR-KBN
              ADD 1 TO WK-HOLD-CNT
           ELSE
              MOVE CN-PRINT-APPROVE TO RP-PRINT-KBN
              MOVE CN-ERR-NONE TO RP-ERROR-KBN
              ADD 1 TO WK-OK-CNT
           END-IF
           MOVE WK-NET-PAY-AMT TO RP-PRINT-AMT
           WRITE LFREPF-REC
           IF FS-LFREPF NOT = "00"
              DISPLAY "LFREPF 書込失敗 ST=" FS-LFREPF
              SET HARD-ERR TO TRUE
           END-IF

           ADD 1 TO WK-ADJ-SEQ
           MOVE WK-ADJ-SEQ TO AJ-ADJ-ID
           MOVE RQ-POL-NO TO AJ-POL-NO
           MOVE CN-REQ-CANCEL TO AJ-EVENT-KBN
           MOVE CN-ACCT-CV TO AJ-DR-ACCT-CD
           MOVE CN-ACCT-CASH TO AJ-CR-ACCT-CD
           MOVE WK-NET-PAY-AMT TO WK-AJ-AMT
           MOVE WK-AJ-AMT TO AJ-AMT
           IF HOLD-ON
              MOVE CN-POST-HOLD TO AJ-POST-STATUS-KBN
           ELSE
              MOVE CN-POST-READY TO AJ-POST-STATUS-KBN
           END-IF
           WRITE LFACJF-REC
           IF FS-LFACJF NOT = "00"
              DISPLAY "LFACJF 書込失敗 ST=" FS-LFACJF
              SET HARD-ERR TO TRUE
           END-IF

           IF LN-FOUND AND WK-LOAN-DEDUCT-AMT > ZERO
              ADD 1 TO WK-ADJ-SEQ
              MOVE WK-ADJ-SEQ TO AJ-ADJ-ID
              MOVE RQ-POL-NO TO AJ-POL-NO
              MOVE CN-REQ-CANCEL TO AJ-EVENT-KBN
              MOVE CN-ACCT-LOAN TO AJ-DR-ACCT-CD
              MOVE CN-ACCT-CV TO AJ-CR-ACCT-CD
              MOVE WK-LOAN-DEDUCT-AMT TO WK-AJ-AMT
              MOVE WK-AJ-AMT TO AJ-AMT
              IF HOLD-ON
                 MOVE CN-POST-HOLD TO AJ-POST-STATUS-KBN
              ELSE
                 MOVE CN-POST-READY TO AJ-POST-STATUS-KBN
              END-IF
              WRITE LFACJF-REC
              IF FS-LFACJF NOT = "00"
                 DISPLAY "LFACJF 貸付書込失敗 ST=" FS-LFACJF
                 SET HARD-ERR TO TRUE
              END-IF
           END-IF

           IF DV-FOUND AND WK-DIV-ADD-AMT > ZERO
              ADD 1 TO WK-ADJ-SEQ
              MOVE WK-ADJ-SEQ TO AJ-ADJ-ID
              MOVE RQ-POL-NO TO AJ-POL-NO
              MOVE CN-REQ-CANCEL TO AJ-EVENT-KBN
              MOVE CN-ACCT-DIV TO AJ-DR-ACCT-CD
              MOVE CN-ACCT-CASH TO AJ-CR-ACCT-CD
              MOVE WK-DIV-ADD-AMT TO WK-AJ-AMT
              MOVE WK-AJ-AMT TO AJ-AMT
              IF HOLD-ON
                 MOVE CN-POST-HOLD TO AJ-POST-STATUS-KBN
              ELSE
                 MOVE CN-POST-READY TO AJ-POST-STATUS-KBN
              END-IF
              WRITE LFACJF-REC
              IF FS-LFACJF NOT = "00"
                 DISPLAY "LFACJF 配当書込失敗 ST=" FS-LFACJF
                 SET HARD-ERR TO TRUE
              END-IF
           END-IF.

       6000-ACCUMULATE-SUMMARY.
           IF NOT POL-FOUND
              EXIT PARAGRAPH
           END-IF

           IF WK-PREV-PRODUCT-CD = LOW-VALUE
              MOVE PO-PRODUCT-CD TO WK-PREV-PRODUCT-CD
              MOVE PO-CONTRACT-STATUS-KBN TO WK-PREV-STATUS-KBN
           END-IF

           IF PO-PRODUCT-CD NOT = WK-PREV-PRODUCT-CD
              OR PO-CONTRACT-STATUS-KBN NOT = WK-PREV-STATUS-KBN
              PERFORM 7000-WRITE-SUMMARY
              MOVE PO-PRODUCT-CD TO WK-PREV-PRODUCT-CD
              MOVE PO-CONTRACT-STATUS-KBN TO WK-PREV-STATUS-KBN
              MOVE ZERO TO WK-MT-POL-CNT WK-MT-RSV-AMT WK-MT-CV-AMT
           END-IF

           ADD 1 TO WK-MT-POL-CNT
           IF RS-FOUND
              ADD RS-RESERVE-AMT TO WK-MT-RSV-AMT
           END-IF
           IF CO-FOUND
              ADD CO-CV-AMT TO WK-MT-CV-AMT
           END-IF.

       7000-WRITE-SUMMARY.
           IF WK-MT-POL-CNT = ZERO
              EXIT PARAGRAPH
           END-IF

           IF RQ-REQ-DATE NUMERIC
              MOVE RQ-REQ-DATE TO WK-REQ-DATE-N
              DIVIDE WK-REQ-DATE-N BY 100 GIVING WK-SUMMARY-YM
           ELSE
              MOVE ZERO TO WK-SUMMARY-YM
           END-IF

           MOVE WK-SUMMARY-YM TO MT-SUMMARY-YM
           MOVE WK-PREV-PRODUCT-CD TO MT-PRODUCT-CD
           MOVE WK-PREV-STATUS-KBN TO MT-CONTRACT-STATUS-KBN
           MOVE WK-MT-POL-CNT TO MT-POL-CNT
           MOVE WK-MT-RSV-AMT TO MT-RESERVE-TOTAL-AMT
           MOVE WK-MT-CV-AMT TO MT-CV-TOTAL-AMT
           WRITE LFMTHF-REC
           IF FS-LFMTHF NOT = "00"
              DISPLAY "LFMTHF 書込失敗 ST=" FS-LFMTHF
              SET HARD-ERR TO TRUE
           END-IF.

       9000-CLOSE-FILES.
           CLOSE LFREQF LFCVPF LFCVRF LFPOLF2 LFRSVF
                 LFLOANF LFDIVF LVCHGF LFMTHF LFREPF LFACJF

           IF FS-LFREQF NOT = "00"
              DISPLAY "LFREQF クローズ状態 ST=" FS-LFREQF
           END-IF
           IF FS-LFCVPF NOT = "00"
              DISPLAY "LFCVPF クローズ状態 ST=" FS-LFCVPF
           END-IF
           IF FS-LFCVRF NOT = "00"
              DISPLAY "LFCVRF クローズ状態 ST=" FS-LFCVRF
           END-IF
           IF FS-LFPOLF2 NOT = "00"
              DISPLAY "LFPOLF2 クローズ状態 ST=" FS-LFPOLF2
           END-IF
           IF FS-LFRSVF NOT = "00"
              DISPLAY "LFRSVF クローズ状態 ST=" FS-LFRSVF
           END-IF
           IF FS-LFLOANF NOT = "00"
              DISPLAY "LFLOANF クローズ状態 ST=" FS-LFLOANF
           END-IF
           IF FS-LFDIVF NOT = "00"
              DISPLAY "LFDIVF クローズ状態 ST=" FS-LFDIVF
           END-IF
           IF FS-LVCHGF NOT = "00"
              DISPLAY "LVCHGF クローズ状態 ST=" FS-LVCHGF
           END-IF
           IF FS-LFMTHF NOT = "00"
              DISPLAY "LFMTHF クローズ状態 ST=" FS-LFMTHF
           END-IF
           IF FS-LFREPF NOT = "00"
              DISPLAY "LFREPF クローズ状態 ST=" FS-LFREPF
           END-IF
           IF FS-LFACJF NOT = "00"
              DISPLAY "LFACJF クローズ状態 ST=" FS-LFACJF
           END-IF.
