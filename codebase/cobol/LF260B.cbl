       IDENTIFICATION DIVISION.
       PROGRAM-ID. LF260B.
      *
      * 版数  年月日        担当      概要
      * ----  ----------  --------  ----------------------------------
      * 01    20180101    契約システム課   初版：契約異動受付反映
      * 02    20190401    契約システム課   保険金額変更時の再計算対象判定追加
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LFCHGF ASSIGN TO "LFCHGF"
               ORGANIZATION IS INDEXED
               RECORD KEY IS CG-CHANGE-ID
               FILE STATUS IS FS-LFCHGF.
           
           SELECT LFCNTF ASSIGN TO "LFCNTF"
               ORGANIZATION IS INDEXED
               RECORD KEY IS CN-POL-NO
               FILE STATUS IS FS-LFCNTF.
           
           SELECT LFPOLF ASSIGN TO "LFPOLF"
               ORGANIZATION IS INDEXED
               RECORD KEY IS PO-POL-NO
               FILE STATUS IS FS-LFPOLF.
       
       DATA DIVISION.
       FILE SECTION.
       FD  LFCHGF.
       COPY LFCHGFC.
       
       FD  LFCNTF.
       COPY LFCNTFC.
       
       FD  LFPOLF.
       COPY LFPOLFC.
       
       WORKING-STORAGE SECTION.
       01  FS-LFCHGF              PIC XX    VALUE SPACES.
       01  FS-LFCNTF              PIC XX    VALUE SPACES.
       01  FS-LFPOLF              PIC XX    VALUE SPACES.
       
       01  WS-EOF-FLAG            PIC 9    VALUE 0.
           88  EOF                           VALUE 1.
       
       01  WS-ERROR-FLAG          PIC 9    VALUE 0.
           88  ERROR-OCCURRED                VALUE 1.
       
       01  WS-CURRENT-YMD         PIC 9(8).
       01  WS-MONTH-END-YMD       PIC 9(8).
       01  WS-MONTH-YM            PIC 9(6).
       01  WS-YEAR                PIC 9(4).
       01  WS-MONTH               PIC 99.
       01  WS-DAY                 PIC 99.
       01  WS-LAST-DAY            PIC 99.
       01  WS-IS-LEAP             PIC 9    VALUE 0.
       
       01  WS-APPLY-YMD           PIC 9(8).
       01  WS-APPLY-MONTH-YM      PIC 9(6).
       01  WS-APPROVAL-KBN        PIC X    VALUE SPACE.
       01  WS-CHANGE-TYPE-KBN     PIC 99.
       
       01  WS-POLICY-FOUND        PIC 9    VALUE 0.
           88  POL-EXISTS                    VALUE 1.
       
       01  WS-POLICY-VALID        PIC 9    VALUE 0.
           88  POL-VALID                     VALUE 1.
       
       01  WS-OLD-PAY-METHOD      PIC 99.
       01  WS-NEW-PAY-METHOD      PIC 99.
       01  WS-PAY-METHOD-CHG      PIC 9    VALUE 0.
           88  PAY-METHOD-CHANGED           VALUE 1.
       
       01  WS-OLD-SUM-AMT         PIC S9(11)V99.
       01  WS-NEW-SUM-AMT         PIC S9(11)V99.
       01  WS-SUM-AMT-CHG         PIC 9    VALUE 0.
           88  SUM-AMT-CHANGED              VALUE 1.
       
       01  WS-NEEDS-RECALC        PIC 9    VALUE 0.
           88  RECALC-NEEDED                VALUE 1.
       
       01  WS-REASON-CODE         PIC 99 VALUE 00.
       
       01  WS-PROCESSED-CNT       PIC 9(6) COMP VALUE 0.
       01  WS-SKIPPED-CNT         PIC 9(6) COMP VALUE 0.
       01  WS-ERROR-CNT           PIC 9(6) COMP VALUE 0.
       
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM INITIALIZE-PROCESS.
           PERFORM CALCULATE-MONTH-END.
           
           PERFORM OPEN-ALL-FILES.
           IF ERROR-OCCURRED
               GO TO MAIN-ERROR-EXIT
           END-IF.
           
           PERFORM MAIN-READ-LOOP.
           PERFORM CLOSE-ALL-FILES.
           
           PERFORM DISPLAY-SUMMARY.
           MOVE 0 TO RETURN-CODE.
           GOBACK.
       
       MAIN-ERROR-EXIT.
           PERFORM CLOSE-ALL-FILES.
           MOVE 8 TO RETURN-CODE.
           GOBACK.
       
       INITIALIZE-PROCESS.
           ACCEPT WS-CURRENT-YMD FROM DATE YYYYMMDD.
           MOVE FUNCTION INTEGER(WS-CURRENT-YMD / 10000)
               TO WS-YEAR.
           MOVE FUNCTION INTEGER(
               FUNCTION MOD(WS-CURRENT-YMD, 10000) / 100)
               TO WS-MONTH.
           MOVE FUNCTION MOD(WS-CURRENT-YMD, 100)
               TO WS-DAY.
           MOVE 0 TO WS-PROCESSED-CNT
               WS-SKIPPED-CNT WS-ERROR-CNT.
       
       CALCULATE-MONTH-END.
           STRING WS-YEAR DELIMITED BY SIZE
               WS-MONTH DELIMITED BY SIZE
               INTO WS-MONTH-YM
           END-STRING.
           
           PERFORM DETERMINE-MONTH-LAST-DAY.
           STRING WS-YEAR DELIMITED BY SIZE
               WS-MONTH DELIMITED BY SIZE
               WS-LAST-DAY DELIMITED BY SIZE
               INTO WS-MONTH-END-YMD
           END-STRING.
       
       DETERMINE-MONTH-LAST-DAY.
           MOVE 0 TO WS-IS-LEAP.
           MOVE WS-YEAR TO WS-YEAR.
           DIVIDE WS-YEAR BY 400 
               GIVING WS-YEAR REMAINDER WS-DAY.
           IF WS-DAY = 0
               MOVE 1 TO WS-IS-LEAP
           ELSE
               MOVE WS-YEAR TO WS-YEAR
               DIVIDE WS-YEAR BY 100 
                   GIVING WS-YEAR REMAINDER WS-DAY
               IF WS-DAY NOT = 0
                   MOVE WS-YEAR TO WS-YEAR
                   DIVIDE WS-YEAR BY 4 
                       GIVING WS-YEAR REMAINDER WS-DAY
                   IF WS-DAY = 0
                       MOVE 1 TO WS-IS-LEAP
                   END-IF
               END-IF
           END-IF.
           
           EVALUATE WS-MONTH
               WHEN 1
               WHEN 3
               WHEN 5
               WHEN 7
               WHEN 8
               WHEN 10
               WHEN 12
                   MOVE 31 TO WS-LAST-DAY
               WHEN 4
               WHEN 6
               WHEN 9
               WHEN 11
                   MOVE 30 TO WS-LAST-DAY
               WHEN 2
                   IF WS-IS-LEAP = 1
                       MOVE 29 TO WS-LAST-DAY
                   ELSE
                       MOVE 28 TO WS-LAST-DAY
                   END-IF
           END-EVALUATE.
       
       OPEN-ALL-FILES.
           OPEN I-O LFCHGF.
           IF FS-LFCHGF NOT = "00"
               DISPLAY "Error: LFCHGF open failed ST="
                   FS-LFCHGF
               SET ERROR-OCCURRED TO TRUE
               EXIT PARAGRAPH
           END-IF.
           
           OPEN I-O LFCNTF.
           IF FS-LFCNTF NOT = "00"
               DISPLAY "Error: LFCNTF open failed ST="
                   FS-LFCNTF
               CLOSE LFCHGF
               SET ERROR-OCCURRED TO TRUE
               EXIT PARAGRAPH
           END-IF.
           
           OPEN INPUT LFPOLF.
           IF FS-LFPOLF NOT = "00"
               DISPLAY "Error: LFPOLF open failed ST="
                   FS-LFPOLF
               CLOSE LFCHGF
               CLOSE LFCNTF
               SET ERROR-OCCURRED TO TRUE
               EXIT PARAGRAPH
           END-IF.
       
       MAIN-READ-LOOP.
           MOVE 0 TO WS-EOF-FLAG.
           PERFORM UNTIL EOF
               READ LFCHGF
                   AT END
                       SET EOF TO TRUE
                   NOT AT END
                       PERFORM PROCESS-ONE-CHANGE
               END-READ
           END-PERFORM.
       
       PROCESS-ONE-CHANGE.
           MOVE 0 TO WS-REASON-CODE
               WS-POLICY-FOUND WS-POLICY-VALID
               WS-PAY-METHOD-CHG WS-SUM-AMT-CHG
               WS-NEEDS-RECALC.
           
           MOVE CG-APPROVAL-STATUS-KBN TO WS-APPROVAL-KBN.
           MOVE FUNCTION NUMVAL(CG-CHANGE-TYPE-KBN)
               TO WS-CHANGE-TYPE-KBN.
           MOVE FUNCTION NUMVAL(CG-APPLY-DATE)
               TO WS-APPLY-YMD.
           
           IF WS-APPROVAL-KBN NOT = "1"
               MOVE 10 TO WS-REASON-CODE
               ADD 1 TO WS-SKIPPED-CNT
               PERFORM UPDATE-CHANGE-RECORD
               EXIT PARAGRAPH
           END-IF.
           
           IF WS-APPLY-YMD > WS-MONTH-END-YMD
               MOVE 11 TO WS-REASON-CODE
               ADD 1 TO WS-SKIPPED-CNT
               PERFORM UPDATE-CHANGE-RECORD
               EXIT PARAGRAPH
           END-IF.
           
           PERFORM FETCH-POLICY-RECORD.
           IF NOT POL-EXISTS
               MOVE 13 TO WS-REASON-CODE
               ADD 1 TO WS-ERROR-CNT
               PERFORM UPDATE-CHANGE-RECORD
               EXIT PARAGRAPH
           END-IF.
           
           IF PO-POL-STATUS-KBN NOT = "01"
               MOVE 12 TO WS-REASON-CODE
               ADD 1 TO WS-SKIPPED-CNT
               PERFORM UPDATE-CHANGE-RECORD
               EXIT PARAGRAPH
           END-IF.
           
           SET POL-VALID TO TRUE.
           
           PERFORM FETCH-CONTRACT-RECORD.
           IF NOT POL-EXISTS
               MOVE 21 TO WS-REASON-CODE
               ADD 1 TO WS-ERROR-CNT
               PERFORM UPDATE-CHANGE-RECORD
               EXIT PARAGRAPH
           END-IF.
           
           EVALUATE WS-CHANGE-TYPE-KBN
               WHEN 1
                   PERFORM APPLY-ADDRESS-CHANGE
               WHEN 2
                   PERFORM APPLY-PAY-METHOD-CHANGE
               WHEN 3
                   PERFORM APPLY-SUM-ASSURED-CHANGE
               WHEN OTHER
                   MOVE 15 TO WS-REASON-CODE
           END-EVALUATE.
           
           IF WS-REASON-CODE = 0
               PERFORM UPDATE-CONTRACT-RECORD
               IF FS-LFCNTF NOT = "00"
                   MOVE 20 TO WS-REASON-CODE
                   ADD 1 TO WS-ERROR-CNT
               ELSE
                   ADD 1 TO WS-PROCESSED-CNT
                   IF RECALC-NEEDED
                       MOVE "2" TO CG-APPROVAL-STATUS-KBN
                   ELSE
                       MOVE "3" TO CG-APPROVAL-STATUS-KBN
                   END-IF
               END-IF
           ELSE
               MOVE WS-REASON-CODE TO CG-APPROVAL-STATUS-KBN
           END-IF.
           
           PERFORM UPDATE-CHANGE-RECORD.
       
       FETCH-POLICY-RECORD.
           MOVE 0 TO WS-POLICY-FOUND.
           MOVE CG-POL-NO TO PO-POL-NO.
           READ LFPOLF
               INVALID KEY
                   MOVE 0 TO WS-POLICY-FOUND
               NOT INVALID KEY
                   MOVE 1 TO WS-POLICY-FOUND
                   SET POL-EXISTS TO TRUE
           END-READ.
       
       FETCH-CONTRACT-RECORD.
           MOVE 0 TO WS-POLICY-FOUND.
           MOVE CG-POL-NO TO CN-POL-NO.
           READ LFCNTF
               INVALID KEY
                   MOVE 0 TO WS-POLICY-FOUND
               NOT INVALID KEY
                   MOVE 1 TO WS-POLICY-FOUND
                   SET POL-EXISTS TO TRUE
           END-READ.
       
       APPLY-ADDRESS-CHANGE.
           MOVE CG-APPLY-DATE TO CN-LAST-CHANGE-DATE.
       
       APPLY-PAY-METHOD-CHANGE.
           MOVE FUNCTION NUMVAL(CG-OLD-VALUE) 
               TO WS-OLD-PAY-METHOD.
           MOVE FUNCTION NUMVAL(CG-NEW-VALUE) 
               TO WS-NEW-PAY-METHOD.
           MOVE CG-NEW-VALUE TO CN-PAY-METHOD-KBN.
           MOVE CG-APPLY-DATE TO CN-LAST-CHANGE-DATE.
       
       APPLY-SUM-ASSURED-CHANGE.
           MOVE FUNCTION NUMVAL(CG-OLD-VALUE) 
               TO WS-OLD-SUM-AMT.
           MOVE FUNCTION NUMVAL(CG-NEW-VALUE) 
               TO WS-NEW-SUM-AMT.
           MOVE CG-APPLY-DATE TO CN-LAST-CHANGE-DATE.
           SET RECALC-NEEDED TO TRUE.
       
       UPDATE-CONTRACT-RECORD.
           REWRITE LFCNTF-REC.
       
       UPDATE-CHANGE-RECORD.
           MOVE WS-REASON-CODE TO CG-APPROVAL-STATUS-KBN.
           REWRITE LFCHGF-REC.
       
       CLOSE-ALL-FILES.
           CLOSE LFCHGF.
           CLOSE LFCNTF.
           CLOSE LFPOLF.
       
       DISPLAY-SUMMARY.
           DISPLAY "Contract change batch completed".
           DISPLAY "  Processed: " WS-PROCESSED-CNT.
           DISPLAY "  Skipped: " WS-SKIPPED-CNT.
           DISPLAY "  Errors: " WS-ERROR-CNT.
