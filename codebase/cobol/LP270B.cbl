       IDENTIFICATION DIVISION.
       PROGRAM-ID. LP270B.
      *版数   日付       担当       概要
      *1.0    20210510   収納システム課  初版：未収保険料督促抽出
      *
      *未収保険料督促抽出処理
      *DUE-YMが締切を過ぎてもRECEIPT-AMTがBILL-AMTに満たない請求を
      *抽出し、督促帳票用のLRRPTF明細を作成する。
      *契約状態により督促レベルを判定する。
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LPCLMF ASSIGN TO "LPCLMF"
               ORGANIZATION IS INDEXED
               RECORD KEY IS CL-CLAIM-ID
               FILE STATUS IS FS-LPCLMF.
           SELECT LFCNTF ASSIGN TO "LFCNTF"
               ORGANIZATION IS INDEXED
               RECORD KEY IS CN-POL-NO
               FILE STATUS IS FS-LFCNTF.
           SELECT LFPOLF ASSIGN TO "LFPOLF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-LFPOLF.
           SELECT LRRPTF ASSIGN TO "LRRPTF"
               ORGANIZATION IS INDEXED
               RECORD KEY IS RP-REPORT-ID
               FILE STATUS IS FS-LRRPTF.
       
       DATA DIVISION.
       FILE SECTION.
       FD  LPCLMF.
       COPY LPCLMFC.
       
       FD  LFCNTF.
       COPY LFCNTFC.
       
       FD  LFPOLF.
       COPY LFPOLFC.
       
       FD  LRRPTF.
       COPY LRRPTFC.
       
       WORKING-STORAGE SECTION.
       01  WK-FILE-STATUS.
           05  FS-LPCLMF          PIC XX VALUE "00".
           05  FS-LFCNTF          PIC XX VALUE "00".
           05  FS-LFPOLF          PIC XX VALUE "00".
           05  FS-LRRPTF          PIC XX VALUE "00".
       
       01  WK-PROCESS-FLAGS.
           05  END-OF-LPCLMF      PIC X VALUE "N".
           05  CONTRACT-EXISTS    PIC X VALUE "N".
           05  POLICY-EXISTS      PIC X VALUE "N".
       
       01  WK-WORK-FIELDS.
           05  WK-CURRENT-YM      PIC 9(6).
           05  WK-REPORT-ID       PIC 9(10) VALUE 0.
           05  WK-LINE-NO         PIC 9(5) VALUE 0.
           05  WK-DUE-AMOUNT      PIC S9(13)V99 VALUE 0.
           05  WK-MATURITY-YM     PIC 9(6) VALUE 0.
           05  WK-MONTHS-TO-LIFE  PIC S9(3) VALUE 0.
           05  WK-MATURITY-YEAR   PIC 9(4) VALUE 0.
           05  WK-MATURITY-MONTH  PIC 9(2) VALUE 0.
           05  WK-CURRENT-YEAR    PIC 9(4) VALUE 0.
           05  WK-CURRENT-MONTH   PIC 9(2) VALUE 0.
       
       01  WK-COUNTERS.
           05  WK-PROCESSED       PIC 9(8) VALUE 0.
           05  WK-REPORTED        PIC 9(8) VALUE 0.
           05  WK-ERRORS          PIC 9(8) VALUE 0.
       
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM INITIALIZE-RUN.
           
           PERFORM OPEN-FILES.
           
           EVALUATE TRUE
               WHEN FS-LPCLMF NOT = "00"
                   DISPLAY "LPCLMF オープン失敗 ST=" FS-LPCLMF
               WHEN FS-LFCNTF NOT = "00"
                   DISPLAY "LFCNTF オープン失敗 ST=" FS-LFCNTF
               WHEN FS-LFPOLF NOT = "00"
                   DISPLAY "LFPOLF オープン失敗 ST=" FS-LFPOLF
               WHEN FS-LRRPTF NOT = "00"
                   DISPLAY "LRRPTF オープン失敗 ST=" FS-LRRPTF
               WHEN OTHER
                   PERFORM PROCESS-ALL-CLAIMS
           END-EVALUATE.
           
           PERFORM CLOSE-FILES.
           
           IF WK-ERRORS > 0
               MOVE 8 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.
           
           GOBACK.
       
       INITIALIZE-RUN.
           ACCEPT WK-CURRENT-YM FROM DATE YYYYMMDD.
           MOVE WK-CURRENT-YM(1:4) TO WK-CURRENT-YEAR.
           MOVE WK-CURRENT-YM(5:2) TO WK-CURRENT-MONTH.
           
           MOVE 0 TO WK-REPORT-ID.
           MOVE 0 TO WK-LINE-NO.
           MOVE 0 TO WK-PROCESSED.
           MOVE 0 TO WK-REPORTED.
           MOVE 0 TO WK-ERRORS.
           MOVE "N" TO END-OF-LPCLMF.
       
       OPEN-FILES.
           OPEN INPUT LPCLMF.
           OPEN INPUT LFCNTF.
           OPEN INPUT LFPOLF.
           OPEN OUTPUT LRRPTF.
       
       CLOSE-FILES.
           CLOSE LPCLMF.
           CLOSE LFCNTF.
           CLOSE LFPOLF.
           CLOSE LRRPTF.
       
       PROCESS-ALL-CLAIMS.
           PERFORM UNTIL END-OF-LPCLMF = "Y"
               PERFORM READ-LPCLMF-RECORD
               IF END-OF-LPCLMF = "N"
                   ADD 1 TO WK-PROCESSED
                   
                   IF CL-DUE-YM < WK-CURRENT-YM
                       IF CL-RECEIPT-AMT < CL-BILL-AMT
                           PERFORM PROCESS-CLAIM-RECORD
                       END-IF
                   END-IF
               END-IF
           END-PERFORM.
       
       READ-LPCLMF-RECORD.
           READ LPCLMF
               AT END MOVE "Y" TO END-OF-LPCLMF
               NOT AT END MOVE "N" TO END-OF-LPCLMF
           END-READ.
           
           IF FS-LPCLMF NOT = "00" AND FS-LPCLMF NOT = "10"
               DISPLAY "LPCLMF読込エラー ST=" FS-LPCLMF
               ADD 1 TO WK-ERRORS
               MOVE "Y" TO END-OF-LPCLMF
           END-IF.
       
       PROCESS-CLAIM-RECORD.
           MOVE "N" TO CONTRACT-EXISTS.
           
           PERFORM READ-CONTRACT-RECORD.
           
           IF CONTRACT-EXISTS = "Y"
               MOVE "N" TO POLICY-EXISTS
               PERFORM READ-POLICY-RECORD
               IF POLICY-EXISTS = "Y"
                   PERFORM DETERMINE-REPORT-TYPE
                   PERFORM CALCULATE-AMOUNTS
                   PERFORM WRITE-REPORT-RECORD
               ELSE
                   ADD 1 TO WK-ERRORS
               END-IF
           ELSE
               ADD 1 TO WK-ERRORS
           END-IF.
       
       READ-CONTRACT-RECORD.
           MOVE CL-POL-NO TO CN-POL-NO.
           
           READ LFCNTF
           END-READ.
           
           EVALUATE FS-LFCNTF
               WHEN "00"
                   MOVE "Y" TO CONTRACT-EXISTS
               WHEN "23"
                   MOVE "N" TO CONTRACT-EXISTS
               WHEN OTHER
                   DISPLAY "LFCNTF読込エラー ST=" FS-LFCNTF
                       " POL=" CL-POL-NO
                   ADD 1 TO WK-ERRORS
                   MOVE "N" TO CONTRACT-EXISTS
           END-EVALUATE.
       
       READ-POLICY-RECORD.
           CLOSE LFPOLF.
           OPEN INPUT LFPOLF.
           
           PERFORM UNTIL POLICY-EXISTS = "Y" OR END-OF-LPCLMF = "Y"
               READ LFPOLF
                   AT END
                       MOVE "Y" TO END-OF-LPCLMF
                   NOT AT END
                       IF PO-POL-NO = CN-POL-NO
                           MOVE "Y" TO POLICY-EXISTS
                       END-IF
               END-READ
               
               IF FS-LFPOLF NOT = "00" AND FS-LFPOLF NOT = "10"
                   DISPLAY "LFPOLF読込エラー ST=" FS-LFPOLF
                   ADD 1 TO WK-ERRORS
                   MOVE "Y" TO POLICY-EXISTS
               END-IF
           END-PERFORM.
       
       DETERMINE-REPORT-TYPE.
           MOVE CN-MATURITY-DATE(1:4) TO WK-MATURITY-YEAR.
           MOVE CN-MATURITY-DATE(5:2) TO WK-MATURITY-MONTH.
           
           IF WK-MATURITY-YEAR = 0 OR WK-MATURITY-MONTH = 0
               MOVE 3 TO RP-REPORT-TYPE-KBN
           ELSE
               COMPUTE WK-MONTHS-TO-LIFE =
                   (WK-MATURITY-YEAR - WK-CURRENT-YEAR) * 12 +
                   (WK-MATURITY-MONTH - WK-CURRENT-MONTH)
               
               EVALUATE PO-POL-STATUS-KBN
                   WHEN "01"
                       IF WK-MONTHS-TO-LIFE <= 0
                           MOVE 3 TO RP-REPORT-TYPE-KBN
                       ELSE
                           IF WK-MONTHS-TO-LIFE <= 1
                               MOVE 2 TO RP-REPORT-TYPE-KBN
                           ELSE
                               MOVE 1 TO RP-REPORT-TYPE-KBN
                           END-IF
                       END-IF
                   WHEN "02"
                       MOVE 3 TO RP-REPORT-TYPE-KBN
                   WHEN "09"
                       MOVE 2 TO RP-REPORT-TYPE-KBN
                   WHEN OTHER
                       MOVE 3 TO RP-REPORT-TYPE-KBN
               END-EVALUATE
           END-IF.
       
       CALCULATE-AMOUNTS.
           COMPUTE WK-DUE-AMOUNT = 
               CL-BILL-AMT - CL-RECEIPT-AMT.
           
           IF WK-DUE-AMOUNT < 0
               MOVE 0 TO WK-DUE-AMOUNT
           END-IF.
           
           MOVE WK-DUE-AMOUNT TO RP-PRINT-AMT.
       
       WRITE-REPORT-RECORD.
           ADD 1 TO WK-REPORT-ID.
           ADD 1 TO WK-LINE-NO.
           
           MOVE WK-REPORT-ID TO RP-REPORT-ID.
           MOVE WK-CURRENT-YM TO RP-REPORT-YM.
           MOVE CL-POL-NO TO RP-POL-NO.
           MOVE WK-LINE-NO TO RP-LINE-NO.
           MOVE "0" TO RP-OUTPUT-STATUS-KBN.
           
           WRITE LRRPTF-REC
           END-WRITE.
           
           EVALUATE FS-LRRPTF
               WHEN "00"
                   ADD 1 TO WK-REPORTED
               WHEN OTHER
                   DISPLAY "LRRPTF書込エラー ST=" FS-LRRPTF
                   ADD 1 TO WK-ERRORS
           END-EVALUATE.
