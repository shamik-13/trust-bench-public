       IDENTIFICATION DIVISION.
       PROGRAM-ID. LR260B.
       AUTHOR.     みらい生命 システム部.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LFCHGF ASSIGN TO "LFCHGF"
               ORGANIZATION IS INDEXED
               ACCESS MODE  IS SEQUENTIAL
               RECORD KEY   IS CG-CHANGE-ID
               FILE STATUS  IS FS-LFCHGF.
      *
           SELECT LPCLMF ASSIGN TO "LPCLMF"
               ORGANIZATION IS INDEXED
               ACCESS MODE  IS DYNAMIC
               RECORD KEY   IS CL-CLAIM-ID
               FILE STATUS  IS FS-LPCLMF.
      *
           SELECT LFCNTF ASSIGN TO "LFCNTF"
               ORGANIZATION IS INDEXED
               ACCESS MODE  IS RANDOM
               RECORD KEY   IS CN-POL-NO
               FILE STATUS  IS FS-LFCNTF.
      *
           SELECT LRRPTF ASSIGN TO "LRRPTF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE  IS SEQUENTIAL
               FILE STATUS  IS FS-LRRPTF.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  LFCHGF.
           COPY LFCHGFC.
      *
       FD  LPCLMF.
           COPY LPCLMFC.
      *
       FD  LFCNTF.
           COPY LFCNTFC.
      *
       FD  LRRPTF.
           COPY LRRPTFC.
      *
       WORKING-STORAGE SECTION.
       01  FS-AREA.
           05 FS-LFCHGF                 PIC XX VALUE SPACE.
           05 FS-LPCLMF                 PIC XX VALUE SPACE.
           05 FS-LFCNTF                 PIC XX VALUE SPACE.
           05 FS-LRRPTF                 PIC XX VALUE SPACE.
      *
       01  SW-AREA.
           05 SW-EOF-LFCHGF             PIC X VALUE "N".
              88 EOF-LFCHGF                   VALUE "Y".
           05 SW-EOF-LPCLMF             PIC X VALUE "N".
              88 EOF-LPCLMF                   VALUE "Y".
           05 SW-HARD-ERR               PIC X VALUE "N".
              88 HARD-ERR                     VALUE "Y".
           05 SW-CONTRACT-FOUND         PIC X VALUE "N".
              88 CONTRACT-FOUND               VALUE "Y".
           05 SW-CLAIM-FOUND            PIC X VALUE "N".
              88 CLAIM-FOUND                  VALUE "Y".
      *
       01  WK-AREA.
           05 WK-REPORT-YM              PIC 9(06) VALUE ZERO.
           05 WK-REPORT-SEQ             PIC 9(07) VALUE ZERO.
           05 WK-LINE-NO                PIC 9(05) VALUE ZERO.
           05 WK-REPORT-ID              PIC X(16) VALUE SPACE.
           05 WK-REPORT-TYPE            PIC X VALUE SPACE.
           05 WK-OLDEST-DUE-YM          PIC 9(06) VALUE ZERO.
           05 WK-UNPAID-AMT             PIC S9(11)V99 VALUE ZERO.
           05 WK-BAL-AMT                PIC S9(11)V99 VALUE ZERO.
           05 WK-READ-CNT               PIC 9(09) VALUE ZERO.
           05 WK-SKIP-CNT               PIC 9(09) VALUE ZERO.
           05 WK-OUT-CNT                PIC 9(09) VALUE ZERO.
           05 WK-CLAIM-CNT              PIC 9(09) VALUE ZERO.
           05 WK-ZERO-DATE              PIC 9(08) VALUE ZERO.
      *
       01  CNST-AREA.
           05 CNST-CHANGE-CANCEL        PIC X VALUE "1".
           05 CNST-CHANGE-LAPSE         PIC X VALUE "2".
           05 CNST-APPROVAL-WAIT        PIC X VALUE "0".
           05 CNST-CLAIM-OPEN           PIC X VALUE "0".
           05 CNST-CLAIM-PARTIAL        PIC X VALUE "1".
           05 CNST-OUT-NORMAL           PIC X VALUE "0".
      *
       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE 8 TO RETURN-CODE
           PERFORM 1000-INIT
           IF NOT HARD-ERR
              PERFORM 2000-PROCESS UNTIL EOF-LFCHGF OR HARD-ERR
           END-IF
           PERFORM 9000-END
           GOBACK.
      *
       1000-INIT.
           MOVE FUNCTION NUMVAL(FUNCTION CURRENT-DATE(1:6))
             TO WK-REPORT-YM
      *
           OPEN INPUT LFCHGF
           IF FS-LFCHGF NOT = "00"
              DISPLAY "LFCHGF オープン失敗 ST=" FS-LFCHGF
              SET HARD-ERR TO TRUE
              EXIT PARAGRAPH
           END-IF
      *
           OPEN INPUT LPCLMF
           IF FS-LPCLMF NOT = "00"
              DISPLAY "LPCLMF オープン失敗 ST=" FS-LPCLMF
              SET HARD-ERR TO TRUE
              EXIT PARAGRAPH
           END-IF
      *
           OPEN INPUT LFCNTF
           IF FS-LFCNTF NOT = "00"
              DISPLAY "LFCNTF オープン失敗 ST=" FS-LFCNTF
              SET HARD-ERR TO TRUE
              EXIT PARAGRAPH
           END-IF
      *
           OPEN OUTPUT LRRPTF
           IF FS-LRRPTF NOT = "00"
              DISPLAY "LRRPTF オープン失敗 ST=" FS-LRRPTF
              SET HARD-ERR TO TRUE
              EXIT PARAGRAPH
           END-IF
      *
           PERFORM 2100-READ-LFCHGF.
      *
       2000-PROCESS.
           ADD 1 TO WK-READ-CNT
      *
           IF CG-APPROVAL-STATUS-KBN NOT = CNST-APPROVAL-WAIT
              ADD 1 TO WK-SKIP-CNT
              PERFORM 2100-READ-LFCHGF
              EXIT PARAGRAPH
           END-IF
      *
           IF CG-CHANGE-TYPE-KBN NOT = CNST-CHANGE-CANCEL
              AND CG-CHANGE-TYPE-KBN NOT = CNST-CHANGE-LAPSE
              ADD 1 TO WK-SKIP-CNT
              PERFORM 2100-READ-LFCHGF
              EXIT PARAGRAPH
           END-IF
      *
           PERFORM 3000-READ-CONTRACT
           IF HARD-ERR
              EXIT PARAGRAPH
           END-IF
      *
           IF NOT CONTRACT-FOUND
              DISPLAY "契約マスタ未登録 POL=" CG-POL-NO
              ADD 1 TO WK-SKIP-CNT
              PERFORM 2100-READ-LFCHGF
              EXIT PARAGRAPH
           END-IF
      *
           IF CN-LAST-CHANGE-DATE NOT = WK-ZERO-DATE
              AND CN-LAST-CHANGE-DATE >= CG-APPLY-DATE
              ADD 1 TO WK-SKIP-CNT
              PERFORM 2100-READ-LFCHGF
              EXIT PARAGRAPH
           END-IF
      *
           PERFORM 4000-SCAN-CLAIM
           IF HARD-ERR
              EXIT PARAGRAPH
           END-IF
      *
           IF CLAIM-FOUND
              PERFORM 5000-WRITE-REPORT
           ELSE
              ADD 1 TO WK-SKIP-CNT
           END-IF
      *
           PERFORM 2100-READ-LFCHGF.
      *
       2100-READ-LFCHGF.
           READ LFCHGF
              AT END
                 SET EOF-LFCHGF TO TRUE
              NOT AT END
                 IF FS-LFCHGF NOT = "00"
                    DISPLAY "LFCHGF 読込失敗 ST=" FS-LFCHGF
                    SET HARD-ERR TO TRUE
                 END-IF
           END-READ.
      *
       3000-READ-CONTRACT.
           MOVE "N" TO SW-CONTRACT-FOUND
           MOVE CG-POL-NO TO CN-POL-NO
           READ LFCNTF
              INVALID KEY
                 IF FS-LFCNTF = "23"
                    CONTINUE
                 ELSE
                    DISPLAY "LFCNTF 読込失敗 ST=" FS-LFCNTF
                    DISPLAY "対象契約番号=" CG-POL-NO
                    SET HARD-ERR TO TRUE
                 END-IF
              NOT INVALID KEY
                 SET CONTRACT-FOUND TO TRUE
           END-READ.
      *
       4000-SCAN-CLAIM.
           MOVE "N" TO SW-CLAIM-FOUND
           MOVE ZERO TO WK-OLDEST-DUE-YM
           MOVE ZERO TO WK-UNPAID-AMT
           MOVE ZERO TO WK-CLAIM-CNT
           MOVE "N" TO SW-EOF-LPCLMF
      *
           MOVE LOW-VALUES TO CL-CLAIM-ID
           START LPCLMF KEY NOT < CL-CLAIM-ID
              INVALID KEY
                 IF FS-LPCLMF = "23"
                    SET EOF-LPCLMF TO TRUE
                 ELSE
                    DISPLAY "LPCLMF START失敗 ST=" FS-LPCLMF
                    SET HARD-ERR TO TRUE
                 END-IF
           END-START
      *
           PERFORM UNTIL EOF-LPCLMF OR HARD-ERR
              READ LPCLMF NEXT RECORD
                 AT END
                    SET EOF-LPCLMF TO TRUE
                 NOT AT END
                    IF FS-LPCLMF NOT = "00"
                       DISPLAY "LPCLMF 順読失敗 ST=" FS-LPCLMF
                       SET HARD-ERR TO TRUE
                    ELSE
                       PERFORM 4100-JUDGE-CLAIM
                    END-IF
              END-READ
           END-PERFORM.
      *
       4100-JUDGE-CLAIM.
           IF CL-POL-NO NOT = CG-POL-NO
              EXIT PARAGRAPH
           END-IF
      *
           IF CL-CLAIM-STATUS-KBN NOT = CNST-CLAIM-OPEN
              AND CL-CLAIM-STATUS-KBN NOT = CNST-CLAIM-PARTIAL
              EXIT PARAGRAPH
           END-IF
      *
           COMPUTE WK-BAL-AMT = CL-BILL-AMT - CL-RECEIPT-AMT
           IF WK-BAL-AMT <= ZERO
              EXIT PARAGRAPH
           END-IF
      *
           ADD WK-BAL-AMT TO WK-UNPAID-AMT
           ADD 1 TO WK-CLAIM-CNT
           SET CLAIM-FOUND TO TRUE
      *
           IF WK-OLDEST-DUE-YM = ZERO
              OR CL-DUE-YM < WK-OLDEST-DUE-YM
              MOVE CL-DUE-YM TO WK-OLDEST-DUE-YM
           END-IF.
      *
       5000-WRITE-REPORT.
           INITIALIZE LRRPTF-REC
           ADD 1 TO WK-REPORT-SEQ
           ADD 1 TO WK-LINE-NO
           ADD 1 TO WK-OUT-CNT
      *
           MOVE SPACES TO WK-REPORT-ID
           STRING "LR260B" WK-REPORT-YM WK-REPORT-SEQ
             DELIMITED BY SIZE INTO WK-REPORT-ID
           END-STRING
      *
           IF CG-CHANGE-TYPE-KBN = CNST-CHANGE-CANCEL
              MOVE "1" TO WK-REPORT-TYPE
           ELSE
              MOVE "2" TO WK-REPORT-TYPE
           END-IF
      *
           MOVE WK-REPORT-ID       TO RP-REPORT-ID
           MOVE WK-REPORT-YM       TO RP-REPORT-YM
           MOVE WK-REPORT-TYPE     TO RP-REPORT-TYPE-KBN
           MOVE CG-POL-NO          TO RP-POL-NO
           MOVE WK-LINE-NO         TO RP-LINE-NO
           MOVE WK-UNPAID-AMT      TO RP-PRINT-AMT
           MOVE CNST-OUT-NORMAL    TO RP-OUTPUT-STATUS-KBN
      *
           WRITE LRRPTF-REC
           IF FS-LRRPTF NOT = "00"
              DISPLAY "LRRPTF 書込失敗 ST=" FS-LRRPTF
              DISPLAY "対象契約番号=" CG-POL-NO
              SET HARD-ERR TO TRUE
           END-IF.
      *
       9000-END.
           IF FS-LFCHGF = "00"
              CLOSE LFCHGF
              IF FS-LFCHGF NOT = "00"
                 DISPLAY "LFCHGF クローズ失敗 ST=" FS-LFCHGF
                 SET HARD-ERR TO TRUE
              END-IF
           END-IF
      *
           IF FS-LPCLMF = "00"
              CLOSE LPCLMF
              IF FS-LPCLMF NOT = "00"
                 DISPLAY "LPCLMF クローズ失敗 ST=" FS-LPCLMF
                 SET HARD-ERR TO TRUE
              END-IF
           END-IF
      *
           IF FS-LFCNTF = "00"
              CLOSE LFCNTF
              IF FS-LFCNTF NOT = "00"
                 DISPLAY "LFCNTF クローズ失敗 ST=" FS-LFCNTF
                 SET HARD-ERR TO TRUE
              END-IF
           END-IF
      *
           IF FS-LRRPTF = "00"
              CLOSE LRRPTF
              IF FS-LRRPTF NOT = "00"
                 DISPLAY "LRRPTF クローズ失敗 ST=" FS-LRRPTF
                 SET HARD-ERR TO TRUE
              END-IF
           END-IF
      *
           DISPLAY "LR260B 件数 読込=" WK-READ-CNT
                   " 出力=" WK-OUT-CNT
                   " 除外=" WK-SKIP-CNT
      *
           IF HARD-ERR
              MOVE 8 TO RETURN-CODE
              DISPLAY "LR260B 異常終了"
           ELSE
              MOVE 0 TO RETURN-CODE
              DISPLAY "LR260B 正常終了"
           END-IF.
