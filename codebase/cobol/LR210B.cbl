       IDENTIFICATION DIVISION.
       PROGRAM-ID. LR210B.
      *
      * 変更履歴
      * 版数 | 年月日    | 担当   | 概要
      * ----+----------+------+----------------------------
      * 1.0 | 20190801 | 帳票システム課 | 初版：保険料明細帳票作成
      *
      * 用途: LFPRMF/LFPOLF/LFCNTF から契約者向け保
      *       険料明細帳票を作成し LRRPTF へ出力。
      *       契約無効や満期済み除外。
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LFPRMF ASSIGN TO LS-LFPRMF-PATH
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS LS-LFPRMF-ST.
           SELECT LFPOLF ASSIGN TO LS-LFPOLF-PATH
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS LS-LFPOLF-ST.
           SELECT LFCNTF ASSIGN TO LS-LFCNTF-PATH
               ORGANIZATION IS INDEXED
               ACCESS IS RANDOM
               RECORD KEY IS CN-POL-NO
               FILE STATUS IS LS-LFCNTF-ST.
           SELECT LRRPTF ASSIGN TO LS-LRRPTF-PATH
               ORGANIZATION IS INDEXED
               ACCESS IS SEQUENTIAL
               RECORD KEY IS RP-REPORT-ID
               FILE STATUS IS LS-LRRPTF-ST.

       DATA DIVISION.
       FILE SECTION.
       FD  LFPRMF.
       COPY LFPRMFC.

       FD  LFPOLF.
       COPY LFPOLFC.

       FD  LFCNTF.
       COPY LFCNTFC.

       FD  LRRPTF.
       COPY LRRPTFC.

       WORKING-STORAGE SECTION.
       01  LS-FILE-PATHS.
           05  LS-LFPRMF-PATH         PIC X(256).
           05  LS-LFPOLF-PATH         PIC X(256).
           05  LS-LFCNTF-PATH         PIC X(256).
           05  LS-LRRPTF-PATH         PIC X(256).

       01  LS-FILE-STATUS.
           05  LS-LFPRMF-ST           PIC XX.
           05  LS-LFPOLF-ST           PIC XX.
           05  LS-LFCNTF-ST           PIC XX.
           05  LS-LRRPTF-ST           PIC XX.

       01  LS-PROCESS-CONTROL.
           05  LS-REPORT-ID           PIC 9(10) VALUE 100000000.
           05  LS-REPORT-YM           PIC 9(6).
           05  LS-REPORT-TYPE         PIC X VALUE "1".
           05  LS-LINE-NO             PIC 9(5).
           05  LS-EOF-LFPRMF          PIC X VALUE "N".
           05  LS-EOF-LFPOLF          PIC X VALUE "N".
           05  LS-POLF-FOUND          PIC X VALUE "N".

       01  LS-WORK-FIELDS.
           05  LS-ENTRY-AGE           PIC 9(3).
           05  LS-CALC-BAND           PIC XX.
           05  LS-MATURITY-YM         PIC 9(6).
           05  LS-CURRENT-YM          PIC 9(6).
           05  LS-CUR-DATE            PIC 9(8).
           05  LS-TEMP-POL-NO         PIC X(10).

       01  LS-COUNTERS.
           05  LS-REC-READ            PIC 9(10).
           05  LS-REC-OUTPUT          PIC 9(10).
           05  LS-REC-ERROR           PIC 9(10).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM ACCEPT-ENVIRONMENT.
           PERFORM OPEN-ALL-FILES.
           IF RETURN-CODE NOT = 0
               MOVE 8 TO RETURN-CODE
               DISPLAY "ファイルオープン失敗 RC=" 
                   RETURN-CODE
               GOBACK
           END-IF.

           PERFORM EXTRACT-CURRENT-DATE.
           PERFORM PROCESS-PREMIUM-RECORDS.
           PERFORM CLOSE-ALL-FILES.

           IF LS-REC-ERROR > 0
               MOVE 8 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.
           GOBACK.

       ACCEPT-ENVIRONMENT.
           ACCEPT LS-LFPRMF-PATH FROM ENVIRONMENT 
               "LFPRMF_PATH".
           ACCEPT LS-LFPOLF-PATH FROM ENVIRONMENT 
               "LFPOLF_PATH".
           ACCEPT LS-LFCNTF-PATH FROM ENVIRONMENT 
               "LFCNTF_PATH".
           ACCEPT LS-LRRPTF-PATH FROM ENVIRONMENT 
               "LRRPTF_PATH".
           ACCEPT LS-CUR-DATE FROM DATE YYYYMMDD.

       EXTRACT-CURRENT-DATE.
           DIVIDE LS-CUR-DATE BY 100 GIVING LS-CURRENT-YM.
           MOVE LS-CURRENT-YM TO LS-REPORT-YM.

       OPEN-ALL-FILES.
           OPEN INPUT LFPRMF.
           IF LS-LFPRMF-ST NOT = "00"
               DISPLAY "LFPRMF オープン失敗 ST=" 
                   LS-LFPRMF-ST
               MOVE 1 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF.

           OPEN INPUT LFPOLF.
           IF LS-LFPOLF-ST NOT = "00"
               DISPLAY "LFPOLF オープン失敗 ST=" 
                   LS-LFPOLF-ST
               MOVE 1 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF.

           OPEN INPUT LFCNTF.
           IF LS-LFCNTF-ST NOT = "00"
               DISPLAY "LFCNTF オープン失敗 ST=" 
                   LS-LFCNTF-ST
               MOVE 1 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF.

           OPEN OUTPUT LRRPTF.
           IF LS-LRRPTF-ST NOT = "00"
               DISPLAY "LRRPTF オープン失敗 ST=" 
                   LS-LRRPTF-ST
               MOVE 1 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF.

       PROCESS-PREMIUM-RECORDS.
           MOVE "N" TO LS-EOF-LFPRMF.
           PERFORM UNTIL LS-EOF-LFPRMF = "Y"
               READ LFPRMF
                   AT END
                       MOVE "Y" TO LS-EOF-LFPRMF
                   NOT AT END
                       ADD 1 TO LS-REC-READ
                       MOVE PR-POL-NO TO LS-TEMP-POL-NO
                       PERFORM VALIDATE-AND-OUTPUT-RECORD
               END-READ
           END-PERFORM.

       VALIDATE-AND-OUTPUT-RECORD.
           PERFORM FIND-POLICY-STATUS.
           IF LS-POLF-FOUND NOT = "Y"
               ADD 1 TO LS-REC-ERROR
               DISPLAY "契約状態取得失敗 POL=" 
                   LS-TEMP-POL-NO
               EXIT PARAGRAPH
           END-IF.

           IF PO-POL-STATUS-KBN NOT = "01"
               EXIT PARAGRAPH
           END-IF.

           PERFORM CHECK-MATURITY-DATE.
           IF RETURN-CODE NOT = 0
               EXIT PARAGRAPH
           END-IF.

           PERFORM CALCULATE-AGE-BAND.
           PERFORM BUILD-AND-WRITE-OUTPUT.

       FIND-POLICY-STATUS.
           MOVE "N" TO LS-POLF-FOUND.
           CLOSE LFPOLF.
           OPEN INPUT LFPOLF.
           MOVE "N" TO LS-EOF-LFPOLF.
           PERFORM UNTIL LS-EOF-LFPOLF = "Y" 
               OR LS-POLF-FOUND = "Y"
               READ LFPOLF
                   AT END
                       MOVE "Y" TO LS-EOF-LFPOLF
                   NOT AT END
                       IF PO-POL-NO = LS-TEMP-POL-NO
                           MOVE "Y" TO LS-POLF-FOUND
                       END-IF
               END-READ
           END-PERFORM.

       CHECK-MATURITY-DATE.
           MOVE LS-TEMP-POL-NO TO CN-POL-NO.
           READ LFCNTF
               AT END
                   MOVE 1 TO RETURN-CODE
                   DISPLAY "契約レコード取得失敗 POL=" 
                       LS-TEMP-POL-NO
                   EXIT PARAGRAPH
               NOT AT END
                   CONTINUE
           END-READ.

           IF CN-MATURITY-DATE = ZEROS
               MOVE 0 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF.

           MOVE CN-MATURITY-DATE TO LS-MATURITY-YM.
           DIVIDE LS-MATURITY-YM BY 100 
               GIVING LS-MATURITY-YM.

           IF LS-MATURITY-YM < LS-CURRENT-YM
               MOVE 1 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       CALCULATE-AGE-BAND.
           MOVE PO-ENTRY-AGE-CNT TO LS-ENTRY-AGE.
           EVALUATE TRUE
               WHEN LS-ENTRY-AGE <= 29
                   MOVE "A1" TO LS-CALC-BAND
               WHEN LS-ENTRY-AGE <= 39
                   MOVE "A2" TO LS-CALC-BAND
               WHEN LS-ENTRY-AGE <= 49
                   MOVE "A3" TO LS-CALC-BAND
               WHEN LS-ENTRY-AGE <= 59
                   MOVE "A4" TO LS-CALC-BAND
               WHEN OTHER
                   MOVE "A5" TO LS-CALC-BAND
           END-EVALUATE.

       BUILD-AND-WRITE-OUTPUT.
           ADD 1 TO LS-REPORT-ID.
           MOVE LS-REPORT-ID TO RP-REPORT-ID.
           MOVE LS-REPORT-YM TO RP-REPORT-YM.
           MOVE LS-REPORT-TYPE TO RP-REPORT-TYPE-KBN.
           MOVE LS-TEMP-POL-NO TO RP-POL-NO.
           ADD 1 TO LS-LINE-NO.
           MOVE LS-LINE-NO TO RP-LINE-NO.
           MOVE PR-PRM-AMT TO RP-PRINT-AMT.
           MOVE PR-CALC-STATUS-KBN TO RP-OUTPUT-STATUS-KBN.

           WRITE LRRPTF-REC.
           IF LS-LRRPTF-ST NOT = "00"
               ADD 1 TO LS-REC-ERROR
               DISPLAY "帳票出力失敗 POL=" 
                   LS-TEMP-POL-NO
                   " ST=" LS-LRRPTF-ST
           ELSE
               ADD 1 TO LS-REC-OUTPUT
           END-IF.

       CLOSE-ALL-FILES.
           CLOSE LFPRMF.
           IF LS-LFPRMF-ST NOT = "00"
               DISPLAY "LFPRMF クローズ失敗 ST=" 
                   LS-LFPRMF-ST
           END-IF.

           CLOSE LFPOLF.
           IF LS-LFPOLF-ST NOT = "00"
               DISPLAY "LFPOLF クローズ失敗 ST=" 
                   LS-LFPOLF-ST
           END-IF.

           CLOSE LFCNTF.
           IF LS-LFCNTF-ST NOT = "00"
               DISPLAY "LFCNTF クローズ失敗 ST=" 
                   LS-LFCNTF-ST
           END-IF.

           CLOSE LRRPTF.
           IF LS-LRRPTF-ST NOT = "00"
               DISPLAY "LRRPTF クローズ失敗 ST=" 
                   LS-LRRPTF-ST
           END-IF.
