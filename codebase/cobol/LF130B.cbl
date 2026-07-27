       IDENTIFICATION DIVISION.
       PROGRAM-ID. LF130B.

      * ==============================================================
      * LF130B - 解約受付取込バッチ
      * 営業店受付データを契約マスタに照会し、契約番号存在、
      * 契約状態、受付日妥当性を検査して解約受付レコードを
      * LFREQFへ登録する。無効契約や重複受付は受付状態区分を
      * エラーにして後続の返戻金計算投入から除外する。
      * ==============================================================
      * 変更履歴
      * 版数  年月日    担当      概要
      * -----  --------  --------  --------------------------------
      * 1.0    20200101  初版      初版作成
      * 1.1    20220625  保守      ロジック実装
      * ==============================================================

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RCVDATA-FILE ASSIGN TO EXTERNAL RCVDATA
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS RCVDATA-FILE-STATUS.
           SELECT LFPOLF2 ASSIGN TO EXTERNAL LFPOLF2
               ORGANIZATION IS INDEXED
               RECORD KEY IS PO-POL-NO
               FILE STATUS IS LFPOLF2-FILE-STATUS.
           SELECT LFREQF ASSIGN TO EXTERNAL LFREQF
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS LFREQF-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  RCVDATA-FILE.
       01  RCVDATA-REC.
           05  RCV-POL-NO PIC X(10).
           05  RCV-REQ-DATE PIC 9(8).
           05  RCV-OP-ID PIC X(5).
           05  RCV-BRANCH-CD PIC X(3).

       FD  LFPOLF2.
       COPY LFPOLF2C.

       FD  LFREQF.
       COPY LFREQC.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05  RCVDATA-FILE-STATUS PIC XX VALUE '00'.
           05  LFPOLF2-FILE-STATUS PIC XX VALUE '00'.
           05  LFREQF-FILE-STATUS PIC XX VALUE '00'.

       01  WS-CONSTANTS.
           05  WS-STATUS-INVALID PIC X(2) VALUE '99'.
           05  WS-STATUS-CLOSED PIC X(2) VALUE '03'.
           05  WS-STATUS-NORMAL PIC X(2) VALUE '01'.
           05  WS-REQ-TYPE-CANCEL PIC X(2) VALUE '02'.
           05  WS-RCV-EMPTY PIC X(10) VALUE SPACES.
           05  WS-TODAY-MIN PIC 9(8) VALUE 19000101.
           05  WS-TODAY-MAX PIC 9(8) VALUE 20991231.

       01  WS-COUNTERS.
           05  WS-REC-READ PIC 9(9) VALUE 0.
           05  WS-REC-VALID PIC 9(9) VALUE 0.
           05  WS-REC-ERROR PIC 9(9) VALUE 0.
           05  WS-REQ-ID-SEQ PIC 9(9) VALUE 0.

       01  WS-CONTROL.
           05  WS-EOF-FLAG PIC X VALUE 'N'.
           05  WS-ERR-FLAG PIC X VALUE 'N'.
           05  WS-SYS-DATE PIC 9(8).
           05  WS-FOUND-FLAG PIC X VALUE 'N'.

       01  WS-WORK-FIELDS.
           05  WS-TEMP-DATE PIC 9(8).
           05  WS-ISSUE-DATE-NUM PIC 9(8).
           05  WS-RCV-DATE-NUM PIC 9(8).

       01  WS-REQUEST-DATA.
           05  WS-WK-REQ-ID PIC 9(9).
           05  WS-WK-POL-NO PIC X(10).
           05  WS-WK-REQ-DATE PIC 9(8).
           05  WS-WK-REQ-TYPE PIC X(2).
           05  WS-WK-REQ-STATUS PIC X(2).
           05  WS-WK-OP-ID PIC X(5).
           05  WS-WK-BRANCH PIC X(3).

       PROCEDURE DIVISION.
       MAIN-PROC.
           MOVE 0 TO RETURN-CODE.
           MOVE FUNCTION CURRENT-DATE(1:8) TO WS-SYS-DATE.

           PERFORM OPEN-FILES.
           IF RETURN-CODE NOT = 0
               MOVE 8 TO RETURN-CODE
               GOBACK
           END-IF.

           PERFORM MAIN-LOOP UNTIL WS-EOF-FLAG = 'Y'.

           PERFORM CLOSE-FILES.
           PERFORM WRITE-SUMMARY.

           IF WS-ERR-FLAG = 'Y'
               MOVE 8 TO RETURN-CODE
           ELSE
               IF WS-REC-ERROR > 0
                   MOVE 4 TO RETURN-CODE
               ELSE
                   MOVE 0 TO RETURN-CODE
               END-IF
           END-IF.

           GOBACK.

       OPEN-FILES.
           OPEN INPUT RCVDATA-FILE.
           IF RCVDATA-FILE-STATUS NOT = '00'
               DISPLAY '入力ファイルOPEN失敗 ST='
                   RCVDATA-FILE-STATUS
               MOVE 'Y' TO WS-ERR-FLAG
               EXIT PARAGRAPH
           END-IF.

           OPEN INPUT LFPOLF2.
           IF LFPOLF2-FILE-STATUS NOT = '00'
               DISPLAY 'LFPOLF2 OPEN失敗 ST='
                   LFPOLF2-FILE-STATUS
               MOVE 'Y' TO WS-ERR-FLAG
               EXIT PARAGRAPH
           END-IF.

           OPEN OUTPUT LFREQF.
           IF LFREQF-FILE-STATUS NOT = '00'
               DISPLAY 'LFREQF OPEN失敗 ST='
                   LFREQF-FILE-STATUS
               MOVE 'Y' TO WS-ERR-FLAG
               EXIT PARAGRAPH
           END-IF.

       MAIN-LOOP.
           READ RCVDATA-FILE
               AT END MOVE 'Y' TO WS-EOF-FLAG
               NOT AT END
                   PERFORM PROCESS-RECORD
           END-READ.

           IF RCVDATA-FILE-STATUS NOT = '00' AND '10'
               DISPLAY '入力READ エラー ST='
                   RCVDATA-FILE-STATUS
               MOVE 'Y' TO WS-ERR-FLAG
               MOVE 'Y' TO WS-EOF-FLAG
           END-IF.

       PROCESS-RECORD.
           ADD 1 TO WS-REC-READ.
           MOVE 'N' TO WS-FOUND-FLAG.
           MOVE '01' TO WS-WK-REQ-STATUS.

           PERFORM LOOKUP-CONTRACT.

           PERFORM BUILD-REQUEST-RECORD.

       LOOKUP-CONTRACT.
           IF RCV-POL-NO = WS-RCV-EMPTY
               MOVE '06' TO WS-WK-REQ-STATUS
               ADD 1 TO WS-REC-ERROR
               EXIT PARAGRAPH
           END-IF.

           MOVE RCV-POL-NO TO PO-POL-NO.
           READ LFPOLF2 KEY IS PO-POL-NO.

           IF LFPOLF2-FILE-STATUS NOT = '00'
               MOVE '02' TO WS-WK-REQ-STATUS
               ADD 1 TO WS-REC-ERROR
               EXIT PARAGRAPH
           END-IF.

           MOVE 'Y' TO WS-FOUND-FLAG.

           IF PO-CONTRACT-STATUS-KBN = WS-STATUS-INVALID
               MOVE '03' TO WS-WK-REQ-STATUS
               ADD 1 TO WS-REC-ERROR
               EXIT PARAGRAPH
           END-IF.

           IF PO-CONTRACT-STATUS-KBN = WS-STATUS-CLOSED
               MOVE '04' TO WS-WK-REQ-STATUS
               ADD 1 TO WS-REC-ERROR
               EXIT PARAGRAPH
           END-IF.

           IF PO-CONTRACT-STATUS-KBN NOT = WS-STATUS-NORMAL
               MOVE '05' TO WS-WK-REQ-STATUS
               ADD 1 TO WS-REC-ERROR
               EXIT PARAGRAPH
           END-IF.

           PERFORM VALIDATE-RECEIPT-DATE.

       VALIDATE-RECEIPT-DATE.
           MOVE RCV-REQ-DATE TO WS-RCV-DATE-NUM.
           MOVE PO-ISSUE-DATE TO WS-ISSUE-DATE-NUM.

           IF WS-RCV-DATE-NUM < WS-TODAY-MIN
               MOVE '07' TO WS-WK-REQ-STATUS
               ADD 1 TO WS-REC-ERROR
               EXIT PARAGRAPH
           END-IF.

           IF WS-RCV-DATE-NUM > WS-TODAY-MAX
               MOVE '07' TO WS-WK-REQ-STATUS
               ADD 1 TO WS-REC-ERROR
               EXIT PARAGRAPH
           END-IF.

           IF WS-RCV-DATE-NUM < WS-ISSUE-DATE-NUM
               MOVE '08' TO WS-WK-REQ-STATUS
               ADD 1 TO WS-REC-ERROR
               EXIT PARAGRAPH
           END-IF.

           IF WS-RCV-DATE-NUM > WS-SYS-DATE
               MOVE '08' TO WS-WK-REQ-STATUS
               ADD 1 TO WS-REC-ERROR
               EXIT PARAGRAPH
           END-IF.

           MOVE '00' TO WS-WK-REQ-STATUS.
           ADD 1 TO WS-REC-VALID.

       BUILD-REQUEST-RECORD.
           ADD 1 TO WS-REQ-ID-SEQ.
           MOVE WS-REQ-ID-SEQ TO RQ-REQ-ID.
           MOVE RCV-POL-NO TO RQ-POL-NO.
           MOVE RCV-REQ-DATE TO RQ-REQ-DATE.
           MOVE WS-REQ-TYPE-CANCEL TO RQ-REQ-TYPE-KBN.
           MOVE WS-WK-REQ-STATUS TO RQ-REQ-STATUS-KBN.
           MOVE RCV-OP-ID TO RQ-OPERATOR-ID.
           MOVE RCV-BRANCH-CD TO RQ-RECEIPT-BRANCH-CD.

           WRITE LFREQF-REC.

           IF LFREQF-FILE-STATUS NOT = '00'
               DISPLAY 'LFREQF WRITE失敗 ST='
                   LFREQF-FILE-STATUS
               MOVE 'Y' TO WS-ERR-FLAG
               MOVE 'Y' TO WS-EOF-FLAG
           END-IF.

       CLOSE-FILES.
           CLOSE RCVDATA-FILE.
           CLOSE LFPOLF2.
           CLOSE LFREQF.

       WRITE-SUMMARY.
           DISPLAY '============================================'.
           DISPLAY '解約受付取込バッチ処理完了'.
           DISPLAY '読込件数: ' WS-REC-READ.
           DISPLAY '正常件数: ' WS-REC-VALID.
           DISPLAY 'エラー件数: ' WS-REC-ERROR.
           DISPLAY '============================================'.
