       IDENTIFICATION DIVISION.
       PROGRAM-ID. LF150B.
      *
      * 失効契約返戻確認バッチ
      * 保険料未納により失効候補となった契約を抽出し、
      * 返戻金計算対象にすべき契約だけを失効受付としてLFREQFへ起票する。
      * 契約状態区分と払込到達日を検査し、計算金額はLF210B入力項目の
      * 整合性確認までに限定する。
      *
      * 版数  年月日    担当      概要
      * V1.0  20200101  SHIMIZU   新規作成
      * V1.1  20220615  TANAKA    返戻対象判定ロジック追加
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LFPOLF2 ASSIGN TO LFPOLF2F
               ORGANIZATION IS INDEXED
               RECORD KEY IS PO-POL-NO
               FILE STATUS IS W-FD-STATUS-POL.
           SELECT LFCVPF ASSIGN TO LFCVPFF
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS W-FD-STATUS-CV.
           SELECT LFREQF ASSIGN TO LFREQFF
               ORGANIZATION IS INDEXED
               RECORD KEY IS RQ-REQ-ID
               FILE STATUS IS W-FD-STATUS-REQ.
       DATA DIVISION.
       FILE SECTION.
       FD  LFPOLF2.
       COPY LFPOLF2C.
       FD  LFCVPF.
       COPY LFCVPFC.
       FD  LFREQF.
       COPY LFREQC.
       WORKING-STORAGE SECTION.
      * ファイル制御領域
       01  W-FD-STATUS-POL            PIC XX VALUE SPACES.
       01  W-FD-STATUS-CV             PIC XX VALUE SPACES.
       01  W-FD-STATUS-REQ            PIC XX VALUE SPACES.
      * 処理制御フラグ
       01  W-EOF-LFPOLF2              PIC X VALUE 'N'.
       01  W-EOF-LFCVPF               PIC X VALUE 'N'.
       01  W-CV-FOUND                 PIC X VALUE 'N'.
      * 統計・カウンタ
       01  W-POL-READ-CNT             PIC 9(8) VALUE 0.
       01  W-CV-READ-CNT              PIC 9(8) VALUE 0.
       01  W-REQ-WRITE-CNT            PIC 9(8) VALUE 0.
       01  W-ERROR-CNT                PIC 9(8) VALUE 0.
      * 日付・時刻領域
       01  W-CURRENT-DATE             PIC 9(8) VALUE 0.
       01  W-CURRENT-DATE-STR.
           05  W-CURRENT-YEAR         PIC 9(4).
           05  W-CURRENT-MONTH        PIC 9(2).
           05  W-CURRENT-DAY          PIC 9(2).
      * リクエスト採番領域
       01  W-REQ-ID-SERIAL            PIC 9(10) VALUE 1000000001.
      * 検証用ワーク領域
       01  W-POL-STATUS-VALID         PIC X VALUE 'N'.
       01  W-DAYS-FROM-PAID           PIC S9(5) COMP-3 VALUE 0.
       01  W-PAID-TO-DATE-NUM         PIC 9(8) VALUE 0.
       01  W-CALC-DATE-DIFF           PIC 9(8) VALUE 0.
       01  W-MONTH-DIFF               PIC 9(3) VALUE 0.
       01  W-TEMP-MONTH               PIC 9(2) VALUE 0.
       01  W-GRACE-PERIOD-DAYS        PIC 9(3) VALUE 90.
       01  W-RETURN-CODE-WORK         PIC 9 VALUE 0.
      * 履歴テーブル（CV複合キー検索用）
       01  W-CV-TABLE.
           05  W-CV-TABLE-ENTRY OCCURS 999 TIMES.
               10  W-CV-POL-NO        PIC X(12).
               10  W-CV-STATUS        PIC X(2).
               10  W-CV-RESERVE       PIC 9(13)V99.
       01  W-CV-TABLE-IDX             PIC 9(4) VALUE 0.
       01  W-CV-TABLE-MAX             PIC 9(4) VALUE 0.
       01  W-SEARCH-IDX               PIC 9(4) VALUE 0.
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           PERFORM INITIALIZE-PROCESS.
           PERFORM OPEN-ALL-FILES.
           IF W-RETURN-CODE-WORK NOT = 0
               DISPLAY '失効確認：ファイルオープン失敗'
               MOVE 8 TO RETURN-CODE
               GOBACK
           END-IF.
           PERFORM LOAD-CV-TABLE.
           IF W-RETURN-CODE-WORK NOT = 0
               DISPLAY '失効確認：CV テーブル読込失敗'
               PERFORM CLOSE-ALL-FILES
               MOVE 8 TO RETURN-CODE
               GOBACK
           END-IF.
           PERFORM PROCESS-POL-RECORDS.
           PERFORM CLOSE-ALL-FILES.
           PERFORM WRITE-COMPLETION-LOG.
           MOVE 0 TO RETURN-CODE.
           GOBACK.
       INITIALIZE-PROCESS.
           MOVE 0 TO W-RETURN-CODE-WORK.
           MOVE 0 TO W-POL-READ-CNT.
           MOVE 0 TO W-CV-READ-CNT.
           MOVE 0 TO W-REQ-WRITE-CNT.
           MOVE 0 TO W-ERROR-CNT.
           MOVE 0 TO W-CV-TABLE-MAX.
           MOVE FUNCTION CURRENT-DATE(1:8) 
               TO W-CURRENT-DATE.
       OPEN-ALL-FILES.
           OPEN INPUT LFPOLF2.
           IF W-FD-STATUS-POL NOT = '00'
               DISPLAY '失効確認：LFPOLF2 オープン失敗 ST=' 
                   W-FD-STATUS-POL
               MOVE 1 TO W-RETURN-CODE-WORK
               EXIT PARAGRAPH
           END-IF.
           OPEN INPUT LFCVPF.
           IF W-FD-STATUS-CV NOT = '00'
               DISPLAY '失効確認：LFCVPF オープン失敗 ST=' 
                   W-FD-STATUS-CV
               CLOSE LFPOLF2
               MOVE 1 TO W-RETURN-CODE-WORK
               EXIT PARAGRAPH
           END-IF.
           OPEN OUTPUT LFREQF.
           IF W-FD-STATUS-REQ NOT = '00'
               DISPLAY '失効確認：LFREQF オープン失敗 ST=' 
                   W-FD-STATUS-REQ
               CLOSE LFPOLF2
               CLOSE LFCVPF
               MOVE 1 TO W-RETURN-CODE-WORK
               EXIT PARAGRAPH
           END-IF.
       LOAD-CV-TABLE.
           MOVE 0 TO W-RETURN-CODE-WORK.
           MOVE 0 TO W-CV-READ-CNT.
           MOVE 0 TO W-CV-TABLE-IDX.
           PERFORM UNTIL 1 = 0
               READ LFCVPF INTO LFCVPF-REC
                   AT END
                       MOVE 'Y' TO W-EOF-LFCVPF
                   NOT AT END
                       ADD 1 TO W-CV-READ-CNT
                       IF W-CV-TABLE-IDX < 999
                           ADD 1 TO W-CV-TABLE-IDX
                           MOVE CI-POL-NO TO 
                               W-CV-POL-NO(W-CV-TABLE-IDX)
                           MOVE CI-CV-STATUS-KBN TO 
                               W-CV-STATUS(W-CV-TABLE-IDX)
                           MOVE CI-RESERVE-AMT TO 
                               W-CV-RESERVE(W-CV-TABLE-IDX)
                       END-IF
               END-READ
               IF W-EOF-LFCVPF = 'Y'
                   MOVE W-CV-TABLE-IDX TO W-CV-TABLE-MAX
                   EXIT PERFORM
               END-IF
           END-PERFORM.
           MOVE 'N' TO W-EOF-LFCVPF.
       PROCESS-POL-RECORDS.
           PERFORM UNTIL 1 = 0
               READ LFPOLF2 INTO LFPOLF2-REC
                   AT END
                       MOVE 'Y' TO W-EOF-LFPOLF2
                   NOT AT END
                       ADD 1 TO W-POL-READ-CNT
                       PERFORM EVALUATE-POLICY-FOR-RETURN
               END-READ
               IF W-EOF-LFPOLF2 = 'Y'
                   EXIT PERFORM
               END-IF
           END-PERFORM.
       EVALUATE-POLICY-FOR-RETURN.
           PERFORM LOOKUP-CV-STATUS.
           IF W-CV-FOUND NOT = 'Y'
               EXIT PARAGRAPH
           END-IF.
           PERFORM VALIDATE-CONTRACT-ELIGIBILITY.
           IF W-POL-STATUS-VALID NOT = 'Y'
               EXIT PARAGRAPH
           END-IF.
           PERFORM CREATE-REQUEST-RECORD.
       LOOKUP-CV-STATUS.
           MOVE 'N' TO W-CV-FOUND.
           MOVE 0 TO W-SEARCH-IDX.
           PERFORM VARYING W-SEARCH-IDX FROM 1 BY 1
               UNTIL W-SEARCH-IDX > W-CV-TABLE-MAX
               IF W-CV-POL-NO(W-SEARCH-IDX) = PO-POL-NO
                   MOVE 'Y' TO W-CV-FOUND
                   EXIT PERFORM
               END-IF
           END-PERFORM.
       VALIDATE-CONTRACT-ELIGIBILITY.
           MOVE 'N' TO W-POL-STATUS-VALID.
           IF W-CV-FOUND NOT = 'Y'
               EXIT PARAGRAPH
           END-IF.
           IF W-CV-STATUS(W-SEARCH-IDX) NOT = '01'
               EXIT PARAGRAPH
           END-IF.
           MOVE FUNCTION NUMVAL(PO-PAID-TO-DATE)
               TO W-PAID-TO-DATE-NUM.
           IF W-PAID-TO-DATE-NUM = 0 OR 
               W-PAID-TO-DATE-NUM > W-CURRENT-DATE
               EXIT PARAGRAPH
           END-IF.
           COMPUTE W-CALC-DATE-DIFF = 
               W-CURRENT-DATE - W-PAID-TO-DATE-NUM.
           DIVIDE W-CALC-DATE-DIFF BY 100
               GIVING W-MONTH-DIFF
               REMAINDER W-TEMP-MONTH.
           IF W-MONTH-DIFF < 3
               EXIT PARAGRAPH
           END-IF.
           MOVE 'Y' TO W-POL-STATUS-VALID.
       CREATE-REQUEST-RECORD.
           MOVE W-REQ-ID-SERIAL TO RQ-REQ-ID.
           ADD 1 TO W-REQ-ID-SERIAL.
           MOVE PO-POL-NO TO RQ-POL-NO.
           MOVE W-CURRENT-DATE TO RQ-REQ-DATE.
           MOVE '01' TO RQ-REQ-TYPE-KBN.
           MOVE '01' TO RQ-REQ-STATUS-KBN.
           MOVE '9999' TO RQ-OPERATOR-ID.
           MOVE '0100' TO RQ-RECEIPT-BRANCH-CD.
           WRITE LFREQF-REC.
           IF W-FD-STATUS-REQ = '00'
               ADD 1 TO W-REQ-WRITE-CNT
           ELSE
               ADD 1 TO W-ERROR-CNT
               DISPLAY '失効確認：LFREQF 書込エラー ST=' 
                   W-FD-STATUS-REQ ' POL=' PO-POL-NO
           END-IF.
       CLOSE-ALL-FILES.
           CLOSE LFPOLF2.
           CLOSE LFCVPF.
           CLOSE LFREQF.
       WRITE-COMPLETION-LOG.
           DISPLAY '失効確認バッチ 処理完了'.
           DISPLAY '処理日付 : ' W-CURRENT-DATE.
           DISPLAY '契約読込件数 : ' W-POL-READ-CNT.
           DISPLAY 'CV読込件数 : ' W-CV-READ-CNT.
           DISPLAY 'リクエスト作成件数 : ' W-REQ-WRITE-CNT.
           DISPLAY 'エラー件数 : ' W-ERROR-CNT.
