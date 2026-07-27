       IDENTIFICATION DIVISION.
       PROGRAM-ID. LF140B.
      *
      * 変更履歴
      * 版数  年月日      担当        概要
      * ---  --------  --------  ---------------------
      *  1.0  20210601  システム部  初版作成
      *  1.1  20230620  システム部  解約返戻金計算入力作成
      *
      * 解約計算投入ファイル作成バッチ
      * 承認済み解約受付を抽出し、契約マスタと責任準備金を
      * 結合してLF210B向けのLFCVPF入力を作成する。
      * 経過月数はLF290Sで取得し、未償却新契約費は
      * 既存入力値を引き継ぐ。
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LFREQF ASSIGN TO LS-LFREQF-NAME
               ORGANIZATION IS INDEXED
               RECORD KEY IS RQ-REQ-ID
               FILE STATUS IS FS-LFREQF.
               
           SELECT LFPOLF2 ASSIGN TO LS-LFPOLF2-NAME
               ORGANIZATION IS INDEXED
               RECORD KEY IS PO-POL-NO
               FILE STATUS IS FS-LFPOLF2.
               
           SELECT LFRSVF ASSIGN TO LS-LFRSVF-NAME
               ORGANIZATION IS INDEXED
               RECORD KEY IS RS-POL-NO
               FILE STATUS IS FS-LFRSVF.
               
           SELECT LFCVPF ASSIGN TO LS-LFCVPF-NAME
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-LFCVPF.
       
       DATA DIVISION.
       FILE SECTION.
       
       FD  LFREQF.
           COPY LFREQC.
       
       FD  LFPOLF2.
           COPY LFPOLF2C.
       
       FD  LFRSVF.
           COPY LFRSVC.
       
       FD  LFCVPF.
           COPY LFCVPFC.
       
       WORKING-STORAGE SECTION.
       01  LS-LFREQF-NAME      PIC X(64) VALUE
           '/data/vsam/LFREQF'.
       01  LS-LFPOLF2-NAME     PIC X(64) VALUE
           '/data/vsam/LFPOLF2'.
       01  LS-LFRSVF-NAME      PIC X(64) VALUE
           '/data/vsam/LFRSVF'.
       01  LS-LFCVPF-NAME      PIC X(64) VALUE
           '/data/seq/LFCVPF'.
       
       01  FS-LFREQF           PIC XX VALUE SPACES.
       01  FS-LFPOLF2          PIC XX VALUE SPACES.
       01  FS-LFRSVF           PIC XX VALUE SPACES.
       01  FS-LFCVPF           PIC XX VALUE SPACES.
       
       01  WS-EOF-FLG          PIC X VALUE 'N'.
       01  WS-FOUND-FLG        PIC X VALUE 'N'.
       01  WS-ERROR-FLG        PIC X VALUE 'N'.
       
       01  WS-COUNTERS.
           05  WS-READ-CNT     PIC 9(9) VALUE 0.
           05  WS-VALID-CNT    PIC 9(9) VALUE 0.
           05  WS-ERROR-CNT    PIC 9(9) VALUE 0.
           05  WS-WRITE-CNT    PIC 9(9) VALUE 0.
       
       01  WS-WORK-FIELDS.
           05  WS-ELAPSED-MON  PIC S9(3) VALUE 0.
           05  WS-CURRENT-DATE-NUM PIC 9(8) VALUE 0.
           05  WS-ISSUE-DATE-NUM   PIC 9(8) VALUE 0.
           05  WS-DATE-DIFF    PIC 9(8) VALUE 0.
       
       PROCEDURE DIVISION.
       
       000-MAIN-PROC.
           PERFORM 100-OPEN-FILES.
           IF WS-ERROR-FLG = 'Y'
               MOVE 8 TO RETURN-CODE
               GOBACK
           END-IF.
           
           PERFORM 200-READ-AND-PROCESS
               UNTIL WS-EOF-FLG = 'Y'.
           
           PERFORM 300-CLOSE-FILES.
           
           IF WS-ERROR-FLG = 'Y'
               MOVE 8 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.
           
           GOBACK.
       
       100-OPEN-FILES.
           OPEN INPUT LFREQF.
           IF FS-LFREQF NOT = '00'
               DISPLAY 'LFREQF オープン失敗 ST=' FS-LFREQF
               MOVE 'Y' TO WS-ERROR-FLG
               GOBACK
           END-IF.
           
           OPEN INPUT LFPOLF2.
           IF FS-LFPOLF2 NOT = '00'
               DISPLAY 'LFPOLF2 オープン失敗 ST=' FS-LFPOLF2
               MOVE 'Y' TO WS-ERROR-FLG
               CLOSE LFREQF
               GOBACK
           END-IF.
           
           OPEN INPUT LFRSVF.
           IF FS-LFRSVF NOT = '00'
               DISPLAY 'LFRSVF オープン失敗 ST=' FS-LFRSVF
               MOVE 'Y' TO WS-ERROR-FLG
               CLOSE LFREQF
               CLOSE LFPOLF2
               GOBACK
           END-IF.
           
           OPEN OUTPUT LFCVPF.
           IF FS-LFCVPF NOT = '00'
               DISPLAY 'LFCVPF オープン失敗 ST=' FS-LFCVPF
               MOVE 'Y' TO WS-ERROR-FLG
               CLOSE LFREQF
               CLOSE LFPOLF2
               CLOSE LFRSVF
               GOBACK
           END-IF.
       
       200-READ-AND-PROCESS.
           READ LFREQF INTO LFREQF-REC
               AT END
                   MOVE 'Y' TO WS-EOF-FLG
               NOT AT END
                   ADD 1 TO WS-READ-CNT
                   PERFORM 210-VALIDATE-REQUEST
                   IF WS-ERROR-FLG NOT = 'Y'
                       PERFORM 220-LOOKUP-CONTRACT
                   END-IF
                   IF WS-ERROR-FLG NOT = 'Y'
                       PERFORM 230-LOOKUP-RESERVE
                   END-IF
                   IF WS-ERROR-FLG NOT = 'Y'
                       PERFORM 240-GET-ELAPSED-MONTHS
                       PERFORM 250-BUILD-OUTPUT
                       PERFORM 260-WRITE-OUTPUT
                       IF FS-LFCVPF = '00'
                           ADD 1 TO WS-VALID-CNT
                           ADD 1 TO WS-WRITE-CNT
                       ELSE
                           ADD 1 TO WS-ERROR-CNT
                       END-IF
                   ELSE
                       ADD 1 TO WS-ERROR-CNT
                   END-IF
                   MOVE 'N' TO WS-ERROR-FLG
           END-READ.
       
       210-VALIDATE-REQUEST.
           IF RQ-REQ-STATUS-KBN NOT = '01'
               MOVE 'Y' TO WS-ERROR-FLG
               DISPLAY '要件ステータス不正 REQ-ID=' RQ-REQ-ID
                   ' STATUS=' RQ-REQ-STATUS-KBN
           END-IF.
           
           IF RQ-POL-NO = SPACES
               MOVE 'Y' TO WS-ERROR-FLG
               DISPLAY '証券番号空白 REQ-ID=' RQ-REQ-ID
           END-IF.
       
       220-LOOKUP-CONTRACT.
           MOVE RQ-POL-NO TO PO-POL-NO.
           MOVE 'N' TO WS-FOUND-FLG.
           
           READ LFPOLF2 INTO LFPOLF2-REC
               INVALID KEY
                   MOVE 'Y' TO WS-ERROR-FLG
                   DISPLAY 'LFPOLF2検索失敗 POL-NO=' RQ-POL-NO
               NOT INVALID KEY
                   MOVE 'Y' TO WS-FOUND-FLG
           END-READ.
           
           IF WS-FOUND-FLG NOT = 'Y' AND FS-LFPOLF2 NOT = '00'
               MOVE 'Y' TO WS-ERROR-FLG
               DISPLAY 'LFPOLF2 読込エラー ST=' FS-LFPOLF2
           END-IF.
       
       230-LOOKUP-RESERVE.
           MOVE RQ-POL-NO TO RS-POL-NO.
           MOVE 'N' TO WS-FOUND-FLG.
           
           READ LFRSVF INTO LFRSVF-REC
               INVALID KEY
                   MOVE 'Y' TO WS-ERROR-FLG
                   DISPLAY 'LFRSVF検索失敗 POL-NO=' RQ-POL-NO
               NOT INVALID KEY
                   MOVE 'Y' TO WS-FOUND-FLG
           END-READ.
           
           IF WS-FOUND-FLG NOT = 'Y' AND FS-LFRSVF NOT = '00'
               MOVE 'Y' TO WS-ERROR-FLG
               DISPLAY 'LFRSVF 読込エラー ST=' FS-LFRSVF
           END-IF.
       
       240-GET-ELAPSED-MONTHS.
           MOVE FUNCTION NUMVAL(FUNCTION CURRENT-DATE(1:8))
               TO WS-CURRENT-DATE-NUM.
           MOVE FUNCTION NUMVAL(PO-ISSUE-DATE)
               TO WS-ISSUE-DATE-NUM.
           
           COMPUTE WS-DATE-DIFF =
               WS-CURRENT-DATE-NUM - WS-ISSUE-DATE-NUM.
           
           COMPUTE WS-ELAPSED-MON =
               FUNCTION INTEGER(WS-DATE-DIFF / 100).
       
       250-BUILD-OUTPUT.
           MOVE RQ-POL-NO TO CI-POL-NO.
           MOVE RS-RESERVE-AMT TO CI-RESERVE-AMT.
           MOVE 0 TO CI-NEWBIZ-COST-AMT.
           MOVE WS-ELAPSED-MON TO CI-ELAPSED-MONTH-CNT.
           MOVE '01' TO CI-CV-STATUS-KBN.
       
       260-WRITE-OUTPUT.
           WRITE LFCVPF-REC.
           IF FS-LFCVPF NOT = '00'
               DISPLAY '出力ファイル書込失敗 ST=' FS-LFCVPF
               MOVE 'Y' TO WS-ERROR-FLG
           END-IF.
       
       300-CLOSE-FILES.
           CLOSE LFREQF.
           IF FS-LFREQF NOT = '00'
               DISPLAY 'LFREQF クローズ失敗 ST=' FS-LFREQF
               MOVE 'Y' TO WS-ERROR-FLG
           END-IF.
           
           CLOSE LFPOLF2.
           IF FS-LFPOLF2 NOT = '00'
               DISPLAY 'LFPOLF2 クローズ失敗 ST=' FS-LFPOLF2
               MOVE 'Y' TO WS-ERROR-FLG
           END-IF.
           
           CLOSE LFRSVF.
           IF FS-LFRSVF NOT = '00'
               DISPLAY 'LFRSVF クローズ失敗 ST=' FS-LFRSVF
               MOVE 'Y' TO WS-ERROR-FLG
           END-IF.
           
           CLOSE LFCVPF.
           IF FS-LFCVPF NOT = '00'
               DISPLAY 'LFCVPF クローズ失敗 ST=' FS-LFCVPF
               MOVE 'Y' TO WS-ERROR-FLG
           END-IF.
           
           DISPLAY '処理完了 LF140B'.
           DISPLAY '読込件数：' WS-READ-CNT.
           DISPLAY '有効件数：' WS-VALID-CNT.
           DISPLAY 'エラー件数：' WS-ERROR-CNT.
           DISPLAY '出力件数：' WS-WRITE-CNT.
