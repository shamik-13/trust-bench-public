       IDENTIFICATION DIVISION.
       PROGRAM-ID. LP250B.
      *【保険料試算照会バッチAPI】
      * PROGRAM: LP250B
      * PURPOSE: 照会用の仮契約条件をLFPOLF互換レイアウトへ編集し、
      *          LF190Sで加入年齢帯を確認してからLF110Bの計算経路へ渡す。
      *          出力LFPRMFには試算用PRM-IDを採番する。
      *
      *【変更履歴】
      * 版数 年月日   担当        概要
      * 1.0  20210201 収納システム課  初期版

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           FUNCTION ALL INTRINSIC.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LFPOLF-FILE ASSIGN TO "LFPOLF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS LFPOLF-STATUS.

           SELECT LFAGBF-FILE ASSIGN TO "LFAGBF"
               ORGANIZATION IS INDEXED
               ACCESS IS RANDOM
               RECORD KEY IS AB-BAND-KBN
               FILE STATUS IS LFAGBF-STATUS.

           SELECT LFPRMF-FILE ASSIGN TO "LFPRMF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS LFPRMF-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  LFPOLF-FILE.
       COPY LFPOLFC.

       FD  LFAGBF-FILE.
       COPY LFAGBFC.

       FD  LFPRMF-FILE.
       COPY LFPRMFC.

       WORKING-STORAGE SECTION.
      *ファイル状態
           01  LFPOLF-STATUS              PIC XX VALUE SPACES.
           01  LFAGBF-STATUS              PIC XX VALUE SPACES.
           01  LFPRMF-STATUS              PIC XX VALUE SPACES.

      *処理統計
           01  WS-REC-READ-CNT             PIC 9(8) VALUE 0.
           01  WS-REC-WRITTEN-CNT          PIC 9(8) VALUE 0.
           01  WS-REC-SKIPPED-CNT          PIC 9(8) VALUE 0.
           01  WS-ERROR-CNT                PIC 9(8) VALUE 0.

      *処理制御
           01  WS-EOF-FLAG                 PIC X VALUE 'N'.
           01  WS-LOOKUP-RESULT            PIC X VALUE 'N'.

      *採番・計算用
           01  WS-PRM-ID-SEQ               PIC 9(9) VALUE 100000001.
           01  WS-BAND-KBN                 PIC X(2) VALUE SPACES.
           01  WS-CALC-AMT                 PIC 9(11)V99 VALUE 0.
           01  WS-CALC-STATUS              PIC X(2) VALUE SPACES.

      *エラーメッセージ
           01  WS-MSG-BUFFER               PIC X(70) VALUE SPACES.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM INITIALIZE-PROCESS.

           OPEN INPUT LFPOLF-FILE.
           IF LFPOLF-STATUS NOT = '00'
               MOVE 12 TO RETURN-CODE
               STRING 'LFPOLF オープン失敗 ST=' LFPOLF-STATUS
                   DELIMITED BY SIZE INTO WS-MSG-BUFFER
               DISPLAY WS-MSG-BUFFER
               PERFORM ABORT-PROCESS
           END-IF.

           OPEN INPUT LFAGBF-FILE.
           IF LFAGBF-STATUS NOT = '00'
               MOVE 12 TO RETURN-CODE
               STRING 'LFAGBF オープン失敗 ST=' LFAGBF-STATUS
                   DELIMITED BY SIZE INTO WS-MSG-BUFFER
               DISPLAY WS-MSG-BUFFER
               PERFORM ABORT-PROCESS
           END-IF.

           OPEN OUTPUT LFPRMF-FILE.
           IF LFPRMF-STATUS NOT = '00'
               MOVE 12 TO RETURN-CODE
               STRING 'LFPRMF オープン失敗 ST=' LFPRMF-STATUS
                   DELIMITED BY SIZE INTO WS-MSG-BUFFER
               DISPLAY WS-MSG-BUFFER
               PERFORM ABORT-PROCESS
           END-IF.

           PERFORM READ-LFPOLF-LOOP.
           PERFORM FINALIZE-PROCESS.

           MOVE 0 TO RETURN-CODE.
           GOBACK.

       INITIALIZE-PROCESS.
           DISPLAY '保険料試算照会バッチ LP250B 開始'.
           MOVE 0 TO WS-REC-READ-CNT.
           MOVE 0 TO WS-REC-WRITTEN-CNT.
           MOVE 0 TO WS-REC-SKIPPED-CNT.
           MOVE 0 TO WS-ERROR-CNT.
           MOVE 100000001 TO WS-PRM-ID-SEQ.

       READ-LFPOLF-LOOP.
           PERFORM UNTIL WS-EOF-FLAG = 'Y'
               READ LFPOLF-FILE
                   AT END
                       MOVE 'Y' TO WS-EOF-FLAG
                   NOT AT END
                       ADD 1 TO WS-REC-READ-CNT
                       PERFORM PROCESS-CONTRACT-RECORD
               END-READ

               IF LFPOLF-STATUS NOT = '00'
                   AND LFPOLF-STATUS NOT = '10'
                   MOVE 'Y' TO WS-EOF-FLAG
                   ADD 1 TO WS-ERROR-CNT
                   STRING 'LFPOLF 読込失敗 ST=' LFPOLF-STATUS
                       DELIMITED BY SIZE INTO WS-MSG-BUFFER
                   DISPLAY WS-MSG-BUFFER
               END-IF
           END-PERFORM.

       PROCESS-CONTRACT-RECORD.
      *契約状態チェック：有効('01')のみ処理
           IF PO-POL-STATUS-KBN NOT = '01'
               ADD 1 TO WS-REC-SKIPPED-CNT
               EVALUATE PO-POL-STATUS-KBN
                   WHEN '02'
                       DISPLAY '契約失効 ' PO-POL-NO
                   WHEN '09'
                       DISPLAY '契約解約 ' PO-POL-NO
                   WHEN OTHER
                       DISPLAY '不正な状態=' PO-POL-STATUS-KBN
                           ' POL=' PO-POL-NO
               END-EVALUATE
               EXIT PARAGRAPH
           END-IF.

      *年齢帯区分を決定
           PERFORM DETERMINE-BAND-CATEGORY.

      *年齢帯マスタをLFAGBFで検索
           PERFORM LOOKUP-BAND-MASTER.

      *試算保険料を計算
           PERFORM CALCULATE-PREMIUM-AMOUNT.

      *出力レコード編集と書込
           PERFORM OUTPUT-TO-LFPRMF.

       DETERMINE-BAND-CATEGORY.
      *加入年齢から年齢帯区分を決定
      * A1: <= 29, A2: <= 39, A3: <= 49, A4: <= 59, A5: それ以上
           EVALUATE TRUE
               WHEN PO-ENTRY-AGE-CNT <= 29
                   MOVE 'A1' TO WS-BAND-KBN
               WHEN PO-ENTRY-AGE-CNT <= 39
                   MOVE 'A2' TO WS-BAND-KBN
               WHEN PO-ENTRY-AGE-CNT <= 49
                   MOVE 'A3' TO WS-BAND-KBN
               WHEN PO-ENTRY-AGE-CNT <= 59
                   MOVE 'A4' TO WS-BAND-KBN
               WHEN OTHER
                   MOVE 'A5' TO WS-BAND-KBN
           END-EVALUATE.

       LOOKUP-BAND-MASTER.
      *LFAGBF から該当年齢帯マスタを検索（ランダムアクセス）
           MOVE WS-BAND-KBN TO AB-BAND-KBN.
           MOVE 'N' TO WS-LOOKUP-RESULT.

           READ LFAGBF-FILE
               INVALID KEY
                   ADD 1 TO WS-ERROR-CNT
                   STRING 'LFAGBF 検索失敗 BAND=' WS-BAND-KBN
                       DELIMITED BY SIZE INTO WS-MSG-BUFFER
                   DISPLAY WS-MSG-BUFFER
               NOT INVALID KEY
                   MOVE 'Y' TO WS-LOOKUP-RESULT
           END-READ.

           IF LFAGBF-STATUS NOT = '00' AND LFAGBF-STATUS NOT = '23'
               ADD 1 TO WS-ERROR-CNT
               STRING 'LFAGBF I/O エラー ST=' LFAGBF-STATUS
                   DELIMITED BY SIZE INTO WS-MSG-BUFFER
               DISPLAY WS-MSG-BUFFER
               MOVE 'N' TO WS-LOOKUP-RESULT
           END-IF.

       CALCULATE-PREMIUM-AMOUNT.
      *試算保険料額の確定は行わない。月額保険料率の適用と算定は
      *保険料率規程に基づきLF110Bの計算経路が一元的に担うため、
      *当バッチは年齢帯の確定までを行い、保険料未計算(09)の試算枠
      *としてLFPRMFへ出力する。LF110Bが後続で保険料を確定する。
           IF WS-LOOKUP-RESULT = 'Y'
               MOVE 0 TO WS-CALC-AMT
               MOVE '09' TO WS-CALC-STATUS
           ELSE
               MOVE 0 TO WS-CALC-AMT
               MOVE '02' TO WS-CALC-STATUS
           END-IF.

       OUTPUT-TO-LFPRMF.
      *出力レコードの編集
           MOVE WS-PRM-ID-SEQ TO PR-PRM-ID.
           MOVE PO-POL-NO TO PR-POL-NO.
           MOVE PO-SUM-ASSURED-AMT TO PR-SUM-ASSURED-AMT.
           MOVE WS-CALC-AMT TO PR-PRM-AMT.
           MOVE WS-BAND-KBN TO PR-BAND-KBN.
           MOVE WS-CALC-STATUS TO PR-CALC-STATUS-KBN.

           WRITE LFPRMF-REC.

           IF LFPRMF-STATUS NOT = '00'
               ADD 1 TO WS-ERROR-CNT
               STRING 'LFPRMF 書込失敗 ST=' LFPRMF-STATUS
                   DELIMITED BY SIZE INTO WS-MSG-BUFFER
               DISPLAY WS-MSG-BUFFER
           ELSE
               ADD 1 TO WS-REC-WRITTEN-CNT
           END-IF.

           ADD 1 TO WS-PRM-ID-SEQ.

       FINALIZE-PROCESS.
           CLOSE LFPOLF-FILE.
           CLOSE LFAGBF-FILE.
           CLOSE LFPRMF-FILE.

           DISPLAY '保険料試算照会バッチ LP250B 完了'.
           DISPLAY '読込件数 ：' WS-REC-READ-CNT.
           DISPLAY '出力件数 ：' WS-REC-WRITTEN-CNT.
           DISPLAY 'スキップ件数：' WS-REC-SKIPPED-CNT.
           DISPLAY 'エラー件数 ：' WS-ERROR-CNT.

       ABORT-PROCESS.
           CLOSE LFPOLF-FILE.
           CLOSE LFAGBF-FILE.
           CLOSE LFPRMF-FILE.
           GOBACK.
