       IDENTIFICATION DIVISION.
       PROGRAM-ID. LF250B.
      *月次保険料明細再作成バッチ
      *当月請求対象の契約をLFCNTFから抽出し、LF220Sで有効性を確認
      *したうえでLF110Bへ計算対象を供給してLFPRMFを再作成する。
      *既存LFPRMFに同一POL-NOかつ当月相当の正常明細がある場合は
      *重複作成を抑止し、CALC-STATUS-KBN異常分だけ再計算対象にする。
      *
      *版数  実施日      担当            概要
      *001  20210405    みらい生命 システム部   新規作成
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LFPOLF ASSIGN TO EXTERNAL LFPOLF-FILE
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS LFPOLF-ST.
           SELECT LFCNTF ASSIGN TO EXTERNAL LFCNTF-FILE
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS LFCNTF-ST.
           SELECT LFPRMF ASSIGN TO EXTERNAL LFPRMF-FILE
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS LFPRMF-ST.
       DATA DIVISION.
       FILE SECTION.
       FD  LFPOLF.
       COPY LFPOLFC.
       FD  LFCNTF.
       COPY LFCNTFC.
       FD  LFPRMF.
       COPY LFPRMFC.
       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05  LFPOLF-ST          PIC XX VALUE SPACES.
           05  LFCNTF-ST          PIC XX VALUE SPACES.
           05  LFPRMF-ST          PIC XX VALUE SPACES.
       01  WS-CONTROL.
           05  WS-CURRENT-YM      PIC 9(6) VALUE 202104.
           05  WS-EOJ-FLAG        PIC X VALUE 'N'.
           05  WS-PROC-CNT        PIC 9(8) VALUE ZERO.
           05  WS-ERR-CNT         PIC 9(8) VALUE ZERO.
           05  WS-DUP-FLAG        PIC X VALUE SPACE.
           05  WS-MATCH-FLAG      PIC X VALUE SPACE.
           05  WS-BAND-KBN        PIC X(2) VALUE SPACES.
       01  WS-CALC-FIELDS.
           05  WS-AGE              PIC 9(3) VALUE ZERO.
       01  WS-TEMP-DATA.
           05  WS-POL-NO          PIC X(12) VALUE SPACES.
           05  WS-PRM-ID          PIC 9(10) VALUE ZERO.
       01  WS-MESSAGES.
           05  MSG-START           PIC X(30) VALUE
               'LF250B 処理開始'.
           05  MSG-END             PIC X(30) VALUE
               'LF250B 正常終了'.
           05  MSG-LFPOLF-OPEN-NG  PIC X(41) VALUE
               'エラー：LFPOLF オープン失敗 ST='.
           05  MSG-LFCNTF-OPEN-NG  PIC X(41) VALUE
               'エラー：LFCNTF オープン失敗 ST='.
           05  MSG-LFPRMF-OPEN-NG  PIC X(41) VALUE
               'エラー：LFPRMF オープン失敗 ST='.
           05  MSG-POL-NOT-FOUND   PIC X(30) VALUE
               '警告：契約情報未検出'.
           05  MSG-STAT-INVALID    PIC X(30) VALUE
               '警告：契約状態不正'.
           05  MSG-REC-DUP         PIC X(30) VALUE
               '情報：保険料記録重複'.
           05  MSG-WRITE-NG        PIC X(32) VALUE
               'エラー：LFPRMF 書込失敗'.
           05  MSG-READ-LFPRMF-NG  PIC X(41) VALUE
               'エラー：LFPRMF 読取失敗 ST='.
       01  WS-SUMMARY.
           05  WS-SUMMARY-LBL     PIC X(31) VALUE
               '処理件数/エラー件数：'.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM INIT-FILES.
           IF RETURN-CODE NOT = ZERO
               MOVE 8 TO RETURN-CODE
               GO TO MAIN-END
           END-IF.
           PERFORM PROCESS-CONTRACTS.
           PERFORM FINALIZE.
       MAIN-END.
           GOBACK.
       INIT-FILES.
           MOVE 0 TO RETURN-CODE.
           DISPLAY MSG-START.
           OPEN INPUT LFPOLF.
           IF LFPOLF-ST NOT = '00'
               DISPLAY MSG-LFPOLF-OPEN-NG LFPOLF-ST
               MOVE 8 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF.
           OPEN INPUT LFCNTF.
           IF LFCNTF-ST NOT = '00'
               DISPLAY MSG-LFCNTF-OPEN-NG LFCNTF-ST
               CLOSE LFPOLF
               MOVE 8 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF.
           OPEN I-O LFPRMF.
           IF LFPRMF-ST NOT = '00'
               DISPLAY MSG-LFPRMF-OPEN-NG LFPRMF-ST
               CLOSE LFPOLF LFCNTF
               MOVE 8 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF.
       PROCESS-CONTRACTS.
           MOVE 'N' TO WS-EOJ-FLAG.
           READ LFCNTF
               AT END MOVE 'Y' TO WS-EOJ-FLAG
           END-READ.
           PERFORM UNTIL WS-EOJ-FLAG = 'Y'
               IF CN-NEXT-DUE-YM = WS-CURRENT-YM
                   PERFORM PROCESS-ONE-CONTRACT
               END-IF
               READ LFCNTF
                   AT END MOVE 'Y' TO WS-EOJ-FLAG
                   NOT AT END
                       IF LFCNTF-ST NOT = '00'
                           MOVE 8 TO RETURN-CODE
                           MOVE 'Y' TO WS-EOJ-FLAG
                       END-IF
               END-READ
           END-PERFORM.
       PROCESS-ONE-CONTRACT.
           MOVE CN-POL-NO TO WS-POL-NO.
           PERFORM LOOKUP-POLICY.
           IF RETURN-CODE NOT = ZERO
               DISPLAY MSG-POL-NOT-FOUND
               ADD 1 TO WS-ERR-CNT
               EXIT PARAGRAPH
           END-IF.
           IF PO-POL-STATUS-KBN NOT = '01'
               DISPLAY MSG-STAT-INVALID
               ADD 1 TO WS-ERR-CNT
               EXIT PARAGRAPH
           END-IF.
           MOVE PO-ENTRY-AGE-CNT TO WS-AGE.
           PERFORM DETERMINE-BAND.
           PERFORM CHECK-DUPLICATE.
           IF WS-DUP-FLAG = 'Y'
               DISPLAY MSG-REC-DUP
               EXIT PARAGRAPH
           END-IF.
           PERFORM CREATE-PREMIUM-RECORD.
           IF RETURN-CODE = ZERO
               ADD 1 TO WS-PROC-CNT
           ELSE
               ADD 1 TO WS-ERR-CNT
           END-IF.
       LOOKUP-POLICY.
           MOVE 0 TO RETURN-CODE.
           MOVE 'N' TO WS-MATCH-FLAG.
           CLOSE LFPOLF.
           OPEN INPUT LFPOLF.
           IF LFPOLF-ST NOT = '00'
               MOVE 8 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF.
           READ LFPOLF
               AT END CONTINUE
               NOT AT END
                   IF PO-POL-NO = WS-POL-NO
                       MOVE 'Y' TO WS-MATCH-FLAG
                   END-IF
           END-READ.
           PERFORM UNTIL WS-MATCH-FLAG = 'Y'
               READ LFPOLF
                   AT END GO TO LOOKUP-DONE
                   NOT AT END
                       IF PO-POL-NO = WS-POL-NO
                           MOVE 'Y' TO WS-MATCH-FLAG
                       END-IF
               END-READ
           END-PERFORM.
       LOOKUP-DONE.
           IF WS-MATCH-FLAG = 'N'
               MOVE 8 TO RETURN-CODE
           END-IF.
       DETERMINE-BAND.
           EVALUATE TRUE
               WHEN WS-AGE <= 29
                   MOVE 'A1' TO WS-BAND-KBN
               WHEN WS-AGE <= 39
                   MOVE 'A2' TO WS-BAND-KBN
               WHEN WS-AGE <= 49
                   MOVE 'A3' TO WS-BAND-KBN
               WHEN WS-AGE <= 59
                   MOVE 'A4' TO WS-BAND-KBN
               WHEN OTHER
                   MOVE 'A5' TO WS-BAND-KBN
           END-EVALUATE.
       CHECK-DUPLICATE.
           MOVE 'N' TO WS-DUP-FLAG.
           MOVE 'N' TO WS-MATCH-FLAG.
           CLOSE LFPRMF.
           OPEN INPUT LFPRMF.
           IF LFPRMF-ST NOT = '00'
               DISPLAY MSG-READ-LFPRMF-NG LFPRMF-ST
               MOVE 'N' TO WS-DUP-FLAG
               EXIT PARAGRAPH
           END-IF.
           READ LFPRMF
               AT END CONTINUE
               NOT AT END
                   IF PR-POL-NO = WS-POL-NO
                       MOVE 'Y' TO WS-DUP-FLAG
                       MOVE 'Y' TO WS-MATCH-FLAG
                   END-IF
           END-READ.
           PERFORM UNTIL WS-MATCH-FLAG = 'Y'
               READ LFPRMF
                   AT END GO TO DUPCHECK-DONE
                   NOT AT END
                       IF PR-POL-NO = WS-POL-NO
                           MOVE 'Y' TO WS-DUP-FLAG
                           MOVE 'Y' TO WS-MATCH-FLAG
                       END-IF
               END-READ
           END-PERFORM.
       DUPCHECK-DONE.
           CLOSE LFPRMF.
       CREATE-PREMIUM-RECORD.
      *    当バッチは計算対象の供給のみを行う。月額保険料の算定は
      *    保険料率規程に基づきLF110Bが行うため、ここでは保険料を
      *    確定させず、未計算(P)状態でLFPRMFに明細枠を作成する。
           MOVE 0 TO RETURN-CODE.
           MOVE CN-POL-NO TO PR-POL-NO.
           MOVE PO-SUM-ASSURED-AMT TO PR-SUM-ASSURED-AMT.
           MOVE ZERO TO PR-PRM-AMT.
           MOVE WS-BAND-KBN TO PR-BAND-KBN.
           MOVE 'P' TO PR-CALC-STATUS-KBN.
           MOVE FUNCTION REM(
               FUNCTION ORD(CN-CONTRACTOR-NO) +
               FUNCTION ORD(CN-INSURED-NO),
               9999999999)
               TO WS-PRM-ID.
           MOVE WS-PRM-ID TO PR-PRM-ID.
           OPEN EXTEND LFPRMF.
           IF LFPRMF-ST NOT = '00'
               DISPLAY MSG-WRITE-NG
               MOVE 8 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF.
           WRITE LFPRMF-REC
               END-WRITE.
           IF LFPRMF-ST NOT = '00'
               DISPLAY MSG-WRITE-NG
               MOVE 8 TO RETURN-CODE
           END-IF.
           CLOSE LFPRMF.
       FINALIZE.
           CLOSE LFPOLF LFCNTF LFPRMF.
           DISPLAY MSG-END.
           DISPLAY WS-SUMMARY-LBL WS-PROC-CNT '/' WS-ERR-CNT.
           IF WS-ERR-CNT > ZERO
               MOVE 8 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.
