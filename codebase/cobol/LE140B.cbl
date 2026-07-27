       IDENTIFICATION DIVISION.
       PROGRAM-ID. LE140B.
       AUTHOR. みらい生命システム部契約経理チーム.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LEJRNF ASSIGN TO DISK-LEJRNF
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-LEJRNF.
           
           SELECT LFACJF ASSIGN TO DISK-LFACJF
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-LFACJF.
           
           SELECT LFCVRF ASSIGN TO DISK-LFCVRF
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-LFCVRF.
           
           SELECT LFREPF ASSIGN TO DISK-LFREPF
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-LFREPF.
       
       DATA DIVISION.
       FILE SECTION.
       FD LEJRNF.
       COPY LEJRNC.
       
       FD LFACJF.
       COPY LFACJC.
       
       FD LFCVRF.
       COPY LFCVRFC.
       
       FD LFREPF.
       COPY LFREPC.
       
       WORKING-STORAGE SECTION.
      *> ================================================================
      *> 版数  年月日     担当         概要
      *> ================================================================
      *> 1.0   20200115   情報システム部  初版作成
      *> 1.1   20210610   情報システム部  仕訳マッチング処理改善
      *> 1.2   20230420   情報システム部  返戻金状態判定追加
      *> ================================================================
       
       01 FS-LEJRNF           PIC 99.
       01 FS-LFACJF           PIC 99.
       01 FS-LFCVRF           PIC 99.
       01 FS-LFREPF           PIC 99.
       
       01 WS-EOF-FLAGS.
           05 WS-LEJRNF-EOF    PIC X VALUE 'N'.
           05 WS-LFACJF-EOF    PIC X VALUE 'N'.
           05 WS-LFCVRF-EOF    PIC X VALUE 'N'.
       
       01 WS-COUNTERS.
           05 WS-JOUR-COUNT    PIC 9(8) VALUE 0.
           05 WS-MATCH-COUNT   PIC 9(8) VALUE 0.
           05 WS-ERROR-COUNT   PIC 9(8) VALUE 0.
           05 WS-OUTPUT-COUNT  PIC 9(8) VALUE 0.
           05 WS-REPORT-ID     PIC 9(10) VALUE 0.
           05 WS-LINE-NO       PIC 9(5) VALUE 0.
       
       01 WS-WORK-FIELDS.
           05 WS-CURRENT-POL   PIC X(10).
           05 WS-JOURNAL-AMT   PIC 9(13)V99.
           05 WS-ADJ-AMT       PIC 9(13)V99.
           05 WS-AMT-DIFF      PIC S9(13)V99.
           05 WS-RESERVE-AMT   PIC 9(13)V99.
           05 WS-ERROR-CLASS   PIC X(2).
           05 WS-MATCH-FOUND   PIC X VALUE 'N'.
           05 WS-CV-FOUND      PIC X VALUE 'N'.
           05 WS-AJ-POST-STS   PIC X(2).
       
       01 WS-ERROR-MESSAGES.
           05 WS-MSG-AM-DIFF   VALUE '金額差異'.
           05 WS-MSG-NO-APPROV VALUE '未承認'.
           05 WS-MSG-NO-SUBJ   VALUE '科目未設定'.
           05 WS-MSG-FILE-ERR  VALUE 'ファイルオープン失敗'.
       
       01 WS-ENUM-VALUES.
           05 CONST-CALC-OK    PIC X(2) VALUE '01'.
           05 CONST-CALC-NG    PIC X(2) VALUE '08'.
           05 CONST-CALC-INV   PIC X(2) VALUE '09'.
           05 CONST-JR-ERROR   PIC X(2) VALUE '02'.
           05 CONST-JR-HOLD    PIC X(2) VALUE '03'.
           05 CONST-AJ-HOLD    PIC X(2) VALUE '02'.
           05 CONST-AJ-REJECT  PIC X(2) VALUE '03'.
       
       PROCEDURE DIVISION.
       
       MAIN-PROCEDURE.
           PERFORM INITIALIZE-PROCESS.
           
           IF RETURN-CODE NOT = 0
               PERFORM ERROR-EXIT
           END-IF.
           
           PERFORM PROCESS-JOURNALS.
           
           PERFORM CLOSE-FILES.
           
           MOVE 0 TO RETURN-CODE.
           GOBACK.
       
       INITIALIZE-PROCESS.
           DISPLAY '経理連携エラー照会帳票バッチ開始'.
           
           MOVE 0 TO WS-JOUR-COUNT.
           MOVE 0 TO WS-MATCH-COUNT.
           MOVE 0 TO WS-ERROR-COUNT.
           MOVE 0 TO WS-OUTPUT-COUNT.
           MOVE 0 TO WS-REPORT-ID.
           MOVE 'N' TO WS-LEJRNF-EOF.
           MOVE 'N' TO WS-LFACJF-EOF.
           MOVE 'N' TO WS-LFCVRF-EOF.
           
           OPEN INPUT LEJRNF.
           IF FS-LEJRNF NOT = 0
               DISPLAY 'LE140B LEJRNF オープン失敗 ST=' FS-LEJRNF
               MOVE 8 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF.
           
           OPEN INPUT LFACJF.
           IF FS-LFACJF NOT = 0
               DISPLAY 'LE140B LFACJF オープン失敗 ST=' FS-LFACJF
               MOVE 8 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF.
           
           OPEN INPUT LFCVRF.
           IF FS-LFCVRF NOT = 0
               DISPLAY 'LE140B LFCVRF オープン失敗 ST=' FS-LFCVRF
               MOVE 8 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF.
           
           OPEN OUTPUT LFREPF.
           IF FS-LFREPF NOT = 0
               DISPLAY 'LE140B LFREPF オープン失敗 ST=' FS-LFREPF
               MOVE 8 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF.
       
       PROCESS-JOURNALS.
           PERFORM UNTIL WS-LEJRNF-EOF = 'Y'
               READ LEJRNF
                   AT END
                       MOVE 'Y' TO WS-LEJRNF-EOF
                   NOT AT END
                       PERFORM PROCESS-ONE-JOURNAL
               END-READ
           END-PERFORM.
       
       PROCESS-ONE-JOURNAL.
           ADD 1 TO WS-JOUR-COUNT.
           
           IF JR-JOURNAL-STATUS-KBN NOT = CONST-JR-ERROR
               AND JR-JOURNAL-STATUS-KBN NOT = CONST-JR-HOLD
               EXIT PARAGRAPH
           END-IF.
           
           MOVE 'N' TO WS-MATCH-FOUND.
           MOVE 'N' TO WS-CV-FOUND.
           MOVE SPACES TO WS-ERROR-CLASS.
           MOVE JR-POL-NO TO WS-CURRENT-POL.
           MOVE JR-AMT TO WS-JOURNAL-AMT.
           
           PERFORM LOOKUP-ADJUSTMENT.
           
           IF WS-MATCH-FOUND = 'N'
               PERFORM OUTPUT-UNMATCH-REPORT
               EXIT PARAGRAPH
           END-IF.
           
           PERFORM LOOKUP-CONVERSION.
           PERFORM CLASSIFY-ERROR.
           PERFORM OUTPUT-ERROR-REPORT.
           
           ADD 1 TO WS-ERROR-COUNT.
       
       LOOKUP-ADJUSTMENT.
           MOVE 'N' TO WS-MATCH-FOUND.
           MOVE 0 TO WS-ADJ-AMT.
           MOVE 0 TO WS-AMT-DIFF.
           MOVE SPACES TO WS-AJ-POST-STS.
           
           PERFORM UNTIL WS-LFACJF-EOF = 'Y'
               READ LFACJF
                   AT END
                       MOVE 'Y' TO WS-LFACJF-EOF
                   NOT AT END
                       IF AJ-POL-NO = WS-CURRENT-POL
                           MOVE 'Y' TO WS-MATCH-FOUND
                           MOVE AJ-AMT TO WS-ADJ-AMT
                           MOVE AJ-POST-STATUS-KBN 
                               TO WS-AJ-POST-STS
                           EXIT PERFORM
                       END-IF
               END-READ
           END-PERFORM.
           
           IF WS-MATCH-FOUND = 'Y'
               SUBTRACT WS-ADJ-AMT FROM WS-JOURNAL-AMT
                   GIVING WS-AMT-DIFF
           END-IF.
       
       LOOKUP-CONVERSION.
           MOVE 'N' TO WS-CV-FOUND.
           MOVE 0 TO WS-RESERVE-AMT.
           
           PERFORM UNTIL WS-LFCVRF-EOF = 'Y'
               READ LFCVRF
                   AT END
                       MOVE 'Y' TO WS-LFCVRF-EOF
                   NOT AT END
                       IF CO-POL-NO = WS-CURRENT-POL
                           MOVE 'Y' TO WS-CV-FOUND
                           MOVE CO-RESERVE-AMT 
                               TO WS-RESERVE-AMT
                           EXIT PERFORM
                       END-IF
               END-READ
           END-PERFORM.
       
       CLASSIFY-ERROR.
           MOVE SPACES TO WS-ERROR-CLASS.
           
           IF WS-AMT-DIFF NOT = 0
               MOVE '01' TO WS-ERROR-CLASS
           ELSE IF WS-AJ-POST-STS = CONST-AJ-HOLD
               MOVE '02' TO WS-ERROR-CLASS
           ELSE IF WS-AJ-POST-STS = CONST-AJ-REJECT
               MOVE '02' TO WS-ERROR-CLASS
           ELSE IF JR-DR-ACCT-CD = SPACES
               OR JR-CR-ACCT-CD = SPACES
               MOVE '03' TO WS-ERROR-CLASS
           ELSE
               MOVE '99' TO WS-ERROR-CLASS
           END-IF.
       
       OUTPUT-ERROR-REPORT.
           ADD 1 TO WS-REPORT-ID.
           ADD 1 TO WS-LINE-NO.
           ADD 1 TO WS-OUTPUT-COUNT.
           
           MOVE WS-REPORT-ID TO RP-REPORT-ID.
           MOVE WS-LINE-NO TO RP-LINE-NO.
           MOVE WS-CURRENT-POL TO RP-POL-NO.
           MOVE WS-ERROR-CLASS TO RP-PRINT-KBN.
           MOVE WS-JOURNAL-AMT TO RP-PRINT-AMT.
           MOVE WS-ERROR-CLASS TO RP-ERROR-KBN.
           
           WRITE LFREPF-REC.
           IF FS-LFREPF NOT = 0
               DISPLAY 'LE140B LFREPF 書込失敗 ST=' FS-LFREPF
               MOVE 8 TO RETURN-CODE
           END-IF.
       
       OUTPUT-UNMATCH-REPORT.
           ADD 1 TO WS-REPORT-ID.
           ADD 1 TO WS-LINE-NO.
           ADD 1 TO WS-OUTPUT-COUNT.
           
           MOVE WS-REPORT-ID TO RP-REPORT-ID.
           MOVE WS-LINE-NO TO RP-LINE-NO.
           MOVE WS-CURRENT-POL TO RP-POL-NO.
           MOVE '04' TO RP-PRINT-KBN.
           MOVE WS-JOURNAL-AMT TO RP-PRINT-AMT.
           MOVE '04' TO RP-ERROR-KBN.
           
           WRITE LFREPF-REC.
           IF FS-LFREPF NOT = 0
               DISPLAY 'LE140B LFREPF 書込失敗 ST=' FS-LFREPF
               MOVE 8 TO RETURN-CODE
           END-IF.
       
       CLOSE-FILES.
           CLOSE LEJRNF.
           CLOSE LFACJF.
           CLOSE LFCVRF.
           CLOSE LFREPF.
           
           DISPLAY '経理連携エラー照会帳票バッチ終了'.
           DISPLAY '処理仕訳件数: ' WS-JOUR-COUNT.
           DISPLAY 'マッチ件数: ' WS-MATCH-COUNT.
           DISPLAY 'エラー件数: ' WS-ERROR-COUNT.
           DISPLAY '帳票出力件数: ' WS-OUTPUT-COUNT.
       
       ERROR-EXIT.
           MOVE 8 TO RETURN-CODE.
           GOBACK.
