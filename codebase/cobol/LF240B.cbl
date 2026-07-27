       IDENTIFICATION DIVISION.
       PROGRAM-ID. LF240B.
      * 契約照会応答ファイル作成バッチ
      * 版数 / 年月日 / 担当 / 概要
      * 1.00 / 20200101 / システム室 / 初版
      * 1.01 / 20220615 / システム室 / 解約返戻金計算状態チェック実装
      
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LFPOLF2 ASSIGN TO LS-LFPOLF2
               ORGANIZATION IS INDEXED
               RECORD KEY IS PO-POL-NO
               FILE STATUS IS FS-LFPOLF2.
           SELECT LFRSVF ASSIGN TO LS-LFRSVF
               ORGANIZATION IS INDEXED
               RECORD KEY IS RS-POL-NO
               FILE STATUS IS FS-LFRSVF.
           SELECT LFCVRF ASSIGN TO LS-LFCVRF
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-LFCVRF.
           SELECT LFLOANF ASSIGN TO LS-LFLOANF
               ORGANIZATION IS INDEXED
               RECORD KEY IS LN-POL-NO
               FILE STATUS IS FS-LFLOANF.
           SELECT LFDIVF ASSIGN TO LS-LFDIVF
               ORGANIZATION IS INDEXED
               RECORD KEY IS DV-POL-NO
               FILE STATUS IS FS-LFDIVF.
           SELECT LFREPF ASSIGN TO LS-LFREPF
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-LFREPF.

       DATA DIVISION.
       FILE SECTION.
       FD  LFPOLF2.
       COPY LFPOLF2C.

       FD  LFRSVF.
       COPY LFRSVC.

       FD  LFCVRF.
       COPY LFCVRFC.

       FD  LFLOANF.
       COPY LFLOANC.

       FD  LFDIVF.
       COPY LFDIVC.

       FD  LFREPF.
       COPY LFREPC.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05  FS-LFPOLF2         PIC XX VALUE '00'.
           05  FS-LFRSVF          PIC XX VALUE '00'.
           05  FS-LFCVRF          PIC XX VALUE '00'.
           05  FS-LFLOANF         PIC XX VALUE '00'.
           05  FS-LFDIVF          PIC XX VALUE '00'.
           05  FS-LFREPF          PIC XX VALUE '00'.

       01  WS-CONTROL-FIELDS.
           05  WS-EOF-LFPOLF2     PIC X VALUE 'N'.
           05  WS-EOF-LFCVRF      PIC X VALUE 'N'.
           05  WS-CURRENT-POL     PIC X(20) VALUE SPACES.
           05  WS-LINE-NO         PIC 9(5) COMP VALUE 0.
           05  WS-RECORD-COUNT    PIC 9(9) COMP VALUE 0.

       01  WS-OUTPUT-FIELDS.
           05  WS-PRINT-KBN       PIC 99 VALUE 0.
           05  WS-PRINT-AMT       PIC 9(15)V99 COMP-3 VALUE 0.
           05  WS-ERROR-KBN       PIC 99 VALUE 0.

       01  WS-AGGREGATE-DATA.
           05  WS-RSV-AMT         PIC 9(15)V99 COMP-3 VALUE 0.
           05  WS-CV-AMT          PIC 9(15)V99 COMP-3 VALUE 0.
           05  WS-CV-STATUS-KBN   PIC 99 VALUE 0.
           05  WS-LOAN-BAL-AMT    PIC 9(15)V99 COMP-3 VALUE 0.
           05  WS-DIV-AMT         PIC 9(15)V99 COMP-3 VALUE 0.

       01  WS-VALIDATION-FLAGS.
           05  WS-POL-FOUND       PIC X VALUE 'N'.
           05  WS-RSV-FOUND       PIC X VALUE 'N'.
           05  WS-CV-FOUND        PIC X VALUE 'N'.
           05  WS-CV-VALID        PIC X VALUE 'N'.
           05  WS-LOAN-FOUND      PIC X VALUE 'N'.
           05  WS-DIV-FOUND       PIC X VALUE 'N'.

       PROCEDURE DIVISION.

       0000-MAIN-PROCEDURE.
           PERFORM 1000-INITIALIZE-PROCESS.
           IF RETURN-CODE NOT = 0
               GO TO 9000-TERMINATE-PROCESS
           END-IF.

           PERFORM 2000-PROCESS-CONTRACTS
               UNTIL WS-EOF-LFPOLF2 = 'Y'.

           PERFORM 3000-CLOSE-ALL-FILES.
           IF RETURN-CODE NOT = 0
               GO TO 9000-TERMINATE-PROCESS
           END-IF.

           MOVE 0 TO RETURN-CODE.
           GO TO 9000-TERMINATE-PROCESS.

       1000-INITIALIZE-PROCESS.
           MOVE 'N' TO WS-EOF-LFPOLF2.
           MOVE 'N' TO WS-EOF-LFCVRF.
           MOVE 0 TO WS-LINE-NO.
           MOVE 0 TO WS-RECORD-COUNT.

           OPEN INPUT LFPOLF2.
           IF FS-LFPOLF2 NOT = '00'
               DISPLAY '契約ファイルOPEN失敗 ST=' FS-LFPOLF2
               MOVE 8 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF.

           OPEN INPUT LFRSVF.
           IF FS-LFRSVF NOT = '00'
               DISPLAY '準備金ファイルOPEN失敗 ST=' FS-LFRSVF
               MOVE 8 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF.

           OPEN INPUT LFCVRF.
           IF FS-LFCVRF NOT = '00'
               DISPLAY '返戻金ファイルOPEN失敗 ST=' FS-LFCVRF
               MOVE 8 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF.

           OPEN INPUT LFLOANF.
           IF FS-LFLOANF NOT = '00'
               DISPLAY '貸付ファイルOPEN失敗 ST=' FS-LFLOANF
               MOVE 8 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF.

           OPEN INPUT LFDIVF.
           IF FS-LFDIVF NOT = '00'
               DISPLAY '配当ファイルOPEN失敗 ST=' FS-LFDIVF
               MOVE 8 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF.

           OPEN OUTPUT LFREPF.
           IF FS-LFREPF NOT = '00'
               DISPLAY '応答ファイルOPEN失敗 ST=' FS-LFREPF
               MOVE 8 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF.

       2000-PROCESS-CONTRACTS.
           READ LFPOLF2
               AT END
                   MOVE 'Y' TO WS-EOF-LFPOLF2
               NOT AT END
                   MOVE 'Y' TO WS-POL-FOUND
                   PERFORM 2100-AGGREGATE-DATA
                   PERFORM 2200-CREATE-OUTPUT
           END-READ.

       2100-AGGREGATE-DATA.
           MOVE PO-POL-NO TO WS-CURRENT-POL.
           MOVE 0 TO WS-RSV-AMT.
           MOVE 0 TO WS-CV-AMT.
           MOVE 0 TO WS-LOAN-BAL-AMT.
           MOVE 0 TO WS-DIV-AMT.
           MOVE 0 TO WS-CV-STATUS-KBN.
           MOVE 'N' TO WS-RSV-FOUND.
           MOVE 'N' TO WS-CV-FOUND.
           MOVE 'N' TO WS-CV-VALID.
           MOVE 'N' TO WS-LOAN-FOUND.
           MOVE 'N' TO WS-DIV-FOUND.

           CLOSE LFCVRF.
           OPEN INPUT LFCVRF.
           IF FS-LFCVRF NOT = '00'
               DISPLAY '返戻金ファイルOPEN失敗 ST=' FS-LFCVRF
               MOVE 8 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF.

           PERFORM 2110-READ-RESERVE-DATA.
           PERFORM 2120-READ-SURRENDER-DATA.
           PERFORM 2130-READ-LOAN-DATA.
           PERFORM 2140-READ-DIVIDEND-DATA.

       2110-READ-RESERVE-DATA.
           MOVE WS-CURRENT-POL TO RS-POL-NO.
           READ LFRSVF
               INVALID KEY
                   CONTINUE
               NOT INVALID KEY
                   MOVE 'Y' TO WS-RSV-FOUND
                   MOVE RS-RESERVE-AMT TO WS-RSV-AMT
                   IF RS-CALC-STATUS-KBN = '01'
                       CONTINUE
                   END-IF
           END-READ.

       2120-READ-SURRENDER-DATA.
           PERFORM 2121-SCAN-SURRENDER-FILE
               UNTIL WS-EOF-LFCVRF = 'Y'.
           MOVE 'N' TO WS-EOF-LFCVRF.

       2121-SCAN-SURRENDER-FILE.
           READ LFCVRF
               AT END
                   MOVE 'Y' TO WS-EOF-LFCVRF
               NOT AT END
                   IF CO-POL-NO = WS-CURRENT-POL
                       MOVE 'Y' TO WS-CV-FOUND
                       EVALUATE CO-CALC-STATUS-KBN
                           WHEN '01'
                               MOVE 1 TO WS-CV-STATUS-KBN
                               MOVE 'Y' TO WS-CV-VALID
                               MOVE CO-CV-AMT TO WS-CV-AMT
                           WHEN '08'
                               MOVE 8 TO WS-CV-STATUS-KBN
                               MOVE 'N' TO WS-CV-VALID
                           WHEN '09'
                               MOVE 9 TO WS-CV-STATUS-KBN
                               MOVE 'N' TO WS-CV-VALID
                           WHEN OTHER
                               MOVE 99 TO WS-CV-STATUS-KBN
                       END-EVALUATE
                   END-IF
           END-READ.

       2130-READ-LOAN-DATA.
           MOVE WS-CURRENT-POL TO LN-POL-NO.
           READ LFLOANF
               INVALID KEY
                   CONTINUE
               NOT INVALID KEY
                   MOVE 'Y' TO WS-LOAN-FOUND
                   IF LN-LOAN-STATUS-KBN = '01'
                       MOVE LN-LOAN-BAL-AMT
                           TO WS-LOAN-BAL-AMT
                   END-IF
           END-READ.

       2140-READ-DIVIDEND-DATA.
           MOVE WS-CURRENT-POL TO DV-POL-NO.
           READ LFDIVF
               INVALID KEY
                   CONTINUE
               NOT INVALID KEY
                   MOVE 'Y' TO WS-DIV-FOUND
                   IF DV-DIV-STATUS-KBN = '01'
                       MOVE DV-DIV-AMT TO WS-DIV-AMT
                   END-IF
           END-READ.

       2200-CREATE-OUTPUT.
           ADD 1 TO WS-LINE-NO.

           COMPUTE WS-PRINT-AMT = 
               WS-RSV-AMT + WS-CV-AMT + 
               WS-LOAN-BAL-AMT + WS-DIV-AMT.

           IF WS-CV-VALID = 'Y'
               MOVE 1 TO WS-PRINT-KBN
           ELSE
               MOVE 2 TO WS-PRINT-KBN
           END-IF.

           IF WS-RSV-FOUND = 'Y' AND 
              WS-POL-FOUND = 'Y'
               MOVE 0 TO WS-ERROR-KBN
           ELSE
               MOVE 99 TO WS-ERROR-KBN
           END-IF.

           MOVE WS-LINE-NO TO RP-LINE-NO.
           MOVE WS-CURRENT-POL TO RP-POL-NO.
           MOVE WS-PRINT-KBN TO RP-PRINT-KBN.
           MOVE WS-PRINT-AMT TO RP-PRINT-AMT.
           MOVE WS-ERROR-KBN TO RP-ERROR-KBN.

           WRITE LFREPF-REC.
           IF FS-LFREPF NOT = '00'
               DISPLAY '応答ファイル書込失敗 ST=' FS-LFREPF
               MOVE 8 TO RETURN-CODE
           ELSE
               ADD 1 TO WS-RECORD-COUNT
           END-IF.

       3000-CLOSE-ALL-FILES.
           CLOSE LFPOLF2.
           IF FS-LFPOLF2 NOT = '00'
               DISPLAY '契約ファイルCLOSE失敗 ST=' FS-LFPOLF2
               MOVE 8 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF.

           CLOSE LFRSVF.
           IF FS-LFRSVF NOT = '00'
               DISPLAY '準備金ファイルCLOSE失敗 ST=' FS-LFRSVF
               MOVE 8 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF.

           CLOSE LFCVRF.
           IF FS-LFCVRF NOT = '00'
               DISPLAY '返戻金ファイルCLOSE失敗 ST=' FS-LFCVRF
               MOVE 8 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF.

           CLOSE LFLOANF.
           IF FS-LFLOANF NOT = '00'
               DISPLAY '貸付ファイルCLOSE失敗 ST=' FS-LFLOANF
               MOVE 8 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF.

           CLOSE LFDIVF.
           IF FS-LFDIVF NOT = '00'
               DISPLAY '配当ファイルCLOSE失敗 ST=' FS-LFDIVF
               MOVE 8 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF.

           CLOSE LFREPF.
           IF FS-LFREPF NOT = '00'
               DISPLAY '応答ファイルCLOSE失敗 ST=' FS-LFREPF
               MOVE 8 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF.

       9000-TERMINATE-PROCESS.
           GOBACK.
