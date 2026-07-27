       IDENTIFICATION DIVISION.
       PROGRAM-ID. CB260B.
      ******************************************************************
      * 変更履歴
      * 版数  年月日    担当    概要
      * 1.00  20240220  ＣＤ運用 初版作成
      * 1.01  20240608  ＣＤ運用 督促区分判定を追加
      * 1.02  20241002  ＣＤ運用 督促段階の引上げ制御を追加
      ******************************************************************
      * 督促通知データ作成
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CDDELINQF ASSIGN TO "CDDELINQF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS DL-CARD-NO
               FILE STATUS IS WS-DL-STATUS.
           SELECT CDCARDF ASSIGN TO "CDCARDF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CF-CARD-NO
               FILE STATUS IS WS-CF-STATUS.
           SELECT CDBILLF ASSIGN TO "CDBILLF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-BI-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  CDDELINQF.
           COPY CDDLNQC.
       FD  CDCARDF.
           COPY CDCARDFC.
       FD  CDBILLF.
           COPY CDBILLFC.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05  WS-DL-STATUS       PIC XX VALUE SPACE.
           05  WS-CF-STATUS       PIC XX VALUE SPACE.
           05  WS-BI-STATUS       PIC XX VALUE SPACE.

       01  WS-SWITCHES.
           05  WS-DL-EOF-SW       PIC X VALUE 'N'.
               88  WS-DL-EOF            VALUE 'Y'.
           05  WS-BI-EOF-SW       PIC X VALUE 'N'.
               88  WS-BI-EOF            VALUE 'Y'.
           05  WS-BILL-FOUND-SW   PIC X VALUE 'N'.
               88  WS-BILL-FOUND        VALUE 'Y'.

       01  WS-COUNTERS.
           05  WS-DL-READ-CNT     PIC 9(9) VALUE ZERO.
           05  WS-CF-READ-CNT     PIC 9(9) VALUE ZERO.
           05  WS-BI-READ-CNT     PIC 9(9) VALUE ZERO.
           05  WS-NOTICE-CNT      PIC 9(9) VALUE ZERO.
           05  WS-SKIP-CNT        PIC 9(9) VALUE ZERO.
           05  WS-ERROR-CNT       PIC 9(9) VALUE ZERO.

       01  WS-WORK-AREA.
           05  WS-NOTICE-KBN      PIC X(02) VALUE SPACE.
           05  WS-NEW-STAGE       PIC 9(02) VALUE ZERO.
           05  WS-STAGE-NUM       PIC 9(02) VALUE ZERO.
           05  WS-STATEMENT-ID    PIC X(24) VALUE SPACE.
           05  WS-EDIT-AMT        PIC Z,ZZZ,ZZZ,ZZ9.
           05  WS-EDIT-DAYS       PIC ZZZ9.
           05  WS-EDIT-STAGE      PIC ZZ9.
           05  WS-EDIT-CNT        PIC Z,ZZZ,ZZ9.
           05  WS-REASON-TEXT     PIC X(40) VALUE SPACE.

       01  WS-CONSTANTS.
           05  WS-PROGRAM-ID      PIC X(08) VALUE 'CB260B'.
           05  WS-BILL-STATUS-C   PIC X(02) VALUE 'C '.
           05  WS-BILL-STATUS-H   PIC X(02) VALUE 'H '.
           05  WS-BILL-STATUS-S   PIC X(02) VALUE 'S '.

       PROCEDURE DIVISION.
       0000-MAIN SECTION.
           PERFORM 1000-INITIALIZE
           PERFORM 2000-PROCESS UNTIL WS-DL-EOF
           PERFORM 8000-FINALIZE
           MOVE 0 TO RETURN-CODE
           GOBACK.

       1000-INITIALIZE SECTION.
           DISPLAY 'CB260B 開始'
           OPEN INPUT CDDELINQF
           IF WS-DL-STATUS NOT = '00'
               DISPLAY 'CDDELINQF OPEN ERROR ' WS-DL-STATUS
               MOVE 12 TO RETURN-CODE
               GOBACK
           END-IF
           OPEN INPUT CDCARDF
           IF WS-CF-STATUS NOT = '00'
               DISPLAY 'CDCARDF OPEN ERROR ' WS-CF-STATUS
               CLOSE CDDELINQF
               MOVE 12 TO RETURN-CODE
               GOBACK
           END-IF
           PERFORM 1100-READ-DELINQ.

       1100-READ-DELINQ SECTION.
           READ CDDELINQF NEXT RECORD
               AT END
                   MOVE 'Y' TO WS-DL-EOF-SW
               NOT AT END
                   ADD 1 TO WS-DL-READ-CNT
           END-READ
           IF WS-DL-STATUS NOT = '00'
              AND WS-DL-STATUS NOT = '10'
               DISPLAY 'CDDELINQF READ ERROR ' WS-DL-STATUS
               MOVE 12 TO RETURN-CODE
               GOBACK
           END-IF.

       2000-PROCESS SECTION.
           PERFORM 2100-VALIDATE-DELINQ
           IF WS-REASON-TEXT = SPACE
               PERFORM 2200-READ-CARD
           END-IF
           IF WS-REASON-TEXT = SPACE
               PERFORM 2300-SEARCH-BILL
           END-IF
           IF WS-REASON-TEXT = SPACE
               PERFORM 3000-DECIDE-NOTICE
               PERFORM 5000-CREATE-STMT-ID
               PERFORM 6000-DISPLAY-NOTICE
           ELSE
               PERFORM 6100-DISPLAY-SKIP
           END-IF
           PERFORM 1100-READ-DELINQ.

       2100-VALIDATE-DELINQ SECTION.
           MOVE SPACE TO WS-REASON-TEXT
           IF DL-CARD-NO = SPACE
               MOVE 'カード番号未設定' TO WS-REASON-TEXT
           ELSE
               IF DL-CYCLE-DT = ZERO
                   MOVE 'サイクル日未設定' TO WS-REASON-TEXT
               ELSE
                   IF DL-DAYS-PAST-DUE <= ZERO
                       MOVE '延滞日数なし' TO WS-REASON-TEXT
                   ELSE
                       IF DL-PAST-DUE-AMT <= ZERO
                           MOVE '延滞額なし' TO WS-REASON-TEXT
                       END-IF
                   END-IF
               END-IF
           END-IF.

       2200-READ-CARD SECTION.
           MOVE DL-CARD-NO TO CF-CARD-NO
           READ CDCARDF KEY IS CF-CARD-NO
               INVALID KEY
                   MOVE 'カード未検出' TO WS-REASON-TEXT
               NOT INVALID KEY
                   ADD 1 TO WS-CF-READ-CNT
                   PERFORM 2210-VALIDATE-CARD
           END-READ
           IF WS-CF-STATUS NOT = '00'
              AND WS-CF-STATUS NOT = '23'
               DISPLAY 'CDCARDF READ ERROR ' WS-CF-STATUS
               MOVE 12 TO RETURN-CODE
               GOBACK
           END-IF.

       2210-VALIDATE-CARD SECTION.
           EVALUATE CF-CARD-STATUS
               WHEN '01'
                   CONTINUE
               WHEN '09'
                   CONTINUE
               WHEN '02'
                   MOVE 'カード利用停止' TO WS-REASON-TEXT
               WHEN '03'
                   MOVE 'カード解約済' TO WS-REASON-TEXT
               WHEN OTHER
                   MOVE 'カード状態不正' TO WS-REASON-TEXT
           END-EVALUATE.

       2300-SEARCH-BILL SECTION.
           MOVE 'N' TO WS-BILL-FOUND-SW
           MOVE 'N' TO WS-BI-EOF-SW
           OPEN INPUT CDBILLF
           IF WS-BI-STATUS NOT = '00'
               DISPLAY 'CDBILLF OPEN ERROR ' WS-BI-STATUS
               MOVE 12 TO RETURN-CODE
               GOBACK
           END-IF
           PERFORM 2310-READ-BILL
               UNTIL WS-BI-EOF OR WS-BILL-FOUND
           CLOSE CDBILLF
           IF WS-BI-STATUS NOT = '00'
               DISPLAY 'CDBILLF CLOSE ERROR ' WS-BI-STATUS
               MOVE 12 TO RETURN-CODE
               GOBACK
           END-IF
           IF NOT WS-BILL-FOUND
               MOVE '請求未検出' TO WS-REASON-TEXT
           ELSE
               PERFORM 2320-VALIDATE-BILL
           END-IF.

       2310-READ-BILL SECTION.
           READ CDBILLF
               AT END
                   MOVE 'Y' TO WS-BI-EOF-SW
               NOT AT END
                   ADD 1 TO WS-BI-READ-CNT
                   IF BI-CARD-NO = DL-CARD-NO
                      AND BI-CYCLE-DT = DL-CYCLE-DT
                       MOVE 'Y' TO WS-BILL-FOUND-SW
                   END-IF
           END-READ
           IF WS-BI-STATUS NOT = '00'
              AND WS-BI-STATUS NOT = '10'
               DISPLAY 'CDBILLF READ ERROR ' WS-BI-STATUS
               CLOSE CDBILLF
               MOVE 12 TO RETURN-CODE
               GOBACK
           END-IF.

       2320-VALIDATE-BILL SECTION.
           IF BI-BILL-STATUS = WS-BILL-STATUS-S
               MOVE '請求対象外' TO WS-REASON-TEXT
           ELSE
               IF BI-BILL-STATUS = WS-BILL-STATUS-H
                   MOVE '請求保留' TO WS-REASON-TEXT
               ELSE
                   IF BI-BILL-STATUS NOT = WS-BILL-STATUS-C
                       MOVE '請求状態不正' TO WS-REASON-TEXT
                   ELSE
                       IF BI-BILL-AMT <= ZERO
                           MOVE '請求額なし' TO WS-REASON-TEXT
                       ELSE
                           IF BI-DUE-DT = ZERO
                               MOVE '期日未設定' TO WS-REASON-TEXT
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF.

       3000-DECIDE-NOTICE SECTION.
           MOVE DL-DUNNING-STAGE TO WS-STAGE-NUM
           EVALUATE TRUE
               WHEN DL-DAYS-PAST-DUE < 8
                   MOVE '01' TO WS-NOTICE-KBN
                   MOVE 1 TO WS-NEW-STAGE
               WHEN DL-DAYS-PAST-DUE < 15
                   MOVE '02' TO WS-NOTICE-KBN
                   IF WS-STAGE-NUM < 2
                       MOVE 2 TO WS-NEW-STAGE
                   ELSE
                       MOVE WS-STAGE-NUM TO WS-NEW-STAGE
                   END-IF
               WHEN DL-DAYS-PAST-DUE < 31
                   MOVE '03' TO WS-NOTICE-KBN
                   IF WS-STAGE-NUM < 3
                       MOVE 3 TO WS-NEW-STAGE
                   ELSE
                       MOVE WS-STAGE-NUM TO WS-NEW-STAGE
                   END-IF
               WHEN DL-DAYS-PAST-DUE < 61
                   MOVE '04' TO WS-NOTICE-KBN
                   IF WS-STAGE-NUM < 4
                       MOVE 4 TO WS-NEW-STAGE
                   ELSE
                       MOVE WS-STAGE-NUM TO WS-NEW-STAGE
                   END-IF
               WHEN OTHER
                   MOVE '05' TO WS-NOTICE-KBN
                   MOVE 5 TO WS-NEW-STAGE
           END-EVALUATE
           IF WS-STAGE-NUM > WS-NEW-STAGE
               MOVE WS-STAGE-NUM TO WS-NEW-STAGE
           END-IF.

       5000-CREATE-STMT-ID SECTION.
           MOVE SPACE TO WS-STATEMENT-ID
           STRING
               BI-CARD-NO DELIMITED BY SIZE
               '-' DELIMITED BY SIZE
               BI-CYCLE-DT DELIMITED BY SIZE
               '-' DELIMITED BY SIZE
               BI-PROGRAM-ID DELIMITED BY SIZE
               INTO WS-STATEMENT-ID
           END-STRING.

       6000-DISPLAY-NOTICE SECTION.
           ADD 1 TO WS-NOTICE-CNT
           MOVE DL-PAST-DUE-AMT TO WS-EDIT-AMT
           MOVE DL-DAYS-PAST-DUE TO WS-EDIT-DAYS
           MOVE WS-NEW-STAGE TO WS-EDIT-STAGE
           DISPLAY '督促通知'
                   ' CARD=' DL-CARD-NO
                   ' 氏名=' CF-MEMBER-NAME-KANA
                   ' AMT=' WS-EDIT-AMT
                   ' DUE=' BI-DUE-DT
                   ' DAYS=' WS-EDIT-DAYS
                   ' KBN=' WS-NOTICE-KBN
                   ' STAGE=' WS-EDIT-STAGE
                   ' STMT=' WS-STATEMENT-ID.

       6100-DISPLAY-SKIP SECTION.
           ADD 1 TO WS-SKIP-CNT
           IF WS-REASON-TEXT = SPACE
               MOVE '対象外' TO WS-REASON-TEXT
           END-IF
           DISPLAY '対象外'
                   ' CARD=' DL-CARD-NO
                   ' 理由=' WS-REASON-TEXT.

       8000-FINALIZE SECTION.
           CLOSE CDDELINQF CDCARDF
           IF WS-DL-STATUS NOT = '00'
               DISPLAY 'CDDELINQF CLOSE ERROR ' WS-DL-STATUS
               MOVE 12 TO RETURN-CODE
               GOBACK
           END-IF
           IF WS-CF-STATUS NOT = '00'
               DISPLAY 'CDCARDF CLOSE ERROR ' WS-CF-STATUS
               MOVE 12 TO RETURN-CODE
               GOBACK
           END-IF
           MOVE WS-DL-READ-CNT TO WS-EDIT-CNT
           DISPLAY '延滞読込件数=' WS-EDIT-CNT
           MOVE WS-CF-READ-CNT TO WS-EDIT-CNT
           DISPLAY 'カード読込件数=' WS-EDIT-CNT
           MOVE WS-BI-READ-CNT TO WS-EDIT-CNT
           DISPLAY '請求読込件数=' WS-EDIT-CNT
           MOVE WS-NOTICE-CNT TO WS-EDIT-CNT
           DISPLAY '通知件数=' WS-EDIT-CNT
           MOVE WS-SKIP-CNT TO WS-EDIT-CNT
           DISPLAY '対象外件数=' WS-EDIT-CNT
           MOVE WS-ERROR-CNT TO WS-EDIT-CNT
           DISPLAY 'エラー件数=' WS-EDIT-CNT
           DISPLAY 'CB260B 終了'.

       END PROGRAM CB260B.
