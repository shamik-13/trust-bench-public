       IDENTIFICATION DIVISION.
       PROGRAM-ID. CB214B.
      *
      * 売上確定取込バッチ
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CDSALF ASSIGN TO "CDSALF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS FS-CDSALF.

           SELECT CDAUTHF ASSIGN TO "CDAUTHF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS FS-CDAUTHF.

           SELECT CDCARDF ASSIGN TO "CDCARDF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS CF-CARD-NO
               FILE STATUS IS FS-CDCARDF.

           SELECT CDBALF ASSIGN TO "CDBALF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS FS-CDBALF.

       DATA DIVISION.
       FILE SECTION.
       FD  CDSALF.
           COPY CDSALC.

       FD  CDAUTHF.
           COPY CDAUTHFC.

       FD  CDCARDF.
           COPY CDCARD02.

       FD  CDBALF.
           COPY CDBALFC2.

       WORKING-STORAGE SECTION.
       01  FS-CDSALF                 PIC XX VALUE SPACE.
       01  FS-CDAUTHF                PIC XX VALUE SPACE.
       01  FS-CDCARDF                PIC XX VALUE SPACE.
       01  FS-CDBALF                 PIC XX VALUE SPACE.

       01  WK-END-FLAG               PIC X VALUE 'N'.
           88  WK-END                VALUE 'Y'.
           88  WK-NOT-END            VALUE 'N'.

       01  WK-AUTH-END-FLAG          PIC X VALUE 'N'.
           88  WK-AUTH-END           VALUE 'Y'.
           88  WK-AUTH-NOT-END       VALUE 'N'.

       01  WK-HARD-ERROR-FLAG        PIC X VALUE 'N'.
           88  WK-HARD-ERROR         VALUE 'Y'.
           88  WK-NO-HARD-ERROR      VALUE 'N'.

       01  WK-AUTH-FOUND-FLAG        PIC X VALUE 'N'.
           88  WK-AUTH-FOUND         VALUE 'Y'.
           88  WK-AUTH-NOT-FOUND     VALUE 'N'.

       01  WK-CARD-FOUND-FLAG        PIC X VALUE 'N'.
           88  WK-CARD-FOUND         VALUE 'Y'.
           88  WK-CARD-NOT-FOUND     VALUE 'N'.

       01  WK-BAL-WRITE-FLAG         PIC X VALUE 'N'.
           88  WK-BAL-WRITTEN        VALUE 'Y'.
           88  WK-BAL-NOT-WRITTEN    VALUE 'N'.

       01  WK-CDAUTHF-OPEN-FLAG      PIC X VALUE 'N'.
           88  WK-CDAUTHF-OPEN       VALUE 'Y'.
           88  WK-CDAUTHF-CLOSED     VALUE 'N'.

       01  WK-AMT-DIFF               PIC S9(13)V99 VALUE ZERO.
       01  WK-ABS-DIFF               PIC 9(13)V99 VALUE ZERO.
       01  WK-TOLERANCE-AMT          PIC 9(13)V99 VALUE 1.00.
       01  WK-PROCESS-DATE           PIC 9(8) VALUE ZERO.

       01  WK-READ-CNT               PIC 9(9) VALUE ZERO.
       01  WK-OK-CNT                 PIC 9(9) VALUE ZERO.
       01  WK-NG-CNT                 PIC 9(9) VALUE ZERO.
       01  WK-AUTH-UPD-CNT           PIC 9(9) VALUE ZERO.
       01  WK-BAL-WRT-CNT            PIC 9(9) VALUE ZERO.

       01  AR-DECISION-KBN           PIC X VALUE SPACE.
       01  AR-DECLINE-REASON         PIC X(3) VALUE SPACE.
       01  AR-EVENT-TEXT             PIC X(80) VALUE SPACE.

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE ZERO TO RETURN-CODE
           ACCEPT WK-PROCESS-DATE FROM DATE YYYYMMDD

           PERFORM 1000-OPEN-FILES
           IF WK-NO-HARD-ERROR
               PERFORM 2000-READ-SALES
               PERFORM UNTIL WK-END OR WK-HARD-ERROR
                   ADD 1 TO WK-READ-CNT
                   PERFORM 3000-PROCESS-SALES
                   PERFORM 2000-READ-SALES
               END-PERFORM
           END-IF

           PERFORM 9000-CLOSE-FILES

           IF WK-HARD-ERROR
               MOVE 12 TO RETURN-CODE
           ELSE
               MOVE ZERO TO RETURN-CODE
               DISPLAY 'CB214B NORMAL END READ=' WK-READ-CNT
               DISPLAY 'OK=' WK-OK-CNT
                       ' NG=' WK-NG-CNT
                       ' AUTH-UPD=' WK-AUTH-UPD-CNT
               DISPLAY 'BAL-WRT=' WK-BAL-WRT-CNT
           END-IF

           GOBACK.

       1000-OPEN-FILES.
           OPEN INPUT CDSALF
           IF FS-CDSALF NOT = '00'
               DISPLAY 'CDSALF OPEN ERROR ST=' FS-CDSALF
               SET WK-HARD-ERROR TO TRUE
           END-IF

           IF WK-NO-HARD-ERROR
               OPEN INPUT CDCARDF
               IF FS-CDCARDF NOT = '00'
                   DISPLAY 'CDCARDF OPEN ERROR ST=' FS-CDCARDF
                   SET WK-HARD-ERROR TO TRUE
               END-IF
           END-IF

           IF WK-NO-HARD-ERROR
               OPEN OUTPUT CDBALF
               IF FS-CDBALF NOT = '00'
                   DISPLAY 'CDBALF OPEN ERROR ST=' FS-CDBALF
                   SET WK-HARD-ERROR TO TRUE
               END-IF
           END-IF.

       2000-READ-SALES.
           READ CDSALF
               AT END
                   SET WK-END TO TRUE
               NOT AT END
                   SET WK-NOT-END TO TRUE
           END-READ

           IF FS-CDSALF NOT = '00' AND FS-CDSALF NOT = '10'
               DISPLAY 'CDSALF READ ERROR ST=' FS-CDSALF
                       ' SALES-ID=' SL-SALES-ID
               SET WK-HARD-ERROR TO TRUE
           END-IF.

       3000-PROCESS-SALES.
           MOVE 'D' TO AR-DECISION-KBN
           MOVE SPACES TO AR-DECLINE-REASON
           MOVE SPACES TO AR-EVENT-TEXT
           SET WK-AUTH-NOT-FOUND TO TRUE
           SET WK-CARD-NOT-FOUND TO TRUE
           SET WK-BAL-NOT-WRITTEN TO TRUE

           PERFORM 3100-FIND-AUTH
           IF WK-HARD-ERROR
               EXIT PARAGRAPH
           END-IF

           IF WK-AUTH-NOT-FOUND
               MOVE 'CUR' TO AR-DECLINE-REASON
               MOVE 'オーソリ履歴なし' TO AR-EVENT-TEXT
               PERFORM 3900-SEND-INVESTIGATION
               ADD 1 TO WK-NG-CNT
               EXIT PARAGRAPH
           END-IF

           PERFORM 3200-READ-CARD
           IF WK-HARD-ERROR
               EXIT PARAGRAPH
           END-IF

           PERFORM 3300-VALIDATE-SALES

           IF AR-DECISION-KBN = 'A'
               PERFORM 3400-WRITE-BALANCE
               IF WK-HARD-ERROR
                   EXIT PARAGRAPH
               END-IF
               PERFORM 3500-UPDATE-AUTH
               IF WK-HARD-ERROR
                   EXIT PARAGRAPH
               END-IF
               ADD 1 TO WK-OK-CNT
           ELSE
               PERFORM 3900-SEND-INVESTIGATION
               ADD 1 TO WK-NG-CNT
           END-IF.

       3100-FIND-AUTH.
           SET WK-AUTH-NOT-FOUND TO TRUE
           SET WK-AUTH-NOT-END TO TRUE
           SET WK-CDAUTHF-CLOSED TO TRUE

           OPEN INPUT CDAUTHF
           IF FS-CDAUTHF NOT = '00'
               DISPLAY 'CDAUTHF OPEN ERROR ST=' FS-CDAUTHF
               SET WK-HARD-ERROR TO TRUE
               EXIT PARAGRAPH
           END-IF
           SET WK-CDAUTHF-OPEN TO TRUE

           PERFORM UNTIL WK-AUTH-END OR WK-AUTH-FOUND
                   OR WK-HARD-ERROR
               READ CDAUTHF
                   AT END
                       SET WK-AUTH-END TO TRUE
                   NOT AT END
                       IF AU-AUTH-ID = SL-AUTH-ID
                           SET WK-AUTH-FOUND TO TRUE
                       END-IF
               END-READ

               IF FS-CDAUTHF NOT = '00'
                   AND FS-CDAUTHF NOT = '10'
                   DISPLAY 'CDAUTHF READ ERROR ST=' FS-CDAUTHF
                           ' AUTH-ID=' SL-AUTH-ID
                   SET WK-HARD-ERROR TO TRUE
               END-IF
           END-PERFORM

           CLOSE CDAUTHF
           SET WK-CDAUTHF-CLOSED TO TRUE
           IF FS-CDAUTHF NOT = '00'
               DISPLAY 'CDAUTHF CLOSE ERROR ST=' FS-CDAUTHF
               SET WK-HARD-ERROR TO TRUE
           END-IF.

       3200-READ-CARD.
           MOVE SL-CARD-NO TO CF-CARD-NO

           READ CDCARDF KEY IS CF-CARD-NO
               INVALID KEY
                   SET WK-CARD-NOT-FOUND TO TRUE
               NOT INVALID KEY
                   SET WK-CARD-FOUND TO TRUE
           END-READ

           IF FS-CDCARDF NOT = '00'
               AND FS-CDCARDF NOT = '23'
               DISPLAY 'CDCARDF READ ERROR ST=' FS-CDCARDF
                       ' CARD-NO=' SL-CARD-NO
               SET WK-HARD-ERROR TO TRUE
           END-IF.

       3300-VALIDATE-SALES.
           MOVE 'D' TO AR-DECISION-KBN
           MOVE SPACES TO AR-DECLINE-REASON
           MOVE SPACES TO AR-EVENT-TEXT

           IF WK-CARD-NOT-FOUND
               MOVE 'STS' TO AR-DECLINE-REASON
               MOVE 'カードマスタ未登録' TO AR-EVENT-TEXT
               EXIT PARAGRAPH
           END-IF

           IF SL-CARD-NO NOT = AU-CARD-NO
               MOVE 'STS' TO AR-DECLINE-REASON
               MOVE 'カード番号不一致' TO AR-EVENT-TEXT
               EXIT PARAGRAPH
           END-IF

           IF CF-CARD-STATUS NOT = '01'
               MOVE 'STS' TO AR-DECLINE-REASON
               MOVE 'カード状態不正' TO AR-EVENT-TEXT
               EXIT PARAGRAPH
           END-IF

           IF AU-AUTH-RESULT = '30'
               MOVE 'STS' TO AR-DECLINE-REASON
               MOVE 'オーソリ確定済' TO AR-EVENT-TEXT
               EXIT PARAGRAPH
           END-IF

           IF AU-AUTH-RESULT NOT = '00'
               MOVE 'STS' TO AR-DECLINE-REASON
               MOVE '有効保留なし' TO AR-EVENT-TEXT
               EXIT PARAGRAPH
           END-IF

           IF AU-CURRENCY-CD NOT = 'JPY'
               MOVE 'CUR' TO AR-DECLINE-REASON
               MOVE '通貨非対応' TO AR-EVENT-TEXT
               EXIT PARAGRAPH
           END-IF

           IF AU-MERCHANT-CODE NOT = SL-MERCHANT-CODE
               MOVE 'CUR' TO AR-DECLINE-REASON
               MOVE '加盟店不一致' TO AR-EVENT-TEXT
               EXIT PARAGRAPH
           END-IF

           COMPUTE WK-AMT-DIFF = SL-SALES-AMT - AU-AUTH-AMT

           IF WK-AMT-DIFF < ZERO
               COMPUTE WK-ABS-DIFF = WK-AMT-DIFF * -1
           ELSE
               MOVE WK-AMT-DIFF TO WK-ABS-DIFF
           END-IF

           IF WK-ABS-DIFF > WK-TOLERANCE-AMT
               MOVE 'LIM' TO AR-DECLINE-REASON
               MOVE '金額許容差超過' TO AR-EVENT-TEXT
               EXIT PARAGRAPH
           END-IF

           IF SL-SALES-AMT > CF-CREDIT-LIMIT
               MOVE 'LIM' TO AR-DECLINE-REASON
               MOVE '与信限度超過' TO AR-EVENT-TEXT
               EXIT PARAGRAPH
           END-IF

           MOVE 'A' TO AR-DECISION-KBN
           MOVE SPACES TO AR-EVENT-TEXT.

       3400-WRITE-BALANCE.
           MOVE SL-CARD-NO TO BL-CARD-NO
           MOVE SL-SALES-AMT TO BL-CURRENT-BAL-AMT
           MOVE ZERO TO BL-LAST-STMT-AMT
           MOVE WK-PROCESS-DATE TO BL-CYCLE-DT

           WRITE CDBALF-REC

           IF FS-CDBALF NOT = '00'
               DISPLAY 'CDBALF WRITE ERROR ST=' FS-CDBALF
                       ' CARD-NO=' SL-CARD-NO
               SET WK-HARD-ERROR TO TRUE
           ELSE
               SET WK-BAL-WRITTEN TO TRUE
               ADD 1 TO WK-BAL-WRT-CNT
           END-IF.

       3500-UPDATE-AUTH.
           SET WK-AUTH-NOT-FOUND TO TRUE
           SET WK-AUTH-NOT-END TO TRUE
           SET WK-CDAUTHF-CLOSED TO TRUE

           OPEN I-O CDAUTHF
           IF FS-CDAUTHF NOT = '00'
               DISPLAY 'CDAUTHF I-O OPEN ERROR ST=' FS-CDAUTHF
               SET WK-HARD-ERROR TO TRUE
               EXIT PARAGRAPH
           END-IF
           SET WK-CDAUTHF-OPEN TO TRUE

           PERFORM UNTIL WK-AUTH-END OR WK-AUTH-FOUND
                   OR WK-HARD-ERROR
               READ CDAUTHF
                   AT END
                       SET WK-AUTH-END TO TRUE
                   NOT AT END
                       IF AU-AUTH-ID = SL-AUTH-ID
                           MOVE '30' TO AU-AUTH-RESULT
                           REWRITE CDAUTHF-REC
                           IF FS-CDAUTHF NOT = '00'
                               DISPLAY 'CDAUTHF REWRITE ERR ST='
                                       FS-CDAUTHF
                               DISPLAY 'AUTH-ID=' SL-AUTH-ID
                               SET WK-HARD-ERROR TO TRUE
                           ELSE
                               ADD 1 TO WK-AUTH-UPD-CNT
                           END-IF
                           SET WK-AUTH-FOUND TO TRUE
                       END-IF
               END-READ

               IF FS-CDAUTHF NOT = '00'
                   AND FS-CDAUTHF NOT = '10'
                   DISPLAY 'CDAUTHF UPDATE READ ERR ST='
                           FS-CDAUTHF
                   DISPLAY 'AUTH-ID=' SL-AUTH-ID
                   SET WK-HARD-ERROR TO TRUE
               END-IF
           END-PERFORM

           CLOSE CDAUTHF
           SET WK-CDAUTHF-CLOSED TO TRUE
           IF FS-CDAUTHF NOT = '00'
               DISPLAY 'CDAUTHF I-O CLOSE ERR ST=' FS-CDAUTHF
               SET WK-HARD-ERROR TO TRUE
           END-IF.

       3900-SEND-INVESTIGATION.
           DISPLAY 'INVEST SALES-ID=' SL-SALES-ID
                   ' AUTH-ID=' SL-AUTH-ID
                   ' DECISION=' AR-DECISION-KBN
           DISPLAY 'REASON=' AR-DECLINE-REASON
                   ' TEXT=' AR-EVENT-TEXT.

       9000-CLOSE-FILES.
           CLOSE CDSALF
           IF FS-CDSALF NOT = '00'
               DISPLAY 'CDSALF CLOSE ERROR ST=' FS-CDSALF
               SET WK-HARD-ERROR TO TRUE
           END-IF

           CLOSE CDCARDF
           IF FS-CDCARDF NOT = '00'
               DISPLAY 'CDCARDF CLOSE ERROR ST=' FS-CDCARDF
               SET WK-HARD-ERROR TO TRUE
           END-IF

           CLOSE CDBALF
           IF FS-CDBALF NOT = '00'
               DISPLAY 'CDBALF CLOSE ERROR ST=' FS-CDBALF
               SET WK-HARD-ERROR TO TRUE
           END-IF.
