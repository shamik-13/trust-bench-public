       IDENTIFICATION DIVISION.
       PROGRAM-ID. CB103B.
      *
      * 変更履歴
      * 版数  年月日    担当      概要
      * 1.00  20240115  開発一課  初版作成
      * 1.01  20240308  開発一課  返品額超過判定を追加
      * 1.02  20240520  保守二課  例外出力理由を整理
      *
      * 取消返品反映バッチ
      * 承認済み返品を請求明細へ反映し、異常分を例外出力する。
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CDRTNF
               ASSIGN TO "CDRTNF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-CDRTNF-ST.
           SELECT CDCAPF
               ASSIGN TO "CDCAPF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-CDCAPF-ST.
           SELECT CDSALEF
               ASSIGN TO "CDSALEF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-CDSALEF-ST.
           SELECT CDEXCPF
               ASSIGN TO "CDEXCPF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-CDEXCPF-ST.

       DATA DIVISION.
       FILE SECTION.
       FD  CDRTNF.
           COPY CDRTNC.
       FD  CDCAPF.
           COPY CDCAPFC.
       FD  CDSALEF.
           COPY CDSALEFC.
       FD  CDEXCPF.
           COPY CDEXCPC.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05 WS-CDRTNF-ST          PIC XX VALUE SPACES.
           05 WS-CDCAPF-ST          PIC XX VALUE SPACES.
           05 WS-CDSALEF-ST         PIC XX VALUE SPACES.
           05 WS-CDEXCPF-ST         PIC XX VALUE SPACES.

       01  WS-SWITCHES.
           05 WS-RT-END-SW          PIC X VALUE 'N'.
              88 RT-END                  VALUE 'Y'.
              88 RT-NOT-END              VALUE 'N'.
           05 WS-CAP-END-SW         PIC X VALUE 'N'.
              88 CAP-END                 VALUE 'Y'.
              88 CAP-NOT-END             VALUE 'N'.
           05 WS-SALE-END-SW        PIC X VALUE 'N'.
              88 SALE-END                VALUE 'Y'.
              88 SALE-NOT-END            VALUE 'N'.
           05 WS-CAP-FOUND-SW       PIC X VALUE 'N'.
              88 CAP-FOUND               VALUE 'Y'.
              88 CAP-NOT-FOUND           VALUE 'N'.
           05 WS-SALE-FOUND-SW      PIC X VALUE 'N'.
              88 SALE-FOUND              VALUE 'Y'.
              88 SALE-NOT-FOUND          VALUE 'N'.
           05 WS-HARD-ERR-SW        PIC X VALUE 'N'.
              88 HARD-ERROR              VALUE 'Y'.
              88 NO-HARD-ERROR           VALUE 'N'.

       01  WS-WORK.
           05 WS-TODAY              PIC 9(8) VALUE ZERO.
           05 WS-EXCEPTION-SEQ      PIC 9(6) VALUE ZERO.
           05 WS-NEW-BILLED-AMT     PIC S9(11)V99 COMP-3 VALUE ZERO.
           05 WS-RT-COUNT           PIC 9(9) VALUE ZERO.
           05 WS-APPROVED-COUNT     PIC 9(9) VALUE ZERO.
           05 WS-UPDATE-COUNT       PIC 9(9) VALUE ZERO.
           05 WS-EXCEPT-COUNT       PIC 9(9) VALUE ZERO.
           05 WS-SKIP-COUNT         PIC 9(9) VALUE ZERO.

       01  WS-REASON.
           05 WS-REASON-CD          PIC X(2) VALUE SPACES.
              88 REASON-NO-CAP           VALUE '01'.
              88 REASON-AMT-OVER         VALUE '02'.
              88 REASON-CHARGEBACK       VALUE '03'.
              88 REASON-NO-SALE          VALUE '04'.
              88 REASON-CARD-MISMATCH    VALUE '05'.

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE 8 TO RETURN-CODE
           PERFORM 1000-INITIALIZE
           IF NO-HARD-ERROR
               PERFORM 2000-PROCESS UNTIL RT-END OR HARD-ERROR
           END-IF
           PERFORM 9000-FINALIZE
           GOBACK.

       1000-INITIALIZE.
           SET NO-HARD-ERROR TO TRUE
           SET RT-NOT-END TO TRUE
           ACCEPT WS-TODAY FROM DATE YYYYMMDD
           OPEN INPUT CDRTNF
           IF WS-CDRTNF-ST NOT = '00'
               DISPLAY 'CDRTNF OPEN ERROR ST='
                       WS-CDRTNF-ST
               SET HARD-ERROR TO TRUE
           END-IF
           IF NO-HARD-ERROR
               OPEN OUTPUT CDEXCPF
               IF WS-CDEXCPF-ST NOT = '00'
                   DISPLAY 'CDEXCPF OPEN ERROR ST='
                           WS-CDEXCPF-ST
                   SET HARD-ERROR TO TRUE
               END-IF
           END-IF
           IF NO-HARD-ERROR
               PERFORM 2100-READ-RETURN
           END-IF.

       2000-PROCESS.
           ADD 1 TO WS-RT-COUNT
           IF RT-APPROVAL-STATUS = 'A'
               ADD 1 TO WS-APPROVED-COUNT
               PERFORM 3000-FIND-CAPTURE
               IF NO-HARD-ERROR
                   IF CAP-FOUND
                       PERFORM 4000-CHECK-SALE
                       PERFORM 5000-APPLY-RETURN
                   ELSE
                       SET REASON-NO-CAP TO TRUE
                       PERFORM 7000-WRITE-EXCEPTION
                   END-IF
               END-IF
           ELSE
               ADD 1 TO WS-SKIP-COUNT
           END-IF
           IF NO-HARD-ERROR
               PERFORM 2100-READ-RETURN
           END-IF.

       2100-READ-RETURN.
           READ CDRTNF
               AT END
                   SET RT-END TO TRUE
               NOT AT END
                   CONTINUE
           END-READ
           IF WS-CDRTNF-ST NOT = '00'
              AND WS-CDRTNF-ST NOT = '10'
               DISPLAY 'CDRTNF READ ERROR ST='
                       WS-CDRTNF-ST
               SET HARD-ERROR TO TRUE
           END-IF.

       3000-FIND-CAPTURE.
           SET CAP-NOT-FOUND TO TRUE
           SET CAP-NOT-END TO TRUE
           OPEN I-O CDCAPF
           IF WS-CDCAPF-ST NOT = '00'
               DISPLAY 'CDCAPF OPEN ERROR ST='
                       WS-CDCAPF-ST
               SET HARD-ERROR TO TRUE
           END-IF
           PERFORM UNTIL CAP-END OR CAP-FOUND OR HARD-ERROR
               READ CDCAPF
                   AT END
                       SET CAP-END TO TRUE
                   NOT AT END
                       IF BC-SALE-ID = RT-SALE-ID
                           SET CAP-FOUND TO TRUE
                       END-IF
               END-READ
               IF WS-CDCAPF-ST NOT = '00'
                  AND WS-CDCAPF-ST NOT = '10'
                   DISPLAY 'CDCAPF READ ERROR ST='
                           WS-CDCAPF-ST
                   SET HARD-ERROR TO TRUE
               END-IF
           END-PERFORM
           IF HARD-ERROR
               CLOSE CDCAPF
           END-IF.

       4000-CHECK-SALE.
           SET SALE-NOT-FOUND TO TRUE
           SET SALE-NOT-END TO TRUE
           OPEN INPUT CDSALEF
           IF WS-CDSALEF-ST NOT = '00'
               DISPLAY 'CDSALEF OPEN ERROR ST='
                       WS-CDSALEF-ST
               SET HARD-ERROR TO TRUE
           END-IF
           PERFORM UNTIL SALE-END OR SALE-FOUND OR HARD-ERROR
               READ CDSALEF
                   AT END
                       SET SALE-END TO TRUE
                   NOT AT END
                       IF SL-SALE-ID = RT-SALE-ID
                           SET SALE-FOUND TO TRUE
                       END-IF
               END-READ
               IF WS-CDSALEF-ST NOT = '00'
                  AND WS-CDSALEF-ST NOT = '10'
                   DISPLAY 'CDSALEF READ ERROR ST='
                           WS-CDSALEF-ST
                   SET HARD-ERROR TO TRUE
               END-IF
           END-PERFORM
           CLOSE CDSALEF
           IF WS-CDSALEF-ST NOT = '00'
               DISPLAY 'CDSALEF CLOSE ERROR ST='
                       WS-CDSALEF-ST
               SET HARD-ERROR TO TRUE
           END-IF.

       5000-APPLY-RETURN.
           IF NO-HARD-ERROR
               IF SALE-NOT-FOUND
                   SET REASON-NO-SALE TO TRUE
                   PERFORM 7000-WRITE-EXCEPTION
               ELSE
                   IF BC-CARD-NO NOT = RT-CARD-NO
                       SET REASON-CARD-MISMATCH TO TRUE
                       PERFORM 7000-WRITE-EXCEPTION
                   ELSE
                       IF BC-CAP-STATUS = 'H'
                           SET REASON-CHARGEBACK TO TRUE
                           PERFORM 7000-WRITE-EXCEPTION
                       ELSE
                           IF RT-RETURN-AMT > BC-BILLED-AMT
                               SET REASON-AMT-OVER TO TRUE
                               PERFORM 7000-WRITE-EXCEPTION
                           ELSE
                               SUBTRACT RT-RETURN-AMT
                                   FROM BC-BILLED-AMT
                                   GIVING WS-NEW-BILLED-AMT
                               MOVE WS-NEW-BILLED-AMT
                                   TO BC-BILLED-AMT
                               MOVE 'S' TO BC-CAP-STATUS
                               MOVE 'CB103B' TO BC-PROGRAM-ID
                               REWRITE CDCAPF-REC
                               IF WS-CDCAPF-ST = '00'
                                   ADD 1 TO WS-UPDATE-COUNT
                               ELSE
                                   DISPLAY 'CDCAPF REWRITE ERROR ST='
                                           WS-CDCAPF-ST
                                   SET HARD-ERROR TO TRUE
                               END-IF
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF
           CLOSE CDCAPF
           IF WS-CDCAPF-ST NOT = '00'
               DISPLAY 'CDCAPF CLOSE ERROR ST='
                       WS-CDCAPF-ST
               SET HARD-ERROR TO TRUE
           END-IF.

       7000-WRITE-EXCEPTION.
           ADD 1 TO WS-EXCEPTION-SEQ
           INITIALIZE CDEXCPF-REC
           MOVE WS-EXCEPTION-SEQ TO EX-EXCEPTION-ID
           MOVE RT-SALE-ID       TO EX-SALE-ID
           MOVE RT-CARD-NO       TO EX-CARD-NO
           MOVE WS-REASON-CD     TO EX-REASON-CD
           MOVE 'CB103B'         TO EX-DETECTED-PGM
           MOVE WS-TODAY         TO EX-EXCEPTION-DT
           MOVE '0'              TO EX-ACTION-STATUS
           WRITE CDEXCPF-REC
           IF WS-CDEXCPF-ST = '00'
               ADD 1 TO WS-EXCEPT-COUNT
           ELSE
               DISPLAY 'CDEXCPF WRITE ERROR ST='
                       WS-CDEXCPF-ST
               SET HARD-ERROR TO TRUE
           END-IF.

       9000-FINALIZE.
           IF WS-CDRTNF-ST NOT = SPACES
               CLOSE CDRTNF
               IF WS-CDRTNF-ST NOT = '00'
                   DISPLAY 'CDRTNF CLOSE ERROR ST='
                           WS-CDRTNF-ST
                   SET HARD-ERROR TO TRUE
               END-IF
           END-IF
           IF WS-CDEXCPF-ST NOT = SPACES
               CLOSE CDEXCPF
               IF WS-CDEXCPF-ST NOT = '00'
                   DISPLAY 'CDEXCPF CLOSE ERROR ST='
                           WS-CDEXCPF-ST
                   SET HARD-ERROR TO TRUE
               END-IF
           END-IF
           DISPLAY 'RT COUNT=' WS-RT-COUNT
           DISPLAY 'APPROVED COUNT=' WS-APPROVED-COUNT
           DISPLAY 'UPDATE COUNT=' WS-UPDATE-COUNT
           DISPLAY 'EXCEPTION COUNT=' WS-EXCEPT-COUNT
           DISPLAY 'SKIP COUNT=' WS-SKIP-COUNT
           IF HARD-ERROR
               MOVE 8 TO RETURN-CODE
               DISPLAY 'CB103B ABEND'
           ELSE
               MOVE 0 TO RETURN-CODE
               DISPLAY 'CB103B NORMAL END'
           END-IF.
