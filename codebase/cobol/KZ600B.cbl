       IDENTIFICATION DIVISION.
       PROGRAM-ID. KZ600B.
      * 変更履歴
      * 版数  年月日(和暦)   担当                     概要
      * 1.00  平成28年04月01日 システム部 勘定系チーム 約定変更受付初版
      * 1.01  令和02年10月15日 システム部 勘定系チーム 入力項目妥当性強化
      * 1.02  令和05年06月20日 システム部 勘定系チーム 受付結果出力見直し

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZRSCF ASSIGN TO KZRSCF
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RSC-ST.

           SELECT KZDLQF ASSIGN TO KZDLQF
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-DLQ-ST.

           SELECT KZSCHF ASSIGN TO KZSCHF
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS SCH-ACCT-NO
               FILE STATUS IS WS-SCH-ST.

           SELECT KZAUDF ASSIGN TO KZAUDF
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-AUD-ST.

           SELECT SYSIN-F ASSIGN TO SYSIN
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-SYSIN-ST.

       DATA DIVISION.
       FILE SECTION.
       FD  KZRSCF.
           COPY KZRSCCF.

       FD  KZDLQF.
           COPY KZDLQFC.

       FD  KZSCHF.
           COPY KZSCHCF.

       FD  KZAUDF.
           COPY KZAUDCF.

       FD  SYSIN-F.
       01  SYSIN-REC.
           05 SYSIN-MIN-PAYMENT        PIC 9(09)V99.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05 WS-RSC-ST                PIC X(02) VALUE SPACE.
           05 WS-DLQ-ST                PIC X(02) VALUE SPACE.
           05 WS-SCH-ST                PIC X(02) VALUE SPACE.
           05 WS-AUD-ST                PIC X(02) VALUE SPACE.
           05 WS-SYSIN-ST              PIC X(02) VALUE SPACE.

       01  WS-FLAGS.
           05 WS-RSC-EOF-SW            PIC X VALUE 'N'.
              88 WS-RSC-EOF                 VALUE 'Y'.
           05 WS-DLQ-EOF-SW            PIC X VALUE 'N'.
              88 WS-DLQ-EOF                 VALUE 'Y'.
           05 WS-HARD-ERR-SW           PIC X VALUE 'N'.
              88 WS-HARD-ERR                VALUE 'Y'.
           05 WS-REJECT-SW             PIC X VALUE 'N'.
              88 WS-REJECT                  VALUE 'Y'.
              88 WS-ACCEPT                  VALUE 'N'.

       01  WS-CONTROL.
           05 WS-MIN-PAYMENT-AMT       PIC 9(09)V99 VALUE 0.
           05 WS-REJECT-CD             PIC X(04) VALUE SPACE.
           05 WS-OLD-STATUS            PIC X(02) VALUE SPACE.
           05 WS-NEW-STATUS            PIC X(02) VALUE SPACE.
           05 WS-PROC-DATE             PIC 9(08) VALUE 0.
           05 WS-PROC-DATE-X REDEFINES WS-PROC-DATE PIC X(08).

       01  WS-COUNTERS.
           05 WS-RSC-READ-CNT          PIC 9(09) VALUE 0.
           05 WS-DLQ-READ-CNT          PIC 9(09) VALUE 0.
           05 WS-APPROVE-CNT           PIC 9(09) VALUE 0.
           05 WS-REJECT-CNT            PIC 9(09) VALUE 0.
           05 WS-AUD-WRITE-CNT         PIC 9(09) VALUE 0.

       01  WS-DATE-CALC.
           05 WS-ASOF-Y                PIC 9(04) VALUE 0.
           05 WS-ASOF-M                PIC 9(02) VALUE 0.
           05 WS-ASOF-D                PIC 9(02) VALUE 0.
           05 WS-CYCLE-Y               PIC 9(04) VALUE 0.
           05 WS-CYCLE-M               PIC 9(02) VALUE 0.
           05 WS-MIN-DUE-DT            PIC 9(08) VALUE 0.
           05 WS-MIN-DUE-DT-X REDEFINES WS-MIN-DUE-DT PIC X(08).

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE 0 TO RETURN-CODE
           PERFORM 1000-INITIALIZE
           IF NOT WS-HARD-ERR
               PERFORM 2000-PROCESS UNTIL WS-RSC-EOF OR WS-HARD-ERR
           END-IF
           PERFORM 9000-TERMINATE
           GOBACK.

       1000-INITIALIZE.
           ACCEPT WS-PROC-DATE FROM DATE YYYYMMDD

           OPEN INPUT SYSIN-F
           IF WS-SYSIN-ST NOT = '00'
               DISPLAY 'SYSIN オープン失敗 ST=' WS-SYSIN-ST
               MOVE 'Y' TO WS-HARD-ERR-SW
               MOVE 8 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF

           READ SYSIN-F
               AT END
                   DISPLAY 'SYSIN 未指定'
                   MOVE 'Y' TO WS-HARD-ERR-SW
                   MOVE 8 TO RETURN-CODE
               NOT AT END
                   MOVE SYSIN-MIN-PAYMENT TO WS-MIN-PAYMENT-AMT
           END-READ

           CLOSE SYSIN-F
           IF WS-SYSIN-ST NOT = '00'
               DISPLAY 'SYSIN クローズ失敗 ST=' WS-SYSIN-ST
               MOVE 'Y' TO WS-HARD-ERR-SW
               MOVE 8 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF

           IF WS-HARD-ERR
               EXIT PARAGRAPH
           END-IF

           OPEN INPUT KZRSCF
           IF WS-RSC-ST NOT = '00'
               DISPLAY 'KZRSCF オープン失敗 ST=' WS-RSC-ST
               MOVE 'Y' TO WS-HARD-ERR-SW
               MOVE 8 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF

           OPEN I-O KZDLQF
           IF WS-DLQ-ST NOT = '00'
               DISPLAY 'KZDLQF オープン失敗 ST=' WS-DLQ-ST
               MOVE 'Y' TO WS-HARD-ERR-SW
               MOVE 8 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF

           OPEN I-O KZSCHF
           IF WS-SCH-ST NOT = '00'
               DISPLAY 'KZSCHF オープン失敗 ST=' WS-SCH-ST
               MOVE 'Y' TO WS-HARD-ERR-SW
               MOVE 8 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF

           OPEN OUTPUT KZAUDF
           IF WS-AUD-ST NOT = '00'
               DISPLAY 'KZAUDF オープン失敗 ST=' WS-AUD-ST
               MOVE 'Y' TO WS-HARD-ERR-SW
               MOVE 8 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF

           PERFORM 1100-READ-RSC
           PERFORM 1200-READ-DLQ.

       1100-READ-RSC.
           READ KZRSCF
               AT END
                   MOVE 'Y' TO WS-RSC-EOF-SW
               NOT AT END
                   ADD 1 TO WS-RSC-READ-CNT
           END-READ
           IF WS-RSC-ST NOT = '00' AND WS-RSC-ST NOT = '10'
               DISPLAY 'KZRSCF 読込失敗 ST=' WS-RSC-ST
               MOVE 'Y' TO WS-HARD-ERR-SW
               MOVE 8 TO RETURN-CODE
           END-IF.

       1200-READ-DLQ.
           READ KZDLQF
               AT END
                   MOVE 'Y' TO WS-DLQ-EOF-SW
               NOT AT END
                   ADD 1 TO WS-DLQ-READ-CNT
           END-READ
           IF WS-DLQ-ST NOT = '00' AND WS-DLQ-ST NOT = '10'
               DISPLAY 'KZDLQF 読込失敗 ST=' WS-DLQ-ST
               MOVE 'Y' TO WS-HARD-ERR-SW
               MOVE 8 TO RETURN-CODE
           END-IF.

       2000-PROCESS.
           PERFORM 2100-ALIGN-DLQ
           IF WS-HARD-ERR
               EXIT PARAGRAPH
           END-IF

           MOVE 'N' TO WS-REJECT-SW
           MOVE SPACE TO WS-REJECT-CD
           MOVE SPACE TO WS-OLD-STATUS
           MOVE SPACE TO WS-NEW-STATUS

           IF WS-DLQ-EOF OR RSC-ACCT-NO NOT = DQ-ACCT-NO
               MOVE 'Y' TO WS-REJECT-SW
               MOVE 'R001' TO WS-REJECT-CD
               MOVE '00' TO WS-OLD-STATUS
               MOVE '00' TO WS-NEW-STATUS
           ELSE
               MOVE DQ-CURR-STATUS TO WS-OLD-STATUS
               MOVE DQ-CURR-STATUS TO WS-NEW-STATUS
               PERFORM 2200-VALIDATE-REQUEST
           END-IF

           IF WS-REJECT
               PERFORM 2300-REJECT-REQUEST
           ELSE
               PERFORM 2400-APPLY-REQUEST
           END-IF

           IF NOT WS-HARD-ERR
               PERFORM 1100-READ-RSC
           END-IF.

       2100-ALIGN-DLQ.
           PERFORM UNTIL WS-DLQ-EOF
              OR DQ-ACCT-NO >= RSC-ACCT-NO
              OR WS-HARD-ERR
               PERFORM 1200-READ-DLQ
           END-PERFORM.

       2200-VALIDATE-REQUEST.
           IF DQ-CURR-STATUS = '30'
               MOVE 'Y' TO WS-REJECT-SW
               MOVE 'R002' TO WS-REJECT-CD
               EXIT PARAGRAPH
           END-IF

           IF RSC-APPROVAL-STATUS NOT = 'AP'
               MOVE 'Y' TO WS-REJECT-SW
               MOVE 'R003' TO WS-REJECT-CD
               EXIT PARAGRAPH
           END-IF

           IF RSC-NEW-PAYMENT-AMT < WS-MIN-PAYMENT-AMT
               MOVE 'Y' TO WS-REJECT-SW
               MOVE 'R004' TO WS-REJECT-CD
               EXIT PARAGRAPH
           END-IF

           PERFORM 2210-CALC-MIN-DUE-DATE
           IF RSC-NEW-DUE-DT < WS-MIN-DUE-DT
               MOVE 'Y' TO WS-REJECT-SW
               MOVE 'R005' TO WS-REJECT-CD
           END-IF.

       2210-CALC-MIN-DUE-DATE.
           MOVE DQ-ASOF-DT(1:4) TO WS-ASOF-Y
           MOVE DQ-ASOF-DT(5:2) TO WS-ASOF-M
           MOVE DQ-ASOF-DT(7:2) TO WS-ASOF-D

           MOVE WS-ASOF-Y TO WS-CYCLE-Y
           MOVE WS-ASOF-M TO WS-CYCLE-M
           ADD 1 TO WS-CYCLE-M
           IF WS-CYCLE-M > 12
               MOVE 1 TO WS-CYCLE-M
               ADD 1 TO WS-CYCLE-Y
           END-IF

           MOVE WS-CYCLE-Y TO WS-MIN-DUE-DT-X(1:4)
           MOVE WS-CYCLE-M TO WS-MIN-DUE-DT-X(5:2)
           MOVE WS-ASOF-D TO WS-MIN-DUE-DT-X(7:2).

       2300-REJECT-REQUEST.
           ADD 1 TO WS-REJECT-CNT
           DISPLAY '約定変更却下 口座=' RSC-ACCT-NO
                   ' 理由=' WS-REJECT-CD
           PERFORM 2500-WRITE-AUDIT.

       2400-APPLY-REQUEST.
           MOVE RSC-NEW-DUE-DT TO DQ-DUE-DT
           REWRITE KZDLQF-REC
           IF WS-DLQ-ST NOT = '00'
               DISPLAY 'KZDLQF 更新失敗 ST=' WS-DLQ-ST
                       ' 口座=' DQ-ACCT-NO
               MOVE 'Y' TO WS-HARD-ERR-SW
               MOVE 8 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF

           PERFORM 2410-UPSERT-SCHEDULE
           IF WS-HARD-ERR
               EXIT PARAGRAPH
           END-IF

           ADD 1 TO WS-APPROVE-CNT
           MOVE '10' TO WS-NEW-STATUS
           PERFORM 2500-WRITE-AUDIT.

       2410-UPSERT-SCHEDULE.
           MOVE RSC-ACCT-NO TO SCH-ACCT-NO
           READ KZSCHF
               KEY IS SCH-ACCT-NO
               INVALID KEY
                   PERFORM 2420-INSERT-SCHEDULE
               NOT INVALID KEY
                   PERFORM 2430-UPDATE-SCHEDULE
           END-READ

           IF WS-SCH-ST NOT = '00'
              AND WS-SCH-ST NOT = '23'
               DISPLAY 'KZSCHF 読込失敗 ST=' WS-SCH-ST
                       ' 口座=' RSC-ACCT-NO
               MOVE 'Y' TO WS-HARD-ERR-SW
               MOVE 8 TO RETURN-CODE
           END-IF.

       2420-INSERT-SCHEDULE.
           MOVE RSC-ACCT-NO TO SCH-ACCT-NO
           MOVE RSC-NEW-DUE-DT TO SCH-NEXT-DUE-DT
           MOVE DQ-DUE-DT TO SCH-ORIG-DUE-DT
           MOVE RSC-NEW-DUE-DT TO SCH-REVISED-DUE-DT
           MOVE 1 TO SCH-RESCHEDULE-CNT
           WRITE KZSCHF-REC
           IF WS-SCH-ST NOT = '00'
               DISPLAY 'KZSCHF 追加失敗 ST=' WS-SCH-ST
                       ' 口座=' RSC-ACCT-NO
               MOVE 'Y' TO WS-HARD-ERR-SW
               MOVE 8 TO RETURN-CODE
           END-IF.

       2430-UPDATE-SCHEDULE.
           MOVE RSC-NEW-DUE-DT TO SCH-REVISED-DUE-DT
           MOVE RSC-NEW-DUE-DT TO SCH-NEXT-DUE-DT
           ADD 1 TO SCH-RESCHEDULE-CNT
           REWRITE KZSCHF-REC
           IF WS-SCH-ST NOT = '00'
               DISPLAY 'KZSCHF 更新失敗 ST=' WS-SCH-ST
                       ' 口座=' RSC-ACCT-NO
               MOVE 'Y' TO WS-HARD-ERR-SW
               MOVE 8 TO RETURN-CODE
           END-IF.

       2500-WRITE-AUDIT.
           MOVE RSC-ACCT-NO TO AUD-ACCT-NO
           MOVE WS-PROC-DATE TO AUD-EVENT-DT
           MOVE 'RSC' TO AUD-EVENT-TYPE
           MOVE WS-OLD-STATUS TO AUD-OLD-STATUS
           MOVE WS-NEW-STATUS TO AUD-NEW-STATUS
           MOVE RSC-NEW-PAYMENT-AMT TO AUD-CHANGE-AMT
           WRITE KZAUDF-REC
           IF WS-AUD-ST NOT = '00'
               DISPLAY 'KZAUDF 書込失敗 ST=' WS-AUD-ST
                       ' 口座=' RSC-ACCT-NO
               MOVE 'Y' TO WS-HARD-ERR-SW
               MOVE 8 TO RETURN-CODE
           ELSE
               ADD 1 TO WS-AUD-WRITE-CNT
           END-IF.

       9000-TERMINATE.
           CLOSE KZRSCF
           IF WS-RSC-ST NOT = '00'
              AND WS-RSC-ST NOT = '42'
               DISPLAY 'KZRSCF クローズ失敗 ST=' WS-RSC-ST
               MOVE 'Y' TO WS-HARD-ERR-SW
               MOVE 8 TO RETURN-CODE
           END-IF

           CLOSE KZDLQF
           IF WS-DLQ-ST NOT = '00'
              AND WS-DLQ-ST NOT = '42'
               DISPLAY 'KZDLQF クローズ失敗 ST=' WS-DLQ-ST
               MOVE 'Y' TO WS-HARD-ERR-SW
               MOVE 8 TO RETURN-CODE
           END-IF

           CLOSE KZSCHF
           IF WS-SCH-ST NOT = '00'
              AND WS-SCH-ST NOT = '42'
               DISPLAY 'KZSCHF クローズ失敗 ST=' WS-SCH-ST
               MOVE 'Y' TO WS-HARD-ERR-SW
               MOVE 8 TO RETURN-CODE
           END-IF

           CLOSE KZAUDF
           IF WS-AUD-ST NOT = '00'
              AND WS-AUD-ST NOT = '42'
               DISPLAY 'KZAUDF クローズ失敗 ST=' WS-AUD-ST
               MOVE 'Y' TO WS-HARD-ERR-SW
               MOVE 8 TO RETURN-CODE
           END-IF

           DISPLAY '約定変更受付 件数=' WS-RSC-READ-CNT
                   ' 承認=' WS-APPROVE-CNT
                   ' 却下=' WS-REJECT-CNT
                   ' 監査=' WS-AUD-WRITE-CNT

           IF WS-HARD-ERR
               IF RETURN-CODE = 0
                   MOVE 8 TO RETURN-CODE
               END-IF
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.
