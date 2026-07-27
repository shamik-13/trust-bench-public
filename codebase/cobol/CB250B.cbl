       IDENTIFICATION DIVISION.
      ******************************************************************
      * 変更履歴
      * 版数  年月日    担当  概要
      * 1.00  20240201  B01   初版作成
      * 1.01  20240515  B02   解約カード未収抽出対応
      * 1.02  20240930  B03   入金消込判定の期日条件追加
      ******************************************************************
       PROGRAM-ID. CB250B.
       DATE-WRITTEN. 20240930.
       DATE-COMPILED.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CDBILLF ASSIGN TO "CDBILLF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-BI-STAT.
           SELECT CDPAYF ASSIGN TO "CDPAYF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-PY-STAT.
           SELECT CDCARDF ASSIGN TO "CDCARDF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CF-CARD-NO
               FILE STATUS IS WS-CF-STAT.
           SELECT CDDELINQF ASSIGN TO "CDDELINQF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS DL-CARD-NO
               FILE STATUS IS WS-DL-STAT.

       DATA DIVISION.
       FILE SECTION.
       FD  CDBILLF.
           COPY CDBILLFC.
       FD  CDPAYF.
           COPY CDPAYC.
       FD  CDCARDF.
           COPY CDCARDFC.
       FD  CDDELINQF.
           COPY CDDLNQC.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05 WS-BI-STAT             PIC XX VALUE SPACE.
           05 WS-PY-STAT             PIC XX VALUE SPACE.
           05 WS-CF-STAT             PIC XX VALUE SPACE.
           05 WS-DL-STAT             PIC XX VALUE SPACE.

       01  WS-SWITCHES.
           05 WS-BI-EOF              PIC X VALUE 'N'.
              88 BI-EOF                    VALUE 'Y'.
              88 BI-NOT-EOF                VALUE 'N'.
           05 WS-PY-EOF              PIC X VALUE 'N'.
              88 PY-EOF                    VALUE 'Y'.
              88 PY-NOT-EOF                VALUE 'N'.
           05 WS-HARD-ERROR          PIC X VALUE 'N'.
              88 HARD-ERROR                VALUE 'Y'.
              88 NO-HARD-ERROR             VALUE 'N'.
           05 WS-CARD-FOUND          PIC X VALUE 'N'.
              88 CARD-FOUND                VALUE 'Y'.
              88 CARD-NOT-FOUND            VALUE 'N'.
           05 WS-DL-FOUND            PIC X VALUE 'N'.
              88 DL-FOUND                  VALUE 'Y'.
              88 DL-NOT-FOUND              VALUE 'N'.

       01  WS-WORK-AREA.
           05 WS-RUN-DATE            PIC X(08) VALUE SPACE.
           05 WS-CURR-DATE           PIC 9(08) VALUE ZERO.
           05 WS-TODAY-INT           PIC S9(09) COMP VALUE ZERO.
           05 WS-DUE-INT             PIC S9(09) COMP VALUE ZERO.
           05 WS-DAYS-PAST-DUE       PIC S9(05) COMP VALUE ZERO.
           05 WS-PAID-AMT            PIC S9(13)V99 COMP-3 VALUE ZERO.
           05 WS-PAST-DUE-AMT        PIC S9(13)V99 COMP-3 VALUE ZERO.
           05 WS-TARGET-AMT          PIC S9(13)V99 COMP-3 VALUE ZERO.
           05 WS-READ-BILL-CNT       PIC 9(09) VALUE ZERO.
           05 WS-READ-PAY-CNT        PIC 9(09) VALUE ZERO.
           05 WS-WRITE-DL-CNT        PIC 9(09) VALUE ZERO.
           05 WS-SKIP-CNT            PIC 9(09) VALUE ZERO.
           05 WS-ERR-CNT             PIC 9(09) VALUE ZERO.
           05 WS-DUNNING-STAGE       PIC X(02) VALUE SPACE.

       01  WS-DATE-EDIT.
           05 WS-DATE-IN             PIC 9(08) VALUE ZERO.
           05 WS-DATE-YYYY           PIC 9(04) VALUE ZERO.
           05 WS-DATE-MM             PIC 9(02) VALUE ZERO.
           05 WS-DATE-DD             PIC 9(02) VALUE ZERO.
           05 WS-DATE-OK             PIC X VALUE 'N'.
              88 DATE-OK                   VALUE 'Y'.
              88 DATE-NG                   VALUE 'N'.

       01  WS-MESSAGE.
           05 WS-MSG-FILE            PIC X(12) VALUE SPACE.
           05 WS-MSG-STATUS          PIC XX VALUE SPACE.

       PROCEDURE DIVISION.
       0000-MAIN SECTION.
       0000-START.
           MOVE 0 TO RETURN-CODE
           PERFORM 1000-INITIALIZE
           IF NO-HARD-ERROR
              PERFORM 2000-MAIN-PROCESS
           END-IF
           PERFORM 9000-FINALIZE
           GOBACK
           .

       1000-INITIALIZE SECTION.
       1000-START.
           MOVE FUNCTION CURRENT-DATE(1:8) TO WS-RUN-DATE
           MOVE WS-RUN-DATE TO WS-CURR-DATE
           MOVE WS-CURR-DATE TO WS-DATE-IN
           PERFORM 8100-CHECK-DATE
           IF DATE-NG
              DISPLAY '処理日付不正 日付=' WS-CURR-DATE
              SET HARD-ERROR TO TRUE
              MOVE 12 TO RETURN-CODE
              EXIT SECTION
           END-IF

           COMPUTE WS-TODAY-INT =
               FUNCTION INTEGER-OF-DATE(WS-CURR-DATE)

           OPEN INPUT CDBILLF
           IF WS-BI-STAT NOT = '00'
              MOVE 'CDBILLF' TO WS-MSG-FILE
              MOVE WS-BI-STAT TO WS-MSG-STATUS
              PERFORM 9100-OPEN-ERROR
              EXIT SECTION
           END-IF

           OPEN INPUT CDCARDF
           IF WS-CF-STAT NOT = '00'
              MOVE 'CDCARDF' TO WS-MSG-FILE
              MOVE WS-CF-STAT TO WS-MSG-STATUS
              PERFORM 9100-OPEN-ERROR
              EXIT SECTION
           END-IF

           OPEN I-O CDDELINQF
           IF WS-DL-STAT NOT = '00'
              MOVE 'CDDELINQF' TO WS-MSG-FILE
              MOVE WS-DL-STAT TO WS-MSG-STATUS
              PERFORM 9100-OPEN-ERROR
              EXIT SECTION
           END-IF
           .

       2000-MAIN-PROCESS SECTION.
       2000-START.
           SET BI-NOT-EOF TO TRUE
           PERFORM 2100-READ-BILL
           PERFORM UNTIL BI-EOF OR HARD-ERROR
              ADD 1 TO WS-READ-BILL-CNT
              PERFORM 2200-PROCESS-BILL
              PERFORM 2100-READ-BILL
           END-PERFORM
           .

       2100-READ-BILL SECTION.
       2100-START.
           READ CDBILLF
              AT END
                 SET BI-EOF TO TRUE
              NOT AT END
                 IF WS-BI-STAT NOT = '00'
                    DISPLAY 'CDBILLF 読込失敗 ST=' WS-BI-STAT
                    SET HARD-ERROR TO TRUE
                    MOVE 12 TO RETURN-CODE
                 END-IF
           END-READ
           .

       2200-PROCESS-BILL SECTION.
       2200-START.
           IF BI-BILL-STATUS(1:1) = 'S'
              ADD 1 TO WS-SKIP-CNT
              EXIT SECTION
           END-IF

           IF BI-BILL-STATUS(1:1) NOT = 'C'
              ADD 1 TO WS-SKIP-CNT
              EXIT SECTION
           END-IF

           MOVE BI-DUE-DT TO WS-DATE-IN
           PERFORM 8100-CHECK-DATE
           IF DATE-NG
              ADD 1 TO WS-ERR-CNT
              DISPLAY '支払期日不正 CARD=' BI-CARD-NO
              DISPLAY '期日=' BI-DUE-DT
              EXIT SECTION
           END-IF

           COMPUTE WS-DUE-INT =
               FUNCTION INTEGER-OF-DATE(BI-DUE-DT)

           IF WS-DUE-INT >= WS-TODAY-INT
              ADD 1 TO WS-SKIP-CNT
              EXIT SECTION
           END-IF

           PERFORM 2300-READ-CARD
           IF CARD-NOT-FOUND
              ADD 1 TO WS-ERR-CNT
              DISPLAY 'カード未検出 CARD=' BI-CARD-NO
              EXIT SECTION
           END-IF

           IF CF-CARD-STATUS = '02'
              ADD 1 TO WS-SKIP-CNT
              EXIT SECTION
           END-IF

           PERFORM 2400-SUM-PAYMENT
           IF HARD-ERROR
              EXIT SECTION
           END-IF

           MOVE BI-BILL-AMT TO WS-TARGET-AMT
           COMPUTE WS-PAST-DUE-AMT = WS-TARGET-AMT - WS-PAID-AMT

           IF WS-PAST-DUE-AMT <= ZERO
              ADD 1 TO WS-SKIP-CNT
              EXIT SECTION
           END-IF

           COMPUTE WS-DAYS-PAST-DUE = WS-TODAY-INT - WS-DUE-INT
           PERFORM 2500-SET-DUNNING-STAGE
           PERFORM 2600-WRITE-DELINQ
           .

       2300-READ-CARD SECTION.
       2300-START.
           SET CARD-NOT-FOUND TO TRUE
           MOVE BI-CARD-NO TO CF-CARD-NO

           READ CDCARDF KEY IS CF-CARD-NO
              INVALID KEY
                 SET CARD-NOT-FOUND TO TRUE
              NOT INVALID KEY
                 SET CARD-FOUND TO TRUE
           END-READ

           IF WS-CF-STAT NOT = '00' AND WS-CF-STAT NOT = '23'
              DISPLAY 'CDCARDF 読込失敗 ST=' WS-CF-STAT
              DISPLAY 'CARD=' BI-CARD-NO
              SET HARD-ERROR TO TRUE
              MOVE 12 TO RETURN-CODE
           END-IF
           .

       2400-SUM-PAYMENT SECTION.
       2400-START.
           MOVE ZERO TO WS-PAID-AMT
           SET PY-NOT-EOF TO TRUE

           OPEN INPUT CDPAYF
           IF WS-PY-STAT NOT = '00'
              DISPLAY 'CDPAYF オープン失敗 ST=' WS-PY-STAT
              SET HARD-ERROR TO TRUE
              MOVE 12 TO RETURN-CODE
              EXIT SECTION
           END-IF

           PERFORM 2410-READ-PAY
           PERFORM UNTIL PY-EOF OR HARD-ERROR
              ADD 1 TO WS-READ-PAY-CNT
              IF PY-CARD-NO = BI-CARD-NO
                 IF PY-ALLOC-STATUS(1:1) = 'A'
                    IF PY-RECEIVED-DT <= WS-CURR-DATE
                       ADD PY-PAY-AMT TO WS-PAID-AMT
                    END-IF
                 END-IF
              END-IF
              PERFORM 2410-READ-PAY
           END-PERFORM

           CLOSE CDPAYF
           IF WS-PY-STAT NOT = '00'
              DISPLAY 'CDPAYF クローズ失敗 ST=' WS-PY-STAT
              SET HARD-ERROR TO TRUE
              MOVE 12 TO RETURN-CODE
           END-IF
           .

       2410-READ-PAY SECTION.
       2410-START.
           READ CDPAYF
              AT END
                 SET PY-EOF TO TRUE
              NOT AT END
                 IF WS-PY-STAT NOT = '00'
                    DISPLAY 'CDPAYF 読込失敗 ST=' WS-PY-STAT
                    SET HARD-ERROR TO TRUE
                    MOVE 12 TO RETURN-CODE
                 END-IF
           END-READ
           .

       2500-SET-DUNNING-STAGE SECTION.
       2500-START.
           EVALUATE TRUE
              WHEN WS-DAYS-PAST-DUE <= 10
                 MOVE '01' TO WS-DUNNING-STAGE
              WHEN WS-DAYS-PAST-DUE <= 30
                 MOVE '02' TO WS-DUNNING-STAGE
              WHEN WS-DAYS-PAST-DUE <= 60
                 MOVE '03' TO WS-DUNNING-STAGE
              WHEN OTHER
                 MOVE '04' TO WS-DUNNING-STAGE
           END-EVALUATE
           .

       2600-WRITE-DELINQ SECTION.
       2600-START.
           SET DL-NOT-FOUND TO TRUE
           MOVE BI-CARD-NO TO DL-CARD-NO

           READ CDDELINQF KEY IS DL-CARD-NO
              INVALID KEY
                 SET DL-NOT-FOUND TO TRUE
              NOT INVALID KEY
                 SET DL-FOUND TO TRUE
           END-READ

           IF WS-DL-STAT NOT = '00' AND WS-DL-STAT NOT = '23'
              DISPLAY 'CDDELINQF 読込失敗 ST=' WS-DL-STAT
              DISPLAY 'CARD=' BI-CARD-NO
              SET HARD-ERROR TO TRUE
              MOVE 12 TO RETURN-CODE
              EXIT SECTION
           END-IF

           MOVE BI-CARD-NO        TO DL-CARD-NO
           MOVE BI-CYCLE-DT       TO DL-CYCLE-DT
           MOVE WS-DAYS-PAST-DUE  TO DL-DAYS-PAST-DUE
           MOVE WS-PAST-DUE-AMT   TO DL-PAST-DUE-AMT
           MOVE WS-DUNNING-STAGE  TO DL-DUNNING-STAGE
           MOVE WS-CURR-DATE      TO DL-EXTRACT-DT

           IF DL-FOUND
              REWRITE CDDELINQF-REC
              IF WS-DL-STAT NOT = '00'
                 DISPLAY 'CDDELINQF 更新失敗 ST=' WS-DL-STAT
                 DISPLAY 'CARD=' BI-CARD-NO
                 SET HARD-ERROR TO TRUE
                 MOVE 12 TO RETURN-CODE
              ELSE
                 ADD 1 TO WS-WRITE-DL-CNT
              END-IF
           ELSE
              WRITE CDDELINQF-REC
              IF WS-DL-STAT NOT = '00'
                 DISPLAY 'CDDELINQF 書込失敗 ST=' WS-DL-STAT
                 DISPLAY 'CARD=' BI-CARD-NO
                 SET HARD-ERROR TO TRUE
                 MOVE 12 TO RETURN-CODE
              ELSE
                 ADD 1 TO WS-WRITE-DL-CNT
              END-IF
           END-IF

           IF NO-HARD-ERROR
              IF CF-CARD-STATUS = '03'
                 DISPLAY '解約カード延滞 CARD=' BI-CARD-NO
                 DISPLAY '延滞額=' WS-PAST-DUE-AMT
              ELSE
                 DISPLAY '延滞更新 CARD=' BI-CARD-NO
                 DISPLAY '督促段階=' WS-DUNNING-STAGE
              END-IF
           END-IF
           .

       8100-CHECK-DATE SECTION.
       8100-START.
           SET DATE-NG TO TRUE

           IF WS-DATE-IN NOT NUMERIC
              EXIT SECTION
           END-IF

           MOVE WS-DATE-IN(1:4) TO WS-DATE-YYYY
           MOVE WS-DATE-IN(5:2) TO WS-DATE-MM
           MOVE WS-DATE-IN(7:2) TO WS-DATE-DD

           IF WS-DATE-YYYY < 1900
              EXIT SECTION
           END-IF

           IF WS-DATE-MM < 1 OR WS-DATE-MM > 12
              EXIT SECTION
           END-IF

           IF WS-DATE-DD < 1 OR WS-DATE-DD > 31
              EXIT SECTION
           END-IF

           EVALUATE WS-DATE-MM
              WHEN 4
              WHEN 6
              WHEN 9
              WHEN 11
                 IF WS-DATE-DD > 30
                    EXIT SECTION
                 END-IF
              WHEN 2
                 IF FUNCTION MOD(WS-DATE-YYYY 400) = 0
                    IF WS-DATE-DD > 29
                       EXIT SECTION
                    END-IF
                 ELSE
                    IF FUNCTION MOD(WS-DATE-YYYY 100) = 0
                       IF WS-DATE-DD > 28
                          EXIT SECTION
                       END-IF
                    ELSE
                       IF FUNCTION MOD(WS-DATE-YYYY 4) = 0
                          IF WS-DATE-DD > 29
                             EXIT SECTION
                          END-IF
                       ELSE
                          IF WS-DATE-DD > 28
                             EXIT SECTION
                          END-IF
                       END-IF
                    END-IF
                 END-IF
           END-EVALUATE

           SET DATE-OK TO TRUE
           .

       9000-FINALIZE SECTION.
       9000-START.
           IF WS-BI-STAT NOT = SPACE
              CLOSE CDBILLF
              IF WS-BI-STAT NOT = '00'
                 IF WS-BI-STAT NOT = '42'
                    DISPLAY 'CDBILLF クローズ失敗 ST=' WS-BI-STAT
                    SET HARD-ERROR TO TRUE
                    MOVE 12 TO RETURN-CODE
                 END-IF
              END-IF
           END-IF

           IF WS-CF-STAT NOT = SPACE
              CLOSE CDCARDF
              IF WS-CF-STAT NOT = '00'
                 IF WS-CF-STAT NOT = '42'
                    DISPLAY 'CDCARDF クローズ失敗 ST=' WS-CF-STAT
                    SET HARD-ERROR TO TRUE
                    MOVE 12 TO RETURN-CODE
                 END-IF
              END-IF
           END-IF

           IF WS-DL-STAT NOT = SPACE
              CLOSE CDDELINQF
              IF WS-DL-STAT NOT = '00'
                 IF WS-DL-STAT NOT = '42'
                    DISPLAY 'CDDELINQF CLOSE ST=' WS-DL-STAT
                    SET HARD-ERROR TO TRUE
                    MOVE 12 TO RETURN-CODE
                 END-IF
              END-IF
           END-IF

           DISPLAY 'CB250B 終了 請求読込件数=' WS-READ-BILL-CNT
           DISPLAY '入金読込件数=' WS-READ-PAY-CNT
           DISPLAY '延滞書込件数=' WS-WRITE-DL-CNT
           DISPLAY '対象外件数=' WS-SKIP-CNT
           DISPLAY '警告件数=' WS-ERR-CNT

           IF HARD-ERROR
              IF RETURN-CODE = 0
                 MOVE 8 TO RETURN-CODE
              END-IF
           ELSE
              MOVE 0 TO RETURN-CODE
           END-IF
           .

       9100-OPEN-ERROR SECTION.
       9100-START.
           DISPLAY WS-MSG-FILE ' オープン失敗 ST=' WS-MSG-STATUS
           SET HARD-ERROR TO TRUE
           MOVE 12 TO RETURN-CODE
           .
