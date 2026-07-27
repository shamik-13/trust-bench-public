       IDENTIFICATION DIVISION.
       PROGRAM-ID. CB213S.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CDMERF ASSIGN TO "CDMERF"
               ORGANIZATION IS INDEXED
               ACCESS MODE  IS DYNAMIC
               RECORD KEY   IS MR-MERCHANT-CODE
               FILE STATUS  IS WS-CDMERF-ST.

       DATA DIVISION.
       FILE SECTION.
       FD  CDMERF.
           COPY CDMERC.

       WORKING-STORAGE SECTION.
       01  WS-CDMERF-ST             PIC XX.
       01  WS-OPEN-FLG              PIC X       VALUE '0'.
           88  WS-CDMERF-OPEN                   VALUE '1'.

       01  WS-WORK.
           05  WS-BASE-CNT          PIC 9(05)   VALUE ZERO.
           05  WS-BASE-AMT          PIC 9(11)   VALUE ZERO.
           05  WS-WORK-AMT          PIC 9(13)   VALUE ZERO.
           05  WS-ADJ-RATE          PIC 9(03)   VALUE 100.
           05  WS-ERR-FLG           PIC X       VALUE '0'.
               88  WS-HARD-ERR                  VALUE '1'.
           05  WS-RSN-CD            PIC X(04)   VALUE SPACE.

       01  WS-MSG.
           05  WS-MSG-HEAD          PIC X(08)   VALUE 'CB213S '.
           05  WS-MSG-TEXT          PIC X(80).

       LINKAGE SECTION.
       01  CB213S-PARM.
           05  LS-MERCHANT-CODE     PIC X(15).
           05  LS-TRAN-TIME         PIC 9(06).
           05  LS-TRAN-COUNT        PIC 9(05).
           05  LS-TRAN-AMOUNT       PIC 9(11).
           05  LS-CNT-THRESHOLD     PIC 9(05).
           05  LS-AMT-THRESHOLD     PIC 9(11).
           05  LS-HANTEI-FLG        PIC X.
           05  LS-REASON-CD         PIC X(04).

       PROCEDURE DIVISION USING CB213S-PARM.
       0000-MAIN.
           MOVE 0 TO RETURN-CODE
           PERFORM 1000-INIT
           IF NOT WS-HARD-ERR
               PERFORM 2000-READ-MERCHANT
           END-IF
           IF NOT WS-HARD-ERR
               PERFORM 3000-SET-BASE
           END-IF
           IF NOT WS-HARD-ERR
               PERFORM 4000-ADJUST-THRESHOLD
           END-IF
           IF NOT WS-HARD-ERR
               PERFORM 5000-SET-FLAG
           END-IF
           PERFORM 9000-FINAL
           GOBACK.

       1000-INIT.
           MOVE ZERO  TO LS-CNT-THRESHOLD
           MOVE ZERO  TO LS-AMT-THRESHOLD
           MOVE '9'   TO LS-HANTEI-FLG
           MOVE SPACE TO LS-REASON-CD
           MOVE '0'   TO WS-ERR-FLG

           IF LS-MERCHANT-CODE = SPACE
               MOVE '1'    TO WS-ERR-FLG
               MOVE 'P001' TO LS-REASON-CD
               DISPLAY WS-MSG-HEAD '加盟店コード未設定'
               MOVE 8 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF

           IF LS-TRAN-TIME NOT NUMERIC
               MOVE '1'    TO WS-ERR-FLG
               MOVE 'P002' TO LS-REASON-CD
               DISPLAY WS-MSG-HEAD '取引時刻属性不正'
               MOVE 8 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF

           IF LS-TRAN-TIME > 235959
               MOVE '1'    TO WS-ERR-FLG
               MOVE 'P003' TO LS-REASON-CD
               DISPLAY WS-MSG-HEAD '取引時刻範囲不正'
               MOVE 8 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF

           OPEN INPUT CDMERF
           IF WS-CDMERF-ST = '00'
               MOVE '1' TO WS-OPEN-FLG
           ELSE
               MOVE '1'    TO WS-ERR-FLG
               MOVE 'F001' TO LS-REASON-CD
               DISPLAY WS-MSG-HEAD 'CDMERF オープン失敗 ST='
                       WS-CDMERF-ST
               MOVE 8 TO RETURN-CODE
           END-IF.

       2000-READ-MERCHANT.
           MOVE LS-MERCHANT-CODE TO MR-MERCHANT-CODE
           READ CDMERF KEY IS MR-MERCHANT-CODE
               INVALID KEY
                   IF WS-CDMERF-ST = '23'
                       MOVE 'M001' TO LS-REASON-CD
                       MOVE '1'    TO LS-HANTEI-FLG
                   ELSE
                       MOVE '1'    TO WS-ERR-FLG
                       MOVE 'F002' TO LS-REASON-CD
                       DISPLAY WS-MSG-HEAD 'CDMERF 読込失敗 ST='
                               WS-CDMERF-ST
                       MOVE 8 TO RETURN-CODE
                   END-IF
           END-READ.

       3000-SET-BASE.
           IF LS-HANTEI-FLG = '1'
               MOVE ZERO TO WS-BASE-CNT
               MOVE ZERO TO WS-BASE-AMT
               EXIT PARAGRAPH
           END-IF

           IF MR-STATUS NOT = '1'
               MOVE 'M002' TO LS-REASON-CD
               MOVE '1'    TO LS-HANTEI-FLG
               EXIT PARAGRAPH
           END-IF

           EVALUATE MR-MCC
               WHEN '6010'
               WHEN '6011'
               WHEN '6051'
                   MOVE 10 TO WS-BASE-CNT
                   MOVE 1500000 TO WS-BASE-AMT
               WHEN '4829'
               WHEN '5933'
                   MOVE 12 TO WS-BASE-CNT
                   MOVE 2000000 TO WS-BASE-AMT
               WHEN '5812'
               WHEN '5814'
               WHEN '5499'
                   MOVE 40 TO WS-BASE-CNT
                   MOVE 800000 TO WS-BASE-AMT
               WHEN '7011'
               WHEN '4722'
                   MOVE 18 TO WS-BASE-CNT
                   MOVE 2500000 TO WS-BASE-AMT
               WHEN OTHER
                   MOVE 25 TO WS-BASE-CNT
                   MOVE 1200000 TO WS-BASE-AMT
           END-EVALUATE

           EVALUATE MR-RISK-RANK
               WHEN 'A'
                   CONTINUE
               WHEN 'B'
                   COMPUTE WS-BASE-CNT = WS-BASE-CNT * 80 / 100
                   COMPUTE WS-BASE-AMT = WS-BASE-AMT * 80 / 100
               WHEN 'C'
                   COMPUTE WS-BASE-CNT = WS-BASE-CNT * 60 / 100
                   COMPUTE WS-BASE-AMT = WS-BASE-AMT * 60 / 100
               WHEN 'D'
                   COMPUTE WS-BASE-CNT = WS-BASE-CNT * 40 / 100
                   COMPUTE WS-BASE-AMT = WS-BASE-AMT * 40 / 100
               WHEN OTHER
                   MOVE 'M003' TO LS-REASON-CD
                   MOVE '1'    TO LS-HANTEI-FLG
           END-EVALUATE.

       4000-ADJUST-THRESHOLD.
           IF LS-HANTEI-FLG = '1'
               MOVE ZERO TO LS-CNT-THRESHOLD
               MOVE ZERO TO LS-AMT-THRESHOLD
               EXIT PARAGRAPH
           END-IF

           MOVE 100 TO WS-ADJ-RATE

           IF LS-TRAN-TIME >= 000000 AND LS-TRAN-TIME <= 055959
               COMPUTE WS-ADJ-RATE = WS-ADJ-RATE * 80 / 100
           END-IF

           IF MR-COUNTRY-CD NOT = 'JP'
               COMPUTE WS-ADJ-RATE = WS-ADJ-RATE * 70 / 100
           END-IF

           COMPUTE LS-CNT-THRESHOLD =
               WS-BASE-CNT * WS-ADJ-RATE / 100

           IF LS-CNT-THRESHOLD < 1
               MOVE 1 TO LS-CNT-THRESHOLD
           END-IF

           COMPUTE WS-WORK-AMT =
               WS-BASE-AMT * WS-ADJ-RATE / 100
           IF WS-WORK-AMT > 99999999999
               MOVE 99999999999 TO LS-AMT-THRESHOLD
           ELSE
               MOVE WS-WORK-AMT TO LS-AMT-THRESHOLD
           END-IF.

       5000-SET-FLAG.
           IF LS-TRAN-COUNT > LS-CNT-THRESHOLD
               MOVE '2'    TO LS-HANTEI-FLG
               MOVE 'V001' TO LS-REASON-CD
               EXIT PARAGRAPH
           END-IF

           IF LS-TRAN-AMOUNT > LS-AMT-THRESHOLD
               MOVE '2'    TO LS-HANTEI-FLG
               MOVE 'V002' TO LS-REASON-CD
               EXIT PARAGRAPH
           END-IF

           MOVE '0'    TO LS-HANTEI-FLG
           MOVE '0000' TO LS-REASON-CD.

       9000-FINAL.
           IF WS-CDMERF-OPEN
               CLOSE CDMERF
               IF WS-CDMERF-ST NOT = '00'
                   DISPLAY WS-MSG-HEAD 'CDMERF クローズ失敗 ST='
                           WS-CDMERF-ST
                   IF RETURN-CODE = 0
                       MOVE 8 TO RETURN-CODE
                   END-IF
               END-IF
           END-IF.
