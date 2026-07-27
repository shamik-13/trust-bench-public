       IDENTIFICATION DIVISION.
       PROGRAM-ID. CB150B.
      ******************************************************************
      *  遅延損害金計算バッチ
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CDOSF
               ASSIGN TO "CDOSF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS OS-CARD-NO
               FILE STATUS IS FS-CDOSF.

           SELECT CDTRRF
               ASSIGN TO "CDTRRF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-CDTRRF.

           SELECT CDLATEF
               ASSIGN TO "CDLATEF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS LAT-CARD-NO
               FILE STATUS IS FS-CDLATEF.

           SELECT CDHISTF
               ASSIGN TO "CDHISTF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS HIS-CARD-NO
               FILE STATUS IS FS-CDHISTF.

       DATA DIVISION.
       FILE SECTION.
       FD  CDOSF.
           COPY CDOSFC.

       FD  CDTRRF.
           COPY CDTRRC.

       FD  CDLATEF.
           COPY CDLATEC.

       FD  CDHISTF.
           COPY CDHISTC.

       WORKING-STORAGE SECTION.
       01  FS-AREA.
           05 FS-CDOSF                 PIC XX VALUE SPACE.
           05 FS-CDTRRF                PIC XX VALUE SPACE.
           05 FS-CDLATEF               PIC XX VALUE SPACE.
           05 FS-CDHISTF               PIC XX VALUE SPACE.

       01  END-FLAGS.
           05 SW-END-OS                PIC X VALUE "N".
              88 END-OS                      VALUE "Y".
           05 SW-END-TRR               PIC X VALUE "N".
              88 END-TRR                     VALUE "Y".

       01  RUN-AREA.
           05 WS-CURRENT-DATE          PIC X(21) VALUE SPACE.
           05 WS-CALC-DT               PIC 9(8) VALUE ZERO.
           05 WS-CALC-DATE             PIC 9(8) VALUE ZERO.
           05 WS-CYCLE-DATE            PIC 9(8) VALUE ZERO.
           05 WS-DELINQ-DAYS           PIC S9(5) COMP-3 VALUE ZERO.
           05 WS-BASE-AMT              PIC S9(13) COMP-3 VALUE ZERO.
           05 WS-NET-BASE-AMT          PIC S9(13) COMP-3 VALUE ZERO.
           05 WS-SETTLED-AMT           PIC S9(13) COMP-3 VALUE ZERO.
           05 WS-ANNUAL-RATE           PIC 9(3)V9(5) COMP-3 VALUE ZERO.
           05 WS-INTEREST-WORK         PIC S9(15)V9(7) COMP-3
                                           VALUE ZERO.
           05 WS-DELAY-INTEREST        PIC S9(13) COMP-3 VALUE ZERO.
           05 WS-PREV-DAYS             PIC S9(5) COMP-3 VALUE ZERO.
           05 WS-OVERLAP-INTEREST      PIC S9(13) COMP-3 VALUE ZERO.
           05 WS-TRR-COUNT             PIC 9(7) COMP-3 VALUE ZERO.
           05 WS-OS-COUNT              PIC 9(7) COMP-3 VALUE ZERO.
           05 WS-DELAY-COUNT           PIC 9(7) COMP-3 VALUE ZERO.
           05 WS-HIST-COUNT            PIC 9(7) COMP-3 VALUE ZERO.
           05 WS-SKIP-COUNT            PIC 9(7) COMP-3 VALUE ZERO.

       01  TRR-TABLE-AREA.
           05 WS-TRR-IDX               PIC 9(5) COMP VALUE ZERO.
           05 WS-TRR-MAX               PIC 9(5) COMP VALUE 20000.
           05 WS-TRR-USED              PIC 9(5) COMP VALUE ZERO.
           05 WS-FOUND-SW              PIC X VALUE "N".
              88 WS-FOUND                    VALUE "Y".
           05 WS-TRR-TABLE OCCURS 20000 TIMES.
              10 TB-CARD-NO            PIC X(20).
              10 TB-SETTLED-AMT        PIC S9(13) COMP-3.
              10 TB-PENDING-SW         PIC X.
              10 TB-LAST-RESULT-ID     PIC X(20).
              10 TB-LAST-RESULT-DT     PIC 9(8).

       01  DATE-CHECK-AREA.
           05 WS-YYYY                  PIC 9(4) VALUE ZERO.
           05 WS-MM                    PIC 9(2) VALUE ZERO.
           05 WS-DD                    PIC 9(2) VALUE ZERO.
           05 WS-DATE-OK-SW            PIC X VALUE "N".
              88 WS-DATE-OK                  VALUE "Y".

       PROCEDURE DIVISION.
       MAIN-SECTION.
           PERFORM 1000-INIT
           PERFORM 2000-LOAD-TRR UNTIL END-TRR
           PERFORM 3000-PROCESS-OS UNTIL END-OS
           PERFORM 9000-FINAL
           GOBACK.

       1000-INIT.
           MOVE 0 TO RETURN-CODE
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE
           MOVE WS-CURRENT-DATE(1:8) TO WS-CALC-DT

           OPEN INPUT CDTRRF
           IF FS-CDTRRF NOT = "00"
              DISPLAY "CDTRRF オープン失敗 ST=" FS-CDTRRF
              MOVE 8 TO RETURN-CODE
              GOBACK
           END-IF

           OPEN INPUT CDOSF
           IF FS-CDOSF NOT = "00"
              DISPLAY "CDOSF オープン失敗 ST=" FS-CDOSF
              MOVE 8 TO RETURN-CODE
              GOBACK
           END-IF

           OPEN OUTPUT CDLATEF
           IF FS-CDLATEF NOT = "00"
              DISPLAY "CDLATEF オープン失敗 ST=" FS-CDLATEF
              MOVE 8 TO RETURN-CODE
              GOBACK
           END-IF

           OPEN OUTPUT CDHISTF
           IF FS-CDHISTF NOT = "00"
              DISPLAY "CDHISTF オープン失敗 ST=" FS-CDHISTF
              MOVE 8 TO RETURN-CODE
              GOBACK
           END-IF

           PERFORM 2100-READ-TRR
           PERFORM 3100-READ-OS.

       2000-LOAD-TRR.
           ADD 1 TO WS-TRR-COUNT

           IF TRR-CARD-NO = SPACE
              DISPLAY "結果カード番号未設定 ID=" TRR-RESULT-ID
              PERFORM 2100-READ-TRR
              EXIT PARAGRAPH
           END-IF

           IF TRR-RESULT-DT NOT NUMERIC
              DISPLAY "結果日不正 ID=" TRR-RESULT-ID
              PERFORM 2100-READ-TRR
              EXIT PARAGRAPH
           END-IF

           MOVE "N" TO WS-FOUND-SW
           PERFORM VARYING WS-TRR-IDX FROM 1 BY 1
             UNTIL WS-TRR-IDX > WS-TRR-USED OR WS-FOUND
              IF TB-CARD-NO(WS-TRR-IDX) = TRR-CARD-NO
                 MOVE "Y" TO WS-FOUND-SW
              END-IF
           END-PERFORM

           IF NOT WS-FOUND
              IF WS-TRR-USED >= WS-TRR-MAX
                 DISPLAY "結果ワークテーブル超過"
                 MOVE 12 TO RETURN-CODE
                 GOBACK
              END-IF
              ADD 1 TO WS-TRR-USED
              MOVE WS-TRR-USED TO WS-TRR-IDX
              MOVE TRR-CARD-NO TO TB-CARD-NO(WS-TRR-IDX)
              MOVE 0 TO TB-SETTLED-AMT(WS-TRR-IDX)
              MOVE "N" TO TB-PENDING-SW(WS-TRR-IDX)
           ELSE
              SUBTRACT 1 FROM WS-TRR-IDX
           END-IF

           IF TRR-RESULT-CD = "00"
              ADD TRR-SETTLED-AMT TO TB-SETTLED-AMT(WS-TRR-IDX)
           ELSE
              IF TRR-RESULT-CD = "10" OR TRR-RESULT-CD = "11"
                 MOVE "Y" TO TB-PENDING-SW(WS-TRR-IDX)
              END-IF
           END-IF

           MOVE TRR-RESULT-ID TO TB-LAST-RESULT-ID(WS-TRR-IDX)
           MOVE TRR-RESULT-DT TO TB-LAST-RESULT-DT(WS-TRR-IDX)

           PERFORM 2100-READ-TRR.

       2100-READ-TRR.
           READ CDTRRF
              AT END
                 SET END-TRR TO TRUE
              NOT AT END
                 CONTINUE
           END-READ
           IF FS-CDTRRF NOT = "00" AND FS-CDTRRF NOT = "10"
              DISPLAY "CDTRRF 読込失敗 ST=" FS-CDTRRF
              MOVE 8 TO RETURN-CODE
              GOBACK
           END-IF.

       3000-PROCESS-OS.
           ADD 1 TO WS-OS-COUNT

           PERFORM 3200-VALIDATE-OS
           IF NOT WS-DATE-OK
              ADD 1 TO WS-SKIP-COUNT
              PERFORM 3100-READ-OS
              EXIT PARAGRAPH
           END-IF

           COMPUTE WS-BASE-AMT =
               OS-FEE-BAL-AMT
             + OS-INTEREST-BAL-AMT
             + OS-PRINCIPAL-BAL-AMT

           IF WS-BASE-AMT <= 0
              ADD 1 TO WS-SKIP-COUNT
              PERFORM 3100-READ-OS
              EXIT PARAGRAPH
           END-IF

           MOVE 0 TO WS-SETTLED-AMT
           MOVE "N" TO WS-FOUND-SW
           PERFORM VARYING WS-TRR-IDX FROM 1 BY 1
             UNTIL WS-TRR-IDX > WS-TRR-USED OR WS-FOUND
              IF TB-CARD-NO(WS-TRR-IDX) = OS-CARD-NO
                 MOVE "Y" TO WS-FOUND-SW
              END-IF
           END-PERFORM

           IF WS-FOUND
              SUBTRACT 1 FROM WS-TRR-IDX
              IF TB-PENDING-SW(WS-TRR-IDX) = "Y"
                 ADD 1 TO WS-SKIP-COUNT
                 PERFORM 3100-READ-OS
                 EXIT PARAGRAPH
              END-IF
              MOVE TB-SETTLED-AMT(WS-TRR-IDX) TO WS-SETTLED-AMT
           END-IF

           COMPUTE WS-NET-BASE-AMT = WS-BASE-AMT - WS-SETTLED-AMT
           IF WS-NET-BASE-AMT <= 0
              ADD 1 TO WS-SKIP-COUNT
              PERFORM 3100-READ-OS
              EXIT PARAGRAPH
           END-IF

           MOVE OS-CYCLE-DT TO WS-CYCLE-DATE
           MOVE WS-CALC-DT TO WS-CALC-DATE
           COMPUTE WS-DELINQ-DAYS =
               FUNCTION INTEGER-OF-DATE(WS-CALC-DATE)
             - FUNCTION INTEGER-OF-DATE(WS-CYCLE-DATE)

           IF WS-DELINQ-DAYS <= 0
              ADD 1 TO WS-SKIP-COUNT
              PERFORM 3100-READ-OS
              EXIT PARAGRAPH
           END-IF

           PERFORM 3300-DECIDE-RATE
           MOVE 0 TO WS-PREV-DAYS
           MOVE 0 TO WS-OVERLAP-INTEREST

           COMPUTE WS-INTEREST-WORK ROUNDED =
               WS-NET-BASE-AMT
             * WS-ANNUAL-RATE
             * WS-DELINQ-DAYS
             / 36500

           COMPUTE WS-DELAY-INTEREST =
               FUNCTION INTEGER(WS-INTEREST-WORK)
             - WS-OVERLAP-INTEREST

           IF WS-DELAY-INTEREST < 0
              MOVE 0 TO WS-DELAY-INTEREST
           END-IF

           PERFORM 3400-WRITE-DELAY
           PERFORM 3500-WRITE-HIST
           PERFORM 3100-READ-OS.

       3100-READ-OS.
           READ CDOSF
              AT END
                 SET END-OS TO TRUE
              NOT AT END
                 CONTINUE
           END-READ
           IF FS-CDOSF NOT = "00" AND FS-CDOSF NOT = "10"
              DISPLAY "CDOSF READ ERROR ST=" FS-CDOSF
              MOVE 8 TO RETURN-CODE
              GOBACK
           END-IF.

       3200-VALIDATE-OS.
           MOVE "N" TO WS-DATE-OK-SW

           IF OS-CARD-NO = SPACE
              DISPLAY "残高カード番号未設定"
              EXIT PARAGRAPH
           END-IF

           IF OS-CYCLE-DT NOT NUMERIC
              DISPLAY "請求サイクル日不正 CARD=" OS-CARD-NO
              EXIT PARAGRAPH
           END-IF

           MOVE OS-CYCLE-DT(1:4) TO WS-YYYY
           MOVE OS-CYCLE-DT(5:2) TO WS-MM
           MOVE OS-CYCLE-DT(7:2) TO WS-DD

           IF WS-YYYY < 2000 OR WS-YYYY > 2099
              DISPLAY "請求サイクル年不正 CARD=" OS-CARD-NO
              EXIT PARAGRAPH
           END-IF

           IF WS-MM < 1 OR WS-MM > 12
              DISPLAY "請求サイクル月不正 CARD=" OS-CARD-NO
              EXIT PARAGRAPH
           END-IF

           IF WS-DD < 1 OR WS-DD > 31
              DISPLAY "請求サイクル日数不正 CARD=" OS-CARD-NO
              EXIT PARAGRAPH
           END-IF

           SET WS-DATE-OK TO TRUE.

       3300-DECIDE-RATE.
           EVALUATE OS-CARD-NO(1:2)
              WHEN "10"
                 MOVE 14.60000 TO WS-ANNUAL-RATE
              WHEN "20"
                 MOVE 12.80000 TO WS-ANNUAL-RATE
              WHEN "30"
                 MOVE 18.00000 TO WS-ANNUAL-RATE
              WHEN OTHER
                 MOVE 15.00000 TO WS-ANNUAL-RATE
           END-EVALUATE.

       3400-WRITE-DELAY.
           INITIALIZE CDLATEF-REC
           MOVE OS-CARD-NO        TO LAT-CARD-NO
           MOVE OS-CYCLE-DT       TO LAT-CYCLE-DT
           MOVE WS-DELINQ-DAYS    TO LAT-DELINQ-DAYS
           MOVE WS-DELAY-INTEREST TO LAT-LATE-INTEREST-AMT
           MOVE WS-NET-BASE-AMT   TO LAT-CALC-BASE-AMT
           MOVE WS-CALC-DT        TO LAT-CALC-DT

           WRITE CDLATEF-REC
           IF FS-CDLATEF NOT = "00"
              DISPLAY "CDLATEF 書込失敗 ST=" FS-CDLATEF
              DISPLAY "CARD=" OS-CARD-NO
              MOVE 8 TO RETURN-CODE
              GOBACK
           END-IF
           ADD 1 TO WS-DELAY-COUNT.

       3500-WRITE-HIST.
           INITIALIZE CDHISTF-REC
           MOVE OS-CARD-NO        TO HIS-CARD-NO
           MOVE "LATECALC"        TO HIS-PAY-ID
           MOVE 1                 TO HIS-EVENT-SEQ
           MOVE "DL"              TO HIS-EVENT-TYPE
           MOVE WS-DELAY-INTEREST TO HIS-EVENT-AMT
           MOVE WS-CALC-DT        TO HIS-EVENT-DT
           MOVE "CB150B"          TO HIS-SOURCE-PROGRAM

           WRITE CDHISTF-REC
           IF FS-CDHISTF NOT = "00"
              DISPLAY "CDHISTF 書込失敗 ST=" FS-CDHISTF
              DISPLAY "CARD=" OS-CARD-NO
              MOVE 8 TO RETURN-CODE
              GOBACK
           END-IF
           ADD 1 TO WS-HIST-COUNT.

       9000-FINAL.
           CLOSE CDTRRF
           IF FS-CDTRRF NOT = "00"
              DISPLAY "CDTRRF クローズ失敗 ST=" FS-CDTRRF
              MOVE 8 TO RETURN-CODE
           END-IF

           CLOSE CDOSF
           IF FS-CDOSF NOT = "00"
              DISPLAY "CDOSF クローズ失敗 ST=" FS-CDOSF
              MOVE 8 TO RETURN-CODE
           END-IF

           CLOSE CDLATEF
           IF FS-CDLATEF NOT = "00"
              DISPLAY "CDLATEF クローズ失敗 ST=" FS-CDLATEF
              MOVE 8 TO RETURN-CODE
           END-IF

           CLOSE CDHISTF
           IF FS-CDHISTF NOT = "00"
              DISPLAY "CDHISTF クローズ失敗 ST=" FS-CDHISTF
              MOVE 8 TO RETURN-CODE
           END-IF

           DISPLAY "遅延損害金計算 終了"
           DISPLAY "結果読込件数=" WS-TRR-COUNT
           DISPLAY "残高読込件数=" WS-OS-COUNT
           DISPLAY "遅延損害金件数=" WS-DELAY-COUNT
           DISPLAY "履歴件数=" WS-HIST-COUNT
           DISPLAY "対象外件数=" WS-SKIP-COUNT

           IF RETURN-CODE NOT = 0
              GOBACK
           END-IF

           MOVE 0 TO RETURN-CODE.
