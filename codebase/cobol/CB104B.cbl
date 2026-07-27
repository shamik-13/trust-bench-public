       IDENTIFICATION DIVISION.
       PROGRAM-ID. CB104B.
      *---------------------------------------------------------------*
      * 計上後の売上検証ドライバ。                        *
      *---------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CDSALEF ASSIGN TO "CDSALEF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-SL-STAT.
           SELECT CDCARDF ASSIGN TO "CDCARDF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS CF-CARD-NO
               FILE STATUS IS WS-CF-STAT.
           SELECT CDRTEXF ASSIGN TO "CDRTEXF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS FX-CURRENCY-CD
               FILE STATUS IS WS-FX-STAT.
           SELECT CDEXCPF ASSIGN TO "CDEXCPF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-EX-STAT.

       DATA DIVISION.
       FILE SECTION.
       FD  CDSALEF.
           COPY CDSALEFC.
       FD  CDCARDF.
           COPY CDCARD03.
       FD  CDRTEXF.
           COPY CDRTEXC.
       FD  CDEXCPF.
           COPY CDEXCPC.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05 WS-SL-STAT              PIC XX VALUE SPACES.
           05 WS-CF-STAT              PIC XX VALUE SPACES.
           05 WS-FX-STAT              PIC XX VALUE SPACES.
           05 WS-EX-STAT              PIC XX VALUE SPACES.

       01  WS-SWITCHES.
           05 WS-SL-EOF               PIC X VALUE "N".
              88 SL-EOF                     VALUE "Y".
           05 WS-EX-EOF               PIC X VALUE "N".
              88 EX-EOF                     VALUE "Y".
           05 WS-HARD-ERR             PIC X VALUE "N".
              88 HARD-ERROR                 VALUE "Y".
           05 WS-FOUND-UNRESOLVED     PIC X VALUE "N".
              88 FOUND-UNRESOLVED           VALUE "Y".

       01  WS-COUNTERS.
           05 WS-SL-CNT               PIC 9(9) VALUE ZERO.
           05 WS-EX-IN-CNT            PIC 9(9) VALUE ZERO.
           05 WS-EX-OUT-CNT           PIC 9(9) VALUE ZERO.
           05 WS-NG-CNT               PIC 9(9) VALUE ZERO.
           05 WS-ID-SEQ               PIC 9(7) VALUE ZERO.
           05 WS-I                    PIC 9(4) VALUE ZERO.
           05 WS-UNRES-CNT            PIC 9(4) VALUE ZERO.

       01  WS-CONSTANTS.
           05 WS-PGM-ID               PIC X(8) VALUE "CB104B".
           05 WS-BASE-CUR             PIC X(3) VALUE "JPY".
           05 WS-STAT-OPEN            PIC XX VALUE "00".
           05 WS-STAT-EOF             PIC XX VALUE "10".
           05 WS-STAT-NOTFND          PIC XX VALUE "23".
           05 WS-ACT-OPEN             PIC X(2) VALUE "00".
           05 WS-FX-OK                PIC X VALUE "0".

       01  WS-DATE-AREA.
           05 WS-CUR-DATE             PIC 9(8) VALUE ZERO.

       01  WS-NEW-EX-ID.
           05 WS-NEW-EX-PGM           PIC X(3) VALUE "VLD".
           05 WS-NEW-EX-DATE          PIC 9(8) VALUE ZERO.
           05 WS-NEW-EX-SEQ           PIC 9(7) VALUE ZERO.

       01  WS-UNRES-TABLE.
           05 WS-UNRES-ITEM OCCURS 500 TIMES.
              10 WS-UNRES-SALE-ID     PIC X(20).
              10 WS-UNRES-CARD-NO     PIC X(19).
              10 WS-UNRES-REASON      PIC X(4).

       01  WS-DISPLAY-AREA.
           05 WS-DISP-CNT             PIC Z(9).
           05 WS-DISP-ST              PIC XX.
           05 WS-DISP-ID              PIC X(20).

           COPY LK-FXFEE-PARM.

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE 0 TO RETURN-CODE
           ACCEPT WS-CUR-DATE FROM DATE YYYYMMDD
           MOVE WS-CUR-DATE TO WS-NEW-EX-DATE
           PERFORM 1000-OPEN-FILES
           IF NOT HARD-ERROR
              PERFORM 2000-LOAD-UNRESOLVED
           END-IF
           IF NOT HARD-ERROR
              CLOSE CDEXCPF
              MOVE "N" TO WS-EX-EOF
              PERFORM 2100-OPEN-EX-EXTEND
           END-IF
           IF NOT HARD-ERROR
              PERFORM 3000-SCAN-SALES
                 UNTIL SL-EOF OR HARD-ERROR
           END-IF
           PERFORM 9000-CLOSE-FILES
           IF HARD-ERROR
              MOVE 8 TO RETURN-CODE
           ELSE
              MOVE 0 TO RETURN-CODE
           END-IF
           GOBACK.

       1000-OPEN-FILES.
           OPEN INPUT CDSALEF
           IF WS-SL-STAT NOT = WS-STAT-OPEN
              MOVE WS-SL-STAT TO WS-DISP-ST
              DISPLAY "CDSALEF オープン失敗 ST=" WS-DISP-ST
              SET HARD-ERROR TO TRUE
           END-IF
           IF NOT HARD-ERROR
              OPEN INPUT CDCARDF
              IF WS-CF-STAT NOT = WS-STAT-OPEN
                 MOVE WS-CF-STAT TO WS-DISP-ST
                 DISPLAY "CDCARDF オープン失敗 ST=" WS-DISP-ST
                 SET HARD-ERROR TO TRUE
              END-IF
           END-IF
           IF NOT HARD-ERROR
              OPEN INPUT CDRTEXF
              IF WS-FX-STAT NOT = WS-STAT-OPEN
                 MOVE WS-FX-STAT TO WS-DISP-ST
                 DISPLAY "CDRTEXF オープン失敗 ST=" WS-DISP-ST
                 SET HARD-ERROR TO TRUE
              END-IF
           END-IF
           IF NOT HARD-ERROR
              OPEN INPUT CDEXCPF
              IF WS-EX-STAT NOT = WS-STAT-OPEN
                 MOVE WS-EX-STAT TO WS-DISP-ST
                 DISPLAY "CDEXCPF オープン失敗 ST=" WS-DISP-ST
                 SET HARD-ERROR TO TRUE
              END-IF
           END-IF.

       2000-LOAD-UNRESOLVED.
           PERFORM 2010-READ-EXCEPTION
           PERFORM UNTIL EX-EOF OR HARD-ERROR
              ADD 1 TO WS-EX-IN-CNT
              IF EX-ACTION-STATUS = WS-ACT-OPEN
                 IF WS-UNRES-CNT < 500
                    ADD 1 TO WS-UNRES-CNT
                    MOVE EX-SALE-ID
                      TO WS-UNRES-SALE-ID(WS-UNRES-CNT)
                    MOVE EX-CARD-NO
                      TO WS-UNRES-CARD-NO(WS-UNRES-CNT)
                    MOVE EX-REASON-CD
                      TO WS-UNRES-REASON(WS-UNRES-CNT)
                 ELSE
                    DISPLAY "未処理テーブル満杯"
                    SET HARD-ERROR TO TRUE
                 END-IF
              END-IF
              IF NOT HARD-ERROR
                 PERFORM 2010-READ-EXCEPTION
              END-IF
           END-PERFORM.

       2010-READ-EXCEPTION.
           READ CDEXCPF
              AT END
                 SET EX-EOF TO TRUE
              NOT AT END
                 CONTINUE
           END-READ
           IF WS-EX-STAT NOT = WS-STAT-OPEN
              AND WS-EX-STAT NOT = WS-STAT-EOF
              MOVE WS-EX-STAT TO WS-DISP-ST
              DISPLAY "CDEXCPF 読込失敗 ST=" WS-DISP-ST
              SET HARD-ERROR TO TRUE
           END-IF.

       2100-OPEN-EX-EXTEND.
           OPEN EXTEND CDEXCPF
           IF WS-EX-STAT NOT = WS-STAT-OPEN
              MOVE WS-EX-STAT TO WS-DISP-ST
              DISPLAY "CDEXCPF 拡張失敗 ST=" WS-DISP-ST
              SET HARD-ERROR TO TRUE
           END-IF.

       3000-SCAN-SALES.
           READ CDSALEF
              AT END
                 SET SL-EOF TO TRUE
              NOT AT END
                 ADD 1 TO WS-SL-CNT
                 PERFORM 3100-VALIDATE-SALE
           END-READ
           IF WS-SL-STAT NOT = WS-STAT-OPEN
              AND WS-SL-STAT NOT = WS-STAT-EOF
              MOVE WS-SL-STAT TO WS-DISP-ST
              DISPLAY "CDSALEF 読込失敗 ST=" WS-DISP-ST
              SET HARD-ERROR TO TRUE
           END-IF.

       3100-VALIDATE-SALE.
           MOVE SL-SALE-ID TO WS-DISP-ID
           PERFORM 3200-CHECK-UNRESOLVED
           MOVE SL-CARD-NO TO CF-CARD-NO
           READ CDCARDF
              INVALID KEY
                 PERFORM 4100-WRITE-CARD-EXCEPTION
              NOT INVALID KEY
                 PERFORM 3300-CHECK-CARD-STATUS
           END-READ
           IF WS-CF-STAT NOT = WS-STAT-OPEN
              AND WS-CF-STAT NOT = WS-STAT-NOTFND
              MOVE WS-CF-STAT TO WS-DISP-ST
              DISPLAY "CDCARDF 読込失敗 ST=" WS-DISP-ST
              SET HARD-ERROR TO TRUE
           END-IF
           IF NOT HARD-ERROR
              IF SL-CURRENCY-CD NOT = WS-BASE-CUR
                 PERFORM 3400-CHECK-FOREIGN-FEE
              END-IF
           END-IF.

       3200-CHECK-UNRESOLVED.
           MOVE "N" TO WS-FOUND-UNRESOLVED
           PERFORM VARYING WS-I FROM 1 BY 1
              UNTIL WS-I > WS-UNRES-CNT
              IF WS-UNRES-SALE-ID(WS-I) = SL-SALE-ID
                 AND WS-UNRES-CARD-NO(WS-I) = SL-CARD-NO
                 SET FOUND-UNRESOLVED TO TRUE
              END-IF
           END-PERFORM
           IF FOUND-UNRESOLVED
              PERFORM 4300-WRITE-UNRES-EXCEPTION
           END-IF.

       3300-CHECK-CARD-STATUS.
           EVALUATE CF-CARD-STATUS
              WHEN "01"
                 CONTINUE
              WHEN "02"
                 PERFORM 4200-WRITE-STOP-EXCEPTION
              WHEN "03"
                 PERFORM 4210-WRITE-CLOSE-EXCEPTION
              WHEN "09"
                 PERFORM 4220-WRITE-DELAY-EXCEPTION
              WHEN OTHER
                 PERFORM 4230-WRITE-STAT-EXCEPTION
           END-EVALUATE.

       3400-CHECK-FOREIGN-FEE.
           MOVE SL-CURRENCY-CD TO FX-CURRENCY-CD
           READ CDRTEXF
              INVALID KEY
                 PERFORM 4400-WRITE-RATE-EXCEPTION
              NOT INVALID KEY
                 IF FX-APPLY-STATUS NOT = "1"
                    PERFORM 4410-WRITE-RATE-EXCEPTION
                 ELSE
                    PERFORM 3500-CALL-FXFEE
                 END-IF
           END-READ
           IF WS-FX-STAT NOT = WS-STAT-OPEN
              AND WS-FX-STAT NOT = WS-STAT-NOTFND
              MOVE WS-FX-STAT TO WS-DISP-ST
              DISPLAY "CDRTEXF READ ERROR ST=" WS-DISP-ST
              SET HARD-ERROR TO TRUE
           END-IF.

       3500-CALL-FXFEE.
           INITIALIZE LK-FXFEE-PARM
           MOVE SL-SALE-AMT    TO LK-FX-SALE-AMT
           MOVE SL-CURRENCY-CD TO LK-FX-CURRENCY
           CALL "CB590S" USING LK-FXFEE-PARM
           IF LK-FX-RET NOT = WS-FX-OK
              PERFORM 4500-WRITE-FEE-EXCEPTION
           END-IF.

       4100-WRITE-CARD-EXCEPTION.
           DISPLAY "カード未登録 SALE-ID=" WS-DISP-ID
           MOVE "C001" TO EX-REASON-CD
           PERFORM 5000-WRITE-EXCEPTION.

       4200-WRITE-STOP-EXCEPTION.
           DISPLAY "カード利用停止 SALE-ID=" WS-DISP-ID
           MOVE "C002" TO EX-REASON-CD
           PERFORM 5000-WRITE-EXCEPTION.

       4210-WRITE-CLOSE-EXCEPTION.
           DISPLAY "カード解約済 SALE-ID=" WS-DISP-ID
           MOVE "C003" TO EX-REASON-CD
           PERFORM 5000-WRITE-EXCEPTION.

       4220-WRITE-DELAY-EXCEPTION.
           DISPLAY "カード延滞 SALE-ID=" WS-DISP-ID
           MOVE "C009" TO EX-REASON-CD
           PERFORM 5000-WRITE-EXCEPTION.

       4230-WRITE-STAT-EXCEPTION.
           DISPLAY "カード状態異常 SALE-ID=" WS-DISP-ID
           MOVE "C099" TO EX-REASON-CD
           PERFORM 5000-WRITE-EXCEPTION.

       4300-WRITE-UNRES-EXCEPTION.
           DISPLAY "未処理例外 SALE-ID=" WS-DISP-ID
           MOVE "E001" TO EX-REASON-CD
           PERFORM 5000-WRITE-EXCEPTION.

       4400-WRITE-RATE-EXCEPTION.
           DISPLAY "レート未登録 SALE-ID=" WS-DISP-ID
           MOVE "F001" TO EX-REASON-CD
           PERFORM 5000-WRITE-EXCEPTION.

       4410-WRITE-RATE-EXCEPTION.
           DISPLAY "レート無効 SALE-ID=" WS-DISP-ID
           MOVE "F002" TO EX-REASON-CD
           PERFORM 5000-WRITE-EXCEPTION.

       4500-WRITE-FEE-EXCEPTION.
           DISPLAY "手数料チェック異常 SALE-ID=" WS-DISP-ID
           MOVE "F003" TO EX-REASON-CD
           PERFORM 5000-WRITE-EXCEPTION.

       5000-WRITE-EXCEPTION.
           ADD 1 TO WS-ID-SEQ
           INITIALIZE CDEXCPF-REC
           MOVE WS-ID-SEQ      TO WS-NEW-EX-SEQ
           MOVE WS-NEW-EX-ID   TO EX-EXCEPTION-ID
           MOVE SL-SALE-ID     TO EX-SALE-ID
           MOVE SL-CARD-NO     TO EX-CARD-NO
           MOVE WS-PGM-ID      TO EX-DETECTED-PGM
           MOVE WS-CUR-DATE    TO EX-EXCEPTION-DT
           MOVE WS-ACT-OPEN    TO EX-ACTION-STATUS
           WRITE CDEXCPF-REC
           IF WS-EX-STAT = WS-STAT-OPEN
              ADD 1 TO WS-EX-OUT-CNT
              ADD 1 TO WS-NG-CNT
           ELSE
              MOVE WS-EX-STAT TO WS-DISP-ST
              DISPLAY "CDEXCPF 書込失敗 ST=" WS-DISP-ST
              SET HARD-ERROR TO TRUE
           END-IF.

       9000-CLOSE-FILES.
           CLOSE CDSALEF
           CLOSE CDCARDF
           CLOSE CDRTEXF
           CLOSE CDEXCPF
           MOVE WS-SL-CNT TO WS-DISP-CNT
           DISPLAY "売上読込件数=" WS-DISP-CNT
           MOVE WS-EX-IN-CNT TO WS-DISP-CNT
           DISPLAY "既存例外件数=" WS-DISP-CNT
           MOVE WS-EX-OUT-CNT TO WS-DISP-CNT
           DISPLAY "新規例外件数=" WS-DISP-CNT
           MOVE WS-NG-CNT TO WS-DISP-CNT
           DISPLAY "エラー件数=" WS-DISP-CNT.
