       IDENTIFICATION DIVISION.
       PROGRAM-ID. CB161S.
       AUTHOR. TRUST-BATCH.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CDREFDF ASSIGN TO "CDREFDF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS REF-REFUND-ID
               FILE STATUS IS WS-REF-ST.
           SELECT CDCANF ASSIGN TO "CDCANF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-CAN-ST.
           SELECT CDEXCPF2 ASSIGN TO "CDEXCPF2"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-EXP-ST.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  CDREFDF.
           COPY CDREFDC.
       FD  CDCANF.
           COPY CDCANC.
       FD  CDEXCPF2.
           COPY CDEXCPF2C.
      *
       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05 WS-REF-ST              PIC XX VALUE SPACE.
           05 WS-CAN-ST              PIC XX VALUE SPACE.
           05 WS-EXP-ST              PIC XX VALUE SPACE.
      *
       01  WS-CONTROL.
           05 WS-REF-EOF             PIC X VALUE 'N'.
              88 REF-EOF                   VALUE 'Y'.
           05 WS-CAN-EOF             PIC X VALUE 'N'.
              88 CAN-EOF                   VALUE 'Y'.
           05 WS-HARD-ERR            PIC X VALUE 'N'.
              88 HARD-ERR                  VALUE 'Y'.
           05 WS-OPENED-REF          PIC X VALUE 'N'.
           05 WS-OPENED-CAN          PIC X VALUE 'N'.
           05 WS-OPENED-EXP          PIC X VALUE 'N'.
      *
       01  WS-INPUT.
           05 WS-IN-PAY-ID           PIC X(20).
           05 WS-IN-CARD-NO          PIC X(16).
           05 WS-IN-REFUND-AMT       PIC S9(11)V99 COMP-3.
      *
       01  WS-JUDGE.
           05 WS-REFUND-FOUND        PIC X VALUE 'N'.
              88 REFUND-FOUND              VALUE 'Y'.
           05 WS-CANCEL-FOUND        PIC X VALUE 'N'.
              88 CANCEL-FOUND              VALUE 'Y'.
           05 WS-ALLOW-FLG           PIC X VALUE 'N'.
           05 WS-REASON-CD           PIC X(4) VALUE SPACE.
           05 WS-CANCEL-AMT          PIC S9(11)V99 COMP-3 VALUE ZERO.
           05 WS-EXCEPTION-AMT       PIC S9(11)V99 COMP-3 VALUE ZERO.
      *
       01  WS-CONSTANT.
           05 WS-MIN-REFUND-AMT      PIC S9(11)V99 COMP-3 VALUE 100.
           05 WS-MAX-REFUND-AMT      PIC S9(11)V99 COMP-3
                                      VALUE 9999999.
           05 WS-PGM-ID              PIC X(8) VALUE 'CB161S'.
      *
       01  WS-DATE-WORK.
           05 WS-CUR-DATE            PIC 9(8).
           05 WS-CUR-TIME            PIC 9(8).
           05 WS-DT-14               PIC 9(14).
      *
       01  WS-SEQ.
           05 WS-EXP-SEQ             PIC 9(6) VALUE ZERO.
           05 WS-EXP-SEQ-X           PIC X(6).
      *
       LINKAGE SECTION.
       01  LK-CB161S-AREA.
           05 LK-PAY-ID              PIC X(20).
           05 LK-CARD-NO             PIC X(16).
           05 LK-REFUND-AMT          PIC S9(11)V99 COMP-3.
           05 LK-REFUND-OK           PIC X.
           05 LK-RESULT-CD           PIC X(4).
      *
       PROCEDURE DIVISION USING LK-CB161S-AREA.
      *
       0000-MAIN.
           PERFORM 1000-INIT
           IF NOT HARD-ERR
               PERFORM 2000-CHECK-REFUND
           END-IF
           IF NOT HARD-ERR
               PERFORM 3000-CHECK-CANCEL
           END-IF
           IF NOT HARD-ERR
               PERFORM 4000-JUDGE
           END-IF
           PERFORM 9000-CLOSE
           GOBACK.
      *
       1000-INIT.
           MOVE 0 TO RETURN-CODE
           MOVE LK-PAY-ID TO WS-IN-PAY-ID
           MOVE LK-CARD-NO TO WS-IN-CARD-NO
           MOVE LK-REFUND-AMT TO WS-IN-REFUND-AMT
           MOVE 'N' TO LK-REFUND-OK
           MOVE SPACE TO LK-RESULT-CD
           ACCEPT WS-CUR-DATE FROM DATE YYYYMMDD
           ACCEPT WS-CUR-TIME FROM TIME
           MOVE WS-CUR-DATE TO WS-DT-14(1:8)
           MOVE WS-CUR-TIME(1:6) TO WS-DT-14(9:6)
           OPEN INPUT CDREFDF
           IF WS-REF-ST = '00'
               MOVE 'Y' TO WS-OPENED-REF
           ELSE
               DISPLAY 'CDREFDF オープン失敗 ST=' WS-REF-ST
               MOVE 8 TO RETURN-CODE
               SET HARD-ERR TO TRUE
           END-IF
           IF NOT HARD-ERR
               OPEN INPUT CDCANF
               IF WS-CAN-ST = '00'
                   MOVE 'Y' TO WS-OPENED-CAN
               ELSE
                   DISPLAY 'CDCANF オープン失敗 ST=' WS-CAN-ST
                   MOVE 8 TO RETURN-CODE
                   SET HARD-ERR TO TRUE
               END-IF
           END-IF
           IF NOT HARD-ERR
               OPEN OUTPUT CDEXCPF2
               IF WS-EXP-ST = '00'
                   MOVE 'Y' TO WS-OPENED-EXP
               ELSE
                   DISPLAY 'CDEXCPF2 オープン失敗 ST=' WS-EXP-ST
                   MOVE 8 TO RETURN-CODE
                   SET HARD-ERR TO TRUE
               END-IF
           END-IF.
      *
       2000-CHECK-REFUND.
           PERFORM UNTIL REF-EOF OR HARD-ERR
               READ CDREFDF NEXT RECORD
                   AT END
                       SET REF-EOF TO TRUE
                   NOT AT END
                       IF WS-REF-ST = '00'
                           PERFORM 2100-TEST-REFUND
                       ELSE
                           DISPLAY 'CDREFDF 読込失敗 ST=' WS-REF-ST
                           MOVE 8 TO RETURN-CODE
                           SET HARD-ERR TO TRUE
                       END-IF
               END-READ
           END-PERFORM.
      *
       2100-TEST-REFUND.
           IF REF-PAY-ID = WS-IN-PAY-ID
              AND REF-CARD-NO = WS-IN-CARD-NO
              AND REF-REFUND-STATUS NOT = '9'
               SET REFUND-FOUND TO TRUE
               MOVE REF-REFUND-AMT TO WS-EXCEPTION-AMT
           END-IF.
      *
       3000-CHECK-CANCEL.
           PERFORM UNTIL CAN-EOF OR HARD-ERR
               READ CDCANF
                   AT END
                       SET CAN-EOF TO TRUE
                   NOT AT END
                       IF WS-CAN-ST = '00'
                           PERFORM 3100-TEST-CANCEL
                       ELSE
                           DISPLAY 'CDCANF 読込失敗 ST=' WS-CAN-ST
                           MOVE 8 TO RETURN-CODE
                           SET HARD-ERR TO TRUE
                       END-IF
               END-READ
           END-PERFORM.
      *
       3100-TEST-CANCEL.
           IF CAN-PAY-ID = WS-IN-PAY-ID
              AND CAN-CARD-NO = WS-IN-CARD-NO
               SET CANCEL-FOUND TO TRUE
               ADD CAN-CANCEL-AMT TO WS-CANCEL-AMT
           END-IF.
      *
       4000-JUDGE.
           EVALUATE TRUE
               WHEN WS-IN-PAY-ID = SPACE
                   MOVE 'N' TO WS-ALLOW-FLG
                   MOVE 'E001' TO WS-REASON-CD
               WHEN WS-IN-CARD-NO = SPACE
                   MOVE 'N' TO WS-ALLOW-FLG
                   MOVE 'E002' TO WS-REASON-CD
               WHEN REFUND-FOUND
                   MOVE 'N' TO WS-ALLOW-FLG
                   MOVE 'DUPL' TO WS-REASON-CD
               WHEN NOT CANCEL-FOUND
                   MOVE 'N' TO WS-ALLOW-FLG
                   MOVE 'NCAN' TO WS-REASON-CD
               WHEN WS-IN-REFUND-AMT < WS-MIN-REFUND-AMT
                   MOVE 'N' TO WS-ALLOW-FLG
                   MOVE 'MINR' TO WS-REASON-CD
               WHEN WS-IN-REFUND-AMT > WS-MAX-REFUND-AMT
                   MOVE 'N' TO WS-ALLOW-FLG
                   MOVE 'MAXR' TO WS-REASON-CD
               WHEN WS-IN-REFUND-AMT > WS-CANCEL-AMT
                   MOVE 'N' TO WS-ALLOW-FLG
                   MOVE 'OVER' TO WS-REASON-CD
               WHEN OTHER
                   MOVE 'Y' TO WS-ALLOW-FLG
                   MOVE '0000' TO WS-REASON-CD
           END-EVALUATE
           MOVE WS-ALLOW-FLG TO LK-REFUND-OK
           MOVE WS-REASON-CD TO LK-RESULT-CD
           IF REFUND-FOUND
               PERFORM 4100-WRITE-EXCEPTION
           END-IF.
      *
       4100-WRITE-EXCEPTION.
           ADD 1 TO WS-EXP-SEQ
           MOVE WS-EXP-SEQ TO WS-EXP-SEQ-X
           MOVE SPACE TO CDEXCPF2-REC
           STRING WS-PGM-ID DELIMITED BY SIZE
                  WS-CUR-DATE DELIMITED BY SIZE
                  WS-EXP-SEQ-X DELIMITED BY SIZE
             INTO EXP-EXCEPTION-ID
           END-STRING
           MOVE WS-IN-PAY-ID TO EXP-PAY-ID
           MOVE WS-IN-CARD-NO TO EXP-CARD-NO
           MOVE WS-REASON-CD TO EXP-EXCEPTION-CD
           MOVE WS-EXCEPTION-AMT TO EXP-EXCEPTION-AMT
           MOVE WS-PGM-ID TO EXP-DETECTED-PROGRAM
           MOVE WS-DT-14 TO EXP-DETECTED-DT
           WRITE CDEXCPF2-REC
           IF WS-EXP-ST NOT = '00'
               DISPLAY 'CDEXCPF2 書込失敗 ST=' WS-EXP-ST
               MOVE 8 TO RETURN-CODE
               SET HARD-ERR TO TRUE
           ELSE
               DISPLAY '二重返金疑い PAY=' WS-IN-PAY-ID
           END-IF.
      *
       9000-CLOSE.
           IF WS-OPENED-REF = 'Y'
               CLOSE CDREFDF
               IF WS-REF-ST NOT = '00'
                   DISPLAY 'CDREFDF クローズ失敗 ST=' WS-REF-ST
                   MOVE 8 TO RETURN-CODE
               END-IF
           END-IF
           IF WS-OPENED-CAN = 'Y'
               CLOSE CDCANF
               IF WS-CAN-ST NOT = '00'
                   DISPLAY 'CDCANF クローズ失敗 ST=' WS-CAN-ST
                   MOVE 8 TO RETURN-CODE
               END-IF
           END-IF
           IF WS-OPENED-EXP = 'Y'
               CLOSE CDEXCPF2
               IF WS-EXP-ST NOT = '00'
                   DISPLAY 'CDEXCPF2 クローズ失敗 ST=' WS-EXP-ST
                   MOVE 8 TO RETURN-CODE
               END-IF
           END-IF.
