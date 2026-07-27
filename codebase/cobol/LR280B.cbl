       IDENTIFICATION DIVISION.
       PROGRAM-ID. LR280B.
       AUTHOR. みらい生命 システム部.
      *
      * 保険料試算結果控え出力
      * 試算用ＰＲＭを抽出し、契約登録前の見積控えを出力する。
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LFPRMF ASSIGN TO "LFPRMF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-LFPRMF-ST.
           SELECT LFPOLF ASSIGN TO "LFPOLF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-LFPOLF-ST.
           SELECT LRRPTF ASSIGN TO "LRRPTF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-LRRPTF-ST.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  LFPRMF.
           COPY LFPRMFC.
      *
       FD  LFPOLF.
           COPY LFPOLFC.
      *
       FD  LRRPTF.
           COPY LRRPTFC.
      *
       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05 WS-LFPRMF-ST             PIC XX VALUE SPACES.
           05 WS-LFPOLF-ST             PIC XX VALUE SPACES.
           05 WS-LRRPTF-ST             PIC XX VALUE SPACES.
      *
       01  WS-SWITCHES.
           05 WS-LFPRMF-EOF-SW         PIC X VALUE "N".
              88 LFPRMF-EOF                 VALUE "Y".
           05 WS-LFPOLF-EOF-SW         PIC X VALUE "N".
              88 LFPOLF-EOF                 VALUE "Y".
           05 WS-HARD-ERR-SW           PIC X VALUE "N".
              88 HARD-ERROR                 VALUE "Y".
           05 WS-POL-FOUND-SW          PIC X VALUE "N".
              88 POL-FOUND                  VALUE "Y".
      *
       01  WS-COUNTERS.
           05 WS-PRM-READ-CNT          PIC 9(9) VALUE ZERO.
           05 WS-POL-READ-CNT          PIC 9(9) VALUE ZERO.
           05 WS-RPT-WRITE-CNT         PIC 9(9) VALUE ZERO.
           05 WS-SKIP-CNT              PIC 9(9) VALUE ZERO.
           05 WS-LINE-NO               PIC 9(7) VALUE ZERO.
           05 WS-POL-IDX               PIC 9(5) VALUE ZERO.
           05 WS-POL-MAX               PIC 9(5) VALUE ZERO.
           05 WS-POL-SUB               PIC 9(5) VALUE ZERO.
      *
       01  WS-RUN-DATA.
           05 WS-CURRENT-DATE          PIC X(21) VALUE SPACES.
           05 WS-REPORT-YM             PIC 9(6) VALUE ZERO.
           05 WS-REPORT-ID-WORK        PIC X(20) VALUE SPACES.
           05 WS-AMT-WORK              PIC S9(13) VALUE ZERO.
           05 WS-AGE-BAND              PIC XX VALUE SPACES.
           05 WS-REASON-CD             PIC X(4) VALUE SPACES.
      *
       01  WS-POL-TABLE.
           05 WS-POL-ENTRY OCCURS 5000 TIMES.
              10 T-PO-POL-NO           PIC X(20).
              10 T-PO-AGE              PIC 9(3).
              10 T-PO-SEX-KBN          PIC X.
              10 T-PO-SUM-AMT          PIC S9(13).
              10 T-PO-STATUS-KBN       PIC XX.
      *
       01  WS-CONSTANTS.
           05 C-YES                    PIC X VALUE "Y".
           05 C-NO                     PIC X VALUE "N".
           05 C-PRM-OK                 PIC X VALUE "1".
           05 C-STATUS-VALID           PIC XX VALUE "01".
           05 C-RPT-TYPE               PIC X VALUE "M".
           05 C-RPT-OUT-OK             PIC X VALUE "0".
      *
       PROCEDURE DIVISION.
       MAIN-RTN.
           MOVE 0 TO RETURN-CODE
           PERFORM 1000-INIT
           IF NOT HARD-ERROR
              PERFORM 2000-LOAD-POLICY
           END-IF
           IF NOT HARD-ERROR
              PERFORM 3000-PROCESS-PRM
           END-IF
           PERFORM 9000-FINAL
           GOBACK.
      *
       1000-INIT.
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE
           MOVE WS-CURRENT-DATE(1:6) TO WS-REPORT-YM
      *
           OPEN INPUT LFPOLF
           IF WS-LFPOLF-ST NOT = "00"
              DISPLAY "LFPOLF オープン失敗 ST=" WS-LFPOLF-ST
              MOVE C-YES TO WS-HARD-ERR-SW
              MOVE 8 TO RETURN-CODE
           END-IF
      *
           IF NOT HARD-ERROR
              OPEN INPUT LFPRMF
              IF WS-LFPRMF-ST NOT = "00"
                 DISPLAY "LFPRMF オープン失敗 ST=" WS-LFPRMF-ST
                 MOVE C-YES TO WS-HARD-ERR-SW
                 MOVE 8 TO RETURN-CODE
              END-IF
           END-IF
      *
           IF NOT HARD-ERROR
              OPEN OUTPUT LRRPTF
              IF WS-LRRPTF-ST NOT = "00"
                 DISPLAY "LRRPTF オープン失敗 ST=" WS-LRRPTF-ST
                 MOVE C-YES TO WS-HARD-ERR-SW
                 MOVE 8 TO RETURN-CODE
              END-IF
           END-IF.
      *
       2000-LOAD-POLICY.
           PERFORM UNTIL LFPOLF-EOF OR HARD-ERROR
              READ LFPOLF
                 AT END
                    MOVE C-YES TO WS-LFPOLF-EOF-SW
                 NOT AT END
                    IF WS-LFPOLF-ST = "00"
                       ADD 1 TO WS-POL-READ-CNT
                       PERFORM 2100-STORE-POLICY
                    ELSE
                       DISPLAY "LFPOLF 読込失敗 ST=" WS-LFPOLF-ST
                       MOVE C-YES TO WS-HARD-ERR-SW
                       MOVE 8 TO RETURN-CODE
                    END-IF
              END-READ
           END-PERFORM.
      *
       2100-STORE-POLICY.
           IF WS-POL-MAX >= 5000
              DISPLAY "LFPOLF 保持件数超過"
              MOVE C-YES TO WS-HARD-ERR-SW
              MOVE 12 TO RETURN-CODE
           ELSE
              ADD 1 TO WS-POL-MAX
              MOVE PO-POL-NO TO T-PO-POL-NO(WS-POL-MAX)
              MOVE PO-ENTRY-AGE-CNT TO T-PO-AGE(WS-POL-MAX)
              MOVE PO-SEX-KBN TO T-PO-SEX-KBN(WS-POL-MAX)
              MOVE PO-SUM-ASSURED-AMT TO T-PO-SUM-AMT(WS-POL-MAX)
              MOVE PO-POL-STATUS-KBN TO T-PO-STATUS-KBN(WS-POL-MAX)
           END-IF.
      *
       3000-PROCESS-PRM.
           PERFORM UNTIL LFPRMF-EOF OR HARD-ERROR
              READ LFPRMF
                 AT END
                    MOVE C-YES TO WS-LFPRMF-EOF-SW
                 NOT AT END
                    IF WS-LFPRMF-ST = "00"
                       ADD 1 TO WS-PRM-READ-CNT
                       PERFORM 3100-EDIT-PRM
                    ELSE
                       DISPLAY "LFPRMF 読込失敗 ST=" WS-LFPRMF-ST
                       MOVE C-YES TO WS-HARD-ERR-SW
                       MOVE 8 TO RETURN-CODE
                    END-IF
              END-READ
           END-PERFORM.
      *
       3100-EDIT-PRM.
           MOVE SPACES TO WS-REASON-CD
           MOVE SPACES TO WS-AGE-BAND
           IF PR-PRM-ID = SPACES
              MOVE "P001" TO WS-REASON-CD
           END-IF
           IF WS-REASON-CD = SPACES
              IF PR-CALC-STATUS-KBN NOT = C-PRM-OK
                 MOVE "P002" TO WS-REASON-CD
              END-IF
           END-IF
           IF WS-REASON-CD = SPACES
              IF PR-PRM-AMT <= ZERO
                 MOVE "P003" TO WS-REASON-CD
              END-IF
           END-IF
           IF WS-REASON-CD = SPACES
              PERFORM 3200-FIND-POLICY
              IF NOT POL-FOUND
                 MOVE "P004" TO WS-REASON-CD
              END-IF
           END-IF
           IF WS-REASON-CD = SPACES
              PERFORM 3300-VALIDATE-POLICY
           END-IF
           IF WS-REASON-CD = SPACES
              PERFORM 3400-WRITE-REPORT
           ELSE
              ADD 1 TO WS-SKIP-CNT
              DISPLAY "試算控え出力対象外 理由=" WS-REASON-CD
                      " PRM-ID=" PR-PRM-ID
           END-IF.
      *
       3200-FIND-POLICY.
           MOVE C-NO TO WS-POL-FOUND-SW
           MOVE ZERO TO WS-POL-SUB
           PERFORM VARYING WS-POL-IDX FROM 1 BY 1
             UNTIL WS-POL-IDX > WS-POL-MAX OR POL-FOUND
              IF T-PO-POL-NO(WS-POL-IDX) = PR-POL-NO
                 MOVE WS-POL-IDX TO WS-POL-SUB
                 MOVE C-YES TO WS-POL-FOUND-SW
              END-IF
           END-PERFORM.
      *
       3300-VALIDATE-POLICY.
           IF T-PO-STATUS-KBN(WS-POL-SUB) NOT = C-STATUS-VALID
              MOVE "S001" TO WS-REASON-CD
           END-IF
           IF WS-REASON-CD = SPACES
              IF T-PO-SEX-KBN(WS-POL-SUB) NOT = "1"
                 AND T-PO-SEX-KBN(WS-POL-SUB) NOT = "2"
                 MOVE "S002" TO WS-REASON-CD
              END-IF
           END-IF
           IF WS-REASON-CD = SPACES
              IF PR-SUM-ASSURED-AMT <= ZERO
                 MOVE "S003" TO WS-REASON-CD
              END-IF
           END-IF
           IF WS-REASON-CD = SPACES
              IF PR-SUM-ASSURED-AMT
                    NOT = T-PO-SUM-AMT(WS-POL-SUB)
                 MOVE "S004" TO WS-REASON-CD
              END-IF
           END-IF
           IF WS-REASON-CD = SPACES
              PERFORM 3310-SET-AGE-BAND
           END-IF.
      *
       3310-SET-AGE-BAND.
           EVALUATE TRUE
              WHEN T-PO-AGE(WS-POL-SUB) <= 29
                 MOVE "A1" TO WS-AGE-BAND
              WHEN T-PO-AGE(WS-POL-SUB) <= 39
                 MOVE "A2" TO WS-AGE-BAND
              WHEN T-PO-AGE(WS-POL-SUB) <= 49
                 MOVE "A3" TO WS-AGE-BAND
              WHEN T-PO-AGE(WS-POL-SUB) <= 59
                 MOVE "A4" TO WS-AGE-BAND
              WHEN OTHER
                 MOVE "A5" TO WS-AGE-BAND
           END-EVALUATE.
      *
       3400-WRITE-REPORT.
           ADD 1 TO WS-LINE-NO
           INITIALIZE LRRPTF-REC
           MOVE PR-PRM-ID TO WS-REPORT-ID-WORK
           MOVE WS-REPORT-ID-WORK TO RP-REPORT-ID
           MOVE WS-REPORT-YM TO RP-REPORT-YM
           MOVE C-RPT-TYPE TO RP-REPORT-TYPE-KBN
           MOVE PR-POL-NO TO RP-POL-NO
           MOVE WS-LINE-NO TO RP-LINE-NO
           MOVE PR-PRM-AMT TO WS-AMT-WORK
           MOVE WS-AMT-WORK TO RP-PRINT-AMT
           MOVE C-RPT-OUT-OK TO RP-OUTPUT-STATUS-KBN
           WRITE LRRPTF-REC
           IF WS-LRRPTF-ST = "00"
              ADD 1 TO WS-RPT-WRITE-CNT
           ELSE
              DISPLAY "LRRPTF 書込失敗 ST=" WS-LRRPTF-ST
                      " PRM-ID=" PR-PRM-ID
              MOVE C-YES TO WS-HARD-ERR-SW
              MOVE 8 TO RETURN-CODE
           END-IF.
      *
       9000-FINAL.
           IF WS-LFPOLF-ST NOT = SPACES
              CLOSE LFPOLF
              IF WS-LFPOLF-ST NOT = "00"
                 AND WS-LFPOLF-ST NOT = "42"
                 DISPLAY "LFPOLF クローズ失敗 ST=" WS-LFPOLF-ST
                 MOVE C-YES TO WS-HARD-ERR-SW
                 MOVE 8 TO RETURN-CODE
              END-IF
           END-IF
      *
           IF WS-LFPRMF-ST NOT = SPACES
              CLOSE LFPRMF
              IF WS-LFPRMF-ST NOT = "00"
                 AND WS-LFPRMF-ST NOT = "42"
                 DISPLAY "LFPRMF クローズ失敗 ST=" WS-LFPRMF-ST
                 MOVE C-YES TO WS-HARD-ERR-SW
                 MOVE 8 TO RETURN-CODE
              END-IF
           END-IF
      *
           IF WS-LRRPTF-ST NOT = SPACES
              CLOSE LRRPTF
              IF WS-LRRPTF-ST NOT = "00"
                 AND WS-LRRPTF-ST NOT = "42"
                 DISPLAY "LRRPTF クローズ失敗 ST=" WS-LRRPTF-ST
                 MOVE C-YES TO WS-HARD-ERR-SW
                 MOVE 8 TO RETURN-CODE
              END-IF
           END-IF
      *
           DISPLAY "LR280B 終了 PRM読込=" WS-PRM-READ-CNT
                   " POL読込=" WS-POL-READ-CNT
                   " RPT出力=" WS-RPT-WRITE-CNT
                   " 除外=" WS-SKIP-CNT
           IF NOT HARD-ERROR
              MOVE 0 TO RETURN-CODE
           END-IF.
