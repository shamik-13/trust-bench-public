       IDENTIFICATION DIVISION.
       PROGRAM-ID. TG630B.
      * 変更履歴
      * 版数  年月日(和暦)   担当                         概要
      * 1.00  平成28年04月01日 システム部 情報系チーム     新規作成
      * 1.01  令和02年10月15日 システム部 対外系チーム     交換明細集計条件見直し
      * 1.02  令和06年06月03日 システム部 情報系/対外系チーム 日次交換集計表出力項目追加
       AUTHOR. TRUST-BATCH.
      *
      * 日次交換集計表作成バッチ
      * TGTOTFとTGCLRFを読み合わせ、未通知または照合未済の
      * 相手銀行単位集計行をTGTOTFへ追記する。
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TGTOTF ASSIGN TO "TGTOTF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-TGTOTF.

           SELECT TGCLRF ASSIGN TO "TGCLRF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-TGCLRF.

           SELECT TGACKF ASSIGN TO "TGACKF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-TGACKF.

           SELECT TGBANKF ASSIGN TO "TGBANKF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS BK-COUNTER-BANK
               FILE STATUS IS FS-TGBANKF.

       DATA DIVISION.
       FILE SECTION.
       FD  TGTOTF.
       COPY TGTOTFC.

       FD  TGCLRF.
       COPY TGCLRFC.

       FD  TGACKF.
       COPY TGACKFC.

       FD  TGBANKF.
       COPY TGBANKFC.

       WORKING-STORAGE SECTION.
       01  FS-AREA.
           05 FS-TGTOTF              PIC X(02) VALUE SPACE.
           05 FS-TGCLRF              PIC X(02) VALUE SPACE.
           05 FS-TGACKF              PIC X(02) VALUE SPACE.
           05 FS-TGBANKF             PIC X(02) VALUE SPACE.

       01  EOF-AREA.
           05 EOF-TGTOTF             PIC X VALUE "N".
           05 EOF-TGCLRF             PIC X VALUE "N".
           05 EOF-TGACKF             PIC X VALUE "N".

       01  WK-AREA.
           05 WK-PROCESS-DT          PIC 9(08) VALUE ZERO.
           05 WK-SAVE-DT             PIC 9(08) VALUE ZERO.
           05 WK-SAVE-BANK           PIC X(04) VALUE SPACE.
           05 WK-SAVE-TYPE           PIC X(02) VALUE SPACE.
           05 WK-TOT-CNT             PIC 9(09) VALUE ZERO.
           05 WK-TOT-AMT             PIC S9(13) VALUE ZERO.
           05 WK-CLR-CNT             PIC 9(09) VALUE ZERO.
           05 WK-CLR-AMT             PIC S9(13) VALUE ZERO.
           05 WK-DIFF-CNT            PIC S9(10) VALUE ZERO.
           05 WK-DIFF-AMT            PIC S9(14) VALUE ZERO.
           05 WK-ACK-CNT             PIC 9(09) VALUE ZERO.
           05 WK-IDX                 PIC 9(04) VALUE ZERO.
           05 WK-FOUND-ACK           PIC X VALUE "N".
           05 WK-BANK-OK             PIC X VALUE "N".
           05 WK-HARD-ERR            PIC X VALUE "N".
           05 WK-OUT-CNT             PIC 9(09) VALUE ZERO.

       01  ACK-TABLE.
           05 ACK-ENTRY OCCURS 2000 TIMES.
              10 AT-VALUE-DT         PIC 9(08).
              10 AT-COUNTER-BANK     PIC X(04).
              10 AT-NOTICE-TYPE      PIC X(02).
              10 AT-RESULT-CD        PIC X(02).
              10 AT-ITEM-COUNT       PIC 9(09).

       01  CONST-AREA.
           05 CT-TYPE-PAY            PIC X(02) VALUE "01".
           05 CT-TYPE-RECV           PIC X(02) VALUE "02".
           05 CT-STS-SETTLED         PIC X VALUE "S".
           05 CT-STS-HELD            PIC X VALUE "H".
           05 CT-MATCH-OK            PIC X VALUE "0".
           05 CT-MATCH-NG            PIC X VALUE "1".
           05 CT-MATCH-NOTICE        PIC X VALUE "2".
           05 CT-MATCH-HOLD          PIC X VALUE "3".
           05 CT-MATCH-BANK          PIC X VALUE "4".
           05 CT-BANK-ACTIVE         PIC X VALUE "1".
           05 CT-SUM-TYPE            PIC X(02) VALUE "99".

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE 0 TO RETURN-CODE
           PERFORM 1000-OPEN-INPUT
           IF WK-HARD-ERR = "N"
               PERFORM 2000-LOAD-ACK
           END-IF
           IF WK-HARD-ERR = "N"
               PERFORM 3000-PREPARE-FIRST
           END-IF
           IF WK-HARD-ERR = "N"
               PERFORM 4000-MATCH-LOOP
           END-IF
           PERFORM 8000-CLOSE-INPUT
           IF WK-HARD-ERR = "N"
               DISPLAY "TG630B NORMAL END COUNT=" WK-OUT-CNT
               MOVE 0 TO RETURN-CODE
           ELSE
               DISPLAY "TG630B ABNORMAL END"
               MOVE 8 TO RETURN-CODE
           END-IF
           GOBACK.

       1000-OPEN-INPUT.
           OPEN INPUT TGTOTF
           IF FS-TGTOTF NOT = "00"
               DISPLAY "TGTOTF OPEN ERROR ST=" FS-TGTOTF
               MOVE "Y" TO WK-HARD-ERR
           END-IF
           OPEN INPUT TGCLRF
           IF FS-TGCLRF NOT = "00"
               DISPLAY "TGCLRF OPEN ERROR ST=" FS-TGCLRF
               MOVE "Y" TO WK-HARD-ERR
           END-IF
           OPEN INPUT TGACKF
           IF FS-TGACKF NOT = "00"
               DISPLAY "TGACKF OPEN ERROR ST=" FS-TGACKF
               MOVE "Y" TO WK-HARD-ERR
           END-IF
           OPEN INPUT TGBANKF
           IF FS-TGBANKF NOT = "00"
               DISPLAY "TGBANKF OPEN ERROR ST=" FS-TGBANKF
               MOVE "Y" TO WK-HARD-ERR
           END-IF.

       2000-LOAD-ACK.
           PERFORM UNTIL EOF-TGACKF = "Y"
               READ TGACKF
                   AT END
                       MOVE "Y" TO EOF-TGACKF
                   NOT AT END
                       IF AK-VALUE-DT NUMERIC
                           IF WK-PROCESS-DT = ZERO
                               MOVE AK-VALUE-DT TO WK-PROCESS-DT
                           END-IF
                       ELSE
                           DISPLAY "TGACKF DATE ERROR"
                           MOVE "Y" TO WK-HARD-ERR
                           MOVE "Y" TO EOF-TGACKF
                       END-IF
                       IF WK-HARD-ERR = "N"
                           IF AK-NOTICE-TYPE = CT-TYPE-PAY
                              OR AK-NOTICE-TYPE = CT-TYPE-RECV
                               IF WK-ACK-CNT < 2000
                                   ADD 1 TO WK-ACK-CNT
                                   MOVE AK-VALUE-DT
                                     TO AT-VALUE-DT(WK-ACK-CNT)
                                   MOVE AK-COUNTER-BANK
                                     TO AT-COUNTER-BANK(WK-ACK-CNT)
                                   MOVE AK-NOTICE-TYPE
                                     TO AT-NOTICE-TYPE(WK-ACK-CNT)
                                   MOVE AK-RESULT-CD
                                     TO AT-RESULT-CD(WK-ACK-CNT)
                                   MOVE AK-ITEM-COUNT
                                     TO AT-ITEM-COUNT(WK-ACK-CNT)
                               ELSE
                                   DISPLAY "TGACKF TOO MANY RECORDS"
                                   MOVE "Y" TO WK-HARD-ERR
                                   MOVE "Y" TO EOF-TGACKF
                               END-IF
                           END-IF
                       END-IF
               END-READ
           END-PERFORM.

       3000-PREPARE-FIRST.
           PERFORM 3100-READ-TOT
           PERFORM 3200-READ-CLR.

       3100-READ-TOT.
           READ TGTOTF
               AT END
                   MOVE "Y" TO EOF-TGTOTF
               NOT AT END
                   IF TT-VALUE-DT NOT NUMERIC
                       DISPLAY "TGTOTF DATE ERROR"
                       MOVE "Y" TO WK-HARD-ERR
                   END-IF
                   IF TT-TOTAL-TYPE NOT = CT-TYPE-PAY
                      AND TT-TOTAL-TYPE NOT = CT-TYPE-RECV
                      AND TT-TOTAL-TYPE NOT = CT-SUM-TYPE
                       DISPLAY "TGTOTF TYPE ERROR BANK="
                               TT-COUNTER-BANK
                       MOVE "Y" TO WK-HARD-ERR
                   END-IF
           END-READ.

       3200-READ-CLR.
           READ TGCLRF
               AT END
                   MOVE "Y" TO EOF-TGCLRF
               NOT AT END
                   IF CL-SETTLE-DT NOT NUMERIC
                       DISPLAY "TGCLRF DATE ERROR"
                       MOVE "Y" TO WK-HARD-ERR
                   END-IF
                   IF CL-SETTLE-STATUS NOT = CT-STS-SETTLED
                      AND CL-SETTLE-STATUS NOT = CT-STS-HELD
                       DISPLAY "TGCLRF STATUS ERROR BANK="
                               CL-COUNTER-BANK
                       MOVE "Y" TO WK-HARD-ERR
                   END-IF
           END-READ.

       4000-MATCH-LOOP.
           PERFORM UNTIL WK-HARD-ERR = "Y"
              OR (EOF-TGTOTF = "Y" AND EOF-TGCLRF = "Y")
               IF EOF-TGCLRF = "Y"
                   PERFORM 4100-TOTAL-ONLY
               ELSE
                   IF EOF-TGTOTF = "Y"
                       PERFORM 4200-CLEAR-ONLY
                   ELSE
                       EVALUATE TRUE
                           WHEN TT-VALUE-DT < CL-SETTLE-DT
                               PERFORM 4100-TOTAL-ONLY
                           WHEN TT-VALUE-DT > CL-SETTLE-DT
                               PERFORM 4200-CLEAR-ONLY
                           WHEN TT-COUNTER-BANK < CL-COUNTER-BANK
                               PERFORM 4100-TOTAL-ONLY
                           WHEN TT-COUNTER-BANK > CL-COUNTER-BANK
                               PERFORM 4200-CLEAR-ONLY
                           WHEN OTHER
                               PERFORM 4300-MATCHED
                       END-EVALUATE
                   END-IF
               END-IF
           END-PERFORM.

       4100-TOTAL-ONLY.
           IF TT-TOTAL-TYPE NOT = CT-SUM-TYPE
               MOVE TT-VALUE-DT TO WK-SAVE-DT
               MOVE TT-COUNTER-BANK TO WK-SAVE-BANK
               MOVE TT-TOTAL-TYPE TO WK-SAVE-TYPE
               MOVE TT-ITEM-COUNT TO WK-TOT-CNT
               MOVE TT-TOTAL-AMT TO WK-TOT-AMT
               MOVE ZERO TO WK-CLR-CNT
               MOVE ZERO TO WK-CLR-AMT
               PERFORM 5000-BUILD-OUTPUT
           END-IF
           PERFORM 3100-READ-TOT.

       4200-CLEAR-ONLY.
           MOVE CL-SETTLE-DT TO WK-SAVE-DT
           MOVE CL-COUNTER-BANK TO WK-SAVE-BANK
           MOVE CT-SUM-TYPE TO WK-SAVE-TYPE
           IF CL-NET-AMT < ZERO
               COMPUTE WK-CLR-AMT = CL-NET-AMT * -1
           ELSE
               MOVE CL-NET-AMT TO WK-CLR-AMT
           END-IF
           MOVE ZERO TO WK-TOT-CNT
           MOVE ZERO TO WK-TOT-AMT
           MOVE CL-ITEM-COUNT TO WK-CLR-CNT
           PERFORM 5000-BUILD-OUTPUT
           PERFORM 3200-READ-CLR.

       4300-MATCHED.
           IF TT-TOTAL-TYPE = CT-SUM-TYPE
               PERFORM 3100-READ-TOT
           ELSE
               MOVE TT-VALUE-DT TO WK-SAVE-DT
               MOVE TT-COUNTER-BANK TO WK-SAVE-BANK
               MOVE TT-TOTAL-TYPE TO WK-SAVE-TYPE
               MOVE TT-ITEM-COUNT TO WK-TOT-CNT
               MOVE TT-TOTAL-AMT TO WK-TOT-AMT
               IF CL-NET-AMT < ZERO
                   COMPUTE WK-CLR-AMT = CL-NET-AMT * -1
               ELSE
                   MOVE CL-NET-AMT TO WK-CLR-AMT
               END-IF
               MOVE CL-ITEM-COUNT TO WK-CLR-CNT
               PERFORM 5000-BUILD-OUTPUT
               PERFORM 3100-READ-TOT
               PERFORM 3200-READ-CLR
           END-IF.

       5000-BUILD-OUTPUT.
           PERFORM 5100-CHECK-BANK
           PERFORM 5200-FIND-ACK
           COMPUTE WK-DIFF-CNT = WK-TOT-CNT - WK-CLR-CNT
           COMPUTE WK-DIFF-AMT = WK-TOT-AMT - WK-CLR-AMT
           MOVE SPACES TO TGTOTF-REC
           MOVE WK-SAVE-DT TO TT-VALUE-DT
           MOVE WK-SAVE-BANK TO TT-COUNTER-BANK
           MOVE CT-SUM-TYPE TO TT-TOTAL-TYPE
           IF WK-TOT-CNT > WK-CLR-CNT
               MOVE WK-TOT-CNT TO TT-ITEM-COUNT
           ELSE
               MOVE WK-CLR-CNT TO TT-ITEM-COUNT
           END-IF
           IF WK-TOT-AMT > WK-CLR-AMT
               MOVE WK-TOT-AMT TO TT-TOTAL-AMT
           ELSE
               MOVE WK-CLR-AMT TO TT-TOTAL-AMT
           END-IF
           EVALUATE TRUE
               WHEN WK-BANK-OK NOT = "Y"
                   MOVE CT-MATCH-BANK TO TT-MATCH-STATUS
               WHEN CL-SETTLE-STATUS = CT-STS-HELD
                   MOVE CT-MATCH-HOLD TO TT-MATCH-STATUS
               WHEN WK-FOUND-ACK NOT = "Y"
                   MOVE CT-MATCH-NOTICE TO TT-MATCH-STATUS
               WHEN WK-DIFF-CNT NOT = ZERO
                   MOVE CT-MATCH-NG TO TT-MATCH-STATUS
               WHEN WK-DIFF-AMT NOT = ZERO
                   MOVE CT-MATCH-NG TO TT-MATCH-STATUS
               WHEN OTHER
                   MOVE CT-MATCH-OK TO TT-MATCH-STATUS
           END-EVALUATE
           IF TT-MATCH-STATUS NOT = CT-MATCH-OK
               PERFORM 9100-WRITE-OUTPUT
           END-IF.

       5100-CHECK-BANK.
           MOVE "N" TO WK-BANK-OK
           MOVE WK-SAVE-BANK TO BK-COUNTER-BANK
           READ TGBANKF KEY IS BK-COUNTER-BANK
               INVALID KEY
                   DISPLAY "TGBANKF BANK NOT FOUND BANK="
                           WK-SAVE-BANK
               NOT INVALID KEY
                   IF BK-STATUS = CT-BANK-ACTIVE
                      AND BK-VALID-FROM <= WK-SAVE-DT
                      AND BK-VALID-TO >= WK-SAVE-DT
                       MOVE "Y" TO WK-BANK-OK
                   ELSE
                       DISPLAY "TGBANKF BANK NOT ACTIVE BANK="
                               WK-SAVE-BANK
                   END-IF
           END-READ
           IF FS-TGBANKF NOT = "00" AND FS-TGBANKF NOT = "23"
               DISPLAY "TGBANKF READ ERROR ST=" FS-TGBANKF
               MOVE "Y" TO WK-HARD-ERR
           END-IF.

       5200-FIND-ACK.
           MOVE "N" TO WK-FOUND-ACK
           PERFORM VARYING WK-IDX FROM 1 BY 1
             UNTIL WK-IDX > WK-ACK-CNT OR WK-FOUND-ACK = "Y"
               IF AT-VALUE-DT(WK-IDX) = WK-SAVE-DT
                  AND AT-COUNTER-BANK(WK-IDX) = WK-SAVE-BANK
                  AND AT-NOTICE-TYPE(WK-IDX) = WK-SAVE-TYPE
                  AND AT-RESULT-CD(WK-IDX) = "00"
                   MOVE "Y" TO WK-FOUND-ACK
               END-IF
           END-PERFORM.

       8000-CLOSE-INPUT.
           CLOSE TGTOTF TGCLRF TGACKF TGBANKF.

       9100-WRITE-OUTPUT.
           OPEN EXTEND TGTOTF
           IF FS-TGTOTF NOT = "00"
               DISPLAY "TGTOTF EXTEND OPEN ERROR ST=" FS-TGTOTF
               MOVE "Y" TO WK-HARD-ERR
           ELSE
               WRITE TGTOTF-REC
               IF FS-TGTOTF NOT = "00"
                   DISPLAY "TGTOTF WRITE ERROR ST=" FS-TGTOTF
                   MOVE "Y" TO WK-HARD-ERR
               ELSE
                   ADD 1 TO WK-OUT-CNT
               END-IF
               CLOSE TGTOTF
           END-IF.
