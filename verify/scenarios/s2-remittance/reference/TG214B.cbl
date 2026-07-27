       IDENTIFICATION DIVISION.
       PROGRAM-ID. TG214B.
      *
      * 変更履歴
      * 版数   年月日    担当     概要
      * 1.00   20260618  信託基盤 新規作成
      * 1.01   20260618  信託基盤 名義照合および拒否記録追加
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TGINRMF ASSIGN TO "TGINRMF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-IR-STATUS.
           SELECT KZACCTF ASSIGN TO "KZACCTF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS AC-ACCT-NO
               FILE STATUS IS WS-AC-STATUS.
           SELECT TGREJLF ASSIGN TO "TGREJLF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-RJ-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  TGINRMF.
           COPY TGINRMFC.
       FD  KZACCTF.
           COPY KZACCTC2.
       FD  TGREJLF.
           COPY TGREJLFC.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05 WS-IR-STATUS          PIC X(02) VALUE SPACE.
           05 WS-AC-STATUS          PIC X(02) VALUE SPACE.
           05 WS-RJ-STATUS          PIC X(02) VALUE SPACE.

       01  WS-CONTROL.
           05 WS-END-SW             PIC X     VALUE "N".
              88 END-OF-TGINRMF               VALUE "Y".
           05 WS-HARD-ERROR-SW      PIC X     VALUE "N".
              88 HARD-ERROR                   VALUE "Y".
           05 WS-REJECT-SW          PIC X     VALUE "N".
              88 REJECT-REMIT                 VALUE "Y".

       01  WS-CONSTANTS.
           05 WS-PROGRAM-ID         PIC X(08) VALUE "TG214B".
           05 WS-STATUS-OK          PIC X(02) VALUE "00".
           05 WS-STATUS-EOF         PIC X(02) VALUE "10".
           05 WS-STATUS-NOTFOUND    PIC X(02) VALUE "23".
           05 WS-VALID-AC-STATUS    PIC X(02) VALUE "01".
           05 WS-AMT-LIMIT          PIC 9(11) VALUE 10000000.
           05 WS-RC-NORMAL          PIC 9(02) VALUE 0.
           05 WS-RC-ERROR           PIC 9(02) VALUE 8.

       01  WS-REJECT.
           05 WS-REJ-REASON         PIC X(04) VALUE SPACE.
           05 WS-PAYEE-NORM         PIC X(80) VALUE SPACE.
           05 WS-MASTER-NORM        PIC X(80) VALUE SPACE.

       01  WS-REJECT-CODES.
           05 WS-REJ-NAME           PIC X(04) VALUE "N1".
           05 WS-REJ-NOACCT         PIC X(04) VALUE "A1".
           05 WS-REJ-BADSTAT        PIC X(04) VALUE "A2".
           05 WS-REJ-FORMAT         PIC X(04) VALUE "F1".
           05 WS-REJ-LIMIT          PIC X(04) VALUE "L1".
           05 WS-REJ-UNSUP          PIC X(04) VALUE "S1".

       01  WS-WORK.
           05 WS-DISPLAY-AMT        PIC ZZZ,ZZZ,ZZZ,ZZ9.

           COPY LK-KANA-PARM.
           COPY LK-REJLOG-PARM.

       PROCEDURE DIVISION.
       MAIN-RTN.
           MOVE WS-RC-NORMAL TO RETURN-CODE
           PERFORM OPEN-FILES
           IF NOT HARD-ERROR
              PERFORM UNTIL END-OF-TGINRMF OR HARD-ERROR
                 PERFORM READ-TGINRMF
                 IF NOT END-OF-TGINRMF
                    PERFORM PROCESS-REMIT
                 END-IF
              END-PERFORM
           END-IF
           PERFORM CLOSE-FILES
           IF HARD-ERROR
              MOVE WS-RC-ERROR TO RETURN-CODE
           ELSE
              MOVE WS-RC-NORMAL TO RETURN-CODE
           END-IF
           GOBACK.

       OPEN-FILES.
           OPEN INPUT TGINRMF
           IF WS-IR-STATUS NOT = WS-STATUS-OK
              DISPLAY "TGINRMF オープン失敗 ST=" WS-IR-STATUS
              SET HARD-ERROR TO TRUE
           END-IF
           IF NOT HARD-ERROR
              OPEN INPUT KZACCTF
              IF WS-AC-STATUS NOT = WS-STATUS-OK
                 DISPLAY "KZACCTF オープン失敗 ST=" WS-AC-STATUS
                 SET HARD-ERROR TO TRUE
              END-IF
           END-IF
           IF NOT HARD-ERROR
              OPEN OUTPUT TGREJLF
              IF WS-RJ-STATUS NOT = WS-STATUS-OK
                 DISPLAY "TGREJLF オープン失敗 ST=" WS-RJ-STATUS
                 SET HARD-ERROR TO TRUE
              END-IF
           END-IF.

       READ-TGINRMF.
           READ TGINRMF
              AT END
                 SET END-OF-TGINRMF TO TRUE
              NOT AT END
                 IF WS-IR-STATUS NOT = WS-STATUS-OK
                    DISPLAY "TGINRMF 読込失敗 ST=" WS-IR-STATUS
                    SET HARD-ERROR TO TRUE
                 END-IF
           END-READ.

       PROCESS-REMIT.
           MOVE "N" TO WS-REJECT-SW
           MOVE SPACE TO WS-REJ-REASON
           EVALUATE IR-REMIT-TYPE
              WHEN "10"
              WHEN "20"
              WHEN "30"
              WHEN "40"
                 PERFORM VALIDATE-DEPOSIT
              WHEN "90"
                 CONTINUE
              WHEN OTHER
                 MOVE WS-REJ-UNSUP TO WS-REJ-REASON
                 SET REJECT-REMIT TO TRUE
           END-EVALUATE
           IF REJECT-REMIT AND NOT HARD-ERROR
              PERFORM WRITE-REJECT
           END-IF.

       VALIDATE-DEPOSIT.
           IF IR-REMIT-DT = SPACE
              OR IR-CENTER-SEQ = SPACE
              OR IR-PAYEE-ACCT-NO = SPACE
              OR IR-PAYEE-NAME-KANA = SPACE
              OR IR-REMIT-AMT = ZERO
              MOVE WS-REJ-FORMAT TO WS-REJ-REASON
              SET REJECT-REMIT TO TRUE
           END-IF
           IF NOT REJECT-REMIT
              PERFORM READ-ACCOUNT
           END-IF
           IF NOT REJECT-REMIT AND NOT HARD-ERROR
              IF AC-ACCT-NO = SPACE
                 OR AC-ACCT-NAME-KANA = SPACE
                 OR AC-STATUS = SPACE
                 MOVE WS-REJ-FORMAT TO WS-REJ-REASON
                 SET REJECT-REMIT TO TRUE
              END-IF
           END-IF
           IF NOT REJECT-REMIT AND NOT HARD-ERROR
              IF AC-STATUS NOT = WS-VALID-AC-STATUS
                 MOVE WS-REJ-BADSTAT TO WS-REJ-REASON
                 SET REJECT-REMIT TO TRUE
              END-IF
           END-IF
           IF NOT REJECT-REMIT AND NOT HARD-ERROR
              IF IR-REMIT-AMT > WS-AMT-LIMIT
                 MOVE WS-REJ-LIMIT TO WS-REJ-REASON
                 SET REJECT-REMIT TO TRUE
              END-IF
           END-IF
           IF NOT REJECT-REMIT AND NOT HARD-ERROR
              PERFORM COMPARE-KANA-NAME
           END-IF.

       READ-ACCOUNT.
           MOVE IR-PAYEE-ACCT-NO TO AC-ACCT-NO
           READ KZACCTF KEY IS AC-ACCT-NO
              INVALID KEY
                 IF WS-AC-STATUS = WS-STATUS-NOTFOUND
                    MOVE WS-REJ-NOACCT TO WS-REJ-REASON
                    SET REJECT-REMIT TO TRUE
                 ELSE
                    DISPLAY "KZACCTF 読込失敗 ST=" WS-AC-STATUS
                    SET HARD-ERROR TO TRUE
                 END-IF
              NOT INVALID KEY
                 IF WS-AC-STATUS NOT = WS-STATUS-OK
                    DISPLAY "KZACCTF 読込失敗 ST=" WS-AC-STATUS
                    SET HARD-ERROR TO TRUE
                 END-IF
           END-READ.

       COMPARE-KANA-NAME.
           MOVE SPACE TO LK-RAW-KANA LK-NORM-KANA LK-KANA-RET
           MOVE IR-PAYEE-NAME-KANA TO LK-RAW-KANA
           CALL "TG912S" USING LK-KANA-PARM
           IF LK-KANA-RET NOT = WS-STATUS-OK
              MOVE WS-REJ-FORMAT TO WS-REJ-REASON
              SET REJECT-REMIT TO TRUE
           ELSE
              MOVE LK-NORM-KANA TO WS-PAYEE-NORM
           END-IF
           IF NOT REJECT-REMIT
              MOVE SPACE TO LK-RAW-KANA LK-NORM-KANA LK-KANA-RET
              MOVE AC-ACCT-NAME-KANA TO LK-RAW-KANA
              CALL "TG912S" USING LK-KANA-PARM
              IF LK-KANA-RET NOT = WS-STATUS-OK
                 MOVE WS-REJ-FORMAT TO WS-REJ-REASON
                 SET REJECT-REMIT TO TRUE
              ELSE
                 MOVE LK-NORM-KANA TO WS-MASTER-NORM
              END-IF
           END-IF
           IF NOT REJECT-REMIT
              IF WS-PAYEE-NORM NOT = WS-MASTER-NORM
                 MOVE WS-REJ-NAME TO WS-REJ-REASON
                 SET REJECT-REMIT TO TRUE
              END-IF
           END-IF.

       WRITE-REJECT.
           MOVE SPACE TO LK-RL-PROGRAM-ID
                         LK-RL-REASON
                         LK-RL-LOG-TS
                         LK-RL-RET
           MOVE WS-PROGRAM-ID TO LK-RL-PROGRAM-ID
           MOVE WS-REJ-REASON TO LK-RL-REASON
           CALL "TG920S" USING LK-REJLOG-PARM
           IF LK-RL-RET NOT = WS-STATUS-OK
              DISPLAY "拒否ログ採番失敗 RC=" LK-RL-RET
              SET HARD-ERROR TO TRUE
           ELSE
              MOVE SPACE TO TGREJLF-REC
              MOVE IR-REMIT-DT TO RJ-REMIT-DT
              MOVE IR-CENTER-SEQ TO RJ-CENTER-SEQ
              MOVE WS-REJ-REASON TO RJ-REJ-REASON
              MOVE WS-PROGRAM-ID TO RJ-PROGRAM-ID
              MOVE IR-PAYEE-ACCT-NO TO RJ-PAYEE-ACCT-NO
              MOVE IR-PAYEE-NAME-KANA TO RJ-PAYEE-NAME-KANA
              MOVE AC-ACCT-NAME-KANA TO RJ-MASTER-NAME-KANA
              MOVE IR-REMIT-AMT TO RJ-REJ-AMT
              MOVE LK-RL-LOG-TS TO RJ-LOG-TS
              WRITE TGREJLF-REC
              IF WS-RJ-STATUS NOT = WS-STATUS-OK
                 DISPLAY "TGREJLF 書込失敗 ST=" WS-RJ-STATUS
                 SET HARD-ERROR TO TRUE
              END-IF
           END-IF
           IF NOT HARD-ERROR
              MOVE IR-REMIT-AMT TO WS-DISPLAY-AMT
              DISPLAY "拒否記録 REMIT-DT=" IR-REMIT-DT
                      " CENTER-SEQ=" IR-CENTER-SEQ
                      " 理由=" WS-REJ-REASON
                      " 金額=" WS-DISPLAY-AMT
           END-IF.

       CLOSE-FILES.
           IF WS-IR-STATUS = WS-STATUS-OK
              OR WS-IR-STATUS = WS-STATUS-EOF
              CLOSE TGINRMF
              IF WS-IR-STATUS NOT = WS-STATUS-OK
                 DISPLAY "TGINRMF クローズ失敗 ST=" WS-IR-STATUS
                 SET HARD-ERROR TO TRUE
              END-IF
           END-IF
           IF WS-AC-STATUS = WS-STATUS-OK
              OR WS-AC-STATUS = WS-STATUS-NOTFOUND
              CLOSE KZACCTF
              IF WS-AC-STATUS NOT = WS-STATUS-OK
                 DISPLAY "KZACCTF クローズ失敗 ST=" WS-AC-STATUS
                 SET HARD-ERROR TO TRUE
              END-IF
           END-IF
           IF WS-RJ-STATUS = WS-STATUS-OK
              CLOSE TGREJLF
              IF WS-RJ-STATUS NOT = WS-STATUS-OK
                 DISPLAY "TGREJLF クローズ失敗 ST=" WS-RJ-STATUS
                 SET HARD-ERROR TO TRUE
              END-IF
           END-IF.
