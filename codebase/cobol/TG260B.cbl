       IDENTIFICATION DIVISION.
      ******************************************************************
      * VERSION  DATE      AUTHOR  DESCRIPTION
      * 1.00     20260410  DEV1    NEW
      * 1.01     20260508  DEV1    ADD REJECT REASON EDIT
      * 1.02     20260612  DEV2    FIX CLEARING MATCH CONDITION
      ******************************************************************
       PROGRAM-ID. TG260B.
      ******************************************************************
      * 版数  年月日      担当                         概要
      * 1.00  令和08.04.10 システム部 為替・対外接続チーム 新規作成
      * 1.01  令和08.05.08 システム部 為替・対外接続チーム 返却理由編集追加
      * 1.02  令和08.06.12 システム部 為替・対外接続チーム 照合条件修正
      ******************************************************************

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TGREJLF ASSIGN TO "TGREJLF"
              ORGANIZATION IS INDEXED
              ACCESS MODE IS SEQUENTIAL
              RECORD KEY IS RJ-REMIT-DT
              FILE STATUS IS WS-ST-TGREJLF.
           SELECT TGOUTSF ASSIGN TO "TGOUTSF"
              ORGANIZATION IS SEQUENTIAL
              ACCESS MODE IS SEQUENTIAL
              FILE STATUS IS WS-ST-TGOUTSF.
           SELECT TGCLJNF ASSIGN TO "TGCLJNF"
              ORGANIZATION IS INDEXED
              ACCESS MODE IS SEQUENTIAL
              RECORD KEY IS CJ-CLEARING-DT
              FILE STATUS IS WS-ST-TGCLJNF.
           SELECT TGACKNF ASSIGN TO "TGACKNF"
              ORGANIZATION IS SEQUENTIAL
              ACCESS MODE IS SEQUENTIAL
              FILE STATUS IS WS-ST-TGACKNF.

       DATA DIVISION.
       FILE SECTION.
       FD  TGREJLF.
           COPY TGREJLFC.
       FD  TGOUTSF.
           COPY TGOUTSC.
       FD  TGCLJNF.
           COPY TGCLJNC.
       FD  TGACKNF.
           COPY TGACKNC.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05 WS-ST-TGREJLF        PIC XX.
           05 WS-ST-TGOUTSF        PIC XX.
           05 WS-ST-TGCLJNF        PIC XX.
           05 WS-ST-TGACKNF        PIC XX.

       01  WS-FLAGS.
           05 WS-EOF-REJ           PIC X VALUE "N".
              88 REJ-EOF                 VALUE "Y".
           05 WS-EOF-OUT           PIC X VALUE "N".
              88 OUT-EOF                 VALUE "Y".
           05 WS-EOF-CLJ           PIC X VALUE "N".
              88 CLJ-EOF                 VALUE "Y".
           05 WS-ABEND-SW          PIC X VALUE "N".
              88 ABEND-ON                VALUE "Y".

       01  WS-COUNTERS.
           05 WS-REJ-READ-CNT      PIC 9(9) VALUE ZERO.
           05 WS-OUT-READ-CNT      PIC 9(9) VALUE ZERO.
           05 WS-CLJ-READ-CNT      PIC 9(9) VALUE ZERO.
           05 WS-ACK-WRITE-CNT     PIC 9(9) VALUE ZERO.
           05 WS-SEND-ACK-CNT      PIC 9(9) VALUE ZERO.
           05 WS-CLEAR-ACK-CNT     PIC 9(9) VALUE ZERO.
           05 WS-REJECT-ACK-CNT    PIC 9(9) VALUE ZERO.

       01  WS-WORK.
           05 WS-OUT-KEY.
              10 WS-OUT-DT         PIC 9(8).
              10 WS-OUT-SEQ        PIC 9(10).
           05 WS-CLJ-KEY.
              10 WS-CLJ-DT         PIC 9(8).
              10 WS-CLJ-SEQ        PIC 9(10).
           05 WS-EDIT-TEXT         PIC X(80).
           05 WS-ERR-FILE          PIC X(8).
           05 WS-ERR-ST            PIC XX.
           05 WS-REASON-TEXT       PIC X(40).

       01  WS-CONST.
           05 CT-PGM-ID            PIC X(8) VALUE "TG260B".
           05 CT-NOTICE-SEND       PIC X(2) VALUE "01".
           05 CT-NOTICE-CLEAR      PIC X(2) VALUE "02".
           05 CT-NOTICE-REJECT     PIC X(2) VALUE "03".
           05 CT-RESULT-OK         PIC X(4) VALUE "0000".
           05 CT-RESULT-CLEAR      PIC X(4) VALUE "0100".
           05 CT-RESULT-REJECT     PIC X(4) VALUE "9000".
           05 CT-DIRECTION-OUT     PIC X VALUE "O".
           05 CT-MATCH-OK          PIC X VALUE "1".

           COPY LK-REJLOG-PARM.

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE 0 TO RETURN-CODE
           PERFORM 1000-OPEN
           IF ABEND-ON
              PERFORM 9000-CLOSE
              GOBACK
           END-IF

           PERFORM 2000-READ-CLJ
           PERFORM 2100-READ-OUT
           PERFORM 2200-READ-REJ

           PERFORM 3000-PROCESS-OUT UNTIL OUT-EOF OR ABEND-ON
           PERFORM 4000-PROCESS-REJ UNTIL REJ-EOF OR ABEND-ON

           PERFORM 9000-CLOSE

           IF ABEND-ON
              MOVE 8 TO RETURN-CODE
           ELSE
              MOVE 0 TO RETURN-CODE
              DISPLAY "TG260B 正常終了"
              DISPLAY "送信読込件数=" WS-OUT-READ-CNT
              DISPLAY "照合読込件数=" WS-CLJ-READ-CNT
              DISPLAY "返却読込件数=" WS-REJ-READ-CNT
              DISPLAY "ACK書込件数=" WS-ACK-WRITE-CNT
           END-IF
           GOBACK.

       1000-OPEN.
           OPEN INPUT TGREJLF
           IF WS-ST-TGREJLF NOT = "00"
              MOVE "TGREJLF " TO WS-ERR-FILE
              MOVE WS-ST-TGREJLF TO WS-ERR-ST
              PERFORM 9100-OPEN-ERROR
           END-IF

           OPEN INPUT TGOUTSF
           IF WS-ST-TGOUTSF NOT = "00"
              MOVE "TGOUTSF " TO WS-ERR-FILE
              MOVE WS-ST-TGOUTSF TO WS-ERR-ST
              PERFORM 9100-OPEN-ERROR
           END-IF

           OPEN INPUT TGCLJNF
           IF WS-ST-TGCLJNF NOT = "00"
              MOVE "TGCLJNF " TO WS-ERR-FILE
              MOVE WS-ST-TGCLJNF TO WS-ERR-ST
              PERFORM 9100-OPEN-ERROR
           END-IF

           OPEN OUTPUT TGACKNF
           IF WS-ST-TGACKNF NOT = "00"
              MOVE "TGACKNF " TO WS-ERR-FILE
              MOVE WS-ST-TGACKNF TO WS-ERR-ST
              PERFORM 9100-OPEN-ERROR
           END-IF.

       2000-READ-CLJ.
           READ TGCLJNF
              AT END
                 SET CLJ-EOF TO TRUE
              NOT AT END
                 ADD 1 TO WS-CLJ-READ-CNT
                 MOVE CJ-CLEARING-DT TO WS-CLJ-DT
                 MOVE CJ-CENTER-SEQ  TO WS-CLJ-SEQ
           END-READ
           IF WS-ST-TGCLJNF NOT = "00" AND WS-ST-TGCLJNF NOT = "10"
              MOVE "TGCLJNF " TO WS-ERR-FILE
              MOVE WS-ST-TGCLJNF TO WS-ERR-ST
              PERFORM 9200-IO-ERROR
           END-IF.

       2100-READ-OUT.
           READ TGOUTSF
              AT END
                 SET OUT-EOF TO TRUE
              NOT AT END
                 ADD 1 TO WS-OUT-READ-CNT
                 MOVE OS-SEND-DT  TO WS-OUT-DT
                 MOVE OS-SEND-SEQ TO WS-OUT-SEQ
           END-READ
           IF WS-ST-TGOUTSF NOT = "00" AND WS-ST-TGOUTSF NOT = "10"
              MOVE "TGOUTSF " TO WS-ERR-FILE
              MOVE WS-ST-TGOUTSF TO WS-ERR-ST
              PERFORM 9200-IO-ERROR
           END-IF.

       2200-READ-REJ.
           READ TGREJLF
              AT END
                 SET REJ-EOF TO TRUE
              NOT AT END
                 ADD 1 TO WS-REJ-READ-CNT
           END-READ
           IF WS-ST-TGREJLF NOT = "00" AND WS-ST-TGREJLF NOT = "10"
              MOVE "TGREJLF " TO WS-ERR-FILE
              MOVE WS-ST-TGREJLF TO WS-ERR-ST
              PERFORM 9200-IO-ERROR
           END-IF.

       3000-PROCESS-OUT.
           PERFORM 3100-WRITE-SEND-ACK

           PERFORM 3200-SKIP-OLD-CLJ
              UNTIL CLJ-EOF
                 OR WS-CLJ-DT > WS-OUT-DT
                 OR (WS-CLJ-DT = WS-OUT-DT
                    AND WS-CLJ-SEQ >= WS-OUT-SEQ)
                 OR ABEND-ON

           IF NOT CLJ-EOF
              IF WS-CLJ-DT = WS-OUT-DT
                 AND WS-CLJ-SEQ = WS-OUT-SEQ
                 AND CJ-DIRECTION = CT-DIRECTION-OUT
                 AND CJ-MATCH-STATUS = CT-MATCH-OK
                 PERFORM 3300-WRITE-CLEAR-ACK
                 PERFORM 2000-READ-CLJ
              END-IF
           END-IF

           PERFORM 2100-READ-OUT.

       3100-WRITE-SEND-ACK.
           IF OS-SEND-AMT <= ZERO
              DISPLAY "送信金額不正 DT=" OS-SEND-DT
                      " SEQ=" OS-SEND-SEQ
              MOVE 8 TO RETURN-CODE
              SET ABEND-ON TO TRUE
           ELSE
              INITIALIZE TGACKNF-REC
              MOVE OS-SEND-DT        TO AK-ACK-DT
              MOVE OS-SEND-SEQ       TO AK-CENTER-SEQ
              MOVE CT-NOTICE-SEND    TO AK-NOTICE-TYPE
              MOVE CT-PGM-ID         TO AK-SOURCE-PROGRAM
              MOVE OS-PAYEE-BANK     TO AK-TARGET-BANK
              MOVE OS-PAYEE-BRANCH   TO AK-TARGET-BRANCH
              MOVE OS-SEND-AMT       TO AK-ITEM-AMT
              MOVE CT-RESULT-OK      TO AK-RESULT-CD
              MOVE "対外送信完了" TO AK-NOTICE-TEXT
              PERFORM 8000-WRITE-ACK
              ADD 1 TO WS-SEND-ACK-CNT
           END-IF.

       3200-SKIP-OLD-CLJ.
           IF CJ-MATCH-STATUS = CT-MATCH-OK
              DISPLAY "未対応の清算データ DT=" CJ-CLEARING-DT
                      " SEQ=" CJ-CENTER-SEQ
           END-IF
           PERFORM 2000-READ-CLJ.

       3300-WRITE-CLEAR-ACK.
           IF CJ-TOTAL-AMT NOT = OS-SEND-AMT
              DISPLAY "清算金額不一致 DT=" OS-SEND-DT
                      " SEQ=" OS-SEND-SEQ
              MOVE 8 TO RETURN-CODE
              SET ABEND-ON TO TRUE
           ELSE
              INITIALIZE TGACKNF-REC
              MOVE CJ-CLEARING-DT    TO AK-ACK-DT
              MOVE CJ-CENTER-SEQ     TO AK-CENTER-SEQ
              MOVE CT-NOTICE-CLEAR   TO AK-NOTICE-TYPE
              MOVE CT-PGM-ID         TO AK-SOURCE-PROGRAM
              MOVE CJ-BANK-CD        TO AK-TARGET-BANK
              MOVE CJ-BRANCH-CD      TO AK-TARGET-BRANCH
              MOVE CJ-TOTAL-AMT      TO AK-ITEM-AMT
              MOVE CT-RESULT-CLEAR   TO AK-RESULT-CD
              MOVE "清算計上済" TO AK-NOTICE-TEXT
              PERFORM 8000-WRITE-ACK
              ADD 1 TO WS-CLEAR-ACK-CNT
           END-IF.

       4000-PROCESS-REJ.
           PERFORM 4100-EDIT-REJECT-TEXT
           INITIALIZE TGACKNF-REC
           MOVE RJ-REMIT-DT          TO AK-ACK-DT
           MOVE RJ-CENTER-SEQ        TO AK-CENTER-SEQ
           MOVE CT-NOTICE-REJECT     TO AK-NOTICE-TYPE
           MOVE CT-PGM-ID            TO AK-SOURCE-PROGRAM
           MOVE SPACES               TO AK-TARGET-BANK
           MOVE SPACES               TO AK-TARGET-BRANCH
           MOVE RJ-REJ-AMT           TO AK-ITEM-AMT
           MOVE CT-RESULT-REJECT     TO AK-RESULT-CD
           MOVE WS-EDIT-TEXT         TO AK-NOTICE-TEXT
           PERFORM 8000-WRITE-ACK
           ADD 1 TO WS-REJECT-ACK-CNT
           PERFORM 2200-READ-REJ.

       4100-EDIT-REJECT-TEXT.
           INITIALIZE LK-REJLOG-PARM
           MOVE RJ-PROGRAM-ID        TO LK-RL-PROGRAM-ID
           MOVE RJ-REJ-REASON        TO LK-RL-REASON
           MOVE RJ-LOG-TS            TO LK-RL-LOG-TS
           CALL "TG920S" USING LK-REJLOG-PARM
           IF LK-RL-RET = SPACES
              EVALUATE RJ-REJ-REASON
                 WHEN "01"
                    MOVE "返却:口座不正" TO WS-REASON-TEXT
                 WHEN "02"
                    MOVE "返却:名義不一致" TO WS-REASON-TEXT
                 WHEN "03"
                    MOVE "返却:金額不正" TO WS-REASON-TEXT
                 WHEN OTHER
                    MOVE "返却:検証エラー" TO WS-REASON-TEXT
              END-EVALUATE
              MOVE WS-REASON-TEXT TO WS-EDIT-TEXT
           ELSE
              MOVE LK-RL-RET TO WS-EDIT-TEXT
           END-IF.

       8000-WRITE-ACK.
           WRITE TGACKNF-REC
           IF WS-ST-TGACKNF = "00"
              ADD 1 TO WS-ACK-WRITE-CNT
           ELSE
              MOVE "TGACKNF " TO WS-ERR-FILE
              MOVE WS-ST-TGACKNF TO WS-ERR-ST
              PERFORM 9200-IO-ERROR
           END-IF.

       9000-CLOSE.
           CLOSE TGREJLF
           CLOSE TGOUTSF
           CLOSE TGCLJNF
           CLOSE TGACKNF.

       9100-OPEN-ERROR.
           DISPLAY WS-ERR-FILE "オープンエラー ST=" WS-ERR-ST
           MOVE 8 TO RETURN-CODE
           SET ABEND-ON TO TRUE.

       9200-IO-ERROR.
           DISPLAY WS-ERR-FILE "入出力エラー ST=" WS-ERR-ST
           MOVE 8 TO RETURN-CODE
           SET ABEND-ON TO TRUE.
