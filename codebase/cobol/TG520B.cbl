       IDENTIFICATION DIVISION.
       PROGRAM-ID. TG520B.
      *---------------------------------------------------------------*
      * 変更履歴                                                      *
      * 版数  年月日      担当                         概要           *
      * 1.00  令和03年04月01日 システム部 為替・対外接続チーム 新規作成*
      * 1.01  令和04年10月11日 システム部 為替・対外接続チーム 照合強化*
      * 1.02  令和06年02月19日 システム部 為替・対外接続チーム 表示整備*
      *---------------------------------------------------------------*
       AUTHOR. TRUST-BATCH.
      *---------------------------------------------------------------*
      * 仕向ACK受信照合バッチ
      *---------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TGACKNF ASSIGN TO "TGACKNF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-TGACKNF.
           SELECT TGOUTSF ASSIGN TO "TGOUTSF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-TGOUTSF.
           SELECT TGCLJNF ASSIGN TO "TGCLJNF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-TGCLJNF.
           SELECT TGREJLF ASSIGN TO "TGREJLF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-TGREJLF.

       DATA DIVISION.
       FILE SECTION.
       FD  TGACKNF.
           COPY TGACKNC.
       FD  TGOUTSF.
           COPY TGOUTSC.
       FD  TGCLJNF.
           COPY TGCLJNC.
       FD  TGREJLF.
           COPY TGREJLFC.

       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID              PIC X(08) VALUE "TG520B".
       01  WS-ABEND-FLG               PIC X(01) VALUE "0".
           88  ABEND-FOUND                      VALUE "1".
       01  WS-EOF-FLG.
           05  WS-ACK-EOF             PIC X(01) VALUE "0".
               88  ACK-EOF                      VALUE "1".
           05  WS-OUT-EOF             PIC X(01) VALUE "0".
               88  OUT-EOF                      VALUE "1".

       01  WS-FILE-STATUS.
           05  FS-TGACKNF             PIC X(02) VALUE SPACE.
           05  FS-TGOUTSF             PIC X(02) VALUE SPACE.
           05  FS-TGCLJNF             PIC X(02) VALUE SPACE.
           05  FS-TGREJLF             PIC X(02) VALUE SPACE.

       01  WS-CURRENT-DATE.
           05  WS-CUR-YYYYMMDD        PIC 9(08).
           05  WS-CUR-HHMMSSCC        PIC 9(08).
           05  WS-CUR-GMT             PIC X(05).
       01  WS-JOURNAL-TS.
           05  WS-JTS-DATE            PIC 9(08).
           05  WS-JTS-TIME            PIC 9(06).

       01  WS-COUNTERS.
           05  WS-OUT-CNT             PIC 9(06) VALUE ZERO.
           05  WS-ACK-CNT             PIC 9(06) VALUE ZERO.
           05  WS-MATCH-CNT           PIC 9(06) VALUE ZERO.
           05  WS-MISS-CNT            PIC 9(06) VALUE ZERO.
           05  WS-DUP-CNT             PIC 9(06) VALUE ZERO.
           05  WS-AMTNG-CNT           PIC 9(06) VALUE ZERO.
           05  WS-REJ-CNT             PIC 9(06) VALUE ZERO.

       01  WS-SUB                     PIC 9(06) COMP VALUE ZERO.
       01  WS-HIT-SUB                 PIC 9(06) COMP VALUE ZERO.
       01  WS-HIT-FLG                 PIC X(01) VALUE "0".
           88  HIT-FOUND                        VALUE "1".
       01  WS-MAX-OUT                 PIC 9(06) VALUE 20000.

       01  WS-OUT-TABLE.
           05  WS-OUT-ENT OCCURS 20000 TIMES.
               10  T-OS-SEND-DT       PIC 9(08).
               10  T-OS-SEND-SEQ      PIC 9(10).
               10  T-OS-REMIT-TYPE    PIC X(02).
               10  T-OS-SENDER-BANK   PIC X(04).
               10  T-OS-SENDER-BRANCH PIC X(03).
               10  T-OS-PAYEE-BANK    PIC X(04).
               10  T-OS-PAYEE-BRANCH  PIC X(03).
               10  T-OS-PAYEE-ACCT-TYPE
                                           PIC X(01).
               10  T-OS-PAYEE-ACCT-NO PIC X(07).
               10  T-OS-PAYEE-NAME-KANA
                                           PIC X(40).
               10  T-OS-SEND-AMT      PIC 9(13).
               10  T-OS-SIGNATURE     PIC X(32).
               10  T-OS-BUILD-PROGRAM PIC X(08).
               10  T-MATCH-FLG        PIC X(01).
                   88  T-MATCHED                 VALUE "1".

       01  WS-REASON-TEXT             PIC X(40) VALUE SPACE.
       01  WS-MATCH-STATUS            PIC X(02) VALUE SPACE.

           COPY LK-REJLOG-PARM.

       PROCEDURE DIVISION.
       MAIN-RTN.
           MOVE 8 TO RETURN-CODE
           PERFORM 1000-OPEN
           IF NOT ABEND-FOUND
              PERFORM 2000-LOAD-OUTSF
           END-IF
           IF NOT ABEND-FOUND
              PERFORM 3000-PROCESS-ACK
           END-IF
           IF NOT ABEND-FOUND
              PERFORM 4000-OUTPUT-MISSING
           END-IF
           PERFORM 9000-CLOSE
           IF ABEND-FOUND
              MOVE 8 TO RETURN-CODE
           ELSE
              MOVE 0 TO RETURN-CODE
              DISPLAY "TG520B 正常終了"
              DISPLAY "送信=" WS-OUT-CNT
                      " ACK=" WS-ACK-CNT
                      " 照合済=" WS-MATCH-CNT
              DISPLAY "未着=" WS-MISS-CNT
                      " 重複=" WS-DUP-CNT
                      " 金額不一致=" WS-AMTNG-CNT
                      " 拒否=" WS-REJ-CNT
           END-IF
           GOBACK.

       1000-OPEN.
           OPEN INPUT TGACKNF TGOUTSF
                OUTPUT TGCLJNF TGREJLF
           IF FS-TGACKNF NOT = "00"
              DISPLAY "TGACKNF オープンエラー ST=" FS-TGACKNF
              MOVE "1" TO WS-ABEND-FLG
           END-IF
           IF FS-TGOUTSF NOT = "00"
              DISPLAY "TGOUTSF オープンエラー ST=" FS-TGOUTSF
              MOVE "1" TO WS-ABEND-FLG
           END-IF
           IF FS-TGCLJNF NOT = "00"
              DISPLAY "TGCLJNF オープンエラー ST=" FS-TGCLJNF
              MOVE "1" TO WS-ABEND-FLG
           END-IF
           IF FS-TGREJLF NOT = "00"
              DISPLAY "TGREJLF オープンエラー ST=" FS-TGREJLF
              MOVE "1" TO WS-ABEND-FLG
           END-IF.

       2000-LOAD-OUTSF.
           PERFORM UNTIL OUT-EOF OR ABEND-FOUND
              READ TGOUTSF
                 AT END
                    MOVE "1" TO WS-OUT-EOF
                 NOT AT END
                    IF FS-TGOUTSF = "00"
                       PERFORM 2100-STORE-OUT
                    ELSE
                       DISPLAY "TGOUTSF 読込エラー ST="
                               FS-TGOUTSF
                       MOVE "1" TO WS-ABEND-FLG
                    END-IF
              END-READ
           END-PERFORM.

       2100-STORE-OUT.
           IF OS-SEND-DT = ZERO OR OS-SEND-SEQ = ZERO
              DISPLAY "OUT キーエラー DT=" OS-SEND-DT
              DISPLAY "OUT キーエラー SEQ=" OS-SEND-SEQ
              MOVE "1" TO WS-ABEND-FLG
           ELSE
              IF OS-SEND-AMT <= ZERO
                 DISPLAY "OUT 金額エラー SEQ=" OS-SEND-SEQ
                 MOVE "1" TO WS-ABEND-FLG
              ELSE
                 IF WS-OUT-CNT >= WS-MAX-OUT
                    DISPLAY "OUT テーブルあふれ CNT=" WS-OUT-CNT
                    MOVE "1" TO WS-ABEND-FLG
                 ELSE
                    ADD 1 TO WS-OUT-CNT
                    MOVE OS-SEND-DT
                      TO T-OS-SEND-DT(WS-OUT-CNT)
                    MOVE OS-SEND-SEQ
                      TO T-OS-SEND-SEQ(WS-OUT-CNT)
                    MOVE OS-REMIT-TYPE
                      TO T-OS-REMIT-TYPE(WS-OUT-CNT)
                    MOVE OS-SENDER-BANK
                      TO T-OS-SENDER-BANK(WS-OUT-CNT)
                    MOVE OS-SENDER-BRANCH
                      TO T-OS-SENDER-BRANCH(WS-OUT-CNT)
                    MOVE OS-PAYEE-BANK
                      TO T-OS-PAYEE-BANK(WS-OUT-CNT)
                    MOVE OS-PAYEE-BRANCH
                      TO T-OS-PAYEE-BRANCH(WS-OUT-CNT)
                    MOVE OS-PAYEE-ACCT-TYPE
                      TO T-OS-PAYEE-ACCT-TYPE(WS-OUT-CNT)
                    MOVE OS-PAYEE-ACCT-NO
                      TO T-OS-PAYEE-ACCT-NO(WS-OUT-CNT)
                    MOVE OS-PAYEE-NAME-KANA
                      TO T-OS-PAYEE-NAME-KANA(WS-OUT-CNT)
                    MOVE OS-SEND-AMT
                      TO T-OS-SEND-AMT(WS-OUT-CNT)
                    MOVE OS-SIGNATURE
                      TO T-OS-SIGNATURE(WS-OUT-CNT)
                    MOVE OS-BUILD-PROGRAM
                      TO T-OS-BUILD-PROGRAM(WS-OUT-CNT)
                    MOVE "0" TO T-MATCH-FLG(WS-OUT-CNT)
                 END-IF
              END-IF
           END-IF.

       3000-PROCESS-ACK.
           PERFORM UNTIL ACK-EOF OR ABEND-FOUND
              READ TGACKNF
                 AT END
                    MOVE "1" TO WS-ACK-EOF
                 NOT AT END
                    IF FS-TGACKNF = "00"
                       ADD 1 TO WS-ACK-CNT
                       PERFORM 3100-CHECK-ACK
                    ELSE
                       DISPLAY "TGACKNF 読込エラー ST="
                               FS-TGACKNF
                       MOVE "1" TO WS-ABEND-FLG
                    END-IF
              END-READ
           END-PERFORM.

       3100-CHECK-ACK.
           IF AK-ACK-DT = ZERO OR AK-CENTER-SEQ = ZERO
              MOVE "キーエラー" TO WS-REASON-TEXT
              MOVE "ER" TO WS-MATCH-STATUS
              PERFORM 5000-WRITE-CLJ-ACK
           ELSE
              PERFORM 3200-FIND-OUT
              IF NOT HIT-FOUND
                 MOVE "OUT レコードなし" TO WS-REASON-TEXT
                 MOVE "MS" TO WS-MATCH-STATUS
                 ADD 1 TO WS-MISS-CNT
                 PERFORM 5000-WRITE-CLJ-ACK
              ELSE
                 IF T-MATCHED(WS-HIT-SUB)
                    MOVE "ACK 重複" TO WS-REASON-TEXT
                    MOVE "DP" TO WS-MATCH-STATUS
                    ADD 1 TO WS-DUP-CNT
                    PERFORM 5000-WRITE-CLJ-ACK
                 ELSE
                    PERFORM 3300-JUDGE-MATCH
                 END-IF
              END-IF
           END-IF.

       3200-FIND-OUT.
           MOVE "0" TO WS-HIT-FLG
           MOVE ZERO TO WS-HIT-SUB
           PERFORM VARYING WS-SUB FROM 1 BY 1
             UNTIL WS-SUB > WS-OUT-CNT OR HIT-FOUND
              IF T-OS-SEND-DT(WS-SUB) = AK-ACK-DT
                 AND T-OS-SEND-SEQ(WS-SUB) = AK-CENTER-SEQ
                 MOVE "1" TO WS-HIT-FLG
                 MOVE WS-SUB TO WS-HIT-SUB
              END-IF
           END-PERFORM.

       3300-JUDGE-MATCH.
           IF AK-ITEM-AMT NOT = T-OS-SEND-AMT(WS-HIT-SUB)
              MOVE "金額不一致" TO WS-REASON-TEXT
              MOVE "AM" TO WS-MATCH-STATUS
              ADD 1 TO WS-AMTNG-CNT
              MOVE "1" TO T-MATCH-FLG(WS-HIT-SUB)
              PERFORM 5000-WRITE-CLJ-ACK
           ELSE
              IF AK-RESULT-CD NOT = "00"
                 MOVE "センター拒否" TO WS-REASON-TEXT
                 MOVE "RJ" TO WS-MATCH-STATUS
                 ADD 1 TO WS-REJ-CNT
                 MOVE "1" TO T-MATCH-FLG(WS-HIT-SUB)
                 PERFORM 5000-WRITE-CLJ-ACK
                 PERFORM 6000-WRITE-REJLOG
              ELSE
                 MOVE "照合済" TO WS-REASON-TEXT
                 MOVE "OK" TO WS-MATCH-STATUS
                 ADD 1 TO WS-MATCH-CNT
                 MOVE "1" TO T-MATCH-FLG(WS-HIT-SUB)
                 PERFORM 5000-WRITE-CLJ-ACK
              END-IF
           END-IF.

       4000-OUTPUT-MISSING.
           PERFORM VARYING WS-SUB FROM 1 BY 1
             UNTIL WS-SUB > WS-OUT-CNT OR ABEND-FOUND
              IF T-MATCH-FLG(WS-SUB) NOT = "1"
                 MOVE "ACK 未着" TO WS-REASON-TEXT
                 MOVE "NA" TO WS-MATCH-STATUS
                 ADD 1 TO WS-MISS-CNT
                 PERFORM 5100-WRITE-CLJ-OUT
              END-IF
           END-PERFORM.

       5000-WRITE-CLJ-ACK.
           PERFORM 7000-MAKE-TS
           MOVE SPACE TO TGCLJNF-REC
           MOVE AK-ACK-DT TO CJ-CLEARING-DT
           MOVE AK-CENTER-SEQ TO CJ-CENTER-SEQ
           MOVE "S" TO CJ-DIRECTION
           MOVE AK-TARGET-BANK TO CJ-BANK-CD
           MOVE AK-TARGET-BRANCH TO CJ-BRANCH-CD
           MOVE 1 TO CJ-ITEM-COUNT
           MOVE AK-ITEM-AMT TO CJ-TOTAL-AMT
           MOVE WS-MATCH-STATUS TO CJ-MATCH-STATUS
           MOVE WS-JOURNAL-TS TO CJ-JOURNAL-TS
           WRITE TGCLJNF-REC
           IF FS-TGCLJNF NOT = "00"
              DISPLAY "TGCLJNF 書込エラー ST=" FS-TGCLJNF
              MOVE "1" TO WS-ABEND-FLG
           END-IF.

       5100-WRITE-CLJ-OUT.
           PERFORM 7000-MAKE-TS
           MOVE SPACE TO TGCLJNF-REC
           MOVE T-OS-SEND-DT(WS-SUB) TO CJ-CLEARING-DT
           MOVE T-OS-SEND-SEQ(WS-SUB) TO CJ-CENTER-SEQ
           MOVE "S" TO CJ-DIRECTION
           MOVE T-OS-PAYEE-BANK(WS-SUB) TO CJ-BANK-CD
           MOVE T-OS-PAYEE-BRANCH(WS-SUB) TO CJ-BRANCH-CD
           MOVE 1 TO CJ-ITEM-COUNT
           MOVE T-OS-SEND-AMT(WS-SUB) TO CJ-TOTAL-AMT
           MOVE WS-MATCH-STATUS TO CJ-MATCH-STATUS
           MOVE WS-JOURNAL-TS TO CJ-JOURNAL-TS
           WRITE TGCLJNF-REC
           IF FS-TGCLJNF NOT = "00"
              DISPLAY "TGCLJNF 書込エラー ST=" FS-TGCLJNF
              MOVE "1" TO WS-ABEND-FLG
           END-IF.

       6000-WRITE-REJLOG.
           PERFORM 7000-MAKE-TS
           MOVE SPACE TO LK-REJLOG-PARM
           MOVE WS-PROGRAM-ID TO LK-RL-PROGRAM-ID
           MOVE WS-REASON-TEXT TO LK-RL-REASON
           MOVE WS-JOURNAL-TS TO LK-RL-LOG-TS
           MOVE ZERO TO LK-RL-RET
           CALL "TG920S" USING LK-REJLOG-PARM
           IF LK-RL-RET NOT = ZERO
              DISPLAY "TG920S エラー SEQ=" AK-CENTER-SEQ
              MOVE "1" TO WS-ABEND-FLG
           ELSE
              MOVE SPACE TO TGREJLF-REC
              MOVE AK-ACK-DT TO RJ-REMIT-DT
              MOVE AK-CENTER-SEQ TO RJ-CENTER-SEQ
              MOVE LK-RL-REASON TO RJ-REJ-REASON
              MOVE WS-PROGRAM-ID TO RJ-PROGRAM-ID
              MOVE T-OS-PAYEE-ACCT-NO(WS-HIT-SUB)
                TO RJ-PAYEE-ACCT-NO
              MOVE T-OS-PAYEE-NAME-KANA(WS-HIT-SUB)
                TO RJ-PAYEE-NAME-KANA
              MOVE AK-NOTICE-TEXT TO RJ-MASTER-NAME-KANA
              MOVE AK-ITEM-AMT TO RJ-REJ-AMT
              MOVE LK-RL-LOG-TS TO RJ-LOG-TS
              WRITE TGREJLF-REC
              IF FS-TGREJLF NOT = "00"
                 DISPLAY "TGREJLF 書込エラー ST=" FS-TGREJLF
                 MOVE "1" TO WS-ABEND-FLG
              END-IF
           END-IF.

       7000-MAKE-TS.
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE
           MOVE WS-CUR-YYYYMMDD TO WS-JTS-DATE
           MOVE WS-CUR-HHMMSSCC(1:6) TO WS-JTS-TIME.

       9000-CLOSE.
           CLOSE TGACKNF TGOUTSF TGCLJNF TGREJLF
           IF FS-TGACKNF NOT = "00"
              DISPLAY "TGACKNF クローズエラー ST=" FS-TGACKNF
              MOVE "1" TO WS-ABEND-FLG
           END-IF
           IF FS-TGOUTSF NOT = "00"
              DISPLAY "TGOUTSF クローズエラー ST=" FS-TGOUTSF
              MOVE "1" TO WS-ABEND-FLG
           END-IF
           IF FS-TGCLJNF NOT = "00"
              DISPLAY "TGCLJNF クローズエラー ST=" FS-TGCLJNF
              MOVE "1" TO WS-ABEND-FLG
           END-IF
           IF FS-TGREJLF NOT = "00"
              DISPLAY "TGREJLF クローズエラー ST=" FS-TGREJLF
              MOVE "1" TO WS-ABEND-FLG
           END-IF.
