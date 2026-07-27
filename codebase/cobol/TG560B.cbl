       IDENTIFICATION DIVISION.
       PROGRAM-ID. TG560B.
      *---------------------------------------------------------------*
      * 変更履歴                                                      *
      * 版数  年月日      担当                         概要           *
      * 1.00  平成30年04月 システム部 情報系チーム     新規作成       *
      * 1.10  令和02年10月 システム部 対外系チーム     通知集計対応   *
      * 1.20  令和05年06月 システム部 情報系/対外系チーム 表示整備    *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      * 組戻返却登録バッチ                                            *
      * TGZENFの被仕向明細を検査し返却登録と通知件数を作成する。      *
      *---------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TGZENF ASSIGN TO "TGZENF"
              ORGANIZATION IS LINE SEQUENTIAL
              FILE STATUS IS FS-TGZENF.

           SELECT KZACCTF ASSIGN TO "KZACCTF"
              ORGANIZATION IS INDEXED
              ACCESS MODE IS RANDOM
              RECORD KEY IS AC-ACCT-NO
              FILE STATUS IS FS-KZACCTF.

           SELECT TGNETCF ASSIGN TO "TGNETCF"
              ORGANIZATION IS INDEXED
              ACCESS MODE IS RANDOM
              RECORD KEY IS NC-COUNTER-BANK
              FILE STATUS IS FS-TGNETCF.

           SELECT TGRTRF ASSIGN TO "TGRTRF"
              ORGANIZATION IS INDEXED
              ACCESS MODE IS RANDOM
              RECORD KEY IS RT-RETURN-KEY
              FILE STATUS IS FS-TGRTRF.

           SELECT TGACKF ASSIGN TO "TGACKF"
              ORGANIZATION IS LINE SEQUENTIAL
              FILE STATUS IS FS-TGACKF.

       DATA DIVISION.
       FILE SECTION.

       FD  TGZENF.
           COPY TGZENFC.

       FD  KZACCTF.
           COPY KZACCTC2.

       FD  TGNETCF.
           COPY TGNETCFC.

       FD  TGRTRF.
           COPY TGRTRFC.

       FD  TGACKF.
           COPY TGACKFC.

       WORKING-STORAGE SECTION.
       01  FS-AREA.
           05 FS-TGZENF              PIC X(02) VALUE SPACE.
           05 FS-KZACCTF             PIC X(02) VALUE SPACE.
           05 FS-TGNETCF             PIC X(02) VALUE SPACE.
           05 FS-TGRTRF              PIC X(02) VALUE SPACE.
           05 FS-TGACKF              PIC X(02) VALUE SPACE.

       01  SW-AREA.
           05 SW-EOF                 PIC X(01) VALUE "N".
              88 EOF-TGZENF                    VALUE "Y".
           05 SW-HARD-ERROR          PIC X(01) VALUE "N".
              88 HARD-ERROR                    VALUE "Y".

       01  WK-COUNT-AREA.
           05 WK-READ-CNT            PIC 9(09) VALUE 0.
           05 WK-RECV-CNT            PIC 9(09) VALUE 0.
           05 WK-RETURN-CNT          PIC 9(09) VALUE 0.
           05 WK-ACK-CNT             PIC 9(09) VALUE 0.
           05 WK-IDX                 PIC 9(04) VALUE 0.
           05 WK-FREE-IDX            PIC 9(04) VALUE 0.

       01  WK-RETURN-AREA.
           05 WK-RETURN-SW           PIC X(01) VALUE "N".
              88 RETURN-TARGET                 VALUE "Y".
           05 WK-RETURN-REASON       PIC X(02) VALUE SPACE.
           05 WK-RESULT-CD           PIC X(02) VALUE SPACE.
           05 WK-NOTICE-TEXT         PIC X(40) VALUE SPACE.
           05 WK-DUP-SW              PIC X(01) VALUE "N".
              88 DUPLICATE-RETURN              VALUE "Y".

       01  WK-KEY-AREA.
           05 WK-RT-KEY              PIC X(40) VALUE SPACE.
           05 WK-SEQ-TEXT            PIC X(20) VALUE SPACE.

       01  WK-EDIT-AREA.
           05 WK-DISPLAY-MSG         PIC X(80) VALUE SPACE.
           05 WK-NAME-IN             PIC X(40) VALUE SPACE.
           05 WK-NAME-AC             PIC X(40) VALUE SPACE.

       01  ACK-TABLE.
           05 ACK-ENTRY OCCURS 500 TIMES.
              10 ACK-BANK            PIC X(04) VALUE SPACE.
              10 ACK-COUNT           PIC 9(09) VALUE 0.
              10 ACK-AMT             PIC S9(13)V99 VALUE 0.
              10 ACK-USED            PIC X(01) VALUE "N".

       01  CONST-AREA.
           05 CN-ZEN-RECV            PIC X(02) VALUE "02".
           05 CN-NET-DONE            PIC X(01) VALUE "Y".
           05 CN-ACCT-CLOSED         PIC X(01) VALUE "9".
           05 CN-ACCT-ACTIVE         PIC X(01) VALUE "1".
           05 CN-RSN-CLOSED          PIC X(02) VALUE "01".
           05 CN-RSN-NAME            PIC X(02) VALUE "02".
           05 CN-RSN-DAYNG           PIC X(02) VALUE "03".
           05 CN-AK-TYPE             PIC X(02) VALUE "RT".
           05 CN-RESULT-OK           PIC X(02) VALUE "00".
           05 CN-RESULT-WARN         PIC X(02) VALUE "04".

           COPY LK-NET-PARM.

       PROCEDURE DIVISION.
       MAIN-RTN.
           MOVE 0 TO RETURN-CODE
           PERFORM OPEN-RTN
           IF NOT HARD-ERROR
              PERFORM READ-TGZENF
              PERFORM UNTIL EOF-TGZENF OR HARD-ERROR
                 ADD 1 TO WK-READ-CNT
                 IF ZE-ZEN-TYPE = CN-ZEN-RECV
                    ADD 1 TO WK-RECV-CNT
                    PERFORM DECIDE-RETURN
                    IF RETURN-TARGET
                       PERFORM WRITE-RETURN
                       IF NOT HARD-ERROR
                          PERFORM ADD-ACK-COUNT
                       END-IF
                    END-IF
                 END-IF
                 IF NOT HARD-ERROR
                    PERFORM READ-TGZENF
                 END-IF
              END-PERFORM
           END-IF

           IF NOT HARD-ERROR
              PERFORM WRITE-ACKS
           END-IF

           PERFORM CLOSE-RTN

           IF HARD-ERROR
              MOVE 8 TO RETURN-CODE
           ELSE
              MOVE 0 TO RETURN-CODE
              DISPLAY "TG560B 正常終了 読込="
                      WK-READ-CNT " 被仕向=" WK-RECV-CNT
                      " 返却=" WK-RETURN-CNT " 通知=" WK-ACK-CNT
           END-IF
           GOBACK.

       OPEN-RTN.
           OPEN INPUT TGZENF
           IF FS-TGZENF NOT = "00"
              DISPLAY "TGZENF オープン失敗 状態=" FS-TGZENF
              SET HARD-ERROR TO TRUE
           END-IF

           IF NOT HARD-ERROR
              OPEN INPUT KZACCTF
              IF FS-KZACCTF NOT = "00"
                 DISPLAY "KZACCTF オープン失敗 状態=" FS-KZACCTF
                 SET HARD-ERROR TO TRUE
              END-IF
           END-IF

           IF NOT HARD-ERROR
              OPEN INPUT TGNETCF
              IF FS-TGNETCF NOT = "00"
                 DISPLAY "TGNETCF オープン失敗 状態=" FS-TGNETCF
                 SET HARD-ERROR TO TRUE
              END-IF
           END-IF

           IF NOT HARD-ERROR
              OPEN OUTPUT TGRTRF
              IF FS-TGRTRF NOT = "00"
                 DISPLAY "TGRTRF オープン失敗 状態=" FS-TGRTRF
                 SET HARD-ERROR TO TRUE
              END-IF
           END-IF

           IF NOT HARD-ERROR
              OPEN OUTPUT TGACKF
              IF FS-TGACKF NOT = "00"
                 DISPLAY "TGACKF オープン失敗 状態=" FS-TGACKF
                 SET HARD-ERROR TO TRUE
              END-IF
           END-IF.

       READ-TGZENF.
           READ TGZENF
              AT END
                 SET EOF-TGZENF TO TRUE
              NOT AT END
                 IF FS-TGZENF NOT = "00"
                    DISPLAY "TGZENF 読込失敗 状態=" FS-TGZENF
                    SET HARD-ERROR TO TRUE
                 END-IF
           END-READ.

       DECIDE-RETURN.
           MOVE "N" TO WK-RETURN-SW
           MOVE SPACE TO WK-RETURN-REASON
           MOVE SPACE TO WK-NOTICE-TEXT
           MOVE ZE-REMIT-NAME-KANA TO WK-NAME-IN

           MOVE ZE-RECV-ACCT-NO TO AC-ACCT-NO
           READ KZACCTF
              INVALID KEY
                 IF FS-KZACCTF = "23"
                    MOVE "Y" TO WK-RETURN-SW
                    MOVE CN-RSN-CLOSED TO WK-RETURN-REASON
                    MOVE "口座該当なし" TO WK-NOTICE-TEXT
                 ELSE
                    DISPLAY "KZACCTF 読込失敗 状態=" FS-KZACCTF
                    SET HARD-ERROR TO TRUE
                 END-IF
              NOT INVALID KEY
                 IF FS-KZACCTF NOT = "00"
                    DISPLAY "KZACCTF 読込失敗 状態=" FS-KZACCTF
                    SET HARD-ERROR TO TRUE
                 ELSE
                    PERFORM CHECK-ACCOUNT
                 END-IF
           END-READ

           IF NOT HARD-ERROR AND NOT RETURN-TARGET
              PERFORM CHECK-NET-CONTROL
           END-IF.

       CHECK-ACCOUNT.
           MOVE AC-ACCT-NAME-KANA TO WK-NAME-AC

           IF AC-STATUS = CN-ACCT-CLOSED
              MOVE "Y" TO WK-RETURN-SW
              MOVE CN-RSN-CLOSED TO WK-RETURN-REASON
              MOVE "口座閉鎖" TO WK-NOTICE-TEXT
           ELSE
              IF AC-STATUS NOT = CN-ACCT-ACTIVE
                 MOVE "Y" TO WK-RETURN-SW
                 MOVE CN-RSN-DAYNG TO WK-RETURN-REASON
                 MOVE "口座状態不正" TO WK-NOTICE-TEXT
              ELSE
                 IF WK-NAME-IN NOT = WK-NAME-AC
                    MOVE "Y" TO WK-RETURN-SW
                    MOVE CN-RSN-NAME TO WK-RETURN-REASON
                    MOVE "名義不一致" TO WK-NOTICE-TEXT
                 END-IF
              END-IF
           END-IF.

       CHECK-NET-CONTROL.
           MOVE ZE-COUNTER-BANK TO NC-COUNTER-BANK
           READ TGNETCF
              INVALID KEY
                 IF FS-TGNETCF = "23"
                    MOVE "Y" TO WK-RETURN-SW
                    MOVE CN-RSN-DAYNG TO WK-RETURN-REASON
                    MOVE "相手行制御なし" TO WK-NOTICE-TEXT
                 ELSE
                    DISPLAY "TGNETCF 読込失敗 状態=" FS-TGNETCF
                    SET HARD-ERROR TO TRUE
                 END-IF
              NOT INVALID KEY
                 IF FS-TGNETCF NOT = "00"
                    DISPLAY "TGNETCF 読込失敗 状態=" FS-TGNETCF
                    SET HARD-ERROR TO TRUE
                 ELSE
                    IF NC-IN-FLAG NOT = CN-NET-DONE
                       MOVE "Y" TO WK-RETURN-SW
                       MOVE CN-RSN-DAYNG TO WK-RETURN-REASON
                       MOVE "ネット未完了" TO WK-NOTICE-TEXT
                    ELSE
                       IF NC-CTL-DT NOT = ZE-VALUE-DT
                          MOVE "Y" TO WK-RETURN-SW
                          MOVE CN-RSN-DAYNG TO WK-RETURN-REASON
                          MOVE "制御日不一致" TO WK-NOTICE-TEXT
                       END-IF
                    END-IF
                 END-IF
           END-READ.

       WRITE-RETURN.
           MOVE SPACE TO TGRTRF-REC
           MOVE ZE-CENTER-SEQ TO WK-SEQ-TEXT
           MOVE SPACE TO WK-RT-KEY
           STRING ZE-VALUE-DT DELIMITED BY SIZE
                  ZE-COUNTER-BANK DELIMITED BY SIZE
                  WK-SEQ-TEXT DELIMITED BY SIZE
              INTO WK-RT-KEY
           END-STRING

           MOVE WK-RT-KEY TO RT-RETURN-KEY
           MOVE ZE-VALUE-DT TO RT-VALUE-DT
           MOVE ZE-CENTER-SEQ TO RT-CENTER-SEQ
           MOVE ZE-COUNTER-BANK TO RT-COUNTER-BANK
           MOVE ZE-RECV-ACCT-NO TO RT-RECV-ACCT-NO
           MOVE WK-RETURN-REASON TO RT-RETURN-REASON
           MOVE ZE-REMIT-AMT TO RT-REMIT-AMT
           MOVE "N" TO RT-REENTRY-FLAG

           WRITE TGRTRF-REC
              INVALID KEY
                 IF FS-TGRTRF = "22"
                    MOVE "Y" TO WK-DUP-SW
                    DISPLAY "TGRTRF 重複キー RTKEY=" RT-RETURN-KEY
                 ELSE
                    DISPLAY "TGRTRF 書込失敗 状態=" FS-TGRTRF
                    SET HARD-ERROR TO TRUE
                 END-IF
              NOT INVALID KEY
                 IF FS-TGRTRF NOT = "00"
                    DISPLAY "TGRTRF 書込失敗 状態=" FS-TGRTRF
                    SET HARD-ERROR TO TRUE
                 ELSE
                    ADD 1 TO WK-RETURN-CNT
                    PERFORM CALL-TG935S
                 END-IF
           END-WRITE.

       CALL-TG935S.
           MOVE ZERO TO LK-NET-PAY-AMT
           MOVE ZE-REMIT-AMT TO LK-NET-RECV-AMT
           MOVE ZERO TO LK-NET-AMT
           MOVE ZERO TO LK-NET-RET

           CALL "TG935S" USING LK-NET-PARM

           IF LK-NET-RET NOT = ZERO
              DISPLAY "TG935S 算定失敗 戻り値=" LK-NET-RET
              SET HARD-ERROR TO TRUE
           END-IF.

       ADD-ACK-COUNT.
           MOVE ZERO TO WK-FREE-IDX
           PERFORM VARYING WK-IDX FROM 1 BY 1
             UNTIL WK-IDX > 500
                OR ACK-BANK(WK-IDX) = ZE-COUNTER-BANK
              IF ACK-USED(WK-IDX) = "N" AND WK-FREE-IDX = ZERO
                 MOVE WK-IDX TO WK-FREE-IDX
              END-IF
           END-PERFORM

           IF WK-IDX <= 500
              ADD 1 TO ACK-COUNT(WK-IDX)
              ADD ZE-REMIT-AMT TO ACK-AMT(WK-IDX)
           ELSE
              IF WK-FREE-IDX > ZERO
                 MOVE ZE-COUNTER-BANK TO ACK-BANK(WK-FREE-IDX)
                 MOVE 1 TO ACK-COUNT(WK-FREE-IDX)
                 MOVE ZE-REMIT-AMT TO ACK-AMT(WK-FREE-IDX)
                 MOVE "Y" TO ACK-USED(WK-FREE-IDX)
              ELSE
                 DISPLAY "TGACKF 集計表オーバー"
                 SET HARD-ERROR TO TRUE
              END-IF
           END-IF.

       WRITE-ACKS.
           PERFORM VARYING WK-IDX FROM 1 BY 1
             UNTIL WK-IDX > 500 OR HARD-ERROR
              IF ACK-USED(WK-IDX) = "Y"
                 MOVE SPACE TO TGACKF-REC
                 MOVE ZE-VALUE-DT TO AK-VALUE-DT
                 MOVE WK-IDX TO AK-CENTER-SEQ
                 MOVE CN-AK-TYPE TO AK-NOTICE-TYPE
                 MOVE ACK-BANK(WK-IDX) TO AK-COUNTER-BANK
                 IF ACK-COUNT(WK-IDX) > ZERO
                    MOVE CN-RESULT-OK TO AK-RESULT-CD
                 ELSE
                    MOVE CN-RESULT-WARN TO AK-RESULT-CD
                 END-IF
                 MOVE ACK-COUNT(WK-IDX) TO AK-ITEM-COUNT
                 MOVE "返却通知件数" TO AK-NOTICE-TEXT
                 WRITE TGACKF-REC
                 IF FS-TGACKF NOT = "00"
                    DISPLAY "TGACKF 書込失敗 状態=" FS-TGACKF
                    SET HARD-ERROR TO TRUE
                 ELSE
                    ADD 1 TO WK-ACK-CNT
                 END-IF
              END-IF
           END-PERFORM.

       CLOSE-RTN.
           CLOSE TGZENF
           IF FS-TGZENF NOT = "00"
              DISPLAY "TGZENF クローズ失敗 状態=" FS-TGZENF
              SET HARD-ERROR TO TRUE
           END-IF

           CLOSE KZACCTF
           IF FS-KZACCTF NOT = "00"
              DISPLAY "KZACCTF クローズ失敗 状態=" FS-KZACCTF
              SET HARD-ERROR TO TRUE
           END-IF

           CLOSE TGNETCF
           IF FS-TGNETCF NOT = "00"
              DISPLAY "TGNETCF クローズ失敗 状態=" FS-TGNETCF
              SET HARD-ERROR TO TRUE
           END-IF

           CLOSE TGRTRF
           IF FS-TGRTRF NOT = "00"
              DISPLAY "TGRTRF クローズ失敗 状態=" FS-TGRTRF
              SET HARD-ERROR TO TRUE
           END-IF

           CLOSE TGACKF
           IF FS-TGACKF NOT = "00"
              DISPLAY "TGACKF クローズ失敗 状態=" FS-TGACKF
              SET HARD-ERROR TO TRUE
           END-IF.
