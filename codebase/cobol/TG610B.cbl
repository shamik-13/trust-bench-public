       IDENTIFICATION DIVISION.
       PROGRAM-ID. TG610B.
      *================================================================*
      * 変更履歴                                                       *
      * 版数  年月日        担当                         概要          *
      * 1.00  平成28年04月  システム部 情報系チーム       新規作成      *
      * 1.01  令和02年10月  システム部 対外系チーム       電文対応      *
      * 1.02  令和05年06月  システム部 情報系/対外系チーム 精度向上     *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TGZENF ASSIGN TO "TGZENF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-TGZENF.
           SELECT TGRTRF ASSIGN TO "TGRTRF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS RT-RETURN-KEY
               FILE STATUS IS FS-TGRTRF.
           SELECT TGCLRF ASSIGN TO "TGCLRF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-TGCLRF.
           SELECT TGERRF ASSIGN TO "TGERRF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-TGERRF.
           SELECT TGBANKF ASSIGN TO "TGBANKF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS BK-COUNTER-BANK
               FILE STATUS IS FS-TGBANKF.
           SELECT TGACKF ASSIGN TO "TGACKF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-TGACKF.

       DATA DIVISION.
       FILE SECTION.
       FD  TGZENF.
           COPY TGZENFC.
       FD  TGRTRF.
           COPY TGRTRFC.
       FD  TGCLRF.
           COPY TGCLRFC.
       FD  TGERRF.
           COPY TGERRFC.
       FD  TGBANKF.
           COPY TGBANKFC.
       FD  TGACKF.
           COPY TGACKFC.

       WORKING-STORAGE SECTION.
           COPY LK-NET-PARM.

       01  FS-AREA.
           05 FS-TGZENF              PIC XX VALUE SPACE.
           05 FS-TGRTRF              PIC XX VALUE SPACE.
           05 FS-TGCLRF              PIC XX VALUE SPACE.
           05 FS-TGERRF              PIC XX VALUE SPACE.
           05 FS-TGBANKF             PIC XX VALUE SPACE.
           05 FS-TGACKF              PIC XX VALUE SPACE.

       01  SW-AREA.
           05 EOF-TGZENF             PIC X VALUE "N".
           05 EOF-TGRTRF             PIC X VALUE "N".
           05 EOF-TGCLRF             PIC X VALUE "N".
           05 EOF-TGERRF             PIC X VALUE "N".
           05 ABEND-SW               PIC X VALUE "N".

       01  CTL-AREA.
           05 WK-VALUE-DT            PIC 9(08) VALUE ZERO.
           05 WK-FIRST-DT            PIC 9(08) VALUE ZERO.
           05 WK-CENTER-SEQ          PIC 9(08) VALUE ZERO.
           05 WK-OUT-SEQ             PIC 9(08) VALUE ZERO.
           05 WK-IDX                 PIC 9(04) COMP VALUE ZERO.
           05 WK-HIT-IDX             PIC 9(04) COMP VALUE ZERO.
           05 WK-MAX-IDX             PIC 9(04) COMP VALUE ZERO.
           05 WK-FOUND-SW            PIC X VALUE "N".
           05 WK-FIND-BANK           PIC X(04) VALUE SPACE.
           05 WK-FIND-BRANCH         PIC X(03) VALUE SPACE.

       01  BANK-SUM-TABLE.
           05 BANK-SUM OCCURS 500 TIMES.
              10 TB-BANK             PIC X(04) VALUE SPACE.
              10 TB-BRANCH           PIC X(03) VALUE SPACE.
              10 TB-PAY-CNT          PIC 9(07) VALUE ZERO.
              10 TB-PAY-AMT          PIC S9(13) VALUE ZERO.
              10 TB-RECV-CNT         PIC 9(07) VALUE ZERO.
              10 TB-RECV-AMT         PIC S9(13) VALUE ZERO.
              10 TB-RTN-CNT          PIC 9(07) VALUE ZERO.
              10 TB-RTN-AMT          PIC S9(13) VALUE ZERO.
              10 TB-ERR-CNT          PIC 9(07) VALUE ZERO.
              10 TB-CLR-CNT          PIC 9(07) VALUE ZERO.
              10 TB-CLR-AMT          PIC S9(13) VALUE ZERO.
              10 TB-NET-AMT          PIC S9(13) VALUE ZERO.
              10 TB-CLR-STAT         PIC X VALUE SPACE.
              10 TB-BANK-STAT        PIC X VALUE SPACE.
              10 TB-REASON           PIC X(06) VALUE SPACE.
              10 TB-ERR-CD           PIC X(06) VALUE SPACE.

       01  EDIT-AREA.
           05 ED-AMT                 PIC -(13)9.
           05 ED-CNT                 PIC Z(6)9.
           05 MSG-TEXT               PIC X(120) VALUE SPACE.
           05 WK-RESULT-CD           PIC X(02) VALUE SPACE.
           05 WK-NOTICE-TYPE         PIC X(02) VALUE SPACE.
           05 WK-ITEM-COUNT          PIC 9(07) VALUE ZERO.
           05 WK-BANK-NAME           PIC X(40) VALUE SPACE.

       PROCEDURE DIVISION.
       MAIN-RTN.
           MOVE 0 TO RETURN-CODE
           PERFORM INIT-RTN
           IF ABEND-SW = "N"
              PERFORM READ-TGZENF-RTN
              PERFORM UNTIL EOF-TGZENF = "Y" OR ABEND-SW = "Y"
                 PERFORM EDIT-ZEN-RTN
                 PERFORM READ-TGZENF-RTN
              END-PERFORM
           END-IF
           IF ABEND-SW = "N"
              PERFORM READ-TGRTRF-RTN
              PERFORM UNTIL EOF-TGRTRF = "Y" OR ABEND-SW = "Y"
                 PERFORM EDIT-RTR-RTN
                 PERFORM READ-TGRTRF-RTN
              END-PERFORM
           END-IF
           IF ABEND-SW = "N"
              PERFORM READ-TGERRF-RTN
              PERFORM UNTIL EOF-TGERRF = "Y" OR ABEND-SW = "Y"
                 PERFORM EDIT-ERR-RTN
                 PERFORM READ-TGERRF-RTN
              END-PERFORM
           END-IF
           IF ABEND-SW = "N"
              PERFORM READ-TGCLRF-RTN
              PERFORM UNTIL EOF-TGCLRF = "Y" OR ABEND-SW = "Y"
                 PERFORM EDIT-CLR-RTN
                 PERFORM READ-TGCLRF-RTN
              END-PERFORM
           END-IF
           IF ABEND-SW = "N"
              PERFORM MAKE-ACK-RTN
           END-IF
           PERFORM CLOSE-RTN
           IF ABEND-SW = "Y"
              MOVE 8 TO RETURN-CODE
           END-IF
           GOBACK.

       INIT-RTN.
           OPEN INPUT TGZENF TGRTRF TGCLRF TGERRF TGBANKF
                OUTPUT TGACKF
           IF FS-TGZENF NOT = "00"
              DISPLAY "TGZENF オープンエラー ST=" FS-TGZENF
              MOVE "Y" TO ABEND-SW
           END-IF
           IF FS-TGRTRF NOT = "00"
              DISPLAY "TGRTRF オープンエラー ST=" FS-TGRTRF
              MOVE "Y" TO ABEND-SW
           END-IF
           IF FS-TGCLRF NOT = "00"
              DISPLAY "TGCLRF オープンエラー ST=" FS-TGCLRF
              MOVE "Y" TO ABEND-SW
           END-IF
           IF FS-TGERRF NOT = "00"
              DISPLAY "TGERRF オープンエラー ST=" FS-TGERRF
              MOVE "Y" TO ABEND-SW
           END-IF
           IF FS-TGBANKF NOT = "00"
              DISPLAY "TGBANKF オープンエラー ST=" FS-TGBANKF
              MOVE "Y" TO ABEND-SW
           END-IF
           IF FS-TGACKF NOT = "00"
              DISPLAY "TGACKF オープンエラー ST=" FS-TGACKF
              MOVE "Y" TO ABEND-SW
           END-IF.

       READ-TGZENF-RTN.
           READ TGZENF
              AT END
                 MOVE "Y" TO EOF-TGZENF
              NOT AT END
                 IF FS-TGZENF NOT = "00"
                    DISPLAY "TGZENF 読込エラー ST=" FS-TGZENF
                    MOVE "Y" TO ABEND-SW
                 END-IF
           END-READ.

       READ-TGRTRF-RTN.
           READ TGRTRF NEXT RECORD
              AT END
                 MOVE "Y" TO EOF-TGRTRF
              NOT AT END
                 IF FS-TGRTRF NOT = "00"
                    DISPLAY "TGRTRF 読込エラー ST=" FS-TGRTRF
                    MOVE "Y" TO ABEND-SW
                 END-IF
           END-READ.

       READ-TGERRF-RTN.
           READ TGERRF
              AT END
                 MOVE "Y" TO EOF-TGERRF
              NOT AT END
                 IF FS-TGERRF NOT = "00"
                    DISPLAY "TGERRF 読込エラー ST=" FS-TGERRF
                    MOVE "Y" TO ABEND-SW
                 END-IF
           END-READ.

       READ-TGCLRF-RTN.
           READ TGCLRF
              AT END
                 MOVE "Y" TO EOF-TGCLRF
              NOT AT END
                 IF FS-TGCLRF NOT = "00"
                    DISPLAY "TGCLRF 読込エラー ST=" FS-TGCLRF
                    MOVE "Y" TO ABEND-SW
                 END-IF
           END-READ.

       EDIT-ZEN-RTN.
           IF ZE-ZEN-TYPE NOT = "01"
              AND ZE-ZEN-TYPE NOT = "02"
              DISPLAY "ZEN 種別エラー SEQ=" ZE-CENTER-SEQ
              PERFORM ADD-ERR-BY-ZEN-RTN
           ELSE
              IF WK-FIRST-DT = ZERO
                 MOVE ZE-VALUE-DT TO WK-FIRST-DT
              END-IF
              MOVE ZE-COUNTER-BANK TO WK-FIND-BANK
              MOVE ZE-COUNTER-BRANCH TO WK-FIND-BRANCH
              PERFORM FIND-BANK-RTN
              IF WK-HIT-IDX > ZERO
                 MOVE ZE-VALUE-DT TO WK-VALUE-DT
                 IF ZE-CENTER-SEQ > WK-CENTER-SEQ
                    MOVE ZE-CENTER-SEQ TO WK-CENTER-SEQ
                 END-IF
                 IF ZE-ZEN-TYPE = "01"
                    ADD 1 TO TB-PAY-CNT(WK-HIT-IDX)
                    ADD ZE-REMIT-AMT TO TB-PAY-AMT(WK-HIT-IDX)
                 ELSE
                    ADD 1 TO TB-RECV-CNT(WK-HIT-IDX)
                    ADD ZE-REMIT-AMT TO TB-RECV-AMT(WK-HIT-IDX)
                 END-IF
              END-IF
           END-IF.

       ADD-ERR-BY-ZEN-RTN.
           MOVE ZE-COUNTER-BANK TO WK-FIND-BANK
           MOVE ZE-COUNTER-BRANCH TO WK-FIND-BRANCH
           PERFORM FIND-BANK-RTN
           IF WK-HIT-IDX > ZERO
              ADD 1 TO TB-ERR-CNT(WK-HIT-IDX)
              IF TB-ERR-CD(WK-HIT-IDX) = SPACE
                 MOVE "ZETYPE" TO TB-ERR-CD(WK-HIT-IDX)
              END-IF
           END-IF.

       EDIT-RTR-RTN.
           MOVE RT-COUNTER-BANK TO WK-FIND-BANK
           MOVE SPACE TO WK-FIND-BRANCH
           PERFORM FIND-BANK-RTN
           IF WK-HIT-IDX > ZERO
              ADD 1 TO TB-RTN-CNT(WK-HIT-IDX)
              ADD RT-REMIT-AMT TO TB-RTN-AMT(WK-HIT-IDX)
              IF RT-CENTER-SEQ > WK-CENTER-SEQ
                 MOVE RT-CENTER-SEQ TO WK-CENTER-SEQ
              END-IF
              IF TB-REASON(WK-HIT-IDX) = SPACE
                 MOVE RT-RETURN-REASON TO TB-REASON(WK-HIT-IDX)
              END-IF
              IF RT-REENTRY-FLAG NOT = "Y"
                 AND RT-REENTRY-FLAG NOT = "N"
                 ADD 1 TO TB-ERR-CNT(WK-HIT-IDX)
                 IF TB-ERR-CD(WK-HIT-IDX) = SPACE
                    MOVE "RTRENT" TO TB-ERR-CD(WK-HIT-IDX)
                 END-IF
              END-IF
           END-IF.

       EDIT-ERR-RTN.
           PERFORM FIND-BANK-FROM-RAW-RTN
           IF WK-HIT-IDX > ZERO
              ADD 1 TO TB-ERR-CNT(WK-HIT-IDX)
              IF ER-CENTER-SEQ > WK-CENTER-SEQ
                 MOVE ER-CENTER-SEQ TO WK-CENTER-SEQ
              END-IF
              IF TB-ERR-CD(WK-HIT-IDX) = SPACE
                 MOVE ER-ERROR-CD TO TB-ERR-CD(WK-HIT-IDX)
              END-IF
           END-IF.

       FIND-BANK-FROM-RAW-RTN.
           MOVE ZERO TO WK-HIT-IDX
           IF ER-RAW-RECORD(1:4) NOT = SPACE
              MOVE ER-RAW-RECORD(1:4) TO WK-FIND-BANK
              MOVE SPACE TO WK-FIND-BRANCH
              PERFORM FIND-BANK-RTN
           ELSE
              DISPLAY "ERR RAW 銀行未設定 SEQ="
                      ER-CENTER-SEQ
           END-IF.

       EDIT-CLR-RTN.
           MOVE CL-COUNTER-BANK TO WK-FIND-BANK
           MOVE SPACE TO WK-FIND-BRANCH
           PERFORM FIND-BANK-RTN
           IF WK-HIT-IDX > ZERO
              ADD CL-ITEM-COUNT TO TB-CLR-CNT(WK-HIT-IDX)
              ADD CL-NET-AMT TO TB-CLR-AMT(WK-HIT-IDX)
              MOVE CL-SETTLE-STATUS TO TB-CLR-STAT(WK-HIT-IDX)
              IF CL-SETTLE-STATUS NOT = "S"
                 AND CL-SETTLE-STATUS NOT = "H"
                 ADD 1 TO TB-ERR-CNT(WK-HIT-IDX)
                 IF TB-ERR-CD(WK-HIT-IDX) = SPACE
                    MOVE "CLSTAT" TO TB-ERR-CD(WK-HIT-IDX)
                 END-IF
              END-IF
           END-IF.

       FIND-BANK-RTN.
           MOVE ZERO TO WK-HIT-IDX
           MOVE "N" TO WK-FOUND-SW
           PERFORM VARYING WK-IDX FROM 1 BY 1
              UNTIL WK-IDX > WK-MAX-IDX OR WK-FOUND-SW = "Y"
              IF TB-BANK(WK-IDX) = WK-FIND-BANK
                 MOVE WK-IDX TO WK-HIT-IDX
                 MOVE "Y" TO WK-FOUND-SW
              END-IF
           END-PERFORM
           IF WK-HIT-IDX = ZERO
              IF WK-MAX-IDX < 500
                 ADD 1 TO WK-MAX-IDX
                 MOVE WK-MAX-IDX TO WK-HIT-IDX
                 MOVE WK-FIND-BANK TO TB-BANK(WK-HIT-IDX)
                 MOVE WK-FIND-BRANCH TO TB-BRANCH(WK-HIT-IDX)
                 PERFORM READ-BANK-RTN
              ELSE
                 DISPLAY "銀行テーブル満杯 BANK="
                         WK-FIND-BANK
                 MOVE "Y" TO ABEND-SW
              END-IF
           END-IF.

       READ-BANK-RTN.
           MOVE TB-BANK(WK-HIT-IDX) TO BK-COUNTER-BANK
           READ TGBANKF KEY IS BK-COUNTER-BANK
              INVALID KEY
                 MOVE "9" TO TB-BANK-STAT(WK-HIT-IDX)
                 DISPLAY "銀行マスタ未登録 BANK="
                         TB-BANK(WK-HIT-IDX)
              NOT INVALID KEY
                 MOVE BK-STATUS TO TB-BANK-STAT(WK-HIT-IDX)
                 MOVE BK-BANK-NAME-KANA TO WK-BANK-NAME
                 IF BK-STATUS NOT = "1"
                    DISPLAY "銀行マスタ状態エラー BANK="
                            BK-COUNTER-BANK
                 END-IF
           END-READ
           IF FS-TGBANKF NOT = "00"
              AND FS-TGBANKF NOT = "23"
              DISPLAY "TGBANKF 読込エラー ST=" FS-TGBANKF
              MOVE "Y" TO ABEND-SW
           END-IF.

       MAKE-ACK-RTN.
           PERFORM VARYING WK-IDX FROM 1 BY 1
              UNTIL WK-IDX > WK-MAX-IDX OR ABEND-SW = "Y"
              PERFORM CALC-NET-RTN
              IF TB-PAY-CNT(WK-IDX) > ZERO
                 MOVE "01" TO WK-NOTICE-TYPE
                 MOVE "00" TO WK-RESULT-CD
                 MOVE TB-PAY-CNT(WK-IDX) TO WK-ITEM-COUNT
                 MOVE TB-PAY-AMT(WK-IDX) TO ED-AMT
                 STRING "支払受付 金額=" DELIMITED BY SIZE
                        ED-AMT DELIMITED BY SIZE
                        INTO MSG-TEXT
                 END-STRING
                 PERFORM WRITE-ACK-RTN
              END-IF
              IF TB-RECV-CNT(WK-IDX) > ZERO
                 MOVE "02" TO WK-NOTICE-TYPE
                 MOVE "00" TO WK-RESULT-CD
                 MOVE TB-RECV-CNT(WK-IDX) TO WK-ITEM-COUNT
                 MOVE TB-RECV-AMT(WK-IDX) TO ED-AMT
                 STRING "受取受付 金額=" DELIMITED BY SIZE
                        ED-AMT DELIMITED BY SIZE
                        INTO MSG-TEXT
                 END-STRING
                 PERFORM WRITE-ACK-RTN
              END-IF
              IF TB-RTN-CNT(WK-IDX) > ZERO
                 MOVE "03" TO WK-NOTICE-TYPE
                 MOVE "10" TO WK-RESULT-CD
                 MOVE TB-RTN-CNT(WK-IDX) TO WK-ITEM-COUNT
                 STRING "返戻理由=" DELIMITED BY SIZE
                        TB-REASON(WK-IDX) DELIMITED BY SIZE
                        INTO MSG-TEXT
                 END-STRING
                 PERFORM WRITE-ACK-RTN
              END-IF
              IF TB-ERR-CNT(WK-IDX) > ZERO
                 MOVE "04" TO WK-NOTICE-TYPE
                 MOVE "20" TO WK-RESULT-CD
                 MOVE TB-ERR-CNT(WK-IDX) TO WK-ITEM-COUNT
                 STRING "形式エラー コード=" DELIMITED BY SIZE
                        TB-ERR-CD(WK-IDX) DELIMITED BY SIZE
                        INTO MSG-TEXT
                 END-STRING
                 PERFORM WRITE-ACK-RTN
              END-IF
              IF TB-CLR-CNT(WK-IDX) > ZERO
                 MOVE "05" TO WK-NOTICE-TYPE
                 IF TB-CLR-STAT(WK-IDX) = "S"
                    MOVE "00" TO WK-RESULT-CD
                 ELSE
                    MOVE "30" TO WK-RESULT-CD
                 END-IF
                 MOVE TB-CLR-CNT(WK-IDX) TO WK-ITEM-COUNT
                 MOVE TB-NET-AMT(WK-IDX) TO ED-AMT
                 STRING "清算差引=" DELIMITED BY SIZE
                        ED-AMT DELIMITED BY SIZE
                        " 状態=" DELIMITED BY SIZE
                        TB-CLR-STAT(WK-IDX) DELIMITED BY SIZE
                        INTO MSG-TEXT
                 END-STRING
                 PERFORM WRITE-ACK-RTN
              END-IF
           END-PERFORM.

       CALC-NET-RTN.
           MOVE TB-PAY-AMT(WK-IDX) TO LK-NET-PAY-AMT
           MOVE TB-RECV-AMT(WK-IDX) TO LK-NET-RECV-AMT
           MOVE ZERO TO LK-NET-AMT
           MOVE SPACE TO LK-NET-RET
           CALL "TG935S" USING LK-NET-PARM
           IF LK-NET-RET = "00"
              MOVE LK-NET-AMT TO TB-NET-AMT(WK-IDX)
           ELSE
              DISPLAY "TG935S エラー BANK=" TB-BANK(WK-IDX)
                      " RET=" LK-NET-RET
              MOVE TB-CLR-AMT(WK-IDX) TO TB-NET-AMT(WK-IDX)
           END-IF.

       WRITE-ACK-RTN.
           ADD 1 TO WK-OUT-SEQ
           MOVE SPACE TO TGACKF-REC
           IF WK-VALUE-DT = ZERO
              MOVE WK-FIRST-DT TO AK-VALUE-DT
           ELSE
              MOVE WK-VALUE-DT TO AK-VALUE-DT
           END-IF
           MOVE WK-OUT-SEQ TO AK-CENTER-SEQ
           MOVE WK-NOTICE-TYPE TO AK-NOTICE-TYPE
           MOVE TB-BANK(WK-IDX) TO AK-COUNTER-BANK
           MOVE WK-RESULT-CD TO AK-RESULT-CD
           MOVE WK-ITEM-COUNT TO AK-ITEM-COUNT
           MOVE MSG-TEXT TO AK-NOTICE-TEXT
           WRITE TGACKF-REC
           IF FS-TGACKF NOT = "00"
              DISPLAY "TGACKF 書込エラー ST=" FS-TGACKF
              MOVE "Y" TO ABEND-SW
           END-IF
           MOVE SPACE TO MSG-TEXT.

       CLOSE-RTN.
           CLOSE TGZENF TGRTRF TGCLRF TGERRF TGBANKF TGACKF
           IF FS-TGZENF NOT = "00"
              DISPLAY "TGZENF クローズエラー ST=" FS-TGZENF
              MOVE "Y" TO ABEND-SW
           END-IF
           IF FS-TGRTRF NOT = "00"
              DISPLAY "TGRTRF クローズエラー ST=" FS-TGRTRF
              MOVE "Y" TO ABEND-SW
           END-IF
           IF FS-TGCLRF NOT = "00"
              DISPLAY "TGCLRF クローズエラー ST=" FS-TGCLRF
              MOVE "Y" TO ABEND-SW
           END-IF
           IF FS-TGERRF NOT = "00"
              DISPLAY "TGERRF クローズエラー ST=" FS-TGERRF
              MOVE "Y" TO ABEND-SW
           END-IF
           IF FS-TGBANKF NOT = "00"
              DISPLAY "TGBANKF クローズエラー ST=" FS-TGBANKF
              MOVE "Y" TO ABEND-SW
           END-IF
           IF FS-TGACKF NOT = "00"
              DISPLAY "TGACKF クローズエラー ST=" FS-TGACKF
              MOVE "Y" TO ABEND-SW
           END-IF.
