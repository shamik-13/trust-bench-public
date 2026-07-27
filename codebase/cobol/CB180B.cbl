       IDENTIFICATION DIVISION.
       PROGRAM-ID. CB180B.
      *
      * 督促インタフェースファイル作成バッチ
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CDOSF ASSIGN TO "CDOSF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS OS-CARD-NO
               FILE STATUS IS FS-CDOSF.
           SELECT CDLATEF ASSIGN TO "CDLATEF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS LAT-CARD-NO
               FILE STATUS IS FS-CDLATEF.
           SELECT CDRTRYF ASSIGN TO "CDRTRYF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS RTY-RETRY-ID
               FILE STATUS IS FS-CDRTRYF.
           SELECT CDDUNF ASSIGN TO "CDDUNF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-CDDUNF.
           SELECT CDHISTF ASSIGN TO "CDHISTF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS HIS-CARD-NO
               FILE STATUS IS FS-CDHISTF.

       DATA DIVISION.
       FILE SECTION.
       FD  CDOSF.
           COPY CDOSFC.
       FD  CDLATEF.
           COPY CDLATEC.
       FD  CDRTRYF.
           COPY CDRTRYC.
       FD  CDDUNF.
           COPY CDDUNC.
       FD  CDHISTF.
           COPY CDHISTC.

       WORKING-STORAGE SECTION.
       01  FILE-STATUS-AREA.
           05 FS-CDOSF              PIC XX VALUE SPACES.
           05 FS-CDLATEF            PIC XX VALUE SPACES.
           05 FS-CDRTRYF            PIC XX VALUE SPACES.
           05 FS-CDDUNF             PIC XX VALUE SPACES.
           05 FS-CDHISTF            PIC XX VALUE SPACES.

       01  SWITCH-AREA.
           05 SW-EOF-CDRTRYF        PIC X VALUE "N".
              88 EOF-CDRTRYF              VALUE "Y".
           05 SW-DUPLICATE          PIC X VALUE "N".
              88 DUPLICATE-NOTICE         VALUE "Y".
           05 SW-HIST-FOUND         PIC X VALUE "N".
              88 HIST-FOUND               VALUE "Y".
           05 SW-HARD-ERROR         PIC X VALUE "N".
              88 HARD-ERROR               VALUE "Y".

       01  COUNT-AREA.
           05 CNT-READ              PIC 9(9) VALUE ZERO.
           05 CNT-WRITE             PIC 9(9) VALUE ZERO.
           05 CNT-SKIP              PIC 9(9) VALUE ZERO.
           05 CNT-DUP               PIC 9(9) VALUE ZERO.
           05 CNT-ERR               PIC 9(9) VALUE ZERO.

       01  WORK-AREA.
           05 WK-CURRENT-DATE       PIC 9(8) VALUE ZERO.
           05 WK-CURRENT-TIME       PIC 9(8) VALUE ZERO.
           05 WK-DELINQ-AMT         PIC S9(13) COMP-3 VALUE ZERO.
           05 WK-BALANCE-AMT        PIC S9(13) COMP-3 VALUE ZERO.
           05 WK-NOTICE-SEQ         PIC 9(7) VALUE ZERO.
           05 WK-HIS-EVENT-SEQ      PIC 9(7) VALUE ZERO.
           05 WK-NOTICE-ID          PIC X(20) VALUE SPACES.
           05 WK-RANK               PIC X VALUE SPACE.
           05 WK-CHANNEL            PIC X(2) VALUE SPACES.
           05 WK-SOURCE-PGM         PIC X(8) VALUE "CB180B".
           05 WK-REASON             PIC X(40) VALUE SPACES.

       01  CONSTANT-AREA.
           05 CN-RTY-ACTIVE         PIC X VALUE "A".
           05 CN-RTY-NG             PIC X VALUE "N".
           05 CN-RTY-STOP           PIC X VALUE "S".
           05 CN-EVT-DUN            PIC X(3) VALUE "DUN".
           05 CN-RANK-A             PIC X VALUE "A".
           05 CN-RANK-B             PIC X VALUE "B".
           05 CN-RANK-C             PIC X VALUE "C".
           05 CN-CH-MAIL            PIC X(2) VALUE "01".
           05 CN-CH-POST            PIC X(2) VALUE "02".
           05 CN-CH-CALL            PIC X(2) VALUE "03".

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE 8 TO RETURN-CODE
           PERFORM 1000-INITIALIZE
           IF NOT HARD-ERROR
               PERFORM 2000-PROCESS
                   UNTIL EOF-CDRTRYF OR HARD-ERROR
           END-IF
           PERFORM 9000-FINALIZE
           GOBACK.

       1000-INITIALIZE.
           ACCEPT WK-CURRENT-DATE FROM DATE YYYYMMDD
           ACCEPT WK-CURRENT-TIME FROM TIME
           DISPLAY "CB180B 開始 処理日=" WK-CURRENT-DATE

           OPEN INPUT CDOSF
           IF FS-CDOSF NOT = "00"
               MOVE "CDOSF オープン失敗" TO WK-REASON
               PERFORM 9100-ABEND
           END-IF

           IF NOT HARD-ERROR
               OPEN INPUT CDLATEF
               IF FS-CDLATEF NOT = "00"
                   MOVE "CDLATEF オープン失敗" TO WK-REASON
                   PERFORM 9100-ABEND
               END-IF
           END-IF

           IF NOT HARD-ERROR
               OPEN INPUT CDRTRYF
               IF FS-CDRTRYF NOT = "00"
                   MOVE "CDRTRYF オープン失敗" TO WK-REASON
                   PERFORM 9100-ABEND
               END-IF
           END-IF

           IF NOT HARD-ERROR
               OPEN OUTPUT CDDUNF
               IF FS-CDDUNF NOT = "00"
                   MOVE "CDDUNF オープン失敗" TO WK-REASON
                   PERFORM 9100-ABEND
               END-IF
           END-IF

           IF NOT HARD-ERROR
               OPEN I-O CDHISTF
               IF FS-CDHISTF NOT = "00"
                   MOVE "CDHISTF オープン失敗" TO WK-REASON
                   PERFORM 9100-ABEND
               END-IF
           END-IF

           IF NOT HARD-ERROR
               PERFORM 2100-READ-RETRY
           END-IF.

       2000-PROCESS.
           ADD 1 TO CNT-READ
           MOVE "N" TO SW-DUPLICATE
           MOVE "N" TO SW-HIST-FOUND
           MOVE ZERO TO WK-DELINQ-AMT
           MOVE ZERO TO WK-BALANCE-AMT
           MOVE SPACE TO WK-RANK
           MOVE SPACES TO WK-CHANNEL

           EVALUATE RTY-RETRY-STATUS
               WHEN CN-RTY-ACTIVE
               WHEN CN-RTY-NG
                   PERFORM 2200-READ-LATE
                   IF NOT HARD-ERROR
                       PERFORM 2300-READ-OS
                   END-IF
                   IF NOT HARD-ERROR
                       PERFORM 2400-VALIDATE-AND-CALC
                   END-IF
                   IF NOT HARD-ERROR
                   AND WK-DELINQ-AMT > ZERO
                       PERFORM 2500-CHECK-HISTORY
                   END-IF
                   IF NOT HARD-ERROR
                   AND WK-DELINQ-AMT > ZERO
                   AND NOT DUPLICATE-NOTICE
                       PERFORM 2600-DECIDE-NOTICE
                       PERFORM 2700-WRITE-DUNNING
                       IF NOT HARD-ERROR
                           PERFORM 2800-WRITE-HISTORY
                       END-IF
                   END-IF
               WHEN CN-RTY-STOP
                   ADD 1 TO CNT-SKIP
               WHEN OTHER
                   ADD 1 TO CNT-ERR
                   DISPLAY "再請求状態不正 ID=" RTY-RETRY-ID
                   DISPLAY "状態=" RTY-RETRY-STATUS
           END-EVALUATE

           IF NOT HARD-ERROR
               PERFORM 2100-READ-RETRY
           END-IF.

       2100-READ-RETRY.
           READ CDRTRYF NEXT RECORD
               AT END
                   SET EOF-CDRTRYF TO TRUE
               NOT AT END
                   CONTINUE
           END-READ
           IF FS-CDRTRYF NOT = "00"
           AND FS-CDRTRYF NOT = "10"
               MOVE "CDRTRYF 読込失敗" TO WK-REASON
               PERFORM 9100-ABEND
           END-IF.

       2200-READ-LATE.
           MOVE RTY-CARD-NO TO LAT-CARD-NO
           READ CDLATEF KEY IS LAT-CARD-NO
               INVALID KEY
                   ADD 1 TO CNT-SKIP
                   MOVE ZERO TO WK-DELINQ-AMT
               NOT INVALID KEY
                   CONTINUE
           END-READ
           IF FS-CDLATEF NOT = "00"
           AND FS-CDLATEF NOT = "23"
               MOVE "CDLATEF 読込失敗" TO WK-REASON
               PERFORM 9100-ABEND
           END-IF.

       2300-READ-OS.
           IF FS-CDLATEF = "00"
               MOVE RTY-CARD-NO TO OS-CARD-NO
               READ CDOSF KEY IS OS-CARD-NO
                   INVALID KEY
                       ADD 1 TO CNT-SKIP
                       MOVE ZERO TO WK-DELINQ-AMT
                   NOT INVALID KEY
                       CONTINUE
               END-READ
               IF FS-CDOSF NOT = "00"
               AND FS-CDOSF NOT = "23"
                   MOVE "CDOSF 読込失敗" TO WK-REASON
                   PERFORM 9100-ABEND
               END-IF
           END-IF.

       2400-VALIDATE-AND-CALC.
           IF FS-CDLATEF NOT = "00"
           OR FS-CDOSF NOT = "00"
               EXIT PARAGRAPH
           END-IF

           IF LAT-CYCLE-DT NOT = OS-CYCLE-DT
               ADD 1 TO CNT-SKIP
               DISPLAY "CYCLE UNMATCH CARD=" RTY-CARD-NO
               DISPLAY "LAT=" LAT-CYCLE-DT
               DISPLAY "OS=" OS-CYCLE-DT
               EXIT PARAGRAPH
           END-IF

           IF RTY-NEXT-REQUEST-DT > WK-CURRENT-DATE
               ADD 1 TO CNT-SKIP
               EXIT PARAGRAPH
           END-IF

           IF LAT-DELINQ-DAYS <= ZERO
               ADD 1 TO CNT-SKIP
               EXIT PARAGRAPH
           END-IF

           COMPUTE WK-BALANCE-AMT =
                   OS-FEE-BAL-AMT
                 + OS-INTEREST-BAL-AMT
                 + OS-PRINCIPAL-BAL-AMT
                 + LAT-LATE-INTEREST-AMT

           IF WK-BALANCE-AMT <= ZERO
               ADD 1 TO CNT-SKIP
               EXIT PARAGRAPH
           END-IF

           IF RTY-RETRY-AMT > ZERO
               IF RTY-RETRY-AMT < WK-BALANCE-AMT
                   MOVE RTY-RETRY-AMT TO WK-DELINQ-AMT
               ELSE
                   MOVE WK-BALANCE-AMT TO WK-DELINQ-AMT
               END-IF
           ELSE
               MOVE WK-BALANCE-AMT TO WK-DELINQ-AMT
           END-IF.

       2500-CHECK-HISTORY.
           MOVE RTY-CARD-NO TO HIS-CARD-NO
           READ CDHISTF KEY IS HIS-CARD-NO
               INVALID KEY
                   MOVE "N" TO SW-HIST-FOUND
               NOT INVALID KEY
                   SET HIST-FOUND TO TRUE
           END-READ

           IF FS-CDHISTF NOT = "00"
           AND FS-CDHISTF NOT = "23"
               MOVE "CDHISTF 読込失敗" TO WK-REASON
               PERFORM 9100-ABEND
               EXIT PARAGRAPH
           END-IF

           IF HIST-FOUND
           AND HIS-EVENT-TYPE = CN-EVT-DUN
           AND HIS-EVENT-DT = LAT-CYCLE-DT
               SET DUPLICATE-NOTICE TO TRUE
               ADD 1 TO CNT-DUP
           END-IF.

       2600-DECIDE-NOTICE.
           IF LAT-DELINQ-DAYS >= 60
           OR RTY-RETRY-COUNT >= 3
           OR WK-DELINQ-AMT >= 500000
               MOVE CN-RANK-A TO WK-RANK
               MOVE CN-CH-CALL TO WK-CHANNEL
           ELSE
               IF LAT-DELINQ-DAYS >= 30
               OR RTY-RETRY-COUNT >= 2
               OR WK-DELINQ-AMT >= 100000
                   MOVE CN-RANK-B TO WK-RANK
                   MOVE CN-CH-POST TO WK-CHANNEL
               ELSE
                   MOVE CN-RANK-C TO WK-RANK
                   MOVE CN-CH-MAIL TO WK-CHANNEL
               END-IF
           END-IF.

       2700-WRITE-DUNNING.
           ADD 1 TO WK-NOTICE-SEQ
           MOVE SPACES TO WK-NOTICE-ID
           STRING "D" WK-CURRENT-DATE WK-NOTICE-SEQ
               DELIMITED BY SIZE
               INTO WK-NOTICE-ID
           END-STRING

           MOVE WK-NOTICE-ID TO DUN-NOTICE-ID
           MOVE RTY-CARD-NO TO DUN-CARD-NO
           MOVE LAT-CYCLE-DT TO DUN-CYCLE-DT
           MOVE WK-DELINQ-AMT TO DUN-DELINQ-AMT
           MOVE WK-RANK TO DUN-NOTICE-RANK
           MOVE WK-CHANNEL TO DUN-CHANNEL-CD
           MOVE WK-CURRENT-DATE TO DUN-CREATE-DT

           WRITE CDDUNF-REC
           IF FS-CDDUNF NOT = "00"
               MOVE "CDDUNF 書込失敗" TO WK-REASON
               PERFORM 9100-ABEND
           ELSE
               ADD 1 TO CNT-WRITE
           END-IF.

       2800-WRITE-HISTORY.
           MOVE RTY-CARD-NO TO HIS-CARD-NO
           MOVE RTY-ORIGINAL-REQUEST-ID TO HIS-PAY-ID
           MOVE CN-EVT-DUN TO HIS-EVENT-TYPE
           MOVE WK-DELINQ-AMT TO HIS-EVENT-AMT
           MOVE LAT-CYCLE-DT TO HIS-EVENT-DT
           MOVE WK-SOURCE-PGM TO HIS-SOURCE-PROGRAM

           IF HIST-FOUND
               MOVE HIS-EVENT-SEQ TO WK-HIS-EVENT-SEQ
               ADD 1 TO WK-HIS-EVENT-SEQ
               MOVE WK-HIS-EVENT-SEQ TO HIS-EVENT-SEQ
               REWRITE CDHISTF-REC
               IF FS-CDHISTF NOT = "00"
                   MOVE "CDHISTF 更新失敗" TO WK-REASON
                   PERFORM 9100-ABEND
               END-IF
           ELSE
               MOVE 1 TO HIS-EVENT-SEQ
               WRITE CDHISTF-REC
               IF FS-CDHISTF NOT = "00"
                   MOVE "CDHISTF 書込失敗" TO WK-REASON
                   PERFORM 9100-ABEND
               END-IF
           END-IF.

       9000-FINALIZE.
           CLOSE CDOSF
           CLOSE CDLATEF
           CLOSE CDRTRYF
           CLOSE CDDUNF
           CLOSE CDHISTF

           IF HARD-ERROR
               DISPLAY "CB180B 異常終了"
           ELSE
               MOVE 0 TO RETURN-CODE
               DISPLAY "CB180B 正常終了"
           END-IF

           DISPLAY "読込件数=" CNT-READ
           DISPLAY "出力件数=" CNT-WRITE
           DISPLAY "対象外件数=" CNT-SKIP
           DISPLAY "重複件数=" CNT-DUP
           DISPLAY "警告件数=" CNT-ERR.

       9100-ABEND.
           SET HARD-ERROR TO TRUE
           MOVE 12 TO RETURN-CODE
           DISPLAY WK-REASON
           DISPLAY "FS CDOSF=" FS-CDOSF
           DISPLAY "FS CDLATEF=" FS-CDLATEF
           DISPLAY "FS CDRTRYF=" FS-CDRTRYF
           DISPLAY "FS CDDUNF=" FS-CDDUNF
           DISPLAY "FS CDHISTF=" FS-CDHISTF.
