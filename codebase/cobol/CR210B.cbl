       IDENTIFICATION DIVISION.
       PROGRAM-ID. CR210B.
       AUTHOR.     MFG-SHIKIN-BATCH.
      *資金繰り日次集計バッチ
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CONSOLE IS SYSOUT.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CCPOSF ASSIGN TO "CCPOSF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS PS-ORG-CD
               FILE STATUS IS WS-PS-ST.
           SELECT CCDTLF ASSIGN TO "CCDTLF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-DL-ST.
           SELECT CCXFRF ASSIGN TO "CCXFRF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS XF-XFER-ID
               FILE STATUS IS WS-XF-ST.
           SELECT CCCALF ASSIGN TO "CCCALF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-CL-ST.
           SELECT CCRPTF ASSIGN TO "CCRPTF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RP-ST.
           SELECT CCERRF ASSIGN TO "CCERRF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS ER-ERROR-ID
               FILE STATUS IS WS-ER-ST.

       DATA DIVISION.
       FILE SECTION.
       FD  CCPOSF.
           COPY CCPOSC.
       FD  CCDTLF.
           COPY CCDTLC.
       FD  CCXFRF.
           COPY CCXFRC.
       FD  CCCALF.
           COPY CCCALFC.
       FD  CCRPTF.
           COPY CCRPTC.
       FD  CCERRF.
           COPY CCERRC.

       WORKING-STORAGE SECTION.
       01  WS-CONSTANT.
           05 WS-PGM-ID              PIC X(08) VALUE "CR210B".
           05 WS-RPT-KBN-HEAD        PIC X(02) VALUE "HD".
           05 WS-RPT-KBN-DATA        PIC X(02) VALUE "DT".
           05 WS-RPT-KBN-WARN        PIC X(02) VALUE "WN".
           05 WS-RPT-KBN-END         PIC X(02) VALUE "ED".
           05 WS-STATUS-VALID        PIC X(02) VALUE "01".
           05 WS-HOLD                PIC X(02) VALUE "08".
           05 WS-CANCEL              PIC X(02) VALUE "09".
           05 WS-BUSINESS-DAY        PIC X(01) VALUE "N".
           05 WS-MAX-ORG             PIC 9(04) VALUE 500.

       01  WS-FILE-STATUS.
           05 WS-PS-ST               PIC X(02).
           05 WS-DL-ST               PIC X(02).
           05 WS-XF-ST               PIC X(02).
           05 WS-CL-ST               PIC X(02).
           05 WS-RP-ST               PIC X(02).
           05 WS-ER-ST               PIC X(02).

       01  WS-SWITCH.
           05 WS-EOF-PS              PIC X VALUE "N".
           05 WS-EOF-DL              PIC X VALUE "N".
           05 WS-EOF-XF              PIC X VALUE "N".
           05 WS-EOF-CL              PIC X VALUE "N".
           05 WS-CAL-FOUND           PIC X VALUE "N".
           05 WS-HARD-ERROR          PIC X VALUE "N".
           05 WS-WARN-FOUND          PIC X VALUE "N".

       01  WS-WORK.
           05 WS-BASE-DT             PIC 9(08).
           05 WS-BASE-DT-X           PIC X(08).
           05 WS-LINE-NO             PIC 9(07) VALUE 0.
           05 WS-ERR-NO              PIC 9(07) VALUE 0.
           05 WS-ORG-CNT             PIC 9(04) VALUE 0.
           05 WS-IDX                 PIC 9(04) VALUE 0.
           05 WS-SRCH-IDX            PIC 9(04) VALUE 0.
           05 WS-FIND-IDX            PIC 9(04) VALUE 0.
           05 WS-FOUND-SW            PIC X VALUE "N".
           05 WS-NET-AMT             PIC S9(15)V99 COMP-3 VALUE 0.
           05 WS-TOTAL-IN            PIC S9(15)V99 COMP-3 VALUE 0.
           05 WS-TOTAL-OUT           PIC S9(15)V99 COMP-3 VALUE 0.
           05 WS-TOTAL-NET           PIC S9(15)V99 COMP-3 VALUE 0.
           05 WS-TEXT                PIC X(120).
           05 WS-KEY-TEXT            PIC X(40).

       01  WS-EDIT.
           05 WS-AVAILABLE-X         PIC -ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99.
           05 WS-RESERVED-X          PIC -ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99.
           05 WS-DETAIL-IN-X         PIC -ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99.
           05 WS-XFER-IN-X           PIC -ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99.
           05 WS-XFER-OUT-X          PIC -ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99.
           05 WS-NET-AMT-X           PIC -ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99.
           05 WS-TOTAL-IN-X          PIC -ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99.
           05 WS-TOTAL-OUT-X         PIC -ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99.
           05 WS-TOTAL-NET-X         PIC -ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99.

       01  WS-ORG-TABLE.
           05 WS-ORG-ENTRY OCCURS 500 TIMES.
              10 WS-TB-ORG-CD        PIC X(10).
              10 WS-TB-POS-BASE-DT   PIC 9(08).
              10 WS-TB-AVAILABLE     PIC S9(15)V99 COMP-3.
              10 WS-TB-RESERVED      PIC S9(15)V99 COMP-3.
              10 WS-TB-DETAIL-IN     PIC S9(15)V99 COMP-3.
              10 WS-TB-XFER-IN       PIC S9(15)V99 COMP-3.
              10 WS-TB-XFER-OUT      PIC S9(15)V99 COMP-3.
              10 WS-TB-STATUS        PIC X(02).
              10 WS-TB-WARN          PIC X.

       PROCEDURE DIVISION.
       MAIN-SECTION.
           PERFORM 0000-INIT
           IF WS-HARD-ERROR = "N"
              PERFORM 1000-VALIDATE-CALENDAR
           END-IF
           IF WS-HARD-ERROR = "N"
              PERFORM 2000-LOAD-POSITION
           END-IF
           IF WS-HARD-ERROR = "N"
              PERFORM 3000-LOAD-DETAIL
           END-IF
           IF WS-HARD-ERROR = "N"
              PERFORM 4000-LOAD-XFER
           END-IF
           IF WS-HARD-ERROR = "N"
              PERFORM 5000-WRITE-REPORT
           END-IF
           PERFORM 9000-CLOSE
           IF WS-HARD-ERROR = "N"
              MOVE 0 TO RETURN-CODE
           ELSE
              MOVE 12 TO RETURN-CODE
           END-IF
           GOBACK.

       0000-INIT.
           ACCEPT WS-BASE-DT FROM DATE YYYYMMDD
           MOVE WS-BASE-DT TO WS-BASE-DT-X
           MOVE "N" TO WS-HARD-ERROR
           MOVE 0 TO WS-LINE-NO WS-ERR-NO WS-ORG-CNT
           MOVE 0 TO WS-TOTAL-IN WS-TOTAL-OUT WS-TOTAL-NET
           OPEN INPUT CCPOSF CCDTLF CCXFRF CCCALF
           IF WS-PS-ST NOT = "00"
              DISPLAY "CCPOSF オープン失敗 ST=" WS-PS-ST
              MOVE "Y" TO WS-HARD-ERROR
           END-IF
           IF WS-DL-ST NOT = "00"
              DISPLAY "CCDTLF オープン失敗 ST=" WS-DL-ST
              MOVE "Y" TO WS-HARD-ERROR
           END-IF
           IF WS-XF-ST NOT = "00"
              DISPLAY "CCXFRF オープン失敗 ST=" WS-XF-ST
              MOVE "Y" TO WS-HARD-ERROR
           END-IF
           IF WS-CL-ST NOT = "00"
              DISPLAY "CCCALF オープン失敗 ST=" WS-CL-ST
              MOVE "Y" TO WS-HARD-ERROR
           END-IF
           OPEN OUTPUT CCRPTF
           IF WS-RP-ST NOT = "00"
              DISPLAY "CCRPTF オープン失敗 ST=" WS-RP-ST
              MOVE "Y" TO WS-HARD-ERROR
           END-IF
           OPEN OUTPUT CCERRF
           IF WS-ER-ST NOT = "00"
              DISPLAY "CCERRF オープン失敗 ST=" WS-ER-ST
              MOVE "Y" TO WS-HARD-ERROR
           END-IF.

       1000-VALIDATE-CALENDAR.
           PERFORM UNTIL WS-EOF-CL = "Y" OR WS-CAL-FOUND = "Y"
              READ CCCALF
                 AT END
                    MOVE "Y" TO WS-EOF-CL
                 NOT AT END
                    IF CL-CAL-DT = WS-BASE-DT
                       MOVE "Y" TO WS-CAL-FOUND
                       IF CL-HOLIDAY-FLAG NOT = WS-BUSINESS-DAY
                          PERFORM 8100-WRITE-CALENDAR-ERROR
                          MOVE "Y" TO WS-HARD-ERROR
                       END-IF
                    END-IF
              END-READ
              IF WS-CL-ST NOT = "00" AND WS-CL-ST NOT = "10"
                 DISPLAY "CCCALF 読込失敗 ST=" WS-CL-ST
                 MOVE "Y" TO WS-HARD-ERROR
                 MOVE "Y" TO WS-EOF-CL
              END-IF
           END-PERFORM
           IF WS-CAL-FOUND NOT = "Y" AND WS-HARD-ERROR = "N"
              PERFORM 8100-WRITE-CALENDAR-ERROR
              MOVE "Y" TO WS-HARD-ERROR
           END-IF.

       2000-LOAD-POSITION.
           PERFORM UNTIL WS-EOF-PS = "Y"
              READ CCPOSF NEXT RECORD
                 AT END
                    MOVE "Y" TO WS-EOF-PS
                 NOT AT END
                    IF PS-BASE-DT = WS-BASE-DT
                       IF PS-POSITION-STATUS-KBN = WS-STATUS-VALID
                          PERFORM 2100-ADD-POSITION
                       ELSE
                          MOVE PS-ORG-CD TO WS-KEY-TEXT
                          MOVE "ポジション状態不正" TO WS-TEXT
                          PERFORM 8000-WRITE-ERROR
                       END-IF
                    END-IF
              END-READ
              IF WS-PS-ST NOT = "00" AND WS-PS-ST NOT = "10"
                 DISPLAY "CCPOSF 読込失敗 ST=" WS-PS-ST
                 MOVE "Y" TO WS-HARD-ERROR
                 MOVE "Y" TO WS-EOF-PS
              END-IF
           END-PERFORM.

       2100-ADD-POSITION.
           PERFORM 7000-FIND-ORG
           IF WS-FOUND-SW = "N"
              PERFORM 7100-APPEND-ORG
           END-IF
           IF WS-FIND-IDX > 0
              MOVE PS-BASE-DT TO WS-TB-POS-BASE-DT(WS-FIND-IDX)
              ADD PS-AVAILABLE-AMT TO WS-TB-AVAILABLE(WS-FIND-IDX)
              ADD PS-RESERVED-AMT  TO WS-TB-RESERVED(WS-FIND-IDX)
              MOVE PS-POSITION-STATUS-KBN TO WS-TB-STATUS(WS-FIND-IDX)
           END-IF.

       3000-LOAD-DETAIL.
           PERFORM UNTIL WS-EOF-DL = "Y"
              READ CCDTLF
                 AT END
                    MOVE "Y" TO WS-EOF-DL
                 NOT AT END
                    IF DL-VALUE-DT = WS-BASE-DT
                       EVALUATE DL-DETAIL-STATUS-KBN
                          WHEN "01"
                             MOVE DL-ORG-CD TO PS-ORG-CD
                             PERFORM 7000-FIND-ORG
                             IF WS-FOUND-SW = "N"
                                PERFORM 7100-APPEND-ORG
                             END-IF
                             IF WS-FIND-IDX > 0
                                ADD DL-DETAIL-AMT
                                  TO WS-TB-DETAIL-IN(WS-FIND-IDX)
                             END-IF
                          WHEN "08"
                             CONTINUE
                          WHEN "09"
                             CONTINUE
                          WHEN OTHER
                             MOVE DL-VAL-ID TO WS-KEY-TEXT
                             MOVE "受渡明細状態不正" TO WS-TEXT
                             PERFORM 8000-WRITE-ERROR
                       END-EVALUATE
                    END-IF
              END-READ
              IF WS-DL-ST NOT = "00" AND WS-DL-ST NOT = "10"
                 DISPLAY "CCDTLF 読込失敗 ST=" WS-DL-ST
                 MOVE "Y" TO WS-HARD-ERROR
                 MOVE "Y" TO WS-EOF-DL
              END-IF
           END-PERFORM.

       4000-LOAD-XFER.
           PERFORM UNTIL WS-EOF-XF = "Y"
              READ CCXFRF NEXT RECORD
                 AT END
                    MOVE "Y" TO WS-EOF-XF
                 NOT AT END
                    IF XF-VALUE-DT = WS-BASE-DT
                       EVALUATE XF-XFER-STATUS-KBN
                          WHEN "01"
                             PERFORM 4100-APPLY-XFER
                          WHEN "08"
                             CONTINUE
                          WHEN "09"
                             CONTINUE
                          WHEN OTHER
                             MOVE XF-XFER-ID TO WS-KEY-TEXT
                             MOVE "振替予定状態不正" TO WS-TEXT
                             PERFORM 8000-WRITE-ERROR
                       END-EVALUATE
                    END-IF
              END-READ
              IF WS-XF-ST NOT = "00" AND WS-XF-ST NOT = "10"
                 DISPLAY "CCXFRF 読込失敗 ST=" WS-XF-ST
                 MOVE "Y" TO WS-HARD-ERROR
                 MOVE "Y" TO WS-EOF-XF
              END-IF
           END-PERFORM.

       4100-APPLY-XFER.
           MOVE XF-FROM-ORG-CD TO PS-ORG-CD
           PERFORM 7000-FIND-ORG
           IF WS-FOUND-SW = "N"
              PERFORM 7100-APPEND-ORG
           END-IF
           IF WS-FIND-IDX > 0
              ADD XF-XFER-AMT TO WS-TB-XFER-OUT(WS-FIND-IDX)
           END-IF
           MOVE XF-TO-ORG-CD TO PS-ORG-CD
           PERFORM 7000-FIND-ORG
           IF WS-FOUND-SW = "N"
              PERFORM 7100-APPEND-ORG
           END-IF
           IF WS-FIND-IDX > 0
              ADD XF-XFER-AMT TO WS-TB-XFER-IN(WS-FIND-IDX)
           END-IF.

       5000-WRITE-REPORT.
           MOVE "資金繰り日次表 基準日=" TO WS-TEXT
           PERFORM 8200-WRITE-HEAD
           PERFORM VARYING WS-IDX FROM 1 BY 1
                   UNTIL WS-IDX > WS-ORG-CNT
              COMPUTE WS-NET-AMT =
                 WS-TB-AVAILABLE(WS-IDX)
               - WS-TB-RESERVED(WS-IDX)
               + WS-TB-DETAIL-IN(WS-IDX)
               + WS-TB-XFER-IN(WS-IDX)
               - WS-TB-XFER-OUT(WS-IDX)
              ADD WS-TB-DETAIL-IN(WS-IDX) TO WS-TOTAL-IN
              ADD WS-TB-XFER-IN(WS-IDX)   TO WS-TOTAL-IN
              ADD WS-TB-XFER-OUT(WS-IDX)  TO WS-TOTAL-OUT
              ADD WS-NET-AMT              TO WS-TOTAL-NET
              IF WS-NET-AMT < 0
                 MOVE "Y" TO WS-TB-WARN(WS-IDX)
                 MOVE "Y" TO WS-WARN-FOUND
              ELSE
                 MOVE "N" TO WS-TB-WARN(WS-IDX)
              END-IF
              PERFORM 5100-WRITE-DETAIL
              IF WS-TB-WARN(WS-IDX) = "Y"
                 PERFORM 5200-WRITE-WARNING
              END-IF
           END-PERFORM
           PERFORM 5300-WRITE-END.

       5100-WRITE-DETAIL.
           INITIALIZE CCRPTF-REC
           PERFORM 8300-FORMAT-AMOUNTS
           ADD 1 TO WS-LINE-NO
           STRING "ORG=" WS-TB-ORG-CD(WS-IDX)
                  " 有効=" WS-AVAILABLE-X
                  " 拘束=" WS-RESERVED-X
                  " 入金=" WS-DETAIL-IN-X
                  " 振入=" WS-XFER-IN-X
                  " 振出=" WS-XFER-OUT-X
                  " 残高=" WS-NET-AMT-X
             DELIMITED BY SIZE INTO RP-REPORT-TEXT
           MOVE WS-LINE-NO      TO RP-LINE-NO
           MOVE WS-BASE-DT      TO RP-BASE-DT
           MOVE WS-RPT-KBN-DATA TO RP-REPORT-KBN
           MOVE WS-LINE-NO      TO RP-REPORT-ID
           WRITE CCRPTF-REC
           IF WS-RP-ST NOT = "00"
              DISPLAY "CCRPTF 書込失敗 ST=" WS-RP-ST
              MOVE "Y" TO WS-HARD-ERROR
           END-IF.

       5200-WRITE-WARNING.
           INITIALIZE CCRPTF-REC
           MOVE WS-NET-AMT TO WS-NET-AMT-X
           ADD 1 TO WS-LINE-NO
           STRING "流動性警告 ORG=" WS-TB-ORG-CD(WS-IDX)
                  " 資金不足見込額=" WS-NET-AMT-X
             DELIMITED BY SIZE INTO RP-REPORT-TEXT
           MOVE WS-LINE-NO      TO RP-LINE-NO
           MOVE WS-BASE-DT      TO RP-BASE-DT
           MOVE WS-RPT-KBN-WARN TO RP-REPORT-KBN
           MOVE WS-LINE-NO      TO RP-REPORT-ID
           WRITE CCRPTF-REC
           IF WS-RP-ST NOT = "00"
              DISPLAY "CCRPTF 警告書込失敗 ST=" WS-RP-ST
              MOVE "Y" TO WS-HARD-ERROR
           END-IF.

       5300-WRITE-END.
           INITIALIZE CCRPTF-REC
           MOVE WS-TOTAL-IN  TO WS-TOTAL-IN-X
           MOVE WS-TOTAL-OUT TO WS-TOTAL-OUT-X
           MOVE WS-TOTAL-NET TO WS-TOTAL-NET-X
           ADD 1 TO WS-LINE-NO
           STRING "合計 入金=" WS-TOTAL-IN-X
                  " 出金=" WS-TOTAL-OUT-X
                  " 差引=" WS-TOTAL-NET-X
             DELIMITED BY SIZE INTO RP-REPORT-TEXT
           MOVE WS-LINE-NO     TO RP-LINE-NO
           MOVE WS-BASE-DT     TO RP-BASE-DT
           MOVE WS-RPT-KBN-END TO RP-REPORT-KBN
           MOVE WS-LINE-NO     TO RP-REPORT-ID
           WRITE CCRPTF-REC
           IF WS-RP-ST NOT = "00"
              DISPLAY "CCRPTF 終了行書込失敗 ST=" WS-RP-ST
              MOVE "Y" TO WS-HARD-ERROR
           END-IF.

       7000-FIND-ORG.
           MOVE "N" TO WS-FOUND-SW
           MOVE 0 TO WS-FIND-IDX
           PERFORM VARYING WS-SRCH-IDX FROM 1 BY 1
                   UNTIL WS-SRCH-IDX > WS-ORG-CNT
                      OR WS-FOUND-SW = "Y"
              IF WS-TB-ORG-CD(WS-SRCH-IDX) = PS-ORG-CD
                 MOVE "Y" TO WS-FOUND-SW
                 MOVE WS-SRCH-IDX TO WS-FIND-IDX
              END-IF
           END-PERFORM.

       7100-APPEND-ORG.
           IF WS-ORG-CNT < WS-MAX-ORG
              ADD 1 TO WS-ORG-CNT
              MOVE WS-ORG-CNT TO WS-FIND-IDX
              MOVE PS-ORG-CD TO WS-TB-ORG-CD(WS-FIND-IDX)
              MOVE WS-BASE-DT TO WS-TB-POS-BASE-DT(WS-FIND-IDX)
              MOVE 0 TO WS-TB-AVAILABLE(WS-FIND-IDX)
                        WS-TB-RESERVED(WS-FIND-IDX)
                        WS-TB-DETAIL-IN(WS-FIND-IDX)
                        WS-TB-XFER-IN(WS-FIND-IDX)
                        WS-TB-XFER-OUT(WS-FIND-IDX)
              MOVE "00" TO WS-TB-STATUS(WS-FIND-IDX)
              MOVE "N" TO WS-TB-WARN(WS-FIND-IDX)
           ELSE
              MOVE PS-ORG-CD TO WS-KEY-TEXT
              MOVE "組織集計表上限超過" TO WS-TEXT
              PERFORM 8000-WRITE-ERROR
              DISPLAY "組織集計表上限超過"
              MOVE "Y" TO WS-HARD-ERROR
           END-IF.

       8000-WRITE-ERROR.
           INITIALIZE CCERRF-REC
           ADD 1 TO WS-ERR-NO
           MOVE WS-ERR-NO   TO ER-ERROR-ID
           MOVE WS-PGM-ID   TO ER-PGM-ID
           MOVE WS-BASE-DT  TO ER-BASE-DT
           MOVE WS-KEY-TEXT TO ER-RECORD-KEY
           MOVE "E1"        TO ER-ERROR-KBN
           MOVE WS-TEXT     TO ER-ERROR-TEXT
           WRITE CCERRF-REC
           IF WS-ER-ST NOT = "00"
              DISPLAY "CCERRF 書込失敗 ST=" WS-ER-ST
              MOVE "Y" TO WS-HARD-ERROR
           END-IF.

       8100-WRITE-CALENDAR-ERROR.
           MOVE WS-BASE-DT TO WS-KEY-TEXT
           IF WS-CAL-FOUND = "Y"
              MOVE "基準日が営業日でない" TO WS-TEXT
           ELSE
              MOVE "基準日カレンダー未登録" TO WS-TEXT
           END-IF
           PERFORM 8000-WRITE-ERROR
           DISPLAY WS-TEXT.

       8200-WRITE-HEAD.
           INITIALIZE CCRPTF-REC
           ADD 1 TO WS-LINE-NO
           STRING WS-TEXT WS-BASE-DT-X
             DELIMITED BY SIZE INTO RP-REPORT-TEXT
           MOVE WS-LINE-NO      TO RP-LINE-NO
           MOVE WS-BASE-DT      TO RP-BASE-DT
           MOVE WS-RPT-KBN-HEAD TO RP-REPORT-KBN
           MOVE WS-LINE-NO      TO RP-REPORT-ID
           WRITE CCRPTF-REC
           IF WS-RP-ST NOT = "00"
              DISPLAY "CCRPTF 見出書込失敗 ST=" WS-RP-ST
              MOVE "Y" TO WS-HARD-ERROR
           END-IF.

       8300-FORMAT-AMOUNTS.
           MOVE WS-TB-AVAILABLE(WS-IDX) TO WS-AVAILABLE-X
           MOVE WS-TB-RESERVED(WS-IDX)  TO WS-RESERVED-X
           MOVE WS-TB-DETAIL-IN(WS-IDX) TO WS-DETAIL-IN-X
           MOVE WS-TB-XFER-IN(WS-IDX)   TO WS-XFER-IN-X
           MOVE WS-TB-XFER-OUT(WS-IDX)  TO WS-XFER-OUT-X
           MOVE WS-NET-AMT              TO WS-NET-AMT-X.

       9000-CLOSE.
           CLOSE CCPOSF CCDTLF CCXFRF CCCALF CCRPTF CCERRF
           IF WS-PS-ST NOT = "00"
              DISPLAY "CCPOSF クローズ状態 ST=" WS-PS-ST
           END-IF
           IF WS-DL-ST NOT = "00"
              DISPLAY "CCDTLF クローズ状態 ST=" WS-DL-ST
           END-IF
           IF WS-XF-ST NOT = "00"
              DISPLAY "CCXFRF クローズ状態 ST=" WS-XF-ST
           END-IF
           IF WS-CL-ST NOT = "00"
              DISPLAY "CCCALF クローズ状態 ST=" WS-CL-ST
           END-IF
           IF WS-RP-ST NOT = "00"
              DISPLAY "CCRPTF クローズ状態 ST=" WS-RP-ST
           END-IF
           IF WS-ER-ST NOT = "00"
              DISPLAY "CCERRF クローズ状態 ST=" WS-ER-ST
           END-IF.
