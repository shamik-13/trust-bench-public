       IDENTIFICATION DIVISION.
       PROGRAM-ID. CR230B.
      *===============================================================*
      * 変更履歴                                                      *
      * 版数  年月日    担当    概要                                 *
      * 1.00  20240401  T001    初版作成                             *
      * 1.10  20240520  T002    未確定・ポジション検証追加           *
      * 1.20  20240615  T003    金額不一致エラー出力追加             *
      *===============================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CCDTLF ASSIGN TO "CCDTLF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-CCDTLF.
           SELECT CCFCTF ASSIGN TO "CCFCTF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-CCFCTF.
           SELECT CCVALF ASSIGN TO "CCVALF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-CCVALF.
           SELECT CCPOSF ASSIGN TO "CCPOSF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS PS-ORG-CD
               FILE STATUS IS FS-CCPOSF.
           SELECT CCXFRF ASSIGN TO "CCXFRF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-CCXFRF.
           SELECT CCERRF ASSIGN TO "CCERRF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-CCERRF.

       DATA DIVISION.
       FILE SECTION.
       FD  CCDTLF.
           COPY CCDTLC.
       FD  CCFCTF.
           COPY CCFCTFC.
       FD  CCVALF.
           COPY CCVALFC.
       FD  CCPOSF.
           COPY CCPOSC.
       FD  CCXFRF.
           COPY CCXFRC.
       FD  CCERRF.
           COPY CCERRC.

       WORKING-STORAGE SECTION.
       01  FS-AREA.
           05 FS-CCDTLF             PIC XX.
           05 FS-CCFCTF             PIC XX.
           05 FS-CCVALF             PIC XX.
           05 FS-CCPOSF             PIC XX.
           05 FS-CCXFRF             PIC XX.
           05 FS-CCERRF             PIC XX.

       01  SW-AREA.
           05 EOF-CCDTLF            PIC X VALUE "N".
           05 EOF-CCFCTF            PIC X VALUE "N".
           05 EOF-CCVALF            PIC X VALUE "N".
           05 HARD-ERROR-SW         PIC X VALUE "N".
           05 FCT-FOUND-SW          PIC X VALUE "N".
           05 VAL-FOUND-SW          PIC X VALUE "N".
           05 POS-FOUND-SW          PIC X VALUE "N".
           05 RECORD-ERROR-SW       PIC X VALUE "N".

       01  CONST-AREA.
           05 C-PGM-ID              PIC X(08) VALUE "CR230B".
           05 C-CENT-ORG-CD         PIC X(10) VALUE "CNTR000001".
           05 C-STAT-DECIDE         PIC X(02) VALUE "01".
           05 C-STAT-HOLD           PIC X(02) VALUE "08".
           05 C-STAT-CANCEL         PIC X(02) VALUE "09".
           05 C-XFER-NEW            PIC X(02) VALUE "01".
           05 C-ERR-MIKAKUTEI       PIC X(02) VALUE "11".
           05 C-ERR-POSNASHI        PIC X(02) VALUE "21".
           05 C-ERR-KINGAKU         PIC X(02) VALUE "31".
           05 C-ERR-JOTAI           PIC X(02) VALUE "41".
           05 C-ERR-IO              PIC X(02) VALUE "90".

       01  COUNTER-AREA.
           05 FCT-CNT               PIC 9(05) VALUE ZERO.
           05 VAL-CNT               PIC 9(05) VALUE ZERO.
           05 IN-CNT                PIC 9(09) VALUE ZERO.
           05 OUT-CNT               PIC 9(09) VALUE ZERO.
           05 ERR-CNT               PIC 9(09) VALUE ZERO.
           05 XFER-SEQ              PIC 9(09) VALUE ZERO.
           05 ERR-SEQ               PIC 9(09) VALUE ZERO.
           05 TBL-IDX               PIC 9(05) VALUE ZERO.
           05 SEARCH-IDX            PIC 9(05) VALUE ZERO.

       01  WORK-AREA.
           05 WK-BASE-DT            PIC 9(08) VALUE ZERO.
           05 WK-XFER-AMT           PIC S9(13)V99 VALUE ZERO.
           05 WK-FCT-AMT            PIC S9(13)V99 VALUE ZERO.
           05 WK-ABS-AMT            PIC S9(13)V99 VALUE ZERO.
           05 WK-DETAIL-SUM         PIC S9(13)V99 VALUE ZERO.
           05 WK-KEY                PIC X(30) VALUE SPACES.
           05 WK-TEXT               PIC X(60) VALUE SPACES.
           05 WK-XFER-ID            PIC X(16) VALUE SPACES.
           05 WK-ERROR-ID           PIC X(16) VALUE SPACES.

       01  FCT-TABLE.
           05 FCT-ENTRY OCCURS 3000 TIMES.
              10 T-FCT-ID           PIC X(20).
              10 T-FCT-DT           PIC 9(08).
              10 T-FCT-AMT          PIC S9(13)V99.
              10 T-FCT-STATUS       PIC X(02).
              10 T-FCT-SUM          PIC S9(13)V99.

       01  VAL-TABLE.
           05 VAL-ENTRY OCCURS 3000 TIMES.
              10 T-VAL-ID           PIC X(20).
              10 T-VAL-FCT-ID       PIC X(20).
              10 T-VAL-DT           PIC 9(08).
              10 T-VAL-STATUS       PIC X(02).

       PROCEDURE DIVISION.
       MAIN-RTN.
           MOVE 0 TO RETURN-CODE
           PERFORM OPEN-RTN
           IF HARD-ERROR-SW = "N"
              PERFORM LOAD-FCT-RTN
              PERFORM LOAD-VAL-RTN
              PERFORM PROCESS-DTL-RTN
           END-IF
           PERFORM CLOSE-RTN
           DISPLAY "CR230B 入力件数=" IN-CNT
                   " 出力件数=" OUT-CNT
                   " エラー件数=" ERR-CNT
           IF HARD-ERROR-SW = "Y"
              MOVE 8 TO RETURN-CODE
           ELSE
              MOVE 0 TO RETURN-CODE
           END-IF
           GOBACK.

       OPEN-RTN.
           OPEN INPUT CCDTLF
           IF FS-CCDTLF NOT = "00"
              DISPLAY "CCDTLF オープン失敗 ST=" FS-CCDTLF
              MOVE "Y" TO HARD-ERROR-SW
           END-IF
           OPEN INPUT CCFCTF
           IF FS-CCFCTF NOT = "00"
              DISPLAY "CCFCTF オープン失敗 ST=" FS-CCFCTF
              MOVE "Y" TO HARD-ERROR-SW
           END-IF
           OPEN INPUT CCVALF
           IF FS-CCVALF NOT = "00"
              DISPLAY "CCVALF オープン失敗 ST=" FS-CCVALF
              MOVE "Y" TO HARD-ERROR-SW
           END-IF
           OPEN INPUT CCPOSF
           IF FS-CCPOSF NOT = "00"
              DISPLAY "CCPOSF オープン失敗 ST=" FS-CCPOSF
              MOVE "Y" TO HARD-ERROR-SW
           END-IF
           OPEN OUTPUT CCXFRF
           IF FS-CCXFRF NOT = "00"
              DISPLAY "CCXFRF オープン失敗 ST=" FS-CCXFRF
              MOVE "Y" TO HARD-ERROR-SW
           END-IF
           OPEN OUTPUT CCERRF
           IF FS-CCERRF NOT = "00"
              DISPLAY "CCERRF オープン失敗 ST=" FS-CCERRF
              MOVE "Y" TO HARD-ERROR-SW
           END-IF.

       LOAD-FCT-RTN.
           PERFORM UNTIL EOF-CCFCTF = "Y"
              READ CCFCTF
                 AT END
                    MOVE "Y" TO EOF-CCFCTF
                 NOT AT END
                    IF FS-CCFCTF NOT = "00"
                       DISPLAY "CCFCTF 読込失敗 ST=" FS-CCFCTF
                       MOVE "Y" TO HARD-ERROR-SW
                       MOVE "Y" TO EOF-CCFCTF
                    ELSE
                       ADD 1 TO FCT-CNT
                       IF FCT-CNT > 3000
                          DISPLAY "CCFCTF 件数上限超過"
                          MOVE "Y" TO HARD-ERROR-SW
                          MOVE "Y" TO EOF-CCFCTF
                       ELSE
                          MOVE FC-FCT-ID TO T-FCT-ID(FCT-CNT)
                          MOVE FC-TRIGGER-DT TO T-FCT-DT(FCT-CNT)
                          MOVE FC-CONC-AMT TO T-FCT-AMT(FCT-CNT)
                          MOVE FC-FCT-STATUS-KBN
                            TO T-FCT-STATUS(FCT-CNT)
                          MOVE ZERO TO T-FCT-SUM(FCT-CNT)
                       END-IF
                    END-IF
              END-READ
           END-PERFORM.

       LOAD-VAL-RTN.
           PERFORM UNTIL EOF-CCVALF = "Y"
              READ CCVALF
                 AT END
                    MOVE "Y" TO EOF-CCVALF
                 NOT AT END
                    IF FS-CCVALF NOT = "00"
                       DISPLAY "CCVALF 読込失敗 ST=" FS-CCVALF
                       MOVE "Y" TO HARD-ERROR-SW
                       MOVE "Y" TO EOF-CCVALF
                    ELSE
                       ADD 1 TO VAL-CNT
                       IF VAL-CNT > 3000
                          DISPLAY "CCVALF 件数上限超過"
                          MOVE "Y" TO HARD-ERROR-SW
                          MOVE "Y" TO EOF-CCVALF
                       ELSE
                          MOVE VL-VAL-ID TO T-VAL-ID(VAL-CNT)
                          MOVE VL-FCT-ID TO T-VAL-FCT-ID(VAL-CNT)
                          MOVE VL-VALUE-DT TO T-VAL-DT(VAL-CNT)
                          MOVE VL-VAL-STATUS-KBN
                            TO T-VAL-STATUS(VAL-CNT)
                       END-IF
                    END-IF
              END-READ
           END-PERFORM.

       PROCESS-DTL-RTN.
           PERFORM UNTIL EOF-CCDTLF = "Y" OR HARD-ERROR-SW = "Y"
              READ CCDTLF
                 AT END
                    MOVE "Y" TO EOF-CCDTLF
                 NOT AT END
                    IF FS-CCDTLF NOT = "00"
                       DISPLAY "CCDTLF 読込失敗 ST=" FS-CCDTLF
                       MOVE "Y" TO HARD-ERROR-SW
                    ELSE
                       ADD 1 TO IN-CNT
                       PERFORM PROCESS-ONE-DTL
                    END-IF
              END-READ
           END-PERFORM.

       PROCESS-ONE-DTL.
           MOVE "N" TO RECORD-ERROR-SW
           MOVE SPACES TO WK-TEXT
           PERFORM FIND-FCT
           PERFORM FIND-VAL
           IF FCT-FOUND-SW = "N" OR VAL-FOUND-SW = "N"
              MOVE "受渡確定情報未登録" TO WK-TEXT
              MOVE DL-VAL-ID TO WK-KEY
              PERFORM WRITE-ERR-MIKAKUTEI
              MOVE "Y" TO RECORD-ERROR-SW
           END-IF
           IF RECORD-ERROR-SW = "N"
              IF T-FCT-STATUS(SEARCH-IDX) NOT = C-STAT-DECIDE
                 MOVE "資金集中指図未確定" TO WK-TEXT
                 MOVE DL-FCT-ID TO WK-KEY
                 PERFORM WRITE-ERR-MIKAKUTEI
                 MOVE "Y" TO RECORD-ERROR-SW
              END-IF
           END-IF
           IF RECORD-ERROR-SW = "N"
              IF T-VAL-STATUS(TBL-IDX) NOT = C-STAT-DECIDE
                 MOVE "受渡日未確定" TO WK-TEXT
                 MOVE DL-VAL-ID TO WK-KEY
                 PERFORM WRITE-ERR-MIKAKUTEI
                 MOVE "Y" TO RECORD-ERROR-SW
              END-IF
           END-IF
           IF RECORD-ERROR-SW = "N"
              IF DL-DETAIL-STATUS-KBN NOT = C-STAT-DECIDE
                 MOVE "受渡明細状態不正" TO WK-TEXT
                 MOVE DL-VAL-ID TO WK-KEY
                 PERFORM WRITE-ERR-JOTAI
                 MOVE "Y" TO RECORD-ERROR-SW
              END-IF
           END-IF
           IF RECORD-ERROR-SW = "N"
              IF DL-VALUE-DT NOT = T-VAL-DT(TBL-IDX)
                 MOVE "受渡明細日付不一致" TO WK-TEXT
                 MOVE DL-VAL-ID TO WK-KEY
                 PERFORM WRITE-ERR-MIKAKUTEI
                 MOVE "Y" TO RECORD-ERROR-SW
              END-IF
           END-IF
           IF RECORD-ERROR-SW = "N"
              ADD DL-DETAIL-AMT TO T-FCT-SUM(SEARCH-IDX)
              IF FUNCTION ABS(T-FCT-SUM(SEARCH-IDX))
                 > FUNCTION ABS(T-FCT-AMT(SEARCH-IDX))
                 MOVE "資金集中金額超過" TO WK-TEXT
                 MOVE DL-FCT-ID TO WK-KEY
                 PERFORM WRITE-ERR-KINGAKU
                 MOVE "Y" TO RECORD-ERROR-SW
              END-IF
           END-IF
           IF RECORD-ERROR-SW = "N"
              PERFORM CHECK-POSITION
           END-IF
           IF RECORD-ERROR-SW = "N"
              PERFORM WRITE-XFER
           END-IF.

       FIND-FCT.
           MOVE "N" TO FCT-FOUND-SW
           MOVE 1 TO SEARCH-IDX
           PERFORM UNTIL SEARCH-IDX > FCT-CNT
              OR FCT-FOUND-SW = "Y"
              IF T-FCT-ID(SEARCH-IDX) = DL-FCT-ID
                 MOVE "Y" TO FCT-FOUND-SW
              ELSE
                 ADD 1 TO SEARCH-IDX
              END-IF
           END-PERFORM.

       FIND-VAL.
           MOVE "N" TO VAL-FOUND-SW
           MOVE 1 TO TBL-IDX
           PERFORM UNTIL TBL-IDX > VAL-CNT
              OR VAL-FOUND-SW = "Y"
              IF T-VAL-ID(TBL-IDX) = DL-VAL-ID
                 AND T-VAL-FCT-ID(TBL-IDX) = DL-FCT-ID
                 MOVE "Y" TO VAL-FOUND-SW
              ELSE
                 ADD 1 TO TBL-IDX
              END-IF
           END-PERFORM.

       CHECK-POSITION.
           MOVE DL-ORG-CD TO PS-ORG-CD
           READ CCPOSF KEY IS PS-ORG-CD
              INVALID KEY
                 MOVE "N" TO POS-FOUND-SW
              NOT INVALID KEY
                 MOVE "Y" TO POS-FOUND-SW
           END-READ
           IF FS-CCPOSF NOT = "00" AND FS-CCPOSF NOT = "23"
              DISPLAY "CCPOSF 読込失敗 ST=" FS-CCPOSF
              MOVE "Y" TO HARD-ERROR-SW
           ELSE
              IF POS-FOUND-SW = "N"
                 MOVE "ポジション未登録" TO WK-TEXT
                 MOVE DL-ORG-CD TO WK-KEY
                 PERFORM WRITE-ERR-POSNASHI
                 MOVE "Y" TO RECORD-ERROR-SW
              ELSE
                 IF PS-POSITION-STATUS-KBN NOT = C-STAT-DECIDE
                    MOVE "ポジション状態不正" TO WK-TEXT
                    MOVE DL-ORG-CD TO WK-KEY
                    PERFORM WRITE-ERR-JOTAI
                    MOVE "Y" TO RECORD-ERROR-SW
                 END-IF
              END-IF
           END-IF
           IF RECORD-ERROR-SW = "N"
              IF DL-DETAIL-AMT < ZERO
                 COMPUTE WK-ABS-AMT = DL-DETAIL-AMT * -1
                 COMPUTE WK-XFER-AMT = WK-ABS-AMT
                 IF PS-AVAILABLE-AMT - PS-RESERVED-AMT < WK-ABS-AMT
                    MOVE "利用可能ポジション不足" TO WK-TEXT
                    MOVE DL-ORG-CD TO WK-KEY
                    PERFORM WRITE-ERR-POSNASHI
                    MOVE "Y" TO RECORD-ERROR-SW
                 END-IF
              ELSE
                 MOVE DL-DETAIL-AMT TO WK-XFER-AMT
              END-IF
           END-IF.

       WRITE-XFER.
           ADD 1 TO XFER-SEQ
           ADD 1 TO OUT-CNT
           MOVE SPACES TO CCXFRF-REC
           MOVE XFER-SEQ TO WK-XFER-ID(8:9)
           MOVE "XF" TO WK-XFER-ID(1:2)
           MOVE DL-VALUE-DT TO WK-XFER-ID(3:8)
           MOVE WK-XFER-ID TO XF-XFER-ID
           MOVE DL-FCT-ID TO XF-FCT-ID
           IF DL-DETAIL-AMT < ZERO
              MOVE DL-ORG-CD TO XF-FROM-ORG-CD
              MOVE C-CENT-ORG-CD TO XF-TO-ORG-CD
           ELSE
              MOVE C-CENT-ORG-CD TO XF-FROM-ORG-CD
              MOVE DL-ORG-CD TO XF-TO-ORG-CD
           END-IF
           MOVE T-VAL-DT(TBL-IDX) TO XF-VALUE-DT
           MOVE WK-XFER-AMT TO XF-XFER-AMT
           MOVE C-XFER-NEW TO XF-XFER-STATUS-KBN
           WRITE CCXFRF-REC
           IF FS-CCXFRF NOT = "00"
              DISPLAY "CCXFRF 書込失敗 ST=" FS-CCXFRF
              MOVE "Y" TO HARD-ERROR-SW
           END-IF.

       WRITE-ERR-MIKAKUTEI.
           MOVE C-ERR-MIKAKUTEI TO ER-ERROR-KBN
           PERFORM WRITE-ERR-COMMON.

       WRITE-ERR-POSNASHI.
           MOVE C-ERR-POSNASHI TO ER-ERROR-KBN
           PERFORM WRITE-ERR-COMMON.

       WRITE-ERR-KINGAKU.
           MOVE C-ERR-KINGAKU TO ER-ERROR-KBN
           PERFORM WRITE-ERR-COMMON.

       WRITE-ERR-JOTAI.
           MOVE C-ERR-JOTAI TO ER-ERROR-KBN
           PERFORM WRITE-ERR-COMMON.

       WRITE-ERR-COMMON.
           ADD 1 TO ERR-SEQ
           ADD 1 TO ERR-CNT
           MOVE SPACES TO CCERRF-REC
           MOVE ERR-SEQ TO WK-ERROR-ID(8:9)
           MOVE "ER" TO WK-ERROR-ID(1:2)
           MOVE DL-VALUE-DT TO WK-ERROR-ID(3:8)
           MOVE WK-ERROR-ID TO ER-ERROR-ID
           MOVE C-PGM-ID TO ER-PGM-ID
           MOVE DL-VALUE-DT TO ER-BASE-DT
           MOVE WK-KEY TO ER-RECORD-KEY
           MOVE WK-TEXT TO ER-ERROR-TEXT
           WRITE CCERRF-REC
           IF FS-CCERRF NOT = "00"
              DISPLAY "CCERRF 書込失敗 ST=" FS-CCERRF
              MOVE "Y" TO HARD-ERROR-SW
           END-IF.

       CLOSE-RTN.
           CLOSE CCDTLF
           IF FS-CCDTLF NOT = "00"
              DISPLAY "CCDTLF クローズ失敗 ST=" FS-CCDTLF
              MOVE "Y" TO HARD-ERROR-SW
           END-IF
           CLOSE CCFCTF
           IF FS-CCFCTF NOT = "00"
              DISPLAY "CCFCTF クローズ失敗 ST=" FS-CCFCTF
              MOVE "Y" TO HARD-ERROR-SW
           END-IF
           CLOSE CCVALF
           IF FS-CCVALF NOT = "00"
              DISPLAY "CCVALF クローズ失敗 ST=" FS-CCVALF
              MOVE "Y" TO HARD-ERROR-SW
           END-IF
           CLOSE CCPOSF
           IF FS-CCPOSF NOT = "00"
              DISPLAY "CCPOSF クローズ失敗 ST=" FS-CCPOSF
              MOVE "Y" TO HARD-ERROR-SW
           END-IF
           CLOSE CCXFRF
           IF FS-CCXFRF NOT = "00"
              DISPLAY "CCXFRF クローズ失敗 ST=" FS-CCXFRF
              MOVE "Y" TO HARD-ERROR-SW
           END-IF
           CLOSE CCERRF
           IF FS-CCERRF NOT = "00"
              DISPLAY "CCERRF クローズ失敗 ST=" FS-CCERRF
              MOVE "Y" TO HARD-ERROR-SW
           END-IF.
