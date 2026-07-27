       IDENTIFICATION DIVISION.
       PROGRAM-ID. CT260B.
       AUTHOR. MFG-SHIKIN-BATCH.
      * 資金ポジション日次更新バッチ
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CCINSF ASSIGN TO "CCINSF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS IN-INS-ID
               FILE STATUS IS FS-CCINSF.
           SELECT CCDTLF ASSIGN TO "CCDTLF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS FS-CCDTLF.
           SELECT CCXFRF ASSIGN TO "CCXFRF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS FS-CCXFRF.
           SELECT CCPOSF ASSIGN TO "CCPOSF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS PS-ORG-CD
               FILE STATUS IS FS-CCPOSF.
           SELECT CCERRF ASSIGN TO "CCERRF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS FS-CCERRF.

       DATA DIVISION.
       FILE SECTION.
       FD  CCINSF.
           COPY CCINSC.
       FD  CCDTLF.
           COPY CCDTLC.
       FD  CCXFRF.
           COPY CCXFRC.
       FD  CCPOSF.
           COPY CCPOSC.
       FD  CCERRF.
           COPY CCERRC.

       WORKING-STORAGE SECTION.
       01  WS-PGM-ID                    PIC X(08) VALUE "CT260B".
       01  WS-BASE-DT                   PIC 9(08) VALUE ZERO.
       01  WS-SYS-DATE.
           05 WS-SYS-YYYY               PIC 9(04).
           05 WS-SYS-MM                 PIC 9(02).
           05 WS-SYS-DD                 PIC 9(02).

       01  WS-FILE-STATUS.
           05 FS-CCINSF                 PIC X(02).
           05 FS-CCDTLF                 PIC X(02).
           05 FS-CCXFRF                 PIC X(02).
           05 FS-CCPOSF                 PIC X(02).
           05 FS-CCERRF                 PIC X(02).

       01  WS-FLAGS.
           05 WS-EOF-INS                PIC X VALUE "N".
              88 EOF-INS                      VALUE "Y".
           05 WS-EOF-DTL                PIC X VALUE "N".
              88 EOF-DTL                      VALUE "Y".
           05 WS-EOF-XFR                PIC X VALUE "N".
              88 EOF-XFR                      VALUE "Y".
           05 WS-HARD-ERR               PIC X VALUE "N".
              88 HARD-ERR                     VALUE "Y".
           05 WS-ORG-FOUND              PIC X VALUE "N".
           05 WS-FCT-FOUND              PIC X VALUE "N".
           05 WS-DUP-FOUND              PIC X VALUE "N".
           05 WS-POS-FOUND              PIC X VALUE "N".

       01  WS-COUNTERS.
           05 IX-ORG                    PIC 9(04) COMP VALUE ZERO.
           05 IX-FCT                    PIC 9(04) COMP VALUE ZERO.
           05 WS-ORG-CNT                PIC 9(04) COMP VALUE ZERO.
           05 WS-DL-FCT-CNT             PIC 9(04) COMP VALUE ZERO.
           05 WS-ERR-SEQ                PIC 9(07) VALUE ZERO.
           05 WS-READ-INS-CNT           PIC 9(09) VALUE ZERO.
           05 WS-READ-DTL-CNT           PIC 9(09) VALUE ZERO.
           05 WS-READ-XFR-CNT           PIC 9(09) VALUE ZERO.
           05 WS-UPD-POS-CNT            PIC 9(09) VALUE ZERO.
           05 WS-WRT-ERR-CNT            PIC 9(09) VALUE ZERO.

       01  WS-WORK.
           05 WS-CUR-ORG                PIC X(10).
           05 WS-CUR-FCT                PIC X(20).
           05 WS-CUR-AMT                PIC S9(13)V99 COMP-3.
           05 WS-REC-KEY                PIC X(40).
           05 WS-ERR-TEXT               PIC X(80).

       01  WS-ORG-TABLE.
           05 WS-ORG-ENTRY OCCURS 200 TIMES.
              10 TB-ORG-CD              PIC X(10).
              10 TB-INSTR-AMT           PIC S9(13)V99 COMP-3.
              10 TB-DETAIL-AMT          PIC S9(13)V99 COMP-3.
              10 TB-XFR-IN-AMT          PIC S9(13)V99 COMP-3.
              10 TB-XFR-OUT-AMT         PIC S9(13)V99 COMP-3.
              10 TB-ERR-CNT             PIC 9(05) COMP.

       01  WS-DL-FCT-TABLE.
           05 WS-DL-FCT-ENTRY OCCURS 1000 TIMES.
              10 TB-DL-FCT-ID           PIC X(20).

       PROCEDURE DIVISION.
       MAIN-RTN.
           MOVE 0 TO RETURN-CODE
           ACCEPT WS-SYS-DATE FROM DATE YYYYMMDD
           MOVE WS-SYS-DATE TO WS-BASE-DT
           PERFORM 1000-OPEN
           IF HARD-ERR
              PERFORM 9000-CLOSE
              MOVE 12 TO RETURN-CODE
              GOBACK
           END-IF

           PERFORM 2000-READ-INSTRUCTIONS
           PERFORM 3000-READ-DETAILS
           PERFORM 4000-READ-TRANSFERS

           IF NOT HARD-ERR
              PERFORM 5000-UPDATE-POSITIONS
           END-IF

           PERFORM 9000-CLOSE

           IF HARD-ERR
              MOVE 12 TO RETURN-CODE
           ELSE
              DISPLAY "CT260B 正常終了 "
                      "指図=" WS-READ-INS-CNT
                      " 明細=" WS-READ-DTL-CNT
                      " 振替=" WS-READ-XFR-CNT
                      " 更新=" WS-UPD-POS-CNT
                      " 異常=" WS-WRT-ERR-CNT
              MOVE 0 TO RETURN-CODE
           END-IF
           GOBACK.

       1000-OPEN.
           OPEN INPUT CCINSF
           IF FS-CCINSF NOT = "00"
              DISPLAY "CCINSF オープン失敗 ST=" FS-CCINSF
              SET HARD-ERR TO TRUE
           END-IF

           OPEN INPUT CCDTLF
           IF FS-CCDTLF NOT = "00"
              DISPLAY "CCDTLF オープン失敗 ST=" FS-CCDTLF
              SET HARD-ERR TO TRUE
           END-IF

           OPEN INPUT CCXFRF
           IF FS-CCXFRF NOT = "00"
              DISPLAY "CCXFRF オープン失敗 ST=" FS-CCXFRF
              SET HARD-ERR TO TRUE
           END-IF

           OPEN I-O CCPOSF
           IF FS-CCPOSF NOT = "00"
              DISPLAY "CCPOSF オープン失敗 ST=" FS-CCPOSF
              SET HARD-ERR TO TRUE
           END-IF

           OPEN OUTPUT CCERRF
           IF FS-CCERRF NOT = "00"
              DISPLAY "CCERRF オープン失敗 ST=" FS-CCERRF
              SET HARD-ERR TO TRUE
           END-IF.

       2000-READ-INSTRUCTIONS.
           MOVE LOW-VALUES TO IN-INS-ID
           START CCINSF KEY IS >= IN-INS-ID
             INVALID KEY
                IF FS-CCINSF = "23"
                   SET EOF-INS TO TRUE
                ELSE
                   DISPLAY "CCINSF 開始失敗 ST=" FS-CCINSF
                   SET HARD-ERR TO TRUE
                END-IF
           END-START

           PERFORM UNTIL EOF-INS OR HARD-ERR
              READ CCINSF NEXT RECORD
                AT END
                   SET EOF-INS TO TRUE
                NOT AT END
                   ADD 1 TO WS-READ-INS-CNT
                   PERFORM 2100-CHECK-INSTRUCTION
              END-READ
              IF FS-CCINSF NOT = "00" AND FS-CCINSF NOT = "10"
                 DISPLAY "CCINSF 読込失敗 ST=" FS-CCINSF
                 SET HARD-ERR TO TRUE
              END-IF
           END-PERFORM.

       2100-CHECK-INSTRUCTION.
           MOVE SPACES TO WS-ERR-TEXT
           IF IN-ORG-CD = SPACES
              MOVE "組織コード未設定" TO WS-ERR-TEXT
           ELSE
              IF IN-FCT-ID = SPACES
                 MOVE "FCT-ID未設定" TO WS-ERR-TEXT
              ELSE
                 IF IN-INSTR-AMT <= ZERO
                    MOVE "指図金額不正" TO WS-ERR-TEXT
                 ELSE
                    IF IN-INSTR-STATUS-KBN NOT = "1"
                       AND IN-INSTR-STATUS-KBN NOT = "2"
                       MOVE "指図状態不正" TO WS-ERR-TEXT
                    END-IF
                 END-IF
              END-IF
           END-IF

           IF WS-ERR-TEXT NOT = SPACES
              MOVE IN-INS-ID TO WS-REC-KEY
              PERFORM 8100-WRITE-ERROR
           ELSE
              IF IN-INSTR-STATUS-KBN = "1"
                 MOVE IN-ORG-CD TO WS-CUR-ORG
                 MOVE IN-INSTR-AMT TO WS-CUR-AMT
                 PERFORM 7100-ADD-INSTR
              END-IF
           END-IF.

       3000-READ-DETAILS.
           PERFORM UNTIL EOF-DTL OR HARD-ERR
              READ CCDTLF
                AT END
                   SET EOF-DTL TO TRUE
                NOT AT END
                   ADD 1 TO WS-READ-DTL-CNT
                   PERFORM 3100-CHECK-DETAIL
              END-READ
              IF FS-CCDTLF NOT = "00" AND FS-CCDTLF NOT = "10"
                 DISPLAY "CCDTLF 読込失敗 ST=" FS-CCDTLF
                 SET HARD-ERR TO TRUE
              END-IF
           END-PERFORM.

       3100-CHECK-DETAIL.
           MOVE SPACES TO WS-ERR-TEXT
           IF DL-ORG-CD = SPACES
              MOVE "明細組織コード未設定" TO WS-ERR-TEXT
           ELSE
              IF DL-FCT-ID = SPACES
                 MOVE "明細FCT-ID未設定" TO WS-ERR-TEXT
              ELSE
                 IF DL-DETAIL-AMT <= ZERO
                    MOVE "受渡明細金額不正" TO WS-ERR-TEXT
                 ELSE
                    IF DL-DETAIL-STATUS-KBN NOT = "1"
                       AND DL-DETAIL-STATUS-KBN NOT = "2"
                       MOVE "受渡明細状態不正" TO WS-ERR-TEXT
                    END-IF
                 END-IF
              END-IF
           END-IF

           IF WS-ERR-TEXT NOT = SPACES
              MOVE DL-VAL-ID TO WS-REC-KEY
              PERFORM 8100-WRITE-ERROR
           ELSE
              IF DL-DETAIL-STATUS-KBN = "1"
                 MOVE DL-ORG-CD TO WS-CUR-ORG
                 MOVE DL-DETAIL-AMT TO WS-CUR-AMT
                 PERFORM 7200-ADD-DETAIL
                 MOVE DL-FCT-ID TO WS-CUR-FCT
                 PERFORM 7400-STORE-DL-FCT
              END-IF
           END-IF.

       4000-READ-TRANSFERS.
           PERFORM UNTIL EOF-XFR OR HARD-ERR
              READ CCXFRF
                AT END
                   SET EOF-XFR TO TRUE
                NOT AT END
                   ADD 1 TO WS-READ-XFR-CNT
                   PERFORM 4100-CHECK-TRANSFER
              END-READ
              IF FS-CCXFRF NOT = "00" AND FS-CCXFRF NOT = "10"
                 DISPLAY "CCXFRF 読込失敗 ST=" FS-CCXFRF
                 SET HARD-ERR TO TRUE
              END-IF
           END-PERFORM.

       4100-CHECK-TRANSFER.
           MOVE SPACES TO WS-ERR-TEXT
           IF XF-FCT-ID = SPACES
              MOVE "振替FCT-ID未設定" TO WS-ERR-TEXT
           ELSE
              IF XF-FROM-ORG-CD = SPACES OR XF-TO-ORG-CD = SPACES
                 MOVE "振替組織コード未設定" TO WS-ERR-TEXT
              ELSE
                 IF XF-XFER-AMT <= ZERO
                    MOVE "振替金額不正" TO WS-ERR-TEXT
                 ELSE
                    IF XF-XFER-STATUS-KBN NOT = "1"
                       AND XF-XFER-STATUS-KBN NOT = "2"
                       MOVE "振替状態不正" TO WS-ERR-TEXT
                    END-IF
                 END-IF
              END-IF
           END-IF

           IF WS-ERR-TEXT NOT = SPACES
              MOVE XF-XFER-ID TO WS-REC-KEY
              PERFORM 8100-WRITE-ERROR
           ELSE
              IF XF-XFER-STATUS-KBN = "1"
                 MOVE XF-FCT-ID TO WS-CUR-FCT
                 PERFORM 7500-CHECK-DUP-FCT
                 IF WS-DUP-FOUND = "Y"
                    MOVE "明細振替FCT-ID二重計上" TO WS-ERR-TEXT
                    MOVE XF-XFER-ID TO WS-REC-KEY
                    PERFORM 8100-WRITE-ERROR
                 ELSE
                    MOVE XF-FROM-ORG-CD TO WS-CUR-ORG
                    MOVE XF-XFER-AMT TO WS-CUR-AMT
                    PERFORM 7300-ADD-XFR-OUT
                    MOVE XF-TO-ORG-CD TO WS-CUR-ORG
                    MOVE XF-XFER-AMT TO WS-CUR-AMT
                    PERFORM 7350-ADD-XFR-IN
                 END-IF
              END-IF
           END-IF.

       5000-UPDATE-POSITIONS.
           PERFORM VARYING IX-ORG FROM 1 BY 1
                   UNTIL IX-ORG > WS-ORG-CNT OR HARD-ERR
              MOVE TB-ORG-CD(IX-ORG) TO PS-ORG-CD
              READ CCPOSF
                INVALID KEY
                   MOVE "N" TO WS-POS-FOUND
                NOT INVALID KEY
                   MOVE "Y" TO WS-POS-FOUND
              END-READ

              IF FS-CCPOSF = "00" OR FS-CCPOSF = "23"
                 IF WS-POS-FOUND = "N"
                    INITIALIZE CCPOSF-REC
                    MOVE TB-ORG-CD(IX-ORG) TO PS-ORG-CD
                    MOVE WS-BASE-DT TO PS-BASE-DT
                    MOVE ZERO TO PS-AVAILABLE-AMT
                    MOVE ZERO TO PS-RESERVED-AMT
                    MOVE "1" TO PS-POSITION-STATUS-KBN
                 END-IF

                 COMPUTE PS-RESERVED-AMT =
                         TB-INSTR-AMT(IX-ORG)
                       - TB-DETAIL-AMT(IX-ORG)

                 COMPUTE PS-AVAILABLE-AMT =
                         PS-AVAILABLE-AMT
                       + TB-DETAIL-AMT(IX-ORG)
                       + TB-XFR-IN-AMT(IX-ORG)
                       - TB-XFR-OUT-AMT(IX-ORG)

                 IF PS-RESERVED-AMT < ZERO
                    MOVE ZERO TO PS-RESERVED-AMT
                 END-IF

                 MOVE WS-BASE-DT TO PS-BASE-DT
                 IF TB-ERR-CNT(IX-ORG) > ZERO
                    MOVE "9" TO PS-POSITION-STATUS-KBN
                 ELSE
                    MOVE "1" TO PS-POSITION-STATUS-KBN
                 END-IF

                 IF WS-POS-FOUND = "Y"
                    REWRITE CCPOSF-REC
                 ELSE
                    WRITE CCPOSF-REC
                 END-IF

                 IF FS-CCPOSF = "00"
                    ADD 1 TO WS-UPD-POS-CNT
                 ELSE
                    DISPLAY "CCPOSF 更新失敗 ST=" FS-CCPOSF
                    SET HARD-ERR TO TRUE
                 END-IF
              ELSE
                 DISPLAY "CCPOSF 読込失敗 ST=" FS-CCPOSF
                 SET HARD-ERR TO TRUE
              END-IF
           END-PERFORM.

       7100-ADD-INSTR.
           PERFORM 7000-FIND-ORG
           IF WS-ORG-FOUND = "Y"
              ADD WS-CUR-AMT TO TB-INSTR-AMT(IX-ORG)
           END-IF.

       7200-ADD-DETAIL.
           PERFORM 7000-FIND-ORG
           IF WS-ORG-FOUND = "Y"
              ADD WS-CUR-AMT TO TB-DETAIL-AMT(IX-ORG)
           END-IF.

       7300-ADD-XFR-OUT.
           PERFORM 7000-FIND-ORG
           IF WS-ORG-FOUND = "Y"
              ADD WS-CUR-AMT TO TB-XFR-OUT-AMT(IX-ORG)
           END-IF.

       7350-ADD-XFR-IN.
           PERFORM 7000-FIND-ORG
           IF WS-ORG-FOUND = "Y"
              ADD WS-CUR-AMT TO TB-XFR-IN-AMT(IX-ORG)
           END-IF.

       7000-FIND-ORG.
           MOVE "N" TO WS-ORG-FOUND
           PERFORM VARYING IX-ORG FROM 1 BY 1
                   UNTIL IX-ORG > WS-ORG-CNT
                      OR WS-ORG-FOUND = "Y"
              IF TB-ORG-CD(IX-ORG) = WS-CUR-ORG
                 MOVE "Y" TO WS-ORG-FOUND
              END-IF
           END-PERFORM

           IF WS-ORG-FOUND = "N"
              IF WS-ORG-CNT < 200
                 ADD 1 TO WS-ORG-CNT
                 MOVE WS-ORG-CNT TO IX-ORG
                 INITIALIZE WS-ORG-ENTRY(IX-ORG)
                 MOVE WS-CUR-ORG TO TB-ORG-CD(IX-ORG)
                 MOVE "Y" TO WS-ORG-FOUND
              ELSE
                 MOVE "組織集計領域不足" TO WS-ERR-TEXT
                 MOVE WS-CUR-ORG TO WS-REC-KEY
                 PERFORM 8100-WRITE-ERROR
                 SET HARD-ERR TO TRUE
              END-IF
           END-IF.

       7400-STORE-DL-FCT.
           MOVE "N" TO WS-FCT-FOUND
           PERFORM VARYING IX-FCT FROM 1 BY 1
                   UNTIL IX-FCT > WS-DL-FCT-CNT
                      OR WS-FCT-FOUND = "Y"
              IF TB-DL-FCT-ID(IX-FCT) = WS-CUR-FCT
                 MOVE "Y" TO WS-FCT-FOUND
              END-IF
           END-PERFORM

           IF WS-FCT-FOUND = "N"
              IF WS-DL-FCT-CNT < 1000
                 ADD 1 TO WS-DL-FCT-CNT
                 MOVE WS-CUR-FCT TO TB-DL-FCT-ID(WS-DL-FCT-CNT)
              ELSE
                 MOVE "FCT集計領域不足" TO WS-ERR-TEXT
                 MOVE WS-CUR-FCT TO WS-REC-KEY
                 PERFORM 8100-WRITE-ERROR
                 SET HARD-ERR TO TRUE
              END-IF
           END-IF.

       7500-CHECK-DUP-FCT.
           MOVE "N" TO WS-DUP-FOUND
           PERFORM VARYING IX-FCT FROM 1 BY 1
                   UNTIL IX-FCT > WS-DL-FCT-CNT
                      OR WS-DUP-FOUND = "Y"
              IF TB-DL-FCT-ID(IX-FCT) = WS-CUR-FCT
                 MOVE "Y" TO WS-DUP-FOUND
              END-IF
           END-PERFORM.

       8100-WRITE-ERROR.
           INITIALIZE CCERRF-REC
           ADD 1 TO WS-ERR-SEQ
           MOVE WS-ERR-SEQ TO ER-ERROR-ID
           MOVE WS-PGM-ID TO ER-PGM-ID
           MOVE WS-BASE-DT TO ER-BASE-DT
           MOVE WS-REC-KEY TO ER-RECORD-KEY
           MOVE "E" TO ER-ERROR-KBN
           MOVE WS-ERR-TEXT TO ER-ERROR-TEXT
           WRITE CCERRF-REC
           IF FS-CCERRF = "00"
              ADD 1 TO WS-WRT-ERR-CNT
              PERFORM 8200-MARK-ORG-ERROR
           ELSE
              DISPLAY "CCERRF 書込失敗 ST=" FS-CCERRF
              SET HARD-ERR TO TRUE
           END-IF.

       8200-MARK-ORG-ERROR.
           IF WS-CUR-ORG NOT = SPACES
              PERFORM 7000-FIND-ORG
              IF WS-ORG-FOUND = "Y"
                 ADD 1 TO TB-ERR-CNT(IX-ORG)
              END-IF
           END-IF.

       9000-CLOSE.
           CLOSE CCINSF
           CLOSE CCDTLF
           CLOSE CCXFRF
           CLOSE CCPOSF
           CLOSE CCERRF.
