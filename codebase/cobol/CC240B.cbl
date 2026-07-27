       IDENTIFICATION DIVISION.
       PROGRAM-ID. CC240B.
      *
      *  変更履歴
      *  版数  年月日    担当    概要
      *  1.00  20240510  ST01    初版作成
      *  1.01  20240822  ST02    エラー明細出力を追加
      *  1.02  20241115  ST03    営業日区分検査を追加
      *
      *  受渡日明細作成バッチ
      *  CCVALFの確定済み受渡日を指図単位へ展開しCCDTLFへ出力する。
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CCVALF ASSIGN TO "CCVALF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-CCVALF.
           SELECT CCFCTF ASSIGN TO "CCFCTF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-CCFCTF.
           SELECT CCINSF ASSIGN TO "CCINSF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS IN-INS-ID
               FILE STATUS IS FS-CCINSF.
           SELECT CCCALF ASSIGN TO "CCCALF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-CCCALF.
           SELECT CCDTLF ASSIGN TO "CCDTLF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-CCDTLF.
           SELECT CCERRF ASSIGN TO "CCERRF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-CCERRF.

       DATA DIVISION.
       FILE SECTION.
       FD  CCVALF.
       COPY CCVALFC.
       FD  CCFCTF.
       COPY CCFCTFC.
       FD  CCINSF.
       COPY CCINSC.
       FD  CCCALF.
       COPY CCCALFC.
       FD  CCDTLF.
       COPY CCDTLC.
       FD  CCERRF.
       COPY CCERRC.

       WORKING-STORAGE SECTION.
       01  WS-PGM-ID                  PIC X(08) VALUE "CC240B".
       01  WS-EOF-FLAGS.
           05  WS-VAL-EOF             PIC X VALUE "N".
           05  WS-FCT-EOF             PIC X VALUE "N".
           05  WS-INS-EOF             PIC X VALUE "N".
           05  WS-CAL-EOF             PIC X VALUE "N".
       01  WS-FILE-STATUS.
           05  FS-CCVALF              PIC XX VALUE SPACES.
           05  FS-CCFCTF              PIC XX VALUE SPACES.
           05  FS-CCINSF              PIC XX VALUE SPACES.
           05  FS-CCCALF              PIC XX VALUE SPACES.
           05  FS-CCDTLF              PIC XX VALUE SPACES.
           05  FS-CCERRF              PIC XX VALUE SPACES.

       01  WS-SWITCHES.
           05  WS-HARD-ERROR          PIC X VALUE "N".
           05  WS-FCT-FOUND           PIC X VALUE "N".
           05  WS-CAL-FOUND           PIC X VALUE "N".
           05  WS-HAS-INSTRUCTION     PIC X VALUE "N".
           05  WS-VALUE-DT-OK         PIC X VALUE "N".

       01  WS-COUNTERS.
           05  WS-FCT-CNT             PIC 9(05) VALUE ZERO.
           05  WS-INS-CNT             PIC 9(05) VALUE ZERO.
           05  WS-CAL-CNT             PIC 9(05) VALUE ZERO.
           05  WS-DTL-CNT             PIC 9(07) VALUE ZERO.
           05  WS-ERR-CNT             PIC 9(07) VALUE ZERO.
           05  WS-IDX                 PIC 9(05) VALUE ZERO.
           05  WS-JDX                 PIC 9(05) VALUE ZERO.

       01  WS-AMOUNTS.
           05  WS-INSTR-SUM           PIC S9(15)V99 VALUE ZERO.
           05  WS-DETAIL-AMT          PIC S9(15)V99 VALUE ZERO.

       01  WS-WORK-KEYS.
           05  WS-VAL-KEY             PIC X(40) VALUE SPACES.
           05  WS-FCT-KEY             PIC X(40) VALUE SPACES.
           05  WS-INS-KEY             PIC X(40) VALUE SPACES.
           05  WS-ERR-ID-N            PIC 9(09) VALUE ZERO.
           05  WS-ERR-ID-X            PIC X(09) VALUE SPACES.

       01  WS-FCT-TABLE.
           05  WS-FCT-ENTRY OCCURS 20000 TIMES.
               10  T-FCT-ID           PIC X(40).
               10  T-TRIGGER-DT       PIC 9(08).
               10  T-CONC-AMT         PIC S9(15)V99.
               10  T-FCT-STATUS-KBN   PIC X(02).

       01  WS-INS-TABLE.
           05  WS-INS-ENTRY OCCURS 50000 TIMES.
               10  T-INS-ID           PIC X(40).
               10  T-RECV-DT          PIC 9(08).
               10  T-ORG-CD           PIC X(12).
               10  T-INS-FCT-ID       PIC X(40).
               10  T-INSTR-AMT        PIC S9(15)V99.
               10  T-INSTR-KBN        PIC X(02).
               10  T-INSTR-STATUS-KBN PIC X(02).

       01  WS-CAL-TABLE.
           05  WS-CAL-ENTRY OCCURS 20000 TIMES.
               10  T-CAL-DT           PIC 9(08).
               10  T-HOLIDAY-FLAG     PIC X.

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE 0 TO RETURN-CODE
           PERFORM 1000-OPEN-FILES
           IF WS-HARD-ERROR = "N"
               PERFORM 2000-LOAD-MASTER
           END-IF
           IF WS-HARD-ERROR = "N"
               PERFORM 3000-PROCESS-VALUES
           END-IF
           PERFORM 9000-CLOSE-FILES
           IF WS-HARD-ERROR = "Y"
               MOVE 8 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
               DISPLAY "CC240B 正常終了 明細=" WS-DTL-CNT
                       " エラー=" WS-ERR-CNT
           END-IF
           GOBACK.

       1000-OPEN-FILES.
           OPEN INPUT CCVALF CCFCTF CCINSF CCCALF
                OUTPUT CCDTLF CCERRF
           IF FS-CCVALF NOT = "00"
               DISPLAY "CCVALF オープン失敗 ST=" FS-CCVALF
               MOVE "Y" TO WS-HARD-ERROR
           END-IF
           IF FS-CCFCTF NOT = "00"
               DISPLAY "CCFCTF オープン失敗 ST=" FS-CCFCTF
               MOVE "Y" TO WS-HARD-ERROR
           END-IF
           IF FS-CCINSF NOT = "00"
               DISPLAY "CCINSF オープン失敗 ST=" FS-CCINSF
               MOVE "Y" TO WS-HARD-ERROR
           END-IF
           IF FS-CCCALF NOT = "00"
               DISPLAY "CCCALF オープン失敗 ST=" FS-CCCALF
               MOVE "Y" TO WS-HARD-ERROR
           END-IF
           IF FS-CCDTLF NOT = "00"
               DISPLAY "CCDTLF オープン失敗 ST=" FS-CCDTLF
               MOVE "Y" TO WS-HARD-ERROR
           END-IF
           IF FS-CCERRF NOT = "00"
               DISPLAY "CCERRF オープン失敗 ST=" FS-CCERRF
               MOVE "Y" TO WS-HARD-ERROR
           END-IF.

       2000-LOAD-MASTER.
           PERFORM 2100-LOAD-FCTF
           IF WS-HARD-ERROR = "N"
               PERFORM 2200-LOAD-INSF
           END-IF
           IF WS-HARD-ERROR = "N"
               PERFORM 2300-LOAD-CALF
           END-IF.

       2100-LOAD-FCTF.
           PERFORM UNTIL WS-FCT-EOF = "Y" OR WS-HARD-ERROR = "Y"
               READ CCFCTF
                   AT END
                       MOVE "Y" TO WS-FCT-EOF
                   NOT AT END
                       IF FS-CCFCTF NOT = "00"
                           DISPLAY "CCFCTF 読込失敗 ST=" FS-CCFCTF
                           MOVE "Y" TO WS-HARD-ERROR
                       ELSE
                           ADD 1 TO WS-FCT-CNT
                           IF WS-FCT-CNT > 20000
                               DISPLAY "CCFCTF 件数上限超過"
                               MOVE "Y" TO WS-HARD-ERROR
                           ELSE
                               MOVE FC-FCT-ID
                                 TO T-FCT-ID(WS-FCT-CNT)
                               MOVE FC-TRIGGER-DT
                                 TO T-TRIGGER-DT(WS-FCT-CNT)
                               MOVE FC-CONC-AMT
                                 TO T-CONC-AMT(WS-FCT-CNT)
                               MOVE FC-FCT-STATUS-KBN
                                 TO T-FCT-STATUS-KBN(WS-FCT-CNT)
                           END-IF
                       END-IF
               END-READ
           END-PERFORM.

       2200-LOAD-INSF.
           PERFORM UNTIL WS-INS-EOF = "Y" OR WS-HARD-ERROR = "Y"
               READ CCINSF NEXT RECORD
                   AT END
                       MOVE "Y" TO WS-INS-EOF
                   NOT AT END
                       IF FS-CCINSF NOT = "00"
                           DISPLAY "CCINSF 読込失敗 ST=" FS-CCINSF
                           MOVE "Y" TO WS-HARD-ERROR
                       ELSE
                           ADD 1 TO WS-INS-CNT
                           IF WS-INS-CNT > 50000
                               DISPLAY "CCINSF 件数上限超過"
                               MOVE "Y" TO WS-HARD-ERROR
                           ELSE
                               MOVE IN-INS-ID
                                 TO T-INS-ID(WS-INS-CNT)
                               MOVE IN-RECV-DT
                                 TO T-RECV-DT(WS-INS-CNT)
                               MOVE IN-ORG-CD
                                 TO T-ORG-CD(WS-INS-CNT)
                               MOVE IN-FCT-ID
                                 TO T-INS-FCT-ID(WS-INS-CNT)
                               MOVE IN-INSTR-AMT
                                 TO T-INSTR-AMT(WS-INS-CNT)
                               MOVE IN-INSTR-KBN
                                 TO T-INSTR-KBN(WS-INS-CNT)
                               MOVE IN-INSTR-STATUS-KBN
                                 TO T-INSTR-STATUS-KBN(WS-INS-CNT)
                           END-IF
                       END-IF
               END-READ
           END-PERFORM.

       2300-LOAD-CALF.
           PERFORM UNTIL WS-CAL-EOF = "Y" OR WS-HARD-ERROR = "Y"
               READ CCCALF
                   AT END
                       MOVE "Y" TO WS-CAL-EOF
                   NOT AT END
                       IF FS-CCCALF NOT = "00"
                           DISPLAY "CCCALF 読込失敗 ST=" FS-CCCALF
                           MOVE "Y" TO WS-HARD-ERROR
                       ELSE
                           ADD 1 TO WS-CAL-CNT
                           IF WS-CAL-CNT > 20000
                               DISPLAY "CCCALF 件数上限超過"
                               MOVE "Y" TO WS-HARD-ERROR
                           ELSE
                               MOVE CL-CAL-DT
                                 TO T-CAL-DT(WS-CAL-CNT)
                               MOVE CL-HOLIDAY-FLAG
                                 TO T-HOLIDAY-FLAG(WS-CAL-CNT)
                           END-IF
                       END-IF
               END-READ
           END-PERFORM.

       3000-PROCESS-VALUES.
           PERFORM UNTIL WS-VAL-EOF = "Y" OR WS-HARD-ERROR = "Y"
               READ CCVALF
                   AT END
                       MOVE "Y" TO WS-VAL-EOF
                   NOT AT END
                       IF FS-CCVALF NOT = "00"
                           DISPLAY "CCVALF 読込失敗 ST=" FS-CCVALF
                           MOVE "Y" TO WS-HARD-ERROR
                       ELSE
                           PERFORM 3100-PROCESS-ONE-VAL
                       END-IF
               END-READ
           END-PERFORM.

       3100-PROCESS-ONE-VAL.
           MOVE "N" TO WS-FCT-FOUND
           MOVE "N" TO WS-VALUE-DT-OK
           MOVE "N" TO WS-HAS-INSTRUCTION
           MOVE ZERO TO WS-INSTR-SUM
           MOVE SPACES TO WS-FCT-KEY
           PERFORM 3200-FIND-FCT
           PERFORM 3300-CHECK-VALUE-DT
           IF VL-VAL-STATUS-KBN NOT = "01"
               MOVE VL-VAL-ID TO WS-VAL-KEY
               PERFORM 8100-WRITE-UNFIXED-ERR
           END-IF
           IF WS-FCT-FOUND = "N"
               MOVE VL-FCT-ID TO WS-FCT-KEY
               PERFORM 8200-WRITE-NOFCT-ERR
           ELSE
               IF T-FCT-STATUS-KBN(WS-IDX) NOT = "01"
                   MOVE T-FCT-ID(WS-IDX) TO WS-FCT-KEY
                   PERFORM 8300-WRITE-FCTSTAT-ERR
               END-IF
           END-IF
           IF WS-VALUE-DT-OK = "N"
               MOVE VL-VALUE-DT TO WS-VAL-KEY
               PERFORM 8400-WRITE-CAL-ERR
           END-IF
           IF VL-VAL-STATUS-KBN = "01"
              AND WS-FCT-FOUND = "Y"
              AND T-FCT-STATUS-KBN(WS-IDX) = "01"
              AND WS-VALUE-DT-OK = "Y"
               PERFORM 3400-SUM-INSTRUCTIONS
               IF WS-HAS-INSTRUCTION = "N"
                   MOVE VL-FCT-ID TO WS-FCT-KEY
                   PERFORM 8500-WRITE-NOINS-ERR
               ELSE
                   IF WS-INSTR-SUM NOT = T-CONC-AMT(WS-IDX)
                       MOVE VL-FCT-ID TO WS-FCT-KEY
                       PERFORM 8600-WRITE-AMT-ERR
                   ELSE
                       PERFORM 3500-WRITE-DETAILS
                   END-IF
               END-IF
           END-IF.

       3200-FIND-FCT.
           PERFORM VARYING WS-IDX FROM 1 BY 1
             UNTIL WS-IDX > WS-FCT-CNT OR WS-FCT-FOUND = "Y"
               IF T-FCT-ID(WS-IDX) = VL-FCT-ID
                   MOVE "Y" TO WS-FCT-FOUND
               END-IF
           END-PERFORM.

       3300-CHECK-VALUE-DT.
           MOVE "N" TO WS-CAL-FOUND
           PERFORM VARYING WS-JDX FROM 1 BY 1
             UNTIL WS-JDX > WS-CAL-CNT OR WS-CAL-FOUND = "Y"
               IF T-CAL-DT(WS-JDX) = VL-VALUE-DT
                   MOVE "Y" TO WS-CAL-FOUND
                   IF T-HOLIDAY-FLAG(WS-JDX) = "N"
                       MOVE "Y" TO WS-VALUE-DT-OK
                   END-IF
               END-IF
           END-PERFORM.

       3400-SUM-INSTRUCTIONS.
           PERFORM VARYING WS-JDX FROM 1 BY 1
             UNTIL WS-JDX > WS-INS-CNT
               IF T-INS-FCT-ID(WS-JDX) = VL-FCT-ID
                  AND T-INSTR-STATUS-KBN(WS-JDX) = "01"
                   ADD T-INSTR-AMT(WS-JDX) TO WS-INSTR-SUM
                   MOVE "Y" TO WS-HAS-INSTRUCTION
               END-IF
           END-PERFORM.

       3500-WRITE-DETAILS.
           PERFORM VARYING WS-JDX FROM 1 BY 1
             UNTIL WS-JDX > WS-INS-CNT OR WS-HARD-ERROR = "Y"
               IF T-INS-FCT-ID(WS-JDX) = VL-FCT-ID
                  AND T-INSTR-STATUS-KBN(WS-JDX) = "01"
                   INITIALIZE CCDTLF-REC
                   MOVE VL-VAL-ID         TO DL-VAL-ID
                   MOVE VL-FCT-ID         TO DL-FCT-ID
                   MOVE T-ORG-CD(WS-JDX)  TO DL-ORG-CD
                   MOVE VL-VALUE-DT       TO DL-VALUE-DT
                   MOVE T-INSTR-AMT(WS-JDX)
                                          TO DL-DETAIL-AMT
                   MOVE "01"              TO DL-DETAIL-STATUS-KBN
                   WRITE CCDTLF-REC
                   IF FS-CCDTLF NOT = "00"
                       DISPLAY "CCDTLF 書込失敗 ST=" FS-CCDTLF
                       MOVE "Y" TO WS-HARD-ERROR
                   ELSE
                       ADD 1 TO WS-DTL-CNT
                   END-IF
               END-IF
           END-PERFORM.

       8000-PREPARE-ERROR.
           ADD 1 TO WS-ERR-CNT
           MOVE WS-ERR-CNT TO WS-ERR-ID-N
           MOVE WS-ERR-ID-N TO WS-ERR-ID-X
           INITIALIZE CCERRF-REC
           MOVE WS-ERR-ID-X TO ER-ERROR-ID
           MOVE WS-PGM-ID   TO ER-PGM-ID
           MOVE VL-VALUE-DT TO ER-BASE-DT.

       8100-WRITE-UNFIXED-ERR.
           PERFORM 8000-PREPARE-ERROR
           MOVE VL-VAL-ID TO ER-RECORD-KEY
           MOVE "E101" TO ER-ERROR-KBN
           MOVE "受渡日確定ステータス不正" TO ER-ERROR-TEXT
           PERFORM 8900-WRITE-ERROR.

       8200-WRITE-NOFCT-ERR.
           PERFORM 8000-PREPARE-ERROR
           MOVE VL-FCT-ID TO ER-RECORD-KEY
           MOVE "E102" TO ER-ERROR-KBN
           MOVE "資金集中対象なし" TO ER-ERROR-TEXT
           PERFORM 8900-WRITE-ERROR.

       8300-WRITE-FCTSTAT-ERR.
           PERFORM 8000-PREPARE-ERROR
           MOVE WS-FCT-KEY TO ER-RECORD-KEY
           MOVE "E103" TO ER-ERROR-KBN
           MOVE "資金集中指図状態不正" TO ER-ERROR-TEXT
           PERFORM 8900-WRITE-ERROR.

       8400-WRITE-CAL-ERR.
           PERFORM 8000-PREPARE-ERROR
           MOVE WS-VAL-KEY TO ER-RECORD-KEY
           MOVE "E104" TO ER-ERROR-KBN
           MOVE "受渡日カレンダー不整合" TO ER-ERROR-TEXT
           PERFORM 8900-WRITE-ERROR.

       8500-WRITE-NOINS-ERR.
           PERFORM 8000-PREPARE-ERROR
           MOVE WS-FCT-KEY TO ER-RECORD-KEY
           MOVE "E105" TO ER-ERROR-KBN
           MOVE "指図明細なし" TO ER-ERROR-TEXT
           PERFORM 8900-WRITE-ERROR.

       8600-WRITE-AMT-ERR.
           PERFORM 8000-PREPARE-ERROR
           MOVE WS-FCT-KEY TO ER-RECORD-KEY
           MOVE "E106" TO ER-ERROR-KBN
           MOVE "指図合計金額不一致" TO ER-ERROR-TEXT
           PERFORM 8900-WRITE-ERROR.

       8900-WRITE-ERROR.
           WRITE CCERRF-REC
           IF FS-CCERRF NOT = "00"
               DISPLAY "CCERRF 書込失敗 ST=" FS-CCERRF
               MOVE "Y" TO WS-HARD-ERROR
           END-IF.

       9000-CLOSE-FILES.
           CLOSE CCVALF CCFCTF CCINSF CCCALF CCDTLF CCERRF
           IF FS-CCVALF NOT = "00"
               DISPLAY "CCVALF クローズ失敗 ST=" FS-CCVALF
               MOVE "Y" TO WS-HARD-ERROR
           END-IF
           IF FS-CCFCTF NOT = "00"
               DISPLAY "CCFCTF クローズ失敗 ST=" FS-CCFCTF
               MOVE "Y" TO WS-HARD-ERROR
           END-IF
           IF FS-CCINSF NOT = "00"
               DISPLAY "CCINSF クローズ失敗 ST=" FS-CCINSF
               MOVE "Y" TO WS-HARD-ERROR
           END-IF
           IF FS-CCCALF NOT = "00"
               DISPLAY "CCCALF クローズ失敗 ST=" FS-CCCALF
               MOVE "Y" TO WS-HARD-ERROR
           END-IF
           IF FS-CCDTLF NOT = "00"
               DISPLAY "CCDTLF クローズ失敗 ST=" FS-CCDTLF
               MOVE "Y" TO WS-HARD-ERROR
           END-IF
           IF FS-CCERRF NOT = "00"
               DISPLAY "CCERRF クローズ失敗 ST=" FS-CCERRF
               MOVE "Y" TO WS-HARD-ERROR
           END-IF.
