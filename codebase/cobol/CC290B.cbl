       IDENTIFICATION DIVISION.
       PROGRAM-ID. CC290B.
      *
      *---------------------------------------------------------------*
      *  変更履歴                                                     *
      *  版数  年月日    担当    概要                                 *
      *  0.1   20250303  開発    初版作成                             *
      *  0.2   20250418  開発    不一致組織のエラー出力追加           *
      *  0.3   20250610  開発    月境界日カレンダー存在検証追加       *
      *---------------------------------------------------------------*
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CCVALF ASSIGN TO "CCVALF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-CCVALF.
           SELECT CCDTLF ASSIGN TO "CCDTLF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-CCDTLF.
           SELECT CCCALF ASSIGN TO "CCCALF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-CCCALF.
           SELECT CCMONF ASSIGN TO "CCMONF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-CCMONF.
           SELECT CCERRF ASSIGN TO "CCERRF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-CCERRF.

       DATA DIVISION.
       FILE SECTION.
       FD  CCVALF.
           COPY CCVALFC.
       FD  CCDTLF.
           COPY CCDTLC.
       FD  CCCALF.
           COPY CCCALFC.
       FD  CCMONF.
           COPY CCMONC.
       FD  CCERRF.
           COPY CCERRC.

       WORKING-STORAGE SECTION.
       01  FS-AREA.
           05 FS-CCVALF              PIC XX.
           05 FS-CCDTLF              PIC XX.
           05 FS-CCCALF              PIC XX.
           05 FS-CCMONF              PIC XX.
           05 FS-CCERRF              PIC XX.

       01  CTL-AREA.
           05 CTL-PARM               PIC X(32).
           05 CTL-YYYYMM             PIC 9(6).
           05 CTL-FROM-DT            PIC 9(8).
           05 CTL-TO-DT              PIC 9(8).
           05 CTL-MM                 PIC 99.
           05 CTL-LAST-DD            PIC 99.
           05 CTL-LEAP-REM4          PIC 9(4).
           05 CTL-LEAP-REM100        PIC 9(4).
           05 CTL-LEAP-REM400        PIC 9(4).

       01  SW-AREA.
           05 EOF-CCVALF             PIC X VALUE SPACE.
              88 CCVALF-END          VALUE "Y".
           05 EOF-CCDTLF             PIC X VALUE SPACE.
              88 CCDTLF-END          VALUE "Y".
           05 EOF-CCCALF             PIC X VALUE SPACE.
              88 CCCALF-END          VALUE "Y".
           05 SW-FROM-FOUND          PIC X VALUE SPACE.
              88 FROM-FOUND          VALUE "Y".
           05 SW-TO-FOUND            PIC X VALUE SPACE.
              88 TO-FOUND            VALUE "Y".
           05 SW-FOUND               PIC X VALUE SPACE.
              88 FOUND               VALUE "Y".

       01  WK-AREA.
           05 WK-IDX                 PIC 9(4) COMP.
           05 WK-MON-IDX             PIC 9(4) COMP.
           05 WK-VAL-IDX             PIC 9(4) COMP.
           05 WK-MON-CNT             PIC 9(4) COMP VALUE 0.
           05 WK-VAL-CNT             PIC 9(4) COMP VALUE 0.
           05 WK-ERR-SEQ             PIC 9(7) COMP VALUE 0.
           05 WK-DIFF-AMT            PIC S9(15)V99 COMP-3.
           05 WK-DISP-ST             PIC XX.
           05 WK-DISP-KEY            PIC X(40).
           05 WK-ERR-TEXT            PIC X(80).

       01  VAL-TABLE.
           05 VAL-ENTRY OCCURS 10000 TIMES.
              10 T-VAL-ID            PIC X(20).
              10 T-FCT-ID            PIC X(20).
              10 T-VALUE-DT          PIC 9(8).
              10 T-USED              PIC X.

       01  MON-TABLE.
           05 MON-ENTRY OCCURS 2000 TIMES.
              10 T-ORG-CD            PIC X(10).
              10 T-YYYYMM            PIC 9(6).
              10 T-INSTR-AMT         PIC S9(15)V99 COMP-3.
              10 T-VALUE-AMT         PIC S9(15)V99 COMP-3.
              10 T-COUNT-INSTR       PIC 9(9) COMP.
              10 T-COUNT-VALUE       PIC 9(9) COMP.

       PROCEDURE DIVISION.
       MAIN-RTN.
           MOVE 0 TO RETURN-CODE
           PERFORM 0100-INIT
           IF RETURN-CODE NOT = 0
              GOBACK
           END-IF

           PERFORM 1000-READ-CALENDAR
           IF RETURN-CODE NOT = 0
              PERFORM 9000-CLOSE-FILES
              GOBACK
           END-IF

           PERFORM 2000-LOAD-VALUES
           IF RETURN-CODE NOT = 0
              PERFORM 9000-CLOSE-FILES
              GOBACK
           END-IF

           PERFORM 3000-READ-DETAILS
           IF RETURN-CODE NOT = 0
              PERFORM 9000-CLOSE-FILES
              GOBACK
           END-IF

           PERFORM 4000-WRITE-MONTHLY
           IF RETURN-CODE NOT = 0
              PERFORM 9000-CLOSE-FILES
              GOBACK
           END-IF

           PERFORM 5000-CHECK-DIFF
           IF RETURN-CODE NOT = 0
              PERFORM 9000-CLOSE-FILES
              GOBACK
           END-IF

           PERFORM 9000-CLOSE-FILES
           IF RETURN-CODE = 0
              DISPLAY "CC290B 月次受渡実績集計 正常終了"
           END-IF
           GOBACK.

       0100-INIT.
           ACCEPT CTL-PARM FROM COMMAND-LINE
           IF CTL-PARM(1:6) NOT NUMERIC
              DISPLAY "CC290B 起動年月不正"
              MOVE 8 TO RETURN-CODE
              EXIT PARAGRAPH
           END-IF

           MOVE CTL-PARM(1:6) TO CTL-YYYYMM
           MOVE CTL-YYYYMM(5:2) TO CTL-MM
           IF CTL-MM < 1 OR CTL-MM > 12
              DISPLAY "CC290B 起動年月の月不正"
              MOVE 8 TO RETURN-CODE
              EXIT PARAGRAPH
           END-IF

           PERFORM 0110-MAKE-RANGE
           IF RETURN-CODE NOT = 0
              EXIT PARAGRAPH
           END-IF

           OPEN INPUT  CCVALF
                       CCDTLF
                       CCCALF
                OUTPUT CCMONF
                       CCERRF

           IF FS-CCVALF NOT = "00"
              MOVE FS-CCVALF TO WK-DISP-ST
              DISPLAY "CCVALF オープン失敗 ST=" WK-DISP-ST
              MOVE 12 TO RETURN-CODE
           END-IF
           IF FS-CCDTLF NOT = "00"
              MOVE FS-CCDTLF TO WK-DISP-ST
              DISPLAY "CCDTLF オープン失敗 ST=" WK-DISP-ST
              MOVE 12 TO RETURN-CODE
           END-IF
           IF FS-CCCALF NOT = "00"
              MOVE FS-CCCALF TO WK-DISP-ST
              DISPLAY "CCCALF オープン失敗 ST=" WK-DISP-ST
              MOVE 12 TO RETURN-CODE
           END-IF
           IF FS-CCMONF NOT = "00"
              MOVE FS-CCMONF TO WK-DISP-ST
              DISPLAY "CCMONF オープン失敗 ST=" WK-DISP-ST
              MOVE 12 TO RETURN-CODE
           END-IF
           IF FS-CCERRF NOT = "00"
              MOVE FS-CCERRF TO WK-DISP-ST
              DISPLAY "CCERRF オープン失敗 ST=" WK-DISP-ST
              MOVE 12 TO RETURN-CODE
           END-IF.

       0110-MAKE-RANGE.
           MOVE 31 TO CTL-LAST-DD
           EVALUATE CTL-MM
              WHEN 4
              WHEN 6
              WHEN 9
              WHEN 11
                 MOVE 30 TO CTL-LAST-DD
              WHEN 2
                 DIVIDE CTL-YYYYMM(1:4) BY 4
                    GIVING WK-IDX REMAINDER CTL-LEAP-REM4
                 DIVIDE CTL-YYYYMM(1:4) BY 100
                    GIVING WK-IDX REMAINDER CTL-LEAP-REM100
                 DIVIDE CTL-YYYYMM(1:4) BY 400
                    GIVING WK-IDX REMAINDER CTL-LEAP-REM400
                 IF CTL-LEAP-REM400 = 0
                    MOVE 29 TO CTL-LAST-DD
                 ELSE
                    IF CTL-LEAP-REM4 = 0
                       AND CTL-LEAP-REM100 NOT = 0
                       MOVE 29 TO CTL-LAST-DD
                    ELSE
                       MOVE 28 TO CTL-LAST-DD
                    END-IF
                 END-IF
           END-EVALUATE

           MOVE CTL-YYYYMM TO CTL-FROM-DT(1:6)
           MOVE 01 TO CTL-FROM-DT(7:2)
           MOVE CTL-YYYYMM TO CTL-TO-DT(1:6)
           MOVE CTL-LAST-DD TO CTL-TO-DT(7:2).

       1000-READ-CALENDAR.
           PERFORM UNTIL CCCALF-END
              READ CCCALF
                 AT END
                    SET CCCALF-END TO TRUE
                 NOT AT END
                    IF FS-CCCALF NOT = "00"
                       MOVE FS-CCCALF TO WK-DISP-ST
                       DISPLAY "CCCALF 読込失敗 ST=" WK-DISP-ST
                       MOVE 12 TO RETURN-CODE
                       EXIT PERFORM
                    END-IF
                    IF CL-CAL-DT = CTL-FROM-DT
                       MOVE "Y" TO SW-FROM-FOUND
                    END-IF
                    IF CL-CAL-DT = CTL-TO-DT
                       MOVE "Y" TO SW-TO-FOUND
                    END-IF
              END-READ
           END-PERFORM

           IF RETURN-CODE NOT = 0
              EXIT PARAGRAPH
           END-IF
           IF NOT FROM-FOUND
              MOVE CTL-FROM-DT TO WK-DISP-KEY(1:8)
              DISPLAY "CCCALF 月初日未登録 DT=" WK-DISP-KEY(1:8)
              MOVE 8 TO RETURN-CODE
           END-IF
           IF NOT TO-FOUND
              MOVE CTL-TO-DT TO WK-DISP-KEY(1:8)
              DISPLAY "CCCALF 月末日未登録 DT=" WK-DISP-KEY(1:8)
              MOVE 8 TO RETURN-CODE
           END-IF.

       2000-LOAD-VALUES.
           PERFORM UNTIL CCVALF-END
              READ CCVALF
                 AT END
                    SET CCVALF-END TO TRUE
                 NOT AT END
                    IF FS-CCVALF NOT = "00"
                       MOVE FS-CCVALF TO WK-DISP-ST
                       DISPLAY "CCVALF 読込失敗 ST=" WK-DISP-ST
                       MOVE 12 TO RETURN-CODE
                       EXIT PERFORM
                    END-IF
                    IF VL-VAL-STATUS-KBN = "01"
                       AND VL-VALUE-DT >= CTL-FROM-DT
                       AND VL-VALUE-DT <= CTL-TO-DT
                       PERFORM 2100-ADD-VALUE
                    END-IF
              END-READ
           END-PERFORM.

       2100-ADD-VALUE.
           IF WK-VAL-CNT >= 10000
              DISPLAY "CC290B 確定対象テーブル満杯"
              MOVE 12 TO RETURN-CODE
              EXIT PARAGRAPH
           END-IF
           ADD 1 TO WK-VAL-CNT
           MOVE VL-VAL-ID TO T-VAL-ID(WK-VAL-CNT)
           MOVE VL-FCT-ID TO T-FCT-ID(WK-VAL-CNT)
           MOVE VL-VALUE-DT TO T-VALUE-DT(WK-VAL-CNT)
           MOVE SPACE TO T-USED(WK-VAL-CNT).

       3000-READ-DETAILS.
           PERFORM UNTIL CCDTLF-END
              READ CCDTLF
                 AT END
                    SET CCDTLF-END TO TRUE
                 NOT AT END
                    IF FS-CCDTLF NOT = "00"
                       MOVE FS-CCDTLF TO WK-DISP-ST
                       DISPLAY "CCDTLF 読込失敗 ST=" WK-DISP-ST
                       MOVE 12 TO RETURN-CODE
                       EXIT PERFORM
                    END-IF
                    IF DL-DETAIL-STATUS-KBN = "01"
                       AND DL-VALUE-DT >= CTL-FROM-DT
                       AND DL-VALUE-DT <= CTL-TO-DT
                       PERFORM 3100-ADD-DETAIL
                    END-IF
              END-READ
           END-PERFORM.

       3100-ADD-DETAIL.
           PERFORM 3200-FIND-MONTHLY
           IF RETURN-CODE NOT = 0
              EXIT PARAGRAPH
           END-IF

           ADD DL-DETAIL-AMT TO T-INSTR-AMT(WK-MON-IDX)
           ADD 1 TO T-COUNT-INSTR(WK-MON-IDX)

           PERFORM 3300-FIND-VALUE
           IF FOUND
              ADD DL-DETAIL-AMT TO T-VALUE-AMT(WK-MON-IDX)
              ADD 1 TO T-COUNT-VALUE(WK-MON-IDX)
              MOVE "Y" TO T-USED(WK-VAL-IDX)
           ELSE
              MOVE DL-VAL-ID TO WK-DISP-KEY(1:20)
              MOVE "確定対象なし" TO WK-ERR-TEXT
              PERFORM 6100-WRITE-ERROR
           END-IF.

       3200-FIND-MONTHLY.
           MOVE 1 TO WK-IDX
           MOVE SPACE TO SW-FOUND
           PERFORM UNTIL WK-IDX > WK-MON-CNT OR FOUND
              IF T-ORG-CD(WK-IDX) = DL-ORG-CD
                 AND T-YYYYMM(WK-IDX) = CTL-YYYYMM
                 MOVE "Y" TO SW-FOUND
                 MOVE WK-IDX TO WK-MON-IDX
              ELSE
                 ADD 1 TO WK-IDX
              END-IF
           END-PERFORM

           IF NOT FOUND
              IF WK-MON-CNT >= 2000
                 DISPLAY "CC290B 組織月テーブル満杯"
                 MOVE 12 TO RETURN-CODE
                 EXIT PARAGRAPH
              END-IF
              ADD 1 TO WK-MON-CNT
              MOVE WK-MON-CNT TO WK-MON-IDX
              MOVE DL-ORG-CD TO T-ORG-CD(WK-MON-IDX)
              MOVE CTL-YYYYMM TO T-YYYYMM(WK-MON-IDX)
              MOVE 0 TO T-INSTR-AMT(WK-MON-IDX)
              MOVE 0 TO T-VALUE-AMT(WK-MON-IDX)
              MOVE 0 TO T-COUNT-INSTR(WK-MON-IDX)
              MOVE 0 TO T-COUNT-VALUE(WK-MON-IDX)
           END-IF.

       3300-FIND-VALUE.
           MOVE 1 TO WK-IDX
           MOVE SPACE TO SW-FOUND
           PERFORM UNTIL WK-IDX > WK-VAL-CNT OR FOUND
              IF T-VAL-ID(WK-IDX) = DL-VAL-ID
                 AND T-FCT-ID(WK-IDX) = DL-FCT-ID
                 AND T-VALUE-DT(WK-IDX) = DL-VALUE-DT
                 MOVE "Y" TO SW-FOUND
                 MOVE WK-IDX TO WK-VAL-IDX
              ELSE
                 ADD 1 TO WK-IDX
              END-IF
           END-PERFORM.

       4000-WRITE-MONTHLY.
           PERFORM VARYING WK-IDX FROM 1 BY 1
              UNTIL WK-IDX > WK-MON-CNT
              MOVE SPACES TO CCMONF-REC
              MOVE T-ORG-CD(WK-IDX) TO MN-ORG-CD
              MOVE T-YYYYMM(WK-IDX) TO MN-YYYYMM
              MOVE T-INSTR-AMT(WK-IDX) TO MN-TOTAL-INSTR-AMT
              MOVE T-VALUE-AMT(WK-IDX) TO MN-TOTAL-VALUE-AMT
              MOVE T-COUNT-INSTR(WK-IDX) TO MN-COUNT-INSTR
              MOVE T-COUNT-VALUE(WK-IDX) TO MN-COUNT-VALUE
              WRITE CCMONF-REC
              IF FS-CCMONF NOT = "00"
                 MOVE FS-CCMONF TO WK-DISP-ST
                 DISPLAY "CCMONF 書込失敗 ST=" WK-DISP-ST
                 MOVE 12 TO RETURN-CODE
                 EXIT PERFORM
              END-IF
           END-PERFORM.

       5000-CHECK-DIFF.
           PERFORM VARYING WK-IDX FROM 1 BY 1
              UNTIL WK-IDX > WK-MON-CNT
              COMPUTE WK-DIFF-AMT =
                 T-INSTR-AMT(WK-IDX) - T-VALUE-AMT(WK-IDX)
              IF WK-DIFF-AMT NOT = 0
                 MOVE T-ORG-CD(WK-IDX) TO WK-DISP-KEY(1:10)
                 MOVE "金額不一致" TO WK-ERR-TEXT
                 PERFORM 6100-WRITE-ERROR
              END-IF
           END-PERFORM

           PERFORM VARYING WK-IDX FROM 1 BY 1
              UNTIL WK-IDX > WK-VAL-CNT
              IF T-USED(WK-IDX) NOT = "Y"
                 MOVE T-VAL-ID(WK-IDX) TO WK-DISP-KEY(1:20)
                 MOVE "対応明細なし" TO WK-ERR-TEXT
                 PERFORM 6100-WRITE-ERROR
              END-IF
           END-PERFORM.

       6100-WRITE-ERROR.
           ADD 1 TO WK-ERR-SEQ
           MOVE SPACES TO CCERRF-REC
           MOVE WK-ERR-SEQ TO ER-ERROR-ID
           MOVE "CC290B" TO ER-PGM-ID
           MOVE CTL-FROM-DT TO ER-BASE-DT
           MOVE WK-DISP-KEY TO ER-RECORD-KEY
           MOVE "E290" TO ER-ERROR-KBN
           MOVE WK-ERR-TEXT TO ER-ERROR-TEXT
           WRITE CCERRF-REC
           IF FS-CCERRF NOT = "00"
              MOVE FS-CCERRF TO WK-DISP-ST
              DISPLAY "CCERRF 書込失敗 ST=" WK-DISP-ST
              MOVE 12 TO RETURN-CODE
           END-IF
           MOVE SPACES TO WK-DISP-KEY
           MOVE SPACES TO WK-ERR-TEXT.

       9000-CLOSE-FILES.
           CLOSE CCVALF
                 CCDTLF
                 CCCALF
                 CCMONF
                 CCERRF.
