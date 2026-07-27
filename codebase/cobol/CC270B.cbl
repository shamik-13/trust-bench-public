       IDENTIFICATION DIVISION.
       PROGRAM-ID. CC270B.
      ******************************************************************
      * 変更履歴
      * 版数  年月日    担当     概要
      * 0.1   20250303  共通基盤  新規作成
      * 0.2   20250418  共通基盤  監査エラー出力追加
      * 0.3   20250602  共通基盤  営業日検査を追加
      ******************************************************************
      * 価値日確定結果監査バッチ
      * CC110Bが出力した価値日確定結果を再計算せず監査する。
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CCVALF ASSIGN TO "CCVALF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-ST-CCVALF.
           SELECT CCFCTF ASSIGN TO "CCFCTF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-ST-CCFCTF.
           SELECT CCCALF ASSIGN TO "CCCALF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-ST-CCCALF.
           SELECT CCERRF ASSIGN TO "CCERRF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-ST-CCERRF.

       DATA DIVISION.
       FILE SECTION.
       FD  CCVALF.
       COPY CCVALFC.
       FD  CCFCTF.
       COPY CCFCTFC.
       FD  CCCALF.
       COPY CCCALFC.
       FD  CCERRF.
       COPY CCERRC.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05 WS-ST-CCVALF             PIC XX VALUE SPACES.
           05 WS-ST-CCFCTF             PIC XX VALUE SPACES.
           05 WS-ST-CCCALF             PIC XX VALUE SPACES.
           05 WS-ST-CCERRF             PIC XX VALUE SPACES.

       01  WS-SWITCHES.
           05 WS-EOF-CCVALF            PIC X VALUE 'N'.
           05 WS-EOF-CCFCTF            PIC X VALUE 'N'.
           05 WS-EOF-CCCALF            PIC X VALUE 'N'.
           05 WS-ABEND-SW              PIC X VALUE 'N'.
           05 WS-FCT-FOUND-SW          PIC X VALUE 'N'.
           05 WS-CAL-FOUND-SW          PIC X VALUE 'N'.

       01  WS-COUNTERS.
           05 WS-VAL-CNT               PIC 9(9) VALUE ZERO.
           05 WS-FCT-CNT               PIC 9(9) VALUE ZERO.
           05 WS-CAL-CNT               PIC 9(9) VALUE ZERO.
           05 WS-ERR-CNT               PIC 9(9) VALUE ZERO.
           05 WS-FCT-SUB               PIC 9(5) VALUE ZERO.
           05 WS-CAL-SUB               PIC 9(5) VALUE ZERO.
           05 WS-ERR-SEQ               PIC 9(9) VALUE ZERO.

       01  WS-CONSTANTS.
           05 CN-PGM-ID                PIC X(08) VALUE 'CC270B'.
           05 CN-FCT-KAKUTEI           PIC X(02) VALUE '01'.
           05 CN-FCT-HORYU             PIC X(02) VALUE '08'.
           05 CN-FCT-TORIKESHI         PIC X(02) VALUE '09'.
           05 CN-CAL-BUSINESS          PIC X     VALUE 'N'.
           05 CN-VL-RESULT-OK          PIC X(02) VALUE '00'.
           05 CN-ERR-BIZDAY            PIC X(03) VALUE 'C01'.
           05 CN-ERR-NOCAL             PIC X(03) VALUE 'C02'.
           05 CN-ERR-NOFCT             PIC X(03) VALUE 'F01'.
           05 CN-ERR-FCTHOLD           PIC X(03) VALUE 'F08'.
           05 CN-ERR-FCTCANCEL         PIC X(03) VALUE 'F09'.
           05 CN-ERR-FCTSTS            PIC X(03) VALUE 'F99'.
           05 CN-ERR-VLSTS             PIC X(03) VALUE 'V99'.

       01  WS-WORK.
           05 WS-BASE-DT               PIC 9(08) VALUE ZERO.
           05 WS-RECORD-KEY            PIC X(40) VALUE SPACES.
           05 WS-ERROR-KBN             PIC X(03) VALUE SPACES.
           05 WS-ERROR-TEXT            PIC X(80) VALUE SPACES.
           05 WS-ERROR-ID-WK           PIC 9(12) VALUE ZERO.
           05 WS-DISP-CNT              PIC Z(9)9.
           05 WS-DISP-ERR              PIC Z(9)9.

       01  WS-FCT-TABLE.
           05 WS-FCT-ENTRY OCCURS 20000 TIMES.
              10 TB-FCT-ID             PIC X(20).
              10 TB-FCT-TRIGGER-DT     PIC 9(08).
              10 TB-FCT-STATUS-KBN     PIC X(02).

       01  WS-CAL-TABLE.
           05 WS-CAL-ENTRY OCCURS 40000 TIMES.
              10 TB-CAL-DT             PIC 9(08).
              10 TB-CAL-HOLIDAY-FLAG   PIC X.

       PROCEDURE DIVISION.
       MAIN-RTN.
           MOVE 0 TO RETURN-CODE
           PERFORM 1000-INIT
           IF WS-ABEND-SW = 'N'
              PERFORM 2000-MAIN
           END-IF
           PERFORM 9000-END
           GOBACK.

       1000-INIT.
           DISPLAY 'CC270B START'
           OPEN INPUT CCFCTF
           IF WS-ST-CCFCTF NOT = '00'
              DISPLAY 'CCFCTF OPEN ERROR ST=' WS-ST-CCFCTF
              MOVE 12 TO RETURN-CODE
              MOVE 'Y' TO WS-ABEND-SW
              EXIT PARAGRAPH
           END-IF

           OPEN INPUT CCCALF
           IF WS-ST-CCCALF NOT = '00'
              DISPLAY 'CCCALF OPEN ERROR ST=' WS-ST-CCCALF
              MOVE 12 TO RETURN-CODE
              MOVE 'Y' TO WS-ABEND-SW
              CLOSE CCFCTF
              EXIT PARAGRAPH
           END-IF

           OPEN INPUT CCVALF
           IF WS-ST-CCVALF NOT = '00'
              DISPLAY 'CCVALF OPEN ERROR ST=' WS-ST-CCVALF
              MOVE 12 TO RETURN-CODE
              MOVE 'Y' TO WS-ABEND-SW
              CLOSE CCFCTF CCCALF
              EXIT PARAGRAPH
           END-IF

           OPEN OUTPUT CCERRF
           IF WS-ST-CCERRF NOT = '00'
              DISPLAY 'CCERRF OPEN ERROR ST=' WS-ST-CCERRF
              MOVE 12 TO RETURN-CODE
              MOVE 'Y' TO WS-ABEND-SW
              CLOSE CCFCTF CCCALF CCVALF
              EXIT PARAGRAPH
           END-IF

           PERFORM 1100-LOAD-FCTF
           IF WS-ABEND-SW = 'N'
              PERFORM 1200-LOAD-CALF
           END-IF.

       1100-LOAD-FCTF.
           PERFORM UNTIL WS-EOF-CCFCTF = 'Y'
              READ CCFCTF
                 AT END
                    MOVE 'Y' TO WS-EOF-CCFCTF
                 NOT AT END
                    IF WS-ST-CCFCTF = '00'
                       ADD 1 TO WS-FCT-CNT
                       IF WS-FCT-CNT > 20000
                          DISPLAY 'CCFCTF COUNT OVER'
                          MOVE 12 TO RETURN-CODE
                          MOVE 'Y' TO WS-ABEND-SW
                          EXIT PERFORM
                       END-IF
                       MOVE FC-FCT-ID
                         TO TB-FCT-ID(WS-FCT-CNT)
                       MOVE FC-TRIGGER-DT
                         TO TB-FCT-TRIGGER-DT(WS-FCT-CNT)
                       MOVE FC-FCT-STATUS-KBN
                         TO TB-FCT-STATUS-KBN(WS-FCT-CNT)
                    ELSE
                       DISPLAY 'CCFCTF READ ERROR ST=' WS-ST-CCFCTF
                       MOVE 12 TO RETURN-CODE
                       MOVE 'Y' TO WS-ABEND-SW
                       EXIT PERFORM
                    END-IF
              END-READ
           END-PERFORM.

       1200-LOAD-CALF.
           PERFORM UNTIL WS-EOF-CCCALF = 'Y'
              READ CCCALF
                 AT END
                    MOVE 'Y' TO WS-EOF-CCCALF
                 NOT AT END
                    IF WS-ST-CCCALF = '00'
                       ADD 1 TO WS-CAL-CNT
                       IF WS-CAL-CNT > 40000
                          DISPLAY 'CCCALF COUNT OVER'
                          MOVE 12 TO RETURN-CODE
                          MOVE 'Y' TO WS-ABEND-SW
                          EXIT PERFORM
                       END-IF
                       MOVE CL-CAL-DT
                         TO TB-CAL-DT(WS-CAL-CNT)
                       MOVE CL-HOLIDAY-FLAG
                         TO TB-CAL-HOLIDAY-FLAG(WS-CAL-CNT)
                    ELSE
                       DISPLAY 'CCCALF READ ERROR ST=' WS-ST-CCCALF
                       MOVE 12 TO RETURN-CODE
                       MOVE 'Y' TO WS-ABEND-SW
                       EXIT PERFORM
                    END-IF
              END-READ
           END-PERFORM.

       2000-MAIN.
           PERFORM UNTIL WS-EOF-CCVALF = 'Y'
              READ CCVALF
                 AT END
                    MOVE 'Y' TO WS-EOF-CCVALF
                 NOT AT END
                    IF WS-ST-CCVALF = '00'
                       ADD 1 TO WS-VAL-CNT
                       PERFORM 2100-AUDIT-VAL
                    ELSE
                       DISPLAY 'CCVALF READ ERROR ST=' WS-ST-CCVALF
                       MOVE 12 TO RETURN-CODE
                       MOVE 'Y' TO WS-ABEND-SW
                       EXIT PERFORM
                    END-IF
              END-READ
           END-PERFORM.

       2100-AUDIT-VAL.
           MOVE SPACES TO WS-RECORD-KEY
           STRING VL-VAL-ID DELIMITED BY SIZE
                  '/'       DELIMITED BY SIZE
                  VL-FCT-ID DELIMITED BY SIZE
             INTO WS-RECORD-KEY
           END-STRING

           PERFORM 2200-FIND-CALENDAR
           IF WS-CAL-FOUND-SW = 'N'
              MOVE VL-VALUE-DT  TO WS-BASE-DT
              MOVE CN-ERR-NOCAL TO WS-ERROR-KBN
              MOVE 'VALUE DATE NOT IN CALENDAR'
                TO WS-ERROR-TEXT
              PERFORM 8000-WRITE-ERROR
           ELSE
              IF TB-CAL-HOLIDAY-FLAG(WS-CAL-SUB)
                 NOT = CN-CAL-BUSINESS
                 MOVE VL-VALUE-DT     TO WS-BASE-DT
                 MOVE CN-ERR-BIZDAY   TO WS-ERROR-KBN
                 MOVE 'VALUE DATE IS NOT BUSINESS DAY'
                   TO WS-ERROR-TEXT
                 PERFORM 8000-WRITE-ERROR
              END-IF
           END-IF

           PERFORM 2300-FIND-FCT
           IF WS-FCT-FOUND-SW = 'N'
              MOVE VL-VALUE-DT  TO WS-BASE-DT
              MOVE CN-ERR-NOFCT TO WS-ERROR-KBN
              MOVE 'FCT NOT FOUND'
                TO WS-ERROR-TEXT
              PERFORM 8000-WRITE-ERROR
           ELSE
              EVALUATE TB-FCT-STATUS-KBN(WS-FCT-SUB)
                 WHEN CN-FCT-KAKUTEI
                    IF VL-VAL-STATUS-KBN NOT = CN-VL-RESULT-OK
                       MOVE VL-VALUE-DT TO WS-BASE-DT
                       MOVE CN-ERR-VLSTS TO WS-ERROR-KBN
                       MOVE 'CONFIRMED FCT RESULT STATUS ERROR'
                         TO WS-ERROR-TEXT
                       PERFORM 8000-WRITE-ERROR
                    END-IF
                 WHEN CN-FCT-HORYU
                    IF VL-VAL-STATUS-KBN = CN-VL-RESULT-OK
                       MOVE TB-FCT-TRIGGER-DT(WS-FCT-SUB)
                         TO WS-BASE-DT
                       MOVE CN-ERR-FCTHOLD TO WS-ERROR-KBN
                       MOVE 'HELD FCT HAS NORMAL RESULT'
                         TO WS-ERROR-TEXT
                       PERFORM 8000-WRITE-ERROR
                    END-IF
                 WHEN CN-FCT-TORIKESHI
                    IF VL-VAL-STATUS-KBN = CN-VL-RESULT-OK
                       MOVE TB-FCT-TRIGGER-DT(WS-FCT-SUB)
                         TO WS-BASE-DT
                       MOVE CN-ERR-FCTCANCEL TO WS-ERROR-KBN
                       MOVE 'CANCELLED FCT HAS NORMAL RESULT'
                         TO WS-ERROR-TEXT
                       PERFORM 8000-WRITE-ERROR
                    END-IF
                 WHEN OTHER
                    MOVE TB-FCT-TRIGGER-DT(WS-FCT-SUB)
                      TO WS-BASE-DT
                    MOVE CN-ERR-FCTSTS TO WS-ERROR-KBN
                    MOVE 'FCT STATUS ERROR'
                      TO WS-ERROR-TEXT
                    PERFORM 8000-WRITE-ERROR
              END-EVALUATE
           END-IF.

       2200-FIND-CALENDAR.
           MOVE 'N' TO WS-CAL-FOUND-SW
           MOVE 1 TO WS-CAL-SUB
           PERFORM UNTIL WS-CAL-SUB > WS-CAL-CNT
              OR WS-CAL-FOUND-SW = 'Y'
              IF TB-CAL-DT(WS-CAL-SUB) = VL-VALUE-DT
                 MOVE 'Y' TO WS-CAL-FOUND-SW
              ELSE
                 ADD 1 TO WS-CAL-SUB
              END-IF
           END-PERFORM.

       2300-FIND-FCT.
           MOVE 'N' TO WS-FCT-FOUND-SW
           MOVE 1 TO WS-FCT-SUB
           PERFORM UNTIL WS-FCT-SUB > WS-FCT-CNT
              OR WS-FCT-FOUND-SW = 'Y'
              IF TB-FCT-ID(WS-FCT-SUB) = VL-FCT-ID
                 MOVE 'Y' TO WS-FCT-FOUND-SW
              ELSE
                 ADD 1 TO WS-FCT-SUB
              END-IF
           END-PERFORM.

       8000-WRITE-ERROR.
           ADD 1 TO WS-ERR-CNT
           ADD 1 TO WS-ERR-SEQ
           MOVE WS-ERR-SEQ TO WS-ERROR-ID-WK

           MOVE SPACES TO CCERRF-REC
           MOVE WS-ERROR-ID-WK TO ER-ERROR-ID
           MOVE CN-PGM-ID      TO ER-PGM-ID
           MOVE WS-BASE-DT     TO ER-BASE-DT
           MOVE WS-RECORD-KEY  TO ER-RECORD-KEY
           MOVE WS-ERROR-KBN   TO ER-ERROR-KBN
           MOVE WS-ERROR-TEXT  TO ER-ERROR-TEXT

           WRITE CCERRF-REC
           IF WS-ST-CCERRF NOT = '00'
              DISPLAY 'CCERRF WRITE ERROR ST=' WS-ST-CCERRF
              MOVE 12 TO RETURN-CODE
              MOVE 'Y' TO WS-ABEND-SW
           END-IF.

       9000-END.
           IF WS-ST-CCVALF NOT = SPACES
              CLOSE CCVALF
           END-IF
           IF WS-ST-CCFCTF NOT = SPACES
              CLOSE CCFCTF
           END-IF
           IF WS-ST-CCCALF NOT = SPACES
              CLOSE CCCALF
           END-IF
           IF WS-ST-CCERRF NOT = SPACES
              CLOSE CCERRF
           END-IF

           MOVE WS-VAL-CNT TO WS-DISP-CNT
           MOVE WS-ERR-CNT TO WS-DISP-ERR
           DISPLAY 'CC270B INPUT=' WS-DISP-CNT
                   ' ERROR=' WS-DISP-ERR

           IF WS-ABEND-SW = 'Y'
              IF RETURN-CODE = 0
                 MOVE 12 TO RETURN-CODE
              END-IF
              DISPLAY 'CC270B ABEND RC=' RETURN-CODE
           ELSE
              MOVE 0 TO RETURN-CODE
              DISPLAY 'CC270B NORMAL END'
           END-IF.
