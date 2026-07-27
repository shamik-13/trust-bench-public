       IDENTIFICATION DIVISION.
       PROGRAM-ID. KZ130B.
      * 変更履歴
      * 版数  年月日(和暦)  担当                      概要
      * 1.00  令和03.04.01  システム部 勘定系チーム  新規作成
      * 1.01  令和04.10.01  システム部 勘定系チーム  休日判定処理見直し
      * 1.02  令和06.04.01  システム部 勘定系チーム  会計期末日設定追加
       AUTHOR. KZ-BATCH.
       INSTALLATION. みらい信託銀行.
       DATE-WRITTEN. 20250310.
       DATE-COMPILED.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZCALF ASSIGN TO "KZCALF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CA-CAL-DT
               FILE STATUS IS FS-KZCALF.

           SELECT KZCYCF ASSIGN TO "KZCYCF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS FS-KZCYCF.

           SELECT KZFPRF ASSIGN TO "KZFPRF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS FP-FISCAL-PERIOD-ID
               FILE STATUS IS FS-KZFPRF.

       DATA DIVISION.
       FILE SECTION.

       FD  KZCALF.
       COPY KZCALFC.

       FD  KZCYCF.
       COPY KZCYCFC.

       FD  KZFPRF.
       COPY KZFPRFC.

       WORKING-STORAGE SECTION.
       COPY LK-CAL-PARM.

       01  FILE-STATUS-AREA.
           05 FS-KZCALF                 PIC XX VALUE SPACES.
           05 FS-KZCYCF                 PIC XX VALUE SPACES.
           05 FS-KZFPRF                 PIC XX VALUE SPACES.

       01  SWITCH-AREA.
           05 SW-END-CYCLE              PIC X VALUE 'N'.
              88 END-CYCLE                    VALUE 'Y'.
              88 NOT-END-CYCLE                VALUE 'N'.
           05 SW-HARD-ERROR             PIC X VALUE 'N'.
              88 HARD-ERROR                   VALUE 'Y'.
              88 NOT-HARD-ERROR               VALUE 'N'.
           05 SW-REJECT-PERIOD          PIC X VALUE 'N'.
              88 REJECT-PERIOD                VALUE 'Y'.
              88 ACCEPT-PERIOD                VALUE 'N'.
           05 SW-FOUND-EXIST            PIC X VALUE 'N'.
              88 FOUND-EXIST                  VALUE 'Y'.
              88 NOT-FOUND-EXIST              VALUE 'N'.

       01  WORK-DATE-AREA.
           05 WK-FISCAL-START-DT        PIC 9(8) VALUE ZERO.
           05 WK-CYCLE-NOMINAL-DT       PIC 9(8) VALUE ZERO.
           05 WK-START-DT               PIC 9(8) VALUE ZERO.
           05 WK-END-DT                 PIC 9(8) VALUE ZERO.
           05 WK-RESOLVED-DT            PIC 9(8) VALUE ZERO.
           05 WK-DATE-INT               PIC S9(9) COMP VALUE ZERO.
           05 WK-DATE-INT2              PIC S9(9) COMP VALUE ZERO.
           05 WK-YEAR                   PIC 9(4) VALUE ZERO.
           05 WK-MONTH                  PIC 9(2) VALUE ZERO.
           05 WK-DAY                    PIC 9(2) VALUE ZERO.
           05 WK-QTR                    PIC 9 VALUE ZERO.
           05 WK-FISCAL-YEAR            PIC 9(4) VALUE ZERO.
           05 WK-PRIOR-END-DT           PIC 9(8) VALUE ZERO.
           05 WK-NEXT-START-DT          PIC 9(8) VALUE ZERO.

       01  WORK-ID-AREA.
           05 WK-PERIOD-TYPE            PIC X VALUE SPACE.
           05 WK-PERIOD-ID              PIC X(20) VALUE SPACES.
           05 WK-ID-YYYY                PIC 9(4) VALUE ZERO.
           05 WK-ID-MM                  PIC 9(2) VALUE ZERO.
           05 WK-ID-Q                   PIC 9 VALUE ZERO.

       01  COUNTER-AREA.
           05 CT-CYCLE-READ             PIC 9(9) VALUE ZERO.
           05 CT-PERIOD-WRITE           PIC 9(9) VALUE ZERO.
           05 CT-PERIOD-REJECT          PIC 9(9) VALUE ZERO.
           05 CT-CAL-ERR                PIC 9(9) VALUE ZERO.

       01  CONSTANT-AREA.
           05 CN-CLOSED                 PIC X VALUE 'C'.
           05 CN-OPEN                   PIC X VALUE '0'.
           05 CN-MONTH-TYPE             PIC X VALUE 'M'.
           05 CN-QTR-TYPE               PIC X VALUE 'Q'.
           05 CN-YEAR-TYPE              PIC X VALUE 'Y'.

       PROCEDURE DIVISION.
       MAIN-SECTION.
           PERFORM 1000-INIT
           IF NOT HARD-ERROR
               PERFORM 2000-MAIN
           END-IF
           PERFORM 9000-END
           GOBACK
           .

       1000-INIT.
           MOVE 0 TO RETURN-CODE
           SET NOT-HARD-ERROR TO TRUE
           SET NOT-END-CYCLE TO TRUE

           OPEN INPUT KZCALF
           IF FS-KZCALF NOT = '00'
               DISPLAY 'KZCALF OPEN ERR ST=' FS-KZCALF
               MOVE 12 TO RETURN-CODE
               SET HARD-ERROR TO TRUE
           END-IF

           IF NOT HARD-ERROR
               OPEN INPUT KZCYCF
               IF FS-KZCYCF NOT = '00'
                   DISPLAY 'KZCYCF OPEN ERR ST=' FS-KZCYCF
                   MOVE 12 TO RETURN-CODE
                   SET HARD-ERROR TO TRUE
               END-IF
           END-IF

           IF NOT HARD-ERROR
               OPEN I-O KZFPRF
               IF FS-KZFPRF NOT = '00'
                   DISPLAY 'KZFPRF OPEN ERR ST=' FS-KZFPRF
                   MOVE 12 TO RETURN-CODE
                   SET HARD-ERROR TO TRUE
               END-IF
           END-IF
           .

       2000-MAIN.
           PERFORM 2100-READ-CYCLE
           IF END-CYCLE
               DISPLAY 'KZCYCF NO INPUT'
               MOVE 8 TO RETURN-CODE
               SET HARD-ERROR TO TRUE
           ELSE
               MOVE CY-NOMINAL-DT TO WK-FISCAL-START-DT
           END-IF

           PERFORM UNTIL END-CYCLE OR HARD-ERROR
               ADD 1 TO CT-CYCLE-READ
               MOVE CY-NOMINAL-DT TO WK-CYCLE-NOMINAL-DT
               PERFORM 3000-VALIDATE-CYCLE
               IF NOT HARD-ERROR
                   MOVE CN-MONTH-TYPE TO WK-PERIOD-TYPE
                   PERFORM 4000-BUILD-MONTH
                   IF NOT HARD-ERROR
                       PERFORM 5000-REGISTER-PERIOD
                   END-IF
                   MOVE CN-QTR-TYPE TO WK-PERIOD-TYPE
                   PERFORM 4100-BUILD-QUARTER
                   IF NOT HARD-ERROR
                       PERFORM 5000-REGISTER-PERIOD
                   END-IF
                   MOVE CN-YEAR-TYPE TO WK-PERIOD-TYPE
                   PERFORM 4200-BUILD-YEAR
                   IF NOT HARD-ERROR
                       PERFORM 5000-REGISTER-PERIOD
                   END-IF
               END-IF
               PERFORM 2100-READ-CYCLE
           END-PERFORM
           .

       2100-READ-CYCLE.
           READ KZCYCF
               AT END
                   SET END-CYCLE TO TRUE
               NOT AT END
                   IF FS-KZCYCF NOT = '00'
                       DISPLAY 'KZCYCF READ ERR ST=' FS-KZCYCF
                       MOVE 12 TO RETURN-CODE
                       SET HARD-ERROR TO TRUE
                   END-IF
           END-READ
           .

       3000-VALIDATE-CYCLE.
           IF WK-CYCLE-NOMINAL-DT < WK-FISCAL-START-DT
               DISPLAY 'CYCLE BEFORE START ID=' CY-CYCLE-ID
               MOVE 8 TO RETURN-CODE
               SET HARD-ERROR TO TRUE
           END-IF

           IF NOT HARD-ERROR
               PERFORM 4300-SPLIT-CYCLE-DATE
           END-IF

           IF NOT HARD-ERROR
               MOVE WK-CYCLE-NOMINAL-DT TO CA-CAL-DT
               READ KZCALF KEY IS CA-CAL-DT
                   INVALID KEY
                       DISPLAY 'CALENDAR MISSING DT='
                       DISPLAY WK-CYCLE-NOMINAL-DT
                       ADD 1 TO CT-CAL-ERR
                       MOVE 8 TO RETURN-CODE
                       SET HARD-ERROR TO TRUE
                   NOT INVALID KEY
                       IF CA-HOLIDAY-FLAG NOT = 'Y'
                          AND CA-HOLIDAY-FLAG NOT = 'N'
                           DISPLAY 'HOLIDAY FLAG ERR DT=' CA-CAL-DT
                           MOVE 8 TO RETURN-CODE
                           SET HARD-ERROR TO TRUE
                       END-IF
               END-READ
           END-IF
           .

       4000-BUILD-MONTH.
           PERFORM 4300-SPLIT-CYCLE-DATE
           IF NOT HARD-ERROR
               COMPUTE WK-START-DT =
                   WK-YEAR * 10000 + WK-MONTH * 100 + 1
               IF WK-MONTH = 12
                   COMPUTE WK-END-DT =
                       (WK-YEAR + 1) * 10000 + 101
               ELSE
                   COMPUTE WK-END-DT =
                       WK-YEAR * 10000 + (WK-MONTH + 1) * 100 + 1
               END-IF
               COMPUTE WK-DATE-INT2 =
                   FUNCTION INTEGER-OF-DATE(WK-END-DT) - 1
               COMPUTE WK-END-DT =
                   FUNCTION DATE-OF-INTEGER(WK-DATE-INT2)
               MOVE WK-YEAR TO WK-ID-YYYY
               MOVE WK-MONTH TO WK-ID-MM
               MOVE SPACES TO WK-PERIOD-ID
               STRING 'M' WK-ID-YYYY WK-ID-MM
                   DELIMITED BY SIZE INTO WK-PERIOD-ID
               END-STRING
           END-IF
           .

       4100-BUILD-QUARTER.
           PERFORM 4300-SPLIT-CYCLE-DATE
           IF NOT HARD-ERROR
               EVALUATE WK-MONTH
                   WHEN 1 THRU 3
                       MOVE 1 TO WK-QTR
                       MOVE 1 TO WK-MONTH
                   WHEN 4 THRU 6
                       MOVE 2 TO WK-QTR
                       MOVE 4 TO WK-MONTH
                   WHEN 7 THRU 9
                       MOVE 3 TO WK-QTR
                       MOVE 7 TO WK-MONTH
                   WHEN OTHER
                       MOVE 4 TO WK-QTR
                       MOVE 10 TO WK-MONTH
               END-EVALUATE
               COMPUTE WK-START-DT =
                   WK-YEAR * 10000 + WK-MONTH * 100 + 1
               IF WK-QTR = 4
                   COMPUTE WK-END-DT =
                       (WK-YEAR + 1) * 10000 + 101
               ELSE
                   COMPUTE WK-END-DT =
                       WK-YEAR * 10000 + (WK-MONTH + 3) * 100 + 1
               END-IF
               COMPUTE WK-DATE-INT2 =
                   FUNCTION INTEGER-OF-DATE(WK-END-DT) - 1
               COMPUTE WK-END-DT =
                   FUNCTION DATE-OF-INTEGER(WK-DATE-INT2)
               MOVE WK-YEAR TO WK-ID-YYYY
               MOVE WK-QTR TO WK-ID-Q
               MOVE SPACES TO WK-PERIOD-ID
               STRING 'Q' WK-ID-YYYY WK-ID-Q
                   DELIMITED BY SIZE INTO WK-PERIOD-ID
               END-STRING
           END-IF
           .

       4200-BUILD-YEAR.
           PERFORM 4300-SPLIT-CYCLE-DATE
           IF NOT HARD-ERROR
               MOVE WK-YEAR TO WK-FISCAL-YEAR
               COMPUTE WK-START-DT = WK-FISCAL-YEAR * 10000 + 101
               COMPUTE WK-END-DT =
                   (WK-FISCAL-YEAR + 1) * 10000 + 101
               COMPUTE WK-DATE-INT2 =
                   FUNCTION INTEGER-OF-DATE(WK-END-DT) - 1
               COMPUTE WK-END-DT =
                   FUNCTION DATE-OF-INTEGER(WK-DATE-INT2)
               MOVE WK-FISCAL-YEAR TO WK-ID-YYYY
               MOVE SPACES TO WK-PERIOD-ID
               STRING 'Y' WK-ID-YYYY
                   DELIMITED BY SIZE INTO WK-PERIOD-ID
               END-STRING
           END-IF
           .

       4300-SPLIT-CYCLE-DATE.
           COMPUTE WK-YEAR = WK-CYCLE-NOMINAL-DT / 10000
           COMPUTE WK-MONTH =
               FUNCTION MOD(WK-CYCLE-NOMINAL-DT / 100, 100)
           COMPUTE WK-DAY =
               FUNCTION MOD(WK-CYCLE-NOMINAL-DT, 100)

           IF WK-MONTH < 1 OR WK-MONTH > 12
               DISPLAY 'CYCLE MONTH ERR ID=' CY-CYCLE-ID
               MOVE 8 TO RETURN-CODE
               SET HARD-ERROR TO TRUE
           END-IF

           IF WK-DAY < 1 OR WK-DAY > 31
               DISPLAY 'CYCLE DAY ERR ID=' CY-CYCLE-ID
               MOVE 8 TO RETURN-CODE
               SET HARD-ERROR TO TRUE
           END-IF

           IF NOT HARD-ERROR
               COMPUTE WK-DATE-INT =
                   FUNCTION INTEGER-OF-DATE(WK-CYCLE-NOMINAL-DT)
               IF WK-DATE-INT = 0
                   DISPLAY 'CYCLE DATE ERR ID=' CY-CYCLE-ID
                   MOVE 8 TO RETURN-CODE
                   SET HARD-ERROR TO TRUE
               END-IF
           END-IF
           .

       5000-REGISTER-PERIOD.
           SET ACCEPT-PERIOD TO TRUE
           SET NOT-FOUND-EXIST TO TRUE
           MOVE WK-PERIOD-ID TO FP-FISCAL-PERIOD-ID

           READ KZFPRF KEY IS FP-FISCAL-PERIOD-ID
               INVALID KEY
                   SET NOT-FOUND-EXIST TO TRUE
               NOT INVALID KEY
                   SET FOUND-EXIST TO TRUE
           END-READ

           IF FOUND-EXIST
               IF FP-CLOSE-STATUS = CN-CLOSED
                   DISPLAY 'CLOSED PERIOD REJECT ID='
                   DISPLAY FP-FISCAL-PERIOD-ID
                   SET REJECT-PERIOD TO TRUE
                   ADD 1 TO CT-PERIOD-REJECT
               ELSE
                   DISPLAY 'DUP PERIOD REJECT ID='
                   DISPLAY FP-FISCAL-PERIOD-ID
                   SET REJECT-PERIOD TO TRUE
                   ADD 1 TO CT-PERIOD-REJECT
               END-IF
           END-IF

           IF ACCEPT-PERIOD
               PERFORM 5100-CHECK-GAP
           END-IF

           IF ACCEPT-PERIOD
               PERFORM 5200-RESOLVE-BOUNDARY
           END-IF

           IF ACCEPT-PERIOD AND NOT HARD-ERROR
               MOVE WK-PERIOD-ID TO FP-FISCAL-PERIOD-ID
               MOVE WK-START-DT TO FP-PERIOD-START-DT
               MOVE WK-END-DT TO FP-PERIOD-END-DT
               MOVE CN-OPEN TO FP-CLOSE-STATUS
               MOVE CY-CYCLE-ID TO FP-BASE-CYCLE-ID
               WRITE KZFPRF-REC
                   INVALID KEY
                       DISPLAY 'KZFPRF DUP WRITE ID='
                       DISPLAY FP-FISCAL-PERIOD-ID
                       SET REJECT-PERIOD TO TRUE
                       ADD 1 TO CT-PERIOD-REJECT
                   NOT INVALID KEY
                       ADD 1 TO CT-PERIOD-WRITE
               END-WRITE
               IF FS-KZFPRF NOT = '00' AND FS-KZFPRF NOT = '22'
                   DISPLAY 'KZFPRF WRITE ERR ST=' FS-KZFPRF
                   MOVE 12 TO RETURN-CODE
                   SET HARD-ERROR TO TRUE
               END-IF
           END-IF
           .

       5100-CHECK-GAP.
           MOVE ZERO TO WK-PRIOR-END-DT
           MOVE ZERO TO WK-NEXT-START-DT

           IF WK-PERIOD-TYPE = CN-MONTH-TYPE
               COMPUTE WK-DATE-INT =
                   FUNCTION INTEGER-OF-DATE(WK-START-DT) - 1
               COMPUTE WK-PRIOR-END-DT =
                   FUNCTION DATE-OF-INTEGER(WK-DATE-INT)
               IF WK-START-DT > WK-FISCAL-START-DT
                  AND CT-PERIOD-WRITE = 0
                   DISPLAY 'PERIOD GAP DETECTED ID=' WK-PERIOD-ID
                   SET REJECT-PERIOD TO TRUE
                   ADD 1 TO CT-PERIOD-REJECT
               END-IF
           END-IF
           .

       5200-RESOLVE-BOUNDARY.
           MOVE WK-START-DT TO LK-CAL-NOMINAL-DT
           MOVE ZERO TO LK-CAL-RESOLVED-DT
           MOVE SPACE TO LK-CAL-ROLLED-FLAG
           MOVE ZERO TO LK-CAL-RET
           CALL 'KZ900S' USING LK-CAL-PARM

           IF LK-CAL-RET NOT = 0
               DISPLAY 'KZ900S START ERR DT=' WK-START-DT
               MOVE 12 TO RETURN-CODE
               SET HARD-ERROR TO TRUE
           ELSE
               MOVE LK-CAL-RESOLVED-DT TO WK-RESOLVED-DT
               IF LK-CAL-ROLLED-FLAG = 'Y'
                   DISPLAY 'START DATE ROLLED ID=' WK-PERIOD-ID
               END-IF
           END-IF

           IF NOT HARD-ERROR
               MOVE WK-END-DT TO LK-CAL-NOMINAL-DT
               MOVE ZERO TO LK-CAL-RESOLVED-DT
               MOVE SPACE TO LK-CAL-ROLLED-FLAG
               MOVE ZERO TO LK-CAL-RET
               CALL 'KZ900S' USING LK-CAL-PARM
               IF LK-CAL-RET NOT = 0
                   DISPLAY 'KZ900S END ERR DT=' WK-END-DT
                   MOVE 12 TO RETURN-CODE
                   SET HARD-ERROR TO TRUE
               ELSE
                   IF LK-CAL-ROLLED-FLAG = 'Y'
                       DISPLAY 'END DATE ROLLED ID=' WK-PERIOD-ID
                   END-IF
               END-IF
           END-IF
           .

       9000-END.
           IF FS-KZCALF = '00'
               CLOSE KZCALF
               IF FS-KZCALF NOT = '00'
                   DISPLAY 'KZCALF CLOSE ERR ST=' FS-KZCALF
                   MOVE 8 TO RETURN-CODE
               END-IF
           END-IF

           IF FS-KZCYCF = '00'
               CLOSE KZCYCF
               IF FS-KZCYCF NOT = '00'
                   DISPLAY 'KZCYCF CLOSE ERR ST=' FS-KZCYCF
                   MOVE 8 TO RETURN-CODE
               END-IF
           END-IF

           IF FS-KZFPRF = '00'
               CLOSE KZFPRF
               IF FS-KZFPRF NOT = '00'
                   DISPLAY 'KZFPRF CLOSE ERR ST=' FS-KZFPRF
                   MOVE 8 TO RETURN-CODE
               END-IF
           END-IF

           DISPLAY 'KZ130B READ COUNT=' CT-CYCLE-READ
           DISPLAY 'KZ130B WRITE COUNT=' CT-PERIOD-WRITE
           DISPLAY 'KZ130B REJECT COUNT=' CT-PERIOD-REJECT
           DISPLAY 'KZ130B CAL ERR COUNT=' CT-CAL-ERR
           .
