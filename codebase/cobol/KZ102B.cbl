       IDENTIFICATION DIVISION.
       PROGRAM-ID. KZ102B.
      * 変更履歴
      * 版数  年月日(和暦)   担当                       概要
      * 1.00  令和03年04月01日 システム部 勘定系チーム  新規作成
      * 1.01  令和04年10月17日 システム部 勘定系チーム  祝日判定追加
      * 1.02  令和06年03月25日 システム部 勘定系チーム  休日暦不整合検知強化
      ******************************************************************
      * KZHOLF休日暦とKZCALF営業日属性の整合検査を行う。
      * 確定可能な未反映休日のみKZCALFへ反映する。
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZHOLF ASSIGN TO "KZHOLF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS HO-HOLIDAY-DT
               FILE STATUS IS WS-KZHOLF-ST.
           SELECT KZCALF ASSIGN TO "KZCALF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CA-CAL-DT
               FILE STATUS IS WS-KZCALF-ST.

       DATA DIVISION.
       FILE SECTION.
       FD  KZHOLF.
           COPY KZHOLFC.
       FD  KZCALF.
           COPY KZCALFC.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05 WS-KZHOLF-ST              PIC XX VALUE SPACE.
           05 WS-KZCALF-ST              PIC XX VALUE SPACE.

       01  WS-SWITCHES.
           05 WS-EOF-SW                 PIC X VALUE 'N'.
              88 KZHOLF-END                  VALUE 'Y'.
              88 KZHOLF-NOT-END              VALUE 'N'.
           05 WS-ABEND-SW               PIC X VALUE 'N'.
              88 ABEND-FOUND                 VALUE 'Y'.
              88 ABEND-NOT-FOUND             VALUE 'N'.

       01  WS-WORK.
           05 WS-PREV-HOLIDAY-DT        PIC 9(08) VALUE ZERO.
           05 WS-DATE-X                 PIC X(08) VALUE SPACE.
           05 WS-YYYY                   PIC 9(04) VALUE ZERO.
           05 WS-MM                     PIC 9(02) VALUE ZERO.
           05 WS-DD                     PIC 9(02) VALUE ZERO.
           05 WS-MAX-DD                 PIC 9(02) VALUE ZERO.
           05 WS-LEAP-SW                PIC X VALUE 'N'.
              88 LEAP-YEAR                   VALUE 'Y'.
              88 NOT-LEAP-YEAR               VALUE 'N'.
           05 WS-DATE-VALID-SW          PIC X VALUE 'N'.
              88 DATE-VALID                  VALUE 'Y'.
              88 DATE-INVALID                VALUE 'N'.
           05 WS-UPDATE-SW              PIC X VALUE 'N'.
              88 UPDATE-NEEDED               VALUE 'Y'.
              88 UPDATE-NOT-NEEDED           VALUE 'N'.

       01  WS-COUNTERS.
           05 WS-HOL-READ-CNT           PIC 9(09) VALUE ZERO.
           05 WS-CAL-READ-CNT           PIC 9(09) VALUE ZERO.
           05 WS-CAL-UPD-CNT            PIC 9(09) VALUE ZERO.
           05 WS-MISSING-CNT            PIC 9(09) VALUE ZERO.
           05 WS-DUP-CNT                PIC 9(09) VALUE ZERO.
           05 WS-INVALID-CNT            PIC 9(09) VALUE ZERO.
           05 WS-RESIDUAL-CNT           PIC 9(09) VALUE ZERO.
           05 WS-NOACT-CNT              PIC 9(09) VALUE ZERO.
           05 WS-CALL-ERR-CNT           PIC 9(09) VALUE ZERO.

       01  WS-DISPLAY-COUNTERS.
           05 WS-DISP-NUM               PIC Z(08)9.

       01  WS-MESSAGES.
           05 WS-MSG-PREFIX             PIC X(08) VALUE 'KZ115B '.

       COPY LK-CAL-PARM.

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE 0 TO RETURN-CODE
           PERFORM 1000-OPEN-FILES
           IF ABEND-NOT-FOUND
              PERFORM 2000-PROCESS UNTIL KZHOLF-END OR ABEND-FOUND
           END-IF
           PERFORM 8000-CLOSE-FILES
           IF ABEND-FOUND
              MOVE 8 TO RETURN-CODE
           ELSE
              MOVE 0 TO RETURN-CODE
              PERFORM 9000-DISPLAY-SUMMARY
           END-IF
           GOBACK.

       1000-OPEN-FILES.
           SET ABEND-NOT-FOUND TO TRUE
           OPEN INPUT KZHOLF
           IF WS-KZHOLF-ST NOT = '00'
              DISPLAY WS-MSG-PREFIX 'KZHOLF OPEN NG ST='
                      WS-KZHOLF-ST
              SET ABEND-FOUND TO TRUE
           END-IF
           IF ABEND-NOT-FOUND
              OPEN I-O KZCALF
              IF WS-KZCALF-ST NOT = '00'
                 DISPLAY WS-MSG-PREFIX 'KZCALF OPEN NG ST='
                         WS-KZCALF-ST
                 SET ABEND-FOUND TO TRUE
              END-IF
           END-IF
           IF ABEND-NOT-FOUND
              SET KZHOLF-NOT-END TO TRUE
              PERFORM 2100-READ-HOL
           END-IF.

       2000-PROCESS.
           ADD 1 TO WS-HOL-READ-CNT
           PERFORM 3000-VALIDATE-HOL
           IF DATE-INVALID
              ADD 1 TO WS-INVALID-CNT
              DISPLAY WS-MSG-PREFIX 'BAD HOL DATE DT='
                      HO-HOLIDAY-DT
           ELSE
              PERFORM 4000-CHECK-DUPLICATE
              PERFORM 5000-CHECK-CALENDAR
           END-IF
           PERFORM 2100-READ-HOL.

       2100-READ-HOL.
           READ KZHOLF NEXT RECORD
              AT END
                 SET KZHOLF-END TO TRUE
              NOT AT END
                 CONTINUE
           END-READ
           IF WS-KZHOLF-ST NOT = '00'
              AND WS-KZHOLF-ST NOT = '10'
              DISPLAY WS-MSG-PREFIX 'KZHOLF READ NG ST='
                      WS-KZHOLF-ST
              SET ABEND-FOUND TO TRUE
           END-IF.

       3000-VALIDATE-HOL.
           SET DATE-INVALID TO TRUE
           MOVE HO-HOLIDAY-DT TO WS-DATE-X
           IF WS-DATE-X NOT NUMERIC
              EXIT PARAGRAPH
           END-IF
           MOVE WS-DATE-X(1:4) TO WS-YYYY
           MOVE WS-DATE-X(5:2) TO WS-MM
           MOVE WS-DATE-X(7:2) TO WS-DD
           IF WS-YYYY < 1900 OR WS-YYYY > 2099
              EXIT PARAGRAPH
           END-IF
           IF WS-MM < 1 OR WS-MM > 12
              EXIT PARAGRAPH
           END-IF
           EVALUATE WS-MM
              WHEN 1
              WHEN 3
              WHEN 5
              WHEN 7
              WHEN 8
              WHEN 10
              WHEN 12
                 MOVE 31 TO WS-MAX-DD
              WHEN 4
              WHEN 6
              WHEN 9
              WHEN 11
                 MOVE 30 TO WS-MAX-DD
              WHEN 2
                 PERFORM 3100-CHECK-LEAP
                 IF LEAP-YEAR
                    MOVE 29 TO WS-MAX-DD
                 ELSE
                    MOVE 28 TO WS-MAX-DD
                 END-IF
           END-EVALUATE
           IF WS-DD >= 1 AND WS-DD <= WS-MAX-DD
              SET DATE-VALID TO TRUE
           END-IF
           IF DATE-VALID
              PERFORM 3200-CALL-CALENDAR
           END-IF.

       3100-CHECK-LEAP.
           SET NOT-LEAP-YEAR TO TRUE
           IF FUNCTION MOD(WS-YYYY, 400) = 0
              SET LEAP-YEAR TO TRUE
           ELSE
              IF FUNCTION MOD(WS-YYYY, 100) NOT = 0
                 AND FUNCTION MOD(WS-YYYY, 4) = 0
                 SET LEAP-YEAR TO TRUE
              END-IF
           END-IF.

       3200-CALL-CALENDAR.
           MOVE HO-HOLIDAY-DT TO LK-CAL-NOMINAL-DT
           MOVE ZERO         TO LK-CAL-RESOLVED-DT
           MOVE SPACE        TO LK-CAL-ROLLED-FLAG
           MOVE ZERO         TO LK-CAL-RET
           CALL 'KZ900S' USING LK-CAL-PARM
           IF LK-CAL-RET NOT = ZERO
              ADD 1 TO WS-CALL-ERR-CNT
              SET DATE-INVALID TO TRUE
              DISPLAY WS-MSG-PREFIX 'CAL CALL NG DT='
                      HO-HOLIDAY-DT
                      ' RC=' LK-CAL-RET
           END-IF.

       4000-CHECK-DUPLICATE.
           IF WS-PREV-HOLIDAY-DT = HO-HOLIDAY-DT
              ADD 1 TO WS-DUP-CNT
              DISPLAY WS-MSG-PREFIX 'DUP HOL DT='
                      HO-HOLIDAY-DT
           END-IF
           MOVE HO-HOLIDAY-DT TO WS-PREV-HOLIDAY-DT.

       5000-CHECK-CALENDAR.
           MOVE HO-HOLIDAY-DT TO CA-CAL-DT
           READ KZCALF KEY IS CA-CAL-DT
              INVALID KEY
                 ADD 1 TO WS-MISSING-CNT
                 DISPLAY WS-MSG-PREFIX 'CAL MISSING DT='
                         HO-HOLIDAY-DT
              NOT INVALID KEY
                 ADD 1 TO WS-CAL-READ-CNT
                 PERFORM 5100-JUDGE-CALENDAR
           END-READ
           IF WS-KZCALF-ST NOT = '00'
              AND WS-KZCALF-ST NOT = '23'
              DISPLAY WS-MSG-PREFIX 'KZCALF READ NG ST='
                      WS-KZCALF-ST
                      ' DT=' HO-HOLIDAY-DT
              SET ABEND-FOUND TO TRUE
           END-IF.

       5100-JUDGE-CALENDAR.
           SET UPDATE-NOT-NEEDED TO TRUE
           IF HO-VALID-FLAG = 'Y'
              IF CA-HOLIDAY-FLAG = 'N'
                 SET UPDATE-NEEDED TO TRUE
              ELSE
                 IF CA-HOLIDAY-FLAG = 'Y'
                    ADD 1 TO WS-NOACT-CNT
                 ELSE
                    ADD 1 TO WS-INVALID-CNT
                    DISPLAY WS-MSG-PREFIX 'BAD CAL ATTR DT='
                            CA-CAL-DT
                 END-IF
              END-IF
           ELSE
              IF HO-VALID-FLAG = 'N'
                 IF CA-HOLIDAY-FLAG = 'Y'
                    ADD 1 TO WS-RESIDUAL-CNT
                    DISPLAY WS-MSG-PREFIX 'OLD HOL REMAIN DT='
                            HO-HOLIDAY-DT
                 ELSE
                    ADD 1 TO WS-NOACT-CNT
                 END-IF
              ELSE
                 ADD 1 TO WS-INVALID-CNT
                 DISPLAY WS-MSG-PREFIX 'BAD HOL FLAG DT='
                         HO-HOLIDAY-DT
              END-IF
           END-IF
           IF UPDATE-NEEDED
              PERFORM 5200-UPDATE-CALENDAR
           END-IF.

       5200-UPDATE-CALENDAR.
           MOVE 'Y' TO CA-HOLIDAY-FLAG
           REWRITE KZCALF-REC
           IF WS-KZCALF-ST = '00'
              ADD 1 TO WS-CAL-UPD-CNT
              DISPLAY WS-MSG-PREFIX 'CAL UPDATED DT='
                      CA-CAL-DT
           ELSE
              DISPLAY WS-MSG-PREFIX 'KZCALF WRITE NG ST='
                      WS-KZCALF-ST
                      ' DT=' CA-CAL-DT
              SET ABEND-FOUND TO TRUE
           END-IF.

       8000-CLOSE-FILES.
           IF WS-KZHOLF-ST NOT = SPACE
              CLOSE KZHOLF
              IF WS-KZHOLF-ST NOT = '00'
                 DISPLAY WS-MSG-PREFIX 'KZHOLF CLOSE NG ST='
                         WS-KZHOLF-ST
                 SET ABEND-FOUND TO TRUE
              END-IF
           END-IF
           IF WS-KZCALF-ST NOT = SPACE
              CLOSE KZCALF
              IF WS-KZCALF-ST NOT = '00'
                 DISPLAY WS-MSG-PREFIX 'KZCALF CLOSE NG ST='
                         WS-KZCALF-ST
                 SET ABEND-FOUND TO TRUE
              END-IF
           END-IF.

       9000-DISPLAY-SUMMARY.
           MOVE WS-HOL-READ-CNT TO WS-DISP-NUM
           DISPLAY WS-MSG-PREFIX 'KZHOLF READ CNT=' WS-DISP-NUM
           MOVE WS-CAL-READ-CNT TO WS-DISP-NUM
           DISPLAY WS-MSG-PREFIX 'KZCALF READ CNT=' WS-DISP-NUM
           MOVE WS-CAL-UPD-CNT TO WS-DISP-NUM
           DISPLAY WS-MSG-PREFIX 'KZCALF UPD CNT=' WS-DISP-NUM
           MOVE WS-MISSING-CNT TO WS-DISP-NUM
           DISPLAY WS-MSG-PREFIX 'CAL MISSING CNT=' WS-DISP-NUM
           MOVE WS-DUP-CNT TO WS-DISP-NUM
           DISPLAY WS-MSG-PREFIX 'DUP CNT=' WS-DISP-NUM
           MOVE WS-RESIDUAL-CNT TO WS-DISP-NUM
           DISPLAY WS-MSG-PREFIX 'RESIDUAL CNT=' WS-DISP-NUM
           MOVE WS-INVALID-CNT TO WS-DISP-NUM
           DISPLAY WS-MSG-PREFIX 'INVALID CNT=' WS-DISP-NUM
           MOVE WS-CALL-ERR-CNT TO WS-DISP-NUM
           DISPLAY WS-MSG-PREFIX 'CALL ERR CNT=' WS-DISP-NUM
           MOVE WS-NOACT-CNT TO WS-DISP-NUM
           DISPLAY WS-MSG-PREFIX 'NO ACTION CNT=' WS-DISP-NUM.
