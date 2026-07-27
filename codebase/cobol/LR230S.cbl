       IDENTIFICATION DIVISION.
       PROGRAM-ID. LR230S.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LRRPTF ASSIGN TO "LRRPTF"
              ORGANIZATION IS INDEXED
              ACCESS MODE  IS DYNAMIC
              RECORD KEY   IS RP-REPORT-ID
              FILE STATUS  IS WK-LRRPTF-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  LRRPTF.
           COPY LRRPTFC.

       WORKING-STORAGE SECTION.
       01  WK-LRRPTF-STATUS        PIC XX VALUE SPACE.
       01  WK-EOF-SW               PIC X  VALUE "0".
           88 WK-EOF                      VALUE "1".
           88 WK-NOT-EOF                  VALUE "0".
       01  WK-FOUND-SW             PIC X  VALUE "0".
           88 WK-FOUND                    VALUE "1".
           88 WK-NOT-FOUND                VALUE "0".
       01  WK-HARD-ERR-SW          PIC X  VALUE "0".
           88 WK-HARD-ERR                 VALUE "1".
           88 WK-NO-HARD-ERR             VALUE "0".

       01  WK-EDIT-WORK.
           05 WK-AMT-ABS           PIC 9(13) VALUE ZERO.
           05 WK-AMT-LIMIT         PIC 9(13) VALUE ZERO.
           05 WK-AMT-DISP          PIC ZZZ,ZZZ,ZZZ,ZZ9.
           05 WK-AMT-DISP-S        PIC -ZZ,ZZZ,ZZZ,ZZ9.
           05 WK-YM-WORK           PIC 9(6) VALUE ZERO.
           05 WK-YM-YYYY           PIC 9(4) VALUE ZERO.
           05 WK-YM-MM             PIC 9(2) VALUE ZERO.
           05 WK-YM-EDIT           PIC X(12) VALUE SPACE.
           05 WK-POL-EDIT          PIC X(16) VALUE SPACE.
           05 WK-LINE-EDIT         PIC X(132) VALUE SPACE.
           05 WK-REASON            PIC X(40) VALUE SPACE.
           05 WK-WARN-SW           PIC X VALUE "0".
              88 WK-WARN                 VALUE "1".
              88 WK-NO-WARN              VALUE "0".
           05 WK-PAGE-SW           PIC X VALUE "0".
              88 WK-PAGE                 VALUE "1".
              88 WK-NO-PAGE              VALUE "0".

       01  WK-CHECK-FIELDS.
           05 WK-MM-NUM            PIC 99 VALUE ZERO.
           05 WK-LINE-NUM          PIC 9(5) VALUE ZERO.

       LINKAGE SECTION.
       01  LK-LR230S-PARM.
           05 LK-REPORT-ID         PIC X(12).
           05 LK-PRINT-LINE        PIC X(132).
           05 LK-PAGE-KBN          PIC X.
           05 LK-EDIT-STATUS-KBN   PIC X.
           05 LK-REASON-TEXT       PIC X(40).

       PROCEDURE DIVISION USING LK-LR230S-PARM.
       0000-MAIN.
           MOVE 0 TO RETURN-CODE
           PERFORM 1000-INIT
           IF WK-NO-HARD-ERR
              PERFORM 2000-READ-REPORT
           END-IF
           IF WK-NO-HARD-ERR AND WK-FOUND
              PERFORM 3000-VALIDATE-REPORT
              IF WK-NO-HARD-ERR
                 PERFORM 4000-EDIT-LINE
              END-IF
           END-IF
           PERFORM 9000-FINAL
           GOBACK.

       1000-INIT.
           MOVE SPACE TO LK-PRINT-LINE
           MOVE "0"   TO LK-PAGE-KBN
           MOVE "0"   TO LK-EDIT-STATUS-KBN
           MOVE SPACE TO LK-REASON-TEXT
           SET WK-NOT-EOF TO TRUE
           SET WK-NOT-FOUND TO TRUE
           SET WK-NO-HARD-ERR TO TRUE
           SET WK-NO-WARN TO TRUE
           SET WK-NO-PAGE TO TRUE
           OPEN INPUT LRRPTF
           IF WK-LRRPTF-STATUS NOT = "00"
              DISPLAY "LRRPTF OPEN ST=" WK-LRRPTF-STATUS
              MOVE "LRRPTF オープン失敗" TO LK-REASON-TEXT
              MOVE "9" TO LK-EDIT-STATUS-KBN
              MOVE 8 TO RETURN-CODE
              SET WK-HARD-ERR TO TRUE
           END-IF.

       2000-READ-REPORT.
           MOVE LK-REPORT-ID TO RP-REPORT-ID
           READ LRRPTF KEY IS RP-REPORT-ID
              INVALID KEY
                 MOVE "帳票管理レコードなし" TO LK-REASON-TEXT
                 MOVE "9" TO LK-EDIT-STATUS-KBN
                 DISPLAY "REPORT NOT FOUND"
                 DISPLAY LK-REPORT-ID
                 MOVE 8 TO RETURN-CODE
                 SET WK-HARD-ERR TO TRUE
              NOT INVALID KEY
                 SET WK-FOUND TO TRUE
           END-READ
           IF WK-LRRPTF-STATUS NOT = "00"
              AND WK-LRRPTF-STATUS NOT = "23"
              DISPLAY "LRRPTF READ ST=" WK-LRRPTF-STATUS
              MOVE "LRRPTF 読込失敗" TO LK-REASON-TEXT
              MOVE "9" TO LK-EDIT-STATUS-KBN
              MOVE 8 TO RETURN-CODE
              SET WK-HARD-ERR TO TRUE
           END-IF.

       3000-VALIDATE-REPORT.
           MOVE RP-REPORT-YM TO WK-YM-WORK
           MOVE WK-YM-WORK(1:4) TO WK-YM-YYYY
           MOVE WK-YM-WORK(5:2) TO WK-YM-MM
           MOVE WK-YM-MM TO WK-MM-NUM
           MOVE RP-LINE-NO TO WK-LINE-NUM

           IF WK-YM-YYYY < 1989 OR WK-MM-NUM < 1 OR WK-MM-NUM > 12
              MOVE "年月不正" TO WK-REASON
              SET WK-WARN TO TRUE
           END-IF

           IF RP-REPORT-TYPE-KBN NOT = "1"
              AND RP-REPORT-TYPE-KBN NOT = "2"
              AND RP-REPORT-TYPE-KBN NOT = "3"
              MOVE "帳票種別不正" TO LK-REASON-TEXT
              MOVE "9" TO LK-EDIT-STATUS-KBN
              DISPLAY "REPORT TYPE ERROR"
              DISPLAY RP-REPORT-ID
              MOVE 8 TO RETURN-CODE
              SET WK-HARD-ERR TO TRUE
           END-IF

           IF WK-NO-HARD-ERR
              EVALUATE TRUE
                 WHEN RP-PRINT-AMT < ZERO
                    MOVE "金額符号注意" TO WK-REASON
                    SET WK-WARN TO TRUE
                 WHEN RP-PRINT-AMT = ZERO
                    MOVE "金額ゼロ注意" TO WK-REASON
                    SET WK-WARN TO TRUE
              END-EVALUATE
           END-IF

           IF WK-NO-HARD-ERR
              COMPUTE WK-AMT-ABS = FUNCTION ABS(RP-PRINT-AMT)
              EVALUATE RP-REPORT-TYPE-KBN
                 WHEN "1"
                    MOVE 999999999999 TO WK-AMT-LIMIT
                 WHEN "2"
                    MOVE 99999999999 TO WK-AMT-LIMIT
                 WHEN "3"
                    MOVE 999999999 TO WK-AMT-LIMIT
              END-EVALUATE
              IF WK-AMT-ABS > WK-AMT-LIMIT
                 MOVE "金額桁あふれ" TO LK-REASON-TEXT
                 MOVE "8" TO LK-EDIT-STATUS-KBN
                 DISPLAY "AMOUNT OVERFLOW"
                 DISPLAY RP-REPORT-ID
                 DISPLAY RP-PRINT-AMT
                 MOVE 8 TO RETURN-CODE
                 SET WK-HARD-ERR TO TRUE
              END-IF
           END-IF

           IF WK-NO-HARD-ERR
              IF WK-LINE-NUM = 1 OR WK-LINE-NUM > 55
                 SET WK-PAGE TO TRUE
              END-IF
           END-IF.

       4000-EDIT-LINE.
           MOVE SPACE TO WK-LINE-EDIT
           MOVE SPACE TO WK-POL-EDIT
           MOVE SPACE TO WK-YM-EDIT
           MOVE ZERO  TO WK-AMT-DISP
           MOVE ZERO  TO WK-AMT-DISP-S

           EVALUATE RP-REPORT-TYPE-KBN
              WHEN "1"
                 MOVE RP-POL-NO TO WK-POL-EDIT
                 STRING WK-YM-WORK(1:4) "/" WK-YM-WORK(5:2)
                    DELIMITED BY SIZE INTO WK-YM-EDIT
                 MOVE RP-PRINT-AMT TO WK-AMT-DISP-S
                 STRING "管理 " WK-YM-EDIT " " WK-POL-EDIT
                        " " WK-AMT-DISP-S
                    DELIMITED BY SIZE INTO WK-LINE-EDIT
              WHEN "2"
                 MOVE RP-POL-NO(5:8) TO WK-POL-EDIT(1:8)
                 STRING WK-YM-WORK(1:4) "年" WK-YM-WORK(5:2)
                        "月"
                    DELIMITED BY SIZE INTO WK-YM-EDIT
                 MOVE WK-AMT-ABS TO WK-AMT-DISP
                 STRING "顧客 " WK-YM-EDIT " 契約"
                        WK-POL-EDIT(1:8) " " WK-AMT-DISP
                    DELIMITED BY SIZE INTO WK-LINE-EDIT
              WHEN "3"
                 COMPUTE WK-AMT-ABS = WK-AMT-ABS / 1000
                 MOVE WK-AMT-ABS TO WK-AMT-DISP
                 STRING "集計 " WK-YM-WORK(1:4) "."
                        WK-YM-WORK(5:2) " "
                        RP-POL-NO(1:4) "-" RP-POL-NO(5:4)
                        " " WK-AMT-DISP "千円"
                    DELIMITED BY SIZE INTO WK-LINE-EDIT
           END-EVALUATE

           MOVE WK-LINE-EDIT TO LK-PRINT-LINE

           IF WK-PAGE
              MOVE "1" TO LK-PAGE-KBN
           ELSE
              MOVE "0" TO LK-PAGE-KBN
           END-IF

           IF WK-WARN
              MOVE "1" TO LK-EDIT-STATUS-KBN
              MOVE WK-REASON TO LK-REASON-TEXT
              DISPLAY WK-REASON
              DISPLAY RP-REPORT-ID
           ELSE
              MOVE RP-OUTPUT-STATUS-KBN TO LK-EDIT-STATUS-KBN
              MOVE SPACE TO LK-REASON-TEXT
           END-IF.

       9000-FINAL.
           IF WK-LRRPTF-STATUS = "00"
              CLOSE LRRPTF
              IF WK-LRRPTF-STATUS NOT = "00"
                 DISPLAY "LRRPTF CLOSE ST=" WK-LRRPTF-STATUS
                 MOVE "LRRPTF クローズ失敗" TO LK-REASON-TEXT
                 MOVE "9" TO LK-EDIT-STATUS-KBN
                 MOVE 8 TO RETURN-CODE
              END-IF
           END-IF.
