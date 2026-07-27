       IDENTIFICATION DIVISION.
       PROGRAM-ID. CB115S.
      *===============================================================*
      * 変更履歴                                                     *
      * 版数  年月日    担当  概要                                  *
      * 1.00  20240401  BT01  新規作成                              *
      * 1.01  20240715  BT02  小数桁数検査を追加                    *
      * 1.02  20241030  BT03  前回値差分の理由返却を追加            *
      *===============================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CDRTEXF ASSIGN TO "CDRTEXF"
             ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC
             RECORD KEY IS FX-CURRENCY-CD
             FILE STATUS IS WS-CDRTEXF-ST.

       DATA DIVISION.
       FILE SECTION.
       FD  CDRTEXF.
           COPY CDRTEXC.

       WORKING-STORAGE SECTION.
       01  WS-CDRTEXF-ST              PIC X(02) VALUE SPACE.
       01  WS-FILE-OPEN-SW            PIC X(01) VALUE "N".
           88  WS-FILE-OPENED                   VALUE "Y".

       01  WS-VALIDATION-AREA.
           05  WS-INPUT-OK-SW         PIC X(01) VALUE "Y".
               88  WS-INPUT-OK                 VALUE "Y".
               88  WS-INPUT-NG                 VALUE "N".
           05  WS-DIFF-RATE           PIC S9(09)V9(06) VALUE 0.
           05  WS-DIFF-ABS            PIC  9(09)V9(06) VALUE 0.
           05  WS-DIFF-LIMIT          PIC  9(09)V9(06) VALUE 0.100000.
           05  WS-MAX-DECIMAL         PIC  9(02) VALUE 6.

       01  WS-MESSAGE-AREA.
           05  WS-DISP-ST             PIC X(02) VALUE SPACE.

       LINKAGE SECTION.
       01  LK-CB115S-PARM.
           05  LK-IN-CURRENCY-CD      PIC X(03).
           05  LK-IN-APPLY-DT         PIC 9(08).
           05  LK-IN-TTM-RATE         PIC S9(09)V9(06).
           05  LK-IN-DECIMAL-CNT      PIC 9(02).
           05  LK-OUT-RET-CD          PIC 9(02).
           05  LK-OUT-REASON-CD       PIC X(08).

       PROCEDURE DIVISION USING LK-CB115S-PARM.
       0000-MAIN.
           MOVE 0        TO RETURN-CODE
           MOVE 0        TO LK-OUT-RET-CD
           MOVE SPACE    TO LK-OUT-REASON-CD

           PERFORM 1000-OPEN-FILE
           IF RETURN-CODE NOT = 0
              GOBACK
           END-IF

           PERFORM 2000-CHECK-PARAMETER

           IF WS-INPUT-OK
              PERFORM 3000-READ-RATE
           END-IF

           IF WS-FILE-OPENED
              PERFORM 9000-CLOSE-FILE
           END-IF

           IF RETURN-CODE = 0
              MOVE 0 TO RETURN-CODE
           END-IF

           GOBACK.

       1000-OPEN-FILE.
           OPEN INPUT CDRTEXF

           IF WS-CDRTEXF-ST = "00"
              MOVE "Y" TO WS-FILE-OPEN-SW
           ELSE
              MOVE WS-CDRTEXF-ST TO WS-DISP-ST
              DISPLAY "CDRTEXF オープン失敗 ST=" WS-DISP-ST
              MOVE 8 TO RETURN-CODE
              MOVE 8 TO LK-OUT-RET-CD
              MOVE "FOPEN" TO LK-OUT-REASON-CD
           END-IF.

       2000-CHECK-PARAMETER.
           IF LK-IN-CURRENCY-CD = SPACE
              SET WS-INPUT-NG TO TRUE
              MOVE 4 TO LK-OUT-RET-CD
              MOVE "CURR" TO LK-OUT-REASON-CD
           END-IF

           IF WS-INPUT-OK
              IF LK-IN-APPLY-DT = ZERO
                 SET WS-INPUT-NG TO TRUE
                 MOVE 4 TO LK-OUT-RET-CD
                 MOVE "DATE" TO LK-OUT-REASON-CD
              END-IF
           END-IF

           IF WS-INPUT-OK
              IF LK-IN-TTM-RATE <= ZERO
                 SET WS-INPUT-NG TO TRUE
                 MOVE 4 TO LK-OUT-RET-CD
                 MOVE "SIGN" TO LK-OUT-REASON-CD
              END-IF
           END-IF

           IF WS-INPUT-OK
              IF LK-IN-DECIMAL-CNT > WS-MAX-DECIMAL
                 SET WS-INPUT-NG TO TRUE
                 MOVE 4 TO LK-OUT-RET-CD
                 MOVE "DECS" TO LK-OUT-REASON-CD
              END-IF
           END-IF.

       3000-READ-RATE.
           MOVE LK-IN-CURRENCY-CD TO FX-CURRENCY-CD

           READ CDRTEXF KEY IS FX-CURRENCY-CD
              INVALID KEY
                 MOVE 4 TO LK-OUT-RET-CD
                 MOVE "NOREC" TO LK-OUT-REASON-CD
              NOT INVALID KEY
                 PERFORM 3100-CHECK-RATE-RECORD
           END-READ

           IF WS-CDRTEXF-ST NOT = "00"
              AND WS-CDRTEXF-ST NOT = "23"
              MOVE WS-CDRTEXF-ST TO WS-DISP-ST
              DISPLAY "CDRTEXF 読込失敗 ST=" WS-DISP-ST
              MOVE 8 TO RETURN-CODE
              MOVE 8 TO LK-OUT-RET-CD
              MOVE "FREAD" TO LK-OUT-REASON-CD
           END-IF.

       3100-CHECK-RATE-RECORD.
           IF FX-APPLY-STATUS NOT = "1"
              MOVE 4 TO LK-OUT-RET-CD
              MOVE "STATUS" TO LK-OUT-REASON-CD
           END-IF

           IF LK-OUT-RET-CD = 0
              IF FX-RATE-DT > LK-IN-APPLY-DT
                 MOVE 4 TO LK-OUT-RET-CD
                 MOVE "EFFDT" TO LK-OUT-REASON-CD
              END-IF
           END-IF

           IF LK-OUT-RET-CD = 0
              IF FX-TTM-RATE <= ZERO
                 MOVE 4 TO LK-OUT-RET-CD
                 MOVE "MSTSGN" TO LK-OUT-REASON-CD
              END-IF
           END-IF

           IF LK-OUT-RET-CD = 0
              SUBTRACT FX-TTM-RATE FROM LK-IN-TTM-RATE
                 GIVING WS-DIFF-RATE
              IF WS-DIFF-RATE < ZERO
                 COMPUTE WS-DIFF-ABS = WS-DIFF-RATE * -1
              ELSE
                 MOVE WS-DIFF-RATE TO WS-DIFF-ABS
              END-IF
              IF WS-DIFF-ABS > WS-DIFF-LIMIT
                 MOVE 4 TO LK-OUT-RET-CD
                 MOVE "DIFF" TO LK-OUT-REASON-CD
              END-IF
           END-IF.

       9000-CLOSE-FILE.
           CLOSE CDRTEXF

           IF WS-CDRTEXF-ST = "00"
              MOVE "N" TO WS-FILE-OPEN-SW
           ELSE
              MOVE WS-CDRTEXF-ST TO WS-DISP-ST
              DISPLAY "CDRTEXF クローズ失敗 ST=" WS-DISP-ST
              MOVE 8 TO RETURN-CODE
              MOVE 8 TO LK-OUT-RET-CD
              MOVE "FCLOSE" TO LK-OUT-REASON-CD
           END-IF.
