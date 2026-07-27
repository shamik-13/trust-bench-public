       IDENTIFICATION DIVISION.
       PROGRAM-ID. JH310B.
       AUTHOR. SYSTEM-BU.
      ******************************************************************
      * 顧客セグメント付与バッチ
      * 変更履歴
      * 版数  年月日        担当                   概要
      * 1.00  平成30年04月  システム部 情報系チーム 新規作成
      * 1.01  令和02年09月  システム部 情報系チーム 5区分化対応
      * 1.02  令和06年07月  システム部 情報系チーム 判定サブ呼出見直し
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT JHCBALF
               ASSIGN TO "JHCBALF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-CBAL-ST.

           SELECT JHSEGRF
               ASSIGN TO "JHSEGRF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-SEGR-ST.

       DATA DIVISION.
       FILE SECTION.
       FD  JHCBALF.
           COPY JHCBALFC.

       FD  JHSEGRF.
           COPY JHSEGRFC.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05  WS-CBAL-ST             PIC X(02) VALUE SPACES.
           05  WS-SEGR-ST             PIC X(02) VALUE SPACES.

       01  WS-FLAG-AREA.
           05  WS-END-FLG             PIC X(01) VALUE "N".
               88  END-OF-CBAL                  VALUE "Y".
               88  NOT-END-CBAL                VALUE "N".
           05  WS-ABEND-FLG           PIC X(01) VALUE "N".
               88  ABEND-ON                     VALUE "Y".
               88  ABEND-OFF                    VALUE "N".

       01  WS-COUNT-AREA.
           05  WS-READ-CNT            PIC 9(09) VALUE ZERO.
           05  WS-WRITE-CNT           PIC 9(09) VALUE ZERO.
           05  WS-ERR-CNT             PIC 9(09) VALUE ZERO.

       01  WS-DISPLAY-AREA.
           05  WS-DISP-READ           PIC ZZZ,ZZZ,ZZ9.
           05  WS-DISP-WRITE          PIC ZZZ,ZZZ,ZZ9.
           05  WS-DISP-ERR            PIC ZZZ,ZZZ,ZZ9.

       01  WS-CONSTANT-AREA.
           05  WS-NORMAL-ST           PIC X(02) VALUE "00".
           05  WS-EOF-ST              PIC X(02) VALUE "10".
           05  WS-NORMAL-RC           PIC S9(04) COMP VALUE 0.
           05  WS-ERROR-RC            PIC S9(04) COMP VALUE 8.
           05  WS-SUB-NORMAL          PIC X(02) VALUE "00".

       01  WS-SEGMENT-CHECK.
           05  WS-VALID-SEG-FLG       PIC X(01) VALUE "N".
               88  VALID-SEGMENT                VALUE "Y".
               88  INVALID-SEGMENT              VALUE "N".
           05  WS-SG01-CD             PIC X(04) VALUE "SG01".
           05  WS-SG02-CD             PIC X(04) VALUE "SG02".
           05  WS-SG03-CD             PIC X(04) VALUE "SG03".
           05  WS-SG04-CD             PIC X(04) VALUE "SG04".
           05  WS-SG05-CD             PIC X(04) VALUE "SG05".

           COPY LK-SEG-PARM.

       PROCEDURE DIVISION.
       MAIN-RTN.
           PERFORM INIT-RTN
           IF ABEND-OFF
               PERFORM MAIN-PROC UNTIL END-OF-CBAL OR ABEND-ON
           END-IF
           PERFORM END-RTN
           GOBACK.

       INIT-RTN.
           MOVE WS-NORMAL-RC TO RETURN-CODE
           SET NOT-END-CBAL TO TRUE
           SET ABEND-OFF TO TRUE
           MOVE ZERO TO WS-READ-CNT
           MOVE ZERO TO WS-WRITE-CNT
           MOVE ZERO TO WS-ERR-CNT

           DISPLAY "JH310B 開始"

           OPEN INPUT JHCBALF
           IF WS-CBAL-ST NOT = WS-NORMAL-ST
               DISPLAY "JHCBALF オープン失敗 ST=" WS-CBAL-ST
               PERFORM ABEND-RTN
           END-IF

           IF ABEND-OFF
               OPEN OUTPUT JHSEGRF
               IF WS-SEGR-ST NOT = WS-NORMAL-ST
                   DISPLAY "JHSEGRF オープン失敗 ST=" WS-SEGR-ST
                   PERFORM ABEND-RTN
               END-IF
           END-IF.

       MAIN-PROC.
           PERFORM READ-CBAL-RTN
           IF NOT-END-CBAL AND ABEND-OFF
               PERFORM EDIT-AND-CALL-RTN
               IF ABEND-OFF
                   PERFORM WRITE-SEGR-RTN
               END-IF
           END-IF.

       READ-CBAL-RTN.
           READ JHCBALF
               AT END
                   SET END-OF-CBAL TO TRUE
               NOT AT END
                   ADD 1 TO WS-READ-CNT
           END-READ

           IF WS-CBAL-ST NOT = WS-NORMAL-ST
               AND WS-CBAL-ST NOT = WS-EOF-ST
               DISPLAY "JHCBALF 読込失敗 ST=" WS-CBAL-ST
               PERFORM ABEND-RTN
           END-IF.

       EDIT-AND-CALL-RTN.
           IF CB-CUST-ID = SPACE
               DISPLAY "顧客番号空白 件数=" WS-READ-CNT
               ADD 1 TO WS-ERR-CNT
               PERFORM ABEND-RTN
           END-IF

           IF ABEND-OFF
               INITIALIZE LK-SEG-PARM
               MOVE CB-AVG-BAL TO LK-SEG-BAL
               CALL "JH315S" USING LK-SEG-PARM
               IF LK-SEG-RET NOT = WS-SUB-NORMAL
                   DISPLAY "JH315S 異常 顧客=" CB-CUST-ID
                           " RT=" LK-SEG-RET
                   ADD 1 TO WS-ERR-CNT
                   PERFORM ABEND-RTN
               END-IF
           END-IF

           IF ABEND-OFF
               PERFORM CHECK-SEGMENT-RTN
               IF INVALID-SEGMENT
                   DISPLAY "区分不正 顧客=" CB-CUST-ID
                           " CD=" LK-SEG-CD
                   ADD 1 TO WS-ERR-CNT
                   PERFORM ABEND-RTN
               END-IF
           END-IF.

       CHECK-SEGMENT-RTN.
           SET INVALID-SEGMENT TO TRUE

           EVALUATE LK-SEG-CD
               WHEN WS-SG01-CD
                   SET VALID-SEGMENT TO TRUE
               WHEN WS-SG02-CD
                   SET VALID-SEGMENT TO TRUE
               WHEN WS-SG03-CD
                   SET VALID-SEGMENT TO TRUE
               WHEN WS-SG04-CD
                   SET VALID-SEGMENT TO TRUE
               WHEN WS-SG05-CD
                   SET VALID-SEGMENT TO TRUE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE.

       WRITE-SEGR-RTN.
           INITIALIZE JHSEGRF-REC
           MOVE CB-CUST-ID    TO SR-CUST-ID
           MOVE CB-AVG-BAL    TO SR-AVG-BAL
           MOVE LK-SEG-CD     TO SR-SEG-CD
           MOVE LK-SEG-NAME   TO SR-SEG-NAME

           WRITE JHSEGRF-REC
           IF WS-SEGR-ST = WS-NORMAL-ST
               ADD 1 TO WS-WRITE-CNT
           ELSE
               DISPLAY "JHSEGRF 書込失敗 ST=" WS-SEGR-ST
                       " CUST=" SR-CUST-ID
               ADD 1 TO WS-ERR-CNT
               PERFORM ABEND-RTN
           END-IF.

       END-RTN.
           IF WS-CBAL-ST = WS-NORMAL-ST
               OR WS-CBAL-ST = WS-EOF-ST
               CLOSE JHCBALF
               IF WS-CBAL-ST NOT = WS-NORMAL-ST
                   DISPLAY "JHCBALF クローズ失敗 ST=" WS-CBAL-ST
                   PERFORM ABEND-RTN
               END-IF
           END-IF

           IF WS-SEGR-ST = WS-NORMAL-ST
               CLOSE JHSEGRF
               IF WS-SEGR-ST NOT = WS-NORMAL-ST
                   DISPLAY "JHSEGRF クローズ失敗 ST=" WS-SEGR-ST
                   PERFORM ABEND-RTN
               END-IF
           END-IF

           MOVE WS-READ-CNT TO WS-DISP-READ
           MOVE WS-WRITE-CNT TO WS-DISP-WRITE
           MOVE WS-ERR-CNT TO WS-DISP-ERR

           DISPLAY "JH310B 読込件数=" WS-DISP-READ
           DISPLAY "JH310B 出力件数=" WS-DISP-WRITE
           DISPLAY "JH310B 異常件数=" WS-DISP-ERR

           IF ABEND-ON
               MOVE WS-ERROR-RC TO RETURN-CODE
               DISPLAY "JH310B 異常終了 RC=08"
           ELSE
               MOVE WS-NORMAL-RC TO RETURN-CODE
               DISPLAY "JH310B 正常終了 RC=00"
           END-IF.

       ABEND-RTN.
           SET ABEND-ON TO TRUE
           MOVE WS-ERROR-RC TO RETURN-CODE.
