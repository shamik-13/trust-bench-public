       IDENTIFICATION DIVISION.
       PROGRAM-ID. CT270B.
      ******************************************************************
      * 資金ポジション照会帳票バッチ
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CCPOSF ASSIGN TO "CCPOSF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS PS-ORG-CD
               FILE STATUS IS WS-CCPOSF-ST.
           SELECT CCMONF ASSIGN TO "CCMONF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-CCMONF-ST.
           SELECT CCRPTF ASSIGN TO "CCRPTF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-CCRPTF-ST.

       DATA DIVISION.
       FILE SECTION.
       FD  CCPOSF.
           COPY CCPOSC.
       FD  CCMONF.
           COPY CCMONC.
       FD  CCRPTF.
           COPY CCRPTC.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05 WS-CCPOSF-ST              PIC X(02) VALUE SPACE.
           05 WS-CCMONF-ST              PIC X(02) VALUE SPACE.
           05 WS-CCRPTF-ST              PIC X(02) VALUE SPACE.

       01  WS-END-SW.
           05 WS-PS-EOF-SW              PIC X VALUE "N".
              88 PS-EOF                VALUE "Y".
              88 PS-NOT-EOF            VALUE "N".
           05 WS-MN-EOF-SW              PIC X VALUE "N".
              88 MN-EOF                VALUE "Y".
              88 MN-NOT-EOF            VALUE "N".

       01  WS-CONTROL.
           05 WS-REPORT-ID              PIC X(10) VALUE "CT270B".
           05 WS-REPORT-KBN             PIC X(02) VALUE "01".
           05 WS-LINE-NO                PIC 9(07) VALUE ZERO.
           05 WS-BASE-DT                PIC 9(08) VALUE ZERO.
           05 WS-PROCESS-YYYYMM         PIC 9(06) VALUE ZERO.
           05 WS-WARN-KBN               PIC X VALUE SPACE.
           05 WS-MONTH-FOUND-SW         PIC X VALUE "N".
              88 MONTH-FOUND           VALUE "Y".
              88 MONTH-NOT-FOUND       VALUE "N".

       01  WS-COUNTERS.
           05 WS-PS-READ-CNT            PIC 9(09) VALUE ZERO.
           05 WS-MN-READ-CNT            PIC 9(09) VALUE ZERO.
           05 WS-RP-WRITE-CNT           PIC 9(09) VALUE ZERO.
           05 WS-WARN-CNT               PIC 9(09) VALUE ZERO.
           05 WS-SKIP-MN-CNT            PIC 9(09) VALUE ZERO.

       01  WS-AMOUNTS.
           05 WS-MN-INSTR-AMT           PIC S9(15) VALUE ZERO.
           05 WS-MN-VALUE-AMT           PIC S9(15) VALUE ZERO.
           05 WS-MN-INSTR-CNT           PIC 9(09) VALUE ZERO.
           05 WS-MN-VALUE-CNT           PIC 9(09) VALUE ZERO.

       01  WS-EDIT.
           05 WS-AVAIL-E                PIC ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.
           05 WS-RESV-E                 PIC ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.
           05 WS-INSTR-E                PIC ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.
           05 WS-VALUE-E                PIC ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.
           05 WS-ICNT-E                 PIC ZZZ,ZZZ,ZZ9.
           05 WS-VCNT-E                 PIC ZZZ,ZZZ,ZZ9.
           05 WS-LINE-E                 PIC ZZZZZZ9.
           05 WS-WARN-CNT-E             PIC ZZZZZZZZ9.
           05 WS-SKIP-MN-CNT-E          PIC ZZZZZZZZ9.

       01  WS-REPORT-LINE               PIC X(132) VALUE SPACE.
       01  WS-ABEND-MSG                 PIC X(80) VALUE SPACE.

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE 0 TO RETURN-CODE
           PERFORM 1000-OPEN
           PERFORM 2000-INITIAL-READ
           PERFORM 3000-WRITE-HEADER
           PERFORM 4000-MAIN-PROCESS UNTIL PS-EOF
           PERFORM 8000-WRITE-TRAILER
           PERFORM 9000-CLOSE
           DISPLAY "CT270B 正常終了 "
           DISPLAY "POS件数=" WS-PS-READ-CNT
           DISPLAY "MON件数=" WS-MN-READ-CNT
           DISPLAY "出力件数=" WS-RP-WRITE-CNT
           DISPLAY "警告件数=" WS-WARN-CNT
           GOBACK.

       1000-OPEN.
           OPEN INPUT CCPOSF
           IF WS-CCPOSF-ST NOT = "00"
              STRING "CCPOSF オープン失敗 ST=" WS-CCPOSF-ST
                 DELIMITED BY SIZE INTO WS-ABEND-MSG
              END-STRING
              PERFORM 9900-ABEND
           END-IF

           OPEN INPUT CCMONF
           IF WS-CCMONF-ST NOT = "00"
              STRING "CCMONF オープン失敗 ST=" WS-CCMONF-ST
                 DELIMITED BY SIZE INTO WS-ABEND-MSG
              END-STRING
              PERFORM 9900-ABEND
           END-IF

           OPEN OUTPUT CCRPTF
           IF WS-CCRPTF-ST NOT = "00"
              STRING "CCRPTF オープン失敗 ST=" WS-CCRPTF-ST
                 DELIMITED BY SIZE INTO WS-ABEND-MSG
              END-STRING
              PERFORM 9900-ABEND
           END-IF.

       2000-INITIAL-READ.
           PERFORM 2100-READ-PS
           PERFORM 2200-READ-MN
           IF PS-NOT-EOF
              MOVE PS-BASE-DT TO WS-BASE-DT
              COMPUTE WS-PROCESS-YYYYMM = PS-BASE-DT / 100
           END-IF.

       2100-READ-PS.
           READ CCPOSF NEXT RECORD
              AT END
                 SET PS-EOF TO TRUE
              NOT AT END
                 ADD 1 TO WS-PS-READ-CNT
                 PERFORM 2300-VALIDATE-PS
           END-READ
           IF WS-CCPOSF-ST NOT = "00" AND WS-CCPOSF-ST NOT = "10"
              STRING "CCPOSF 読込失敗 ST=" WS-CCPOSF-ST
                 DELIMITED BY SIZE INTO WS-ABEND-MSG
              END-STRING
              PERFORM 9900-ABEND
           END-IF.

       2200-READ-MN.
           READ CCMONF
              AT END
                 SET MN-EOF TO TRUE
              NOT AT END
                 ADD 1 TO WS-MN-READ-CNT
                 PERFORM 2400-VALIDATE-MN
           END-READ
           IF WS-CCMONF-ST NOT = "00" AND WS-CCMONF-ST NOT = "10"
              STRING "CCMONF 読込失敗 ST=" WS-CCMONF-ST
                 DELIMITED BY SIZE INTO WS-ABEND-MSG
              END-STRING
              PERFORM 9900-ABEND
           END-IF.

       2300-VALIDATE-PS.
           IF PS-ORG-CD = SPACE
              STRING "CCPOSF 組織コード未設定 件数="
                     WS-PS-READ-CNT
                 DELIMITED BY SIZE INTO WS-ABEND-MSG
              END-STRING
              PERFORM 9900-ABEND
           END-IF
           IF PS-BASE-DT = ZERO
              STRING "CCPOSF 基準日未設定 組織=" PS-ORG-CD
                 DELIMITED BY SIZE INTO WS-ABEND-MSG
              END-STRING
              PERFORM 9900-ABEND
           END-IF
           IF PS-POSITION-STATUS-KBN NOT = "0"
              AND PS-POSITION-STATUS-KBN NOT = "1"
              DISPLAY "ポジション状態区分注意"
              DISPLAY "組織=" PS-ORG-CD
              DISPLAY "区分=" PS-POSITION-STATUS-KBN
           END-IF.

       2400-VALIDATE-MN.
           IF MN-ORG-CD = SPACE
              STRING "CCMONF 組織コード未設定 件数="
                     WS-MN-READ-CNT
                 DELIMITED BY SIZE INTO WS-ABEND-MSG
              END-STRING
              PERFORM 9900-ABEND
           END-IF
           IF MN-YYYYMM = ZERO
              STRING "CCMONF 年月未設定 組織=" MN-ORG-CD
                 DELIMITED BY SIZE INTO WS-ABEND-MSG
              END-STRING
              PERFORM 9900-ABEND
           END-IF.

       3000-WRITE-HEADER.
           MOVE SPACE TO WS-REPORT-LINE
           STRING "資金ポジション照会帳票 "
                  "基準日=" WS-BASE-DT
              DELIMITED BY SIZE INTO WS-REPORT-LINE
           END-STRING
           PERFORM 7000-WRITE-RP

           MOVE SPACE TO WS-REPORT-LINE
           STRING "組織  利用可能額       予約済額         "
                  "月中指図額       月中決済額       "
                  "指図件数 決済件数 警告"
              DELIMITED BY SIZE INTO WS-REPORT-LINE
           END-STRING
           PERFORM 7000-WRITE-RP.

       4000-MAIN-PROCESS.
           PERFORM 4100-LOCATE-MONTH
           PERFORM 5000-BUILD-DETAIL
           PERFORM 7000-WRITE-RP
           PERFORM 2100-READ-PS.

       4100-LOCATE-MONTH.
           SET MONTH-NOT-FOUND TO TRUE
           MOVE ZERO TO WS-MN-INSTR-AMT
           MOVE ZERO TO WS-MN-VALUE-AMT
           MOVE ZERO TO WS-MN-INSTR-CNT
           MOVE ZERO TO WS-MN-VALUE-CNT

           PERFORM UNTIL MN-EOF
              OR MN-ORG-CD >= PS-ORG-CD
              PERFORM 4200-SKIP-MONTH
           END-PERFORM

           IF MN-NOT-EOF AND MN-ORG-CD = PS-ORG-CD
              IF MN-YYYYMM = WS-PROCESS-YYYYMM
                 MOVE MN-TOTAL-INSTR-AMT TO WS-MN-INSTR-AMT
                 MOVE MN-TOTAL-VALUE-AMT TO WS-MN-VALUE-AMT
                 MOVE MN-COUNT-INSTR TO WS-MN-INSTR-CNT
                 MOVE MN-COUNT-VALUE TO WS-MN-VALUE-CNT
                 SET MONTH-FOUND TO TRUE
              END-IF
              PERFORM 2200-READ-MN
           END-IF.

       4200-SKIP-MONTH.
           ADD 1 TO WS-SKIP-MN-CNT
           DISPLAY "月次のみ存在のため読飛ばし"
           DISPLAY "組織=" MN-ORG-CD
           DISPLAY "年月=" MN-YYYYMM
           PERFORM 2200-READ-MN.

       5000-BUILD-DETAIL.
           MOVE SPACE TO WS-WARN-KBN
           IF PS-AVAILABLE-AMT < PS-RESERVED-AMT
              MOVE "W" TO WS-WARN-KBN
              ADD 1 TO WS-WARN-CNT
           END-IF

           MOVE PS-AVAILABLE-AMT TO WS-AVAIL-E
           MOVE PS-RESERVED-AMT TO WS-RESV-E
           MOVE WS-MN-INSTR-AMT TO WS-INSTR-E
           MOVE WS-MN-VALUE-AMT TO WS-VALUE-E
           MOVE WS-MN-INSTR-CNT TO WS-ICNT-E
           MOVE WS-MN-VALUE-CNT TO WS-VCNT-E

           MOVE SPACE TO WS-REPORT-LINE
           STRING PS-ORG-CD DELIMITED BY SIZE
                  " " DELIMITED BY SIZE
                  WS-AVAIL-E DELIMITED BY SIZE
                  " " DELIMITED BY SIZE
                  WS-RESV-E DELIMITED BY SIZE
                  " " DELIMITED BY SIZE
                  WS-INSTR-E DELIMITED BY SIZE
                  " " DELIMITED BY SIZE
                  WS-VALUE-E DELIMITED BY SIZE
                  " " DELIMITED BY SIZE
                  WS-ICNT-E DELIMITED BY SIZE
                  " " DELIMITED BY SIZE
                  WS-VCNT-E DELIMITED BY SIZE
                  " " DELIMITED BY SIZE
                  WS-WARN-KBN DELIMITED BY SIZE
              INTO WS-REPORT-LINE
           END-STRING

           IF MONTH-NOT-FOUND
              STRING WS-REPORT-LINE DELIMITED BY SIZE
                     " 月次なし" DELIMITED BY SIZE
                 INTO WS-REPORT-LINE
              END-STRING
           END-IF.

       7000-WRITE-RP.
           ADD 1 TO WS-LINE-NO
           MOVE WS-REPORT-ID TO RP-REPORT-ID
           MOVE WS-BASE-DT TO RP-BASE-DT
           MOVE WS-REPORT-KBN TO RP-REPORT-KBN
           MOVE WS-LINE-NO TO RP-LINE-NO
           MOVE WS-REPORT-LINE TO RP-REPORT-TEXT

           WRITE CCRPTF-REC
           IF WS-CCRPTF-ST NOT = "00"
              STRING "CCRPTF 書込失敗 ST=" WS-CCRPTF-ST
                     " 行=" WS-LINE-NO
                 DELIMITED BY SIZE INTO WS-ABEND-MSG
              END-STRING
              PERFORM 9900-ABEND
           END-IF
           ADD 1 TO WS-RP-WRITE-CNT.

       8000-WRITE-TRAILER.
           MOVE WS-LINE-NO TO WS-LINE-E
           MOVE WS-WARN-CNT TO WS-WARN-CNT-E
           MOVE WS-SKIP-MN-CNT TO WS-SKIP-MN-CNT-E
           MOVE SPACE TO WS-REPORT-LINE
           STRING "帳票終了 明細行数=" WS-LINE-E
                  " 警告件数=" WS-WARN-CNT-E
                  " 月次読飛ばし=" WS-SKIP-MN-CNT-E
              DELIMITED BY SIZE INTO WS-REPORT-LINE
           END-STRING
           PERFORM 7000-WRITE-RP.

       9000-CLOSE.
           CLOSE CCPOSF
           IF WS-CCPOSF-ST NOT = "00"
              STRING "CCPOSF クローズ失敗 ST=" WS-CCPOSF-ST
                 DELIMITED BY SIZE INTO WS-ABEND-MSG
              END-STRING
              PERFORM 9900-ABEND
           END-IF

           CLOSE CCMONF
           IF WS-CCMONF-ST NOT = "00"
              STRING "CCMONF クローズ失敗 ST=" WS-CCMONF-ST
                 DELIMITED BY SIZE INTO WS-ABEND-MSG
              END-STRING
              PERFORM 9900-ABEND
           END-IF

           CLOSE CCRPTF
           IF WS-CCRPTF-ST NOT = "00"
              STRING "CCRPTF クローズ失敗 ST=" WS-CCRPTF-ST
                 DELIMITED BY SIZE INTO WS-ABEND-MSG
              END-STRING
              PERFORM 9900-ABEND
           END-IF.

       9900-ABEND.
           DISPLAY "CT270B 異常終了 " WS-ABEND-MSG
           MOVE 8 TO RETURN-CODE
           GOBACK.
