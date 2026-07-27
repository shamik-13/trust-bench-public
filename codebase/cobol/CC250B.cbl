       IDENTIFICATION DIVISION.
       PROGRAM-ID. CC250B.
       AUTHOR.     COMMON.
       DATE-WRITTEN. 2024-11-12.
      *
      * 受渡日明細帳票編集バッチ
      * CCDTLFを読み、CCVALF/CCFCTFとの整合を確認して帳票を作成する。
      * 不整合明細は集計対象外とし、帳票末尾へ注記する。
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CCDTLF ASSIGN TO "CCDTLF"
              ORGANIZATION IS SEQUENTIAL
              FILE STATUS IS WS-ST-CCDTLF.
           SELECT CCVALF ASSIGN TO "CCVALF"
              ORGANIZATION IS SEQUENTIAL
              FILE STATUS IS WS-ST-CCVALF.
           SELECT CCFCTF ASSIGN TO "CCFCTF"
              ORGANIZATION IS SEQUENTIAL
              FILE STATUS IS WS-ST-CCFCTF.
           SELECT CCRPTF ASSIGN TO "CCRPTF"
              ORGANIZATION IS SEQUENTIAL
              FILE STATUS IS WS-ST-CCRPTF.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  CCDTLF.
           COPY CCDTLC.
       FD  CCVALF.
           COPY CCVALFC.
       FD  CCFCTF.
           COPY CCFCTFC.
       FD  CCRPTF.
           COPY CCRPTC.
      *
       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05 WS-ST-CCDTLF              PIC XX VALUE SPACE.
           05 WS-ST-CCVALF              PIC XX VALUE SPACE.
           05 WS-ST-CCFCTF              PIC XX VALUE SPACE.
           05 WS-ST-CCRPTF              PIC XX VALUE SPACE.
      *
       01  WS-FLAGS.
           05 WS-EOF-CCDTLF             PIC X VALUE "N".
              88 END-CCDTLF                  VALUE "Y".
           05 WS-EOF-CCVALF             PIC X VALUE "N".
              88 END-CCVALF                  VALUE "Y".
           05 WS-EOF-CCFCTF             PIC X VALUE "N".
              88 END-CCFCTF                  VALUE "Y".
           05 WS-FOUND-VAL              PIC X VALUE "N".
              88 VAL-FOUND                   VALUE "Y".
           05 WS-FOUND-FCT              PIC X VALUE "N".
              88 FCT-FOUND                   VALUE "Y".
           05 WS-HARD-ERROR             PIC X VALUE "N".
              88 HARD-ERROR                  VALUE "Y".
      *
       01  WS-COUNTERS.
           05 WS-READ-DL-CNT            PIC 9(9) VALUE 0.
           05 WS-OK-DL-CNT              PIC 9(9) VALUE 0.
           05 WS-SKIP-DL-CNT            PIC 9(9) VALUE 0.
           05 WS-SKIP-VAL-CNT           PIC 9(9) VALUE 0.
           05 WS-SKIP-FCT-CNT           PIC 9(9) VALUE 0.
           05 WS-SKIP-STATUS-CNT        PIC 9(9) VALUE 0.
           05 WS-ZERO-DL-CNT            PIC 9(9) VALUE 0.
           05 WS-RP-LINE-NO             PIC 9(7) VALUE 0.
           05 WS-RP-ID-SEQ              PIC 9(7) VALUE 0.
      *
       01  WS-GROUP-AREA.
           05 WS-CUR-VALUE-DT           PIC X(8) VALUE SPACE.
           05 WS-CUR-ORG-CD             PIC X(10) VALUE SPACE.
           05 WS-CUR-FCT-ID             PIC X(20) VALUE SPACE.
           05 WS-GROUP-INIT             PIC X VALUE "N".
              88 GROUP-STARTED               VALUE "Y".
           05 WS-GROUP-CNT              PIC 9(7) VALUE 0.
           05 WS-GROUP-AMT              PIC S9(15) VALUE 0.
      *
       01  WS-EDIT-AREA.
           05 WS-REPORT-ID              PIC X(20) VALUE SPACE.
           05 WS-DISP-AMT               PIC -ZZZ,ZZZ,ZZZ,ZZ9.
           05 WS-DISP-TOTAL             PIC -ZZZ,ZZZ,ZZZ,ZZ9.
           05 WS-DISP-CNT               PIC ZZZ,ZZ9.
           05 WS-DISP-LINE              PIC ZZZZZZ9.
           05 WS-STATUS-TEXT            PIC X(16) VALUE SPACE.
           05 WS-REASON-TEXT            PIC X(40) VALUE SPACE.
           05 WS-REPORT-TEXT            PIC X(200) VALUE SPACE.
      *
       01  WS-WORK-AMT                  PIC S9(15) VALUE 0.
       01  WS-ABS-AMT                   PIC 9(15) VALUE 0.
      *
       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE 0 TO RETURN-CODE
           PERFORM 1000-OPEN-FILES
           IF NOT HARD-ERROR
              PERFORM 2000-WRITE-HEADER
              PERFORM 3000-PROCESS-DETAILS
              IF GROUP-STARTED
                 PERFORM 5200-WRITE-GROUP-TOTAL
              END-IF
              PERFORM 6000-WRITE-NOTES
           END-IF
           PERFORM 9000-CLOSE-FILES
           IF HARD-ERROR
              MOVE 8 TO RETURN-CODE
           ELSE
              MOVE 0 TO RETURN-CODE
           END-IF
           GOBACK.
      *
       1000-OPEN-FILES.
           OPEN INPUT CCDTLF
           IF WS-ST-CCDTLF NOT = "00"
              DISPLAY "CCDTLF オープン失敗 ST=" WS-ST-CCDTLF
              SET HARD-ERROR TO TRUE
           END-IF
           OPEN OUTPUT CCRPTF
           IF WS-ST-CCRPTF NOT = "00"
              DISPLAY "CCRPTF オープン失敗 ST=" WS-ST-CCRPTF
              SET HARD-ERROR TO TRUE
           END-IF.
      *
       2000-WRITE-HEADER.
           MOVE SPACE TO WS-REPORT-TEXT
           STRING "受渡日明細帳票  作成バッチ=CC250B"
              DELIMITED BY SIZE INTO WS-REPORT-TEXT
           END-STRING
           MOVE "00000000" TO WS-CUR-VALUE-DT
           PERFORM 5100-WRITE-REPORT-LINE.
      *
       3000-PROCESS-DETAILS.
           PERFORM UNTIL END-CCDTLF OR HARD-ERROR
              READ CCDTLF
                 AT END
                    SET END-CCDTLF TO TRUE
                 NOT AT END
                    IF WS-ST-CCDTLF = "00"
                       ADD 1 TO WS-READ-DL-CNT
                       PERFORM 3100-VERIFY-DETAIL
                       IF NOT HARD-ERROR
                          IF VAL-FOUND AND FCT-FOUND
                             AND FC-FCT-STATUS-KBN = "01"
                             PERFORM 4000-ACCEPT-DETAIL
                          ELSE
                             PERFORM 4500-SKIP-DETAIL
                          END-IF
                       END-IF
                    ELSE
                       DISPLAY "CCDTLF 読込失敗 ST=" WS-ST-CCDTLF
                       SET HARD-ERROR TO TRUE
                    END-IF
              END-READ
           END-PERFORM.
      *
       3100-VERIFY-DETAIL.
           MOVE "N" TO WS-FOUND-VAL
           MOVE "N" TO WS-FOUND-FCT
           PERFORM 3200-FIND-VAL
           IF VAL-FOUND
              IF VL-FCT-ID = DL-FCT-ID
                 AND VL-VALUE-DT = DL-VALUE-DT
                 PERFORM 3300-FIND-FCT
              ELSE
                 MOVE "評価キー不整合" TO WS-REASON-TEXT
                 ADD 1 TO WS-SKIP-VAL-CNT
              END-IF
           ELSE
              MOVE "評価データ未登録" TO WS-REASON-TEXT
              ADD 1 TO WS-SKIP-VAL-CNT
           END-IF
           IF VAL-FOUND AND FCT-FOUND
              IF FC-FCT-STATUS-KBN NOT = "01"
                 MOVE "指図状態が確定対象外" TO WS-REASON-TEXT
                 ADD 1 TO WS-SKIP-STATUS-CNT
              END-IF
           END-IF.
      *
       3200-FIND-VAL.
           MOVE "N" TO WS-EOF-CCVALF
           OPEN INPUT CCVALF
           IF WS-ST-CCVALF NOT = "00"
              DISPLAY "CCVALF オープン失敗 ST=" WS-ST-CCVALF
              SET HARD-ERROR TO TRUE
           END-IF
           PERFORM UNTIL END-CCVALF OR VAL-FOUND OR HARD-ERROR
              READ CCVALF
                 AT END
                    SET END-CCVALF TO TRUE
                 NOT AT END
                    IF WS-ST-CCVALF = "00"
                       IF VL-VAL-ID = DL-VAL-ID
                          SET VAL-FOUND TO TRUE
                       END-IF
                    ELSE
                       DISPLAY "CCVALF 読込失敗 ST=" WS-ST-CCVALF
                       SET HARD-ERROR TO TRUE
                    END-IF
              END-READ
           END-PERFORM
           CLOSE CCVALF
           IF WS-ST-CCVALF NOT = "00"
              DISPLAY "CCVALF クローズ失敗 ST=" WS-ST-CCVALF
              SET HARD-ERROR TO TRUE
           END-IF.
      *
       3300-FIND-FCT.
           MOVE "N" TO WS-EOF-CCFCTF
           OPEN INPUT CCFCTF
           IF WS-ST-CCFCTF NOT = "00"
              DISPLAY "CCFCTF オープン失敗 ST=" WS-ST-CCFCTF
              SET HARD-ERROR TO TRUE
           END-IF
           PERFORM UNTIL END-CCFCTF OR FCT-FOUND OR HARD-ERROR
              READ CCFCTF
                 AT END
                    SET END-CCFCTF TO TRUE
                 NOT AT END
                    IF WS-ST-CCFCTF = "00"
                       IF FC-FCT-ID = DL-FCT-ID
                          SET FCT-FOUND TO TRUE
                       END-IF
                    ELSE
                       DISPLAY "CCFCTF 読込失敗 ST=" WS-ST-CCFCTF
                       SET HARD-ERROR TO TRUE
                    END-IF
              END-READ
           END-PERFORM
           CLOSE CCFCTF
           IF WS-ST-CCFCTF NOT = "00"
              DISPLAY "CCFCTF クローズ失敗 ST=" WS-ST-CCFCTF
              SET HARD-ERROR TO TRUE
           END-IF
           IF NOT FCT-FOUND
              MOVE "指図データ未登録" TO WS-REASON-TEXT
              ADD 1 TO WS-SKIP-FCT-CNT
           END-IF.
      *
       4000-ACCEPT-DETAIL.
           IF NOT GROUP-STARTED
              PERFORM 4100-START-GROUP
           ELSE
              IF DL-VALUE-DT NOT = WS-CUR-VALUE-DT
                 OR DL-ORG-CD NOT = WS-CUR-ORG-CD
                 OR DL-FCT-ID NOT = WS-CUR-FCT-ID
                 PERFORM 5200-WRITE-GROUP-TOTAL
                 PERFORM 4100-START-GROUP
              END-IF
           END-IF
           MOVE DL-DETAIL-AMT TO WS-WORK-AMT
           ADD WS-WORK-AMT TO WS-GROUP-AMT
           ADD 1 TO WS-GROUP-CNT
           ADD 1 TO WS-OK-DL-CNT
           IF DL-DETAIL-AMT = ZERO
              ADD 1 TO WS-ZERO-DL-CNT
           END-IF
           PERFORM 4200-WRITE-DETAIL-LINE.
      *
       4100-START-GROUP.
           MOVE "Y" TO WS-GROUP-INIT
           MOVE DL-VALUE-DT TO WS-CUR-VALUE-DT
           MOVE DL-ORG-CD TO WS-CUR-ORG-CD
           MOVE DL-FCT-ID TO WS-CUR-FCT-ID
           MOVE ZERO TO WS-GROUP-CNT
           MOVE ZERO TO WS-GROUP-AMT.
      *
       4200-WRITE-DETAIL-LINE.
           PERFORM 4300-EDIT-STATUS
           MOVE DL-DETAIL-AMT TO WS-DISP-AMT
           MOVE SPACE TO WS-REPORT-TEXT
           STRING
              "明細 受渡日=" DELIMITED BY SIZE
              DL-VALUE-DT DELIMITED BY SIZE
              " 組織=" DELIMITED BY SIZE
              DL-ORG-CD DELIMITED BY SIZE
              " 指図=" DELIMITED BY SIZE
              DL-FCT-ID DELIMITED BY SIZE
              " 評価=" DELIMITED BY SIZE
              DL-VAL-ID DELIMITED BY SIZE
              " 金額=" DELIMITED BY SIZE
              WS-DISP-AMT DELIMITED BY SIZE
              " 状態=" DELIMITED BY SIZE
              WS-STATUS-TEXT DELIMITED BY SIZE
              INTO WS-REPORT-TEXT
           END-STRING
           PERFORM 5100-WRITE-REPORT-LINE.
      *
       4300-EDIT-STATUS.
           EVALUATE DL-DETAIL-STATUS-KBN
              WHEN "01"
                 MOVE "正常" TO WS-STATUS-TEXT
              WHEN "08"
                 MOVE "保留" TO WS-STATUS-TEXT
              WHEN "09"
                 MOVE "取消" TO WS-STATUS-TEXT
              WHEN OTHER
                 MOVE "状態未定義" TO WS-STATUS-TEXT
           END-EVALUATE.
      *
       4500-SKIP-DETAIL.
           ADD 1 TO WS-SKIP-DL-CNT.
      *
       5100-WRITE-REPORT-LINE.
           ADD 1 TO WS-RP-LINE-NO
           ADD 1 TO WS-RP-ID-SEQ
           MOVE SPACE TO CCRPTF-REC
           MOVE WS-RP-ID-SEQ TO WS-DISP-LINE
           STRING "CC250B-" WS-DISP-LINE
              DELIMITED BY SIZE INTO WS-REPORT-ID
           END-STRING
           MOVE WS-REPORT-ID TO RP-REPORT-ID
           MOVE WS-CUR-VALUE-DT TO RP-BASE-DT
           MOVE "DTL" TO RP-REPORT-KBN
           MOVE WS-RP-LINE-NO TO RP-LINE-NO
           MOVE WS-REPORT-TEXT TO RP-REPORT-TEXT
           WRITE CCRPTF-REC
           IF WS-ST-CCRPTF NOT = "00"
              DISPLAY "CCRPTF 書込失敗 ST=" WS-ST-CCRPTF
              SET HARD-ERROR TO TRUE
           END-IF.
      *
       5200-WRITE-GROUP-TOTAL.
           MOVE WS-GROUP-AMT TO WS-DISP-TOTAL
           MOVE WS-GROUP-CNT TO WS-DISP-CNT
           MOVE SPACE TO WS-REPORT-TEXT
           STRING
              "小計 受渡日=" DELIMITED BY SIZE
              WS-CUR-VALUE-DT DELIMITED BY SIZE
              " 組織=" DELIMITED BY SIZE
              WS-CUR-ORG-CD DELIMITED BY SIZE
              " 指図=" DELIMITED BY SIZE
              WS-CUR-FCT-ID DELIMITED BY SIZE
              " 件数=" DELIMITED BY SIZE
              WS-DISP-CNT DELIMITED BY SIZE
              " 金額=" DELIMITED BY SIZE
              WS-DISP-TOTAL DELIMITED BY SIZE
              INTO WS-REPORT-TEXT
           END-STRING
           PERFORM 5100-WRITE-REPORT-LINE.
      *
       6000-WRITE-NOTES.
           MOVE SPACE TO WS-REPORT-TEXT
           STRING
              "注記 集計対象外明細=" DELIMITED BY SIZE
              WS-SKIP-DL-CNT DELIMITED BY SIZE
              " 評価不整合=" DELIMITED BY SIZE
              WS-SKIP-VAL-CNT DELIMITED BY SIZE
              " 指図不整合=" DELIMITED BY SIZE
              WS-SKIP-FCT-CNT DELIMITED BY SIZE
              " 状態対象外=" DELIMITED BY SIZE
              WS-SKIP-STATUS-CNT DELIMITED BY SIZE
              INTO WS-REPORT-TEXT
           END-STRING
           PERFORM 5100-WRITE-REPORT-LINE
           MOVE SPACE TO WS-REPORT-TEXT
           STRING
              "注記 読込明細=" DELIMITED BY SIZE
              WS-READ-DL-CNT DELIMITED BY SIZE
              " 出力明細=" DELIMITED BY SIZE
              WS-OK-DL-CNT DELIMITED BY SIZE
              " ゼロ金額明細=" DELIMITED BY SIZE
              WS-ZERO-DL-CNT DELIMITED BY SIZE
              INTO WS-REPORT-TEXT
           END-STRING
           PERFORM 5100-WRITE-REPORT-LINE.
      *
       9000-CLOSE-FILES.
           CLOSE CCDTLF
           IF WS-ST-CCDTLF NOT = "00"
              DISPLAY "CCDTLF クローズ失敗 ST=" WS-ST-CCDTLF
              SET HARD-ERROR TO TRUE
           END-IF
           CLOSE CCRPTF
           IF WS-ST-CCRPTF NOT = "00"
              DISPLAY "CCRPTF クローズ失敗 ST=" WS-ST-CCRPTF
              SET HARD-ERROR TO TRUE
           END-IF.
