       IDENTIFICATION DIVISION.
       PROGRAM-ID. LR220B.
      ******************************************************************
      * 変更履歴
      * 版数  年月日    担当    概要
      * 1.00  20240315  BATCH   新規作成
      * 1.01  20240920  BATCH   保留更新判定を追加
      * 1.02  20250110  BATCH   帳票出力明細を整理
      ******************************************************************
      * 料率改定通知帳票作成
      * LFRVSF旧保険料とLFPRMF再計算保険料を突合し差額契約を出力
      ******************************************************************

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LFRVSF-FILE ASSIGN TO "LFRVSF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS FS-LFRVSF.
           SELECT LFPRMF-FILE ASSIGN TO "LFPRMF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS FS-LFPRMF.
           SELECT LFCNTF-FILE ASSIGN TO "LFCNTF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS CN-POL-NO
               FILE STATUS IS FS-LFCNTF.
           SELECT LRRPTF-FILE ASSIGN TO "LRRPTF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS RP-REPORT-ID
               FILE STATUS IS FS-LRRPTF.

       DATA DIVISION.
       FILE SECTION.
       FD  LFRVSF-FILE.
           COPY LFRVSFC.

       FD  LFPRMF-FILE.
           COPY LFPRMFC.

       FD  LFCNTF-FILE.
           COPY LFCNTFC.

       FD  LRRPTF-FILE.
           COPY LRRPTFC.

       WORKING-STORAGE SECTION.
       01  FILE-STATUS-AREA.
           05 FS-LFRVSF             PIC XX VALUE SPACE.
           05 FS-LFPRMF             PIC XX VALUE SPACE.
           05 FS-LFCNTF             PIC XX VALUE SPACE.
           05 FS-LRRPTF             PIC XX VALUE SPACE.

       01  SWITCH-AREA.
           05 SW-END-RV             PIC X VALUE "0".
              88 END-RV                  VALUE "1".
           05 SW-END-PR             PIC X VALUE "0".
              88 END-PR                  VALUE "1".
           05 SW-PR-FOUND           PIC X VALUE "0".
              88 PR-FOUND                VALUE "1".
           05 SW-HARD-ERR           PIC X VALUE "0".
              88 HARD-ERR                VALUE "1".
           05 SW-CONTRACT-OK        PIC X VALUE "0".
              88 CONTRACT-OK             VALUE "1".

       01  COUNTER-AREA.
           05 CNT-RV-READ           PIC 9(9) VALUE 0.
           05 CNT-PR-READ           PIC 9(9) VALUE 0.
           05 CNT-CN-READ           PIC 9(9) VALUE 0.
           05 CNT-RPT-WRITE         PIC 9(9) VALUE 0.
           05 CNT-RV-REWRITE        PIC 9(9) VALUE 0.
           05 CNT-HOLD              PIC 9(9) VALUE 0.
           05 CNT-SKIP              PIC 9(9) VALUE 0.
           05 CNT-ERR               PIC 9(9) VALUE 0.

       01  CONSTANT-AREA.
           05 CST-RC-NORMAL         PIC 99 VALUE 0.
           05 CST-RC-ERROR          PIC 99 VALUE 8.
           05 CST-RC-ABEND          PIC 99 VALUE 12.
           05 CST-STATUS-MIRYO      PIC X VALUE "0".
           05 CST-STATUS-SEIJO      PIC X VALUE "1".
           05 CST-STATUS-IJO        PIC X VALUE "9".
           05 CST-NOTICE-MISYUTSU   PIC X VALUE "0".
           05 CST-NOTICE-SYUTSU     PIC X VALUE "1".
           05 CST-NOTICE-HORYU      PIC X VALUE "7".
           05 CST-RPT-TYPE          PIC XX VALUE "RV".
           05 CST-RPT-STATUS        PIC X VALUE "0".

       01  WORK-AREA.
           05 WK-DIFF-AMT           PIC S9(11)V99 VALUE 0.
           05 WK-REPORT-SEQ         PIC 9(9) VALUE 0.
           05 WK-REPORT-ID          PIC X(20) VALUE SPACE.
           05 WK-REPORT-YM          PIC 9(6) VALUE 0.
           05 WK-LINE-NO            PIC 9 VALUE 0.
           05 WK-HOLD-REASON        PIC X(40) VALUE SPACE.

       01  PRM-TABLE-AREA.
           05 PRM-MAX               PIC 9(5) VALUE 20000.
           05 PRM-CNT               PIC 9(5) VALUE 0.
           05 PRM-IDX               PIC 9(5) VALUE 0.
           05 PRM-TBL OCCURS 20000 TIMES.
              10 T-PR-POL-NO        PIC X(20).
              10 T-PR-PRM-AMT       PIC 9(11)V99.
              10 T-PR-CALC-STATUS   PIC X.

       PROCEDURE DIVISION.
       MAIN-SECTION.
           PERFORM 0000-INIT
           IF NOT HARD-ERR
              PERFORM 1000-LOAD-PRM
           END-IF
           IF NOT HARD-ERR
              PERFORM 2000-PROCESS-RVS
           END-IF
           PERFORM 9000-FINAL
           GOBACK.

       0000-INIT.
           MOVE CST-RC-NORMAL TO RETURN-CODE

           OPEN I-O LFRVSF-FILE
           IF FS-LFRVSF NOT = "00"
              DISPLAY "LFRVSF オープン失敗 ST=" FS-LFRVSF
              MOVE CST-RC-ABEND TO RETURN-CODE
              SET HARD-ERR TO TRUE
           END-IF

           OPEN INPUT LFPRMF-FILE
           IF FS-LFPRMF NOT = "00"
              DISPLAY "LFPRMF オープン失敗 ST=" FS-LFPRMF
              MOVE CST-RC-ABEND TO RETURN-CODE
              SET HARD-ERR TO TRUE
           END-IF

           OPEN INPUT LFCNTF-FILE
           IF FS-LFCNTF NOT = "00"
              DISPLAY "LFCNTF オープン失敗 ST=" FS-LFCNTF
              MOVE CST-RC-ABEND TO RETURN-CODE
              SET HARD-ERR TO TRUE
           END-IF

           OPEN OUTPUT LRRPTF-FILE
           IF FS-LRRPTF NOT = "00"
              DISPLAY "LRRPTF オープン失敗 ST=" FS-LRRPTF
              MOVE CST-RC-ABEND TO RETURN-CODE
              SET HARD-ERR TO TRUE
           END-IF
           .

       1000-LOAD-PRM.
           PERFORM UNTIL END-PR OR HARD-ERR
              READ LFPRMF-FILE
                 AT END
                    SET END-PR TO TRUE
                 NOT AT END
                    ADD 1 TO CNT-PR-READ
                    IF PRM-CNT >= PRM-MAX
                       DISPLAY "LFPRMF 件数超過"
                       MOVE CST-RC-ABEND TO RETURN-CODE
                       SET HARD-ERR TO TRUE
                    ELSE
                       ADD 1 TO PRM-CNT
                       MOVE PR-POL-NO TO T-PR-POL-NO(PRM-CNT)
                       MOVE PR-PRM-AMT TO T-PR-PRM-AMT(PRM-CNT)
                       MOVE PR-CALC-STATUS-KBN
                         TO T-PR-CALC-STATUS(PRM-CNT)
                    END-IF
              END-READ

              IF FS-LFPRMF NOT = "00" AND FS-LFPRMF NOT = "10"
                 DISPLAY "LFPRMF 読込失敗 ST=" FS-LFPRMF
                 MOVE CST-RC-ABEND TO RETURN-CODE
                 SET HARD-ERR TO TRUE
              END-IF
           END-PERFORM
           .

       2000-PROCESS-RVS.
           PERFORM UNTIL END-RV OR HARD-ERR
              READ LFRVSF-FILE
                 AT END
                    SET END-RV TO TRUE
                 NOT AT END
                    ADD 1 TO CNT-RV-READ
                    PERFORM 2100-PROCESS-ONE-RV
              END-READ

              IF FS-LFRVSF NOT = "00" AND FS-LFRVSF NOT = "10"
                 DISPLAY "LFRVSF 読込失敗 ST=" FS-LFRVSF
                 MOVE CST-RC-ABEND TO RETURN-CODE
                 SET HARD-ERR TO TRUE
              END-IF
           END-PERFORM
           .

       2100-PROCESS-ONE-RV.
           MOVE "0" TO SW-PR-FOUND
           MOVE "0" TO SW-CONTRACT-OK
           MOVE SPACE TO WK-HOLD-REASON

           IF RV-NOTICE-STATUS-KBN NOT = CST-NOTICE-MISYUTSU
              ADD 1 TO CNT-SKIP
              EXIT PARAGRAPH
           END-IF

           PERFORM 2200-FIND-PRM
           IF NOT PR-FOUND
              MOVE "再計算保険料なし" TO WK-HOLD-REASON
              PERFORM 2400-HOLD-RVS
              EXIT PARAGRAPH
           END-IF

           IF T-PR-CALC-STATUS(PRM-IDX) = CST-STATUS-MIRYO
              MOVE "再計算未了" TO WK-HOLD-REASON
              PERFORM 2400-HOLD-RVS
              EXIT PARAGRAPH
           END-IF

           IF T-PR-CALC-STATUS(PRM-IDX) = CST-STATUS-IJO
              MOVE "再計算異常終了" TO WK-HOLD-REASON
              PERFORM 2400-HOLD-RVS
              EXIT PARAGRAPH
           END-IF

           IF T-PR-CALC-STATUS(PRM-IDX) NOT = CST-STATUS-SEIJO
              MOVE "再計算状態不正" TO WK-HOLD-REASON
              PERFORM 2400-HOLD-RVS
              EXIT PARAGRAPH
           END-IF

           PERFORM 2300-READ-CONTRACT
           IF HARD-ERR
              EXIT PARAGRAPH
           END-IF

           IF NOT CONTRACT-OK
              PERFORM 2400-HOLD-RVS
              EXIT PARAGRAPH
           END-IF

           SUBTRACT RV-OLD-PRM-AMT FROM T-PR-PRM-AMT(PRM-IDX)
              GIVING WK-DIFF-AMT

           IF WK-DIFF-AMT = 0
              ADD 1 TO CNT-SKIP
           ELSE
              MOVE T-PR-PRM-AMT(PRM-IDX) TO RV-NEW-PRM-AMT
              PERFORM 2500-WRITE-NOTICE
              IF NOT HARD-ERR
                 MOVE CST-NOTICE-SYUTSU TO RV-NOTICE-STATUS-KBN
                 PERFORM 2600-REWRITE-RVS
              END-IF
           END-IF
           .

       2200-FIND-PRM.
           MOVE "0" TO SW-PR-FOUND

           PERFORM VARYING PRM-IDX FROM 1 BY 1
              UNTIL PRM-IDX > PRM-CNT OR PR-FOUND
              IF T-PR-POL-NO(PRM-IDX) = RV-POL-NO
                 SET PR-FOUND TO TRUE
              END-IF
           END-PERFORM
           .

       2300-READ-CONTRACT.
           MOVE "0" TO SW-CONTRACT-OK
           MOVE RV-POL-NO TO CN-POL-NO

           READ LFCNTF-FILE
              INVALID KEY
                 MOVE "契約情報なし" TO WK-HOLD-REASON
              NOT INVALID KEY
                 ADD 1 TO CNT-CN-READ
                 IF CN-PAY-METHOD-KBN = SPACE
                    MOVE "払込方法区分不正" TO WK-HOLD-REASON
                 ELSE
                    IF CN-NEXT-DUE-YM < RV-NOTICE-YM
                       MOVE "次回応当年月不整合" TO WK-HOLD-REASON
                    ELSE
                       SET CONTRACT-OK TO TRUE
                    END-IF
                 END-IF
           END-READ

           IF FS-LFCNTF NOT = "00" AND FS-LFCNTF NOT = "23"
              DISPLAY "LFCNTF 読込失敗 ST=" FS-LFCNTF
              MOVE CST-RC-ABEND TO RETURN-CODE
              SET HARD-ERR TO TRUE
           END-IF
           .

       2400-HOLD-RVS.
           MOVE CST-NOTICE-HORYU TO RV-NOTICE-STATUS-KBN
           PERFORM 2600-REWRITE-RVS

           IF NOT HARD-ERR
              ADD 1 TO CNT-HOLD
              DISPLAY "通知保留 POL=" RV-POL-NO
                      " 理由=" WK-HOLD-REASON
           END-IF
           .

       2500-WRITE-NOTICE.
           MOVE RV-NOTICE-YM TO WK-REPORT-YM

           PERFORM 2510-WRITE-RPT-LINE
              VARYING WK-LINE-NO FROM 1 BY 1
              UNTIL WK-LINE-NO > 4 OR HARD-ERR
           .

       2510-WRITE-RPT-LINE.
           ADD 1 TO WK-REPORT-SEQ
           MOVE SPACES TO LRRPTF-REC
           MOVE SPACES TO WK-REPORT-ID
           MOVE "LR220B-" TO WK-REPORT-ID(1:7)
           MOVE WK-REPORT-SEQ TO WK-REPORT-ID(12:9)

           MOVE WK-REPORT-ID TO RP-REPORT-ID
           MOVE WK-REPORT-YM TO RP-REPORT-YM
           MOVE CST-RPT-TYPE TO RP-REPORT-TYPE-KBN
           MOVE WK-LINE-NO TO RP-LINE-NO
           MOVE RV-POL-NO TO RP-POL-NO
           MOVE CST-RPT-STATUS TO RP-OUTPUT-STATUS-KBN

           EVALUATE WK-LINE-NO
              WHEN 1
                 MOVE RV-OLD-PRM-AMT TO RP-PRINT-AMT
              WHEN 2
                 MOVE T-PR-PRM-AMT(PRM-IDX) TO RP-PRINT-AMT
              WHEN 3
                 MOVE RV-NOTICE-YM TO RP-PRINT-AMT
              WHEN OTHER
                 MOVE WK-DIFF-AMT TO RP-PRINT-AMT
           END-EVALUATE

           WRITE LRRPTF-REC
           IF FS-LRRPTF = "00"
              ADD 1 TO CNT-RPT-WRITE
           ELSE
              DISPLAY "LRRPTF 書込失敗 ST=" FS-LRRPTF
              MOVE CST-RC-ABEND TO RETURN-CODE
              SET HARD-ERR TO TRUE
           END-IF
           .

       2600-REWRITE-RVS.
           REWRITE LFRVSF-REC
           IF FS-LFRVSF = "00"
              ADD 1 TO CNT-RV-REWRITE
           ELSE
              DISPLAY "LFRVSF 更新失敗 ST=" FS-LFRVSF
              MOVE CST-RC-ABEND TO RETURN-CODE
              SET HARD-ERR TO TRUE
           END-IF
           .

       9000-FINAL.
           IF FS-LRRPTF NOT = SPACE
              CLOSE LRRPTF-FILE
              IF FS-LRRPTF NOT = "00"
                 DISPLAY "LRRPTF クローズ失敗 ST=" FS-LRRPTF
                 MOVE CST-RC-ERROR TO RETURN-CODE
              END-IF
           END-IF

           IF FS-LFCNTF NOT = SPACE
              CLOSE LFCNTF-FILE
              IF FS-LFCNTF NOT = "00"
                 DISPLAY "LFCNTF クローズ失敗 ST=" FS-LFCNTF
                 MOVE CST-RC-ERROR TO RETURN-CODE
              END-IF
           END-IF

           IF FS-LFPRMF NOT = SPACE
              CLOSE LFPRMF-FILE
              IF FS-LFPRMF NOT = "00"
                 DISPLAY "LFPRMF クローズ失敗 ST=" FS-LFPRMF
                 MOVE CST-RC-ERROR TO RETURN-CODE
              END-IF
           END-IF

           IF FS-LFRVSF NOT = SPACE
              CLOSE LFRVSF-FILE
              IF FS-LFRVSF NOT = "00"
                 DISPLAY "LFRVSF クローズ失敗 ST=" FS-LFRVSF
                 MOVE CST-RC-ERROR TO RETURN-CODE
              END-IF
           END-IF

           DISPLAY "LR220B 処理件数 RV=" CNT-RV-READ
                   " PR=" CNT-PR-READ
                   " CN=" CNT-CN-READ
                   " RPT=" CNT-RPT-WRITE
                   " 保留=" CNT-HOLD
                   " 対象外=" CNT-SKIP

           IF HARD-ERR
              IF RETURN-CODE < CST-RC-ABEND
                 MOVE CST-RC-ABEND TO RETURN-CODE
              END-IF
           ELSE
              IF RETURN-CODE NOT = CST-RC-NORMAL
                 ADD 1 TO CNT-ERR
              ELSE
                 MOVE CST-RC-NORMAL TO RETURN-CODE
              END-IF
           END-IF
           .
