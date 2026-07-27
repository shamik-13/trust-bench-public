       IDENTIFICATION DIVISION.
       PROGRAM-ID. CG220B.
      ******************************************************************
      * 変更履歴
      * 版数  年月日    担当  概要
      * 1.00  20240115  共通  初版作成
      * 1.01  20240520  共通  標準列挙値判定を追加
      * 1.02  20240930  共通  有効期間重複検出を追加
      ******************************************************************
      * 共通コード日次反映バッチ
      * 外部登録済み共通コード差分を基準日順に確認し、同一コード
      * の有効期間重複を検出してエラー出力する。
      ******************************************************************

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CGCODF ASSIGN TO "CGCODF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS GC-CODE-ID
               FILE STATUS IS WS-CGCODF-ST.

           SELECT CKERRF ASSIGN TO "CKERRF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-CKERRF-ST.

       DATA DIVISION.
       FILE SECTION.
       FD  CGCODF.
           COPY CGCODC.

       FD  CKERRF.
           COPY CKERRC.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05 WS-CGCODF-ST              PIC XX.
           05 WS-CKERRF-ST              PIC XX.

       01  WS-SWITCHES.
           05 WS-EOF-SW                 PIC X VALUE "N".
              88 WS-EOF                       VALUE "Y".
              88 WS-NOT-EOF                   VALUE "N".
           05 WS-HARD-ERR-SW            PIC X VALUE "N".
              88 WS-HARD-ERR                  VALUE "Y".
              88 WS-NORMAL                    VALUE "N".
           05 WS-VALID-REC-SW           PIC X VALUE "N".
              88 WS-VALID-REC                 VALUE "Y".
              88 WS-INVALID-REC               VALUE "N".
           05 WS-OVERLAP-SW             PIC X VALUE "N".
              88 WS-OVERLAP                   VALUE "Y".
              88 WS-NO-OVERLAP                VALUE "N".

       01  WS-COUNTERS.
           05 WS-READ-CNT               PIC 9(9) VALUE ZERO.
           05 WS-REWRITE-CNT            PIC 9(9) VALUE ZERO.
           05 WS-ERR-CNT                PIC 9(9) VALUE ZERO.
           05 WS-HIST-CNT               PIC 9(4) VALUE ZERO.
           05 WS-IDX                    PIC 9(4) VALUE ZERO.

       01  WS-WORK-DATE.
           05 WS-RUN-DATE               PIC 9(8) VALUE ZERO.
           05 WS-FROM-DT                PIC 9(8) VALUE ZERO.
           05 WS-TO-DT                  PIC 9(8) VALUE ZERO.

       01  WS-ERROR-WORK.
           05 WS-ERROR-SEQ              PIC 9(8) VALUE ZERO.
           05 WS-ERROR-ID-WK            PIC X(20) VALUE SPACE.
           05 WS-ERROR-CD-WK            PIC X(8) VALUE SPACE.
           05 WS-ERROR-KEY-WK           PIC X(64) VALUE SPACE.

       01  WS-CODE-HISTORY.
           05 WS-HIST-ENTRY OCCURS 5000 TIMES.
              10 WS-H-CODE-KBN          PIC X(16).
              10 WS-H-CODE-VALUE        PIC X(32).
              10 WS-H-CODE-ID           PIC X(40).
              10 WS-H-FROM-DT           PIC 9(8).
              10 WS-H-TO-DT             PIC 9(8).

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE 0 TO RETURN-CODE
           PERFORM 1000-INITIALIZE
           IF WS-NORMAL
               PERFORM 2000-PROCESS UNTIL WS-EOF OR WS-HARD-ERR
           END-IF
           PERFORM 9000-FINALIZE
           GOBACK.

       1000-INITIALIZE.
           SET WS-NOT-EOF TO TRUE
           SET WS-NORMAL TO TRUE
           MOVE FUNCTION CURRENT-DATE(1:8) TO WS-RUN-DATE

           OPEN I-O CGCODF
           IF WS-CGCODF-ST NOT = "00"
               DISPLAY "CGCODF オープン失敗 ST=" WS-CGCODF-ST
               SET WS-HARD-ERR TO TRUE
               MOVE 12 TO RETURN-CODE
           END-IF

           IF WS-NORMAL
               OPEN OUTPUT CKERRF
               IF WS-CKERRF-ST NOT = "00"
                   DISPLAY "CKERRF オープン失敗 ST=" WS-CKERRF-ST
                   SET WS-HARD-ERR TO TRUE
                   MOVE 12 TO RETURN-CODE
               END-IF
           END-IF

           IF WS-NORMAL
               MOVE LOW-VALUES TO GC-CODE-ID
               START CGCODF KEY IS NOT LESS THAN GC-CODE-ID
                   INVALID KEY
                       SET WS-EOF TO TRUE
                   NOT INVALID KEY
                       CONTINUE
               END-START
               IF WS-CGCODF-ST NOT = "00" AND WS-CGCODF-ST NOT = "10"
                   DISPLAY "CGCODF START失敗 ST=" WS-CGCODF-ST
                   SET WS-HARD-ERR TO TRUE
                   MOVE 12 TO RETURN-CODE
               END-IF
           END-IF.

       2000-PROCESS.
           READ CGCODF NEXT RECORD
               AT END
                   SET WS-EOF TO TRUE
               NOT AT END
                   ADD 1 TO WS-READ-CNT
                   PERFORM 2100-VALIDATE-RECORD
                   IF WS-VALID-REC
                       PERFORM 2200-CHECK-OVERLAP
                       IF WS-NO-OVERLAP
                           PERFORM 2300-STORE-HISTORY
                           PERFORM 2400-REWRITE-CODE
                       ELSE
                           MOVE "E0004" TO WS-ERROR-CD-WK
                           PERFORM 8000-WRITE-ERROR
                       END-IF
                   END-IF
           END-READ

           IF WS-CGCODF-ST NOT = "00" AND WS-CGCODF-ST NOT = "10"
               DISPLAY "CGCODF READ失敗 ST=" WS-CGCODF-ST
               SET WS-HARD-ERR TO TRUE
               MOVE 12 TO RETURN-CODE
           END-IF.

       2100-VALIDATE-RECORD.
           SET WS-VALID-REC TO TRUE
           MOVE ZERO TO WS-FROM-DT WS-TO-DT

           EVALUATE GC-CODE-KBN
               WHEN "CUST-ST"
               WHEN "GENDER"
               WHEN "MOVE-KB"
               WHEN "SEND-ST"
                   CONTINUE
               WHEN OTHER
                   MOVE "E0001" TO WS-ERROR-CD-WK
                   PERFORM 8000-WRITE-ERROR
                   SET WS-INVALID-REC TO TRUE
           END-EVALUATE

           IF WS-VALID-REC
               IF GC-CODE-VALUE = SPACE
                   MOVE "E0002" TO WS-ERROR-CD-WK
                   PERFORM 8000-WRITE-ERROR
                   SET WS-INVALID-REC TO TRUE
               END-IF
           END-IF

           IF WS-VALID-REC
               IF GC-CODE-NAME = SPACE
                   MOVE "E0003" TO WS-ERROR-CD-WK
                   PERFORM 8000-WRITE-ERROR
                   SET WS-INVALID-REC TO TRUE
               END-IF
           END-IF

           IF WS-VALID-REC
               IF GC-VALID-FROM-DT IS NUMERIC
                   MOVE GC-VALID-FROM-DT TO WS-FROM-DT
               ELSE
                   MOVE "E0005" TO WS-ERROR-CD-WK
                   PERFORM 8000-WRITE-ERROR
                   SET WS-INVALID-REC TO TRUE
               END-IF
           END-IF

           IF WS-VALID-REC
               IF GC-VALID-TO-DT IS NUMERIC
                   MOVE GC-VALID-TO-DT TO WS-TO-DT
               ELSE
                   MOVE "E0006" TO WS-ERROR-CD-WK
                   PERFORM 8000-WRITE-ERROR
                   SET WS-INVALID-REC TO TRUE
               END-IF
           END-IF

           IF WS-VALID-REC
               IF WS-FROM-DT = ZERO OR WS-TO-DT = ZERO
                   MOVE "E0007" TO WS-ERROR-CD-WK
                   PERFORM 8000-WRITE-ERROR
                   SET WS-INVALID-REC TO TRUE
               END-IF
           END-IF

           IF WS-VALID-REC
               IF WS-FROM-DT > WS-TO-DT
                   MOVE "E0008" TO WS-ERROR-CD-WK
                   PERFORM 8000-WRITE-ERROR
                   SET WS-INVALID-REC TO TRUE
               END-IF
           END-IF

           IF WS-VALID-REC
               EVALUATE GC-CODE-KBN
                   WHEN "CUST-ST"
                       IF GC-CODE-VALUE NOT = "0" AND
                          GC-CODE-VALUE NOT = "1" AND
                          GC-CODE-VALUE NOT = "2" AND
                          GC-CODE-VALUE NOT = "9"
                           MOVE "E0011" TO WS-ERROR-CD-WK
                           PERFORM 8000-WRITE-ERROR
                           SET WS-INVALID-REC TO TRUE
                       END-IF
                   WHEN "GENDER"
                       IF GC-CODE-VALUE NOT = "1" AND
                          GC-CODE-VALUE NOT = "2" AND
                          GC-CODE-VALUE NOT = "9"
                           MOVE "E0012" TO WS-ERROR-CD-WK
                           PERFORM 8000-WRITE-ERROR
                           SET WS-INVALID-REC TO TRUE
                       END-IF
                   WHEN "MOVE-KB"
                       IF GC-CODE-VALUE NOT = "01" AND
                          GC-CODE-VALUE NOT = "02" AND
                          GC-CODE-VALUE NOT = "03" AND
                          GC-CODE-VALUE NOT = "90"
                           MOVE "E0013" TO WS-ERROR-CD-WK
                           PERFORM 8000-WRITE-ERROR
                           SET WS-INVALID-REC TO TRUE
                       END-IF
                   WHEN "SEND-ST"
                       IF GC-CODE-VALUE NOT = "0" AND
                          GC-CODE-VALUE NOT = "1" AND
                          GC-CODE-VALUE NOT = "2" AND
                          GC-CODE-VALUE NOT = "8"
                           MOVE "E0014" TO WS-ERROR-CD-WK
                           PERFORM 8000-WRITE-ERROR
                           SET WS-INVALID-REC TO TRUE
                       END-IF
               END-EVALUATE
           END-IF.

       2200-CHECK-OVERLAP.
           SET WS-NO-OVERLAP TO TRUE
           PERFORM VARYING WS-IDX FROM 1 BY 1
               UNTIL WS-IDX > WS-HIST-CNT OR WS-OVERLAP
               IF WS-H-CODE-KBN(WS-IDX) = GC-CODE-KBN AND
                  WS-H-CODE-VALUE(WS-IDX) = GC-CODE-VALUE
                   IF WS-FROM-DT <= WS-H-TO-DT(WS-IDX) AND
                      WS-TO-DT >= WS-H-FROM-DT(WS-IDX)
                       SET WS-OVERLAP TO TRUE
                   END-IF
               END-IF
           END-PERFORM.

       2300-STORE-HISTORY.
           IF WS-HIST-CNT < 5000
               ADD 1 TO WS-HIST-CNT
               MOVE GC-CODE-KBN       TO WS-H-CODE-KBN(WS-HIST-CNT)
               MOVE GC-CODE-VALUE     TO WS-H-CODE-VALUE(WS-HIST-CNT)
               MOVE GC-CODE-ID        TO WS-H-CODE-ID(WS-HIST-CNT)
               MOVE WS-FROM-DT        TO WS-H-FROM-DT(WS-HIST-CNT)
               MOVE WS-TO-DT          TO WS-H-TO-DT(WS-HIST-CNT)
           ELSE
               MOVE "E0099" TO WS-ERROR-CD-WK
               PERFORM 8000-WRITE-ERROR
               DISPLAY "履歴作業領域不足 CODE-ID=" GC-CODE-ID
           END-IF.

       2400-REWRITE-CODE.
           REWRITE CGCODF-REC
               INVALID KEY
                   DISPLAY "CGCODF REWRITE失敗 ST=" WS-CGCODF-ST
                   SET WS-HARD-ERR TO TRUE
                   MOVE 12 TO RETURN-CODE
               NOT INVALID KEY
                   ADD 1 TO WS-REWRITE-CNT
           END-REWRITE.

       8000-WRITE-ERROR.
           ADD 1 TO WS-ERROR-SEQ
           ADD 1 TO WS-ERR-CNT
           MOVE SPACE TO CKERRF-REC
           MOVE WS-ERROR-SEQ TO WS-ERROR-ID-WK(13:8)
           MOVE "CG220B" TO WS-ERROR-ID-WK(1:6)
           MOVE WS-ERROR-ID-WK TO ER-ERROR-ID
           MOVE "CG220B" TO ER-SOURCE-PGM-ID
           MOVE ZERO TO ER-CIF-NO
           MOVE SPACE TO WS-ERROR-KEY-WK
           STRING GC-CODE-KBN DELIMITED BY SIZE
                  ":" DELIMITED BY SIZE
                  GC-CODE-VALUE DELIMITED BY SIZE
                  ":" DELIMITED BY SIZE
                  GC-CODE-ID DELIMITED BY SIZE
             INTO WS-ERROR-KEY-WK
           END-STRING
           MOVE WS-ERROR-KEY-WK TO ER-KEY-ID
           MOVE WS-ERROR-CD-WK TO ER-ERROR-CD
           MOVE WS-RUN-DATE TO ER-ERROR-DT
           WRITE CKERRF-REC
           IF WS-CKERRF-ST NOT = "00"
               DISPLAY "CKERRF WRITE失敗 ST=" WS-CKERRF-ST
               SET WS-HARD-ERR TO TRUE
               MOVE 12 TO RETURN-CODE
           END-IF.

       9000-FINALIZE.
           IF WS-CGCODF-ST NOT = SPACE
               CLOSE CGCODF
               IF WS-CGCODF-ST NOT = "00"
                   DISPLAY "CGCODF CLOSE失敗 ST=" WS-CGCODF-ST
                   SET WS-HARD-ERR TO TRUE
                   MOVE 12 TO RETURN-CODE
               END-IF
           END-IF

           IF WS-CKERRF-ST NOT = SPACE
               CLOSE CKERRF
               IF WS-CKERRF-ST NOT = "00"
                   DISPLAY "CKERRF CLOSE失敗 ST=" WS-CKERRF-ST
                   SET WS-HARD-ERR TO TRUE
                   MOVE 12 TO RETURN-CODE
               END-IF
           END-IF

           DISPLAY "CG220B 読込件数=" WS-READ-CNT
           DISPLAY "CG220B 更新件数=" WS-REWRITE-CNT
           DISPLAY "CG220B エラー件数=" WS-ERR-CNT

           IF WS-HARD-ERR
               IF RETURN-CODE = 0
                   MOVE 12 TO RETURN-CODE
               END-IF
           ELSE
               IF WS-ERR-CNT > ZERO
                   MOVE 8 TO RETURN-CODE
               ELSE
                   MOVE 0 TO RETURN-CODE
               END-IF
           END-IF.
