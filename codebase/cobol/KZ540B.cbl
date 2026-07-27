       IDENTIFICATION DIVISION.
       PROGRAM-ID. KZ540B.
      ******************************************************************
      * 変更履歴
      * 版数  年月日    担当    概要
      * 1.00  20260401  KZB01   初版作成
      * 1.01  20260510  KZB02   回収案件更新監査追加
      * 1.02  20260619  KZB03   支店別担当者経路対応
      ******************************************************************
      * 回収案件登録バッチ
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZDLRF
               ASSIGN TO "KZDLRF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-KZDLRF.
           SELECT KZDLQF
               ASSIGN TO "KZDLQF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-KZDLQF.
           SELECT KZCLCF
               ASSIGN TO "KZCLCF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CLC-ACCT-NO
               FILE STATUS IS FS-KZCLCF.
           SELECT KZAUDF
               ASSIGN TO "KZAUDF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-KZAUDF.
           SELECT SYSIN
               ASSIGN TO "SYSIN"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-SYSIN.

       DATA DIVISION.
       FILE SECTION.
       FD  KZDLRF.
       COPY KZDLRFC.
       FD  KZDLQF.
       COPY KZDLQFC.
       FD  KZCLCF.
       COPY KZCLCCF.
       FD  KZAUDF.
       COPY KZAUDCF.
       FD  SYSIN.
       01  SYSIN-REC.
           05  SI-BRANCH-CODE          PIC X(03).
           05  SI-COLLECTOR-ID         PIC X(08).
           05  FILLER                  PIC X(69).

       WORKING-STORAGE SECTION.
       01  FILE-STATUS-AREA.
           05  FS-KZDLRF               PIC X(02).
           05  FS-KZDLQF               PIC X(02).
           05  FS-KZCLCF               PIC X(02).
           05  FS-KZAUDF               PIC X(02).
           05  FS-SYSIN                PIC X(02).

       01  END-FLAGS.
           05  SW-DR-END               PIC X VALUE "N".
               88  DR-END              VALUE "Y".
           05  SW-DQ-END               PIC X VALUE "N".
               88  DQ-END              VALUE "Y".
           05  SW-SI-END               PIC X VALUE "N".
               88  SI-END              VALUE "Y".

       01  CONTROL-AREA.
           05  WS-TODAY                PIC 9(08).
           05  WS-BRANCH-CODE          PIC X(03).
           05  WS-COLLECTOR-ID         PIC X(08).
           05  WS-NEW-PRIORITY         PIC X(01).
           05  WS-CHANGE-AMT           PIC S9(13)V99 COMP-3.
           05  WS-DQ-FOUND             PIC X VALUE "N".
               88  DQ-FOUND            VALUE "Y".
           05  WS-CASE-FOUND           PIC X VALUE "N".
               88  CASE-FOUND          VALUE "Y".
           05  WS-OPEN-CASE            PIC X VALUE "N".
               88  OPEN-CASE           VALUE "Y".
           05  WS-SCORE                PIC 9(09) COMP-3.
           05  WS-AMT-WEIGHT           PIC 9(09) COMP-3.
           05  WS-DAY-WEIGHT           PIC 9(09) COMP-3.

       01  COUNT-AREA.
           05  CNT-DR-READ             PIC 9(09) VALUE ZERO.
           05  CNT-DQ-READ             PIC 9(09) VALUE ZERO.
           05  CNT-INSERT              PIC 9(09) VALUE ZERO.
           05  CNT-UPDATE              PIC 9(09) VALUE ZERO.
           05  CNT-AUDIT               PIC 9(09) VALUE ZERO.
           05  CNT-SKIP                PIC 9(09) VALUE ZERO.
           05  CNT-ROUTE               PIC 9(04) VALUE ZERO.

       01  ROUTE-TABLE.
           05  RT-ENTRY OCCURS 200 TIMES.
               10  RT-BRANCH-CODE      PIC X(03).
               10  RT-COLLECTOR-ID     PIC X(08).

       01  ROUTE-IDX                   PIC 9(04) COMP.
       01  ROUTE-MAX                   PIC 9(04) COMP VALUE 200.

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE 0 TO RETURN-CODE
           ACCEPT WS-TODAY FROM DATE YYYYMMDD
           PERFORM 1000-OPEN-FILES
           PERFORM 2000-LOAD-ROUTE
           PERFORM 3000-READ-DQ
           PERFORM 3100-READ-DR
           PERFORM 4000-PROCESS UNTIL DR-END
           PERFORM 9000-CLOSE-FILES
           DISPLAY "KZ540B 正常終了"
           DISPLAY "KZDLRF 読込件数=" CNT-DR-READ
           DISPLAY "KZDLQF 読込件数=" CNT-DQ-READ
           DISPLAY "案件登録件数=" CNT-INSERT
           DISPLAY "案件更新件数=" CNT-UPDATE
           DISPLAY "監査出力件数=" CNT-AUDIT
           DISPLAY "対象外件数=" CNT-SKIP
           MOVE 0 TO RETURN-CODE
           GOBACK.

       1000-OPEN-FILES.
           OPEN INPUT KZDLRF
           IF FS-KZDLRF NOT = "00"
              DISPLAY "KZDLRF オープン失敗 ST=" FS-KZDLRF
              PERFORM 9900-ABEND
           END-IF
           OPEN INPUT KZDLQF
           IF FS-KZDLQF NOT = "00"
              DISPLAY "KZDLQF オープン失敗 ST=" FS-KZDLQF
              PERFORM 9900-ABEND
           END-IF
           OPEN I-O KZCLCF
           IF FS-KZCLCF NOT = "00"
              DISPLAY "KZCLCF オープン失敗 ST=" FS-KZCLCF
              PERFORM 9900-ABEND
           END-IF
           OPEN EXTEND KZAUDF
           IF FS-KZAUDF NOT = "00"
              DISPLAY "KZAUDF オープン失敗 ST=" FS-KZAUDF
              PERFORM 9900-ABEND
           END-IF
           OPEN INPUT SYSIN
           IF FS-SYSIN NOT = "00"
              DISPLAY "SYSIN オープン失敗 ST=" FS-SYSIN
              PERFORM 9900-ABEND
           END-IF.

       2000-LOAD-ROUTE.
           PERFORM UNTIL SI-END
              READ SYSIN
                 AT END
                    SET SI-END TO TRUE
                 NOT AT END
                    IF FS-SYSIN NOT = "00"
                       DISPLAY "SYSIN 読込失敗 ST=" FS-SYSIN
                       PERFORM 9900-ABEND
                    END-IF
                    IF SI-BRANCH-CODE NOT = SPACES
                       AND SI-COLLECTOR-ID NOT = SPACES
                       IF CNT-ROUTE < ROUTE-MAX
                          ADD 1 TO CNT-ROUTE
                          MOVE SI-BRANCH-CODE
                            TO RT-BRANCH-CODE(CNT-ROUTE)
                          MOVE SI-COLLECTOR-ID
                            TO RT-COLLECTOR-ID(CNT-ROUTE)
                       ELSE
                          DISPLAY "SYSIN 経路表件数超過"
                          PERFORM 9900-ABEND
                       END-IF
                    END-IF
              END-READ
           END-PERFORM
           IF CNT-ROUTE = ZERO
              DISPLAY "SYSIN 経路表未設定"
              PERFORM 9900-ABEND
           END-IF.

       3000-READ-DQ.
           READ KZDLQF
              AT END
                 SET DQ-END TO TRUE
              NOT AT END
                 IF FS-KZDLQF NOT = "00"
                    DISPLAY "KZDLQF 読込失敗 ST=" FS-KZDLQF
                    PERFORM 9900-ABEND
                 END-IF
                 ADD 1 TO CNT-DQ-READ
           END-READ.

       3100-READ-DR.
           READ KZDLRF
              AT END
                 SET DR-END TO TRUE
              NOT AT END
                 IF FS-KZDLRF NOT = "00"
                    DISPLAY "KZDLRF 読込失敗 ST=" FS-KZDLRF
                    PERFORM 9900-ABEND
                 END-IF
                 ADD 1 TO CNT-DR-READ
           END-READ.

       4000-PROCESS.
           IF DR-NEW-STATUS = "30"
              PERFORM 4100-VALIDATE-DR
              PERFORM 4200-FIND-DQ
              IF DQ-FOUND
                 PERFORM 4300-CALC-PRIORITY
                 PERFORM 4400-FIND-CASE
                 IF CASE-FOUND
                    IF OPEN-CASE
                       PERFORM 4700-UPDATE-OPEN-CASE
                    ELSE
                       ADD 1 TO CNT-SKIP
                    END-IF
                 ELSE
                    PERFORM 4500-SELECT-COLLECTOR
                    PERFORM 4600-INSERT-CASE
                 END-IF
              ELSE
                 DISPLAY "延滞残高未検出"
                 DISPLAY "口座=" DR-ACCT-NO
                 ADD 1 TO CNT-SKIP
              END-IF
           ELSE
              ADD 1 TO CNT-SKIP
           END-IF
           PERFORM 3100-READ-DR.

       4100-VALIDATE-DR.
           IF DR-ACCT-NO = SPACES
              DISPLAY "口座番号不正"
              PERFORM 9900-ABEND
           END-IF
           IF DR-NEW-STATUS NOT = "00" AND
              DR-NEW-STATUS NOT = "10" AND
              DR-NEW-STATUS NOT = "30"
              DISPLAY "延滞状態不正"
              DISPLAY "口座=" DR-ACCT-NO
              PERFORM 9900-ABEND
           END-IF
           IF DR-AGING-BUCKET NOT = "B1" AND
              DR-AGING-BUCKET NOT = "B2" AND
              DR-AGING-BUCKET NOT = "B3" AND
              DR-AGING-BUCKET NOT = "B4"
              DISPLAY "経過区分不正"
              DISPLAY "口座=" DR-ACCT-NO
              PERFORM 9900-ABEND
           END-IF.

       4200-FIND-DQ.
           MOVE "N" TO WS-DQ-FOUND
           PERFORM UNTIL DQ-END OR DQ-ACCT-NO >= DR-ACCT-NO
              PERFORM 3000-READ-DQ
           END-PERFORM
           IF NOT DQ-END
              IF DQ-ACCT-NO = DR-ACCT-NO
                 IF DQ-CURR-STATUS = "30"
                    SET DQ-FOUND TO TRUE
                 ELSE
                    DISPLAY "延滞残高状態不一致"
                    DISPLAY "口座=" DR-ACCT-NO
                 END-IF
              END-IF
           END-IF.

       4300-CALC-PRIORITY.
           COMPUTE WS-AMT-WEIGHT =
              FUNCTION INTEGER(DQ-OVERDUE-AMT / 10000)
           COMPUTE WS-DAY-WEIGHT = DR-DAYS-OVERDUE * 3
           COMPUTE WS-SCORE = WS-AMT-WEIGHT + WS-DAY-WEIGHT
           EVALUATE TRUE
              WHEN WS-SCORE >= 500
                 MOVE "H" TO WS-NEW-PRIORITY
              WHEN WS-SCORE >= 200
                 MOVE "M" TO WS-NEW-PRIORITY
              WHEN OTHER
                 MOVE "L" TO WS-NEW-PRIORITY
           END-EVALUATE.

       4400-FIND-CASE.
           MOVE "N" TO WS-CASE-FOUND
           MOVE "N" TO WS-OPEN-CASE
           MOVE DR-ACCT-NO TO CLC-ACCT-NO
           READ KZCLCF KEY IS CLC-ACCT-NO
              INVALID KEY
                 IF FS-KZCLCF NOT = "23"
                    DISPLAY "KZCLCF 読込失敗 ST=" FS-KZCLCF
                    PERFORM 9900-ABEND
                 END-IF
              NOT INVALID KEY
                 IF FS-KZCLCF NOT = "00"
                    DISPLAY "KZCLCF 読込失敗 ST=" FS-KZCLCF
                    PERFORM 9900-ABEND
                 END-IF
                 SET CASE-FOUND TO TRUE
                 IF CLC-CASE-STATUS = "OP"
                    SET OPEN-CASE TO TRUE
                 END-IF
           END-READ.

       4500-SELECT-COLLECTOR.
           MOVE DR-ACCT-NO(1:3) TO WS-BRANCH-CODE
           MOVE SPACES TO WS-COLLECTOR-ID
           PERFORM VARYING ROUTE-IDX FROM 1 BY 1
              UNTIL ROUTE-IDX > CNT-ROUTE
                 OR WS-COLLECTOR-ID NOT = SPACES
              IF RT-BRANCH-CODE(ROUTE-IDX) = WS-BRANCH-CODE
                 MOVE RT-COLLECTOR-ID(ROUTE-IDX)
                   TO WS-COLLECTOR-ID
              END-IF
           END-PERFORM
           IF WS-COLLECTOR-ID = SPACES
              DISPLAY "支店経路未設定"
              DISPLAY "口座=" DR-ACCT-NO
              PERFORM 9900-ABEND
           END-IF.

       4600-INSERT-CASE.
           MOVE DR-ACCT-NO        TO CLC-ACCT-NO
           MOVE WS-TODAY          TO CLC-CASE-OPEN-DT
           MOVE "OP"              TO CLC-CASE-STATUS
           MOVE WS-COLLECTOR-ID   TO CLC-COLLECTOR-ID
           MOVE WS-NEW-PRIORITY   TO CLC-PRIORITY-CODE
           MOVE WS-TODAY          TO CLC-LAST-ACTION-DT
           WRITE KZCLCF-REC
              INVALID KEY
                 DISPLAY "KZCLCF 登録失敗 ST=" FS-KZCLCF
                 PERFORM 9900-ABEND
              NOT INVALID KEY
                 IF FS-KZCLCF NOT = "00"
                    DISPLAY "KZCLCF 登録失敗 ST=" FS-KZCLCF
                    PERFORM 9900-ABEND
                 END-IF
                 ADD 1 TO CNT-INSERT
                 MOVE DQ-OVERDUE-AMT TO WS-CHANGE-AMT
                 PERFORM 4800-WRITE-AUDIT
           END-WRITE.

       4700-UPDATE-OPEN-CASE.
           IF CLC-PRIORITY-CODE NOT = WS-NEW-PRIORITY
              MOVE WS-NEW-PRIORITY TO CLC-PRIORITY-CODE
              MOVE WS-TODAY        TO CLC-LAST-ACTION-DT
              REWRITE KZCLCF-REC
                 INVALID KEY
                    DISPLAY "KZCLCF 更新失敗 ST=" FS-KZCLCF
                    PERFORM 9900-ABEND
                 NOT INVALID KEY
                    IF FS-KZCLCF NOT = "00"
                       DISPLAY "KZCLCF 更新失敗 ST=" FS-KZCLCF
                       PERFORM 9900-ABEND
                    END-IF
                    ADD 1 TO CNT-UPDATE
                    MOVE DQ-OVERDUE-AMT TO WS-CHANGE-AMT
                    PERFORM 4800-WRITE-AUDIT
              END-REWRITE
           ELSE
              ADD 1 TO CNT-SKIP
           END-IF.

       4800-WRITE-AUDIT.
           MOVE DR-ACCT-NO      TO AUD-ACCT-NO
           MOVE WS-TODAY        TO AUD-EVENT-DT
           MOVE "CCU"           TO AUD-EVENT-TYPE
           MOVE DR-NEW-STATUS   TO AUD-OLD-STATUS
           MOVE "30"            TO AUD-NEW-STATUS
           MOVE WS-CHANGE-AMT   TO AUD-CHANGE-AMT
           WRITE KZAUDF-REC
           IF FS-KZAUDF NOT = "00"
              DISPLAY "KZAUDF 出力失敗 ST=" FS-KZAUDF
              PERFORM 9900-ABEND
           END-IF
           ADD 1 TO CNT-AUDIT.

       9000-CLOSE-FILES.
           CLOSE KZDLRF
           IF FS-KZDLRF NOT = "00"
              DISPLAY "KZDLRF クローズ失敗 ST=" FS-KZDLRF
              PERFORM 9900-ABEND
           END-IF
           CLOSE KZDLQF
           IF FS-KZDLQF NOT = "00"
              DISPLAY "KZDLQF クローズ失敗 ST=" FS-KZDLQF
              PERFORM 9900-ABEND
           END-IF
           CLOSE KZCLCF
           IF FS-KZCLCF NOT = "00"
              DISPLAY "KZCLCF クローズ失敗 ST=" FS-KZCLCF
              PERFORM 9900-ABEND
           END-IF
           CLOSE KZAUDF
           IF FS-KZAUDF NOT = "00"
              DISPLAY "KZAUDF クローズ失敗 ST=" FS-KZAUDF
              PERFORM 9900-ABEND
           END-IF
           CLOSE SYSIN
           IF FS-SYSIN NOT = "00"
              DISPLAY "SYSIN クローズ失敗 ST=" FS-SYSIN
              PERFORM 9900-ABEND
           END-IF.

       9900-ABEND.
           DISPLAY "KZ540B 異常終了"
           MOVE 12 TO RETURN-CODE
           GOBACK.
