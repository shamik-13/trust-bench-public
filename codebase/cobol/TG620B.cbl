       IDENTIFICATION DIVISION.
       PROGRAM-ID. TG620B.
      ******************************************************************
      * 変更履歴
      * 版数  年月日    担当                         概要
      * 1.00  令和08年01月10日 システム部 情報系チーム 新規作成
      * 1.01  令和08年03月18日 システム部 情報系チーム 返戻明細控除追加
      * 1.02  令和08年06月01日 システム部 対外系チーム 純額照合へ変更
      ******************************************************************
      * 交換尻照合バッチ
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TGCLRF
               ASSIGN TO "TGCLRF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-TGCLRF.
           SELECT TGZENF
               ASSIGN TO "TGZENF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-TGZENF.
           SELECT TGRTRF
               ASSIGN TO "TGRTRF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS RT-RETURN-KEY
               FILE STATUS IS FS-TGRTRF.
           SELECT TGNETCF
               ASSIGN TO "TGNETCF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS NC-COUNTER-BANK
               FILE STATUS IS FS-TGNETCF.
           SELECT TGBANKF
               ASSIGN TO "TGBANKF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS BK-COUNTER-BANK
               FILE STATUS IS FS-TGBANKF.
           SELECT TGTOTF
               ASSIGN TO "TGTOTF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-TGTOTF.
           SELECT TGERRF
               ASSIGN TO "TGERRF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-TGERRF.

       DATA DIVISION.
       FILE SECTION.
       FD  TGCLRF.
           COPY TGCLRFC.

       FD  TGZENF.
           COPY TGZENFC.

       FD  TGRTRF.
           COPY TGRTRFC.

       FD  TGNETCF.
           COPY TGNETCFC.

       FD  TGBANKF.
           COPY TGBANKFC.

       FD  TGTOTF.
           COPY TGTOTFC.

       FD  TGERRF.
           COPY TGERRFC.

       WORKING-STORAGE SECTION.
           COPY LK-NET-PARM.

       01  FS-AREA.
           05 FS-TGCLRF             PIC XX VALUE SPACES.
           05 FS-TGZENF             PIC XX VALUE SPACES.
           05 FS-TGRTRF             PIC XX VALUE SPACES.
           05 FS-TGNETCF            PIC XX VALUE SPACES.
           05 FS-TGBANKF            PIC XX VALUE SPACES.
           05 FS-TGTOTF             PIC XX VALUE SPACES.
           05 FS-TGERRF             PIC XX VALUE SPACES.

       01  END-FLAGS.
           05 SW-END-TGCLRF         PIC X VALUE "N".
           05 SW-END-TGZENF         PIC X VALUE "N".
           05 SW-END-TGRTRF         PIC X VALUE "N".

       01  CONST-AREA.
           05 C-SW-ON               PIC X VALUE "Y".
           05 C-SW-OFF              PIC X VALUE "N".
           05 C-ZEN-PAY             PIC XX VALUE "01".
           05 C-ZEN-RECV            PIC XX VALUE "02".
           05 C-ST-HELD             PIC X VALUE "H".
           05 C-FLAG-NOT            PIC X VALUE "N".
           05 C-BANK-ACTIVE         PIC X VALUE "1".
           05 C-TYPE-PAY            PIC X(03) VALUE "PAY".
           05 C-TYPE-RECV           PIC X(03) VALUE "RCV".
           05 C-TYPE-NET            PIC X(03) VALUE "NET".
           05 C-MATCH-OK            PIC X VALUE "0".
           05 C-MATCH-NG            PIC X VALUE "1".

       01  WORK-AREA.
           05 WS-IDX                PIC 9(04) COMP VALUE 0.
           05 WS-MAX-IDX            PIC 9(04) COMP VALUE 500.
           05 WS-HIT-IDX            PIC 9(04) COMP VALUE 0.
           05 WS-ENTRY-COUNT        PIC 9(04) COMP VALUE 0.
           05 WS-DIFF-AMT           PIC S9(15) COMP-3 VALUE 0.
           05 WS-CALC-COUNT         PIC 9(09) COMP-3 VALUE 0.
           05 WS-REASON-CD          PIC X(08) VALUE SPACES.
           05 WS-REASON-TEXT        PIC X(80) VALUE SPACES.
           05 WS-ERROR-FIELD        PIC X(30) VALUE SPACES.
           05 WS-RAW-AREA           PIC X(512) VALUE SPACES.

       01  AGG-TABLE.
           05 AGG-ENTRY OCCURS 500 TIMES.
              10 AGG-USED           PIC X VALUE "N".
              10 AGG-VALUE-DT       PIC 9(08) VALUE 0.
              10 AGG-BANK           PIC X(04) VALUE SPACES.
              10 AGG-PAY-COUNT      PIC 9(09) COMP-3 VALUE 0.
              10 AGG-RECV-COUNT     PIC 9(09) COMP-3 VALUE 0.
              10 AGG-RET-COUNT      PIC 9(09) COMP-3 VALUE 0.
              10 AGG-PAY-AMT        PIC S9(15) COMP-3 VALUE 0.
              10 AGG-RECV-AMT       PIC S9(15) COMP-3 VALUE 0.
              10 AGG-RET-AMT        PIC S9(15) COMP-3 VALUE 0.
              10 AGG-MATCH          PIC X VALUE "0".

       PROCEDURE DIVISION.
       MAIN-SECTION.
           PERFORM 1000-INIT
           IF RETURN-CODE NOT = 0
              GOBACK
           END-IF

           PERFORM 2000-LOAD-ZEN
           IF RETURN-CODE NOT = 0
              PERFORM 9000-CLOSE
              GOBACK
           END-IF

           PERFORM 3000-LOAD-RETURN
           IF RETURN-CODE NOT = 0
              PERFORM 9000-CLOSE
              GOBACK
           END-IF

           PERFORM 4000-CHECK-CLEARING
           IF RETURN-CODE NOT = 0
              PERFORM 9000-CLOSE
              GOBACK
           END-IF

           MOVE 0 TO RETURN-CODE
           PERFORM 9000-CLOSE
           GOBACK.

       1000-INIT.
           MOVE 0 TO RETURN-CODE
           INITIALIZE AGG-TABLE
           OPEN INPUT  TGCLRF
                       TGZENF
                       TGRTRF
                       TGNETCF
                       TGBANKF
                OUTPUT TGTOTF
                       TGERRF

           IF FS-TGCLRF NOT = "00"
              DISPLAY "TGCLRF オープン失敗 ST=" FS-TGCLRF
              MOVE 12 TO RETURN-CODE
           END-IF
           IF FS-TGZENF NOT = "00"
              DISPLAY "TGZENF オープン失敗 ST=" FS-TGZENF
              MOVE 12 TO RETURN-CODE
           END-IF
           IF FS-TGRTRF NOT = "00"
              DISPLAY "TGRTRF オープン失敗 ST=" FS-TGRTRF
              MOVE 12 TO RETURN-CODE
           END-IF
           IF FS-TGNETCF NOT = "00"
              DISPLAY "TGNETCF オープン失敗 ST=" FS-TGNETCF
              MOVE 12 TO RETURN-CODE
           END-IF
           IF FS-TGBANKF NOT = "00"
              DISPLAY "TGBANKF オープン失敗 ST=" FS-TGBANKF
              MOVE 12 TO RETURN-CODE
           END-IF
           IF FS-TGTOTF NOT = "00"
              DISPLAY "TGTOTF オープン失敗 ST=" FS-TGTOTF
              MOVE 12 TO RETURN-CODE
           END-IF
           IF FS-TGERRF NOT = "00"
              DISPLAY "TGERRF オープン失敗 ST=" FS-TGERRF
              MOVE 12 TO RETURN-CODE
           END-IF.

       2000-LOAD-ZEN.
           PERFORM UNTIL SW-END-TGZENF = C-SW-ON
              READ TGZENF
                 AT END
                    MOVE C-SW-ON TO SW-END-TGZENF
                 NOT AT END
                    IF FS-TGZENF = "00"
                       PERFORM 2100-ADD-ZEN
                    ELSE
                       DISPLAY "TGZENF 読込失敗 ST=" FS-TGZENF
                       MOVE 12 TO RETURN-CODE
                       MOVE C-SW-ON TO SW-END-TGZENF
                    END-IF
              END-READ
           END-PERFORM.

       2100-ADD-ZEN.
           IF ZE-ZEN-TYPE NOT = C-ZEN-PAY
              AND ZE-ZEN-TYPE NOT = C-ZEN-RECV
              MOVE "E-ZENTYP" TO WS-REASON-CD
              MOVE "全銀種別不正" TO WS-REASON-TEXT
              MOVE "ZE-ZEN-TYPE" TO WS-ERROR-FIELD
              MOVE TGZENF-REC TO WS-RAW-AREA
              PERFORM 8200-WRITE-ERROR
           ELSE
              PERFORM 7000-FIND-AGG-ZEN
              IF WS-HIT-IDX = 0
                 PERFORM 7100-ALLOC-AGG-ZEN
              END-IF
              IF WS-HIT-IDX > 0
                 IF ZE-ZEN-TYPE = C-ZEN-PAY
                    ADD 1 TO AGG-PAY-COUNT(WS-HIT-IDX)
                    ADD ZE-REMIT-AMT TO AGG-PAY-AMT(WS-HIT-IDX)
                 ELSE
                    ADD 1 TO AGG-RECV-COUNT(WS-HIT-IDX)
                    ADD ZE-REMIT-AMT TO AGG-RECV-AMT(WS-HIT-IDX)
                 END-IF
              END-IF
           END-IF.

       3000-LOAD-RETURN.
           PERFORM UNTIL SW-END-TGRTRF = C-SW-ON
              READ TGRTRF NEXT RECORD
                 AT END
                    MOVE C-SW-ON TO SW-END-TGRTRF
                 NOT AT END
                    IF FS-TGRTRF = "00"
                       PERFORM 3100-ADD-RETURN
                    ELSE
                       DISPLAY "TGRTRF 読込失敗 ST=" FS-TGRTRF
                       MOVE 12 TO RETURN-CODE
                       MOVE C-SW-ON TO SW-END-TGRTRF
                    END-IF
              END-READ
           END-PERFORM.

       3100-ADD-RETURN.
           PERFORM 7200-FIND-AGG-RETURN
           IF WS-HIT-IDX = 0
              PERFORM 7300-ALLOC-AGG-RETURN
           END-IF
           IF WS-HIT-IDX > 0
              ADD 1 TO AGG-RET-COUNT(WS-HIT-IDX)
              ADD RT-REMIT-AMT TO AGG-RET-AMT(WS-HIT-IDX)
              SUBTRACT RT-REMIT-AMT FROM AGG-PAY-AMT(WS-HIT-IDX)
           END-IF.

       4000-CHECK-CLEARING.
           PERFORM UNTIL SW-END-TGCLRF = C-SW-ON
              READ TGCLRF
                 AT END
                    MOVE C-SW-ON TO SW-END-TGCLRF
                 NOT AT END
                    IF FS-TGCLRF = "00"
                       PERFORM 4100-CHECK-ONE
                    ELSE
                       DISPLAY "TGCLRF 読込失敗 ST=" FS-TGCLRF
                       MOVE 12 TO RETURN-CODE
                       MOVE C-SW-ON TO SW-END-TGCLRF
                    END-IF
              END-READ
           END-PERFORM.

       4100-CHECK-ONE.
           PERFORM 7400-FIND-AGG-CLEAR
           IF WS-HIT-IDX = 0
              MOVE "E-NODATA" TO WS-REASON-CD
              MOVE "再集計対象明細なし" TO WS-REASON-TEXT
              MOVE "CL-COUNTER-BANK" TO WS-ERROR-FIELD
              MOVE TGCLRF-REC TO WS-RAW-AREA
              PERFORM 8200-WRITE-ERROR
              PERFORM 8400-WRITE-ZERO-TOTAL
           ELSE
              COMPUTE WS-CALC-COUNT =
                      AGG-PAY-COUNT(WS-HIT-IDX)
                    + AGG-RECV-COUNT(WS-HIT-IDX)
                    + AGG-RET-COUNT(WS-HIT-IDX)
              INITIALIZE LK-NET-PARM
              MOVE AGG-PAY-AMT(WS-HIT-IDX) TO LK-NET-PAY-AMT
              MOVE AGG-RECV-AMT(WS-HIT-IDX) TO LK-NET-RECV-AMT
              CALL "TG935S" USING LK-NET-PARM
              IF LK-NET-RET NOT = ZERO
                 MOVE "E-TG935" TO WS-REASON-CD
                 MOVE "純額計算異常" TO WS-REASON-TEXT
                 MOVE "LK-NET-RET" TO WS-ERROR-FIELD
                 MOVE TGCLRF-REC TO WS-RAW-AREA
                 PERFORM 8200-WRITE-ERROR
                 MOVE 8 TO RETURN-CODE
              ELSE
                 PERFORM 8300-WRITE-TOTALS
                 IF LK-NET-AMT NOT = CL-NET-AMT
                    SUBTRACT CL-NET-AMT FROM LK-NET-AMT
                       GIVING WS-DIFF-AMT
                    PERFORM 5000-DECIDE-REASON
                    PERFORM 8200-WRITE-ERROR
                 END-IF
              END-IF
           END-IF.

       5000-DECIDE-REASON.
           MOVE SPACES TO WS-REASON-CD
           MOVE SPACES TO WS-REASON-TEXT
           MOVE SPACES TO WS-ERROR-FIELD
           MOVE CL-COUNTER-BANK TO NC-COUNTER-BANK
           READ TGNETCF KEY IS NC-COUNTER-BANK
              INVALID KEY
                 MOVE "E-NOCFG" TO WS-REASON-CD
                 MOVE "ネット管理設定なし" TO WS-REASON-TEXT
                 MOVE "NC-COUNTER-BANK" TO WS-ERROR-FIELD
              NOT INVALID KEY
                 IF FS-TGNETCF NOT = "00"
                    MOVE "E-CFGRD" TO WS-REASON-CD
                    MOVE "ネット管理読込異常" TO WS-REASON-TEXT
                    MOVE "TGNETCF" TO WS-ERROR-FIELD
                 END-IF
           END-READ

           IF WS-REASON-CD = SPACES
              MOVE CL-COUNTER-BANK TO BK-COUNTER-BANK
              READ TGBANKF KEY IS BK-COUNTER-BANK
                 INVALID KEY
                    MOVE "E-NOBANK" TO WS-REASON-CD
                    MOVE "相手銀行マスタなし" TO WS-REASON-TEXT
                    MOVE "BK-COUNTER-BANK" TO WS-ERROR-FIELD
                 NOT INVALID KEY
                    IF FS-TGBANKF NOT = "00"
                       MOVE "E-BKRD" TO WS-REASON-CD
                       MOVE "マスタ読込異常" TO WS-REASON-TEXT
                       MOVE "TGBANKF" TO WS-ERROR-FIELD
                    END-IF
              END-READ
           END-IF

           IF WS-REASON-CD = SPACES
              IF BK-STATUS NOT = C-BANK-ACTIVE
                 MOVE "E-BKSTAT" TO WS-REASON-CD
                 MOVE "相手銀行状態無効" TO WS-REASON-TEXT
                 MOVE "BK-STATUS" TO WS-ERROR-FIELD
              ELSE
                 IF CL-SETTLE-DT < BK-VALID-FROM
                    OR CL-SETTLE-DT > BK-VALID-TO
                    MOVE "E-BKDATE" TO WS-REASON-CD
                    MOVE "相手銀行有効期間外" TO WS-REASON-TEXT
                    MOVE "BK-VALID-FROM" TO WS-ERROR-FIELD
                 END-IF
              END-IF
           END-IF

           IF WS-REASON-CD = SPACES
              IF CL-SETTLE-STATUS = C-ST-HELD
                 MOVE "E-CLHOLD" TO WS-REASON-CD
                 MOVE "交換尻保留状態" TO WS-REASON-TEXT
                 MOVE "CL-SETTLE-STATUS" TO WS-ERROR-FIELD
              ELSE
                 MOVE "E-DIFF" TO WS-REASON-CD
                 MOVE "交換尻純額差異" TO WS-REASON-TEXT
                 MOVE "CL-NET-AMT" TO WS-ERROR-FIELD
              END-IF
           END-IF

           MOVE TGCLRF-REC TO WS-RAW-AREA.

       7000-FIND-AGG-ZEN.
           MOVE 0 TO WS-HIT-IDX
           PERFORM VARYING WS-IDX FROM 1 BY 1
             UNTIL WS-IDX > WS-MAX-IDX OR WS-HIT-IDX > 0
              IF AGG-USED(WS-IDX) = C-SW-ON
                 AND AGG-VALUE-DT(WS-IDX) = ZE-VALUE-DT
                 AND AGG-BANK(WS-IDX) = ZE-COUNTER-BANK
                 MOVE WS-IDX TO WS-HIT-IDX
              END-IF
           END-PERFORM.

       7100-ALLOC-AGG-ZEN.
           PERFORM VARYING WS-IDX FROM 1 BY 1
             UNTIL WS-IDX > WS-MAX-IDX OR WS-HIT-IDX > 0
              IF AGG-USED(WS-IDX) NOT = C-SW-ON
                 MOVE C-SW-ON TO AGG-USED(WS-IDX)
                 MOVE ZE-VALUE-DT TO AGG-VALUE-DT(WS-IDX)
                 MOVE ZE-COUNTER-BANK TO AGG-BANK(WS-IDX)
                 MOVE WS-IDX TO WS-HIT-IDX
                 ADD 1 TO WS-ENTRY-COUNT
              END-IF
           END-PERFORM
           IF WS-HIT-IDX = 0
              DISPLAY "集計テーブル超過"
              MOVE 12 TO RETURN-CODE
           END-IF.

       7200-FIND-AGG-RETURN.
           MOVE 0 TO WS-HIT-IDX
           PERFORM VARYING WS-IDX FROM 1 BY 1
             UNTIL WS-IDX > WS-MAX-IDX OR WS-HIT-IDX > 0
              IF AGG-USED(WS-IDX) = C-SW-ON
                 AND AGG-VALUE-DT(WS-IDX) = RT-VALUE-DT
                 AND AGG-BANK(WS-IDX) = RT-COUNTER-BANK
                 MOVE WS-IDX TO WS-HIT-IDX
              END-IF
           END-PERFORM.

       7300-ALLOC-AGG-RETURN.
           PERFORM VARYING WS-IDX FROM 1 BY 1
             UNTIL WS-IDX > WS-MAX-IDX OR WS-HIT-IDX > 0
              IF AGG-USED(WS-IDX) NOT = C-SW-ON
                 MOVE C-SW-ON TO AGG-USED(WS-IDX)
                 MOVE RT-VALUE-DT TO AGG-VALUE-DT(WS-IDX)
                 MOVE RT-COUNTER-BANK TO AGG-BANK(WS-IDX)
                 MOVE WS-IDX TO WS-HIT-IDX
                 ADD 1 TO WS-ENTRY-COUNT
              END-IF
           END-PERFORM
           IF WS-HIT-IDX = 0
              DISPLAY "返戻集計テーブル超過"
              MOVE 12 TO RETURN-CODE
           END-IF.

       7400-FIND-AGG-CLEAR.
           MOVE 0 TO WS-HIT-IDX
           PERFORM VARYING WS-IDX FROM 1 BY 1
             UNTIL WS-IDX > WS-MAX-IDX OR WS-HIT-IDX > 0
              IF AGG-USED(WS-IDX) = C-SW-ON
                 AND AGG-VALUE-DT(WS-IDX) = CL-SETTLE-DT
                 AND AGG-BANK(WS-IDX) = CL-COUNTER-BANK
                 MOVE WS-IDX TO WS-HIT-IDX
              END-IF
           END-PERFORM.

       8200-WRITE-ERROR.
           INITIALIZE TGERRF-REC
           MOVE CL-SETTLE-DT TO ER-VALUE-DT
           MOVE ZERO TO ER-CENTER-SEQ
           MOVE WS-REASON-CD TO ER-ERROR-CD
           MOVE WS-ERROR-FIELD TO ER-ERROR-FIELD
           MOVE WS-REASON-TEXT TO ER-ERROR-TEXT
           MOVE WS-RAW-AREA TO ER-RAW-RECORD
           WRITE TGERRF-REC
           IF FS-TGERRF NOT = "00"
              DISPLAY "TGERRF 書込失敗 ST=" FS-TGERRF
              MOVE 12 TO RETURN-CODE
           END-IF.

       8300-WRITE-TOTALS.
           IF LK-NET-AMT = CL-NET-AMT
              MOVE C-MATCH-OK TO AGG-MATCH(WS-HIT-IDX)
           ELSE
              MOVE C-MATCH-NG TO AGG-MATCH(WS-HIT-IDX)
           END-IF

           INITIALIZE TGTOTF-REC
           MOVE CL-SETTLE-DT TO TT-VALUE-DT
           MOVE CL-COUNTER-BANK TO TT-COUNTER-BANK
           MOVE C-TYPE-PAY TO TT-TOTAL-TYPE
           MOVE AGG-PAY-COUNT(WS-HIT-IDX) TO TT-ITEM-COUNT
           MOVE AGG-PAY-AMT(WS-HIT-IDX) TO TT-TOTAL-AMT
           MOVE AGG-MATCH(WS-HIT-IDX) TO TT-MATCH-STATUS
           WRITE TGTOTF-REC
           IF FS-TGTOTF NOT = "00"
              DISPLAY "TGTOTF 仕向書込失敗 ST=" FS-TGTOTF
              MOVE 12 TO RETURN-CODE
           END-IF

           INITIALIZE TGTOTF-REC
           MOVE CL-SETTLE-DT TO TT-VALUE-DT
           MOVE CL-COUNTER-BANK TO TT-COUNTER-BANK
           MOVE C-TYPE-RECV TO TT-TOTAL-TYPE
           MOVE AGG-RECV-COUNT(WS-HIT-IDX) TO TT-ITEM-COUNT
           MOVE AGG-RECV-AMT(WS-HIT-IDX) TO TT-TOTAL-AMT
           MOVE AGG-MATCH(WS-HIT-IDX) TO TT-MATCH-STATUS
           WRITE TGTOTF-REC
           IF FS-TGTOTF NOT = "00"
              DISPLAY "TGTOTF 被仕向書込失敗 ST=" FS-TGTOTF
              MOVE 12 TO RETURN-CODE
           END-IF

           INITIALIZE TGTOTF-REC
           MOVE CL-SETTLE-DT TO TT-VALUE-DT
           MOVE CL-COUNTER-BANK TO TT-COUNTER-BANK
           MOVE C-TYPE-NET TO TT-TOTAL-TYPE
           MOVE WS-CALC-COUNT TO TT-ITEM-COUNT
           MOVE LK-NET-AMT TO TT-TOTAL-AMT
           MOVE AGG-MATCH(WS-HIT-IDX) TO TT-MATCH-STATUS
           WRITE TGTOTF-REC
           IF FS-TGTOTF NOT = "00"
              DISPLAY "TGTOTF 純額書込失敗 ST=" FS-TGTOTF
              MOVE 12 TO RETURN-CODE
           END-IF.

       8400-WRITE-ZERO-TOTAL.
           INITIALIZE TGTOTF-REC
           MOVE CL-SETTLE-DT TO TT-VALUE-DT
           MOVE CL-COUNTER-BANK TO TT-COUNTER-BANK
           MOVE C-TYPE-NET TO TT-TOTAL-TYPE
           MOVE ZERO TO TT-ITEM-COUNT
           MOVE ZERO TO TT-TOTAL-AMT
           MOVE C-MATCH-NG TO TT-MATCH-STATUS
           WRITE TGTOTF-REC
           IF FS-TGTOTF NOT = "00"
              DISPLAY "TGTOTF ゼロ純額書込失敗 ST=" FS-TGTOTF
              MOVE 12 TO RETURN-CODE
           END-IF.

       9000-CLOSE.
           CLOSE TGCLRF
                 TGZENF
                 TGRTRF
                 TGNETCF
                 TGBANKF
                 TGTOTF
                 TGERRF.
