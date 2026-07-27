       IDENTIFICATION DIVISION.
       PROGRAM-ID. CB210B.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CDREVF
               ASSIGN       TO "CDREVF"
               ORGANIZATION IS INDEXED
               ACCESS MODE  IS RANDOM
               RECORD KEY   IS RV-CARD-NO
               FILE STATUS  IS FS-CDREVF.

           SELECT CDRBALF
               ASSIGN       TO "CDRBALF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE  IS SEQUENTIAL
               FILE STATUS  IS FS-CDRBALF.

           SELECT CDRSLDF
               ASSIGN       TO "CDRSLDF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE  IS SEQUENTIAL
               FILE STATUS  IS FS-CDRSLDF.

       DATA DIVISION.
       FILE SECTION.
       FD  CDREVF.
           COPY CDREVFC.

       FD  CDRBALF.
           COPY CDRBALFC.

       FD  CDRSLDF.
           COPY CDRSLDFC.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05  FS-CDREVF              PIC X(02).
           05  FS-CDRBALF             PIC X(02).
           05  FS-CDRSLDF             PIC X(02).

       01  WS-CONST.
           05  C-PROGRAM-ID           PIC X(08) VALUE "CB210B".
           05  C-REV-ST-BILLABLE      PIC X(02) VALUE "01".
           05  C-REV-ST-SUSPEND       PIC X(02) VALUE "02".
           05  C-REV-ST-CANCEL        PIC X(02) VALUE "03".
           05  C-RSLD-COMPLETE        PIC X(01) VALUE "C".
           05  C-RSLD-SKIP            PIC X(01) VALUE "S".
           05  C-RET-NORMAL           PIC X(02) VALUE "00".
           05  C-MONTH-RATE           PIC 9V9(04) VALUE 0.0125.

       01  WS-SWITCH.
           05  WS-EOF-SW              PIC X(01) VALUE "N".
               88  CDRBALF-EOF                  VALUE "Y".
               88  CDRBALF-NOT-EOF              VALUE "N".
           05  WS-ABEND-SW            PIC X(01) VALUE "N".
               88  ABEND-ON                     VALUE "Y".
               88  ABEND-OFF                    VALUE "N".

       01  WS-WORK.
           05  WS-FEE-AMT             PIC 9(13) VALUE ZERO.
           05  WS-TOTAL-IN            PIC 9(09) VALUE ZERO.
           05  WS-TOTAL-COMPLETE      PIC 9(09) VALUE ZERO.
           05  WS-TOTAL-SKIP          PIC 9(09) VALUE ZERO.
           05  WS-TOTAL-ERROR         PIC 9(09) VALUE ZERO.

       01  WS-MESSAGE.
           05  WS-MSG-TEXT            PIC X(80).

           COPY LK-SLIDE-PARM.
           COPY LK-RSLED-PARM.

       PROCEDURE DIVISION.
       0000-MAIN SECTION.
      *    残高スライド算定は本バッチとCB290Sが正とする。
      *    オーソリRPG及び会員サービスJavaでは元金額を算定しない。
           PERFORM 1000-INIT
           IF ABEND-OFF
               PERFORM 2000-MAIN-PROCESS
                   UNTIL CDRBALF-EOF OR ABEND-ON
           END-IF
           PERFORM 9000-FINAL
           GOBACK.

       1000-INIT SECTION.
           SET ABEND-OFF TO TRUE
           SET CDRBALF-NOT-EOF TO TRUE
           MOVE 0 TO RETURN-CODE
           OPEN INPUT  CDREVF
                INPUT  CDRBALF
                OUTPUT CDRSLDF

           IF FS-CDREVF NOT = "00"
               MOVE "CDREVF オープン失敗 ST=" TO WS-MSG-TEXT
               DISPLAY WS-MSG-TEXT FS-CDREVF
               PERFORM 9100-SET-ABEND
           END-IF

           IF FS-CDRBALF NOT = "00"
               MOVE "CDRBALF オープン失敗 ST=" TO WS-MSG-TEXT
               DISPLAY WS-MSG-TEXT FS-CDRBALF
               PERFORM 9100-SET-ABEND
           END-IF

           IF FS-CDRSLDF NOT = "00"
               MOVE "CDRSLDF オープン失敗 ST=" TO WS-MSG-TEXT
               DISPLAY WS-MSG-TEXT FS-CDRSLDF
               PERFORM 9100-SET-ABEND
           END-IF

           IF ABEND-OFF
               PERFORM 2100-READ-BALANCE
           END-IF.

       2000-MAIN-PROCESS SECTION.
           ADD 1 TO WS-TOTAL-IN
           PERFORM 2200-VALIDATE-BALANCE

           IF ABEND-OFF
               PERFORM 2300-READ-REVOLVING
           END-IF

           IF ABEND-OFF
               EVALUATE RV-REV-STATUS
                   WHEN C-REV-ST-BILLABLE
                       PERFORM 3000-PROCESS-BILLABLE
                   WHEN C-REV-ST-SUSPEND
                       PERFORM 4000-WRITE-SKIP
                   WHEN C-REV-ST-CANCEL
                       PERFORM 4000-WRITE-SKIP
                   WHEN OTHER
                       DISPLAY "口座状態不正 CARD=" RB-CARD-NO
                           " STATUS=" RV-REV-STATUS
                       PERFORM 9100-SET-ABEND
               END-EVALUATE
           END-IF

           IF ABEND-OFF
               PERFORM 2100-READ-BALANCE
           END-IF.

       2100-READ-BALANCE SECTION.
           READ CDRBALF
               AT END
                   SET CDRBALF-EOF TO TRUE
               NOT AT END
                   CONTINUE
           END-READ

           IF FS-CDRBALF NOT = "00" AND FS-CDRBALF NOT = "10"
               DISPLAY "CDRBALF 読込失敗 ST=" FS-CDRBALF
               PERFORM 9100-SET-ABEND
           END-IF.

       2200-VALIDATE-BALANCE SECTION.
           IF RB-CARD-NO = SPACE
               DISPLAY "カード番号未設定 CDRBALF"
               PERFORM 9100-SET-ABEND
           END-IF

           IF RB-CYCLE-DT NOT NUMERIC
               DISPLAY "締日不正 CARD=" RB-CARD-NO
               PERFORM 9100-SET-ABEND
           END-IF

           IF RB-REV-BAL-AMT NOT NUMERIC
               DISPLAY "リボ残高不正 CARD=" RB-CARD-NO
               PERFORM 9100-SET-ABEND
           END-IF

           IF RB-CARRIED-FEE-AMT NOT NUMERIC
               DISPLAY "繰越手数料不正 CARD=" RB-CARD-NO
               PERFORM 9100-SET-ABEND
           END-IF

           IF RB-NEW-REV-AMT NOT NUMERIC
               DISPLAY "新規リボ額不正 CARD=" RB-CARD-NO
               PERFORM 9100-SET-ABEND
           END-IF.

       2300-READ-REVOLVING SECTION.
           MOVE RB-CARD-NO TO RV-CARD-NO
           READ CDREVF
               INVALID KEY
                   DISPLAY "CDREVF 該当なし CARD=" RB-CARD-NO
                   PERFORM 9100-SET-ABEND
               NOT INVALID KEY
                   CONTINUE
           END-READ

           IF FS-CDREVF NOT = "00" AND FS-CDREVF NOT = "23"
               DISPLAY "CDREVF 読込失敗 ST=" FS-CDREVF
                   " CARD=" RB-CARD-NO
               PERFORM 9100-SET-ABEND
           END-IF

           IF ABEND-OFF
               PERFORM 2400-VALIDATE-REVOLVING
           END-IF.

       2400-VALIDATE-REVOLVING SECTION.
           IF RV-CARD-NO NOT = RB-CARD-NO
               DISPLAY "カード番号突合不一致 BAL=" RB-CARD-NO
                   " REV=" RV-CARD-NO
               PERFORM 9100-SET-ABEND
           END-IF

           IF RV-MEMBER-ID = SPACE
               DISPLAY "会員番号未設定 CARD=" RB-CARD-NO
               PERFORM 9100-SET-ABEND
           END-IF

           IF RV-REV-COURSE-CD = SPACE
               DISPLAY "リボコース未設定 CARD=" RB-CARD-NO
               PERFORM 9100-SET-ABEND
           END-IF

           IF RV-REV-START-DT NOT NUMERIC
               DISPLAY "リボ開始日不正 CARD=" RB-CARD-NO
               PERFORM 9100-SET-ABEND
           END-IF.

       3000-PROCESS-BILLABLE SECTION.
           INITIALIZE LK-SLIDE-PARM
           MOVE RB-REV-BAL-AMT TO LK-SL-REV-BAL
           CALL "CB290S" USING LK-SLIDE-PARM

           IF LK-SL-RET NOT = C-RET-NORMAL
               DISPLAY "CB290S 異常 CARD=" RB-CARD-NO
                   " RET=" LK-SL-RET
               PERFORM 9100-SET-ABEND
           END-IF

           IF ABEND-OFF
               IF LK-SL-TIER NOT = "T1" AND
                  LK-SL-TIER NOT = "T2" AND
                  LK-SL-TIER NOT = "T3" AND
                  LK-SL-TIER NOT = "T4"
                   DISPLAY "スライド区分不正 CARD=" RB-CARD-NO
                       " TIER=" LK-SL-TIER
                   PERFORM 9100-SET-ABEND
               END-IF
           END-IF

           IF ABEND-OFF
               COMPUTE WS-FEE-AMT =
                   FUNCTION INTEGER(RB-REV-BAL-AMT * C-MONTH-RATE)
               INITIALIZE LK-RSLED-PARM
               MOVE RB-CARD-NO     TO LK-SE-CARD-NO
               MOVE RB-CYCLE-DT    TO LK-SE-CYCLE-DT
               COMPUTE LK-SE-PAY-AMT = LK-SL-PRIN-AMT + WS-FEE-AMT
               CALL "CB280S" USING LK-RSLED-PARM
               IF LK-SE-RET NOT = C-RET-NORMAL
                   DISPLAY "CB280S 異常 CARD=" RB-CARD-NO
                       " RET=" LK-SE-RET
                   PERFORM 9100-SET-ABEND
               END-IF
           END-IF

           IF ABEND-OFF
               INITIALIZE CDRSLDF-REC
               MOVE RB-CARD-NO       TO RS-CARD-NO
               MOVE RB-CYCLE-DT      TO RS-CYCLE-DT
               MOVE LK-SL-PRIN-AMT   TO RS-PRIN-AMT
               MOVE WS-FEE-AMT       TO RS-FEE-AMT
               COMPUTE RS-PAY-AMT = RS-PRIN-AMT + RS-FEE-AMT
               MOVE LK-SL-TIER       TO RS-SLIDE-TIER
               MOVE C-RSLD-COMPLETE  TO RS-RSLD-STATUS
               MOVE C-PROGRAM-ID     TO RS-PROGRAM-ID
               PERFORM 5000-WRITE-RSLD
               IF ABEND-OFF
                   ADD 1 TO WS-TOTAL-COMPLETE
               END-IF
           END-IF.

       4000-WRITE-SKIP SECTION.
           INITIALIZE CDRSLDF-REC
           MOVE RB-CARD-NO      TO RS-CARD-NO
           MOVE RB-CYCLE-DT     TO RS-CYCLE-DT
           MOVE ZERO            TO RS-PRIN-AMT
           MOVE ZERO            TO RS-FEE-AMT
           MOVE ZERO            TO RS-PAY-AMT
           MOVE SPACE           TO RS-SLIDE-TIER
           MOVE C-RSLD-SKIP     TO RS-RSLD-STATUS
           MOVE C-PROGRAM-ID    TO RS-PROGRAM-ID
           PERFORM 5000-WRITE-RSLD
           IF ABEND-OFF
               ADD 1 TO WS-TOTAL-SKIP
           END-IF.

       5000-WRITE-RSLD SECTION.
           WRITE CDRSLDF-REC
           IF FS-CDRSLDF NOT = "00"
               DISPLAY "CDRSLDF 書込失敗 ST=" FS-CDRSLDF
                   " CARD=" RS-CARD-NO
               PERFORM 9100-SET-ABEND
           END-IF.

       9000-FINAL SECTION.
           CLOSE CDREVF CDRBALF CDRSLDF

           IF FS-CDREVF NOT = "00"
               DISPLAY "CDREVF クローズ確認 ST=" FS-CDREVF
           END-IF

           IF FS-CDRBALF NOT = "00"
               DISPLAY "CDRBALF クローズ確認 ST=" FS-CDRBALF
           END-IF

           IF FS-CDRSLDF NOT = "00"
               DISPLAY "CDRSLDF クローズ確認 ST=" FS-CDRSLDF
           END-IF

           IF ABEND-ON
               MOVE 12 TO RETURN-CODE
               DISPLAY "CB210B 異常終了 入力件数=" WS-TOTAL-IN
                   " 確定=" WS-TOTAL-COMPLETE
                   " 対象外=" WS-TOTAL-SKIP
                   " エラー=" WS-TOTAL-ERROR
           ELSE
               MOVE 0 TO RETURN-CODE
               DISPLAY "CB210B 正常終了 入力件数=" WS-TOTAL-IN
                   " 確定=" WS-TOTAL-COMPLETE
                   " 対象外=" WS-TOTAL-SKIP
           END-IF.

       9100-SET-ABEND SECTION.
           SET ABEND-ON TO TRUE
           ADD 1 TO WS-TOTAL-ERROR
           MOVE 12 TO RETURN-CODE.
