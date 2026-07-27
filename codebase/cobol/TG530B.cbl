       IDENTIFICATION DIVISION.
       PROGRAM-ID. TG530B.
      *---------------------------------------------------------------*
      * 変更履歴
      * 版数  年月日        担当                   概要
      * 1.00  平成29年04月  システム部 対外系チーム 新規作成
      * 1.01  令和02年02月  システム部 対外系チーム 上限検査追加
      * 1.02  令和05年03月  システム部 対外系チーム 管理ファイル参照追加
      *---------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TGZENF ASSIGN TO "TGZENF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-TGZENF.
           SELECT TGNETCF ASSIGN TO "TGNETCF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS NC-COUNTER-BANK
               FILE STATUS IS FS-TGNETCF.
           SELECT TGCLRF ASSIGN TO "TGCLRF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-TGCLRF.

       DATA DIVISION.
       FILE SECTION.
       FD  TGZENF.
           COPY TGZENFC.
       FD  TGNETCF.
           COPY TGNETCFC.
       FD  TGCLRF.
           COPY TGCLRFC.

       WORKING-STORAGE SECTION.
       01  FS-TGZENF                  PIC XX VALUE SPACES.
       01  FS-TGNETCF                 PIC XX VALUE SPACES.
       01  FS-TGCLRF                  PIC XX VALUE SPACES.

       01  WS-END-FLAG                PIC X VALUE "N".
           88  WS-END                       VALUE "Y".
       01  WS-ABEND-FLAG              PIC X VALUE "N".
           88  WS-ABEND                     VALUE "Y".

       01  WS-RUN-DATE                PIC 9(08).
       01  WS-TABLE-COUNT             PIC 9(04) VALUE 0.
       01  WS-IDX                     PIC 9(04) VALUE 0.
       01  WS-HIT-IDX                 PIC 9(04) VALUE 0.
       01  WS-FOUND-FLAG              PIC X VALUE "N".
           88  WS-FOUND                     VALUE "Y".
       01  WS-OUT-IDX                 PIC 9(04) VALUE 0.
       01  WS-MAX-COUNT               PIC 9(04) VALUE 200.

       01  WS-COUNTER-TABLE.
           05  WS-COUNTER-ENTRY OCCURS 200 TIMES.
               10  WS-TB-COUNTER-BANK PIC X(04).
               10  WS-TB-PAY-AMT      PIC S9(13)V99 COMP-3.
               10  WS-TB-RECV-AMT     PIC S9(13)V99 COMP-3.
               10  WS-TB-ITEM-COUNT   PIC 9(07) COMP-3.

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE 0 TO RETURN-CODE
           ACCEPT WS-RUN-DATE FROM DATE YYYYMMDD
           PERFORM 1000-OPEN-FILES
           IF NOT WS-ABEND
               PERFORM 2000-READ-TGZENF
               PERFORM UNTIL WS-END OR WS-ABEND
                   PERFORM 3000-ACCUMULATE
                   IF NOT WS-ABEND
                       PERFORM 2000-READ-TGZENF
                   END-IF
               END-PERFORM
           END-IF
           IF NOT WS-ABEND
               PERFORM 4000-OUTPUT-RESULTS
           END-IF
           PERFORM 9000-CLOSE-FILES
           GOBACK.

       1000-OPEN-FILES.
           OPEN INPUT TGZENF
           IF FS-TGZENF NOT = "00"
               DISPLAY "TGZENF オープン失敗 ST=" FS-TGZENF
               PERFORM 9900-ABEND
           END-IF

           IF NOT WS-ABEND
               OPEN INPUT TGNETCF
               IF FS-TGNETCF NOT = "00"
                   DISPLAY "TGNETCF オープン失敗 ST=" FS-TGNETCF
                   PERFORM 9900-ABEND
               END-IF
           END-IF

           IF NOT WS-ABEND
               OPEN OUTPUT TGCLRF
               IF FS-TGCLRF NOT = "00"
                   DISPLAY "TGCLRF オープン失敗 ST=" FS-TGCLRF
                   PERFORM 9900-ABEND
               END-IF
           END-IF.

       2000-READ-TGZENF.
           READ TGZENF
               AT END
                   SET WS-END TO TRUE
               NOT AT END
                   IF FS-TGZENF NOT = "00"
                       DISPLAY "TGZENF 読込失敗 ST=" FS-TGZENF
                       PERFORM 9900-ABEND
                   END-IF
           END-READ.

       3000-ACCUMULATE.
           IF ZE-COUNTER-BANK = SPACES
               DISPLAY "相手銀行番号が空白"
               DISPLAY "SEQ=" ZE-CENTER-SEQ
               PERFORM 9900-ABEND
           ELSE
               PERFORM 3100-FIND-COUNTER
               IF NOT WS-FOUND
                   PERFORM 3200-ADD-COUNTER
               END-IF
               IF NOT WS-ABEND
                   PERFORM 3300-ADD-AMOUNT
               END-IF
           END-IF.

       3100-FIND-COUNTER.
           MOVE "N" TO WS-FOUND-FLAG
           MOVE 0 TO WS-HIT-IDX
           MOVE 1 TO WS-IDX
           PERFORM UNTIL WS-IDX > WS-TABLE-COUNT OR WS-FOUND
               IF WS-TB-COUNTER-BANK(WS-IDX) = ZE-COUNTER-BANK
                   MOVE WS-IDX TO WS-HIT-IDX
                   SET WS-FOUND TO TRUE
               ELSE
                   ADD 1 TO WS-IDX
               END-IF
           END-PERFORM.

       3200-ADD-COUNTER.
           IF WS-TABLE-COUNT >= WS-MAX-COUNT
               DISPLAY "相手行テーブル溢れ"
               DISPLAY "BANK=" ZE-COUNTER-BANK
               PERFORM 9900-ABEND
           ELSE
               ADD 1 TO WS-TABLE-COUNT
               MOVE WS-TABLE-COUNT TO WS-HIT-IDX
               MOVE ZE-COUNTER-BANK
                   TO WS-TB-COUNTER-BANK(WS-HIT-IDX)
               MOVE 0 TO WS-TB-PAY-AMT(WS-HIT-IDX)
               MOVE 0 TO WS-TB-RECV-AMT(WS-HIT-IDX)
               MOVE 0 TO WS-TB-ITEM-COUNT(WS-HIT-IDX)
           END-IF.

       3300-ADD-AMOUNT.
           EVALUATE ZE-ZEN-TYPE
               WHEN "01"
                   ADD ZE-REMIT-AMT TO WS-TB-PAY-AMT(WS-HIT-IDX)
                   ADD 1 TO WS-TB-ITEM-COUNT(WS-HIT-IDX)
               WHEN "02"
                   ADD ZE-REMIT-AMT TO WS-TB-RECV-AMT(WS-HIT-IDX)
                   ADD 1 TO WS-TB-ITEM-COUNT(WS-HIT-IDX)
               WHEN OTHER
                   DISPLAY "電文種別不正=" ZE-ZEN-TYPE
                   DISPLAY "SEQ=" ZE-CENTER-SEQ
                   PERFORM 9900-ABEND
           END-EVALUATE.

       4000-OUTPUT-RESULTS.
           MOVE 1 TO WS-OUT-IDX
           PERFORM UNTIL WS-OUT-IDX > WS-TABLE-COUNT OR WS-ABEND
               PERFORM 4100-READ-CONTROL
               IF NOT WS-ABEND
                   PERFORM 4200-WRITE-CLEAR
               END-IF
               ADD 1 TO WS-OUT-IDX
           END-PERFORM.

       4100-READ-CONTROL.
           MOVE WS-TB-COUNTER-BANK(WS-OUT-IDX)
               TO NC-COUNTER-BANK
           READ TGNETCF
               INVALID KEY
                   IF FS-TGNETCF = "23"
                       DISPLAY "ネット管理レコードなし"
                       DISPLAY "BANK=" NC-COUNTER-BANK
                   ELSE
                       DISPLAY "TGNETCF 読込失敗 ST=" FS-TGNETCF
                       DISPLAY "BANK=" NC-COUNTER-BANK
                       PERFORM 9900-ABEND
                   END-IF
               NOT INVALID KEY
                   IF FS-TGNETCF NOT = "00"
                       DISPLAY "TGNETCF 読込失敗 ST=" FS-TGNETCF
                       DISPLAY "BANK=" NC-COUNTER-BANK
                       PERFORM 9900-ABEND
                   END-IF
           END-READ.

       4200-WRITE-CLEAR.
           MOVE WS-RUN-DATE TO CL-SETTLE-DT
           MOVE WS-TB-COUNTER-BANK(WS-OUT-IDX)
               TO CL-COUNTER-BANK
           COMPUTE CL-NET-AMT =
               WS-TB-PAY-AMT(WS-OUT-IDX)
             - WS-TB-RECV-AMT(WS-OUT-IDX)
           MOVE WS-TB-ITEM-COUNT(WS-OUT-IDX)
               TO CL-ITEM-COUNT
           MOVE "S" TO CL-SETTLE-STATUS
           WRITE TGCLRF-REC
           IF FS-TGCLRF NOT = "00"
               DISPLAY "TGCLRF 書込失敗 ST=" FS-TGCLRF
               DISPLAY "BANK=" CL-COUNTER-BANK
               PERFORM 9900-ABEND
           END-IF.

       9000-CLOSE-FILES.
           CLOSE TGZENF
           IF FS-TGZENF NOT = "00" AND FS-TGZENF NOT = "42"
               DISPLAY "TGZENF クローズ失敗 ST=" FS-TGZENF
               PERFORM 9900-ABEND
           END-IF

           CLOSE TGNETCF
           IF FS-TGNETCF NOT = "00" AND FS-TGNETCF NOT = "42"
               DISPLAY "TGNETCF クローズ失敗 ST=" FS-TGNETCF
               PERFORM 9900-ABEND
           END-IF

           CLOSE TGCLRF
           IF FS-TGCLRF NOT = "00" AND FS-TGCLRF NOT = "42"
               DISPLAY "TGCLRF クローズ失敗 ST=" FS-TGCLRF
               PERFORM 9900-ABEND
           END-IF.

       9900-ABEND.
           SET WS-ABEND TO TRUE
           MOVE 8 TO RETURN-CODE.
