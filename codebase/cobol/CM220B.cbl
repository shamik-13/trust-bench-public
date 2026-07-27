       IDENTIFICATION DIVISION.
       PROGRAM-ID. CM220B.
      ******************************************************************
      * 変更履歴
      * 版数  年月日    担当      概要
      * 1.00  20240401  第一開発  初版作成
      * 1.01  20240915  第一開発  CIF状態判定を追加
      * 1.02  20250220  第二開発  キー状態区分検査を追加
      ******************************************************************
      * 顧客照会索引作成バッチ
      * CIF番号、統合キーを照会用結果ファイルへ編集する。
      * CIF状態が抑止対象の場合は出力しない。
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CMCIFF ASSIGN TO "CMCIFF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-CMCIFF.
           SELECT CMATTF ASSIGN TO "CMATTF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CA-CIF-NO
               FILE STATUS IS FS-CMATTF.
           SELECT CMKEYF ASSIGN TO "CMKEYF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-CMKEYF.
           SELECT CMRSLF ASSIGN TO "CMRSLF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-CMRSLF.

       DATA DIVISION.
       FILE SECTION.
       FD  CMCIFF.
           COPY CMCIFFC.
       FD  CMATTF.
           COPY CMATTC.
       FD  CMKEYF.
           COPY CMKEYFC.
       FD  CMRSLF.
           COPY CMRSLC.

       WORKING-STORAGE SECTION.
       01  FS-CMCIFF                 PIC XX VALUE SPACE.
       01  FS-CMATTF                 PIC XX VALUE SPACE.
       01  FS-CMKEYF                 PIC XX VALUE SPACE.
       01  FS-CMRSLF                 PIC XX VALUE SPACE.

       01  SW-END-CMCIFF             PIC X VALUE 'N'.
           88  END-CMCIFF            VALUE 'Y'.
       01  SW-END-CMKEYF             PIC X VALUE 'N'.
           88  END-CMKEYF            VALUE 'Y'.
       01  SW-HARD-ERROR             PIC X VALUE 'N'.
           88  HARD-ERROR            VALUE 'Y'.

       01  CNST.
           05  CN-CIF-ACTIVE         PIC XX VALUE '01'.
           05  CN-CIF-EXCLUDE        PIC XX VALUE '08'.
           05  CN-CIF-INVALID        PIC XX VALUE '09'.
           05  CN-KEY-ACTIVE         PIC XX VALUE '01'.
           05  CN-RSL-NORMAL         PIC XX VALUE '00'.
           05  CN-RSL-WARN           PIC XX VALUE '10'.
           05  CN-RSN-NORMAL         PIC X(04) VALUE '0000'.
           05  CN-RSN-NO-ATTR        PIC X(04) VALUE 'A001'.
           05  CN-RSN-ATTR-STOP      PIC X(04) VALUE 'A002'.
           05  CN-RSN-NO-KEY         PIC X(04) VALUE 'K001'.

       01  WK-CURRENT-DATE.
           05  WK-DATE-YYYYMMDD      PIC 9(08).
           05  WK-DATE-TIME          PIC 9(08).
           05  WK-DATE-DIFF          PIC S9(04).
       01  WK-RS-ID.
           05  WK-RS-DATE            PIC 9(08).
           05  WK-RS-SEQ             PIC 9(09).
       01  WK-CIF-NO                 PIC X(20).
       01  WK-FOUND-KEY              PIC X VALUE 'N'.
           88  FOUND-KEY             VALUE 'Y'.
       01  WK-FOUND-ATTR             PIC X VALUE 'N'.
           88  FOUND-ATTR            VALUE 'Y'.
       01  WK-ABEND-MSG              PIC X(80) VALUE SPACE.

       01  CTL-COUNTERS.
           05  CNT-CIF-READ          PIC 9(09) VALUE ZERO.
           05  CNT-KEY-READ          PIC 9(09) VALUE ZERO.
           05  CNT-KEY-HOLD          PIC 9(09) VALUE ZERO.
           05  CNT-RSL-WRITE         PIC 9(09) VALUE ZERO.
           05  CNT-CIF-SKIP          PIC 9(09) VALUE ZERO.
           05  CNT-ATTR-MISS         PIC 9(09) VALUE ZERO.
           05  CNT-KEY-MISS          PIC 9(09) VALUE ZERO.

       01  KEY-TABLE.
           05  KEY-ENTRY OCCURS 50000 TIMES
               INDEXED BY K-IDX.
               10  KT-CIF-NO         PIC X(20).
               10  KT-KEY-ID         PIC X(40).
               10  KT-CD-CNT         PIC X(04).

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE 0 TO RETURN-CODE
           PERFORM 1000-INITIAL
           IF NOT HARD-ERROR
               PERFORM 2000-LOAD-KEY
           END-IF
           IF NOT HARD-ERROR
               PERFORM 3000-PROCESS-CIF
           END-IF
           PERFORM 9000-FINAL
           GOBACK.

       1000-INITIAL.
           DISPLAY 'CM220B 顧客照会索引作成バッチ 開始'
           MOVE FUNCTION CURRENT-DATE TO WK-CURRENT-DATE
           MOVE WK-DATE-YYYYMMDD TO WK-RS-DATE
           OPEN INPUT CMKEYF
           IF FS-CMKEYF NOT = '00'
               MOVE 8 TO RETURN-CODE
               SET HARD-ERROR TO TRUE
               DISPLAY 'CMKEYF オープン失敗 ST=' FS-CMKEYF
           END-IF
           IF NOT HARD-ERROR
               OPEN INPUT CMCIFF
               IF FS-CMCIFF NOT = '00'
                   MOVE 8 TO RETURN-CODE
                   SET HARD-ERROR TO TRUE
                   DISPLAY 'CMCIFF オープン失敗 ST=' FS-CMCIFF
               END-IF
           END-IF
           IF NOT HARD-ERROR
               OPEN INPUT CMATTF
               IF FS-CMATTF NOT = '00'
                   MOVE 8 TO RETURN-CODE
                   SET HARD-ERROR TO TRUE
                   DISPLAY 'CMATTF オープン失敗 ST=' FS-CMATTF
               END-IF
           END-IF
           IF NOT HARD-ERROR
               OPEN OUTPUT CMRSLF
               IF FS-CMRSLF NOT = '00'
                   MOVE 8 TO RETURN-CODE
                   SET HARD-ERROR TO TRUE
                   DISPLAY 'CMRSLF オープン失敗 ST=' FS-CMRSLF
               END-IF
           END-IF.

       2000-LOAD-KEY.
           PERFORM UNTIL END-CMKEYF OR HARD-ERROR
               READ CMKEYF
                   AT END
                       SET END-CMKEYF TO TRUE
                   NOT AT END
                       IF FS-CMKEYF NOT = '00'
                           MOVE 8 TO RETURN-CODE
                           SET HARD-ERROR TO TRUE
                           DISPLAY 'CMKEYF 読込失敗 ST=' FS-CMKEYF
                       ELSE
                           ADD 1 TO CNT-KEY-READ
                           IF CK-KEY-STATUS-KBN = CN-KEY-ACTIVE
                               IF CNT-KEY-HOLD < 50000
                                   ADD 1 TO CNT-KEY-HOLD
                                   SET K-IDX TO CNT-KEY-HOLD
                                   MOVE CK-CIF-NO TO KT-CIF-NO(K-IDX)
                                   MOVE CK-KEY-ID TO KT-KEY-ID(K-IDX)
                                   MOVE CK-CHECK-DIGIT-CNT
                                     TO KT-CD-CNT(K-IDX)
                               ELSE
                                   MOVE 12 TO RETURN-CODE
                                   SET HARD-ERROR TO TRUE
                                   DISPLAY 'CMKEYF 保持件数超過'
                               END-IF
                           END-IF
                       END-IF
               END-READ
           END-PERFORM.

       3000-PROCESS-CIF.
           PERFORM UNTIL END-CMCIFF OR HARD-ERROR
               READ CMCIFF
                   AT END
                       SET END-CMCIFF TO TRUE
                   NOT AT END
                       IF FS-CMCIFF NOT = '00'
                           MOVE 8 TO RETURN-CODE
                           SET HARD-ERROR TO TRUE
                           DISPLAY 'CMCIFF 読込失敗 ST=' FS-CMCIFF
                       ELSE
                           ADD 1 TO CNT-CIF-READ
                           PERFORM 3100-EDIT-CIF
                       END-IF
               END-READ
           END-PERFORM.

       3100-EDIT-CIF.
           IF CF-CIF-STATUS-KBN = CN-CIF-EXCLUDE
              OR CF-CIF-STATUS-KBN = CN-CIF-INVALID
               ADD 1 TO CNT-CIF-SKIP
           ELSE
               IF CF-CIF-STATUS-KBN NOT = CN-CIF-ACTIVE
                   ADD 1 TO CNT-CIF-SKIP
                   DISPLAY 'CIF状態区分不正 CIF=' CF-CIF-NO
                           ' 状態=' CF-CIF-STATUS-KBN
               ELSE
                   MOVE CF-CIF-NO TO WK-CIF-NO
                   PERFORM 3200-READ-ATTR
                   IF FOUND-ATTR
                       PERFORM 3300-FIND-KEY
                       PERFORM 3400-WRITE-RESULT
                   END-IF
               END-IF
           END-IF.

       3200-READ-ATTR.
           MOVE 'N' TO WK-FOUND-ATTR
           MOVE CF-CIF-NO TO CA-CIF-NO
           READ CMATTF KEY IS CA-CIF-NO
               INVALID KEY
                   IF FS-CMATTF = '23'
                       ADD 1 TO CNT-ATTR-MISS
                       PERFORM 3500-WRITE-ATTR-MISS
                   ELSE
                       MOVE 8 TO RETURN-CODE
                       SET HARD-ERROR TO TRUE
                       DISPLAY 'CMATTF 読込失敗 ST=' FS-CMATTF
                               ' CIF=' CF-CIF-NO
                   END-IF
               NOT INVALID KEY
                   IF FS-CMATTF = '00'
                       IF CA-ATTR-STATUS-KBN = CN-CIF-ACTIVE
                           MOVE 'Y' TO WK-FOUND-ATTR
                       ELSE
                           PERFORM 3600-WRITE-ATTR-STOP
                       END-IF
                   ELSE
                       MOVE 8 TO RETURN-CODE
                       SET HARD-ERROR TO TRUE
                       DISPLAY 'CMATTF 読込失敗 ST=' FS-CMATTF
                               ' CIF=' CF-CIF-NO
                   END-IF
           END-READ.

       3300-FIND-KEY.
           MOVE 'N' TO WK-FOUND-KEY
           SET K-IDX TO 1
           PERFORM UNTIL K-IDX > CNT-KEY-HOLD OR FOUND-KEY
               IF KT-CIF-NO(K-IDX) = WK-CIF-NO
                   MOVE 'Y' TO WK-FOUND-KEY
               ELSE
                   SET K-IDX UP BY 1
               END-IF
           END-PERFORM.

       3400-WRITE-RESULT.
           IF FOUND-KEY
               INITIALIZE CMRSLF-REC
               ADD 1 TO WK-RS-SEQ
               MOVE WK-RS-ID TO RS-RESULT-ID
               MOVE CF-CIF-NO TO RS-CIF-NO
               MOVE KT-KEY-ID(K-IDX) TO RS-KEY-ID
               MOVE CN-RSL-NORMAL TO RS-RESULT-KBN
               MOVE CN-RSN-NORMAL TO RS-REASON-CD
               MOVE WK-DATE-YYYYMMDD TO RS-OUTPUT-DT
               WRITE CMRSLF-REC
               IF FS-CMRSLF = '00'
                   ADD 1 TO CNT-RSL-WRITE
               ELSE
                   MOVE 8 TO RETURN-CODE
                   SET HARD-ERROR TO TRUE
                   DISPLAY 'CMRSLF 書込失敗 ST=' FS-CMRSLF
                           ' CIF=' CF-CIF-NO
               END-IF
           ELSE
               ADD 1 TO CNT-KEY-MISS
               INITIALIZE CMRSLF-REC
               ADD 1 TO WK-RS-SEQ
               MOVE WK-RS-ID TO RS-RESULT-ID
               MOVE CF-CIF-NO TO RS-CIF-NO
               MOVE SPACE TO RS-KEY-ID
               MOVE CN-RSL-WARN TO RS-RESULT-KBN
               MOVE CN-RSN-NO-KEY TO RS-REASON-CD
               MOVE WK-DATE-YYYYMMDD TO RS-OUTPUT-DT
               WRITE CMRSLF-REC
               IF FS-CMRSLF = '00'
                   ADD 1 TO CNT-RSL-WRITE
               ELSE
                   MOVE 8 TO RETURN-CODE
                   SET HARD-ERROR TO TRUE
                   DISPLAY 'CMRSLF 書込失敗 ST=' FS-CMRSLF
                           ' CIF=' CF-CIF-NO
               END-IF
           END-IF.

       3500-WRITE-ATTR-MISS.
           INITIALIZE CMRSLF-REC
           ADD 1 TO WK-RS-SEQ
           MOVE WK-RS-ID TO RS-RESULT-ID
           MOVE CF-CIF-NO TO RS-CIF-NO
           MOVE SPACE TO RS-KEY-ID
           MOVE CN-RSL-WARN TO RS-RESULT-KBN
           MOVE CN-RSN-NO-ATTR TO RS-REASON-CD
           MOVE WK-DATE-YYYYMMDD TO RS-OUTPUT-DT
           WRITE CMRSLF-REC
           IF FS-CMRSLF = '00'
               ADD 1 TO CNT-RSL-WRITE
           ELSE
               MOVE 8 TO RETURN-CODE
               SET HARD-ERROR TO TRUE
               DISPLAY 'CMRSLF 書込失敗 ST=' FS-CMRSLF
                       ' CIF=' CF-CIF-NO
           END-IF.

       3600-WRITE-ATTR-STOP.
           INITIALIZE CMRSLF-REC
           ADD 1 TO WK-RS-SEQ
           MOVE WK-RS-ID TO RS-RESULT-ID
           MOVE CF-CIF-NO TO RS-CIF-NO
           MOVE SPACE TO RS-KEY-ID
           MOVE CN-RSL-WARN TO RS-RESULT-KBN
           MOVE CN-RSN-ATTR-STOP TO RS-REASON-CD
           MOVE WK-DATE-YYYYMMDD TO RS-OUTPUT-DT
           WRITE CMRSLF-REC
           IF FS-CMRSLF = '00'
               ADD 1 TO CNT-RSL-WRITE
           ELSE
               MOVE 8 TO RETURN-CODE
               SET HARD-ERROR TO TRUE
               DISPLAY 'CMRSLF 書込失敗 ST=' FS-CMRSLF
                       ' CIF=' CF-CIF-NO
           END-IF.

       9000-FINAL.
           IF FS-CMCIFF = '00'
               CLOSE CMCIFF
               IF FS-CMCIFF NOT = '00'
                   MOVE 8 TO RETURN-CODE
                   DISPLAY 'CMCIFF クローズ失敗 ST=' FS-CMCIFF
               END-IF
           END-IF
           IF FS-CMATTF = '00'
               CLOSE CMATTF
               IF FS-CMATTF NOT = '00'
                   MOVE 8 TO RETURN-CODE
                   DISPLAY 'CMATTF クローズ失敗 ST=' FS-CMATTF
               END-IF
           END-IF
           IF FS-CMKEYF = '00'
               CLOSE CMKEYF
               IF FS-CMKEYF NOT = '00'
                   MOVE 8 TO RETURN-CODE
                   DISPLAY 'CMKEYF クローズ失敗 ST=' FS-CMKEYF
               END-IF
           END-IF
           IF FS-CMRSLF = '00'
               CLOSE CMRSLF
               IF FS-CMRSLF NOT = '00'
                   MOVE 8 TO RETURN-CODE
                   DISPLAY 'CMRSLF クローズ失敗 ST=' FS-CMRSLF
               END-IF
           END-IF
           DISPLAY 'CM220B 顧客照会索引作成バッチ 終了'
           DISPLAY 'CIF読込件数=' CNT-CIF-READ
           DISPLAY 'キー読込件数=' CNT-KEY-READ
           DISPLAY 'キー保持件数=' CNT-KEY-HOLD
           DISPLAY '結果出力件数=' CNT-RSL-WRITE
           DISPLAY 'CIF抑止件数=' CNT-CIF-SKIP
           DISPLAY '属性未登録件数=' CNT-ATTR-MISS
           DISPLAY 'キー未登録件数=' CNT-KEY-MISS.
