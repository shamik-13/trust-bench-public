       IDENTIFICATION DIVISION.
       PROGRAM-ID. TG181S.
      *---------------------------------------------------------------*
      * 変更履歴                                                      *
      * 版数  年月日      担当                         概要          *
      * 1.00  平成30年04月 システム部 為替・対外接続チーム 初版作成  *
      * 1.01  令和02年10月 システム部 為替・対外接続チーム 文言整備  *
      * 1.02  令和05年06月 システム部 為替・対外接続チーム 運用対応  *
      *---------------------------------------------------------------*

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  LK-KANA-PARM.
           05 LK-RAW-KANA                 PIC X(48).
           05 LK-NORM-KANA                PIC X(48).
           05 LK-KANA-RET                 PIC 9(02) COMP.

       01  TG181S-WORK.
           05 TG181S-WK-IDX               PIC 9(02) COMP VALUE 0.
           05 TG181S-WK-CENTER-IDX        PIC 9(02) COMP VALUE 0.
           05 TG181S-WK-DEP-TYPE          PIC X(01) VALUE SPACE.
           05 TG181S-WK-ERR-SW            PIC X(01) VALUE '0'.
              88 TG181S-WK-ERR-ON         VALUE '1'.
              88 TG181S-WK-ERR-OFF        VALUE '0'.
           05 TG181S-WK-HARD-ERR-SW       PIC X(01) VALUE '0'.
              88 TG181S-WK-HARD-ERR-ON    VALUE '1'.
              88 TG181S-WK-HARD-ERR-OFF   VALUE '0'.

       01  TG181S-CONST.
           05 TG181S-C-MAX-REC            PIC 9(02) VALUE 9.
           05 TG181S-C-BANK-POS           PIC 9(03) VALUE 1.
           05 TG181S-C-BRANCH-POS         PIC 9(03) VALUE 5.
           05 TG181S-C-DEP-POS            PIC 9(03) VALUE 8.
           05 TG181S-C-ACCT-POS           PIC 9(03) VALUE 9.
           05 TG181S-C-KANA-POS           PIC 9(03) VALUE 16.

       LINKAGE SECTION.

       01  TG181S-PARM.
           05 TG181S-IN-REC-CNT           PIC 9(02).
           05 TG181S-IN-TABLE OCCURS 9.
              10 TG181S-IN-SEQ            PIC 9(06).
              10 TG181S-IN-BANK-CD        PIC X(04).
              10 TG181S-IN-BRANCH-CD      PIC X(03).
              10 TG181S-IN-DEP-TYPE       PIC X(02).
              10 TG181S-IN-ACCT-NO        PIC X(07).
              10 TG181S-IN-RECV-KANA      PIC X(48).
           05 TG181S-OUT-AREA.
              10 TG181S-OUT-ITEMS         PIC X(63).
              10 TG181S-MISS-BANK-POS     PIC 9(03).
              10 TG181S-MISS-BRANCH-POS   PIC 9(03).
              10 TG181S-MISS-DEP-POS      PIC 9(03).
              10 TG181S-MISS-ACCT-POS     PIC 9(03).
              10 TG181S-MISS-KANA-POS     PIC 9(03).
              10 TG181S-OUT-CENTER-SEQ    PIC 9(06).
              10 TG181S-OUT-RET           PIC 9(02).
              10 TG181S-OUT-MSG           PIC X(40).

       PROCEDURE DIVISION USING TG181S-PARM.

       0000-MAIN.
           PERFORM 1000-INIT
           PERFORM 2000-CHECK-PARM

           IF TG181S-WK-HARD-ERR-OFF
              PERFORM 3000-EDIT-ITEM
           END-IF

           PERFORM 9000-END
           GOBACK
           .

       1000-INIT.
           MOVE SPACES TO TG181S-OUT-ITEMS
                          TG181S-OUT-MSG
                          LK-RAW-KANA
                          LK-NORM-KANA
                          TG181S-WK-DEP-TYPE
           MOVE ZERO   TO TG181S-MISS-BANK-POS
                          TG181S-MISS-BRANCH-POS
                          TG181S-MISS-DEP-POS
                          TG181S-MISS-ACCT-POS
                          TG181S-MISS-KANA-POS
                          TG181S-OUT-CENTER-SEQ
                          TG181S-OUT-RET
                          LK-KANA-RET
                          TG181S-WK-IDX
                          TG181S-WK-CENTER-IDX
           SET TG181S-WK-ERR-OFF TO TRUE
           SET TG181S-WK-HARD-ERR-OFF TO TRUE
           .

       2000-CHECK-PARM.
           IF TG181S-IN-REC-CNT IS NOT NUMERIC
              MOVE 12 TO TG181S-OUT-RET
              MOVE '入力件数エラー' TO TG181S-OUT-MSG
              DISPLAY 'TG181S 入力件数エラー'
              SET TG181S-WK-HARD-ERR-ON TO TRUE
              EXIT PARAGRAPH
           END-IF

           IF TG181S-IN-REC-CNT < 1
              OR TG181S-IN-REC-CNT > TG181S-C-MAX-REC
              MOVE 12 TO TG181S-OUT-RET
              MOVE '入力件数範囲エラー' TO TG181S-OUT-MSG
              DISPLAY 'TG181S 入力件数範囲エラー'
              SET TG181S-WK-HARD-ERR-ON TO TRUE
              EXIT PARAGRAPH
           END-IF

           COMPUTE TG181S-WK-CENTER-IDX =
                   (TG181S-IN-REC-CNT + 1) / 2

           IF TG181S-IN-SEQ(TG181S-WK-CENTER-IDX) IS NUMERIC
              MOVE TG181S-IN-SEQ(TG181S-WK-CENTER-IDX)
                TO TG181S-OUT-CENTER-SEQ
           ELSE
              MOVE ZERO TO TG181S-OUT-CENTER-SEQ
              MOVE 8 TO TG181S-OUT-RET
              MOVE '中央明細番号エラー' TO TG181S-OUT-MSG
              DISPLAY 'TG181S 中央明細番号エラー'
              SET TG181S-WK-ERR-ON TO TRUE
           END-IF
           .

       3000-EDIT-ITEM.
           MOVE TG181S-WK-CENTER-IDX TO TG181S-WK-IDX

           PERFORM 3100-CHECK-BANK
           PERFORM 3200-CHECK-BRANCH
           PERFORM 3300-CHECK-DEP
           PERFORM 3400-CHECK-ACCT
           PERFORM 3500-CHECK-KANA

           IF TG181S-WK-HARD-ERR-ON
              EXIT PARAGRAPH
           END-IF

           IF TG181S-IN-BANK-CD(TG181S-WK-IDX) IS NUMERIC
              MOVE TG181S-IN-BANK-CD(TG181S-WK-IDX)
                TO TG181S-OUT-ITEMS(TG181S-C-BANK-POS:4)
           END-IF

           IF TG181S-IN-BRANCH-CD(TG181S-WK-IDX) IS NUMERIC
              MOVE TG181S-IN-BRANCH-CD(TG181S-WK-IDX)
                TO TG181S-OUT-ITEMS(TG181S-C-BRANCH-POS:3)
           END-IF

           IF TG181S-WK-DEP-TYPE NOT = SPACE
              MOVE TG181S-WK-DEP-TYPE
                TO TG181S-OUT-ITEMS(TG181S-C-DEP-POS:1)
           END-IF

           IF TG181S-IN-ACCT-NO(TG181S-WK-IDX) IS NUMERIC
              MOVE TG181S-IN-ACCT-NO(TG181S-WK-IDX)
                TO TG181S-OUT-ITEMS(TG181S-C-ACCT-POS:7)
           END-IF

           IF LK-KANA-RET = 0
              MOVE LK-NORM-KANA
                TO TG181S-OUT-ITEMS(TG181S-C-KANA-POS:48)
           END-IF

           IF TG181S-WK-ERR-ON
              IF TG181S-OUT-RET = 0
                 MOVE 8 TO TG181S-OUT-RET
                 MOVE '入力項目エラー' TO TG181S-OUT-MSG
              END-IF
           END-IF
           .

       3100-CHECK-BANK.
           IF TG181S-IN-BANK-CD(TG181S-WK-IDX) = SPACES
              MOVE TG181S-C-BANK-POS TO TG181S-MISS-BANK-POS
              MOVE 8 TO TG181S-OUT-RET
              MOVE '銀行コード未設定' TO TG181S-OUT-MSG
              DISPLAY 'TG181S 銀行コード未設定'
              SET TG181S-WK-ERR-ON TO TRUE
              EXIT PARAGRAPH
           END-IF

           IF TG181S-IN-BANK-CD(TG181S-WK-IDX) IS NOT NUMERIC
              MOVE TG181S-C-BANK-POS TO TG181S-MISS-BANK-POS
              MOVE 8 TO TG181S-OUT-RET
              MOVE '銀行コード数字外' TO TG181S-OUT-MSG
              DISPLAY 'TG181S 銀行コード数字外'
              SET TG181S-WK-ERR-ON TO TRUE
           END-IF
           .

       3200-CHECK-BRANCH.
           IF TG181S-IN-BRANCH-CD(TG181S-WK-IDX) = SPACES
              MOVE TG181S-C-BRANCH-POS TO TG181S-MISS-BRANCH-POS
              MOVE 8 TO TG181S-OUT-RET
              MOVE '支店コード未設定' TO TG181S-OUT-MSG
              DISPLAY 'TG181S 支店コード未設定'
              SET TG181S-WK-ERR-ON TO TRUE
              EXIT PARAGRAPH
           END-IF

           IF TG181S-IN-BRANCH-CD(TG181S-WK-IDX) IS NOT NUMERIC
              MOVE TG181S-C-BRANCH-POS TO TG181S-MISS-BRANCH-POS
              MOVE 8 TO TG181S-OUT-RET
              MOVE '支店コード数字外' TO TG181S-OUT-MSG
              DISPLAY 'TG181S 支店コード数字外'
              SET TG181S-WK-ERR-ON TO TRUE
           END-IF
           .

       3300-CHECK-DEP.
           IF TG181S-IN-DEP-TYPE(TG181S-WK-IDX) = SPACES
              MOVE TG181S-C-DEP-POS TO TG181S-MISS-DEP-POS
              MOVE 8 TO TG181S-OUT-RET
              MOVE '預金種目未設定' TO TG181S-OUT-MSG
              DISPLAY 'TG181S 預金種目未設定'
              SET TG181S-WK-ERR-ON TO TRUE
              EXIT PARAGRAPH
           END-IF

           EVALUATE TG181S-IN-DEP-TYPE(TG181S-WK-IDX)
              WHEN '01'
                 MOVE '1' TO TG181S-WK-DEP-TYPE
              WHEN '02'
                 MOVE '2' TO TG181S-WK-DEP-TYPE
              WHEN '04'
                 MOVE '4' TO TG181S-WK-DEP-TYPE
              WHEN OTHER
                 MOVE TG181S-C-DEP-POS TO TG181S-MISS-DEP-POS
                 MOVE 8 TO TG181S-OUT-RET
                 MOVE '預金種目エラー' TO TG181S-OUT-MSG
                 DISPLAY 'TG181S 預金種目エラー'
                 SET TG181S-WK-ERR-ON TO TRUE
           END-EVALUATE
           .

       3400-CHECK-ACCT.
           IF TG181S-IN-ACCT-NO(TG181S-WK-IDX) = SPACES
              MOVE TG181S-C-ACCT-POS TO TG181S-MISS-ACCT-POS
              MOVE 8 TO TG181S-OUT-RET
              MOVE '口座番号未設定' TO TG181S-OUT-MSG
              DISPLAY 'TG181S 口座番号未設定'
              SET TG181S-WK-ERR-ON TO TRUE
              EXIT PARAGRAPH
           END-IF

           IF TG181S-IN-ACCT-NO(TG181S-WK-IDX) IS NOT NUMERIC
              MOVE TG181S-C-ACCT-POS TO TG181S-MISS-ACCT-POS
              MOVE 8 TO TG181S-OUT-RET
              MOVE '口座番号数字外' TO TG181S-OUT-MSG
              DISPLAY 'TG181S 口座番号数字外'
              SET TG181S-WK-ERR-ON TO TRUE
           END-IF
           .

       3500-CHECK-KANA.
           IF TG181S-IN-RECV-KANA(TG181S-WK-IDX) = SPACES
              MOVE TG181S-C-KANA-POS TO TG181S-MISS-KANA-POS
              MOVE 8 TO TG181S-OUT-RET
              MOVE '受取人カナ未設定' TO TG181S-OUT-MSG
              DISPLAY 'TG181S 受取人カナ未設定'
              SET TG181S-WK-ERR-ON TO TRUE
              EXIT PARAGRAPH
           END-IF

           MOVE TG181S-IN-RECV-KANA(TG181S-WK-IDX) TO LK-RAW-KANA

           CALL 'TG912S' USING LK-KANA-PARM
              ON EXCEPTION
                 MOVE 12 TO TG181S-OUT-RET
                 MOVE 'カナ正規化呼出エラー' TO TG181S-OUT-MSG
                 DISPLAY 'TG181S TG912S 呼出エラー'
                 SET TG181S-WK-HARD-ERR-ON TO TRUE
                 EXIT PARAGRAPH
           END-CALL

           IF LK-KANA-RET NOT = 0
              MOVE TG181S-C-KANA-POS TO TG181S-MISS-KANA-POS
              MOVE 8 TO TG181S-OUT-RET
              MOVE '受取人カナエラー' TO TG181S-OUT-MSG
              DISPLAY 'TG181S 受取人カナエラー'
              SET TG181S-WK-ERR-ON TO TRUE
           END-IF
           .

       9000-END.
           IF TG181S-WK-HARD-ERR-ON
              MOVE 12 TO RETURN-CODE
           ELSE
              IF TG181S-OUT-RET = 0
                 MOVE '正常終了' TO TG181S-OUT-MSG
                 MOVE 0 TO RETURN-CODE
              ELSE
                 MOVE 8 TO RETURN-CODE
              END-IF
           END-IF
           .

       END PROGRAM TG181S.
