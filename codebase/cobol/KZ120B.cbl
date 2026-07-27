       IDENTIFICATION DIVISION.
       PROGRAM-ID. KZ120B.
      *
      * 変更履歴
      * 版数  年月日    担当    概要
      * 1.00  20230116  KZ01    初版作成
      * 1.01  20230904  KZ02    限度額入力検査を追加
      * 1.02  20240520  KZ03    顧客KYC照合を追加
      *
      * 口座マスタ保守反映バッチ
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZMAINTF ASSIGN TO "KZMAINTF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS FS-KZMAINTF.

           SELECT KZACCTF ASSIGN TO "KZACCTF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS AC-ACCT-NO
               FILE STATUS IS FS-KZACCTF.

           SELECT KZCUSTF ASSIGN TO "KZCUSTF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CU-CUST-ID
               FILE STATUS IS FS-KZCUSTF.

       DATA DIVISION.
       FILE SECTION.
       FD  KZMAINTF.
           COPY KZMAINTFC.

       FD  KZACCTF.
           COPY KZACCTC4.

       FD  KZCUSTF.
           COPY KZCUSTC.

       WORKING-STORAGE SECTION.
       01  FS-KZMAINTF                 PIC XX VALUE SPACES.
       01  FS-KZACCTF                  PIC XX VALUE SPACES.
       01  FS-KZCUSTF                  PIC XX VALUE SPACES.

       01  SW-END                      PIC X VALUE "N".
           88  END-OF-MAINT                  VALUE "Y".
           88  NOT-END-OF-MAINT              VALUE "N".

       01  WS-COUNTERS.
           05  CNT-READ                PIC 9(09) VALUE ZERO.
           05  CNT-APPLIED             PIC 9(09) VALUE ZERO.
           05  CNT-REJECTED            PIC 9(09) VALUE ZERO.

       01  WS-WORK.
           05  WS-ERR-FLG              PIC X VALUE "N".
               88  HAS-ERROR                 VALUE "Y".
               88  NO-ERROR                  VALUE "N".
           05  WS-REASON               PIC X(40) VALUE SPACES.
           05  WS-NEW-VALUE            PIC X(60) VALUE SPACES.
           05  WS-NUMERIC-TEXT         PIC X(15) VALUE SPACES.
           05  WS-LIMIT-N              PIC 9(13) VALUE ZERO.
           05  WS-MAX-LIMIT            PIC 9(13) VALUE 0500000000000.
           05  WS-ZERO-BAL             PIC S9(13)V99 VALUE ZERO.
           05  WS-ZERO-AMT             PIC S9(13)V99 VALUE ZERO.
           05  WS-CLOSE-GROUP          PIC X(06) VALUE "CLOSE ".
           05  WS-OPEN-TYPE            PIC X VALUE "1".
           05  WS-CHANGE-TYPE          PIC X VALUE "2".
           05  WS-CLOSE-TYPE           PIC X VALUE "3".
           05  WS-FLD-ACCT-TYPE        PIC XX VALUE "01".
           05  WS-FLD-GROUP            PIC XX VALUE "02".
           05  WS-FLD-LIMIT            PIC XX VALUE "03".
           05  WS-PROD-CARD            PIC XX VALUE "01".
           05  WS-PROD-OVER            PIC XX VALUE "02".
           05  WS-PROD-LOAN            PIC XX VALUE "03".
           05  WS-IDX                  PIC 9(02) VALUE ZERO.
           05  WS-CHAR                 PIC X VALUE SPACE.
           05  WS-DIGIT-SEEN           PIC X VALUE "N".
               88  DIGIT-SEEN                VALUE "Y".
               88  DIGIT-NOT-SEEN            VALUE "N".
           05  WS-SPACE-SEEN           PIC X VALUE "N".
               88  SPACE-SEEN                VALUE "Y".
               88  SPACE-NOT-SEEN            VALUE "N".

       PROCEDURE DIVISION.
       MAIN-SECTION.
           PERFORM 1000-INIT
           IF RETURN-CODE NOT = 0
               GOBACK
           END-IF

           PERFORM 2000-MAIN UNTIL END-OF-MAINT

           PERFORM 9000-FINAL
           GOBACK.

       1000-INIT.
           MOVE 0 TO RETURN-CODE
           MOVE "N" TO SW-END
           MOVE ZERO TO CNT-READ CNT-APPLIED CNT-REJECTED

           OPEN INPUT KZMAINTF
           IF FS-KZMAINTF NOT = "00"
               DISPLAY "KZMAINTF オープン失敗 ST=" FS-KZMAINTF
               MOVE 12 TO RETURN-CODE
               SET END-OF-MAINT TO TRUE
               EXIT PARAGRAPH
           END-IF

           OPEN I-O KZACCTF
           IF FS-KZACCTF NOT = "00"
               DISPLAY "KZACCTF オープン失敗 ST=" FS-KZACCTF
               MOVE 12 TO RETURN-CODE
               SET END-OF-MAINT TO TRUE
               CLOSE KZMAINTF
               EXIT PARAGRAPH
           END-IF

           OPEN INPUT KZCUSTF
           IF FS-KZCUSTF NOT = "00"
               DISPLAY "KZCUSTF オープン失敗 ST=" FS-KZCUSTF
               MOVE 12 TO RETURN-CODE
               SET END-OF-MAINT TO TRUE
               CLOSE KZMAINTF KZACCTF
               EXIT PARAGRAPH
           END-IF.

       2000-MAIN.
           READ KZMAINTF
               AT END
                   SET END-OF-MAINT TO TRUE
               NOT AT END
                   ADD 1 TO CNT-READ
                   PERFORM 2100-PROCESS-REQUEST
           END-READ.

       2100-PROCESS-REQUEST.
           SET NO-ERROR TO TRUE
           MOVE SPACES TO WS-REASON

           PERFORM 2200-VALIDATE-COMMON
           IF HAS-ERROR
               PERFORM 8000-REJECT-REQUEST
               EXIT PARAGRAPH
           END-IF

           EVALUATE MT-REQUEST-TYPE
               WHEN WS-OPEN-TYPE
                   PERFORM 3000-OPEN-ACCOUNT
               WHEN WS-CHANGE-TYPE
                   PERFORM 4000-CHANGE-ACCOUNT
               WHEN WS-CLOSE-TYPE
                   PERFORM 5000-CLOSE-ACCOUNT
               WHEN OTHER
                   MOVE "依頼区分不正" TO WS-REASON
                   SET HAS-ERROR TO TRUE
           END-EVALUATE

           IF HAS-ERROR
               PERFORM 8000-REJECT-REQUEST
           ELSE
               ADD 1 TO CNT-APPLIED
           END-IF.

       2200-VALIDATE-COMMON.
           IF MT-REQUEST-ID = SPACES
               MOVE "依頼番号未設定" TO WS-REASON
               SET HAS-ERROR TO TRUE
               EXIT PARAGRAPH
           END-IF

           IF MT-CUST-ID = SPACES
               MOVE "顧客番号未設定" TO WS-REASON
               SET HAS-ERROR TO TRUE
               EXIT PARAGRAPH
           END-IF

           IF MT-ACCT-NO = SPACES
               MOVE "口座番号未設定" TO WS-REASON
               SET HAS-ERROR TO TRUE
               EXIT PARAGRAPH
           END-IF

           MOVE MT-CUST-ID TO CU-CUST-ID
           READ KZCUSTF
               INVALID KEY
                   MOVE "顧客マスタ未登録" TO WS-REASON
                   SET HAS-ERROR TO TRUE
                   EXIT PARAGRAPH
           END-READ

           IF FS-KZCUSTF NOT = "00"
               MOVE "顧客マスタ読込失敗" TO WS-REASON
               SET HAS-ERROR TO TRUE
               EXIT PARAGRAPH
           END-IF

           IF CU-KYC-STATUS NOT = "1"
           AND CU-KYC-STATUS NOT = "0"
           AND CU-KYC-STATUS NOT = "9"
               MOVE "本人確認状態不正" TO WS-REASON
               SET HAS-ERROR TO TRUE
               EXIT PARAGRAPH
           END-IF.

       3000-OPEN-ACCOUNT.
           MOVE MT-ACCT-NO TO AC-ACCT-NO
           READ KZACCTF
               INVALID KEY
                   CONTINUE
           END-READ

           IF FS-KZACCTF = "00"
               MOVE "口座番号重複" TO WS-REASON
               SET HAS-ERROR TO TRUE
               EXIT PARAGRAPH
           END-IF

           IF FS-KZACCTF NOT = "23"
               MOVE "口座マスタ重複確認失敗" TO WS-REASON
               SET HAS-ERROR TO TRUE
               EXIT PARAGRAPH
           END-IF

           INITIALIZE KZACCTF-REC
           MOVE MT-ACCT-NO TO AC-ACCT-NO
           MOVE MT-CUST-ID TO AC-CUST-ID
           MOVE MT-NEW-VALUE(1:2) TO AC-ACCT-TYPE
           MOVE SPACES TO AC-GROUP-CODE
           MOVE WS-ZERO-BAL TO AC-CUR-BAL
           MOVE WS-ZERO-BAL TO AC-AVG-BAL
           MOVE WS-ZERO-AMT TO AC-CREDIT-LIMIT

           PERFORM 3100-VALIDATE-ACCT-TYPE
           IF HAS-ERROR
               EXIT PARAGRAPH
           END-IF

           IF AC-ACCT-TYPE = WS-PROD-CARD
               OR AC-ACCT-TYPE = WS-PROD-LOAN
               IF MT-APPROVAL-ID = SPACES
                   MOVE "承認番号未設定" TO WS-REASON
                   SET HAS-ERROR TO TRUE
                   EXIT PARAGRAPH
               END-IF
           END-IF

           WRITE KZACCTF-REC
               INVALID KEY
                   MOVE "口座マスタ追加失敗" TO WS-REASON
                   SET HAS-ERROR TO TRUE
           END-WRITE

           IF FS-KZACCTF NOT = "00"
               MOVE "口座マスタ書込失敗" TO WS-REASON
               SET HAS-ERROR TO TRUE
           END-IF.

       3100-VALIDATE-ACCT-TYPE.
           IF AC-ACCT-TYPE NOT = WS-PROD-CARD
               AND AC-ACCT-TYPE NOT = WS-PROD-OVER
               AND AC-ACCT-TYPE NOT = WS-PROD-LOAN
               MOVE "口座種別不正" TO WS-REASON
               SET HAS-ERROR TO TRUE
               EXIT PARAGRAPH
           END-IF.

       4000-CHANGE-ACCOUNT.
           MOVE MT-ACCT-NO TO AC-ACCT-NO
           READ KZACCTF
               INVALID KEY
                   MOVE "口座マスタ未登録" TO WS-REASON
                   SET HAS-ERROR TO TRUE
                   EXIT PARAGRAPH
           END-READ

           IF FS-KZACCTF NOT = "00"
               MOVE "口座マスタ読込失敗" TO WS-REASON
               SET HAS-ERROR TO TRUE
               EXIT PARAGRAPH
           END-IF

           IF AC-CUST-ID NOT = MT-CUST-ID
               MOVE "口座顧客不一致" TO WS-REASON
               SET HAS-ERROR TO TRUE
               EXIT PARAGRAPH
           END-IF

           EVALUATE MT-FIELD-CODE
               WHEN WS-FLD-ACCT-TYPE
                   MOVE MT-NEW-VALUE(1:2) TO AC-ACCT-TYPE
                   PERFORM 3100-VALIDATE-ACCT-TYPE
               WHEN WS-FLD-GROUP
                   MOVE MT-NEW-VALUE(1:6) TO AC-GROUP-CODE
               WHEN WS-FLD-LIMIT
                   PERFORM 4100-VALIDATE-LIMIT
                   IF NO-ERROR
                       MOVE WS-LIMIT-N TO AC-CREDIT-LIMIT
                   END-IF
               WHEN OTHER
                   MOVE "変更項目不正" TO WS-REASON
                   SET HAS-ERROR TO TRUE
           END-EVALUATE

           IF HAS-ERROR
               EXIT PARAGRAPH
           END-IF

           PERFORM 4200-VALIDATE-REQUIRED
           IF HAS-ERROR
               EXIT PARAGRAPH
           END-IF

           REWRITE KZACCTF-REC
               INVALID KEY
                   MOVE "口座マスタ更新失敗" TO WS-REASON
                   SET HAS-ERROR TO TRUE
           END-REWRITE

           IF FS-KZACCTF NOT = "00"
               MOVE "口座マスタ更新失敗" TO WS-REASON
               SET HAS-ERROR TO TRUE
           END-IF.

       4100-VALIDATE-LIMIT.
           MOVE ZERO TO WS-LIMIT-N
           MOVE SPACES TO WS-NUMERIC-TEXT
           MOVE MT-NEW-VALUE TO WS-NEW-VALUE
           MOVE WS-NEW-VALUE(1:15) TO WS-NUMERIC-TEXT
           SET DIGIT-NOT-SEEN TO TRUE
           SET SPACE-NOT-SEEN TO TRUE

           PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > 15
               MOVE WS-NUMERIC-TEXT(WS-IDX:1) TO WS-CHAR
               IF WS-CHAR = SPACE
                   SET SPACE-SEEN TO TRUE
               ELSE
                   IF SPACE-SEEN
                       MOVE "限度額数字不正" TO WS-REASON
                       SET HAS-ERROR TO TRUE
                       EXIT PERFORM
                   END-IF
                   IF WS-CHAR < "0" OR WS-CHAR > "9"
                       MOVE "限度額数字不正" TO WS-REASON
                       SET HAS-ERROR TO TRUE
                       EXIT PERFORM
                   END-IF
                   SET DIGIT-SEEN TO TRUE
               END-IF
           END-PERFORM

           IF HAS-ERROR
               EXIT PARAGRAPH
           END-IF

           IF DIGIT-NOT-SEEN
               MOVE "限度額未設定" TO WS-REASON
               SET HAS-ERROR TO TRUE
               EXIT PARAGRAPH
           END-IF

           COMPUTE WS-LIMIT-N = FUNCTION NUMVAL(WS-NUMERIC-TEXT)

           IF WS-LIMIT-N > WS-MAX-LIMIT
               MOVE "限度額入力上限超過" TO WS-REASON
               SET HAS-ERROR TO TRUE
               EXIT PARAGRAPH
           END-IF.

       4200-VALIDATE-REQUIRED.
           PERFORM 3100-VALIDATE-ACCT-TYPE
           IF HAS-ERROR
               EXIT PARAGRAPH
           END-IF

           IF AC-ACCT-TYPE = WS-PROD-OVER
               AND AC-GROUP-CODE = SPACES
               MOVE "当座貸越グループ未設定" TO WS-REASON
               SET HAS-ERROR TO TRUE
               EXIT PARAGRAPH
           END-IF

           IF AC-ACCT-TYPE = WS-PROD-CARD
               AND AC-CREDIT-LIMIT = ZERO
               MOVE "カードローン限度額未設定" TO WS-REASON
               SET HAS-ERROR TO TRUE
               EXIT PARAGRAPH
           END-IF.

       5000-CLOSE-ACCOUNT.
           MOVE MT-ACCT-NO TO AC-ACCT-NO
           READ KZACCTF
               INVALID KEY
                   MOVE "口座マスタ未登録" TO WS-REASON
                   SET HAS-ERROR TO TRUE
                   EXIT PARAGRAPH
           END-READ

           IF FS-KZACCTF NOT = "00"
               MOVE "口座マスタ読込失敗" TO WS-REASON
               SET HAS-ERROR TO TRUE
               EXIT PARAGRAPH
           END-IF

           IF AC-CUST-ID NOT = MT-CUST-ID
               MOVE "口座顧客不一致" TO WS-REASON
               SET HAS-ERROR TO TRUE
               EXIT PARAGRAPH
           END-IF

           IF AC-CUR-BAL NOT = ZERO
               MOVE "閉鎖予約残高あり" TO WS-REASON
               SET HAS-ERROR TO TRUE
               EXIT PARAGRAPH
           END-IF

           MOVE WS-CLOSE-GROUP TO AC-GROUP-CODE
           REWRITE KZACCTF-REC
               INVALID KEY
                   MOVE "閉鎖予約更新失敗" TO WS-REASON
                   SET HAS-ERROR TO TRUE
           END-REWRITE

           IF FS-KZACCTF NOT = "00"
               MOVE "閉鎖予約更新失敗" TO WS-REASON
               SET HAS-ERROR TO TRUE
           END-IF.

       8000-REJECT-REQUEST.
           ADD 1 TO CNT-REJECTED
           DISPLAY "依頼却下 ID=" MT-REQUEST-ID
                   " 口座=" MT-ACCT-NO
                   " 理由=" WS-REASON.

       9000-FINAL.
           CLOSE KZMAINTF KZACCTF KZCUSTF

           IF RETURN-CODE = 0
               DISPLAY "KZ120B 正常終了"
               DISPLAY "読込件数=" CNT-READ
                       " 反映件数=" CNT-APPLIED
                       " 却下件数=" CNT-REJECTED
               MOVE 0 TO RETURN-CODE
           ELSE
               DISPLAY "KZ120B 異常終了 RC=" RETURN-CODE
           END-IF.
