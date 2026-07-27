       IDENTIFICATION DIVISION.
       PROGRAM-ID. KZ100B.
      *
      *変更履歴
      * 版数  年月日    担当    概要
      * 1.00  20240401  KZB01   初版作成
      * 1.01  20241015  KZB02   適用日検査を追加
      * 1.02  20250602  KZB03   顧客属性値検査を強化
      *
      * 顧客属性変更取込バッチ
      * 承認済み変更依頼を依頼ＩＤ順に処理し顧客マスタを更新する。
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZMAINTF ASSIGN TO "KZMAINTF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS MT-REQUEST-ID
               FILE STATUS IS FS-KZMAINTF.

           SELECT KZCUSTF ASSIGN TO "KZCUSTF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CU-CUST-ID
               FILE STATUS IS FS-KZCUSTF.

       DATA DIVISION.
       FILE SECTION.
       FD  KZMAINTF.
           COPY KZMAINTFC.

       FD  KZCUSTF.
           COPY KZCUSTC.

       WORKING-STORAGE SECTION.
       01  FILE-STATUS-AREA.
           05  FS-KZMAINTF              PIC X(02) VALUE SPACE.
           05  FS-KZCUSTF               PIC X(02) VALUE SPACE.

       01  CONTROL-AREA.
           05  END-SW                   PIC X(01) VALUE "N".
               88  END-OF-KZMAINTF                VALUE "Y".
           05  HARD-ERROR-SW            PIC X(01) VALUE "N".
               88  HARD-ERROR                     VALUE "Y".
           05  UPDATE-SW                PIC X(01) VALUE "N".
               88  UPDATE-NEEDED                  VALUE "Y".
           05  VALID-SW                 PIC X(01) VALUE "N".
               88  VALID-REQUEST                  VALUE "Y".
           05  SAVE-REQUEST-ID          PIC X(20) VALUE SPACE.
           05  PREV-REQUEST-ID          PIC X(20) VALUE LOW-VALUE.
           05  WS-CURRENT-DATE.
               10  WS-CURR-YYYY         PIC 9(04) VALUE ZERO.
               10  WS-CURR-MM           PIC 9(02) VALUE ZERO.
               10  WS-CURR-DD           PIC 9(02) VALUE ZERO.
           05  WS-CURRENT-YYYYMMDD      PIC 9(08) VALUE ZERO.
           05  WS-EFFECTIVE-DATE        PIC 9(08) VALUE ZERO.
           05  WS-BRANCH-NUM            PIC 9(03) VALUE ZERO.

       01  COUNT-AREA.
           05  CNT-READ                 PIC 9(09) VALUE ZERO.
           05  CNT-SKIP                 PIC 9(09) VALUE ZERO.
           05  CNT-UPDATE               PIC 9(09) VALUE ZERO.
           05  CNT-ERROR                PIC 9(09) VALUE ZERO.

       01  MESSAGE-AREA.
           05  MSG-REQUEST-ID           PIC X(20) VALUE SPACE.
           05  MSG-CUST-ID              PIC X(20) VALUE SPACE.
           05  MSG-REASON               PIC X(40) VALUE SPACE.

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE 8 TO RETURN-CODE
           PERFORM 1000-INIT
           IF NOT HARD-ERROR
               PERFORM 2000-PROCESS UNTIL END-OF-KZMAINTF
                  OR HARD-ERROR
           END-IF
           PERFORM 9000-FINAL
           GOBACK.

       1000-INIT.
           MOVE FUNCTION CURRENT-DATE(1:8) TO WS-CURRENT-DATE
           MOVE FUNCTION NUMVAL(WS-CURRENT-DATE) TO WS-CURRENT-YYYYMMDD

           OPEN INPUT KZMAINTF
           IF FS-KZMAINTF NOT = "00"
               DISPLAY "KZMAINTF オープン失敗 ST=" FS-KZMAINTF
               SET HARD-ERROR TO TRUE
           END-IF

           IF NOT HARD-ERROR
               OPEN I-O KZCUSTF
               IF FS-KZCUSTF NOT = "00"
                   DISPLAY "KZCUSTF オープン失敗 ST=" FS-KZCUSTF
                   SET HARD-ERROR TO TRUE
               END-IF
           END-IF

           IF NOT HARD-ERROR
               PERFORM 2100-READ-MAINT
           END-IF.

       2000-PROCESS.
           ADD 1 TO CNT-READ
           MOVE MT-REQUEST-ID TO SAVE-REQUEST-ID
           MOVE "N" TO VALID-SW
           MOVE "N" TO UPDATE-SW

           IF MT-REQUEST-ID < PREV-REQUEST-ID
               MOVE "依頼ＩＤ順序不正" TO MSG-REASON
               PERFORM 8100-REJECT-REQUEST
           ELSE
               MOVE MT-REQUEST-ID TO PREV-REQUEST-ID
               PERFORM 3000-VALIDATE-REQUEST
               IF VALID-REQUEST
                   PERFORM 4000-APPLY-REQUEST
               END-IF
           END-IF

           IF NOT HARD-ERROR
               PERFORM 2100-READ-MAINT
           END-IF.

       2100-READ-MAINT.
           READ KZMAINTF
               AT END
                   SET END-OF-KZMAINTF TO TRUE
               NOT AT END
                   IF FS-KZMAINTF NOT = "00"
                       DISPLAY "KZMAINTF 読込失敗 ST=" FS-KZMAINTF
                       SET HARD-ERROR TO TRUE
                   END-IF
           END-READ.

       3000-VALIDATE-REQUEST.
           IF MT-APPROVAL-ID = SPACE
               MOVE "承認番号未設定" TO MSG-REASON
               PERFORM 8100-REJECT-REQUEST
           ELSE
               IF MT-REQUEST-TYPE NOT = "C"
                   MOVE "依頼種別不正" TO MSG-REASON
                   PERFORM 8100-REJECT-REQUEST
               ELSE
                   PERFORM 3100-CHECK-DATE
                   IF VALID-REQUEST
                       PERFORM 3200-CHECK-FIELD
                   END-IF
               END-IF
           END-IF.

       3100-CHECK-DATE.
           IF MT-EFFECTIVE-DATE NOT NUMERIC
               MOVE "適用日数字不正" TO MSG-REASON
               PERFORM 8100-REJECT-REQUEST
           ELSE
               MOVE MT-EFFECTIVE-DATE TO WS-EFFECTIVE-DATE
               IF WS-EFFECTIVE-DATE > WS-CURRENT-YYYYMMDD
                   MOVE "適用日未来日" TO MSG-REASON
                   PERFORM 8100-REJECT-REQUEST
               ELSE
                   IF MT-EFFECTIVE-DATE(5:2) < "01"
                   OR MT-EFFECTIVE-DATE(5:2) > "12"
                   OR MT-EFFECTIVE-DATE(7:2) < "01"
                   OR MT-EFFECTIVE-DATE(7:2) > "31"
                       MOVE "適用日範囲不正" TO MSG-REASON
                       PERFORM 8100-REJECT-REQUEST
                   ELSE
                       SET VALID-REQUEST TO TRUE
                   END-IF
               END-IF
           END-IF.

       3200-CHECK-FIELD.
           EVALUATE MT-FIELD-CODE
               WHEN "BR"
                   PERFORM 3210-CHECK-BRANCH
               WHEN "KY"
                   PERFORM 3220-CHECK-KYC
               WHEN OTHER
                   MOVE "変更項目不正" TO MSG-REASON
                   PERFORM 8100-REJECT-REQUEST
           END-EVALUATE.

       3210-CHECK-BRANCH.
           IF MT-NEW-VALUE(1:3) NOT NUMERIC
               MOVE "店番数字不正" TO MSG-REASON
               PERFORM 8100-REJECT-REQUEST
           ELSE
               MOVE MT-NEW-VALUE(1:3) TO WS-BRANCH-NUM
               IF WS-BRANCH-NUM = ZERO
                   MOVE "店番範囲不正" TO MSG-REASON
                   PERFORM 8100-REJECT-REQUEST
               ELSE
                   SET VALID-REQUEST TO TRUE
               END-IF
           END-IF.

       3220-CHECK-KYC.
           IF MT-NEW-VALUE(1:1) = "0"
           OR MT-NEW-VALUE(1:1) = "1"
           OR MT-NEW-VALUE(1:1) = "9"
               SET VALID-REQUEST TO TRUE
           ELSE
               MOVE "本人確認状態不正" TO MSG-REASON
               PERFORM 8100-REJECT-REQUEST
           END-IF.

       4000-APPLY-REQUEST.
           MOVE MT-CUST-ID TO CU-CUST-ID
           READ KZCUSTF
               KEY IS CU-CUST-ID
               INVALID KEY
                   MOVE "顧客マスタ未登録" TO MSG-REASON
                   PERFORM 8100-REJECT-REQUEST
               NOT INVALID KEY
                   IF FS-KZCUSTF NOT = "00"
                       DISPLAY "KZCUSTF 読込失敗 ST=" FS-KZCUSTF
                       SET HARD-ERROR TO TRUE
                   ELSE
                       PERFORM 4100-SET-CUSTOMER
                       IF UPDATE-NEEDED
                           PERFORM 4200-REWRITE-CUSTOMER
                       ELSE
                           ADD 1 TO CNT-SKIP
                       END-IF
                   END-IF
           END-READ.

       4100-SET-CUSTOMER.
           EVALUATE MT-FIELD-CODE
               WHEN "BR"
                   IF CU-BRANCH-CODE NOT = MT-NEW-VALUE(1:3)
                       MOVE MT-NEW-VALUE(1:3) TO CU-BRANCH-CODE
                       SET UPDATE-NEEDED TO TRUE
                   END-IF
               WHEN "KY"
                   IF CU-KYC-STATUS NOT = MT-NEW-VALUE(1:1)
                       MOVE MT-NEW-VALUE(1:1) TO CU-KYC-STATUS
                       SET UPDATE-NEEDED TO TRUE
                   END-IF
           END-EVALUATE.

       4200-REWRITE-CUSTOMER.
           REWRITE KZCUSTF-REC
           IF FS-KZCUSTF = "00"
               ADD 1 TO CNT-UPDATE
           ELSE
               DISPLAY "KZCUSTF 更新失敗 ST=" FS-KZCUSTF
                   " 依頼ＩＤ=" SAVE-REQUEST-ID
               SET HARD-ERROR TO TRUE
           END-IF.

       8100-REJECT-REQUEST.
           ADD 1 TO CNT-ERROR
           MOVE MT-REQUEST-ID TO MSG-REQUEST-ID
           MOVE MT-CUST-ID TO MSG-CUST-ID
           DISPLAY "変更依頼エラー 依頼ＩＤ=" MSG-REQUEST-ID
               " 顧客ＩＤ=" MSG-CUST-ID
               " 理由=" MSG-REASON.

       9000-FINAL.
           IF FS-KZMAINTF NOT = SPACE
               CLOSE KZMAINTF
           END-IF
           IF FS-KZCUSTF NOT = SPACE
               CLOSE KZCUSTF
           END-IF

           DISPLAY "KZ110B 処理件数 読込=" CNT-READ
               " 更新=" CNT-UPDATE
               " 未更新=" CNT-SKIP
               " エラー=" CNT-ERROR

           IF HARD-ERROR
               MOVE 12 TO RETURN-CODE
               DISPLAY "KZ110B 異常終了"
           ELSE
               MOVE 0 TO RETURN-CODE
               DISPLAY "KZ110B 正常終了"
           END-IF.
