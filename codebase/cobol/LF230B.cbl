       IDENTIFICATION DIVISION.
       PROGRAM-ID. LF230B.
      *加入年齢区分メンテ取込
      *
      * 版数    年月日        担当      概要
      * 1.0     20190401      保険計理部  初版
      * 1.1     20200615      保険計理部  逆転・重複・未登録チェック追加
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LFAGBF ASSIGN TO LFAGBF-FILE
               ORGANIZATION IS INDEXED
               RECORD KEY IS AB-BAND-KBN
               FILE STATUS IS LFAGBF-STATUS.
           SELECT MAINT-INPUT ASSIGN TO MAINT-INPUT-FILE
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS INPUT-STATUS.
       
       DATA DIVISION.
       FILE SECTION.
       FD LFAGBF.
       COPY LFAGBFC.
       
       FD MAINT-INPUT.
       01 MAINT-REC.
           05 MI-BAND-KBN PIC 9(3).
           05 MI-AGE-FROM PIC 9(3).
           05 MI-AGE-TO PIC 9(3).
           05 MI-VALID-FROM-YM PIC 9(6).
           05 MI-VALID-TO-YM PIC 9(6).
           05 FILLER PIC X(59).
       
       WORKING-STORAGE SECTION.
       01 FILE-STATUS-SECTION.
           05 LFAGBF-STATUS PIC XX VALUE '00'.
           05 INPUT-STATUS PIC XX VALUE '00'.
       
       01 PROGRAM-CONTROL.
           05 EOF-MAINT PIC X VALUE 'N'.
           05 ERROR-FLG PIC X VALUE 'N'.
           05 VALID-FLG PIC X VALUE 'Y'.
       
       01 COUNTERS.
           05 TOTAL-REC-COUNT PIC 9(6) VALUE 0.
           05 ACCEPTED-COUNT PIC 9(6) VALUE 0.
           05 REJECTED-COUNT PIC 9(6) VALUE 0.
           05 SAMPLE-VERIFIED PIC 9(3) VALUE 0.
           05 SAMPLE-ERRORS PIC 9(3) VALUE 0.
       
       01 VALIDATION-ERRORS.
           05 AGE-INVERT-FLG PIC X VALUE 'N'.
           05 PERIOD-INVERT-FLG PIC X VALUE 'N'.
           05 BAND-UNREG-FLG PIC X VALUE 'N'.
           05 BOUNDARY-ERR-FLG PIC X VALUE 'N'.
       
       01 WORK-STORAGE.
           05 WK-BAND-KBN PIC 9(3).
           05 WK-AGE-FROM PIC 9(3).
           05 WK-AGE-TO PIC 9(3).
           05 WK-VALID-FROM-YM PIC 9(6).
           05 WK-VALID-TO-YM PIC 9(6).
           05 WK-REC-REMAINDER PIC 9(3).
           05 WK-SAMPLE-MOD PIC 9(3) VALUE 10.
           05 WK-STATUS-CODE PIC 9(2).
       
       PROCEDURE DIVISION.
       MAIN-PROC.
           PERFORM OPEN-FILES.
           IF ERROR-FLG = 'Y'
               MOVE 8 TO RETURN-CODE
               PERFORM CLOSE-FILES
               GOBACK
           END-IF.
           
           PERFORM PROCESS-INPUT-RECORDS.
           
           PERFORM CLOSE-FILES.
           
           PERFORM REPORT-SUMMARY.
           
           IF ERROR-FLG = 'Y'
               MOVE 12 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.
           
           GOBACK.
       
       OPEN-FILES.
           OPEN INPUT MAINT-INPUT.
           IF INPUT-STATUS NOT = '00'
               MOVE 'メンテ入力ファイルOPEN失敗 ST=' 
                   TO ERROR-FLG
               DISPLAY 'メンテ入力ファイルOPEN失敗 ST='
                   INPUT-STATUS
               MOVE 'Y' TO ERROR-FLG
               EXIT PARAGRAPH
           END-IF.
           
           OPEN I-O LFAGBF.
           IF LFAGBF-STATUS NOT = '00'
               DISPLAY 'LFAGBF ファイルOPEN失敗 ST='
                   LFAGBF-STATUS
               MOVE 'Y' TO ERROR-FLG
               CLOSE MAINT-INPUT
           END-IF.
       
       CLOSE-FILES.
           CLOSE MAINT-INPUT.
           CLOSE LFAGBF.
       
       PROCESS-INPUT-RECORDS.
           MOVE 'N' TO EOF-MAINT.
           
           PERFORM UNTIL EOF-MAINT = 'Y'
               PERFORM READ-MAINT-RECORD
               IF EOF-MAINT = 'N'
                   ADD 1 TO TOTAL-REC-COUNT
                   MOVE 'Y' TO VALID-FLG
                   MOVE 'N' TO AGE-INVERT-FLG
                   MOVE 'N' TO PERIOD-INVERT-FLG
                   MOVE 'N' TO BAND-UNREG-FLG
                   MOVE 'N' TO BOUNDARY-ERR-FLG
                   
                   PERFORM CHECK-AGE-INVERSION
                   PERFORM CHECK-PERIOD-INVERSION
                   PERFORM CHECK-BAND-REGISTRATION
                   PERFORM SAMPLE-VERIFY-BOUNDARY
                   
                   PERFORM UPDATE-LFAGBF
               END-IF
           END-PERFORM.
       
       READ-MAINT-RECORD.
           READ MAINT-INPUT INTO MAINT-REC
               AT END
                   MOVE 'Y' TO EOF-MAINT
               NOT AT END
                   CONTINUE
           END-READ.
           
           IF INPUT-STATUS NOT = '00' AND
              INPUT-STATUS NOT = '10'
               DISPLAY 'メンテ入力読込エラー ST='
                   INPUT-STATUS
               MOVE 'Y' TO ERROR-FLG
               MOVE 'Y' TO EOF-MAINT
           END-IF.
       
       CHECK-AGE-INVERSION.
           IF MI-AGE-FROM >= MI-AGE-TO
               MOVE 'N' TO VALID-FLG
               MOVE 'Y' TO AGE-INVERT-FLG
               DISPLAY '年齢逆転エラー: FROM=' MI-AGE-FROM
                   ' TO=' MI-AGE-TO ' REC#=' TOTAL-REC-COUNT
           END-IF.
       
       CHECK-PERIOD-INVERSION.
           IF MI-VALID-FROM-YM > MI-VALID-TO-YM
               MOVE 'N' TO VALID-FLG
               MOVE 'Y' TO PERIOD-INVERT-FLG
               DISPLAY '期間逆転エラー: FROM=' MI-VALID-FROM-YM
                   ' TO=' MI-VALID-TO-YM ' REC#=' TOTAL-REC-COUNT
           END-IF.
       
       CHECK-BAND-REGISTRATION.
           IF MI-BAND-KBN = 0 OR MI-BAND-KBN > 999
               MOVE 'N' TO VALID-FLG
               MOVE 'Y' TO BAND-UNREG-FLG
               DISPLAY 'BAND-KBN未登録コード: ' MI-BAND-KBN
                   ' REC#=' TOTAL-REC-COUNT
           END-IF.
       
       SAMPLE-VERIFY-BOUNDARY.
           DIVIDE TOTAL-REC-COUNT BY WK-SAMPLE-MOD
               GIVING WK-SAMPLE-MOD REMAINDER WK-REC-REMAINDER.
           
           IF WK-REC-REMAINDER = 0
               ADD 1 TO SAMPLE-VERIFIED
               PERFORM CHECK-BOUNDARY-CONSISTENCY
           END-IF.
       
       CHECK-BOUNDARY-CONSISTENCY.
           IF MI-AGE-FROM < 0 OR MI-AGE-FROM > 120
               MOVE 'N' TO VALID-FLG
               MOVE 'Y' TO BOUNDARY-ERR-FLG
               ADD 1 TO SAMPLE-ERRORS
               DISPLAY '境界年齢不一致: AGE-FROM=' MI-AGE-FROM
                   ' REC#=' TOTAL-REC-COUNT
           END-IF.
           
           IF MI-AGE-TO < 0 OR MI-AGE-TO > 120
               MOVE 'N' TO VALID-FLG
               MOVE 'Y' TO BOUNDARY-ERR-FLG
               ADD 1 TO SAMPLE-ERRORS
               DISPLAY '境界年齢不一致: AGE-TO=' MI-AGE-TO
                   ' REC#=' TOTAL-REC-COUNT
           END-IF.
       
       UPDATE-LFAGBF.
           MOVE MI-BAND-KBN TO AB-BAND-KBN.
           MOVE MI-AGE-FROM TO AB-AGE-FROM.
           MOVE MI-AGE-TO TO AB-AGE-TO.
           MOVE MI-VALID-FROM-YM TO AB-VALID-FROM-YM.
           MOVE MI-VALID-TO-YM TO AB-VALID-TO-YM.
           
           IF VALID-FLG = 'Y'
               MOVE 0 TO AB-MAINT-STATUS-KBN
               ADD 1 TO ACCEPTED-COUNT
           ELSE
               IF AGE-INVERT-FLG = 'Y'
                   MOVE 1 TO AB-MAINT-STATUS-KBN
               ELSE IF PERIOD-INVERT-FLG = 'Y'
                   MOVE 2 TO AB-MAINT-STATUS-KBN
               ELSE IF BAND-UNREG-FLG = 'Y'
                   MOVE 3 TO AB-MAINT-STATUS-KBN
               ELSE IF BOUNDARY-ERR-FLG = 'Y'
                   MOVE 4 TO AB-MAINT-STATUS-KBN
               ELSE
                   MOVE 9 TO AB-MAINT-STATUS-KBN
               END-IF
               END-IF
               END-IF
               END-IF
               ADD 1 TO REJECTED-COUNT
           END-IF.
           
           WRITE LFAGBF-REC
               INVALID KEY
                   MOVE 'Y' TO ERROR-FLG
                   DISPLAY 'LFAGBF 追加失敗: KEY=' MI-BAND-KBN
                       ' ST=' LFAGBF-STATUS
               NOT INVALID KEY
                   CONTINUE
           END-WRITE.
       
       REPORT-SUMMARY.
           DISPLAY '====== 処理結果報告 ======'.
           DISPLAY '処理レコード合計 : ' TOTAL-REC-COUNT.
           DISPLAY '受付レコード数   : ' ACCEPTED-COUNT.
           DISPLAY '却下レコード数   : ' REJECTED-COUNT.
           DISPLAY 'サンプル検証数   : ' SAMPLE-VERIFIED.
           DISPLAY 'サンプル不一致   : ' SAMPLE-ERRORS.
           
           IF ERROR-FLG = 'Y'
               DISPLAY '処理状態: 異常終了'
           ELSE
               DISPLAY '処理状態: 正常終了'
           END-IF.
           
           DISPLAY '========================'.
