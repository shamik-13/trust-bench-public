       IDENTIFICATION DIVISION.
       PROGRAM-ID. CB117B.
      ******************************************************************
      * 変更履歴
      * 版数  年月日    担当    概要
      * 1.00  20240401  T001    初版作成
      * 1.01  20240819  T002    保留期限判定追加
      * 1.02  20241107  T003    請求サイクル集計追加
      ******************************************************************
      * 加盟店売上データを承認履歴、カード属性と照合し、
      * 確定可能な売上のみ計上する。
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CDAUTHF ASSIGN TO "CDAUTHF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS AU-AUTH-ID
               FILE STATUS IS FS-CDAUTHF.
           SELECT CDSALESF ASSIGN TO "CDSALESF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS SL-SALES-ID
               FILE STATUS IS FS-CDSALESF.
           SELECT CDCARDF ASSIGN TO "CDCARDF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CF-CARD-NO
               FILE STATUS IS FS-CDCARDF.
           SELECT CDBALF ASSIGN TO "CDBALF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS FS-CDBALF.

       DATA DIVISION.
       FILE SECTION.
       FD  CDAUTHF.
       COPY CDAUTHC.

       FD  CDSALESF.
       COPY CDSALEC.

       FD  CDCARDF.
       COPY CDCARDFC.

       FD  CDBALF.
       COPY CDBALFC.

       WORKING-STORAGE SECTION.
       01  FS-CDAUTHF                  PIC X(02) VALUE SPACE.
       01  FS-CDSALESF                 PIC X(02) VALUE SPACE.
       01  FS-CDCARDF                  PIC X(02) VALUE SPACE.
       01  FS-CDBALF                   PIC X(02) VALUE SPACE.

       01  WS-END-SW                   PIC X VALUE "N".
           88  END-OF-SALES                  VALUE "Y".
       01  WS-ABEND-SW                 PIC X VALUE "N".
           88  ABEND-DETECTED                VALUE "Y".

       01  WS-TODAY                    PIC 9(08) VALUE ZERO.
       01  WS-SALES-TOTAL              PIC 9(09) VALUE ZERO.
       01  WS-CONFIRM-TOTAL            PIC 9(09) VALUE ZERO.
       01  WS-SKIP-TOTAL               PIC 9(09) VALUE ZERO.

       01  WS-POST-AMT                 PIC S9(13)V99 COMP-3
                                               VALUE ZERO.
       01  WS-TABLE-IDX                PIC 9(04) COMP VALUE ZERO.
       01  WS-FREE-IDX                 PIC 9(04) COMP VALUE ZERO.
       01  WS-BAL-COUNT                PIC 9(04) COMP VALUE ZERO.
       01  WS-FOUND-SW                 PIC X VALUE "N".
           88  BAL-FOUND                     VALUE "Y".

       01  WS-BAL-TABLE.
           05  WS-BAL-ENTRY OCCURS 5000 TIMES.
               10  TBL-CARD-NO        PIC X(19).
               10  TBL-CYCLE-DT       PIC 9(08).
               10  TBL-ADD-AMT        PIC S9(13)V99 COMP-3.

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE 8 TO RETURN-CODE
           ACCEPT WS-TODAY FROM DATE YYYYMMDD
           PERFORM 1000-OPEN-FILES
           IF NOT ABEND-DETECTED
              PERFORM 2000-PROCESS-SALES
           END-IF
           IF NOT ABEND-DETECTED
              PERFORM 5000-WRITE-BALANCE
           END-IF
           PERFORM 9000-CLOSE-FILES
           IF ABEND-DETECTED
              MOVE 8 TO RETURN-CODE
           ELSE
              DISPLAY "CB210B 正常終了 売上件数="
                      WS-SALES-TOTAL
                      " 確定件数="
                      WS-CONFIRM-TOTAL
                      " 対象外件数="
                      WS-SKIP-TOTAL
              MOVE 0 TO RETURN-CODE
           END-IF
           GOBACK.

       1000-OPEN-FILES.
           OPEN INPUT CDAUTHF
           IF FS-CDAUTHF NOT = "00"
              DISPLAY "CDAUTHF オープン失敗 ST="
                      FS-CDAUTHF
              SET ABEND-DETECTED TO TRUE
           END-IF
           OPEN I-O CDSALESF
           IF FS-CDSALESF NOT = "00"
              DISPLAY "CDSALESF オープン失敗 ST="
                      FS-CDSALESF
              SET ABEND-DETECTED TO TRUE
           END-IF
           OPEN INPUT CDCARDF
           IF FS-CDCARDF NOT = "00"
              DISPLAY "CDCARDF オープン失敗 ST="
                      FS-CDCARDF
              SET ABEND-DETECTED TO TRUE
           END-IF
           OPEN OUTPUT CDBALF
           IF FS-CDBALF NOT = "00"
              DISPLAY "CDBALF オープン失敗 ST="
                      FS-CDBALF
              SET ABEND-DETECTED TO TRUE
           END-IF.

       2000-PROCESS-SALES.
           MOVE LOW-VALUE TO SL-SALES-ID
           START CDSALESF KEY IS NOT LESS THAN SL-SALES-ID
              INVALID KEY
                 IF FS-CDSALESF = "23"
                    SET END-OF-SALES TO TRUE
                 ELSE
                    DISPLAY "CDSALESF START 失敗 ST="
                            FS-CDSALESF
                    SET ABEND-DETECTED TO TRUE
                 END-IF
           END-START
           PERFORM UNTIL END-OF-SALES OR ABEND-DETECTED
              READ CDSALESF NEXT RECORD
                 AT END
                    SET END-OF-SALES TO TRUE
                 NOT AT END
                    IF FS-CDSALESF = "00"
                       ADD 1 TO WS-SALES-TOTAL
                       PERFORM 3000-JUDGE-SALES
                    ELSE
                       DISPLAY "CDSALESF 読込失敗 ST="
                               FS-CDSALESF
                       SET ABEND-DETECTED TO TRUE
                    END-IF
              END-READ
           END-PERFORM.

       3000-JUDGE-SALES.
           EVALUATE TRUE
              WHEN SL-CAPTURE-STATUS = "1"
                 CONTINUE
              WHEN SL-CAPTURE-STATUS = "9"
                 CONTINUE
              WHEN SL-AUTH-ID = SPACE
                 DISPLAY "承認番号未設定 売上ID="
                         SL-SALES-ID
                 MOVE "9" TO SL-CAPTURE-STATUS
                 PERFORM 3900-REWRITE-SALES
                 ADD 1 TO WS-SKIP-TOTAL
              WHEN OTHER
                 PERFORM 3100-READ-AUTH
                 IF NOT ABEND-DETECTED
                    PERFORM 3200-READ-CARD
                 END-IF
                 IF NOT ABEND-DETECTED
                    PERFORM 3300-VALIDATE-AND-POST
                 END-IF
           END-EVALUATE.

       3100-READ-AUTH.
           MOVE SL-AUTH-ID TO AU-AUTH-ID
           READ CDAUTHF KEY IS AU-AUTH-ID
              INVALID KEY
                 IF FS-CDAUTHF = "23"
                    DISPLAY "承認履歴なし 売上ID="
                            SL-SALES-ID
                    MOVE "9" TO SL-CAPTURE-STATUS
                    PERFORM 3900-REWRITE-SALES
                    ADD 1 TO WS-SKIP-TOTAL
                 ELSE
                    DISPLAY "CDAUTHF 読込失敗 ST="
                            FS-CDAUTHF
                    SET ABEND-DETECTED TO TRUE
                 END-IF
              NOT INVALID KEY
                 IF FS-CDAUTHF NOT = "00"
                    DISPLAY "CDAUTHF 読込失敗 ST="
                            FS-CDAUTHF
                    SET ABEND-DETECTED TO TRUE
                 END-IF
           END-READ.

       3200-READ-CARD.
           IF SL-CAPTURE-STATUS = "9"
              EXIT PARAGRAPH
           END-IF
           MOVE SL-CARD-NO TO CF-CARD-NO
           READ CDCARDF KEY IS CF-CARD-NO
              INVALID KEY
                 IF FS-CDCARDF = "23"
                    DISPLAY "カード属性なし 売上ID="
                            SL-SALES-ID
                    MOVE "9" TO SL-CAPTURE-STATUS
                    PERFORM 3900-REWRITE-SALES
                    ADD 1 TO WS-SKIP-TOTAL
                 ELSE
                    DISPLAY "CDCARDF 読込失敗 ST="
                            FS-CDCARDF
                    SET ABEND-DETECTED TO TRUE
                 END-IF
              NOT INVALID KEY
                 IF FS-CDCARDF NOT = "00"
                    DISPLAY "CDCARDF 読込失敗 ST="
                            FS-CDCARDF
                    SET ABEND-DETECTED TO TRUE
                 END-IF
           END-READ.

       3300-VALIDATE-AND-POST.
           IF SL-CAPTURE-STATUS = "9"
              EXIT PARAGRAPH
           END-IF
           COMPUTE WS-POST-AMT = SL-SALES-AMT + SL-TAX-AMT
           EVALUATE TRUE
              WHEN AU-AUTH-RESULT NOT = "00"
                 DISPLAY "承認結果不可 売上ID="
                         SL-SALES-ID
                 MOVE "9" TO SL-CAPTURE-STATUS
                 ADD 1 TO WS-SKIP-TOTAL
              WHEN SL-CARD-NO NOT = AU-CARD-NO
                 DISPLAY "カード番号不一致 売上ID="
                         SL-SALES-ID
                 MOVE "9" TO SL-CAPTURE-STATUS
                 ADD 1 TO WS-SKIP-TOTAL
              WHEN SL-MERCHANT-ID NOT = AU-MERCHANT-ID
                 DISPLAY "加盟店番号不一致 売上ID="
                         SL-SALES-ID
                 MOVE "9" TO SL-CAPTURE-STATUS
                 ADD 1 TO WS-SKIP-TOTAL
              WHEN WS-POST-AMT > AU-AUTH-AMT
                 DISPLAY "承認金額超過 売上ID="
                         SL-SALES-ID
                 MOVE "9" TO SL-CAPTURE-STATUS
                 ADD 1 TO WS-SKIP-TOTAL
              WHEN AU-HOLD-EXP-DT < SL-SALES-DT
                 DISPLAY "保留期限超過 売上ID="
                         SL-SALES-ID
                 MOVE "9" TO SL-CAPTURE-STATUS
                 ADD 1 TO WS-SKIP-TOTAL
              WHEN CF-CARD-STATUS = "02"
                 DISPLAY "カード利用停止 売上ID="
                         SL-SALES-ID
                 MOVE "9" TO SL-CAPTURE-STATUS
                 ADD 1 TO WS-SKIP-TOTAL
              WHEN CF-CARD-STATUS = "03"
                 DISPLAY "カード解約済 売上ID="
                         SL-SALES-ID
                 MOVE "9" TO SL-CAPTURE-STATUS
                 ADD 1 TO WS-SKIP-TOTAL
              WHEN CF-CARD-STATUS NOT = "01"
               AND CF-CARD-STATUS NOT = "09"
                 DISPLAY "カード状態不正 売上ID="
                         SL-SALES-ID
                 MOVE "9" TO SL-CAPTURE-STATUS
                 ADD 1 TO WS-SKIP-TOTAL
              WHEN OTHER
                 MOVE "1" TO SL-CAPTURE-STATUS
                 MOVE WS-TODAY TO SL-POSTING-DT
                 PERFORM 3400-ADD-BAL-TABLE
                 ADD 1 TO WS-CONFIRM-TOTAL
           END-EVALUATE
           PERFORM 3900-REWRITE-SALES.

       3400-ADD-BAL-TABLE.
           MOVE "N" TO WS-FOUND-SW
           PERFORM VARYING WS-TABLE-IDX FROM 1 BY 1
             UNTIL WS-TABLE-IDX > WS-BAL-COUNT OR BAL-FOUND
              IF TBL-CARD-NO(WS-TABLE-IDX) = SL-CARD-NO
               AND TBL-CYCLE-DT(WS-TABLE-IDX) = CF-BILL-CYCLE-CD
                 ADD WS-POST-AMT TO TBL-ADD-AMT(WS-TABLE-IDX)
                 SET BAL-FOUND TO TRUE
              END-IF
           END-PERFORM
           IF NOT BAL-FOUND
              IF WS-BAL-COUNT < 5000
                 ADD 1 TO WS-BAL-COUNT
                 MOVE WS-BAL-COUNT TO WS-FREE-IDX
                 MOVE SL-CARD-NO TO TBL-CARD-NO(WS-FREE-IDX)
                 MOVE CF-BILL-CYCLE-CD TO TBL-CYCLE-DT(WS-FREE-IDX)
                 MOVE WS-POST-AMT TO TBL-ADD-AMT(WS-FREE-IDX)
              ELSE
                 DISPLAY "集計表オーバーフロー"
                 SET ABEND-DETECTED TO TRUE
              END-IF
           END-IF.

       3900-REWRITE-SALES.
           REWRITE CDSALESF-REC
              INVALID KEY
                 DISPLAY "CDSALESF 更新失敗 売上ID="
                         SL-SALES-ID
                         " ST="
                         FS-CDSALESF
                 SET ABEND-DETECTED TO TRUE
           END-REWRITE
           IF FS-CDSALESF NOT = "00"
              DISPLAY "CDSALESF 更新異常 売上ID="
                      SL-SALES-ID
                      " ST="
                      FS-CDSALESF
              SET ABEND-DETECTED TO TRUE
           END-IF.

       5000-WRITE-BALANCE.
           PERFORM VARYING WS-TABLE-IDX FROM 1 BY 1
             UNTIL WS-TABLE-IDX > WS-BAL-COUNT OR ABEND-DETECTED
              INITIALIZE CDBALF-REC
              MOVE TBL-CARD-NO(WS-TABLE-IDX) TO BL-CARD-NO
              MOVE TBL-CYCLE-DT(WS-TABLE-IDX) TO BL-CYCLE-DT
              MOVE ZERO TO BL-CLOSING-BAL-AMT
              MOVE ZERO TO BL-REVOLVING-BAL-AMT
              MOVE TBL-ADD-AMT(WS-TABLE-IDX) TO BL-NEW-CHARGE-AMT
              MOVE ZERO TO BL-CASH-ADV-AMT
              WRITE CDBALF-REC
              IF FS-CDBALF NOT = "00"
                 DISPLAY "CDBALF 書込失敗 ST="
                         FS-CDBALF
                 SET ABEND-DETECTED TO TRUE
              END-IF
           END-PERFORM.

       9000-CLOSE-FILES.
           CLOSE CDAUTHF
           IF FS-CDAUTHF NOT = "00"
              DISPLAY "CDAUTHF クローズ失敗 ST="
                      FS-CDAUTHF
              SET ABEND-DETECTED TO TRUE
           END-IF
           CLOSE CDSALESF
           IF FS-CDSALESF NOT = "00"
              DISPLAY "CDSALESF クローズ失敗 ST="
                      FS-CDSALESF
              SET ABEND-DETECTED TO TRUE
           END-IF
           CLOSE CDCARDF
           IF FS-CDCARDF NOT = "00"
              DISPLAY "CDCARDF クローズ失敗 ST="
                      FS-CDCARDF
              SET ABEND-DETECTED TO TRUE
           END-IF
           CLOSE CDBALF
           IF FS-CDBALF NOT = "00"
              DISPLAY "CDBALF クローズ失敗 ST="
                      FS-CDBALF
              SET ABEND-DETECTED TO TRUE
           END-IF.
