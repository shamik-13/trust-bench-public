       IDENTIFICATION DIVISION.
       PROGRAM-ID. CB310B.
      *
      * 変更履歴
      * 版数  年月日    担当    概要
      * 1.00  20240305  ＣＤ運用 初版作成
      * 1.01  20240701  ＣＤ運用 重複手数料の除外判定を追加
      * 1.02  20241010  ＣＤ運用 残高新規利用額への加算処理を整理
      *
      * 手数料投稿ゲート。
      * 年会費および売上関連手数料を請求サイクル単位で受け付ける。
      * 同一ＦＥＥ－ＩＤまたは同一カード同一手数料日の重複を除外し、
      * 投稿承認分を計上済みにして残高ファイルの新規利用額へ加算する。
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CDFEEF
               ASSIGN TO "CDFEEF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-CDFEEF-ST.
           SELECT CDBALF
               ASSIGN TO "CDBALF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-CDBALF-ST.

       DATA DIVISION.
       FILE SECTION.
       FD  CDFEEF.
       COPY CDFEEC.
       FD  CDBALF.
       COPY CDBALFC.

       WORKING-STORAGE SECTION.
       01  WS-CDFEEF-ST              PIC X(02) VALUE SPACES.
       01  WS-CDBALF-ST              PIC X(02) VALUE SPACES.
       01  WS-CDFEEF-EOF             PIC X VALUE "N".
           88 CDFEEF-EOF             VALUE "Y".
       01  WS-CDBALF-EOF             PIC X VALUE "N".
           88 CDBALF-EOF             VALUE "Y".
       01  WS-HARD-ERR               PIC X VALUE "N".
           88 HARD-ERR               VALUE "Y".
       01  WS-FOUND                  PIC X VALUE "N".
           88 FOUND                  VALUE "Y".
       01  WS-DUPLICATE              PIC X VALUE "N".
           88 DUPLICATE-FEE          VALUE "Y".
       01  WS-VALID-FEE              PIC X VALUE "N".
           88 VALID-FEE              VALUE "Y".

       01  WS-IDX                    PIC 9(5) COMP VALUE 0.
       01  WS-JDX                    PIC 9(5) COMP VALUE 0.
       01  WS-BAL-CNT                PIC 9(5) COMP VALUE 0.
       01  WS-FEE-CNT                PIC 9(5) COMP VALUE 0.
       01  WS-POST-CNT               PIC 9(5) COMP VALUE 0.
       01  WS-DUP-CNT                PIC 9(5) COMP VALUE 0.
       01  WS-ERR-CNT                PIC 9(5) COMP VALUE 0.

       01  WS-MAX-BAL                PIC 9(5) COMP VALUE 20000.
       01  WS-MAX-POST               PIC 9(5) COMP VALUE 30000.

       01  WS-BAL-TABLE.
           05 WS-BAL-ENT OCCURS 20000 TIMES.
              10 WS-TBL-CARD-NO      PIC X(19).
              10 WS-TBL-CYCLE-DT     PIC X(08).
              10 WS-TBL-CLOSING      PIC S9(13)V99 COMP-3.
              10 WS-TBL-REVOLVING    PIC S9(13)V99 COMP-3.
              10 WS-TBL-NEW-CHARGE   PIC S9(13)V99 COMP-3.
              10 WS-TBL-CASH-ADV     PIC S9(13)V99 COMP-3.
              10 WS-TBL-UPDATED      PIC X.

       01  WS-POST-TABLE.
           05 WS-POST-ENT OCCURS 30000 TIMES.
              10 WS-PST-FEE-ID       PIC X(20).
              10 WS-PST-CARD-NO      PIC X(19).
              10 WS-PST-FEE-DT       PIC X(08).

       01  WS-DATE-EDIT.
           05 WS-DATE-YYYY           PIC 9(04).
           05 WS-DATE-MM             PIC 9(02).
           05 WS-DATE-DD             PIC 9(02).

       01  WS-MSG.
           05 FILLER                 PIC X(21) VALUE
              "CB310B 件数 読込=".
           05 WS-MSG-FEE-CNT         PIC Z(5)9.
           05 FILLER                 PIC X(09) VALUE " 計上=".
           05 WS-MSG-POST-CNT        PIC Z(5)9.
           05 FILLER                 PIC X(09) VALUE " 重複=".
           05 WS-MSG-DUP-CNT         PIC Z(5)9.
           05 FILLER                 PIC X(09) VALUE " 不正=".
           05 WS-MSG-ERR-CNT         PIC Z(5)9.

       PROCEDURE DIVISION.
       MAIN-RTN.
           MOVE 0 TO RETURN-CODE
           PERFORM INIT-RTN
           IF NOT HARD-ERR
              PERFORM LOAD-BALANCE-RTN
           END-IF
           IF NOT HARD-ERR
              PERFORM PROCESS-FEE-RTN
           END-IF
           IF NOT HARD-ERR
              PERFORM WRITE-BALANCE-RTN
           END-IF
           PERFORM END-RTN
           GOBACK.

       INIT-RTN.
           DISPLAY "CB310B 手数料投稿ゲート開始"
           MOVE "N" TO WS-HARD-ERR
           MOVE "N" TO WS-CDFEEF-EOF
           MOVE "N" TO WS-CDBALF-EOF
           MOVE 0 TO WS-BAL-CNT
                     WS-FEE-CNT
                     WS-POST-CNT
                     WS-DUP-CNT
                     WS-ERR-CNT.

       LOAD-BALANCE-RTN.
           OPEN INPUT CDBALF
           IF WS-CDBALF-ST NOT = "00"
              DISPLAY "CDBALF OPEN ERR ST=" WS-CDBALF-ST
              MOVE "Y" TO WS-HARD-ERR
              MOVE 8 TO RETURN-CODE
           ELSE
              PERFORM UNTIL CDBALF-EOF OR HARD-ERR
                 READ CDBALF
                    AT END
                       MOVE "Y" TO WS-CDBALF-EOF
                    NOT AT END
                       PERFORM SAVE-BALANCE-RTN
                 END-READ
              END-PERFORM
              CLOSE CDBALF
              IF WS-CDBALF-ST NOT = "00"
                 DISPLAY "CDBALF CLOSE ERR ST=" WS-CDBALF-ST
                 MOVE "Y" TO WS-HARD-ERR
                 MOVE 8 TO RETURN-CODE
              END-IF
           END-IF.

       SAVE-BALANCE-RTN.
           IF WS-BAL-CNT >= WS-MAX-BAL
              DISPLAY "CDBALF 件数上限超過"
              MOVE "Y" TO WS-HARD-ERR
              MOVE 12 TO RETURN-CODE
           ELSE
              ADD 1 TO WS-BAL-CNT
              MOVE BL-CARD-NO TO WS-TBL-CARD-NO(WS-BAL-CNT)
              MOVE BL-CYCLE-DT TO WS-TBL-CYCLE-DT(WS-BAL-CNT)
              MOVE BL-CLOSING-BAL-AMT
                   TO WS-TBL-CLOSING(WS-BAL-CNT)
              MOVE BL-REVOLVING-BAL-AMT
                   TO WS-TBL-REVOLVING(WS-BAL-CNT)
              MOVE BL-NEW-CHARGE-AMT
                   TO WS-TBL-NEW-CHARGE(WS-BAL-CNT)
              MOVE BL-CASH-ADV-AMT
                   TO WS-TBL-CASH-ADV(WS-BAL-CNT)
              MOVE "N" TO WS-TBL-UPDATED(WS-BAL-CNT)
           END-IF.

       PROCESS-FEE-RTN.
           MOVE "N" TO WS-CDFEEF-EOF
           OPEN I-O CDFEEF
           IF WS-CDFEEF-ST NOT = "00"
              DISPLAY "CDFEEF OPEN ERR ST=" WS-CDFEEF-ST
              MOVE "Y" TO WS-HARD-ERR
              MOVE 8 TO RETURN-CODE
           ELSE
              PERFORM UNTIL CDFEEF-EOF OR HARD-ERR
                 READ CDFEEF
                    AT END
                       MOVE "Y" TO WS-CDFEEF-EOF
                    NOT AT END
                       ADD 1 TO WS-FEE-CNT
                       PERFORM HANDLE-FEE-RTN
                 END-READ
              END-PERFORM
              CLOSE CDFEEF
              IF WS-CDFEEF-ST NOT = "00"
                 DISPLAY "CDFEEF CLOSE ERR ST=" WS-CDFEEF-ST
                 MOVE "Y" TO WS-HARD-ERR
                 MOVE 8 TO RETURN-CODE
              END-IF
           END-IF.

       HANDLE-FEE-RTN.
           PERFORM VALIDATE-FEE-RTN
           IF VALID-FEE
              PERFORM CHECK-DUPLICATE-RTN
              IF DUPLICATE-FEE
                 MOVE "D" TO FE-POST-STATUS
                 REWRITE CDFEEF-REC
                 IF WS-CDFEEF-ST NOT = "00"
                    DISPLAY "CDFEEF DUP ERR ST=" WS-CDFEEF-ST
                    MOVE "Y" TO WS-HARD-ERR
                    MOVE 8 TO RETURN-CODE
                 ELSE
                    ADD 1 TO WS-DUP-CNT
                 END-IF
              ELSE
                 PERFORM APPLY-FEE-RTN
              END-IF
           ELSE
              MOVE "E" TO FE-POST-STATUS
              REWRITE CDFEEF-REC
              IF WS-CDFEEF-ST NOT = "00"
                 DISPLAY "CDFEEF BAD ERR ST=" WS-CDFEEF-ST
                 MOVE "Y" TO WS-HARD-ERR
                 MOVE 8 TO RETURN-CODE
              ELSE
                 ADD 1 TO WS-ERR-CNT
              END-IF
           END-IF.

       VALIDATE-FEE-RTN.
           MOVE "Y" TO WS-VALID-FEE
           IF FE-POST-STATUS NOT = "0" AND FE-POST-STATUS NOT = SPACE
              MOVE "N" TO WS-VALID-FEE
           END-IF
           IF FE-FEE-ID = SPACES
              DISPLAY "手数料ＩＤ不正"
              MOVE "N" TO WS-VALID-FEE
           END-IF
           IF FE-CARD-NO = SPACES
              DISPLAY "カード番号不正"
              MOVE "N" TO WS-VALID-FEE
           END-IF
           IF FE-FEE-AMT <= ZERO
              DISPLAY "手数料金額不正"
              MOVE "N" TO WS-VALID-FEE
           END-IF
           IF FE-FEE-TYPE NOT = "AF" AND
              FE-FEE-TYPE NOT = "SF" AND
              FE-FEE-TYPE NOT = "LF"
              DISPLAY "手数料種別不正"
              MOVE "N" TO WS-VALID-FEE
           END-IF
           IF FE-BILL-CYCLE-CD = SPACES
              DISPLAY "請求サイクル不正"
              MOVE "N" TO WS-VALID-FEE
           END-IF
           IF FE-FEE-DT NOT NUMERIC
              DISPLAY "手数料日不正"
              MOVE "N" TO WS-VALID-FEE
           ELSE
              MOVE FE-FEE-DT TO WS-DATE-EDIT
              IF WS-DATE-MM < 1 OR WS-DATE-MM > 12
                 DISPLAY "手数料月不正"
                 MOVE "N" TO WS-VALID-FEE
              END-IF
              IF WS-DATE-DD < 1 OR WS-DATE-DD > 31
                 DISPLAY "手数料日付不正"
                 MOVE "N" TO WS-VALID-FEE
              END-IF
           END-IF.

       CHECK-DUPLICATE-RTN.
           MOVE "N" TO WS-DUPLICATE
           MOVE 1 TO WS-JDX
           PERFORM UNTIL WS-JDX > WS-POST-CNT OR DUPLICATE-FEE
              IF WS-PST-FEE-ID(WS-JDX) = FE-FEE-ID
                 MOVE "Y" TO WS-DUPLICATE
              ELSE
                 IF WS-PST-CARD-NO(WS-JDX) = FE-CARD-NO AND
                    WS-PST-FEE-DT(WS-JDX) = FE-FEE-DT
                    MOVE "Y" TO WS-DUPLICATE
                 END-IF
              END-IF
              ADD 1 TO WS-JDX
           END-PERFORM.

       APPLY-FEE-RTN.
           MOVE "N" TO WS-FOUND
           MOVE 1 TO WS-IDX
           PERFORM UNTIL WS-IDX > WS-BAL-CNT OR FOUND
              IF WS-TBL-CARD-NO(WS-IDX) = FE-CARD-NO
                 MOVE "Y" TO WS-FOUND
              ELSE
                 ADD 1 TO WS-IDX
              END-IF
           END-PERFORM
           IF FOUND
              ADD FE-FEE-AMT TO WS-TBL-NEW-CHARGE(WS-IDX)
              MOVE "Y" TO WS-TBL-UPDATED(WS-IDX)
              MOVE "1" TO FE-POST-STATUS
              REWRITE CDFEEF-REC
              IF WS-CDFEEF-ST NOT = "00"
                 DISPLAY "CDFEEF POST ERR ST=" WS-CDFEEF-ST
                 MOVE "Y" TO WS-HARD-ERR
                 MOVE 8 TO RETURN-CODE
              ELSE
                 PERFORM SAVE-POSTED-KEY-RTN
              END-IF
           ELSE
              DISPLAY "残高対象カードなし CARD=" FE-CARD-NO
              MOVE "E" TO FE-POST-STATUS
              REWRITE CDFEEF-REC
              IF WS-CDFEEF-ST NOT = "00"
                 DISPLAY "CDFEEF NO BAL ERR ST=" WS-CDFEEF-ST
                 MOVE "Y" TO WS-HARD-ERR
                 MOVE 8 TO RETURN-CODE
              ELSE
                 ADD 1 TO WS-ERR-CNT
              END-IF
           END-IF.

       SAVE-POSTED-KEY-RTN.
           IF WS-POST-CNT >= WS-MAX-POST
              DISPLAY "計上済キー件数上限超過"
              MOVE "Y" TO WS-HARD-ERR
              MOVE 12 TO RETURN-CODE
           ELSE
              ADD 1 TO WS-POST-CNT
              MOVE FE-FEE-ID TO WS-PST-FEE-ID(WS-POST-CNT)
              MOVE FE-CARD-NO TO WS-PST-CARD-NO(WS-POST-CNT)
              MOVE FE-FEE-DT TO WS-PST-FEE-DT(WS-POST-CNT)
           END-IF.

       WRITE-BALANCE-RTN.
           OPEN OUTPUT CDBALF
           IF WS-CDBALF-ST NOT = "00"
              DISPLAY "CDBALF OUT OPEN ERR ST=" WS-CDBALF-ST
              MOVE "Y" TO WS-HARD-ERR
              MOVE 8 TO RETURN-CODE
           ELSE
              MOVE 1 TO WS-IDX
              PERFORM UNTIL WS-IDX > WS-BAL-CNT OR HARD-ERR
                 MOVE WS-TBL-CARD-NO(WS-IDX) TO BL-CARD-NO
                 MOVE WS-TBL-CYCLE-DT(WS-IDX) TO BL-CYCLE-DT
                 MOVE WS-TBL-CLOSING(WS-IDX)
                      TO BL-CLOSING-BAL-AMT
                 MOVE WS-TBL-REVOLVING(WS-IDX)
                      TO BL-REVOLVING-BAL-AMT
                 MOVE WS-TBL-NEW-CHARGE(WS-IDX)
                      TO BL-NEW-CHARGE-AMT
                 MOVE WS-TBL-CASH-ADV(WS-IDX)
                      TO BL-CASH-ADV-AMT
                 WRITE CDBALF-REC
                 IF WS-CDBALF-ST NOT = "00"
                    DISPLAY "CDBALF WRITE ERR ST=" WS-CDBALF-ST
                    MOVE "Y" TO WS-HARD-ERR
                    MOVE 8 TO RETURN-CODE
                 END-IF
                 ADD 1 TO WS-IDX
              END-PERFORM
              CLOSE CDBALF
              IF WS-CDBALF-ST NOT = "00"
                 DISPLAY "CDBALF OUT CLOSE ERR ST=" WS-CDBALF-ST
                 MOVE "Y" TO WS-HARD-ERR
                 MOVE 8 TO RETURN-CODE
              END-IF
           END-IF.

       END-RTN.
           MOVE WS-FEE-CNT TO WS-MSG-FEE-CNT
           MOVE WS-POST-CNT TO WS-MSG-POST-CNT
           MOVE WS-DUP-CNT TO WS-MSG-DUP-CNT
           MOVE WS-ERR-CNT TO WS-MSG-ERR-CNT
           DISPLAY WS-MSG
           IF HARD-ERR
              IF RETURN-CODE = 0
                 MOVE 8 TO RETURN-CODE
              END-IF
              DISPLAY "CB310B 異常終了 RC=" RETURN-CODE
           ELSE
              MOVE 0 TO RETURN-CODE
              DISPLAY "CB310B 正常終了"
           END-IF.
