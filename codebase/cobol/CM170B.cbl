       IDENTIFICATION DIVISION.
       PROGRAM-ID. CM170B.
      ******************************************************************
      *  変更履歴
      *  版数  年月日    担当    概要
      *  1.00  20240315  CM開発  新規作成
      *  1.01  20240930  CM保守  要確認閾値を五十点へ改定
      *  1.02  20250120  CM保守  自動一致閾値を八十点へ改定
      ******************************************************************
      *  名寄せ候補詳細採点バッチ
      *  一次候補の属性一致点を再評価し判定区分を更新する。
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CMDUPF ASSIGN TO "CMDUPF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS DP-CANDIDATE-ID
               FILE STATUS IS WS-CMDUPF-ST.
           SELECT CMATTF ASSIGN TO "CMATTF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS CA-CIF-NO
               FILE STATUS IS WS-CMATTF-ST.
           SELECT CMCIFF ASSIGN TO "CMCIFF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-CMCIFF-ST.

       DATA DIVISION.
       FILE SECTION.
       FD  CMDUPF.
           COPY CMDUPC.
       FD  CMATTF.
           COPY CMATTC.
       FD  CMCIFF.
           COPY CMCIFFC.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05  WS-CMDUPF-ST        PIC XX.
           05  WS-CMATTF-ST        PIC XX.
           05  WS-CMCIFF-ST        PIC XX.

       01  WS-END-FLAGS.
           05  WS-CMDUPF-EOF       PIC X VALUE "N".
               88  CMDUPF-END            VALUE "Y".
           05  WS-CMCIFF-EOF       PIC X VALUE "N".
               88  CMCIFF-END            VALUE "Y".

       01  WS-SAVE-ATTR-1.
           05  WS-KANA-1           PIC X(60).
           05  WS-ADDR-1           PIC X(10).
           05  WS-PHONE-1          PIC X(15).
           05  WS-ATTR-STS-1       PIC X(02).
           05  WS-BIRTH-1          PIC 9(08).
           05  WS-CIF-STS-1        PIC X(02).

       01  WS-SAVE-ATTR-2.
           05  WS-KANA-2           PIC X(60).
           05  WS-ADDR-2           PIC X(10).
           05  WS-PHONE-2          PIC X(15).
           05  WS-ATTR-STS-2       PIC X(02).
           05  WS-BIRTH-2          PIC 9(08).
           05  WS-CIF-STS-2        PIC X(02).

       01  WS-WORK.
           05  WS-SEARCH-CIF       PIC X(10).
           05  WS-SCORE            PIC 9(03) VALUE ZERO.
           05  WS-FOUND-SW         PIC X VALUE "N".
               88  CIF-FOUND             VALUE "Y".
           05  WS-VALID-SW         PIC X VALUE "N".
               88  CIF-VALID             VALUE "Y".
           05  WS-READ-CNT         PIC 9(09) VALUE ZERO.
           05  WS-UPD-CNT          PIC 9(09) VALUE ZERO.
           05  WS-REJ-CNT          PIC 9(09) VALUE ZERO.

       01  WS-CONST.
           05  CN-AUTO-SCORE       PIC 9(03) VALUE 080.
           05  CN-CHECK-SCORE      PIC 9(03) VALUE 050.
           05  CN-CIF-VALID        PIC X(02) VALUE "01".
           05  CN-ATTR-VALID       PIC X(02) VALUE "01".
           05  CN-JDG-AUTO         PIC X VALUE "A".
           05  CN-JDG-CHECK        PIC X VALUE "C".
           05  CN-JDG-REJECT       PIC X VALUE "R".

       PROCEDURE DIVISION.
       MAIN-RTN.
      *    処理開始。異常終了時は必ず復帰コードを設定する。
           MOVE 0 TO RETURN-CODE
           PERFORM OPEN-RTN
           IF RETURN-CODE NOT = 0
              GOBACK
           END-IF

           PERFORM UNTIL CMDUPF-END
              READ CMDUPF NEXT RECORD
                 AT END
                    SET CMDUPF-END TO TRUE
                 NOT AT END
                    ADD 1 TO WS-READ-CNT
                    PERFORM SCORE-CANDIDATE-RTN
              END-READ
              IF WS-CMDUPF-ST NOT = "00"
                 AND WS-CMDUPF-ST NOT = "10"
                 DISPLAY "CMDUPF 読込失敗 ST=" WS-CMDUPF-ST
                 MOVE 8 TO RETURN-CODE
                 SET CMDUPF-END TO TRUE
              END-IF
           END-PERFORM

           PERFORM CLOSE-RTN
           IF RETURN-CODE = 0
              DISPLAY "CM170B 正常終了 読込=" WS-READ-CNT
                      " 更新=" WS-UPD-CNT " 棄却=" WS-REJ-CNT
           END-IF
           GOBACK.

       OPEN-RTN.
      *    入出力ファイルを開設する。
           OPEN I-O CMDUPF
           IF WS-CMDUPF-ST NOT = "00"
              DISPLAY "CMDUPF オープン失敗 ST=" WS-CMDUPF-ST
              MOVE 8 TO RETURN-CODE
              EXIT PARAGRAPH
           END-IF

           OPEN INPUT CMATTF
           IF WS-CMATTF-ST NOT = "00"
              DISPLAY "CMATTF オープン失敗 ST=" WS-CMATTF-ST
              MOVE 8 TO RETURN-CODE
              CLOSE CMDUPF
              EXIT PARAGRAPH
           END-IF.

       CLOSE-RTN.
      *    使用済ファイルを閉鎖する。
           CLOSE CMDUPF CMATTF
           IF WS-CMDUPF-ST NOT = "00"
              DISPLAY "CMDUPF クローズ失敗 ST=" WS-CMDUPF-ST
              MOVE 8 TO RETURN-CODE
           END-IF
           IF WS-CMATTF-ST NOT = "00"
              DISPLAY "CMATTF クローズ失敗 ST=" WS-CMATTF-ST
              MOVE 8 TO RETURN-CODE
           END-IF.

       SCORE-CANDIDATE-RTN.
      *    候補二件のCIF存在確認と属性取得を行う。
           PERFORM CLEAR-WORK-RTN

           MOVE DP-CIF-NO-1 TO CA-CIF-NO
           READ CMATTF KEY IS CA-CIF-NO
              INVALID KEY
                 DISPLAY "属性未登録 CIF=" DP-CIF-NO-1
                 PERFORM REJECT-CANDIDATE-RTN
                 EXIT PARAGRAPH
           END-READ
           IF WS-CMATTF-ST NOT = "00"
              DISPLAY "CMATTF 読込失敗 ST=" WS-CMATTF-ST
              MOVE 8 TO RETURN-CODE
              SET CMDUPF-END TO TRUE
              EXIT PARAGRAPH
           END-IF

           MOVE CA-KANA-NAME TO WS-KANA-1
           MOVE CA-ADDR-CD TO WS-ADDR-1
           MOVE CA-PHONE-NO TO WS-PHONE-1
           MOVE CA-ATTR-STATUS-KBN TO WS-ATTR-STS-1

           MOVE DP-CIF-NO-1 TO WS-SEARCH-CIF
           PERFORM FIND-CIF-RTN
           IF NOT CIF-VALID
              DISPLAY "CIF状態不正 CIF=" DP-CIF-NO-1
              PERFORM REJECT-CANDIDATE-RTN
              EXIT PARAGRAPH
           END-IF
           MOVE CF-BIRTH-DT TO WS-BIRTH-1
           MOVE CF-CIF-STATUS-KBN TO WS-CIF-STS-1

           MOVE DP-CIF-NO-2 TO CA-CIF-NO
           READ CMATTF KEY IS CA-CIF-NO
              INVALID KEY
                 DISPLAY "属性未登録 CIF=" DP-CIF-NO-2
                 PERFORM REJECT-CANDIDATE-RTN
                 EXIT PARAGRAPH
           END-READ
           IF WS-CMATTF-ST NOT = "00"
              DISPLAY "CMATTF 読込失敗 ST=" WS-CMATTF-ST
              MOVE 8 TO RETURN-CODE
              SET CMDUPF-END TO TRUE
              EXIT PARAGRAPH
           END-IF

           MOVE CA-KANA-NAME TO WS-KANA-2
           MOVE CA-ADDR-CD TO WS-ADDR-2
           MOVE CA-PHONE-NO TO WS-PHONE-2
           MOVE CA-ATTR-STATUS-KBN TO WS-ATTR-STS-2

           MOVE DP-CIF-NO-2 TO WS-SEARCH-CIF
           PERFORM FIND-CIF-RTN
           IF NOT CIF-VALID
              DISPLAY "CIF状態不正 CIF=" DP-CIF-NO-2
              PERFORM REJECT-CANDIDATE-RTN
              EXIT PARAGRAPH
           END-IF
           MOVE CF-BIRTH-DT TO WS-BIRTH-2
           MOVE CF-CIF-STATUS-KBN TO WS-CIF-STS-2

      *    属性状態が有効な候補だけ詳細採点する。
           IF WS-ATTR-STS-1 NOT = CN-ATTR-VALID
              OR WS-ATTR-STS-2 NOT = CN-ATTR-VALID
              DISPLAY "属性状態不正 候補=" DP-CANDIDATE-ID
              PERFORM REJECT-CANDIDATE-RTN
              EXIT PARAGRAPH
           END-IF

           PERFORM CALC-SCORE-RTN
           PERFORM UPDATE-JUDGE-RTN.

       CLEAR-WORK-RTN.
           MOVE SPACES TO WS-KANA-1 WS-ADDR-1 WS-PHONE-1
           MOVE SPACES TO WS-KANA-2 WS-ADDR-2 WS-PHONE-2
           MOVE SPACES TO WS-ATTR-STS-1 WS-ATTR-STS-2
           MOVE SPACES TO WS-CIF-STS-1 WS-CIF-STS-2
           MOVE ZERO TO WS-BIRTH-1 WS-BIRTH-2 WS-SCORE
           MOVE "N" TO WS-FOUND-SW WS-VALID-SW.

       FIND-CIF-RTN.
      *    CM190S相当の存在確認としてCIF台帳を参照する。
      *    検査数字や番号体系の内部計算は本処理では行わない。
           MOVE "N" TO WS-FOUND-SW WS-VALID-SW
           MOVE "N" TO WS-CMCIFF-EOF
           OPEN INPUT CMCIFF
           IF WS-CMCIFF-ST NOT = "00"
              DISPLAY "CMCIFF オープン失敗 ST=" WS-CMCIFF-ST
              MOVE 8 TO RETURN-CODE
              SET CMDUPF-END TO TRUE
              EXIT PARAGRAPH
           END-IF

           PERFORM UNTIL CMCIFF-END OR CIF-FOUND
              READ CMCIFF
                 AT END
                    SET CMCIFF-END TO TRUE
                 NOT AT END
                    IF CF-CIF-NO = WS-SEARCH-CIF
                       SET CIF-FOUND TO TRUE
                    END-IF
              END-READ
              IF WS-CMCIFF-ST NOT = "00"
                 AND WS-CMCIFF-ST NOT = "10"
                 DISPLAY "CMCIFF 読込失敗 ST=" WS-CMCIFF-ST
                 MOVE 8 TO RETURN-CODE
                 SET CMCIFF-END TO TRUE
              END-IF
           END-PERFORM

           IF CIF-FOUND
              IF CF-CIF-STATUS-KBN = CN-CIF-VALID
                 SET CIF-VALID TO TRUE
              END-IF
           ELSE
              DISPLAY "CIF未登録 CIF=" WS-SEARCH-CIF
           END-IF

           CLOSE CMCIFF
           IF WS-CMCIFF-ST NOT = "00"
              DISPLAY "CMCIFF クローズ失敗 ST=" WS-CMCIFF-ST
              MOVE 8 TO RETURN-CODE
              SET CMDUPF-END TO TRUE
           END-IF.

       CALC-SCORE-RTN.
      *    氏名カナ、住所、電話、生年月日を重み付きで採点する。
           MOVE ZERO TO WS-SCORE
           IF WS-KANA-1 = WS-KANA-2
              ADD 40 TO WS-SCORE
           END-IF
           IF WS-ADDR-1 = WS-ADDR-2
              ADD 25 TO WS-SCORE
           END-IF
           IF WS-PHONE-1 = WS-PHONE-2
              AND WS-PHONE-1 NOT = SPACES
              ADD 20 TO WS-SCORE
           END-IF
           IF WS-BIRTH-1 = WS-BIRTH-2
              AND WS-BIRTH-1 NOT = ZERO
              ADD 15 TO WS-SCORE
           END-IF.

       UPDATE-JUDGE-RTN.
      *    閾値により自動一致、要確認、棄却を判定する。
           MOVE WS-SCORE TO DP-MATCH-SCORE
           IF WS-SCORE >= CN-AUTO-SCORE
              MOVE CN-JDG-AUTO TO DP-JUDGE-KBN
           ELSE
              IF WS-SCORE >= CN-CHECK-SCORE
                 MOVE CN-JDG-CHECK TO DP-JUDGE-KBN
              ELSE
                 MOVE CN-JDG-REJECT TO DP-JUDGE-KBN
                 ADD 1 TO WS-REJ-CNT
              END-IF
           END-IF

           REWRITE CMDUPF-REC
              INVALID KEY
                 DISPLAY "CMDUPF 更新失敗 候補=" DP-CANDIDATE-ID
                 MOVE 8 TO RETURN-CODE
                 SET CMDUPF-END TO TRUE
              NOT INVALID KEY
                 ADD 1 TO WS-UPD-CNT
           END-REWRITE
           IF WS-CMDUPF-ST NOT = "00"
              DISPLAY "CMDUPF 更新失敗 ST=" WS-CMDUPF-ST
              MOVE 8 TO RETURN-CODE
              SET CMDUPF-END TO TRUE
           END-IF.

       REJECT-CANDIDATE-RTN.
      *    採点不能な候補は棄却へ更新する。
           MOVE ZERO TO DP-MATCH-SCORE
           MOVE CN-JDG-REJECT TO DP-JUDGE-KBN
           REWRITE CMDUPF-REC
              INVALID KEY
                 DISPLAY "CMDUPF 棄却更新失敗 候補="
                         DP-CANDIDATE-ID
                 MOVE 8 TO RETURN-CODE
                 SET CMDUPF-END TO TRUE
              NOT INVALID KEY
                 ADD 1 TO WS-UPD-CNT
                 ADD 1 TO WS-REJ-CNT
           END-REWRITE
           IF WS-CMDUPF-ST NOT = "00"
              DISPLAY "CMDUPF 棄却更新失敗 ST=" WS-CMDUPF-ST
              MOVE 8 TO RETURN-CODE
              SET CMDUPF-END TO TRUE
           END-IF.
