       IDENTIFICATION DIVISION.
       PROGRAM-ID. CB420S.
      *==============================================================*
      * 変更履歴                                                     *
      * 版数  年月日    担当  概要                                  *
      * 1.00  20240401  共通  初版作成                              *
      * 1.01  20241015  共通  端数寄せ判定を追加                    *
      * 1.02  20250320  共通  入力妥当性検査を強化                  *
      *==============================================================*
      * 入金按分サブルーチン                                        *
      * 請求額、元金額、手数料額、入金額を受け取り、手数料、元金、  *
      * 未収残の順に消込額を算出する。過入金は未充当額へ返す。      *
      * 開始時点の実装想定は、ホスト移行前の既存バッチから呼ばれる  *
      * 単純な金額按分部品であり、呼出元が同一領域を再利用するため  *
      * 出力領域は本処理で必ず初期化する。                          *
      *==============================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WK-WORK-AREA.
           05 WK-SEIKYU-AMT        PIC S9(13)V99 COMP-3 VALUE ZERO.
           05 WK-GANKIN-AMT        PIC S9(13)V99 COMP-3 VALUE ZERO.
           05 WK-TESURYO-AMT       PIC S9(13)V99 COMP-3 VALUE ZERO.
           05 WK-NYUKIN-AMT        PIC S9(13)V99 COMP-3 VALUE ZERO.
           05 WK-FEE-TARGET        PIC S9(13)V99 COMP-3 VALUE ZERO.
           05 WK-PRN-TARGET        PIC S9(13)V99 COMP-3 VALUE ZERO.
           05 WK-REMAIN-PAY        PIC S9(13)V99 COMP-3 VALUE ZERO.
           05 WK-TOTAL-APPLIED     PIC S9(13)V99 COMP-3 VALUE ZERO.
           05 WK-CLAIM-BASE        PIC S9(13)V99 COMP-3 VALUE ZERO.

       01  WK-CTL-AREA.
           05 WK-ERR-SW            PIC X VALUE SPACE.
              88 WK-NORMAL               VALUE SPACE.
              88 WK-ERROR                VALUE 'E'.

       LINKAGE SECTION.
       01  LK-CB420S-PARM.
           05 LK-CB420S-IN.
              10 LK-CB420S-SEIKYU-GAKU   PIC S9(13)V99 COMP-3.
              10 LK-CB420S-GANKIN-GAKU   PIC S9(13)V99 COMP-3.
              10 LK-CB420S-TESURYO-GAKU  PIC S9(13)V99 COMP-3.
              10 LK-CB420S-NYUKIN-GAKU   PIC S9(13)V99 COMP-3.
           05 LK-CB420S-OUT.
              10 LK-CB420S-TESURYO-KESH  PIC S9(13)V99 COMP-3.
              10 LK-CB420S-GANKIN-KESH   PIC S9(13)V99 COMP-3.
              10 LK-CB420S-MISHU-ZAN     PIC S9(13)V99 COMP-3.
              10 LK-CB420S-MIJUTO-GAKU   PIC S9(13)V99 COMP-3.
              10 LK-CB420S-GANKIN-ZAN    PIC S9(13)V99 COMP-3.
              10 LK-CB420S-STATUS        PIC X(02).

       PROCEDURE DIVISION USING LK-CB420S-PARM.
       0000-MAIN SECTION.
           PERFORM 1000-INIT
           PERFORM 2000-CHECK
           IF WK-NORMAL
              PERFORM 3000-CALC
              MOVE 0 TO RETURN-CODE
           ELSE
              MOVE 8 TO RETURN-CODE
           END-IF
           GOBACK
           .

       1000-INIT SECTION.
           MOVE SPACE TO WK-ERR-SW
           MOVE ZERO  TO LK-CB420S-TESURYO-KESH
           MOVE ZERO  TO LK-CB420S-GANKIN-KESH
           MOVE ZERO  TO LK-CB420S-MISHU-ZAN
           MOVE ZERO  TO LK-CB420S-MIJUTO-GAKU
           MOVE ZERO  TO LK-CB420S-GANKIN-ZAN
           MOVE '00'  TO LK-CB420S-STATUS

           MOVE LK-CB420S-SEIKYU-GAKU  TO WK-SEIKYU-AMT
           MOVE LK-CB420S-GANKIN-GAKU  TO WK-GANKIN-AMT
           MOVE LK-CB420S-TESURYO-GAKU TO WK-TESURYO-AMT
           MOVE LK-CB420S-NYUKIN-GAKU  TO WK-NYUKIN-AMT
           .

       2000-CHECK SECTION.
           IF WK-SEIKYU-AMT < ZERO
              MOVE 'E' TO WK-ERR-SW
              MOVE '21' TO LK-CB420S-STATUS
              DISPLAY 'CB420S 請求額不正'
           END-IF

           IF WK-NORMAL AND WK-GANKIN-AMT < ZERO
              MOVE 'E' TO WK-ERR-SW
              MOVE '22' TO LK-CB420S-STATUS
              DISPLAY 'CB420S 元金額不正'
           END-IF

           IF WK-NORMAL AND WK-TESURYO-AMT < ZERO
              MOVE 'E' TO WK-ERR-SW
              MOVE '23' TO LK-CB420S-STATUS
              DISPLAY 'CB420S 手数料額不正'
           END-IF

           IF WK-NORMAL AND WK-NYUKIN-AMT < ZERO
              MOVE 'E' TO WK-ERR-SW
              MOVE '24' TO LK-CB420S-STATUS
              DISPLAY 'CB420S 入金額不正'
           END-IF
           .

       3000-CALC SECTION.
           MOVE WK-SEIKYU-AMT  TO WK-CLAIM-BASE
           MOVE WK-TESURYO-AMT TO WK-FEE-TARGET
           MOVE WK-GANKIN-AMT  TO WK-PRN-TARGET

           IF WK-FEE-TARGET > WK-CLAIM-BASE
              MOVE WK-CLAIM-BASE TO WK-FEE-TARGET
              MOVE ZERO TO WK-PRN-TARGET
              MOVE '10' TO LK-CB420S-STATUS
           ELSE
              COMPUTE WK-PRN-TARGET =
                      WK-CLAIM-BASE - WK-FEE-TARGET
              IF WK-PRN-TARGET > WK-GANKIN-AMT
                 MOVE WK-GANKIN-AMT TO WK-PRN-TARGET
              END-IF
           END-IF

           MOVE WK-NYUKIN-AMT TO WK-REMAIN-PAY

           IF WK-REMAIN-PAY > WK-FEE-TARGET
              MOVE WK-FEE-TARGET TO LK-CB420S-TESURYO-KESH
              SUBTRACT WK-FEE-TARGET FROM WK-REMAIN-PAY
           ELSE
              MOVE WK-REMAIN-PAY TO LK-CB420S-TESURYO-KESH
              MOVE ZERO TO WK-REMAIN-PAY
           END-IF

           IF WK-REMAIN-PAY > WK-PRN-TARGET
              MOVE WK-PRN-TARGET TO LK-CB420S-GANKIN-KESH
              SUBTRACT WK-PRN-TARGET FROM WK-REMAIN-PAY
           ELSE
              MOVE WK-REMAIN-PAY TO LK-CB420S-GANKIN-KESH
              MOVE ZERO TO WK-REMAIN-PAY
           END-IF

           COMPUTE WK-TOTAL-APPLIED =
                   LK-CB420S-TESURYO-KESH
                 + LK-CB420S-GANKIN-KESH

           COMPUTE LK-CB420S-MISHU-ZAN =
                   WK-CLAIM-BASE - WK-TOTAL-APPLIED

           IF LK-CB420S-MISHU-ZAN < ZERO
              ADD LK-CB420S-MISHU-ZAN
                  TO LK-CB420S-GANKIN-KESH
              MOVE ZERO TO LK-CB420S-MISHU-ZAN
              MOVE '11' TO LK-CB420S-STATUS
           END-IF

           COMPUTE LK-CB420S-GANKIN-ZAN =
                   WK-GANKIN-AMT - LK-CB420S-GANKIN-KESH

           IF LK-CB420S-GANKIN-ZAN < ZERO
              ADD LK-CB420S-GANKIN-ZAN
                  TO LK-CB420S-GANKIN-KESH
              MOVE ZERO TO LK-CB420S-GANKIN-ZAN
              COMPUTE LK-CB420S-MISHU-ZAN =
                      WK-CLAIM-BASE
                    - LK-CB420S-TESURYO-KESH
                    - LK-CB420S-GANKIN-KESH
              MOVE '12' TO LK-CB420S-STATUS
           END-IF

           IF WK-REMAIN-PAY > ZERO
              MOVE WK-REMAIN-PAY TO LK-CB420S-MIJUTO-GAKU
              IF LK-CB420S-STATUS = '00'
                 MOVE '01' TO LK-CB420S-STATUS
              END-IF
           END-IF
           .
