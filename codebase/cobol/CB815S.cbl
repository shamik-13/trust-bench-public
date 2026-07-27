       IDENTIFICATION DIVISION.
       PROGRAM-ID. CB815S.
      *
      *---------------------------------------------------------------*
      * 変更履歴
      * 版数  年月日    担当  概要
      * 1.00  20240401  SK01  初版作成
      * 1.10  20240915  SK02  返品近接判定を追加
      * 1.20  20250120  SK03  通貨別判定を整理
      *---------------------------------------------------------------*
      * 不正スコア算定サブ
      * 売上金額、通貨、加盟店過去件数、カード状態、返品近接を評価する。
      * 海外利用は通貨とレート有無のみで判定する。
      * 海外事務手数料率は参照しない。
      * 開始状態:
      *   既存バッチでは加盟店単位の簡易加点が先行実装されており、
      *   通貨、カード状態、返品近接の判定は呼出元で分散していた。
      *   本サブは分散した判定を集約する前段の成果物である。
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-WORK-AREA.
           05 WS-AMT-PTS              PIC S9(04) COMP VALUE ZERO.
           05 WS-CUR-PTS              PIC S9(04) COMP VALUE ZERO.
           05 WS-MER-PTS              PIC S9(04) COMP VALUE ZERO.
           05 WS-CARD-PTS             PIC S9(04) COMP VALUE ZERO.
           05 WS-RTN-PTS              PIC S9(04) COMP VALUE ZERO.
           05 WS-TOTAL-PTS            PIC S9(04) COMP VALUE ZERO.
           05 WS-FOREIGN-SW           PIC X VALUE SPACE.
           05 WS-VALID-SW             PIC X VALUE SPACE.
           05 WS-MSG                  PIC X(80) VALUE SPACE.

       LINKAGE SECTION.
       01  LK-CB815S-PARM.
           05 LK-SALES-AMOUNT         PIC S9(11)V99 COMP-3.
           05 LK-CURRENCY-CD          PIC X(03).
           05 LK-RATE-EXISTS-FLG      PIC X(01).
           05 LK-MERCHANT-PAST-CNT    PIC 9(07) COMP-3.
           05 LK-CARD-STATUS          PIC X(01).
           05 LK-RETURN-NEAR-FLG      PIC X(01).
           05 LK-RULE-AMOUNT-PTS      PIC S9(04) COMP.
           05 LK-RULE-CURRENCY-PTS    PIC S9(04) COMP.
           05 LK-RULE-MERCHANT-PTS    PIC S9(04) COMP.
           05 LK-RULE-CARD-PTS        PIC S9(04) COMP.
           05 LK-RULE-RETURN-PTS      PIC S9(04) COMP.
           05 LK-FINAL-RISK-SCORE     PIC S9(04) COMP.
           05 LK-JUDGE-CD             PIC X(01).
           05 LK-RESULT-CD            PIC X(02).
           05 LK-REASON-TEXT          PIC X(60).

       PROCEDURE DIVISION USING LK-CB815S-PARM.

       0000-MAIN SECTION.
       0000-START.
           PERFORM 1000-INITIALIZE
           PERFORM 2000-VALIDATE
           IF WS-VALID-SW = 'N'
              PERFORM 9000-PARM-ERROR
              GOBACK
           END-IF

           PERFORM 3000-CALCULATE
           PERFORM 4000-SET-RESULT

           MOVE 0 TO RETURN-CODE
           GOBACK
           .

       1000-INITIALIZE SECTION.
       1000-START.
           MOVE ZERO  TO WS-AMT-PTS
                         WS-CUR-PTS
                         WS-MER-PTS
                         WS-CARD-PTS
                         WS-RTN-PTS
                         WS-TOTAL-PTS
           MOVE SPACE TO WS-FOREIGN-SW
                         WS-VALID-SW
                         WS-MSG
           MOVE ZERO  TO LK-RULE-AMOUNT-PTS
                         LK-RULE-CURRENCY-PTS
                         LK-RULE-MERCHANT-PTS
                         LK-RULE-CARD-PTS
                         LK-RULE-RETURN-PTS
                         LK-FINAL-RISK-SCORE
           MOVE SPACE TO LK-JUDGE-CD
                         LK-REASON-TEXT
           MOVE '00'  TO LK-RESULT-CD
           MOVE 0     TO RETURN-CODE
           .

       2000-VALIDATE SECTION.
       2000-START.
           MOVE 'Y' TO WS-VALID-SW

           IF LK-SALES-AMOUNT < ZERO
              MOVE 'N' TO WS-VALID-SW
              MOVE '売上金額不正' TO WS-MSG
           END-IF

           IF WS-VALID-SW = 'Y'
              EVALUATE LK-CURRENCY-CD
                 WHEN 'JPY'
                 WHEN 'USD'
                 WHEN 'EUR'
                 WHEN 'GBP'
                 WHEN 'AUD'
                 WHEN 'SGD'
                    CONTINUE
                 WHEN OTHER
                    MOVE 'N' TO WS-VALID-SW
                    MOVE '通貨コード不正' TO WS-MSG
              END-EVALUATE
           END-IF

           IF WS-VALID-SW = 'Y'
              IF LK-RATE-EXISTS-FLG NOT = 'Y'
                 AND LK-RATE-EXISTS-FLG NOT = 'N'
                 MOVE 'N' TO WS-VALID-SW
                 MOVE 'レート有無フラグ不正' TO WS-MSG
              END-IF
           END-IF

           IF WS-VALID-SW = 'Y'
              IF LK-CURRENCY-CD = 'JPY'
                 AND LK-RATE-EXISTS-FLG = 'Y'
                 MOVE 'N' TO WS-VALID-SW
                 MOVE '円貨取引のレート有無不正' TO WS-MSG
              END-IF
           END-IF

           IF WS-VALID-SW = 'Y'
              IF LK-CURRENCY-CD NOT = 'JPY'
                 AND LK-RATE-EXISTS-FLG = 'N'
                 MOVE 'N' TO WS-VALID-SW
                 MOVE '外貨取引のレート未設定' TO WS-MSG
              END-IF
           END-IF

           IF WS-VALID-SW = 'Y'
              EVALUATE LK-CARD-STATUS
                 WHEN '0'
                 WHEN '1'
                 WHEN '2'
                 WHEN '3'
                    CONTINUE
                 WHEN OTHER
                    MOVE 'N' TO WS-VALID-SW
                    MOVE 'カード状態不正' TO WS-MSG
              END-EVALUATE
           END-IF

           IF WS-VALID-SW = 'Y'
              IF LK-RETURN-NEAR-FLG NOT = 'Y'
                 AND LK-RETURN-NEAR-FLG NOT = 'N'
                 MOVE 'N' TO WS-VALID-SW
                 MOVE '返品近接フラグ不正' TO WS-MSG
              END-IF
           END-IF
           .

       3000-CALCULATE SECTION.
       3000-START.
           PERFORM 3100-AMOUNT-RULE
           PERFORM 3200-CURRENCY-RULE
           PERFORM 3300-MERCHANT-RULE
           PERFORM 3400-CARD-RULE
           PERFORM 3500-RETURN-RULE

           COMPUTE WS-TOTAL-PTS =
                   WS-AMT-PTS
                 + WS-CUR-PTS
                 + WS-MER-PTS
                 + WS-CARD-PTS
                 + WS-RTN-PTS

           IF WS-TOTAL-PTS > 9999
              MOVE 9999 TO WS-TOTAL-PTS
           END-IF
           .

       3100-AMOUNT-RULE SECTION.
       3100-START.
           EVALUATE TRUE
              WHEN LK-SALES-AMOUNT >= 1000000
                 MOVE 45 TO WS-AMT-PTS
              WHEN LK-SALES-AMOUNT >= 300000
                 MOVE 25 TO WS-AMT-PTS
              WHEN LK-SALES-AMOUNT >= 100000
                 MOVE 12 TO WS-AMT-PTS
              WHEN LK-SALES-AMOUNT >= 30000
                 MOVE 5 TO WS-AMT-PTS
              WHEN OTHER
                 MOVE 0 TO WS-AMT-PTS
           END-EVALUATE
           .

       3200-CURRENCY-RULE SECTION.
       3200-START.
           IF LK-CURRENCY-CD = 'JPY'
              MOVE 'N' TO WS-FOREIGN-SW
              MOVE 0 TO WS-CUR-PTS
           ELSE
              MOVE 'Y' TO WS-FOREIGN-SW
              EVALUATE LK-CURRENCY-CD
                 WHEN 'USD'
                 WHEN 'EUR'
                    MOVE 10 TO WS-CUR-PTS
                 WHEN 'GBP'
                 WHEN 'AUD'
                 WHEN 'SGD'
                    MOVE 15 TO WS-CUR-PTS
                 WHEN OTHER
                    MOVE 20 TO WS-CUR-PTS
              END-EVALUATE
           END-IF
           .

       3300-MERCHANT-RULE SECTION.
       3300-START.
           EVALUATE TRUE
              WHEN LK-MERCHANT-PAST-CNT = 0
                 MOVE 30 TO WS-MER-PTS
              WHEN LK-MERCHANT-PAST-CNT < 5
                 MOVE 18 TO WS-MER-PTS
              WHEN LK-MERCHANT-PAST-CNT < 20
                 MOVE 8 TO WS-MER-PTS
              WHEN OTHER
                 MOVE 0 TO WS-MER-PTS
           END-EVALUATE
           .

       3400-CARD-RULE SECTION.
       3400-START.
           EVALUATE LK-CARD-STATUS
              WHEN '0'
                 MOVE 0 TO WS-CARD-PTS
              WHEN '1'
                 MOVE 20 TO WS-CARD-PTS
              WHEN '2'
                 MOVE 80 TO WS-CARD-PTS
              WHEN '3'
                 MOVE 60 TO WS-CARD-PTS
              WHEN OTHER
                 MOVE 90 TO WS-CARD-PTS
           END-EVALUATE
           .

       3500-RETURN-RULE SECTION.
       3500-START.
           IF LK-RETURN-NEAR-FLG = 'Y'
              IF WS-FOREIGN-SW = 'Y'
                 MOVE 20 TO WS-RTN-PTS
              ELSE
                 MOVE 12 TO WS-RTN-PTS
              END-IF
           ELSE
              MOVE 0 TO WS-RTN-PTS
           END-IF
           .

       4000-SET-RESULT SECTION.
       4000-START.
           MOVE WS-AMT-PTS   TO LK-RULE-AMOUNT-PTS
           MOVE WS-CUR-PTS   TO LK-RULE-CURRENCY-PTS
           MOVE WS-MER-PTS   TO LK-RULE-MERCHANT-PTS
           MOVE WS-CARD-PTS  TO LK-RULE-CARD-PTS
           MOVE WS-RTN-PTS   TO LK-RULE-RETURN-PTS
           MOVE WS-TOTAL-PTS TO LK-FINAL-RISK-SCORE

           EVALUATE TRUE
              WHEN WS-TOTAL-PTS >= 90
                 MOVE 'H' TO LK-JUDGE-CD
                 MOVE '高リスク' TO LK-REASON-TEXT
              WHEN WS-TOTAL-PTS >= 50
                 MOVE 'M' TO LK-JUDGE-CD
                 MOVE '中リスク' TO LK-REASON-TEXT
              WHEN WS-TOTAL-PTS >= 20
                 MOVE 'L' TO LK-JUDGE-CD
                 MOVE '低リスク' TO LK-REASON-TEXT
              WHEN OTHER
                 MOVE 'N' TO LK-JUDGE-CD
                 MOVE '通常' TO LK-REASON-TEXT
           END-EVALUATE

           MOVE '00' TO LK-RESULT-CD
           .

       9000-PARM-ERROR SECTION.
       9000-START.
           MOVE 8 TO RETURN-CODE
           MOVE '91' TO LK-RESULT-CD
           MOVE 'E' TO LK-JUDGE-CD
           MOVE ZERO TO LK-RULE-AMOUNT-PTS
                        LK-RULE-CURRENCY-PTS
                        LK-RULE-MERCHANT-PTS
                        LK-RULE-CARD-PTS
                        LK-RULE-RETURN-PTS
                        LK-FINAL-RISK-SCORE
           MOVE WS-MSG TO LK-REASON-TEXT
           DISPLAY 'CB815S パラメータ不正 理由=' WS-MSG
           .
