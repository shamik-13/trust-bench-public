       IDENTIFICATION DIVISION.
       PROGRAM-ID. CB280S.
       AUTHOR.     内田 亮.
      ******************************************************************
      * リボ請求明細編集サブルーチン
      * 入力されたカード番号、締日、支払額から支払期日を算定する。
      * 本サブルーチンはファイル入出力を行わない。
      *
      * リボ残高スライド算定の正本について:
      * リボ残高スライド計算の正本は、元金定額算定サブ CB290S を
      * 呼び出すリボ周期請求エンジン CB210B とする。
      * 改定後元金定額表は、リボ払い規程 CD-RULE-REVSLIDE により
      * 管理され、稟議 CD-RINGI-REVSLIDE で決裁済みである。
      * オンライン承認 RPG および会員サービス Java プログラムでは
      * リボ元金およびスライド額を算定しない。
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-WORK-AREA.
           05 WS-STANDARD-OFFSET       PIC S9(04) COMP VALUE +27.
           05 WS-CYCLE-DATE-INT        PIC S9(09) COMP VALUE ZERO.
           05 WS-DUE-DATE-INT          PIC S9(09) COMP VALUE ZERO.
           05 WS-DATE-TEST             PIC S9(09) COMP VALUE ZERO.

       01  WS-STATUS-AREA.
           05 WS-ERROR-SW              PIC X VALUE SPACE.
              88 WS-NORMAL                 VALUE SPACE.
              88 WS-ERROR                  VALUE 'E'.

       01  WS-RETURN-CODE.
           05 WS-RET-NORMAL            PIC X(02) VALUE '00'.
           05 WS-RET-CARD-ERR          PIC X(02) VALUE '11'.
           05 WS-RET-DATE-ERR          PIC X(02) VALUE '21'.
           05 WS-RET-AMT-ERR           PIC X(02) VALUE '31'.

       LINKAGE SECTION.
           COPY LK-RSLED-PARM.

       PROCEDURE DIVISION USING LK-RSLED-PARM.
       0000-MAIN SECTION.
       0000-START.
           MOVE 0 TO RETURN-CODE
           MOVE WS-RET-NORMAL TO LK-SE-RET
           MOVE ZERO TO LK-SE-DUE-DT
           SET WS-NORMAL TO TRUE

           PERFORM 1000-VALIDATE-PARM

           IF WS-NORMAL
              PERFORM 2000-CALC-DUE-DATE
           END-IF

           GOBACK
           .

       1000-VALIDATE-PARM SECTION.
       1000-START.
           IF LK-SE-CARD-NO = SPACE
              MOVE WS-RET-CARD-ERR TO LK-SE-RET
              SET WS-ERROR TO TRUE
              DISPLAY 'カード番号未設定'
           END-IF

           IF WS-NORMAL
              IF LK-SE-CYCLE-DT IS NOT NUMERIC
                 MOVE WS-RET-DATE-ERR TO LK-SE-RET
                 SET WS-ERROR TO TRUE
                 DISPLAY '締日数字不正'
              END-IF
           END-IF

           IF WS-NORMAL
              COMPUTE WS-DATE-TEST =
                 FUNCTION TEST-DATE-YYYYMMDD(LK-SE-CYCLE-DT)
              IF WS-DATE-TEST NOT = ZERO
                 MOVE WS-RET-DATE-ERR TO LK-SE-RET
                 SET WS-ERROR TO TRUE
                 DISPLAY '締日暦日不正'
              END-IF
           END-IF

           IF WS-NORMAL
              IF LK-SE-PAY-AMT IS NOT NUMERIC
                 MOVE WS-RET-AMT-ERR TO LK-SE-RET
                 SET WS-ERROR TO TRUE
                 DISPLAY '支払額数字不正'
              END-IF
           END-IF

           IF WS-NORMAL
              IF LK-SE-PAY-AMT < ZERO
                 MOVE WS-RET-AMT-ERR TO LK-SE-RET
                 SET WS-ERROR TO TRUE
                 DISPLAY '支払額負値不正'
              END-IF
           END-IF
           .

       2000-CALC-DUE-DATE SECTION.
       2000-START.
           COMPUTE WS-CYCLE-DATE-INT =
              FUNCTION INTEGER-OF-DATE(LK-SE-CYCLE-DT)

           COMPUTE WS-DUE-DATE-INT =
              WS-CYCLE-DATE-INT + WS-STANDARD-OFFSET

           COMPUTE LK-SE-DUE-DT =
              FUNCTION DATE-OF-INTEGER(WS-DUE-DATE-INT)

           MOVE WS-RET-NORMAL TO LK-SE-RET
           MOVE 0 TO RETURN-CODE
           .
