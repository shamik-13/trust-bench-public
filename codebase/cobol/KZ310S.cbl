       IDENTIFICATION DIVISION.
       PROGRAM-ID. KZ310S.
      *-----------------------------------------------------------------
      * 変更履歴
      * 版数  年月日       担当                         概要
      * 1.00  令和07.04.01 システム部 勘定系チーム     新規作成
      * 1.01  令和07.10.15 システム部 勘定系チーム     取消判定追加
      * 1.02  令和08.02.18 システム部 勘定系チーム     摘要編集見直し
      *-----------------------------------------------------------------
       AUTHOR.     システム部 勘定系チーム.
       DATE-WRITTEN. 2024-04-01.
      *
      * GL仕訳変換サブ
      * 取引種別、金額符号、発生元区分からKZGLDTF向け項目を編集する。
      * 取消明細では貸借反転の妥当性を確認し、異常区分を返す。
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
       01  WK-AREA.
           05  WK-VALID-SW              PIC X     VALUE SPACE.
               88  WK-VALID                     VALUE '1'.
               88  WK-INVALID                   VALUE '0'.
           05  WK-CANCEL-SW             PIC X     VALUE SPACE.
               88  WK-CANCEL                    VALUE '1'.
               88  WK-NORMAL                    VALUE '0'.
           05  WK-BASE-DRCR             PIC X     VALUE SPACE.
           05  WK-BASE-ENTRY            PIC X(04) VALUE SPACES.
           05  WK-BASE-TEKIYO           PIC X(03) VALUE SPACES.
           05  WK-REV-DRCR              PIC X     VALUE SPACE.
      *
       01  WK-CONSTANT.
           05  WK-ERR-NORMAL            PIC X(02) VALUE '00'.
           05  WK-ERR-TRN               PIC X(02) VALUE '11'.
           05  WK-ERR-SIGN              PIC X(02) VALUE '12'.
           05  WK-ERR-GEN               PIC X(02) VALUE '13'.
           05  WK-ERR-CANCEL            PIC X(02) VALUE '21'.
      *
       LINKAGE SECTION.
      *
       01  LK-KZ310S-PARM.
           05  LK-TRN-KBN               PIC X(02).
           05  LK-AMT-SIGN              PIC X.
           05  LK-GEN-KBN               PIC X.
           05  LK-CANCEL-KBN            PIC X.
           05  LK-ENTRY-CD              PIC X(04).
           05  LK-DR-CR-KBN             PIC X.
           05  LK-TEKIYO-HOJO-CD        PIC X(03).
           05  LK-ERR-KBN               PIC X(02).
      *
       PROCEDURE DIVISION USING LK-KZ310S-PARM.
      *
       0000-MAIN SECTION.
      *
           PERFORM 1000-INIT.
           PERFORM 2000-CHECK-INPUT.
           IF WK-VALID
              PERFORM 3000-MAKE-BASE
              IF WK-VALID
                 PERFORM 4000-EDIT-OUTPUT
              END-IF
           END-IF.
           GOBACK.
      *
       1000-INIT SECTION.
      *
      *    呼出元返却領域を毎回初期化する。
           SET WK-INVALID TO TRUE.
           SET WK-NORMAL TO TRUE.
           MOVE SPACE       TO WK-BASE-DRCR.
           MOVE SPACE       TO WK-REV-DRCR.
           MOVE SPACES      TO WK-BASE-ENTRY.
           MOVE SPACES      TO WK-BASE-TEKIYO.
           MOVE SPACES      TO LK-ENTRY-CD.
           MOVE SPACE       TO LK-DR-CR-KBN.
           MOVE SPACES      TO LK-TEKIYO-HOJO-CD.
           MOVE WK-ERR-NORMAL TO LK-ERR-KBN.
      *
       2000-CHECK-INPUT SECTION.
      *
      *    入力値の組合せを仕訳変換前に確認する。
           SET WK-VALID TO TRUE.
      *
           IF LK-AMT-SIGN NOT = '+'
              AND LK-AMT-SIGN NOT = '-'
              MOVE WK-ERR-SIGN TO LK-ERR-KBN
              SET WK-INVALID TO TRUE
           END-IF.
      *
           IF WK-VALID
              IF LK-GEN-KBN NOT = '1'
                 AND LK-GEN-KBN NOT = '2'
                 AND LK-GEN-KBN NOT = '3'
                 AND LK-GEN-KBN NOT = '9'
                 MOVE WK-ERR-GEN TO LK-ERR-KBN
                 SET WK-INVALID TO TRUE
              END-IF
           END-IF.
      *
           IF WK-VALID
              IF LK-CANCEL-KBN = '1'
                 SET WK-CANCEL TO TRUE
              ELSE
                 IF LK-CANCEL-KBN = '0'
                    SET WK-NORMAL TO TRUE
                 ELSE
                    MOVE WK-ERR-CANCEL TO LK-ERR-KBN
                    SET WK-INVALID TO TRUE
                 END-IF
              END-IF
           END-IF.
      *
      *    取消は取消元区分で到着する前提で確認する。
           IF WK-VALID
              IF WK-CANCEL
                 IF LK-GEN-KBN NOT = '9'
                    MOVE WK-ERR-CANCEL TO LK-ERR-KBN
                    SET WK-INVALID TO TRUE
                 END-IF
              ELSE
                 IF LK-GEN-KBN = '9'
                    MOVE WK-ERR-CANCEL TO LK-ERR-KBN
                    SET WK-INVALID TO TRUE
                 END-IF
              END-IF
           END-IF.
      *
       3000-MAKE-BASE SECTION.
      *
      *    取引種別ごとの基本仕訳を設定する。
           EVALUATE LK-TRN-KBN
              WHEN '01'
                 MOVE 'DPST' TO WK-BASE-ENTRY
                 MOVE 'C'    TO WK-BASE-DRCR
                 MOVE '101'  TO WK-BASE-TEKIYO
              WHEN '02'
                 MOVE 'WDRW' TO WK-BASE-ENTRY
                 MOVE 'D'    TO WK-BASE-DRCR
                 MOVE '102'  TO WK-BASE-TEKIYO
              WHEN '11'
                 MOVE 'INTP' TO WK-BASE-ENTRY
                 MOVE 'D'    TO WK-BASE-DRCR
                 MOVE '201'  TO WK-BASE-TEKIYO
              WHEN '12'
                 MOVE 'TAXW' TO WK-BASE-ENTRY
                 MOVE 'C'    TO WK-BASE-DRCR
                 MOVE '202'  TO WK-BASE-TEKIYO
              WHEN '21'
                 MOVE 'FEEP' TO WK-BASE-ENTRY
                 MOVE 'C'    TO WK-BASE-DRCR
                 MOVE '301'  TO WK-BASE-TEKIYO
              WHEN '31'
                 MOVE 'TRBY' TO WK-BASE-ENTRY
                 MOVE 'D'    TO WK-BASE-DRCR
                 MOVE '401'  TO WK-BASE-TEKIYO
              WHEN '32'
                 MOVE 'TRSL' TO WK-BASE-ENTRY
                 MOVE 'C'    TO WK-BASE-DRCR
                 MOVE '402'  TO WK-BASE-TEKIYO
              WHEN '41'
                 MOVE 'TFIN' TO WK-BASE-ENTRY
                 MOVE 'C'    TO WK-BASE-DRCR
                 MOVE '501'  TO WK-BASE-TEKIYO
              WHEN '42'
                 MOVE 'TFOT' TO WK-BASE-ENTRY
                 MOVE 'D'    TO WK-BASE-DRCR
                 MOVE '502'  TO WK-BASE-TEKIYO
              WHEN OTHER
                 MOVE WK-ERR-TRN TO LK-ERR-KBN
                 SET WK-INVALID TO TRUE
           END-EVALUATE.
      *
      *    金額符号がマイナスの通常明細は基本貸借を反転する。
           IF WK-VALID
              IF LK-AMT-SIGN = '-'
                 PERFORM 3100-REVERSE-DRCR
                 MOVE WK-REV-DRCR TO WK-BASE-DRCR
              END-IF
           END-IF.
      *
       3100-REVERSE-DRCR SECTION.
      *
           IF WK-BASE-DRCR = 'D'
              MOVE 'C' TO WK-REV-DRCR
           ELSE
              MOVE 'D' TO WK-REV-DRCR
           END-IF.
      *
       4000-EDIT-OUTPUT SECTION.
      *
      *    取消明細は元明細の貸借に戻らないよう再反転する。
           IF WK-CANCEL
              IF LK-AMT-SIGN = '+'
                 MOVE WK-ERR-CANCEL TO LK-ERR-KBN
                 SET WK-INVALID TO TRUE
              ELSE
                 PERFORM 3100-REVERSE-DRCR
                 MOVE WK-REV-DRCR TO WK-BASE-DRCR
              END-IF
           END-IF.
      *
           IF WK-VALID
              MOVE WK-BASE-ENTRY  TO LK-ENTRY-CD
              MOVE WK-BASE-DRCR   TO LK-DR-CR-KBN
              MOVE WK-BASE-TEKIYO TO LK-TEKIYO-HOJO-CD
              MOVE WK-ERR-NORMAL  TO LK-ERR-KBN
           ELSE
              MOVE SPACES TO LK-ENTRY-CD
              MOVE SPACE  TO LK-DR-CR-KBN
              MOVE SPACES TO LK-TEKIYO-HOJO-CD
           END-IF.
      *
       END PROGRAM KZ310S.
