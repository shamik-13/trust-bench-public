       IDENTIFICATION DIVISION.
       PROGRAM-ID. KZ521S.
      * 変更履歴
      * 版数  年月日(和暦)  担当                         概要
      * 1.00  H24.04.01    システム部 勘定系チーム      新規作成
      * 1.01  H28.10.15    システム部 勘定系チーム      督促区分判定を追加
      * 1.02  R03.06.30    システム部 勘定系チーム      文面選択条件を見直し

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  W-RETURN-SW.
           05 W-HARD-ERR-SW              PIC X VALUE 'N'.
              88 W-HARD-ERR                  VALUE 'Y'.

       01  W-EDIT-AREA.
           05 W-AGING-NUM                PIC 9(02) VALUE ZERO.
           05 W-NOTICE-NUM               PIC 9(02) VALUE ZERO.
           05 W-CEILING-CNT              PIC 9(02) VALUE ZERO.
           05 W-LOOKUP-KEY               PIC 9(04) VALUE ZERO.

       01  W-NOTICE-MATRIX.
           05 FILLER PIC X(11) VALUE '0101A011001'.
           05 FILLER PIC X(11) VALUE '0102A012002'.
           05 FILLER PIC X(11) VALUE '0103A013003'.
           05 FILLER PIC X(11) VALUE '0104A014004'.
           05 FILLER PIC X(11) VALUE '0201B021001'.
           05 FILLER PIC X(11) VALUE '0202B022002'.
           05 FILLER PIC X(11) VALUE '0203B023003'.
           05 FILLER PIC X(11) VALUE '0204B024004'.
           05 FILLER PIC X(11) VALUE '0301C031001'.
           05 FILLER PIC X(11) VALUE '0302C032002'.
           05 FILLER PIC X(11) VALUE '0303C033003'.
           05 FILLER PIC X(11) VALUE '0304C034004'.
           05 FILLER PIC X(11) VALUE '0401D041002'.
           05 FILLER PIC X(11) VALUE '0402D042003'.
           05 FILLER PIC X(11) VALUE '0403D043004'.
           05 FILLER PIC X(11) VALUE '0404D044005'.
           05 FILLER PIC X(11) VALUE '0501E051003'.
           05 FILLER PIC X(11) VALUE '0502E052004'.
           05 FILLER PIC X(11) VALUE '0503E053005'.
           05 FILLER PIC X(11) VALUE '0504E054006'.

       01  W-NOTICE-TABLE REDEFINES W-NOTICE-MATRIX.
           05 W-NOTICE-ENTRY OCCURS 20 TIMES
              ASCENDING KEY IS WNM-KEY
              INDEXED BY WNM-IDX.
              10 WNM-KEY                 PIC 9(04).
              10 WNM-TEMPLATE-CODE       PIC X(04).
              10 WNM-ESCALATION-LVL      PIC 9(03).

       LINKAGE SECTION.
       01  DNN-PARAM.
           05 DNN-AGING-BUCKET           PIC 9(02).
           05 DNN-NOTICE-COUNT           PIC 9(02).
           05 DNN-LEGAL-FLAG             PIC X.
           05 DNN-TEMPLATE-CODE          PIC X(04).
           05 DNN-ESCALATION-LVL         PIC 9(03).
           05 DNN-SUPPRESS-IND           PIC X.

       PROCEDURE DIVISION USING DNN-PARAM.
       MAIN-RTN.
           MOVE 0 TO RETURN-CODE
           PERFORM INIT-RTN
           PERFORM VALIDATE-RTN
           IF W-HARD-ERR
              MOVE 8 TO RETURN-CODE
              GOBACK
           END-IF
           PERFORM DECIDE-CEILING-RTN
           IF DNN-NOTICE-COUNT > W-CEILING-CNT
              MOVE 'Y' TO DNN-SUPPRESS-IND
              GOBACK
           END-IF
           PERFORM LOOKUP-RTN
           IF W-HARD-ERR
              MOVE 12 TO RETURN-CODE
           END-IF
           GOBACK.

       INIT-RTN.
           MOVE SPACES TO DNN-TEMPLATE-CODE
           MOVE ZERO   TO DNN-ESCALATION-LVL
           MOVE 'N'    TO DNN-SUPPRESS-IND
           MOVE 'N'    TO W-HARD-ERR-SW.

       VALIDATE-RTN.
           IF DNN-AGING-BUCKET NOT NUMERIC
              DISPLAY '督促階層不正 値=' DNN-AGING-BUCKET
              MOVE 'Y' TO W-HARD-ERR-SW
              EXIT PARAGRAPH
           END-IF
           IF DNN-NOTICE-COUNT NOT NUMERIC
              DISPLAY '督促回数不正 値=' DNN-NOTICE-COUNT
              MOVE 'Y' TO W-HARD-ERR-SW
              EXIT PARAGRAPH
           END-IF
           IF DNN-LEGAL-FLAG NOT = 'Y'
              AND DNN-LEGAL-FLAG NOT = 'N'
              DISPLAY '法的移管区分不正 値=' DNN-LEGAL-FLAG
              MOVE 'Y' TO W-HARD-ERR-SW
              EXIT PARAGRAPH
           END-IF
           IF DNN-AGING-BUCKET < 1 OR DNN-AGING-BUCKET > 5
              DISPLAY '督促階層範囲外 値=' DNN-AGING-BUCKET
              MOVE 'Y' TO W-HARD-ERR-SW
              EXIT PARAGRAPH
           END-IF
           IF DNN-NOTICE-COUNT < 1
              DISPLAY '督促回数ゼロ不正 値=' DNN-NOTICE-COUNT
              MOVE 'Y' TO W-HARD-ERR-SW
           END-IF.

       DECIDE-CEILING-RTN.
           EVALUATE DNN-AGING-BUCKET
              WHEN 1
                 MOVE 1 TO W-CEILING-CNT
              WHEN 2
                 MOVE 2 TO W-CEILING-CNT
              WHEN 3
                 MOVE 3 TO W-CEILING-CNT
              WHEN 4
                 MOVE 4 TO W-CEILING-CNT
              WHEN 5
                 MOVE 4 TO W-CEILING-CNT
           END-EVALUATE
           IF DNN-LEGAL-FLAG = 'Y'
              AND W-CEILING-CNT > 2
              MOVE 2 TO W-CEILING-CNT
           END-IF.

       LOOKUP-RTN.
           COMPUTE W-LOOKUP-KEY =
              (DNN-AGING-BUCKET * 100) + DNN-NOTICE-COUNT
           SEARCH ALL W-NOTICE-ENTRY
              AT END
                 DISPLAY '督促文面表未登録 キー=' W-LOOKUP-KEY
                 MOVE 'Y' TO W-HARD-ERR-SW
              WHEN WNM-KEY(WNM-IDX) = W-LOOKUP-KEY
                 MOVE WNM-TEMPLATE-CODE(WNM-IDX)
                   TO DNN-TEMPLATE-CODE
                 MOVE WNM-ESCALATION-LVL(WNM-IDX)
                   TO DNN-ESCALATION-LVL
           END-SEARCH.
