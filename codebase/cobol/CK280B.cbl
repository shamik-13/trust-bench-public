       IDENTIFICATION DIVISION.
       PROGRAM-ID. CK280B.
      *================================================================*
      * 変更履歴                                                       *
      * 版数  年月日    担当   概要                                    *
      * 1.00  20240401  基盤   新規作成                                *
      * 1.01  20240915  基盤   未定義コード集約追加                    *
      * 1.02  20250120  基盤   監査帳票文言見直し                      *
      *================================================================*
      * 統合キーエラー集約バッチ                                       *
      * CKERRFをプログラム、エラーコード、CIF有無、KEY-ID有無で集約する *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CKERRF ASSIGN TO "CKERRF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-CKERRF.
           SELECT CGCODF ASSIGN TO "CGCODF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS GC-CODE-ID
               FILE STATUS IS FS-CGCODF.
           SELECT CKRPTF ASSIGN TO "CKRPTF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-CKRPTF.

       DATA DIVISION.
       FILE SECTION.
       FD  CKERRF.
           COPY CKERRC.
       FD  CGCODF.
           COPY CGCODC.
       FD  CKRPTF.
           COPY CKRPTC.

       WORKING-STORAGE SECTION.
       01  FS-AREA.
           05 FS-CKERRF              PIC XX VALUE SPACES.
           05 FS-CGCODF              PIC XX VALUE SPACES.
           05 FS-CKRPTF              PIC XX VALUE SPACES.

       01  SW-AREA.
           05 SW-EOF-CKERRF          PIC X VALUE "N".
              88 EOF-CKERRF                VALUE "Y".
              88 NOT-EOF-CKERRF            VALUE "N".
           05 SW-CODE-FOUND          PIC X VALUE "N".
              88 CODE-FOUND                VALUE "Y".
              88 CODE-NOT-FOUND            VALUE "N".
           05 SW-AGG-FOUND           PIC X VALUE "N".
              88 AGG-FOUND                 VALUE "Y".
              88 AGG-NOT-FOUND             VALUE "N".
           05 SW-CIF-ARI             PIC X VALUE "N".
           05 SW-KEY-ARI             PIC X VALUE "N".
           05 SW-HARD-ERROR          PIC X VALUE "N".
              88 HARD-ERROR                VALUE "Y".

       01  CONST-AREA.
           05 CN-PGM-ID              PIC X(08) VALUE "CK280B".
           05 CN-CODE-KBN            PIC X(10) VALUE "CKERRCD".
           05 CN-VALID-LOW           PIC X(08) VALUE "00000000".
           05 CN-VALID-HIGH          PIC X(08) VALUE "99999999".
           05 CN-SECTION-DEF         PIC X(02) VALUE "01".
           05 CN-SECTION-UNDEF       PIC X(02) VALUE "02".
           05 CN-SECTION-TOTAL       PIC X(02) VALUE "99".
           05 CN-UNCLASS-NAME        PIC X(40) VALUE
              "未分類".
           05 CN-MAX-AGG             PIC 9(04) VALUE 1000.

       01  WK-AREA.
           05 WK-REPORT-YYYYMM       PIC 9(06) VALUE ZERO.
           05 WK-LINE-NO             PIC 9(06) VALUE ZERO.
           05 WK-REPORT-ID-N         PIC 9(10) VALUE ZERO.
           05 WK-REPORT-ID           PIC X(12) VALUE SPACES.
           05 WK-IDX                 PIC 9(04) VALUE ZERO.
           05 WK-AGG-CNT             PIC 9(04) VALUE ZERO.
           05 WK-MATCH-IDX           PIC 9(04) VALUE ZERO.
           05 WK-READ-CNT            PIC 9(09) VALUE ZERO.
           05 WK-WRITE-CNT           PIC 9(09) VALUE ZERO.
           05 WK-DEF-CNT             PIC 9(09) VALUE ZERO.
           05 WK-UNDEF-CNT           PIC 9(09) VALUE ZERO.
           05 WK-DATE8               PIC 9(08) VALUE ZERO.
           05 WK-DATE6               PIC 9(06) VALUE ZERO.
           05 WK-CODE-NAME           PIC X(40) VALUE SPACES.
           05 WK-MESSAGE             PIC X(80) VALUE SPACES.
           05 WK-TEXT                PIC X(120) VALUE SPACES.
           05 WK-COUNT-Z             PIC Z(08)9.
           05 WK-LINE-Z              PIC Z(05)9.
           05 WK-SORT-SW             PIC X VALUE "N".
           05 WK-SORT-IDX            PIC 9(04) VALUE ZERO.
           05 WK-NEXT-IDX            PIC 9(04) VALUE ZERO.

       01  AGG-TABLE.
           05 AGG-ENTRY OCCURS 1000 TIMES.
              10 AG-PGM-ID           PIC X(08) VALUE SPACES.
              10 AG-ERROR-CD         PIC X(10) VALUE SPACES.
              10 AG-CIF-FLG          PIC X VALUE SPACE.
              10 AG-KEY-FLG          PIC X VALUE SPACE.
              10 AG-CODE-NAME        PIC X(40) VALUE SPACES.
              10 AG-DEFINED-FLG      PIC X VALUE SPACE.
              10 AG-COUNT            PIC 9(09) VALUE ZERO.

       01  SAVE-ENTRY.
           05 SV-PGM-ID              PIC X(08) VALUE SPACES.
           05 SV-ERROR-CD            PIC X(10) VALUE SPACES.
           05 SV-CIF-FLG             PIC X VALUE SPACE.
           05 SV-KEY-FLG             PIC X VALUE SPACE.
           05 SV-CODE-NAME           PIC X(40) VALUE SPACES.
           05 SV-DEFINED-FLG         PIC X VALUE SPACE.
           05 SV-COUNT               PIC 9(09) VALUE ZERO.

       PROCEDURE DIVISION.
       0000-MAIN SECTION.
           PERFORM 0100-INIT
           IF NOT HARD-ERROR
               PERFORM 1000-READ-LOOP
           END-IF
           IF NOT HARD-ERROR
               PERFORM 3000-SORT-AGG
               PERFORM 4000-WRITE-REPORT
           END-IF
           PERFORM 9000-FINAL
           GOBACK.

       0100-INIT SECTION.
           MOVE 0 TO RETURN-CODE
           ACCEPT WK-DATE8 FROM DATE YYYYMMDD
           COMPUTE WK-DATE6 = WK-DATE8 / 100
           MOVE WK-DATE6 TO WK-REPORT-YYYYMM

           OPEN INPUT CKERRF
           IF FS-CKERRF NOT = "00"
               STRING "CKERRF オープン失敗 ST=" FS-CKERRF
                   DELIMITED BY SIZE INTO WK-MESSAGE
               END-STRING
               DISPLAY WK-MESSAGE
               PERFORM 9100-HARD-ERROR
           END-IF

           IF NOT HARD-ERROR
               OPEN INPUT CGCODF
               IF FS-CGCODF NOT = "00"
                   STRING "CGCODF オープン失敗 ST=" FS-CGCODF
                       DELIMITED BY SIZE INTO WK-MESSAGE
                   END-STRING
                   DISPLAY WK-MESSAGE
                   PERFORM 9100-HARD-ERROR
               END-IF
           END-IF

           IF NOT HARD-ERROR
               OPEN OUTPUT CKRPTF
               IF FS-CKRPTF NOT = "00"
                   STRING "CKRPTF オープン失敗 ST=" FS-CKRPTF
                       DELIMITED BY SIZE INTO WK-MESSAGE
                   END-STRING
                   DISPLAY WK-MESSAGE
                   PERFORM 9100-HARD-ERROR
               END-IF
           END-IF.

       1000-READ-LOOP SECTION.
           SET NOT-EOF-CKERRF TO TRUE
           PERFORM UNTIL EOF-CKERRF OR HARD-ERROR
               READ CKERRF
                   AT END
                       SET EOF-CKERRF TO TRUE
                   NOT AT END
                       IF FS-CKERRF = "00"
                           ADD 1 TO WK-READ-CNT
                           PERFORM 1100-VALIDATE-ERROR
                           IF NOT HARD-ERROR
                               PERFORM 1200-FIND-CODE
                               PERFORM 1300-ACCUMULATE
                           END-IF
                       ELSE
                           STRING "CKERRF 読込失敗 ST=" FS-CKERRF
                               DELIMITED BY SIZE INTO WK-MESSAGE
                           END-STRING
                           DISPLAY WK-MESSAGE
                           PERFORM 9100-HARD-ERROR
                       END-IF
               END-READ
           END-PERFORM.

       1100-VALIDATE-ERROR SECTION.
           IF ER-SOURCE-PGM-ID = SPACES
               DISPLAY "CKERRF プログラムID未設定"
               PERFORM 9100-HARD-ERROR
           END-IF

           IF ER-ERROR-CD = SPACES
               DISPLAY "CKERRF エラーコード未設定"
               PERFORM 9100-HARD-ERROR
           END-IF

           IF ER-ERROR-DT < CN-VALID-LOW
              OR ER-ERROR-DT > CN-VALID-HIGH
               DISPLAY "CKERRF エラー日付形式不正"
               PERFORM 9100-HARD-ERROR
           END-IF

           IF ER-CIF-NO = SPACES OR ER-CIF-NO = ZERO
               MOVE "N" TO SW-CIF-ARI
           ELSE
               MOVE "Y" TO SW-CIF-ARI
           END-IF

           IF ER-KEY-ID = SPACES OR ER-KEY-ID = ZERO
               MOVE "N" TO SW-KEY-ARI
           ELSE
               MOVE "Y" TO SW-KEY-ARI
           END-IF.

       1200-FIND-CODE SECTION.
           SET CODE-NOT-FOUND TO TRUE
           MOVE SPACES TO WK-CODE-NAME
           MOVE CN-CODE-KBN TO GC-CODE-KBN
           MOVE ER-ERROR-CD TO GC-CODE-VALUE
           STRING GC-CODE-KBN GC-CODE-VALUE
               DELIMITED BY SIZE INTO GC-CODE-ID
           END-STRING

           READ CGCODF KEY IS GC-CODE-ID
               INVALID KEY
                   IF FS-CGCODF = "23"
                       SET CODE-NOT-FOUND TO TRUE
                   ELSE
                       STRING "CGCODF 読込失敗 ST=" FS-CGCODF
                           DELIMITED BY SIZE INTO WK-MESSAGE
                       END-STRING
                       DISPLAY WK-MESSAGE
                       PERFORM 9100-HARD-ERROR
                   END-IF
               NOT INVALID KEY
                   IF FS-CGCODF = "00"
                      AND GC-CODE-KBN = CN-CODE-KBN
                      AND GC-CODE-VALUE = ER-ERROR-CD
                      AND WK-DATE8 >= GC-VALID-FROM-DT
                      AND WK-DATE8 <= GC-VALID-TO-DT
                       SET CODE-FOUND TO TRUE
                       MOVE GC-CODE-NAME TO WK-CODE-NAME
                   ELSE
                       SET CODE-NOT-FOUND TO TRUE
                   END-IF
           END-READ.

       1300-ACCUMULATE SECTION.
           SET AGG-NOT-FOUND TO TRUE
           MOVE ZERO TO WK-MATCH-IDX

           PERFORM VARYING WK-IDX FROM 1 BY 1
                   UNTIL WK-IDX > WK-AGG-CNT OR AGG-FOUND
               IF AG-PGM-ID(WK-IDX) = ER-SOURCE-PGM-ID
                  AND AG-ERROR-CD(WK-IDX) = ER-ERROR-CD
                  AND AG-CIF-FLG(WK-IDX) = SW-CIF-ARI
                  AND AG-KEY-FLG(WK-IDX) = SW-KEY-ARI
                   SET AGG-FOUND TO TRUE
                   MOVE WK-IDX TO WK-MATCH-IDX
               END-IF
           END-PERFORM

           IF AGG-FOUND
               ADD 1 TO AG-COUNT(WK-MATCH-IDX)
           ELSE
               IF WK-AGG-CNT >= CN-MAX-AGG
                   DISPLAY "集約表件数上限超過"
                   PERFORM 9100-HARD-ERROR
               ELSE
                   ADD 1 TO WK-AGG-CNT
                   MOVE ER-SOURCE-PGM-ID TO AG-PGM-ID(WK-AGG-CNT)
                   MOVE ER-ERROR-CD TO AG-ERROR-CD(WK-AGG-CNT)
                   MOVE SW-CIF-ARI TO AG-CIF-FLG(WK-AGG-CNT)
                   MOVE SW-KEY-ARI TO AG-KEY-FLG(WK-AGG-CNT)
                   MOVE 1 TO AG-COUNT(WK-AGG-CNT)
                   IF CODE-FOUND
                       MOVE "Y" TO AG-DEFINED-FLG(WK-AGG-CNT)
                       MOVE WK-CODE-NAME TO AG-CODE-NAME(WK-AGG-CNT)
                   ELSE
                       MOVE "N" TO AG-DEFINED-FLG(WK-AGG-CNT)
                       MOVE CN-UNCLASS-NAME TO AG-CODE-NAME(WK-AGG-CNT)
                   END-IF
               END-IF
           END-IF.

       3000-SORT-AGG SECTION.
           IF WK-AGG-CNT > 1
               MOVE "Y" TO WK-SORT-SW
               PERFORM UNTIL WK-SORT-SW = "N"
                   MOVE "N" TO WK-SORT-SW
                   PERFORM VARYING WK-SORT-IDX FROM 1 BY 1
                       UNTIL WK-SORT-IDX >= WK-AGG-CNT
                       COMPUTE WK-NEXT-IDX = WK-SORT-IDX + 1
                       IF AG-DEFINED-FLG(WK-SORT-IDX)
                          > AG-DEFINED-FLG(WK-NEXT-IDX)
                          OR (AG-DEFINED-FLG(WK-SORT-IDX)
                              = AG-DEFINED-FLG(WK-NEXT-IDX)
                          AND AG-PGM-ID(WK-SORT-IDX)
                              > AG-PGM-ID(WK-NEXT-IDX))
                          OR (AG-DEFINED-FLG(WK-SORT-IDX)
                              = AG-DEFINED-FLG(WK-NEXT-IDX)
                          AND AG-PGM-ID(WK-SORT-IDX)
                              = AG-PGM-ID(WK-NEXT-IDX)
                          AND AG-ERROR-CD(WK-SORT-IDX)
                              > AG-ERROR-CD(WK-NEXT-IDX))
                           PERFORM 3100-SWAP-AGG
                           MOVE "Y" TO WK-SORT-SW
                       END-IF
                   END-PERFORM
               END-PERFORM
           END-IF.

       3100-SWAP-AGG SECTION.
           MOVE AG-PGM-ID(WK-SORT-IDX) TO SV-PGM-ID
           MOVE AG-ERROR-CD(WK-SORT-IDX) TO SV-ERROR-CD
           MOVE AG-CIF-FLG(WK-SORT-IDX) TO SV-CIF-FLG
           MOVE AG-KEY-FLG(WK-SORT-IDX) TO SV-KEY-FLG
           MOVE AG-CODE-NAME(WK-SORT-IDX) TO SV-CODE-NAME
           MOVE AG-DEFINED-FLG(WK-SORT-IDX) TO SV-DEFINED-FLG
           MOVE AG-COUNT(WK-SORT-IDX) TO SV-COUNT

           MOVE AG-PGM-ID(WK-NEXT-IDX) TO AG-PGM-ID(WK-SORT-IDX)
           MOVE AG-ERROR-CD(WK-NEXT-IDX) TO AG-ERROR-CD(WK-SORT-IDX)
           MOVE AG-CIF-FLG(WK-NEXT-IDX) TO AG-CIF-FLG(WK-SORT-IDX)
           MOVE AG-KEY-FLG(WK-NEXT-IDX) TO AG-KEY-FLG(WK-SORT-IDX)
           MOVE AG-CODE-NAME(WK-NEXT-IDX) TO AG-CODE-NAME(WK-SORT-IDX)
           MOVE AG-DEFINED-FLG(WK-NEXT-IDX)
               TO AG-DEFINED-FLG(WK-SORT-IDX)
           MOVE AG-COUNT(WK-NEXT-IDX) TO AG-COUNT(WK-SORT-IDX)

           MOVE SV-PGM-ID TO AG-PGM-ID(WK-NEXT-IDX)
           MOVE SV-ERROR-CD TO AG-ERROR-CD(WK-NEXT-IDX)
           MOVE SV-CIF-FLG TO AG-CIF-FLG(WK-NEXT-IDX)
           MOVE SV-KEY-FLG TO AG-KEY-FLG(WK-NEXT-IDX)
           MOVE SV-CODE-NAME TO AG-CODE-NAME(WK-NEXT-IDX)
           MOVE SV-DEFINED-FLG TO AG-DEFINED-FLG(WK-NEXT-IDX)
           MOVE SV-COUNT TO AG-COUNT(WK-NEXT-IDX).

       4000-WRITE-REPORT SECTION.
           PERFORM 4100-WRITE-HEADER

           PERFORM VARYING WK-IDX FROM 1 BY 1 UNTIL WK-IDX > WK-AGG-CNT
               IF AG-DEFINED-FLG(WK-IDX) = "Y"
                   ADD AG-COUNT(WK-IDX) TO WK-DEF-CNT
                   MOVE CN-SECTION-DEF TO RP-SECTION-KBN
               ELSE
                   ADD AG-COUNT(WK-IDX) TO WK-UNDEF-CNT
                   MOVE CN-SECTION-UNDEF TO RP-SECTION-KBN
               END-IF
               PERFORM 4200-WRITE-DETAIL
           END-PERFORM

           PERFORM 4300-WRITE-TOTAL.

       4100-WRITE-HEADER SECTION.
           MOVE CN-SECTION-DEF TO RP-SECTION-KBN
           MOVE "定義済エラーコード別集約" TO WK-TEXT
           PERFORM 4900-WRITE-LINE

           MOVE CN-SECTION-UNDEF TO RP-SECTION-KBN
           MOVE "未定義エラーコード別集約" TO WK-TEXT
           PERFORM 4900-WRITE-LINE.

       4200-WRITE-DETAIL SECTION.
           MOVE AG-COUNT(WK-IDX) TO WK-COUNT-Z
           MOVE SPACES TO WK-TEXT
           STRING
               "PGM=" AG-PGM-ID(WK-IDX)
               " CD=" AG-ERROR-CD(WK-IDX)
               " 名称=" AG-CODE-NAME(WK-IDX)
               " CIF有無=" AG-CIF-FLG(WK-IDX)
               " KEY有無=" AG-KEY-FLG(WK-IDX)
               " 件数=" WK-COUNT-Z
               DELIMITED BY SIZE INTO WK-TEXT
           END-STRING
           PERFORM 4900-WRITE-LINE.

       4300-WRITE-TOTAL SECTION.
           MOVE CN-SECTION-TOTAL TO RP-SECTION-KBN
           MOVE WK-READ-CNT TO WK-COUNT-Z
           MOVE SPACES TO WK-TEXT
           STRING "入力件数=" WK-COUNT-Z
               DELIMITED BY SIZE INTO WK-TEXT
           END-STRING
           PERFORM 4900-WRITE-LINE

           MOVE WK-DEF-CNT TO WK-COUNT-Z
           MOVE SPACES TO WK-TEXT
           STRING "定義済集約対象件数=" WK-COUNT-Z
               DELIMITED BY SIZE INTO WK-TEXT
           END-STRING
           PERFORM 4900-WRITE-LINE

           MOVE WK-UNDEF-CNT TO WK-COUNT-Z
           MOVE SPACES TO WK-TEXT
           STRING "未定義集約対象件数=" WK-COUNT-Z
               DELIMITED BY SIZE INTO WK-TEXT
           END-STRING
           PERFORM 4900-WRITE-LINE.

       4900-WRITE-LINE SECTION.
           ADD 1 TO WK-LINE-NO
           ADD 1 TO WK-REPORT-ID-N
           MOVE WK-REPORT-ID-N TO WK-REPORT-ID
           MOVE WK-REPORT-ID TO RP-REPORT-ID
           MOVE WK-REPORT-YYYYMM TO RP-REPORT-YYYYMM
           MOVE WK-LINE-NO TO RP-LINE-NO
           MOVE WK-TEXT TO RP-REPORT-TEXT

           WRITE CKRPTF-REC
           IF FS-CKRPTF = "00"
               ADD 1 TO WK-WRITE-CNT
           ELSE
               STRING "CKRPTF 書込失敗 ST=" FS-CKRPTF
                   DELIMITED BY SIZE INTO WK-MESSAGE
               END-STRING
               DISPLAY WK-MESSAGE
               PERFORM 9100-HARD-ERROR
           END-IF.

       9000-FINAL SECTION.
           IF FS-CKERRF = "00"
              OR FS-CKERRF = "10"
               CLOSE CKERRF
           END-IF

           IF FS-CGCODF = "00"
              OR FS-CGCODF = "23"
               CLOSE CGCODF
           END-IF

           IF FS-CKRPTF = "00"
               CLOSE CKRPTF
           END-IF

           IF HARD-ERROR
               IF RETURN-CODE = 0
                   MOVE 8 TO RETURN-CODE
               END-IF
           ELSE
               MOVE 0 TO RETURN-CODE
               MOVE WK-WRITE-CNT TO WK-COUNT-Z
               DISPLAY "CK280B 正常終了 出力件数=" WK-COUNT-Z
           END-IF.

       9100-HARD-ERROR SECTION.
           SET HARD-ERROR TO TRUE
           MOVE 8 TO RETURN-CODE.
