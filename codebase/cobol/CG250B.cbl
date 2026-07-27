       IDENTIFICATION DIVISION.
       PROGRAM-ID. CG250B.
      ******************************************************************
      * 変更履歴
      * 版数  年月日    担当    概要
      * 1.00  20240401  開発一  新規作成
      * 1.01  20240615  保守一  基準日判定を追加
      * 1.02  20240930  保守一  未使用コード出力を追加
      ******************************************************************
      * 顧客区分マスタ整合バッチ
      * CIF基本ファイルに出現する性別区分と顧客状態区分について、
      * 基準日時点の共通コード登録有無を検査する。
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CGCODF ASSIGN TO DYNAMIC WS-CGCODF-NAME
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS GC-CODE-ID
               FILE STATUS IS FS-CGCODF.
           SELECT CMCIFF ASSIGN TO DYNAMIC WS-CMCIFF-NAME
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-CMCIFF.
           SELECT CKERRF ASSIGN TO DYNAMIC WS-CKERRF-NAME
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-CKERRF.

       DATA DIVISION.
       FILE SECTION.
       FD  CGCODF.
           COPY CGCODC.
       FD  CMCIFF.
           COPY CMCIFFC.
       FD  CKERRF.
           COPY CKERRC.

       WORKING-STORAGE SECTION.
       01  WS-FILE-NAMES.
           05 WS-CGCODF-NAME        PIC X(08) VALUE "CGCODF".
           05 WS-CMCIFF-NAME        PIC X(08) VALUE "CMCIFF".
           05 WS-CKERRF-NAME        PIC X(08) VALUE "CKERRF".

       01  WS-FILE-STATUS.
           05 FS-CGCODF             PIC XX VALUE SPACE.
           05 FS-CMCIFF             PIC XX VALUE SPACE.
           05 FS-CKERRF             PIC XX VALUE SPACE.

       01  WS-CONSTANTS.
           05 CN-PGM-ID             PIC X(08) VALUE "CG250B".
           05 CN-CODE-SEX           PIC X(20) VALUE "SEX".
           05 CN-CODE-STATUS        PIC X(20) VALUE "CIF-STATUS".
           05 CN-ERR-UNDEF-SEX      PIC X(08) VALUE "E250101".
           05 CN-ERR-UNDEF-STS      PIC X(08) VALUE "E250102".
           05 CN-ERR-UNUSED-SEX     PIC X(08) VALUE "W250201".
           05 CN-ERR-UNUSED-STS     PIC X(08) VALUE "W250202".
           05 CN-SEX-KEY            PIC X(20) VALUE "CF-SEX-KBN".
           05 CN-STS-KEY            PIC X(20) VALUE "CF-CIF-STATUS-KBN".

       01  WS-CURRENT-DATE.
           05 WS-DATE-YYYYMMDD      PIC 9(08).
           05 WS-DATE-REST          PIC X(13).

       01  WS-FLAGS.
           05 WS-EOF-CGCODF         PIC X VALUE "N".
           05 WS-EOF-CMCIFF         PIC X VALUE "N".
           05 WS-FOUND-FLG          PIC X VALUE "N".

       01  WS-COUNTERS.
           05 WS-SEX-CNT            PIC 9(04) VALUE 0.
           05 WS-STS-CNT            PIC 9(04) VALUE 0.
           05 WS-CIF-READ-CNT       PIC 9(09) VALUE 0.
           05 WS-CODE-READ-CNT      PIC 9(09) VALUE 0.
           05 WS-ERR-WRITE-CNT      PIC 9(09) VALUE 0.
           05 WS-ERROR-SEQ          PIC 9(09) VALUE 0.
           05 IX                    PIC 9(04) VALUE 0.

       01  WS-CODE-WORK.
           05 WS-GC-FROM-DT         PIC 9(08) VALUE 0.
           05 WS-GC-TO-DT           PIC 9(08) VALUE 0.
           05 WS-CODE-VALUE-WK      PIC X(10) VALUE SPACE.

       01  WS-SEX-TABLE.
           05 WS-SEX-ENT OCCURS 100 TIMES.
              10 WS-SEX-VAL         PIC X(10).
              10 WS-SEX-USED        PIC X.
       01  WS-STS-TABLE.
           05 WS-STS-ENT OCCURS 100 TIMES.
              10 WS-STS-VAL         PIC X(10).
              10 WS-STS-USED        PIC X.

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE
           MOVE 0 TO RETURN-CODE
           PERFORM 1000-OPEN-FILES
           IF RETURN-CODE = 0
              PERFORM 2000-LOAD-CODE-MASTER
           END-IF
           IF RETURN-CODE = 0
              PERFORM 3000-CHECK-CIF-FILE
           END-IF
           IF RETURN-CODE = 0
              PERFORM 4000-WRITE-UNUSED-CODE
           END-IF
           PERFORM 9000-CLOSE-FILES
           IF RETURN-CODE = 0
              DISPLAY "CG250B 正常終了 CIF件数=" WS-CIF-READ-CNT
                      " エラー出力件数=" WS-ERR-WRITE-CNT
           END-IF
           GOBACK.

       1000-OPEN-FILES.
           OPEN INPUT CGCODF
           IF FS-CGCODF NOT = "00"
              DISPLAY "CGCODF オープン失敗 ST=" FS-CGCODF
              MOVE 12 TO RETURN-CODE
           END-IF
           IF RETURN-CODE = 0
              OPEN INPUT CMCIFF
              IF FS-CMCIFF NOT = "00"
                 DISPLAY "CMCIFF オープン失敗 ST=" FS-CMCIFF
                 MOVE 12 TO RETURN-CODE
              END-IF
           END-IF
           IF RETURN-CODE = 0
              OPEN OUTPUT CKERRF
              IF FS-CKERRF NOT = "00"
                 DISPLAY "CKERRF オープン失敗 ST=" FS-CKERRF
                 MOVE 12 TO RETURN-CODE
              END-IF
           END-IF.

       2000-LOAD-CODE-MASTER.
           PERFORM UNTIL WS-EOF-CGCODF = "Y"
              READ CGCODF
                 AT END
                    MOVE "Y" TO WS-EOF-CGCODF
                 NOT AT END
                    ADD 1 TO WS-CODE-READ-CNT
                    PERFORM 2100-TAKE-ACTIVE-CODE
              END-READ
              IF FS-CGCODF NOT = "00" AND FS-CGCODF NOT = "10"
                 DISPLAY "CGCODF 読込失敗 ST=" FS-CGCODF
                 MOVE 12 TO RETURN-CODE
                 MOVE "Y" TO WS-EOF-CGCODF
              END-IF
           END-PERFORM.

       2100-TAKE-ACTIVE-CODE.
           MOVE GC-VALID-FROM-DT TO WS-GC-FROM-DT
           MOVE GC-VALID-TO-DT   TO WS-GC-TO-DT
           IF WS-GC-FROM-DT <= WS-DATE-YYYYMMDD
              AND WS-GC-TO-DT >= WS-DATE-YYYYMMDD
              EVALUATE GC-CODE-KBN
                 WHEN CN-CODE-SEX
                    IF WS-SEX-CNT < 100
                       ADD 1 TO WS-SEX-CNT
                       MOVE GC-CODE-VALUE TO WS-SEX-VAL(WS-SEX-CNT)
                       MOVE "N" TO WS-SEX-USED(WS-SEX-CNT)
                    ELSE
                       DISPLAY "性別区分マスタ件数超過"
                       MOVE 12 TO RETURN-CODE
                       MOVE "Y" TO WS-EOF-CGCODF
                    END-IF
                 WHEN CN-CODE-STATUS
                    IF WS-STS-CNT < 100
                       ADD 1 TO WS-STS-CNT
                       MOVE GC-CODE-VALUE TO WS-STS-VAL(WS-STS-CNT)
                       MOVE "N" TO WS-STS-USED(WS-STS-CNT)
                    ELSE
                       DISPLAY "顧客状態区分マスタ件数超過"
                       MOVE 12 TO RETURN-CODE
                       MOVE "Y" TO WS-EOF-CGCODF
                    END-IF
              END-EVALUATE
           END-IF.

       3000-CHECK-CIF-FILE.
           PERFORM UNTIL WS-EOF-CMCIFF = "Y"
              READ CMCIFF
                 AT END
                    MOVE "Y" TO WS-EOF-CMCIFF
                 NOT AT END
                    ADD 1 TO WS-CIF-READ-CNT
                    PERFORM 3100-CHECK-SEX
                    PERFORM 3200-CHECK-STATUS
              END-READ
              IF FS-CMCIFF NOT = "00" AND FS-CMCIFF NOT = "10"
                 DISPLAY "CMCIFF 読込失敗 ST=" FS-CMCIFF
                 MOVE 12 TO RETURN-CODE
                 MOVE "Y" TO WS-EOF-CMCIFF
              END-IF
              IF RETURN-CODE NOT = 0
                 MOVE "Y" TO WS-EOF-CMCIFF
              END-IF
           END-PERFORM.

       3100-CHECK-SEX.
           MOVE "N" TO WS-FOUND-FLG
           MOVE CF-SEX-KBN TO WS-CODE-VALUE-WK
           PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > WS-SEX-CNT
              IF WS-CODE-VALUE-WK = WS-SEX-VAL(IX)
                 MOVE "Y" TO WS-FOUND-FLG
                 MOVE "Y" TO WS-SEX-USED(IX)
                 MOVE WS-SEX-CNT TO IX
              END-IF
           END-PERFORM
           IF WS-FOUND-FLG NOT = "Y"
              PERFORM 5000-INIT-ERROR
              MOVE CF-CIF-NO TO ER-CIF-NO
              MOVE CN-SEX-KEY TO ER-KEY-ID
              MOVE CN-ERR-UNDEF-SEX TO ER-ERROR-CD
              PERFORM 5100-WRITE-ERROR
           END-IF.

       3200-CHECK-STATUS.
           MOVE "N" TO WS-FOUND-FLG
           MOVE CF-CIF-STATUS-KBN TO WS-CODE-VALUE-WK
           PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > WS-STS-CNT
              IF WS-CODE-VALUE-WK = WS-STS-VAL(IX)
                 MOVE "Y" TO WS-FOUND-FLG
                 MOVE "Y" TO WS-STS-USED(IX)
                 MOVE WS-STS-CNT TO IX
              END-IF
           END-PERFORM
           IF WS-FOUND-FLG NOT = "Y"
              PERFORM 5000-INIT-ERROR
              MOVE CF-CIF-NO TO ER-CIF-NO
              MOVE CN-STS-KEY TO ER-KEY-ID
              MOVE CN-ERR-UNDEF-STS TO ER-ERROR-CD
              PERFORM 5100-WRITE-ERROR
           END-IF.

       4000-WRITE-UNUSED-CODE.
           PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > WS-SEX-CNT
              IF WS-SEX-USED(IX) NOT = "Y"
                 PERFORM 5000-INIT-ERROR
                 MOVE CN-SEX-KEY TO ER-KEY-ID
                 MOVE CN-ERR-UNUSED-SEX TO ER-ERROR-CD
                 PERFORM 5100-WRITE-ERROR
              END-IF
           END-PERFORM
           PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > WS-STS-CNT
              IF WS-STS-USED(IX) NOT = "Y"
                 PERFORM 5000-INIT-ERROR
                 MOVE CN-STS-KEY TO ER-KEY-ID
                 MOVE CN-ERR-UNUSED-STS TO ER-ERROR-CD
                 PERFORM 5100-WRITE-ERROR
              END-IF
           END-PERFORM.

       5000-INIT-ERROR.
           INITIALIZE CKERRF-REC
           ADD 1 TO WS-ERROR-SEQ
           MOVE WS-ERROR-SEQ TO ER-ERROR-ID
           MOVE CN-PGM-ID TO ER-SOURCE-PGM-ID
           MOVE WS-DATE-YYYYMMDD TO ER-ERROR-DT.

       5100-WRITE-ERROR.
           WRITE CKERRF-REC
           IF FS-CKERRF = "00"
              ADD 1 TO WS-ERR-WRITE-CNT
           ELSE
              DISPLAY "CKERRF 書込失敗 ST=" FS-CKERRF
              MOVE 12 TO RETURN-CODE
           END-IF.

       9000-CLOSE-FILES.
           CLOSE CGCODF
           IF FS-CGCODF NOT = "00"
              DISPLAY "CGCODF クローズ失敗 ST=" FS-CGCODF
              MOVE 8 TO RETURN-CODE
           END-IF
           CLOSE CMCIFF
           IF FS-CMCIFF NOT = "00"
              DISPLAY "CMCIFF クローズ失敗 ST=" FS-CMCIFF
              MOVE 8 TO RETURN-CODE
           END-IF
           CLOSE CKERRF
           IF FS-CKERRF NOT = "00"
              DISPLAY "CKERRF クローズ失敗 ST=" FS-CKERRF
              MOVE 8 TO RETURN-CODE
           END-IF.
