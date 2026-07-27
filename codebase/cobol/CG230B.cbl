       IDENTIFICATION DIVISION.
       PROGRAM-ID. CG230B.
      *
      *  変更履歴
      *  版数    年月日    担当        概要
      *  1.00    20240401  共通基盤    初版作成
      *  1.01    20240930  勘定系      状態区分エラー出力追加
      *  1.02    20250131  情報系      統合キー有無別集計追加
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CMCIFF ASSIGN TO "CMCIFF"
             ORGANIZATION IS SEQUENTIAL
             FILE STATUS IS FS-CMCIFF.
           SELECT CMATTF ASSIGN TO "CMATTF"
             ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC
             RECORD KEY IS CA-CIF-NO
             FILE STATUS IS FS-CMATTF.
           SELECT CMKEYF ASSIGN TO "CMKEYF"
             ORGANIZATION IS SEQUENTIAL
             FILE STATUS IS FS-CMKEYF.
           SELECT CGCODF ASSIGN TO "CGCODF"
             ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC
             RECORD KEY IS GC-CODE-ID
             FILE STATUS IS FS-CGCODF.
           SELECT CGSUMF ASSIGN TO "CGSUMF"
             ORGANIZATION IS SEQUENTIAL
             FILE STATUS IS FS-CGSUMF.
           SELECT CKERRF ASSIGN TO "CKERRF"
             ORGANIZATION IS SEQUENTIAL
             FILE STATUS IS FS-CKERRF.
       DATA DIVISION.
       FILE SECTION.
       FD  CMCIFF.
           COPY CMCIFFC.
       FD  CMATTF.
           COPY CMATTC.
       FD  CMKEYF.
           COPY CMKEYFC.
       FD  CGCODF.
           COPY CGCODC.
       FD  CGSUMF.
           COPY CGSUMC.
       FD  CKERRF.
           COPY CKERRC.
       WORKING-STORAGE SECTION.
       01  FS-AREA.
           05 FS-CMCIFF              PIC XX VALUE SPACE.
           05 FS-CMATTF              PIC XX VALUE SPACE.
           05 FS-CMKEYF              PIC XX VALUE SPACE.
           05 FS-CGCODF              PIC XX VALUE SPACE.
           05 FS-CGSUMF              PIC XX VALUE SPACE.
           05 FS-CKERRF              PIC XX VALUE SPACE.
       01  SW-AREA.
           05 SW-CIF-EOF             PIC X VALUE "N".
              88 CIF-EOF                  VALUE "Y".
           05 SW-KEY-EOF             PIC X VALUE "N".
              88 KEY-EOF                  VALUE "Y".
           05 SW-CODE-EOF            PIC X VALUE "N".
              88 CODE-EOF                 VALUE "Y".
           05 SW-HARD-ERR            PIC X VALUE "N".
              88 HARD-ERR                 VALUE "Y".
           05 SW-FOUND               PIC X VALUE "N".
              88 FOUND                    VALUE "Y".
       01  CONST-AREA.
           05 CN-PGM-ID              PIC X(08) VALUE "CG230B".
           05 CN-STATUS-KBN          PIC X(06) VALUE "CIFSTS".
           05 CN-SEGMENT-KBN         PIC X(06) VALUE "SEG   ".
           05 CN-BASE-DT             PIC 9(08) VALUE 20250131.
           05 CN-BASE-YYYYMM         PIC 9(06) VALUE 202501.
           05 CN-STATUS-ACTIVE       PIC X(02) VALUE "01".
           05 CN-STATUS-EXCLUDE      PIC X(02) VALUE "08".
           05 CN-STATUS-INVALID      PIC X(02) VALUE "09".
           05 CN-ERR-CODE            PIC X(04) VALUE "E101".
           05 CN-ERR-SEG             PIC X(04) VALUE "E102".
           05 CN-ERR-ATTR            PIC X(04) VALUE "E103".
       01  COUNT-AREA.
           05 CNT-CIF-READ           PIC 9(09) VALUE ZERO.
           05 CNT-KEY-READ           PIC 9(09) VALUE ZERO.
           05 CNT-CODE-READ          PIC 9(09) VALUE ZERO.
           05 CNT-SUM-WRITE          PIC 9(09) VALUE ZERO.
           05 CNT-ERR-WRITE          PIC 9(09) VALUE ZERO.
           05 CNT-KEY-TBL            PIC 9(05) VALUE ZERO.
           05 CNT-CODE-TBL           PIC 9(04) VALUE ZERO.
           05 CNT-SUM-TBL            PIC 9(04) VALUE ZERO.
       01  WORK-AREA.
           05 IX                     PIC 9(05) VALUE ZERO.
           05 JX                     PIC 9(05) VALUE ZERO.
           05 WS-KEY-ARI             PIC X VALUE SPACE.
           05 WS-SEGMENT             PIC X(02) VALUE SPACE.
           05 WS-SUM-SEG             PIC X(03) VALUE SPACE.
           05 WS-ERR-ID              PIC 9(09) VALUE ZERO.
       01  KEY-TABLE.
           05 KEY-ENT OCCURS 5000 TIMES.
              10 KT-CIF-NO           PIC X(20).
       01  CODE-TABLE.
           05 CODE-ENT OCCURS 200 TIMES.
              10 CT-CODE-KBN         PIC X(06).
              10 CT-CODE-VALUE       PIC X(10).
       01  SUM-TABLE.
           05 SUM-ENT OCCURS 400 TIMES.
              10 ST-USED             PIC X.
              10 ST-SEGMENT          PIC X(03).
              10 ST-CUSTOMER-CNT     PIC 9(09).
              10 ST-ACTIVE-CNT       PIC 9(09).
              10 ST-STOP-CNT         PIC 9(09).
              10 ST-NEW-CNT          PIC 9(09).
       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE 0 TO RETURN-CODE
           PERFORM 1000-OPEN
           IF NOT HARD-ERR
              PERFORM 2000-LOAD-CODE
              PERFORM 2100-LOAD-KEY
              PERFORM 3000-PROCESS-CIF
              PERFORM 4000-WRITE-SUMMARY
           END-IF
           PERFORM 9000-CLOSE
           IF HARD-ERR
              MOVE 12 TO RETURN-CODE
           ELSE
              MOVE 0 TO RETURN-CODE
              DISPLAY "CG230B 正常終了 CIF=" CNT-CIF-READ
                      " ERR=" CNT-ERR-WRITE
           END-IF
           GOBACK.
       1000-OPEN.
           OPEN INPUT CMCIFF CMATTF CMKEYF CGCODF
                OUTPUT CGSUMF CKERRF
           IF FS-CMCIFF NOT = "00"
              DISPLAY "CMCIFF オープン失敗 ST=" FS-CMCIFF
              SET HARD-ERR TO TRUE
           END-IF
           IF FS-CMATTF NOT = "00"
              DISPLAY "CMATTF オープン失敗 ST=" FS-CMATTF
              SET HARD-ERR TO TRUE
           END-IF
           IF FS-CMKEYF NOT = "00"
              DISPLAY "CMKEYF オープン失敗 ST=" FS-CMKEYF
              SET HARD-ERR TO TRUE
           END-IF
           IF FS-CGCODF NOT = "00"
              DISPLAY "CGCODF オープン失敗 ST=" FS-CGCODF
              SET HARD-ERR TO TRUE
           END-IF
           IF FS-CGSUMF NOT = "00"
              DISPLAY "CGSUMF オープン失敗 ST=" FS-CGSUMF
              SET HARD-ERR TO TRUE
           END-IF
           IF FS-CKERRF NOT = "00"
              DISPLAY "CKERRF オープン失敗 ST=" FS-CKERRF
              SET HARD-ERR TO TRUE
           END-IF.
       2000-LOAD-CODE.
           PERFORM UNTIL CODE-EOF OR HARD-ERR
              READ CGCODF NEXT RECORD
                 AT END
                    SET CODE-EOF TO TRUE
                 NOT AT END
                    ADD 1 TO CNT-CODE-READ
                    IF (GC-CODE-KBN = CN-STATUS-KBN
                        OR GC-CODE-KBN = CN-SEGMENT-KBN)
                       AND GC-VALID-FROM-DT <= CN-BASE-DT
                       AND GC-VALID-TO-DT >= CN-BASE-DT
                       IF CNT-CODE-TBL < 200
                          ADD 1 TO CNT-CODE-TBL
                          MOVE GC-CODE-KBN
                            TO CT-CODE-KBN(CNT-CODE-TBL)
                          MOVE GC-CODE-VALUE
                            TO CT-CODE-VALUE(CNT-CODE-TBL)
                       ELSE
                          DISPLAY "コード表件数超過"
                          SET HARD-ERR TO TRUE
                       END-IF
                    END-IF
              END-READ
              IF FS-CGCODF NOT = "00" AND FS-CGCODF NOT = "10"
                 DISPLAY "CGCODF 読込失敗 ST=" FS-CGCODF
                 SET HARD-ERR TO TRUE
              END-IF
           END-PERFORM.
       2100-LOAD-KEY.
           PERFORM UNTIL KEY-EOF OR HARD-ERR
              READ CMKEYF
                 AT END
                    SET KEY-EOF TO TRUE
                 NOT AT END
                    ADD 1 TO CNT-KEY-READ
                    IF CK-KEY-STATUS-KBN = CN-STATUS-ACTIVE
                       IF CNT-KEY-TBL < 5000
                          ADD 1 TO CNT-KEY-TBL
                          MOVE CK-CIF-NO TO KT-CIF-NO(CNT-KEY-TBL)
                       ELSE
                          DISPLAY "統合キー表件数超過"
                          SET HARD-ERR TO TRUE
                       END-IF
                    END-IF
              END-READ
              IF FS-CMKEYF NOT = "00" AND FS-CMKEYF NOT = "10"
                 DISPLAY "CMKEYF 読込失敗 ST=" FS-CMKEYF
                 SET HARD-ERR TO TRUE
              END-IF
           END-PERFORM.
       3000-PROCESS-CIF.
           PERFORM UNTIL CIF-EOF OR HARD-ERR
              READ CMCIFF
                 AT END
                    SET CIF-EOF TO TRUE
                 NOT AT END
                    ADD 1 TO CNT-CIF-READ
                    PERFORM 3100-VALIDATE-CIF
              END-READ
              IF FS-CMCIFF NOT = "00" AND FS-CMCIFF NOT = "10"
                 DISPLAY "CMCIFF 読込失敗 ST=" FS-CMCIFF
                 SET HARD-ERR TO TRUE
              END-IF
           END-PERFORM.
       3100-VALIDATE-CIF.
           MOVE "N" TO SW-FOUND
           PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > CNT-CODE-TBL
              IF CT-CODE-KBN(IX) = CN-STATUS-KBN
                 AND CT-CODE-VALUE(IX)(1:2) = CF-CIF-STATUS-KBN
                 SET FOUND TO TRUE
              END-IF
           END-PERFORM
           IF NOT FOUND
              MOVE CN-ERR-CODE TO ER-ERROR-CD
              PERFORM 8000-WRITE-ERROR
           ELSE
              EVALUATE CF-CIF-STATUS-KBN
                 WHEN CN-STATUS-ACTIVE
                    PERFORM 3200-READ-ATTR
                 WHEN CN-STATUS-EXCLUDE
                    CONTINUE
                 WHEN CN-STATUS-INVALID
                    PERFORM 3200-READ-ATTR
                 WHEN OTHER
                    MOVE CN-ERR-CODE TO ER-ERROR-CD
                    PERFORM 8000-WRITE-ERROR
              END-EVALUATE
           END-IF.
       3200-READ-ATTR.
           MOVE CF-CIF-NO TO CA-CIF-NO
           READ CMATTF KEY IS CA-CIF-NO
              INVALID KEY
                 MOVE CN-ERR-ATTR TO ER-ERROR-CD
                 PERFORM 8000-WRITE-ERROR
              NOT INVALID KEY
                 IF CA-ATTR-STATUS-KBN = CN-STATUS-ACTIVE
                    MOVE CA-ADDR-CD(1:2) TO WS-SEGMENT
                    PERFORM 3300-VALIDATE-SEGMENT
                 ELSE
                    MOVE CN-ERR-ATTR TO ER-ERROR-CD
                    PERFORM 8000-WRITE-ERROR
                 END-IF
           END-READ
           IF FS-CMATTF NOT = "00" AND FS-CMATTF NOT = "23"
              DISPLAY "CMATTF 読込失敗 ST=" FS-CMATTF
              SET HARD-ERR TO TRUE
           END-IF.
       3300-VALIDATE-SEGMENT.
           MOVE "N" TO SW-FOUND
           PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > CNT-CODE-TBL
              IF CT-CODE-KBN(IX) = CN-SEGMENT-KBN
                 AND CT-CODE-VALUE(IX)(1:2) = WS-SEGMENT
                 SET FOUND TO TRUE
              END-IF
           END-PERFORM
           IF FOUND
              PERFORM 3400-CHECK-KEY
              PERFORM 3500-ADD-SUM
           ELSE
              MOVE CN-ERR-SEG TO ER-ERROR-CD
              PERFORM 8000-WRITE-ERROR
           END-IF.
       3400-CHECK-KEY.
           MOVE "0" TO WS-KEY-ARI
           PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > CNT-KEY-TBL
              IF KT-CIF-NO(IX) = CF-CIF-NO
                 MOVE "1" TO WS-KEY-ARI
                 MOVE CNT-KEY-TBL TO IX
              END-IF
           END-PERFORM
           STRING WS-SEGMENT WS-KEY-ARI
             DELIMITED BY SIZE INTO WS-SUM-SEG.
       3500-ADD-SUM.
           MOVE ZERO TO JX
           PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > CNT-SUM-TBL
              IF ST-USED(IX) = "Y" AND ST-SEGMENT(IX) = WS-SUM-SEG
                 MOVE IX TO JX
              END-IF
           END-PERFORM
           IF JX = ZERO
              IF CNT-SUM-TBL < 400
                 ADD 1 TO CNT-SUM-TBL
                 MOVE CNT-SUM-TBL TO JX
                 MOVE "Y" TO ST-USED(JX)
                 MOVE WS-SUM-SEG TO ST-SEGMENT(JX)
                 MOVE ZERO TO ST-CUSTOMER-CNT(JX)
                              ST-ACTIVE-CNT(JX)
                              ST-STOP-CNT(JX)
                              ST-NEW-CNT(JX)
              ELSE
                 DISPLAY "集計表件数超過"
                 SET HARD-ERR TO TRUE
              END-IF
           END-IF
           IF NOT HARD-ERR
              ADD 1 TO ST-CUSTOMER-CNT(JX)
              EVALUATE CF-CIF-STATUS-KBN
                 WHEN CN-STATUS-ACTIVE
                    ADD 1 TO ST-ACTIVE-CNT(JX)
                    IF CF-BIRTH-DT(1:6) = CN-BASE-YYYYMM
                       ADD 1 TO ST-NEW-CNT(JX)
                    END-IF
                 WHEN CN-STATUS-INVALID
                    ADD 1 TO ST-STOP-CNT(JX)
              END-EVALUATE
           END-IF.
       4000-WRITE-SUMMARY.
           PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > CNT-SUM-TBL
              INITIALIZE CGSUMF-REC
              MOVE CN-BASE-YYYYMM TO GS-SUMMARY-YYYYMM
              MOVE ST-SEGMENT(IX) TO GS-SEGMENT-KBN
              MOVE ST-CUSTOMER-CNT(IX) TO GS-CUSTOMER-CNT
              MOVE ST-ACTIVE-CNT(IX) TO GS-ACTIVE-CNT
              MOVE ST-STOP-CNT(IX) TO GS-STOP-CNT
              MOVE ST-NEW-CNT(IX) TO GS-NEW-CNT
              WRITE CGSUMF-REC
              IF FS-CGSUMF = "00"
                 ADD 1 TO CNT-SUM-WRITE
              ELSE
                 DISPLAY "CGSUMF 書込失敗 ST=" FS-CGSUMF
                 SET HARD-ERR TO TRUE
                 MOVE CNT-SUM-TBL TO IX
              END-IF
           END-PERFORM.
       8000-WRITE-ERROR.
           ADD 1 TO WS-ERR-ID
           INITIALIZE CKERRF-REC
           MOVE WS-ERR-ID TO ER-ERROR-ID
           MOVE CN-PGM-ID TO ER-SOURCE-PGM-ID
           MOVE CF-CIF-NO TO ER-CIF-NO
           MOVE SPACE TO ER-KEY-ID
           MOVE CN-BASE-DT TO ER-ERROR-DT
           WRITE CKERRF-REC
           IF FS-CKERRF = "00"
              ADD 1 TO CNT-ERR-WRITE
           ELSE
              DISPLAY "CKERRF 書込失敗 ST=" FS-CKERRF
              SET HARD-ERR TO TRUE
           END-IF.
       9000-CLOSE.
           CLOSE CMCIFF CMATTF CMKEYF CGCODF CGSUMF CKERRF.
