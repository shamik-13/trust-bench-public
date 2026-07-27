       IDENTIFICATION DIVISION.
       PROGRAM-ID. CG280B.
       AUTHOR. MFG-KYOTSU-KIBAN.
      *
      * 名寄せ結果帳票出力バッチ
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.
      *
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CMRSLF ASSIGN TO "CMRSLF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-CMRSLF.
           SELECT CMKEYF ASSIGN TO "CMKEYF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-CMKEYF.
           SELECT CGCODF ASSIGN TO "CGCODF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS GC-CODE-ID
               FILE STATUS IS FS-CGCODF.
           SELECT CKRPTF ASSIGN TO "CKRPTF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-CKRPTF.
           SELECT CKERRF ASSIGN TO "CKERRF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-CKERRF.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  CMRSLF.
           COPY CMRSLC.
       FD  CMKEYF.
           COPY CMKEYFC.
       FD  CGCODF.
           COPY CGCODC.
       FD  CKRPTF.
           COPY CKRPTC.
       FD  CKERRF.
           COPY CKERRC.
      *
       WORKING-STORAGE SECTION.
       01  WS-PGM-ID                 PIC X(08) VALUE "CG280B".
      *
       01  WS-FILE-STATUS.
           05 FS-CMRSLF              PIC X(02) VALUE SPACES.
           05 FS-CMKEYF              PIC X(02) VALUE SPACES.
           05 FS-CGCODF              PIC X(02) VALUE SPACES.
           05 FS-CKRPTF              PIC X(02) VALUE SPACES.
           05 FS-CKERRF              PIC X(02) VALUE SPACES.
      *
       01  WS-SWITCHES.
           05 WS-CMRSLF-EOF          PIC X VALUE "N".
              88 CMRSLF-EOF                VALUE "Y".
           05 WS-CMKEYF-EOF          PIC X VALUE "N".
              88 CMKEYF-EOF                VALUE "Y".
           05 WS-KEY-FOUND-SW        PIC X VALUE "N".
              88 KEY-FOUND                 VALUE "Y".
           05 WS-CODE-FOUND-SW       PIC X VALUE "N".
              88 CODE-FOUND                VALUE "Y".
           05 WS-HARD-ERROR-SW       PIC X VALUE "N".
              88 HARD-ERROR                VALUE "Y".
      *
       01  WS-CURRENT-DATE.
           05 WS-CUR-YYYY            PIC 9(04).
           05 WS-CUR-MM              PIC 9(02).
           05 WS-CUR-DD              PIC 9(02).
       01  WS-REPORT-YYYYMM          PIC 9(06).
       01  WS-TODAY                  PIC 9(08).
      *
       01  WS-COUNTERS.
           05 WS-READ-RSL-CNT        PIC 9(09) VALUE ZERO.
           05 WS-READ-KEY-CNT        PIC 9(09) VALUE ZERO.
           05 WS-RPT-WRITE-CNT       PIC 9(09) VALUE ZERO.
           05 WS-ERR-WRITE-CNT       PIC 9(09) VALUE ZERO.
           05 WS-LINE-NO             PIC 9(07) VALUE ZERO.
           05 WS-ERROR-SEQ           PIC 9(07) VALUE ZERO.
           05 WS-MATCH-CNT           PIC 9(09) VALUE ZERO.
           05 WS-UNMATCH-CNT         PIC 9(09) VALUE ZERO.
           05 WS-HOLD-CNT            PIC 9(09) VALUE ZERO.
           05 WS-NG-CNT              PIC 9(09) VALUE ZERO.
      *
       01  WS-EDIT-AREA.
           05 WS-REPORT-ID           PIC X(20).
           05 WS-ERROR-ID            PIC X(20).
           05 WS-CODE-ID             PIC X(20).
           05 WS-STATUS-NAME         PIC X(30).
           05 WS-REASON-NAME         PIC X(30).
           05 WS-DIGIT-PRINT-KBN     PIC X.
           05 WS-RESULT-TEXT         PIC X(16).
           05 WS-LINE-TEXT           PIC X(120).
           05 WS-NUM-EDIT            PIC ZZZ,ZZZ,ZZ9.
      *
       01  WS-KEY-TABLE-AREA.
           05 WS-KEY-MAX             PIC 9(05) VALUE 10000.
           05 WS-KEY-CNT             PIC 9(05) VALUE ZERO.
           05 WS-KEY-TABLE OCCURS 10000 TIMES
              INDEXED BY KEY-IDX.
              10 T-KEY-ID            PIC X(20).
              10 T-CIF-NO            PIC X(20).
              10 T-CHECK-DIGIT-CNT   PIC 9(03).
              10 T-KEY-STATUS-KBN    PIC X(02).
      *
       PROCEDURE DIVISION.
       MAIN-SECTION.
           PERFORM 0000-INIT
           IF NOT HARD-ERROR
              PERFORM 1000-LOAD-KEY
              PERFORM 2000-PROCESS-RSL
              PERFORM 8000-WRITE-SUMMARY
           END-IF
           PERFORM 9000-FINAL
           GOBACK.
      *
       0000-INIT.
           MOVE 0 TO RETURN-CODE
           ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD
           MOVE WS-CUR-YYYY TO WS-REPORT-YYYYMM(1:4)
           MOVE WS-CUR-MM   TO WS-REPORT-YYYYMM(5:2)
           MOVE WS-CURRENT-DATE TO WS-TODAY
      *
           OPEN INPUT CMRSLF
           IF FS-CMRSLF NOT = "00"
              DISPLAY "CMRSLF オープン失敗 ST=" FS-CMRSLF
              MOVE "Y" TO WS-HARD-ERROR-SW
              MOVE 12 TO RETURN-CODE
           END-IF
      *
           OPEN INPUT CMKEYF
           IF FS-CMKEYF NOT = "00"
              DISPLAY "CMKEYF オープン失敗 ST=" FS-CMKEYF
              MOVE "Y" TO WS-HARD-ERROR-SW
              MOVE 12 TO RETURN-CODE
           END-IF
      *
           OPEN INPUT CGCODF
           IF FS-CGCODF NOT = "00"
              DISPLAY "CGCODF オープン失敗 ST=" FS-CGCODF
              MOVE "Y" TO WS-HARD-ERROR-SW
              MOVE 12 TO RETURN-CODE
           END-IF
      *
           OPEN OUTPUT CKRPTF
           IF FS-CKRPTF NOT = "00"
              DISPLAY "CKRPTF オープン失敗 ST=" FS-CKRPTF
              MOVE "Y" TO WS-HARD-ERROR-SW
              MOVE 12 TO RETURN-CODE
           END-IF
      *
           OPEN OUTPUT CKERRF
           IF FS-CKERRF NOT = "00"
              DISPLAY "CKERRF オープン失敗 ST=" FS-CKERRF
              MOVE "Y" TO WS-HARD-ERROR-SW
              MOVE 12 TO RETURN-CODE
           END-IF
      *
           IF NOT HARD-ERROR
              PERFORM 8100-WRITE-HEADER
           END-IF.
      *
       1000-LOAD-KEY.
           PERFORM UNTIL CMKEYF-EOF OR HARD-ERROR
              READ CMKEYF
                 AT END
                    MOVE "Y" TO WS-CMKEYF-EOF
                 NOT AT END
                    IF FS-CMKEYF = "00"
                       ADD 1 TO WS-READ-KEY-CNT
                       IF WS-KEY-CNT < WS-KEY-MAX
                          ADD 1 TO WS-KEY-CNT
                          SET KEY-IDX TO WS-KEY-CNT
                          MOVE CK-KEY-ID TO T-KEY-ID(KEY-IDX)
                          MOVE CK-CIF-NO TO T-CIF-NO(KEY-IDX)
                          MOVE CK-CHECK-DIGIT-CNT
                            TO T-CHECK-DIGIT-CNT(KEY-IDX)
                          MOVE CK-KEY-STATUS-KBN
                            TO T-KEY-STATUS-KBN(KEY-IDX)
                       ELSE
                          DISPLAY "CMKEYF 件数上限超過"
                          MOVE "Y" TO WS-HARD-ERROR-SW
                          MOVE 12 TO RETURN-CODE
                       END-IF
                    ELSE
                       DISPLAY "CMKEYF 読込失敗 ST=" FS-CMKEYF
                       MOVE "Y" TO WS-HARD-ERROR-SW
                       MOVE 12 TO RETURN-CODE
                    END-IF
              END-READ
           END-PERFORM.
      *
       2000-PROCESS-RSL.
           PERFORM UNTIL CMRSLF-EOF OR HARD-ERROR
              READ CMRSLF
                 AT END
                    MOVE "Y" TO WS-CMRSLF-EOF
                 NOT AT END
                    IF FS-CMRSLF = "00"
                       ADD 1 TO WS-READ-RSL-CNT
                       PERFORM 2100-EDIT-RESULT
                    ELSE
                       DISPLAY "CMRSLF 読込失敗 ST=" FS-CMRSLF
                       MOVE "Y" TO WS-HARD-ERROR-SW
                       MOVE 12 TO RETURN-CODE
                    END-IF
              END-READ
           END-PERFORM.
      *
       2100-EDIT-RESULT.
           MOVE "N" TO WS-KEY-FOUND-SW
           SET KEY-IDX TO 1
           SEARCH WS-KEY-TABLE
              AT END
                 MOVE "N" TO WS-KEY-FOUND-SW
              WHEN T-KEY-ID(KEY-IDX) = RS-KEY-ID
                 MOVE "Y" TO WS-KEY-FOUND-SW
           END-SEARCH
      *
           EVALUATE RS-RESULT-KBN
              WHEN "01"
                 MOVE "一致" TO WS-RESULT-TEXT
                 ADD 1 TO WS-MATCH-CNT
              WHEN "02"
                 MOVE "不一致" TO WS-RESULT-TEXT
                 ADD 1 TO WS-UNMATCH-CNT
              WHEN "08"
                 MOVE "保留" TO WS-RESULT-TEXT
                 ADD 1 TO WS-HOLD-CNT
              WHEN OTHER
                 MOVE "結果区分不正" TO WS-RESULT-TEXT
                 ADD 1 TO WS-NG-CNT
                 PERFORM 6200-WRITE-ERROR-RESULT
           END-EVALUATE
      *
           PERFORM 3100-GET-REASON-NAME
      *
           IF KEY-FOUND
              PERFORM 3200-GET-STATUS-NAME
              IF T-CHECK-DIGIT-CNT(KEY-IDX) > 0
                 MOVE "1" TO WS-DIGIT-PRINT-KBN
              ELSE
                 MOVE "0" TO WS-DIGIT-PRINT-KBN
              END-IF
              IF T-KEY-STATUS-KBN(KEY-IDX) NOT = "01"
                 PERFORM 6100-WRITE-ERROR-STATUS
              END-IF
           ELSE
              MOVE "キー未登録" TO WS-STATUS-NAME
              MOVE "0" TO WS-DIGIT-PRINT-KBN
              PERFORM 6000-WRITE-ERROR-NOKEY
           END-IF
      *
           PERFORM 5000-WRITE-REPORT.
      *
       3100-GET-REASON-NAME.
           MOVE SPACES TO WS-CODE-ID
           STRING "RSN" RS-REASON-CD
              DELIMITED BY SIZE INTO WS-CODE-ID
           END-STRING
           PERFORM 7000-READ-CODE
           IF CODE-FOUND
              MOVE GC-CODE-NAME TO WS-REASON-NAME
           ELSE
              MOVE "理由コード未登録" TO WS-REASON-NAME
           END-IF.
      *
       3200-GET-STATUS-NAME.
           MOVE SPACES TO WS-CODE-ID
           STRING "KSTS" T-KEY-STATUS-KBN(KEY-IDX)
              DELIMITED BY SIZE INTO WS-CODE-ID
           END-STRING
           PERFORM 7000-READ-CODE
           IF CODE-FOUND
              MOVE GC-CODE-NAME TO WS-STATUS-NAME
           ELSE
              MOVE "キー状態未登録" TO WS-STATUS-NAME
           END-IF.
      *
       5000-WRITE-REPORT.
           ADD 1 TO WS-LINE-NO
           INITIALIZE CKRPTF-REC
           MOVE SPACES TO WS-REPORT-ID
           STRING WS-PGM-ID WS-TODAY WS-LINE-NO
              DELIMITED BY SIZE INTO WS-REPORT-ID
           END-STRING
           MOVE WS-REPORT-ID TO RP-REPORT-ID
           MOVE WS-REPORT-YYYYMM TO RP-REPORT-YYYYMM
           MOVE WS-LINE-NO TO RP-LINE-NO
           MOVE RS-RESULT-KBN TO RP-SECTION-KBN
      *
           MOVE SPACES TO WS-LINE-TEXT
           STRING "結果ID=" RS-RESULT-ID
                  " CIF=" RS-CIF-NO
                  " キー=" RS-KEY-ID
                  " 結果=" WS-RESULT-TEXT
                  " 理由=" RS-REASON-CD
                  " " WS-REASON-NAME
                  " 状態=" WS-STATUS-NAME
                  " 検数印字=" WS-DIGIT-PRINT-KBN
              DELIMITED BY SIZE INTO WS-LINE-TEXT
           END-STRING
           MOVE WS-LINE-TEXT TO RP-REPORT-TEXT
      *
           WRITE CKRPTF-REC
           IF FS-CKRPTF = "00"
              ADD 1 TO WS-RPT-WRITE-CNT
           ELSE
              DISPLAY "CKRPTF 書込失敗 ST=" FS-CKRPTF
              MOVE "Y" TO WS-HARD-ERROR-SW
              MOVE 12 TO RETURN-CODE
           END-IF.
      *
       6000-WRITE-ERROR-NOKEY.
           MOVE "E201" TO ER-ERROR-CD
           PERFORM 6900-WRITE-ERROR.
      *
       6100-WRITE-ERROR-STATUS.
           MOVE "E202" TO ER-ERROR-CD
           PERFORM 6900-WRITE-ERROR.
      *
       6200-WRITE-ERROR-RESULT.
           MOVE "E203" TO ER-ERROR-CD
           PERFORM 6900-WRITE-ERROR.
      *
       6900-WRITE-ERROR.
           ADD 1 TO WS-ERROR-SEQ
           INITIALIZE CKERRF-REC
           MOVE SPACES TO WS-ERROR-ID
           STRING WS-PGM-ID WS-TODAY WS-ERROR-SEQ
              DELIMITED BY SIZE INTO WS-ERROR-ID
           END-STRING
           MOVE WS-ERROR-ID TO ER-ERROR-ID
           MOVE WS-PGM-ID TO ER-SOURCE-PGM-ID
           MOVE RS-CIF-NO TO ER-CIF-NO
           MOVE RS-KEY-ID TO ER-KEY-ID
           MOVE WS-TODAY TO ER-ERROR-DT
           WRITE CKERRF-REC
           IF FS-CKERRF = "00"
              ADD 1 TO WS-ERR-WRITE-CNT
           ELSE
              DISPLAY "CKERRF 書込失敗 ST=" FS-CKERRF
              MOVE "Y" TO WS-HARD-ERROR-SW
              MOVE 12 TO RETURN-CODE
           END-IF.
      *
       7000-READ-CODE.
           MOVE "N" TO WS-CODE-FOUND-SW
           INITIALIZE CGCODF-REC
           MOVE WS-CODE-ID TO GC-CODE-ID
           READ CGCODF KEY IS GC-CODE-ID
              INVALID KEY
                 MOVE "N" TO WS-CODE-FOUND-SW
              NOT INVALID KEY
                 IF FS-CGCODF = "00"
                    IF GC-VALID-FROM-DT <= WS-TODAY
                       AND GC-VALID-TO-DT >= WS-TODAY
                       MOVE "Y" TO WS-CODE-FOUND-SW
                    END-IF
                 ELSE
                    DISPLAY "CGCODF 読込失敗 ST=" FS-CGCODF
                    MOVE "Y" TO WS-HARD-ERROR-SW
                    MOVE 12 TO RETURN-CODE
                 END-IF
           END-READ.
      *
       8000-WRITE-SUMMARY.
           MOVE "1" TO RP-SECTION-KBN
           MOVE WS-READ-RSL-CNT TO WS-NUM-EDIT
           MOVE SPACES TO WS-LINE-TEXT
           STRING "処理件数=" WS-NUM-EDIT
              DELIMITED BY SIZE INTO WS-LINE-TEXT
           END-STRING
           PERFORM 8200-WRITE-CONTROL-LINE
      *
           MOVE WS-MATCH-CNT TO WS-NUM-EDIT
           MOVE SPACES TO WS-LINE-TEXT
           STRING "一致件数=" WS-NUM-EDIT
              DELIMITED BY SIZE INTO WS-LINE-TEXT
           END-STRING
           PERFORM 8200-WRITE-CONTROL-LINE
      *
           MOVE WS-UNMATCH-CNT TO WS-NUM-EDIT
           MOVE SPACES TO WS-LINE-TEXT
           STRING "不一致件数=" WS-NUM-EDIT
              DELIMITED BY SIZE INTO WS-LINE-TEXT
           END-STRING
           PERFORM 8200-WRITE-CONTROL-LINE
      *
           MOVE WS-ERR-WRITE-CNT TO WS-NUM-EDIT
           MOVE SPACES TO WS-LINE-TEXT
           STRING "エラー件数=" WS-NUM-EDIT
              DELIMITED BY SIZE INTO WS-LINE-TEXT
           END-STRING
           PERFORM 8200-WRITE-CONTROL-LINE.
      *
       8100-WRITE-HEADER.
           MOVE SPACES TO WS-LINE-TEXT
           STRING "名寄せ結果帳票 " WS-TODAY
              DELIMITED BY SIZE INTO WS-LINE-TEXT
           END-STRING
           PERFORM 8200-WRITE-CONTROL-LINE.
      *
       8200-WRITE-CONTROL-LINE.
           ADD 1 TO WS-LINE-NO
           INITIALIZE CKRPTF-REC
           MOVE SPACES TO WS-REPORT-ID
           STRING WS-PGM-ID WS-TODAY WS-LINE-NO
              DELIMITED BY SIZE INTO WS-REPORT-ID
           END-STRING
           MOVE WS-REPORT-ID TO RP-REPORT-ID
           MOVE WS-REPORT-YYYYMM TO RP-REPORT-YYYYMM
           MOVE WS-LINE-NO TO RP-LINE-NO
           MOVE "9" TO RP-SECTION-KBN
           MOVE WS-LINE-TEXT TO RP-REPORT-TEXT
           WRITE CKRPTF-REC
           IF FS-CKRPTF = "00"
              ADD 1 TO WS-RPT-WRITE-CNT
           ELSE
              DISPLAY "CKRPTF 制御行書込失敗 ST=" FS-CKRPTF
              MOVE "Y" TO WS-HARD-ERROR-SW
              MOVE 12 TO RETURN-CODE
           END-IF.
      *
       9000-FINAL.
           IF FS-CMRSLF = "00"
              CLOSE CMRSLF
           END-IF
           IF FS-CMKEYF = "00"
              CLOSE CMKEYF
           END-IF
           IF FS-CGCODF = "00"
              CLOSE CGCODF
           END-IF
           IF FS-CKRPTF = "00"
              CLOSE CKRPTF
           END-IF
           IF FS-CKERRF = "00"
              CLOSE CKERRF
           END-IF
      *
           IF HARD-ERROR
              IF RETURN-CODE = 0
                 MOVE 8 TO RETURN-CODE
              END-IF
              DISPLAY "CG280B 異常終了 RC=" RETURN-CODE
           ELSE
              MOVE 0 TO RETURN-CODE
              DISPLAY "CG280B 正常終了"
           END-IF.
