       IDENTIFICATION DIVISION.
       PROGRAM-ID. CB115B.
       AUTHOR. MIRAI-CARD-SEISAN.
      ******************************************************************
      * 変更履歴
      * 版数   年月日     担当       概要
      * 01.00  20190401   精算一課   国際ブランド精算一括処理新設
      * 01.10  20210712   精算一課   ブランド仕様改定による返金判定追加
      * 01.20  20230403   基盤更改   金額桁拡張および為替丸め見直し
      * 01.30  20251118   夜間改善   遅延キャプチャ検索順序を改善
      ******************************************************************
      * 為替国際ブランド精算モノリス
      * 国際ブランド到着を取引、キャプチャ、既存精算、為替、
      * 会員口座と突合し、精算明細、請求表示、ログを生成する。
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CDTXNF ASSIGN TO "CDTXNF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS TX-TXN-ID
               FILE STATUS IS FS-CDTXNF.
           SELECT CDCAPF2 ASSIGN TO "CDCAPF2"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS FS-CDCAPF2.
           SELECT CDOVSF ASSIGN TO "CDOVSF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS FS-CDOVSF.
           SELECT CDFXRF ASSIGN TO "CDFXRF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS FX-RATE-DT
               FILE STATUS IS FS-CDFXRF.
           SELECT CDBRDF ASSIGN TO "CDBRDF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS FS-CDBRDF.
           SELECT CDACCF ASSIGN TO "CDACCF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS AC-CARD-NO
               FILE STATUS IS FS-CDACCF.
           SELECT CDSTMF ASSIGN TO "CDSTMF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS FS-CDSTMF.
           SELECT CDLOGF ASSIGN TO "CDLOGF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS FS-CDLOGF.

       DATA DIVISION.
       FILE SECTION.
       FD  CDTXNF.
           COPY CDTXNFC.
       FD  CDCAPF2.
           COPY CDCAPF2C.
       FD  CDOVSF.
           COPY CDOVSFC.
       FD  CDFXRF.
           COPY CDFXRFC.
       FD  CDBRDF.
           COPY CDBRDFC.
       FD  CDACCF.
           COPY CDACCFC.
       FD  CDSTMF.
           COPY CDSTMFC.
       FD  CDLOGF.
           COPY CDLOGFC.

       WORKING-STORAGE SECTION.
       01  FS-AREA.
           05 FS-CDTXNF                 PIC XX VALUE "00".
           05 FS-CDCAPF2                PIC XX VALUE "00".
           05 FS-CDOVSF                 PIC XX VALUE "00".
           05 FS-CDFXRF                 PIC XX VALUE "00".
           05 FS-CDBRDF                 PIC XX VALUE "00".
           05 FS-CDACCF                 PIC XX VALUE "00".
           05 FS-CDSTMF                 PIC XX VALUE "00".
           05 FS-CDLOGF                 PIC XX VALUE "00".

       01  SW-AREA.
           05 SW-CAP-EOF                PIC X VALUE "N".
           05 SW-OV-EOF                 PIC X VALUE "N".
           05 SW-BR-EOF                 PIC X VALUE "N".
           05 SW-HARD-ERR               PIC X VALUE "N".
           05 SW-OV-DUP                 PIC X VALUE "N".
           05 SW-BR-DUP                 PIC X VALUE "N".
           05 SW-VALID                  PIC X VALUE "N".

       01  WK-AREA.
           05 WK-PROGRAM-ID             PIC X(08) VALUE "CB115B".
           05 WK-TODAY                  PIC 9(08) VALUE ZERO.
           05 WK-TIME                   PIC 9(08) VALUE ZERO.
           05 WK-LOG-SEQ                PIC 9(09) VALUE ZERO.
           05 WK-BRAND-SEQ              PIC 9(09) VALUE ZERO.
           05 WK-STMT-SEQ               PIC 9(09) VALUE ZERO.
           05 WK-RATE-WORK              PIC 9(07)V9(6) VALUE ZERO.
           05 WK-MARKUP-WORK            PIC 9(03)V9(6) VALUE ZERO.
           05 WK-RATE-ADD               PIC 9(07)V9(6) VALUE ZERO.
           05 WK-JPY-WORK               PIC S9(13)V99 VALUE ZERO.
           05 WK-FEE-WORK               PIC S9(11)V99 VALUE ZERO.
           05 WK-SETL-WORK              PIC S9(13)V99 VALUE ZERO.
           05 WK-ABS-CAP-AMT            PIC 9(13)V99 VALUE ZERO.
           05 WK-POINT-TARGET           PIC X VALUE "N".
           05 WK-DISP-KBN               PIC X VALUE SPACE.
           05 WK-REASON-CD              PIC X(08) VALUE SPACE.

       01  CNT-AREA.
           05 CNT-CAP-READ              PIC 9(09) VALUE ZERO.
           05 CNT-NORMAL                PIC 9(09) VALUE ZERO.
           05 CNT-HOLD                  PIC 9(09) VALUE ZERO.
           05 CNT-SKIP                  PIC 9(09) VALUE ZERO.
           05 CNT-LOG                   PIC 9(09) VALUE ZERO.

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE 0 TO RETURN-CODE
           ACCEPT WK-TODAY FROM DATE YYYYMMDD
           ACCEPT WK-TIME FROM TIME
           PERFORM 1000-OPEN-FILES
           IF SW-HARD-ERR = "N"
              PERFORM 2000-MAIN-LOOP
           END-IF
           PERFORM 9000-CLOSE-FILES
           IF SW-HARD-ERR = "Y"
              MOVE 8 TO RETURN-CODE
           ELSE
              MOVE 0 TO RETURN-CODE
              DISPLAY "CB115B 正常終了 読込="
                      CNT-CAP-READ " 確定=" CNT-NORMAL
                      " 保留=" CNT-HOLD " 対象外=" CNT-SKIP
           END-IF
           GOBACK.

       1000-OPEN-FILES.
           OPEN INPUT CDTXNF CDCAPF2 CDFXRF CDACCF
           IF FS-CDTXNF NOT = "00"
              DISPLAY "CDTXNF オープン失敗 ST=" FS-CDTXNF
              MOVE "Y" TO SW-HARD-ERR
           END-IF
           IF FS-CDCAPF2 NOT = "00"
              DISPLAY "CDCAPF2 オープン失敗 ST=" FS-CDCAPF2
              MOVE "Y" TO SW-HARD-ERR
           END-IF
           IF FS-CDFXRF NOT = "00"
              DISPLAY "CDFXRF オープン失敗 ST=" FS-CDFXRF
              MOVE "Y" TO SW-HARD-ERR
           END-IF
           IF FS-CDACCF NOT = "00"
              DISPLAY "CDACCF オープン失敗 ST=" FS-CDACCF
              MOVE "Y" TO SW-HARD-ERR
           END-IF

           OPEN I-O CDOVSF CDBRDF
           IF FS-CDOVSF NOT = "00"
              DISPLAY "CDOVSF オープン失敗 ST=" FS-CDOVSF
              MOVE "Y" TO SW-HARD-ERR
           END-IF
           IF FS-CDBRDF NOT = "00"
              DISPLAY "CDBRDF オープン失敗 ST=" FS-CDBRDF
              MOVE "Y" TO SW-HARD-ERR
           END-IF

           OPEN OUTPUT CDSTMF CDLOGF
           IF FS-CDSTMF NOT = "00"
              DISPLAY "CDSTMF オープン失敗 ST=" FS-CDSTMF
              MOVE "Y" TO SW-HARD-ERR
           END-IF
           IF FS-CDLOGF NOT = "00"
              DISPLAY "CDLOGF オープン失敗 ST=" FS-CDLOGF
              MOVE "Y" TO SW-HARD-ERR
           END-IF.

       2000-MAIN-LOOP.
           PERFORM 2100-READ-CAPTURE
           PERFORM UNTIL SW-CAP-EOF = "Y" OR SW-HARD-ERR = "Y"
              ADD 1 TO CNT-CAP-READ
              MOVE "N" TO SW-VALID
              MOVE SPACE TO WK-REASON-CD
              PERFORM 3000-VALIDATE-CAPTURE
              IF SW-VALID = "Y"
                 PERFORM 4000-MATCH-INPUTS
                 IF SW-VALID = "Y"
                    PERFORM 5000-CALCULATE-SETTLEMENT
                    PERFORM 6000-WRITE-OUTPUTS
                 ELSE
                    PERFORM 8200-WRITE-SKIP-LOG
                    ADD 1 TO CNT-SKIP
                 END-IF
              ELSE
                 PERFORM 8200-WRITE-SKIP-LOG
                 ADD 1 TO CNT-SKIP
              END-IF
              PERFORM 2100-READ-CAPTURE
           END-PERFORM.

       2100-READ-CAPTURE.
           READ CDCAPF2
              AT END
                 MOVE "Y" TO SW-CAP-EOF
              NOT AT END
                 IF FS-CDCAPF2 NOT = "00"
                    DISPLAY "CDCAPF2 読込失敗 ST=" FS-CDCAPF2
                    MOVE "Y" TO SW-HARD-ERR
                 END-IF
           END-READ.

       3000-VALIDATE-CAPTURE.
           MOVE "Y" TO SW-VALID
           IF CP-CAPTURE-ID = SPACE
              MOVE "CPCAPID" TO WK-REASON-CD
              MOVE "N" TO SW-VALID
           END-IF
           IF CP-TXN-ID = SPACE
              MOVE "CPTXNID" TO WK-REASON-CD
              MOVE "N" TO SW-VALID
           END-IF
           IF CP-CARD-NO = SPACE
              MOVE "CPCARD" TO WK-REASON-CD
              MOVE "N" TO SW-VALID
           END-IF
           IF CP-CAPTURE-AMT = ZERO
              MOVE "CPAMT" TO WK-REASON-CD
              MOVE "N" TO SW-VALID
           END-IF
           IF CP-BRAND-KBN NOT = "01"
              AND CP-BRAND-KBN NOT = "02"
              AND CP-BRAND-KBN NOT = "03"
              MOVE "CPBRAND" TO WK-REASON-CD
              MOVE "N" TO SW-VALID
           END-IF.

       4000-MATCH-INPUTS.
           MOVE CP-TXN-ID TO TX-TXN-ID
           READ CDTXNF
              INVALID KEY
                 MOVE "TXNONE" TO WK-REASON-CD
                 MOVE "N" TO SW-VALID
              NOT INVALID KEY
                 IF FS-CDTXNF NOT = "00"
                    DISPLAY "CDTXNF 読込失敗 ST=" FS-CDTXNF
                    MOVE "Y" TO SW-HARD-ERR
                 END-IF
           END-READ
           IF SW-VALID = "Y"
              IF TX-CARD-NO NOT = CP-CARD-NO
                 MOVE "TXCARD" TO WK-REASON-CD
                 MOVE "N" TO SW-VALID
              END-IF
           END-IF
           IF SW-VALID = "Y"
              IF TX-TXN-KBN NOT = "P2" AND TX-TXN-KBN NOT = "C2"
                 MOVE "TXKBN" TO WK-REASON-CD
                 MOVE "N" TO SW-VALID
              END-IF
           END-IF
           IF SW-VALID = "Y"
              IF TX-CHANNEL-KBN NOT = "04"
                 AND TX-CHANNEL-KBN NOT = "05"
                 MOVE "TXCH" TO WK-REASON-CD
                 MOVE "N" TO SW-VALID
              END-IF
           END-IF

           IF SW-VALID = "Y"
              MOVE CP-CARD-NO TO AC-CARD-NO
              READ CDACCF
                 INVALID KEY
                    MOVE "ACNONE" TO WK-REASON-CD
                    MOVE "N" TO SW-VALID
                 NOT INVALID KEY
                    IF FS-CDACCF NOT = "00"
                       DISPLAY "CDACCF 読込失敗 ST=" FS-CDACCF
                       MOVE "Y" TO SW-HARD-ERR
                    END-IF
              END-READ
           END-IF
           IF SW-VALID = "Y"
              IF AC-STATUS-KBN NOT = "0"
                 MOVE "ACSTAT" TO WK-REASON-CD
                 MOVE "N" TO SW-VALID
              END-IF
           END-IF

           IF SW-VALID = "Y"
              PERFORM 4100-CHECK-OV-DUP
           END-IF
           IF SW-VALID = "Y"
              PERFORM 4200-CHECK-BR-DUP
           END-IF
           IF SW-VALID = "Y"
              MOVE CP-CAPTURE-DT TO FX-RATE-DT
              READ CDFXRF
                 INVALID KEY
                    MOVE "FXNONE" TO WK-REASON-CD
                    MOVE "N" TO SW-VALID
                 NOT INVALID KEY
                    IF FS-CDFXRF NOT = "00"
                       DISPLAY "CDFXRF 読込失敗 ST=" FS-CDFXRF
                       MOVE "Y" TO SW-HARD-ERR
                    END-IF
              END-READ
           END-IF
           IF SW-VALID = "Y"
              IF FX-BRAND-KBN NOT = CP-BRAND-KBN
                 MOVE "FXBRAND" TO WK-REASON-CD
                 MOVE "N" TO SW-VALID
              END-IF
           END-IF.

       4100-CHECK-OV-DUP.
           MOVE "N" TO SW-OV-DUP
           MOVE "N" TO SW-OV-EOF
           CLOSE CDOVSF
           OPEN I-O CDOVSF
           IF FS-CDOVSF NOT = "00"
              DISPLAY "CDOVSF 再オープン失敗 ST=" FS-CDOVSF
              MOVE "Y" TO SW-HARD-ERR
           END-IF
           PERFORM UNTIL SW-OV-EOF = "Y" OR SW-OV-DUP = "Y"
              READ CDOVSF
                 AT END
                    MOVE "Y" TO SW-OV-EOF
                 NOT AT END
                    IF OV-TXN-ID = CP-TXN-ID
                       MOVE "Y" TO SW-OV-DUP
                    END-IF
              END-READ
           END-PERFORM
           IF SW-OV-DUP = "Y"
              MOVE "OVDUP" TO WK-REASON-CD
              MOVE "N" TO SW-VALID
           END-IF.

       4200-CHECK-BR-DUP.
           MOVE "N" TO SW-BR-DUP
           MOVE "N" TO SW-BR-EOF
           CLOSE CDBRDF
           OPEN I-O CDBRDF
           IF FS-CDBRDF NOT = "00"
              DISPLAY "CDBRDF 再オープン失敗 ST=" FS-CDBRDF
              MOVE "Y" TO SW-HARD-ERR
           END-IF
           PERFORM UNTIL SW-BR-EOF = "Y" OR SW-BR-DUP = "Y"
              READ CDBRDF
                 AT END
                    MOVE "Y" TO SW-BR-EOF
                 NOT AT END
                    IF BR-TXN-ID = CP-TXN-ID
                       MOVE "Y" TO SW-BR-DUP
                    END-IF
              END-READ
           END-PERFORM
           IF SW-BR-DUP = "Y"
              MOVE "BRDUP" TO WK-REASON-CD
              MOVE "N" TO SW-VALID
           END-IF.

       5000-CALCULATE-SETTLEMENT.
           MOVE FX-FX-RATE TO WK-RATE-WORK
           MOVE FX-MARKUP-RATE TO WK-MARKUP-WORK
           COMPUTE WK-RATE-ADD ROUNDED =
              WK-RATE-WORK * (1 + WK-MARKUP-WORK)
           COMPUTE WK-JPY-WORK ROUNDED =
              CP-CAPTURE-AMT * WK-RATE-ADD
           IF CP-CAPTURE-AMT < ZERO
              COMPUTE WK-ABS-CAP-AMT = CP-CAPTURE-AMT * -1
           ELSE
              MOVE CP-CAPTURE-AMT TO WK-ABS-CAP-AMT
           END-IF

           MOVE ZERO TO WK-FEE-WORK
           IF TX-TXN-KBN = "P2"
              COMPUTE WK-FEE-WORK ROUNDED =
                 WK-FEE-WORK + (WK-JPY-WORK * 0.010)
           END-IF
           COMPUTE WK-SETL-WORK ROUNDED = WK-JPY-WORK + WK-FEE-WORK

           MOVE "N" TO WK-POINT-TARGET
           IF TX-TXN-KBN = "P2"
              AND CP-CAPTURE-AMT > ZERO
              AND AC-DELAY-KBN NOT = "1"
              MOVE "Y" TO WK-POINT-TARGET
           END-IF

           IF TX-TXN-KBN = "P2"
              MOVE "S" TO WK-DISP-KBN
           ELSE
              MOVE "K" TO WK-DISP-KBN
           END-IF.

       6000-WRITE-OUTPUTS.
           PERFORM 6100-WRITE-BRAND
           IF SW-HARD-ERR = "N"
              PERFORM 6200-WRITE-OVS
           END-IF
           IF SW-HARD-ERR = "N"
              PERFORM 6300-WRITE-STMT
           END-IF
           IF SW-HARD-ERR = "N"
              PERFORM 8100-WRITE-NORMAL-LOG
              ADD 1 TO CNT-NORMAL
           END-IF.

       6100-WRITE-BRAND.
           ADD 1 TO WK-BRAND-SEQ
           STRING WK-PROGRAM-ID WK-TODAY WK-BRAND-SEQ
              DELIMITED BY SIZE INTO BR-BRAND-SETL-ID
           END-STRING
           MOVE CP-TXN-ID       TO BR-TXN-ID
           MOVE CP-CARD-NO      TO BR-CARD-NO
           MOVE CP-BRAND-KBN    TO BR-BRAND-KBN
           MOVE FX-CCY-CD       TO BR-CCY-CD
           MOVE CP-CAPTURE-AMT  TO BR-BRAND-AMT
           MOVE WK-JPY-WORK     TO BR-JPY-AMT
           MOVE WK-TODAY        TO BR-SETL-DT
           WRITE CDBRDF-REC
           IF FS-CDBRDF NOT = "00"
              DISPLAY "CDBRDF 書込失敗 ST=" FS-CDBRDF
              MOVE "Y" TO SW-HARD-ERR
           END-IF.

       6200-WRITE-OVS.
           MOVE CP-TXN-ID       TO OV-TXN-ID
           MOVE CP-CARD-NO      TO OV-CARD-NO
           MOVE TX-TXN-KBN      TO OV-TXN-KBN
      * ブランド為替手数料の有無のみを区分する。
      * 海外ＡＴＭ事務手数料および利息起算は精算エンジンが付与する。
           IF WK-FEE-WORK NOT = ZERO
              MOVE "FB" TO OV-FEE-KBN
           ELSE
              MOVE "00" TO OV-FEE-KBN
           END-IF
           MOVE WK-FEE-WORK     TO OV-FEE-AMT
           MOVE ZERO            TO OV-INT-START-DT
           MOVE WK-SETL-WORK    TO OV-SETL-AMT
           IF AC-DELAY-KBN = "1"
              MOVE "H" TO OV-SETL-KBN
              ADD 1 TO CNT-HOLD
           ELSE
              MOVE "D" TO OV-SETL-KBN
           END-IF
           MOVE WK-PROGRAM-ID   TO OV-PROGRAM-ID
           WRITE CDOVSF-REC
           IF FS-CDOVSF NOT = "00"
              DISPLAY "CDOVSF 書込失敗 ST=" FS-CDOVSF
              MOVE "Y" TO SW-HARD-ERR
           END-IF.

       6300-WRITE-STMT.
           ADD 1 TO WK-STMT-SEQ
           MOVE CP-CARD-NO      TO ST-CARD-NO
           STRING CP-CARD-NO WK-TODAY WK-STMT-SEQ
              DELIMITED BY SIZE INTO ST-STATEMENT-ID
           END-STRING
           MOVE CP-TXN-ID       TO ST-TXN-ID
           MOVE WK-DISP-KBN     TO ST-LINE-KBN
           MOVE WK-SETL-WORK    TO ST-LINE-AMT
           IF CP-CAPTURE-AMT < ZERO
              MOVE "国際ブランド返金" TO ST-LINE-LABEL
           ELSE
              IF TX-TXN-KBN = "C2"
                 MOVE "海外キャッシング利用" TO ST-LINE-LABEL
              ELSE
                 MOVE "海外ショッピング利用" TO ST-LINE-LABEL
              END-IF
           END-IF
           WRITE CDSTMF-REC
           IF FS-CDSTMF NOT = "00"
              DISPLAY "CDSTMF 書込失敗 ST=" FS-CDSTMF
              MOVE "Y" TO SW-HARD-ERR
           END-IF.

       8100-WRITE-NORMAL-LOG.
           ADD 1 TO WK-LOG-SEQ
           ADD 1 TO CNT-LOG
           STRING WK-TODAY WK-TIME WK-LOG-SEQ
              DELIMITED BY SIZE INTO LG-LOG-ID
           END-STRING
           MOVE WK-PROGRAM-ID TO LG-PROGRAM-ID
           MOVE CP-CARD-NO    TO LG-CARD-NO
           IF WK-POINT-TARGET = "Y"
              MOVE "PT" TO LG-EVENT-KBN
              MOVE "POINTOK" TO LG-DETAIL-CD
           ELSE
              MOVE "ST" TO LG-EVENT-KBN
              MOVE "SETLOK" TO LG-DETAIL-CD
           END-IF
           MOVE WK-TODAY      TO LG-EVENT-DT
           WRITE CDLOGF-REC
           IF FS-CDLOGF NOT = "00"
              DISPLAY "CDLOGF 書込失敗 ST=" FS-CDLOGF
              MOVE "Y" TO SW-HARD-ERR
           END-IF.

       8200-WRITE-SKIP-LOG.
           ADD 1 TO WK-LOG-SEQ
           ADD 1 TO CNT-LOG
           STRING WK-TODAY WK-TIME WK-LOG-SEQ
              DELIMITED BY SIZE INTO LG-LOG-ID
           END-STRING
           MOVE WK-PROGRAM-ID TO LG-PROGRAM-ID
           MOVE CP-CARD-NO    TO LG-CARD-NO
           MOVE "ER"          TO LG-EVENT-KBN
           MOVE WK-TODAY      TO LG-EVENT-DT
           MOVE WK-REASON-CD  TO LG-DETAIL-CD
           WRITE CDLOGF-REC
           IF FS-CDLOGF NOT = "00"
              DISPLAY "CDLOGF 異常ログ書込失敗 ST=" FS-CDLOGF
              MOVE "Y" TO SW-HARD-ERR
           END-IF.

       9000-CLOSE-FILES.
           CLOSE CDTXNF CDCAPF2 CDOVSF CDFXRF CDBRDF
                 CDACCF CDSTMF CDLOGF.
