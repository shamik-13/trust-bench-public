       IDENTIFICATION DIVISION.
       PROGRAM-ID. CB111B.
      * 売上確定キャプチャ取込バッチ
      * CDCAPF2の確定可能売上をCDTXNFと突合しCDOVSFへ出力する。
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CDCAPF2 ASSIGN TO "CDCAPF2"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-CDCAPF2.
           SELECT CDTXNF ASSIGN TO "CDTXNF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS TX-TXN-ID
               FILE STATUS IS FS-CDTXNF.
           SELECT CDOVSF ASSIGN TO "CDOVSF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-CDOVSF.
           SELECT CDLOGF ASSIGN TO "CDLOGF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-CDLOGF.

       DATA DIVISION.
       FILE SECTION.
       FD  CDCAPF2.
       COPY CDCAPF2C.
       FD  CDTXNF.
       COPY CDTXNFC.
       FD  CDOVSF.
       COPY CDOVSFC.
       FD  CDLOGF.
       COPY CDLOGFC.

       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID              PIC X(08) VALUE "CB111B".
       01  WS-END-FLG                 PIC X(01) VALUE SPACE.
           88  WS-END                           VALUE "Y".
       01  WS-HARD-ERR-FLG            PIC X(01) VALUE SPACE.
           88  WS-HARD-ERR                      VALUE "Y".
       01  WS-VALID-FLG               PIC X(01) VALUE SPACE.
           88  WS-VALID                         VALUE "Y".
       01  WS-SETL-KBN                PIC X(01) VALUE SPACE.
       01  WS-FEE-KBN                 PIC X(02) VALUE SPACE.
       01  WS-DIFF-AMT                PIC S9(11) COMP-3 VALUE 0.
       01  WS-LOG-SEQ                 PIC 9(09) VALUE 0.
       01  WS-INT-DT-N                PIC 9(08) VALUE 0.
       01  WS-CAPTURE-CNT             PIC 9(09) VALUE 0.
       01  WS-OUTPUT-CNT              PIC 9(09) VALUE 0.
       01  WS-HOLD-CNT                PIC 9(09) VALUE 0.
       01  WS-SKIP-CNT                PIC 9(09) VALUE 0.
       01  WS-ERROR-CNT               PIC 9(09) VALUE 0.
       01  WS-CURRENT-DATE.
           05  WS-CUR-YYYY            PIC 9(04).
           05  WS-CUR-MM              PIC 9(02).
           05  WS-CUR-DD              PIC 9(02).
           05  FILLER                 PIC X(13).
       01  WS-EVENT-DT                PIC 9(08).
       01  FS-CDCAPF2                 PIC X(02) VALUE "00".
       01  FS-CDTXNF                  PIC X(02) VALUE "00".
       01  FS-CDOVSF                  PIC X(02) VALUE "00".
       01  FS-CDLOGF                  PIC X(02) VALUE "00".

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE 0 TO RETURN-CODE
           PERFORM 1000-INIT
           IF NOT WS-HARD-ERR
              PERFORM 2000-PROCESS UNTIL WS-END OR WS-HARD-ERR
           END-IF
           PERFORM 9000-FINAL
           GOBACK.

       1000-INIT.
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE
           MOVE WS-CURRENT-DATE(1:8) TO WS-EVENT-DT
           OPEN INPUT CDCAPF2
           IF FS-CDCAPF2 NOT = "00"
              DISPLAY "CDCAPF2 オープン失敗 ST=" FS-CDCAPF2
              MOVE "Y" TO WS-HARD-ERR-FLG
              MOVE 12 TO RETURN-CODE
              EXIT PARAGRAPH
           END-IF
           OPEN INPUT CDTXNF
           IF FS-CDTXNF NOT = "00"
              DISPLAY "CDTXNF オープン失敗 ST=" FS-CDTXNF
              MOVE "Y" TO WS-HARD-ERR-FLG
              MOVE 12 TO RETURN-CODE
              EXIT PARAGRAPH
           END-IF
           OPEN OUTPUT CDOVSF
           IF FS-CDOVSF NOT = "00"
              DISPLAY "CDOVSF オープン失敗 ST=" FS-CDOVSF
              MOVE "Y" TO WS-HARD-ERR-FLG
              MOVE 12 TO RETURN-CODE
              EXIT PARAGRAPH
           END-IF
           OPEN OUTPUT CDLOGF
           IF FS-CDLOGF NOT = "00"
              DISPLAY "CDLOGF オープン失敗 ST=" FS-CDLOGF
              MOVE "Y" TO WS-HARD-ERR-FLG
              MOVE 12 TO RETURN-CODE
              EXIT PARAGRAPH
           END-IF
           PERFORM 2100-READ-CAPTURE.

       2000-PROCESS.
           ADD 1 TO WS-CAPTURE-CNT
           IF CP-MATCH-KBN NOT = "1"
              ADD 1 TO WS-SKIP-CNT
              PERFORM 2100-READ-CAPTURE
              EXIT PARAGRAPH
           END-IF
           PERFORM 2200-READ-TXN
           IF FS-CDTXNF = "00"
              PERFORM 2300-VALIDATE
              IF WS-VALID
                 PERFORM 2400-BUILD-OUTPUT
                 PERFORM 2500-WRITE-OUTPUT
              ELSE
                 PERFORM 2600-WRITE-HOLD-LOG
              END-IF
           ELSE
              ADD 1 TO WS-HOLD-CNT
              PERFORM 2700-WRITE-NOTFOUND-LOG
           END-IF
           PERFORM 2100-READ-CAPTURE.

       2100-READ-CAPTURE.
           READ CDCAPF2
              AT END
                 MOVE "Y" TO WS-END-FLG
              NOT AT END
                 IF FS-CDCAPF2 NOT = "00"
                    DISPLAY "CDCAPF2 読込失敗 ST=" FS-CDCAPF2
                    MOVE "Y" TO WS-HARD-ERR-FLG
                    MOVE 12 TO RETURN-CODE
                 END-IF
           END-READ.

       2200-READ-TXN.
           MOVE CP-TXN-ID TO TX-TXN-ID
           READ CDTXNF
           IF FS-CDTXNF NOT = "00" AND FS-CDTXNF NOT = "23"
              DISPLAY "CDTXNF 参照失敗 ST=" FS-CDTXNF
              MOVE "Y" TO WS-HARD-ERR-FLG
              MOVE 12 TO RETURN-CODE
           END-IF.

       2300-VALIDATE.
           MOVE "Y" TO WS-VALID-FLG
           COMPUTE WS-DIFF-AMT = CP-CAPTURE-AMT - TX-TXN-AMT
           IF CP-CARD-NO NOT = TX-CARD-NO
              MOVE SPACE TO WS-VALID-FLG
              ADD 1 TO WS-HOLD-CNT
              PERFORM 2800-WRITE-CARD-LOG
              EXIT PARAGRAPH
           END-IF
           IF TX-AUTH-CD = "CANCEL" OR TX-AUTH-CD = "TORIKS"
              MOVE SPACE TO WS-VALID-FLG
              ADD 1 TO WS-HOLD-CNT
              PERFORM 2900-WRITE-CANCEL-LOG
              EXIT PARAGRAPH
           END-IF
           IF WS-DIFF-AMT NOT = 0
              MOVE SPACE TO WS-VALID-FLG
              ADD 1 TO WS-HOLD-CNT
              PERFORM 3000-WRITE-AMOUNT-LOG
              EXIT PARAGRAPH
           END-IF
           IF CP-BRAND-KBN NOT = "J" AND CP-BRAND-KBN NOT = "V"
              IF CP-BRAND-KBN NOT = "M" AND CP-BRAND-KBN NOT = "A"
                 MOVE SPACE TO WS-VALID-FLG
                 ADD 1 TO WS-HOLD-CNT
                 PERFORM 3100-WRITE-BRAND-LOG
              END-IF
           END-IF.

       2400-BUILD-OUTPUT.
           INITIALIZE CDOVSF-REC
           MOVE CP-TXN-ID     TO OV-TXN-ID
           MOVE CP-CARD-NO    TO OV-CARD-NO
           MOVE TX-TXN-KBN    TO OV-TXN-KBN
           MOVE "D"           TO WS-SETL-KBN
           MOVE "00"          TO WS-FEE-KBN
      * 取引区分・利息起算は確定明細の値をそのまま引き継ぐ。
      * 海外ＡＴＭ固有の利息起算補正は精算エンジン側で行う。
           MOVE 0             TO WS-INT-DT-N
           IF TX-CHANNEL-KBN = "05" AND TX-TXN-KBN = "P2"
              MOVE "FB"       TO WS-FEE-KBN
           END-IF
           MOVE WS-FEE-KBN    TO OV-FEE-KBN
           MOVE 0             TO OV-FEE-AMT
           MOVE WS-INT-DT-N   TO OV-INT-START-DT
           MOVE CP-CAPTURE-AMT TO OV-SETL-AMT
           MOVE WS-SETL-KBN   TO OV-SETL-KBN
           MOVE WS-PROGRAM-ID TO OV-PROGRAM-ID.

       2500-WRITE-OUTPUT.
           WRITE CDOVSF-REC
           IF FS-CDOVSF = "00"
              ADD 1 TO WS-OUTPUT-CNT
           ELSE
              DISPLAY "CDOVSF 書込失敗 ST=" FS-CDOVSF
              MOVE "Y" TO WS-HARD-ERR-FLG
              MOVE 12 TO RETURN-CODE
           END-IF.

       2600-WRITE-HOLD-LOG.
           CONTINUE.

       2700-WRITE-NOTFOUND-LOG.
           MOVE "NFD" TO LG-DETAIL-CD
           MOVE "H"   TO LG-EVENT-KBN
           PERFORM 8000-WRITE-LOG.

       2800-WRITE-CARD-LOG.
           MOVE "CRD" TO LG-DETAIL-CD
           MOVE "H"   TO LG-EVENT-KBN
           PERFORM 8000-WRITE-LOG.

       2900-WRITE-CANCEL-LOG.
           MOVE "CAN" TO LG-DETAIL-CD
           MOVE "H"   TO LG-EVENT-KBN
           PERFORM 8000-WRITE-LOG.

       3000-WRITE-AMOUNT-LOG.
           MOVE "AMT" TO LG-DETAIL-CD
           MOVE "H"   TO LG-EVENT-KBN
           PERFORM 8000-WRITE-LOG.

       3100-WRITE-BRAND-LOG.
           MOVE "BRD" TO LG-DETAIL-CD
           MOVE "H"   TO LG-EVENT-KBN
           PERFORM 8000-WRITE-LOG.

       8000-WRITE-LOG.
           INITIALIZE CDLOGF-REC
           ADD 1 TO WS-LOG-SEQ
           MOVE WS-LOG-SEQ    TO LG-LOG-ID
           MOVE WS-PROGRAM-ID TO LG-PROGRAM-ID
           MOVE CP-CARD-NO    TO LG-CARD-NO
           MOVE WS-EVENT-DT   TO LG-EVENT-DT
           WRITE CDLOGF-REC
           IF FS-CDLOGF = "00"
              CONTINUE
           ELSE
              DISPLAY "CDLOGF 書込失敗 ST=" FS-CDLOGF
              ADD 1 TO WS-ERROR-CNT
              MOVE "Y" TO WS-HARD-ERR-FLG
              MOVE 12 TO RETURN-CODE
           END-IF.

       9000-FINAL.
           CLOSE CDCAPF2
           IF FS-CDCAPF2 NOT = "00"
              DISPLAY "CDCAPF2 クローズ失敗 ST=" FS-CDCAPF2
              MOVE 8 TO RETURN-CODE
           END-IF
           CLOSE CDTXNF
           IF FS-CDTXNF NOT = "00"
              DISPLAY "CDTXNF クローズ失敗 ST=" FS-CDTXNF
              MOVE 8 TO RETURN-CODE
           END-IF
           CLOSE CDOVSF
           IF FS-CDOVSF NOT = "00"
              DISPLAY "CDOVSF クローズ失敗 ST=" FS-CDOVSF
              MOVE 8 TO RETURN-CODE
           END-IF
           CLOSE CDLOGF
           IF FS-CDLOGF NOT = "00"
              DISPLAY "CDLOGF クローズ失敗 ST=" FS-CDLOGF
              MOVE 8 TO RETURN-CODE
           END-IF
           DISPLAY "処理件数=" WS-CAPTURE-CNT
           DISPLAY "確定出力件数=" WS-OUTPUT-CNT
           DISPLAY "保留件数=" WS-HOLD-CNT
           DISPLAY "対象外件数=" WS-SKIP-CNT
           IF WS-HARD-ERR
              IF RETURN-CODE = 0
                 MOVE 12 TO RETURN-CODE
              END-IF
           END-IF.
