       IDENTIFICATION DIVISION.
       PROGRAM-ID. CB114B.
      *============================================================*
      * 変更履歴                                                   *
      * 版数  年月日    担当     概要                              *
      * 1.00  20230110  開発一課 初版作成                          *
      * 1.10  20240401  開発一課 国内ショッピング付与率改定        *
      * 1.20  20251001  開発一課 海外ショッピング係数見直し        *
      *============================================================*
      * 確定済み利用明細から対象外取引を除外しポイントを付与する。 *
      * 対象外理由は監査用ログへ出力する。                         *
      *============================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CDOVSF ASSIGN TO "CDOVSF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS FS-CDOVSF.
           SELECT CDACCF ASSIGN TO "CDACCF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS AC-CARD-NO
               FILE STATUS IS FS-CDACCF.
           SELECT CDPNTF ASSIGN TO "CDPNTF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS PN-CARD-NO
               FILE STATUS IS FS-CDPNTF.
           SELECT CDLOGF ASSIGN TO "CDLOGF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS FS-CDLOGF.

       DATA DIVISION.
       FILE SECTION.
       FD  CDOVSF.
           COPY CDOVSFC.
       FD  CDACCF.
           COPY CDACCFC.
       FD  CDPNTF.
           COPY CDPNTFC.
       FD  CDLOGF.
           COPY CDLOGFC.

       WORKING-STORAGE SECTION.
       01  FILE-STATUS-AREA.
           05  FS-CDOVSF              PIC XX VALUE SPACE.
           05  FS-CDACCF              PIC XX VALUE SPACE.
           05  FS-CDPNTF              PIC XX VALUE SPACE.
           05  FS-CDLOGF              PIC XX VALUE SPACE.

       01  SWITCH-AREA.
           05  EOF-CDOVSF             PIC X VALUE "N".
               88  CDOVSF-END               VALUE "Y".
           05  ABEND-SW               PIC X VALUE "N".
               88  ABEND-ON                 VALUE "Y".
           05  POINT-TAISHO-SW        PIC X VALUE "N".
               88  POINT-TAISHO             VALUE "Y".

       01  WORK-AREA.
           05  WK-PROGRAM-ID          PIC X(08) VALUE "CB114B".
           05  WK-BATCH-DT            PIC 9(08) VALUE ZERO.
           05  WK-LOG-SEQ             PIC 9(10) VALUE ZERO.
           05  WK-BASE-AMT            PIC S9(13) COMP-3 VALUE ZERO.
           05  WK-BASE-POINT          PIC S9(09) COMP-3 VALUE ZERO.
           05  WK-CAMPAIGN-BAIRITU    PIC 9(03) COMP-3 VALUE ZERO.
           05  WK-EARN-POINT          PIC S9(09) COMP-3 VALUE ZERO.
           05  WK-READ-CNT            PIC 9(09) VALUE ZERO.
           05  WK-ADD-CNT             PIC 9(09) VALUE ZERO.
           05  WK-SKIP-CNT            PIC 9(09) VALUE ZERO.
           05  WK-ERR-CNT             PIC 9(09) VALUE ZERO.

       01  DISPLAY-AREA.
           05  DSP-TEXT               PIC X(40) VALUE SPACE.
           05  DSP-STATUS             PIC XX VALUE SPACE.

       01  DETAIL-CODE.
           05  DC-MIKAKUTEI           PIC X(04) VALUE "D001".
           05  DC-TORIKESHI           PIC X(04) VALUE "D002".
           05  DC-HITAISHO            PIC X(04) VALUE "D003".
           05  DC-KAIIN-NASHI         PIC X(04) VALUE "D004".
           05  DC-ENTAI               PIC X(04) VALUE "D005".
           05  DC-PNT-NASHI           PIC X(04) VALUE "D006".
           05  DC-KINGAKU-FUSEI       PIC X(04) VALUE "D007".

       PROCEDURE DIVISION.
       MAIN-RTN.
           PERFORM INIT-RTN.
           IF NOT ABEND-ON
               PERFORM UNTIL CDOVSF-END OR ABEND-ON
                   PERFORM READ-CDOVSF-RTN
                   IF NOT CDOVSF-END AND NOT ABEND-ON
                       PERFORM PROCESS-RTN
                   END-IF
               END-PERFORM
           END-IF.
           PERFORM END-RTN.
           GOBACK.

       INIT-RTN.
           MOVE 0 TO RETURN-CODE.
           ACCEPT WK-BATCH-DT FROM DATE YYYYMMDD.
           OPEN INPUT CDOVSF.
           IF FS-CDOVSF NOT = "00"
               MOVE "CDOVSF オープン失敗 ST=" TO DSP-TEXT
               MOVE FS-CDOVSF TO DSP-STATUS
               DISPLAY DSP-TEXT DSP-STATUS
               SET ABEND-ON TO TRUE
               MOVE 12 TO RETURN-CODE
           END-IF.
           IF NOT ABEND-ON
               OPEN INPUT CDACCF
               IF FS-CDACCF NOT = "00"
                   MOVE "CDACCF オープン失敗 ST=" TO DSP-TEXT
                   MOVE FS-CDACCF TO DSP-STATUS
                   DISPLAY DSP-TEXT DSP-STATUS
                   SET ABEND-ON TO TRUE
                   MOVE 12 TO RETURN-CODE
               END-IF
           END-IF.
           IF NOT ABEND-ON
               OPEN I-O CDPNTF
               IF FS-CDPNTF NOT = "00"
                   MOVE "CDPNTF オープン失敗 ST=" TO DSP-TEXT
                   MOVE FS-CDPNTF TO DSP-STATUS
                   DISPLAY DSP-TEXT DSP-STATUS
                   SET ABEND-ON TO TRUE
                   MOVE 12 TO RETURN-CODE
               END-IF
           END-IF.
           IF NOT ABEND-ON
               OPEN OUTPUT CDLOGF
               IF FS-CDLOGF NOT = "00"
                   MOVE "CDLOGF オープン失敗 ST=" TO DSP-TEXT
                   MOVE FS-CDLOGF TO DSP-STATUS
                   DISPLAY DSP-TEXT DSP-STATUS
                   SET ABEND-ON TO TRUE
                   MOVE 12 TO RETURN-CODE
               END-IF
           END-IF.

       READ-CDOVSF-RTN.
           READ CDOVSF
               AT END
                   SET CDOVSF-END TO TRUE
               NOT AT END
                   ADD 1 TO WK-READ-CNT
           END-READ.
           IF FS-CDOVSF NOT = "00" AND FS-CDOVSF NOT = "10"
               MOVE "CDOVSF 読込失敗 ST=" TO DSP-TEXT
               MOVE FS-CDOVSF TO DSP-STATUS
               DISPLAY DSP-TEXT DSP-STATUS
               SET ABEND-ON TO TRUE
               MOVE 12 TO RETURN-CODE
           END-IF.

       PROCESS-RTN.
           MOVE "N" TO POINT-TAISHO-SW.
           MOVE ZERO TO WK-BASE-AMT WK-BASE-POINT WK-EARN-POINT.
           MOVE ZERO TO WK-CAMPAIGN-BAIRITU.

           IF OV-SETL-KBN NOT = "D"
               PERFORM WRITE-LOG-MIKAKUTEI
               ADD 1 TO WK-SKIP-CNT
               EXIT PARAGRAPH
           END-IF.

           IF OV-SETL-AMT < ZERO
               PERFORM WRITE-LOG-TORIKESHI
               ADD 1 TO WK-SKIP-CNT
               EXIT PARAGRAPH
           END-IF.

           IF OV-SETL-AMT = ZERO
               PERFORM WRITE-LOG-KINGAKU
               ADD 1 TO WK-SKIP-CNT
               EXIT PARAGRAPH
           END-IF.

           IF OV-TXN-KBN NOT = "P1" AND OV-TXN-KBN NOT = "P2"
               PERFORM WRITE-LOG-HITAISHO
               ADD 1 TO WK-SKIP-CNT
               EXIT PARAGRAPH
           END-IF.

           MOVE OV-CARD-NO TO AC-CARD-NO.
           READ CDACCF
               INVALID KEY
                   PERFORM WRITE-LOG-KAIIN-NASHI
                   ADD 1 TO WK-SKIP-CNT
                   EXIT PARAGRAPH
           END-READ.
           IF FS-CDACCF NOT = "00" AND FS-CDACCF NOT = "23"
               MOVE "CDACCF 読込失敗 ST=" TO DSP-TEXT
               MOVE FS-CDACCF TO DSP-STATUS
               DISPLAY DSP-TEXT DSP-STATUS
               SET ABEND-ON TO TRUE
               MOVE 12 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF.

           IF AC-DELAY-KBN = "1"
               PERFORM WRITE-LOG-ENTAI
               ADD 1 TO WK-SKIP-CNT
               EXIT PARAGRAPH
           END-IF.

           MOVE OV-CARD-NO TO PN-CARD-NO.
           READ CDPNTF
               INVALID KEY
                   PERFORM WRITE-LOG-PNT-NASHI
                   ADD 1 TO WK-SKIP-CNT
                   EXIT PARAGRAPH
           END-READ.
           IF FS-CDPNTF NOT = "00" AND FS-CDPNTF NOT = "23"
               MOVE "CDPNTF 読込失敗 ST=" TO DSP-TEXT
               MOVE FS-CDPNTF TO DSP-STATUS
               DISPLAY DSP-TEXT DSP-STATUS
               SET ABEND-ON TO TRUE
               MOVE 12 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF.

           SET POINT-TAISHO TO TRUE.
           MOVE OV-SETL-AMT TO WK-BASE-AMT.
           COMPUTE WK-BASE-POINT = WK-BASE-AMT / 100.

           IF OV-TXN-KBN = "P2"
               MOVE 200 TO WK-CAMPAIGN-BAIRITU
           ELSE
               MOVE 120 TO WK-CAMPAIGN-BAIRITU
           END-IF.

           COMPUTE WK-EARN-POINT =
               WK-BASE-POINT * WK-CAMPAIGN-BAIRITU / 100.

           ADD WK-EARN-POINT TO PN-POINT-BAL.
           MOVE WK-BATCH-DT TO PN-LAST-EARN-DT.
           MOVE WK-PROGRAM-ID TO PN-PROGRAM-ID.

           REWRITE CDPNTF-REC.
           IF FS-CDPNTF NOT = "00"
               MOVE "CDPNTF 更新失敗 ST=" TO DSP-TEXT
               MOVE FS-CDPNTF TO DSP-STATUS
               DISPLAY DSP-TEXT DSP-STATUS
               SET ABEND-ON TO TRUE
               MOVE 12 TO RETURN-CODE
           ELSE
               ADD 1 TO WK-ADD-CNT
           END-IF.

       WRITE-LOG-MIKAKUTEI.
           MOVE DC-MIKAKUTEI TO LG-DETAIL-CD.
           PERFORM WRITE-LOG-COMMON.

       WRITE-LOG-TORIKESHI.
           MOVE DC-TORIKESHI TO LG-DETAIL-CD.
           PERFORM WRITE-LOG-COMMON.

       WRITE-LOG-HITAISHO.
           MOVE DC-HITAISHO TO LG-DETAIL-CD.
           PERFORM WRITE-LOG-COMMON.

       WRITE-LOG-KAIIN-NASHI.
           MOVE DC-KAIIN-NASHI TO LG-DETAIL-CD.
           PERFORM WRITE-LOG-COMMON.

       WRITE-LOG-ENTAI.
           MOVE DC-ENTAI TO LG-DETAIL-CD.
           PERFORM WRITE-LOG-COMMON.

       WRITE-LOG-PNT-NASHI.
           MOVE DC-PNT-NASHI TO LG-DETAIL-CD.
           PERFORM WRITE-LOG-COMMON.

       WRITE-LOG-KINGAKU.
           MOVE DC-KINGAKU-FUSEI TO LG-DETAIL-CD.
           PERFORM WRITE-LOG-COMMON.

       WRITE-LOG-COMMON.
           ADD 1 TO WK-LOG-SEQ.
           MOVE WK-LOG-SEQ TO LG-LOG-ID.
           MOVE WK-PROGRAM-ID TO LG-PROGRAM-ID.
           MOVE OV-CARD-NO TO LG-CARD-NO.
           MOVE "EX" TO LG-EVENT-KBN.
           MOVE WK-BATCH-DT TO LG-EVENT-DT.
           WRITE CDLOGF-REC.
           IF FS-CDLOGF NOT = "00"
               MOVE "CDLOGF 書込失敗 ST=" TO DSP-TEXT
               MOVE FS-CDLOGF TO DSP-STATUS
               DISPLAY DSP-TEXT DSP-STATUS
               SET ABEND-ON TO TRUE
               MOVE 12 TO RETURN-CODE
               ADD 1 TO WK-ERR-CNT
           END-IF.

       END-RTN.
           CLOSE CDOVSF CDACCF CDPNTF CDLOGF.
           IF RETURN-CODE = 0
               DISPLAY "CB114B 正常終了"
               DISPLAY "読込件数=" WK-READ-CNT
               DISPLAY "付与件数=" WK-ADD-CNT
               DISPLAY "対象外件数=" WK-SKIP-CNT
           ELSE
               DISPLAY "CB114B 異常終了"
               DISPLAY "読込件数=" WK-READ-CNT
               DISPLAY "付与件数=" WK-ADD-CNT
               DISPLAY "対象外件数=" WK-SKIP-CNT
               DISPLAY "エラー件数=" WK-ERR-CNT
           END-IF.
