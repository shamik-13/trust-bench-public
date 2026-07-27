       IDENTIFICATION DIVISION.
       PROGRAM-ID. CM130B.
       AUTHOR. CM.
      *================================================================*
      * 顧客属性日次取込バッチ                                         *
      * 顧客異動受付データを日付順に読み、有効なCIFのみ属性へ反映する。*
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CMCIFF ASSIGN TO "CMCIFF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-CMCIFF.
           SELECT CMMOVF ASSIGN TO "CMMOVF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-CMMOVF.
           SELECT CGCODF ASSIGN TO "CGCODF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS GC-CODE-ID
               FILE STATUS IS FS-CGCODF.
           SELECT CMATTF ASSIGN TO "CMATTF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CA-CIF-NO
               FILE STATUS IS FS-CMATTF.
           SELECT CKERRF ASSIGN TO "CKERRF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-CKERRF.

       DATA DIVISION.
       FILE SECTION.
       FD  CMCIFF.
           COPY CMCIFFC.
       FD  CMMOVF.
           COPY CMMOVC.
       FD  CGCODF.
           COPY CGCODC.
       FD  CMATTF.
           COPY CMATTC.
       FD  CKERRF.
           COPY CKERRC.

       WORKING-STORAGE SECTION.
       77  FS-CMCIFF                 PIC XX VALUE SPACE.
       77  FS-CMMOVF                 PIC XX VALUE SPACE.
       77  FS-CGCODF                 PIC XX VALUE SPACE.
       77  FS-CMATTF                 PIC XX VALUE SPACE.
       77  FS-CKERRF                 PIC XX VALUE SPACE.

       77  WK-ABEND-FLG              PIC X VALUE "0".
       77  WK-CIF-EOF                PIC X VALUE "0".
       77  WK-MOV-EOF                PIC X VALUE "0".
       77  WK-COD-EOF                PIC X VALUE "0".
       77  WK-TODAY                  PIC 9(8) VALUE ZERO.
       77  WK-ERR-SEQ                PIC 9(7) VALUE ZERO.
       77  WK-CIF-IDX                PIC 9(5) VALUE ZERO.
       77  WK-COD-IDX                PIC 9(5) VALUE ZERO.
       77  WK-HIT-IDX                PIC 9(5) VALUE ZERO.
       77  WK-CIF-CNT                PIC 9(5) VALUE ZERO.
       77  WK-COD-CNT                PIC 9(5) VALUE ZERO.
       77  WK-READ-CNT               PIC 9(9) VALUE ZERO.
       77  WK-UPD-CNT                PIC 9(9) VALUE ZERO.
       77  WK-ERR-CNT                PIC 9(9) VALUE ZERO.
       77  WK-SKIP-CNT               PIC 9(9) VALUE ZERO.
       77  WK-PREV-DT                PIC 9(8) VALUE ZERO.
       77  WK-CODE-OK                PIC X VALUE "0".
       77  WK-CIF-OK                 PIC X VALUE "0".

       01  WK-DATE-AREA.
           05 WK-DATE-YYYY           PIC 9(4).
           05 WK-DATE-MM             PIC 9(2).
           05 WK-DATE-DD             PIC 9(2).

       01  WK-CIF-TABLE.
           05 WK-CIF-ENT OCCURS 30000 TIMES
              ASCENDING KEY IS TB-CIF-NO
              INDEXED BY IX-CIF.
              10 TB-CIF-NO           PIC X(20).
              10 TB-CIF-STATUS-KBN   PIC X(2).
              10 TB-BIRTH-DT         PIC 9(8).
              10 TB-SEX-KBN          PIC X(1).

       01  WK-CODE-TABLE.
           05 WK-CODE-ENT OCCURS 5000 TIMES
              ASCENDING KEY IS TB-CODE-KEY
              INDEXED BY IX-COD.
              10 TB-CODE-KEY         PIC X(40).
              10 TB-CODE-KBN         PIC X(10).
              10 TB-CODE-VALUE       PIC X(20).
              10 TB-VALID-FROM-DT    PIC 9(8).
              10 TB-VALID-TO-DT      PIC 9(8).

       PROCEDURE DIVISION.
       0000-MAIN.
           ACCEPT WK-DATE-AREA FROM DATE YYYYMMDD
           MOVE WK-DATE-AREA TO WK-TODAY
           MOVE 0 TO RETURN-CODE
           PERFORM 1000-OPEN
           IF WK-ABEND-FLG = "0"
              PERFORM 2000-LOAD-CIF
              PERFORM 2100-LOAD-CODE
              PERFORM 3000-PROCESS-MOVE
           END-IF
           PERFORM 9000-CLOSE
           IF WK-ABEND-FLG = "1"
              MOVE 8 TO RETURN-CODE
           ELSE
              MOVE 0 TO RETURN-CODE
           END-IF
           DISPLAY "CM130B 終了 読込=" WK-READ-CNT
                   " 更新=" WK-UPD-CNT
                   " 監査=" WK-ERR-CNT
                   " 除外=" WK-SKIP-CNT
           GOBACK.

       1000-OPEN.
           OPEN INPUT CMCIFF
           IF FS-CMCIFF NOT = "00"
              DISPLAY "CMCIFF オープン失敗 ST=" FS-CMCIFF
              MOVE "1" TO WK-ABEND-FLG
           END-IF
           OPEN INPUT CMMOVF
           IF FS-CMMOVF NOT = "00"
              DISPLAY "CMMOVF オープン失敗 ST=" FS-CMMOVF
              MOVE "1" TO WK-ABEND-FLG
           END-IF
           OPEN INPUT CGCODF
           IF FS-CGCODF NOT = "00"
              DISPLAY "CGCODF オープン失敗 ST=" FS-CGCODF
              MOVE "1" TO WK-ABEND-FLG
           END-IF
           OPEN I-O CMATTF
           IF FS-CMATTF NOT = "00"
              DISPLAY "CMATTF オープン失敗 ST=" FS-CMATTF
              MOVE "1" TO WK-ABEND-FLG
           END-IF
           OPEN OUTPUT CKERRF
           IF FS-CKERRF NOT = "00"
              DISPLAY "CKERRF オープン失敗 ST=" FS-CKERRF
              MOVE "1" TO WK-ABEND-FLG
           END-IF.

       2000-LOAD-CIF.
           PERFORM UNTIL WK-CIF-EOF = "1" OR WK-ABEND-FLG = "1"
              READ CMCIFF
                 AT END
                    MOVE "1" TO WK-CIF-EOF
                 NOT AT END
                    IF WK-CIF-CNT < 30000
                       ADD 1 TO WK-CIF-CNT
                       MOVE CF-CIF-NO TO TB-CIF-NO(WK-CIF-CNT)
                       MOVE CF-CIF-STATUS-KBN
                         TO TB-CIF-STATUS-KBN(WK-CIF-CNT)
                       MOVE CF-BIRTH-DT TO TB-BIRTH-DT(WK-CIF-CNT)
                       MOVE CF-SEX-KBN TO TB-SEX-KBN(WK-CIF-CNT)
                    ELSE
                       DISPLAY "CIF件数上限超過"
                       MOVE "1" TO WK-ABEND-FLG
                    END-IF
              END-READ
              IF FS-CMCIFF NOT = "00" AND FS-CMCIFF NOT = "10"
                 DISPLAY "CMCIFF 読込失敗 ST=" FS-CMCIFF
                 MOVE "1" TO WK-ABEND-FLG
              END-IF
           END-PERFORM.

       2100-LOAD-CODE.
           PERFORM UNTIL WK-COD-EOF = "1" OR WK-ABEND-FLG = "1"
              READ CGCODF NEXT RECORD
                 AT END
                    MOVE "1" TO WK-COD-EOF
                 NOT AT END
                    IF WK-COD-CNT < 5000
                       ADD 1 TO WK-COD-CNT
                       MOVE GC-CODE-ID TO TB-CODE-KEY(WK-COD-CNT)
                       MOVE GC-CODE-KBN TO TB-CODE-KBN(WK-COD-CNT)
                       MOVE GC-CODE-VALUE TO TB-CODE-VALUE(WK-COD-CNT)
                       MOVE GC-VALID-FROM-DT
                         TO TB-VALID-FROM-DT(WK-COD-CNT)
                       MOVE GC-VALID-TO-DT
                         TO TB-VALID-TO-DT(WK-COD-CNT)
                    ELSE
                       DISPLAY "共通コード件数上限超過"
                       MOVE "1" TO WK-ABEND-FLG
                    END-IF
              END-READ
              IF FS-CGCODF NOT = "00" AND FS-CGCODF NOT = "10"
                 DISPLAY "CGCODF 読込失敗 ST=" FS-CGCODF
                 MOVE "1" TO WK-ABEND-FLG
              END-IF
           END-PERFORM.

       3000-PROCESS-MOVE.
           PERFORM UNTIL WK-MOV-EOF = "1" OR WK-ABEND-FLG = "1"
              READ CMMOVF
                 AT END
                    MOVE "1" TO WK-MOV-EOF
                 NOT AT END
                    ADD 1 TO WK-READ-CNT
                    PERFORM 3100-CHECK-DATE-ORDER
                    PERFORM 3200-EDIT-MOVE
                    IF WK-CIF-OK = "1" AND WK-CODE-OK = "1"
                       PERFORM 3300-UPDATE-ATTR
                    END-IF
              END-READ
              IF FS-CMMOVF NOT = "00" AND FS-CMMOVF NOT = "10"
                 DISPLAY "CMMOVF 読込失敗 ST=" FS-CMMOVF
                 MOVE "1" TO WK-ABEND-FLG
              END-IF
           END-PERFORM.

       3100-CHECK-DATE-ORDER.
           IF WK-PREV-DT > ZERO AND MV-REQUEST-DT < WK-PREV-DT
              PERFORM 8000-WRITE-ERR-DATE
           ELSE
              MOVE MV-REQUEST-DT TO WK-PREV-DT
           END-IF.

       3200-EDIT-MOVE.
           MOVE "0" TO WK-CIF-OK
           MOVE "0" TO WK-CODE-OK
           IF MV-MOVE-STATUS-KBN = "90" OR MV-MOVE-STATUS-KBN = "99"
              PERFORM 8010-WRITE-ERR-CANCEL
              ADD 1 TO WK-SKIP-CNT
           ELSE
              PERFORM 4100-FIND-CIF
              IF WK-CIF-OK = "1"
                 PERFORM 4200-CHECK-MOVE-CODE
                 IF WK-CODE-OK = "1"
                    PERFORM 4300-CHECK-STATUS-CODE
                 END-IF
              END-IF
           END-IF.

       3300-UPDATE-ATTR.
           INITIALIZE CMATTF-REC
           MOVE MV-CIF-NO TO CA-CIF-NO
           READ CMATTF
              INVALID KEY
                 PERFORM 8020-WRITE-ERR-ATTR
              NOT INVALID KEY
                 MOVE MV-REQUEST-DT TO CA-UPDATE-DT
                 MOVE "01" TO CA-ATTR-STATUS-KBN
                 REWRITE CMATTF-REC
                    INVALID KEY
                       DISPLAY "CMATTF 更新失敗 CIF=" MV-CIF-NO
                               " ST=" FS-CMATTF
                       MOVE "1" TO WK-ABEND-FLG
                    NOT INVALID KEY
                       ADD 1 TO WK-UPD-CNT
                 END-REWRITE
           END-READ.

       4100-FIND-CIF.
           MOVE ZERO TO WK-HIT-IDX
           PERFORM VARYING WK-CIF-IDX FROM 1 BY 1
             UNTIL WK-CIF-IDX > WK-CIF-CNT OR WK-HIT-IDX > ZERO
              IF TB-CIF-NO(WK-CIF-IDX) = MV-CIF-NO
                 MOVE WK-CIF-IDX TO WK-HIT-IDX
              END-IF
           END-PERFORM
           IF WK-HIT-IDX = ZERO
              PERFORM 8030-WRITE-ERR-CIF-NOTFOUND
           ELSE
              IF TB-CIF-STATUS-KBN(WK-HIT-IDX) = "01"
                 MOVE "1" TO WK-CIF-OK
              ELSE
                 PERFORM 8040-WRITE-ERR-CIF-STATUS
              END-IF
           END-IF.

       4200-CHECK-MOVE-CODE.
           MOVE "0" TO WK-CODE-OK
           PERFORM VARYING WK-COD-IDX FROM 1 BY 1
             UNTIL WK-COD-IDX > WK-COD-CNT OR WK-CODE-OK = "1"
              IF TB-CODE-KBN(WK-COD-IDX) = "MOVE"
                 AND TB-CODE-VALUE(WK-COD-IDX) = MV-MOVE-KBN
                 AND TB-VALID-FROM-DT(WK-COD-IDX) <= MV-REQUEST-DT
                 AND TB-VALID-TO-DT(WK-COD-IDX) >= MV-REQUEST-DT
                    MOVE "1" TO WK-CODE-OK
              END-IF
           END-PERFORM
           IF WK-CODE-OK NOT = "1"
              PERFORM 8050-WRITE-ERR-MOVE-CODE
           END-IF.

       4300-CHECK-STATUS-CODE.
           MOVE "0" TO WK-CODE-OK
           PERFORM VARYING WK-COD-IDX FROM 1 BY 1
             UNTIL WK-COD-IDX > WK-COD-CNT OR WK-CODE-OK = "1"
              IF TB-CODE-KBN(WK-COD-IDX) = "MVSTS"
                 AND TB-CODE-VALUE(WK-COD-IDX) = MV-MOVE-STATUS-KBN
                 AND TB-VALID-FROM-DT(WK-COD-IDX) <= MV-REQUEST-DT
                 AND TB-VALID-TO-DT(WK-COD-IDX) >= MV-REQUEST-DT
                    MOVE "1" TO WK-CODE-OK
              END-IF
           END-PERFORM
           IF WK-CODE-OK NOT = "1"
              PERFORM 8060-WRITE-ERR-STATUS-CODE
           END-IF.

       8000-WRITE-ERR-DATE.
           PERFORM 8900-INIT-ERR
           MOVE "CM130B" TO ER-SOURCE-PGM-ID
           MOVE MV-CIF-NO TO ER-CIF-NO
           MOVE MV-RECEIPT-NO TO ER-KEY-ID
           MOVE "E001" TO ER-ERROR-CD
           PERFORM 8990-WRITE-ERR.

       8010-WRITE-ERR-CANCEL.
           PERFORM 8900-INIT-ERR
           MOVE "CM130B" TO ER-SOURCE-PGM-ID
           MOVE MV-CIF-NO TO ER-CIF-NO
           MOVE MV-RECEIPT-NO TO ER-KEY-ID
           MOVE "E002" TO ER-ERROR-CD
           PERFORM 8990-WRITE-ERR.

       8020-WRITE-ERR-ATTR.
           PERFORM 8900-INIT-ERR
           MOVE "CM130B" TO ER-SOURCE-PGM-ID
           MOVE MV-CIF-NO TO ER-CIF-NO
           MOVE MV-RECEIPT-NO TO ER-KEY-ID
           MOVE "E003" TO ER-ERROR-CD
           PERFORM 8990-WRITE-ERR.

       8030-WRITE-ERR-CIF-NOTFOUND.
           PERFORM 8900-INIT-ERR
           MOVE "CM130B" TO ER-SOURCE-PGM-ID
           MOVE MV-CIF-NO TO ER-CIF-NO
           MOVE MV-RECEIPT-NO TO ER-KEY-ID
           MOVE "E004" TO ER-ERROR-CD
           PERFORM 8990-WRITE-ERR.

       8040-WRITE-ERR-CIF-STATUS.
           PERFORM 8900-INIT-ERR
           MOVE "CM130B" TO ER-SOURCE-PGM-ID
           MOVE MV-CIF-NO TO ER-CIF-NO
           MOVE MV-RECEIPT-NO TO ER-KEY-ID
           MOVE "E005" TO ER-ERROR-CD
           PERFORM 8990-WRITE-ERR.

       8050-WRITE-ERR-MOVE-CODE.
           PERFORM 8900-INIT-ERR
           MOVE "CM130B" TO ER-SOURCE-PGM-ID
           MOVE MV-CIF-NO TO ER-CIF-NO
           MOVE MV-RECEIPT-NO TO ER-KEY-ID
           MOVE "E006" TO ER-ERROR-CD
           PERFORM 8990-WRITE-ERR.

       8060-WRITE-ERR-STATUS-CODE.
           PERFORM 8900-INIT-ERR
           MOVE "CM130B" TO ER-SOURCE-PGM-ID
           MOVE MV-CIF-NO TO ER-CIF-NO
           MOVE MV-RECEIPT-NO TO ER-KEY-ID
           MOVE "E007" TO ER-ERROR-CD
           PERFORM 8990-WRITE-ERR.

       8900-INIT-ERR.
           INITIALIZE CKERRF-REC
           ADD 1 TO WK-ERR-SEQ
           MOVE WK-ERR-SEQ TO ER-ERROR-ID
           MOVE WK-TODAY TO ER-ERROR-DT.

       8990-WRITE-ERR.
           WRITE CKERRF-REC
           IF FS-CKERRF NOT = "00"
              DISPLAY "CKERRF 書込失敗 ST=" FS-CKERRF
              MOVE "1" TO WK-ABEND-FLG
           ELSE
              ADD 1 TO WK-ERR-CNT
           END-IF.

       9000-CLOSE.
           CLOSE CMCIFF
           IF FS-CMCIFF NOT = "00"
              DISPLAY "CMCIFF クローズ失敗 ST=" FS-CMCIFF
              MOVE "1" TO WK-ABEND-FLG
           END-IF
           CLOSE CMMOVF
           IF FS-CMMOVF NOT = "00"
              DISPLAY "CMMOVF クローズ失敗 ST=" FS-CMMOVF
              MOVE "1" TO WK-ABEND-FLG
           END-IF
           CLOSE CGCODF
           IF FS-CGCODF NOT = "00"
              DISPLAY "CGCODF クローズ失敗 ST=" FS-CGCODF
              MOVE "1" TO WK-ABEND-FLG
           END-IF
           CLOSE CMATTF
           IF FS-CMATTF NOT = "00"
              DISPLAY "CMATTF クローズ失敗 ST=" FS-CMATTF
              MOVE "1" TO WK-ABEND-FLG
           END-IF
           CLOSE CKERRF
           IF FS-CKERRF NOT = "00"
              DISPLAY "CKERRF クローズ失敗 ST=" FS-CKERRF
              MOVE "1" TO WK-ABEND-FLG
           END-IF.
