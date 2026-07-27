       IDENTIFICATION DIVISION.
       PROGRAM-ID. CK270B.
      ******************************************************************
      * 変更履歴
      * 版数  年月日    担当      概要
      * 1.00  20230410  KIBAN01   初版作成
      * 1.01  20230925  KIBAN02   属性未登録CIF除外処理追加
      * 1.02  20240115  KIBAN02   呼出元連携状態補完出力追加
      ******************************************************************
      * 統合キー照会索引補完バッチ
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CMKEYF ASSIGN TO "CMKEYF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-CMKEYF-ST.
           SELECT CKLNKF ASSIGN TO "CKLNKF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS LK-LINK-ID
               FILE STATUS IS WS-CKLNKF-ST.
           SELECT CMATTF ASSIGN TO "CMATTF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CA-CIF-NO
               FILE STATUS IS WS-CMATTF-ST.
           SELECT CMRSLF ASSIGN TO "CMRSLF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-CMRSLF-ST.

       DATA DIVISION.
       FILE SECTION.
       FD  CMKEYF.
           COPY CMKEYFC.
       FD  CKLNKF.
           COPY CKLNKC.
       FD  CMATTF.
           COPY CMATTC.
       FD  CMRSLF.
           COPY CMRSLC.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05 WS-CMKEYF-ST             PIC XX VALUE SPACE.
           05 WS-CKLNKF-ST             PIC XX VALUE SPACE.
           05 WS-CMATTF-ST             PIC XX VALUE SPACE.
           05 WS-CMRSLF-ST             PIC XX VALUE SPACE.

       01  WS-FLAGS.
           05 WS-EOF-CKLNKF            PIC X VALUE "N".
              88 CKLNKF-END                 VALUE "Y".
           05 WS-EOF-CMKEYF            PIC X VALUE "N".
              88 CMKEYF-END                 VALUE "Y".
           05 WS-KEY-FOUND             PIC X VALUE "N".
              88 KEY-FOUND                  VALUE "Y".
           05 WS-ATTR-FOUND            PIC X VALUE "N".
              88 ATTR-FOUND                 VALUE "Y".

       01  WS-WORK.
           05 WS-RUN-DATE              PIC 9(08) VALUE ZERO.
           05 WS-RESULT-SEQ            PIC 9(09) VALUE ZERO.
           05 WS-READ-LINK-CNT         PIC 9(09) VALUE ZERO.
           05 WS-READ-KEY-CNT          PIC 9(09) VALUE ZERO.
           05 WS-OUT-CNT               PIC 9(09) VALUE ZERO.
           05 WS-SKIP-ATTR-CNT         PIC 9(09) VALUE ZERO.
           05 WS-SKIP-KEY-CNT          PIC 9(09) VALUE ZERO.
           05 WS-SKIP-STATUS-CNT       PIC 9(09) VALUE ZERO.
           05 WS-ABEND-FLG             PIC X VALUE "N".
              88 ABEND-OCCURRED             VALUE "Y".
           05 WS-RESULT-ID-WK          PIC 9(09) VALUE ZERO.

       01  WS-STATUS-CONST.
           05 WS-CIF-VALID             PIC XX VALUE "01".
           05 WS-CIF-EXCLUDE           PIC XX VALUE "08".
           05 WS-CIF-INVALID           PIC XX VALUE "09".

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE 8 TO RETURN-CODE
           ACCEPT WS-RUN-DATE FROM DATE YYYYMMDD
           PERFORM 1000-OPEN-FILES
           IF NOT ABEND-OCCURRED
              PERFORM 2000-PROCESS-LINKS
           END-IF
           PERFORM 9000-CLOSE-FILES
           IF ABEND-OCCURRED
              MOVE 12 TO RETURN-CODE
           ELSE
              MOVE 0 TO RETURN-CODE
              DISPLAY "CK270B 正常終了"
              DISPLAY "リンク読込件数=" WS-READ-LINK-CNT
              DISPLAY "結果出力件数=" WS-OUT-CNT
              DISPLAY "属性未登録除外件数=" WS-SKIP-ATTR-CNT
              DISPLAY "キー未登録除外件数=" WS-SKIP-KEY-CNT
              DISPLAY "状態不正除外件数=" WS-SKIP-STATUS-CNT
           END-IF
           GOBACK.

       1000-OPEN-FILES.
           OPEN INPUT CKLNKF
           IF WS-CKLNKF-ST NOT = "00"
              DISPLAY "CKLNKF オープン失敗 ST=" WS-CKLNKF-ST
              SET ABEND-OCCURRED TO TRUE
           END-IF

           IF NOT ABEND-OCCURRED
              OPEN INPUT CMATTF
              IF WS-CMATTF-ST NOT = "00"
                 DISPLAY "CMATTF オープン失敗 ST=" WS-CMATTF-ST
                 SET ABEND-OCCURRED TO TRUE
              END-IF
           END-IF

           IF NOT ABEND-OCCURRED
              OPEN OUTPUT CMRSLF
              IF WS-CMRSLF-ST NOT = "00"
                 DISPLAY "CMRSLF オープン失敗 ST=" WS-CMRSLF-ST
                 SET ABEND-OCCURRED TO TRUE
              END-IF
           END-IF.

       2000-PROCESS-LINKS.
           PERFORM UNTIL CKLNKF-END OR ABEND-OCCURRED
              READ CKLNKF NEXT RECORD
                 AT END
                    SET CKLNKF-END TO TRUE
                 NOT AT END
                    IF WS-CKLNKF-ST = "00"
                       ADD 1 TO WS-READ-LINK-CNT
                       PERFORM 2100-EDIT-LINK
                    ELSE
                       DISPLAY "CKLNKF 読込失敗 ST=" WS-CKLNKF-ST
                       SET ABEND-OCCURRED TO TRUE
                    END-IF
              END-READ
           END-PERFORM.

       2100-EDIT-LINK.
           IF LK-KEY-ID = SPACE OR LK-CIF-NO = SPACE
              ADD 1 TO WS-SKIP-KEY-CNT
           ELSE
              PERFORM 2200-READ-ATTR
              IF ATTR-FOUND
                 PERFORM 2300-FIND-KEY
                 IF KEY-FOUND
                    PERFORM 2400-WRITE-RESULT
                 ELSE
                    ADD 1 TO WS-SKIP-KEY-CNT
                 END-IF
              ELSE
                 ADD 1 TO WS-SKIP-ATTR-CNT
              END-IF
           END-IF.

       2200-READ-ATTR.
           MOVE "N" TO WS-ATTR-FOUND
           MOVE LK-CIF-NO TO CA-CIF-NO
           READ CMATTF KEY IS CA-CIF-NO
              INVALID KEY
                 IF WS-CMATTF-ST = "23"
                    CONTINUE
                 ELSE
                    DISPLAY "CMATTF 参照失敗 ST="
                       WS-CMATTF-ST
                    SET ABEND-OCCURRED TO TRUE
                 END-IF
              NOT INVALID KEY
                 IF WS-CMATTF-ST = "00"
                    IF CA-ATTR-STATUS-KBN = WS-CIF-VALID
                       SET ATTR-FOUND TO TRUE
                    ELSE
                       IF CA-ATTR-STATUS-KBN = WS-CIF-EXCLUDE
                          ADD 1 TO WS-SKIP-STATUS-CNT
                       ELSE
                          IF CA-ATTR-STATUS-KBN = WS-CIF-INVALID
                             ADD 1 TO WS-SKIP-STATUS-CNT
                          ELSE
                             DISPLAY "属性状態区分不正"
                             DISPLAY "CIF=" CA-CIF-NO
                             DISPLAY "KBN=" CA-ATTR-STATUS-KBN
                             ADD 1 TO WS-SKIP-STATUS-CNT
                          END-IF
                       END-IF
                    END-IF
                 ELSE
                    DISPLAY "CMATTF 参照後状態不正 ST="
                       WS-CMATTF-ST
                    SET ABEND-OCCURRED TO TRUE
                 END-IF
           END-READ.

       2300-FIND-KEY.
           MOVE "N" TO WS-KEY-FOUND
           MOVE "N" TO WS-EOF-CMKEYF
           OPEN INPUT CMKEYF
           IF WS-CMKEYF-ST NOT = "00"
              DISPLAY "CMKEYF オープン失敗 ST=" WS-CMKEYF-ST
              SET ABEND-OCCURRED TO TRUE
           ELSE
              PERFORM UNTIL CMKEYF-END OR KEY-FOUND
                 OR ABEND-OCCURRED
                 READ CMKEYF
                    AT END
                       SET CMKEYF-END TO TRUE
                    NOT AT END
                       IF WS-CMKEYF-ST = "00"
                          ADD 1 TO WS-READ-KEY-CNT
                          IF CK-KEY-ID = LK-KEY-ID
                             AND CK-CIF-NO = LK-CIF-NO
                             IF CK-KEY-STATUS-KBN = WS-CIF-VALID
                                SET KEY-FOUND TO TRUE
                             ELSE
                                ADD 1 TO WS-SKIP-STATUS-CNT
                             END-IF
                          END-IF
                       ELSE
                          DISPLAY "CMKEYF 読込失敗 ST="
                             WS-CMKEYF-ST
                          SET ABEND-OCCURRED TO TRUE
                       END-IF
                 END-READ
              END-PERFORM

              CLOSE CMKEYF
              IF WS-CMKEYF-ST NOT = "00"
                 DISPLAY "CMKEYF クローズ失敗 ST="
                    WS-CMKEYF-ST
                 SET ABEND-OCCURRED TO TRUE
              END-IF
           END-IF.

       2400-WRITE-RESULT.
           ADD 1 TO WS-RESULT-SEQ
           MOVE WS-RESULT-SEQ TO WS-RESULT-ID-WK
           MOVE WS-RESULT-ID-WK TO RS-RESULT-ID
           MOVE LK-CIF-NO TO RS-CIF-NO
           MOVE LK-KEY-ID TO RS-KEY-ID
           MOVE LK-SEND-STATUS-KBN TO RS-RESULT-KBN
           MOVE LK-TARGET-SYS-ID TO RS-REASON-CD
           MOVE WS-RUN-DATE TO RS-OUTPUT-DT
           WRITE CMRSLF-REC
           IF WS-CMRSLF-ST = "00"
              ADD 1 TO WS-OUT-CNT
           ELSE
              DISPLAY "CMRSLF 書込失敗 ST=" WS-CMRSLF-ST
              SET ABEND-OCCURRED TO TRUE
           END-IF.

       9000-CLOSE-FILES.
           CLOSE CKLNKF
           IF WS-CKLNKF-ST NOT = "00"
              DISPLAY "CKLNKF クローズ失敗 ST=" WS-CKLNKF-ST
              SET ABEND-OCCURRED TO TRUE
           END-IF

           CLOSE CMATTF
           IF WS-CMATTF-ST NOT = "00"
              DISPLAY "CMATTF クローズ失敗 ST=" WS-CMATTF-ST
              SET ABEND-OCCURRED TO TRUE
           END-IF

           CLOSE CMRSLF
           IF WS-CMRSLF-ST NOT = "00"
              DISPLAY "CMRSLF クローズ失敗 ST=" WS-CMRSLF-ST
              SET ABEND-OCCURRED TO TRUE
           END-IF.
