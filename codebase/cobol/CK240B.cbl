       IDENTIFICATION DIVISION.
       PROGRAM-ID. CK240B.
      ******************************************************************
      *  統合キー連携送信結果反映バッチ
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CKLNKF ASSIGN TO "CKLNKF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS LK-LINK-ID
               FILE STATUS IS WS-CKLNKF-ST.
           SELECT CGCODF ASSIGN TO "CGCODF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS GC-CODE-ID
               FILE STATUS IS WS-CGCODF-ST.
           SELECT CKERRF ASSIGN TO "CKERRF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-CKERRF-ST.

       DATA DIVISION.
       FILE SECTION.
       FD  CKLNKF.
           COPY CKLNKC.
       FD  CGCODF.
           COPY CGCODC.
       FD  CKERRF.
           COPY CKERRC.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05 WS-CKLNKF-ST             PIC XX VALUE SPACE.
           05 WS-CGCODF-ST             PIC XX VALUE SPACE.
           05 WS-CKERRF-ST             PIC XX VALUE SPACE.

       01  WS-WORK.
           05 WS-PGM-ID                PIC X(08) VALUE "CK240B".
           05 WS-END-FLG               PIC X VALUE "0".
              88 WS-END                     VALUE "1".
           05 WS-HARD-ERR-FLG          PIC X VALUE "0".
              88 WS-HARD-ERR               VALUE "1".
           05 WS-BUSINESS-ERR-FLG      PIC X VALUE "0".
              88 WS-BUSINESS-ERR           VALUE "1".
           05 WS-SYS-DATE              PIC 9(08) VALUE ZERO.
           05 WS-SYS-TIME              PIC 9(08) VALUE ZERO.
           05 WS-ERROR-SEQ             PIC 9(07) VALUE ZERO.
           05 WS-CODE-LINK-ID          PIC X(20) VALUE SPACE.
           05 WS-CODE-TARGET-SYS       PIC X(06) VALUE SPACE.
           05 WS-CODE-RESULT-KBN       PIC X(02) VALUE SPACE.
           05 WS-NEW-STATUS            PIC X(01) VALUE SPACE.
           05 WS-ERR-CD                PIC X(04) VALUE SPACE.
           05 WS-VALID-FROM            PIC 9(08) VALUE ZERO.
           05 WS-VALID-TO              PIC 9(08) VALUE ZERO.
           05 WS-READ-CNT              PIC 9(09) VALUE ZERO.
           05 WS-UPD-CNT               PIC 9(09) VALUE ZERO.
           05 WS-ERR-CNT               PIC 9(09) VALUE ZERO.
           05 WS-SKIP-CNT              PIC 9(09) VALUE ZERO.

       01  WS-CONST.
           05 CN-CODE-KBN-RESULT       PIC X(02) VALUE "SR".
           05 CN-ST-MISOU              PIC X(01) VALUE "0".
           05 CN-ST-SOUTYUU            PIC X(01) VALUE "1".
           05 CN-ST-SEIJYOU            PIC X(01) VALUE "2".
           05 CN-ST-SAISOU             PIC X(01) VALUE "3".
           05 CN-ST-TEISI              PIC X(01) VALUE "9".
           05 CN-RS-SEIJYOU            PIC X(02) VALUE "00".
           05 CN-RS-SAISOU             PIC X(02) VALUE "10".
           05 CN-RS-TEISI              PIC X(02) VALUE "90".
           05 CN-ERR-LINK-NASHI        PIC X(04) VALUE "E101".
           05 CN-ERR-SYS-FUICHI        PIC X(04) VALUE "E102".
           05 CN-ERR-SENI-FUKA         PIC X(04) VALUE "E103".
           05 CN-ERR-CODE-FUSEI        PIC X(04) VALUE "E104".
           05 CN-ERR-KIGEN-GAI         PIC X(04) VALUE "E105".

       PROCEDURE DIVISION.
       MAIN-RTN.
           PERFORM 0100-INIT
           IF NOT WS-HARD-ERR
              PERFORM 0200-MAIN UNTIL WS-END OR WS-HARD-ERR
           END-IF
           PERFORM 0900-END
           GOBACK.

       0100-INIT.
           ACCEPT WS-SYS-DATE FROM DATE YYYYMMDD
           ACCEPT WS-SYS-TIME FROM TIME
           MOVE 0 TO RETURN-CODE
           DISPLAY "CK240B START DATE=" WS-SYS-DATE
           OPEN I-O CKLNKF
           IF WS-CKLNKF-ST NOT = "00"
              DISPLAY "CKLNKF OPEN ERROR ST=" WS-CKLNKF-ST
              SET WS-HARD-ERR TO TRUE
              MOVE 12 TO RETURN-CODE
           END-IF
           IF NOT WS-HARD-ERR
              OPEN INPUT CGCODF
              IF WS-CGCODF-ST NOT = "00"
                 DISPLAY "CGCODF OPEN ERROR ST=" WS-CGCODF-ST
                 SET WS-HARD-ERR TO TRUE
                 MOVE 12 TO RETURN-CODE
              END-IF
           END-IF
           IF NOT WS-HARD-ERR
              OPEN EXTEND CKERRF
              IF WS-CKERRF-ST NOT = "00"
                 DISPLAY "CKERRF OPEN ERROR ST=" WS-CKERRF-ST
                 SET WS-HARD-ERR TO TRUE
                 MOVE 12 TO RETURN-CODE
              END-IF
           END-IF.

       0200-MAIN.
           READ CGCODF NEXT RECORD
              AT END
                 SET WS-END TO TRUE
              NOT AT END
                 IF WS-CGCODF-ST = "00"
                    ADD 1 TO WS-READ-CNT
                    PERFORM 0300-PROCESS-CODE
                 ELSE
                    DISPLAY "CGCODF READ ERROR ST=" WS-CGCODF-ST
                    SET WS-HARD-ERR TO TRUE
                    MOVE 12 TO RETURN-CODE
                 END-IF
           END-READ.

       0300-PROCESS-CODE.
           MOVE "0" TO WS-BUSINESS-ERR-FLG
           MOVE SPACE TO WS-ERR-CD
           MOVE SPACE TO WS-NEW-STATUS
           MOVE SPACE TO LK-CIF-NO
           MOVE SPACE TO LK-KEY-ID
           IF GC-CODE-KBN NOT = CN-CODE-KBN-RESULT
              ADD 1 TO WS-SKIP-CNT
           ELSE
              PERFORM 0310-EDIT-CODE
              IF NOT WS-BUSINESS-ERR
                 PERFORM 0320-READ-LINK
              END-IF
              IF NOT WS-BUSINESS-ERR
                 PERFORM 0330-CHECK-TRANSITION
              END-IF
              IF NOT WS-BUSINESS-ERR
                 PERFORM 0340-REWRITE-LINK
              ELSE
                 PERFORM 0800-WRITE-ERROR
              END-IF
           END-IF.

       0310-EDIT-CODE.
           MOVE GC-CODE-ID          TO WS-CODE-LINK-ID
           MOVE GC-CODE-VALUE(1:2)  TO WS-CODE-RESULT-KBN
           MOVE GC-CODE-VALUE(3:6)  TO WS-CODE-TARGET-SYS
           MOVE GC-VALID-FROM-DT    TO WS-VALID-FROM
           MOVE GC-VALID-TO-DT      TO WS-VALID-TO
           EVALUATE TRUE
              WHEN WS-SYS-DATE < WS-VALID-FROM
                 MOVE CN-ERR-KIGEN-GAI TO WS-ERR-CD
                 SET WS-BUSINESS-ERR TO TRUE
              WHEN WS-VALID-TO NOT = ZERO
               AND WS-SYS-DATE > WS-VALID-TO
                 MOVE CN-ERR-KIGEN-GAI TO WS-ERR-CD
                 SET WS-BUSINESS-ERR TO TRUE
              WHEN WS-CODE-RESULT-KBN = CN-RS-SEIJYOU
                 MOVE CN-ST-SEIJYOU TO WS-NEW-STATUS
              WHEN WS-CODE-RESULT-KBN = CN-RS-SAISOU
                 MOVE CN-ST-SAISOU TO WS-NEW-STATUS
              WHEN WS-CODE-RESULT-KBN = CN-RS-TEISI
                 MOVE CN-ST-TEISI TO WS-NEW-STATUS
              WHEN OTHER
                 MOVE CN-ERR-CODE-FUSEI TO WS-ERR-CD
                 SET WS-BUSINESS-ERR TO TRUE
           END-EVALUATE.

       0320-READ-LINK.
           MOVE WS-CODE-LINK-ID TO LK-LINK-ID
           READ CKLNKF KEY IS LK-LINK-ID
              INVALID KEY
                 IF WS-CKLNKF-ST = "23"
                    MOVE CN-ERR-LINK-NASHI TO WS-ERR-CD
                    SET WS-BUSINESS-ERR TO TRUE
                    MOVE SPACE TO LK-CIF-NO
                    MOVE SPACE TO LK-KEY-ID
                 ELSE
                    DISPLAY "CKLNKF READ ERROR ST=" WS-CKLNKF-ST
                    SET WS-HARD-ERR TO TRUE
                    MOVE 12 TO RETURN-CODE
                 END-IF
              NOT INVALID KEY
                 IF WS-CKLNKF-ST NOT = "00"
                    DISPLAY "CKLNKF READ ERROR ST=" WS-CKLNKF-ST
                    SET WS-HARD-ERR TO TRUE
                    MOVE 12 TO RETURN-CODE
                 END-IF
           END-READ.

       0330-CHECK-TRANSITION.
           IF LK-TARGET-SYS-ID NOT = WS-CODE-TARGET-SYS
              MOVE CN-ERR-SYS-FUICHI TO WS-ERR-CD
              SET WS-BUSINESS-ERR TO TRUE
           ELSE
              EVALUATE LK-SEND-STATUS-KBN ALSO WS-NEW-STATUS
                 WHEN CN-ST-MISOU   ALSO CN-ST-SEIJYOU
                 WHEN CN-ST-SOUTYUU ALSO CN-ST-SEIJYOU
                 WHEN CN-ST-MISOU   ALSO CN-ST-SAISOU
                 WHEN CN-ST-SOUTYUU ALSO CN-ST-SAISOU
                 WHEN CN-ST-SAISOU  ALSO CN-ST-SEIJYOU
                 WHEN CN-ST-MISOU   ALSO CN-ST-TEISI
                 WHEN CN-ST-SOUTYUU ALSO CN-ST-TEISI
                 WHEN CN-ST-SAISOU  ALSO CN-ST-TEISI
                    CONTINUE
                 WHEN OTHER
                    MOVE CN-ERR-SENI-FUKA TO WS-ERR-CD
                    SET WS-BUSINESS-ERR TO TRUE
              END-EVALUATE
           END-IF.

       0340-REWRITE-LINK.
           MOVE WS-NEW-STATUS TO LK-SEND-STATUS-KBN
           MOVE WS-SYS-DATE   TO LK-SEND-DT
           REWRITE CKLNKF-REC
           IF WS-CKLNKF-ST = "00"
              ADD 1 TO WS-UPD-CNT
           ELSE
              DISPLAY "CKLNKF REWRITE ERROR ST=" WS-CKLNKF-ST
              SET WS-HARD-ERR TO TRUE
              MOVE 12 TO RETURN-CODE
           END-IF.

       0800-WRITE-ERROR.
           ADD 1 TO WS-ERROR-SEQ
           MOVE SPACE TO CKERRF-REC
           STRING WS-PGM-ID    DELIMITED BY SIZE
                  WS-SYS-DATE  DELIMITED BY SIZE
                  WS-ERROR-SEQ DELIMITED BY SIZE
             INTO ER-ERROR-ID
           END-STRING
           MOVE WS-PGM-ID   TO ER-SOURCE-PGM-ID
           MOVE LK-CIF-NO   TO ER-CIF-NO
           MOVE LK-KEY-ID   TO ER-KEY-ID
           MOVE WS-ERR-CD   TO ER-ERROR-CD
           MOVE WS-SYS-DATE TO ER-ERROR-DT
           WRITE CKERRF-REC
           IF WS-CKERRF-ST = "00"
              ADD 1 TO WS-ERR-CNT
              DISPLAY "ERROR LINK=" WS-CODE-LINK-ID
                      " CD=" WS-ERR-CD
           ELSE
              DISPLAY "CKERRF WRITE ERROR ST=" WS-CKERRF-ST
              SET WS-HARD-ERR TO TRUE
              MOVE 12 TO RETURN-CODE
           END-IF.

       0900-END.
           IF WS-CKLNKF-ST NOT = SPACE
              CLOSE CKLNKF
              IF WS-CKLNKF-ST NOT = "00"
                 DISPLAY "CKLNKF CLOSE ERROR ST=" WS-CKLNKF-ST
                 SET WS-HARD-ERR TO TRUE
                 MOVE 12 TO RETURN-CODE
              END-IF
           END-IF
           IF WS-CGCODF-ST NOT = SPACE
              CLOSE CGCODF
              IF WS-CGCODF-ST NOT = "00"
                 DISPLAY "CGCODF CLOSE ERROR ST=" WS-CGCODF-ST
                 SET WS-HARD-ERR TO TRUE
                 MOVE 12 TO RETURN-CODE
              END-IF
           END-IF
           IF WS-CKERRF-ST NOT = SPACE
              CLOSE CKERRF
              IF WS-CKERRF-ST NOT = "00"
                 DISPLAY "CKERRF CLOSE ERROR ST=" WS-CKERRF-ST
                 SET WS-HARD-ERR TO TRUE
                 MOVE 12 TO RETURN-CODE
              END-IF
           END-IF
           DISPLAY "CK240B COUNT READ=" WS-READ-CNT
                   " UPDATE=" WS-UPD-CNT
                   " ERROR=" WS-ERR-CNT
                   " SKIP=" WS-SKIP-CNT
           IF WS-HARD-ERR
              DISPLAY "CK240B ABEND RC=" RETURN-CODE
           ELSE
              MOVE 0 TO RETURN-CODE
              DISPLAY "CK240B NORMAL END RC=0"
           END-IF.
