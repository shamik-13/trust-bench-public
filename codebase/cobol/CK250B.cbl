       IDENTIFICATION DIVISION.
       PROGRAM-ID. CK250B.
       AUTHOR. CIF-BATCH.
      ******************************************************************
      * 統合キー未連携監査バッチ
      ******************************************************************

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CMKEYF ASSIGN TO "CMKEYF"
              ORGANIZATION IS SEQUENTIAL
              ACCESS MODE IS SEQUENTIAL
              FILE STATUS IS WS-CMKEYF-ST.

           SELECT CKLNKF ASSIGN TO "CKLNKF"
              ORGANIZATION IS INDEXED
              ACCESS MODE IS SEQUENTIAL
              RECORD KEY IS LK-LINK-ID
              FILE STATUS IS WS-CKLNKF-ST.

           SELECT CMCIFF ASSIGN TO "CMCIFF"
              ORGANIZATION IS SEQUENTIAL
              ACCESS MODE IS SEQUENTIAL
              FILE STATUS IS WS-CMCIFF-ST.

           SELECT CKERRF ASSIGN TO "CKERRF"
              ORGANIZATION IS SEQUENTIAL
              ACCESS MODE IS SEQUENTIAL
              FILE STATUS IS WS-CKERRF-ST.

           SELECT CMRSLF ASSIGN TO "CMRSLF"
              ORGANIZATION IS SEQUENTIAL
              ACCESS MODE IS SEQUENTIAL
              FILE STATUS IS WS-CMRSLF-ST.

       DATA DIVISION.
       FILE SECTION.

       FD  CMKEYF.
           COPY CMKEYFC.

       FD  CKLNKF.
           COPY CKLNKC.

       FD  CMCIFF.
           COPY CMCIFFC.

       FD  CKERRF.
           COPY CKERRC.

       FD  CMRSLF.
           COPY CMRSLC.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05 WS-CMKEYF-ST       PIC XX VALUE SPACE.
           05 WS-CKLNKF-ST       PIC XX VALUE SPACE.
           05 WS-CMCIFF-ST       PIC XX VALUE SPACE.
           05 WS-CKERRF-ST       PIC XX VALUE SPACE.
           05 WS-CMRSLF-ST       PIC XX VALUE SPACE.

       01  WS-FLAGS.
           05 WS-EOF-CMKEYF      PIC X VALUE "N".
              88 EOF-CMKEYF            VALUE "Y".
           05 WS-EOF-CKLNKF      PIC X VALUE "N".
              88 EOF-CKLNKF            VALUE "Y".
           05 WS-EOF-CMCIFF      PIC X VALUE "N".
              88 EOF-CMCIFF            VALUE "Y".
           05 WS-LINK-FOUND-SW   PIC X VALUE "N".
              88 LINK-FOUND            VALUE "Y".
           05 WS-CIF-FOUND-SW    PIC X VALUE "N".
              88 CIF-FOUND             VALUE "Y".
           05 WS-ABEND-SW        PIC X VALUE "N".
              88 ABEND-OCCURRED        VALUE "Y".

       01  WS-WORK.
           05 WS-TODAY           PIC 9(8) VALUE ZERO.
           05 WS-SAVE-KEY-ID     PIC X(30) VALUE SPACE.
           05 WS-SAVE-CIF-NO     PIC X(20) VALUE SPACE.
           05 WS-RESULT-SEQ      PIC 9(9) VALUE ZERO.
           05 WS-ERROR-SEQ       PIC 9(9) VALUE ZERO.
           05 WS-READ-KEY-CNT    PIC 9(9) VALUE ZERO.
           05 WS-AUDIT-CNT       PIC 9(9) VALUE ZERO.
           05 WS-LINKED-CNT      PIC 9(9) VALUE ZERO.
           05 WS-EXCLUDE-CNT     PIC 9(9) VALUE ZERO.
           05 WS-ERROR-CNT       PIC 9(9) VALUE ZERO.
           05 WS-DISP-CNT        PIC ZZZ,ZZZ,ZZ9.

       01  WS-CONSTANTS.
           05 WS-PGM-ID          PIC X(08) VALUE "CK250B".
           05 WS-KEY-ACTIVE      PIC X(02) VALUE "01".
           05 WS-KEY-HOLD        PIC X(02) VALUE "08".
           05 WS-CIF-ACTIVE      PIC X(02) VALUE "01".
           05 WS-CIF-EXCLUDE     PIC X(02) VALUE "08".
           05 WS-CIF-STOP        PIC X(02) VALUE "09".
           05 WS-RS-NORMAL       PIC X(02) VALUE "00".
           05 WS-RS-EXCLUDE      PIC X(02) VALUE "10".
           05 WS-RS-ERROR        PIC X(02) VALUE "90".
           05 WS-RSN-LINKED      PIC X(04) VALUE "L001".
           05 WS-RSN-KEY-HOLD    PIC X(04) VALUE "K008".
           05 WS-RSN-KEY-STAT    PIC X(04) VALUE "K999".
           05 WS-RSN-CIF-STOP    PIC X(04) VALUE "C009".
           05 WS-RSN-CIF-EXCL    PIC X(04) VALUE "C008".
           05 WS-RSN-CIF-NOTF    PIC X(04) VALUE "C404".
           05 WS-RSN-NOLINK      PIC X(04) VALUE "E250".
           05 WS-ERR-NOLINK      PIC X(06) VALUE "CK250E".

       PROCEDURE DIVISION.
       MAIN-PROCESS.
           PERFORM 0000-INIT
           IF NOT ABEND-OCCURRED
              PERFORM 1000-MAIN-LOOP
           END-IF
           PERFORM 9000-END
           GOBACK.

       0000-INIT.
           MOVE 0 TO RETURN-CODE
           ACCEPT WS-TODAY FROM DATE YYYYMMDD

           OPEN INPUT CMKEYF
           IF WS-CMKEYF-ST NOT = "00"
              DISPLAY "CMKEYF OPEN ST=" WS-CMKEYF-ST
              PERFORM 9900-ABEND
           END-IF

           IF NOT ABEND-OCCURRED
              OPEN OUTPUT CKERRF
              IF WS-CKERRF-ST NOT = "00"
                 DISPLAY "CKERRF OPEN ST=" WS-CKERRF-ST
                 PERFORM 9900-ABEND
              END-IF
           END-IF

           IF NOT ABEND-OCCURRED
              OPEN OUTPUT CMRSLF
              IF WS-CMRSLF-ST NOT = "00"
                 DISPLAY "CMRSLF OPEN ST=" WS-CMRSLF-ST
                 PERFORM 9900-ABEND
              END-IF
           END-IF.

       1000-MAIN-LOOP.
           PERFORM UNTIL EOF-CMKEYF OR ABEND-OCCURRED
              READ CMKEYF
                 AT END
                    SET EOF-CMKEYF TO TRUE
                 NOT AT END
                    IF WS-CMKEYF-ST = "00"
                       ADD 1 TO WS-READ-KEY-CNT
                       PERFORM 2000-PROCESS-KEY
                    ELSE
                       DISPLAY "CMKEYF READ ST=" WS-CMKEYF-ST
                       PERFORM 9900-ABEND
                    END-IF
              END-READ
           END-PERFORM.

       2000-PROCESS-KEY.
           MOVE CK-KEY-ID TO WS-SAVE-KEY-ID
           MOVE CK-CIF-NO TO WS-SAVE-CIF-NO

           EVALUATE CK-KEY-STATUS-KBN
              WHEN WS-KEY-ACTIVE
                 PERFORM 2100-CHECK-LINK
                 IF NOT ABEND-OCCURRED
                    IF LINK-FOUND
                       ADD 1 TO WS-LINKED-CNT
                       PERFORM 5000-WRITE-RESULT-LINKED
                    ELSE
                       PERFORM 2200-CHECK-CIF
                       IF NOT ABEND-OCCURRED
                          PERFORM 2300-JUDGE-UNLINKED
                       END-IF
                    END-IF
                 END-IF
              WHEN WS-KEY-HOLD
                 ADD 1 TO WS-EXCLUDE-CNT
                 PERFORM 5100-WRITE-RESULT-KEY-HOLD
              WHEN OTHER
                 ADD 1 TO WS-EXCLUDE-CNT
                 PERFORM 5110-WRITE-RESULT-KEY-STAT
           END-EVALUATE.

       2100-CHECK-LINK.
           MOVE "N" TO WS-LINK-FOUND-SW
           MOVE "N" TO WS-EOF-CKLNKF

           OPEN INPUT CKLNKF
           IF WS-CKLNKF-ST NOT = "00"
              DISPLAY "CKLNKF OPEN ST=" WS-CKLNKF-ST
              PERFORM 9900-ABEND
           END-IF

           PERFORM UNTIL EOF-CKLNKF OR LINK-FOUND
              OR ABEND-OCCURRED
              READ CKLNKF NEXT RECORD
                 AT END
                    SET EOF-CKLNKF TO TRUE
                 NOT AT END
                    IF WS-CKLNKF-ST = "00"
                       IF LK-KEY-ID = WS-SAVE-KEY-ID
                          AND LK-CIF-NO = WS-SAVE-CIF-NO
                          SET LINK-FOUND TO TRUE
                       END-IF
                    ELSE
                       DISPLAY "CKLNKF READ ST=" WS-CKLNKF-ST
                       PERFORM 9900-ABEND
                    END-IF
              END-READ
           END-PERFORM

           IF WS-CKLNKF-ST = "00" OR WS-CKLNKF-ST = "10"
              CLOSE CKLNKF
              IF WS-CKLNKF-ST NOT = "00"
                 DISPLAY "CKLNKF CLOSE ST=" WS-CKLNKF-ST
                 PERFORM 9900-ABEND
              END-IF
           END-IF.

       2200-CHECK-CIF.
           MOVE "N" TO WS-CIF-FOUND-SW
           MOVE "N" TO WS-EOF-CMCIFF

           OPEN INPUT CMCIFF
           IF WS-CMCIFF-ST NOT = "00"
              DISPLAY "CMCIFF OPEN ST=" WS-CMCIFF-ST
              PERFORM 9900-ABEND
           END-IF

           PERFORM UNTIL EOF-CMCIFF OR CIF-FOUND
              OR ABEND-OCCURRED
              READ CMCIFF
                 AT END
                    SET EOF-CMCIFF TO TRUE
                 NOT AT END
                    IF WS-CMCIFF-ST = "00"
                       IF CF-CIF-NO = WS-SAVE-CIF-NO
                          SET CIF-FOUND TO TRUE
                       END-IF
                    ELSE
                       DISPLAY "CMCIFF READ ST=" WS-CMCIFF-ST
                       PERFORM 9900-ABEND
                    END-IF
              END-READ
           END-PERFORM

           IF WS-CMCIFF-ST = "00" OR WS-CMCIFF-ST = "10"
              CLOSE CMCIFF
              IF WS-CMCIFF-ST NOT = "00"
                 DISPLAY "CMCIFF CLOSE ST=" WS-CMCIFF-ST
                 PERFORM 9900-ABEND
              END-IF
           END-IF.

       2300-JUDGE-UNLINKED.
           IF NOT CIF-FOUND
              ADD 1 TO WS-ERROR-CNT
              PERFORM 5200-WRITE-RESULT-CIF-NOTF
              PERFORM 6000-WRITE-ERROR
           ELSE
              EVALUATE CF-CIF-STATUS-KBN
                 WHEN WS-CIF-ACTIVE
                    ADD 1 TO WS-AUDIT-CNT
                    ADD 1 TO WS-ERROR-CNT
                    PERFORM 5300-WRITE-RESULT-NOLINK
                    PERFORM 6000-WRITE-ERROR
                 WHEN WS-CIF-EXCLUDE
                    ADD 1 TO WS-EXCLUDE-CNT
                    PERFORM 5210-WRITE-RESULT-CIF-EXCL
                 WHEN WS-CIF-STOP
                    ADD 1 TO WS-EXCLUDE-CNT
                    PERFORM 5220-WRITE-RESULT-CIF-STOP
                 WHEN OTHER
                    ADD 1 TO WS-ERROR-CNT
                    PERFORM 5200-WRITE-RESULT-CIF-NOTF
                    PERFORM 6000-WRITE-ERROR
              END-EVALUATE
           END-IF.

       5000-WRITE-RESULT-LINKED.
           MOVE SPACES TO CMRSLF-REC
           ADD 1 TO WS-RESULT-SEQ
           MOVE WS-RESULT-SEQ TO RS-RESULT-ID
           MOVE WS-SAVE-CIF-NO TO RS-CIF-NO
           MOVE WS-SAVE-KEY-ID TO RS-KEY-ID
           MOVE WS-RS-NORMAL TO RS-RESULT-KBN
           MOVE WS-RSN-LINKED TO RS-REASON-CD
           MOVE WS-TODAY TO RS-OUTPUT-DT
           PERFORM 5900-WRITE-CMRSLF.

       5100-WRITE-RESULT-KEY-HOLD.
           MOVE SPACES TO CMRSLF-REC
           ADD 1 TO WS-RESULT-SEQ
           MOVE WS-RESULT-SEQ TO RS-RESULT-ID
           MOVE WS-SAVE-CIF-NO TO RS-CIF-NO
           MOVE WS-SAVE-KEY-ID TO RS-KEY-ID
           MOVE WS-RS-EXCLUDE TO RS-RESULT-KBN
           MOVE WS-RSN-KEY-HOLD TO RS-REASON-CD
           MOVE WS-TODAY TO RS-OUTPUT-DT
           PERFORM 5900-WRITE-CMRSLF.

       5110-WRITE-RESULT-KEY-STAT.
           MOVE SPACES TO CMRSLF-REC
           ADD 1 TO WS-RESULT-SEQ
           MOVE WS-RESULT-SEQ TO RS-RESULT-ID
           MOVE WS-SAVE-CIF-NO TO RS-CIF-NO
           MOVE WS-SAVE-KEY-ID TO RS-KEY-ID
           MOVE WS-RS-EXCLUDE TO RS-RESULT-KBN
           MOVE WS-RSN-KEY-STAT TO RS-REASON-CD
           MOVE WS-TODAY TO RS-OUTPUT-DT
           PERFORM 5900-WRITE-CMRSLF.

       5200-WRITE-RESULT-CIF-NOTF.
           MOVE SPACES TO CMRSLF-REC
           ADD 1 TO WS-RESULT-SEQ
           MOVE WS-RESULT-SEQ TO RS-RESULT-ID
           MOVE WS-SAVE-CIF-NO TO RS-CIF-NO
           MOVE WS-SAVE-KEY-ID TO RS-KEY-ID
           MOVE WS-RS-ERROR TO RS-RESULT-KBN
           MOVE WS-RSN-CIF-NOTF TO RS-REASON-CD
           MOVE WS-TODAY TO RS-OUTPUT-DT
           PERFORM 5900-WRITE-CMRSLF.

       5210-WRITE-RESULT-CIF-EXCL.
           MOVE SPACES TO CMRSLF-REC
           ADD 1 TO WS-RESULT-SEQ
           MOVE WS-RESULT-SEQ TO RS-RESULT-ID
           MOVE WS-SAVE-CIF-NO TO RS-CIF-NO
           MOVE WS-SAVE-KEY-ID TO RS-KEY-ID
           MOVE WS-RS-EXCLUDE TO RS-RESULT-KBN
           MOVE WS-RSN-CIF-EXCL TO RS-REASON-CD
           MOVE WS-TODAY TO RS-OUTPUT-DT
           PERFORM 5900-WRITE-CMRSLF.

       5220-WRITE-RESULT-CIF-STOP.
           MOVE SPACES TO CMRSLF-REC
           ADD 1 TO WS-RESULT-SEQ
           MOVE WS-RESULT-SEQ TO RS-RESULT-ID
           MOVE WS-SAVE-CIF-NO TO RS-CIF-NO
           MOVE WS-SAVE-KEY-ID TO RS-KEY-ID
           MOVE WS-RS-EXCLUDE TO RS-RESULT-KBN
           MOVE WS-RSN-CIF-STOP TO RS-REASON-CD
           MOVE WS-TODAY TO RS-OUTPUT-DT
           PERFORM 5900-WRITE-CMRSLF.

       5300-WRITE-RESULT-NOLINK.
           MOVE SPACES TO CMRSLF-REC
           ADD 1 TO WS-RESULT-SEQ
           MOVE WS-RESULT-SEQ TO RS-RESULT-ID
           MOVE WS-SAVE-CIF-NO TO RS-CIF-NO
           MOVE WS-SAVE-KEY-ID TO RS-KEY-ID
           MOVE WS-RS-ERROR TO RS-RESULT-KBN
           MOVE WS-RSN-NOLINK TO RS-REASON-CD
           MOVE WS-TODAY TO RS-OUTPUT-DT
           PERFORM 5900-WRITE-CMRSLF.

       5900-WRITE-CMRSLF.
           WRITE CMRSLF-REC
           IF WS-CMRSLF-ST NOT = "00"
              DISPLAY "CMRSLF WRITE ST=" WS-CMRSLF-ST
              PERFORM 9900-ABEND
           END-IF.

       6000-WRITE-ERROR.
           MOVE SPACES TO CKERRF-REC
           ADD 1 TO WS-ERROR-SEQ
           MOVE WS-ERROR-SEQ TO ER-ERROR-ID
           MOVE WS-PGM-ID TO ER-SOURCE-PGM-ID
           MOVE WS-SAVE-CIF-NO TO ER-CIF-NO
           MOVE WS-SAVE-KEY-ID TO ER-KEY-ID
           MOVE WS-ERR-NOLINK TO ER-ERROR-CD
           MOVE WS-TODAY TO ER-ERROR-DT
           WRITE CKERRF-REC
           IF WS-CKERRF-ST NOT = "00"
              DISPLAY "CKERRF WRITE ST=" WS-CKERRF-ST
              PERFORM 9900-ABEND
           END-IF.

       9000-END.
           IF WS-CMKEYF-ST = "00" OR WS-CMKEYF-ST = "10"
              CLOSE CMKEYF
              IF WS-CMKEYF-ST NOT = "00"
                 DISPLAY "CMKEYF CLOSE ST=" WS-CMKEYF-ST
                 PERFORM 9900-ABEND
              END-IF
           END-IF

           IF WS-CKERRF-ST = "00"
              CLOSE CKERRF
              IF WS-CKERRF-ST NOT = "00"
                 DISPLAY "CKERRF CLOSE ST=" WS-CKERRF-ST
                 PERFORM 9900-ABEND
              END-IF
           END-IF

           IF WS-CMRSLF-ST = "00"
              CLOSE CMRSLF
              IF WS-CMRSLF-ST NOT = "00"
                 DISPLAY "CMRSLF CLOSE ST=" WS-CMRSLF-ST
                 PERFORM 9900-ABEND
              END-IF
           END-IF

           MOVE WS-READ-KEY-CNT TO WS-DISP-CNT
           DISPLAY "CK250B READ KEY COUNT=" WS-DISP-CNT
           MOVE WS-LINKED-CNT TO WS-DISP-CNT
           DISPLAY "CK250B LINKED COUNT=" WS-DISP-CNT
           MOVE WS-EXCLUDE-CNT TO WS-DISP-CNT
           DISPLAY "CK250B EXCLUDE COUNT=" WS-DISP-CNT
           MOVE WS-ERROR-CNT TO WS-DISP-CNT
           DISPLAY "CK250B ERROR COUNT=" WS-DISP-CNT

           IF ABEND-OCCURRED
              MOVE 8 TO RETURN-CODE
           ELSE
              MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           SET ABEND-OCCURRED TO TRUE
           MOVE 8 TO RETURN-CODE.
