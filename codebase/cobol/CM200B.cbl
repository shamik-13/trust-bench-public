       IDENTIFICATION DIVISION.
       PROGRAM-ID. CM200B.
      ******************************************************************
      * CIF TO INTEGRATED KEY REQUEST RESULT BATCH
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CMCIFF ASSIGN TO "CMCIFF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-CMCIFF.
           SELECT CMATTF ASSIGN TO "CMATTF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS CA-CIF-NO
               FILE STATUS IS FS-CMATTF.
           SELECT CMKEYF ASSIGN TO "CMKEYF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-CMKEYF.
           SELECT CMRSLF ASSIGN TO "CMRSLF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-CMRSLF.
           SELECT CKERRF ASSIGN TO "CKERRF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-CKERRF.

       DATA DIVISION.
       FILE SECTION.
       FD  CMCIFF.
           COPY CMCIFFC.
       FD  CMATTF.
           COPY CMATTC.
       FD  CMKEYF.
           COPY CMKEYFC.
       FD  CMRSLF.
           COPY CMRSLC.
       FD  CKERRF.
           COPY CKERRC.

       WORKING-STORAGE SECTION.
       01  FS-AREA.
           05 FS-CMCIFF              PIC XX VALUE SPACE.
           05 FS-CMATTF              PIC XX VALUE SPACE.
           05 FS-CMKEYF              PIC XX VALUE SPACE.
           05 FS-CMRSLF              PIC XX VALUE SPACE.
           05 FS-CKERRF              PIC XX VALUE SPACE.

       01  SW-AREA.
           05 EOF-CMCIFF             PIC X VALUE "0".
              88 CMCIFF-END               VALUE "1".
           05 EOF-CMKEYF             PIC X VALUE "0".
              88 CMKEYF-END               VALUE "1".
           05 WS-HARD-ERR-SW         PIC X VALUE "0".
              88 HARD-ERROR               VALUE "1".
              88 NO-HARD-ERROR            VALUE "0".

       01  CNT-AREA.
           05 CNT-CIF-READ           PIC 9(9) VALUE ZERO.
           05 CNT-KEY-READ           PIC 9(9) VALUE ZERO.
           05 CNT-REQ-OUT            PIC 9(9) VALUE ZERO.
           05 CNT-EXC-OUT            PIC 9(9) VALUE ZERO.
           05 CNT-ERR-OUT            PIC 9(9) VALUE ZERO.
           05 CNT-KEY-TBL            PIC 9(5) VALUE ZERO.
           05 CNT-RSL-SEQ            PIC 9(9) VALUE ZERO.
           05 CNT-ERR-SEQ            PIC 9(9) VALUE ZERO.

       01  CONST-AREA.
           05 C-PGM-ID               PIC X(8) VALUE "CM200B".
           05 C-CIF-VALID            PIC XX VALUE "01".
           05 C-ATTR-VALID           PIC XX VALUE "01".
           05 C-KEY-VALID            PIC XX VALUE "01".
           05 C-RS-REQUEST           PIC X  VALUE "1".
           05 C-RS-EXCLUDE           PIC X  VALUE "2".
           05 C-RSN-REQ              PIC X(4) VALUE "0000".
           05 C-RSN-CIFSTAT          PIC X(4) VALUE "0101".
           05 C-RSN-KEYDUP           PIC X(4) VALUE "0201".
           05 C-RSN-NOATTR           PIC X(4) VALUE "0301".
           05 C-RSN-ATTRSTAT         PIC X(4) VALUE "0302".
           05 C-RSN-NAME             PIC X(4) VALUE "0303".
           05 C-RSN-ADDR             PIC X(4) VALUE "0304".
           05 C-RSN-PHONE            PIC X(4) VALUE "0305".
           05 C-RSN-DATE             PIC X(4) VALUE "0306".
           05 C-ERR-OPEN             PIC X(4) VALUE "E001".
           05 C-ERR-READ             PIC X(4) VALUE "E002".
           05 C-ERR-WRITE            PIC X(4) VALUE "E003".
           05 C-ERR-TBLOVF           PIC X(4) VALUE "E004".

       01  KEY-TABLE-AREA.
           05 KEY-TBL OCCURS 50000 TIMES
              ASCENDING KEY IS TBL-CIF-NO
              INDEXED BY KIX.
              10 TBL-CIF-NO          PIC X(10).
              10 TBL-KEY-ID          PIC X(16).
              10 TBL-KEY-STATUS-KBN  PIC XX.

       01  WORK-AREA.
           05 WS-TODAY               PIC 9(8).
           05 WS-REASON-CD           PIC X(4).
           05 WS-RESULT-KBN          PIC X.
           05 WS-KEY-ID              PIC X(16).
           05 WS-FOUND-SW            PIC X VALUE "0".
              88 KEY-FOUND                VALUE "1".
              88 KEY-NOT-FOUND            VALUE "0".
           05 WS-ATTR-OK-SW          PIC X VALUE "0".
              88 ATTR-OK                  VALUE "1".
              88 ATTR-NG                  VALUE "0".

       PROCEDURE DIVISION.
       MAIN-RTN.
           MOVE 0 TO RETURN-CODE
           ACCEPT WS-TODAY FROM DATE YYYYMMDD
           PERFORM INIT-RTN
           IF NO-HARD-ERROR
               PERFORM LOAD-KEY-RTN
           END-IF
           IF NO-HARD-ERROR
               PERFORM CIF-MAIN-RTN
           END-IF
           PERFORM TERM-RTN
           IF HARD-ERROR
               MOVE 8 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF
           GOBACK.

       INIT-RTN.
           DISPLAY "CM200B START"
           OPEN INPUT CMKEYF
           IF FS-CMKEYF NOT = "00"
               DISPLAY "CMKEYF OPEN ERROR " FS-CMKEYF
               MOVE SPACE TO ER-CIF-NO
               MOVE SPACE TO ER-KEY-ID
               MOVE C-ERR-OPEN TO ER-ERROR-CD
               SET HARD-ERROR TO TRUE
           END-IF

           IF NO-HARD-ERROR
               OPEN INPUT CMCIFF
               IF FS-CMCIFF NOT = "00"
                   DISPLAY "CMCIFF OPEN ERROR " FS-CMCIFF
                   MOVE SPACE TO ER-CIF-NO
                   MOVE SPACE TO ER-KEY-ID
                   MOVE C-ERR-OPEN TO ER-ERROR-CD
                   SET HARD-ERROR TO TRUE
               END-IF
           END-IF

           IF NO-HARD-ERROR
               OPEN INPUT CMATTF
               IF FS-CMATTF NOT = "00"
                   DISPLAY "CMATTF OPEN ERROR " FS-CMATTF
                   MOVE SPACE TO ER-CIF-NO
                   MOVE SPACE TO ER-KEY-ID
                   MOVE C-ERR-OPEN TO ER-ERROR-CD
                   SET HARD-ERROR TO TRUE
               END-IF
           END-IF

           IF NO-HARD-ERROR
               OPEN OUTPUT CMRSLF
               IF FS-CMRSLF NOT = "00"
                   DISPLAY "CMRSLF OPEN ERROR " FS-CMRSLF
                   MOVE SPACE TO ER-CIF-NO
                   MOVE SPACE TO ER-KEY-ID
                   MOVE C-ERR-OPEN TO ER-ERROR-CD
                   SET HARD-ERROR TO TRUE
               END-IF
           END-IF

           IF NO-HARD-ERROR
               OPEN OUTPUT CKERRF
               IF FS-CKERRF NOT = "00"
                   DISPLAY "CKERRF OPEN ERROR " FS-CKERRF
                   SET HARD-ERROR TO TRUE
               END-IF
           END-IF

           IF HARD-ERROR AND FS-CKERRF = "00"
               PERFORM WRITE-ERR-ONLY-RTN
           END-IF.

       LOAD-KEY-RTN.
           PERFORM UNTIL CMKEYF-END OR HARD-ERROR
               READ CMKEYF
                   AT END
                       SET CMKEYF-END TO TRUE
                   NOT AT END
                       ADD 1 TO CNT-KEY-READ
                       IF CK-KEY-STATUS-KBN = C-KEY-VALID
                           IF CNT-KEY-TBL >= 50000
                               DISPLAY "KEY TABLE OVERFLOW"
                               MOVE SPACE TO ER-CIF-NO
                               MOVE SPACE TO ER-KEY-ID
                               MOVE C-ERR-TBLOVF TO ER-ERROR-CD
                               PERFORM WRITE-ERR-ONLY-RTN
                               SET HARD-ERROR TO TRUE
                           ELSE
                               ADD 1 TO CNT-KEY-TBL
                               SET KIX TO CNT-KEY-TBL
                               MOVE CK-CIF-NO TO TBL-CIF-NO(KIX)
                               MOVE CK-KEY-ID TO TBL-KEY-ID(KIX)
                               MOVE CK-KEY-STATUS-KBN
                                 TO TBL-KEY-STATUS-KBN(KIX)
                           END-IF
                       END-IF
               END-READ
               IF FS-CMKEYF NOT = "00" AND FS-CMKEYF NOT = "10"
                   DISPLAY "CMKEYF READ ERROR " FS-CMKEYF
                   MOVE CK-CIF-NO TO ER-CIF-NO
                   MOVE CK-KEY-ID TO ER-KEY-ID
                   MOVE C-ERR-READ TO ER-ERROR-CD
                   PERFORM WRITE-ERR-ONLY-RTN
                   SET HARD-ERROR TO TRUE
               END-IF
           END-PERFORM.

       CIF-MAIN-RTN.
           PERFORM UNTIL CMCIFF-END OR HARD-ERROR
               READ CMCIFF
                   AT END
                       SET CMCIFF-END TO TRUE
                   NOT AT END
                       ADD 1 TO CNT-CIF-READ
                       PERFORM JUDGE-CIF-RTN
               END-READ
               IF FS-CMCIFF NOT = "00" AND FS-CMCIFF NOT = "10"
                   DISPLAY "CMCIFF READ ERROR " FS-CMCIFF
                   MOVE CF-CIF-NO TO ER-CIF-NO
                   MOVE SPACE TO ER-KEY-ID
                   MOVE C-ERR-READ TO ER-ERROR-CD
                   PERFORM WRITE-ERR-ONLY-RTN
                   SET HARD-ERROR TO TRUE
               END-IF
           END-PERFORM.

       JUDGE-CIF-RTN.
           MOVE SPACE TO WS-KEY-ID
           MOVE C-RS-EXCLUDE TO WS-RESULT-KBN
           MOVE C-RSN-REQ TO WS-REASON-CD

           IF CF-CIF-STATUS-KBN NOT = C-CIF-VALID
               MOVE C-RSN-CIFSTAT TO WS-REASON-CD
               PERFORM WRITE-RESULT-RTN
           ELSE
               PERFORM SEARCH-KEY-RTN
               IF KEY-FOUND
                   MOVE C-RSN-KEYDUP TO WS-REASON-CD
                   PERFORM WRITE-RESULT-RTN
               ELSE
                   PERFORM CHECK-ATTR-RTN
                   IF NO-HARD-ERROR
                       IF ATTR-OK
                           MOVE C-RS-REQUEST TO WS-RESULT-KBN
                           MOVE C-RSN-REQ TO WS-REASON-CD
                           PERFORM MAKE-REQUEST-KEY-RTN
                       END-IF
                       PERFORM WRITE-RESULT-RTN
                   END-IF
               END-IF
           END-IF.

       SEARCH-KEY-RTN.
           SET KEY-NOT-FOUND TO TRUE
           IF CNT-KEY-TBL > ZERO
               SET KIX TO 1
               SEARCH KEY-TBL
                   AT END
                       CONTINUE
                   WHEN TBL-CIF-NO(KIX) = CF-CIF-NO
                       SET KEY-FOUND TO TRUE
                       MOVE TBL-KEY-ID(KIX) TO WS-KEY-ID
               END-SEARCH
           END-IF.

       CHECK-ATTR-RTN.
           SET ATTR-NG TO TRUE
           MOVE CF-CIF-NO TO CA-CIF-NO
           READ CMATTF KEY IS CA-CIF-NO
               INVALID KEY
                   MOVE C-RSN-NOATTR TO WS-REASON-CD
               NOT INVALID KEY
                   EVALUATE TRUE
                       WHEN CA-ATTR-STATUS-KBN NOT = C-ATTR-VALID
                           MOVE C-RSN-ATTRSTAT TO WS-REASON-CD
                       WHEN CA-KANJI-NAME = SPACE
                           MOVE C-RSN-NAME TO WS-REASON-CD
                       WHEN CA-KANA-NAME = SPACE
                           MOVE C-RSN-NAME TO WS-REASON-CD
                       WHEN CA-ADDR-CD = SPACE
                           MOVE C-RSN-ADDR TO WS-REASON-CD
                       WHEN CA-PHONE-NO = SPACE
                           MOVE C-RSN-PHONE TO WS-REASON-CD
                       WHEN CA-UPDATE-DT = ZERO
                           MOVE C-RSN-DATE TO WS-REASON-CD
                       WHEN OTHER
                           SET ATTR-OK TO TRUE
                   END-EVALUATE
           END-READ

           IF FS-CMATTF NOT = "00" AND FS-CMATTF NOT = "23"
               DISPLAY "CMATTF READ ERROR " FS-CMATTF
               MOVE CF-CIF-NO TO ER-CIF-NO
               MOVE SPACE TO ER-KEY-ID
               MOVE C-ERR-READ TO ER-ERROR-CD
               PERFORM WRITE-ERR-ONLY-RTN
               SET HARD-ERROR TO TRUE
           END-IF.

       MAKE-REQUEST-KEY-RTN.
           STRING "RQ" CF-CIF-NO WS-TODAY(3:4)
               DELIMITED BY SIZE
               INTO WS-KEY-ID
           END-STRING.

       WRITE-RESULT-RTN.
           IF HARD-ERROR
               EXIT PARAGRAPH
           END-IF

           ADD 1 TO CNT-RSL-SEQ
           MOVE CNT-RSL-SEQ TO RS-RESULT-ID
           MOVE CF-CIF-NO TO RS-CIF-NO
           MOVE WS-KEY-ID TO RS-KEY-ID
           MOVE WS-RESULT-KBN TO RS-RESULT-KBN
           MOVE WS-REASON-CD TO RS-REASON-CD
           MOVE WS-TODAY TO RS-OUTPUT-DT

           WRITE CMRSLF-REC
           IF FS-CMRSLF NOT = "00"
               DISPLAY "CMRSLF WRITE ERROR " FS-CMRSLF
               MOVE CF-CIF-NO TO ER-CIF-NO
               MOVE WS-KEY-ID TO ER-KEY-ID
               MOVE C-ERR-WRITE TO ER-ERROR-CD
               PERFORM WRITE-ERR-ONLY-RTN
               SET HARD-ERROR TO TRUE
           ELSE
               IF WS-RESULT-KBN = C-RS-REQUEST
                   ADD 1 TO CNT-REQ-OUT
               ELSE
                   ADD 1 TO CNT-EXC-OUT
               END-IF
           END-IF.

       WRITE-ERR-ONLY-RTN.
           IF FS-CKERRF = "00"
               ADD 1 TO CNT-ERR-SEQ
               MOVE CNT-ERR-SEQ TO ER-ERROR-ID
               MOVE C-PGM-ID TO ER-SOURCE-PGM-ID
               MOVE WS-TODAY TO ER-ERROR-DT
               WRITE CKERRF-REC
               IF FS-CKERRF NOT = "00"
                   DISPLAY "CKERRF WRITE ERROR " FS-CKERRF
                   SET HARD-ERROR TO TRUE
               ELSE
                   ADD 1 TO CNT-ERR-OUT
               END-IF
           END-IF.

       TERM-RTN.
           IF FS-CMKEYF NOT = SPACE
               CLOSE CMKEYF
           END-IF
           IF FS-CMCIFF NOT = SPACE
               CLOSE CMCIFF
           END-IF
           IF FS-CMATTF NOT = SPACE
               CLOSE CMATTF
           END-IF
           IF FS-CMRSLF NOT = SPACE
               CLOSE CMRSLF
           END-IF
           IF FS-CKERRF NOT = SPACE
               CLOSE CKERRF
           END-IF

           DISPLAY "CM200B END CIF READ " CNT-CIF-READ
           DISPLAY "CM200B END KEY READ " CNT-KEY-READ
           DISPLAY "CM200B END REQUEST " CNT-REQ-OUT
           DISPLAY "CM200B END EXCLUDE " CNT-EXC-OUT
           DISPLAY "CM200B END ERROR " CNT-ERR-OUT.
