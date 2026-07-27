       IDENTIFICATION DIVISION.
       PROGRAM-ID. CK220B.
       AUTHOR. SYSTEM.
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.
      *
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CMKEYF ASSIGN TO "CMKEYF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS FS-CMKEYF.
           SELECT CMCIFF ASSIGN TO "CMCIFF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS FS-CMCIFF.
           SELECT CMRSLF ASSIGN TO "CMRSLF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS FS-CMRSLF.
           SELECT CKLNKF ASSIGN TO "CKLNKF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS LK-LINK-ID
               FILE STATUS IS FS-CKLNKF.
           SELECT CKERRF ASSIGN TO "CKERRF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS FS-CKERRF.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  CMKEYF.
           COPY CMKEYFC.
      *
       FD  CMCIFF.
           COPY CMCIFFC.
      *
       FD  CMRSLF.
           COPY CMRSLC.
      *
       FD  CKLNKF.
           COPY CKLNKC.
      *
       FD  CKERRF.
           COPY CKERRC.
      *
       WORKING-STORAGE SECTION.
      *
       01  FS-AREA.
           05 FS-CMKEYF                 PIC XX.
           05 FS-CMCIFF                 PIC XX.
           05 FS-CMRSLF                 PIC XX.
           05 FS-CKLNKF                 PIC XX.
           05 FS-CKERRF                 PIC XX.
      *
       01  EOF-AREA.
           05 EOF-CMKEYF                PIC X VALUE "N".
              88 CMKEYF-END                  VALUE "Y".
           05 EOF-CMCIFF                PIC X VALUE "N".
              88 CMCIFF-END                  VALUE "Y".
           05 EOF-CMRSLF                PIC X VALUE "N".
              88 CMRSLF-END                  VALUE "Y".
      *
       01  WK-DATE-AREA.
           05 WK-CURRENT-DATE           PIC X(21).
           05 WK-YYYYMMDD               PIC 9(8).
      *
       01  WK-COUNT-AREA.
           05 CNT-READ-KEY              PIC 9(9) VALUE ZERO.
           05 CNT-WRITE-LINK            PIC 9(9) VALUE ZERO.
           05 CNT-WRITE-ERR             PIC 9(9) VALUE ZERO.
           05 CNT-SKIP                  PIC 9(9) VALUE ZERO.
           05 CNT-LINK-SEQ              PIC 9(6) VALUE ZERO.
           05 CNT-ERR-SEQ               PIC 9(6) VALUE ZERO.
      *
       01  WK-SW-AREA.
           05 WK-CIF-MATCH              PIC X VALUE "N".
              88 CIF-MATCH                   VALUE "Y".
           05 WK-RSL-MATCH              PIC X VALUE "N".
              88 RSL-MATCH                   VALUE "Y".
           05 WK-CIF-VALID              PIC X VALUE "N".
              88 CIF-FORMAT-OK               VALUE "Y".
           05 WK-SEND-OK                PIC X VALUE "N".
              88 SEND-OK                     VALUE "Y".
      *
       01  WK-CODE-AREA.
           05 WK-ERROR-CD               PIC X(6).
           05 WK-TARGET-SYS-ID          PIC X(4).
           05 WK-LINK-ID                PIC X(16).
           05 WK-ERROR-ID               PIC X(16).
      *
       01  CM190-AREA.
           05 CM190-CIF-NO              PIC X(12).
           05 CM190-RESULT-CD           PIC XX.
           05 CM190-REASON-CD           PIC X(6).
      *
       01  CONST-AREA.
           05 CN-PGM-ID                 PIC X(8) VALUE "CK220B".
           05 CN-CIF-ACTIVE             PIC XX   VALUE "01".
           05 CN-KEY-ACTIVE             PIC XX   VALUE "01".
           05 CN-RSL-OK                 PIC XX   VALUE "01".
           05 CN-RSL-HOLD               PIC XX   VALUE "08".
           05 CN-RSL-REJECT             PIC XX   VALUE "09".
           05 CN-SEND-READY             PIC XX   VALUE "01".
           05 CN-TARGET-CORE            PIC X(4) VALUE "CORE".
           05 CN-TARGET-INFO            PIC X(4) VALUE "INFO".
           05 CN-TARGET-RISK            PIC X(4) VALUE "RISK".
      *
       PROCEDURE DIVISION.
      *
       0000-MAIN SECTION.
           PERFORM 1000-INIT
           PERFORM 2000-MAIN-PROC UNTIL CMKEYF-END
           PERFORM 9000-END
           GOBACK.
      *
       1000-INIT SECTION.
           MOVE FUNCTION CURRENT-DATE TO WK-CURRENT-DATE
           MOVE WK-CURRENT-DATE(1:8) TO WK-YYYYMMDD
           MOVE 0 TO RETURN-CODE
      *
           OPEN INPUT  CMKEYF
                       CMCIFF
                       CMRSLF
                OUTPUT CKERRF
                I-O    CKLNKF
      *
           IF FS-CMKEYF NOT = "00"
              DISPLAY "CMKEYF OPEN ERROR ST=" FS-CMKEYF
              MOVE 12 TO RETURN-CODE
              GOBACK
           END-IF
           IF FS-CMCIFF NOT = "00"
              DISPLAY "CMCIFF OPEN ERROR ST=" FS-CMCIFF
              MOVE 12 TO RETURN-CODE
              GOBACK
           END-IF
           IF FS-CMRSLF NOT = "00"
              DISPLAY "CMRSLF OPEN ERROR ST=" FS-CMRSLF
              MOVE 12 TO RETURN-CODE
              GOBACK
           END-IF
           IF FS-CKERRF NOT = "00"
              DISPLAY "CKERRF OPEN ERROR ST=" FS-CKERRF
              MOVE 12 TO RETURN-CODE
              GOBACK
           END-IF
           IF FS-CKLNKF NOT = "00"
              DISPLAY "CKLNKF OPEN ERROR ST=" FS-CKLNKF
              MOVE 12 TO RETURN-CODE
              GOBACK
           END-IF
      *
           PERFORM 1100-READ-KEY
           PERFORM 1200-READ-CIF
           PERFORM 1300-READ-RSL.
      *
       1100-READ-KEY SECTION.
           READ CMKEYF
              AT END
                 SET CMKEYF-END TO TRUE
              NOT AT END
                 ADD 1 TO CNT-READ-KEY
           END-READ
           IF FS-CMKEYF NOT = "00" AND FS-CMKEYF NOT = "10"
              DISPLAY "CMKEYF READ ERROR ST=" FS-CMKEYF
              MOVE 12 TO RETURN-CODE
              GOBACK
           END-IF.
      *
       1200-READ-CIF SECTION.
           READ CMCIFF
              AT END
                 SET CMCIFF-END TO TRUE
              NOT AT END
                 CONTINUE
           END-READ
           IF FS-CMCIFF NOT = "00" AND FS-CMCIFF NOT = "10"
              DISPLAY "CMCIFF READ ERROR ST=" FS-CMCIFF
              MOVE 12 TO RETURN-CODE
              GOBACK
           END-IF.
      *
       1300-READ-RSL SECTION.
           READ CMRSLF
              AT END
                 SET CMRSLF-END TO TRUE
              NOT AT END
                 CONTINUE
           END-READ
           IF FS-CMRSLF NOT = "00" AND FS-CMRSLF NOT = "10"
              DISPLAY "CMRSLF READ ERROR ST=" FS-CMRSLF
              MOVE 12 TO RETURN-CODE
              GOBACK
           END-IF.
      *
       2000-MAIN-PROC SECTION.
           MOVE "N" TO WK-SEND-OK
           MOVE "N" TO WK-CIF-MATCH
           MOVE "N" TO WK-RSL-MATCH
           MOVE SPACE TO WK-ERROR-CD
      *
           PERFORM 2100-LOCATE-CIF
           PERFORM 2200-LOCATE-RSL
           PERFORM 2300-JUDGE-KEY
      *
           IF SEND-OK
              PERFORM 3000-WRITE-LINKS
           ELSE
              ADD 1 TO CNT-SKIP
              IF WK-ERROR-CD NOT = SPACE
                 PERFORM 4000-WRITE-ERROR
              END-IF
           END-IF
      *
           PERFORM 1100-READ-KEY.
      *
       2100-LOCATE-CIF SECTION.
           PERFORM UNTIL CMCIFF-END OR CF-CIF-NO >= CK-CIF-NO
              PERFORM 1200-READ-CIF
           END-PERFORM
           IF NOT CMCIFF-END AND CF-CIF-NO = CK-CIF-NO
              SET CIF-MATCH TO TRUE
           END-IF.
      *
       2200-LOCATE-RSL SECTION.
           PERFORM UNTIL CMRSLF-END
                    OR RS-CIF-NO > CK-CIF-NO
                    OR (RS-CIF-NO = CK-CIF-NO
                    AND RS-KEY-ID >= CK-KEY-ID)
              PERFORM 1300-READ-RSL
           END-PERFORM
           IF NOT CMRSLF-END
              IF RS-CIF-NO = CK-CIF-NO
                 AND RS-KEY-ID = CK-KEY-ID
                 SET RSL-MATCH TO TRUE
              END-IF
           END-IF.
      *
       2300-JUDGE-KEY SECTION.
           IF CK-KEY-STATUS-KBN NOT = CN-KEY-ACTIVE
              MOVE "E2101" TO WK-ERROR-CD
              EXIT SECTION
           END-IF
      *
           IF NOT CIF-MATCH
              MOVE "E2201" TO WK-ERROR-CD
              EXIT SECTION
           END-IF
      *
           IF CF-CIF-STATUS-KBN NOT = CN-CIF-ACTIVE
              MOVE "E2202" TO WK-ERROR-CD
              EXIT SECTION
           END-IF
      *
           PERFORM 2400-CHECK-CIF-FORMAT
           IF NOT CIF-FORMAT-OK
              MOVE "E2203" TO WK-ERROR-CD
              EXIT SECTION
           END-IF
      *
           IF NOT RSL-MATCH
              MOVE "E2301" TO WK-ERROR-CD
              EXIT SECTION
           END-IF
      *
           EVALUATE RS-RESULT-KBN
              WHEN CN-RSL-OK
                 SET SEND-OK TO TRUE
              WHEN CN-RSL-HOLD
                 MOVE "E2308" TO WK-ERROR-CD
              WHEN CN-RSL-REJECT
                 MOVE "E2309" TO WK-ERROR-CD
              WHEN OTHER
                 MOVE "E2399" TO WK-ERROR-CD
           END-EVALUATE.
      *
       2400-CHECK-CIF-FORMAT SECTION.
           MOVE "N" TO WK-CIF-VALID
           INITIALIZE CM190-AREA
           MOVE CK-CIF-NO TO CM190-CIF-NO
           CALL "CM190S" USING CM190-AREA
              ON EXCEPTION
                 DISPLAY "CM190S CALL ERROR"
                 MOVE 12 TO RETURN-CODE
                 GOBACK
           END-CALL
           IF CM190-RESULT-CD = "00"
              SET CIF-FORMAT-OK TO TRUE
           END-IF.
      *
       3000-WRITE-LINKS SECTION.
           MOVE CN-TARGET-CORE TO WK-TARGET-SYS-ID
           PERFORM 3100-WRITE-LINK
           MOVE CN-TARGET-INFO TO WK-TARGET-SYS-ID
           PERFORM 3100-WRITE-LINK
           MOVE CN-TARGET-RISK TO WK-TARGET-SYS-ID
           PERFORM 3100-WRITE-LINK.
      *
       3100-WRITE-LINK SECTION.
           ADD 1 TO CNT-LINK-SEQ
           INITIALIZE CKLNKF-REC
           STRING "LK" WK-YYYYMMDD CNT-LINK-SEQ
              DELIMITED BY SIZE INTO WK-LINK-ID
           END-STRING
           MOVE WK-LINK-ID       TO LK-LINK-ID
           MOVE CK-KEY-ID        TO LK-KEY-ID
           MOVE CK-CIF-NO        TO LK-CIF-NO
           MOVE WK-TARGET-SYS-ID TO LK-TARGET-SYS-ID
           MOVE CN-SEND-READY    TO LK-SEND-STATUS-KBN
           MOVE WK-YYYYMMDD      TO LK-SEND-DT
      *
           WRITE CKLNKF-REC
              INVALID KEY
                 DISPLAY "CKLNKF WRITE INVALID KEY"
                 DISPLAY "CKLNKF LINK ID=" LK-LINK-ID
                 DISPLAY "CKLNKF ST=" FS-CKLNKF
                 MOVE 12 TO RETURN-CODE
                 GOBACK
           END-WRITE
           IF FS-CKLNKF NOT = "00"
              DISPLAY "CKLNKF WRITE ERROR ST=" FS-CKLNKF
              MOVE 12 TO RETURN-CODE
              GOBACK
           END-IF
           ADD 1 TO CNT-WRITE-LINK.
      *
       4000-WRITE-ERROR SECTION.
           ADD 1 TO CNT-ERR-SEQ
           INITIALIZE CKERRF-REC
           STRING "ER" WK-YYYYMMDD CNT-ERR-SEQ
              DELIMITED BY SIZE INTO WK-ERROR-ID
           END-STRING
           MOVE WK-ERROR-ID  TO ER-ERROR-ID
           MOVE CN-PGM-ID    TO ER-SOURCE-PGM-ID
           MOVE CK-CIF-NO    TO ER-CIF-NO
           MOVE CK-KEY-ID    TO ER-KEY-ID
           MOVE WK-ERROR-CD  TO ER-ERROR-CD
           MOVE WK-YYYYMMDD  TO ER-ERROR-DT
      *
           WRITE CKERRF-REC
           IF FS-CKERRF NOT = "00"
              DISPLAY "CKERRF WRITE ERROR ST=" FS-CKERRF
              MOVE 12 TO RETURN-CODE
              GOBACK
           END-IF
           ADD 1 TO CNT-WRITE-ERR.
      *
       9000-END SECTION.
           CLOSE CMKEYF CMCIFF CMRSLF CKLNKF CKERRF
           IF FS-CMKEYF NOT = "00"
              DISPLAY "CMKEYF CLOSE ERROR ST=" FS-CMKEYF
              MOVE 8 TO RETURN-CODE
           END-IF
           IF FS-CMCIFF NOT = "00"
              DISPLAY "CMCIFF CLOSE ERROR ST=" FS-CMCIFF
              MOVE 8 TO RETURN-CODE
           END-IF
           IF FS-CMRSLF NOT = "00"
              DISPLAY "CMRSLF CLOSE ERROR ST=" FS-CMRSLF
              MOVE 8 TO RETURN-CODE
           END-IF
           IF FS-CKLNKF NOT = "00"
              DISPLAY "CKLNKF CLOSE ERROR ST=" FS-CKLNKF
              MOVE 8 TO RETURN-CODE
           END-IF
           IF FS-CKERRF NOT = "00"
              DISPLAY "CKERRF CLOSE ERROR ST=" FS-CKERRF
              MOVE 8 TO RETURN-CODE
           END-IF
      *
           DISPLAY "CK220B READ COUNT=" CNT-READ-KEY
           DISPLAY "CK220B LINK COUNT=" CNT-WRITE-LINK
           DISPLAY "CK220B ERROR COUNT=" CNT-WRITE-ERR
           DISPLAY "CK220B SKIP COUNT=" CNT-SKIP
           IF RETURN-CODE NOT = 8
              MOVE 0 TO RETURN-CODE
           END-IF.
