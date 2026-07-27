       IDENTIFICATION DIVISION.
       PROGRAM-ID. CK260B.
      *----------------------------------------------------------------*
      * INTEGRATED KEY DAILY DIFFERENCE SEND BATCH                     *
      * SENDS CONFIRMED NEW/DIFFERENCE LINK RECORDS AND WRITES REPORTS.*
      * CHECK DIGIT COUNT IS A TRANSFER ITEM AND IS NOT USED TO JUDGE. *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CKLNKF ASSIGN TO "CKLNKF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS LK-LINK-ID
               FILE STATUS IS FS-CKLNKF.
           SELECT CMKEYF ASSIGN TO "CMKEYF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-CMKEYF.
           SELECT CMRSLF ASSIGN TO "CMRSLF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-CMRSLF.
           SELECT CKRPTF ASSIGN TO "CKRPTF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-CKRPTF.
           SELECT CKERRF ASSIGN TO "CKERRF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-CKERRF.

       DATA DIVISION.
       FILE SECTION.
       FD  CKLNKF.
           COPY CKLNKC.

       FD  CMKEYF.
           COPY CMKEYFC.

       FD  CMRSLF.
           COPY CMRSLC.

       FD  CKRPTF.
           COPY CKRPTC.

       FD  CKERRF.
           COPY CKERRC.

       WORKING-STORAGE SECTION.
       01  WS-PGM-ID                  PIC X(08) VALUE "CK260B".
       01  WS-ABEND-SW                PIC X(01) VALUE SPACE.
           88  ABEND-ON                         VALUE "1".
       01  WS-EOF-SW.
           05  WS-EOF-CKLNKF          PIC X(01) VALUE SPACE.
               88 EOF-CKLNKF                    VALUE "1".
           05  WS-EOF-CMKEYF          PIC X(01) VALUE SPACE.
               88 EOF-CMKEYF                    VALUE "1".
           05  WS-EOF-CMRSLF          PIC X(01) VALUE SPACE.
               88 EOF-CMRSLF                    VALUE "1".

       01  WS-FILE-STATUS.
           05  FS-CKLNKF              PIC X(02) VALUE "00".
           05  FS-CMKEYF              PIC X(02) VALUE "00".
           05  FS-CMRSLF              PIC X(02) VALUE "00".
           05  FS-CKRPTF              PIC X(02) VALUE "00".
           05  FS-CKERRF              PIC X(02) VALUE "00".

       01  WS-DATE-AREA.
           05  WS-CURRENT-DATE        PIC X(21).
           05  WS-YYYYMMDD            PIC 9(08).
           05  WS-YYYYMM              PIC 9(06).

       01  WS-COUNTERS.
           05  CNT-LINK-READ          PIC 9(09) VALUE ZERO.
           05  CNT-KEY-READ           PIC 9(09) VALUE ZERO.
           05  CNT-RSL-READ           PIC 9(09) VALUE ZERO.
           05  CNT-SEND               PIC 9(09) VALUE ZERO.
           05  CNT-HOLD               PIC 9(09) VALUE ZERO.
           05  CNT-ERR                PIC 9(09) VALUE ZERO.
           05  CNT-SKIP               PIC 9(09) VALUE ZERO.
           05  CNT-RPT-LINE           PIC 9(05) VALUE ZERO.
           05  CNT-ERR-LINE           PIC 9(05) VALUE ZERO.

       01  WS-WORK.
           05  IX-CK                  PIC 9(05) COMP VALUE ZERO.
           05  IX-RS                  PIC 9(05) COMP VALUE ZERO.
           05  WS-CK-IDX              PIC 9(05) COMP VALUE ZERO.
           05  WS-KEY-ID              PIC X(20) VALUE SPACE.
           05  WS-CIF-NO              PIC X(15) VALUE SPACE.
           05  WS-RESULT-KBN          PIC X(02) VALUE SPACE.
           05  WS-REASON-CD           PIC X(04) VALUE SPACE.
           05  WS-KEY-FOUND-SW        PIC X(01) VALUE SPACE.
               88 KEY-FOUND                     VALUE "1".
           05  WS-RSL-FOUND-SW        PIC X(01) VALUE SPACE.
               88 RSL-FOUND                     VALUE "1".
           05  WS-HOLD-CD             PIC X(04) VALUE SPACE.
           05  WS-HOLD-TEXT           PIC X(80) VALUE SPACE.
           05  WS-HOLD-REASON         PIC X(80) VALUE SPACE.
           05  WS-RPT-ID-N            PIC 9(09) VALUE ZERO.
           05  WS-ERR-ID-N            PIC 9(09) VALUE ZERO.

       01  WS-CMKEY-TABLE.
           05  WS-CMKEY-CNT           PIC 9(05) COMP VALUE ZERO.
           05  WS-CMKEY-ENTRY OCCURS 20000 TIMES.
               10  T-CK-KEY-ID        PIC X(20).
               10  T-CK-CIF-NO        PIC X(15).
               10  T-CK-KEY-STATUS    PIC X(02).

       01  WS-CMRSL-TABLE.
           05  WS-CMRSL-CNT           PIC 9(05) COMP VALUE ZERO.
           05  WS-CMRSL-ENTRY OCCURS 20000 TIMES.
               10  T-RS-CIF-NO        PIC X(15).
               10  T-RS-KEY-ID        PIC X(20).
               10  T-RS-RESULT-KBN    PIC X(02).
               10  T-RS-REASON-CD     PIC X(04).
               10  T-RS-OUTPUT-DT     PIC 9(08).

       PROCEDURE DIVISION.
       MAIN-SECTION.
           PERFORM 1000-INIT
           IF NOT ABEND-ON
               PERFORM 2000-LOAD-CMKEYF
           END-IF
           IF NOT ABEND-ON
               PERFORM 3000-LOAD-CMRSLF
           END-IF
           IF NOT ABEND-ON
               PERFORM 4000-PROCESS-LINK
           END-IF
           IF NOT ABEND-ON
               PERFORM 8000-WRITE-SUMMARY
           END-IF
           PERFORM 9000-FINAL
           GOBACK.

       1000-INIT.
           MOVE 0 TO RETURN-CODE
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE
           MOVE WS-CURRENT-DATE(1:8) TO WS-YYYYMMDD
           MOVE WS-CURRENT-DATE(1:6) TO WS-YYYYMM

           OPEN INPUT CMKEYF CMRSLF
           IF FS-CMKEYF NOT = "00"
               DISPLAY "CMKEYF OPEN ERROR ST=" FS-CMKEYF
               PERFORM 9900-SET-ABEND
           END-IF
           IF FS-CMRSLF NOT = "00"
               DISPLAY "CMRSLF OPEN ERROR ST=" FS-CMRSLF
               PERFORM 9900-SET-ABEND
           END-IF

           OPEN I-O CKLNKF
           IF FS-CKLNKF NOT = "00"
               DISPLAY "CKLNKF OPEN ERROR ST=" FS-CKLNKF
               PERFORM 9900-SET-ABEND
           END-IF

           OPEN OUTPUT CKRPTF CKERRF
           IF FS-CKRPTF NOT = "00"
               DISPLAY "CKRPTF OPEN ERROR ST=" FS-CKRPTF
               PERFORM 9900-SET-ABEND
           END-IF
           IF FS-CKERRF NOT = "00"
               DISPLAY "CKERRF OPEN ERROR ST=" FS-CKERRF
               PERFORM 9900-SET-ABEND
           END-IF

           IF NOT ABEND-ON
               MOVE "PROCESS START" TO WS-HOLD-TEXT
               PERFORM 8200-WRITE-RPT
           END-IF.

       2000-LOAD-CMKEYF.
           PERFORM UNTIL EOF-CMKEYF OR ABEND-ON
               READ CMKEYF
                   AT END
                       SET EOF-CMKEYF TO TRUE
                   NOT AT END
                       ADD 1 TO CNT-KEY-READ
                       IF WS-CMKEY-CNT >= 20000
                           DISPLAY "CMKEYF TABLE OVERFLOW"
                           PERFORM 9900-SET-ABEND
                       ELSE
                           ADD 1 TO WS-CMKEY-CNT
                           MOVE CK-KEY-ID
                             TO T-CK-KEY-ID(WS-CMKEY-CNT)
                           MOVE CK-CIF-NO
                             TO T-CK-CIF-NO(WS-CMKEY-CNT)
                           MOVE CK-KEY-STATUS-KBN
                             TO T-CK-KEY-STATUS(WS-CMKEY-CNT)
                       END-IF
               END-READ
               IF FS-CMKEYF NOT = "00" AND FS-CMKEYF NOT = "10"
                   DISPLAY "CMKEYF READ ERROR ST=" FS-CMKEYF
                   PERFORM 9900-SET-ABEND
               END-IF
           END-PERFORM.

       3000-LOAD-CMRSLF.
           PERFORM UNTIL EOF-CMRSLF OR ABEND-ON
               READ CMRSLF
                   AT END
                       SET EOF-CMRSLF TO TRUE
                   NOT AT END
                       ADD 1 TO CNT-RSL-READ
                       IF WS-CMRSL-CNT >= 20000
                           DISPLAY "CMRSLF TABLE OVERFLOW"
                           PERFORM 9900-SET-ABEND
                       ELSE
                           ADD 1 TO WS-CMRSL-CNT
                           MOVE RS-CIF-NO
                             TO T-RS-CIF-NO(WS-CMRSL-CNT)
                           MOVE RS-KEY-ID
                             TO T-RS-KEY-ID(WS-CMRSL-CNT)
                           MOVE RS-RESULT-KBN
                             TO T-RS-RESULT-KBN(WS-CMRSL-CNT)
                           MOVE RS-REASON-CD
                             TO T-RS-REASON-CD(WS-CMRSL-CNT)
                           MOVE RS-OUTPUT-DT
                             TO T-RS-OUTPUT-DT(WS-CMRSL-CNT)
                       END-IF
               END-READ
               IF FS-CMRSLF NOT = "00" AND FS-CMRSLF NOT = "10"
                   DISPLAY "CMRSLF READ ERROR ST=" FS-CMRSLF
                   PERFORM 9900-SET-ABEND
               END-IF
           END-PERFORM.

       4000-PROCESS-LINK.
           MOVE LOW-VALUES TO LK-LINK-ID
           START CKLNKF KEY IS NOT LESS THAN LK-LINK-ID
           IF FS-CKLNKF NOT = "00"
               IF FS-CKLNKF = "23"
                   SET EOF-CKLNKF TO TRUE
               ELSE
                   DISPLAY "CKLNKF START ERROR ST=" FS-CKLNKF
                   PERFORM 9900-SET-ABEND
               END-IF
           END-IF

           PERFORM UNTIL EOF-CKLNKF OR ABEND-ON
               READ CKLNKF NEXT RECORD
                   AT END
                       SET EOF-CKLNKF TO TRUE
                   NOT AT END
                       ADD 1 TO CNT-LINK-READ
                       PERFORM 4100-JUDGE-LINK
               END-READ
               IF FS-CKLNKF NOT = "00" AND FS-CKLNKF NOT = "10"
                   DISPLAY "CKLNKF READ ERROR ST=" FS-CKLNKF
                   PERFORM 9900-SET-ABEND
               END-IF
           END-PERFORM.

       4100-JUDGE-LINK.
           MOVE SPACE TO WS-HOLD-CD
           MOVE SPACE TO WS-HOLD-TEXT
           MOVE SPACE TO WS-HOLD-REASON
           MOVE LK-KEY-ID TO WS-KEY-ID
           MOVE LK-CIF-NO TO WS-CIF-NO

           IF LK-SEND-STATUS-KBN = "9"
               ADD 1 TO CNT-SKIP
           ELSE
               PERFORM 4200-FIND-CMKEY
               PERFORM 4300-FIND-CMRSL
               EVALUATE TRUE
                   WHEN LK-KEY-ID = SPACE
                       MOVE "E001" TO WS-HOLD-CD
                       MOVE "KEY-ID NOT SET" TO WS-HOLD-REASON
                       PERFORM 4600-HOLD-LINK
                   WHEN LK-CIF-NO = SPACE
                       MOVE "E002" TO WS-HOLD-CD
                       MOVE "CIF-NO NOT SET" TO WS-HOLD-REASON
                       PERFORM 4600-HOLD-LINK
                   WHEN LK-TARGET-SYS-ID = SPACE
                       MOVE "E003" TO WS-HOLD-CD
                       MOVE "TARGET SYSTEM NOT SET" TO WS-HOLD-REASON
                       PERFORM 4600-HOLD-LINK
                   WHEN NOT KEY-FOUND
                       MOVE "E101" TO WS-HOLD-CD
                       MOVE "KEY MASTER NOT FOUND" TO WS-HOLD-REASON
                       PERFORM 4600-HOLD-LINK
                   WHEN T-CK-CIF-NO(WS-CK-IDX) NOT = LK-CIF-NO
                       MOVE "E102" TO WS-HOLD-CD
                       MOVE "KEY MASTER CIF MISMATCH" TO WS-HOLD-REASON
                       PERFORM 4600-HOLD-LINK
                   WHEN T-CK-KEY-STATUS(WS-CK-IDX) NOT = "01"
                       MOVE "H101" TO WS-HOLD-CD
                       MOVE "KEY NOT CONFIRMED" TO WS-HOLD-REASON
                       PERFORM 4600-HOLD-LINK
                   WHEN NOT RSL-FOUND
                       MOVE "H201" TO WS-HOLD-CD
                       MOVE "RESULT NOT ARRIVED" TO WS-HOLD-REASON
                       PERFORM 4600-HOLD-LINK
                   WHEN WS-RESULT-KBN NOT = "01"
                       MOVE "H202" TO WS-HOLD-CD
                       MOVE "RESULT NOT CONFIRMED" TO WS-HOLD-REASON
                       PERFORM 4600-HOLD-LINK
                   WHEN LK-SEND-STATUS-KBN = "0"
                       PERFORM 4500-SEND-LINK
                   WHEN LK-SEND-STATUS-KBN = "2"
                       PERFORM 4500-SEND-LINK
                   WHEN OTHER
                       MOVE "E301" TO WS-HOLD-CD
                       MOVE "INVALID SEND STATUS" TO WS-HOLD-REASON
                       PERFORM 4600-HOLD-LINK
               END-EVALUATE
           END-IF.

       4200-FIND-CMKEY.
           MOVE SPACE TO WS-KEY-FOUND-SW
           MOVE ZERO TO WS-CK-IDX
           MOVE 1 TO IX-CK
           PERFORM UNTIL IX-CK > WS-CMKEY-CNT OR KEY-FOUND
               IF T-CK-KEY-ID(IX-CK) = LK-KEY-ID
                   SET KEY-FOUND TO TRUE
                   MOVE IX-CK TO WS-CK-IDX
               ELSE
                   ADD 1 TO IX-CK
               END-IF
           END-PERFORM.

       4300-FIND-CMRSL.
           MOVE SPACE TO WS-RSL-FOUND-SW
           MOVE SPACE TO WS-RESULT-KBN
           MOVE SPACE TO WS-REASON-CD
           MOVE 1 TO IX-RS
           PERFORM UNTIL IX-RS > WS-CMRSL-CNT OR RSL-FOUND
               IF T-RS-CIF-NO(IX-RS) = LK-CIF-NO
                  AND T-RS-KEY-ID(IX-RS) = LK-KEY-ID
                  AND T-RS-OUTPUT-DT(IX-RS) = WS-YYYYMMDD
                   SET RSL-FOUND TO TRUE
                   MOVE T-RS-RESULT-KBN(IX-RS) TO WS-RESULT-KBN
                   MOVE T-RS-REASON-CD(IX-RS) TO WS-REASON-CD
               ELSE
                   ADD 1 TO IX-RS
               END-IF
           END-PERFORM.

       4500-SEND-LINK.
           MOVE "9" TO LK-SEND-STATUS-KBN
           MOVE WS-YYYYMMDD TO LK-SEND-DT
           REWRITE CKLNKF-REC
           IF FS-CKLNKF = "00"
               ADD 1 TO CNT-SEND
               STRING "SEND LINK=" LK-LINK-ID
                      " KEY=" LK-KEY-ID
                      " CIF=" LK-CIF-NO
                      " SYS=" LK-TARGET-SYS-ID
                 DELIMITED BY SIZE INTO WS-HOLD-TEXT
               PERFORM 8200-WRITE-RPT
           ELSE
               DISPLAY "CKLNKF REWRITE ERROR ST=" FS-CKLNKF
               MOVE "E901" TO WS-HOLD-CD
               MOVE "SEND STATUS UPDATE ERROR" TO WS-HOLD-REASON
               PERFORM 4700-WRITE-ERROR
               PERFORM 9900-SET-ABEND
           END-IF.

       4600-HOLD-LINK.
           ADD 1 TO CNT-HOLD
           STRING "HOLD " WS-HOLD-CD
                  " LINK=" LK-LINK-ID
                  " KEY=" LK-KEY-ID
                  " CIF=" LK-CIF-NO
                  " REASON=" WS-HOLD-REASON
             DELIMITED BY SIZE INTO WS-HOLD-TEXT
           PERFORM 8200-WRITE-RPT
           PERFORM 4700-WRITE-ERROR.

       4700-WRITE-ERROR.
           ADD 1 TO CNT-ERR
           ADD 1 TO CNT-ERR-LINE
           COMPUTE WS-ERR-ID-N = WS-YYYYMMDD * 100000 + CNT-ERR-LINE
           INITIALIZE CKERRF-REC
           MOVE WS-ERR-ID-N TO ER-ERROR-ID
           MOVE WS-PGM-ID TO ER-SOURCE-PGM-ID
           MOVE WS-CIF-NO TO ER-CIF-NO
           MOVE WS-KEY-ID TO ER-KEY-ID
           MOVE WS-HOLD-CD TO ER-ERROR-CD
           MOVE WS-YYYYMMDD TO ER-ERROR-DT
           WRITE CKERRF-REC
           IF FS-CKERRF NOT = "00"
               DISPLAY "CKERRF WRITE ERROR ST=" FS-CKERRF
               PERFORM 9900-SET-ABEND
           END-IF.

       8000-WRITE-SUMMARY.
           MOVE "PROCESS SUMMARY" TO WS-HOLD-TEXT
           PERFORM 8200-WRITE-RPT
           STRING "CKLNKF READ=" CNT-LINK-READ
                  " CMKEYF READ=" CNT-KEY-READ
                  " CMRSLF READ=" CNT-RSL-READ
             DELIMITED BY SIZE INTO WS-HOLD-TEXT
           PERFORM 8200-WRITE-RPT
           STRING "SEND=" CNT-SEND
                  " HOLD=" CNT-HOLD
                  " SENT-SKIP=" CNT-SKIP
                  " ERROR=" CNT-ERR
             DELIMITED BY SIZE INTO WS-HOLD-TEXT
           PERFORM 8200-WRITE-RPT.

       8200-WRITE-RPT.
           ADD 1 TO CNT-RPT-LINE
           COMPUTE WS-RPT-ID-N = WS-YYYYMMDD * 100000 + CNT-RPT-LINE
           INITIALIZE CKRPTF-REC
           MOVE WS-RPT-ID-N TO RP-REPORT-ID
           MOVE WS-YYYYMM TO RP-REPORT-YYYYMM
           MOVE CNT-RPT-LINE TO RP-LINE-NO
           IF CNT-RPT-LINE = 1
               MOVE "H" TO RP-SECTION-KBN
           ELSE
               MOVE "D" TO RP-SECTION-KBN
           END-IF
           MOVE WS-HOLD-TEXT TO RP-REPORT-TEXT
           WRITE CKRPTF-REC
           IF FS-CKRPTF NOT = "00"
               DISPLAY "CKRPTF WRITE ERROR ST=" FS-CKRPTF
               PERFORM 9900-SET-ABEND
           END-IF.

       9000-FINAL.
           CLOSE CKLNKF CMKEYF CMRSLF CKRPTF CKERRF

           IF FS-CKLNKF NOT = "00"
               DISPLAY "CKLNKF CLOSE ERROR ST=" FS-CKLNKF
               SET ABEND-ON TO TRUE
           END-IF
           IF FS-CMKEYF NOT = "00"
               DISPLAY "CMKEYF CLOSE ERROR ST=" FS-CMKEYF
               SET ABEND-ON TO TRUE
           END-IF
           IF FS-CMRSLF NOT = "00"
               DISPLAY "CMRSLF CLOSE ERROR ST=" FS-CMRSLF
               SET ABEND-ON TO TRUE
           END-IF
           IF FS-CKRPTF NOT = "00"
               DISPLAY "CKRPTF CLOSE ERROR ST=" FS-CKRPTF
               SET ABEND-ON TO TRUE
           END-IF
           IF FS-CKERRF NOT = "00"
               DISPLAY "CKERRF CLOSE ERROR ST=" FS-CKERRF
               SET ABEND-ON TO TRUE
           END-IF

           IF ABEND-ON
               MOVE 8 TO RETURN-CODE
               DISPLAY "CK260B ABEND RC=8"
           ELSE
               MOVE 0 TO RETURN-CODE
               DISPLAY "CK260B NORMAL END RC=0"
           END-IF.

       9900-SET-ABEND.
           SET ABEND-ON TO TRUE
           MOVE 8 TO RETURN-CODE.
