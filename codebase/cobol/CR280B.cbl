       IDENTIFICATION DIVISION.
      *
      * 変更履歴
      * 1.00  20250401  共通基盤  新規作成
      * 1.01  20250615  共通基盤  送金帳票出力追加
      * 1.02  20250722  共通基盤  可変形式の不具合修正
      *
       PROGRAM-ID. CR280B.
       AUTHOR.     MFG-SHIKIN-BATCH.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CCCALF ASSIGN TO "CCCALF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-CCCALF.
           SELECT CCVALF ASSIGN TO "CCVALF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-CCVALF.
           SELECT CCXFRF ASSIGN TO "CCXFRF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-CCXFRF.
           SELECT CCRPTF ASSIGN TO "CCRPTF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-CCRPTF.

       DATA DIVISION.
       FILE SECTION.
       FD  CCCALF.
           COPY CCCALFC.
       FD  CCVALF.
           COPY CCVALFC.
       FD  CCXFRF.
           COPY CCXFRC.
       FD  CCRPTF.
           COPY CCRPTC.

       WORKING-STORAGE SECTION.
       01  FS-AREA.
           05 FS-CCCALF              PIC XX VALUE SPACE.
           05 FS-CCVALF              PIC XX VALUE SPACE.
           05 FS-CCXFRF              PIC XX VALUE SPACE.
           05 FS-CCRPTF              PIC XX VALUE SPACE.

       01  SW-AREA.
           05 SW-CCCALF-EOF          PIC X VALUE "N".
              88 CCCALF-EOF               VALUE "Y".
           05 SW-CCVALF-EOF          PIC X VALUE "N".
              88 CCVALF-EOF               VALUE "Y".
           05 SW-CCXFRF-EOF          PIC X VALUE "N".
              88 CCXFRF-EOF               VALUE "Y".
           05 SW-HOLIDAY-HIT         PIC X VALUE "N".
              88 HOLIDAY-HIT              VALUE "Y".

       01  CTL-AREA.
           05 CT-HOLIDAY-CNT         PIC 9(5) VALUE 0.
           05 CT-HOLIDAY-MAX         PIC 9(5) VALUE 02000.
           05 CT-VAL-READ            PIC 9(9) VALUE 0.
           05 CT-XFR-READ            PIC 9(9) VALUE 0.
           05 CT-RPT-WRITE           PIC 9(9) VALUE 0.
           05 CT-LINE-NO             PIC 9(7) VALUE 0.
           05 IX-HOL                 PIC 9(5) VALUE 0.

       01  HOLIDAY-TABLE.
           05 HOLIDAY-ENTRY OCCURS 2000 TIMES.
              10 TB-HOLIDAY-DT       PIC X(8).

       01  EDIT-AREA.
           05 WK-BASE-DT             PIC X(8) VALUE SPACE.
           05 WK-VALUE-DT            PIC X(8) VALUE SPACE.
           05 WK-FCT-ID              PIC X(20) VALUE SPACE.
           05 WK-XFER-ID             PIC X(20) VALUE SPACE.
           05 WK-STATUS              PIC X(2) VALUE SPACE.
           05 WK-SRC-KBN             PIC X(4) VALUE SPACE.
           05 WK-RPT-ID              PIC X(12) VALUE "CR280B".
           05 WK-DATE-8              PIC X(8) VALUE SPACE.
           05 WK-TIME-6              PIC X(6) VALUE SPACE.
           05 WK-CURRENT-DATE        PIC X(21) VALUE SPACE.

       PROCEDURE DIVISION.
       MAIN-RTN.
           MOVE 0 TO RETURN-CODE
           PERFORM 1000-OPEN
           PERFORM 2000-LOAD-CALENDAR
           PERFORM 3000-EDIT-HEADER
           PERFORM 4000-SCAN-VAL
           PERFORM 5000-SCAN-XFR
           PERFORM 6000-EDIT-TRAILER
           PERFORM 9000-CLOSE
           DISPLAY "CR280B NORMAL END OUT=" CT-RPT-WRITE
           MOVE 0 TO RETURN-CODE
           GOBACK.

       1000-OPEN.
           OPEN INPUT CCCALF
           IF FS-CCCALF NOT = "00"
              DISPLAY "CCCALF OPEN ERROR ST=" FS-CCCALF
              PERFORM 9900-ABEND
           END-IF

           OPEN INPUT CCVALF
           IF FS-CCVALF NOT = "00"
              DISPLAY "CCVALF OPEN ERROR ST=" FS-CCVALF
              PERFORM 9900-ABEND
           END-IF

           OPEN INPUT CCXFRF
           IF FS-CCXFRF NOT = "00"
              DISPLAY "CCXFRF OPEN ERROR ST=" FS-CCXFRF
              PERFORM 9900-ABEND
           END-IF

           OPEN OUTPUT CCRPTF
           IF FS-CCRPTF NOT = "00"
              DISPLAY "CCRPTF OPEN ERROR ST=" FS-CCRPTF
              PERFORM 9900-ABEND
           END-IF
           .

       2000-LOAD-CALENDAR.
           PERFORM UNTIL CCCALF-EOF
              READ CCCALF
                 AT END
                    SET CCCALF-EOF TO TRUE
                 NOT AT END
                    IF CL-HOLIDAY-FLAG = "Y"
                       IF CT-HOLIDAY-CNT >= CT-HOLIDAY-MAX
                          DISPLAY "HOLIDAY TABLE FULL "
                                  CT-HOLIDAY-CNT
                          PERFORM 9900-ABEND
                       END-IF
                       ADD 1 TO CT-HOLIDAY-CNT
                       MOVE CL-CAL-DT
                         TO TB-HOLIDAY-DT(CT-HOLIDAY-CNT)
                    ELSE
                       IF CL-HOLIDAY-FLAG NOT = "N"
                          DISPLAY "BAD HOLIDAY FLAG DT="
                                  CL-CAL-DT
                                  " FLG="
                                  CL-HOLIDAY-FLAG
                          PERFORM 9900-ABEND
                       END-IF
                    END-IF
              END-READ
              IF FS-CCCALF NOT = "00" AND FS-CCCALF NOT = "10"
                 DISPLAY "CCCALF READ ERROR ST=" FS-CCCALF
                 PERFORM 9900-ABEND
              END-IF
           END-PERFORM
           IF CT-HOLIDAY-CNT = 0
              DISPLAY "NO HOLIDAY ROW"
           END-IF
           .

       3000-EDIT-HEADER.
           MOVE FUNCTION CURRENT-DATE TO WK-CURRENT-DATE
           MOVE WK-CURRENT-DATE(1:8) TO WK-DATE-8
           MOVE WK-CURRENT-DATE(9:6) TO WK-TIME-6
           MOVE SPACE TO RP-REPORT-TEXT
           STRING "CALENDAR IMPACT LIST DATE="
                  DELIMITED BY SIZE
                  WK-DATE-8 DELIMITED BY SIZE
                  " TIME=" DELIMITED BY SIZE
                  WK-TIME-6 DELIMITED BY SIZE
                  " HOLIDAY=" DELIMITED BY SIZE
                  CT-HOLIDAY-CNT DELIMITED BY SIZE
             INTO RP-REPORT-TEXT
           MOVE WK-RPT-ID TO RP-REPORT-ID
           MOVE WK-DATE-8 TO RP-BASE-DT
           MOVE "HD" TO RP-REPORT-KBN
           ADD 1 TO CT-LINE-NO
           MOVE CT-LINE-NO TO RP-LINE-NO
           PERFORM 8000-WRITE-RPT
           .

       4000-SCAN-VAL.
           PERFORM UNTIL CCVALF-EOF
              READ CCVALF
                 AT END
                    SET CCVALF-EOF TO TRUE
                 NOT AT END
                    ADD 1 TO CT-VAL-READ
                    IF VL-VAL-STATUS-KBN = "01"
                       MOVE VL-VALUE-DT TO WK-VALUE-DT
                       PERFORM 7000-FIND-HOLIDAY
                       IF HOLIDAY-HIT
                          MOVE "VAL" TO WK-SRC-KBN
                          MOVE VL-FCT-ID TO WK-FCT-ID
                          MOVE SPACE TO WK-XFER-ID
                          MOVE VL-VAL-STATUS-KBN TO WK-STATUS
                          PERFORM 8100-WRITE-DETAIL
                       END-IF
                    ELSE
                       IF VL-VAL-STATUS-KBN NOT = "08" AND
                          VL-VAL-STATUS-KBN NOT = "09"
                          DISPLAY "BAD VAL STATUS ID="
                                  VL-VAL-ID
                                  " ST="
                                  VL-VAL-STATUS-KBN
                          PERFORM 9900-ABEND
                       END-IF
                    END-IF
              END-READ
              IF FS-CCVALF NOT = "00" AND FS-CCVALF NOT = "10"
                 DISPLAY "CCVALF READ ERROR ST=" FS-CCVALF
                 PERFORM 9900-ABEND
              END-IF
           END-PERFORM
           .

       5000-SCAN-XFR.
           PERFORM UNTIL CCXFRF-EOF
              READ CCXFRF
                 AT END
                    SET CCXFRF-EOF TO TRUE
                 NOT AT END
                    ADD 1 TO CT-XFR-READ
                    IF XF-XFER-STATUS-KBN = "01"
                       MOVE XF-VALUE-DT TO WK-VALUE-DT
                       PERFORM 7000-FIND-HOLIDAY
                       IF HOLIDAY-HIT
                          MOVE "XFR" TO WK-SRC-KBN
                          MOVE XF-FCT-ID TO WK-FCT-ID
                          MOVE XF-XFER-ID TO WK-XFER-ID
                          MOVE XF-XFER-STATUS-KBN TO WK-STATUS
                          PERFORM 8100-WRITE-DETAIL
                       END-IF
                    ELSE
                       IF XF-XFER-STATUS-KBN NOT = "08" AND
                          XF-XFER-STATUS-KBN NOT = "09"
                          DISPLAY "BAD XFR STATUS ID="
                                  XF-XFER-ID
                                  " ST="
                                  XF-XFER-STATUS-KBN
                          PERFORM 9900-ABEND
                       END-IF
                    END-IF
              END-READ
              IF FS-CCXFRF NOT = "00" AND FS-CCXFRF NOT = "10"
                 DISPLAY "CCXFRF READ ERROR ST=" FS-CCXFRF
                 PERFORM 9900-ABEND
              END-IF
           END-PERFORM
           .

       6000-EDIT-TRAILER.
           MOVE SPACE TO RP-REPORT-TEXT
           STRING "INPUT VAL=" DELIMITED BY SIZE
                  CT-VAL-READ DELIMITED BY SIZE
                  " XFR=" DELIMITED BY SIZE
                  CT-XFR-READ DELIMITED BY SIZE
                  " OUT=" DELIMITED BY SIZE
                  CT-RPT-WRITE DELIMITED BY SIZE
             INTO RP-REPORT-TEXT
           MOVE WK-RPT-ID TO RP-REPORT-ID
           MOVE WK-DATE-8 TO RP-BASE-DT
           MOVE "TR" TO RP-REPORT-KBN
           ADD 1 TO CT-LINE-NO
           MOVE CT-LINE-NO TO RP-LINE-NO
           PERFORM 8000-WRITE-RPT
           .

       7000-FIND-HOLIDAY.
           MOVE "N" TO SW-HOLIDAY-HIT
           MOVE 1 TO IX-HOL
           PERFORM UNTIL IX-HOL > CT-HOLIDAY-CNT OR HOLIDAY-HIT
              IF TB-HOLIDAY-DT(IX-HOL) = WK-VALUE-DT
                 MOVE "Y" TO SW-HOLIDAY-HIT
              ELSE
                 ADD 1 TO IX-HOL
              END-IF
           END-PERFORM
           .

       8000-WRITE-RPT.
           WRITE CCRPTF-REC
           IF FS-CCRPTF NOT = "00"
              DISPLAY "CCRPTF WRITE ERROR ST=" FS-CCRPTF
              PERFORM 9900-ABEND
           END-IF
           ADD 1 TO CT-RPT-WRITE
           .

       8100-WRITE-DETAIL.
           MOVE WK-VALUE-DT TO WK-BASE-DT
           MOVE SPACE TO RP-REPORT-TEXT
           STRING "KBN=" DELIMITED BY SIZE
                  WK-SRC-KBN DELIMITED BY SIZE
                  " FCT-ID=" DELIMITED BY SIZE
                  WK-FCT-ID DELIMITED BY SIZE
                  " VALUE-DT=" DELIMITED BY SIZE
                  WK-VALUE-DT DELIMITED BY SIZE
                  " XFER-ID=" DELIMITED BY SIZE
                  WK-XFER-ID DELIMITED BY SIZE
                  " ST=" DELIMITED BY SIZE
                  WK-STATUS DELIMITED BY SIZE
             INTO RP-REPORT-TEXT
           MOVE WK-RPT-ID TO RP-REPORT-ID
           MOVE WK-BASE-DT TO RP-BASE-DT
           MOVE "DT" TO RP-REPORT-KBN
           ADD 1 TO CT-LINE-NO
           MOVE CT-LINE-NO TO RP-LINE-NO
           PERFORM 8000-WRITE-RPT
           .

       9000-CLOSE.
           CLOSE CCCALF
           IF FS-CCCALF NOT = "00"
              DISPLAY "CCCALF CLOSE ERROR ST=" FS-CCCALF
              PERFORM 9900-ABEND
           END-IF

           CLOSE CCVALF
           IF FS-CCVALF NOT = "00"
              DISPLAY "CCVALF CLOSE ERROR ST=" FS-CCVALF
              PERFORM 9900-ABEND
           END-IF

           CLOSE CCXFRF
           IF FS-CCXFRF NOT = "00"
              DISPLAY "CCXFRF CLOSE ERROR ST=" FS-CCXFRF
              PERFORM 9900-ABEND
           END-IF

           CLOSE CCRPTF
           IF FS-CCRPTF NOT = "00"
              DISPLAY "CCRPTF CLOSE ERROR ST=" FS-CCRPTF
              PERFORM 9900-ABEND
           END-IF
           .

       9900-ABEND.
           MOVE 8 TO RETURN-CODE
           DISPLAY "CR280B ABEND RC=" RETURN-CODE
           GOBACK.
