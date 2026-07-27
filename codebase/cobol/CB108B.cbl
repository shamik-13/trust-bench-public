       IDENTIFICATION DIVISION.
       PROGRAM-ID. CB108B.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CDCAPTF
               ASSIGN TO "CDCAPTF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-CDCAPTF.

           SELECT CDRSLDF
               ASSIGN TO "CDRSLDF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-CDRSLDF.

           SELECT CDMEMF
               ASSIGN TO "CDMEMF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS MM-MEMBER-ID
               FILE STATUS IS FS-CDMEMF.

           SELECT CDPTF
               ASSIGN TO "CDPTF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS PT-MEMBER-ID
               FILE STATUS IS FS-CDPTF.

       DATA DIVISION.
       FILE SECTION.

       FD  CDCAPTF.
           COPY CDCAPTC.

       FD  CDRSLDF.
           COPY CDRSLDFC.

       FD  CDMEMF.
           COPY CDMEMC.

       FD  CDPTF.
           COPY CDPTC.

       WORKING-STORAGE SECTION.
       01  FS-AREA.
           05 FS-CDCAPTF              PIC X(02) VALUE SPACES.
           05 FS-CDRSLDF              PIC X(02) VALUE SPACES.
           05 FS-CDMEMF               PIC X(02) VALUE SPACES.
           05 FS-CDPTF                PIC X(02) VALUE SPACES.

       01  EOF-FLAGS.
           05 EOF-CDCAPTF             PIC X VALUE "N".
              88 CDCAPTF-END                VALUE "Y".
           05 EOF-CDRSLDF             PIC X VALUE "N".
              88 CDRSLDF-END                VALUE "Y".
           05 EOF-CDMEMF              PIC X VALUE "N".
              88 CDMEMF-END                 VALUE "Y".

       01  WORK-AREA.
           05 WK-PROGRAM-ID           PIC X(08) VALUE "CB108B".
           05 WK-MEMBER-ID            PIC X(16) VALUE SPACES.
           05 WK-BASE-AMT             PIC S9(13) VALUE ZERO.
           05 WK-RSLD-ADJ-AMT         PIC S9(13) VALUE ZERO.
           05 WK-POINT                PIC S9(09) VALUE ZERO.
           05 WK-EARNED-NUM           PIC 9(10) VALUE ZERO.
           05 WK-CALC-FEE             PIC S9(13) VALUE ZERO.
           05 WK-REV-BAL              PIC S9(13) VALUE ZERO.
           05 WK-MONTHLY-RATE         PIC 9V9999 VALUE 0.0125.
           05 WK-FOUND-MEMBER         PIC X VALUE "N".
              88 MEMBER-FOUND               VALUE "Y".
           05 WK-RSLD-MATCH           PIC X VALUE "N".
              88 RSLD-MATCH                 VALUE "Y".
           05 WK-ANNUAL-FEE-LINE      PIC X VALUE "N".
              88 ANNUAL-FEE-LINE            VALUE "Y".
           05 WK-FEE-LINE             PIC X VALUE "N".
              88 FEE-LINE                   VALUE "Y".

       01  COUNT-AREA.
           05 CNT-CAPTURE-READ        PIC 9(09) VALUE ZERO.
           05 CNT-CAPTURE-SKIP        PIC 9(09) VALUE ZERO.
           05 CNT-MEMBER-NG           PIC 9(09) VALUE ZERO.
           05 CNT-RSLD-SKIP           PIC 9(09) VALUE ZERO.
           05 CNT-POINT-WRITE         PIC 9(09) VALUE ZERO.
           05 CNT-POINT-REWRITE       PIC 9(09) VALUE ZERO.

       01  STATUS-CONST.
           05 CST-STATUS-ACTIVE       PIC X(01) VALUE "1".
           05 CST-CAPTURE-FIXED       PIC X(01) VALUE "C".
           05 CST-RSLD-FIXED          PIC X(01) VALUE "C".
           05 CST-POINT-ACTIVE        PIC X(01) VALUE "1".

       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-INITIALIZE
           PERFORM 2000-PROCESS UNTIL CDCAPTF-END
           PERFORM 9000-TERMINATE
           MOVE 0 TO RETURN-CODE
           GOBACK.

       1000-INITIALIZE.
           OPEN INPUT CDCAPTF
           IF FS-CDCAPTF NOT = "00"
              DISPLAY "CDCAPTF OPEN ERROR ST=" FS-CDCAPTF
              MOVE 8 TO RETURN-CODE
              GOBACK
           END-IF

           OPEN INPUT CDMEMF
           IF FS-CDMEMF NOT = "00"
              DISPLAY "CDMEMF OPEN ERROR ST=" FS-CDMEMF
              MOVE 8 TO RETURN-CODE
              GOBACK
           END-IF

           OPEN I-O CDPTF
           IF FS-CDPTF NOT = "00"
              DISPLAY "CDPTF OPEN ERROR ST=" FS-CDPTF
              MOVE 8 TO RETURN-CODE
              GOBACK
           END-IF

           PERFORM 1100-READ-CAPTURE.

       1100-READ-CAPTURE.
           READ CDCAPTF
              AT END
                 SET CDCAPTF-END TO TRUE
              NOT AT END
                 ADD 1 TO CNT-CAPTURE-READ
           END-READ

           IF FS-CDCAPTF NOT = "00" AND FS-CDCAPTF NOT = "10"
              DISPLAY "CDCAPTF READ ERROR ST=" FS-CDCAPTF
              MOVE 8 TO RETURN-CODE
              GOBACK
           END-IF.

       2000-PROCESS.
           MOVE "N" TO WK-FOUND-MEMBER
           MOVE "N" TO WK-RSLD-MATCH
           MOVE "N" TO WK-ANNUAL-FEE-LINE
           MOVE "N" TO WK-FEE-LINE
           MOVE ZERO TO WK-BASE-AMT
           MOVE ZERO TO WK-RSLD-ADJ-AMT
           MOVE ZERO TO WK-POINT

           IF CP-CAPTURE-STATUS NOT = CST-CAPTURE-FIXED
              ADD 1 TO CNT-CAPTURE-SKIP
              PERFORM 1100-READ-CAPTURE
              EXIT PARAGRAPH
           END-IF

           IF CP-CAPTURE-AMT <= ZERO
              DISPLAY "INVALID CAPTURE AMT ID=" CP-CAPTURE-ID
              ADD 1 TO CNT-CAPTURE-SKIP
              PERFORM 1100-READ-CAPTURE
              EXIT PARAGRAPH
           END-IF

           PERFORM 3000-FIND-MEMBER

           IF NOT MEMBER-FOUND
              DISPLAY "MEMBER NOT FOUND CARD=" CP-CARD-NO
              ADD 1 TO CNT-MEMBER-NG
              PERFORM 1100-READ-CAPTURE
              EXIT PARAGRAPH
           END-IF

           IF MM-MEMBER-STATUS NOT = CST-STATUS-ACTIVE
              ADD 1 TO CNT-MEMBER-NG
              PERFORM 1100-READ-CAPTURE
              EXIT PARAGRAPH
           END-IF

           PERFORM 4000-CHECK-RSLD
           PERFORM 5000-CHECK-EXCLUSION

           IF RSLD-MATCH OR ANNUAL-FEE-LINE OR FEE-LINE
              ADD 1 TO CNT-RSLD-SKIP
              PERFORM 1100-READ-CAPTURE
              EXIT PARAGRAPH
           END-IF

           MOVE CP-CAPTURE-AMT TO WK-BASE-AMT
           SUBTRACT WK-RSLD-ADJ-AMT FROM WK-BASE-AMT

           IF WK-BASE-AMT <= ZERO
              ADD 1 TO CNT-CAPTURE-SKIP
              PERFORM 1100-READ-CAPTURE
              EXIT PARAGRAPH
           END-IF

           COMPUTE WK-POINT = WK-BASE-AMT / 100

           IF WK-POINT > ZERO
              PERFORM 6000-APPLY-POINT
           ELSE
              ADD 1 TO CNT-CAPTURE-SKIP
           END-IF

           PERFORM 1100-READ-CAPTURE.

       3000-FIND-MEMBER.
           MOVE "N" TO EOF-CDMEMF
           MOVE LOW-VALUES TO MM-MEMBER-ID

           START CDMEMF KEY IS >= MM-MEMBER-ID
              INVALID KEY
                 SET CDMEMF-END TO TRUE
              NOT INVALID KEY
                 MOVE "N" TO EOF-CDMEMF
           END-START

           IF FS-CDMEMF NOT = "00" AND FS-CDMEMF NOT = "23"
              DISPLAY "CDMEMF START ERROR ST=" FS-CDMEMF
              MOVE 8 TO RETURN-CODE
              GOBACK
           END-IF

           PERFORM UNTIL CDMEMF-END OR MEMBER-FOUND
              READ CDMEMF NEXT RECORD
                 AT END
                    SET CDMEMF-END TO TRUE
                 NOT AT END
                    IF MM-CARD-NO = CP-CARD-NO
                       MOVE MM-MEMBER-ID TO WK-MEMBER-ID
                       MOVE "Y" TO WK-FOUND-MEMBER
                    END-IF
              END-READ

              IF FS-CDMEMF NOT = "00" AND FS-CDMEMF NOT = "10"
                 DISPLAY "CDMEMF READ ERROR ST=" FS-CDMEMF
                 MOVE 8 TO RETURN-CODE
                 GOBACK
              END-IF
           END-PERFORM.

       4000-CHECK-RSLD.
           MOVE "N" TO EOF-CDRSLDF

           OPEN INPUT CDRSLDF
           IF FS-CDRSLDF NOT = "00"
              DISPLAY "CDRSLDF OPEN ERROR ST=" FS-CDRSLDF
              MOVE 8 TO RETURN-CODE
              GOBACK
           END-IF

           PERFORM UNTIL CDRSLDF-END OR RSLD-MATCH
              READ CDRSLDF
                 AT END
                    SET CDRSLDF-END TO TRUE
                 NOT AT END
                    PERFORM 4100-JUDGE-RSLD
              END-READ

              IF FS-CDRSLDF NOT = "00" AND FS-CDRSLDF NOT = "10"
                 DISPLAY "CDRSLDF READ ERROR ST=" FS-CDRSLDF
                 MOVE 8 TO RETURN-CODE
                 GOBACK
              END-IF
           END-PERFORM

           CLOSE CDRSLDF
           IF FS-CDRSLDF NOT = "00"
              DISPLAY "CDRSLDF CLOSE ERROR ST=" FS-CDRSLDF
              MOVE 8 TO RETURN-CODE
              GOBACK
           END-IF.

       4100-JUDGE-RSLD.
           IF RS-CARD-NO = CP-CARD-NO
              AND RS-CYCLE-DT = CP-SALES-DT
              AND RS-RSLD-STATUS = CST-RSLD-FIXED
              AND RS-PROGRAM-ID NOT = WK-PROGRAM-ID
              IF CP-CAPTURE-AMT = RS-FEE-AMT
                 MOVE "Y" TO WK-RSLD-MATCH
              ELSE
                 IF CP-CAPTURE-AMT = RS-PAY-AMT
                    MOVE "Y" TO WK-RSLD-MATCH
                 ELSE
                    IF CP-CAPTURE-AMT = RS-PRIN-AMT
                       MOVE RS-FEE-AMT TO WK-RSLD-ADJ-AMT
                    END-IF
                 END-IF
              END-IF
           END-IF.

       5000-CHECK-EXCLUSION.
           IF MM-ANNUAL-FEE-CD NOT = SPACE
              IF CP-AUTH-NO(1:2) = "AF"
                 MOVE "Y" TO WK-ANNUAL-FEE-LINE
              END-IF
           END-IF

           IF CP-MERCHANT-ID(1:4) = "FEE "
              MOVE "Y" TO WK-FEE-LINE
           END-IF

           MOVE CP-CAPTURE-AMT TO WK-REV-BAL
           COMPUTE WK-CALC-FEE =
              FUNCTION INTEGER(WK-REV-BAL * WK-MONTHLY-RATE)

           IF CP-CAPTURE-AMT = WK-CALC-FEE
              MOVE "Y" TO WK-FEE-LINE
           END-IF.

       6000-APPLY-POINT.
           MOVE WK-MEMBER-ID TO PT-MEMBER-ID

           READ CDPTF
              INVALID KEY
                 PERFORM 6100-WRITE-POINT
              NOT INVALID KEY
                 PERFORM 6200-REWRITE-POINT
           END-READ

           IF FS-CDPTF NOT = "00" AND FS-CDPTF NOT = "23"
              DISPLAY "CDPTF READ ERROR ST=" FS-CDPTF
              MOVE 8 TO RETURN-CODE
              GOBACK
           END-IF.

       6100-WRITE-POINT.
           MOVE WK-MEMBER-ID TO PT-MEMBER-ID
           MOVE WK-POINT TO PT-POINT-BAL
           MOVE WK-POINT TO WK-EARNED-NUM
           MOVE WK-EARNED-NUM TO PT-EARNED-POINT
           MOVE "0000000000" TO PT-USED-POINT
           MOVE CP-SALES-DT TO PT-LAST-EARN-DT
           MOVE CST-POINT-ACTIVE TO PT-POINT-STATUS

           WRITE CDPTF-REC

           IF FS-CDPTF = "00"
              ADD 1 TO CNT-POINT-WRITE
           ELSE
              DISPLAY "CDPTF WRITE ERROR ST=" FS-CDPTF
              MOVE 8 TO RETURN-CODE
              GOBACK
           END-IF.

       6200-REWRITE-POINT.
           IF PT-POINT-STATUS NOT = CST-POINT-ACTIVE
              DISPLAY "INVALID POINT STATUS MEMBER=" PT-MEMBER-ID
              ADD 1 TO CNT-CAPTURE-SKIP
              EXIT PARAGRAPH
           END-IF

           ADD WK-POINT TO PT-POINT-BAL
           MOVE PT-EARNED-POINT TO WK-EARNED-NUM
           ADD WK-POINT TO WK-EARNED-NUM
           MOVE WK-EARNED-NUM TO PT-EARNED-POINT

           IF CP-SALES-DT > PT-LAST-EARN-DT
              MOVE CP-SALES-DT TO PT-LAST-EARN-DT
           END-IF

           REWRITE CDPTF-REC

           IF FS-CDPTF = "00"
              ADD 1 TO CNT-POINT-REWRITE
           ELSE
              DISPLAY "CDPTF REWRITE ERROR ST=" FS-CDPTF
              MOVE 8 TO RETURN-CODE
              GOBACK
           END-IF.

       9000-TERMINATE.
           CLOSE CDCAPTF CDMEMF CDPTF

           IF FS-CDCAPTF NOT = "00"
              DISPLAY "CDCAPTF CLOSE ERROR ST=" FS-CDCAPTF
              MOVE 8 TO RETURN-CODE
              GOBACK
           END-IF

           IF FS-CDMEMF NOT = "00"
              DISPLAY "CDMEMF CLOSE ERROR ST=" FS-CDMEMF
              MOVE 8 TO RETURN-CODE
              GOBACK
           END-IF

           IF FS-CDPTF NOT = "00"
              DISPLAY "CDPTF CLOSE ERROR ST=" FS-CDPTF
              MOVE 8 TO RETURN-CODE
              GOBACK
           END-IF

           DISPLAY "CB108B NORMAL END"
           DISPLAY "CAPTURE READ=" CNT-CAPTURE-READ
           DISPLAY "CAPTURE SKIP=" CNT-CAPTURE-SKIP
           DISPLAY "MEMBER SKIP=" CNT-MEMBER-NG
           DISPLAY "RSLD SKIP=" CNT-RSLD-SKIP
           DISPLAY "POINT WRITE=" CNT-POINT-WRITE
           DISPLAY "POINT REWRITE=" CNT-POINT-REWRITE.
