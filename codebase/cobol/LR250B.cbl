       IDENTIFICATION DIVISION.
       PROGRAM-ID. LR250B.
       AUTHOR. LRB01.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LFCNTF ASSIGN TO "LFCNTF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS CN-POL-NO
               FILE STATUS IS WS-ST-LFCNTF.
           SELECT LFPOLF ASSIGN TO "LFPOLF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-ST-LFPOLF.
           SELECT LFPRMF ASSIGN TO "LFPRMF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-ST-LFPRMF.
           SELECT LRRPTF ASSIGN TO "LRRPTF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-ST-LRRPTF.

       DATA DIVISION.
       FILE SECTION.

       FD  LFCNTF.
       COPY LFCNTFC.

       FD  LFPOLF.
       COPY LFPOLFC.

       FD  LFPRMF.
       COPY LFPRMFC.

       FD  LRRPTF.
       COPY LRRPTFC.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05 WS-ST-LFCNTF             PIC XX VALUE SPACE.
           05 WS-ST-LFPOLF             PIC XX VALUE SPACE.
           05 WS-ST-LFPRMF             PIC XX VALUE SPACE.
           05 WS-ST-LRRPTF             PIC XX VALUE SPACE.

       01  WS-END-FLAGS.
           05 WS-CN-EOF-SW             PIC X VALUE "N".
              88 CN-EOF                      VALUE "Y".
           05 WS-PO-EOF-SW             PIC X VALUE "N".
              88 PO-EOF                      VALUE "Y".
           05 WS-PR-EOF-SW             PIC X VALUE "N".
              88 PR-EOF                      VALUE "Y".

       01  WS-RUN-CONTROL.
           05 WS-REPORT-ID-BASE        PIC X(12) VALUE "LR250B".
           05 WS-REPORT-YM             PIC 9(06) VALUE ZERO.
           05 WS-REPORT-TYPE-KBN       PIC X VALUE "1".
           05 WS-LINE-NO               PIC 9(07) VALUE ZERO.
           05 WS-OUT-COUNT             PIC 9(09) VALUE ZERO.
           05 WS-SKIP-COUNT            PIC 9(09) VALUE ZERO.
           05 WS-ERROR-COUNT           PIC 9(09) VALUE ZERO.

       01  WS-DATE-AREA.
           05 WS-CURRENT-DATE          PIC 9(08).
           05 WS-CURRENT-DATE-R REDEFINES WS-CURRENT-DATE.
              10 WS-CUR-YYYY           PIC 9(04).
              10 WS-CUR-MM             PIC 9(02).
              10 WS-CUR-DD             PIC 9(02).

       01  WS-WORK-KEYS.
           05 WS-CN-POL-NO             PIC X(12) VALUE LOW-VALUE.
           05 WS-PO-POL-NO             PIC X(12) VALUE HIGH-VALUE.
           05 WS-PR-POL-NO             PIC X(12) VALUE HIGH-VALUE.
           05 WS-PREV-CN-POL-NO        PIC X(12) VALUE LOW-VALUE.
           05 WS-REPORT-ID-NUM         PIC 9(07) VALUE ZERO.

       01  WS-POLICY-WORK.
           05 WS-HAVE-PO-SW            PIC X VALUE "N".
              88 HAVE-PO                     VALUE "Y".
           05 WS-HAVE-PR-SW            PIC X VALUE "N".
              88 HAVE-PR                     VALUE "Y".
           05 WS-STATUS-OUT            PIC X(02) VALUE SPACE.
           05 WS-BAND-KBN              PIC X(02) VALUE SPACE.
           05 WS-PRINT-AMT             PIC 9(11)V99 VALUE ZERO.
           05 WS-VALID-SW              PIC X VALUE "N".
              88 REC-VALID                   VALUE "Y".

       01  WS-MASK-WORK.
           05 WS-MASKED-POL-NO         PIC X(12) VALUE SPACE.
           05 WS-IDX                   PIC 9(02) VALUE ZERO.

       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-INITIALIZE
           IF RETURN-CODE = 0
               PERFORM 2000-PROCESS UNTIL CN-EOF
           END-IF
           PERFORM 9000-FINALIZE
           GOBACK.

       1000-INITIALIZE.
           MOVE 0 TO RETURN-CODE
           ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD
           MOVE WS-CUR-YYYY TO WS-REPORT-YM(1:4)
           MOVE WS-CUR-MM   TO WS-REPORT-YM(5:2)

           OPEN INPUT LFCNTF
           IF WS-ST-LFCNTF NOT = "00"
               DISPLAY "LFCNTF OPEN ERR ST=" WS-ST-LFCNTF
               MOVE 12 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF

           OPEN INPUT LFPOLF
           IF WS-ST-LFPOLF NOT = "00"
               DISPLAY "LFPOLF OPEN ERR ST=" WS-ST-LFPOLF
               MOVE 12 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF

           OPEN INPUT LFPRMF
           IF WS-ST-LFPRMF NOT = "00"
               DISPLAY "LFPRMF OPEN ERR ST=" WS-ST-LFPRMF
               MOVE 12 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF

           OPEN OUTPUT LRRPTF
           IF WS-ST-LRRPTF NOT = "00"
               DISPLAY "LRRPTF OPEN ERR ST=" WS-ST-LRRPTF
               MOVE 12 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF

           PERFORM 1100-READ-CN
           PERFORM 1200-READ-PO
           PERFORM 1300-READ-PR.

       1100-READ-CN.
           READ LFCNTF
               AT END
                   SET CN-EOF TO TRUE
                   MOVE HIGH-VALUE TO WS-CN-POL-NO
               NOT AT END
                   MOVE CN-POL-NO TO WS-CN-POL-NO
           END-READ
           IF WS-ST-LFCNTF NOT = "00" AND WS-ST-LFCNTF NOT = "10"
               DISPLAY "LFCNTF READ ERR ST=" WS-ST-LFCNTF
               MOVE 12 TO RETURN-CODE
               SET CN-EOF TO TRUE
               MOVE HIGH-VALUE TO WS-CN-POL-NO
           END-IF.

       1200-READ-PO.
           READ LFPOLF
               AT END
                   SET PO-EOF TO TRUE
                   MOVE HIGH-VALUE TO WS-PO-POL-NO
               NOT AT END
                   MOVE PO-POL-NO TO WS-PO-POL-NO
           END-READ
           IF WS-ST-LFPOLF NOT = "00" AND WS-ST-LFPOLF NOT = "10"
               DISPLAY "LFPOLF READ ERR ST=" WS-ST-LFPOLF
               MOVE 12 TO RETURN-CODE
               SET PO-EOF TO TRUE
               MOVE HIGH-VALUE TO WS-PO-POL-NO
           END-IF.

       1300-READ-PR.
           READ LFPRMF
               AT END
                   SET PR-EOF TO TRUE
                   MOVE HIGH-VALUE TO WS-PR-POL-NO
               NOT AT END
                   MOVE PR-POL-NO TO WS-PR-POL-NO
           END-READ
           IF WS-ST-LFPRMF NOT = "00" AND WS-ST-LFPRMF NOT = "10"
               DISPLAY "LFPRMF READ ERR ST=" WS-ST-LFPRMF
               MOVE 12 TO RETURN-CODE
               SET PR-EOF TO TRUE
               MOVE HIGH-VALUE TO WS-PR-POL-NO
           END-IF.

       2000-PROCESS.
           IF RETURN-CODE NOT = 0
               SET CN-EOF TO TRUE
               EXIT PARAGRAPH
           END-IF

           MOVE "N" TO WS-HAVE-PO-SW
           MOVE "N" TO WS-HAVE-PR-SW
           MOVE "N" TO WS-VALID-SW
           MOVE ZERO TO WS-PRINT-AMT
           MOVE SPACE TO WS-STATUS-OUT
           MOVE SPACE TO WS-BAND-KBN

           PERFORM 2100-VALIDATE-CN
           IF REC-VALID
               PERFORM 2200-ALIGN-PO
               PERFORM 2300-ALIGN-PR
               PERFORM 2400-EDIT-STATUS
               PERFORM 2500-EDIT-PREMIUM
               PERFORM 2600-WRITE-REPORT
           ELSE
               ADD 1 TO WS-SKIP-COUNT
           END-IF

           MOVE CN-POL-NO TO WS-PREV-CN-POL-NO
           PERFORM 1100-READ-CN.

       2100-VALIDATE-CN.
           MOVE "Y" TO WS-VALID-SW

           IF CN-POL-NO = SPACE OR CN-POL-NO = LOW-VALUE
               DISPLAY "CN POLNO ERR " CN-POL-NO
               MOVE "N" TO WS-VALID-SW
               ADD 1 TO WS-ERROR-COUNT
           END-IF

           IF CN-POL-NO < WS-PREV-CN-POL-NO
               DISPLAY "LFCNTF SEQ ERR " CN-POL-NO
               MOVE "N" TO WS-VALID-SW
               ADD 1 TO WS-ERROR-COUNT
           END-IF

           IF CN-NEXT-DUE-YM NOT NUMERIC
               DISPLAY "CN DUEYM ERR " CN-POL-NO
               MOVE "N" TO WS-VALID-SW
               ADD 1 TO WS-ERROR-COUNT
           END-IF

           IF CN-PAY-METHOD-KBN = SPACE
               DISPLAY "CN PAYMETHOD ERR " CN-POL-NO
               MOVE "N" TO WS-VALID-SW
               ADD 1 TO WS-ERROR-COUNT
           END-IF.

       2200-ALIGN-PO.
           PERFORM UNTIL PO-EOF OR WS-PO-POL-NO >= CN-POL-NO
               PERFORM 1200-READ-PO
           END-PERFORM

           IF WS-PO-POL-NO = CN-POL-NO
               MOVE "Y" TO WS-HAVE-PO-SW
           ELSE
               DISPLAY "LFPOLF MISSING " CN-POL-NO
               ADD 1 TO WS-ERROR-COUNT
           END-IF.

       2300-ALIGN-PR.
           PERFORM UNTIL PR-EOF OR WS-PR-POL-NO >= CN-POL-NO
               PERFORM 1300-READ-PR
           END-PERFORM

           IF WS-PR-POL-NO = CN-POL-NO
               PERFORM UNTIL PR-EOF
                   OR WS-PR-POL-NO NOT = CN-POL-NO
                   IF PR-CALC-STATUS-KBN = "1"
                       MOVE PR-PRM-AMT TO WS-PRINT-AMT
                       MOVE PR-BAND-KBN TO WS-BAND-KBN
                       MOVE "Y" TO WS-HAVE-PR-SW
                   END-IF
                   PERFORM 1300-READ-PR
               END-PERFORM
           END-IF.

       2400-EDIT-STATUS.
           IF HAVE-PO
               EVALUATE PO-POL-STATUS-KBN
                   WHEN "01"
                       MOVE "01" TO WS-STATUS-OUT
                   WHEN "02"
                       MOVE "02" TO WS-STATUS-OUT
                   WHEN "09"
                       MOVE "09" TO WS-STATUS-OUT
                   WHEN OTHER
                       DISPLAY "PO STATUS ERR " CN-POL-NO
                       MOVE "99" TO WS-STATUS-OUT
                       ADD 1 TO WS-ERROR-COUNT
               END-EVALUATE
           ELSE
               MOVE "98" TO WS-STATUS-OUT
           END-IF.

       2500-EDIT-PREMIUM.
           IF NOT HAVE-PO
               MOVE ZERO TO WS-PRINT-AMT
               EXIT PARAGRAPH
           END-IF

           IF PO-POL-STATUS-KBN NOT = "01"
               MOVE ZERO TO WS-PRINT-AMT
               EXIT PARAGRAPH
           END-IF

           IF NOT HAVE-PR
               DISPLAY "LFPRMF MISSING " CN-POL-NO
               MOVE ZERO TO WS-PRINT-AMT
               ADD 1 TO WS-ERROR-COUNT
               EXIT PARAGRAPH
           END-IF

           PERFORM 2510-DECIDE-BAND
           IF WS-BAND-KBN NOT = SPACE
              AND WS-BAND-KBN NOT = "A1"
              AND WS-BAND-KBN NOT = "A2"
              AND WS-BAND-KBN NOT = "A3"
              AND WS-BAND-KBN NOT = "A4"
              AND WS-BAND-KBN NOT = "A5"
               DISPLAY "PR BAND ERR " CN-POL-NO
               ADD 1 TO WS-ERROR-COUNT
           END-IF.

       2510-DECIDE-BAND.
           IF PO-ENTRY-AGE-CNT <= 29
               MOVE "A1" TO WS-BAND-KBN
           ELSE
               IF PO-ENTRY-AGE-CNT <= 39
                   MOVE "A2" TO WS-BAND-KBN
               ELSE
                   IF PO-ENTRY-AGE-CNT <= 49
                       MOVE "A3" TO WS-BAND-KBN
                   ELSE
                       IF PO-ENTRY-AGE-CNT <= 59
                           MOVE "A4" TO WS-BAND-KBN
                       ELSE
                           MOVE "A5" TO WS-BAND-KBN
                       END-IF
                   END-IF
               END-IF
           END-IF.

       2600-WRITE-REPORT.
           INITIALIZE LRRPTF-REC
           ADD 1 TO WS-LINE-NO
           ADD 1 TO WS-REPORT-ID-NUM

           STRING WS-REPORT-ID-BASE DELIMITED BY SPACE
                  WS-REPORT-ID-NUM  DELIMITED BY SIZE
             INTO RP-REPORT-ID
           END-STRING

           MOVE WS-REPORT-YM       TO RP-REPORT-YM
           MOVE WS-REPORT-TYPE-KBN TO RP-REPORT-TYPE-KBN
           PERFORM 2610-EDIT-POL-NO
           MOVE WS-MASKED-POL-NO   TO RP-POL-NO
           MOVE WS-LINE-NO         TO RP-LINE-NO
           MOVE WS-PRINT-AMT       TO RP-PRINT-AMT
           MOVE WS-STATUS-OUT      TO RP-OUTPUT-STATUS-KBN

           WRITE LRRPTF-REC
           IF WS-ST-LRRPTF NOT = "00"
               DISPLAY "LRRPTF WRITE ERR ST=" WS-ST-LRRPTF
               MOVE 12 TO RETURN-CODE
               SET CN-EOF TO TRUE
           ELSE
               ADD 1 TO WS-OUT-COUNT
           END-IF.

       2610-EDIT-POL-NO.
           MOVE CN-POL-NO TO WS-MASKED-POL-NO

           EVALUATE WS-REPORT-TYPE-KBN
               WHEN "1"
                   CONTINUE
               WHEN "2"
                   PERFORM VARYING WS-IDX FROM 5 BY 1
                       UNTIL WS-IDX > 8
                       MOVE "*" TO WS-MASKED-POL-NO(WS-IDX:1)
                   END-PERFORM
               WHEN OTHER
                   PERFORM VARYING WS-IDX FROM 1 BY 1
                       UNTIL WS-IDX > 12
                       IF WS-IDX > 3
                           MOVE "*" TO WS-MASKED-POL-NO(WS-IDX:1)
                       END-IF
                   END-PERFORM
           END-EVALUATE.

       9000-FINALIZE.
           PERFORM 9100-CLOSE-FILES

           IF RETURN-CODE = 0
               DISPLAY "LR250B OK OUT=" WS-OUT-COUNT
               DISPLAY "LR250B SKIP=" WS-SKIP-COUNT
               DISPLAY "LR250B WARN=" WS-ERROR-COUNT
               MOVE 0 TO RETURN-CODE
           ELSE
               DISPLAY "LR250B ABEND RC=" RETURN-CODE
           END-IF.

       9100-CLOSE-FILES.
           IF WS-ST-LFCNTF NOT = SPACE
               CLOSE LFCNTF
               IF WS-ST-LFCNTF NOT = "00"
                   DISPLAY "LFCNTF CLOSE ERR ST=" WS-ST-LFCNTF
                   IF RETURN-CODE = 0
                       MOVE 8 TO RETURN-CODE
                   END-IF
               END-IF
           END-IF

           IF WS-ST-LFPOLF NOT = SPACE
               CLOSE LFPOLF
               IF WS-ST-LFPOLF NOT = "00"
                   DISPLAY "LFPOLF CLOSE ERR ST=" WS-ST-LFPOLF
                   IF RETURN-CODE = 0
                       MOVE 8 TO RETURN-CODE
                   END-IF
               END-IF
           END-IF

           IF WS-ST-LFPRMF NOT = SPACE
               CLOSE LFPRMF
               IF WS-ST-LFPRMF NOT = "00"
                   DISPLAY "LFPRMF CLOSE ERR ST=" WS-ST-LFPRMF
                   IF RETURN-CODE = 0
                       MOVE 8 TO RETURN-CODE
                   END-IF
               END-IF
           END-IF

           IF WS-ST-LRRPTF NOT = SPACE
               CLOSE LRRPTF
               IF WS-ST-LRRPTF NOT = "00"
                   DISPLAY "LRRPTF CLOSE ERR ST=" WS-ST-LRRPTF
                   IF RETURN-CODE = 0
                       MOVE 8 TO RETURN-CODE
                   END-IF
               END-IF
           END-IF.
