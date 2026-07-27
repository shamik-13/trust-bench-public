       IDENTIFICATION DIVISION.
       PROGRAM-ID. CG260S.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CGCODF ASSIGN TO "CGCODF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS GC-CODE-ID
               FILE STATUS IS WS-CGCODF-ST.

       DATA DIVISION.
       FILE SECTION.
       FD  CGCODF.
           COPY CGCODC.

       WORKING-STORAGE SECTION.
       01  WS-CGCODF-ST              PIC X(02) VALUE SPACE.
       01  WS-OPEN-SW                PIC X(01) VALUE "0".
           88  WS-OPENED                      VALUE "1".

       01  WS-CURRENT-DATE.
           05  WS-CUR-YYYY           PIC 9(04).
           05  WS-CUR-MM             PIC 9(02).
           05  WS-CUR-DD             PIC 9(02).
       01  WS-CUR-DT                 PIC 9(08) VALUE ZERO.

       01  WS-WORK-KEY               PIC X(32) VALUE SPACE.
       01  WS-EDIT-YM.
           05  WS-EDIT-YYYY          PIC 9(04).
           05  FILLER                PIC X(02) VALUE "YY".
           05  WS-EDIT-MM            PIC 9(02).
           05  FILLER                PIC X(02) VALUE "MM".
       01  WS-COUNT-EDIT             PIC ZZZ,ZZZ,ZZ9.
       01  WS-COUNT-ABS              PIC 9(09) VALUE ZERO.

       01  WS-TARGET-YM.
           05  WS-TARGET-YYYY        PIC 9(04).
           05  WS-TARGET-MM          PIC 9(02).

       01  WS-WARN-SW                PIC X(01) VALUE "0".
           88  WS-WARN-ON                     VALUE "1".
       01  WS-HARD-ERR-SW            PIC X(01) VALUE "0".
           88  WS-HARD-ERR                    VALUE "1".

       01  WS-UNDEF-NAME             PIC X(40) VALUE "UNDEFINED".

       LINKAGE SECTION.
       01  CG260S-PARM.
           05  CG260S-IN.
               10  CG260S-CODE-KBN   PIC X(04).
               10  CG260S-CODE-VALUE PIC X(12).
               10  CG260S-TARGET-YM  PIC 9(06).
               10  CG260S-COUNT      PIC S9(09) COMP-3.
               10  CG260S-STATE-KBN  PIC X(04).
               10  CG260S-STATE-CD   PIC X(12).
           05  CG260S-OUT.
               10  CG260S-CODE-NAME  PIC X(40).
               10  CG260S-YM-NAME    PIC X(16).
               10  CG260S-COUNT-NAME PIC X(12).
               10  CG260S-STATE-NAME PIC X(40).
               10  CG260S-STATUS     PIC X(02).
               10  CG260S-REASON     PIC X(80).

       PROCEDURE DIVISION USING CG260S-PARM.
       0000-MAIN SECTION.
       0000-START.
           MOVE 0 TO RETURN-CODE
           PERFORM 1000-INIT
           IF NOT WS-HARD-ERR
               PERFORM 2000-OPEN
           END-IF
           IF NOT WS-HARD-ERR
               PERFORM 3000-EDIT-CODE-NAME
               PERFORM 4000-EDIT-YM
               PERFORM 5000-EDIT-COUNT
               PERFORM 6000-EDIT-STATE-NAME
           END-IF
           PERFORM 9000-END
           GOBACK
           .

       1000-INIT SECTION.
           MOVE SPACE TO CG260S-CODE-NAME
           MOVE SPACE TO CG260S-YM-NAME
           MOVE SPACE TO CG260S-COUNT-NAME
           MOVE SPACE TO CG260S-STATE-NAME
           MOVE SPACE TO CG260S-REASON
           MOVE "00" TO CG260S-STATUS
           MOVE "0" TO WS-WARN-SW
           MOVE "0" TO WS-HARD-ERR-SW
           MOVE "0" TO WS-OPEN-SW
           MOVE FUNCTION CURRENT-DATE(1:8) TO WS-CURRENT-DATE
           COMPUTE WS-CUR-DT =
               (WS-CUR-YYYY * 10000)
               + (WS-CUR-MM * 100)
               + WS-CUR-DD
           .

       2000-OPEN SECTION.
           OPEN INPUT CGCODF
           IF WS-CGCODF-ST = "00"
               MOVE "1" TO WS-OPEN-SW
           ELSE
               MOVE "1" TO WS-HARD-ERR-SW
               MOVE "90" TO CG260S-STATUS
               MOVE "CGCODF OPEN ERROR ST=" TO CG260S-REASON
               STRING CG260S-REASON DELIMITED BY SPACE
                      WS-CGCODF-ST  DELIMITED BY SIZE
                 INTO CG260S-REASON
               END-STRING
               MOVE 8 TO RETURN-CODE
           END-IF
           .

       3000-EDIT-CODE-NAME SECTION.
           PERFORM 3100-BUILD-CODE-KEY
           PERFORM 7000-READ-CODE
           IF WS-HARD-ERR
               CONTINUE
           ELSE
               IF CG260S-STATUS = "00"
                   MOVE GC-CODE-NAME TO CG260S-CODE-NAME
               ELSE
                   MOVE WS-UNDEF-NAME TO CG260S-CODE-NAME
                   PERFORM 8100-SET-WARNING
               END-IF
           END-IF
           .

       3100-BUILD-CODE-KEY SECTION.
           MOVE SPACE TO WS-WORK-KEY
           MOVE SPACE TO GC-CODE-ID
           STRING CG260S-CODE-KBN   DELIMITED BY SPACE
                  "-"               DELIMITED BY SIZE
                  CG260S-CODE-VALUE DELIMITED BY SPACE
             INTO WS-WORK-KEY
           END-STRING
           MOVE WS-WORK-KEY TO GC-CODE-ID
           .

       4000-EDIT-YM SECTION.
           MOVE CG260S-TARGET-YM TO WS-TARGET-YM
           IF CG260S-TARGET-YM < 190001
              OR CG260S-TARGET-YM > 209912
              OR WS-TARGET-MM < 1
              OR WS-TARGET-MM > 12
               MOVE "INVALID YM" TO CG260S-YM-NAME
               PERFORM 8100-SET-WARNING
               IF CG260S-REASON = SPACE
                   MOVE "INVALID YM" TO CG260S-REASON
               END-IF
           ELSE
               MOVE WS-TARGET-YYYY TO WS-EDIT-YYYY
               MOVE WS-TARGET-MM TO WS-EDIT-MM
               MOVE WS-EDIT-YM TO CG260S-YM-NAME
           END-IF
           .

       5000-EDIT-COUNT SECTION.
           IF CG260S-COUNT < 0
               COMPUTE WS-COUNT-ABS = CG260S-COUNT * -1
               MOVE WS-COUNT-ABS TO WS-COUNT-EDIT
               STRING "-"            DELIMITED BY SIZE
                      WS-COUNT-EDIT  DELIMITED BY SIZE
                 INTO CG260S-COUNT-NAME
               END-STRING
               PERFORM 8100-SET-WARNING
               IF CG260S-REASON = SPACE
                   MOVE "NEGATIVE COUNT" TO CG260S-REASON
               END-IF
           ELSE
               MOVE CG260S-COUNT TO WS-COUNT-ABS
               MOVE WS-COUNT-ABS TO WS-COUNT-EDIT
               MOVE WS-COUNT-EDIT TO CG260S-COUNT-NAME
           END-IF
           .

       6000-EDIT-STATE-NAME SECTION.
           PERFORM 6100-BUILD-STATE-KEY
           PERFORM 7000-READ-CODE
           IF WS-HARD-ERR
               CONTINUE
           ELSE
               IF CG260S-STATUS = "00"
                   MOVE GC-CODE-NAME TO CG260S-STATE-NAME
               ELSE
                   MOVE WS-UNDEF-NAME TO CG260S-STATE-NAME
                   PERFORM 8100-SET-WARNING
               END-IF
           END-IF
           .

       6100-BUILD-STATE-KEY SECTION.
           MOVE SPACE TO WS-WORK-KEY
           MOVE SPACE TO GC-CODE-ID
           STRING CG260S-STATE-KBN DELIMITED BY SPACE
                  "-"             DELIMITED BY SIZE
                  CG260S-STATE-CD DELIMITED BY SPACE
             INTO WS-WORK-KEY
           END-STRING
           MOVE WS-WORK-KEY TO GC-CODE-ID
           .

       7000-READ-CODE SECTION.
           READ CGCODF KEY IS GC-CODE-ID
           EVALUATE WS-CGCODF-ST
               WHEN "00"
                   IF GC-VALID-FROM-DT <= WS-CUR-DT
                      AND GC-VALID-TO-DT >= WS-CUR-DT
                       MOVE "00" TO CG260S-STATUS
                   ELSE
                       MOVE "04" TO CG260S-STATUS
                       IF CG260S-REASON = SPACE
                           MOVE "CODE EXPIRED" TO CG260S-REASON
                       END-IF
                   END-IF
               WHEN "23"
                   MOVE "04" TO CG260S-STATUS
                   IF CG260S-REASON = SPACE
                       MOVE "CODE NOT FOUND" TO CG260S-REASON
                   END-IF
               WHEN OTHER
                   MOVE "1" TO WS-HARD-ERR-SW
                   MOVE "90" TO CG260S-STATUS
                   MOVE "CGCODF READ ERROR ST=" TO CG260S-REASON
                   STRING CG260S-REASON DELIMITED BY SPACE
                          WS-CGCODF-ST  DELIMITED BY SIZE
                     INTO CG260S-REASON
                   END-STRING
                   MOVE 8 TO RETURN-CODE
           END-EVALUATE
           .

       8100-SET-WARNING SECTION.
           MOVE "1" TO WS-WARN-SW
           IF CG260S-STATUS = "00"
               MOVE "04" TO CG260S-STATUS
           END-IF
           .

       9000-END SECTION.
           IF WS-OPENED
               CLOSE CGCODF
               IF WS-CGCODF-ST NOT = "00"
                   MOVE "90" TO CG260S-STATUS
                   MOVE "CGCODF CLOSE ERROR ST=" TO CG260S-REASON
                   STRING CG260S-REASON DELIMITED BY SPACE
                          WS-CGCODF-ST  DELIMITED BY SIZE
                     INTO CG260S-REASON
                   END-STRING
                   MOVE 8 TO RETURN-CODE
               END-IF
           END-IF
           IF RETURN-CODE = 0
               IF WS-WARN-ON
                   MOVE "04" TO CG260S-STATUS
               ELSE
                   MOVE "00" TO CG260S-STATUS
               END-IF
           END-IF
           .
