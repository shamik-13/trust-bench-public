       IDENTIFICATION DIVISION.
       PROGRAM-ID. CK210S.
       AUTHOR.     BATCH-SECTION.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CMKEYF ASSIGN TO "CMKEYF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-CMKEYF-ST.
           SELECT CGCODF ASSIGN TO "CGCODF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS GC-CODE-ID
               FILE STATUS IS WS-CGCODF-ST.

       DATA DIVISION.
       FILE SECTION.
       FD  CMKEYF.
       COPY CMKEYFC.

       FD  CGCODF.
       COPY CGCODC.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05  WS-CMKEYF-ST             PIC XX VALUE SPACE.
           05  WS-CGCODF-ST             PIC XX VALUE SPACE.

       01  WS-SWITCHES.
           05  WS-CMKEYF-EOF-SW         PIC X VALUE "0".
               88  CMKEYF-EOF                VALUE "1".
           05  WS-CGCODF-EOF-SW         PIC X VALUE "0".
               88  CGCODF-EOF                VALUE "1".
           05  WS-KEY-FOUND-SW          PIC X VALUE "0".
               88  KEY-FOUND                 VALUE "1".
           05  WS-CODE-FOUND-SW         PIC X VALUE "0".
               88  CODE-FOUND                VALUE "1".
           05  WS-HARD-ERROR-SW         PIC X VALUE "0".
               88  HARD-ERROR                VALUE "1".

       01  WS-CONSTANTS.
           05  WC-CODE-KBN-KEYSTAT      PIC X(10) VALUE "KEYSTATUS".
           05  WC-STAT-SEND             PIC XX VALUE "01".
           05  WC-STAT-HOLD             PIC XX VALUE "02".
           05  WC-STAT-STOP             PIC XX VALUE "03".
           05  WC-RSLT-SEND             PIC X  VALUE "1".
           05  WC-RSLT-HOLD             PIC X  VALUE "2".
           05  WC-RSLT-STOP             PIC X  VALUE "3".
           05  WC-RSLT-NOTFOUND         PIC X  VALUE "7".
           05  WC-RSLT-ERROR            PIC X  VALUE "9".
           05  WC-RTN-NORMAL            PIC 9  VALUE 0.
           05  WC-RTN-WARN              PIC 9  VALUE 4.
           05  WC-RTN-ERROR             PIC 9  VALUE 8.
           05  WC-RANGE-MIN             PIC 9(4) VALUE 0.
           05  WC-RANGE-MAX             PIC 9(4) VALUE 9999.

       01  WS-WORK.
           05  WS-BATCH-DATE            PIC 9(8) VALUE 0.
           05  WS-DATE-AREA.
               10  WS-DATE-YYYY         PIC 9(4).
               10  WS-DATE-MM           PIC 9(2).
               10  WS-DATE-DD           PIC 9(2).
           05  WS-DIGIT-CNT             PIC 9(4) VALUE 0.

       LINKAGE SECTION.
       01  LK-CK210S-PARM.
           05  LK-KEY-ID                PIC X(20).
           05  LK-RESULT-KBN            PIC X.
           05  LK-KEY-STATUS-KBN        PIC XX.
           05  LK-CHECK-DIGIT-CNT       PIC 9(4).
           05  LK-REASON-CD             PIC X(4).
           05  LK-REASON-TEXT           PIC X(60).

       PROCEDURE DIVISION USING LK-CK210S-PARM.
       MAIN-SECTION.
           PERFORM 1000-INIT
           PERFORM 2000-OPEN-FILES
           IF NOT HARD-ERROR
               PERFORM 3000-CHECK-INPUT
           END-IF
           IF NOT HARD-ERROR
               PERFORM 4000-FIND-KEY
           END-IF
           IF NOT HARD-ERROR
               IF KEY-FOUND
                   PERFORM 5000-CHECK-CODE
                   IF NOT HARD-ERROR
                       PERFORM 6000-SET-RESULT
                   END-IF
               ELSE
                   MOVE WC-RSLT-NOTFOUND TO LK-RESULT-KBN
                   MOVE "0004"           TO LK-REASON-CD
                   MOVE "KEY NOT FOUND"  TO LK-REASON-TEXT
                   MOVE WC-RTN-WARN      TO RETURN-CODE
               END-IF
           END-IF
           PERFORM 9000-CLOSE-FILES
           IF HARD-ERROR
               MOVE WC-RSLT-ERROR TO LK-RESULT-KBN
               MOVE WC-RTN-ERROR  TO RETURN-CODE
           END-IF
           GOBACK.

       1000-INIT.
           MOVE WC-RTN-NORMAL TO RETURN-CODE
           MOVE SPACE         TO LK-RESULT-KBN
           MOVE SPACE         TO LK-KEY-STATUS-KBN
           MOVE ZERO          TO LK-CHECK-DIGIT-CNT
           MOVE SPACE         TO LK-REASON-CD
           MOVE SPACE         TO LK-REASON-TEXT
           ACCEPT WS-DATE-AREA FROM DATE YYYYMMDD
           MOVE WS-DATE-AREA  TO WS-BATCH-DATE
           MOVE "0"           TO WS-HARD-ERROR-SW
           MOVE "0"           TO WS-CMKEYF-EOF-SW
           MOVE "0"           TO WS-CGCODF-EOF-SW
           MOVE "0"           TO WS-KEY-FOUND-SW
           MOVE "0"           TO WS-CODE-FOUND-SW.

       2000-OPEN-FILES.
           OPEN INPUT CMKEYF
           IF WS-CMKEYF-ST NOT = "00"
               DISPLAY "CMKEYF OPEN ERR " WS-CMKEYF-ST
               MOVE "9001"             TO LK-REASON-CD
               MOVE "CMKEYF OPEN ERR"  TO LK-REASON-TEXT
               MOVE "1"                TO WS-HARD-ERROR-SW
           END-IF
           IF NOT HARD-ERROR
               OPEN INPUT CGCODF
               IF WS-CGCODF-ST NOT = "00"
                   DISPLAY "CGCODF OPEN ERR " WS-CGCODF-ST
                   MOVE "9002"             TO LK-REASON-CD
                   MOVE "CGCODF OPEN ERR"  TO LK-REASON-TEXT
                   MOVE "1"                TO WS-HARD-ERROR-SW
               END-IF
           END-IF.

       3000-CHECK-INPUT.
           IF LK-KEY-ID = SPACE
               DISPLAY "KEY-ID REQUIRED"
               MOVE "0001"          TO LK-REASON-CD
               MOVE "KEY-ID REQUIRED" TO LK-REASON-TEXT
               MOVE "1"             TO WS-HARD-ERROR-SW
           END-IF.

       4000-FIND-KEY.
           PERFORM UNTIL CMKEYF-EOF OR KEY-FOUND OR HARD-ERROR
               READ CMKEYF
                   AT END
                       MOVE "1" TO WS-CMKEYF-EOF-SW
                   NOT AT END
                       IF WS-CMKEYF-ST = "00"
                           IF CK-KEY-ID = LK-KEY-ID
                               MOVE "1" TO WS-KEY-FOUND-SW
                               PERFORM 4100-CHECK-KEY-REC
                           END-IF
                       ELSE
                           DISPLAY "CMKEYF READ ERR " WS-CMKEYF-ST
                           MOVE "9003"             TO LK-REASON-CD
                           MOVE "CMKEYF READ ERR"  TO LK-REASON-TEXT
                           MOVE "1"                TO WS-HARD-ERROR-SW
                       END-IF
               END-READ
           END-PERFORM.

       4100-CHECK-KEY-REC.
           IF CK-CHECK-DIGIT-CNT IS NOT NUMERIC
               DISPLAY "CHECK-DIGIT-CNT NUM ERR " CK-KEY-ID
               MOVE "0002"          TO LK-REASON-CD
               MOVE "DIGIT CNT NUM ERR" TO LK-REASON-TEXT
               MOVE "1"             TO WS-HARD-ERROR-SW
           ELSE
               MOVE CK-CHECK-DIGIT-CNT TO WS-DIGIT-CNT
               IF WS-DIGIT-CNT < WC-RANGE-MIN
                  OR WS-DIGIT-CNT > WC-RANGE-MAX
                   DISPLAY "CHECK-DIGIT-CNT RNG ERR " CK-KEY-ID
                   MOVE "0003"          TO LK-REASON-CD
                   MOVE "DIGIT CNT RNG ERR" TO LK-REASON-TEXT
                   MOVE "1"             TO WS-HARD-ERROR-SW
               ELSE
                   MOVE CK-CHECK-DIGIT-CNT TO LK-CHECK-DIGIT-CNT
                   MOVE CK-KEY-STATUS-KBN  TO LK-KEY-STATUS-KBN
               END-IF
           END-IF.

       5000-CHECK-CODE.
           MOVE "0"                  TO WS-CGCODF-EOF-SW
           MOVE "0"                  TO WS-CODE-FOUND-SW
           MOVE WC-CODE-KBN-KEYSTAT  TO GC-CODE-KBN
           MOVE LK-KEY-STATUS-KBN    TO GC-CODE-VALUE
           START CGCODF KEY IS NOT LESS THAN GC-CODE-ID
               INVALID KEY
                   MOVE "1" TO WS-CGCODF-EOF-SW
           END-START
           PERFORM UNTIL CGCODF-EOF OR CODE-FOUND OR HARD-ERROR
               READ CGCODF NEXT RECORD
                   AT END
                       MOVE "1" TO WS-CGCODF-EOF-SW
                   NOT AT END
                       IF WS-CGCODF-ST = "00"
                           IF GC-CODE-KBN NOT = WC-CODE-KBN-KEYSTAT
                               MOVE "1" TO WS-CGCODF-EOF-SW
                           ELSE
                               PERFORM 5100-MATCH-CODE
                           END-IF
                       ELSE
                           DISPLAY "CGCODF READ ERR " WS-CGCODF-ST
                           MOVE "9004"             TO LK-REASON-CD
                           MOVE "CGCODF READ ERR"  TO LK-REASON-TEXT
                           MOVE "1"                TO WS-HARD-ERROR-SW
                       END-IF
               END-READ
           END-PERFORM
           IF NOT HARD-ERROR AND NOT CODE-FOUND
               DISPLAY "KEY STATUS CODE NOT FOUND "
                   LK-KEY-STATUS-KBN
               MOVE "0005"              TO LK-REASON-CD
               MOVE "STATUS CODE NOT FOUND" TO LK-REASON-TEXT
               MOVE "1"                 TO WS-HARD-ERROR-SW
           END-IF.

       5100-MATCH-CODE.
           IF GC-CODE-KBN = WC-CODE-KBN-KEYSTAT
              AND GC-CODE-VALUE = LK-KEY-STATUS-KBN
               IF GC-VALID-FROM-DT <= WS-BATCH-DATE
                  AND GC-VALID-TO-DT >= WS-BATCH-DATE
                   MOVE "1" TO WS-CODE-FOUND-SW
               ELSE
                   DISPLAY "KEY STATUS CODE EXPIRED "
                       LK-KEY-STATUS-KBN
                   MOVE "0006"          TO LK-REASON-CD
                   MOVE "STATUS CODE EXPIRED" TO LK-REASON-TEXT
                   MOVE "1"             TO WS-HARD-ERROR-SW
               END-IF
           END-IF.

       6000-SET-RESULT.
           EVALUATE LK-KEY-STATUS-KBN
               WHEN WC-STAT-SEND
                   MOVE WC-RSLT-SEND TO LK-RESULT-KBN
                   MOVE "0000"       TO LK-REASON-CD
                   MOVE "SEND OK"    TO LK-REASON-TEXT
               WHEN WC-STAT-HOLD
                   MOVE WC-RSLT-HOLD TO LK-RESULT-KBN
                   MOVE "0000"       TO LK-REASON-CD
                   MOVE "HOLD"       TO LK-REASON-TEXT
               WHEN WC-STAT-STOP
                   MOVE WC-RSLT-STOP TO LK-RESULT-KBN
                   MOVE "0000"       TO LK-REASON-CD
                   MOVE "STOP"       TO LK-REASON-TEXT
               WHEN OTHER
                   DISPLAY "KEY STATUS NOT TARGET "
                       LK-KEY-STATUS-KBN
                   MOVE "0007"       TO LK-REASON-CD
                   MOVE "STATUS NOT TARGET" TO LK-REASON-TEXT
                   MOVE "1"          TO WS-HARD-ERROR-SW
           END-EVALUATE.

       9000-CLOSE-FILES.
           IF WS-CMKEYF-ST = "00"
               CLOSE CMKEYF
               IF WS-CMKEYF-ST NOT = "00"
                   DISPLAY "CMKEYF CLOSE ERR " WS-CMKEYF-ST
                   MOVE "9005"              TO LK-REASON-CD
                   MOVE "CMKEYF CLOSE ERR"  TO LK-REASON-TEXT
                   MOVE "1"                 TO WS-HARD-ERROR-SW
               END-IF
           END-IF
           IF WS-CGCODF-ST = "00"
               CLOSE CGCODF
               IF WS-CGCODF-ST NOT = "00"
                   DISPLAY "CGCODF CLOSE ERR " WS-CGCODF-ST
                   MOVE "9006"              TO LK-REASON-CD
                   MOVE "CGCODF CLOSE ERR"  TO LK-REASON-TEXT
                   MOVE "1"                 TO WS-HARD-ERROR-SW
               END-IF
           END-IF.
