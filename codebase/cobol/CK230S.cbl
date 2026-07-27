       IDENTIFICATION DIVISION.
       PROGRAM-ID. CK230S.
       AUTHOR. CIF-BATCH.
      *
      *================================================================*
      *  連携送信可否判定サブ                                          *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CKLNKF ASSIGN TO "CKLNKF"
               ORGANIZATION IS INDEXED
               ACCESS MODE  IS DYNAMIC
               RECORD KEY   IS LK-LINK-ID
               FILE STATUS  IS FS-CKLNKF.
           SELECT CMKEYF ASSIGN TO "CMKEYF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE  IS SEQUENTIAL
               FILE STATUS  IS FS-CMKEYF.
           SELECT CGCODF ASSIGN TO "CGCODF"
               ORGANIZATION IS INDEXED
               ACCESS MODE  IS DYNAMIC
               RECORD KEY   IS GC-CODE-ID
               FILE STATUS  IS FS-CGCODF.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  CKLNKF.
           COPY CKLNKC.
       FD  CMKEYF.
           COPY CMKEYFC.
       FD  CGCODF.
           COPY CGCODC.
      *
       WORKING-STORAGE SECTION.
       01  FS-AREA.
           05 FS-CKLNKF              PIC XX VALUE SPACE.
           05 FS-CMKEYF              PIC XX VALUE SPACE.
           05 FS-CGCODF              PIC XX VALUE SPACE.
      *
       01  SW-AREA.
           05 SW-LINK-FOUND          PIC X VALUE "0".
              88 LINK-FOUND               VALUE "1".
           05 SW-KEY-FOUND           PIC X VALUE "0".
              88 KEY-FOUND                VALUE "1".
           05 SW-EOF-CMKEYF          PIC X VALUE "0".
              88 EOF-CMKEYF               VALUE "1".
           05 SW-HARD-ERR            PIC X VALUE "0".
              88 HARD-ERR                 VALUE "1".
           05 SW-BUSI-ERR            PIC X VALUE "0".
              88 BUSI-ERR                 VALUE "1".
      *
       01  WK-AREA.
           05 WK-LINK-ID             PIC X(40) VALUE SPACE.
           05 WK-CODE-ID             PIC X(20) VALUE SPACE.
           05 WK-ERR-CD              PIC X(8)  VALUE SPACE.
           05 WK-ERR-MSG             PIC X(80) VALUE SPACE.
           05 WK-STAT-NAME           PIC X(40) VALUE SPACE.
           05 WK-BASE-DT-X           PIC X(8)  VALUE SPACE.
           05 WK-BASE-DT-N           PIC 9(8)  VALUE ZERO.
           05 WK-BASE-MM             PIC 99    VALUE ZERO.
           05 WK-BASE-DD             PIC 99    VALUE ZERO.
           05 WK-DATE-OK             PIC X VALUE "0".
           05 WK-CHECK-DIGIT-CNT     PIC 9(4) VALUE ZERO.
      *
       01  CNST-AREA.
           05 CN-LINK-STAT-KBN       PIC X(8) VALUE "LNKSTAT".
           05 CN-KEY-STAT-KBN        PIC X(8) VALUE "KEYSTAT".
           05 CN-LINK-SENT           PIC XX VALUE "01".
           05 CN-LINK-HOLD           PIC XX VALUE "02".
           05 CN-LINK-ERR            PIC XX VALUE "03".
           05 CN-LINK-STOP           PIC XX VALUE "09".
           05 CN-KEY-ACTIVE          PIC XX VALUE "01".
           05 CN-KEY-EXCLUDE         PIC XX VALUE "08".
           05 CN-KEY-INVALID         PIC XX VALUE "09".
           05 CN-RSLT-SENT           PIC XX VALUE "01".
           05 CN-RSLT-HOLD           PIC XX VALUE "02".
           05 CN-RSLT-RESEND         PIC XX VALUE "03".
           05 CN-RSLT-STOP           PIC XX VALUE "04".
           05 CN-RSLT-BUSIERR        PIC XX VALUE "90".
      *
       LINKAGE SECTION.
       01  PARM-AREA.
           05 PA-TARGET-SYS-ID       PIC X(10).
           05 PA-KEY-ID              PIC X(20).
           05 PA-BASE-DT             PIC 9(8).
           05 PA-RESULT-KBN          PIC XX.
           05 PA-ERR-CD              PIC X(8).
           05 PA-ERR-MSG             PIC X(80).
           05 PA-CHECK-DIGIT-CNT     PIC 9(4).
      *
       PROCEDURE DIVISION USING PARM-AREA.
       MAIN-RTN.
           PERFORM INIT-RTN
           PERFORM PARAM-CHECK-RTN
           IF NOT HARD-ERR AND NOT BUSI-ERR
               PERFORM OPEN-RTN
           END-IF
           IF NOT HARD-ERR AND NOT BUSI-ERR
               PERFORM READ-LINK-RTN
           END-IF
           IF NOT HARD-ERR AND NOT BUSI-ERR
               PERFORM READ-KEY-RTN
           END-IF
           IF NOT HARD-ERR AND NOT BUSI-ERR
               PERFORM DECIDE-RTN
           END-IF
           PERFORM CLOSE-RTN
           PERFORM RETURN-RTN
           GOBACK.
      *
       INIT-RTN.
           MOVE 0              TO RETURN-CODE
           MOVE SPACE          TO PA-RESULT-KBN
           MOVE SPACE          TO PA-ERR-CD
           MOVE SPACE          TO PA-ERR-MSG
           MOVE ZERO           TO PA-CHECK-DIGIT-CNT
           MOVE "0"            TO SW-LINK-FOUND
           MOVE "0"            TO SW-KEY-FOUND
           MOVE "0"            TO SW-EOF-CMKEYF
           MOVE "0"            TO SW-HARD-ERR
           MOVE "0"            TO SW-BUSI-ERR
           MOVE SPACE          TO WK-ERR-CD
           MOVE SPACE          TO WK-ERR-MSG
           MOVE SPACE          TO WK-STAT-NAME
           MOVE SPACE          TO FS-CKLNKF
           MOVE SPACE          TO FS-CMKEYF
           MOVE SPACE          TO FS-CGCODF
           MOVE ZERO           TO WK-CHECK-DIGIT-CNT.
      *
       PARAM-CHECK-RTN.
           IF PA-TARGET-SYS-ID = SPACE
               MOVE "1" TO SW-BUSI-ERR
               MOVE "E230001" TO WK-ERR-CD
               MOVE "TARGET SYS ID REQUIRED" TO WK-ERR-MSG
           END-IF
           IF NOT BUSI-ERR AND PA-KEY-ID = SPACE
               MOVE "1" TO SW-BUSI-ERR
               MOVE "E230002" TO WK-ERR-CD
               MOVE "KEY ID REQUIRED" TO WK-ERR-MSG
           END-IF
           IF NOT BUSI-ERR
               MOVE PA-BASE-DT TO WK-BASE-DT-N
               PERFORM DATE-CHECK-RTN
               IF WK-DATE-OK NOT = "1"
                   MOVE "1" TO SW-BUSI-ERR
                   MOVE "E230003" TO WK-ERR-CD
                   MOVE "INVALID BASE DATE" TO WK-ERR-MSG
               END-IF
           END-IF.
      *
       DATE-CHECK-RTN.
           MOVE "1" TO WK-DATE-OK
           MOVE PA-BASE-DT TO WK-BASE-DT-X
           MOVE WK-BASE-DT-X(5:2) TO WK-BASE-MM
           MOVE WK-BASE-DT-X(7:2) TO WK-BASE-DD
           IF PA-BASE-DT < 19000101 OR PA-BASE-DT > 20991231
               MOVE "0" TO WK-DATE-OK
           END-IF
           IF WK-DATE-OK = "1"
              AND (WK-BASE-MM < 1 OR WK-BASE-MM > 12)
               MOVE "0" TO WK-DATE-OK
           END-IF
           IF WK-DATE-OK = "1"
              AND (WK-BASE-DD < 1 OR WK-BASE-DD > 31)
               MOVE "0" TO WK-DATE-OK
           END-IF.
      *
       OPEN-RTN.
           OPEN INPUT CKLNKF CMKEYF CGCODF
           IF FS-CKLNKF NOT = "00"
               MOVE "1" TO SW-HARD-ERR
               MOVE "A230101" TO WK-ERR-CD
               STRING "CKLNKF OPEN ST=" FS-CKLNKF
                   DELIMITED BY SIZE INTO WK-ERR-MSG
               END-STRING
           END-IF
           IF NOT HARD-ERR AND FS-CMKEYF NOT = "00"
               MOVE "1" TO SW-HARD-ERR
               MOVE "A230102" TO WK-ERR-CD
               STRING "CMKEYF OPEN ST=" FS-CMKEYF
                   DELIMITED BY SIZE INTO WK-ERR-MSG
               END-STRING
           END-IF
           IF NOT HARD-ERR AND FS-CGCODF NOT = "00"
               MOVE "1" TO SW-HARD-ERR
               MOVE "A230103" TO WK-ERR-CD
               STRING "CGCODF OPEN ST=" FS-CGCODF
                   DELIMITED BY SIZE INTO WK-ERR-MSG
               END-STRING
           END-IF.
      *
       READ-LINK-RTN.
           MOVE SPACE TO WK-LINK-ID
           STRING PA-TARGET-SYS-ID PA-KEY-ID
               DELIMITED BY SIZE INTO WK-LINK-ID
           END-STRING
           MOVE WK-LINK-ID TO LK-LINK-ID
           READ CKLNKF KEY IS LK-LINK-ID
               INVALID KEY
                   IF FS-CKLNKF = "23"
                       MOVE "1" TO SW-BUSI-ERR
                       MOVE "E230201" TO WK-ERR-CD
                       MOVE "LINK STATUS NOT FOUND" TO WK-ERR-MSG
                   ELSE
                       MOVE "1" TO SW-HARD-ERR
                       MOVE "A230201" TO WK-ERR-CD
                       STRING "CKLNKF READ ST=" FS-CKLNKF
                           DELIMITED BY SIZE INTO WK-ERR-MSG
                       END-STRING
                   END-IF
               NOT INVALID KEY
                   MOVE "1" TO SW-LINK-FOUND
           END-READ
           IF LINK-FOUND
              AND (LK-KEY-ID NOT = PA-KEY-ID
              OR LK-TARGET-SYS-ID NOT = PA-TARGET-SYS-ID)
               MOVE "1" TO SW-BUSI-ERR
               MOVE "E230202" TO WK-ERR-CD
               MOVE "LINK KEY MISMATCH" TO WK-ERR-MSG
           END-IF
           IF LINK-FOUND AND NOT BUSI-ERR
               MOVE CN-LINK-STAT-KBN TO GC-CODE-KBN
               MOVE LK-SEND-STATUS-KBN TO GC-CODE-VALUE
               PERFORM CODE-CHECK-RTN
               IF BUSI-ERR
                   MOVE "E230203" TO WK-ERR-CD
                   MOVE "INVALID LINK STATUS" TO WK-ERR-MSG
               END-IF
           END-IF.
      *
       READ-KEY-RTN.
           PERFORM UNTIL EOF-CMKEYF OR KEY-FOUND OR HARD-ERR
               READ CMKEYF
                   AT END
                       MOVE "1" TO SW-EOF-CMKEYF
                   NOT AT END
                       IF CK-KEY-ID = PA-KEY-ID
                           MOVE "1" TO SW-KEY-FOUND
                           MOVE CK-CHECK-DIGIT-CNT
                             TO WK-CHECK-DIGIT-CNT
                       END-IF
               END-READ
               IF FS-CMKEYF NOT = "00" AND FS-CMKEYF NOT = "10"
                   MOVE "1" TO SW-HARD-ERR
                   MOVE "A230301" TO WK-ERR-CD
                   STRING "CMKEYF READ ST=" FS-CMKEYF
                       DELIMITED BY SIZE INTO WK-ERR-MSG
                   END-STRING
               END-IF
           END-PERFORM
           IF NOT HARD-ERR AND NOT KEY-FOUND
               MOVE "1" TO SW-BUSI-ERR
               MOVE "E230301" TO WK-ERR-CD
               MOVE "KEY STATUS NOT FOUND" TO WK-ERR-MSG
           END-IF
           IF KEY-FOUND AND NOT BUSI-ERR
               MOVE CN-KEY-STAT-KBN TO GC-CODE-KBN
               MOVE CK-KEY-STATUS-KBN TO GC-CODE-VALUE
               PERFORM CODE-CHECK-RTN
               IF BUSI-ERR
                   MOVE "E230302" TO WK-ERR-CD
                   MOVE "INVALID KEY STATUS" TO WK-ERR-MSG
               END-IF
           END-IF
           IF KEY-FOUND AND NOT BUSI-ERR
              AND LK-CIF-NO NOT = CK-CIF-NO
               MOVE "1" TO SW-BUSI-ERR
               MOVE "E230303" TO WK-ERR-CD
               MOVE "CIF NO MISMATCH" TO WK-ERR-MSG
           END-IF.
      *
       CODE-CHECK-RTN.
           MOVE SPACE TO GC-CODE-ID
           STRING GC-CODE-KBN GC-CODE-VALUE
               DELIMITED BY SIZE INTO GC-CODE-ID
           END-STRING
           READ CGCODF KEY IS GC-CODE-ID
               INVALID KEY
                   IF FS-CGCODF = "23"
                       MOVE "1" TO SW-BUSI-ERR
                   ELSE
                       MOVE "1" TO SW-HARD-ERR
                       MOVE "A230401" TO WK-ERR-CD
                       STRING "CGCODF READ ST=" FS-CGCODF
                           DELIMITED BY SIZE INTO WK-ERR-MSG
                       END-STRING
                   END-IF
               NOT INVALID KEY
                   MOVE GC-CODE-NAME TO WK-STAT-NAME
           END-READ
           IF NOT HARD-ERR AND NOT BUSI-ERR
              AND (PA-BASE-DT < GC-VALID-FROM-DT
              OR PA-BASE-DT > GC-VALID-TO-DT)
               MOVE "1" TO SW-BUSI-ERR
           END-IF.
      *
       DECIDE-RTN.
           EVALUATE TRUE
               WHEN LK-SEND-STATUS-KBN = CN-LINK-SENT
                AND CK-KEY-STATUS-KBN = CN-KEY-ACTIVE
                   MOVE CN-RSLT-SENT TO PA-RESULT-KBN
               WHEN LK-SEND-STATUS-KBN = CN-LINK-HOLD
                AND CK-KEY-STATUS-KBN = CN-KEY-ACTIVE
                   MOVE CN-RSLT-HOLD TO PA-RESULT-KBN
               WHEN LK-SEND-STATUS-KBN = CN-LINK-ERR
                AND CK-KEY-STATUS-KBN = CN-KEY-ACTIVE
                   MOVE CN-RSLT-RESEND TO PA-RESULT-KBN
               WHEN LK-SEND-STATUS-KBN = CN-LINK-STOP
                AND CK-KEY-STATUS-KBN = CN-KEY-ACTIVE
                   MOVE CN-RSLT-STOP TO PA-RESULT-KBN
               WHEN LK-SEND-STATUS-KBN = CN-LINK-STOP
                AND (CK-KEY-STATUS-KBN = CN-KEY-EXCLUDE
                 OR CK-KEY-STATUS-KBN = CN-KEY-INVALID)
                   MOVE CN-RSLT-STOP TO PA-RESULT-KBN
               WHEN LK-SEND-STATUS-KBN NOT = CN-LINK-STOP
                AND (CK-KEY-STATUS-KBN = CN-KEY-EXCLUDE
                 OR CK-KEY-STATUS-KBN = CN-KEY-INVALID)
                   MOVE "1" TO SW-BUSI-ERR
                   MOVE "E230501" TO WK-ERR-CD
                   MOVE "LINK/KEY STATUS CONFLICT" TO WK-ERR-MSG
               WHEN OTHER
                   MOVE "1" TO SW-BUSI-ERR
                   MOVE "E230502" TO WK-ERR-CD
                   MOVE "SEND DECISION FAILED" TO WK-ERR-MSG
           END-EVALUATE
           IF NOT BUSI-ERR
               MOVE WK-CHECK-DIGIT-CNT TO PA-CHECK-DIGIT-CNT
           END-IF.
      *
       CLOSE-RTN.
           IF FS-CKLNKF = "00"
               CLOSE CKLNKF
               IF FS-CKLNKF NOT = "00" AND NOT HARD-ERR
                   MOVE "1" TO SW-HARD-ERR
                   MOVE "A230901" TO WK-ERR-CD
                   STRING "CKLNKF CLOSE ST=" FS-CKLNKF
                       DELIMITED BY SIZE INTO WK-ERR-MSG
                   END-STRING
               END-IF
           END-IF
           IF FS-CMKEYF = "00" OR FS-CMKEYF = "10"
               CLOSE CMKEYF
               IF FS-CMKEYF NOT = "00" AND NOT HARD-ERR
                   MOVE "1" TO SW-HARD-ERR
                   MOVE "A230902" TO WK-ERR-CD
                   STRING "CMKEYF CLOSE ST=" FS-CMKEYF
                       DELIMITED BY SIZE INTO WK-ERR-MSG
                   END-STRING
               END-IF
           END-IF
           IF FS-CGCODF = "00"
               CLOSE CGCODF
               IF FS-CGCODF NOT = "00" AND NOT HARD-ERR
                   MOVE "1" TO SW-HARD-ERR
                   MOVE "A230903" TO WK-ERR-CD
                   STRING "CGCODF CLOSE ST=" FS-CGCODF
                       DELIMITED BY SIZE INTO WK-ERR-MSG
                   END-STRING
               END-IF
           END-IF.
      *
       RETURN-RTN.
           IF HARD-ERR
               MOVE 12 TO RETURN-CODE
               MOVE WK-ERR-CD TO PA-ERR-CD
               MOVE WK-ERR-MSG TO PA-ERR-MSG
               DISPLAY WK-ERR-MSG
           ELSE
               IF BUSI-ERR
                   MOVE 8 TO RETURN-CODE
                   MOVE CN-RSLT-BUSIERR TO PA-RESULT-KBN
                   MOVE WK-ERR-CD TO PA-ERR-CD
                   MOVE WK-ERR-MSG TO PA-ERR-MSG
                   DISPLAY WK-ERR-MSG
               ELSE
                   MOVE 0 TO RETURN-CODE
                   MOVE SPACE TO PA-ERR-CD
                   MOVE SPACE TO PA-ERR-MSG
               END-IF
           END-IF.
