       IDENTIFICATION DIVISION.
       PROGRAM-ID. CM180B.
      ******************************************************************
      * CM180B 名寄せ結果確定バッチ
      * 自動一致または承認済候補から結果ファイルを作成する。
      ******************************************************************

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CMDUPF ASSIGN TO "CMDUPF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS DP-CANDIDATE-ID
               FILE STATUS IS FS-CMDUPF.

           SELECT CMKEYF ASSIGN TO "CMKEYF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-CMKEYF.

           SELECT CMCIFF ASSIGN TO "CMCIFF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-CMCIFF.

           SELECT CMRSLF ASSIGN TO "CMRSLF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-CMRSLF.

           SELECT CKERRF ASSIGN TO "CKERRF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-CKERRF.

       DATA DIVISION.
       FILE SECTION.

       FD  CMDUPF.
           COPY CMDUPC.

       FD  CMKEYF.
           COPY CMKEYFC.

       FD  CMCIFF.
           COPY CMCIFFC.

       FD  CMRSLF.
           COPY CMRSLC.

       FD  CKERRF.
           COPY CKERRC.

       WORKING-STORAGE SECTION.
       01  WS-PGM-ID                   PIC X(08) VALUE "CM180B".
       01  WS-RUN-DT                   PIC 9(08) VALUE ZERO.
       01  WS-RUN-TM                   PIC 9(08) VALUE ZERO.

       01  FS-CMDUPF                   PIC XX VALUE SPACE.
       01  FS-CMKEYF                   PIC XX VALUE SPACE.
       01  FS-CMCIFF                   PIC XX VALUE SPACE.
       01  FS-CMRSLF                   PIC XX VALUE SPACE.
       01  FS-CKERRF                   PIC XX VALUE SPACE.

       01  SW-END-CMDUPF               PIC X VALUE "N".
           88  END-CMDUPF                    VALUE "Y".
       01  SW-END-CMKEYF               PIC X VALUE "N".
           88  END-CMKEYF                    VALUE "Y".
       01  SW-END-CMCIFF               PIC X VALUE "N".
           88  END-CMCIFF                    VALUE "Y".
       01  SW-HARD-ERR                 PIC X VALUE "N".
           88  HARD-ERR                      VALUE "Y".
       01  SW-KEY-FOUND                PIC X VALUE "N".
           88  KEY-FOUND                     VALUE "Y".
       01  SW-CIF1-FOUND               PIC X VALUE "N".
           88  CIF1-FOUND                    VALUE "Y".
       01  SW-CIF2-FOUND               PIC X VALUE "N".
           88  CIF2-FOUND                    VALUE "Y".

       01  CT-KEY                      PIC 9(5) COMP-5 VALUE ZERO.
       01  CT-CIF                      PIC 9(5) COMP-5 VALUE ZERO.
       01  IX                          PIC 9(5) COMP-5 VALUE ZERO.
       01  MAX-KEY                     PIC 9(5) COMP-5 VALUE 20000.
       01  MAX-CIF                     PIC 9(5) COMP-5 VALUE 30000.

       01  CT-READ-DUP                 PIC 9(9) VALUE ZERO.
       01  CT-SKIP-DUP                 PIC 9(9) VALUE ZERO.
       01  CT-OUT-RSL                  PIC 9(9) VALUE ZERO.
       01  CT-OUT-ERR                  PIC 9(9) VALUE ZERO.

       01  WS-RESULT-SEQ               PIC 9(9) VALUE ZERO.
       01  WS-ERROR-SEQ                PIC 9(9) VALUE ZERO.
       01  WS-RESULT-ID                PIC X(16) VALUE SPACE.
       01  WS-ERROR-ID                 PIC X(16) VALUE SPACE.

       01  WS-REP-CIF                  PIC X(10) VALUE SPACE.
       01  WS-PAIR-CIF                 PIC X(10) VALUE SPACE.
       01  WS-REP-STATUS               PIC XX VALUE SPACE.
       01  WS-PAIR-STATUS              PIC XX VALUE SPACE.
       01  WS-KEY-ID                   PIC X(20) VALUE SPACE.
       01  WS-KEY-STATUS               PIC XX VALUE SPACE.
       01  WS-CHECK-CNT                PIC 9(4) VALUE ZERO.
       01  WS-REASON-CD                PIC X(04) VALUE SPACE.
       01  WS-ERROR-CD                 PIC X(04) VALUE SPACE.

       01  WK-ID-EDIT                  PIC 9(9) VALUE ZERO.
       01  WK-SCORE                    PIC 9(3) VALUE ZERO.

       01  KEY-TABLE.
           05 KEY-ENT OCCURS 20000 TIMES.
              10 T-KEY-ID              PIC X(20).
              10 T-KEY-CIF             PIC X(10).
              10 T-CHK-CNT             PIC 9(4).
              10 T-KEY-STS             PIC XX.

       01  CIF-TABLE.
           05 CIF-ENT OCCURS 30000 TIMES.
              10 T-CIF-NO              PIC X(10).
              10 T-BIRTH-DT            PIC 9(8).
              10 T-SEX-KBN             PIC X.
              10 T-CIF-STS             PIC XX.

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE 0 TO RETURN-CODE
           PERFORM 0100-INIT
           IF NOT HARD-ERR
              PERFORM 1000-LOAD-CMKEYF
           END-IF
           IF NOT HARD-ERR
              PERFORM 1100-LOAD-CMCIFF
           END-IF
           IF NOT HARD-ERR
              PERFORM 2000-PROCESS-CMDUPF
           END-IF
           PERFORM 9000-FINAL
           GOBACK.

       0100-INIT.
           ACCEPT WS-RUN-DT FROM DATE YYYYMMDD
           ACCEPT WS-RUN-TM FROM TIME
           DISPLAY "CM180B START DATE=" WS-RUN-DT
                   " TIME=" WS-RUN-TM

           OPEN INPUT CMDUPF
           IF FS-CMDUPF NOT = "00"
              DISPLAY "CMDUPF OPEN ERROR ST=" FS-CMDUPF
              MOVE "Y" TO SW-HARD-ERR
              MOVE 8 TO RETURN-CODE
           END-IF

           OPEN INPUT CMKEYF
           IF FS-CMKEYF NOT = "00"
              DISPLAY "CMKEYF OPEN ERROR ST=" FS-CMKEYF
              MOVE "Y" TO SW-HARD-ERR
              MOVE 8 TO RETURN-CODE
           END-IF

           OPEN INPUT CMCIFF
           IF FS-CMCIFF NOT = "00"
              DISPLAY "CMCIFF OPEN ERROR ST=" FS-CMCIFF
              MOVE "Y" TO SW-HARD-ERR
              MOVE 8 TO RETURN-CODE
           END-IF

           OPEN OUTPUT CMRSLF
           IF FS-CMRSLF NOT = "00"
              DISPLAY "CMRSLF OPEN ERROR ST=" FS-CMRSLF
              MOVE "Y" TO SW-HARD-ERR
              MOVE 8 TO RETURN-CODE
           END-IF

           OPEN OUTPUT CKERRF
           IF FS-CKERRF NOT = "00"
              DISPLAY "CKERRF OPEN ERROR ST=" FS-CKERRF
              MOVE "Y" TO SW-HARD-ERR
              MOVE 8 TO RETURN-CODE
           END-IF.

       1000-LOAD-CMKEYF.
           PERFORM UNTIL END-CMKEYF OR HARD-ERR
              READ CMKEYF
                 AT END
                    SET END-CMKEYF TO TRUE
                 NOT AT END
                    IF FS-CMKEYF NOT = "00"
                       DISPLAY "CMKEYF READ ERROR ST=" FS-CMKEYF
                       MOVE "Y" TO SW-HARD-ERR
                       MOVE 8 TO RETURN-CODE
                    ELSE
                       PERFORM 1010-STORE-KEY
                    END-IF
              END-READ
           END-PERFORM.

       1010-STORE-KEY.
           IF CT-KEY >= MAX-KEY
              DISPLAY "CMKEYF TOO MANY RECORDS"
              MOVE "Y" TO SW-HARD-ERR
              MOVE 12 TO RETURN-CODE
           ELSE
              ADD 1 TO CT-KEY
              MOVE CK-KEY-ID TO T-KEY-ID(CT-KEY)
              MOVE CK-CIF-NO TO T-KEY-CIF(CT-KEY)
              MOVE CK-CHECK-DIGIT-CNT TO T-CHK-CNT(CT-KEY)
              MOVE CK-KEY-STATUS-KBN TO T-KEY-STS(CT-KEY)
           END-IF.

       1100-LOAD-CMCIFF.
           PERFORM UNTIL END-CMCIFF OR HARD-ERR
              READ CMCIFF
                 AT END
                    SET END-CMCIFF TO TRUE
                 NOT AT END
                    IF FS-CMCIFF NOT = "00"
                       DISPLAY "CMCIFF READ ERROR ST=" FS-CMCIFF
                       MOVE "Y" TO SW-HARD-ERR
                       MOVE 8 TO RETURN-CODE
                    ELSE
                       PERFORM 1110-STORE-CIF
                    END-IF
              END-READ
           END-PERFORM.

       1110-STORE-CIF.
           IF CT-CIF >= MAX-CIF
              DISPLAY "CMCIFF TOO MANY RECORDS"
              MOVE "Y" TO SW-HARD-ERR
              MOVE 12 TO RETURN-CODE
           ELSE
              ADD 1 TO CT-CIF
              MOVE CF-CIF-NO TO T-CIF-NO(CT-CIF)
              MOVE CF-BIRTH-DT TO T-BIRTH-DT(CT-CIF)
              MOVE CF-SEX-KBN TO T-SEX-KBN(CT-CIF)
              MOVE CF-CIF-STATUS-KBN TO T-CIF-STS(CT-CIF)
           END-IF.

       2000-PROCESS-CMDUPF.
           PERFORM UNTIL END-CMDUPF OR HARD-ERR
              READ CMDUPF NEXT RECORD
                 AT END
                    SET END-CMDUPF TO TRUE
                 NOT AT END
                    IF FS-CMDUPF NOT = "00"
                       DISPLAY "CMDUPF READ ERROR ST=" FS-CMDUPF
                       MOVE "Y" TO SW-HARD-ERR
                       MOVE 8 TO RETURN-CODE
                    ELSE
                       ADD 1 TO CT-READ-DUP
                       PERFORM 2100-JUDGE-CANDIDATE
                    END-IF
              END-READ
           END-PERFORM.

       2100-JUDGE-CANDIDATE.
           MOVE DP-MATCH-SCORE TO WK-SCORE
           IF DP-JUDGE-KBN = "1"
              PERFORM 2200-CONFIRM-CANDIDATE
           ELSE
              IF DP-JUDGE-KBN = "A" AND WK-SCORE >= 095
                 PERFORM 2200-CONFIRM-CANDIDATE
              ELSE
                 ADD 1 TO CT-SKIP-DUP
              END-IF
           END-IF.

       2200-CONFIRM-CANDIDATE.
           IF DP-CIF-NO-1 <= DP-CIF-NO-2
              MOVE DP-CIF-NO-1 TO WS-REP-CIF
              MOVE DP-CIF-NO-2 TO WS-PAIR-CIF
           ELSE
              MOVE DP-CIF-NO-2 TO WS-REP-CIF
              MOVE DP-CIF-NO-1 TO WS-PAIR-CIF
           END-IF

           MOVE "N" TO SW-CIF1-FOUND
           MOVE "N" TO SW-CIF2-FOUND
           MOVE SPACE TO WS-REP-STATUS
           MOVE SPACE TO WS-PAIR-STATUS
           PERFORM 2300-FIND-CIF

           IF NOT CIF1-FOUND
              MOVE "C101" TO WS-ERROR-CD
              MOVE WS-REP-CIF TO ER-CIF-NO
              MOVE SPACE TO ER-KEY-ID
              PERFORM 8200-WRITE-ERROR
           ELSE
              IF NOT CIF2-FOUND
                 MOVE "C102" TO WS-ERROR-CD
                 MOVE WS-PAIR-CIF TO ER-CIF-NO
                 MOVE SPACE TO ER-KEY-ID
                 PERFORM 8200-WRITE-ERROR
              ELSE
                 PERFORM 2400-CHECK-CIF-STATUS
              END-IF
           END-IF.

       2300-FIND-CIF.
           PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > CT-CIF
              IF T-CIF-NO(IX) = WS-REP-CIF
                 MOVE T-CIF-STS(IX) TO WS-REP-STATUS
                 SET CIF1-FOUND TO TRUE
              END-IF
              IF T-CIF-NO(IX) = WS-PAIR-CIF
                 MOVE T-CIF-STS(IX) TO WS-PAIR-STATUS
                 SET CIF2-FOUND TO TRUE
              END-IF
           END-PERFORM.

       2400-CHECK-CIF-STATUS.
           IF WS-REP-STATUS = "01" AND WS-PAIR-STATUS = "01"
              PERFORM 2500-FIND-KEY
              IF KEY-FOUND
                 PERFORM 2600-OUTPUT-CONFIRMED
              ELSE
                 PERFORM 2700-OUTPUT-PENDING
              END-IF
           ELSE
              IF WS-REP-STATUS NOT = "01"
                 MOVE "C201" TO WS-ERROR-CD
                 MOVE WS-REP-CIF TO ER-CIF-NO
                 MOVE SPACE TO ER-KEY-ID
                 PERFORM 8200-WRITE-ERROR
              END-IF
              IF WS-PAIR-STATUS NOT = "01"
                 MOVE "C202" TO WS-ERROR-CD
                 MOVE WS-PAIR-CIF TO ER-CIF-NO
                 MOVE SPACE TO ER-KEY-ID
                 PERFORM 8200-WRITE-ERROR
              END-IF
           END-IF.

       2500-FIND-KEY.
           MOVE "N" TO SW-KEY-FOUND
           MOVE SPACE TO WS-KEY-ID
           MOVE SPACE TO WS-KEY-STATUS
           MOVE ZERO TO WS-CHECK-CNT
           PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > CT-KEY
              IF T-KEY-CIF(IX) = WS-REP-CIF
                 MOVE T-KEY-ID(IX) TO WS-KEY-ID
                 MOVE T-KEY-STS(IX) TO WS-KEY-STATUS
                 MOVE T-CHK-CNT(IX) TO WS-CHECK-CNT
                 SET KEY-FOUND TO TRUE
                 MOVE CT-KEY TO IX
              END-IF
           END-PERFORM.

       2600-OUTPUT-CONFIRMED.
           IF WS-KEY-STATUS = "01"
              IF WS-CHECK-CNT = ZERO
                 MOVE "K301" TO WS-ERROR-CD
                 MOVE WS-REP-CIF TO ER-CIF-NO
                 MOVE WS-KEY-ID TO ER-KEY-ID
                 PERFORM 8200-WRITE-ERROR
              ELSE
                 MOVE "0000" TO WS-REASON-CD
                 MOVE "1" TO RS-RESULT-KBN
                 PERFORM 8100-WRITE-RESULT
              END-IF
           ELSE
              MOVE "K302" TO WS-ERROR-CD
              MOVE WS-REP-CIF TO ER-CIF-NO
              MOVE WS-KEY-ID TO ER-KEY-ID
              PERFORM 8200-WRITE-ERROR
           END-IF.

       2700-OUTPUT-PENDING.
           MOVE "1100" TO WS-REASON-CD
           MOVE "9" TO RS-RESULT-KBN
           MOVE SPACE TO WS-KEY-ID
           PERFORM 8100-WRITE-RESULT.

       8100-WRITE-RESULT.
           ADD 1 TO WS-RESULT-SEQ
           MOVE WS-RESULT-SEQ TO WK-ID-EDIT
           STRING "RS" WS-RUN-DT WK-ID-EDIT
              DELIMITED BY SIZE INTO WS-RESULT-ID
           END-STRING

           INITIALIZE CMRSLF-REC
           MOVE WS-RESULT-ID TO RS-RESULT-ID
           MOVE WS-REP-CIF TO RS-CIF-NO
           MOVE WS-KEY-ID TO RS-KEY-ID
           MOVE WS-REASON-CD TO RS-REASON-CD
           MOVE WS-RUN-DT TO RS-OUTPUT-DT

           WRITE CMRSLF-REC
           IF FS-CMRSLF NOT = "00"
              DISPLAY "CMRSLF WRITE ERROR ST=" FS-CMRSLF
              MOVE "Y" TO SW-HARD-ERR
              MOVE 8 TO RETURN-CODE
           ELSE
              ADD 1 TO CT-OUT-RSL
           END-IF.

       8200-WRITE-ERROR.
           ADD 1 TO WS-ERROR-SEQ
           MOVE WS-ERROR-SEQ TO WK-ID-EDIT
           STRING "ER" WS-RUN-DT WK-ID-EDIT
              DELIMITED BY SIZE INTO WS-ERROR-ID
           END-STRING

           INITIALIZE CKERRF-REC
           MOVE WS-ERROR-ID TO ER-ERROR-ID
           MOVE WS-PGM-ID TO ER-SOURCE-PGM-ID
           MOVE WS-ERROR-CD TO ER-ERROR-CD
           MOVE WS-RUN-DT TO ER-ERROR-DT

           WRITE CKERRF-REC
           IF FS-CKERRF NOT = "00"
              DISPLAY "CKERRF WRITE ERROR ST=" FS-CKERRF
              MOVE "Y" TO SW-HARD-ERR
              MOVE 8 TO RETURN-CODE
           ELSE
              ADD 1 TO CT-OUT-ERR
              DISPLAY "CHECK ERROR CIF=" ER-CIF-NO
                      " KEY=" ER-KEY-ID
                      " CD=" ER-ERROR-CD
           END-IF.

       9000-FINAL.
           CLOSE CMDUPF
           IF FS-CMDUPF NOT = "00"
              DISPLAY "CMDUPF CLOSE ERROR ST=" FS-CMDUPF
              MOVE 8 TO RETURN-CODE
           END-IF

           CLOSE CMKEYF
           IF FS-CMKEYF NOT = "00"
              DISPLAY "CMKEYF CLOSE ERROR ST=" FS-CMKEYF
              MOVE 8 TO RETURN-CODE
           END-IF

           CLOSE CMCIFF
           IF FS-CMCIFF NOT = "00"
              DISPLAY "CMCIFF CLOSE ERROR ST=" FS-CMCIFF
              MOVE 8 TO RETURN-CODE
           END-IF

           CLOSE CMRSLF
           IF FS-CMRSLF NOT = "00"
              DISPLAY "CMRSLF CLOSE ERROR ST=" FS-CMRSLF
              MOVE 8 TO RETURN-CODE
           END-IF

           CLOSE CKERRF
           IF FS-CKERRF NOT = "00"
              DISPLAY "CKERRF CLOSE ERROR ST=" FS-CKERRF
              MOVE 8 TO RETURN-CODE
           END-IF

           DISPLAY "CM180B END"
           DISPLAY "READ DUP COUNT=" CT-READ-DUP
           DISPLAY "SKIP DUP COUNT=" CT-SKIP-DUP
           DISPLAY "RESULT OUT COUNT=" CT-OUT-RSL
           DISPLAY "ERROR OUT COUNT=" CT-OUT-ERR.
