       IDENTIFICATION DIVISION.
       PROGRAM-ID. KZ190B.
      * 版数  年月日(和暦)   担当                     概要
      * 1.00  令和04.04.01  システム部 勘定系チーム  新規作成
      * 1.01  令和05.10.16  システム部 勘定系チーム  消費税区分判定を追加
      * 1.02  令和06.07.08  システム部 勘定系チーム  日次GL仕訳集計を見直し

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZTRANF ASSIGN TO "KZTRANF"
              ORGANIZATION IS SEQUENTIAL
              ACCESS MODE IS SEQUENTIAL
              FILE STATUS IS FS-KZTRANF.

           SELECT KZFEEHF ASSIGN TO "KZFEEHF"
              ORGANIZATION IS SEQUENTIAL
              ACCESS MODE IS SEQUENTIAL
              FILE STATUS IS FS-KZFEEHF.

           SELECT KZINTAF ASSIGN TO "KZINTAF"
              ORGANIZATION IS SEQUENTIAL
              ACCESS MODE IS SEQUENTIAL
              FILE STATUS IS FS-KZINTAF.

           SELECT KZGLDTF ASSIGN TO "KZGLDTF"
              ORGANIZATION IS SEQUENTIAL
              ACCESS MODE IS SEQUENTIAL
              FILE STATUS IS FS-KZGLDTF.

       DATA DIVISION.
       FILE SECTION.
       FD  KZTRANF.
       COPY KZTRANC.

       FD  KZFEEHF.
       COPY KZFEEHC.

       FD  KZINTAF.
       COPY KZINTAFC.

       FD  KZGLDTF.
       COPY KZGLDTFC.

       WORKING-STORAGE SECTION.
       01  FS-AREA.
           05 FS-KZTRANF             PIC XX VALUE SPACES.
           05 FS-KZFEEHF             PIC XX VALUE SPACES.
           05 FS-KZINTAF             PIC XX VALUE SPACES.
           05 FS-KZGLDTF             PIC XX VALUE SPACES.

       01  EOF-AREA.
           05 EOF-KZTRANF            PIC X VALUE "N".
              88 EOF-TRAN                 VALUE "Y".
           05 EOF-KZFEEHF            PIC X VALUE "N".
              88 EOF-FEE                  VALUE "Y".
           05 EOF-KZINTAF            PIC X VALUE "N".
              88 EOF-INT                  VALUE "Y".

       01  OPEN-AREA.
           05 OPEN-KZTRANF           PIC X VALUE "N".
              88 KZTRANF-OPEN             VALUE "Y".
           05 OPEN-KZFEEHF           PIC X VALUE "N".
              88 KZFEEHF-OPEN             VALUE "Y".
           05 OPEN-KZINTAF           PIC X VALUE "N".
              88 KZINTAF-OPEN             VALUE "Y".
           05 OPEN-KZGLDTF           PIC X VALUE "N".
              88 KZGLDTF-OPEN             VALUE "Y".

       01  BATCH-AREA.
           05 WS-BATCH-ID            PIC X(12) VALUE SPACES.
           05 WS-POST-DT             PIC 9(8)  VALUE ZERO.
           05 WS-SEQ                 PIC 9(6)  VALUE ZERO.

       01  TRAN-GROUP-AREA.
           05 WS-CUR-ACCT-ID         PIC X(20) VALUE SPACES.
           05 WS-TRAN-DEBIT-TOT      PIC S9(15)V99 COMP-3 VALUE ZERO.
           05 WS-TRAN-CREDIT-TOT     PIC S9(15)V99 COMP-3 VALUE ZERO.
           05 WS-GROUP-HAS-DATA      PIC X VALUE "N".
              88 GROUP-HAS-DATA            VALUE "Y".

       01  GL-WORK-AREA.
           05 WS-SRC-KBN             PIC X VALUE SPACES.
           05 WS-JUDGE-CD            PIC X(4) VALUE SPACES.
           05 WS-RATE-CD             PIC X(4) VALUE SPACES.
           05 WS-RAW-AMT             PIC S9(15)V99 COMP-3 VALUE ZERO.
           05 WS-OUT-AMT             PIC S9(15)V99 COMP-3 VALUE ZERO.
           05 WS-ENTRY-CD            PIC X(8) VALUE SPACES.
           05 WS-DR-CR-KBN           PIC X VALUE SPACES.
           05 WS-ERROR-SW            PIC X VALUE "N".
              88 HAS-ERROR                 VALUE "Y".

       01  KZ310S-PARM.
           05 KZ310-SRC-KBN          PIC X.
           05 KZ310-TRAN-CD          PIC X(4).
           05 KZ310-RATE-CD          PIC X(4).
           05 KZ310-ENTRY-CD         PIC X(8).
           05 KZ310-DR-CR-KBN        PIC X.
           05 KZ310-RETURN-CD        PIC 9(2).

       COPY LK-RND-PARM.

       PROCEDURE DIVISION.
       MAIN-RTN.
           PERFORM INIT-RTN
           IF NOT HAS-ERROR
              PERFORM PROCESS-TRANF
              PERFORM PROCESS-FEEHF
              PERFORM PROCESS-INTAF
           END-IF
           PERFORM TERM-RTN
           STOP RUN.

       INIT-RTN.
           ACCEPT WS-POST-DT FROM DATE YYYYMMDD
           STRING "KZ" WS-POST-DT "GL"
              DELIMITED BY SIZE INTO WS-BATCH-ID
           END-STRING

           OPEN INPUT KZTRANF
           IF FS-KZTRANF NOT = "00"
              DISPLAY "KZ190B KZTRANF OPEN ERR " FS-KZTRANF
              SET HAS-ERROR TO TRUE
           ELSE
              MOVE "Y" TO OPEN-KZTRANF
           END-IF

           OPEN INPUT KZFEEHF
           IF FS-KZFEEHF NOT = "00"
              DISPLAY "KZ190B KZFEEHF OPEN ERR " FS-KZFEEHF
              SET HAS-ERROR TO TRUE
           ELSE
              MOVE "Y" TO OPEN-KZFEEHF
           END-IF

           OPEN INPUT KZINTAF
           IF FS-KZINTAF NOT = "00"
              DISPLAY "KZ190B KZINTAF OPEN ERR " FS-KZINTAF
              SET HAS-ERROR TO TRUE
           ELSE
              MOVE "Y" TO OPEN-KZINTAF
           END-IF

           OPEN OUTPUT KZGLDTF
           IF FS-KZGLDTF NOT = "00"
              DISPLAY "KZ190B KZGLDTF OPEN ERR " FS-KZGLDTF
              SET HAS-ERROR TO TRUE
           ELSE
              MOVE "Y" TO OPEN-KZGLDTF
           END-IF.

       PROCESS-TRANF.
           PERFORM READ-TRANF
           PERFORM UNTIL EOF-TRAN
              IF TR-ACCT-ID = SPACES
                 DISPLAY "KZ190B TR ACCT BLANK"
              ELSE
                 IF NOT GROUP-HAS-DATA
                    MOVE TR-ACCT-ID TO WS-CUR-ACCT-ID
                    MOVE "Y" TO WS-GROUP-HAS-DATA
                 END-IF
                 IF TR-ACCT-ID NOT = WS-CUR-ACCT-ID
                    PERFORM WRITE-TRAN-GROUP
                    MOVE TR-ACCT-ID TO WS-CUR-ACCT-ID
                    MOVE ZERO TO WS-TRAN-DEBIT-TOT
                    MOVE ZERO TO WS-TRAN-CREDIT-TOT
                 END-IF
                 PERFORM ADD-TRAN-AMOUNT
              END-IF
              PERFORM READ-TRANF
           END-PERFORM

           IF GROUP-HAS-DATA
              PERFORM WRITE-TRAN-GROUP
           END-IF.

       READ-TRANF.
           READ KZTRANF
              AT END
                 MOVE "Y" TO EOF-KZTRANF
              NOT AT END
                 IF FS-KZTRANF NOT = "00"
                    DISPLAY "KZ190B KZTRANF READ ERR " FS-KZTRANF
                    MOVE "Y" TO EOF-KZTRANF
                    SET HAS-ERROR TO TRUE
                 END-IF
           END-READ.

       ADD-TRAN-AMOUNT.
           IF TR-TRAN-AMT > ZERO
              EVALUATE TR-TRAN-CD
                 WHEN "DEP "
                 WHEN "INT "
                    ADD TR-TRAN-AMT TO WS-TRAN-CREDIT-TOT
                 WHEN "WDL "
                 WHEN "PAY "
                    ADD TR-TRAN-AMT TO WS-TRAN-DEBIT-TOT
                 WHEN OTHER
                    DISPLAY "KZ190B TR CODE ERR " TR-ACCT-ID
                            " " TR-TRAN-CD
              END-EVALUATE
           ELSE
              DISPLAY "KZ190B TR AMT ERR " TR-ACCT-ID
           END-IF.

       WRITE-TRAN-GROUP.
           IF WS-TRAN-DEBIT-TOT > ZERO
              MOVE "T" TO WS-SRC-KBN
              MOVE "WDL " TO WS-JUDGE-CD
              MOVE SPACES TO WS-RATE-CD
              MOVE WS-TRAN-DEBIT-TOT TO WS-RAW-AMT
              PERFORM DECIDE-AND-WRITE-GL
           END-IF

           IF WS-TRAN-CREDIT-TOT > ZERO
              MOVE "T" TO WS-SRC-KBN
              MOVE "DEP " TO WS-JUDGE-CD
              MOVE SPACES TO WS-RATE-CD
              MOVE WS-TRAN-CREDIT-TOT TO WS-RAW-AMT
              PERFORM DECIDE-AND-WRITE-GL
           END-IF.

       PROCESS-FEEHF.
           PERFORM READ-FEEHF
           PERFORM UNTIL EOF-FEE
              IF FE-ACCT-ID = SPACES
                 DISPLAY "KZ190B FE ACCT BLANK"
              ELSE
                 IF FE-FEE-AMT > ZERO
                    IF FE-EXEMPT-FLAG NOT = "Y"
                       MOVE FE-ACCT-ID TO WS-CUR-ACCT-ID
                       MOVE "F" TO WS-SRC-KBN
                       IF FE-CAP-FLAG = "Y"
                          MOVE "FCAP" TO WS-JUDGE-CD
                       ELSE
                          MOVE "FEE " TO WS-JUDGE-CD
                       END-IF
                       MOVE SPACES TO WS-RATE-CD
                       MOVE FE-FEE-AMT TO WS-RAW-AMT
                       PERFORM DECIDE-AND-WRITE-GL
                    END-IF
                 ELSE
                    DISPLAY "KZ190B FE AMT ERR " FE-ACCT-ID
                 END-IF
              END-IF
              PERFORM READ-FEEHF
           END-PERFORM.

       READ-FEEHF.
           READ KZFEEHF
              AT END
                 MOVE "Y" TO EOF-KZFEEHF
              NOT AT END
                 IF FS-KZFEEHF NOT = "00"
                    DISPLAY "KZ190B KZFEEHF READ ERR " FS-KZFEEHF
                    MOVE "Y" TO EOF-KZFEEHF
                    SET HAS-ERROR TO TRUE
                 END-IF
           END-READ.

       PROCESS-INTAF.
           PERFORM READ-INTAF
           PERFORM UNTIL EOF-INT
              IF IA-ACCT-ID = SPACES
                 DISPLAY "KZ190B IA ACCT BLANK"
              ELSE
                 IF IA-INT-AMT > ZERO AND IA-CYCLE-BAL > ZERO
                    MOVE IA-ACCT-ID TO WS-CUR-ACCT-ID
                    MOVE "I" TO WS-SRC-KBN
                    MOVE "INT " TO WS-JUDGE-CD
                    MOVE SPACES TO WS-RATE-CD
                    MOVE IA-INT-AMT TO WS-RAW-AMT
                    PERFORM DECIDE-AND-WRITE-GL
                 ELSE
                    IF IA-INT-AMT < ZERO
                       DISPLAY "KZ190B IA AMT ERR " IA-ACCT-ID
                    END-IF
                 END-IF
              END-IF
              PERFORM READ-INTAF
           END-PERFORM.

       READ-INTAF.
           READ KZINTAF
              AT END
                 MOVE "Y" TO EOF-KZINTAF
              NOT AT END
                 IF FS-KZINTAF NOT = "00"
                    DISPLAY "KZ190B KZINTAF READ ERR " FS-KZINTAF
                    MOVE "Y" TO EOF-KZINTAF
                    SET HAS-ERROR TO TRUE
                 END-IF
           END-READ.

       DECIDE-AND-WRITE-GL.
           MOVE WS-SRC-KBN TO KZ310-SRC-KBN
           MOVE WS-JUDGE-CD TO KZ310-TRAN-CD
           MOVE WS-RATE-CD TO KZ310-RATE-CD
           MOVE SPACES TO KZ310-ENTRY-CD
           MOVE SPACES TO KZ310-DR-CR-KBN
           MOVE ZERO TO KZ310-RETURN-CD

           CALL "KZ310S" USING KZ310S-PARM
           IF KZ310-RETURN-CD NOT = ZERO
              DISPLAY "KZ190B KZ310S ERR " WS-CUR-ACCT-ID
              EXIT PARAGRAPH
           END-IF

           MOVE WS-RAW-AMT TO LK-AMT-RAW
           MOVE ZERO TO LK-AMT-FLOORED
           CALL "KZ130S" USING LK-RND-PARM
           MOVE LK-AMT-FLOORED TO WS-OUT-AMT

           IF WS-OUT-AMT > ZERO
              MOVE KZ310-ENTRY-CD TO WS-ENTRY-CD
              MOVE KZ310-DR-CR-KBN TO WS-DR-CR-KBN
              PERFORM WRITE-GL
           ELSE
              DISPLAY "KZ190B GL ZERO AFTER ROUND " WS-CUR-ACCT-ID
           END-IF.

       WRITE-GL.
           ADD 1 TO WS-SEQ
           MOVE WS-BATCH-ID TO GL-GL-BATCH-ID
           MOVE WS-CUR-ACCT-ID TO GL-ACCT-ID
           MOVE WS-ENTRY-CD TO GL-ENTRY-CD
           MOVE WS-DR-CR-KBN TO GL-DR-CR-KBN
           MOVE WS-OUT-AMT TO GL-ENTRY-AMT
           MOVE WS-POST-DT TO GL-POST-DT

           WRITE KZGLDTF-REC
           IF FS-KZGLDTF NOT = "00"
              DISPLAY "KZ190B KZGLDTF WRITE ERR " FS-KZGLDTF
              SET HAS-ERROR TO TRUE
           END-IF.

       TERM-RTN.
           IF KZTRANF-OPEN
              CLOSE KZTRANF
              IF FS-KZTRANF NOT = "00"
                 DISPLAY "KZ190B KZTRANF CLOSE ERR " FS-KZTRANF
              END-IF
           END-IF

           IF KZFEEHF-OPEN
              CLOSE KZFEEHF
              IF FS-KZFEEHF NOT = "00"
                 DISPLAY "KZ190B KZFEEHF CLOSE ERR " FS-KZFEEHF
              END-IF
           END-IF

           IF KZINTAF-OPEN
              CLOSE KZINTAF
              IF FS-KZINTAF NOT = "00"
                 DISPLAY "KZ190B KZINTAF CLOSE ERR " FS-KZINTAF
              END-IF
           END-IF

           IF KZGLDTF-OPEN
              CLOSE KZGLDTF
              IF FS-KZGLDTF NOT = "00"
                 DISPLAY "KZ190B KZGLDTF CLOSE ERR " FS-KZGLDTF
              END-IF
           END-IF

           IF HAS-ERROR
              DISPLAY "KZ190B ABEND GL COUNT " WS-SEQ
           ELSE
              DISPLAY "KZ190B NORMAL END GL COUNT " WS-SEQ
           END-IF.
