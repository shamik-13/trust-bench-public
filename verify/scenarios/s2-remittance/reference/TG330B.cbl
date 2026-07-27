       IDENTIFICATION DIVISION.
       PROGRAM-ID. TG330B.
       AUTHOR. TRUST-BANK-SYSTEM.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TGINRMF ASSIGN TO "TGINRMF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-TGINRMF.

           SELECT KZACCTF ASSIGN TO "KZACCTF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS AC-ACCT-NO
               FILE STATUS IS FS-KZACCTF.

           SELECT TGOUTCF ASSIGN TO "TGOUTCF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-TGOUTCF.

       DATA DIVISION.
       FILE SECTION.
       FD  TGINRMF.
           COPY TGINRMFC.

       FD  KZACCTF.
           COPY KZACCTC2.

       FD  TGOUTCF.
           COPY TGOUTCFC.

       WORKING-STORAGE SECTION.
       01  FS-AREA.
           05 FS-TGINRMF              PIC XX VALUE SPACES.
           05 FS-KZACCTF              PIC XX VALUE SPACES.
           05 FS-TGOUTCF              PIC XX VALUE SPACES.

       01  SW-AREA.
           05 SW-EOF                  PIC X VALUE "N".
              88 EOF-TGINRMF               VALUE "Y".
              88 NOT-EOF-TGINRMF           VALUE "N".
           05 SW-HARD-ERR             PIC X VALUE "N".
              88 HARD-ERROR                VALUE "Y".
              88 NO-HARD-ERROR             VALUE "N".
           05 SW-REJECT               PIC X VALUE "N".
              88 REJECT-REC                VALUE "Y".
              88 ACCEPT-REC                VALUE "N".
           05 SW-TGINRMF-OPEN         PIC X VALUE "N".
              88 TGINRMF-OPEN              VALUE "Y".
           05 SW-KZACCTF-OPEN         PIC X VALUE "N".
              88 KZACCTF-OPEN              VALUE "Y".
           05 SW-TGOUTCF-OPEN         PIC X VALUE "N".
              88 TGOUTCF-OPEN              VALUE "Y".

       01  COUNT-AREA.
           05 CNT-READ                PIC 9(9) VALUE ZERO.
           05 CNT-SKIP                PIC 9(9) VALUE ZERO.
           05 CNT-WRITE               PIC 9(9) VALUE ZERO.
           05 CNT-REJECT              PIC 9(9) VALUE ZERO.

       01  WORK-AREA.
           05 WK-REJ-REASON           PIC X(04) VALUE SPACES.
           05 WK-CORRECTION-TYPE      PIC X(02) VALUE SPACES.
           05 WK-DISPLAY-AMT          PIC Z,ZZZ,ZZZ,ZZ9.
           05 WK-ZERO-AMT             PIC 9(11) VALUE ZERO.

       01  CONST-AREA.
           05 CN-RT-CORRECTION        PIC X(02) VALUE "90".
           05 CN-AC-VALID             PIC X(02) VALUE "01".
           05 CN-AMT-LIMIT            PIC 9(11) VALUE 10000000.
           05 CN-CT-NAME              PIC X(02) VALUE "01".
           05 CN-CT-ACCT              PIC X(02) VALUE "02".
           05 CN-CT-BANK              PIC X(02) VALUE "03".

       01  REASON-CODES.
           05 RJ-NAME-MISMATCH        PIC X(04) VALUE "N1".
           05 RJ-NO-ACCOUNT           PIC X(04) VALUE "A1".
           05 RJ-BAD-STATUS           PIC X(04) VALUE "A2".
           05 RJ-BAD-FORMAT           PIC X(04) VALUE "F1".
           05 RJ-LIMIT-OVER           PIC X(04) VALUE "L1".
           05 RJ-NOT-TARGET           PIC X(04) VALUE "S1".

           COPY LK-SIG-PARM.

       PROCEDURE DIVISION.
       MAIN-SECTION.
           PERFORM 1000-INIT
           IF NO-HARD-ERROR
              PERFORM 2000-MAIN UNTIL EOF-TGINRMF OR HARD-ERROR
           END-IF
           PERFORM 9000-FINAL
           GOBACK.

       1000-INIT.
           MOVE 0 TO RETURN-CODE
           SET NOT-EOF-TGINRMF TO TRUE
           SET NO-HARD-ERROR TO TRUE

           OPEN INPUT TGINRMF
           IF FS-TGINRMF = "00"
              MOVE "Y" TO SW-TGINRMF-OPEN
           ELSE
              DISPLAY "TGINRMF OPEN ERROR ST=" FS-TGINRMF
              MOVE 8 TO RETURN-CODE
              SET HARD-ERROR TO TRUE
           END-IF

           IF NO-HARD-ERROR
              OPEN INPUT KZACCTF
              IF FS-KZACCTF = "00"
                 MOVE "Y" TO SW-KZACCTF-OPEN
              ELSE
                 DISPLAY "KZACCTF OPEN ERROR ST=" FS-KZACCTF
                 MOVE 8 TO RETURN-CODE
                 SET HARD-ERROR TO TRUE
              END-IF
           END-IF

           IF NO-HARD-ERROR
              OPEN OUTPUT TGOUTCF
              IF FS-TGOUTCF = "00"
                 MOVE "Y" TO SW-TGOUTCF-OPEN
              ELSE
                 DISPLAY "TGOUTCF OPEN ERROR ST=" FS-TGOUTCF
                 MOVE 8 TO RETURN-CODE
                 SET HARD-ERROR TO TRUE
              END-IF
           END-IF.

       2000-MAIN.
           READ TGINRMF
              AT END
                 SET EOF-TGINRMF TO TRUE
              NOT AT END
                 ADD 1 TO CNT-READ
                 PERFORM 2100-PROCESS-INPUT
           END-READ

           IF FS-TGINRMF NOT = "00" AND FS-TGINRMF NOT = "10"
              DISPLAY "TGINRMF READ ERROR ST=" FS-TGINRMF
              MOVE 8 TO RETURN-CODE
              SET HARD-ERROR TO TRUE
           END-IF.

       2100-PROCESS-INPUT.
           IF IR-REMIT-TYPE NOT = CN-RT-CORRECTION
              ADD 1 TO CNT-SKIP
           ELSE
              SET ACCEPT-REC TO TRUE
              MOVE SPACES TO WK-REJ-REASON
              PERFORM 2200-EDIT-CORRECTION-TYPE
              IF ACCEPT-REC
                 PERFORM 2300-VALIDATE-FORMAT
              END-IF
              IF ACCEPT-REC
                 PERFORM 2400-READ-ACCOUNT
              END-IF
              IF ACCEPT-REC AND NO-HARD-ERROR
                 PERFORM 2500-VALIDATE-ACCOUNT
              END-IF
              IF ACCEPT-REC AND NO-HARD-ERROR
                 PERFORM 2600-WRITE-OUTBOUND
              ELSE
                 IF REJECT-REC
                    PERFORM 2700-DISPLAY-REJECT
                 END-IF
              END-IF
           END-IF.

       2200-EDIT-CORRECTION-TYPE.
           MOVE IR-REMIT-MSG(1:2) TO WK-CORRECTION-TYPE
           IF WK-CORRECTION-TYPE NOT = CN-CT-NAME
              AND WK-CORRECTION-TYPE NOT = CN-CT-ACCT
              AND WK-CORRECTION-TYPE NOT = CN-CT-BANK
              MOVE RJ-BAD-FORMAT TO WK-REJ-REASON
              SET REJECT-REC TO TRUE
           END-IF.

       2300-VALIDATE-FORMAT.
           IF IR-CENTER-SEQ = SPACES
              OR IR-REMIT-DT = SPACES
              OR IR-PAYEE-BANK = SPACES
              OR IR-PAYEE-BRANCH = SPACES
              OR IR-PAYEE-ACCT-TYPE = SPACES
              OR IR-PAYEE-ACCT-NO = SPACES
              OR IR-PAYEE-NAME-KANA = SPACES
              MOVE RJ-BAD-FORMAT TO WK-REJ-REASON
              SET REJECT-REC TO TRUE
           END-IF

           IF ACCEPT-REC AND IR-REMIT-AMT <= WK-ZERO-AMT
              MOVE RJ-BAD-FORMAT TO WK-REJ-REASON
              SET REJECT-REC TO TRUE
           END-IF

           IF ACCEPT-REC AND IR-REMIT-AMT > CN-AMT-LIMIT
              MOVE RJ-LIMIT-OVER TO WK-REJ-REASON
              SET REJECT-REC TO TRUE
           END-IF.

       2400-READ-ACCOUNT.
           MOVE IR-PAYEE-ACCT-NO TO AC-ACCT-NO
           READ KZACCTF KEY IS AC-ACCT-NO
              INVALID KEY
                 MOVE RJ-NO-ACCOUNT TO WK-REJ-REASON
                 SET REJECT-REC TO TRUE
              NOT INVALID KEY
                 CONTINUE
           END-READ

           IF FS-KZACCTF NOT = "00" AND FS-KZACCTF NOT = "23"
              DISPLAY "KZACCTF READ ERROR ST=" FS-KZACCTF
              MOVE 8 TO RETURN-CODE
              SET HARD-ERROR TO TRUE
           END-IF.

       2500-VALIDATE-ACCOUNT.
           IF AC-STATUS NOT = CN-AC-VALID
              MOVE RJ-BAD-STATUS TO WK-REJ-REASON
              SET REJECT-REC TO TRUE
           END-IF

           IF ACCEPT-REC
              IF AC-BRANCH NOT = IR-PAYEE-BRANCH
                 OR AC-ACCT-TYPE NOT = IR-PAYEE-ACCT-TYPE
                 MOVE RJ-NO-ACCOUNT TO WK-REJ-REASON
                 SET REJECT-REC TO TRUE
              END-IF
           END-IF

           IF ACCEPT-REC
              IF WK-CORRECTION-TYPE = CN-CT-NAME
                 AND AC-ACCT-NAME-KANA NOT = IR-PAYEE-NAME-KANA
                 MOVE RJ-NAME-MISMATCH TO WK-REJ-REASON
                 SET REJECT-REC TO TRUE
              END-IF
           END-IF.

       2600-WRITE-OUTBOUND.
           INITIALIZE TGOUTCF-REC
           MOVE IR-REMIT-DT          TO OC-CORRECTION-DT
           MOVE IR-CENTER-SEQ        TO OC-ORIG-CENTER-SEQ
           MOVE WK-CORRECTION-TYPE   TO OC-CORRECTION-TYPE
           MOVE IR-SENDER-BANK       TO OC-SENDER-BANK
           MOVE IR-SENDER-BRANCH     TO OC-SENDER-BRANCH
           MOVE IR-PAYEE-BANK        TO OC-PAYEE-BANK
           MOVE IR-PAYEE-BRANCH      TO OC-PAYEE-BRANCH
           MOVE IR-PAYEE-ACCT-TYPE   TO OC-PAYEE-ACCT-TYPE
           MOVE IR-PAYEE-ACCT-NO     TO OC-PAYEE-ACCT-NO
           MOVE IR-PAYEE-NAME-KANA   TO OC-PAYEE-NAME-KANA
           MOVE IR-REMIT-AMT         TO OC-OUT-AMT

           INITIALIZE LK-SIG-PARM
           MOVE IR-PAYEE-BRANCH      TO LK-SIG-BRANCH
           MOVE IR-REMIT-DT          TO LK-SIG-DT
           MOVE IR-CENTER-SEQ        TO LK-SIG-ORIG-SEQ
           MOVE IR-REMIT-AMT         TO LK-SIG-AMT
           CALL "TG931S" USING LK-SIG-PARM
           MOVE LK-SIGNATURE         TO OC-OC-SIGNATURE

           WRITE TGOUTCF-REC
           IF FS-TGOUTCF = "00"
              ADD 1 TO CNT-WRITE
           ELSE
              DISPLAY "TGOUTCF WRITE ERROR ST=" FS-TGOUTCF
              MOVE 8 TO RETURN-CODE
              SET HARD-ERROR TO TRUE
           END-IF.

       2700-DISPLAY-REJECT.
           ADD 1 TO CNT-REJECT
           MOVE IR-REMIT-AMT TO WK-DISPLAY-AMT
           DISPLAY "REJECT REASON=" WK-REJ-REASON
                   " CENTER-SEQ=" IR-CENTER-SEQ
                   " AMT=" WK-DISPLAY-AMT.

       9000-FINAL.
           IF TGINRMF-OPEN
              CLOSE TGINRMF
              IF FS-TGINRMF NOT = "00"
                 DISPLAY "TGINRMF CLOSE ERROR ST=" FS-TGINRMF
                 MOVE 8 TO RETURN-CODE
              END-IF
           END-IF

           IF KZACCTF-OPEN
              CLOSE KZACCTF
              IF FS-KZACCTF NOT = "00"
                 DISPLAY "KZACCTF CLOSE ERROR ST=" FS-KZACCTF
                 MOVE 8 TO RETURN-CODE
              END-IF
           END-IF

           IF TGOUTCF-OPEN
              CLOSE TGOUTCF
              IF FS-TGOUTCF NOT = "00"
                 DISPLAY "TGOUTCF CLOSE ERROR ST=" FS-TGOUTCF
                 MOVE 8 TO RETURN-CODE
              END-IF
           END-IF

           DISPLAY "TG330B END READ=" CNT-READ
                   " SKIP=" CNT-SKIP
                   " REJECT=" CNT-REJECT
                   " WRITE=" CNT-WRITE.
