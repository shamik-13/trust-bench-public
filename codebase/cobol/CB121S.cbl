       IDENTIFICATION DIVISION.
       PROGRAM-ID. CB121S.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CDACTF ASSIGN TO "CDACTF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS AC-ACCOUNT-ID
               FILE STATUS IS WS-CDACTF-ST.
           SELECT CDEXCPF2 ASSIGN TO "CDEXCPF2"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-CDEXCPF2-ST.

       DATA DIVISION.
       FILE SECTION.
       FD  CDACTF.
           COPY CDACTC.

       FD  CDEXCPF2.
           COPY CDEXCPF2C.

       WORKING-STORAGE SECTION.
       01  WS-CDACTF-ST              PIC XX.
       01  WS-CDEXCPF2-ST            PIC XX.
       01  WS-ABEND-FLG              PIC X VALUE "0".
           88  WS-ABEND              VALUE "1".
           88  WS-NORMAL             VALUE "0".
       01  WS-VALID-FLG              PIC X VALUE "1".
           88  WS-VALID              VALUE "1".
           88  WS-INVALID            VALUE "0".
       01  WS-EXCEPTION-FLG          PIC X VALUE "0".
           88  WS-EXCEPTION          VALUE "1".
           88  WS-NO-EXCEPTION       VALUE "0".
       01  WS-OPENED-CDACTF          PIC X VALUE "0".
           88  WS-CDACTF-OPENED      VALUE "1".
       01  WS-OPENED-CDEXCPF2        PIC X VALUE "0".
           88  WS-CDEXCPF2-OPENED    VALUE "1".
       01  WS-DATE-TIME.
           05  WS-DATE-8             PIC 9(8).
           05  WS-TIME-REST          PIC X(13).
       01  WS-EXCEPTION-SEQ          PIC 9(6) VALUE 0.
       01  WS-EXCEPTION-CD           PIC X(4).

       LINKAGE SECTION.
       01  LK-CB121S-PARM.
           05  LK-PAY-ID             PIC X(16).
           05  LK-ACCOUNT-ID         PIC X(18).
           05  LK-CARD-NO            PIC X(16).
           05  LK-CLAIM-AMT          PIC S9(11)V99 COMP-3.
           05  LK-JUDGE-CD           PIC X(2).

       PROCEDURE DIVISION USING LK-CB121S-PARM.
       MAIN-RTN.
           MOVE 0 TO RETURN-CODE
           MOVE "00" TO LK-JUDGE-CD
           SET WS-NORMAL TO TRUE
           PERFORM OPEN-FILE-RTN

           IF NOT WS-ABEND
               PERFORM READ-ACCOUNT-RTN
           END-IF

           IF NOT WS-ABEND
               PERFORM VALIDATE-ACCOUNT-RTN
           END-IF

           IF NOT WS-ABEND
               PERFORM JUDGE-ACCOUNT-RTN
           END-IF

           PERFORM CLOSE-FILE-RTN

           IF WS-ABEND
               MOVE 8 TO RETURN-CODE
           END-IF

           GOBACK.

       OPEN-FILE-RTN.
           OPEN INPUT CDACTF

           IF WS-CDACTF-ST = "00"
               MOVE "1" TO WS-OPENED-CDACTF
           ELSE
               DISPLAY "CDACTF OPEN ST=" WS-CDACTF-ST
               MOVE "1" TO WS-ABEND-FLG
               MOVE 8 TO RETURN-CODE
           END-IF

           IF NOT WS-ABEND
               OPEN EXTEND CDEXCPF2
               IF WS-CDEXCPF2-ST = "00"
                   MOVE "1" TO WS-OPENED-CDEXCPF2
               ELSE
                   DISPLAY "CDEXCPF2 OPEN ST="
                           WS-CDEXCPF2-ST
                   MOVE "1" TO WS-ABEND-FLG
                   MOVE 8 TO RETURN-CODE
               END-IF
           END-IF.

       READ-ACCOUNT-RTN.
           MOVE LK-ACCOUNT-ID TO AC-ACCOUNT-ID

           READ CDACTF KEY IS AC-ACCOUNT-ID
               INVALID KEY
                   IF WS-CDACTF-ST = "23"
                       DISPLAY "CDACTF NOT FOUND ID="
                               LK-ACCOUNT-ID
                       MOVE "21" TO LK-JUDGE-CD
                   ELSE
                       DISPLAY "CDACTF READ ST="
                               WS-CDACTF-ST
                       MOVE "1" TO WS-ABEND-FLG
                       MOVE 8 TO RETURN-CODE
                   END-IF
               NOT INVALID KEY
                   CONTINUE
           END-READ.

       VALIDATE-ACCOUNT-RTN.
           IF LK-JUDGE-CD NOT = "00"
               EXIT PARAGRAPH
           END-IF

           SET WS-VALID TO TRUE

           IF AC-BANK-CD NOT NUMERIC
               SET WS-INVALID TO TRUE
               DISPLAY "AC-BANK-CD INVALID ID="
                       AC-ACCOUNT-ID
           END-IF

           IF AC-BRANCH-CD NOT NUMERIC
               SET WS-INVALID TO TRUE
               DISPLAY "AC-BRANCH-CD INVALID ID="
                       AC-ACCOUNT-ID
           END-IF

           IF AC-DEPOSIT-TYPE NOT NUMERIC
               SET WS-INVALID TO TRUE
               DISPLAY "AC-DEPOSIT-TYPE INVALID ID="
                       AC-ACCOUNT-ID
           END-IF

           IF AC-ACCOUNT-NO NOT NUMERIC
               SET WS-INVALID TO TRUE
               DISPLAY "AC-ACCOUNT-NO INVALID ID="
                       AC-ACCOUNT-ID
           END-IF

           IF AC-BANK-CD = SPACE
               SET WS-INVALID TO TRUE
               DISPLAY "AC-BANK-CD SPACE ID="
                       AC-ACCOUNT-ID
           END-IF

           IF AC-BRANCH-CD = SPACE
               SET WS-INVALID TO TRUE
               DISPLAY "AC-BRANCH-CD SPACE ID="
                       AC-ACCOUNT-ID
           END-IF

           IF AC-DEPOSIT-TYPE = SPACE
               SET WS-INVALID TO TRUE
               DISPLAY "AC-DEPOSIT-TYPE SPACE ID="
                       AC-ACCOUNT-ID
           END-IF

           IF AC-ACCOUNT-NO = SPACE
               SET WS-INVALID TO TRUE
               DISPLAY "AC-ACCOUNT-NO SPACE ID="
                       AC-ACCOUNT-ID
           END-IF

           IF WS-INVALID
               MOVE "20" TO LK-JUDGE-CD
           END-IF.

       JUDGE-ACCOUNT-RTN.
           IF LK-JUDGE-CD NOT = "00"
               EXIT PARAGRAPH
           END-IF

           SET WS-NO-EXCEPTION TO TRUE

           IF AC-HOLDER-KANA = SPACE
               MOVE "K001" TO WS-EXCEPTION-CD
               PERFORM WRITE-EXCEPTION-RTN
               MOVE "1" TO WS-EXCEPTION-FLG
           END-IF

           IF NOT WS-ABEND
               IF AC-TRANSFER-STATUS = "9"
                   MOVE "K002" TO WS-EXCEPTION-CD
                   PERFORM WRITE-EXCEPTION-RTN
                   MOVE "1" TO WS-EXCEPTION-FLG
               END-IF
           END-IF

           IF WS-ABEND
               EXIT PARAGRAPH
           END-IF

           IF WS-EXCEPTION
               MOVE "10" TO LK-JUDGE-CD
           ELSE
               MOVE "00" TO LK-JUDGE-CD
           END-IF.

       WRITE-EXCEPTION-RTN.
           ADD 1 TO WS-EXCEPTION-SEQ

           MOVE SPACE TO CDEXCPF2-REC
           ACCEPT WS-DATE-TIME FROM DATE YYYYMMDD

           STRING AC-ACCOUNT-ID DELIMITED BY SIZE
                  WS-EXCEPTION-SEQ DELIMITED BY SIZE
                  INTO EXP-EXCEPTION-ID
           END-STRING

           MOVE LK-PAY-ID TO EXP-PAY-ID

           IF LK-CARD-NO NOT = SPACE
               MOVE LK-CARD-NO TO EXP-CARD-NO
           ELSE
               MOVE AC-CARD-NO TO EXP-CARD-NO
           END-IF

           MOVE WS-EXCEPTION-CD TO EXP-EXCEPTION-CD
           MOVE LK-CLAIM-AMT TO EXP-EXCEPTION-AMT
           MOVE "CB121S" TO EXP-DETECTED-PROGRAM
           MOVE WS-DATE-8 TO EXP-DETECTED-DT

           WRITE CDEXCPF2-REC

           IF WS-CDEXCPF2-ST NOT = "00"
               DISPLAY "CDEXCPF2 WRITE ST="
                       WS-CDEXCPF2-ST
               MOVE "1" TO WS-ABEND-FLG
               MOVE 8 TO RETURN-CODE
           END-IF.

       CLOSE-FILE-RTN.
           IF WS-CDACTF-OPENED
               CLOSE CDACTF
               IF WS-CDACTF-ST NOT = "00"
                   DISPLAY "CDACTF CLOSE ST="
                           WS-CDACTF-ST
                   MOVE "1" TO WS-ABEND-FLG
                   MOVE 8 TO RETURN-CODE
               END-IF
           END-IF

           IF WS-CDEXCPF2-OPENED
               CLOSE CDEXCPF2
               IF WS-CDEXCPF2-ST NOT = "00"
                   DISPLAY "CDEXCPF2 CLOSE ST="
                           WS-CDEXCPF2-ST
                   MOVE "1" TO WS-ABEND-FLG
                   MOVE 8 TO RETURN-CODE
               END-IF
           END-IF.
