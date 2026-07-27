       IDENTIFICATION DIVISION.
       PROGRAM-ID. CB140B.
       AUTHOR. TRUST-BATCH.
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.
      *
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CDRTRYF ASSIGN TO "CDRTRYF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS RTY-RETRY-ID
               FILE STATUS IS FS-CDRTRYF.
      *
           SELECT CDACTF ASSIGN TO "CDACTF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS AC-ACCOUNT-ID
               FILE STATUS IS FS-CDACTF.
      *
           SELECT CDOSF ASSIGN TO "CDOSF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS OS-CARD-NO
               FILE STATUS IS FS-CDOSF.
      *
           SELECT CDTRQF ASSIGN TO "CDTRQF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-CDTRQF.
      *
           SELECT CDHISTF ASSIGN TO "CDHISTF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS HIS-CARD-NO
               FILE STATUS IS FS-CDHISTF.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  CDRTRYF.
           COPY CDRTRYC.
      *
       FD  CDACTF.
           COPY CDACTC.
      *
       FD  CDOSF.
           COPY CDOSFC.
      *
       FD  CDTRQF.
           COPY CDTRQC.
      *
       FD  CDHISTF.
           COPY CDHISTC.
      *
       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05 FS-CDRTRYF              PIC XX VALUE SPACE.
           05 FS-CDACTF               PIC XX VALUE SPACE.
           05 FS-CDOSF                PIC XX VALUE SPACE.
           05 FS-CDTRQF               PIC XX VALUE SPACE.
           05 FS-CDHISTF              PIC XX VALUE SPACE.
      *
       01  WS-SWITCHES.
           05 WS-EOF-RTY              PIC X VALUE 'N'.
              88 END-RTY                    VALUE 'Y'.
           05 WS-EOF-ACT              PIC X VALUE 'N'.
              88 END-ACT                    VALUE 'Y'.
           05 WS-ACCOUNT-FOUND        PIC X VALUE 'N'.
              88 ACCOUNT-FOUND              VALUE 'Y'.
           05 WS-HARD-ERROR           PIC X VALUE 'N'.
              88 HARD-ERROR                 VALUE 'Y'.
      *
       01  WS-CURRENT-DATE-AREA.
           05 WS-CURRENT-DATE-TIME    PIC X(21).
           05 WS-SYSTEM-DATE          PIC 9(8).
      *
       01  WS-CONSTANTS.
           05 WS-MAX-RETRY-COUNT      PIC 9(2) VALUE 3.
           05 WS-PGM-ID               PIC X(8) VALUE 'CB140B  '.
           05 WS-ACTIVE-STATUS        PIC X VALUE '0'.
           05 WS-RETRY-WAIT-STATUS    PIC X VALUE '1'.
           05 WS-REQUEST-NEW-STATUS   PIC X VALUE '0'.
           05 WS-HIST-CUTOFF          PIC X(2) VALUE 'RC'.
      *
       01  WS-COUNTERS.
           05 WS-RTY-READ-CNT         PIC 9(9) VALUE 0.
           05 WS-ACT-READ-CNT         PIC 9(9) VALUE 0.
           05 WS-TRQ-WRITE-CNT        PIC 9(9) VALUE 0.
           05 WS-HIS-WRITE-CNT        PIC 9(9) VALUE 0.
           05 WS-SKIP-CNT             PIC 9(9) VALUE 0.
      *
       01  WS-CALC.
           05 WS-TABLE-IDX            PIC 9(5) COMP VALUE 0.
           05 WS-ACT-MAX              PIC 9(5) COMP VALUE 20000.
           05 WS-ACT-COUNT            PIC 9(5) COMP VALUE 0.
           05 WS-BALANCE-AMT          PIC S9(13) COMP-3 VALUE 0.
           05 WS-REQUEST-AMT          PIC S9(13) COMP-3 VALUE 0.
           05 WS-EVENT-SEQ            PIC 9(6) VALUE 0.
      *
       01  WS-ACCOUNT-TABLE.
           05 WS-ACT-ENTRY OCCURS 20000 TIMES.
              10 TBL-CARD-NO          PIC X(19).
              10 TBL-BANK-CD          PIC X(4).
              10 TBL-BRANCH-CD        PIC X(3).
              10 TBL-DEPOSIT-TYPE     PIC X(1).
              10 TBL-ACCOUNT-NO       PIC X(7).
              10 TBL-HOLDER-KANA      PIC X(40).
              10 TBL-TRANSFER-STATUS  PIC X(1).
      *
       PROCEDURE DIVISION.
       MAIN-RTN.
           MOVE 0 TO RETURN-CODE
           PERFORM 1000-INITIALIZE
           IF NOT HARD-ERROR
               PERFORM 2000-MAIN-PROCESS
           END-IF
           PERFORM 9000-FINALIZE
           GOBACK.
      *
       1000-INITIALIZE.
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-TIME
           MOVE WS-CURRENT-DATE-TIME(1:8) TO WS-SYSTEM-DATE
      *
           OPEN INPUT  CDRTRYF
                       CDACTF
                       CDOSF
                OUTPUT CDTRQF
                       CDHISTF
      *
           IF FS-CDRTRYF NOT = '00'
               DISPLAY 'CDRTRYF オープン失敗 ST=' FS-CDRTRYF
               PERFORM 9100-SET-ABEND
           END-IF
           IF FS-CDACTF NOT = '00'
               DISPLAY 'CDACTF オープン失敗 ST=' FS-CDACTF
               PERFORM 9100-SET-ABEND
           END-IF
           IF FS-CDOSF NOT = '00'
               DISPLAY 'CDOSF オープン失敗 ST=' FS-CDOSF
               PERFORM 9100-SET-ABEND
           END-IF
           IF FS-CDTRQF NOT = '00'
               DISPLAY 'CDTRQF オープン失敗 ST=' FS-CDTRQF
               PERFORM 9100-SET-ABEND
           END-IF
           IF FS-CDHISTF NOT = '00'
               DISPLAY 'CDHISTF オープン失敗 ST=' FS-CDHISTF
               PERFORM 9100-SET-ABEND
           END-IF
      *
           IF NOT HARD-ERROR
               PERFORM 1100-LOAD-ACCOUNT
           END-IF.
      *
       1100-LOAD-ACCOUNT.
           PERFORM UNTIL END-ACT OR HARD-ERROR
               READ CDACTF NEXT RECORD
                   AT END
                       SET END-ACT TO TRUE
                   NOT AT END
                       ADD 1 TO WS-ACT-READ-CNT
                       IF WS-ACT-COUNT >= WS-ACT-MAX
                           DISPLAY '口座表件数超過'
                           PERFORM 9100-SET-ABEND
                       ELSE
                           ADD 1 TO WS-ACT-COUNT
                           MOVE AC-CARD-NO
                             TO TBL-CARD-NO(WS-ACT-COUNT)
                           MOVE AC-BANK-CD
                             TO TBL-BANK-CD(WS-ACT-COUNT)
                           MOVE AC-BRANCH-CD
                             TO TBL-BRANCH-CD(WS-ACT-COUNT)
                           MOVE AC-DEPOSIT-TYPE
                             TO TBL-DEPOSIT-TYPE(WS-ACT-COUNT)
                           MOVE AC-ACCOUNT-NO
                             TO TBL-ACCOUNT-NO(WS-ACT-COUNT)
                           MOVE AC-HOLDER-KANA
                             TO TBL-HOLDER-KANA(WS-ACT-COUNT)
                           MOVE AC-TRANSFER-STATUS
                             TO TBL-TRANSFER-STATUS(WS-ACT-COUNT)
                       END-IF
               END-READ
               IF FS-CDACTF NOT = '00' AND FS-CDACTF NOT = '10'
                   DISPLAY 'CDACTF 読込失敗 ST=' FS-CDACTF
                   PERFORM 9100-SET-ABEND
               END-IF
           END-PERFORM.
      *
       2000-MAIN-PROCESS.
           PERFORM UNTIL END-RTY OR HARD-ERROR
               READ CDRTRYF NEXT RECORD
                   AT END
                       SET END-RTY TO TRUE
                   NOT AT END
                       ADD 1 TO WS-RTY-READ-CNT
                       PERFORM 2100-PROCESS-RETRY
               END-READ
               IF FS-CDRTRYF NOT = '00' AND FS-CDRTRYF NOT = '10'
                   DISPLAY 'CDRTRYF 読込失敗 ST=' FS-CDRTRYF
                   PERFORM 9100-SET-ABEND
               END-IF
           END-PERFORM.
      *
       2100-PROCESS-RETRY.
           IF RTY-RETRY-STATUS NOT = WS-RETRY-WAIT-STATUS
               ADD 1 TO WS-SKIP-CNT
           ELSE
               IF RTY-NEXT-REQUEST-DT > WS-SYSTEM-DATE
                   ADD 1 TO WS-SKIP-CNT
               ELSE
                   PERFORM 2200-FIND-ACCOUNT
                   IF NOT ACCOUNT-FOUND
                       DISPLAY '口座未登録 カード=' RTY-CARD-NO
                       ADD 1 TO WS-SKIP-CNT
                   ELSE
                       IF TBL-TRANSFER-STATUS(WS-TABLE-IDX)
                          NOT = WS-ACTIVE-STATUS
                           ADD 1 TO WS-SKIP-CNT
                       ELSE
                           IF RTY-RETRY-COUNT >= WS-MAX-RETRY-COUNT
                               PERFORM 2600-WRITE-CUTOFF-HIST
                           ELSE
                               PERFORM 2300-READ-OUTSTANDING
                               IF NOT HARD-ERROR
                                   PERFORM 2400-WRITE-REQUEST
                               END-IF
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF.
      *
       2200-FIND-ACCOUNT.
           MOVE 'N' TO WS-ACCOUNT-FOUND
           MOVE 1 TO WS-TABLE-IDX
           PERFORM UNTIL ACCOUNT-FOUND
              OR WS-TABLE-IDX > WS-ACT-COUNT
               IF TBL-CARD-NO(WS-TABLE-IDX) = RTY-CARD-NO
                   SET ACCOUNT-FOUND TO TRUE
               ELSE
                   ADD 1 TO WS-TABLE-IDX
               END-IF
           END-PERFORM.
      *
       2300-READ-OUTSTANDING.
           MOVE RTY-CARD-NO TO OS-CARD-NO
           READ CDOSF RECORD
               INVALID KEY
                   DISPLAY '残高未登録 カード=' RTY-CARD-NO
                   ADD 1 TO WS-SKIP-CNT
                   MOVE 0 TO WS-REQUEST-AMT
               NOT INVALID KEY
                   COMPUTE WS-BALANCE-AMT =
                       OS-FEE-BAL-AMT
                     + OS-INTEREST-BAL-AMT
                     + OS-PRINCIPAL-BAL-AMT
                   IF WS-BALANCE-AMT <= 0
                       ADD 1 TO WS-SKIP-CNT
                       MOVE 0 TO WS-REQUEST-AMT
                   ELSE
                       IF RTY-RETRY-AMT > WS-BALANCE-AMT
                           MOVE WS-BALANCE-AMT TO WS-REQUEST-AMT
                       ELSE
                           MOVE RTY-RETRY-AMT TO WS-REQUEST-AMT
                       END-IF
                   END-IF
           END-READ
           IF FS-CDOSF NOT = '00' AND FS-CDOSF NOT = '23'
               DISPLAY 'CDOSF 読込失敗 ST=' FS-CDOSF
               PERFORM 9100-SET-ABEND
           END-IF.
      *
       2400-WRITE-REQUEST.
           IF WS-REQUEST-AMT > 0
               INITIALIZE CDTRQF-REC
               STRING WS-PGM-ID RTY-RETRY-ID
                   DELIMITED BY SIZE INTO TRQ-REQUEST-ID
               MOVE RTY-CARD-NO
                 TO TRQ-CARD-NO
               MOVE OS-CYCLE-DT
                 TO TRQ-BILLING-CYCLE-DT
               MOVE WS-REQUEST-AMT
                 TO TRQ-REQUEST-AMT
               MOVE RTY-NEXT-REQUEST-DT
                 TO TRQ-DUE-DT
               MOVE TBL-BANK-CD(WS-TABLE-IDX)
                 TO TRQ-BANK-CD
               MOVE TBL-ACCOUNT-NO(WS-TABLE-IDX)
                 TO TRQ-ACCOUNT-NO
               MOVE WS-REQUEST-NEW-STATUS
                 TO TRQ-REQUEST-STATUS
               WRITE CDTRQF-REC
               IF FS-CDTRQF = '00'
                   ADD 1 TO WS-TRQ-WRITE-CNT
               ELSE
                   DISPLAY 'CDTRQF 書込失敗 ST=' FS-CDTRQF
                   PERFORM 9100-SET-ABEND
               END-IF
           END-IF.
      *
       2600-WRITE-CUTOFF-HIST.
           INITIALIZE CDHISTF-REC
           ADD 1 TO WS-EVENT-SEQ
           MOVE RTY-CARD-NO              TO HIS-CARD-NO
           MOVE RTY-ORIGINAL-REQUEST-ID  TO HIS-PAY-ID
           MOVE WS-EVENT-SEQ             TO HIS-EVENT-SEQ
           MOVE WS-HIST-CUTOFF           TO HIS-EVENT-TYPE
           MOVE RTY-RETRY-AMT            TO HIS-EVENT-AMT
           MOVE WS-SYSTEM-DATE           TO HIS-EVENT-DT
           MOVE WS-PGM-ID                TO HIS-SOURCE-PROGRAM
           WRITE CDHISTF-REC
           IF FS-CDHISTF = '00'
               ADD 1 TO WS-HIS-WRITE-CNT
           ELSE
               DISPLAY 'CDHISTF 書込失敗 ST=' FS-CDHISTF
               PERFORM 9100-SET-ABEND
           END-IF.
      *
       9000-FINALIZE.
           CLOSE CDRTRYF CDACTF CDOSF CDTRQF CDHISTF
           DISPLAY 'CB140B 処理件数 不能=' WS-RTY-READ-CNT
           DISPLAY 'CB140B 口座読込=' WS-ACT-READ-CNT
           DISPLAY 'CB140B 再請求作成=' WS-TRQ-WRITE-CNT
           DISPLAY 'CB140B 履歴作成=' WS-HIS-WRITE-CNT
           DISPLAY 'CB140B 対象外=' WS-SKIP-CNT
           IF NOT HARD-ERROR
               MOVE 0 TO RETURN-CODE
           END-IF.
      *
       9100-SET-ABEND.
           MOVE 'Y' TO WS-HARD-ERROR
           MOVE 8 TO RETURN-CODE.
