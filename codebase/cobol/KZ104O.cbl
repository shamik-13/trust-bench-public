       IDENTIFICATION DIVISION.
       PROGRAM-ID. KZ104O.
      * 版数  年月日(和暦)  担当                    概要
      * 1.00  平成30.04.01 システム部 勘定系チーム 税率マスタ保守新規作成
      * 1.01  令和01.10.01 システム部 勘定系チーム 軽減税率区分入力対応
      * 1.02  令和06.04.01 システム部 勘定系チーム 税率適用日チェック追加
       AUTHOR. KZ-SYSTEM.
      ******************************************************************
      * TAX RATE MASTER ONLINE MAINTENANCE
      ******************************************************************

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZRTMF
               ASSIGN TO "KZRTMF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS RT-RATE-KEY
               FILE STATUS IS WS-KZRTMF-ST.

           SELECT KZADLF
               ASSIGN TO "KZADLF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-KZADLF-ST.

       DATA DIVISION.
       FILE SECTION.

       FD  KZRTMF.
           COPY KZRTMFC.

       FD  KZADLF.
           COPY KZADLFC.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05 WS-KZRTMF-ST              PIC XX VALUE SPACES.
           05 WS-KZADLF-ST              PIC XX VALUE SPACES.

       01  WS-TRANSACTION.
           05 WS-FUNC-CD                PIC X VALUE SPACE.
              88 FUNC-INQUIRY           VALUE "1".
              88 FUNC-UPDATE            VALUE "2".
              88 FUNC-END               VALUE "9".
           05 WS-IN-TAX-KIND            PIC X(04) VALUE SPACES.
           05 WS-IN-EFF-DATE            PIC 9(08) VALUE ZERO.
           05 WS-IN-AUTH-REF-NO         PIC X(12) VALUE SPACES.
           05 WS-IN-RATE-NUM            PIC 9(09) VALUE ZERO.
           05 WS-IN-RATE-DEN            PIC 9(09) VALUE ZERO.

       01  WS-WORK-KEY.
           05 WS-KEY-TAX-KIND           PIC X(04) VALUE SPACES.
           05 WS-KEY-EFF-DATE           PIC 9(08) VALUE ZERO.
           05 WS-KEY-AUTH-REF-NO        PIC X(12) VALUE SPACES.

       01  WS-SAVE-REC.
           05 WS-OLD-NUMERATOR          PIC 9(09) VALUE ZERO.
           05 WS-OLD-DENOMINATOR        PIC 9(09) VALUE ZERO.
           05 WS-OLD-EFF-DATE           PIC 9(08) VALUE ZERO.
           05 WS-OLD-TAX-KIND           PIC X(04) VALUE SPACES.
           05 WS-OLD-AUTH-REF-NO        PIC X(12) VALUE SPACES.

       01  WS-CONTROL.
           05 WS-EOF-SW                 PIC X VALUE "N".
              88 TAX-EOF                VALUE "Y".
              88 TAX-NOT-EOF            VALUE "N".
           05 WS-OVERLAP-SW             PIC X VALUE "N".
              88 OVERLAP-FOUND          VALUE "Y".
              88 OVERLAP-NOT-FOUND      VALUE "N".
           05 WS-HARD-ERR-SW            PIC X VALUE "N".
              88 HARD-ERROR             VALUE "Y".
              88 NO-HARD-ERROR          VALUE "N".
           05 WS-VALID-SW               PIC X VALUE "N".
              88 DATE-VALID             VALUE "Y".
              88 DATE-INVALID           VALUE "N".

       01  WS-DATE-WORK.
           05 WS-TODAY                  PIC 9(08) VALUE ZERO.
           05 WS-DATE-NUM               PIC 9(08) VALUE ZERO.
           05 WS-YEAR                   PIC 9(04) VALUE ZERO.
           05 WS-MONTH                  PIC 9(02) VALUE ZERO.
           05 WS-DAY                    PIC 9(02) VALUE ZERO.

       01  WS-AUDIT-WORK.
           05 WS-AUDIT-SEQ              PIC 9(10) VALUE ZERO.
           05 WS-DIFF-NUM               PIC S9(12)V9(06) VALUE ZERO.
           05 WS-OLD-RATE               PIC S9(12)V9(06) VALUE ZERO.
           05 WS-NEW-RATE               PIC S9(12)V9(06) VALUE ZERO.

       01  WS-KZ116S-AREA.
           05 KZ116S-TAX-KIND           PIC X(04) VALUE SPACES.
           05 KZ116S-TARGET-DATE        PIC 9(08) VALUE ZERO.
           05 KZ116S-RESULT-CD          PIC X(02) VALUE SPACES.
           05 KZ116S-REASON-CD          PIC X(04) VALUE SPACES.

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE 8 TO RETURN-CODE
           PERFORM 1000-INITIALIZE
           IF NO-HARD-ERROR
               PERFORM 2000-MAIN-PROCESS
           END-IF
           PERFORM 9000-TERMINATE
           GOBACK.

       1000-INITIALIZE.
           DISPLAY "KZ104O START"
           MOVE "N" TO WS-HARD-ERR-SW
           ACCEPT WS-TODAY FROM DATE YYYYMMDD

           OPEN I-O KZRTMF
           IF WS-KZRTMF-ST NOT = "00"
               DISPLAY "KZRTMF OPEN ST=" WS-KZRTMF-ST
               MOVE "Y" TO WS-HARD-ERR-SW
           END-IF

           IF NO-HARD-ERROR
               OPEN EXTEND KZADLF
               IF WS-KZADLF-ST NOT = "00"
                   DISPLAY "KZADLF OPEN ST=" WS-KZADLF-ST
                   MOVE "Y" TO WS-HARD-ERR-SW
               END-IF
           END-IF.

       2000-MAIN-PROCESS.
           PERFORM UNTIL FUNC-END OR HARD-ERROR
               PERFORM 2100-ACCEPT-TRAN
               IF NOT FUNC-END
                   PERFORM 2200-VALIDATE-INPUT
                   IF NO-HARD-ERROR AND DATE-VALID
                       EVALUATE TRUE
                           WHEN FUNC-INQUIRY
                               PERFORM 3000-INQUIRY
                           WHEN FUNC-UPDATE
                               PERFORM 4000-UPDATE
                           WHEN OTHER
                               DISPLAY "BAD FUNCTION"
                       END-EVALUATE
                   END-IF
               END-IF
           END-PERFORM

           IF NO-HARD-ERROR
               MOVE 0 TO RETURN-CODE
           END-IF.

       2100-ACCEPT-TRAN.
           DISPLAY "FUNC 1=INQ 2=UPD 9=END"
           ACCEPT WS-FUNC-CD
           IF NOT FUNC-END
               DISPLAY "TAX KIND"
               ACCEPT WS-IN-TAX-KIND
               DISPLAY "EFFECTIVE DATE"
               ACCEPT WS-IN-EFF-DATE
               DISPLAY "AUTH REF NO"
               ACCEPT WS-IN-AUTH-REF-NO
               IF FUNC-UPDATE
                   DISPLAY "RATE NUMERATOR"
                   ACCEPT WS-IN-RATE-NUM
                   DISPLAY "RATE DENOMINATOR"
                   ACCEPT WS-IN-RATE-DEN
               END-IF
           END-IF.

       2200-VALIDATE-INPUT.
           MOVE "N" TO WS-VALID-SW

           IF WS-IN-TAX-KIND = SPACES
               DISPLAY "TAX KIND REQUIRED"
               EXIT PARAGRAPH
           END-IF

           IF WS-IN-AUTH-REF-NO = SPACES
               DISPLAY "AUTH REF REQUIRED"
               EXIT PARAGRAPH
           END-IF

           MOVE WS-IN-EFF-DATE TO WS-DATE-NUM
           PERFORM 2300-CHECK-DATE
           IF DATE-INVALID
               DISPLAY "BAD EFFECTIVE DATE"
               EXIT PARAGRAPH
           END-IF

           MOVE WS-IN-TAX-KIND TO KZ116S-TAX-KIND
           MOVE WS-IN-EFF-DATE TO KZ116S-TARGET-DATE
           MOVE SPACES TO KZ116S-RESULT-CD
           MOVE SPACES TO KZ116S-REASON-CD
           CALL "KZ116S" USING WS-KZ116S-AREA
               ON EXCEPTION
                   DISPLAY "KZ116S CALL FAILED"
                   MOVE "Y" TO WS-HARD-ERR-SW
                   EXIT PARAGRAPH
           END-CALL

           IF KZ116S-RESULT-CD NOT = "00"
               DISPLAY "KZ116S RESULT=" KZ116S-RESULT-CD
               DISPLAY "KZ116S REASON=" KZ116S-REASON-CD
               EXIT PARAGRAPH
           END-IF

           IF FUNC-UPDATE AND WS-IN-RATE-DEN = ZERO
               DISPLAY "RATE DENOMINATOR ZERO"
               EXIT PARAGRAPH
           END-IF

           MOVE "Y" TO WS-VALID-SW.

       2300-CHECK-DATE.
           MOVE "N" TO WS-VALID-SW
           MOVE WS-DATE-NUM(1:4) TO WS-YEAR
           MOVE WS-DATE-NUM(5:2) TO WS-MONTH
           MOVE WS-DATE-NUM(7:2) TO WS-DAY

           IF WS-YEAR < 1989 OR WS-YEAR > 2099
               EXIT PARAGRAPH
           END-IF

           IF WS-MONTH < 1 OR WS-MONTH > 12
               EXIT PARAGRAPH
           END-IF

           EVALUATE WS-MONTH
               WHEN 1
               WHEN 3
               WHEN 5
               WHEN 7
               WHEN 8
               WHEN 10
               WHEN 12
                   IF WS-DAY >= 1 AND WS-DAY <= 31
                       MOVE "Y" TO WS-VALID-SW
                   END-IF
               WHEN 4
               WHEN 6
               WHEN 9
               WHEN 11
                   IF WS-DAY >= 1 AND WS-DAY <= 30
                       MOVE "Y" TO WS-VALID-SW
                   END-IF
               WHEN 2
                   IF FUNCTION MOD(WS-YEAR, 400) = ZERO
                       IF WS-DAY >= 1 AND WS-DAY <= 29
                           MOVE "Y" TO WS-VALID-SW
                       END-IF
                   ELSE
                       IF FUNCTION MOD(WS-YEAR, 100) = ZERO
                           IF WS-DAY >= 1 AND WS-DAY <= 28
                               MOVE "Y" TO WS-VALID-SW
                           END-IF
                       ELSE
                           IF FUNCTION MOD(WS-YEAR, 4) = ZERO
                               IF WS-DAY >= 1 AND WS-DAY <= 29
                                   MOVE "Y" TO WS-VALID-SW
                               END-IF
                           ELSE
                               IF WS-DAY >= 1 AND WS-DAY <= 28
                                   MOVE "Y" TO WS-VALID-SW
                               END-IF
                           END-IF
                       END-IF
                   END-IF
           END-EVALUATE.

       3000-INQUIRY.
           PERFORM 3100-BUILD-KEY
           READ KZRTMF KEY IS RT-RATE-KEY
               INVALID KEY
                   DISPLAY "RATE MASTER NOT FOUND"
               NOT INVALID KEY
                   DISPLAY "RATE NUM=" RT-RATE-NUMERATOR
                   DISPLAY "RATE DEN=" RT-RATE-DENOMINATOR
           END-READ

           IF WS-KZRTMF-ST NOT = "00" AND WS-KZRTMF-ST NOT = "23"
               DISPLAY "KZRTMF READ ST=" WS-KZRTMF-ST
               MOVE "Y" TO WS-HARD-ERR-SW
           END-IF.

       3100-BUILD-KEY.
           MOVE WS-IN-TAX-KIND TO WS-KEY-TAX-KIND
           MOVE WS-IN-EFF-DATE TO WS-KEY-EFF-DATE
           MOVE WS-IN-AUTH-REF-NO TO WS-KEY-AUTH-REF-NO
           MOVE WS-WORK-KEY TO RT-RATE-KEY.

       4000-UPDATE.
           PERFORM 3100-BUILD-KEY
           READ KZRTMF KEY IS RT-RATE-KEY
               INVALID KEY
                   DISPLAY "RATE MASTER NOT FOUND"
                   EXIT PARAGRAPH
               NOT INVALID KEY
                   PERFORM 4100-SAVE-OLD
           END-READ

           IF WS-KZRTMF-ST NOT = "00"
               IF WS-KZRTMF-ST NOT = "23"
                   DISPLAY "KZRTMF READ-BEF ST=" WS-KZRTMF-ST
                   MOVE "Y" TO WS-HARD-ERR-SW
               END-IF
               EXIT PARAGRAPH
           END-IF

           PERFORM 4200-CHECK-OVERLAP
           IF HARD-ERROR OR OVERLAP-FOUND
               EXIT PARAGRAPH
           END-IF

           PERFORM 4300-WRITE-AUDIT-BEFORE
           IF HARD-ERROR
               EXIT PARAGRAPH
           END-IF

           MOVE WS-IN-RATE-NUM TO RT-RATE-NUMERATOR
           MOVE WS-IN-RATE-DEN TO RT-RATE-DENOMINATOR
           REWRITE KZRTMF-REC
           IF WS-KZRTMF-ST NOT = "00"
               DISPLAY "KZRTMF REWRITE ST=" WS-KZRTMF-ST
               MOVE "Y" TO WS-HARD-ERR-SW
               EXIT PARAGRAPH
           END-IF

           PERFORM 4400-WRITE-AUDIT-AFTER
           IF NO-HARD-ERROR
               DISPLAY "RATE MASTER UPDATED"
           END-IF.

       4100-SAVE-OLD.
           MOVE RT-RATE-NUMERATOR TO WS-OLD-NUMERATOR
           MOVE RT-RATE-DENOMINATOR TO WS-OLD-DENOMINATOR
           MOVE RT-EFFECTIVE-DATE TO WS-OLD-EFF-DATE
           MOVE RT-TAX-KIND TO WS-OLD-TAX-KIND
           MOVE RT-AUTH-REF-NO TO WS-OLD-AUTH-REF-NO.

       4200-CHECK-OVERLAP.
           MOVE "N" TO WS-OVERLAP-SW
           MOVE "N" TO WS-EOF-SW
           MOVE LOW-VALUES TO RT-RATE-KEY
           START KZRTMF KEY IS NOT LESS THAN RT-RATE-KEY
               INVALID KEY
                   MOVE "Y" TO WS-EOF-SW
           END-START

           IF WS-KZRTMF-ST NOT = "00" AND WS-KZRTMF-ST NOT = "23"
               DISPLAY "KZRTMF START ST=" WS-KZRTMF-ST
               MOVE "Y" TO WS-HARD-ERR-SW
               EXIT PARAGRAPH
           END-IF

           PERFORM UNTIL TAX-EOF OR OVERLAP-FOUND OR HARD-ERROR
               READ KZRTMF NEXT RECORD
                   AT END
                       MOVE "Y" TO WS-EOF-SW
                   NOT AT END
                       PERFORM 4210-JUDGE-OVERLAP
               END-READ
               IF WS-KZRTMF-ST NOT = "00" AND NOT TAX-EOF
                   DISPLAY "KZRTMF NEXT ST=" WS-KZRTMF-ST
                   MOVE "Y" TO WS-HARD-ERR-SW
               END-IF
           END-PERFORM

           IF OVERLAP-FOUND
               DISPLAY "EFFECTIVE DATE OVERLAP"
           END-IF

           PERFORM 3100-BUILD-KEY
           READ KZRTMF KEY IS RT-RATE-KEY
               INVALID KEY
                   DISPLAY "KZRTMF REREAD FAILED"
                   MOVE "Y" TO WS-HARD-ERR-SW
               NOT INVALID KEY
                   CONTINUE
           END-READ.

       4210-JUDGE-OVERLAP.
           IF RT-TAX-KIND = WS-IN-TAX-KIND
               IF RT-AUTH-REF-NO NOT = WS-IN-AUTH-REF-NO
                   IF RT-EFFECTIVE-DATE = WS-IN-EFF-DATE
                       MOVE "Y" TO WS-OVERLAP-SW
                   END-IF
               END-IF
           END-IF.

       4300-WRITE-AUDIT-BEFORE.
           IF WS-OLD-DENOMINATOR = ZERO
               MOVE ZERO TO WS-OLD-RATE
           ELSE
               COMPUTE WS-OLD-RATE ROUNDED =
                   WS-OLD-NUMERATOR / WS-OLD-DENOMINATOR
           END-IF

           ADD 1 TO WS-AUDIT-SEQ
           MOVE WS-AUDIT-SEQ TO AL-AUDIT-ID
           MOVE WS-TODAY TO AL-RUN-DATE
           MOVE "KZ104O-BEFORE" TO AL-SOURCE-DSID
           MOVE WS-OLD-RATE TO AL-DEBIT-AMT
           MOVE ZERO TO AL-CREDIT-AMT
           MOVE WS-OLD-RATE TO AL-DIFF-AMT
           MOVE "01" TO AL-STATUS-CD
           WRITE KZADLF-REC
           IF WS-KZADLF-ST NOT = "00"
               DISPLAY "KZADLF WRITE-BEF ST=" WS-KZADLF-ST
               MOVE "Y" TO WS-HARD-ERR-SW
           END-IF.

       4400-WRITE-AUDIT-AFTER.
           COMPUTE WS-NEW-RATE ROUNDED =
               WS-IN-RATE-NUM / WS-IN-RATE-DEN
           COMPUTE WS-DIFF-NUM =
               WS-NEW-RATE - WS-OLD-RATE

           ADD 1 TO WS-AUDIT-SEQ
           MOVE WS-AUDIT-SEQ TO AL-AUDIT-ID
           MOVE WS-TODAY TO AL-RUN-DATE
           MOVE "KZ104O-AFTER" TO AL-SOURCE-DSID
           MOVE WS-OLD-RATE TO AL-DEBIT-AMT
           MOVE WS-NEW-RATE TO AL-CREDIT-AMT
           MOVE WS-DIFF-NUM TO AL-DIFF-AMT
           MOVE "02" TO AL-STATUS-CD
           WRITE KZADLF-REC
           IF WS-KZADLF-ST NOT = "00"
               DISPLAY "KZADLF WRITE-AFT ST=" WS-KZADLF-ST
               MOVE "Y" TO WS-HARD-ERR-SW
           END-IF.

       9000-TERMINATE.
           IF WS-KZRTMF-ST NOT = SPACES
               CLOSE KZRTMF
               IF WS-KZRTMF-ST NOT = "00"
                   DISPLAY "KZRTMF CLOSE ST=" WS-KZRTMF-ST
                   MOVE 8 TO RETURN-CODE
               END-IF
           END-IF

           IF WS-KZADLF-ST NOT = SPACES
               CLOSE KZADLF
               IF WS-KZADLF-ST NOT = "00"
                   DISPLAY "KZADLF CLOSE ST=" WS-KZADLF-ST
                   MOVE 8 TO RETURN-CODE
               END-IF
           END-IF

           IF HARD-ERROR
               MOVE 8 TO RETURN-CODE
               DISPLAY "KZ104O ABEND"
           ELSE
               DISPLAY "KZ104O END"
           END-IF.
