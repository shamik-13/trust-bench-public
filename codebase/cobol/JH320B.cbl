       IDENTIFICATION DIVISION.
       PROGRAM-ID. JH320B.
       AUTHOR. JH-BATCH.
      * 顧客属性同期バッチ
      * 外部取込済み属性を合成し、顧客属性ファイルへ同期する。
      * 参照マスタ確認とJH322S結果により更新可否を決定する。
      * 開始時点の処理方針は、先に顧客属性を読み、差分発生時だけ
      * サブルーチンを呼ぶ単純同期である。全件再検証や強制補正は
      * 行わず、既存更新日より新しい取込分のみ反映する。

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT JHREFMST-FILE ASSIGN TO "JHREFMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS RF-REF-TYPE-CD
               FILE STATUS IS WS-JHREFMST-ST.
           SELECT JHCATTF-FILE ASSIGN TO "JHCATTF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CA-CUST-ID
               FILE STATUS IS WS-JHCATTF-ST.

       DATA DIVISION.
       FILE SECTION.

       FD  JHREFMST-FILE.
           COPY JHREFMSTC.

       FD  JHCATTF-FILE.
           COPY JHCATTC.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05 WS-JHREFMST-ST        PIC XX VALUE SPACE.
           05 WS-JHCATTF-ST         PIC XX VALUE SPACE.

       01  WS-CONTROL.
           05 WS-EOF-SW             PIC X VALUE 'N'.
              88 END-OF-REF               VALUE 'Y'.
           05 WS-HARD-ERR-SW        PIC X VALUE 'N'.
              88 HARD-ERROR              VALUE 'Y'.
           05 WS-REF-FOUND-SW       PIC X VALUE 'N'.
              88 REF-FOUND               VALUE 'Y'.
           05 WS-CUST-FOUND-SW      PIC X VALUE 'N'.
              88 CUST-FOUND              VALUE 'Y'.
           05 WS-UPDATE-SW          PIC X VALUE 'N'.
              88 UPDATED                 VALUE 'Y'.
           05 WS-REQ-IDX            PIC 9(5) COMP VALUE 0.
           05 WS-REQ-IDX-DISP       PIC 9(5) VALUE ZERO.
           05 WS-MAX-REQ            PIC 9(5) COMP VALUE 200.
           05 WS-PROC-CNT           PIC 9(7) COMP-3 VALUE 0.
           05 WS-UPD-CNT            PIC 9(7) COMP-3 VALUE 0.
           05 WS-ERR-CNT            PIC 9(7) COMP-3 VALUE 0.
           05 WS-SKIP-CNT           PIC 9(7) COMP-3 VALUE 0.

       01  WS-DATE-WORK.
           05 WS-BASE-DATE          PIC 9(8) VALUE 20260618.
           05 WS-DATE-REM           PIC 9(4) COMP VALUE 0.

       01  WS-REQUEST-REC.
           05 RQ-CUST-ID            PIC X(10).
           05 RQ-PREF-CD            PIC X(02).
           05 RQ-OCCUP-CD           PIC X(04).
           05 RQ-HOUSEHOLD-ID       PIC X(12).
           05 RQ-DM-PERMIT-FLAG     PIC X(01).
           05 RQ-ATTR-UPDATE-DATE   PIC 9(8).

       01  WS-OLD-CATTF.
           05 OLD-PREF-CD           PIC X(02).
           05 OLD-OCCUP-CD          PIC X(04).
           05 OLD-HOUSEHOLD-ID      PIC X(12).
           05 OLD-DM-PERMIT-FLAG    PIC X(01).
           05 OLD-LAST-UPDATE-DATE  PIC 9(8).

       01  WS-REF-KEY.
           05 WS-SEARCH-TYPE        PIC X(02).
           05 WS-SEARCH-CODE        PIC X(12).

       01  WS-ERR-AREA.
           05 WS-ERR-CLASS          PIC X(04).
           05 WS-ERR-REASON         PIC X(30).
           05 WS-ERR-ATTR           PIC X(02).
           05 WS-ERR-CODE           PIC X(12).
           05 WS-ERR-CUST           PIC X(10).

       01  LK-JH322S-PARM.
           05 LK-ATTR-TYPE-CD       PIC X(02).
           05 LK-ATTR-CD            PIC X(12).
           05 LK-BASE-DATE          PIC 9(08).
           05 LK-RESULT-CD          PIC X(01).
              88 LK-VALID                 VALUE '0'.
              88 LK-EXPIRED               VALUE '1'.
              88 LK-NOT-REGISTERED        VALUE '2'.
              88 LK-VALIDATION-ERROR      VALUE '9'.
           05 LK-SEVERITY           PIC X(01).
           05 LK-REASON-CD          PIC X(04).

       PROCEDURE DIVISION.
       MAIN-SECTION.
           PERFORM 0100-INIT
           IF NOT HARD-ERROR
              PERFORM 1000-MAIN-PROCESS
                 UNTIL WS-REQ-IDX >= WS-MAX-REQ
                    OR HARD-ERROR
           END-IF
           PERFORM 9000-FINAL
           GOBACK
           .

       0100-INIT.
           MOVE 0 TO RETURN-CODE
           OPEN INPUT JHREFMST-FILE
           IF WS-JHREFMST-ST NOT = '00'
              DISPLAY 'JHREFMST OPEN ERROR ST=' WS-JHREFMST-ST
              MOVE 12 TO RETURN-CODE
              SET HARD-ERROR TO TRUE
           END-IF

           IF NOT HARD-ERROR
              OPEN I-O JHCATTF-FILE
              IF WS-JHCATTF-ST NOT = '00'
                 DISPLAY 'JHCATTF OPEN ERROR ST=' WS-JHCATTF-ST
                 MOVE 12 TO RETURN-CODE
                 SET HARD-ERROR TO TRUE
              END-IF
           END-IF
           .

       1000-MAIN-PROCESS.
           ADD 1 TO WS-REQ-IDX
           PERFORM 1100-BUILD-REQUEST
           ADD 1 TO WS-PROC-CNT
           PERFORM 1200-READ-CUSTOMER
           IF CUST-FOUND
              IF RQ-ATTR-UPDATE-DATE > CA-LAST-UPDATE-DATE
                 PERFORM 2000-APPLY-ATTRIBUTES
                 IF UPDATED
                    MOVE RQ-ATTR-UPDATE-DATE TO CA-LAST-UPDATE-DATE
                    REWRITE JHCATTF-REC
                    IF WS-JHCATTF-ST = '00'
                       ADD 1 TO WS-UPD-CNT
                    ELSE
                       DISPLAY 'JHCATTF REWRITE ERROR CUST='
                               CA-CUST-ID
                               ' ST=' WS-JHCATTF-ST
                       MOVE 12 TO RETURN-CODE
                       SET HARD-ERROR TO TRUE
                    END-IF
                 ELSE
                    ADD 1 TO WS-SKIP-CNT
                 END-IF
              ELSE
                 ADD 1 TO WS-SKIP-CNT
              END-IF
           ELSE
              PERFORM 1300-CREATE-CUSTOMER
           END-IF
           .

       1100-BUILD-REQUEST.
           MOVE SPACES TO WS-REQUEST-REC
           MOVE WS-REQ-IDX TO WS-REQ-IDX-DISP

           STRING 'C' WS-REQ-IDX-DISP DELIMITED BY SIZE
              INTO RQ-CUST-ID
           END-STRING

           COMPUTE WS-DATE-REM = FUNCTION MOD(WS-REQ-IDX 47)
           COMPUTE RQ-ATTR-UPDATE-DATE = 20260501 + WS-DATE-REM

           EVALUATE FUNCTION MOD(WS-REQ-IDX 6)
              WHEN 0 MOVE '13' TO RQ-PREF-CD
              WHEN 1 MOVE '27' TO RQ-PREF-CD
              WHEN 2 MOVE '40' TO RQ-PREF-CD
              WHEN 3 MOVE '01' TO RQ-PREF-CD
              WHEN 4 MOVE '99' TO RQ-PREF-CD
              WHEN OTHER MOVE '23' TO RQ-PREF-CD
           END-EVALUATE

           EVALUATE FUNCTION MOD(WS-REQ-IDX 5)
              WHEN 0 MOVE '0001' TO RQ-OCCUP-CD
              WHEN 1 MOVE '0002' TO RQ-OCCUP-CD
              WHEN 2 MOVE '0003' TO RQ-OCCUP-CD
              WHEN 3 MOVE '9999' TO RQ-OCCUP-CD
              WHEN OTHER MOVE '0010' TO RQ-OCCUP-CD
           END-EVALUATE

           STRING 'HH' WS-REQ-IDX-DISP DELIMITED BY SIZE
              INTO RQ-HOUSEHOLD-ID
           END-STRING

           IF FUNCTION MOD(WS-REQ-IDX 4) = 0
              MOVE 'N' TO RQ-DM-PERMIT-FLAG
           ELSE
              MOVE 'Y' TO RQ-DM-PERMIT-FLAG
           END-IF
           .

       1200-READ-CUSTOMER.
           MOVE RQ-CUST-ID TO CA-CUST-ID
           READ JHCATTF-FILE
              INVALID KEY
                 MOVE 'N' TO WS-CUST-FOUND-SW
              NOT INVALID KEY
                 MOVE 'Y' TO WS-CUST-FOUND-SW
                 MOVE CA-PREF-CD TO OLD-PREF-CD
                 MOVE CA-OCCUP-CD TO OLD-OCCUP-CD
                 MOVE CA-HOUSEHOLD-ID TO OLD-HOUSEHOLD-ID
                 MOVE CA-DM-PERMIT-FLAG TO OLD-DM-PERMIT-FLAG
                 MOVE CA-LAST-UPDATE-DATE TO OLD-LAST-UPDATE-DATE
           END-READ

           IF WS-JHCATTF-ST NOT = '00'
              AND WS-JHCATTF-ST NOT = '23'
              DISPLAY 'JHCATTF READ ERROR CUST=' RQ-CUST-ID
                      ' ST=' WS-JHCATTF-ST
              MOVE 12 TO RETURN-CODE
              SET HARD-ERROR TO TRUE
           END-IF
           .

       1300-CREATE-CUSTOMER.
           INITIALIZE JHCATTF-REC
           MOVE RQ-CUST-ID TO CA-CUST-ID
           MOVE RQ-ATTR-UPDATE-DATE TO CA-LAST-UPDATE-DATE
           MOVE '20' TO CA-AGE-BAND-CD
           MOVE '00' TO CA-TRUST-REL-CD
           MOVE 'N' TO WS-UPDATE-SW

           PERFORM 2000-APPLY-ATTRIBUTES

           IF UPDATED
              WRITE JHCATTF-REC
              IF WS-JHCATTF-ST = '00'
                 ADD 1 TO WS-UPD-CNT
              ELSE
                 DISPLAY 'JHCATTF WRITE ERROR CUST=' CA-CUST-ID
                         ' ST=' WS-JHCATTF-ST
                 MOVE 12 TO RETURN-CODE
                 SET HARD-ERROR TO TRUE
              END-IF
           ELSE
              ADD 1 TO WS-SKIP-CNT
           END-IF
           .

       2000-APPLY-ATTRIBUTES.
           MOVE 'N' TO WS-UPDATE-SW

           IF RQ-PREF-CD NOT = CA-PREF-CD
              MOVE 'PR' TO LK-ATTR-TYPE-CD
              MOVE RQ-PREF-CD TO LK-ATTR-CD
              PERFORM 3000-VALIDATE-ATTRIBUTE
              IF LK-VALID
                 MOVE RQ-PREF-CD TO CA-PREF-CD
                 MOVE 'Y' TO WS-UPDATE-SW
              ELSE
                 PERFORM 5000-WRITE-DQERR
              END-IF
           END-IF

           IF RQ-OCCUP-CD NOT = CA-OCCUP-CD
              MOVE 'OC' TO LK-ATTR-TYPE-CD
              MOVE RQ-OCCUP-CD TO LK-ATTR-CD
              PERFORM 3000-VALIDATE-ATTRIBUTE
              IF LK-VALID
                 MOVE RQ-OCCUP-CD TO CA-OCCUP-CD
                 MOVE 'Y' TO WS-UPDATE-SW
              ELSE
                 PERFORM 5000-WRITE-DQERR
              END-IF
           END-IF

           IF RQ-HOUSEHOLD-ID NOT = CA-HOUSEHOLD-ID
              MOVE 'HH' TO LK-ATTR-TYPE-CD
              MOVE RQ-HOUSEHOLD-ID TO LK-ATTR-CD
              PERFORM 3000-VALIDATE-ATTRIBUTE
              IF LK-VALID
                 MOVE RQ-HOUSEHOLD-ID TO CA-HOUSEHOLD-ID
                 MOVE 'Y' TO WS-UPDATE-SW
              ELSE
                 PERFORM 5000-WRITE-DQERR
              END-IF
           END-IF

           IF RQ-DM-PERMIT-FLAG NOT = CA-DM-PERMIT-FLAG
              MOVE 'DM' TO LK-ATTR-TYPE-CD
              MOVE RQ-DM-PERMIT-FLAG TO LK-ATTR-CD
              PERFORM 3000-VALIDATE-ATTRIBUTE
              IF LK-VALID
                 MOVE RQ-DM-PERMIT-FLAG TO CA-DM-PERMIT-FLAG
                 MOVE 'Y' TO WS-UPDATE-SW
              ELSE
                 PERFORM 5000-WRITE-DQERR
              END-IF
           END-IF
           .

       3000-VALIDATE-ATTRIBUTE.
           MOVE WS-BASE-DATE TO LK-BASE-DATE
           MOVE SPACE TO LK-RESULT-CD
           MOVE SPACE TO LK-SEVERITY
           MOVE SPACES TO LK-REASON-CD

           CALL 'JH322S' USING LK-JH322S-PARM

           IF LK-VALID
              MOVE LK-ATTR-TYPE-CD TO WS-SEARCH-TYPE
              MOVE LK-ATTR-CD TO WS-SEARCH-CODE
              PERFORM 4000-CHECK-REFMASTER
              IF NOT REF-FOUND
                 MOVE '2' TO LK-RESULT-CD
                 MOVE 'W' TO LK-SEVERITY
                 MOVE 'R404' TO LK-REASON-CD
              END-IF
           END-IF
           .

       4000-CHECK-REFMASTER.
           MOVE 'N' TO WS-REF-FOUND-SW
           MOVE 'N' TO WS-EOF-SW
           MOVE WS-SEARCH-TYPE TO RF-REF-TYPE-CD

           START JHREFMST-FILE KEY IS EQUAL TO RF-REF-TYPE-CD
              INVALID KEY
                 MOVE 'Y' TO WS-EOF-SW
           END-START

           PERFORM UNTIL END-OF-REF OR REF-FOUND
              READ JHREFMST-FILE NEXT RECORD
                 AT END
                    MOVE 'Y' TO WS-EOF-SW
                 NOT AT END
                    IF RF-REF-TYPE-CD NOT = WS-SEARCH-TYPE
                       MOVE 'Y' TO WS-EOF-SW
                    ELSE
                       IF RF-REF-CD = WS-SEARCH-CODE
                          AND RF-ACTIVE-FLAG = '1'
                          AND RF-VALID-FROM-DATE <= WS-BASE-DATE
                          AND RF-VALID-TO-DATE >= WS-BASE-DATE
                          MOVE 'Y' TO WS-REF-FOUND-SW
                       END-IF
                    END-IF
              END-READ
           END-PERFORM

           IF WS-JHREFMST-ST NOT = '00'
              AND WS-JHREFMST-ST NOT = '10'
              AND WS-JHREFMST-ST NOT = '23'
              DISPLAY 'JHREFMST READ ERROR TYPE=' WS-SEARCH-TYPE
                      ' ST=' WS-JHREFMST-ST
              MOVE 12 TO RETURN-CODE
              SET HARD-ERROR TO TRUE
           END-IF
           .

       5000-WRITE-DQERR.
           MOVE LK-ATTR-TYPE-CD TO WS-ERR-ATTR
           MOVE LK-ATTR-CD TO WS-ERR-CODE
           MOVE RQ-CUST-ID TO WS-ERR-CUST

           EVALUATE LK-REASON-CD
              WHEN 'R001'
                 MOVE 'KIGN' TO WS-ERR-CLASS
                 MOVE 'EXPIRED ATTRIBUTE CODE' TO WS-ERR-REASON
              WHEN 'R404'
                 MOVE 'MTOR' TO WS-ERR-CLASS
                 MOVE 'REFERENCE MASTER NOT FOUND' TO WS-ERR-REASON
              WHEN 'R401'
                 MOVE 'MUKO' TO WS-ERR-CLASS
                 MOVE 'REFERENCE MASTER INACTIVE' TO WS-ERR-REASON
              WHEN OTHER
                 IF LK-EXPIRED
                    MOVE 'KIGN' TO WS-ERR-CLASS
                    MOVE 'EXPIRED ATTRIBUTE CODE' TO WS-ERR-REASON
                 ELSE
                    IF LK-NOT-REGISTERED
                       MOVE 'MTOR' TO WS-ERR-CLASS
                       MOVE 'REFERENCE MASTER NOT FOUND'
                         TO WS-ERR-REASON
                    ELSE
                       MOVE 'KENS' TO WS-ERR-CLASS
                       MOVE 'ATTRIBUTE VALIDATION ERROR'
                         TO WS-ERR-REASON
                    END-IF
                 END-IF
           END-EVALUATE

           DISPLAY 'JHDQERRF CLASS CUST=' WS-ERR-CUST
                   ' ATTR=' WS-ERR-ATTR
                   ' CODE=' WS-ERR-CODE
                   ' CLASS=' WS-ERR-CLASS
                   ' SEV=' LK-SEVERITY
                   ' REASON=' LK-REASON-CD
                   ' TEXT=' WS-ERR-REASON
           ADD 1 TO WS-ERR-CNT
           .

       9000-FINAL.
           IF WS-JHREFMST-ST NOT = SPACES
              CLOSE JHREFMST-FILE
              IF WS-JHREFMST-ST NOT = '00'
                 AND WS-JHREFMST-ST NOT = '42'
                 DISPLAY 'JHREFMST CLOSE ERROR ST='
                         WS-JHREFMST-ST
                 MOVE 8 TO RETURN-CODE
              END-IF
           END-IF

           IF WS-JHCATTF-ST NOT = SPACES
              CLOSE JHCATTF-FILE
              IF WS-JHCATTF-ST NOT = '00'
                 AND WS-JHCATTF-ST NOT = '42'
                 DISPLAY 'JHCATTF CLOSE ERROR ST='
                         WS-JHCATTF-ST
                 MOVE 8 TO RETURN-CODE
              END-IF
           END-IF

           IF RETURN-CODE = 0
              DISPLAY 'JH320B NORMAL END PROC=' WS-PROC-CNT
                      ' UPDATE=' WS-UPD-CNT
                      ' ERROR=' WS-ERR-CNT
                      ' SKIP=' WS-SKIP-CNT
           ELSE
              DISPLAY 'JH320B ABNORMAL END RC=' RETURN-CODE
                      ' PROC=' WS-PROC-CNT
                      ' UPDATE=' WS-UPD-CNT
                      ' ERROR=' WS-ERR-CNT
           END-IF
           .
