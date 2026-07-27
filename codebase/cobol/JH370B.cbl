       IDENTIFICATION DIVISION.
       PROGRAM-ID. JH370B.
      * 変更履歴
      * 版数  年月日(和暦)  担当                    概要
      * 1.00  令和05.04.01  システム部 情報系チーム  新規作成
      * 1.01  令和05.10.16  システム部 情報系チーム  抽出条件見直し
      * 1.02  令和06.03.25  システム部 情報系チーム  候補判定項目追加
       AUTHOR. JH-BATCH.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT JHSEGRF ASSIGN TO "JHSEGRF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-JHSEGRF.
           SELECT JHCBALF ASSIGN TO "JHCBALF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-JHCBALF.
           SELECT JHCATTF ASSIGN TO "JHCATTF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CA-CUST-ID
               FILE STATUS IS FS-JHCATTF.
           SELECT JHCMPRF ASSIGN TO "JHCMPRF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-JHCMPRF.
           SELECT JHREFMST ASSIGN TO "JHREFMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS RF-REF-TYPE-CD
               FILE STATUS IS FS-JHREFMST.
           SELECT JHXSELLF ASSIGN TO "JHXSELLF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-JHXSELLF.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  JHSEGRF.
       COPY JHSEGRFC.
      *
       FD  JHCBALF.
       COPY JHCBALFC.
      *
       FD  JHCATTF.
       COPY JHCATTC.
      *
       FD  JHCMPRF.
       COPY JHCMPRC.
      *
       FD  JHREFMST.
       COPY JHREFMSTC.
      *
       FD  JHXSELLF.
       COPY JHXSELLC.
      *
       WORKING-STORAGE SECTION.
       01  FS-AREA.
           05 FS-JHSEGRF              PIC XX.
           05 FS-JHCBALF              PIC XX.
           05 FS-JHCATTF              PIC XX.
           05 FS-JHCMPRF              PIC XX.
           05 FS-JHREFMST             PIC XX.
           05 FS-JHXSELLF             PIC XX.
      *
       01  SW-AREA.
           05 SW-EOF-SEGR             PIC X VALUE "N".
              88 EOF-SEGR                  VALUE "Y".
           05 SW-EOF-CBAL             PIC X VALUE "N".
              88 EOF-CBAL                  VALUE "Y".
           05 SW-EOF-CMPR             PIC X VALUE "N".
              88 EOF-CMPR                  VALUE "Y".
           05 SW-EOF-REF              PIC X VALUE "N".
              88 EOF-REF                   VALUE "Y".
           05 SW-HARD-ERR             PIC X VALUE "N".
              88 HARD-ERR                  VALUE "Y".
           05 SW-ATTR-FOUND           PIC X VALUE "N".
              88 ATTR-FOUND                VALUE "Y".
           05 SW-SUPPRESS             PIC X VALUE "N".
              88 SUPPRESS-YES              VALUE "Y".
      *
       01  WK-AREA.
           05 WK-PROCESS-DATE         PIC 9(08) VALUE 20240930.
           05 WK-RECENT-FROM-DATE     PIC 9(08) VALUE 20240701.
           05 WK-OUT-COUNT            PIC 9(09) VALUE ZERO.
           05 WK-IN-COUNT             PIC 9(09) VALUE ZERO.
           05 WK-SKIP-COUNT           PIC 9(09) VALUE ZERO.
           05 WK-ERR-COUNT            PIC 9(09) VALUE ZERO.
           05 WK-BASE-BAL             PIC S9(13)V99 COMP-3 VALUE ZERO.
           05 WK-SEG-PRIORITY         PIC 9 VALUE ZERO.
           05 WK-PROD-CNT             PIC 9 VALUE ZERO.
           05 WK-SCORE-RANK           PIC 99 VALUE ZERO.
           05 WK-IDX                  PIC 99 VALUE ZERO.
           05 WK-JDX                  PIC 99 VALUE ZERO.
           05 WK-TMP-PROD             PIC X(04) VALUE SPACE.
           05 WK-TMP-PRI              PIC 9 VALUE ZERO.
           05 WK-TMP-SCORE            PIC S9(05) COMP-3 VALUE ZERO.
           05 WK-TMP-RSN              PIC X(06) VALUE SPACE.
           05 WK-TMP-VALID            PIC X VALUE SPACE.
      *
       01  PROD-TABLE.
           05 PROD-ENT OCCURS 5 TIMES.
              10 PROD-CD              PIC X(04).
              10 PROD-PRI             PIC 9.
              10 PROD-SCORE           PIC S9(05) COMP-3.
              10 PROD-REASON          PIC X(06).
              10 PROD-VALID           PIC X.
      *
       01  CMP-TABLE.
           05 CMP-CNT                 PIC 9(04) VALUE ZERO.
           05 CMP-ENT OCCURS 2000 TIMES.
              10 CMP-CUST-ID          PIC X(16).
              10 CMP-PROD-CD          PIC X(04).
              10 CMP-EXT-DATE         PIC 9(08).
      *
       01  REF-TABLE.
           05 REF-CNT                 PIC 9(03) VALUE ZERO.
           05 REF-ENT OCCURS 200 TIMES.
              10 REF-TYPE-CD          PIC X(04).
              10 REF-CD               PIC X(04).
              10 REF-NAME             PIC X(40).
      *
       01  JH372-PARM.
           05 JH372-CUST-ID           PIC X(16).
           05 JH372-SEG-CD            PIC X(04).
           05 JH372-AVG-BAL           PIC S9(13)V99 COMP-3.
           05 JH372-AGE-BAND-CD       PIC X(02).
           05 JH372-PREF-CD           PIC X(02).
           05 JH372-OCCUP-CD          PIC X(04).
           05 JH372-TRUST-REL-CD      PIC X(02).
           05 JH372-PRODUCT-CD        PIC X(04).
           05 JH372-SUPPRESS-FLAG     PIC X.
           05 JH372-SCORE             PIC S9(05) COMP-3.
           05 JH372-REASON-CD         PIC X(06).
           05 JH372-RETURN-CD         PIC 99.
      *
       PROCEDURE DIVISION.
       MAIN-RTN.
           MOVE 0 TO RETURN-CODE
           PERFORM OPEN-RTN
           IF NOT HARD-ERR
              PERFORM LOAD-REF-RTN
              PERFORM LOAD-CMPR-RTN
              PERFORM READ-SEGR-RTN
              PERFORM READ-CBAL-RTN
              PERFORM UNTIL EOF-SEGR OR HARD-ERR
                 PERFORM MATCH-PROCESS-RTN
              END-PERFORM
           END-IF
           PERFORM CLOSE-RTN
           IF HARD-ERR
              MOVE 8 TO RETURN-CODE
           ELSE
              DISPLAY "JH370B END OUT=" WK-OUT-COUNT
              MOVE 0 TO RETURN-CODE
           END-IF
           GOBACK.
      *
       OPEN-RTN.
           OPEN INPUT JHSEGRF
           IF FS-JHSEGRF NOT = "00"
              DISPLAY "JHSEGRF OPEN ST=" FS-JHSEGRF
              SET HARD-ERR TO TRUE
           END-IF
           OPEN INPUT JHCBALF
           IF FS-JHCBALF NOT = "00"
              DISPLAY "JHCBALF OPEN ST=" FS-JHCBALF
              SET HARD-ERR TO TRUE
           END-IF
           OPEN INPUT JHCATTF
           IF FS-JHCATTF NOT = "00"
              DISPLAY "JHCATTF OPEN ST=" FS-JHCATTF
              SET HARD-ERR TO TRUE
           END-IF
           OPEN INPUT JHCMPRF
           IF FS-JHCMPRF NOT = "00"
              DISPLAY "JHCMPRF OPEN ST=" FS-JHCMPRF
              SET HARD-ERR TO TRUE
           END-IF
           OPEN INPUT JHREFMST
           IF FS-JHREFMST NOT = "00"
              DISPLAY "JHREFMST OPEN ST=" FS-JHREFMST
              SET HARD-ERR TO TRUE
           END-IF
           OPEN OUTPUT JHXSELLF
           IF FS-JHXSELLF NOT = "00"
              DISPLAY "JHXSELLF OPEN ST=" FS-JHXSELLF
              SET HARD-ERR TO TRUE
           END-IF.
      *
       LOAD-REF-RTN.
           PERFORM UNTIL EOF-REF OR HARD-ERR
              READ JHREFMST
                 AT END
                    SET EOF-REF TO TRUE
                 NOT AT END
                    IF FS-JHREFMST = "00"
                       IF RF-ACTIVE-FLAG = "1"
                          AND RF-VALID-FROM-DATE <= WK-PROCESS-DATE
                          AND RF-VALID-TO-DATE >= WK-PROCESS-DATE
                          IF REF-CNT < 200
                             ADD 1 TO REF-CNT
                             MOVE RF-REF-TYPE-CD
                               TO REF-TYPE-CD(REF-CNT)
                             MOVE RF-REF-CD
                               TO REF-CD(REF-CNT)
                             MOVE RF-REF-NAME
                               TO REF-NAME(REF-CNT)
                          ELSE
                             DISPLAY "REF TABLE OVERFLOW"
                             SET HARD-ERR TO TRUE
                          END-IF
                       END-IF
                    ELSE
                       DISPLAY "JHREFMST READ ST=" FS-JHREFMST
                       SET HARD-ERR TO TRUE
                    END-IF
              END-READ
           END-PERFORM.
      *
       LOAD-CMPR-RTN.
           PERFORM UNTIL EOF-CMPR OR HARD-ERR
              READ JHCMPRF
                 AT END
                    SET EOF-CMPR TO TRUE
                 NOT AT END
                    IF FS-JHCMPRF = "00"
                       IF CP-EXTRACT-DATE >= WK-RECENT-FROM-DATE
                          IF CMP-CNT < 2000
                             ADD 1 TO CMP-CNT
                             MOVE CP-CUST-ID
                               TO CMP-CUST-ID(CMP-CNT)
                             MOVE CP-TARGET-REASON-CD
                               TO CMP-PROD-CD(CMP-CNT)
                             MOVE CP-EXTRACT-DATE
                               TO CMP-EXT-DATE(CMP-CNT)
                          ELSE
                             DISPLAY "CMP TABLE OVERFLOW"
                             SET HARD-ERR TO TRUE
                          END-IF
                       END-IF
                    ELSE
                       DISPLAY "JHCMPRF READ ST=" FS-JHCMPRF
                       SET HARD-ERR TO TRUE
                    END-IF
              END-READ
           END-PERFORM.
      *
       READ-SEGR-RTN.
           READ JHSEGRF
              AT END
                 SET EOF-SEGR TO TRUE
              NOT AT END
                 IF FS-JHSEGRF NOT = "00"
                    DISPLAY "JHSEGRF READ ST=" FS-JHSEGRF
                    SET HARD-ERR TO TRUE
                 END-IF
           END-READ.
      *
       READ-CBAL-RTN.
           READ JHCBALF
              AT END
                 SET EOF-CBAL TO TRUE
              NOT AT END
                 IF FS-JHCBALF NOT = "00"
                    DISPLAY "JHCBALF READ ST=" FS-JHCBALF
                    SET HARD-ERR TO TRUE
                 END-IF
           END-READ.
      *
       MATCH-PROCESS-RTN.
           IF EOF-CBAL
              DISPLAY "NO BALANCE CUST=" SR-CUST-ID
              ADD 1 TO WK-SKIP-COUNT
              PERFORM READ-SEGR-RTN
           ELSE
              EVALUATE TRUE
                 WHEN SR-CUST-ID = CB-CUST-ID
                    ADD 1 TO WK-IN-COUNT
                    PERFORM CUSTOMER-PROCESS-RTN
                    PERFORM READ-SEGR-RTN
                    PERFORM READ-CBAL-RTN
                 WHEN SR-CUST-ID < CB-CUST-ID
                    DISPLAY "NO BALANCE CUST=" SR-CUST-ID
                    ADD 1 TO WK-SKIP-COUNT
                    PERFORM READ-SEGR-RTN
                 WHEN OTHER
                    DISPLAY "NO SEGMENT CUST=" CB-CUST-ID
                    ADD 1 TO WK-SKIP-COUNT
                    PERFORM READ-CBAL-RTN
              END-EVALUATE
           END-IF.
      *
       CUSTOMER-PROCESS-RTN.
           PERFORM VALIDATE-SEG-RTN
           IF WK-SEG-PRIORITY = ZERO
              ADD 1 TO WK-ERR-COUNT
              DISPLAY "BAD SEGMENT CUST=" SR-CUST-ID
           ELSE
              MOVE "N" TO SW-ATTR-FOUND
              MOVE SR-CUST-ID TO CA-CUST-ID
              READ JHCATTF KEY IS CA-CUST-ID
                 INVALID KEY
                    ADD 1 TO WK-SKIP-COUNT
                    DISPLAY "NO ATTR CUST=" SR-CUST-ID
                 NOT INVALID KEY
                    IF FS-JHCATTF = "00"
                       SET ATTR-FOUND TO TRUE
                    ELSE
                       DISPLAY "JHCATTF READ ST=" FS-JHCATTF
                       SET HARD-ERR TO TRUE
                    END-IF
              END-READ
              IF ATTR-FOUND AND NOT HARD-ERR
                 PERFORM BUILD-CANDIDATE-RTN
                 PERFORM RANK-AND-WRITE-RTN
              END-IF
           END-IF.
      *
       VALIDATE-SEG-RTN.
           MOVE ZERO TO WK-SEG-PRIORITY
           EVALUATE SR-SEG-CD
              WHEN "SG01"
                 MOVE 1 TO WK-SEG-PRIORITY
              WHEN "SG02"
                 MOVE 2 TO WK-SEG-PRIORITY
              WHEN "SG03"
                 MOVE 3 TO WK-SEG-PRIORITY
              WHEN "SG04"
                 MOVE 4 TO WK-SEG-PRIORITY
              WHEN "SG05"
                 MOVE 5 TO WK-SEG-PRIORITY
              WHEN OTHER
                 MOVE ZERO TO WK-SEG-PRIORITY
           END-EVALUATE.
      *
       BUILD-CANDIDATE-RTN.
           INITIALIZE PROD-TABLE
           MOVE ZERO TO WK-PROD-CNT
           IF CA-DM-PERMIT-FLAG NOT = "1"
              EXIT PARAGRAPH
           END-IF
           COMPUTE WK-BASE-BAL = (SR-AVG-BAL + CB-AVG-BAL) / 2
           IF WK-BASE-BAL >= 30000000
              MOVE 1 TO WK-IDX
              PERFORM ADD-CAND-RTN
           END-IF
           IF WK-BASE-BAL >= 10000000
              AND CA-TRUST-REL-CD NOT = "9"
              MOVE 2 TO WK-IDX
              PERFORM ADD-CAND-RTN
           END-IF
           IF CA-AGE-BAND-CD >= "50"
              AND WK-BASE-BAL >= 6000000
              MOVE 3 TO WK-IDX
              PERFORM ADD-CAND-RTN
           END-IF
           IF CA-HOUSEHOLD-ID NOT = SPACES
              AND WK-SEG-PRIORITY >= 3
              MOVE 4 TO WK-IDX
              PERFORM ADD-CAND-RTN
           END-IF
           IF WK-SEG-PRIORITY >= 4
              AND WK-BASE-BAL >= 15000000
              MOVE 5 TO WK-IDX
              PERFORM ADD-CAND-RTN
           END-IF.
      *
       ADD-CAND-RTN.
           IF WK-PROD-CNT < 5
              ADD 1 TO WK-PROD-CNT
              EVALUATE WK-IDX
                 WHEN 1
                    MOVE "TR01" TO PROD-CD(WK-PROD-CNT)
                    MOVE 1      TO PROD-PRI(WK-PROD-CNT)
                 WHEN 2
                    MOVE "FD02" TO PROD-CD(WK-PROD-CNT)
                    MOVE 3      TO PROD-PRI(WK-PROD-CNT)
                 WHEN 3
                    MOVE "IN03" TO PROD-CD(WK-PROD-CNT)
                    MOVE 2      TO PROD-PRI(WK-PROD-CNT)
                 WHEN 4
                    MOVE "LN04" TO PROD-CD(WK-PROD-CNT)
                    MOVE 5      TO PROD-PRI(WK-PROD-CNT)
                 WHEN 5
                    MOVE "WR05" TO PROD-CD(WK-PROD-CNT)
                    MOVE 4      TO PROD-PRI(WK-PROD-CNT)
              END-EVALUATE
              MOVE "1" TO PROD-VALID(WK-PROD-CNT)
              PERFORM SCORE-CAND-RTN
           END-IF.
      *
       SCORE-CAND-RTN.
           MOVE SR-CUST-ID TO JH372-CUST-ID
           MOVE SR-SEG-CD TO JH372-SEG-CD
           MOVE WK-BASE-BAL TO JH372-AVG-BAL
           MOVE CA-AGE-BAND-CD TO JH372-AGE-BAND-CD
           MOVE CA-PREF-CD TO JH372-PREF-CD
           MOVE CA-OCCUP-CD TO JH372-OCCUP-CD
           MOVE CA-TRUST-REL-CD TO JH372-TRUST-REL-CD
           MOVE PROD-CD(WK-PROD-CNT) TO JH372-PRODUCT-CD
           PERFORM CHECK-SUPPRESS-RTN
           IF SUPPRESS-YES
              MOVE "1" TO JH372-SUPPRESS-FLAG
           ELSE
              MOVE "0" TO JH372-SUPPRESS-FLAG
           END-IF
           CALL "JH372S" USING JH372-PARM
           IF JH372-RETURN-CD = 0
              MOVE JH372-SCORE TO PROD-SCORE(WK-PROD-CNT)
              MOVE JH372-REASON-CD TO PROD-REASON(WK-PROD-CNT)
           ELSE
              DISPLAY "SCORE ERROR CUST=" SR-CUST-ID
              MOVE "0" TO PROD-VALID(WK-PROD-CNT)
              ADD 1 TO WK-ERR-COUNT
           END-IF.
      *
       CHECK-SUPPRESS-RTN.
           MOVE "N" TO SW-SUPPRESS
           PERFORM VARYING WK-JDX FROM 1 BY 1
              UNTIL WK-JDX > CMP-CNT OR SUPPRESS-YES
              IF CMP-CUST-ID(WK-JDX) = SR-CUST-ID
                 AND CMP-PROD-CD(WK-JDX) = PROD-CD(WK-PROD-CNT)
                 SET SUPPRESS-YES TO TRUE
              END-IF
           END-PERFORM.
      *
       RANK-AND-WRITE-RTN.
           PERFORM SORT-CAND-RTN
           MOVE ZERO TO WK-SCORE-RANK
           PERFORM VARYING WK-IDX FROM 1 BY 1
              UNTIL WK-IDX > WK-PROD-CNT OR HARD-ERR
              IF PROD-VALID(WK-IDX) = "1"
                 ADD 1 TO WK-SCORE-RANK
                 PERFORM WRITE-XSELL-RTN
              END-IF
           END-PERFORM.
      *
       SORT-CAND-RTN.
           PERFORM VARYING WK-IDX FROM 1 BY 1
              UNTIL WK-IDX >= WK-PROD-CNT
              PERFORM VARYING WK-JDX FROM WK-IDX BY 1
                 UNTIL WK-JDX > WK-PROD-CNT
                 IF PROD-SCORE(WK-JDX) > PROD-SCORE(WK-IDX)
                    OR (PROD-SCORE(WK-JDX) = PROD-SCORE(WK-IDX)
                    AND PROD-PRI(WK-JDX) < PROD-PRI(WK-IDX))
                    PERFORM SWAP-CAND-RTN
                 END-IF
              END-PERFORM
           END-PERFORM.
      *
       SWAP-CAND-RTN.
           MOVE PROD-CD(WK-IDX) TO WK-TMP-PROD
           MOVE PROD-PRI(WK-IDX) TO WK-TMP-PRI
           MOVE PROD-SCORE(WK-IDX) TO WK-TMP-SCORE
           MOVE PROD-REASON(WK-IDX) TO WK-TMP-RSN
           MOVE PROD-VALID(WK-IDX) TO WK-TMP-VALID
           MOVE PROD-CD(WK-JDX) TO PROD-CD(WK-IDX)
           MOVE PROD-PRI(WK-JDX) TO PROD-PRI(WK-IDX)
           MOVE PROD-SCORE(WK-JDX) TO PROD-SCORE(WK-IDX)
           MOVE PROD-REASON(WK-JDX) TO PROD-REASON(WK-IDX)
           MOVE PROD-VALID(WK-JDX) TO PROD-VALID(WK-IDX)
           MOVE WK-TMP-PROD TO PROD-CD(WK-JDX)
           MOVE WK-TMP-PRI TO PROD-PRI(WK-JDX)
           MOVE WK-TMP-SCORE TO PROD-SCORE(WK-JDX)
           MOVE WK-TMP-RSN TO PROD-REASON(WK-JDX)
           MOVE WK-TMP-VALID TO PROD-VALID(WK-JDX).
      *
       WRITE-XSELL-RTN.
           INITIALIZE JHXSELLF-REC
           MOVE SR-CUST-ID TO XS-CUST-ID
           MOVE SR-SEG-CD TO XS-SEG-CD
           MOVE PROD-CD(WK-IDX) TO XS-CANDIDATE-PRODUCT-CD
           MOVE WK-SCORE-RANK TO XS-SCORE-RANK
           MOVE WK-BASE-BAL TO XS-AVG-BAL
           MOVE PROD-REASON(WK-IDX) TO XS-REASON-CD
           WRITE JHXSELLF-REC
           IF FS-JHXSELLF = "00"
              ADD 1 TO WK-OUT-COUNT
           ELSE
              DISPLAY "JHXSELLF WRITE ST=" FS-JHXSELLF
              SET HARD-ERR TO TRUE
           END-IF.
      *
       CLOSE-RTN.
           CLOSE JHSEGRF
           CLOSE JHCBALF
           CLOSE JHCATTF
           CLOSE JHCMPRF
           CLOSE JHREFMST
           CLOSE JHXSELLF.
