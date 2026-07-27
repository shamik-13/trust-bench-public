       IDENTIFICATION DIVISION.
       PROGRAM-ID. CB219B.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CDMERF ASSIGN TO "CDMERF"
             ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC
             RECORD KEY IS MR-MERCHANT-CODE
             FILE STATUS IS CDMERF-ST.

       DATA DIVISION.
       FILE SECTION.
       FD  CDMERF.
           COPY CDMERC.

       WORKING-STORAGE SECTION.
       01  CDMERF-ST                 PIC XX VALUE SPACE.
       01  WK-ABEND-SW               PIC X VALUE SPACE.
           88  WK-ABEND              VALUE '1'.
       01  WK-FILE-OPEN-SW           PIC X VALUE SPACE.
           88  WK-FILE-OPEN          VALUE '1'.

       01  WK-IDX                    PIC 9(03) COMP VALUE ZERO.
       01  WK-JDX                    PIC 9(03) COMP VALUE ZERO.
       01  WK-FIND-IDX               PIC 9(03) COMP VALUE ZERO.
       01  WK-LATEST-CNT             PIC 9(03) COMP VALUE ZERO.
       01  WK-APPLY-CNT              PIC 9(07) COMP-3 VALUE ZERO.
       01  WK-SKIP-CNT               PIC 9(07) COMP-3 VALUE ZERO.
       01  WK-ERR-CNT                PIC 9(07) COMP-3 VALUE ZERO.
       01  WK-DISP-CNT               PIC Z(06)9.

       01  WK-CHG-MAX                PIC 9(03) COMP VALUE 12.

       01  WK-CHG-TABLE.
           05  WK-CHG-REC OCCURS 12 TIMES.
               10  WK-CHG-MER-CD     PIC X(15).
               10  WK-CHG-NAME       PIC X(40).
               10  WK-CHG-MCC        PIC X(04).
               10  WK-CHG-RISK       PIC X.
               10  WK-CHG-STATUS     PIC X.
               10  WK-CHG-COUNTRY    PIC X(03).
               10  WK-CHG-DT         PIC 9(14).

       01  WK-LATEST-TABLE.
           05  WK-LATEST-REC OCCURS 12 TIMES.
               10  WK-L-MER-CD       PIC X(15).
               10  WK-L-NAME         PIC X(40).
               10  WK-L-MCC          PIC X(04).
               10  WK-L-RISK         PIC X.
               10  WK-L-STATUS       PIC X.
               10  WK-L-COUNTRY      PIC X(03).
               10  WK-L-DT           PIC 9(14).

       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 0100-INIT
           IF NOT WK-ABEND
              PERFORM 0200-MAKE-LATEST
           END-IF
           IF NOT WK-ABEND
              PERFORM 0300-OPEN-FILE
           END-IF
           IF NOT WK-ABEND
              PERFORM 0400-APPLY-CHANGES
           END-IF
           PERFORM 9000-END
           GOBACK.

       0100-INIT.
           MOVE 0 TO RETURN-CODE
           MOVE SPACE TO WK-ABEND-SW
           MOVE SPACE TO WK-FILE-OPEN-SW
           MOVE SPACE TO CDMERF-ST
           PERFORM VARYING WK-IDX FROM 1 BY 1 UNTIL WK-IDX > 12
              MOVE SPACE TO WK-CHG-REC(WK-IDX)
              MOVE SPACE TO WK-LATEST-REC(WK-IDX)
              MOVE ZERO  TO WK-CHG-DT(WK-IDX)
              MOVE ZERO  TO WK-L-DT(WK-IDX)
           END-PERFORM

           MOVE 'M000000000001' TO WK-CHG-MER-CD(1)
           MOVE 'TOKYO DENKI HANBAI' TO WK-CHG-NAME(1)
           MOVE '5732' TO WK-CHG-MCC(1)
           MOVE '2' TO WK-CHG-RISK(1)
           MOVE '0' TO WK-CHG-STATUS(1)
           MOVE 'JPN' TO WK-CHG-COUNTRY(1)
           MOVE 20250110103000 TO WK-CHG-DT(1)

           MOVE 'M000000000002' TO WK-CHG-MER-CD(2)
           MOVE 'NANIWA KOTSU SERVICE' TO WK-CHG-NAME(2)
           MOVE '4121' TO WK-CHG-MCC(2)
           MOVE '3' TO WK-CHG-RISK(2)
           MOVE '0' TO WK-CHG-STATUS(2)
           MOVE 'JPN' TO WK-CHG-COUNTRY(2)
           MOVE 20250110120000 TO WK-CHG-DT(2)

           MOVE 'M000000000003' TO WK-CHG-MER-CD(3)
           MOVE 'MINATO KOKUSAI BUPPAN' TO WK-CHG-NAME(3)
           MOVE '5999' TO WK-CHG-MCC(3)
           MOVE '4' TO WK-CHG-RISK(3)
           MOVE '0' TO WK-CHG-STATUS(3)
           MOVE 'USA' TO WK-CHG-COUNTRY(3)
           MOVE 20250110120500 TO WK-CHG-DT(3)

           MOVE 'M000000000002' TO WK-CHG-MER-CD(4)
           MOVE 'NANIWA KOTSU SERVICE' TO WK-CHG-NAME(4)
           MOVE '7995' TO WK-CHG-MCC(4)
           MOVE '5' TO WK-CHG-RISK(4)
           MOVE '9' TO WK-CHG-STATUS(4)
           MOVE 'JPN' TO WK-CHG-COUNTRY(4)
           MOVE 20250110135000 TO WK-CHG-DT(4)

           MOVE 'M000000000004' TO WK-CHG-MER-CD(5)
           MOVE 'KITAKYUSHU RYOKAN' TO WK-CHG-NAME(5)
           MOVE '7011' TO WK-CHG-MCC(5)
           MOVE '1' TO WK-CHG-RISK(5)
           MOVE '0' TO WK-CHG-STATUS(5)
           MOVE 'JPN' TO WK-CHG-COUNTRY(5)
           MOVE 20250110143000 TO WK-CHG-DT(5)

           MOVE 'M000000000005' TO WK-CHG-MER-CD(6)
           MOVE 'SAPPORO TSUSHIN SHOKAI' TO WK-CHG-NAME(6)
           MOVE '4816' TO WK-CHG-MCC(6)
           MOVE '4' TO WK-CHG-RISK(6)
           MOVE '0' TO WK-CHG-STATUS(6)
           MOVE 'JPN' TO WK-CHG-COUNTRY(6)
           MOVE 20250110150000 TO WK-CHG-DT(6)

           MOVE 'M000000000006' TO WK-CHG-MER-CD(7)
           MOVE 'YOKOHAMA YUNYU ZAKKA' TO WK-CHG-NAME(7)
           MOVE '5947' TO WK-CHG-MCC(7)
           MOVE '2' TO WK-CHG-RISK(7)
           MOVE '1' TO WK-CHG-STATUS(7)
           MOVE 'CHN' TO WK-CHG-COUNTRY(7)
           MOVE 20250110161000 TO WK-CHG-DT(7)

           MOVE 'M000000000003' TO WK-CHG-MER-CD(8)
           MOVE 'MINATO KOKUSAI BUPPAN' TO WK-CHG-NAME(8)
           MOVE '5999' TO WK-CHG-MCC(8)
           MOVE '3' TO WK-CHG-RISK(8)
           MOVE '0' TO WK-CHG-STATUS(8)
           MOVE 'USA' TO WK-CHG-COUNTRY(8)
           MOVE 20250110110000 TO WK-CHG-DT(8)

           MOVE 'M000000000007' TO WK-CHG-MER-CD(9)
           MOVE 'NAGOYA SHOKUHIN ICHIBA' TO WK-CHG-NAME(9)
           MOVE '5411' TO WK-CHG-MCC(9)
           MOVE '1' TO WK-CHG-RISK(9)
           MOVE '0' TO WK-CHG-STATUS(9)
           MOVE 'JPN' TO WK-CHG-COUNTRY(9)
           MOVE 20250110170000 TO WK-CHG-DT(9)

           MOVE 'M000000000008' TO WK-CHG-MER-CD(10)
           MOVE 'KOBE KAIGAI SOKIN' TO WK-CHG-NAME(10)
           MOVE '4829' TO WK-CHG-MCC(10)
           MOVE '5' TO WK-CHG-RISK(10)
           MOVE '9' TO WK-CHG-STATUS(10)
           MOVE 'KOR' TO WK-CHG-COUNTRY(10)
           MOVE 20250110173000 TO WK-CHG-DT(10)

           MOVE 'M000000000009' TO WK-CHG-MER-CD(11)
           MOVE 'FUKUOKA KENZAI CENTER' TO WK-CHG-NAME(11)
           MOVE '5211' TO WK-CHG-MCC(11)
           MOVE '2' TO WK-CHG-RISK(11)
           MOVE '0' TO WK-CHG-STATUS(11)
           MOVE 'TWN' TO WK-CHG-COUNTRY(11)
           MOVE 20250110180000 TO WK-CHG-DT(11)

           MOVE 'M000000000010' TO WK-CHG-MER-CD(12)
           MOVE 'SENDAI TICKET RYUTSU' TO WK-CHG-NAME(12)
           MOVE '7995' TO WK-CHG-MCC(12)
           MOVE '4' TO WK-CHG-RISK(12)
           MOVE '9' TO WK-CHG-STATUS(12)
           MOVE 'JPN' TO WK-CHG-COUNTRY(12)
           MOVE 20250110181500 TO WK-CHG-DT(12).

       0200-MAKE-LATEST.
           PERFORM VARYING WK-IDX FROM 1 BY 1
             UNTIL WK-IDX > WK-CHG-MAX
              MOVE ZERO TO WK-FIND-IDX
              PERFORM VARYING WK-JDX FROM 1 BY 1
                UNTIL WK-JDX > WK-LATEST-CNT
                   IF WK-L-MER-CD(WK-JDX) = WK-CHG-MER-CD(WK-IDX)
                      MOVE WK-JDX TO WK-FIND-IDX
                      MOVE WK-LATEST-CNT TO WK-JDX
                   END-IF
              END-PERFORM
              IF WK-FIND-IDX = ZERO
                 ADD 1 TO WK-LATEST-CNT
                 MOVE WK-CHG-REC(WK-IDX)
                   TO WK-LATEST-REC(WK-LATEST-CNT)
              ELSE
                 IF WK-CHG-DT(WK-IDX) > WK-L-DT(WK-FIND-IDX)
                    MOVE WK-CHG-REC(WK-IDX)
                      TO WK-LATEST-REC(WK-FIND-IDX)
                 ELSE
                    ADD 1 TO WK-SKIP-CNT
                 END-IF
              END-IF
           END-PERFORM.

       0300-OPEN-FILE.
           OPEN I-O CDMERF
           IF CDMERF-ST = '00'
              MOVE '1' TO WK-FILE-OPEN-SW
           ELSE
              DISPLAY 'CDMERF OPEN FAILED ST=' CDMERF-ST
              SET WK-ABEND TO TRUE
              MOVE 8 TO RETURN-CODE
           END-IF.

       0400-APPLY-CHANGES.
           PERFORM VARYING WK-IDX FROM 1 BY 1
             UNTIL WK-IDX > WK-LATEST-CNT OR WK-ABEND
              PERFORM 0500-VALIDATE-CHANGE
              IF NOT WK-ABEND
                 MOVE WK-L-MER-CD(WK-IDX) TO MR-MERCHANT-CODE
                 READ CDMERF KEY IS MR-MERCHANT-CODE
                   INVALID KEY
                     DISPLAY 'CDMERF NOT FOUND MER=' WK-L-MER-CD(WK-IDX)
                     ADD 1 TO WK-ERR-CNT
                   NOT INVALID KEY
                     PERFORM 0600-REWRITE-MASTER
                 END-READ
              END-IF
           END-PERFORM.

       0500-VALIDATE-CHANGE.
           IF WK-L-MCC(WK-IDX) NOT NUMERIC
              DISPLAY 'INVALID MCC MER=' WK-L-MER-CD(WK-IDX)
              ADD 1 TO WK-ERR-CNT
           ELSE
              IF WK-L-STATUS(WK-IDX) NOT = '0'
                 AND WK-L-STATUS(WK-IDX) NOT = '1'
                 AND WK-L-STATUS(WK-IDX) NOT = '9'
                 DISPLAY 'INVALID STATUS MER=' WK-L-MER-CD(WK-IDX)
                 ADD 1 TO WK-ERR-CNT
              ELSE
                 PERFORM 0510-VALIDATE-COUNTRY
              END-IF
           END-IF.

       0510-VALIDATE-COUNTRY.
           IF WK-L-COUNTRY(WK-IDX) NOT = 'JPN'
              AND WK-L-COUNTRY(WK-IDX) NOT = 'USA'
              AND WK-L-COUNTRY(WK-IDX) NOT = 'CHN'
              AND WK-L-COUNTRY(WK-IDX) NOT = 'KOR'
              AND WK-L-COUNTRY(WK-IDX) NOT = 'TWN'
              DISPLAY 'INVALID COUNTRY MER=' WK-L-MER-CD(WK-IDX)
              ADD 1 TO WK-ERR-CNT
           ELSE
              PERFORM 0520-VALIDATE-COMBINATION
           END-IF.

       0520-VALIDATE-COMBINATION.
           IF WK-L-MCC(WK-IDX) = '7995'
              AND WK-L-RISK(WK-IDX) < '4'
              DISPLAY 'INVALID RISK FOR MCC MER=' WK-L-MER-CD(WK-IDX)
              ADD 1 TO WK-ERR-CNT
           END-IF
           IF WK-L-MCC(WK-IDX) = '4829'
              AND WK-L-COUNTRY(WK-IDX) = 'JPN'
              DISPLAY 'INVALID COUNTRY FOR REMIT MER='
                 WK-L-MER-CD(WK-IDX)
              ADD 1 TO WK-ERR-CNT
           END-IF
           IF WK-L-STATUS(WK-IDX) = '9'
              AND WK-L-RISK(WK-IDX) < '4'
              DISPLAY 'INVALID RISK FOR STOP MER=' WK-L-MER-CD(WK-IDX)
              ADD 1 TO WK-ERR-CNT
           END-IF.

       0600-REWRITE-MASTER.
           MOVE WK-L-NAME(WK-IDX)    TO MR-MERCHANT-NAME-KANA
           MOVE WK-L-MCC(WK-IDX)     TO MR-MCC
           MOVE WK-L-RISK(WK-IDX)    TO MR-RISK-RANK
           MOVE WK-L-STATUS(WK-IDX)  TO MR-STATUS
           MOVE WK-L-COUNTRY(WK-IDX) TO MR-COUNTRY-CD

           REWRITE CDMERF-REC
           IF CDMERF-ST = '00'
              ADD 1 TO WK-APPLY-CNT
              IF WK-L-RISK(WK-IDX) >= '4'
                 OR WK-L-STATUS(WK-IDX) = '9'
                 DISPLAY 'REVIEW APPLIED MER=' WK-L-MER-CD(WK-IDX)
                    ' DT=' WK-L-DT(WK-IDX)
              END-IF
           ELSE
              DISPLAY 'CDMERF REWRITE FAILED MER=' WK-L-MER-CD(WK-IDX)
                 ' ST=' CDMERF-ST
              SET WK-ABEND TO TRUE
              MOVE 12 TO RETURN-CODE
           END-IF.

       9000-END.
           IF WK-FILE-OPEN
              CLOSE CDMERF
              IF CDMERF-ST NOT = '00'
                 DISPLAY 'CDMERF CLOSE FAILED ST=' CDMERF-ST
                 MOVE 8 TO RETURN-CODE
              END-IF
           END-IF
           MOVE WK-APPLY-CNT TO WK-DISP-CNT
           DISPLAY 'APPLY COUNT=' WK-DISP-CNT
           MOVE WK-SKIP-CNT TO WK-DISP-CNT
           DISPLAY 'SKIP OLD COUNT=' WK-DISP-CNT
           MOVE WK-ERR-CNT TO WK-DISP-CNT
           DISPLAY 'VALIDATION ERROR COUNT=' WK-DISP-CNT
           IF RETURN-CODE = 0
              DISPLAY 'CB219B NORMAL END'
           ELSE
              DISPLAY 'CB219B ABNORMAL END RC=' RETURN-CODE
           END-IF.
