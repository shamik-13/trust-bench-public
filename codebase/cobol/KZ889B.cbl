       IDENTIFICATION DIVISION.
       PROGRAM-ID. KZ889B.
      *----------------------------------------------------------------*
      * 変更履歴                                                       *
      * 版数  年月日    担当    概要                                   *
      * 1.00  20180401  KZB01   初版作成                               *
      * 1.10  20210331  KZB02   訂正レコード原支払年帰属対応           *
      * 1.20  20250331  KZB03   KZ902S残高照合分岐追加                 *
      *----------------------------------------------------------------*
      * 源泉税年間集計モノリス                                         *
      * KZTXRFの既計算税額を積上げ、税額再計算は行わない。             *
      *----------------------------------------------------------------*

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZTXRF
               ASSIGN TO "KZTXRF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-KZTXRF.
           SELECT KZPRIF
               ASSIGN TO "KZPRIF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-KZPRIF.
           SELECT KZPMRF
               ASSIGN TO "KZPMRF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-KZPMRF.
           SELECT KZLCRF
               ASSIGN TO "KZLCRF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-KZLCRF.
           SELECT KZGLPF
               ASSIGN TO "KZGLPF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-KZGLPF.
           SELECT KZNIMF
               ASSIGN TO "KZNIMF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS NI-ACCT-NO
               FILE STATUS IS FS-KZNIMF.
           SELECT KZNRMF
               ASSIGN TO "KZNRMF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS NR-ACCT-NO
               FILE STATUS IS FS-KZNRMF.
           SELECT KZYAGF
               ASSIGN TO "KZYAGF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-KZYAGF.
           SELECT KZADLF
               ASSIGN TO "KZADLF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS AL-AUDIT-ID
               FILE STATUS IS FS-KZADLF.

       DATA DIVISION.
       FILE SECTION.
       FD  KZTXRF.
           COPY KZTXRFC.
       FD  KZPRIF.
           COPY KZPRIFC.
       FD  KZPMRF.
           COPY KZPMRFC.
       FD  KZLCRF.
           COPY KZLCRFC.
       FD  KZGLPF.
           COPY KZGLPFC2.
       FD  KZNIMF.
           COPY KZNIMFC.
       FD  KZNRMF.
           COPY KZNRMFC.
       FD  KZYAGF.
           COPY KZYAGFC.
       FD  KZADLF.
           COPY KZADLFC.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05 FS-KZTXRF              PIC XX.
           05 FS-KZPRIF              PIC XX.
           05 FS-KZPMRF              PIC XX.
           05 FS-KZLCRF              PIC XX.
           05 FS-KZGLPF              PIC XX.
           05 FS-KZNIMF              PIC XX.
           05 FS-KZNRMF              PIC XX.
           05 FS-KZYAGF              PIC XX.
           05 FS-KZADLF              PIC XX.

       01  WS-EOF-FLAGS.
           05 SW-TX-EOF              PIC X VALUE "N".
              88 TX-EOF                    VALUE "Y".
           05 SW-PR-EOF              PIC X VALUE "N".
              88 PR-EOF                    VALUE "Y".
           05 SW-PM-EOF              PIC X VALUE "N".
              88 PM-EOF                    VALUE "Y".
           05 SW-LC-EOF              PIC X VALUE "N".
              88 LC-EOF                    VALUE "Y".
           05 SW-GP-EOF              PIC X VALUE "N".
              88 GP-EOF                    VALUE "Y".
           05 SW-ABEND               PIC X VALUE "N".
              88 ABEND-ON                  VALUE "Y".

       01  WS-RUN.
           05 WS-RUN-DATE            PIC 9(8).
           05 WS-TARGET-YEAR         PIC 9(4).
           05 WS-BASE-YEAR           PIC 9(4).
           05 WS-AUDIT-SEQ           PIC 9(7) VALUE 0.
           05 WS-YA-WRITE-CNT        PIC 9(9) VALUE 0.
           05 WS-AL-WRITE-CNT        PIC 9(9) VALUE 0.
           05 WS-TX-READ-CNT         PIC 9(9) VALUE 0.
           05 WS-PR-READ-CNT         PIC 9(9) VALUE 0.
           05 WS-PM-READ-CNT         PIC 9(9) VALUE 0.
           05 WS-LC-READ-CNT         PIC 9(9) VALUE 0.
           05 WS-GP-READ-CNT         PIC 9(9) VALUE 0.
           05 WS-DISPLAY-AMT         PIC ZZZ,ZZZ,ZZZ,ZZ9.99-.

       01  WS-CURRENT-ACCOUNT.
           05 WS-CUR-ACCT-NO         PIC X(16) VALUE SPACES.
           05 WS-CUR-ACCT-TYPE       PIC X(02) VALUE SPACES.
           05 WS-CUR-GROSS           PIC S9(13)V99 COMP-3 VALUE 0.
           05 WS-CUR-NATIONAL        PIC S9(13)V99 COMP-3 VALUE 0.
           05 WS-CUR-LOCAL           PIC S9(13)V99 COMP-3 VALUE 0.
           05 WS-CUR-TOTAL           PIC S9(13)V99 COMP-3 VALUE 0.
           05 WS-CUR-NET             PIC S9(13)V99 COMP-3 VALUE 0.
           05 WS-CUR-COUNT           PIC 9(7) VALUE 0.
           05 WS-CALC-TOTAL          PIC S9(13)V99 COMP-3 VALUE 0.
           05 WS-CALC-NET            PIC S9(13)V99 COMP-3 VALUE 0.

       01  WS-TAX-OFFICE-TABLE.
           05 WS-TAX-OFFICE OCCURS 200 TIMES.
              10 WT-USED             PIC X VALUE "N".
              10 WT-CD               PIC X(06).
              10 WT-TX-NATIONAL      PIC S9(13)V99 COMP-3.
              10 WT-PM-NATIONAL      PIC S9(13)V99 COMP-3.
              10 WT-STMT-CNT         PIC 9(7).

       01  WS-PREF-TABLE.
           05 WS-PREF OCCURS 60 TIMES.
              10 WP-USED             PIC X VALUE "N".
              10 WP-CD               PIC X(02).
              10 WP-TX-LOCAL         PIC S9(13)V99 COMP-3.
              10 WP-LC-LOCAL         PIC S9(13)V99 COMP-3.
              10 WP-REC-CNT          PIC 9(7).

       01  WS-NR-TABLE.
           05 WS-NR OCCURS 20 TIMES.
              10 WN-USED             PIC X VALUE "N".
              10 WN-CD               PIC X(03).
              10 WN-GROSS            PIC S9(13)V99 COMP-3.
              10 WN-NATIONAL         PIC S9(13)V99 COMP-3.
              10 WN-LOCAL            PIC S9(13)V99 COMP-3.
              10 WN-COUNT            PIC 9(7).

       01  WS-GL-SUMMARY.
           05 WS-TX-NATIONAL-TOTAL   PIC S9(13)V99 COMP-3 VALUE 0.
           05 WS-TX-LOCAL-TOTAL      PIC S9(13)V99 COMP-3 VALUE 0.
           05 WS-TX-NET-TOTAL        PIC S9(13)V99 COMP-3 VALUE 0.
           05 WS-PR-NATIONAL-TOTAL   PIC S9(13)V99 COMP-3 VALUE 0.
           05 WS-PR-LOCAL-TOTAL      PIC S9(13)V99 COMP-3 VALUE 0.
           05 WS-PM-NATIONAL-TOTAL   PIC S9(13)V99 COMP-3 VALUE 0.
           05 WS-LC-LOCAL-TOTAL      PIC S9(13)V99 COMP-3 VALUE 0.
           05 WS-GP-WH-TOTAL         PIC S9(13)V99 COMP-3 VALUE 0.
           05 WS-GP-LOCAL-TOTAL      PIC S9(13)V99 COMP-3 VALUE 0.
           05 WS-GP-NET-TOTAL        PIC S9(13)V99 COMP-3 VALUE 0.

       01  WS-LOOKUP.
           05 WS-FOUND-IDX           PIC 9(4) COMP VALUE 0.
           05 WS-SUB                 PIC 9(4) COMP VALUE 0.
           05 WS-OPEN-SUB            PIC 9(4) COMP VALUE 0.
           05 WS-ADDR-PREF           PIC X(02) VALUE SPACES.
           05 WS-TAXOFFICE-CD        PIC X(06) VALUE SPACES.
           05 WS-COUNTRY-CD          PIC X(03) VALUE SPACES.
           05 WS-PAY-YEAR            PIC 9(4) VALUE 0.
           05 WS-ORIG-YEAR           PIC 9(4) VALUE 0.

       01  WS-KZ902S-LINKAGE.
           05 LK-AUDIT-ID            PIC X(20).
           05 LK-SOURCE-DSID         PIC X(12).
           05 LK-DEBIT-AMT           PIC S9(13)V99 COMP-3.
           05 LK-CREDIT-AMT          PIC S9(13)V99 COMP-3.
           05 LK-RTN-CD              PIC 9(2).
           05 LK-CAUSE-CD            PIC X(2).
           05 LK-DIFF-AMT            PIC S9(13)V99 COMP-3.

       01  WS-AUDIT-WORK.
           05 WS-AUDIT-ID            PIC X(20).
           05 WS-AUDIT-PREFIX        PIC X(08).
           05 WS-AUDIT-SUFFIX        PIC 9(12).
           05 WS-AUDIT-SUFFIX-X REDEFINES WS-AUDIT-SUFFIX
                                      PIC X(12).
           05 WS-STATUS-CD           PIC X(02).
           05 WS-SOURCE-DSID         PIC X(12).
           05 WS-DEBIT-AMT           PIC S9(13)V99 COMP-3.
           05 WS-CREDIT-AMT          PIC S9(13)V99 COMP-3.
           05 WS-DIFF-AMT            PIC S9(13)V99 COMP-3.

       01  WS-CONSTANTS.
           05 CN-YES                 PIC X VALUE "Y".
           05 CN-ZERO-AMT            PIC S9(13)V99 COMP-3 VALUE 0.
           05 CN-TX                  PIC X(12) VALUE "KZTXRF".
           05 CN-PR                  PIC X(12) VALUE "KZPRIF".
           05 CN-PM                  PIC X(12) VALUE "KZPMRF".
           05 CN-LC                  PIC X(12) VALUE "KZLCRF".
           05 CN-GL                  PIC X(12) VALUE "KZGLPF".
           05 CN-STMT                PIC X(12) VALUE "HOUTEI".
           05 CN-NATL                PIC X(12) VALUE "KOKUZEI".
           05 CN-LOC                 PIC X(12) VALUE "CHIHOUZEI".
           05 CN-GLW                 PIC X(12) VALUE "GLAZUKARI".
           05 CN-NONRES              PIC X(12) VALUE "HIKYOJYU".
           05 CN-STAT-OK             PIC X(02) VALUE "00".
           05 CN-STAT-WARN           PIC X(02) VALUE "10".
           05 CN-STAT-ERR            PIC X(02) VALUE "20".

       PROCEDURE DIVISION.
       MAIN-RTN.
           MOVE 0 TO RETURN-CODE
           PERFORM 1000-INIT
           IF NOT ABEND-ON
              PERFORM 2000-READ-TAX-ANNUAL
           END-IF
           IF NOT ABEND-ON
              PERFORM 3000-READ-STATEMENTS
           END-IF
           IF NOT ABEND-ON
              PERFORM 4000-READ-NATIONAL-PAY
           END-IF
           IF NOT ABEND-ON
              PERFORM 5000-READ-LOCAL-PAY
           END-IF
           IF NOT ABEND-ON
              PERFORM 6000-READ-GL
           END-IF
           IF NOT ABEND-ON
              PERFORM 7000-CROSS-AUDIT
           END-IF
           PERFORM 9000-FINAL
           GOBACK.

       1000-INIT.
           ACCEPT WS-RUN-DATE FROM DATE YYYYMMDD
           MOVE WS-RUN-DATE(1:4) TO WS-BASE-YEAR
           SUBTRACT 1 FROM WS-BASE-YEAR GIVING WS-TARGET-YEAR
           PERFORM VARYING WS-SUB FROM 1 BY 1 UNTIL WS-SUB > 200
              MOVE "N" TO WT-USED(WS-SUB)
              MOVE SPACES TO WT-CD(WS-SUB)
              MOVE 0 TO WT-TX-NATIONAL(WS-SUB)
                        WT-PM-NATIONAL(WS-SUB)
                        WT-STMT-CNT(WS-SUB)
           END-PERFORM
           PERFORM VARYING WS-SUB FROM 1 BY 1 UNTIL WS-SUB > 60
              MOVE "N" TO WP-USED(WS-SUB)
              MOVE SPACES TO WP-CD(WS-SUB)
              MOVE 0 TO WP-TX-LOCAL(WS-SUB)
                        WP-LC-LOCAL(WS-SUB)
                        WP-REC-CNT(WS-SUB)
           END-PERFORM
           PERFORM VARYING WS-SUB FROM 1 BY 1 UNTIL WS-SUB > 20
              MOVE "N" TO WN-USED(WS-SUB)
              MOVE SPACES TO WN-CD(WS-SUB)
              MOVE 0 TO WN-GROSS(WS-SUB)
                        WN-NATIONAL(WS-SUB)
                        WN-LOCAL(WS-SUB)
                        WN-COUNT(WS-SUB)
           END-PERFORM
           OPEN INPUT KZTXRF KZPRIF KZPMRF KZLCRF KZGLPF
           IF FS-KZTXRF NOT = "00"
              DISPLAY "KZTXRF OPEN ERROR ST=" FS-KZTXRF
              PERFORM 9100-SET-ABEND
           END-IF
           IF FS-KZPRIF NOT = "00"
              DISPLAY "KZPRIF OPEN ERROR ST=" FS-KZPRIF
              PERFORM 9100-SET-ABEND
           END-IF
           IF FS-KZPMRF NOT = "00"
              DISPLAY "KZPMRF OPEN ERROR ST=" FS-KZPMRF
              PERFORM 9100-SET-ABEND
           END-IF
           IF FS-KZLCRF NOT = "00"
              DISPLAY "KZLCRF OPEN ERROR ST=" FS-KZLCRF
              PERFORM 9100-SET-ABEND
           END-IF
           IF FS-KZGLPF NOT = "00"
              DISPLAY "KZGLPF OPEN ERROR ST=" FS-KZGLPF
              PERFORM 9100-SET-ABEND
           END-IF
           IF NOT ABEND-ON
              OPEN INPUT KZNIMF KZNRMF
              IF FS-KZNIMF NOT = "00"
                 DISPLAY "KZNIMF OPEN ERROR ST=" FS-KZNIMF
                 PERFORM 9100-SET-ABEND
              END-IF
              IF FS-KZNRMF NOT = "00"
                 DISPLAY "KZNRMF OPEN ERROR ST=" FS-KZNRMF
                 PERFORM 9100-SET-ABEND
              END-IF
           END-IF
           IF NOT ABEND-ON
              OPEN OUTPUT KZYAGF
              IF FS-KZYAGF NOT = "00"
                 DISPLAY "KZYAGF OPEN ERROR ST=" FS-KZYAGF
                 PERFORM 9100-SET-ABEND
              END-IF
           END-IF
           IF NOT ABEND-ON
              OPEN OUTPUT KZADLF
              IF FS-KZADLF NOT = "00"
                 DISPLAY "KZADLF OPEN ERROR ST=" FS-KZADLF
                 PERFORM 9100-SET-ABEND
              END-IF
           END-IF.

       2000-READ-TAX-ANNUAL.
           PERFORM 2100-READ-TX
           PERFORM UNTIL TX-EOF OR ABEND-ON
              IF WS-CUR-ACCT-NO = SPACES
                 MOVE TR-ACCT-NO TO WS-CUR-ACCT-NO
                 MOVE TR-ACCT-TYPE TO WS-CUR-ACCT-TYPE
              END-IF
              IF TR-ACCT-NO NOT = WS-CUR-ACCT-NO
                 PERFORM 2500-WRITE-YA
                 MOVE TR-ACCT-NO TO WS-CUR-ACCT-NO
                 MOVE TR-ACCT-TYPE TO WS-CUR-ACCT-TYPE
                 MOVE 0 TO WS-CUR-GROSS WS-CUR-NATIONAL
                           WS-CUR-LOCAL WS-CUR-TOTAL
                           WS-CUR-NET WS-CUR-COUNT
              END-IF
              PERFORM 2200-VALIDATE-TX
              IF NOT ABEND-ON
                 ADD TR-GROSS-INT-AMT TO WS-CUR-GROSS
                 ADD TR-NATIONAL-TAX-AMT TO WS-CUR-NATIONAL
                 ADD TR-LOCAL-TAX-AMT TO WS-CUR-LOCAL
                 ADD TR-TOTAL-TAX-AMT TO WS-CUR-TOTAL
                 ADD TR-NET-AMT TO WS-CUR-NET
                 ADD 1 TO WS-CUR-COUNT
                 ADD TR-NATIONAL-TAX-AMT TO WS-TX-NATIONAL-TOTAL
                 ADD TR-LOCAL-TAX-AMT TO WS-TX-LOCAL-TOTAL
                 ADD TR-NET-AMT TO WS-TX-NET-TOTAL
                 PERFORM 2300-ENRICH-TX
              END-IF
              PERFORM 2100-READ-TX
           END-PERFORM
           IF NOT ABEND-ON AND WS-CUR-ACCT-NO NOT = SPACES
              PERFORM 2500-WRITE-YA
           END-IF.

       2100-READ-TX.
           READ KZTXRF
              AT END
                 MOVE "Y" TO SW-TX-EOF
              NOT AT END
                 ADD 1 TO WS-TX-READ-CNT
           END-READ
           IF FS-KZTXRF NOT = "00" AND FS-KZTXRF NOT = "10"
              DISPLAY "KZTXRF READ ERROR ST=" FS-KZTXRF
              PERFORM 9100-SET-ABEND
           END-IF.

       2200-VALIDATE-TX.
           ADD TR-NATIONAL-TAX-AMT TR-LOCAL-TAX-AMT
               GIVING WS-CALC-TOTAL
           SUBTRACT TR-TOTAL-TAX-AMT FROM TR-GROSS-INT-AMT
               GIVING WS-CALC-NET
           IF TR-ACCT-TYPE NOT = "01" AND
              TR-ACCT-TYPE NOT = "02" AND
              TR-ACCT-TYPE NOT = "03"
              MOVE "AT" TO WS-STATUS-CD
              MOVE CN-TX TO WS-SOURCE-DSID
              MOVE TR-TOTAL-TAX-AMT TO WS-DEBIT-AMT
              MOVE CN-ZERO-AMT TO WS-CREDIT-AMT
              MOVE TR-TOTAL-TAX-AMT TO WS-DIFF-AMT
              PERFORM 8200-WRITE-AUDIT
           END-IF
           IF WS-CALC-TOTAL NOT = TR-TOTAL-TAX-AMT
              MOVE "TT" TO WS-STATUS-CD
              MOVE CN-TX TO WS-SOURCE-DSID
              MOVE WS-CALC-TOTAL TO WS-DEBIT-AMT
              MOVE TR-TOTAL-TAX-AMT TO WS-CREDIT-AMT
              COMPUTE WS-DIFF-AMT =
                      WS-CALC-TOTAL - TR-TOTAL-TAX-AMT
              PERFORM 8200-WRITE-AUDIT
           END-IF
           IF WS-CALC-NET NOT = TR-NET-AMT
              MOVE "NT" TO WS-STATUS-CD
              MOVE CN-TX TO WS-SOURCE-DSID
              MOVE WS-CALC-NET TO WS-DEBIT-AMT
              MOVE TR-NET-AMT TO WS-CREDIT-AMT
              COMPUTE WS-DIFF-AMT = WS-CALC-NET - TR-NET-AMT
              PERFORM 8200-WRITE-AUDIT
           END-IF.

       2300-ENRICH-TX.
           MOVE TR-ACCT-NO TO NI-ACCT-NO
           READ KZNIMF KEY IS NI-ACCT-NO
              INVALID KEY
                 CONTINUE
              NOT INVALID KEY
                 IF NI-STATUS-CD NOT = "1" AND NI-STATUS-CD NOT = "9"
                    MOVE "NI" TO WS-STATUS-CD
                    MOVE CN-TX TO WS-SOURCE-DSID
                    MOVE TR-GROSS-INT-AMT TO WS-DEBIT-AMT
                    MOVE CN-ZERO-AMT TO WS-CREDIT-AMT
                    MOVE TR-GROSS-INT-AMT TO WS-DIFF-AMT
                    PERFORM 8200-WRITE-AUDIT
                 END-IF
           END-READ
           IF FS-KZNIMF NOT = "00" AND FS-KZNIMF NOT = "23"
              DISPLAY "KZNIMF READ ERROR ST=" FS-KZNIMF
              PERFORM 9100-SET-ABEND
           END-IF
           MOVE TR-ACCT-NO TO NR-ACCT-NO
           READ KZNRMF KEY IS NR-ACCT-NO
              INVALID KEY
                 MOVE "JPN" TO WS-COUNTRY-CD
              NOT INVALID KEY
                 MOVE NR-RESIDENCE-COUNTRY-CD TO WS-COUNTRY-CD
                 IF NR-CERT-RECEIVED-FLG NOT = "1"
                    MOVE "NC" TO WS-STATUS-CD
                    MOVE CN-NONRES TO WS-SOURCE-DSID
                    MOVE TR-NATIONAL-TAX-AMT TO WS-DEBIT-AMT
                    MOVE CN-ZERO-AMT TO WS-CREDIT-AMT
                    MOVE TR-NATIONAL-TAX-AMT TO WS-DIFF-AMT
                    PERFORM 8200-WRITE-AUDIT
                 END-IF
           END-READ
           IF FS-KZNRMF NOT = "00" AND FS-KZNRMF NOT = "23"
              DISPLAY "KZNRMF READ ERROR ST=" FS-KZNRMF
              PERFORM 9100-SET-ABEND
           END-IF
           PERFORM 2320-ADD-NR.

       2320-ADD-NR.
           MOVE 0 TO WS-FOUND-IDX WS-OPEN-SUB
           PERFORM VARYING WS-SUB FROM 1 BY 1 UNTIL WS-SUB > 20
              IF WN-USED(WS-SUB) = "Y" AND
                 WN-CD(WS-SUB) = WS-COUNTRY-CD
                 MOVE WS-SUB TO WS-FOUND-IDX
                 MOVE 21 TO WS-SUB
              ELSE
                 IF WN-USED(WS-SUB) NOT = "Y" AND
                    WS-OPEN-SUB = 0
                    MOVE WS-SUB TO WS-OPEN-SUB
                 END-IF
              END-IF
           END-PERFORM
           IF WS-FOUND-IDX = 0 AND WS-OPEN-SUB > 0
              MOVE WS-OPEN-SUB TO WS-FOUND-IDX
              MOVE "Y" TO WN-USED(WS-FOUND-IDX)
              MOVE WS-COUNTRY-CD TO WN-CD(WS-FOUND-IDX)
              MOVE 0 TO WN-GROSS(WS-FOUND-IDX)
                        WN-NATIONAL(WS-FOUND-IDX)
                        WN-LOCAL(WS-FOUND-IDX)
                        WN-COUNT(WS-FOUND-IDX)
           END-IF
           IF WS-FOUND-IDX = 0
              MOVE "NR" TO WS-STATUS-CD
              MOVE CN-NONRES TO WS-SOURCE-DSID
              MOVE TR-GROSS-INT-AMT TO WS-DEBIT-AMT
              MOVE CN-ZERO-AMT TO WS-CREDIT-AMT
              MOVE TR-GROSS-INT-AMT TO WS-DIFF-AMT
              PERFORM 8200-WRITE-AUDIT
           ELSE
              ADD TR-GROSS-INT-AMT TO WN-GROSS(WS-FOUND-IDX)
              ADD TR-NATIONAL-TAX-AMT TO WN-NATIONAL(WS-FOUND-IDX)
              ADD TR-LOCAL-TAX-AMT TO WN-LOCAL(WS-FOUND-IDX)
              ADD 1 TO WN-COUNT(WS-FOUND-IDX)
           END-IF.

       2500-WRITE-YA.
           MOVE WS-CUR-ACCT-NO TO YA-ACCT-NO
           MOVE WS-TARGET-YEAR TO YA-YEAR
           MOVE WS-CUR-GROSS TO YA-TOTAL-GROSS-INT-AMT
           MOVE WS-CUR-NATIONAL TO YA-TOTAL-NATIONAL-TAX-AMT
           MOVE WS-CUR-LOCAL TO YA-TOTAL-LOCAL-TAX-AMT
           MOVE WS-CUR-NET TO YA-TOTAL-NET-AMT
           MOVE WS-CUR-COUNT TO YA-PAYMENT-COUNT
           WRITE KZYAGF-REC
           IF FS-KZYAGF NOT = "00"
              DISPLAY "KZYAGF WRITE ERROR ST=" FS-KZYAGF
              PERFORM 9100-SET-ABEND
           ELSE
              ADD 1 TO WS-YA-WRITE-CNT
           END-IF.

       3000-READ-STATEMENTS.
           PERFORM 3100-READ-PR
           PERFORM UNTIL PR-EOF OR ABEND-ON
              MOVE PR-PAYMENT-DATE(1:4) TO WS-PAY-YEAR
              IF PR-STATEMENT-CLASS = "C"
                 SUBTRACT 1 FROM WS-PAY-YEAR GIVING WS-ORIG-YEAR
              ELSE
                 MOVE WS-PAY-YEAR TO WS-ORIG-YEAR
              END-IF
              IF WS-ORIG-YEAR = WS-TARGET-YEAR
                 ADD PR-NATIONAL-TAX-AMT TO WS-PR-NATIONAL-TOTAL
                 ADD PR-LOCAL-TAX-AMT TO WS-PR-LOCAL-TOTAL
                 MOVE PR-PAYEE-ADDR-CD(1:2) TO WS-ADDR-PREF
                 MOVE PR-PAYEE-ADDR-CD(1:6) TO WS-TAXOFFICE-CD
                 PERFORM 3300-ADD-TAXOFFICE-STMT
                 PERFORM 3400-ADD-PREF-STMT
              END-IF
              PERFORM 3100-READ-PR
           END-PERFORM.

       3100-READ-PR.
           READ KZPRIF
              AT END
                 MOVE "Y" TO SW-PR-EOF
              NOT AT END
                 ADD 1 TO WS-PR-READ-CNT
           END-READ
           IF FS-KZPRIF NOT = "00" AND FS-KZPRIF NOT = "10"
              DISPLAY "KZPRIF READ ERROR ST=" FS-KZPRIF
              PERFORM 9100-SET-ABEND
           END-IF.

       3300-ADD-TAXOFFICE-STMT.
           PERFORM 3310-FIND-TAXOFFICE
           IF WS-FOUND-IDX > 0
              ADD PR-NATIONAL-TAX-AMT TO WT-TX-NATIONAL(WS-FOUND-IDX)
              ADD 1 TO WT-STMT-CNT(WS-FOUND-IDX)
           ELSE
              MOVE "OF" TO WS-STATUS-CD
              MOVE CN-STMT TO WS-SOURCE-DSID
              MOVE PR-NATIONAL-TAX-AMT TO WS-DEBIT-AMT
              MOVE CN-ZERO-AMT TO WS-CREDIT-AMT
              MOVE PR-NATIONAL-TAX-AMT TO WS-DIFF-AMT
              PERFORM 8200-WRITE-AUDIT
           END-IF.

       3310-FIND-TAXOFFICE.
           MOVE 0 TO WS-FOUND-IDX WS-OPEN-SUB
           PERFORM VARYING WS-SUB FROM 1 BY 1 UNTIL WS-SUB > 200
              IF WT-USED(WS-SUB) = "Y" AND
                 WT-CD(WS-SUB) = WS-TAXOFFICE-CD
                 MOVE WS-SUB TO WS-FOUND-IDX
                 MOVE 201 TO WS-SUB
              ELSE
                 IF WT-USED(WS-SUB) NOT = "Y" AND
                    WS-OPEN-SUB = 0
                    MOVE WS-SUB TO WS-OPEN-SUB
                 END-IF
              END-IF
           END-PERFORM
           IF WS-FOUND-IDX = 0 AND WS-OPEN-SUB > 0
              MOVE WS-OPEN-SUB TO WS-FOUND-IDX
              MOVE "Y" TO WT-USED(WS-FOUND-IDX)
              MOVE WS-TAXOFFICE-CD TO WT-CD(WS-FOUND-IDX)
              MOVE 0 TO WT-TX-NATIONAL(WS-FOUND-IDX)
                        WT-PM-NATIONAL(WS-FOUND-IDX)
                        WT-STMT-CNT(WS-FOUND-IDX)
           END-IF.

       3400-ADD-PREF-STMT.
           PERFORM 3410-FIND-PREF
           IF WS-FOUND-IDX > 0
              ADD PR-LOCAL-TAX-AMT TO WP-TX-LOCAL(WS-FOUND-IDX)
              ADD 1 TO WP-REC-CNT(WS-FOUND-IDX)
           ELSE
              MOVE "PF" TO WS-STATUS-CD
              MOVE CN-STMT TO WS-SOURCE-DSID
              MOVE PR-LOCAL-TAX-AMT TO WS-DEBIT-AMT
              MOVE CN-ZERO-AMT TO WS-CREDIT-AMT
              MOVE PR-LOCAL-TAX-AMT TO WS-DIFF-AMT
              PERFORM 8200-WRITE-AUDIT
           END-IF.

       3410-FIND-PREF.
           MOVE 0 TO WS-FOUND-IDX WS-OPEN-SUB
           PERFORM VARYING WS-SUB FROM 1 BY 1 UNTIL WS-SUB > 60
              IF WP-USED(WS-SUB) = "Y" AND
                 WP-CD(WS-SUB) = WS-ADDR-PREF
                 MOVE WS-SUB TO WS-FOUND-IDX
                 MOVE 61 TO WS-SUB
              ELSE
                 IF WP-USED(WS-SUB) NOT = "Y" AND
                    WS-OPEN-SUB = 0
                    MOVE WS-SUB TO WS-OPEN-SUB
                 END-IF
              END-IF
           END-PERFORM
           IF WS-FOUND-IDX = 0 AND WS-OPEN-SUB > 0
              MOVE WS-OPEN-SUB TO WS-FOUND-IDX
              MOVE "Y" TO WP-USED(WS-FOUND-IDX)
              MOVE WS-ADDR-PREF TO WP-CD(WS-FOUND-IDX)
              MOVE 0 TO WP-TX-LOCAL(WS-FOUND-IDX)
                        WP-LC-LOCAL(WS-FOUND-IDX)
                        WP-REC-CNT(WS-FOUND-IDX)
           END-IF.

       4000-READ-NATIONAL-PAY.
           PERFORM 4100-READ-PM
           PERFORM UNTIL PM-EOF OR ABEND-ON
              MOVE PM-PAYMENT-DATE(1:4) TO WS-PAY-YEAR
              IF WS-PAY-YEAR = WS-BASE-YEAR
                 ADD PM-NATIONAL-TAX-AMT TO WS-PM-NATIONAL-TOTAL
                 ADD PM-SPECIAL-RECON-AMT TO WS-PM-NATIONAL-TOTAL
                 MOVE PM-TAX-OFFICE-CD TO WS-TAXOFFICE-CD
                 PERFORM 4200-ADD-TAXOFFICE-PAY
              END-IF
              PERFORM 4100-READ-PM
           END-PERFORM.

       4100-READ-PM.
           READ KZPMRF
              AT END
                 MOVE "Y" TO SW-PM-EOF
              NOT AT END
                 ADD 1 TO WS-PM-READ-CNT
           END-READ
           IF FS-KZPMRF NOT = "00" AND FS-KZPMRF NOT = "10"
              DISPLAY "KZPMRF READ ERROR ST=" FS-KZPMRF
              PERFORM 9100-SET-ABEND
           END-IF.

       4200-ADD-TAXOFFICE-PAY.
           PERFORM 3310-FIND-TAXOFFICE
           IF WS-FOUND-IDX > 0
              ADD PM-NATIONAL-TAX-AMT TO WT-PM-NATIONAL(WS-FOUND-IDX)
              ADD PM-SPECIAL-RECON-AMT
                  TO WT-PM-NATIONAL(WS-FOUND-IDX)
           ELSE
              MOVE "PM" TO WS-STATUS-CD
              MOVE CN-PM TO WS-SOURCE-DSID
              MOVE PM-NATIONAL-TAX-AMT TO WS-DEBIT-AMT
              MOVE CN-ZERO-AMT TO WS-CREDIT-AMT
              MOVE PM-NATIONAL-TAX-AMT TO WS-DIFF-AMT
              PERFORM 8200-WRITE-AUDIT
           END-IF.

       5000-READ-LOCAL-PAY.
           PERFORM 5100-READ-LC
           PERFORM UNTIL LC-EOF OR ABEND-ON
              MOVE LC-REMITTANCE-DATE(1:4) TO WS-PAY-YEAR
              IF WS-PAY-YEAR = WS-BASE-YEAR
                 ADD LC-LOCAL-TAX-AMT TO WS-LC-LOCAL-TOTAL
                 MOVE LC-PREFECTURE-CD TO WS-ADDR-PREF
                 PERFORM 5200-ADD-PREF-PAY
              END-IF
              PERFORM 5100-READ-LC
           END-PERFORM.

       5100-READ-LC.
           READ KZLCRF
              AT END
                 MOVE "Y" TO SW-LC-EOF
              NOT AT END
                 ADD 1 TO WS-LC-READ-CNT
           END-READ
           IF FS-KZLCRF NOT = "00" AND FS-KZLCRF NOT = "10"
              DISPLAY "KZLCRF READ ERROR ST=" FS-KZLCRF
              PERFORM 9100-SET-ABEND
           END-IF.

       5200-ADD-PREF-PAY.
           PERFORM 3410-FIND-PREF
           IF WS-FOUND-IDX > 0
              ADD LC-LOCAL-TAX-AMT TO WP-LC-LOCAL(WS-FOUND-IDX)
           ELSE
              MOVE "LC" TO WS-STATUS-CD
              MOVE CN-LC TO WS-SOURCE-DSID
              MOVE LC-LOCAL-TAX-AMT TO WS-DEBIT-AMT
              MOVE CN-ZERO-AMT TO WS-CREDIT-AMT
              MOVE LC-LOCAL-TAX-AMT TO WS-DIFF-AMT
              PERFORM 8200-WRITE-AUDIT
           END-IF.

       6000-READ-GL.
           PERFORM 6100-READ-GP
           PERFORM UNTIL GP-EOF OR ABEND-ON
              MOVE GP-POSTING-DATE(1:4) TO WS-PAY-YEAR
              IF WS-PAY-YEAR = WS-TARGET-YEAR
                 ADD GP-WITHHOLDING-LIAB-AMT TO WS-GP-WH-TOTAL
                 ADD GP-LOCAL-LIAB-AMT TO WS-GP-LOCAL-TOTAL
                 ADD GP-NET-PAYABLE-AMT TO WS-GP-NET-TOTAL
              END-IF
              PERFORM 6100-READ-GP
           END-PERFORM.

       6100-READ-GP.
           READ KZGLPF
              AT END
                 MOVE "Y" TO SW-GP-EOF
              NOT AT END
                 ADD 1 TO WS-GP-READ-CNT
           END-READ
           IF FS-KZGLPF NOT = "00" AND FS-KZGLPF NOT = "10"
              DISPLAY "KZGLPF READ ERROR ST=" FS-KZGLPF
              PERFORM 9100-SET-ABEND
           END-IF.

       7000-CROSS-AUDIT.
           MOVE CN-STMT TO WS-SOURCE-DSID
           MOVE WS-TX-NATIONAL-TOTAL TO WS-DEBIT-AMT
           MOVE WS-PR-NATIONAL-TOTAL TO WS-CREDIT-AMT
           PERFORM 8100-CALL-KZ902S
           MOVE CN-STMT TO WS-SOURCE-DSID
           MOVE WS-TX-LOCAL-TOTAL TO WS-DEBIT-AMT
           MOVE WS-PR-LOCAL-TOTAL TO WS-CREDIT-AMT
           PERFORM 8100-CALL-KZ902S
           MOVE CN-NATL TO WS-SOURCE-DSID
           MOVE WS-TX-NATIONAL-TOTAL TO WS-DEBIT-AMT
           MOVE WS-PM-NATIONAL-TOTAL TO WS-CREDIT-AMT
           PERFORM 8100-CALL-KZ902S
           MOVE CN-LOC TO WS-SOURCE-DSID
           MOVE WS-TX-LOCAL-TOTAL TO WS-DEBIT-AMT
           MOVE WS-LC-LOCAL-TOTAL TO WS-CREDIT-AMT
           PERFORM 8100-CALL-KZ902S
           MOVE CN-GLW TO WS-SOURCE-DSID
           MOVE WS-TX-NATIONAL-TOTAL TO WS-DEBIT-AMT
           MOVE WS-GP-WH-TOTAL TO WS-CREDIT-AMT
           PERFORM 8100-CALL-KZ902S
           MOVE CN-GL TO WS-SOURCE-DSID
           MOVE WS-TX-LOCAL-TOTAL TO WS-DEBIT-AMT
           MOVE WS-GP-LOCAL-TOTAL TO WS-CREDIT-AMT
           PERFORM 8100-CALL-KZ902S
           MOVE CN-GL TO WS-SOURCE-DSID
           MOVE WS-TX-NET-TOTAL TO WS-DEBIT-AMT
           MOVE WS-GP-NET-TOTAL TO WS-CREDIT-AMT
           PERFORM 8100-CALL-KZ902S
           PERFORM VARYING WS-SUB FROM 1 BY 1 UNTIL WS-SUB > 200
              IF WT-USED(WS-SUB) = "Y"
                 MOVE CN-NATL TO WS-SOURCE-DSID
                 MOVE WT-TX-NATIONAL(WS-SUB) TO WS-DEBIT-AMT
                 MOVE WT-PM-NATIONAL(WS-SUB) TO WS-CREDIT-AMT
                 PERFORM 8100-CALL-KZ902S
              END-IF
           END-PERFORM
           PERFORM VARYING WS-SUB FROM 1 BY 1 UNTIL WS-SUB > 60
              IF WP-USED(WS-SUB) = "Y"
                 MOVE CN-LOC TO WS-SOURCE-DSID
                 MOVE WP-TX-LOCAL(WS-SUB) TO WS-DEBIT-AMT
                 MOVE WP-LC-LOCAL(WS-SUB) TO WS-CREDIT-AMT
                 PERFORM 8100-CALL-KZ902S
              END-IF
           END-PERFORM.

       8100-CALL-KZ902S.
           ADD 1 TO WS-AUDIT-SEQ
           MOVE "KZ889B" TO WS-AUDIT-PREFIX
           MOVE WS-AUDIT-SEQ TO WS-AUDIT-SUFFIX
           STRING WS-AUDIT-PREFIX WS-AUDIT-SUFFIX-X
              DELIMITED BY SIZE INTO LK-AUDIT-ID
           END-STRING
           MOVE WS-SOURCE-DSID TO LK-SOURCE-DSID
           MOVE WS-DEBIT-AMT TO LK-DEBIT-AMT
           MOVE WS-CREDIT-AMT TO LK-CREDIT-AMT
           MOVE 0 TO LK-RTN-CD
           MOVE SPACES TO LK-CAUSE-CD
           MOVE 0 TO LK-DIFF-AMT
           CALL "KZ902S" USING WS-KZ902S-LINKAGE
           IF LK-RTN-CD NOT = 0 OR LK-DIFF-AMT NOT = 0
              MOVE LK-CAUSE-CD TO WS-STATUS-CD
              MOVE LK-DIFF-AMT TO WS-DIFF-AMT
              PERFORM 8200-WRITE-AUDIT
           END-IF.

       8200-WRITE-AUDIT.
           ADD 1 TO WS-AUDIT-SEQ
           MOVE "KZ889B" TO WS-AUDIT-PREFIX
           MOVE WS-AUDIT-SEQ TO WS-AUDIT-SUFFIX
           STRING WS-AUDIT-PREFIX WS-AUDIT-SUFFIX-X
              DELIMITED BY SIZE INTO WS-AUDIT-ID
           END-STRING
           MOVE WS-AUDIT-ID TO AL-AUDIT-ID
           MOVE WS-RUN-DATE TO AL-RUN-DATE
           MOVE WS-SOURCE-DSID TO AL-SOURCE-DSID
           MOVE WS-DEBIT-AMT TO AL-DEBIT-AMT
           MOVE WS-CREDIT-AMT TO AL-CREDIT-AMT
           MOVE WS-DIFF-AMT TO AL-DIFF-AMT
           MOVE WS-STATUS-CD TO AL-STATUS-CD
           WRITE KZADLF-REC
           IF FS-KZADLF NOT = "00"
              DISPLAY "KZADLF WRITE ERROR ST=" FS-KZADLF
              PERFORM 9100-SET-ABEND
           ELSE
              ADD 1 TO WS-AL-WRITE-CNT
           END-IF.

       9000-FINAL.
           IF FS-KZTXRF = "00" OR FS-KZTXRF = "10"
              CLOSE KZTXRF
           END-IF
           IF FS-KZPRIF = "00" OR FS-KZPRIF = "10"
              CLOSE KZPRIF
           END-IF
           IF FS-KZPMRF = "00" OR FS-KZPMRF = "10"
              CLOSE KZPMRF
           END-IF
           IF FS-KZLCRF = "00" OR FS-KZLCRF = "10"
              CLOSE KZLCRF
           END-IF
           IF FS-KZGLPF = "00" OR FS-KZGLPF = "10"
              CLOSE KZGLPF
           END-IF
           IF FS-KZNIMF = "00" OR FS-KZNIMF = "23"
              CLOSE KZNIMF
           END-IF
           IF FS-KZNRMF = "00" OR FS-KZNRMF = "23"
              CLOSE KZNRMF
           END-IF
           IF FS-KZYAGF = "00"
              CLOSE KZYAGF
           END-IF
           IF FS-KZADLF = "00"
              CLOSE KZADLF
           END-IF
           IF ABEND-ON
              MOVE 8 TO RETURN-CODE
              DISPLAY "KZ889B ABEND RC=8"
           ELSE
              MOVE 0 TO RETURN-CODE
              DISPLAY "KZ889B NORMAL END YA CNT="
              DISPLAY WS-YA-WRITE-CNT
              DISPLAY "KZ889B AUDIT CNT="
              DISPLAY WS-AL-WRITE-CNT
           END-IF.

       9100-SET-ABEND.
           MOVE "Y" TO SW-ABEND.
