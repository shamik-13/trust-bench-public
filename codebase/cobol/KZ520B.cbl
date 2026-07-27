       IDENTIFICATION DIVISION.
       PROGRAM-ID. KZ520B.
      *
      *変更履歴
      * 版数  年月日    担当  概要
      * 1.00  20240115  KZB   初版作成
      * 1.01  20240520  KZB   法的措置先除外を追加
      * 1.02  20240930  KZB   督促状抑止件数を集計
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZDLRF ASSIGN TO "KZDLRF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-DLR-STATUS.
           SELECT KZDNNF ASSIGN TO "KZDNNF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS DNN-ACCT-NO
               FILE STATUS IS WS-DNN-STATUS.
           SELECT SYSOUT ASSIGN TO "SYSOUT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-SYS-STATUS.
           SELECT SORTWK ASSIGN TO SORTWK.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  KZDLRF.
           COPY KZDLRFC.
      *
       FD  KZDNNF.
           COPY KZDNNCF.
      *
       FD  SYSOUT.
       01  SYSOUT-REC                         PIC X(132).
      *
       SD  SORTWK.
       01  SORT-REC.
           05  S-BR-CD                        PIC X(03).
           05  S-ACCT-NO                      PIC X(12).
           05  S-LINE                         PIC X(132).
      *
       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05  WS-DLR-STATUS                  PIC X(02) VALUE SPACE.
           05  WS-DNN-STATUS                  PIC X(02) VALUE SPACE.
           05  WS-SYS-STATUS                  PIC X(02) VALUE SPACE.
      *
       01  WS-SWITCHES.
           05  WS-DLR-EOF                     PIC X VALUE "N".
               88  DLR-EOF                         VALUE "Y".
               88  DLR-NOT-EOF                     VALUE "N".
           05  WS-HARD-ERROR                  PIC X VALUE "N".
               88  HARD-ERROR                      VALUE "Y".
      *
       01  WS-CONSTANTS.
           05  WC-STOP-YES                    PIC X VALUE "Y".
           05  WC-LEGAL-YES                   PIC X VALUE "Y".
           05  WC-SUPPRESS-YES                PIC X VALUE "Y".
           05  WC-STATUS-COLLECT              PIC X(02) VALUE "30".
           05  WC-BUCKET-B1                   PIC X(02) VALUE "B1".
           05  WC-BUCKET-B2                   PIC X(02) VALUE "B2".
           05  WC-BUCKET-B3                   PIC X(02) VALUE "B3".
           05  WC-BUCKET-B4                   PIC X(02) VALUE "B4".
      *
       01  WS-COUNTERS.
           05  WS-TOTAL-READ                  PIC 9(09) VALUE ZERO.
           05  WS-NOTICE-ISSUED               PIC 9(09) VALUE ZERO.
           05  WS-SUPPRESSED                  PIC 9(09) VALUE ZERO.
           05  WS-SKIPPED-LEGAL               PIC 9(09) VALUE ZERO.
           05  WS-SKIPPED-STOP                PIC 9(09) VALUE ZERO.
           05  WS-SKIPPED-NOTFND              PIC 9(09) VALUE ZERO.
      *
       01  WS-DATE-AREA.
           05  WS-CURRENT-DATE                PIC 9(08).
           05  WS-DATE-YYYYMMDD               PIC X(08).
      *
       01  WS-EDIT-AREA.
           05  WE-COUNT                       PIC ZZZ,ZZZ,ZZ9.
           05  WE-AMOUNT                      PIC ZZZ,ZZZ,ZZ9.
           05  WE-DAYS                        PIC ZZ9.
      *
       01  KZ521S-PARM.
           05  KZ521S-AGING-BUCKET            PIC X(02).
           05  KZ521S-NOTICE-COUNT            PIC 9(03).
           05  KZ521S-TEMPLATE-CODE           PIC X(04).
           05  KZ521S-SUPPRESS-IND            PIC X.
      *
       01  WS-LINE-AREA.
           05  WL-DATE                        PIC X(08).
           05  FILLER                         PIC X VALUE SPACE.
           05  WL-BRANCH                      PIC X(03).
           05  FILLER                         PIC X VALUE SPACE.
           05  WL-ACCOUNT                     PIC X(12).
           05  FILLER                         PIC X VALUE SPACE.
           05  WL-BUCKET                      PIC X(02).
           05  FILLER                         PIC X VALUE SPACE.
           05  WL-DAYS                        PIC X(03).
           05  FILLER                         PIC X VALUE SPACE.
           05  WL-TEMPLATE                    PIC X(04).
           05  FILLER                         PIC X VALUE SPACE.
           05  WL-LATE-CHARGE                 PIC X(11).
           05  FILLER                         PIC X(84) VALUE SPACES.
      *
       01  WS-TRAILER.
           05  WT-LABEL                       PIC X(24).
           05  WT-NOTICE-LABEL                PIC X(18).
           05  WT-NOTICE-COUNT                PIC ZZZ,ZZZ,ZZ9.
           05  WT-SUPP-LABEL                  PIC X(14).
           05  WT-SUPP-COUNT                  PIC ZZZ,ZZZ,ZZ9.
           05  WT-LEGAL-LABEL                 PIC X(18).
           05  WT-LEGAL-COUNT                 PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                         PIC X(35) VALUE SPACES.
      *
       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE 0 TO RETURN-CODE
           ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD
           MOVE WS-CURRENT-DATE TO WS-DATE-YYYYMMDD
           PERFORM 1000-OPEN-FILES
           IF NOT HARD-ERROR
               SORT SORTWK
                   ON ASCENDING KEY S-BR-CD S-ACCT-NO
                   INPUT PROCEDURE 3000-MAKE-NOTICES
                   OUTPUT PROCEDURE 4000-WRITE-SYSOUT
               IF SORT-RETURN NOT = ZERO
                   DISPLAY "督促状ソート失敗 RC=" SORT-RETURN
                   MOVE 8 TO RETURN-CODE
               END-IF
           END-IF
           PERFORM 9000-CLOSE-FILES
           GOBACK.
      *
       1000-OPEN-FILES.
           OPEN INPUT KZDLRF
           IF WS-DLR-STATUS NOT = "00"
               DISPLAY "KZDLRF オープン失敗 ST=" WS-DLR-STATUS
               SET HARD-ERROR TO TRUE
               MOVE 8 TO RETURN-CODE
           END-IF
           IF NOT HARD-ERROR
               OPEN I-O KZDNNF
               IF WS-DNN-STATUS NOT = "00"
                   DISPLAY "KZDNNF オープン失敗 ST=" WS-DNN-STATUS
                   SET HARD-ERROR TO TRUE
                   MOVE 8 TO RETURN-CODE
               END-IF
           END-IF
           IF NOT HARD-ERROR
               OPEN OUTPUT SYSOUT
               IF WS-SYS-STATUS NOT = "00"
                   DISPLAY "SYSOUT オープン失敗 ST=" WS-SYS-STATUS
                   SET HARD-ERROR TO TRUE
                   MOVE 8 TO RETURN-CODE
               END-IF
           END-IF.
      *
       3000-MAKE-NOTICES.
           PERFORM 3100-READ-DLR
           PERFORM UNTIL DLR-EOF OR HARD-ERROR
               ADD 1 TO WS-TOTAL-READ
               PERFORM 3200-PROCESS-DLR
               PERFORM 3100-READ-DLR
           END-PERFORM.
      *
       3100-READ-DLR.
           READ KZDLRF
               AT END
                   SET DLR-EOF TO TRUE
               NOT AT END
                   CONTINUE
           END-READ
           IF WS-DLR-STATUS NOT = "00" AND
              WS-DLR-STATUS NOT = "10"
               DISPLAY "KZDLRF 読込失敗 ST=" WS-DLR-STATUS
               SET HARD-ERROR TO TRUE
               MOVE 8 TO RETURN-CODE
           END-IF.
      *
       3200-PROCESS-DLR.
           IF DR-DAYS-OVERDUE <= ZERO
               EXIT PARAGRAPH
           END-IF
           IF DR-AGING-BUCKET NOT = WC-BUCKET-B1 AND
              DR-AGING-BUCKET NOT = WC-BUCKET-B2 AND
              DR-AGING-BUCKET NOT = WC-BUCKET-B3 AND
              DR-AGING-BUCKET NOT = WC-BUCKET-B4
               DISPLAY "経過区分不正 口座=" DR-ACCT-NO
               SET HARD-ERROR TO TRUE
               MOVE 8 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF
           MOVE DR-ACCT-NO TO DNN-ACCT-NO
           READ KZDNNF KEY IS DNN-ACCT-NO
               INVALID KEY
                   ADD 1 TO WS-SKIPPED-NOTFND
                   DISPLAY "督促管理なし 口座=" DR-ACCT-NO
               NOT INVALID KEY
                   PERFORM 3300-CHECK-AND-ISSUE
           END-READ
           IF WS-DNN-STATUS NOT = "00" AND
              WS-DNN-STATUS NOT = "23"
               DISPLAY "KZDNNF 読込失敗 ST=" WS-DNN-STATUS
               SET HARD-ERROR TO TRUE
               MOVE 8 TO RETURN-CODE
           END-IF.
      *
       3300-CHECK-AND-ISSUE.
           IF DNN-STOP-FLAG = WC-STOP-YES
               ADD 1 TO WS-SKIPPED-STOP
               EXIT PARAGRAPH
           END-IF
           IF DNN-LEGAL-FLAG = WC-LEGAL-YES
               ADD 1 TO WS-SKIPPED-LEGAL
               EXIT PARAGRAPH
           END-IF
           ADD 1 TO DNN-NOTICE-COUNT
           MOVE WS-DATE-YYYYMMDD TO DNN-NOTICE-DATE
           MOVE DR-AGING-BUCKET TO KZ521S-AGING-BUCKET
           MOVE DNN-NOTICE-COUNT TO KZ521S-NOTICE-COUNT
           MOVE SPACES TO KZ521S-TEMPLATE-CODE
           MOVE SPACE TO KZ521S-SUPPRESS-IND
           CALL "KZ521S" USING KZ521S-PARM
           IF KZ521S-TEMPLATE-CODE = SPACES
               DISPLAY "テンプレート未設定 口座=" DR-ACCT-NO
               SET HARD-ERROR TO TRUE
               MOVE 8 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF
           MOVE KZ521S-TEMPLATE-CODE TO DNN-TEMPLATE-CODE
           MOVE KZ521S-TEMPLATE-CODE TO DNN-NOTICE-TYPE
           IF DR-NEW-STATUS = WC-STATUS-COLLECT
               MOVE WC-STATUS-COLLECT TO DNN-NOTICE-TYPE
           END-IF
           REWRITE KZDNNF-REC
           IF WS-DNN-STATUS NOT = "00"
               DISPLAY "KZDNNF 更新失敗 ST=" WS-DNN-STATUS
               SET HARD-ERROR TO TRUE
               MOVE 8 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF
           IF KZ521S-SUPPRESS-IND = WC-SUPPRESS-YES
               ADD 1 TO WS-SUPPRESSED
           ELSE
               PERFORM 3400-RELEASE-NOTICE
               ADD 1 TO WS-NOTICE-ISSUED
           END-IF.
      *
       3400-RELEASE-NOTICE.
           MOVE SPACES TO WS-LINE-AREA
           MOVE WS-DATE-YYYYMMDD TO WL-DATE
           MOVE DR-ACCT-NO(1:3) TO WL-BRANCH
           MOVE DR-ACCT-NO TO WL-ACCOUNT
           MOVE DR-AGING-BUCKET TO WL-BUCKET
           MOVE DR-DAYS-OVERDUE TO WE-DAYS
           MOVE WE-DAYS TO WL-DAYS
           MOVE DR-LATE-CHARGE-AMT TO WE-AMOUNT
           MOVE WE-AMOUNT TO WL-LATE-CHARGE
           MOVE KZ521S-TEMPLATE-CODE TO WL-TEMPLATE
           MOVE DR-ACCT-NO(1:3) TO S-BR-CD
           MOVE DR-ACCT-NO TO S-ACCT-NO
           MOVE WS-LINE-AREA TO S-LINE
           RELEASE SORT-REC.
      *
       4000-WRITE-SYSOUT.
           RETURN SORTWK
               AT END
                   PERFORM 4100-WRITE-TRAILER
                   EXIT PARAGRAPH
           END-RETURN
           PERFORM UNTIL HARD-ERROR
               MOVE S-LINE TO SYSOUT-REC
               WRITE SYSOUT-REC
               IF WS-SYS-STATUS NOT = "00"
                   DISPLAY "SYSOUT 書込失敗 ST=" WS-SYS-STATUS
                   SET HARD-ERROR TO TRUE
                   MOVE 8 TO RETURN-CODE
               END-IF
               RETURN SORTWK
                   AT END
                       PERFORM 4100-WRITE-TRAILER
                       EXIT PERFORM
               END-RETURN
           END-PERFORM.
      *
       4100-WRITE-TRAILER.
           MOVE SPACES TO WS-TRAILER
           MOVE "督促状生成集計" TO WT-LABEL
           MOVE "発行件数=" TO WT-NOTICE-LABEL
           MOVE WS-NOTICE-ISSUED TO WT-NOTICE-COUNT
           MOVE "抑止件数=" TO WT-SUPP-LABEL
           MOVE WS-SUPPRESSED TO WT-SUPP-COUNT
           MOVE "法的除外件数=" TO WT-LEGAL-LABEL
           MOVE WS-SKIPPED-LEGAL TO WT-LEGAL-COUNT
           MOVE WS-TRAILER TO SYSOUT-REC
           WRITE SYSOUT-REC
           IF WS-SYS-STATUS NOT = "00"
               DISPLAY "SYSOUT 集計書込失敗 ST=" WS-SYS-STATUS
               SET HARD-ERROR TO TRUE
               MOVE 8 TO RETURN-CODE
           END-IF.
      *
       9000-CLOSE-FILES.
           IF WS-DLR-STATUS NOT = SPACES
               CLOSE KZDLRF
           END-IF
           IF WS-DNN-STATUS NOT = SPACES
               CLOSE KZDNNF
           END-IF
           IF WS-SYS-STATUS NOT = SPACES
               CLOSE SYSOUT
           END-IF
           IF HARD-ERROR
               MOVE 8 TO RETURN-CODE
           ELSE
               IF RETURN-CODE NOT = 8 AND RETURN-CODE NOT = 12
                   MOVE 0 TO RETURN-CODE
               END-IF
           END-IF.
