       IDENTIFICATION DIVISION.
       PROGRAM-ID. JH790B.
      * 変更履歴
      * 版数 年月日(和暦) 担当                         概要
      * 1.00 R03.04.01 システム部 情報系チーム       新規作成
      * 1.01 R04.10.15 システム部 情報系チーム       保存期限判定見直し
      * 1.02 R05.12.08 システム部 情報系チーム       パージ対象抽出条件追加
      ******************************************************************
      *  JH790B 情報系保存期限管理パージ
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT JHCTLKF
               ASSIGN TO "JHCTLKF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CT-JOB-ID
               FILE STATUS IS WS-CTL-ST.

           SELECT JHCHGEF
               ASSIGN TO "JHCHGEF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-CHG-ST.

           SELECT JHAUDTF
               ASSIGN TO "JHAUDTF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-AUD-ST.

       DATA DIVISION.
       FILE SECTION.

       FD  JHCTLKF.
           COPY JHCTLC.

       FD  JHCHGEF.
           COPY JHCHGC.

       FD  JHAUDTF.
           COPY JHAUDC.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05 WS-CTL-ST             PIC XX.
           05 WS-CHG-ST             PIC XX.
           05 WS-AUD-ST             PIC XX.

       01  WS-SWITCHES.
           05 WS-CTL-EOF            PIC X VALUE "N".
              88 CTL-EOF                  VALUE "Y".
           05 WS-CHG-EOF            PIC X VALUE "N".
              88 CHG-EOF                  VALUE "Y".
           05 WS-AUD-EOF            PIC X VALUE "N".
              88 AUD-EOF                  VALUE "Y".
           05 WS-HARD-ERR           PIC X VALUE "N".
              88 HARD-ERR                 VALUE "Y".
              88 NO-HARD-ERR              VALUE "N".

       01  WS-DATE-AREA.
           05 WS-CURR-DATE          PIC 9(8).
           05 WS-BIZ-DATE           PIC 9(8).
           05 WS-CTL-LIMIT-DATE     PIC 9(8).
           05 WS-CHG-LIMIT-DATE     PIC 9(8).
           05 WS-AUD-LIMIT-DATE     PIC 9(8).
           05 WS-DATE-INT           PIC S9(9) COMP-5.

       01  WS-CONSTANTS.
           05 WS-JOB-ID             PIC X(8) VALUE "JH790B".
           05 WS-CTL-KEEP-DAYS      PIC S9(9) COMP-5 VALUE 1095.
           05 WS-CHG-KEEP-DAYS      PIC S9(9) COMP-5 VALUE 2555.
           05 WS-AUD-KEEP-DAYS      PIC S9(9) COMP-5 VALUE 2555.
           05 WS-AUD-DATASET        PIC X(8) VALUE "ALLHIST".
           05 WS-AUD-EVENT          PIC X(8) VALUE "PURGESEL".

       01  WS-COUNTERS.
           05 WS-CTL-READ-CNT       PIC 9(11) VALUE 0.
           05 WS-CTL-DEL-CNT        PIC 9(11) VALUE 0.
           05 WS-CTL-SKIP-CNT       PIC 9(11) VALUE 0.
           05 WS-CHG-READ-CNT       PIC 9(11) VALUE 0.
           05 WS-CHG-PURGE-CNT      PIC 9(11) VALUE 0.
           05 WS-AUD-READ-CNT       PIC 9(11) VALUE 0.
           05 WS-AUD-PURGE-CNT      PIC 9(11) VALUE 0.
           05 WS-TOTAL-PURGE-CNT    PIC 9(11) VALUE 0.

       01  WS-WORK-KEYS.
           05 WS-LAST-KEEP-DATE     PIC 9(8) VALUE 0.
           05 WS-MAX-AUDIT-SEQ      PIC 9(18) VALUE 0.
           05 WS-NEXT-AUDIT-SEQ     PIC 9(18) VALUE 0.

       01  WS-NUMERIC-EDITS.
           05 WS-DISP-CNT           PIC ZZZ,ZZZ,ZZZ,ZZ9.

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE 0 TO RETURN-CODE
           PERFORM 1000-INITIALIZE

           IF NO-HARD-ERR
               PERFORM 2000-SCAN-AUDIT-SEQ
           END-IF

           IF NO-HARD-ERR
               PERFORM 3000-PURGE-CONTROL
           END-IF

           IF NO-HARD-ERR
               PERFORM 4000-COUNT-CHANGE
           END-IF

           IF NO-HARD-ERR
               PERFORM 5000-COUNT-AUDIT
           END-IF

           IF NO-HARD-ERR
               PERFORM 6000-WRITE-AUDIT
           END-IF

           IF HARD-ERR
               MOVE 8 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
               MOVE WS-TOTAL-PURGE-CNT TO WS-DISP-CNT
               DISPLAY "JH790B OK CNT=" WS-DISP-CNT
           END-IF

           GOBACK
           .

       1000-INITIALIZE.
           ACCEPT WS-CURR-DATE FROM DATE YYYYMMDD
           MOVE WS-CURR-DATE TO WS-BIZ-DATE

           COMPUTE WS-DATE-INT =
               FUNCTION INTEGER-OF-DATE(WS-CURR-DATE)
               - WS-CTL-KEEP-DAYS
           COMPUTE WS-CTL-LIMIT-DATE =
               FUNCTION DATE-OF-INTEGER(WS-DATE-INT)

           COMPUTE WS-DATE-INT =
               FUNCTION INTEGER-OF-DATE(WS-CURR-DATE)
               - WS-CHG-KEEP-DAYS
           COMPUTE WS-CHG-LIMIT-DATE =
               FUNCTION DATE-OF-INTEGER(WS-DATE-INT)

           COMPUTE WS-DATE-INT =
               FUNCTION INTEGER-OF-DATE(WS-CURR-DATE)
               - WS-AUD-KEEP-DAYS
           COMPUTE WS-AUD-LIMIT-DATE =
               FUNCTION DATE-OF-INTEGER(WS-DATE-INT)

           MOVE WS-CTL-LIMIT-DATE TO WS-LAST-KEEP-DATE
           IF WS-CHG-LIMIT-DATE > WS-LAST-KEEP-DATE
               MOVE WS-CHG-LIMIT-DATE TO WS-LAST-KEEP-DATE
           END-IF
           IF WS-AUD-LIMIT-DATE > WS-LAST-KEEP-DATE
               MOVE WS-AUD-LIMIT-DATE TO WS-LAST-KEEP-DATE
           END-IF
           .

       2000-SCAN-AUDIT-SEQ.
           MOVE "N" TO WS-AUD-EOF
           OPEN INPUT JHAUDTF

           IF WS-AUD-ST NOT = "00"
               DISPLAY "JHAUDTF OPEN ERR ST=" WS-AUD-ST
               SET HARD-ERR TO TRUE
           ELSE
               PERFORM UNTIL AUD-EOF
                   READ JHAUDTF
                       AT END
                           MOVE "Y" TO WS-AUD-EOF
                       NOT AT END
                           IF AUD-AUDIT-SEQ NUMERIC
                               IF AUD-AUDIT-SEQ > WS-MAX-AUDIT-SEQ
                                   MOVE AUD-AUDIT-SEQ
                                     TO WS-MAX-AUDIT-SEQ
                               END-IF
                           ELSE
                               DISPLAY "AUDIT SEQ ERR"
                           END-IF
                   END-READ

                   IF WS-AUD-ST NOT = "00"
                   AND WS-AUD-ST NOT = "10"
                       DISPLAY "JHAUDTF READ ERR ST=" WS-AUD-ST
                       SET HARD-ERR TO TRUE
                       MOVE "Y" TO WS-AUD-EOF
                   END-IF
               END-PERFORM

               CLOSE JHAUDTF
               IF WS-AUD-ST NOT = "00"
                   DISPLAY "JHAUDTF CLOSE ERR ST=" WS-AUD-ST
                   SET HARD-ERR TO TRUE
               END-IF
           END-IF

           ADD 1 TO WS-MAX-AUDIT-SEQ GIVING WS-NEXT-AUDIT-SEQ
           .

       3000-PURGE-CONTROL.
           MOVE "N" TO WS-CTL-EOF
           OPEN I-O JHCTLKF

           IF WS-CTL-ST NOT = "00"
               DISPLAY "JHCTLKF OPEN ERR ST=" WS-CTL-ST
               SET HARD-ERR TO TRUE
           ELSE
               PERFORM UNTIL CTL-EOF
                   READ JHCTLKF NEXT RECORD
                       AT END
                           MOVE "Y" TO WS-CTL-EOF
                       NOT AT END
                           ADD 1 TO WS-CTL-READ-CNT
                           PERFORM 3100-JUDGE-CONTROL
                   END-READ

                   IF WS-CTL-ST NOT = "00"
                   AND WS-CTL-ST NOT = "10"
                       DISPLAY "JHCTLKF READ ERR ST=" WS-CTL-ST
                       SET HARD-ERR TO TRUE
                       MOVE "Y" TO WS-CTL-EOF
                   END-IF
               END-PERFORM

               CLOSE JHCTLKF
               IF WS-CTL-ST NOT = "00"
                   DISPLAY "JHCTLKF CLOSE ERR ST=" WS-CTL-ST
                   SET HARD-ERR TO TRUE
               END-IF
           END-IF
           .

       3100-JUDGE-CONTROL.
           IF CT-BUSINESS-DT NUMERIC
               IF CT-BUSINESS-DT < WS-CTL-LIMIT-DATE
                   IF CT-STATUS-CD = "90"
                       DELETE JHCTLKF RECORD
                       IF WS-CTL-ST = "00"
                           ADD 1 TO WS-CTL-DEL-CNT
                       ELSE
                           DISPLAY "JHCTLKF DELETE ERR ST="
                               WS-CTL-ST
                           SET HARD-ERR TO TRUE
                           MOVE "Y" TO WS-CTL-EOF
                       END-IF
                   ELSE
                       ADD 1 TO WS-CTL-SKIP-CNT
                   END-IF
               END-IF
           ELSE
               DISPLAY "CTL DATE ERR JOB=" CT-JOB-ID
               SET HARD-ERR TO TRUE
               MOVE "Y" TO WS-CTL-EOF
           END-IF
           .

       4000-COUNT-CHANGE.
           MOVE "N" TO WS-CHG-EOF
           OPEN INPUT JHCHGEF

           IF WS-CHG-ST NOT = "00"
               DISPLAY "JHCHGEF OPEN ERR ST=" WS-CHG-ST
               SET HARD-ERR TO TRUE
           ELSE
               PERFORM UNTIL CHG-EOF
                   READ JHCHGEF
                       AT END
                           MOVE "Y" TO WS-CHG-EOF
                       NOT AT END
                           ADD 1 TO WS-CHG-READ-CNT
                           PERFORM 4100-JUDGE-CHANGE
                   END-READ

                   IF WS-CHG-ST NOT = "00"
                   AND WS-CHG-ST NOT = "10"
                       DISPLAY "JHCHGEF READ ERR ST=" WS-CHG-ST
                       SET HARD-ERR TO TRUE
                       MOVE "Y" TO WS-CHG-EOF
                   END-IF
               END-PERFORM

               CLOSE JHCHGEF
               IF WS-CHG-ST NOT = "00"
                   DISPLAY "JHCHGEF CLOSE ERR ST=" WS-CHG-ST
                   SET HARD-ERR TO TRUE
               END-IF
           END-IF
           .

       4100-JUDGE-CHANGE.
           IF CHG-CHANGE-TS(1:8) NUMERIC
               IF CHG-CHANGE-TS(1:8) < WS-CHG-LIMIT-DATE
                   IF CHG-APPLY-STATUS = "90"
                       ADD 1 TO WS-CHG-PURGE-CNT
                   END-IF
               END-IF
           ELSE
               DISPLAY "CHG DATE ERR SEQ=" CHG-CHANGE-SEQ
               SET HARD-ERR TO TRUE
               MOVE "Y" TO WS-CHG-EOF
           END-IF
           .

       5000-COUNT-AUDIT.
           MOVE "N" TO WS-AUD-EOF
           OPEN INPUT JHAUDTF

           IF WS-AUD-ST NOT = "00"
               DISPLAY "JHAUDTF OPEN ERR ST=" WS-AUD-ST
               SET HARD-ERR TO TRUE
           ELSE
               PERFORM UNTIL AUD-EOF
                   READ JHAUDTF
                       AT END
                           MOVE "Y" TO WS-AUD-EOF
                       NOT AT END
                           ADD 1 TO WS-AUD-READ-CNT
                           PERFORM 5100-JUDGE-AUDIT
                   END-READ

                   IF WS-AUD-ST NOT = "00"
                   AND WS-AUD-ST NOT = "10"
                       DISPLAY "JHAUDTF READ ERR ST=" WS-AUD-ST
                       SET HARD-ERR TO TRUE
                       MOVE "Y" TO WS-AUD-EOF
                   END-IF
               END-PERFORM

               CLOSE JHAUDTF
               IF WS-AUD-ST NOT = "00"
                   DISPLAY "JHAUDTF CLOSE ERR ST=" WS-AUD-ST
                   SET HARD-ERR TO TRUE
               END-IF
           END-IF
           .

       5100-JUDGE-AUDIT.
           IF AUD-BUSINESS-DT NUMERIC
               IF AUD-BUSINESS-DT < WS-AUD-LIMIT-DATE
                   IF AUD-JOB-ID NOT = WS-JOB-ID
                       ADD 1 TO WS-AUD-PURGE-CNT
                   END-IF
               END-IF
           ELSE
               DISPLAY "AUD DATE ERR SEQ=" AUD-AUDIT-SEQ
               SET HARD-ERR TO TRUE
               MOVE "Y" TO WS-AUD-EOF
           END-IF
           .

       6000-WRITE-AUDIT.
           COMPUTE WS-TOTAL-PURGE-CNT =
               WS-CTL-DEL-CNT
               + WS-CHG-PURGE-CNT
               + WS-AUD-PURGE-CNT

           OPEN EXTEND JHAUDTF

           IF WS-AUD-ST NOT = "00"
               DISPLAY "JHAUDTF EXTEND ERR ST=" WS-AUD-ST
               SET HARD-ERR TO TRUE
           ELSE
               INITIALIZE JHAUDTF-REC
               MOVE WS-NEXT-AUDIT-SEQ  TO AUD-AUDIT-SEQ
               MOVE WS-JOB-ID          TO AUD-JOB-ID
               MOVE WS-BIZ-DATE        TO AUD-BUSINESS-DT
               MOVE WS-AUD-EVENT       TO AUD-EVENT-CD
               MOVE WS-AUD-DATASET     TO AUD-DATASET-ID
               MOVE WS-TOTAL-PURGE-CNT TO AUD-REC-CNT
               MOVE 0                  TO AUD-AMT-TOTAL
               MOVE WS-CURR-DATE       TO AUD-EVENT-TS
               WRITE JHAUDTF-REC

               IF WS-AUD-ST NOT = "00"
                   DISPLAY "JHAUDTF WRITE ERR ST=" WS-AUD-ST
                   SET HARD-ERR TO TRUE
               END-IF

               IF NO-HARD-ERR
                   INITIALIZE JHAUDTF-REC
                   ADD 1 TO WS-NEXT-AUDIT-SEQ
                   MOVE WS-NEXT-AUDIT-SEQ TO AUD-AUDIT-SEQ
                   MOVE WS-JOB-ID         TO AUD-JOB-ID
                   MOVE WS-LAST-KEEP-DATE TO AUD-BUSINESS-DT
                   MOVE "LASTKEEP"        TO AUD-EVENT-CD
                   MOVE WS-AUD-DATASET    TO AUD-DATASET-ID
                   MOVE WS-CTL-SKIP-CNT   TO AUD-REC-CNT
                   MOVE 0                 TO AUD-AMT-TOTAL
                   MOVE WS-CURR-DATE      TO AUD-EVENT-TS
                   WRITE JHAUDTF-REC

                   IF WS-AUD-ST NOT = "00"
                       DISPLAY "JHAUDTF WRITE2 ERR ST="
                           WS-AUD-ST
                       SET HARD-ERR TO TRUE
                   END-IF
               END-IF

               CLOSE JHAUDTF
               IF WS-AUD-ST NOT = "00"
                   DISPLAY "JHAUDTF CLOSE ERR ST=" WS-AUD-ST
                   SET HARD-ERR TO TRUE
               END-IF
           END-IF
           .
