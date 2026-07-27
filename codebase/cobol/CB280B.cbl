       IDENTIFICATION DIVISION.
       PROGRAM-ID. CB280B.
      *
      * 変更履歴
      * 版数  年月日    担当    概要
      * 1.00  20240401  ＣＤ運用 初版作成
      * 1.01  20240520  ＣＤ運用 延滞段階判定を追加
      * 1.02  20240615  ＣＤ運用 複数カード厳格判定を追加
      *
      * 会員ステータス更新バッチ
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CDDELINQF
               ASSIGN       TO "CDDELINQF"
               ORGANIZATION IS INDEXED
               ACCESS MODE  IS SEQUENTIAL
               RECORD KEY   IS DL-CARD-NO
               FILE STATUS  IS WS-DL-STAT.
           SELECT CDCARDF
               ASSIGN       TO "CDCARDF"
               ORGANIZATION IS INDEXED
               ACCESS MODE  IS RANDOM
               RECORD KEY   IS CF-CARD-NO
               FILE STATUS  IS WS-CF-STAT.
           SELECT CDMEMSTATF
               ASSIGN       TO "CDMEMSTATF"
               ORGANIZATION IS INDEXED
               ACCESS MODE  IS RANDOM
               RECORD KEY   IS MS-MEMBER-ID
               FILE STATUS  IS WS-MS-STAT.

       DATA DIVISION.
       FILE SECTION.
       FD  CDDELINQF.
           COPY CDDLNQC.
       FD  CDCARDF.
           COPY CDCARDFC.
       FD  CDMEMSTATF.
           COPY CDMSTC.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05 WS-DL-STAT              PIC XX.
           05 WS-CF-STAT              PIC XX.
           05 WS-MS-STAT              PIC XX.

       01  WS-SWITCHES.
           05 WS-EOF-SW               PIC X VALUE 'N'.
              88 DL-EOF                     VALUE 'Y'.
           05 WS-HARD-ERR-SW          PIC X VALUE 'N'.
              88 HARD-ERROR                 VALUE 'Y'.

       01  WS-DATE-TIME.
           05 WS-CURRENT-DATE         PIC 9(08).
           05 WS-CURRENT-TIME         PIC 9(08).
           05 WS-CURRENT-TS           PIC 9(14).
           05 WS-THIS-MONTH           PIC 9(06).
           05 WS-OPEN-MONTH           PIC 9(06).

       01  WS-COUNTERS.
           05 WS-DL-READ-CNT          PIC 9(09) VALUE 0.
           05 WS-CF-NOTF-CNT          PIC 9(09) VALUE 0.
           05 WS-MS-NOTF-CNT          PIC 9(09) VALUE 0.
           05 WS-SKIP-CNT             PIC 9(09) VALUE 0.
           05 WS-UPD-CNT              PIC 9(09) VALUE 0.
           05 WS-NOCHG-CNT            PIC 9(09) VALUE 0.

       01  WS-CANDIDATE.
           05 WS-CAND-STATUS          PIC X(02).
           05 WS-CAND-REASON          PIC X(03).
           05 WS-CAND-SEV             PIC 9(02).

       01  WS-CURRENT.
           05 WS-CURR-SEV             PIC 9(02).

       01  WS-WORK.
           05 WS-DL-STAGE-NUM         PIC 9 VALUE 0.
           05 WS-UPDATE-NEEDED        PIC X VALUE 'N'.
              88 UPDATE-NEEDED              VALUE 'Y'.
           05 WS-VALID-CARD           PIC X VALUE 'N'.
              88 VALID-CARD                 VALUE 'Y'.

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE 0 TO RETURN-CODE
           PERFORM 1000-INIT
           IF NOT HARD-ERROR
               PERFORM 2000-PROCESS UNTIL DL-EOF OR HARD-ERROR
           END-IF
           PERFORM 9000-FINAL
           GOBACK.

       1000-INIT.
           ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD
           ACCEPT WS-CURRENT-TIME FROM TIME
           STRING WS-CURRENT-DATE
                  WS-CURRENT-TIME(1:6)
             DELIMITED BY SIZE INTO WS-CURRENT-TS
           END-STRING
           MOVE WS-CURRENT-DATE(1:6) TO WS-THIS-MONTH

           OPEN INPUT CDDELINQF
           IF WS-DL-STAT NOT = '00'
               DISPLAY 'CDDELINQF OPEN ERROR ST=' WS-DL-STAT
               MOVE 'Y' TO WS-HARD-ERR-SW
               MOVE 8 TO RETURN-CODE
           END-IF

           OPEN INPUT CDCARDF
           IF WS-CF-STAT NOT = '00'
               DISPLAY 'CDCARDF OPEN ERROR ST=' WS-CF-STAT
               MOVE 'Y' TO WS-HARD-ERR-SW
               MOVE 8 TO RETURN-CODE
           END-IF

           OPEN I-O CDMEMSTATF
           IF WS-MS-STAT NOT = '00'
               DISPLAY 'CDMEMSTATF OPEN ERROR ST=' WS-MS-STAT
               MOVE 'Y' TO WS-HARD-ERR-SW
               MOVE 8 TO RETURN-CODE
           END-IF.

       2000-PROCESS.
           READ CDDELINQF NEXT RECORD
               AT END
                   MOVE 'Y' TO WS-EOF-SW
               NOT AT END
                   ADD 1 TO WS-DL-READ-CNT
                   PERFORM 2100-READ-CARD
                   IF VALID-CARD
                       PERFORM 2200-BUILD-CANDIDATE
                       IF WS-CAND-SEV > 0
                           PERFORM 2300-UPDATE-MEMBER
                       ELSE
                           ADD 1 TO WS-SKIP-CNT
                       END-IF
                   END-IF
           END-READ.

       2100-READ-CARD.
           MOVE 'N' TO WS-VALID-CARD
           MOVE DL-CARD-NO TO CF-CARD-NO
           READ CDCARDF
               INVALID KEY
                   IF WS-CF-STAT = '23'
                       ADD 1 TO WS-CF-NOTF-CNT
                       DISPLAY 'カード未検出 CARD='
                               DL-CARD-NO
                   ELSE
                       DISPLAY 'CDCARDF READ ERROR ST='
                               WS-CF-STAT
                       MOVE 'Y' TO WS-HARD-ERR-SW
                       MOVE 8 TO RETURN-CODE
                   END-IF
               NOT INVALID KEY
                   IF CF-CARD-STATUS = '01'
                   OR CF-CARD-STATUS = '02'
                   OR CF-CARD-STATUS = '03'
                   OR CF-CARD-STATUS = '09'
                       MOVE 'Y' TO WS-VALID-CARD
                   ELSE
                       ADD 1 TO WS-SKIP-CNT
                       DISPLAY 'カード状態不正 CARD='
                               CF-CARD-NO
                               ' 状態='
                               CF-CARD-STATUS
                   END-IF
           END-READ.

       2200-BUILD-CANDIDATE.
           MOVE SPACES TO WS-CAND-STATUS
           MOVE SPACES TO WS-CAND-REASON
           MOVE 0 TO WS-CAND-SEV
           MOVE 0 TO WS-DL-STAGE-NUM

           IF DL-DUNNING-STAGE NUMERIC
               MOVE DL-DUNNING-STAGE TO WS-DL-STAGE-NUM
           END-IF

           EVALUATE TRUE
               WHEN CF-CARD-STATUS = '03'
                   MOVE '90'  TO WS-CAND-STATUS
                   MOVE 'C03' TO WS-CAND-REASON
                   MOVE 90    TO WS-CAND-SEV
               WHEN CF-CARD-STATUS = '02'
                   MOVE '50'  TO WS-CAND-STATUS
                   MOVE 'C02' TO WS-CAND-REASON
                   MOVE 50    TO WS-CAND-SEV
               WHEN CF-CARD-STATUS = '09'
                   MOVE '80'  TO WS-CAND-STATUS
                   MOVE 'D09' TO WS-CAND-REASON
                   MOVE 80    TO WS-CAND-SEV
               WHEN WS-DL-STAGE-NUM >= 3
                   MOVE '80'  TO WS-CAND-STATUS
                   MOVE 'DL3' TO WS-CAND-REASON
                   MOVE 80    TO WS-CAND-SEV
               WHEN DL-DAYS-PAST-DUE >= 61
                   MOVE '80'  TO WS-CAND-STATUS
                   MOVE 'D61' TO WS-CAND-REASON
                   MOVE 80    TO WS-CAND-SEV
               WHEN DL-DAYS-PAST-DUE >= 31
                   MOVE '60'  TO WS-CAND-STATUS
                   MOVE 'D31' TO WS-CAND-REASON
                   MOVE 60    TO WS-CAND-SEV
               WHEN DL-DAYS-PAST-DUE >= 1
                   MOVE '40'  TO WS-CAND-STATUS
                   MOVE 'D01' TO WS-CAND-REASON
                   MOVE 40    TO WS-CAND-SEV
               WHEN OTHER
                   MOVE CF-OPEN-DT(1:6) TO WS-OPEN-MONTH
                   IF WS-OPEN-MONTH = WS-THIS-MONTH
                       MOVE '10'  TO WS-CAND-STATUS
                       MOVE 'NEW' TO WS-CAND-REASON
                       MOVE 10    TO WS-CAND-SEV
                   END-IF
           END-EVALUATE.

       2300-UPDATE-MEMBER.
           MOVE CF-MEMBER-ID TO MS-MEMBER-ID
           READ CDMEMSTATF
               INVALID KEY
                   IF WS-MS-STAT = '23'
                       ADD 1 TO WS-MS-NOTF-CNT
                       DISPLAY '会員ステータス未検出 会員='
                               CF-MEMBER-ID
                   ELSE
                       DISPLAY 'CDMEMSTATF READ ERROR ST='
                               WS-MS-STAT
                       MOVE 'Y' TO WS-HARD-ERR-SW
                       MOVE 8 TO RETURN-CODE
                   END-IF
               NOT INVALID KEY
                   PERFORM 2310-CALC-CURRENT-SEV
                   PERFORM 2320-JUDGE-DIFFERENCE
                   IF UPDATE-NEEDED
                       PERFORM 2330-REWRITE-MEMBER
                   ELSE
                       ADD 1 TO WS-NOCHG-CNT
                   END-IF
           END-READ.

       2310-CALC-CURRENT-SEV.
           EVALUATE MS-STATUS-CD
               WHEN '90' MOVE 90 TO WS-CURR-SEV
               WHEN '80' MOVE 80 TO WS-CURR-SEV
               WHEN '60' MOVE 60 TO WS-CURR-SEV
               WHEN '50' MOVE 50 TO WS-CURR-SEV
               WHEN '40' MOVE 40 TO WS-CURR-SEV
               WHEN '10' MOVE 10 TO WS-CURR-SEV
               WHEN OTHER MOVE 0 TO WS-CURR-SEV
           END-EVALUATE.

       2320-JUDGE-DIFFERENCE.
           MOVE 'N' TO WS-UPDATE-NEEDED
           IF WS-CAND-SEV > WS-CURR-SEV
               MOVE 'Y' TO WS-UPDATE-NEEDED
           ELSE
               IF WS-CAND-SEV = WS-CURR-SEV
               AND (MS-STATUS-CD NOT = WS-CAND-STATUS
                    OR MS-STATUS-REASON NOT = WS-CAND-REASON)
                   MOVE 'Y' TO WS-UPDATE-NEEDED
               END-IF
           END-IF.

       2330-REWRITE-MEMBER.
           MOVE WS-CAND-STATUS TO MS-STATUS-CD
           MOVE WS-CAND-REASON TO MS-STATUS-REASON
           MOVE WS-CURRENT-DATE TO MS-EFFECTIVE-DT
           MOVE WS-CURRENT-TS   TO MS-LAST-UPDATED-TS
           REWRITE CDMEMSTATF-REC
               INVALID KEY
                   DISPLAY 'CDMEMSTATF REWRITE ERROR ST='
                           WS-MS-STAT
                           ' MEMBER='
                           MS-MEMBER-ID
                   MOVE 'Y' TO WS-HARD-ERR-SW
                   MOVE 8 TO RETURN-CODE
               NOT INVALID KEY
                   ADD 1 TO WS-UPD-CNT
           END-REWRITE.

       9000-FINAL.
           CLOSE CDDELINQF
           CLOSE CDCARDF
           CLOSE CDMEMSTATF

           DISPLAY 'CB280B 会員ステータス更新終了'
           DISPLAY '延滞読込件数=' WS-DL-READ-CNT
           DISPLAY '更新件数=' WS-UPD-CNT
           DISPLAY '変更なし件数=' WS-NOCHG-CNT
           DISPLAY '対象外件数=' WS-SKIP-CNT
           DISPLAY 'カード未検出件数=' WS-CF-NOTF-CNT
           DISPLAY '会員未検出件数=' WS-MS-NOTF-CNT

           IF HARD-ERROR
               IF RETURN-CODE = 0
                   MOVE 8 TO RETURN-CODE
               END-IF
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.
