       IDENTIFICATION DIVISION.
       PROGRAM-ID. CB217B.
       AUTHOR. CARD-BATCH.
      * 否決通知作成バッチ
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.
      *
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CDARSPF ASSIGN TO "CDARSPF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-AR-STAT.
           SELECT CDFRDF ASSIGN TO "CDFRDF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FR-STAT.
           SELECT CDCARDF ASSIGN TO "CDCARDF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS CF-CARD-NO
               FILE STATUS IS WS-CF-STAT.
           SELECT CDNTFF ASSIGN TO "CDNTFF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS NF-NOTICE-ID
               FILE STATUS IS WS-NF-STAT.
      *
       DATA DIVISION.
       FILE SECTION.
      *
       FD  CDARSPF.
           COPY CDARSPFC.
      *
       FD  CDFRDF.
           COPY CDFRDC.
      *
       FD  CDCARDF.
           COPY CDCARD02.
      *
       FD  CDNTFF.
           COPY CDNTFC.
      *
       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05 WS-AR-STAT              PIC XX VALUE SPACE.
           05 WS-FR-STAT              PIC XX VALUE SPACE.
           05 WS-CF-STAT              PIC XX VALUE SPACE.
           05 WS-NF-STAT              PIC XX VALUE SPACE.
      *
       01  WS-END-FLAGS.
           05 WS-AR-EOF               PIC X VALUE "N".
              88 AR-EOF                     VALUE "Y".
           05 WS-FR-EOF               PIC X VALUE "N".
              88 FR-EOF                     VALUE "Y".
      *
       01  WS-COUNTERS.
           05 WS-AR-READ-CNT          PIC 9(9) VALUE ZERO.
           05 WS-FR-READ-CNT          PIC 9(9) VALUE ZERO.
           05 WS-CF-READ-CNT          PIC 9(9) VALUE ZERO.
           05 WS-NF-WRITE-CNT         PIC 9(9) VALUE ZERO.
           05 WS-SKIP-CNT             PIC 9(9) VALUE ZERO.
           05 WS-ERR-CNT              PIC 9(9) VALUE ZERO.
           05 WS-NOTICE-SEQ           PIC 9(9) VALUE ZERO.
      *
       01  WS-DATE-TIME.
           05 WS-CUR-DATE             PIC 9(8) VALUE ZERO.
           05 WS-CUR-TIME             PIC 9(8) VALUE ZERO.
           05 WS-CUR-TS               PIC X(14) VALUE SPACE.
      *
       01  WS-WORK.
           05 WS-LAST4                PIC X(4) VALUE SPACE.
           05 WS-CATEGORY             PIC X(20) VALUE SPACE.
           05 WS-DEC-REASON           PIC X(30) VALUE SPACE.
           05 WS-SCORE-N              PIC 9(3) VALUE ZERO.
           05 WS-AUTH-AMT-N           PIC 9(11) VALUE ZERO.
           05 WS-AVAIL-AMT-N          PIC 9(11) VALUE ZERO.
           05 WS-NOTICE-NEED          PIC X VALUE "N".
              88 NOTICE-NEED                VALUE "Y".
           05 WS-TEXT-1               PIC X(120) VALUE SPACE.
           05 WS-TEXT-2               PIC X(120) VALUE SPACE.
      *
       01  WS-CONSTANTS.
           05 WC-YES                  PIC X VALUE "Y".
           05 WC-NO                   PIC X VALUE "N".
           05 WC-STATUS-OK            PIC XX VALUE "01".
           05 WC-DEC-DENY             PIC X VALUE "D".
           05 WC-CH-MAIL              PIC XX VALUE "01".
           05 WC-NOTICE-FRAUD         PIC XX VALUE "31".
           05 WC-NOTICE-LIMIT         PIC XX VALUE "32".
           05 WC-SCORE-LIMIT          PIC 9(3) VALUE 700.
      *
       PROCEDURE DIVISION.
      *
       0000-MAIN SECTION.
           PERFORM 1000-INIT
           PERFORM 2000-PROCESS UNTIL AR-EOF
           PERFORM 9000-FINAL
           MOVE 0 TO RETURN-CODE
           GOBACK.
      *
       1000-INIT SECTION.
           ACCEPT WS-CUR-DATE FROM DATE YYYYMMDD
           ACCEPT WS-CUR-TIME FROM TIME
           STRING WS-CUR-DATE WS-CUR-TIME(1:6)
               DELIMITED BY SIZE INTO WS-CUR-TS
           END-STRING
      *
           OPEN INPUT CDARSPF
           IF WS-AR-STAT NOT = "00"
              DISPLAY "CDARSPF オープン失敗 ST=" WS-AR-STAT
              PERFORM 9900-ABEND
           END-IF
      *
           OPEN INPUT CDFRDF
           IF WS-FR-STAT NOT = "00"
              DISPLAY "CDFRDF オープン失敗 ST=" WS-FR-STAT
              PERFORM 9900-ABEND
           END-IF
      *
           OPEN INPUT CDCARDF
           IF WS-CF-STAT NOT = "00"
              DISPLAY "CDCARDF オープン失敗 ST=" WS-CF-STAT
              PERFORM 9900-ABEND
           END-IF
      *
           OPEN OUTPUT CDNTFF
           IF WS-NF-STAT NOT = "00"
              DISPLAY "CDNTFF オープン失敗 ST=" WS-NF-STAT
              PERFORM 9900-ABEND
           END-IF
      *
           PERFORM 3100-READ-FRAUD
           PERFORM 3000-READ-AUTH.
      *
       2000-PROCESS SECTION.
           MOVE WC-NO TO WS-NOTICE-NEED
      *
           IF AR-DECISION-KBN = WC-DEC-DENY
              PERFORM 4000-MATCH-FRAUD
              IF NOT FR-EOF
                 IF FR-AUTH-ID = AR-AUTH-ID
                    PERFORM 5000-CHECK-CARD
                    IF NOTICE-NEED
                       PERFORM 6000-WRITE-NOTICE
                    ELSE
                       ADD 1 TO WS-SKIP-CNT
                    END-IF
                 ELSE
                    ADD 1 TO WS-SKIP-CNT
                 END-IF
              ELSE
                 ADD 1 TO WS-SKIP-CNT
              END-IF
           ELSE
              ADD 1 TO WS-SKIP-CNT
           END-IF
      *
           PERFORM 3000-READ-AUTH.
      *
       3000-READ-AUTH SECTION.
           READ CDARSPF
              AT END
                 MOVE WC-YES TO WS-AR-EOF
              NOT AT END
                 ADD 1 TO WS-AR-READ-CNT
           END-READ
           IF WS-AR-STAT NOT = "00" AND WS-AR-STAT NOT = "10"
              DISPLAY "CDARSPF 読込失敗 ST=" WS-AR-STAT
              PERFORM 9900-ABEND
           END-IF.
      *
       3100-READ-FRAUD SECTION.
           READ CDFRDF
              AT END
                 MOVE WC-YES TO WS-FR-EOF
              NOT AT END
                 ADD 1 TO WS-FR-READ-CNT
           END-READ
           IF WS-FR-STAT NOT = "00" AND WS-FR-STAT NOT = "10"
              DISPLAY "CDFRDF 読込失敗 ST=" WS-FR-STAT
              PERFORM 9900-ABEND
           END-IF.
      *
       4000-MATCH-FRAUD SECTION.
           PERFORM UNTIL FR-EOF OR FR-AUTH-ID >= AR-AUTH-ID
              PERFORM 3100-READ-FRAUD
           END-PERFORM.
      *
       5000-CHECK-CARD SECTION.
           MOVE AR-CARD-NO TO CF-CARD-NO
           READ CDCARDF
              INVALID KEY
                 ADD 1 TO WS-ERR-CNT
                 DISPLAY "カードマスタ未登録 AUTH=" AR-AUTH-ID
              NOT INVALID KEY
                 ADD 1 TO WS-CF-READ-CNT
                 PERFORM 5100-EVALUATE-NOTICE
           END-READ
           IF WS-CF-STAT NOT = "00" AND WS-CF-STAT NOT = "23"
              DISPLAY "CDCARDF 読込失敗 ST=" WS-CF-STAT
              PERFORM 9900-ABEND
           END-IF.
      *
       5100-EVALUATE-NOTICE SECTION.
           MOVE FR-FRAUD-SCORE TO WS-SCORE-N
           MOVE AR-AUTH-AMT TO WS-AUTH-AMT-N
           MOVE AR-AVAIL-AMT TO WS-AVAIL-AMT-N
      *
           IF CF-CARD-STATUS = WC-STATUS-OK
              IF WS-SCORE-N >= WC-SCORE-LIMIT
                 MOVE WC-YES TO WS-NOTICE-NEED
                 MOVE "不正利用確認" TO WS-DEC-REASON
              ELSE
                 IF AR-DECLINE-REASON = "LIM"
                    IF WS-AUTH-AMT-N > WS-AVAIL-AMT-N
                       MOVE WC-YES TO WS-NOTICE-NEED
                       MOVE "利用可能枠超過" TO WS-DEC-REASON
                    END-IF
                 END-IF
              END-IF
           ELSE
              IF AR-DECLINE-REASON = "STS"
                 MOVE WC-YES TO WS-NOTICE-NEED
                 MOVE "カード状態確認" TO WS-DEC-REASON
              END-IF
           END-IF.
      *
       6000-WRITE-NOTICE SECTION.
           ADD 1 TO WS-NOTICE-SEQ
           MOVE SPACES TO CDNTFF-REC
           STRING "NF" WS-CUR-DATE WS-NOTICE-SEQ
               DELIMITED BY SIZE INTO NF-NOTICE-ID
           END-STRING
      *
           MOVE AR-CARD-NO TO NF-CARD-NO
           IF WS-SCORE-N >= WC-SCORE-LIMIT
              MOVE WC-NOTICE-FRAUD TO NF-NOTICE-KBN
           ELSE
              MOVE WC-NOTICE-LIMIT TO NF-NOTICE-KBN
           END-IF
           MOVE WC-CH-MAIL TO NF-CHANNEL-CD
           MOVE WS-CUR-TS TO NF-NOTICE-TS
      *
           PERFORM 6100-BUILD-TEXT
           MOVE WS-TEXT-1 TO NF-NOTICE-TEXT
      *
           WRITE CDNTFF-REC
              INVALID KEY
                 DISPLAY "CDNTFF 書込キー重複 ID=" NF-NOTICE-ID
                 PERFORM 9900-ABEND
           END-WRITE
           IF WS-NF-STAT NOT = "00"
              DISPLAY "CDNTFF 書込失敗 ST=" WS-NF-STAT
              PERFORM 9900-ABEND
           END-IF
           ADD 1 TO WS-NF-WRITE-CNT.
      *
       6100-BUILD-TEXT SECTION.
           MOVE AR-CARD-NO(13:4) TO WS-LAST4
           EVALUATE FR-RULE-HIT-CD
              WHEN "EC01"
                 MOVE "電子商取引" TO WS-CATEGORY
              WHEN "ATM1"
                 MOVE "現金関連" TO WS-CATEGORY
              WHEN "TRVL"
                 MOVE "旅行交通" TO WS-CATEGORY
              WHEN "DGTL"
                 MOVE "デジタル役務" TO WS-CATEGORY
              WHEN OTHER
                 MOVE "一般加盟店" TO WS-CATEGORY
           END-EVALUATE
      *
           MOVE SPACES TO WS-TEXT-1
           STRING "カード下4桁" WS-LAST4 "、"
                  "カテゴリ" WS-CATEGORY "、"
                  "時刻" FR-SCORE-TS "、"
                  WS-DEC-REASON "。"
              DELIMITED BY SIZE INTO WS-TEXT-1
           END-STRING.
      *
       9000-FINAL SECTION.
           CLOSE CDARSPF
           IF WS-AR-STAT NOT = "00"
              DISPLAY "CDARSPF クローズ失敗 ST=" WS-AR-STAT
              PERFORM 9900-ABEND
           END-IF
      *
           CLOSE CDFRDF
           IF WS-FR-STAT NOT = "00"
              DISPLAY "CDFRDF クローズ失敗 ST=" WS-FR-STAT
              PERFORM 9900-ABEND
           END-IF
      *
           CLOSE CDCARDF
           IF WS-CF-STAT NOT = "00"
              DISPLAY "CDCARDF クローズ失敗 ST=" WS-CF-STAT
              PERFORM 9900-ABEND
           END-IF
      *
           CLOSE CDNTFF
           IF WS-NF-STAT NOT = "00"
              DISPLAY "CDNTFF クローズ失敗 ST=" WS-NF-STAT
              PERFORM 9900-ABEND
           END-IF
      *
           DISPLAY "CB217B 正常終了"
           DISPLAY "否決応答読込件数=" WS-AR-READ-CNT
           DISPLAY "不正結果読込件数=" WS-FR-READ-CNT
           DISPLAY "カード参照件数=" WS-CF-READ-CNT
           DISPLAY "通知作成件数=" WS-NF-WRITE-CNT
           DISPLAY "対象外件数=" WS-SKIP-CNT
           DISPLAY "エラー件数=" WS-ERR-CNT.
      *
       9900-ABEND SECTION.
           MOVE 8 TO RETURN-CODE
           DISPLAY "CB217B 異常終了"
           GOBACK.
