       IDENTIFICATION DIVISION.
       PROGRAM-ID. CM160B.
      *---------------------------------------------------------------*
      * 変更履歴                                                      *
      * 版数  年月日    担当    概要                                  *
      * 0.1   20230116  開発一  初期作成                              *
      * 0.2   20230306  開発二  属性不足出力を追加                    *
      * 0.3   20230612  開発三  候補登録時の重複検知を追加            *
      *---------------------------------------------------------------*
      * 名寄せ候補一次抽出バッチ                                      *
      * 生年月日、性別、正規化カナ名、住所コードの完全一致により      *
      * CIF重複候補を作成する。検査数字の参照および算出は行わない。    *
      *---------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CMCIFF ASSIGN TO "CMCIFF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-CMCIFF.
           SELECT CMATTF ASSIGN TO "CMATTF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS CA-CIF-NO
               FILE STATUS IS FS-CMATTF.
           SELECT CMDUPF ASSIGN TO "CMDUPF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS DP-CANDIDATE-ID
               FILE STATUS IS FS-CMDUPF.
           SELECT CKERRF ASSIGN TO "CKERRF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-CKERRF.

       DATA DIVISION.
       FILE SECTION.
       FD  CMCIFF.
           COPY CMCIFFC.
       FD  CMATTF.
           COPY CMATTC.
       FD  CMDUPF.
           COPY CMDUPC.
       FD  CKERRF.
           COPY CKERRC.

       WORKING-STORAGE SECTION.
       01  WK-CONSTANTS.
           05  WK-PGM-ID              PIC X(08) VALUE "CM160B".
           05  WK-STAT-OK             PIC X(02) VALUE "00".
           05  WK-STAT-EOF            PIC X(02) VALUE "10".
           05  WK-STAT-DUP            PIC X(02) VALUE "22".
           05  WK-CIF-ACTIVE          PIC X(02) VALUE "01".
           05  WK-CIF-EXCLUDE         PIC X(02) VALUE "08".
           05  WK-CIF-INVALID         PIC X(02) VALUE "09".
           05  WK-ATTR-ACTIVE         PIC X(02) VALUE "01".
           05  WK-JUDGE-AUTO          PIC X(01) VALUE "1".
           05  WK-ERR-CIF-STAT        PIC X(08) VALUE "E160010".
           05  WK-ERR-ATTR-NONE       PIC X(08) VALUE "E160020".
           05  WK-ERR-ATTR-STAT       PIC X(08) VALUE "E160030".
           05  WK-ERR-ATTR-LESS       PIC X(08) VALUE "E160040".
           05  WK-ERR-TBL-FULL        PIC X(08) VALUE "E160050".
           05  WK-MAX-ENTRY           PIC 9(05) VALUE 10000.
           05  WK-MAX-SCORE           PIC 9(03) VALUE 100.

       01  WK-FILE-STATUS.
           05  FS-CMCIFF              PIC X(02).
           05  FS-CMATTF              PIC X(02).
           05  FS-CMDUPF              PIC X(02).
           05  FS-CKERRF              PIC X(02).

       01  WK-SWITCHES.
           05  SW-CMCIFF-EOF          PIC X(01) VALUE "N".
               88  CMCIFF-EOF                   VALUE "Y".
           05  SW-HARD-ERROR          PIC X(01) VALUE "N".
               88  HARD-ERROR                  VALUE "Y".

       01  WK-DATE-AREA.
           05  WK-CURRENT-DATE        PIC X(21).
           05  WK-TODAY               PIC X(08).

       01  WK-COUNTERS.
           05  WK-READ-CNT            PIC 9(09) VALUE ZERO.
           05  WK-ENTRY-CNT           PIC 9(05) VALUE ZERO.
           05  WK-DUP-CNT             PIC 9(09) VALUE ZERO.
           05  WK-ERR-CNT             PIC 9(09) VALUE ZERO.
           05  WK-SKIP-CNT            PIC 9(09) VALUE ZERO.
           05  WK-CAND-SEQ            PIC 9(09) VALUE ZERO.
           05  WK-ERR-SEQ             PIC 9(09) VALUE ZERO.
           05  IX1                    PIC 9(05) VALUE ZERO.
           05  IX2                    PIC 9(05) VALUE ZERO.

       01  WK-WORK.
           05  WK-SCORE               PIC 9(03) VALUE ZERO.
           05  WK-VALID-ATTR          PIC X(01) VALUE "N".
           05  WK-ERR-KEY             PIC X(30) VALUE SPACES.
           05  WK-CANDIDATE-ID        PIC X(20) VALUE SPACES.
           05  WK-ERROR-ID            PIC X(20) VALUE SPACES.

       01  WK-ENTRY-TABLE.
           05  WK-ENTRY OCCURS 10000 TIMES.
               10  TB-CIF-NO          PIC X(20).
               10  TB-BIRTH-DT        PIC X(08).
               10  TB-SEX-KBN         PIC X(02).
               10  TB-KANA-NAME       PIC X(80).
               10  TB-ADDR-CD         PIC X(12).

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE 8 TO RETURN-CODE
           PERFORM 1000-INIT
           IF NOT HARD-ERROR
               PERFORM 2000-LOAD-CIF UNTIL CMCIFF-EOF OR HARD-ERROR
           END-IF
           IF NOT HARD-ERROR
               PERFORM 3000-MAKE-CANDIDATE
           END-IF
           PERFORM 9000-FINAL
           GOBACK.

       1000-INIT.
           MOVE FUNCTION CURRENT-DATE TO WK-CURRENT-DATE
           MOVE WK-CURRENT-DATE(1:8) TO WK-TODAY
           OPEN INPUT  CMCIFF
                INPUT  CMATTF
                OUTPUT CMDUPF
                OUTPUT CKERRF
           IF FS-CMCIFF NOT = WK-STAT-OK
               DISPLAY "CMCIFF オープン失敗 ST=" FS-CMCIFF
               SET HARD-ERROR TO TRUE
           END-IF
           IF FS-CMATTF NOT = WK-STAT-OK
               DISPLAY "CMATTF オープン失敗 ST=" FS-CMATTF
               SET HARD-ERROR TO TRUE
           END-IF
           IF FS-CMDUPF NOT = WK-STAT-OK
               DISPLAY "CMDUPF オープン失敗 ST=" FS-CMDUPF
               SET HARD-ERROR TO TRUE
           END-IF
           IF FS-CKERRF NOT = WK-STAT-OK
               DISPLAY "CKERRF オープン失敗 ST=" FS-CKERRF
               SET HARD-ERROR TO TRUE
           END-IF
           IF NOT HARD-ERROR
               DISPLAY "CM160B 開始 日付=" WK-TODAY
           END-IF.

       2000-LOAD-CIF.
           READ CMCIFF
               AT END
                   SET CMCIFF-EOF TO TRUE
               NOT AT END
                   ADD 1 TO WK-READ-CNT
                   PERFORM 2100-PROCESS-CIF
           END-READ
           IF FS-CMCIFF NOT = WK-STAT-OK
              AND FS-CMCIFF NOT = WK-STAT-EOF
               DISPLAY "CMCIFF 読込失敗 ST=" FS-CMCIFF
               SET HARD-ERROR TO TRUE
           END-IF.

       2100-PROCESS-CIF.
           EVALUATE CF-CIF-STATUS-KBN
               WHEN WK-CIF-ACTIVE
                   PERFORM 2200-READ-ATTRIBUTE
               WHEN WK-CIF-EXCLUDE
               WHEN WK-CIF-INVALID
                   ADD 1 TO WK-SKIP-CNT
               WHEN OTHER
                   MOVE CF-CIF-NO TO WK-ERR-KEY
                   PERFORM 8100-WRITE-CIF-STATUS-ERR
           END-EVALUATE.

       2200-READ-ATTRIBUTE.
           MOVE CF-CIF-NO TO CA-CIF-NO
           READ CMATTF
               INVALID KEY
                   MOVE CF-CIF-NO TO WK-ERR-KEY
                   PERFORM 8200-WRITE-ATTR-NONE-ERR
               NOT INVALID KEY
                   PERFORM 2300-CHECK-ATTRIBUTE
           END-READ
           IF FS-CMATTF NOT = WK-STAT-OK
              AND FS-CMATTF NOT = "23"
               DISPLAY "CMATTF 読込失敗 ST=" FS-CMATTF
               SET HARD-ERROR TO TRUE
           END-IF.

       2300-CHECK-ATTRIBUTE.
           MOVE "Y" TO WK-VALID-ATTR
           IF CA-ATTR-STATUS-KBN NOT = WK-ATTR-ACTIVE
               MOVE "N" TO WK-VALID-ATTR
               MOVE CF-CIF-NO TO WK-ERR-KEY
               PERFORM 8300-WRITE-ATTR-STAT-ERR
           END-IF
           IF CF-BIRTH-DT = SPACES OR CF-BIRTH-DT = ZEROES
               MOVE "N" TO WK-VALID-ATTR
           END-IF
           IF CF-SEX-KBN = SPACES OR CF-SEX-KBN = ZEROES
               MOVE "N" TO WK-VALID-ATTR
           END-IF
           IF CA-KANA-NAME = SPACES
               MOVE "N" TO WK-VALID-ATTR
           END-IF
           IF CA-ADDR-CD = SPACES OR CA-ADDR-CD = ZEROES
               MOVE "N" TO WK-VALID-ATTR
           END-IF
           IF WK-VALID-ATTR = "Y"
               PERFORM 2400-ADD-ENTRY
           ELSE
               MOVE CF-CIF-NO TO WK-ERR-KEY
               PERFORM 8400-WRITE-ATTR-LESS-ERR
           END-IF.

       2400-ADD-ENTRY.
           IF WK-ENTRY-CNT >= WK-MAX-ENTRY
               MOVE CF-CIF-NO TO WK-ERR-KEY
               PERFORM 8500-WRITE-TABLE-FULL-ERR
               DISPLAY "候補作成対象件数超過 CIF=" CF-CIF-NO
           ELSE
               ADD 1 TO WK-ENTRY-CNT
               MOVE CF-CIF-NO      TO TB-CIF-NO(WK-ENTRY-CNT)
               MOVE CF-BIRTH-DT    TO TB-BIRTH-DT(WK-ENTRY-CNT)
               MOVE CF-SEX-KBN     TO TB-SEX-KBN(WK-ENTRY-CNT)
               MOVE CA-KANA-NAME   TO TB-KANA-NAME(WK-ENTRY-CNT)
               MOVE CA-ADDR-CD     TO TB-ADDR-CD(WK-ENTRY-CNT)
           END-IF.

       3000-MAKE-CANDIDATE.
           IF WK-ENTRY-CNT < 2
               CONTINUE
           ELSE
               PERFORM VARYING IX1 FROM 1 BY 1
                   UNTIL IX1 >= WK-ENTRY-CNT OR HARD-ERROR
                   COMPUTE IX2 = IX1 + 1
                   PERFORM UNTIL IX2 > WK-ENTRY-CNT OR HARD-ERROR
                       PERFORM 3100-CHECK-PAIR
                       ADD 1 TO IX2
                   END-PERFORM
               END-PERFORM
           END-IF.

       3100-CHECK-PAIR.
           MOVE ZERO TO WK-SCORE
           IF TB-BIRTH-DT(IX1) = TB-BIRTH-DT(IX2)
               ADD 30 TO WK-SCORE
           END-IF
           IF TB-SEX-KBN(IX1) = TB-SEX-KBN(IX2)
               ADD 10 TO WK-SCORE
           END-IF
           IF TB-KANA-NAME(IX1) = TB-KANA-NAME(IX2)
               ADD 40 TO WK-SCORE
           END-IF
           IF TB-ADDR-CD(IX1) = TB-ADDR-CD(IX2)
               ADD 20 TO WK-SCORE
           END-IF
           IF WK-SCORE = WK-MAX-SCORE
               PERFORM 3200-WRITE-DUP
           END-IF.

       3200-WRITE-DUP.
           ADD 1 TO WK-CAND-SEQ
           STRING "D" WK-TODAY WK-CAND-SEQ
               DELIMITED BY SIZE
               INTO WK-CANDIDATE-ID
           END-STRING
           INITIALIZE CMDUPF-REC
           MOVE WK-CANDIDATE-ID TO DP-CANDIDATE-ID
           MOVE TB-CIF-NO(IX1)  TO DP-CIF-NO-1
           MOVE TB-CIF-NO(IX2)  TO DP-CIF-NO-2
           MOVE WK-SCORE        TO DP-MATCH-SCORE
           MOVE WK-JUDGE-AUTO   TO DP-JUDGE-KBN
           MOVE WK-TODAY        TO DP-CREATE-DT
           WRITE CMDUPF-REC
               INVALID KEY
                   DISPLAY "CMDUPF 候補ID重複 ID=" DP-CANDIDATE-ID
                            " ST=" FS-CMDUPF
                   SET HARD-ERROR TO TRUE
               NOT INVALID KEY
                   ADD 1 TO WK-DUP-CNT
           END-WRITE
           IF FS-CMDUPF NOT = WK-STAT-OK
              AND FS-CMDUPF NOT = WK-STAT-DUP
               DISPLAY "CMDUPF 書込失敗 ST=" FS-CMDUPF
               SET HARD-ERROR TO TRUE
           END-IF.

       8100-WRITE-CIF-STATUS-ERR.
           PERFORM 8000-SET-ERROR-ID
           INITIALIZE CKERRF-REC
           MOVE WK-ERROR-ID     TO ER-ERROR-ID
           MOVE WK-PGM-ID       TO ER-SOURCE-PGM-ID
           MOVE CF-CIF-NO       TO ER-CIF-NO
           MOVE WK-ERR-KEY      TO ER-KEY-ID
           MOVE WK-ERR-CIF-STAT TO ER-ERROR-CD
           MOVE WK-TODAY        TO ER-ERROR-DT
           PERFORM 8900-WRITE-ERROR.

       8200-WRITE-ATTR-NONE-ERR.
           PERFORM 8000-SET-ERROR-ID
           INITIALIZE CKERRF-REC
           MOVE WK-ERROR-ID      TO ER-ERROR-ID
           MOVE WK-PGM-ID        TO ER-SOURCE-PGM-ID
           MOVE CF-CIF-NO        TO ER-CIF-NO
           MOVE WK-ERR-KEY       TO ER-KEY-ID
           MOVE WK-ERR-ATTR-NONE TO ER-ERROR-CD
           MOVE WK-TODAY         TO ER-ERROR-DT
           PERFORM 8900-WRITE-ERROR.

       8300-WRITE-ATTR-STAT-ERR.
           PERFORM 8000-SET-ERROR-ID
           INITIALIZE CKERRF-REC
           MOVE WK-ERROR-ID      TO ER-ERROR-ID
           MOVE WK-PGM-ID        TO ER-SOURCE-PGM-ID
           MOVE CF-CIF-NO        TO ER-CIF-NO
           MOVE WK-ERR-KEY       TO ER-KEY-ID
           MOVE WK-ERR-ATTR-STAT TO ER-ERROR-CD
           MOVE WK-TODAY         TO ER-ERROR-DT
           PERFORM 8900-WRITE-ERROR.

       8400-WRITE-ATTR-LESS-ERR.
           PERFORM 8000-SET-ERROR-ID
           INITIALIZE CKERRF-REC
           MOVE WK-ERROR-ID      TO ER-ERROR-ID
           MOVE WK-PGM-ID        TO ER-SOURCE-PGM-ID
           MOVE CF-CIF-NO        TO ER-CIF-NO
           MOVE WK-ERR-KEY       TO ER-KEY-ID
           MOVE WK-ERR-ATTR-LESS TO ER-ERROR-CD
           MOVE WK-TODAY         TO ER-ERROR-DT
           PERFORM 8900-WRITE-ERROR.

       8500-WRITE-TABLE-FULL-ERR.
           PERFORM 8000-SET-ERROR-ID
           INITIALIZE CKERRF-REC
           MOVE WK-ERROR-ID      TO ER-ERROR-ID
           MOVE WK-PGM-ID        TO ER-SOURCE-PGM-ID
           MOVE CF-CIF-NO        TO ER-CIF-NO
           MOVE WK-ERR-KEY       TO ER-KEY-ID
           MOVE WK-ERR-TBL-FULL  TO ER-ERROR-CD
           MOVE WK-TODAY         TO ER-ERROR-DT
           PERFORM 8900-WRITE-ERROR.

       8000-SET-ERROR-ID.
           ADD 1 TO WK-ERR-SEQ
           STRING "E" WK-TODAY WK-ERR-SEQ
               DELIMITED BY SIZE
               INTO WK-ERROR-ID
           END-STRING.

       8900-WRITE-ERROR.
           WRITE CKERRF-REC
           IF FS-CKERRF = WK-STAT-OK
               ADD 1 TO WK-ERR-CNT
           ELSE
               DISPLAY "CKERRF 書込失敗 ST=" FS-CKERRF
               SET HARD-ERROR TO TRUE
           END-IF.

       9000-FINAL.
           CLOSE CMCIFF CMATTF CMDUPF CKERRF
           IF HARD-ERROR
               MOVE 8 TO RETURN-CODE
               DISPLAY "CM160B 異常終了 RC=8"
           ELSE
               MOVE 0 TO RETURN-CODE
               DISPLAY "CM160B 正常終了"
               DISPLAY "読込件数=" WK-READ-CNT
               DISPLAY "対象件数=" WK-ENTRY-CNT
               DISPLAY "候補件数=" WK-DUP-CNT
               DISPLAY "除外件数=" WK-SKIP-CNT
               DISPLAY "エラー件数=" WK-ERR-CNT
           END-IF.
