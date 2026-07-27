      *================================================================*
      * 変更履歴                                                       *
      * 版数  年月日    担当  概要                                     *
      * 1.00  20240115  開発  初版作成                                 *
      * 1.01  20240603  保守  端数処理を円未満切捨から入力金額準拠へ変更*
      * 1.02  20241021  保守  海外ATM利息は再計算せず明細金額を使用     *
      *================================================================*
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 CB320S.
       AUTHOR.                     MIRAI-CARD-BATCH.
       DATE-WRITTEN.               2024-01-15.
      *================================================================*
      * 入金配賦計算サブ                                               *
      * 対象カードの未精算明細を期日順に並べ、所定の優先順位で         *
      * 入金額を割り当てる。海外ATM固有利息は再計算しない。             *
      *================================================================*

       ENVIRONMENT                 DIVISION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
           SELECT CDOVSF
               ASSIGN TO "CDOVSF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WK-CDOVSF-ST.

       DATA                        DIVISION.
       FILE                        SECTION.
       FD  CDOVSF.
           COPY CDOVSFC.

       WORKING-STORAGE             SECTION.
       01  WK-FILE-STATUS.
           05  WK-CDOVSF-ST        PIC X(02) VALUE SPACE.

       01  WK-SWITCHES.
           05  WK-EOF-SW           PIC X VALUE SPACE.
               88  WK-EOF                VALUE "1".
               88  WK-NOT-EOF            VALUE SPACE.
           05  WK-ERROR-SW         PIC X VALUE SPACE.
               88  WK-ERROR              VALUE "1".
               88  WK-NORMAL             VALUE SPACE.

       01  WK-CONSTANTS.
           05  WK-MAX-IN           PIC 9(04) VALUE 1000.
           05  WK-MAX-OUT          PIC 9(04) VALUE 1500.
           05  WK-PGM-ID           PIC X(08) VALUE "CB320S  ".
           05  WK-SETL-D           PIC X VALUE "D".

       01  WK-COUNTERS.
           05  WK-IN-CNT           PIC 9(04) COMP VALUE ZERO.
           05  WK-OUT-CNT          PIC 9(04) COMP VALUE ZERO.
           05  WK-I                PIC 9(04) COMP VALUE ZERO.
           05  WK-J                PIC 9(04) COMP VALUE ZERO.
           05  WK-POS              PIC 9(04) COMP VALUE ZERO.

       01  WK-AMOUNTS.
           05  WK-PAY-REST         PIC S9(13) COMP-3 VALUE ZERO.
           05  WK-ALLOC-AMT        PIC S9(13) COMP-3 VALUE ZERO.
           05  WK-FEE-AMT          PIC S9(13) COMP-3 VALUE ZERO.
           05  WK-INT-AMT          PIC S9(13) COMP-3 VALUE ZERO.
           05  WK-PRN-AMT          PIC S9(13) COMP-3 VALUE ZERO.
           05  WK-WORK-AMT         PIC S9(13) COMP-3 VALUE ZERO.

       01  WK-INPUT-TABLE.
           05  WK-IN-REC OCCURS 1000 TIMES.
               10  WK-IN-TXN-ID    PIC X(20).
               10  WK-IN-DUE-DT    PIC 9(08).
               10  WK-IN-TXN-KBN   PIC X(02).
               10  WK-IN-FEE-KBN   PIC X(02).
               10  WK-IN-FEE       PIC S9(13) COMP-3.
               10  WK-IN-INT       PIC S9(13) COMP-3.
               10  WK-IN-PRN       PIC S9(13) COMP-3.

       01  WK-SORT-SAVE.
           05  WK-SV-TXN-ID        PIC X(20).
           05  WK-SV-DUE-DT        PIC 9(08).
           05  WK-SV-TXN-KBN       PIC X(02).
           05  WK-SV-FEE-KBN       PIC X(02).
           05  WK-SV-FEE           PIC S9(13) COMP-3.
           05  WK-SV-INT           PIC S9(13) COMP-3.
           05  WK-SV-PRN           PIC S9(13) COMP-3.

       LINKAGE                     SECTION.
       01  LK-CB320-PARM.
           05  LK-CARD-NO          PIC X(16).
           05  LK-NYUKIN-AMT       PIC S9(13) COMP-3.
           05  LK-RTN-CD           PIC X(02).
           05  LK-RTN-REASON       PIC X(60).
           05  LK-HAIFU-CNT        PIC 9(04) COMP.
           05  LK-HAIFU-REC OCCURS 1500 TIMES.
               10  LK-OUT-TXN-ID   PIC X(20).
               10  LK-OUT-DUE-DT   PIC 9(08).
               10  LK-OUT-TXN-KBN  PIC X(02).
               10  LK-OUT-KIN-KBN  PIC X(01).
               10  LK-OUT-AMT      PIC S9(13) COMP-3.

       PROCEDURE                   DIVISION USING LK-CB320-PARM.
       0000-MAIN.
           MOVE 0                  TO RETURN-CODE
           MOVE "00"               TO LK-RTN-CD
           MOVE SPACE              TO LK-RTN-REASON
           MOVE ZERO               TO LK-HAIFU-CNT
           SET WK-NORMAL           TO TRUE

           PERFORM 1000-VALIDATE

           IF WK-NORMAL
              PERFORM 2000-LOAD-CDOVSF
           END-IF

           IF WK-NORMAL
              PERFORM 3000-SORT-DETAIL
           END-IF

           IF WK-NORMAL
              PERFORM 4000-ALLOCATE
           END-IF

           IF WK-ERROR
              MOVE 8               TO RETURN-CODE
           END-IF

           GOBACK.

       1000-VALIDATE.
           IF LK-CARD-NO = SPACE
              MOVE "91"            TO LK-RTN-CD
              MOVE "カード番号未設定" TO LK-RTN-REASON
              SET WK-ERROR         TO TRUE
           END-IF

           IF WK-NORMAL AND LK-NYUKIN-AMT <= ZERO
              MOVE "92"            TO LK-RTN-CD
              MOVE "入金額不正"    TO LK-RTN-REASON
              SET WK-ERROR         TO TRUE
           END-IF.

       2000-LOAD-CDOVSF.
           OPEN INPUT CDOVSF

           IF WK-CDOVSF-ST NOT = "00"
              DISPLAY "CDOVSF オープン失敗 ST=" WK-CDOVSF-ST
              MOVE "93"            TO LK-RTN-CD
              STRING "CDOVSF オープン失敗 ST="
                     WK-CDOVSF-ST
                DELIMITED BY SIZE INTO LK-RTN-REASON
              SET WK-ERROR         TO TRUE
           END-IF

           IF WK-NORMAL
              SET WK-NOT-EOF       TO TRUE
              PERFORM UNTIL WK-EOF OR WK-ERROR
                 READ CDOVSF
                    AT END
                       SET WK-EOF  TO TRUE
                    NOT AT END
                       IF WK-CDOVSF-ST NOT = "00"
                          DISPLAY "CDOVSF 読込失敗 ST=" WK-CDOVSF-ST
                          MOVE "94" TO LK-RTN-CD
                          STRING "CDOVSF 読込失敗 ST="
                                 WK-CDOVSF-ST
                            DELIMITED BY SIZE INTO LK-RTN-REASON
                          SET WK-ERROR TO TRUE
                       ELSE
                          PERFORM 2100-ACCEPT-DETAIL
                       END-IF
                 END-READ
              END-PERFORM
           END-IF

           IF WK-CDOVSF-ST NOT = "00" AND WK-CDOVSF-ST NOT = "10"
              CONTINUE
           END-IF

           CLOSE CDOVSF

           IF WK-NORMAL AND WK-CDOVSF-ST NOT = "00"
              DISPLAY "CDOVSF クローズ失敗 ST=" WK-CDOVSF-ST
              MOVE "95"            TO LK-RTN-CD
              STRING "CDOVSF クローズ失敗 ST="
                     WK-CDOVSF-ST
                DELIMITED BY SIZE INTO LK-RTN-REASON
              SET WK-ERROR         TO TRUE
           END-IF.

       2100-ACCEPT-DETAIL.
           IF OV-CARD-NO = LK-CARD-NO
              AND OV-SETL-KBN = WK-SETL-D
              AND OV-SETL-AMT > ZERO
              PERFORM 2200-EDIT-DOMAIN
           END-IF.

       2200-EDIT-DOMAIN.
           IF OV-TXN-KBN NOT = "P1" AND OV-TXN-KBN NOT = "P2"
              AND OV-TXN-KBN NOT = "C1" AND OV-TXN-KBN NOT = "C2"
              AND OV-TXN-KBN NOT = "A1"
              DISPLAY "取引区分不正 TXN-ID=" OV-TXN-ID
              MOVE "96"            TO LK-RTN-CD
              MOVE "取引区分不正"  TO LK-RTN-REASON
              SET WK-ERROR         TO TRUE
           END-IF

           IF WK-NORMAL
              IF OV-FEE-KBN NOT = "00" AND OV-FEE-KBN NOT = "FA"
                 AND OV-FEE-KBN NOT = "FB"
                 DISPLAY "手数料区分不正 TXN-ID=" OV-TXN-ID
                 MOVE "97"         TO LK-RTN-CD
                 MOVE "手数料区分不正" TO LK-RTN-REASON
                 SET WK-ERROR      TO TRUE
              END-IF
           END-IF

           IF WK-NORMAL
              IF OV-FEE-AMT < ZERO OR OV-SETL-AMT < OV-FEE-AMT
                 DISPLAY "明細金額不正 TXN-ID=" OV-TXN-ID
                 MOVE "98"         TO LK-RTN-CD
                 MOVE "明細金額不正" TO LK-RTN-REASON
                 SET WK-ERROR      TO TRUE
              ELSE
                 PERFORM 2300-STORE-DETAIL
              END-IF
           END-IF.

       2300-STORE-DETAIL.
           IF WK-IN-CNT >= WK-MAX-IN
              DISPLAY "対象明細件数超過"
              MOVE "99"            TO LK-RTN-CD
              MOVE "対象明細件数超過" TO LK-RTN-REASON
              SET WK-ERROR         TO TRUE
           ELSE
              ADD 1                TO WK-IN-CNT
              MOVE OV-TXN-ID       TO WK-IN-TXN-ID  (WK-IN-CNT)
              MOVE OV-INT-START-DT TO WK-IN-DUE-DT  (WK-IN-CNT)
              MOVE OV-TXN-KBN      TO WK-IN-TXN-KBN (WK-IN-CNT)
              MOVE OV-FEE-KBN      TO WK-IN-FEE-KBN (WK-IN-CNT)
              MOVE OV-FEE-AMT      TO WK-IN-FEE     (WK-IN-CNT)
              MOVE ZERO            TO WK-IN-INT     (WK-IN-CNT)
              COMPUTE WK-IN-PRN (WK-IN-CNT) =
                      OV-SETL-AMT - OV-FEE-AMT
           END-IF.

       3000-SORT-DETAIL.
           IF WK-IN-CNT > 1
              PERFORM VARYING WK-I FROM 2 BY 1 UNTIL WK-I > WK-IN-CNT
                 MOVE WK-IN-TXN-ID  (WK-I) TO WK-SV-TXN-ID
                 MOVE WK-IN-DUE-DT  (WK-I) TO WK-SV-DUE-DT
                 MOVE WK-IN-TXN-KBN (WK-I) TO WK-SV-TXN-KBN
                 MOVE WK-IN-FEE-KBN (WK-I) TO WK-SV-FEE-KBN
                 MOVE WK-IN-FEE     (WK-I) TO WK-SV-FEE
                 MOVE WK-IN-INT     (WK-I) TO WK-SV-INT
                 MOVE WK-IN-PRN     (WK-I) TO WK-SV-PRN
                 COMPUTE WK-J = WK-I - 1

                 PERFORM UNTIL WK-J = ZERO
                    OR WK-IN-DUE-DT (WK-J) <= WK-SV-DUE-DT
                    COMPUTE WK-POS = WK-J + 1
                    MOVE WK-IN-TXN-ID  (WK-J)
                                         TO WK-IN-TXN-ID  (WK-POS)
                    MOVE WK-IN-DUE-DT  (WK-J)
                                         TO WK-IN-DUE-DT  (WK-POS)
                    MOVE WK-IN-TXN-KBN (WK-J)
                                         TO WK-IN-TXN-KBN (WK-POS)
                    MOVE WK-IN-FEE-KBN (WK-J)
                                         TO WK-IN-FEE-KBN (WK-POS)
                    MOVE WK-IN-FEE     (WK-J)
                                         TO WK-IN-FEE     (WK-POS)
                    MOVE WK-IN-INT     (WK-J)
                                         TO WK-IN-INT     (WK-POS)
                    MOVE WK-IN-PRN     (WK-J)
                                         TO WK-IN-PRN     (WK-POS)
                    SUBTRACT 1 FROM WK-J
                 END-PERFORM

                 COMPUTE WK-POS = WK-J + 1
                 MOVE WK-SV-TXN-ID     TO WK-IN-TXN-ID  (WK-POS)
                 MOVE WK-SV-DUE-DT     TO WK-IN-DUE-DT  (WK-POS)
                 MOVE WK-SV-TXN-KBN    TO WK-IN-TXN-KBN (WK-POS)
                 MOVE WK-SV-FEE-KBN    TO WK-IN-FEE-KBN (WK-POS)
                 MOVE WK-SV-FEE        TO WK-IN-FEE     (WK-POS)
                 MOVE WK-SV-INT        TO WK-IN-INT     (WK-POS)
                 MOVE WK-SV-PRN        TO WK-IN-PRN     (WK-POS)
              END-PERFORM
           END-IF.

       4000-ALLOCATE.
           MOVE LK-NYUKIN-AMT       TO WK-PAY-REST

           PERFORM VARYING WK-I FROM 1 BY 1
             UNTIL WK-I > WK-IN-CNT OR WK-PAY-REST <= ZERO OR WK-ERROR

              MOVE WK-IN-FEE (WK-I) TO WK-WORK-AMT
              IF WK-WORK-AMT > ZERO
                 PERFORM 4100-ADD-FEE
              END-IF

              MOVE WK-IN-INT (WK-I) TO WK-WORK-AMT
              IF WK-WORK-AMT > ZERO AND WK-PAY-REST > ZERO
                 PERFORM 4200-ADD-INT
              END-IF

              MOVE WK-IN-PRN (WK-I) TO WK-WORK-AMT
              IF WK-WORK-AMT > ZERO AND WK-PAY-REST > ZERO
                 PERFORM 4300-ADD-PRN
              END-IF
           END-PERFORM

           IF WK-NORMAL
              MOVE WK-OUT-CNT       TO LK-HAIFU-CNT
           END-IF.

       4100-ADD-FEE.
           PERFORM 4400-DECIDE-AMT
           IF WK-NORMAL
              MOVE "F"              TO LK-OUT-KIN-KBN (WK-OUT-CNT)
           END-IF.

       4200-ADD-INT.
           PERFORM 4400-DECIDE-AMT
           IF WK-NORMAL
              MOVE "I"              TO LK-OUT-KIN-KBN (WK-OUT-CNT)
           END-IF.

       4300-ADD-PRN.
           PERFORM 4400-DECIDE-AMT
           IF WK-NORMAL
              MOVE "G"              TO LK-OUT-KIN-KBN (WK-OUT-CNT)
           END-IF.

       4400-DECIDE-AMT.
           IF WK-PAY-REST >= WK-WORK-AMT
              MOVE WK-WORK-AMT      TO WK-ALLOC-AMT
           ELSE
              MOVE WK-PAY-REST      TO WK-ALLOC-AMT
           END-IF

           IF WK-OUT-CNT >= WK-MAX-OUT
              DISPLAY "配賦明細件数超過"
              MOVE "99"             TO LK-RTN-CD
              MOVE "配賦明細件数超過" TO LK-RTN-REASON
              SET WK-ERROR          TO TRUE
           ELSE
              ADD 1                 TO WK-OUT-CNT
              MOVE WK-IN-TXN-ID  (WK-I)
                                      TO LK-OUT-TXN-ID  (WK-OUT-CNT)
              MOVE WK-IN-DUE-DT  (WK-I)
                                      TO LK-OUT-DUE-DT  (WK-OUT-CNT)
              MOVE WK-IN-TXN-KBN (WK-I)
                                      TO LK-OUT-TXN-KBN (WK-OUT-CNT)
              MOVE WK-ALLOC-AMT      TO LK-OUT-AMT      (WK-OUT-CNT)
              SUBTRACT WK-ALLOC-AMT FROM WK-PAY-REST
           END-IF.
