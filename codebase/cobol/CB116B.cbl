       IDENTIFICATION DIVISION.
       PROGRAM-ID. CB116B.
      ******************************************************************
      * 変更履歴
      * 版数  年月日    担当    概要
      * 0.1   20240115  基盤    初版作成。状態遷移表を実装
      * 0.2   20240701  業務    入金済み時の注意解除条件を追加
      * 0.3   20250120  業務    ９０日超延滞を解約予定へ改定
      ******************************************************************
      * 会員ステータス一括更新
      * 延滞抽出結果と入金消込結果を突合し、会員状態を更新する。
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CDDELF ASSIGN TO "CDDELF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-CDDELF.

           SELECT CDPYMF ASSIGN TO "CDPYMF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS PY-PAYMENT-ID
               FILE STATUS IS FS-CDPYMF.

           SELECT CDACCF ASSIGN TO "CDACCF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS AC-CARD-NO
               FILE STATUS IS FS-CDACCF.

           SELECT CDLOGF ASSIGN TO "CDLOGF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS FS-CDLOGF.

       DATA DIVISION.
       FILE SECTION.
       FD  CDDELF.
           COPY CDDELFC.

       FD  CDPYMF.
           COPY CDPYMFC.

       FD  CDACCF.
           COPY CDACCFC.

       FD  CDLOGF.
           COPY CDLOGFC.

       WORKING-STORAGE SECTION.
       01  WK-PROGRAM-ID              PIC X(08) VALUE "CB116B".
       01  WK-ABEND-FLG               PIC X(01) VALUE "N".
           88  ABEND-OFF                       VALUE "N".
           88  ABEND-ON                        VALUE "Y".

       01  WK-EOF-FLG.
           05  WK-EOF-DELAY           PIC X(01) VALUE "N".
               88  DELAY-EOF                   VALUE "Y".
               88  DELAY-NOT-EOF               VALUE "N".
           05  WK-EOF-PAYMENT         PIC X(01) VALUE "N".
               88  PAYMENT-EOF                 VALUE "Y".
               88  PAYMENT-NOT-EOF             VALUE "N".

       01  FILE-STATUS-AREA.
           05  FS-CDDELF              PIC X(02) VALUE SPACE.
           05  FS-CDPYMF              PIC X(02) VALUE SPACE.
           05  FS-CDACCF              PIC X(02) VALUE SPACE.
           05  FS-CDLOGF              PIC X(02) VALUE SPACE.

       01  WK-COUNTERS.
           05  CNT-DELAY-READ         PIC 9(09) VALUE ZERO.
           05  CNT-PAY-READ           PIC 9(09) VALUE ZERO.
           05  CNT-ACC-READ           PIC 9(09) VALUE ZERO.
           05  CNT-ACC-UPDATE         PIC 9(09) VALUE ZERO.
           05  CNT-LOG-WRITE          PIC 9(09) VALUE ZERO.
           05  CNT-SKIP               PIC 9(09) VALUE ZERO.
           05  CNT-ERR                PIC 9(09) VALUE ZERO.

       01  WK-PAY-TABLE.
           05  WK-PAY-ENTRY OCCURS 5000 TIMES.
               10  WK-TB-CARD-NO      PIC X(16) VALUE SPACE.
               10  WK-TB-PAY-AMT      PIC S9(11)V99 COMP-3 VALUE ZERO.
               10  WK-TB-UNAPP-AMT    PIC S9(11)V99 COMP-3 VALUE ZERO.
               10  WK-TB-PAY-DT       PIC 9(08) VALUE ZERO.

       01  WK-PAY-WORK.
           05  WK-PAY-MAX             PIC 9(04) COMP VALUE ZERO.
           05  WK-PAY-IDX             PIC 9(04) COMP VALUE ZERO.
           05  WK-TOTAL-PAY-AMT       PIC S9(11)V99 COMP-3 VALUE ZERO.
           05  WK-TOTAL-UNAPP-AMT     PIC S9(11)V99 COMP-3 VALUE ZERO.
           05  WK-LAST-PAY-DT         PIC 9(08) VALUE ZERO.

       01  WK-STATUS-WORK.
           05  WK-OLD-STATUS          PIC X(01) VALUE SPACE.
           05  WK-NEW-STATUS          PIC X(01) VALUE SPACE.
           05  WK-DETAIL-CD           PIC X(08) VALUE SPACE.
           05  WK-EVENT-KBN           PIC X(02) VALUE SPACE.
           05  WK-PAID-OFF-FLG        PIC X(01) VALUE "N".
               88  PAID-OFF                    VALUE "Y".
               88  NOT-PAID-OFF                VALUE "N".
           05  WK-CHANGE-FLG          PIC X(01) VALUE "N".
               88  STATUS-CHANGED              VALUE "Y".
               88  STATUS-NOT-CHANGED          VALUE "N".

       01  WK-DATE-WORK.
           05  WK-CURRENT-DATE        PIC 9(08) VALUE ZERO.
           05  WK-CURRENT-TIME        PIC 9(06) VALUE ZERO.

       01  WK-LOG-WORK.
           05  WK-LOG-SEQ             PIC 9(10) VALUE ZERO.
           05  WK-LOG-ID-WORK         PIC X(20) VALUE SPACE.

       01  STATUS-CONSTANTS.
           05  STS-NORMAL             PIC X(01) VALUE "0".
           05  STS-CAUTION            PIC X(01) VALUE "1".
           05  STS-STOP               PIC X(01) VALUE "2".
           05  STS-CANCEL-PLAN        PIC X(01) VALUE "3".
           05  DLY-NONE               PIC X(01) VALUE "0".
           05  DLY-HAS                PIC X(01) VALUE "1".

       01  EDIT-AREA.
           05  ED-CNT                 PIC Z(08)9.

       PROCEDURE DIVISION.
       MAIN-SECTION.
           PERFORM 0100-INITIALIZE
           IF ABEND-OFF
              PERFORM 1000-MAIN-PROCESS
           END-IF
           PERFORM 9000-TERMINATE
           GOBACK.

       0100-INITIALIZE.
           MOVE 0 TO RETURN-CODE
           ACCEPT WK-CURRENT-DATE FROM DATE YYYYMMDD
           ACCEPT WK-CURRENT-TIME FROM TIME

           OPEN INPUT CDDELF
           IF FS-CDDELF NOT = "00"
              DISPLAY "CDDELF オープン失敗 ST=" FS-CDDELF
              PERFORM 9900-ABEND
           END-IF

           IF ABEND-OFF
              OPEN INPUT CDPYMF
              IF FS-CDPYMF NOT = "00"
                 DISPLAY "CDPYMF オープン失敗 ST=" FS-CDPYMF
                 PERFORM 9900-ABEND
              END-IF
           END-IF

           IF ABEND-OFF
              OPEN I-O CDACCF
              IF FS-CDACCF NOT = "00"
                 DISPLAY "CDACCF オープン失敗 ST=" FS-CDACCF
                 PERFORM 9900-ABEND
              END-IF
           END-IF

           IF ABEND-OFF
              OPEN EXTEND CDLOGF
              IF FS-CDLOGF NOT = "00"
                 DISPLAY "CDLOGF オープン失敗 ST=" FS-CDLOGF
                 PERFORM 9900-ABEND
              END-IF
           END-IF

           IF ABEND-OFF
              PERFORM 0200-LOAD-PAYMENT
           END-IF.

       0200-LOAD-PAYMENT.
           MOVE "N" TO WK-EOF-PAYMENT
           MOVE LOW-VALUES TO PY-PAYMENT-ID

           START CDPYMF KEY IS NOT LESS THAN PY-PAYMENT-ID
              INVALID KEY
                 MOVE "Y" TO WK-EOF-PAYMENT
           END-START

           IF FS-CDPYMF NOT = "00" AND FS-CDPYMF NOT = "23"
              DISPLAY "CDPYMF START失敗 ST=" FS-CDPYMF
              PERFORM 9900-ABEND
           END-IF

           PERFORM UNTIL PAYMENT-EOF OR ABEND-ON
              READ CDPYMF NEXT RECORD
                 AT END
                    MOVE "Y" TO WK-EOF-PAYMENT
                 NOT AT END
                    ADD 1 TO CNT-PAY-READ
                    PERFORM 0210-STORE-PAYMENT
              END-READ
              IF FS-CDPYMF NOT = "00" AND FS-CDPYMF NOT = "10"
                 DISPLAY "CDPYMF 読込失敗 ST=" FS-CDPYMF
                 PERFORM 9900-ABEND
              END-IF
           END-PERFORM.

       0210-STORE-PAYMENT.
           IF PY-CARD-NO = SPACE
              ADD 1 TO CNT-SKIP
              DISPLAY "入金カード番号未設定 ID=" PY-PAYMENT-ID
           ELSE
              IF PY-ALLOC-KBN = "1" OR PY-ALLOC-KBN = "2"
                 IF WK-PAY-MAX < 5000
                    ADD 1 TO WK-PAY-MAX
                    MOVE PY-CARD-NO TO WK-TB-CARD-NO(WK-PAY-MAX)
                    MOVE PY-PAY-AMT TO WK-TB-PAY-AMT(WK-PAY-MAX)
                    MOVE PY-UNAPPLIED-AMT TO
                         WK-TB-UNAPP-AMT(WK-PAY-MAX)
                    MOVE PY-PAY-DT TO WK-TB-PAY-DT(WK-PAY-MAX)
                 ELSE
                    DISPLAY "入金表件数超過"
                    PERFORM 9900-ABEND
                 END-IF
              END-IF
           END-IF.

       1000-MAIN-PROCESS.
           MOVE "N" TO WK-EOF-DELAY
           PERFORM UNTIL DELAY-EOF OR ABEND-ON
              READ CDDELF
                 AT END
                    MOVE "Y" TO WK-EOF-DELAY
                 NOT AT END
                    ADD 1 TO CNT-DELAY-READ
                    PERFORM 1100-PROCESS-DELAY
              END-READ
              IF FS-CDDELF NOT = "00" AND FS-CDDELF NOT = "10"
                 DISPLAY "CDDELF 読込失敗 ST=" FS-CDDELF
                 PERFORM 9900-ABEND
              END-IF
           END-PERFORM.

       1100-PROCESS-DELAY.
           IF DL-CARD-NO = SPACE
              ADD 1 TO CNT-SKIP
              DISPLAY "延滞カード番号未設定"
           ELSE
              MOVE DL-CARD-NO TO AC-CARD-NO
              READ CDACCF
                 INVALID KEY
                    ADD 1 TO CNT-ERR
                    DISPLAY "会員口座なし CARD=" DL-CARD-NO
                 NOT INVALID KEY
                    ADD 1 TO CNT-ACC-READ
                    PERFORM 1200-EVALUATE-STATUS
              END-READ
              IF FS-CDACCF NOT = "00" AND FS-CDACCF NOT = "23"
                 DISPLAY "CDACCF 読込失敗 ST=" FS-CDACCF
                 PERFORM 9900-ABEND
              END-IF
           END-IF.

       1200-EVALUATE-STATUS.
           MOVE AC-STATUS-KBN TO WK-OLD-STATUS
           MOVE AC-STATUS-KBN TO WK-NEW-STATUS
           MOVE "N" TO WK-CHANGE-FLG
           MOVE "N" TO WK-PAID-OFF-FLG
           MOVE SPACE TO WK-DETAIL-CD
           MOVE SPACE TO WK-EVENT-KBN
           MOVE ZERO TO WK-TOTAL-PAY-AMT
           MOVE ZERO TO WK-TOTAL-UNAPP-AMT
           MOVE ZERO TO WK-LAST-PAY-DT

           IF AC-STATUS-KBN NOT = STS-NORMAL
              AND AC-STATUS-KBN NOT = STS-CAUTION
              AND AC-STATUS-KBN NOT = STS-STOP
              AND AC-STATUS-KBN NOT = STS-CANCEL-PLAN
              ADD 1 TO CNT-ERR
              DISPLAY "会員状態不正 CARD=" AC-CARD-NO
                      " ST=" AC-STATUS-KBN
           ELSE
              PERFORM 1210-SUM-PAYMENT
              IF WK-TOTAL-PAY-AMT >= DL-DUE-AMT
                 AND WK-TOTAL-UNAPP-AMT = ZERO
                 MOVE "Y" TO WK-PAID-OFF-FLG
              END-IF
              PERFORM 1220-DECIDE-STATUS
              IF STATUS-CHANGED
                 PERFORM 1300-UPDATE-ACCOUNT
              END-IF
           END-IF.

       1210-SUM-PAYMENT.
           PERFORM VARYING WK-PAY-IDX FROM 1 BY 1
             UNTIL WK-PAY-IDX > WK-PAY-MAX
              IF WK-TB-CARD-NO(WK-PAY-IDX) = DL-CARD-NO
                 ADD WK-TB-PAY-AMT(WK-PAY-IDX) TO WK-TOTAL-PAY-AMT
                 ADD WK-TB-UNAPP-AMT(WK-PAY-IDX) TO
                     WK-TOTAL-UNAPP-AMT
                 IF WK-TB-PAY-DT(WK-PAY-IDX) > WK-LAST-PAY-DT
                    MOVE WK-TB-PAY-DT(WK-PAY-IDX) TO WK-LAST-PAY-DT
                 END-IF
              END-IF
           END-PERFORM.

       1220-DECIDE-STATUS.
           IF AC-STATUS-KBN = STS-CANCEL-PLAN
              IF PAID-OFF
                 MOVE "CNLPAY" TO WK-DETAIL-CD
              ELSE
                 MOVE "CNLHOLD" TO WK-DETAIL-CD
              END-IF
           ELSE
              IF PAID-OFF
                 IF AC-STATUS-KBN = STS-CAUTION
                    OR AC-STATUS-KBN = STS-STOP
                    MOVE STS-NORMAL TO WK-NEW-STATUS
                    MOVE "Y" TO WK-CHANGE-FLG
                    MOVE "PAYCLR" TO WK-DETAIL-CD
                    MOVE "10" TO WK-EVENT-KBN
                 ELSE
                    MOVE "PAYNML" TO WK-DETAIL-CD
                 END-IF
              ELSE
                 EVALUATE TRUE
                    WHEN DL-DELAY-DAYS > 90
                       IF AC-STATUS-KBN = STS-STOP
                          MOVE STS-CANCEL-PLAN TO WK-NEW-STATUS
                          MOVE "Y" TO WK-CHANGE-FLG
                          MOVE "DLY090" TO WK-DETAIL-CD
                          MOVE "30" TO WK-EVENT-KBN
                       ELSE
                          MOVE "DLY090K" TO WK-DETAIL-CD
                       END-IF
                    WHEN DL-DELAY-DAYS >= 30
                       IF AC-STATUS-KBN = STS-CAUTION
                          MOVE STS-STOP TO WK-NEW-STATUS
                          MOVE "Y" TO WK-CHANGE-FLG
                          MOVE "DLY030" TO WK-DETAIL-CD
                          MOVE "20" TO WK-EVENT-KBN
                       ELSE
                          MOVE "DLY030K" TO WK-DETAIL-CD
                       END-IF
                    WHEN DL-DELAY-DAYS > 0
                       IF AC-STATUS-KBN = STS-NORMAL
                          MOVE STS-CAUTION TO WK-NEW-STATUS
                          MOVE "Y" TO WK-CHANGE-FLG
                          MOVE "DLY001" TO WK-DETAIL-CD
                          MOVE "20" TO WK-EVENT-KBN
                       ELSE
                          MOVE "DLY001K" TO WK-DETAIL-CD
                       END-IF
                    WHEN OTHER
                       MOVE "DLYZERO" TO WK-DETAIL-CD
                 END-EVALUATE
              END-IF
           END-IF.

       1300-UPDATE-ACCOUNT.
           MOVE WK-NEW-STATUS TO AC-STATUS-KBN
           IF WK-NEW-STATUS = STS-NORMAL
              MOVE DLY-NONE TO AC-DELAY-KBN
           ELSE
              MOVE DLY-HAS TO AC-DELAY-KBN
           END-IF
           MOVE WK-CURRENT-DATE TO AC-LAST-UPD-DT

           REWRITE CDACCF-REC
           IF FS-CDACCF = "00"
              ADD 1 TO CNT-ACC-UPDATE
              PERFORM 1400-WRITE-LOG
           ELSE
              DISPLAY "CDACCF 更新失敗 CARD=" AC-CARD-NO
                      " ST=" FS-CDACCF
              PERFORM 9900-ABEND
           END-IF.

       1400-WRITE-LOG.
           ADD 1 TO WK-LOG-SEQ
           MOVE SPACE TO CDLOGF-REC
           STRING WK-CURRENT-DATE DELIMITED BY SIZE
                  WK-LOG-SEQ DELIMITED BY SIZE
             INTO WK-LOG-ID-WORK
           END-STRING
           MOVE WK-LOG-ID-WORK TO LG-LOG-ID
           MOVE WK-PROGRAM-ID TO LG-PROGRAM-ID
           MOVE AC-CARD-NO TO LG-CARD-NO
           MOVE WK-EVENT-KBN TO LG-EVENT-KBN
           MOVE WK-CURRENT-DATE TO LG-EVENT-DT
           MOVE WK-DETAIL-CD TO LG-DETAIL-CD

           WRITE CDLOGF-REC
           IF FS-CDLOGF = "00"
              ADD 1 TO CNT-LOG-WRITE
           ELSE
              DISPLAY "CDLOGF 書込失敗 CARD=" AC-CARD-NO
                      " ST=" FS-CDLOGF
              PERFORM 9900-ABEND
           END-IF.

       9000-TERMINATE.
           IF FS-CDDELF NOT = SPACE
              CLOSE CDDELF
           END-IF
           IF FS-CDPYMF NOT = SPACE
              CLOSE CDPYMF
           END-IF
           IF FS-CDACCF NOT = SPACE
              CLOSE CDACCF
           END-IF
           IF FS-CDLOGF NOT = SPACE
              CLOSE CDLOGF
           END-IF

           MOVE CNT-DELAY-READ TO ED-CNT
           DISPLAY "延滞読込件数=" ED-CNT
           MOVE CNT-PAY-READ TO ED-CNT
           DISPLAY "入金読込件数=" ED-CNT
           MOVE CNT-ACC-UPDATE TO ED-CNT
           DISPLAY "会員更新件数=" ED-CNT
           MOVE CNT-LOG-WRITE TO ED-CNT
           DISPLAY "ログ出力件数=" ED-CNT
           MOVE CNT-ERR TO ED-CNT
           DISPLAY "業務エラー件数=" ED-CNT

           IF ABEND-ON
              MOVE 8 TO RETURN-CODE
           ELSE
              MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           SET ABEND-ON TO TRUE
           MOVE 8 TO RETURN-CODE.
