       IDENTIFICATION DIVISION.
       PROGRAM-ID. KZ233B.
      * 変更履歴
      * 版数  年月日(和暦)  担当                      概要
      * 1.00  H28.04.01    システム部 勘定系チーム  新規作成
      * 1.10  R02.01.06    システム部 勘定系チーム  税率判定条件追加
      * 1.20  R05.10.02    システム部 勘定系チーム  抽出対象区分見直し
       AUTHOR. KZ-BATCH.
       INSTALLATION. みらい信託銀行.
       DATE-WRITTEN. 2026-06-01.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZTXIF
               ASSIGN TO "KZTXIF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS FS-KZTXIF.
           SELECT KZNRMF
               ASSIGN TO "KZNRMF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS NR-ACCT-NO
               FILE STATUS IS FS-KZNRMF.
           SELECT KZADLF
               ASSIGN TO "KZADLF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS FS-KZADLF.

       DATA DIVISION.
       FILE SECTION.
       FD  KZTXIF.
           COPY KZTXIFC.
       FD  KZNRMF.
           COPY KZNRMFC.
       FD  KZADLF.
           COPY KZADLFC.

       WORKING-STORAGE SECTION.
       01  FS-KZTXIF                    PIC XX VALUE SPACE.
       01  FS-KZNRMF                    PIC XX VALUE SPACE.
       01  FS-KZADLF                    PIC XX VALUE SPACE.

       01  WK-FLAGS.
           05  WK-EOF-SW                PIC X VALUE "N".
               88  WK-EOF                    VALUE "Y".
           05  WK-NR-FOUND-SW           PIC X VALUE "N".
               88  WK-NR-FOUND               VALUE "Y".
           05  WK-TARGET-SW             PIC X VALUE "N".
               88  WK-TARGET                 VALUE "Y".
           05  WK-AUDIT-NEED-SW         PIC X VALUE "N".
               88  WK-AUDIT-NEED             VALUE "Y".

       01  WK-RUN-DATE                  PIC 9(08) VALUE ZERO.
       01  WK-PAY-DATE                  PIC 9(08) VALUE ZERO.
       01  WK-AUDIT-SEQ                 PIC 9(10) VALUE ZERO.
       01  WK-ERR-COUNT                 PIC 9(09) VALUE ZERO.
       01  WK-IN-COUNT                  PIC 9(09) VALUE ZERO.
       01  WK-OUT-COUNT                 PIC 9(09) VALUE ZERO.
       01  WK-AUDIT-COUNT               PIC 9(09) VALUE ZERO.
       01  WK-SKIP-COUNT                PIC 9(09) VALUE ZERO.

       01  WK-DATE-AREA.
           05  WK-DATE-YYYY             PIC 9(04).
           05  WK-DATE-MM               PIC 9(02).
           05  WK-DATE-DD               PIC 9(02).

       01  WK-REASON-CD                 PIC X(02) VALUE SPACE.
       01  WK-SOURCE-DSID               PIC X(08) VALUE "KZ233B".
       01  WK-ZERO-AMT                  PIC S9(13)V99 COMP-3 VALUE ZERO.

       PROCEDURE DIVISION.
       MAIN-SECTION.
           PERFORM 0000-INIT
           IF RETURN-CODE NOT = 0
               GOBACK
           END-IF

           PERFORM 1000-MAIN-LOOP UNTIL WK-EOF

           PERFORM 9000-FINAL
           GOBACK.

       0000-INIT.
           MOVE 0 TO RETURN-CODE
           ACCEPT WK-DATE-AREA FROM DATE YYYYMMDD
           MOVE WK-DATE-AREA TO WK-RUN-DATE
           MOVE WK-RUN-DATE TO WK-PAY-DATE

           OPEN INPUT KZTXIF
           IF FS-KZTXIF NOT = "00"
               DISPLAY "KZTXIF オープン失敗 ST=" FS-KZTXIF
               MOVE 12 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF

           OPEN INPUT KZNRMF
           IF FS-KZNRMF NOT = "00"
               DISPLAY "KZNRMF オープン失敗 ST=" FS-KZNRMF
               MOVE 12 TO RETURN-CODE
               PERFORM 9100-CLOSE-KZTXIF
               EXIT PARAGRAPH
           END-IF

           OPEN EXTEND KZADLF
           IF FS-KZADLF NOT = "00"
               DISPLAY "KZADLF オープン失敗 ST=" FS-KZADLF
               MOVE 12 TO RETURN-CODE
               PERFORM 9100-CLOSE-KZTXIF
               PERFORM 9200-CLOSE-KZNRMF
               EXIT PARAGRAPH
           END-IF

           PERFORM 1100-READ-KZTXIF.

       1000-MAIN-LOOP.
           ADD 1 TO WK-IN-COUNT
           MOVE "N" TO WK-TARGET-SW
           MOVE "N" TO WK-AUDIT-NEED-SW
           MOVE SPACE TO WK-REASON-CD

           PERFORM 2000-VALIDATE-INPUT

           IF WK-AUDIT-NEED
               PERFORM 4000-WRITE-AUDIT
           ELSE
               PERFORM 3000-CHECK-NONRESIDENT
               IF WK-AUDIT-NEED
                   PERFORM 4000-WRITE-AUDIT
               END-IF
               IF WK-TARGET
                   PERFORM 5000-WRITE-TARGET
               ELSE
                   ADD 1 TO WK-SKIP-COUNT
               END-IF
           END-IF

           PERFORM 1100-READ-KZTXIF.

       1100-READ-KZTXIF.
           READ KZTXIF
               AT END
                   MOVE "Y" TO WK-EOF-SW
               NOT AT END
                   IF FS-KZTXIF NOT = "00"
                       DISPLAY "KZTXIF 読込失敗 ST=" FS-KZTXIF
                       MOVE 12 TO RETURN-CODE
                       MOVE "Y" TO WK-EOF-SW
                   END-IF
           END-READ.

       2000-VALIDATE-INPUT.
           IF TI-ACCT-NO = SPACE
               MOVE "01" TO WK-REASON-CD
               MOVE "Y" TO WK-AUDIT-NEED-SW
               EXIT PARAGRAPH
           END-IF

           IF TI-ACCT-TYPE NOT = "01"
              AND TI-ACCT-TYPE NOT = "02"
              AND TI-ACCT-TYPE NOT = "03"
               MOVE "02" TO WK-REASON-CD
               MOVE "Y" TO WK-AUDIT-NEED-SW
               EXIT PARAGRAPH
           END-IF

           IF TI-INT-AMT <= ZERO
               MOVE "03" TO WK-REASON-CD
               MOVE "Y" TO WK-AUDIT-NEED-SW
               EXIT PARAGRAPH
           END-IF.

       3000-CHECK-NONRESIDENT.
           MOVE "N" TO WK-NR-FOUND-SW
           MOVE TI-ACCT-NO TO NR-ACCT-NO
           READ KZNRMF KEY IS NR-ACCT-NO
               INVALID KEY
                   MOVE "N" TO WK-NR-FOUND-SW
               NOT INVALID KEY
                   MOVE "Y" TO WK-NR-FOUND-SW
           END-READ

           IF FS-KZNRMF NOT = "00" AND FS-KZNRMF NOT = "23"
               DISPLAY "KZNRMF 読込失敗 ST=" FS-KZNRMF
               MOVE 12 TO RETURN-CODE
               MOVE "Y" TO WK-EOF-SW
               EXIT PARAGRAPH
           END-IF

           IF NOT WK-NR-FOUND
               EXIT PARAGRAPH
           END-IF

           IF NR-CERT-RECEIVED-FLG NOT = "1"
               MOVE "11" TO WK-REASON-CD
               MOVE "Y" TO WK-AUDIT-NEED-SW
               EXIT PARAGRAPH
           END-IF

           IF NR-TREATY-CD = SPACE OR NR-TREATY-CD = ZERO
               MOVE "12" TO WK-REASON-CD
               MOVE "Y" TO WK-AUDIT-NEED-SW
               EXIT PARAGRAPH
           END-IF

           IF NR-RESIDENCE-COUNTRY-CD = SPACE
               MOVE "13" TO WK-REASON-CD
               MOVE "Y" TO WK-AUDIT-NEED-SW
               EXIT PARAGRAPH
           END-IF

           IF WK-PAY-DATE < NR-VALID-FROM
              OR WK-PAY-DATE > NR-VALID-TO
               MOVE "14" TO WK-REASON-CD
               MOVE "Y" TO WK-AUDIT-NEED-SW
               EXIT PARAGRAPH
           END-IF

           IF TI-ACCT-TYPE = "03"
               EXIT PARAGRAPH
           END-IF

           MOVE "Y" TO WK-TARGET-SW.

       4000-WRITE-AUDIT.
           ADD 1 TO WK-AUDIT-SEQ
           INITIALIZE KZADLF-REC
           MOVE WK-AUDIT-SEQ TO AL-AUDIT-ID
           MOVE WK-RUN-DATE TO AL-RUN-DATE
           MOVE WK-SOURCE-DSID TO AL-SOURCE-DSID
           MOVE WK-ZERO-AMT TO AL-DEBIT-AMT
           MOVE TI-INT-AMT TO AL-CREDIT-AMT
           COMPUTE AL-DIFF-AMT = AL-CREDIT-AMT - AL-DEBIT-AMT
           MOVE WK-REASON-CD TO AL-STATUS-CD

           WRITE KZADLF-REC
           IF FS-KZADLF NOT = "00"
               DISPLAY "KZADLF 書込失敗 ST=" FS-KZADLF
               MOVE 12 TO RETURN-CODE
               MOVE "Y" TO WK-EOF-SW
           ELSE
               ADD 1 TO WK-AUDIT-COUNT
               ADD 1 TO WK-ERR-COUNT
           END-IF.

       5000-WRITE-TARGET.
      *    PRE判定版では同一IFへ振分対象を再出力する。
           WRITE KZTXIF-REC
           IF FS-KZTXIF NOT = "00"
               DISPLAY "KZTXIF 振分書込失敗 ST=" FS-KZTXIF
               MOVE 12 TO RETURN-CODE
               MOVE "Y" TO WK-EOF-SW
           ELSE
               ADD 1 TO WK-OUT-COUNT
           END-IF.

       9000-FINAL.
           PERFORM 9100-CLOSE-KZTXIF
           PERFORM 9200-CLOSE-KZNRMF
           PERFORM 9300-CLOSE-KZADLF

           DISPLAY "KZ233B 入力件数=" WK-IN-COUNT
           DISPLAY "KZ233B 対象件数=" WK-OUT-COUNT
           DISPLAY "KZ233B 監査件数=" WK-AUDIT-COUNT
           DISPLAY "KZ233B 対象外件数=" WK-SKIP-COUNT

           IF RETURN-CODE = 0
               DISPLAY "KZ233B 正常終了"
           ELSE
               DISPLAY "KZ233B 異常終了 RC=" RETURN-CODE
           END-IF.

       9100-CLOSE-KZTXIF.
           IF FS-KZTXIF = "00"
               CLOSE KZTXIF
               IF FS-KZTXIF NOT = "00"
                   DISPLAY "KZTXIF クローズ失敗 ST=" FS-KZTXIF
                   MOVE 8 TO RETURN-CODE
               END-IF
           END-IF.

       9200-CLOSE-KZNRMF.
           IF FS-KZNRMF = "00" OR FS-KZNRMF = "23"
               CLOSE KZNRMF
               IF FS-KZNRMF NOT = "00"
                   DISPLAY "KZNRMF クローズ失敗 ST=" FS-KZNRMF
                   MOVE 8 TO RETURN-CODE
               END-IF
           END-IF.

       9300-CLOSE-KZADLF.
           IF FS-KZADLF = "00"
               CLOSE KZADLF
               IF FS-KZADLF NOT = "00"
                   DISPLAY "KZADLF クローズ失敗 ST=" FS-KZADLF
                   MOVE 8 TO RETURN-CODE
               END-IF
           END-IF.
