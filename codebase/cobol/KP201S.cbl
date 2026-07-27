       IDENTIFICATION DIVISION.
       PROGRAM-ID. KP201S.
      *
      * 変更履歴
      * 版数  年月日      担当                      概要
      * 1.00  令和05年09月 システム部 勘定系チーム  新規作成
      * 1.01  令和06年07月 システム部 勘定系チーム  検証強化
      * 1.02  令和07年11月 システム部 勘定系チーム  判定整理
      *
       AUTHOR. BATCH-SYSTEM.
       INSTALLATION. みらい信託銀行.
       DATE-WRITTEN. 2023-09-05.
      *
      * 延滞状態判定サブ
      * 既存延滞日数と今回未入金額から延滞状態を判定する。
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KPDELINF ASSIGN TO "KPDELINF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS DL-ACCT-ID
               FILE STATUS IS WS-KPDELINF-STS.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  KPDELINF.
           COPY KPDELINFC.
      *
       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05 WS-KPDELINF-STS        PIC XX VALUE SPACES.
      *
       01  WS-WORK.
           05 WS-EOF-SW              PIC X VALUE "N".
              88 WS-EOF                   VALUE "Y".
              88 WS-NOT-EOF               VALUE "N".
           05 WS-FOUND-SW           PIC X VALUE "N".
              88 WS-FOUND                 VALUE "Y".
              88 WS-NOT-FOUND             VALUE "N".
           05 WS-OPENED-SW          PIC X VALUE "N".
              88 WS-OPENED                VALUE "Y".
              88 WS-NOT-OPENED            VALUE "N".
           05 WS-STATUS-CODE        PIC X VALUE SPACE.
           05 WS-ERROR-CODE         PIC 9(4) VALUE ZERO.
           05 WS-ABS-DUE-AMT        PIC S9(13)V99 COMP-3 VALUE ZERO.
           05 WS-VALID-DATE-SW      PIC X VALUE "N".
              88 WS-VALID-DATE            VALUE "Y".
              88 WS-INVALID-DATE          VALUE "N".
           05 WS-DATE-PARTS.
              10 WS-DATE-YYYY       PIC 9(4) VALUE ZERO.
              10 WS-DATE-MM         PIC 9(2) VALUE ZERO.
              10 WS-DATE-DD         PIC 9(2) VALUE ZERO.
           05 WS-DAYS-IN-MM         PIC 9(2) VALUE ZERO.
      *
       01  WS-CONSTANTS.
           05 WC-ZERO-AMT           PIC S9(13)V99 COMP-3 VALUE ZERO.
           05 WC-MIN-DUE-AMT        PIC S9(13)V99 COMP-3 VALUE 1.00.
           05 WC-MAX-DUE-AMT        PIC S9(13)V99 COMP-3
                                      VALUE 9999999999999.99.
           05 WC-STATE-NEW          PIC X VALUE "1".
           05 WC-STATE-CONT         PIC X VALUE "2".
           05 WC-STATE-CLEAR        PIC X VALUE "3".
           05 WC-STATE-HOLD         PIC X VALUE "4".
           05 WC-FLAG-ON            PIC X VALUE "1".
           05 WC-FLAG-OFF           PIC X VALUE "0".
      *
       LINKAGE SECTION.
       01  LK-KP201S-PARM.
           05 LK-ACCT-ID            PIC X(12).
           05 LK-CUR-DUE-AMT        PIC S9(13)V99 COMP-3.
           05 LK-BASE-DT            PIC 9(8).
           05 LK-STATE-CD           PIC X.
           05 LK-LAST-DUE-UPD-FLG   PIC X.
           05 LK-PENALTY-TGT-FLG    PIC X.
           05 LK-RETURN-CD          PIC 9(4).
      *
       PROCEDURE DIVISION USING LK-KP201S-PARM.
       MAIN-RTN.
           PERFORM INIT-RTN
           PERFORM VALIDATE-RTN
           IF WS-ERROR-CODE = ZERO
              PERFORM OPEN-RTN
           END-IF
           IF WS-ERROR-CODE = ZERO
              PERFORM READ-DELINF-RTN
           END-IF
           IF WS-ERROR-CODE = ZERO
              PERFORM DECIDE-STATUS-RTN
           END-IF
           PERFORM CLOSE-RTN
           PERFORM RETURN-RTN
           GOBACK.
      *
       INIT-RTN.
           MOVE ZERO  TO WS-ERROR-CODE
           MOVE SPACE TO WS-STATUS-CODE
           MOVE WC-STATE-HOLD TO LK-STATE-CD
           MOVE WC-FLAG-OFF   TO LK-LAST-DUE-UPD-FLG
           MOVE WC-FLAG-OFF   TO LK-PENALTY-TGT-FLG
           MOVE ZERO          TO LK-RETURN-CD
           SET WS-NOT-FOUND   TO TRUE
           SET WS-NOT-OPENED  TO TRUE.
      *
       VALIDATE-RTN.
           IF LK-ACCT-ID = SPACES
              MOVE 1001 TO WS-ERROR-CODE
           END-IF
           IF WS-ERROR-CODE = ZERO
              IF LK-CUR-DUE-AMT < WC-ZERO-AMT
                 COMPUTE WS-ABS-DUE-AMT = LK-CUR-DUE-AMT * -1
              ELSE
                 MOVE LK-CUR-DUE-AMT TO WS-ABS-DUE-AMT
              END-IF
              IF WS-ABS-DUE-AMT > WC-MAX-DUE-AMT
                 MOVE 1002 TO WS-ERROR-CODE
              END-IF
           END-IF
           IF WS-ERROR-CODE = ZERO
              PERFORM CHECK-DATE-RTN
              IF WS-INVALID-DATE
                 MOVE 1003 TO WS-ERROR-CODE
              END-IF
           END-IF.
      *
       CHECK-DATE-RTN.
           SET WS-INVALID-DATE TO TRUE
           MOVE LK-BASE-DT(1:4) TO WS-DATE-YYYY
           MOVE LK-BASE-DT(5:2) TO WS-DATE-MM
           MOVE LK-BASE-DT(7:2) TO WS-DATE-DD
           IF WS-DATE-YYYY < 1900 OR WS-DATE-MM < 1 OR WS-DATE-MM > 12
              EXIT PARAGRAPH
           END-IF
           EVALUATE WS-DATE-MM
              WHEN 1  WHEN 3  WHEN 5  WHEN 7
              WHEN 8  WHEN 10 WHEN 12
                   MOVE 31 TO WS-DAYS-IN-MM
              WHEN 4  WHEN 6  WHEN 9  WHEN 11
                   MOVE 30 TO WS-DAYS-IN-MM
              WHEN 2
                   IF FUNCTION MOD(WS-DATE-YYYY 400) = ZERO
                      MOVE 29 TO WS-DAYS-IN-MM
                   ELSE
                      IF FUNCTION MOD(WS-DATE-YYYY 100) = ZERO
                         MOVE 28 TO WS-DAYS-IN-MM
                      ELSE
                         IF FUNCTION MOD(WS-DATE-YYYY 4) = ZERO
                            MOVE 29 TO WS-DAYS-IN-MM
                         ELSE
                            MOVE 28 TO WS-DAYS-IN-MM
                         END-IF
                      END-IF
                   END-IF
           END-EVALUATE
           IF WS-DATE-DD >= 1 AND WS-DATE-DD <= WS-DAYS-IN-MM
              SET WS-VALID-DATE TO TRUE
           END-IF.
      *
       OPEN-RTN.
           OPEN INPUT KPDELINF
           IF WS-KPDELINF-STS = "00"
              SET WS-OPENED TO TRUE
           ELSE
              MOVE 2001 TO WS-ERROR-CODE
           END-IF.
      *
       READ-DELINF-RTN.
           MOVE LK-ACCT-ID TO DL-ACCT-ID
           READ KPDELINF KEY IS DL-ACCT-ID
              INVALID KEY
                 IF WS-KPDELINF-STS = "23"
                    SET WS-NOT-FOUND TO TRUE
                 ELSE
                    MOVE 2002 TO WS-ERROR-CODE
                 END-IF
              NOT INVALID KEY
                 SET WS-FOUND TO TRUE
           END-READ.
      *
       DECIDE-STATUS-RTN.
      *    既存台帳が無い口座は、今回未入金があれば新規延滞とする。
           IF WS-NOT-FOUND
              IF LK-CUR-DUE-AMT >= WC-MIN-DUE-AMT
                 MOVE WC-STATE-NEW TO WS-STATUS-CODE
                 MOVE WC-FLAG-ON   TO LK-LAST-DUE-UPD-FLG
                 MOVE WC-FLAG-ON   TO LK-PENALTY-TGT-FLG
              ELSE
                 MOVE WC-STATE-HOLD TO WS-STATUS-CODE
              END-IF
           ELSE
              PERFORM DECIDE-WITH-LEDGER-RTN
           END-IF
           MOVE WS-STATUS-CODE TO LK-STATE-CD.
      *
       DECIDE-WITH-LEDGER-RTN.
      *    台帳値の異常は保留に倒し、後続ペナルティを止める。
           IF DL-DAYS-PAST-DUE < ZERO
              MOVE WC-STATE-HOLD TO WS-STATUS-CODE
              EXIT PARAGRAPH
           END-IF
           IF DL-DUE-AMT < WC-ZERO-AMT
              MOVE WC-STATE-HOLD TO WS-STATUS-CODE
              EXIT PARAGRAPH
           END-IF
           IF LK-CUR-DUE-AMT >= WC-MIN-DUE-AMT
              IF DL-DAYS-PAST-DUE = ZERO
                 MOVE WC-STATE-NEW TO WS-STATUS-CODE
                 MOVE WC-FLAG-ON   TO LK-LAST-DUE-UPD-FLG
              ELSE
                 MOVE WC-STATE-CONT TO WS-STATUS-CODE
                 MOVE WC-FLAG-OFF   TO LK-LAST-DUE-UPD-FLG
              END-IF
              IF DL-PENALTY-RATE-CD NOT = SPACES
                 MOVE WC-FLAG-ON TO LK-PENALTY-TGT-FLG
              END-IF
           ELSE
              IF DL-DAYS-PAST-DUE > ZERO OR DL-DUE-AMT >= WC-MIN-DUE-AMT
                 MOVE WC-STATE-CLEAR TO WS-STATUS-CODE
                 MOVE WC-FLAG-ON     TO LK-LAST-DUE-UPD-FLG
              ELSE
                 MOVE WC-STATE-HOLD  TO WS-STATUS-CODE
              END-IF
           END-IF.
      *
       CLOSE-RTN.
           IF WS-OPENED
              CLOSE KPDELINF
              IF WS-KPDELINF-STS NOT = "00"
                 IF WS-ERROR-CODE = ZERO
                    MOVE 2003 TO WS-ERROR-CODE
                 END-IF
              END-IF
           END-IF.
      *
       RETURN-RTN.
           MOVE WS-ERROR-CODE TO LK-RETURN-CD
           IF WS-ERROR-CODE NOT = ZERO
              MOVE WC-STATE-HOLD TO LK-STATE-CD
              MOVE WC-FLAG-OFF   TO LK-LAST-DUE-UPD-FLG
              MOVE WC-FLAG-OFF   TO LK-PENALTY-TGT-FLG
           END-IF.
