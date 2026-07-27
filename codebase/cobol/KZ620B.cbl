       IDENTIFICATION DIVISION.
       PROGRAM-ID. KZ620B.
      * 変更履歴
      * 版数  年月日(和暦)   担当              概要
      * 1.00  平成30年04月01日  高橋 誠 (E-021)  新規作成
      * 1.10  令和02年01月06日  速水 涼介 (E-077)  税率表改定対応
      * 1.20  令和05年10月01日  佐伯 美咲 (E-134)  適格請求書対応
       AUTHOR. KZ-BATCH.
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.
      *
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZTXIF
               ASSIGN TO "KZTXIF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-KZTXIF.
           SELECT KZTXRF
               ASSIGN TO "KZTXRF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-KZTXRF.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  KZTXIF
           RECORDING MODE IS F.
           COPY KZTXIFC.
      *
       FD  KZTXRF
           RECORDING MODE IS F.
           COPY KZTXRFC.
      *
       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05 FS-KZTXIF              PIC X(02) VALUE SPACE.
           05 FS-KZTXRF              PIC X(02) VALUE SPACE.
      *
       01  WS-SWITCHES.
           05 WS-EOF-SW              PIC X VALUE "N".
              88 END-OF-KZTXIF             VALUE "Y".
              88 NOT-END-OF-KZTXIF         VALUE "N".
           05 WS-ABEND-SW            PIC X VALUE "N".
              88 ABEND-OCCURRED            VALUE "Y".
              88 NO-ABEND                  VALUE "N".
           05 WS-SKIP-SW             PIC X VALUE "N".
              88 SKIP-RECORD               VALUE "Y".
              88 PROCESS-RECORD            VALUE "N".
      *
       01  WS-COUNTERS.
           05 WS-READ-CNT            PIC 9(11) VALUE ZERO.
           05 WS-WRITE-CNT           PIC 9(11) VALUE ZERO.
           05 WS-SKIP-CNT            PIC 9(11) VALUE ZERO.
      *
       01  WS-TAX-TOTALS.
           05 WS-NATIONAL-TOTAL      PIC 9(15) VALUE ZERO.
           05 WS-LOCAL-TOTAL         PIC 9(15) VALUE ZERO.
           05 WS-TOTAL-TAX-TOTAL     PIC 9(15) VALUE ZERO.
      *
       01  WS-CALC-AREA.
           05 WS-GROSS-AMT           PIC 9(15)V99 VALUE ZERO.
           05 WS-NATIONAL-TAX-YEN    PIC 9(15) VALUE ZERO.
           05 WS-LOCAL-TAX-YEN       PIC 9(15) VALUE ZERO.
           05 WS-TOTAL-TAX-YEN       PIC 9(15) VALUE ZERO.
           05 WS-NET-AMT             PIC S9(15) VALUE ZERO.
           05 WS-COMBINED-RATE       PIC 9V9(05) VALUE 0.20315.
           05 WS-NATIONAL-RATE       PIC 9V9(05) VALUE 0.15315.
      *
       01  WS-DISPLAY-AREA.
           05 WS-DISP-READ-CNT       PIC ZZZ,ZZZ,ZZZ,ZZ9.
           05 WS-DISP-WRITE-CNT      PIC ZZZ,ZZZ,ZZZ,ZZ9.
           05 WS-DISP-SKIP-CNT       PIC ZZZ,ZZZ,ZZZ,ZZ9.
           05 WS-DISP-TAX-TOTAL      PIC ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.
      *
       PROCEDURE DIVISION.
       MAIN-SECTION.
           PERFORM 1000-INITIALIZE
           IF NO-ABEND
               PERFORM 2000-PROCESS UNTIL END-OF-KZTXIF
           END-IF
           PERFORM 9000-TERMINATE
           GOBACK.
      *
       1000-INITIALIZE.
           MOVE 0 TO RETURN-CODE
           SET NO-ABEND TO TRUE
           SET NOT-END-OF-KZTXIF TO TRUE
      *
           OPEN INPUT KZTXIF
           IF FS-KZTXIF NOT = "00"
               DISPLAY "KZ620B KZTXIF オープン失敗"
               DISPLAY "KZ620B ST=" FS-KZTXIF
               PERFORM 9100-ABEND
           END-IF
      *
           IF NO-ABEND
               OPEN OUTPUT KZTXRF
               IF FS-KZTXRF NOT = "00"
                   DISPLAY "KZ620B KZTXRF オープン失敗"
                   DISPLAY "KZ620B ST=" FS-KZTXRF
                   PERFORM 9100-ABEND
               END-IF
           END-IF
      *
           IF NO-ABEND
               DISPLAY "KZ620B 源泉徴収税計算バッチ開始"
               PERFORM 2100-READ-KZTXIF
           END-IF.
      *
       2000-PROCESS.
           SET PROCESS-RECORD TO TRUE
           PERFORM 3000-VALIDATE-INPUT
           IF NO-ABEND AND PROCESS-RECORD
               PERFORM 4000-CALCULATE-TAX
               PERFORM 5000-WRITE-KZTXRF
           END-IF
           IF NO-ABEND
               PERFORM 2100-READ-KZTXIF
           ELSE
               SET END-OF-KZTXIF TO TRUE
           END-IF.
      *
       2100-READ-KZTXIF.
           READ KZTXIF
               AT END
                   SET END-OF-KZTXIF TO TRUE
               NOT AT END
                   ADD 1 TO WS-READ-CNT
           END-READ
      *
           IF FS-KZTXIF NOT = "00" AND FS-KZTXIF NOT = "10"
               DISPLAY "KZ620B KZTXIF 読込失敗"
               DISPLAY "KZ620B ST=" FS-KZTXIF
               PERFORM 9100-ABEND
           END-IF.
      *
       3000-VALIDATE-INPUT.
           IF TI-ACCT-NO = SPACE
               DISPLAY "KZ620B 口座番号不正"
               DISPLAY "KZ620B 口座区分=" TI-ACCT-TYPE
               ADD 1 TO WS-SKIP-CNT
               SET SKIP-RECORD TO TRUE
           ELSE
               IF TI-ACCT-TYPE NOT = "01"
                  AND TI-ACCT-TYPE NOT = "02"
                  AND TI-ACCT-TYPE NOT = "03"
                   DISPLAY "KZ620B 口座区分不正"
                   DISPLAY "KZ620B 口座番号=" TI-ACCT-NO
                   DISPLAY "KZ620B 不正区分=" TI-ACCT-TYPE
                   ADD 1 TO WS-SKIP-CNT
                   SET SKIP-RECORD TO TRUE
               END-IF
           END-IF.
      *
       4000-CALCULATE-TAX.
           MOVE TI-INT-AMT TO WS-GROSS-AMT
           COMPUTE WS-TOTAL-TAX-YEN =
               WS-GROSS-AMT * WS-COMBINED-RATE
           COMPUTE WS-NATIONAL-TAX-YEN =
               WS-GROSS-AMT * WS-NATIONAL-RATE
           COMPUTE WS-LOCAL-TAX-YEN =
               WS-TOTAL-TAX-YEN - WS-NATIONAL-TAX-YEN
           COMPUTE WS-NET-AMT =
               WS-GROSS-AMT - WS-TOTAL-TAX-YEN
      *
           ADD WS-NATIONAL-TAX-YEN TO WS-NATIONAL-TOTAL
           ADD WS-LOCAL-TAX-YEN    TO WS-LOCAL-TOTAL
           ADD WS-TOTAL-TAX-YEN    TO WS-TOTAL-TAX-TOTAL.
      *
       5000-WRITE-KZTXRF.
           MOVE TI-ACCT-NO          TO TR-ACCT-NO
           MOVE TI-ACCT-TYPE        TO TR-ACCT-TYPE
           MOVE TI-INT-AMT          TO TR-GROSS-INT-AMT
           MOVE WS-NATIONAL-TAX-YEN TO TR-NATIONAL-TAX-AMT
           MOVE WS-LOCAL-TAX-YEN    TO TR-LOCAL-TAX-AMT
           MOVE WS-TOTAL-TAX-YEN    TO TR-TOTAL-TAX-AMT
           MOVE WS-NET-AMT          TO TR-NET-AMT
      *
           WRITE KZTXRF-REC
           IF FS-KZTXRF = "00"
               ADD 1 TO WS-WRITE-CNT
           ELSE
               DISPLAY "KZ620B KZTXRF 書込失敗"
               DISPLAY "KZ620B ST=" FS-KZTXRF
               DISPLAY "KZ620B 対象口座番号=" TI-ACCT-NO
               PERFORM 9100-ABEND
           END-IF.
      *
       9000-TERMINATE.
           IF FS-KZTXIF = "00" OR FS-KZTXIF = "10"
               CLOSE KZTXIF
               IF FS-KZTXIF NOT = "00"
                   DISPLAY "KZ620B KZTXIF クローズ失敗"
                   DISPLAY "KZ620B ST=" FS-KZTXIF
                   PERFORM 9100-ABEND
               END-IF
           END-IF
      *
           IF FS-KZTXRF = "00"
               CLOSE KZTXRF
               IF FS-KZTXRF NOT = "00"
                   DISPLAY "KZ620B KZTXRF クローズ失敗"
                   DISPLAY "KZ620B ST=" FS-KZTXRF
                   PERFORM 9100-ABEND
               END-IF
           END-IF
      *
           MOVE WS-READ-CNT        TO WS-DISP-READ-CNT
           MOVE WS-WRITE-CNT       TO WS-DISP-WRITE-CNT
           MOVE WS-SKIP-CNT        TO WS-DISP-SKIP-CNT
           MOVE WS-TOTAL-TAX-TOTAL TO WS-DISP-TAX-TOTAL
      *
           DISPLAY "KZ620B 読込件数=" WS-DISP-READ-CNT
           DISPLAY "KZ620B 書込件数=" WS-DISP-WRITE-CNT
           DISPLAY "KZ620B 除外件数=" WS-DISP-SKIP-CNT
           DISPLAY "KZ620B 税額合計=" WS-DISP-TAX-TOTAL
      *
           IF ABEND-OCCURRED
               DISPLAY "KZ620B 異常終了 RC=" RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
               DISPLAY "KZ620B 正常終了 RC=0"
           END-IF.
      *
       9100-ABEND.
           SET ABEND-OCCURRED TO TRUE
           SET END-OF-KZTXIF TO TRUE
           MOVE 8 TO RETURN-CODE.
