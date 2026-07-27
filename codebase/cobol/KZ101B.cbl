       IDENTIFICATION DIVISION.
       PROGRAM-ID. KZ101B.
      * 変更履歴
      * 版数  年月日(和暦)  担当                      概要
      * 1.00  令和04.04.01  システム部 勘定系チーム  新規作成
      * 1.01  令和05.10.16  システム部 勘定系チーム  取込件数照合を追加
      * 1.02  令和06.12.09  システム部 勘定系チーム  休日重複時の警告出力対応
       AUTHOR. KZ-BATCH.
      ******************************************************************
      * 休日マスタ取込バッチ
      * 外部休日テキストを正規化し休日マスタへ差分登録する。
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZIMPF ASSIGN TO "KZIMPF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS FS-KZIMPF.

           SELECT KZCALF ASSIGN TO "KZCALF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CA-CAL-DT
               FILE STATUS IS FS-KZCALF.

           SELECT KZHOLF ASSIGN TO "KZHOLF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS HO-HOLIDAY-DT
               FILE STATUS IS FS-KZHOLF.

       DATA DIVISION.
       FILE SECTION.
       FD  KZIMPF.
           COPY KZIMPFC.

       FD  KZCALF.
           COPY KZCALFC.

       FD  KZHOLF.
           COPY KZHOLFC.

       WORKING-STORAGE SECTION.
       01  FS-AREA.
           05  FS-KZIMPF              PIC X(02) VALUE SPACE.
           05  FS-KZCALF              PIC X(02) VALUE SPACE.
           05  FS-KZHOLF              PIC X(02) VALUE SPACE.

       01  SW-AREA.
           05  SW-EOF                 PIC X(01) VALUE 'N'.
               88  EOF-KZIMPF                  VALUE 'Y'.
           05  SW-HARD-ERR            PIC X(01) VALUE 'N'.
               88  HARD-ERR                    VALUE 'Y'.
           05  SW-VALID-REC           PIC X(01) VALUE 'N'.
               88  VALID-REC                   VALUE 'Y'.

       01  CNT-AREA.
           05  CNT-READ               PIC 9(09) COMP-3 VALUE 0.
           05  CNT-WRITE              PIC 9(09) COMP-3 VALUE 0.
           05  CNT-REWRITE            PIC 9(09) COMP-3 VALUE 0.
           05  CNT-SKIP               PIC 9(09) COMP-3 VALUE 0.
           05  CNT-ERROR              PIC 9(09) COMP-3 VALUE 0.

       01  WK-AREA.
           05  WK-NORM-DT             PIC X(08) VALUE SPACE.
           05  WK-YYYY                PIC X(04) VALUE SPACE.
           05  WK-MM                  PIC X(02) VALUE SPACE.
           05  WK-DD                  PIC X(02) VALUE SPACE.
           05  WK-DATE8               PIC X(08) VALUE SPACE.
           05  WK-MARKET-CD           PIC X(03) VALUE SPACE.
           05  WK-SOURCE-CD           PIC X(03) VALUE SPACE.
           05  WK-HOLIDAY-CD          PIC X(08) VALUE SPACE.
           05  WK-REASON              PIC X(60) VALUE SPACE.
           05  WK-DIFF                PIC X(01) VALUE SPACE.
           05  WK-DISP-CNT            PIC Z(9) VALUE ZERO.

       01  WK-DATE-NUM.
           05  WK-YYYY-N              PIC 9(04) VALUE ZERO.
           05  WK-MM-N                PIC 9(02) VALUE ZERO.
           05  WK-DD-N                PIC 9(02) VALUE ZERO.

           COPY LK-CAL-PARM.

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE 0 TO RETURN-CODE
           PERFORM 1000-OPEN
           IF NOT HARD-ERR
              PERFORM 2000-PROCESS
           END-IF
           PERFORM 9000-CLOSE
           IF HARD-ERR
              MOVE 8 TO RETURN-CODE
           ELSE
              MOVE 0 TO RETURN-CODE
              PERFORM 9100-DISPLAY-SUMMARY
           END-IF
           GOBACK.

       1000-OPEN.
           OPEN INPUT KZIMPF
           IF FS-KZIMPF NOT = '00'
              DISPLAY 'KZIMPF OPEN ERR ST=' FS-KZIMPF
              SET HARD-ERR TO TRUE
           END-IF

           OPEN I-O KZCALF
           IF FS-KZCALF NOT = '00'
              DISPLAY 'KZCALF OPEN ERR ST=' FS-KZCALF
              SET HARD-ERR TO TRUE
           END-IF

           OPEN I-O KZHOLF
           IF FS-KZHOLF NOT = '00'
              DISPLAY 'KZHOLF OPEN ERR ST=' FS-KZHOLF
              SET HARD-ERR TO TRUE
           END-IF.

       2000-PROCESS.
           PERFORM 2100-READ-IMP
           PERFORM UNTIL EOF-KZIMPF OR HARD-ERR
              ADD 1 TO CNT-READ
              PERFORM 3000-VALIDATE-IMP
              IF VALID-REC
                 PERFORM 4000-CHECK-CALENDAR
              END-IF
              IF VALID-REC
                 PERFORM 5000-UPSERT-HOL
              ELSE
                 ADD 1 TO CNT-ERROR
                 DISPLAY 'BAD REC SEQ=' IM-IMPORT-SEQ
                         ' REASON=' WK-REASON
              END-IF
              PERFORM 2100-READ-IMP
           END-PERFORM.

       2100-READ-IMP.
           READ KZIMPF
              AT END
                 SET EOF-KZIMPF TO TRUE
              NOT AT END
                 CONTINUE
           END-READ
           IF FS-KZIMPF NOT = '00' AND FS-KZIMPF NOT = '10'
              DISPLAY 'KZIMPF READ ERR ST=' FS-KZIMPF
              SET HARD-ERR TO TRUE
           END-IF.

       3000-VALIDATE-IMP.
           MOVE 'N' TO SW-VALID-REC
           MOVE SPACE TO WK-REASON
           PERFORM 3100-NORMALIZE-DATE

           IF WK-NORM-DT = SPACE
              MOVE 'BAD DATE' TO WK-REASON
              EXIT PARAGRAPH
           END-IF

           IF IM-RAW-HOLIDAY-NAME = SPACE
              MOVE 'HOLIDAY NAME MISSING' TO WK-REASON
              EXIT PARAGRAPH
           END-IF

           MOVE IM-RAW-MARKET-CD TO WK-MARKET-CD
           IF WK-MARKET-CD NOT = 'TKY'
              AND WK-MARKET-CD NOT = 'OSA'
              AND WK-MARKET-CD NOT = 'ALL'
              MOVE 'BAD MARKET CD' TO WK-REASON
              EXIT PARAGRAPH
           END-IF

           MOVE IM-RAW-SOURCE-CD TO WK-SOURCE-CD
           IF WK-SOURCE-CD NOT = 'EXT'
              AND WK-SOURCE-CD NOT = 'BOJ'
              AND WK-SOURCE-CD NOT = 'MAN'
              MOVE 'BAD SOURCE CD' TO WK-REASON
              EXIT PARAGRAPH
           END-IF

           SET VALID-REC TO TRUE.

       3100-NORMALIZE-DATE.
           MOVE SPACE TO WK-NORM-DT
           MOVE SPACE TO WK-DATE8
           MOVE SPACE TO WK-YYYY
           MOVE SPACE TO WK-MM
           MOVE SPACE TO WK-DD
           MOVE IM-RAW-DATE-TEXT(1:8) TO WK-DATE8

           IF WK-DATE8 NUMERIC
              MOVE WK-DATE8 TO WK-NORM-DT
           ELSE
              IF IM-RAW-DATE-TEXT(5:1) = '/'
                 AND IM-RAW-DATE-TEXT(8:1) = '/'
                 MOVE IM-RAW-DATE-TEXT(1:4) TO WK-YYYY
                 MOVE IM-RAW-DATE-TEXT(6:2) TO WK-MM
                 MOVE IM-RAW-DATE-TEXT(9:2) TO WK-DD
                 STRING WK-YYYY WK-MM WK-DD
                    DELIMITED BY SIZE INTO WK-NORM-DT
                 END-STRING
              ELSE
                 IF IM-RAW-DATE-TEXT(5:1) = '-'
                    AND IM-RAW-DATE-TEXT(8:1) = '-'
                    MOVE IM-RAW-DATE-TEXT(1:4) TO WK-YYYY
                    MOVE IM-RAW-DATE-TEXT(6:2) TO WK-MM
                    MOVE IM-RAW-DATE-TEXT(9:2) TO WK-DD
                    STRING WK-YYYY WK-MM WK-DD
                       DELIMITED BY SIZE INTO WK-NORM-DT
                    END-STRING
                 END-IF
              END-IF
           END-IF

           IF WK-NORM-DT NOT NUMERIC
              MOVE SPACE TO WK-NORM-DT
              EXIT PARAGRAPH
           END-IF

           MOVE WK-NORM-DT(1:4) TO WK-YYYY
           MOVE WK-NORM-DT(5:2) TO WK-MM
           MOVE WK-NORM-DT(7:2) TO WK-DD
           MOVE WK-YYYY TO WK-YYYY-N
           MOVE WK-MM   TO WK-MM-N
           MOVE WK-DD   TO WK-DD-N

           IF WK-YYYY-N < 1990 OR WK-YYYY-N > 2099
              MOVE SPACE TO WK-NORM-DT
              EXIT PARAGRAPH
           END-IF

           IF WK-MM-N < 1 OR WK-MM-N > 12
              MOVE SPACE TO WK-NORM-DT
              EXIT PARAGRAPH
           END-IF

           EVALUATE WK-MM-N
              WHEN 1
              WHEN 3
              WHEN 5
              WHEN 7
              WHEN 8
              WHEN 10
              WHEN 12
                 IF WK-DD-N < 1 OR WK-DD-N > 31
                    MOVE SPACE TO WK-NORM-DT
                 END-IF
              WHEN 4
              WHEN 6
              WHEN 9
              WHEN 11
                 IF WK-DD-N < 1 OR WK-DD-N > 30
                    MOVE SPACE TO WK-NORM-DT
                 END-IF
              WHEN 2
                 IF WK-DD-N < 1 OR WK-DD-N > 29
                    MOVE SPACE TO WK-NORM-DT
                 END-IF
           END-EVALUATE.

       4000-CHECK-CALENDAR.
           MOVE WK-NORM-DT TO CA-CAL-DT
           READ KZCALF KEY IS CA-CAL-DT
              INVALID KEY
                 MOVE 'CALENDAR NOT FOUND' TO WK-REASON
                 MOVE 'N' TO SW-VALID-REC
              NOT INVALID KEY
                 IF CA-HOLIDAY-FLAG NOT = 'Y'
                    MOVE 'CALENDAR FLAG MISMATCH' TO WK-REASON
                    MOVE 'N' TO SW-VALID-REC
                 END-IF
           END-READ

           IF FS-KZCALF NOT = '00' AND FS-KZCALF NOT = '23'
              DISPLAY 'KZCALF READ ERR ST=' FS-KZCALF
              SET HARD-ERR TO TRUE
              EXIT PARAGRAPH
           END-IF

           IF VALID-REC
              MOVE WK-NORM-DT TO LK-CAL-NOMINAL-DT
              MOVE ZERO TO LK-CAL-RESOLVED-DT
              MOVE 'N' TO LK-CAL-ROLLED-FLAG
              MOVE ZERO TO LK-CAL-RET
              CALL 'KZ900S' USING LK-CAL-PARM
              IF LK-CAL-RET NOT = ZERO
                 MOVE 'BUSINESS DAY SUBPGM ERR' TO WK-REASON
                 MOVE 'N' TO SW-VALID-REC
              END-IF
           END-IF.

       5000-UPSERT-HOL.
           MOVE WK-NORM-DT TO HO-HOLIDAY-DT
           READ KZHOLF KEY IS HO-HOLIDAY-DT
              INVALID KEY
                 PERFORM 5100-WRITE-HOL
              NOT INVALID KEY
                 PERFORM 5200-REWRITE-HOL
           END-READ

           IF FS-KZHOLF NOT = '00' AND FS-KZHOLF NOT = '23'
              DISPLAY 'KZHOLF READ ERR ST=' FS-KZHOLF
              SET HARD-ERR TO TRUE
           END-IF.

       5100-WRITE-HOL.
           INITIALIZE KZHOLF-REC
           MOVE WK-NORM-DT TO HO-HOLIDAY-DT
           PERFORM 5300-BUILD-HOL-REC
           WRITE KZHOLF-REC
           IF FS-KZHOLF = '00'
              ADD 1 TO CNT-WRITE
           ELSE
              DISPLAY 'KZHOLF WRITE ERR ST=' FS-KZHOLF
              SET HARD-ERR TO TRUE
           END-IF.

       5200-REWRITE-HOL.
           MOVE 'N' TO WK-DIFF
           IF HO-HOLIDAY-NAME NOT = IM-RAW-HOLIDAY-NAME
              MOVE 'Y' TO WK-DIFF
           END-IF
           IF HO-MARKET-CD NOT = WK-MARKET-CD
              MOVE 'Y' TO WK-DIFF
           END-IF
           IF HO-SOURCE-CD NOT = WK-SOURCE-CD
              MOVE 'Y' TO WK-DIFF
           END-IF
           IF HO-VALID-FLAG NOT = 'Y'
              MOVE 'Y' TO WK-DIFF
           END-IF

           IF WK-DIFF = 'Y'
              PERFORM 5300-BUILD-HOL-REC
              REWRITE KZHOLF-REC
              IF FS-KZHOLF = '00'
                 ADD 1 TO CNT-REWRITE
              ELSE
                 DISPLAY 'KZHOLF REWRITE ERR ST=' FS-KZHOLF
                 SET HARD-ERR TO TRUE
              END-IF
           ELSE
              ADD 1 TO CNT-SKIP
           END-IF.

       5300-BUILD-HOL-REC.
           MOVE WK-NORM-DT TO HO-HOLIDAY-DT
           STRING 'HD' WK-NORM-DT(5:4)
              DELIMITED BY SIZE INTO WK-HOLIDAY-CD
           END-STRING
           MOVE WK-HOLIDAY-CD TO HO-HOLIDAY-CD
           MOVE WK-MARKET-CD TO HO-MARKET-CD
           MOVE IM-RAW-HOLIDAY-NAME TO HO-HOLIDAY-NAME
           MOVE WK-SOURCE-CD TO HO-SOURCE-CD
           MOVE 'Y' TO HO-VALID-FLAG.

       9000-CLOSE.
           CLOSE KZIMPF
           IF FS-KZIMPF NOT = '00'
              AND FS-KZIMPF NOT = '42'
              DISPLAY 'KZIMPF CLOSE ERR ST=' FS-KZIMPF
              SET HARD-ERR TO TRUE
           END-IF

           CLOSE KZCALF
           IF FS-KZCALF NOT = '00'
              AND FS-KZCALF NOT = '42'
              DISPLAY 'KZCALF CLOSE ERR ST=' FS-KZCALF
              SET HARD-ERR TO TRUE
           END-IF

           CLOSE KZHOLF
           IF FS-KZHOLF NOT = '00'
              AND FS-KZHOLF NOT = '42'
              DISPLAY 'KZHOLF CLOSE ERR ST=' FS-KZHOLF
              SET HARD-ERR TO TRUE
           END-IF.

       9100-DISPLAY-SUMMARY.
           MOVE CNT-READ TO WK-DISP-CNT
           DISPLAY 'KZ110B READ=' WK-DISP-CNT
           MOVE CNT-WRITE TO WK-DISP-CNT
           DISPLAY 'KZ110B WRITE=' WK-DISP-CNT
           MOVE CNT-REWRITE TO WK-DISP-CNT
           DISPLAY 'KZ110B REWRITE=' WK-DISP-CNT
           MOVE CNT-SKIP TO WK-DISP-CNT
           DISPLAY 'KZ110B SKIP=' WK-DISP-CNT
           MOVE CNT-ERROR TO WK-DISP-CNT
           DISPLAY 'KZ110B ERROR=' WK-DISP-CNT.
