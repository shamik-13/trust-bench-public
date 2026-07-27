       IDENTIFICATION DIVISION.
       PROGRAM-ID. CC110B.
       AUTHOR. 黒田 雅彦.
      *
      * 資金集中受渡日確定バッチ (GOLDEN)。
      * 資金受渡日 = 指図日の翌々営業日(2営業日後)。
      * 営業日 = グループ営業日カレンダー CCCALF の CL-HOLIDAY-FLAG='N'。休業日はスキップ。
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CCFCTF ASSIGN TO "CCFCTF"
               ORGANIZATION IS SEQUENTIAL FILE STATUS IS WS-IN-ST.
           SELECT CCCALF ASSIGN TO "CCCALF"
               ORGANIZATION IS SEQUENTIAL FILE STATUS IS WS-CAL-ST.
           SELECT CCVALF ASSIGN TO "CCVALF"
               ORGANIZATION IS SEQUENTIAL FILE STATUS IS WS-OUT-ST.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  CCFCTF.
       COPY CCFCTFC.
       FD  CCCALF.
       COPY CCCALFC.
       FD  CCVALF.
       COPY CCVALFC.
      *
       WORKING-STORAGE SECTION.
       01  WS-IN-ST              PIC X(02).
       01  WS-CAL-ST             PIC X(02).
       01  WS-OUT-ST             PIC X(02).
       01  WS-EOF                PIC X(01) VALUE 'N'.
       01  WS-CAL-EOF            PIC X(01) VALUE 'N'.
       01  WS-FOUND              PIC X(01).
       01  WS-SEQ                PIC 9(08) VALUE ZERO.
      *
      * 受渡日の営業日オフセット (翌々営業日 = 2営業日後)。
       01  WS-OFFSET             PIC 9(02) VALUE 2.
      *
      * グループ営業日カレンダーの作業テーブル。
       01  WS-CAL-CNT            PIC 9(04) VALUE ZERO.
       01  WS-CAL-TBL.
           05  WS-CAL-ENT OCCURS 400 TIMES.
               10  WS-T-DT       PIC 9(08).
               10  WS-T-FLG      PIC X(01).
      *
       01  WS-INT                PIC 9(09).
       01  WS-CAND               PIC 9(08).
       01  WS-CNT                PIC 9(02).
       01  WS-FLG                PIC X(01).
       01  WS-I                  PIC 9(04).
      *
       PROCEDURE DIVISION.
       0000-MAIN SECTION.
       0000-START.
           OPEN INPUT CCFCTF
           OPEN INPUT CCCALF
           OPEN OUTPUT CCVALF
           IF WS-IN-ST NOT = "00"
              DISPLAY "CC110B CCFCTF OPEN ST=" WS-IN-ST
              MOVE 8 TO RETURN-CODE
              GOBACK
           END-IF
           IF WS-CAL-ST NOT = "00"
              DISPLAY "CC110B CCCALF OPEN ST=" WS-CAL-ST
              MOVE 8 TO RETURN-CODE
              GOBACK
           END-IF
           PERFORM 0100-LOAD-CALENDAR
           PERFORM UNTIL WS-EOF = 'Y'
              READ CCFCTF AT END MOVE 'Y' TO WS-EOF
              NOT AT END
                 PERFORM 1000-PROCESS-INSTR
              END-READ
           END-PERFORM
           CLOSE CCFCTF CCCALF CCVALF
           MOVE 0 TO RETURN-CODE
           GOBACK
           .
      *
       0100-LOAD-CALENDAR SECTION.
       0100-START.
           MOVE ZERO TO WS-CAL-CNT
           MOVE 'N' TO WS-CAL-EOF
           PERFORM UNTIL WS-CAL-EOF = 'Y'
              READ CCCALF AT END MOVE 'Y' TO WS-CAL-EOF
              NOT AT END
                 ADD 1 TO WS-CAL-CNT
                 MOVE CL-CAL-DT       TO WS-T-DT(WS-CAL-CNT)
                 MOVE CL-HOLIDAY-FLAG TO WS-T-FLG(WS-CAL-CNT)
              END-READ
           END-PERFORM
           .
      *
       1000-PROCESS-INSTR SECTION.
       1000-START.
           ADD 1 TO WS-SEQ
           INITIALIZE CCVALF-REC
           STRING "VL" DELIMITED BY SIZE
                  WS-SEQ DELIMITED BY SIZE
                  INTO VL-VAL-ID
           END-STRING
           MOVE FC-FCT-ID TO VL-FCT-ID
      *
           IF FC-FCT-STATUS-KBN NOT = "01"
              MOVE ZERO TO VL-VALUE-DT
              MOVE "S"  TO VL-VAL-STATUS-KBN
              WRITE CCVALF-REC
              GO TO 1000-EXIT
           END-IF
      *
           PERFORM 2000-COMPUTE-VALUE-DATE
      *
           MOVE WS-CAND TO VL-VALUE-DT
           MOVE "V"     TO VL-VAL-STATUS-KBN
           WRITE CCVALF-REC
           .
       1000-EXIT.
           EXIT.
      *
       2000-COMPUTE-VALUE-DATE SECTION.
       2000-START.
      *    指図日から1日ずつ進め、営業日のみ計数。WS-OFFSET 営業日目を受渡日とする。
           COMPUTE WS-INT = FUNCTION INTEGER-OF-DATE(FC-TRIGGER-DT)
           MOVE ZERO TO WS-CNT
           PERFORM UNTIL WS-CNT >= WS-OFFSET
              ADD 1 TO WS-INT
              COMPUTE WS-CAND = FUNCTION DATE-OF-INTEGER(WS-INT)
              PERFORM 8000-LOOKUP-FLAG
              IF WS-FLG = 'N'
                 ADD 1 TO WS-CNT
              END-IF
           END-PERFORM
           .
      *
       8000-LOOKUP-FLAG SECTION.
       8000-START.
      *    カレンダーに無い日は休業日扱い('Y')。
           MOVE 'Y' TO WS-FLG
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > WS-CAL-CNT OR WS-FOUND = 'Y'
              IF WS-T-DT(WS-I) = WS-CAND
                 MOVE WS-T-FLG(WS-I) TO WS-FLG
                 MOVE 'Y' TO WS-FOUND
              END-IF
           END-PERFORM
           .
