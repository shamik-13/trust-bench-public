       IDENTIFICATION DIVISION.
       PROGRAM-ID. LF240S.
       AUTHOR. みらい生命 システム部.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LFPRMF ASSIGN TO "LFPRMF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-LFPRMF-ST.
           SELECT LFPOLF ASSIGN TO "LFPOLF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-LFPOLF-ST.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  LFPRMF.
       COPY LFPRMFC.
      *
       FD  LFPOLF.
       COPY LFPOLFC.
      *
       WORKING-STORAGE SECTION.
       01  WS-LFPRMF-ST              PIC X(02) VALUE SPACE.
       01  WS-LFPOLF-ST              PIC X(02) VALUE SPACE.
       01  WS-LFPRMF-EOF             PIC X VALUE "N".
           88  LFPRMF-EOF                  VALUE "Y".
       01  WS-LFPOLF-EOF             PIC X VALUE "N".
           88  LFPOLF-EOF                  VALUE "Y".
       01  WS-PRM-FOUND              PIC X VALUE "N".
           88  PRM-FOUND                   VALUE "Y".
       01  WS-POL-FOUND              PIC X VALUE "N".
           88  POL-FOUND                   VALUE "Y".
       01  WS-HARD-ERR               PIC X VALUE "N".
           88  HARD-ERR                    VALUE "Y".
       01  WS-LFPRMF-OPEN            PIC X VALUE "N".
           88  LFPRMF-OPEN                 VALUE "Y".
       01  WS-LFPOLF-OPEN            PIC X VALUE "N".
           88  LFPOLF-OPEN                 VALUE "Y".
       01  WS-WORK-BAND              PIC X(02).
       01  WS-DISP-AMT               PIC ZZZ,ZZZ,ZZZ,ZZ9.
       01  WS-DISP-SUM               PIC ZZZ,ZZZ,ZZZ,ZZ9.
       01  WS-MSG                    PIC X(80).
      *
       LINKAGE SECTION.
       01  LK-LF240S-AREA.
           05  LK-IN-PRM-ID          PIC X(12).
           05  LK-IN-POL-NO          PIC X(12).
           05  LK-OUT-RTN-KBN        PIC X(02).
           05  LK-OUT-ATTN-CD        PIC X(02).
           05  LK-OUT-POL-NO         PIC X(12).
           05  LK-OUT-PRM-ID         PIC X(12).
           05  LK-OUT-BAND-KBN       PIC X(02).
           05  LK-OUT-PRM-AMT        PIC ZZZ,ZZZ,ZZZ,ZZ9.
           05  LK-OUT-SUM-ASSURED    PIC ZZZ,ZZZ,ZZZ,ZZ9.
           05  LK-OUT-CALC-ST        PIC X(02).
           05  LK-OUT-POL-ST         PIC X(02).
           05  LK-OUT-REASON         PIC X(60).
      *
       PROCEDURE DIVISION USING LK-LF240S-AREA.
       0000-MAIN.
           PERFORM 1000-INIT
           IF NOT HARD-ERR
              PERFORM 2000-OPEN
           END-IF
           IF NOT HARD-ERR
              PERFORM 3000-READ-PRM
           END-IF
           IF NOT HARD-ERR
              PERFORM 4000-READ-POL
           END-IF
           IF NOT HARD-ERR
              PERFORM 5000-EDIT-DETAIL
           END-IF
           PERFORM 9000-CLOSE
           IF HARD-ERR
              MOVE 8 TO RETURN-CODE
           ELSE
              MOVE 0 TO RETURN-CODE
           END-IF
           GOBACK.
      *
       1000-INIT.
           MOVE "00" TO LK-OUT-RTN-KBN
           MOVE SPACE TO LK-OUT-ATTN-CD
           MOVE SPACE TO LK-OUT-POL-NO
           MOVE SPACE TO LK-OUT-PRM-ID
           MOVE SPACE TO LK-OUT-BAND-KBN
           MOVE ZERO TO WS-DISP-AMT
           MOVE ZERO TO WS-DISP-SUM
           MOVE WS-DISP-AMT TO LK-OUT-PRM-AMT
           MOVE WS-DISP-SUM TO LK-OUT-SUM-ASSURED
           MOVE SPACE TO LK-OUT-CALC-ST
           MOVE SPACE TO LK-OUT-POL-ST
           MOVE SPACE TO LK-OUT-REASON
           MOVE "N" TO WS-LFPRMF-EOF
           MOVE "N" TO WS-LFPOLF-EOF
           MOVE "N" TO WS-PRM-FOUND
           MOVE "N" TO WS-POL-FOUND
           MOVE "N" TO WS-HARD-ERR
           MOVE "N" TO WS-LFPRMF-OPEN
           MOVE "N" TO WS-LFPOLF-OPEN
           IF LK-IN-PRM-ID = SPACE OR LK-IN-POL-NO = SPACE
              MOVE "99" TO LK-OUT-RTN-KBN
              MOVE "入力契約番号または明細番号が未設定"
                TO LK-OUT-REASON
              DISPLAY "LF240S 入力値不正"
              SET HARD-ERR TO TRUE
           END-IF.
      *
       2000-OPEN.
           OPEN INPUT LFPRMF
           IF WS-LFPRMF-ST = "00"
              SET LFPRMF-OPEN TO TRUE
           ELSE
              STRING "LFPRMF オープン失敗 ST="
                     WS-LFPRMF-ST
                     DELIMITED BY SIZE
                     INTO WS-MSG
              END-STRING
              DISPLAY WS-MSG
              MOVE "98" TO LK-OUT-RTN-KBN
              MOVE "保険料明細ファイルのオープン失敗"
                TO LK-OUT-REASON
              SET HARD-ERR TO TRUE
           END-IF
           IF NOT HARD-ERR
              OPEN INPUT LFPOLF
              IF WS-LFPOLF-ST = "00"
                 SET LFPOLF-OPEN TO TRUE
              ELSE
                 STRING "LFPOLF オープン失敗 ST="
                        WS-LFPOLF-ST
                        DELIMITED BY SIZE
                        INTO WS-MSG
                 END-STRING
                 DISPLAY WS-MSG
                 MOVE "98" TO LK-OUT-RTN-KBN
                 MOVE "契約ファイルのオープン失敗"
                   TO LK-OUT-REASON
                 SET HARD-ERR TO TRUE
              END-IF
           END-IF.
      *
       3000-READ-PRM.
           PERFORM UNTIL LFPRMF-EOF OR PRM-FOUND OR HARD-ERR
              READ LFPRMF
                 AT END
                    SET LFPRMF-EOF TO TRUE
                 NOT AT END
                    IF WS-LFPRMF-ST = "00"
                       IF PR-PRM-ID = LK-IN-PRM-ID
                          SET PRM-FOUND TO TRUE
                       END-IF
                    ELSE
                       STRING "LFPRMF 読込失敗 ST="
                              WS-LFPRMF-ST
                              DELIMITED BY SIZE
                              INTO WS-MSG
                       END-STRING
                       DISPLAY WS-MSG
                       MOVE "98" TO LK-OUT-RTN-KBN
                       MOVE "保険料明細ファイルの読込失敗"
                         TO LK-OUT-REASON
                       SET HARD-ERR TO TRUE
                    END-IF
              END-READ
           END-PERFORM
           IF NOT HARD-ERR AND NOT PRM-FOUND
              MOVE "11" TO LK-OUT-RTN-KBN
              MOVE "保険料明細が存在しない" TO LK-OUT-REASON
           END-IF.
      *
       4000-READ-POL.
           IF PRM-FOUND
              PERFORM UNTIL LFPOLF-EOF OR POL-FOUND OR HARD-ERR
                 READ LFPOLF
                    AT END
                       SET LFPOLF-EOF TO TRUE
                    NOT AT END
                       IF WS-LFPOLF-ST = "00"
                          IF PO-POL-NO = PR-POL-NO
                             SET POL-FOUND TO TRUE
                          END-IF
                       ELSE
                          STRING "LFPOLF 読込失敗 ST="
                                 WS-LFPOLF-ST
                                 DELIMITED BY SIZE
                                 INTO WS-MSG
                          END-STRING
                          DISPLAY WS-MSG
                          MOVE "98" TO LK-OUT-RTN-KBN
                          MOVE "契約ファイルの読込失敗"
                            TO LK-OUT-REASON
                          SET HARD-ERR TO TRUE
                       END-IF
                 END-READ
              END-PERFORM
              IF NOT HARD-ERR AND NOT POL-FOUND
                 MOVE "12" TO LK-OUT-RTN-KBN
                 MOVE "契約が存在しない" TO LK-OUT-REASON
              END-IF
           END-IF.
      *
       5000-EDIT-DETAIL.
           IF PRM-FOUND AND POL-FOUND
              MOVE PR-POL-NO TO LK-OUT-POL-NO
              MOVE PR-PRM-ID TO LK-OUT-PRM-ID
              MOVE PR-BAND-KBN TO LK-OUT-BAND-KBN
              MOVE PR-CALC-STATUS-KBN TO LK-OUT-CALC-ST
              MOVE PO-POL-STATUS-KBN TO LK-OUT-POL-ST
              MOVE PR-PRM-AMT TO WS-DISP-AMT
              MOVE PR-SUM-ASSURED-AMT TO WS-DISP-SUM
              MOVE WS-DISP-AMT TO LK-OUT-PRM-AMT
              MOVE WS-DISP-SUM TO LK-OUT-SUM-ASSURED
              PERFORM 5100-JUDGE-BAND
              PERFORM 5200-JUDGE-DETAIL
           END-IF.
      *
       5100-JUDGE-BAND.
           EVALUATE TRUE
              WHEN PO-ENTRY-AGE-CNT <= 29
                 MOVE "A1" TO WS-WORK-BAND
              WHEN PO-ENTRY-AGE-CNT <= 39
                 MOVE "A2" TO WS-WORK-BAND
              WHEN PO-ENTRY-AGE-CNT <= 49
                 MOVE "A3" TO WS-WORK-BAND
              WHEN PO-ENTRY-AGE-CNT <= 59
                 MOVE "A4" TO WS-WORK-BAND
              WHEN OTHER
                 MOVE "A5" TO WS-WORK-BAND
           END-EVALUATE
           IF PR-BAND-KBN NOT = WS-WORK-BAND
              MOVE "B1" TO LK-OUT-ATTN-CD
           END-IF.
      *
       5200-JUDGE-DETAIL.
           EVALUATE TRUE
              WHEN PR-POL-NO NOT = LK-IN-POL-NO
                 MOVE "30" TO LK-OUT-RTN-KBN
                 MOVE "契約番号不一致" TO LK-OUT-REASON
              WHEN PR-PRM-AMT = ZERO
                 MOVE "10" TO LK-OUT-RTN-KBN
                 MOVE "保険料金額ゼロ" TO LK-OUT-REASON
              WHEN PR-CALC-STATUS-KBN = "01"
                 MOVE "00" TO LK-OUT-RTN-KBN
                 MOVE "C1" TO LK-OUT-ATTN-CD
                 MOVE "承認待ち異動中" TO LK-OUT-REASON
              WHEN PR-CALC-STATUS-KBN NOT = "00"
                 MOVE "20" TO LK-OUT-RTN-KBN
                 MOVE "保険料計算状態異常" TO LK-OUT-REASON
              WHEN PO-POL-STATUS-KBN = "02"
                 MOVE "40" TO LK-OUT-RTN-KBN
                 MOVE "失効済み契約のため明細表示対象外"
                   TO LK-OUT-REASON
              WHEN PO-POL-STATUS-KBN = "09"
                 MOVE "41" TO LK-OUT-RTN-KBN
                 MOVE "解約済み契約のため明細表示対象外"
                   TO LK-OUT-REASON
              WHEN PO-POL-STATUS-KBN NOT = "01"
                 MOVE "42" TO LK-OUT-RTN-KBN
                 MOVE "契約状態区分不正" TO LK-OUT-REASON
              WHEN PO-SUM-ASSURED-AMT NOT = PR-SUM-ASSURED-AMT
                 MOVE "00" TO LK-OUT-RTN-KBN
                 MOVE "S1" TO LK-OUT-ATTN-CD
                 MOVE "保険金額差異あり" TO LK-OUT-REASON
              WHEN OTHER
                 MOVE "00" TO LK-OUT-RTN-KBN
                 MOVE "正常" TO LK-OUT-REASON
           END-EVALUATE.
      *
       9000-CLOSE.
           IF LFPRMF-OPEN
              CLOSE LFPRMF
              IF WS-LFPRMF-ST NOT = "00"
                 DISPLAY "LFPRMF クローズ失敗 ST=" WS-LFPRMF-ST
                 SET HARD-ERR TO TRUE
              END-IF
           END-IF
           IF LFPOLF-OPEN
              CLOSE LFPOLF
              IF WS-LFPOLF-ST NOT = "00"
                 DISPLAY "LFPOLF クローズ失敗 ST=" WS-LFPOLF-ST
                 SET HARD-ERR TO TRUE
              END-IF
           END-IF.
