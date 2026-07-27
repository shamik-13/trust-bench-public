       IDENTIFICATION DIVISION.
       PROGRAM-ID. CM140B.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CMMOVF ASSIGN TO "CMMOVF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS MV-RECEIPT-NO
               FILE STATUS IS WS-ST-CMMOVF.

           SELECT CMCIFF ASSIGN TO "CMCIFF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-ST-CMCIFF.

           SELECT CKERRF ASSIGN TO "CKERRF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-ST-CKERRF.

       DATA DIVISION.
       FILE SECTION.
       FD  CMMOVF.
           COPY CMMOVC.

       FD  CMCIFF.
           COPY CMCIFFC.

       FD  CKERRF.
           COPY CKERRC.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05 WS-ST-CMMOVF              PIC XX VALUE SPACES.
           05 WS-ST-CMCIFF              PIC XX VALUE SPACES.
           05 WS-ST-CKERRF              PIC XX VALUE SPACES.

       01  WS-SWITCHES.
           05 WS-EOF-CMMOVF             PIC X VALUE "N".
              88 EOF-CMMOVF                  VALUE "Y".
           05 WS-EOF-CMCIFF             PIC X VALUE "N".
              88 EOF-CMCIFF                  VALUE "Y".
           05 WS-HARD-ERROR             PIC X VALUE "N".
              88 HARD-ERROR                  VALUE "Y".
           05 WS-CIF-FOUND              PIC X VALUE "N".
              88 CIF-FOUND                   VALUE "Y".
           05 WS-RECEIPT-DUP            PIC X VALUE "N".
              88 RECEIPT-DUP                 VALUE "Y".
           05 WS-LATEST-FOUND           PIC X VALUE "N".
              88 LATEST-FOUND                VALUE "Y".

       01  WS-CONSTANTS.
           05 WS-PGM-ID                 PIC X(8) VALUE "CM140B".
           05 WS-VALID-CIF-ST           PIC XX VALUE "01".
           05 WS-EXCL-CIF-ST            PIC XX VALUE "08".
           05 WS-INVALID-CIF-ST         PIC XX VALUE "09".
           05 WS-ST-ACTIVE              PIC X VALUE "1".
           05 WS-ST-SUPPRESS            PIC X VALUE "9".
           05 WS-ST-DUPLICATE           PIC X VALUE "8".
           05 WS-CD-DUP-RECEIPT         PIC X(6) VALUE "E14001".
           05 WS-CD-CIF-NOTFOUND        PIC X(6) VALUE "E14002".
           05 WS-CD-CIF-EXCLUDE         PIC X(6) VALUE "E14003".
           05 WS-CD-CIF-INVALID         PIC X(6) VALUE "E14004".
           05 WS-CD-CIF-STATUS          PIC X(6) VALUE "E14005".

       01  WS-COUNTERS.
           05 WS-CF-CNT                 PIC 9(5) VALUE ZERO.
           05 WS-MV-CNT                 PIC 9(6) VALUE ZERO.
           05 WS-ERR-CNT                PIC 9(6) VALUE ZERO.
           05 WS-UPD-CNT                PIC 9(6) VALUE ZERO.
           05 WS-I                      PIC 9(6) VALUE ZERO.
           05 WS-J                      PIC 9(6) VALUE ZERO.
           05 WS-K                      PIC 9(6) VALUE ZERO.

       01  WS-CURRENT-DATE.
           05 WS-CUR-DT                 PIC 9(8).
           05 WS-CUR-TM                 PIC 9(6).
           05 FILLER                    PIC 9(5).

       01  WS-WORK-AREA.
           05 WS-ERR-ID-NUM             PIC 9(10) VALUE ZERO.
           05 WS-PRIORITY               PIC 9 VALUE ZERO.
           05 WS-BEST-PRIORITY          PIC 9 VALUE ZERO.
           05 WS-BEST-DATE              PIC 9(8) VALUE ZERO.
           05 WS-BEST-MOVE-KBN          PIC XX VALUE SPACES.
           05 WS-BEST-RECEIPT           PIC X(20) VALUE SPACES.
           05 WS-KEY-ID                 PIC X(30) VALUE SPACES.
           05 WS-MSG                    PIC X(80) VALUE SPACES.

       01  WS-CIF-TABLE.
           05 WS-CIF-ENTRY OCCURS 2000 TIMES.
              10 TB-CF-CIF-NO           PIC X(12).
              10 TB-CF-STATUS           PIC XX.

       01  WS-MOVE-TABLE.
           05 WS-MOVE-ENTRY OCCURS 5000 TIMES.
              10 TB-MV-RECEIPT-NO       PIC X(20).
              10 TB-MV-CIF-NO           PIC X(12).
              10 TB-MV-MOVE-KBN         PIC XX.
              10 TB-MV-REQUEST-DT       PIC 9(8).
              10 TB-MV-VALID-SW         PIC X.
              10 TB-MV-STATUS           PIC X.

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE 0 TO RETURN-CODE
           ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD
           PERFORM 1000-OPEN-FILES
           IF NOT HARD-ERROR
              PERFORM 2000-LOAD-CIF
           END-IF
           IF NOT HARD-ERROR
              PERFORM 3000-READ-MOVES
           END-IF
           IF NOT HARD-ERROR
              PERFORM 4000-DECIDE-MOVES
           END-IF
           PERFORM 9000-CLOSE-FILES
           IF HARD-ERROR
              MOVE 8 TO RETURN-CODE
           ELSE
              DISPLAY "CM140B 正常終了 入力="
                      WS-MV-CNT " 更新=" WS-UPD-CNT
                      " エラー=" WS-ERR-CNT
              MOVE 0 TO RETURN-CODE
           END-IF
           GOBACK.

       1000-OPEN-FILES.
           OPEN I-O CMMOVF
           IF WS-ST-CMMOVF NOT = "00"
              DISPLAY "CMMOVF オープン失敗 ST=" WS-ST-CMMOVF
              SET HARD-ERROR TO TRUE
           END-IF

           IF NOT HARD-ERROR
              OPEN INPUT CMCIFF
              IF WS-ST-CMCIFF NOT = "00"
                 DISPLAY "CMCIFF オープン失敗 ST=" WS-ST-CMCIFF
                 SET HARD-ERROR TO TRUE
              END-IF
           END-IF

           IF NOT HARD-ERROR
              OPEN OUTPUT CKERRF
              IF WS-ST-CKERRF NOT = "00"
                 DISPLAY "CKERRF オープン失敗 ST=" WS-ST-CKERRF
                 SET HARD-ERROR TO TRUE
              END-IF
           END-IF.

       2000-LOAD-CIF.
           PERFORM UNTIL EOF-CMCIFF OR HARD-ERROR
              READ CMCIFF
                 AT END
                    SET EOF-CMCIFF TO TRUE
                 NOT AT END
                    IF WS-CF-CNT < 2000
                       ADD 1 TO WS-CF-CNT
                       MOVE CF-CIF-NO TO TB-CF-CIF-NO(WS-CF-CNT)
                       MOVE CF-CIF-STATUS-KBN
                         TO TB-CF-STATUS(WS-CF-CNT)
                    ELSE
                       DISPLAY "CMCIFF 件数上限超過"
                       SET HARD-ERROR TO TRUE
                    END-IF
              END-READ
           END-PERFORM.

       3000-READ-MOVES.
           MOVE LOW-VALUE TO MV-RECEIPT-NO
           START CMMOVF KEY IS NOT LESS THAN MV-RECEIPT-NO
              INVALID KEY
                 IF WS-ST-CMMOVF = "23"
                    SET EOF-CMMOVF TO TRUE
                 ELSE
                    DISPLAY "CMMOVF START失敗 ST=" WS-ST-CMMOVF
                    SET HARD-ERROR TO TRUE
                 END-IF
           END-START

           PERFORM UNTIL EOF-CMMOVF OR HARD-ERROR
              READ CMMOVF NEXT RECORD
                 AT END
                    SET EOF-CMMOVF TO TRUE
                 NOT AT END
                    IF WS-MV-CNT < 5000
                       ADD 1 TO WS-MV-CNT
                       MOVE MV-RECEIPT-NO
                         TO TB-MV-RECEIPT-NO(WS-MV-CNT)
                       MOVE MV-CIF-NO TO TB-MV-CIF-NO(WS-MV-CNT)
                       MOVE MV-MOVE-KBN TO TB-MV-MOVE-KBN(WS-MV-CNT)
                       MOVE MV-REQUEST-DT TO TB-MV-REQUEST-DT(WS-MV-CNT)
                       MOVE "Y" TO TB-MV-VALID-SW(WS-MV-CNT)
                       MOVE MV-MOVE-STATUS-KBN
                         TO TB-MV-STATUS(WS-MV-CNT)
                    ELSE
                       DISPLAY "CMMOVF 件数上限超過"
                       SET HARD-ERROR TO TRUE
                    END-IF
              END-READ
           END-PERFORM.

       4000-DECIDE-MOVES.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-MV-CNT
              PERFORM 4100-CHECK-RECEIPT-DUP
              IF RECEIPT-DUP
                 MOVE "N" TO TB-MV-VALID-SW(WS-I)
                 MOVE WS-CD-DUP-RECEIPT TO ER-ERROR-CD
                 MOVE TB-MV-CIF-NO(WS-I) TO ER-CIF-NO
                 MOVE TB-MV-RECEIPT-NO(WS-I) TO WS-KEY-ID
                 PERFORM 8000-WRITE-ERROR
              ELSE
                 PERFORM 4200-CHECK-CIF
              END-IF
           END-PERFORM

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-MV-CNT
              IF TB-MV-VALID-SW(WS-I) = "Y"
                 PERFORM 4300-FIND-LATEST
                 PERFORM 4400-UPDATE-SAME-CIF
              END-IF
           END-PERFORM.

       4100-CHECK-RECEIPT-DUP.
           MOVE "N" TO WS-RECEIPT-DUP
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J >= WS-I
              IF TB-MV-RECEIPT-NO(WS-J) = TB-MV-RECEIPT-NO(WS-I)
                 SET RECEIPT-DUP TO TRUE
              END-IF
           END-PERFORM.

       4200-CHECK-CIF.
           MOVE "N" TO WS-CIF-FOUND
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > WS-CF-CNT
              IF TB-CF-CIF-NO(WS-J) = TB-MV-CIF-NO(WS-I)
                 SET CIF-FOUND TO TRUE
                 EVALUATE TB-CF-STATUS(WS-J)
                    WHEN "01"
                       CONTINUE
                    WHEN "08"
                       MOVE "N" TO TB-MV-VALID-SW(WS-I)
                       MOVE WS-CD-CIF-EXCLUDE TO ER-ERROR-CD
                       MOVE TB-MV-CIF-NO(WS-I) TO ER-CIF-NO
                       MOVE TB-MV-RECEIPT-NO(WS-I) TO WS-KEY-ID
                       PERFORM 8000-WRITE-ERROR
                    WHEN "09"
                       MOVE "N" TO TB-MV-VALID-SW(WS-I)
                       MOVE WS-CD-CIF-INVALID TO ER-ERROR-CD
                       MOVE TB-MV-CIF-NO(WS-I) TO ER-CIF-NO
                       MOVE TB-MV-RECEIPT-NO(WS-I) TO WS-KEY-ID
                       PERFORM 8000-WRITE-ERROR
                    WHEN OTHER
                       MOVE "N" TO TB-MV-VALID-SW(WS-I)
                       MOVE WS-CD-CIF-STATUS TO ER-ERROR-CD
                       MOVE TB-MV-CIF-NO(WS-I) TO ER-CIF-NO
                       MOVE TB-MV-RECEIPT-NO(WS-I) TO WS-KEY-ID
                       PERFORM 8000-WRITE-ERROR
                 END-EVALUATE
                 MOVE WS-CF-CNT TO WS-J
              END-IF
           END-PERFORM

           IF NOT CIF-FOUND
              MOVE "N" TO TB-MV-VALID-SW(WS-I)
              MOVE WS-CD-CIF-NOTFOUND TO ER-ERROR-CD
              MOVE TB-MV-CIF-NO(WS-I) TO ER-CIF-NO
              MOVE TB-MV-RECEIPT-NO(WS-I) TO WS-KEY-ID
              PERFORM 8000-WRITE-ERROR
           END-IF.

       4300-FIND-LATEST.
           MOVE "N" TO WS-LATEST-FOUND
           MOVE ZERO TO WS-BEST-DATE
           MOVE ZERO TO WS-BEST-PRIORITY
           MOVE SPACES TO WS-BEST-RECEIPT
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > WS-MV-CNT
              IF TB-MV-VALID-SW(WS-J) = "Y"
                 AND TB-MV-CIF-NO(WS-J) = TB-MV-CIF-NO(WS-I)
                 PERFORM 4500-SET-PRIORITY
                 IF NOT LATEST-FOUND
                    MOVE TB-MV-REQUEST-DT(WS-J) TO WS-BEST-DATE
                    MOVE WS-PRIORITY TO WS-BEST-PRIORITY
                    MOVE TB-MV-MOVE-KBN(WS-J) TO WS-BEST-MOVE-KBN
                    MOVE TB-MV-RECEIPT-NO(WS-J) TO WS-BEST-RECEIPT
                    SET LATEST-FOUND TO TRUE
                 ELSE
                    IF TB-MV-REQUEST-DT(WS-J) > WS-BEST-DATE
                       MOVE TB-MV-REQUEST-DT(WS-J) TO WS-BEST-DATE
                       MOVE WS-PRIORITY TO WS-BEST-PRIORITY
                       MOVE TB-MV-MOVE-KBN(WS-J) TO WS-BEST-MOVE-KBN
                       MOVE TB-MV-RECEIPT-NO(WS-J) TO WS-BEST-RECEIPT
                    ELSE
                       IF TB-MV-REQUEST-DT(WS-J) = WS-BEST-DATE
                          AND WS-PRIORITY > WS-BEST-PRIORITY
                          MOVE WS-PRIORITY TO WS-BEST-PRIORITY
                          MOVE TB-MV-MOVE-KBN(WS-J)
                            TO WS-BEST-MOVE-KBN
                          MOVE TB-MV-RECEIPT-NO(WS-J)
                            TO WS-BEST-RECEIPT
                       END-IF
                    END-IF
                 END-IF
              END-IF
           END-PERFORM.

       4400-UPDATE-SAME-CIF.
           PERFORM VARYING WS-K FROM 1 BY 1 UNTIL WS-K > WS-MV-CNT
              IF TB-MV-VALID-SW(WS-K) = "Y"
                 AND TB-MV-CIF-NO(WS-K) = TB-MV-CIF-NO(WS-I)
                 MOVE TB-MV-RECEIPT-NO(WS-K) TO MV-RECEIPT-NO
                 READ CMMOVF KEY IS MV-RECEIPT-NO
                    INVALID KEY
                       DISPLAY "CMMOVF 再読込失敗 ST=" WS-ST-CMMOVF
                       SET HARD-ERROR TO TRUE
                    NOT INVALID KEY
                       IF TB-MV-RECEIPT-NO(WS-K) = WS-BEST-RECEIPT
                          MOVE WS-ST-ACTIVE TO MV-MOVE-STATUS-KBN
                          MOVE WS-ST-ACTIVE TO TB-MV-STATUS(WS-K)
                       ELSE
                          MOVE WS-ST-SUPPRESS TO MV-MOVE-STATUS-KBN
                          MOVE WS-ST-SUPPRESS TO TB-MV-STATUS(WS-K)
                       END-IF
                       REWRITE CMMOVF-REC
                          INVALID KEY
                             DISPLAY "CMMOVF 更新失敗 ST="
                                     WS-ST-CMMOVF
                             SET HARD-ERROR TO TRUE
                          NOT INVALID KEY
                             ADD 1 TO WS-UPD-CNT
                       END-REWRITE
                 END-READ
              END-IF
           END-PERFORM.

       4500-SET-PRIORITY.
           EVALUATE TB-MV-MOVE-KBN(WS-J)
              WHEN "30"
                 MOVE 5 TO WS-PRIORITY
              WHEN "20"
                 MOVE 4 TO WS-PRIORITY
              WHEN "10"
                 MOVE 3 TO WS-PRIORITY
              WHEN "40"
                 MOVE 2 TO WS-PRIORITY
              WHEN OTHER
                 MOVE 1 TO WS-PRIORITY
           END-EVALUATE.

       8000-WRITE-ERROR.
           ADD 1 TO WS-ERR-ID-NUM
           ADD 1 TO WS-ERR-CNT
           INITIALIZE CKERRF-REC
           MOVE WS-ERR-ID-NUM TO ER-ERROR-ID
           MOVE WS-PGM-ID TO ER-SOURCE-PGM-ID
           MOVE WS-KEY-ID TO ER-KEY-ID
           MOVE WS-CUR-DT TO ER-ERROR-DT
           WRITE CKERRF-REC
           IF WS-ST-CKERRF NOT = "00"
              DISPLAY "CKERRF 書込失敗 ST=" WS-ST-CKERRF
              SET HARD-ERROR TO TRUE
           END-IF.

       9000-CLOSE-FILES.
           CLOSE CMMOVF
           IF WS-ST-CMMOVF NOT = "00"
              DISPLAY "CMMOVF クローズ失敗 ST=" WS-ST-CMMOVF
              SET HARD-ERROR TO TRUE
           END-IF

           CLOSE CMCIFF
           IF WS-ST-CMCIFF NOT = "00"
              DISPLAY "CMCIFF クローズ失敗 ST=" WS-ST-CMCIFF
              SET HARD-ERROR TO TRUE
           END-IF

           CLOSE CKERRF
           IF WS-ST-CKERRF NOT = "00"
              DISPLAY "CKERRF クローズ失敗 ST=" WS-ST-CKERRF
              SET HARD-ERROR TO TRUE
           END-IF.
