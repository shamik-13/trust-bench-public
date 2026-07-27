       IDENTIFICATION DIVISION.
       PROGRAM-ID. CB710B.
      *
      * 変更履歴
      * 版数  年月日    担当   概要
      * 1.00  20240115  BT01   初版作成
      * 1.01  20240520  BT02   外貨集計を追加
      * 1.02  20241108  BT03   状態検査を強化
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CDEXCPF ASSIGN TO "CDEXCPF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-CDEXCPF.
           SELECT CDSALEF ASSIGN TO "CDSALEF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-CDSALEF.
           SELECT CDCARDF ASSIGN TO "CDCARDF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CF-CARD-NO
               FILE STATUS IS FS-CDCARDF.
           SELECT CDMERCF ASSIGN TO "CDMERCF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS MC-MERCHANT-CODE
               FILE STATUS IS FS-CDMERCF.
           SELECT SORTWK ASSIGN TO "SORTWK".
       DATA DIVISION.
       FILE SECTION.
       FD  CDEXCPF.
           COPY CDEXCPC.
       FD  CDSALEF.
           COPY CDSALEFC.
       FD  CDCARDF.
           COPY CDCARD03.
       FD  CDMERCF.
           COPY CDMERCC.
       SD  SORTWK.
       01  SORT-REC.
           05  SR-REASON-CD             PIC X(02).
           05  SR-DETECTED-PGM          PIC X(08).
           05  SR-MERCHANT-CODE         PIC X(10).
           05  SR-CARD-STATUS           PIC X(02).
           05  SR-EXCEPTION-ID          PIC X(12).
           05  SR-SALE-ID               PIC X(12).
           05  SR-CARD-NO               PIC X(16).
           05  SR-EXCEPTION-DT          PIC 9(08).
           05  SR-SALE-DT               PIC 9(08).
           05  SR-SALE-AMT              PIC S9(11)V99.
           05  SR-CURRENCY-CD           PIC X(03).
           05  SR-MERCHANT-NAME-KANA    PIC X(40).
           05  SR-MERCHANT-STATUS       PIC X(02).
           05  SR-FEE-PLAN-CD           PIC X(04).
           05  SR-MEMBER-ID             PIC X(12).
           05  SR-MEMBER-NAME-KANA      PIC X(40).
           05  SR-FOREIGN-FEE           PIC S9(09)V99.
           05  SR-DEDUCT-AMT            PIC S9(09)V99.
           05  SR-REASON-TEXT           PIC X(40).
           05  SR-NOTE                  PIC X(40).
       WORKING-STORAGE SECTION.
       01  FILE-STATUS-AREA.
           05  FS-CDEXCPF               PIC X(02) VALUE SPACES.
           05  FS-CDSALEF               PIC X(02) VALUE SPACES.
           05  FS-CDCARDF               PIC X(02) VALUE SPACES.
           05  FS-CDMERCF               PIC X(02) VALUE SPACES.
       01  END-FLAGS.
           05  EOF-EXCEPTION            PIC X VALUE "N".
           05  EOF-SALE                 PIC X VALUE "N".
           05  EOF-SORT                 PIC X VALUE "N".
       01  COUNTERS.
           05  CNT-EXCEPTION-IN         PIC 9(09) VALUE ZERO.
           05  CNT-SALE-IN              PIC 9(09) VALUE ZERO.
           05  CNT-SORT-OUT             PIC 9(09) VALUE ZERO.
           05  CNT-SALE-MISS            PIC 9(09) VALUE ZERO.
           05  CNT-CARD-MISS            PIC 9(09) VALUE ZERO.
           05  CNT-MERC-MISS            PIC 9(09) VALUE ZERO.
           05  CNT-SKIP-DONE            PIC 9(09) VALUE ZERO.
       01  SALE-TABLE-AREA.
           05  SALE-MAX                 PIC 9(05) VALUE 20000.
           05  SALE-CNT                 PIC 9(05) VALUE ZERO.
           05  SALE-IDX                 PIC 9(05) VALUE ZERO.
           05  SALE-FOUND-FLAG          PIC X VALUE "N".
           05  SALE-TBL OCCURS 20000 TIMES.
               10  T-SALE-ID            PIC X(12).
               10  T-CARD-NO            PIC X(16).
               10  T-SALE-AMT           PIC S9(11)V99.
               10  T-CURRENCY-CD        PIC X(03).
               10  T-MERCHANT-CODE      PIC X(10).
               10  T-SALE-DT            PIC 9(08).
               10  T-AUTH-ID            PIC X(12).
       01  WORK-AREA.
           05  WK-HARD-ERROR            PIC X VALUE "N".
           05  WK-FOREIGN-RATE          PIC 9V999 VALUE 0.025.
           05  WK-DEDUCT-RATE           PIC 9V999 VALUE 0.010.
           05  WK-CARD-OK               PIC X VALUE "N".
           05  WK-MERC-OK               PIC X VALUE "N".
           05  WK-SALE-AMT              PIC S9(11)V99 VALUE ZERO.
           05  WK-FOREIGN-FEE           PIC S9(09)V99 VALUE ZERO.
           05  WK-DEDUCT-AMT            PIC S9(09)V99 VALUE ZERO.
       01  REPORT-LINE.
           05  RL-REASON-CD             PIC X(02).
           05  FILLER                   PIC X VALUE SPACE.
           05  RL-DETECTED-PGM          PIC X(08).
           05  FILLER                   PIC X VALUE SPACE.
           05  RL-MERCHANT-CODE         PIC X(10).
           05  FILLER                   PIC X VALUE SPACE.
           05  RL-CARD-STATUS           PIC X(02).
           05  FILLER                   PIC X VALUE SPACE.
           05  RL-EXCEPTION-ID          PIC X(12).
           05  FILLER                   PIC X VALUE SPACE.
           05  RL-SALE-ID               PIC X(12).
           05  FILLER                   PIC X VALUE SPACE.
           05  RL-CURRENCY-CD           PIC X(03).
           05  FILLER                   PIC X VALUE SPACE.
           05  RL-SALE-AMT              PIC ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                   PIC X VALUE SPACE.
           05  RL-FOREIGN-FEE           PIC ZZZ,ZZZ,ZZ9.99.
           05  FILLER                   PIC X VALUE SPACE.
           05  RL-DEDUCT-AMT            PIC ZZZ,ZZZ,ZZ9.99.
           05  FILLER                   PIC X VALUE SPACE.
           05  RL-REASON-TEXT           PIC X(40).
       PROCEDURE DIVISION.
       MAIN-SECTION.
           MOVE 0 TO RETURN-CODE
           PERFORM 1000-OPEN-FILES
           IF WK-HARD-ERROR = "N"
               PERFORM 2000-LOAD-SALES
           END-IF
           IF WK-HARD-ERROR = "N"
               SORT SORTWK
                   ON ASCENDING KEY SR-REASON-CD
                                    SR-DETECTED-PGM
                                    SR-MERCHANT-CODE
                                    SR-CARD-STATUS
                                    SR-EXCEPTION-ID
                   INPUT PROCEDURE IS 3000-BUILD-SORT
                   OUTPUT PROCEDURE IS 5000-PRINT-SORT
               IF SORT-RETURN NOT = ZERO
                   DISPLAY "CB710B SORT NG ST=" SORT-RETURN
                   MOVE 12 TO RETURN-CODE
                   MOVE "Y" TO WK-HARD-ERROR
               END-IF
           END-IF
           PERFORM 9000-CLOSE-FILES
           IF WK-HARD-ERROR = "N"
               DISPLAY "CB710B OK EX-IN=" CNT-EXCEPTION-IN
               DISPLAY "CB710B RPT-CNT=" CNT-SORT-OUT
               DISPLAY "CB710B SALE-MISS=" CNT-SALE-MISS
               DISPLAY "CB710B CARD-MISS=" CNT-CARD-MISS
               DISPLAY "CB710B MERC-MISS=" CNT-MERC-MISS
               MOVE 0 TO RETURN-CODE
           END-IF
           GOBACK.
      *
       1000-OPEN-FILES.
           OPEN INPUT CDEXCPF
           IF FS-CDEXCPF NOT = "00"
               DISPLAY "CDEXCPF OPEN NG ST=" FS-CDEXCPF
               MOVE 12 TO RETURN-CODE
               MOVE "Y" TO WK-HARD-ERROR
           END-IF
           OPEN INPUT CDSALEF
           IF FS-CDSALEF NOT = "00"
               DISPLAY "CDSALEF OPEN NG ST=" FS-CDSALEF
               MOVE 12 TO RETURN-CODE
               MOVE "Y" TO WK-HARD-ERROR
           END-IF
           OPEN INPUT CDCARDF
           IF FS-CDCARDF NOT = "00"
               DISPLAY "CDCARDF OPEN NG ST=" FS-CDCARDF
               MOVE 12 TO RETURN-CODE
               MOVE "Y" TO WK-HARD-ERROR
           END-IF
           OPEN INPUT CDMERCF
           IF FS-CDMERCF NOT = "00"
               DISPLAY "CDMERCF OPEN NG ST=" FS-CDMERCF
               MOVE 12 TO RETURN-CODE
               MOVE "Y" TO WK-HARD-ERROR
           END-IF.
      *
       2000-LOAD-SALES.
           PERFORM UNTIL EOF-SALE = "Y" OR WK-HARD-ERROR = "Y"
               READ CDSALEF
                   AT END
                       MOVE "Y" TO EOF-SALE
                   NOT AT END
                       ADD 1 TO CNT-SALE-IN
                       IF SALE-CNT >= SALE-MAX
                           DISPLAY "CDSALEF TABLE FULL"
                           DISPLAY "SALE CNT=" CNT-SALE-IN
                           MOVE 12 TO RETURN-CODE
                           MOVE "Y" TO WK-HARD-ERROR
                       ELSE
                           ADD 1 TO SALE-CNT
                           MOVE SL-SALE-ID
                               TO T-SALE-ID(SALE-CNT)
                           MOVE SL-CARD-NO
                               TO T-CARD-NO(SALE-CNT)
                           MOVE SL-SALE-AMT
                               TO T-SALE-AMT(SALE-CNT)
                           MOVE SL-CURRENCY-CD
                               TO T-CURRENCY-CD(SALE-CNT)
                           MOVE SL-MERCHANT-CODE
                               TO T-MERCHANT-CODE(SALE-CNT)
                           MOVE SL-SALE-DT
                               TO T-SALE-DT(SALE-CNT)
                           MOVE SL-AUTH-ID
                               TO T-AUTH-ID(SALE-CNT)
                       END-IF
               END-READ
               IF FS-CDSALEF NOT = "00" AND FS-CDSALEF NOT = "10"
                   DISPLAY "CDSALEF READ NG ST=" FS-CDSALEF
                   MOVE 12 TO RETURN-CODE
                   MOVE "Y" TO WK-HARD-ERROR
               END-IF
           END-PERFORM.
      *
       3000-BUILD-SORT.
           PERFORM UNTIL EOF-EXCEPTION = "Y" OR WK-HARD-ERROR = "Y"
               READ CDEXCPF
                   AT END
                       MOVE "Y" TO EOF-EXCEPTION
                   NOT AT END
                       ADD 1 TO CNT-EXCEPTION-IN
                       PERFORM 3100-PROCESS-EXCEPTION
               END-READ
               IF FS-CDEXCPF NOT = "00" AND FS-CDEXCPF NOT = "10"
                   DISPLAY "CDEXCPF READ NG ST=" FS-CDEXCPF
                   MOVE 12 TO RETURN-CODE
                   MOVE "Y" TO WK-HARD-ERROR
               END-IF
           END-PERFORM.
      *
       3100-PROCESS-EXCEPTION.
           IF EX-ACTION-STATUS NOT = SPACE
               ADD 1 TO CNT-SKIP-DONE
           ELSE
               PERFORM 3200-FIND-SALE
               PERFORM 3300-READ-CARD
               PERFORM 3400-READ-MERCHANT
               PERFORM 3500-CALCULATE-AMOUNTS
               PERFORM 3600-RELEASE-RECORD
           END-IF.
      *
       3200-FIND-SALE.
           MOVE "N" TO SALE-FOUND-FLAG
           PERFORM VARYING SALE-IDX FROM 1 BY 1
               UNTIL SALE-IDX > SALE-CNT OR SALE-FOUND-FLAG = "Y"
               IF T-SALE-ID(SALE-IDX) = EX-SALE-ID
                   MOVE "Y" TO SALE-FOUND-FLAG
               END-IF
           END-PERFORM
           IF SALE-FOUND-FLAG = "N"
               ADD 1 TO CNT-SALE-MISS
           END-IF.
      *
       3300-READ-CARD.
           MOVE "N" TO WK-CARD-OK
           MOVE EX-CARD-NO TO CF-CARD-NO
           READ CDCARDF KEY IS CF-CARD-NO
               INVALID KEY
                   ADD 1 TO CNT-CARD-MISS
               NOT INVALID KEY
                   IF CF-CARD-STATUS = "01"
                       OR CF-CARD-STATUS = "02"
                       OR CF-CARD-STATUS = "03"
                       OR CF-CARD-STATUS = "09"
                       MOVE "Y" TO WK-CARD-OK
                   ELSE
                       DISPLAY "CDCARDF BAD CARD STATUS"
                       DISPLAY "CARD=" CF-CARD-NO
                       MOVE 8 TO RETURN-CODE
                   END-IF
           END-READ
           IF FS-CDCARDF NOT = "00" AND FS-CDCARDF NOT = "23"
               DISPLAY "CDCARDF READ NG ST=" FS-CDCARDF
               MOVE 12 TO RETURN-CODE
               MOVE "Y" TO WK-HARD-ERROR
           END-IF.
      *
       3400-READ-MERCHANT.
           MOVE "N" TO WK-MERC-OK
           IF SALE-FOUND-FLAG = "Y"
               MOVE T-MERCHANT-CODE(SALE-IDX) TO MC-MERCHANT-CODE
               READ CDMERCF KEY IS MC-MERCHANT-CODE
                   INVALID KEY
                       ADD 1 TO CNT-MERC-MISS
                   NOT INVALID KEY
                       IF MC-MERCHANT-STATUS = "01"
                           OR MC-MERCHANT-STATUS = "02"
                           OR MC-MERCHANT-STATUS = "03"
                           OR MC-MERCHANT-STATUS = "09"
                           MOVE "Y" TO WK-MERC-OK
                       ELSE
                           DISPLAY "CDMERCF BAD MERCHANT STATUS"
                           DISPLAY "MERCHANT=" MC-MERCHANT-CODE
                           MOVE 8 TO RETURN-CODE
                       END-IF
               END-READ
               IF FS-CDMERCF NOT = "00" AND FS-CDMERCF NOT = "23"
                   DISPLAY "CDMERCF READ NG ST=" FS-CDMERCF
                   MOVE 12 TO RETURN-CODE
                   MOVE "Y" TO WK-HARD-ERROR
               END-IF
           ELSE
               ADD 1 TO CNT-MERC-MISS
           END-IF.
      *
       3500-CALCULATE-AMOUNTS.
           MOVE ZERO TO WK-SALE-AMT
           MOVE ZERO TO WK-FOREIGN-FEE
           MOVE ZERO TO WK-DEDUCT-AMT
           IF SALE-FOUND-FLAG = "Y"
               MOVE T-SALE-AMT(SALE-IDX) TO WK-SALE-AMT
               IF T-CURRENCY-CD(SALE-IDX) NOT = "JPY"
                   COMPUTE WK-FOREIGN-FEE ROUNDED =
                       T-SALE-AMT(SALE-IDX) * WK-FOREIGN-RATE
                   END-COMPUTE
               END-IF
               IF WK-MERC-OK = "Y"
                   AND MC-MERCHANT-STATUS NOT = "01"
                   COMPUTE WK-DEDUCT-AMT ROUNDED =
                       T-SALE-AMT(SALE-IDX) * WK-DEDUCT-RATE
                   END-COMPUTE
               END-IF
           END-IF.
      *
       3600-RELEASE-RECORD.
           INITIALIZE SORT-REC
           MOVE EX-REASON-CD TO SR-REASON-CD
           MOVE EX-DETECTED-PGM TO SR-DETECTED-PGM
           MOVE EX-EXCEPTION-ID TO SR-EXCEPTION-ID
           MOVE EX-SALE-ID TO SR-SALE-ID
           MOVE EX-CARD-NO TO SR-CARD-NO
           MOVE EX-EXCEPTION-DT TO SR-EXCEPTION-DT
           IF SALE-FOUND-FLAG = "Y"
               MOVE T-SALE-DT(SALE-IDX) TO SR-SALE-DT
               MOVE T-SALE-AMT(SALE-IDX) TO SR-SALE-AMT
               MOVE T-CURRENCY-CD(SALE-IDX) TO SR-CURRENCY-CD
               MOVE T-MERCHANT-CODE(SALE-IDX) TO SR-MERCHANT-CODE
           ELSE
               MOVE "ウリアゲナシ" TO SR-NOTE
               MOVE "??????????" TO SR-MERCHANT-CODE
           END-IF
           IF WK-CARD-OK = "Y"
               MOVE CF-CARD-STATUS TO SR-CARD-STATUS
               MOVE CF-MEMBER-ID TO SR-MEMBER-ID
               MOVE CF-MEMBER-NAME-KANA TO SR-MEMBER-NAME-KANA
           ELSE
               MOVE "??" TO SR-CARD-STATUS
           END-IF
           IF WK-MERC-OK = "Y"
               MOVE MC-MERCHANT-NAME-KANA TO SR-MERCHANT-NAME-KANA
               MOVE MC-MERCHANT-STATUS TO SR-MERCHANT-STATUS
               MOVE MC-FEE-PLAN-CD TO SR-FEE-PLAN-CD
           END-IF
           MOVE WK-FOREIGN-FEE TO SR-FOREIGN-FEE
           MOVE WK-DEDUCT-AMT TO SR-DEDUCT-AMT
           EVALUATE EX-REASON-CD
               WHEN "01"
                   MOVE "カードジョウタイ" TO SR-REASON-TEXT
               WHEN "02"
                   MOVE "カメイテンジョウタイ"
                       TO SR-REASON-TEXT
               WHEN "03"
                   MOVE "カイガイテスウリョウ"
                       TO SR-REASON-TEXT
               WHEN "04"
                   MOVE "コウジョ" TO SR-REASON-TEXT
               WHEN "05"
                   MOVE "ウリアゲゾクセイ" TO SR-REASON-TEXT
               WHEN OTHER
                   MOVE "リユウフメイ" TO SR-REASON-TEXT
           END-EVALUATE
           IF WK-HARD-ERROR NOT = "Y"
               RELEASE SORT-REC
           END-IF.
      *
       5000-PRINT-SORT.
           DISPLAY "RSN PGM      MERCHANT   ST EXCEPTION"
           DISPLAY "CUR SALE-AMT       FGN-FEE       DEDUCT REASON"
           PERFORM UNTIL EOF-SORT = "Y"
               RETURN SORTWK
                   AT END
                       MOVE "Y" TO EOF-SORT
                   NOT AT END
                       ADD 1 TO CNT-SORT-OUT
                       PERFORM 5100-EDIT-REPORT
                       DISPLAY REPORT-LINE
               END-RETURN
           END-PERFORM.
      *
       5100-EDIT-REPORT.
           MOVE SR-REASON-CD TO RL-REASON-CD
           MOVE SR-DETECTED-PGM TO RL-DETECTED-PGM
           MOVE SR-MERCHANT-CODE TO RL-MERCHANT-CODE
           MOVE SR-CARD-STATUS TO RL-CARD-STATUS
           MOVE SR-EXCEPTION-ID TO RL-EXCEPTION-ID
           MOVE SR-SALE-ID TO RL-SALE-ID
           MOVE SR-CURRENCY-CD TO RL-CURRENCY-CD
           MOVE SR-SALE-AMT TO RL-SALE-AMT
           MOVE SR-FOREIGN-FEE TO RL-FOREIGN-FEE
           MOVE SR-DEDUCT-AMT TO RL-DEDUCT-AMT
           MOVE SR-REASON-TEXT TO RL-REASON-TEXT.
      *
       9000-CLOSE-FILES.
           IF FS-CDEXCPF NOT = SPACES
               CLOSE CDEXCPF
               IF FS-CDEXCPF NOT = "00"
                   DISPLAY "CDEXCPF CLOSE NG ST=" FS-CDEXCPF
                   MOVE 8 TO RETURN-CODE
                   MOVE "Y" TO WK-HARD-ERROR
               END-IF
           END-IF
           IF FS-CDSALEF NOT = SPACES
               CLOSE CDSALEF
               IF FS-CDSALEF NOT = "00"
                   DISPLAY "CDSALEF CLOSE NG ST=" FS-CDSALEF
                   MOVE 8 TO RETURN-CODE
                   MOVE "Y" TO WK-HARD-ERROR
               END-IF
           END-IF
           IF FS-CDCARDF NOT = SPACES
               CLOSE CDCARDF
               IF FS-CDCARDF NOT = "00"
                   DISPLAY "CDCARDF CLOSE NG ST=" FS-CDCARDF
                   MOVE 8 TO RETURN-CODE
                   MOVE "Y" TO WK-HARD-ERROR
               END-IF
           END-IF
           IF FS-CDMERCF NOT = SPACES
               CLOSE CDMERCF
               IF FS-CDMERCF NOT = "00"
                   DISPLAY "CDMERCF CLOSE NG ST=" FS-CDMERCF
                   MOVE 8 TO RETURN-CODE
                   MOVE "Y" TO WK-HARD-ERROR
               END-IF
           END-IF.
