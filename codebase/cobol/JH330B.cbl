       IDENTIFICATION DIVISION.
       PROGRAM-ID. JH330B.
      * 変更履歴
      * 版数  年月日(和暦)   担当                         概要
      * 1.00  令和05年04月01日 システム部 情報系チーム   新規作成
      * 1.10  令和05年10月16日 システム部 情報系チーム   顧客属性集計条件追加
      * 1.20  令和06年03月11日 システム部 情報系チーム   出力件数チェック追加

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT JHCATTF ASSIGN TO "JHCATTF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS CA-CUST-ID
               FILE STATUS IS FS-JHCATTF.

           SELECT JHCBALF ASSIGN TO "JHCBALF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-JHCBALF.

           SELECT JHSEGRF ASSIGN TO "JHSEGRF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-JHSEGRF.

           SELECT JHSEGDST ASSIGN TO "JHSEGDST"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-JHSEGDST.

       DATA DIVISION.
       FILE SECTION.
       FD  JHCATTF.
           COPY JHCATTC.

       FD  JHCBALF.
           COPY JHCBALFC.

       FD  JHSEGRF.
           COPY JHSEGRFC.

       FD  JHSEGDST.
           COPY JHSEGDC.

       WORKING-STORAGE SECTION.
       01  FS-JHCATTF                 PIC XX.
       01  FS-JHCBALF                 PIC XX.
       01  FS-JHSEGRF                 PIC XX.
       01  FS-JHSEGDST                PIC XX.

       01  WK-END-FLAGS.
           05  WK-CA-END              PIC X VALUE "N".
           05  WK-CB-END              PIC X VALUE "N".
           05  WK-SR-END              PIC X VALUE "N".

       01  WK-RUN-DATE.
           05  WK-RUN-YYYY            PIC 9(04).
           05  WK-RUN-MM              PIC 9(02).
           05  WK-RUN-DD              PIC 9(02).

       01  WK-WORK.
           05  WK-FOUND-BAL           PIC X VALUE "N".
           05  WK-FOUND-SEG           PIC X VALUE "N".
           05  WK-PROV-FLAG           PIC X VALUE "N".
           05  WK-VALID-FLAG          PIC X VALUE "Y".
           05  WK-SEG-CD              PIC X(02).
           05  WK-SEG-NAME            PIC X(30).
           05  WK-AVG-BAL             PIC S9(13)V99 COMP-3.
           05  WK-MEAN-BAL            PIC S9(13)V99 COMP-3.
           05  WK-TABLE-IDX           PIC 9(04) COMP.
           05  WK-FREE-IDX            PIC 9(04) COMP VALUE 0.
           05  WK-HIT-IDX             PIC 9(04) COMP.
           05  WK-OUT-IDX             PIC 9(04) COMP.

       01  WK-COUNT.
           05  WK-READ-CA             PIC 9(09) COMP-3 VALUE 0.
           05  WK-READ-CB             PIC 9(09) COMP-3 VALUE 0.
           05  WK-READ-SR             PIC 9(09) COMP-3 VALUE 0.
           05  WK-WRITE-SD            PIC 9(09) COMP-3 VALUE 0.
           05  WK-SKIP-COUNT          PIC 9(09) COMP-3 VALUE 0.
           05  WK-PROV-COUNT          PIC 9(09) COMP-3 VALUE 0.

       01  WK-AGG-TABLE.
           05  WK-AGG-ENT OCCURS 3000 TIMES.
               10  WK-AGG-USED        PIC X.
               10  WK-AGG-SEG-CD      PIC X(02).
               10  WK-AGG-AGE-BAND    PIC X(02).
               10  WK-AGG-PREF-CD     PIC X(02).
               10  WK-AGG-CNT         PIC 9(09) COMP-3.
               10  WK-AGG-SUM         PIC S9(15)V99 COMP-3.
               10  WK-AGG-PROV-CNT    PIC 9(09) COMP-3.

       01  WK-DISPLAY-NUM.
           05  WK-DSP-CNT             PIC Z(8)9.
           05  WK-DSP-RC              PIC 9(02).

           COPY LK-SEG-PARM.

       PROCEDURE DIVISION.
       MAIN-RTN.
           MOVE 0 TO RETURN-CODE
           ACCEPT WK-RUN-DATE FROM DATE YYYYMMDD
           INITIALIZE WK-AGG-TABLE
           PERFORM OPEN-FILES
           IF RETURN-CODE NOT = 0
               PERFORM CLOSE-FILES
               GOBACK
           END-IF

           PERFORM READ-CB
           PERFORM READ-SR
           PERFORM READ-CA

           PERFORM UNTIL WK-CA-END = "Y"
               ADD 1 TO WK-READ-CA
               PERFORM PROCESS-CUSTOMER
               PERFORM READ-CA
           END-PERFORM

           IF RETURN-CODE = 0
               PERFORM WRITE-AGGREGATE
           END-IF

           PERFORM CLOSE-FILES

           IF RETURN-CODE = 0
               MOVE WK-WRITE-SD TO WK-DSP-CNT
               DISPLAY "JH330B NORMAL END OUT=" WK-DSP-CNT
               MOVE WK-PROV-COUNT TO WK-DSP-CNT
               DISPLAY "JH330B PROVISIONAL CNT=" WK-DSP-CNT
           ELSE
               MOVE RETURN-CODE TO WK-DSP-RC
               DISPLAY "JH330B ABEND RC=" WK-DSP-RC
           END-IF
           GOBACK.

       OPEN-FILES.
           OPEN INPUT JHCATTF
           IF FS-JHCATTF NOT = "00"
               DISPLAY "JHCATTF OPEN ERROR ST=" FS-JHCATTF
               MOVE 8 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF

           OPEN INPUT JHCBALF
           IF FS-JHCBALF NOT = "00"
               DISPLAY "JHCBALF OPEN ERROR ST=" FS-JHCBALF
               MOVE 8 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF

           OPEN INPUT JHSEGRF
           IF FS-JHSEGRF NOT = "00"
               DISPLAY "JHSEGRF OPEN ERROR ST=" FS-JHSEGRF
               MOVE 8 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF

           OPEN OUTPUT JHSEGDST
           IF FS-JHSEGDST NOT = "00"
               DISPLAY "JHSEGDST OPEN ERROR ST=" FS-JHSEGDST
               MOVE 8 TO RETURN-CODE
           END-IF.

       CLOSE-FILES.
           CLOSE JHCATTF
           CLOSE JHCBALF
           CLOSE JHSEGRF
           CLOSE JHSEGDST.

       READ-CA.
           READ JHCATTF NEXT RECORD
               AT END
                   MOVE "Y" TO WK-CA-END
               NOT AT END
                   CONTINUE
           END-READ
           IF FS-JHCATTF NOT = "00" AND FS-JHCATTF NOT = "10"
               DISPLAY "JHCATTF READ ERROR ST=" FS-JHCATTF
               MOVE 8 TO RETURN-CODE
               MOVE "Y" TO WK-CA-END
           END-IF.

       READ-CB.
           READ JHCBALF
               AT END
                   MOVE "Y" TO WK-CB-END
               NOT AT END
                   ADD 1 TO WK-READ-CB
           END-READ
           IF FS-JHCBALF NOT = "00" AND FS-JHCBALF NOT = "10"
               DISPLAY "JHCBALF READ ERROR ST=" FS-JHCBALF
               MOVE 8 TO RETURN-CODE
               MOVE "Y" TO WK-CB-END
           END-IF.

       READ-SR.
           READ JHSEGRF
               AT END
                   MOVE "Y" TO WK-SR-END
               NOT AT END
                   ADD 1 TO WK-READ-SR
           END-READ
           IF FS-JHSEGRF NOT = "00" AND FS-JHSEGRF NOT = "10"
               DISPLAY "JHSEGRF READ ERROR ST=" FS-JHSEGRF
               MOVE 8 TO RETURN-CODE
               MOVE "Y" TO WK-SR-END
           END-IF.

       PROCESS-CUSTOMER.
           IF RETURN-CODE NOT = 0
               EXIT PARAGRAPH
           END-IF

           PERFORM VALIDATE-ATTRIBUTE
           IF WK-VALID-FLAG NOT = "Y"
               ADD 1 TO WK-SKIP-COUNT
               EXIT PARAGRAPH
           END-IF

           PERFORM MATCH-BALANCE
           IF WK-FOUND-BAL NOT = "Y"
               ADD 1 TO WK-SKIP-COUNT
               EXIT PARAGRAPH
           END-IF

           PERFORM MATCH-SEGMENT
           IF WK-FOUND-SEG NOT = "Y"
               PERFORM MAKE-PROVISIONAL-SEGMENT
               IF RETURN-CODE NOT = 0
                   EXIT PARAGRAPH
               END-IF
           END-IF

           PERFORM ADD-AGGREGATE.

       VALIDATE-ATTRIBUTE.
           MOVE "Y" TO WK-VALID-FLAG
           IF CA-CUST-ID = SPACES
               DISPLAY "CA CUST-ID IS SPACE"
               MOVE "N" TO WK-VALID-FLAG
           END-IF
           IF CA-AGE-BAND-CD = SPACES
               DISPLAY "CA AGE-BAND SPACE ID=" CA-CUST-ID
               MOVE "N" TO WK-VALID-FLAG
           END-IF
           IF CA-PREF-CD < "01" OR CA-PREF-CD > "47"
               DISPLAY "CA PREF ERROR ID=" CA-CUST-ID
               MOVE "N" TO WK-VALID-FLAG
           END-IF.

       MATCH-BALANCE.
           MOVE "N" TO WK-FOUND-BAL
           PERFORM UNTIL WK-CB-END = "Y"
              OR CB-CUST-ID >= CA-CUST-ID
               PERFORM READ-CB
           END-PERFORM

           IF WK-CB-END NOT = "Y" AND CB-CUST-ID = CA-CUST-ID
               MOVE "Y" TO WK-FOUND-BAL
               MOVE CB-AVG-BAL TO WK-AVG-BAL
           ELSE
               DISPLAY "BALANCE NOT FOUND ID=" CA-CUST-ID
           END-IF.

       MATCH-SEGMENT.
           MOVE "N" TO WK-FOUND-SEG
           MOVE "N" TO WK-PROV-FLAG
           PERFORM UNTIL WK-SR-END = "Y"
              OR SR-CUST-ID >= CA-CUST-ID
               PERFORM READ-SR
           END-PERFORM

           IF WK-SR-END NOT = "Y" AND SR-CUST-ID = CA-CUST-ID
               MOVE "Y" TO WK-FOUND-SEG
               MOVE SR-SEG-CD TO WK-SEG-CD
               MOVE SR-SEG-NAME TO WK-SEG-NAME
               PERFORM VALIDATE-SEGMENT
           END-IF.

       VALIDATE-SEGMENT.
           IF WK-SEG-CD NOT = "01"
              AND WK-SEG-CD NOT = "02"
              AND WK-SEG-CD NOT = "03"
              AND WK-SEG-CD NOT = "04"
              AND WK-SEG-CD NOT = "05"
               DISPLAY "SEGMENT CODE ERROR ID=" CA-CUST-ID
               MOVE "N" TO WK-FOUND-SEG
           END-IF.

       MAKE-PROVISIONAL-SEGMENT.
           INITIALIZE LK-SEG-PARM
           MOVE WK-AVG-BAL TO LK-SEG-BAL
           CALL "JH315S" USING LK-SEG-PARM
           IF LK-SEG-RET NOT = ZERO
               DISPLAY "JH315S ERROR ID=" CA-CUST-ID
               MOVE 8 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF
           MOVE LK-SEG-CD TO WK-SEG-CD
           MOVE LK-SEG-NAME TO WK-SEG-NAME
           MOVE "Y" TO WK-PROV-FLAG
           ADD 1 TO WK-PROV-COUNT.

       ADD-AGGREGATE.
           MOVE 0 TO WK-HIT-IDX
           PERFORM VARYING WK-TABLE-IDX FROM 1 BY 1
             UNTIL WK-TABLE-IDX > WK-FREE-IDX
                OR WK-HIT-IDX NOT = 0
               IF WK-AGG-USED(WK-TABLE-IDX) = "Y"
                  AND WK-AGG-SEG-CD(WK-TABLE-IDX) = WK-SEG-CD
                  AND WK-AGG-AGE-BAND(WK-TABLE-IDX)
                    = CA-AGE-BAND-CD
                  AND WK-AGG-PREF-CD(WK-TABLE-IDX) = CA-PREF-CD
                   MOVE WK-TABLE-IDX TO WK-HIT-IDX
               END-IF
           END-PERFORM

           IF WK-HIT-IDX = 0
               IF WK-FREE-IDX >= 3000
                   DISPLAY "AGGREGATE TABLE FULL ID=" CA-CUST-ID
                   MOVE 12 TO RETURN-CODE
                   EXIT PARAGRAPH
               END-IF
               ADD 1 TO WK-FREE-IDX
               MOVE WK-FREE-IDX TO WK-HIT-IDX
               MOVE "Y" TO WK-AGG-USED(WK-HIT-IDX)
               MOVE WK-SEG-CD TO WK-AGG-SEG-CD(WK-HIT-IDX)
               MOVE CA-AGE-BAND-CD TO WK-AGG-AGE-BAND(WK-HIT-IDX)
               MOVE CA-PREF-CD TO WK-AGG-PREF-CD(WK-HIT-IDX)
               MOVE 0 TO WK-AGG-CNT(WK-HIT-IDX)
               MOVE 0 TO WK-AGG-SUM(WK-HIT-IDX)
               MOVE 0 TO WK-AGG-PROV-CNT(WK-HIT-IDX)
           END-IF

           ADD 1 TO WK-AGG-CNT(WK-HIT-IDX)
           ADD WK-AVG-BAL TO WK-AGG-SUM(WK-HIT-IDX)
           IF WK-PROV-FLAG = "Y"
               ADD 1 TO WK-AGG-PROV-CNT(WK-HIT-IDX)
           END-IF.

       WRITE-AGGREGATE.
           PERFORM VARYING WK-OUT-IDX FROM 1 BY 1
             UNTIL WK-OUT-IDX > WK-FREE-IDX
               IF WK-AGG-USED(WK-OUT-IDX) = "Y"
                  AND WK-AGG-CNT(WK-OUT-IDX) > 0
                   COMPUTE WK-MEAN-BAL ROUNDED =
                       WK-AGG-SUM(WK-OUT-IDX)
                       / WK-AGG-CNT(WK-OUT-IDX)
                   INITIALIZE JHSEGDST-REC
                   MOVE WK-RUN-DATE TO SD-REPORT-DATE
                   MOVE WK-AGG-SEG-CD(WK-OUT-IDX) TO SD-SEG-CD
                   MOVE WK-AGG-AGE-BAND(WK-OUT-IDX)
                     TO SD-AGE-BAND-CD
                   MOVE WK-AGG-PREF-CD(WK-OUT-IDX) TO SD-PREF-CD
                   MOVE WK-AGG-CNT(WK-OUT-IDX)
                     TO SD-CUSTOMER-COUNT
                   COMPUTE SD-AVG-BAL-SUM =
                       WK-AGG-SUM(WK-OUT-IDX)
                   COMPUTE SD-AVG-BAL-MEAN =
                       WK-MEAN-BAL
                   WRITE JHSEGDST-REC
                   IF FS-JHSEGDST NOT = "00"
                       DISPLAY "JHSEGDST WRITE ERROR ST=" FS-JHSEGDST
                       MOVE 8 TO RETURN-CODE
                       EXIT PERFORM
                   END-IF
                   ADD 1 TO WK-WRITE-SD
               END-IF
           END-PERFORM.
