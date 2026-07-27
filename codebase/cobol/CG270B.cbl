       IDENTIFICATION DIVISION.
       PROGRAM-ID. CG270B.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CGSUMF ASSIGN TO "CGSUMF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-CGSUMF-ST.
           SELECT CGCODF ASSIGN TO "CGCODF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS GC-CODE-ID
               FILE STATUS IS WS-CGCODF-ST.
           SELECT CKRPTF ASSIGN TO "CKRPTF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-CKRPTF-ST.
           SELECT SORTWK ASSIGN TO SORTWK.

       DATA DIVISION.
       FILE SECTION.
       FD  CGSUMF.
       COPY CGSUMC.

       FD  CGCODF.
       COPY CGCODC.

       FD  CKRPTF.
       COPY CKRPTC.

       SD  SORTWK.
       01  SORTWK-REC.
           05 SW-SEGMENT-KBN        PIC X(02).
           05 SW-STATUS-KBN         PIC X(01).
           05 SW-ABEND-KBN          PIC X(01).
           05 SW-SUMMARY-YYYYMM     PIC 9(06).
           05 SW-COUNT              PIC S9(09) COMP-3.
           05 SW-CUSTOMER-CNT       PIC S9(09) COMP-3.
           05 SW-LABEL              PIC X(16).
           05 SW-REASON             PIC X(40).

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05 WS-CGSUMF-ST          PIC X(02) VALUE SPACES.
           05 WS-CGCODF-ST          PIC X(02) VALUE SPACES.
           05 WS-CKRPTF-ST          PIC X(02) VALUE SPACES.

       01  WS-SWITCH.
           05 WS-CGSUMF-EOF         PIC X(01) VALUE "N".
              88 CGSUMF-END                   VALUE "Y".
           05 WS-HARD-ERROR         PIC X(01) VALUE "N".
              88 HARD-ERROR                  VALUE "Y".

       01  WS-WORK.
           05 WS-TOTAL-CNT          PIC S9(09) COMP-3 VALUE 0.
           05 WS-LINE-NO            PIC 9(06) VALUE 0.
           05 WS-READ-CNT           PIC 9(09) VALUE 0.
           05 WS-WRITE-CNT          PIC 9(09) VALUE 0.
           05 WS-ERR-CNT            PIC 9(09) VALUE 0.
           05 WS-SEG-NAME           PIC X(30) VALUE SPACES.
           05 WS-EDIT-CNT           PIC ZZZ,ZZZ,ZZ9.
           05 WS-EDIT-CUST          PIC ZZZ,ZZZ,ZZ9.
           05 WS-EDIT-LINE          PIC ZZZ,ZZ9.
           05 WS-REPORT-ID          PIC X(08) VALUE "CG270B".
           05 WS-DETAIL-TEXT        PIC X(120) VALUE SPACES.
           05 WS-KEY-LABEL          PIC X(16) VALUE SPACES.

       PROCEDURE DIVISION.
       MAIN-RTN.
           PERFORM INIT-RTN
           IF NOT HARD-ERROR
               SORT SORTWK
                   ON ASCENDING KEY SW-ABEND-KBN
                                    SW-SEGMENT-KBN
                                    SW-STATUS-KBN
                                    SW-SUMMARY-YYYYMM
                   INPUT PROCEDURE IS SORT-IN-RTN
                   OUTPUT PROCEDURE IS SORT-OUT-RTN
           END-IF
           PERFORM END-RTN
           GOBACK.

       INIT-RTN.
           MOVE 0 TO RETURN-CODE
           OPEN INPUT CGSUMF
           IF WS-CGSUMF-ST NOT = "00"
               DISPLAY "CGSUMF オープン失敗 ST=" WS-CGSUMF-ST
               SET HARD-ERROR TO TRUE
               MOVE 8 TO RETURN-CODE
           END-IF
           OPEN INPUT CGCODF
           IF WS-CGCODF-ST NOT = "00"
               DISPLAY "CGCODF オープン失敗 ST=" WS-CGCODF-ST
               SET HARD-ERROR TO TRUE
               MOVE 8 TO RETURN-CODE
           END-IF
           OPEN OUTPUT CKRPTF
           IF WS-CKRPTF-ST NOT = "00"
               DISPLAY "CKRPTF オープン失敗 ST=" WS-CKRPTF-ST
               SET HARD-ERROR TO TRUE
               MOVE 8 TO RETURN-CODE
           END-IF.

       SORT-IN-RTN.
           PERFORM UNTIL CGSUMF-END OR HARD-ERROR
               READ CGSUMF
                   AT END
                       SET CGSUMF-END TO TRUE
                   NOT AT END
                       ADD 1 TO WS-READ-CNT
                       PERFORM SUMMARY-CHECK-RTN
                       PERFORM RELEASE-ACTIVE-RTN
                       PERFORM RELEASE-STOP-RTN
                       PERFORM RELEASE-NEW-RTN
               END-READ
               IF WS-CGSUMF-ST NOT = "00"
                  AND WS-CGSUMF-ST NOT = "10"
                   DISPLAY "CGSUMF 読込失敗 ST=" WS-CGSUMF-ST
                   SET HARD-ERROR TO TRUE
                   MOVE 8 TO RETURN-CODE
               END-IF
           END-PERFORM.

       SUMMARY-CHECK-RTN.
           COMPUTE WS-TOTAL-CNT = GS-ACTIVE-CNT + GS-STOP-CNT
           MOVE SPACES TO WS-KEY-LABEL
           STRING GS-SUMMARY-YYYYMM DELIMITED BY SIZE
                  "-"              DELIMITED BY SIZE
                  GS-SEGMENT-KBN   DELIMITED BY SIZE
                  INTO WS-KEY-LABEL
           END-STRING
           MOVE SPACE TO SW-ABEND-KBN
           MOVE SPACES TO SW-REASON
           IF GS-CUSTOMER-CNT < 0
               MOVE "1" TO SW-ABEND-KBN
               MOVE "顧客件数負数" TO SW-REASON
               ADD 1 TO WS-ERR-CNT
           ELSE
               IF GS-ACTIVE-CNT < 0
                  OR GS-STOP-CNT < 0
                  OR GS-NEW-CNT < 0
                   MOVE "1" TO SW-ABEND-KBN
                   MOVE "状態別件数負数" TO SW-REASON
                   ADD 1 TO WS-ERR-CNT
               ELSE
                   IF GS-CUSTOMER-CNT NOT = WS-TOTAL-CNT
                       MOVE "1" TO SW-ABEND-KBN
                       MOVE "顧客件数不一致" TO SW-REASON
                       ADD 1 TO WS-ERR-CNT
                   ELSE
                       MOVE "0" TO SW-ABEND-KBN
                       MOVE "正常" TO SW-REASON
                   END-IF
               END-IF
           END-IF.

       RELEASE-ACTIVE-RTN.
           MOVE GS-SEGMENT-KBN    TO SW-SEGMENT-KBN
           MOVE "1"               TO SW-STATUS-KBN
           MOVE GS-SUMMARY-YYYYMM TO SW-SUMMARY-YYYYMM
           MOVE GS-ACTIVE-CNT     TO SW-COUNT
           MOVE GS-CUSTOMER-CNT   TO SW-CUSTOMER-CNT
           MOVE WS-KEY-LABEL      TO SW-LABEL
           RELEASE SORTWK-REC.

       RELEASE-STOP-RTN.
           MOVE GS-SEGMENT-KBN    TO SW-SEGMENT-KBN
           MOVE "2"               TO SW-STATUS-KBN
           MOVE GS-SUMMARY-YYYYMM TO SW-SUMMARY-YYYYMM
           MOVE GS-STOP-CNT       TO SW-COUNT
           MOVE GS-CUSTOMER-CNT   TO SW-CUSTOMER-CNT
           MOVE WS-KEY-LABEL      TO SW-LABEL
           RELEASE SORTWK-REC.

       RELEASE-NEW-RTN.
           MOVE GS-SEGMENT-KBN    TO SW-SEGMENT-KBN
           MOVE "3"               TO SW-STATUS-KBN
           MOVE GS-SUMMARY-YYYYMM TO SW-SUMMARY-YYYYMM
           MOVE GS-NEW-CNT        TO SW-COUNT
           MOVE GS-CUSTOMER-CNT   TO SW-CUSTOMER-CNT
           MOVE WS-KEY-LABEL      TO SW-LABEL
           RELEASE SORTWK-REC.

       SORT-OUT-RTN.
           PERFORM UNTIL HARD-ERROR
               RETURN SORTWK
                   AT END
                       EXIT PERFORM
                   NOT AT END
                       PERFORM WRITE-REPORT-RTN
               END-RETURN
           END-PERFORM.

       WRITE-REPORT-RTN.
           PERFORM CODE-LOOKUP-RTN
           ADD 1 TO WS-LINE-NO
           MOVE WS-REPORT-ID      TO RP-REPORT-ID
           MOVE SW-SUMMARY-YYYYMM TO RP-REPORT-YYYYMM
           MOVE WS-LINE-NO        TO RP-LINE-NO
           IF SW-ABEND-KBN = "1"
               MOVE "E" TO RP-SECTION-KBN
           ELSE
               MOVE SW-STATUS-KBN TO RP-SECTION-KBN
           END-IF
           MOVE SW-COUNT          TO WS-EDIT-CNT
           MOVE SW-CUSTOMER-CNT   TO WS-EDIT-CUST
           MOVE WS-LINE-NO        TO WS-EDIT-LINE
           MOVE SPACES TO WS-DETAIL-TEXT
           STRING "行="             DELIMITED BY SIZE
                  WS-EDIT-LINE      DELIMITED BY SIZE
                  " 月="            DELIMITED BY SIZE
                  SW-SUMMARY-YYYYMM DELIMITED BY SIZE
                  " 顧客区分="      DELIMITED BY SIZE
                  SW-SEGMENT-KBN    DELIMITED BY SIZE
                  " "               DELIMITED BY SIZE
                  WS-SEG-NAME       DELIMITED BY SIZE
                  " 状態="          DELIMITED BY SIZE
                  SW-STATUS-KBN     DELIMITED BY SIZE
                  " 件数="          DELIMITED BY SIZE
                  WS-EDIT-CNT       DELIMITED BY SIZE
                  " 顧客計="        DELIMITED BY SIZE
                  WS-EDIT-CUST      DELIMITED BY SIZE
                  " 統合キー="      DELIMITED BY SIZE
                  SW-LABEL          DELIMITED BY SIZE
                  " 判定="          DELIMITED BY SIZE
                  SW-REASON         DELIMITED BY SIZE
                  INTO WS-DETAIL-TEXT
           END-STRING
           MOVE WS-DETAIL-TEXT TO RP-REPORT-TEXT
           WRITE CKRPTF-REC
           IF WS-CKRPTF-ST NOT = "00"
               DISPLAY "CKRPTF 書込失敗 ST=" WS-CKRPTF-ST
               SET HARD-ERROR TO TRUE
               MOVE 8 TO RETURN-CODE
           ELSE
               ADD 1 TO WS-WRITE-CNT
           END-IF.

       CODE-LOOKUP-RTN.
           MOVE SPACES TO GC-CODE-ID
           MOVE SPACES TO WS-SEG-NAME
           STRING "SEG" DELIMITED BY SIZE
                  SW-SEGMENT-KBN DELIMITED BY SIZE
                  INTO GC-CODE-ID
           END-STRING
           READ CGCODF KEY IS GC-CODE-ID
               INVALID KEY
                   MOVE "顧客区分名称未登録" TO WS-SEG-NAME
               NOT INVALID KEY
                   IF GC-CODE-KBN = "SEG"
                       MOVE GC-CODE-NAME TO WS-SEG-NAME
                   ELSE
                       MOVE "顧客区分種別不正" TO WS-SEG-NAME
                   END-IF
           END-READ
           IF WS-CGCODF-ST NOT = "00"
              AND WS-CGCODF-ST NOT = "23"
               DISPLAY "CGCODF 参照失敗 ST=" WS-CGCODF-ST
               SET HARD-ERROR TO TRUE
               MOVE 8 TO RETURN-CODE
           END-IF.

       END-RTN.
           IF WS-CGSUMF-ST NOT = SPACES
               CLOSE CGSUMF
           END-IF
           IF WS-CGCODF-ST NOT = SPACES
               CLOSE CGCODF
           END-IF
           IF WS-CKRPTF-ST NOT = SPACES
               CLOSE CKRPTF
           END-IF
           IF HARD-ERROR
               MOVE 8 TO RETURN-CODE
               DISPLAY "CG270B 異常終了 入力=" WS-READ-CNT
                       " 出力=" WS-WRITE-CNT
                       " 異常=" WS-ERR-CNT
           ELSE
               MOVE 0 TO RETURN-CODE
               DISPLAY "CG270B 正常終了 入力=" WS-READ-CNT
                       " 出力=" WS-WRITE-CNT
                       " 異常=" WS-ERR-CNT
           END-IF.
