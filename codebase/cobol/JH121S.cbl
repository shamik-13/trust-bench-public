       IDENTIFICATION DIVISION.
       PROGRAM-ID. JH121S.
      * 変更履歴
      * 版数  年月日(和暦)  担当                    概要
      * 1.00  平成28.04.01  システム部 情報系チーム  新規作成
      * 1.01  令和02.10.15  システム部 情報系チーム  制御レコード更新条件見直し
      * 1.02  令和06.03.22  システム部 情報系チーム  情報系連携項目追加対応
      *================================================================*
      * CONTROL RECORD UPDATE SUBPROGRAM                               *
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT JHCTLKF ASSIGN TO "JHCTLKF"
              ORGANIZATION IS INDEXED
              ACCESS MODE  IS DYNAMIC
              RECORD KEY   IS CT-JOB-ID
              FILE STATUS  IS WK-JHCTLKF-ST.

       DATA DIVISION.
       FILE SECTION.
       FD  JHCTLKF.
           COPY JHCTLC.

       WORKING-STORAGE SECTION.
       01  WK-JHCTLKF-ST              PIC X(02) VALUE SPACE.
       01  WK-UPDATE-SW               PIC X(01) VALUE SPACE.
           88  WK-UPDATE-YES                    VALUE "1".
           88  WK-UPDATE-NO                     VALUE "0".
       01  WK-FOUND-SW                PIC X(01) VALUE SPACE.
           88  WK-FOUND-YES                     VALUE "1".
           88  WK-FOUND-NO                      VALUE "0".
       01  WK-OPEN-SW                 PIC X(01) VALUE SPACE.
           88  WK-OPEN-YES                      VALUE "1".
           88  WK-OPEN-NO                       VALUE "0".
       01  WK-IO-ERROR-SW             PIC X(01) VALUE SPACE.
           88  WK-IO-ERROR-YES                  VALUE "1".
           88  WK-IO-ERROR-NO                   VALUE "0".
       01  WK-NOW-DATE                PIC 9(08) VALUE ZERO.
       01  WK-NOW-TIME                PIC 9(08) VALUE ZERO.
       01  WK-NOW-TS                  PIC 9(14) VALUE ZERO.
       01  WK-RC-REJECT               PIC 9(02) VALUE 04.

       LINKAGE SECTION.
       01  LK-JH121S-PARM.
           05  LK-JOB-ID              PIC X(20).
           05  LK-REQUEST-CD          PIC X(01).
               88  LK-REQ-START                 VALUE "S".
               88  LK-REQ-NORMAL-END            VALUE "N".
               88  LK-REQ-ABEND                 VALUE "A".
               88  LK-REQ-RESTART               VALUE "R".
           05  LK-BUSINESS-DT         PIC 9(08).
           05  LK-RUN-SEQ             PIC 9(05).
           05  LK-RESTART-POS         PIC X(40).
           05  LK-INPUT-CNT           PIC 9(11).
           05  LK-OUTPUT-CNT          PIC 9(11).
           05  LK-BUSINESS-RC         PIC 9(02).
           05  LK-EXIST-STATUS-CD     PIC X(01).
           05  LK-REASON-CD           PIC X(04).

       PROCEDURE DIVISION USING LK-JH121S-PARM.
       MAIN-RTN.
           MOVE 0 TO RETURN-CODE
           PERFORM 1000-INIT
           IF WK-IO-ERROR-YES
              PERFORM 9000-END
              GOBACK
           END-IF

           PERFORM 2000-READ-CTL
           IF WK-IO-ERROR-NO
              PERFORM 3000-DECIDE
           END-IF
           IF WK-IO-ERROR-NO AND WK-UPDATE-YES
              PERFORM 4000-WRITE-CTL
           END-IF

           PERFORM 9000-END
           GOBACK.

       1000-INIT.
           SET WK-OPEN-NO      TO TRUE
           SET WK-FOUND-NO     TO TRUE
           SET WK-UPDATE-NO    TO TRUE
           SET WK-IO-ERROR-NO  TO TRUE
           MOVE 00             TO LK-BUSINESS-RC
           MOVE SPACE          TO LK-EXIST-STATUS-CD
           MOVE SPACE          TO LK-REASON-CD

           IF LK-JOB-ID = SPACE
              MOVE 04          TO LK-BUSINESS-RC
              MOVE "E001"      TO LK-REASON-CD
              SET WK-IO-ERROR-YES TO TRUE
              DISPLAY "JH121S JOB-ID NOT SET"
              EXIT PARAGRAPH
           END-IF

           IF NOT LK-REQ-START
              AND NOT LK-REQ-NORMAL-END
              AND NOT LK-REQ-ABEND
              AND NOT LK-REQ-RESTART
              MOVE 04          TO LK-BUSINESS-RC
              MOVE "E002"      TO LK-REASON-CD
              SET WK-IO-ERROR-YES TO TRUE
              DISPLAY "JH121S INVALID REQUEST"
              DISPLAY LK-REQUEST-CD
              EXIT PARAGRAPH
           END-IF

           OPEN I-O JHCTLKF
           IF WK-JHCTLKF-ST = "00"
              SET WK-OPEN-YES TO TRUE
           ELSE
              MOVE 08          TO LK-BUSINESS-RC
              MOVE "F001"      TO LK-REASON-CD
              MOVE 8           TO RETURN-CODE
              SET WK-IO-ERROR-YES TO TRUE
              DISPLAY "JHCTLKF OPEN ERROR"
              DISPLAY WK-JHCTLKF-ST
           END-IF.

       2000-READ-CTL.
           MOVE LK-JOB-ID TO CT-JOB-ID
           READ JHCTLKF WITH LOCK
              INVALID KEY
                 IF WK-JHCTLKF-ST = "23"
                    SET WK-FOUND-NO TO TRUE
                    MOVE SPACE TO LK-EXIST-STATUS-CD
                 ELSE
                    MOVE 08     TO LK-BUSINESS-RC
                    MOVE "F002" TO LK-REASON-CD
                    MOVE 8      TO RETURN-CODE
                    SET WK-IO-ERROR-YES TO TRUE
                    DISPLAY "JHCTLKF READ ERROR"
                    DISPLAY WK-JHCTLKF-ST
                 END-IF
              NOT INVALID KEY
                 SET WK-FOUND-YES TO TRUE
                 MOVE CT-STATUS-CD TO LK-EXIST-STATUS-CD
           END-READ.

       3000-DECIDE.
           SET WK-UPDATE-NO TO TRUE
           EVALUATE TRUE
              WHEN LK-REQ-START
                 PERFORM 3100-DECIDE-START
              WHEN LK-REQ-NORMAL-END
                 PERFORM 3200-DECIDE-END
              WHEN LK-REQ-ABEND
                 PERFORM 3300-DECIDE-ABEND
              WHEN LK-REQ-RESTART
                 PERFORM 3400-DECIDE-RESTART
           END-EVALUATE.

       3100-DECIDE-START.
           IF WK-FOUND-NO
              PERFORM 5100-BUILD-NEW-START
              SET WK-UPDATE-YES TO TRUE
              EXIT PARAGRAPH
           END-IF

           IF CT-STATUS-CD = "0"
              OR CT-STATUS-CD = "2"
              OR CT-STATUS-CD = "3"
              PERFORM 5200-BUILD-EXIST-START
              SET WK-UPDATE-YES TO TRUE
           ELSE
              MOVE 04          TO LK-BUSINESS-RC
              MOVE "B101"      TO LK-REASON-CD
              DISPLAY "JH121S START REJECT"
              DISPLAY CT-STATUS-CD
              DISPLAY LK-JOB-ID
           END-IF.

       3200-DECIDE-END.
           IF WK-FOUND-NO
              MOVE 04          TO LK-BUSINESS-RC
              MOVE "B201"      TO LK-REASON-CD
              DISPLAY "JH121S NORMAL END REJECT NOT FOUND"
              DISPLAY LK-JOB-ID
              EXIT PARAGRAPH
           END-IF

           IF CT-STATUS-CD = "1"
              PERFORM 5300-BUILD-NORMAL-END
              SET WK-UPDATE-YES TO TRUE
           ELSE
              MOVE 04          TO LK-BUSINESS-RC
              MOVE "B202"      TO LK-REASON-CD
              DISPLAY "JH121S NORMAL END REJECT STATUS"
              DISPLAY CT-STATUS-CD
              DISPLAY LK-JOB-ID
           END-IF.

       3300-DECIDE-ABEND.
           IF WK-FOUND-NO
              MOVE 04          TO LK-BUSINESS-RC
              MOVE "B301"      TO LK-REASON-CD
              DISPLAY "JH121S ABEND REJECT NOT FOUND"
              DISPLAY LK-JOB-ID
              EXIT PARAGRAPH
           END-IF

           IF CT-STATUS-CD = "1"
              PERFORM 5400-BUILD-ABEND
              SET WK-UPDATE-YES TO TRUE
           ELSE
              MOVE 04          TO LK-BUSINESS-RC
              MOVE "B302"      TO LK-REASON-CD
              DISPLAY "JH121S ABEND REJECT STATUS"
              DISPLAY CT-STATUS-CD
              DISPLAY LK-JOB-ID
           END-IF.

       3400-DECIDE-RESTART.
           IF WK-FOUND-NO
              MOVE 04          TO LK-BUSINESS-RC
              MOVE "B401"      TO LK-REASON-CD
              DISPLAY "JH121S RESTART REJECT NOT FOUND"
              DISPLAY LK-JOB-ID
              EXIT PARAGRAPH
           END-IF

           IF CT-STATUS-CD = "1"
              PERFORM 5500-BUILD-RESTART
              SET WK-UPDATE-YES TO TRUE
           ELSE
              MOVE 04          TO LK-BUSINESS-RC
              MOVE "B402"      TO LK-REASON-CD
              DISPLAY "JH121S RESTART REJECT STATUS"
              DISPLAY CT-STATUS-CD
              DISPLAY LK-JOB-ID
           END-IF.

       4000-WRITE-CTL.
           IF WK-FOUND-NO AND LK-REQ-START
              WRITE JHCTLKF-REC
                 INVALID KEY
                    MOVE 08     TO LK-BUSINESS-RC
                    MOVE "F003" TO LK-REASON-CD
                    MOVE 8      TO RETURN-CODE
                    SET WK-IO-ERROR-YES TO TRUE
                    DISPLAY "JHCTLKF WRITE ERROR"
                    DISPLAY WK-JHCTLKF-ST
                 NOT INVALID KEY
                    MOVE 00     TO LK-BUSINESS-RC
                    MOVE "0000" TO LK-REASON-CD
              END-WRITE
           ELSE
              REWRITE JHCTLKF-REC
                 INVALID KEY
                    MOVE 08     TO LK-BUSINESS-RC
                    MOVE "F004" TO LK-REASON-CD
                    MOVE 8      TO RETURN-CODE
                    SET WK-IO-ERROR-YES TO TRUE
                    DISPLAY "JHCTLKF REWRITE ERROR"
                    DISPLAY WK-JHCTLKF-ST
                 NOT INVALID KEY
                    MOVE 00     TO LK-BUSINESS-RC
                    MOVE "0000" TO LK-REASON-CD
              END-REWRITE
           END-IF.

       5100-BUILD-NEW-START.
           PERFORM 6000-GET-TIMESTAMP

           INITIALIZE JHCTLKF-REC
           MOVE LK-JOB-ID      TO CT-JOB-ID
           MOVE LK-BUSINESS-DT TO CT-BUSINESS-DT
           MOVE LK-RUN-SEQ     TO CT-RUN-SEQ
           MOVE "1"            TO CT-STATUS-CD
           MOVE WK-NOW-TS      TO CT-START-TS
           MOVE ZERO           TO CT-END-TS
           MOVE LK-RESTART-POS TO CT-RESTART-POS
           MOVE ZERO           TO CT-INPUT-CNT
           MOVE ZERO           TO CT-OUTPUT-CNT.

       5200-BUILD-EXIST-START.
           PERFORM 6000-GET-TIMESTAMP

           MOVE LK-BUSINESS-DT TO CT-BUSINESS-DT
           MOVE LK-RUN-SEQ     TO CT-RUN-SEQ
           MOVE "1"            TO CT-STATUS-CD
           MOVE WK-NOW-TS      TO CT-START-TS
           MOVE ZERO           TO CT-END-TS
           MOVE LK-RESTART-POS TO CT-RESTART-POS
           MOVE ZERO           TO CT-INPUT-CNT
           MOVE ZERO           TO CT-OUTPUT-CNT.

       5300-BUILD-NORMAL-END.
           PERFORM 6000-GET-TIMESTAMP

           MOVE "2"            TO CT-STATUS-CD
           MOVE WK-NOW-TS      TO CT-END-TS
           MOVE LK-RESTART-POS TO CT-RESTART-POS
           MOVE LK-INPUT-CNT   TO CT-INPUT-CNT
           MOVE LK-OUTPUT-CNT  TO CT-OUTPUT-CNT.

       5400-BUILD-ABEND.
           PERFORM 6000-GET-TIMESTAMP

           MOVE "3"            TO CT-STATUS-CD
           MOVE WK-NOW-TS      TO CT-END-TS
           MOVE LK-RESTART-POS TO CT-RESTART-POS
           MOVE LK-INPUT-CNT   TO CT-INPUT-CNT
           MOVE LK-OUTPUT-CNT  TO CT-OUTPUT-CNT.

       5500-BUILD-RESTART.
           MOVE LK-RESTART-POS TO CT-RESTART-POS
           MOVE LK-INPUT-CNT   TO CT-INPUT-CNT
           MOVE LK-OUTPUT-CNT  TO CT-OUTPUT-CNT.

       6000-GET-TIMESTAMP.
           ACCEPT WK-NOW-DATE FROM DATE YYYYMMDD
           ACCEPT WK-NOW-TIME FROM TIME
           COMPUTE WK-NOW-TS = (WK-NOW-DATE * 1000000)
                             + (WK-NOW-TIME / 100).

       9000-END.
           IF WK-OPEN-YES
              CLOSE JHCTLKF
              IF WK-JHCTLKF-ST NOT = "00"
                 MOVE 08       TO LK-BUSINESS-RC
                 MOVE "F005"   TO LK-REASON-CD
                 MOVE 8        TO RETURN-CODE
                 DISPLAY "JHCTLKF CLOSE ERROR"
                 DISPLAY WK-JHCTLKF-ST
              END-IF
           END-IF

           IF LK-BUSINESS-RC = WK-RC-REJECT
              MOVE 0 TO RETURN-CODE
           END-IF.
