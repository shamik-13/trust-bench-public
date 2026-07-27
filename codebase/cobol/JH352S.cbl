       IDENTIFICATION DIVISION.
       PROGRAM-ID. JH352S.
      * 版数  年月日(和暦)  担当                    概要
      * 1.00  平成28.04.01  システム部 情報系チーム  新規作成
      * 1.01  令和02.10.15  システム部 情報系チーム  残高帯境界値判定見直し
      * 1.02  令和06.03.22  システム部 情報系チーム  残高帯コード返却条件追加

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT JHREFMST ASSIGN TO "JHREFMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS RF-REF-TYPE-CD
               FILE STATUS IS WS-RF-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  JHREFMST.
           COPY JHREFMSTC.

       WORKING-STORAGE SECTION.
       01  WS-RF-STATUS              PIC XX.
       01  WS-END-FLAG               PIC X VALUE "N".
           88  WS-END-YES                  VALUE "Y".
           88  WS-END-NO                   VALUE "N".
       01  WS-HIT-FLAG               PIC X VALUE "N".
           88  WS-HIT-YES                  VALUE "Y".
           88  WS-HIT-NO                   VALUE "N".
       01  WS-HARD-ERROR-FLAG        PIC X VALUE "N".
           88  WS-HARD-ERROR-YES           VALUE "Y".
           88  WS-HARD-ERROR-NO            VALUE "N".
       01  WS-CHECK-FLAG             PIC X VALUE "N".
           88  WS-CHECK-OK                 VALUE "Y".
           88  WS-CHECK-NG                 VALUE "N".
       01  WS-LOW-OK-FLAG            PIC X VALUE "N".
           88  WS-LOW-OK                   VALUE "Y".
           88  WS-LOW-NG                   VALUE "N".
       01  WS-HIGH-OK-FLAG           PIC X VALUE "N".
           88  WS-HIGH-OK                  VALUE "Y".
           88  WS-HIGH-NG                  VALUE "N".

       01  WS-TODAY                  PIC 9(8).
       01  WS-WORK-DATE              PIC 9(8).
       01  WS-LOW-AMT-TEXT           PIC X(17).
       01  WS-HIGH-AMT-TEXT          PIC X(17).
       01  WS-LOW-AMT                PIC S9(15)V99 COMP-3.
       01  WS-HIGH-AMT               PIC S9(15)V99 COMP-3.

       01  WS-REF-NAME-WORK.
           05  WS-LOW-ATTR           PIC X.
           05  WS-LOW-TEXT           PIC X(17).
           05  WS-HIGH-ATTR          PIC X.
           05  WS-HIGH-TEXT          PIC X(17).
           05  WS-NAME-REST          PIC X(45).

       LINKAGE SECTION.
       01  LK-JH352S-PARM.
           05  LK-REF-TYPE-CD        PIC X(10).
           05  LK-BASE-DATE          PIC 9(8).
           05  LK-AVG-BALANCE        PIC S9(15)V99 COMP-3.
           05  LK-BAND-CD            PIC X(10).
           05  LK-WARN-FLAG          PIC X.
           05  LK-RETURN-STATUS      PIC X(02).
           05  LK-MESSAGE            PIC X(80).

       PROCEDURE DIVISION USING LK-JH352S-PARM.

       0000-MAIN.
           MOVE 0 TO RETURN-CODE
           PERFORM 1000-INIT
           IF WS-HARD-ERROR-NO
               PERFORM 2000-SEARCH
           END-IF
           PERFORM 9000-FINISH
           GOBACK
           .

       1000-INIT.
           SET WS-END-NO TO TRUE
           SET WS-HIT-NO TO TRUE
           SET WS-HARD-ERROR-NO TO TRUE
           MOVE SPACE TO LK-BAND-CD
           MOVE "N" TO LK-WARN-FLAG
           MOVE "00" TO LK-RETURN-STATUS
           MOVE SPACE TO LK-MESSAGE
           ACCEPT WS-TODAY FROM DATE YYYYMMDD

           IF LK-BASE-DATE = ZERO
               MOVE WS-TODAY TO WS-WORK-DATE
           ELSE
               MOVE LK-BASE-DATE TO WS-WORK-DATE
           END-IF

           IF LK-REF-TYPE-CD = SPACE
               MOVE "E1" TO LK-RETURN-STATUS
               MOVE "REF TYPE REQUIRED" TO LK-MESSAGE
               SET WS-HARD-ERROR-YES TO TRUE
               MOVE 8 TO RETURN-CODE
           END-IF

           IF LK-AVG-BALANCE < ZERO
               MOVE "E2" TO LK-RETURN-STATUS
               MOVE "INVALID BALANCE" TO LK-MESSAGE
               SET WS-HARD-ERROR-YES TO TRUE
               MOVE 8 TO RETURN-CODE
           END-IF
           .

       2000-SEARCH.
           OPEN INPUT JHREFMST

           IF WS-RF-STATUS NOT = "00"
               DISPLAY "JHREFMST OPEN ERROR ST=" WS-RF-STATUS
               MOVE "E3" TO LK-RETURN-STATUS
               MOVE "MASTER OPEN ERROR" TO LK-MESSAGE
               SET WS-HARD-ERROR-YES TO TRUE
               MOVE 8 TO RETURN-CODE
           ELSE
               MOVE LK-REF-TYPE-CD TO RF-REF-TYPE-CD

               START JHREFMST KEY IS >= RF-REF-TYPE-CD
                   INVALID KEY
                       SET WS-END-YES TO TRUE
                   NOT INVALID KEY
                       SET WS-END-NO TO TRUE
               END-START

               IF WS-RF-STATUS NOT = "00"
                   AND WS-RF-STATUS NOT = "23"
                   DISPLAY "JHREFMST START ERROR ST=" WS-RF-STATUS
                   MOVE "E4" TO LK-RETURN-STATUS
                   MOVE "MASTER START ERROR" TO LK-MESSAGE
                   SET WS-HARD-ERROR-YES TO TRUE
                   MOVE 8 TO RETURN-CODE
               ELSE
                   PERFORM 2100-READ-LOOP
                       UNTIL WS-END-YES OR WS-HIT-YES
               END-IF

               CLOSE JHREFMST

               IF WS-RF-STATUS NOT = "00"
                   DISPLAY "JHREFMST CLOSE ERROR ST=" WS-RF-STATUS
                   IF WS-HARD-ERROR-NO
                       MOVE "E5" TO LK-RETURN-STATUS
                       MOVE "MASTER CLOSE ERROR" TO LK-MESSAGE
                       SET WS-HARD-ERROR-YES TO TRUE
                       MOVE 8 TO RETURN-CODE
                   END-IF
               END-IF
           END-IF

           IF WS-HARD-ERROR-NO AND WS-HIT-NO
               MOVE "ZZZZZZZZZZ" TO LK-BAND-CD
               MOVE "Y" TO LK-WARN-FLAG
               MOVE "04" TO LK-RETURN-STATUS
               MOVE "BAND NOT FOUND" TO LK-MESSAGE
               DISPLAY "BAND NOT FOUND REF=" LK-REF-TYPE-CD
           END-IF
           .

       2100-READ-LOOP.
           READ JHREFMST NEXT RECORD
               AT END
                   SET WS-END-YES TO TRUE
               NOT AT END
                   IF WS-RF-STATUS NOT = "00"
                       DISPLAY "JHREFMST READ ERROR ST=" WS-RF-STATUS
                       MOVE "E6" TO LK-RETURN-STATUS
                       MOVE "MASTER READ ERROR" TO LK-MESSAGE
                       SET WS-HARD-ERROR-YES TO TRUE
                       MOVE 8 TO RETURN-CODE
                       SET WS-END-YES TO TRUE
                   ELSE
                       IF RF-REF-TYPE-CD NOT = LK-REF-TYPE-CD
                           SET WS-END-YES TO TRUE
                       ELSE
                           PERFORM 2200-CHECK-RECORD
                       END-IF
                   END-IF
           END-READ
           .

       2200-CHECK-RECORD.
           SET WS-CHECK-NG TO TRUE

           IF RF-ACTIVE-FLAG = "1"
               IF RF-VALID-FROM-DATE <= WS-WORK-DATE
                   IF RF-VALID-TO-DATE = ZERO
                       SET WS-CHECK-OK TO TRUE
                   ELSE
                       IF RF-VALID-TO-DATE >= WS-WORK-DATE
                           SET WS-CHECK-OK TO TRUE
                       END-IF
                   END-IF
               END-IF
           END-IF

           IF WS-CHECK-OK
               PERFORM 2300-JUDGE-BAND
           END-IF
           .

       2300-JUDGE-BAND.
           MOVE RF-REF-NAME TO WS-REF-NAME-WORK
           MOVE WS-LOW-TEXT TO WS-LOW-AMT-TEXT
           MOVE WS-HIGH-TEXT TO WS-HIGH-AMT-TEXT

           IF WS-LOW-AMT-TEXT NUMERIC
               AND WS-HIGH-AMT-TEXT NUMERIC
               COMPUTE WS-LOW-AMT =
                   FUNCTION NUMVAL(WS-LOW-AMT-TEXT)
               COMPUTE WS-HIGH-AMT =
                   FUNCTION NUMVAL(WS-HIGH-AMT-TEXT)

               SET WS-LOW-NG TO TRUE
               SET WS-HIGH-NG TO TRUE

               EVALUATE WS-LOW-ATTR
                   WHEN "I"
                       IF LK-AVG-BALANCE >= WS-LOW-AMT
                           SET WS-LOW-OK TO TRUE
                       END-IF
                   WHEN "E"
                       IF LK-AVG-BALANCE > WS-LOW-AMT
                           SET WS-LOW-OK TO TRUE
                       END-IF
                   WHEN "N"
                       SET WS-LOW-OK TO TRUE
                   WHEN OTHER
                       DISPLAY "LOW ATTR ERROR CD=" RF-REF-CD
               END-EVALUATE

               EVALUATE WS-HIGH-ATTR
                   WHEN "I"
                       IF LK-AVG-BALANCE <= WS-HIGH-AMT
                           SET WS-HIGH-OK TO TRUE
                       END-IF
                   WHEN "E"
                       IF LK-AVG-BALANCE < WS-HIGH-AMT
                           SET WS-HIGH-OK TO TRUE
                       END-IF
                   WHEN "N"
                       SET WS-HIGH-OK TO TRUE
                   WHEN OTHER
                       DISPLAY "HIGH ATTR ERROR CD=" RF-REF-CD
               END-EVALUATE

               IF WS-LOW-OK AND WS-HIGH-OK
                   MOVE RF-REF-CD TO LK-BAND-CD
                   MOVE "00" TO LK-RETURN-STATUS
                   MOVE SPACE TO LK-MESSAGE
                   SET WS-HIT-YES TO TRUE
               END-IF
           ELSE
               DISPLAY "AMOUNT FORMAT ERROR CD=" RF-REF-CD
           END-IF
           .

       9000-FINISH.
           IF WS-HARD-ERROR-YES
               MOVE "Y" TO LK-WARN-FLAG
           ELSE
               IF LK-RETURN-STATUS = "00"
                   MOVE 0 TO RETURN-CODE
               END-IF
           END-IF
           .
