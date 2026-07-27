       IDENTIFICATION DIVISION.
       PROGRAM-ID. KZ116S.
      * 版数  年月日(和暦)  担当                       概要
      * 1.00  平成30.04.01  システム部 勘定系チーム  新規作成
      * 1.01  令和02.10.01  システム部 勘定系チーム  税務共通コード判定追加
      * 1.02  令和05.01.04  システム部 勘定系チーム  入力桁数検証見直し
      *
      * TAX COMMON CODE VALIDATION SUBPROGRAM
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZRTMF
               ASSIGN       TO "KZRTMF"
               ORGANIZATION IS INDEXED
               ACCESS MODE  IS DYNAMIC
               RECORD KEY   IS RT-RATE-KEY
               FILE STATUS  IS WS-KZRTMF-ST.

       DATA DIVISION.
       FILE SECTION.
       FD  KZRTMF.
           COPY KZRTMFC.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05 WS-KZRTMF-ST              PIC XX VALUE SPACE.

       01  WS-SWITCHES.
           05 WS-FOUND-SW               PIC X VALUE "N".
              88 FOUND-RATE                   VALUE "Y".
              88 NOT-FOUND-RATE               VALUE "N".
           05 WS-HARD-ERR-SW            PIC X VALUE "N".
              88 HARD-ERROR                   VALUE "Y".
              88 NO-HARD-ERROR               VALUE "N".
           05 WS-EOF-SW                 PIC X VALUE "N".
              88 END-OF-FILE                  VALUE "Y".
              88 NOT-END-OF-FILE              VALUE "N".
           05 WS-OPEN-SW                PIC X VALUE "N".
              88 FILE-OPENED                  VALUE "Y".
              88 FILE-NOT-OPENED              VALUE "N".

       01  WS-DATE-WORK.
           05 WS-IN-DATE-N              PIC 9(08) VALUE ZERO.
           05 WS-IN-DATE-R REDEFINES WS-IN-DATE-N.
              10 WS-IN-YYYY             PIC 9(04).
              10 WS-IN-MM               PIC 9(02).
              10 WS-IN-DD               PIC 9(02).
           05 WS-DAYS-IN-MONTH          PIC 9(02) VALUE ZERO.
           05 WS-LEAP-QUOTIENT          PIC 9(04) VALUE ZERO.
           05 WS-LEAP-REM-4             PIC 9(04) VALUE ZERO.
           05 WS-LEAP-REM-100           PIC 9(04) VALUE ZERO.
           05 WS-LEAP-REM-400           PIC 9(04) VALUE ZERO.

       01  WS-BEST-RATE.
           05 WS-BEST-DATE              PIC 9(08) VALUE ZERO.
           05 WS-BEST-KEY               PIC X(64) VALUE SPACE.
           05 WS-BEST-NUMERATOR         PIC 9(09)V9(09) VALUE ZERO.
           05 WS-BEST-DENOMINATOR       PIC 9(09)V9(09) VALUE ZERO.
           05 WS-BEST-AUTH-REF-NO       PIC X(20) VALUE SPACE.

       01  WS-MESSAGE.
           05 WS-DISP-ST                PIC XX.
           05 WS-DISP-KEY               PIC X(64).

       LINKAGE SECTION.
       01  LK-KZ116S-PARM.
           05 LK-TAX-KIND               PIC X(02).
           05 LK-PREF-CODE              PIC X(02).
           05 LK-OFFICE-CODE            PIC X(05).
           05 LK-APPLY-DATE             PIC X(08).
           05 LK-REASON-CODE            PIC X(04).
           05 LK-REASON-TEXT            PIC X(40).
           05 LK-VALID-RATE-KEY         PIC X(64).
           05 LK-RATE-NUMERATOR         PIC 9(09)V9(09).
           05 LK-RATE-DENOMINATOR       PIC 9(09)V9(09).
           05 LK-AUTH-REF-NO            PIC X(20).

       PROCEDURE DIVISION USING LK-KZ116S-PARM.

       0000-MAIN SECTION.
       0000-START.
           MOVE 0 TO RETURN-CODE
           PERFORM 1000-INITIALIZE
           PERFORM 2000-EDIT-INPUT
           IF LK-REASON-CODE = SPACE
              PERFORM 3000-OPEN-FILE
           END-IF
           IF LK-REASON-CODE = SPACE
              PERFORM 4000-SEARCH-RATE
           END-IF
           IF FILE-OPENED
              PERFORM 8000-CLOSE-FILE
           END-IF
           IF HARD-ERROR
              MOVE 8 TO RETURN-CODE
           ELSE
              IF LK-REASON-CODE = SPACE
                 MOVE "0000" TO LK-REASON-CODE
                 MOVE "NORMAL" TO LK-REASON-TEXT
              END-IF
           END-IF
           GOBACK.

       1000-INITIALIZE SECTION.
       1000-START.
           MOVE SPACE TO LK-REASON-CODE
           MOVE SPACE TO LK-REASON-TEXT
           MOVE SPACE TO LK-VALID-RATE-KEY
           MOVE ZERO  TO LK-RATE-NUMERATOR
           MOVE ZERO  TO LK-RATE-DENOMINATOR
           MOVE SPACE TO LK-AUTH-REF-NO
           SET NOT-FOUND-RATE TO TRUE
           SET NO-HARD-ERROR TO TRUE
           SET NOT-END-OF-FILE TO TRUE
           SET FILE-NOT-OPENED TO TRUE
           MOVE SPACE TO WS-KZRTMF-ST
           MOVE ZERO  TO WS-IN-DATE-N
           MOVE ZERO  TO WS-BEST-DATE
           MOVE SPACE TO WS-BEST-KEY
           MOVE ZERO  TO WS-BEST-NUMERATOR
           MOVE ZERO  TO WS-BEST-DENOMINATOR
           MOVE SPACE TO WS-BEST-AUTH-REF-NO.

       2000-EDIT-INPUT SECTION.
       2000-START.
           IF LK-TAX-KIND NOT NUMERIC
              MOVE "E101" TO LK-REASON-CODE
              MOVE "INVALID TAX KIND" TO LK-REASON-TEXT
           ELSE
              IF LK-TAX-KIND = "00"
                 MOVE "E101" TO LK-REASON-CODE
                 MOVE "INVALID TAX KIND" TO LK-REASON-TEXT
              END-IF
           END-IF

           IF LK-REASON-CODE = SPACE
              IF LK-PREF-CODE NOT NUMERIC
                 MOVE "E102" TO LK-REASON-CODE
                 MOVE "INVALID PREF CODE" TO LK-REASON-TEXT
              ELSE
                 IF LK-PREF-CODE < "01" OR LK-PREF-CODE > "47"
                    MOVE "E102" TO LK-REASON-CODE
                    MOVE "INVALID PREF CODE" TO LK-REASON-TEXT
                 END-IF
              END-IF
           END-IF

           IF LK-REASON-CODE = SPACE
              IF LK-OFFICE-CODE NOT NUMERIC
                 MOVE "E103" TO LK-REASON-CODE
                 MOVE "INVALID OFFICE CODE" TO LK-REASON-TEXT
              ELSE
                 IF LK-OFFICE-CODE = "00000"
                    MOVE "E103" TO LK-REASON-CODE
                    MOVE "INVALID OFFICE CODE" TO LK-REASON-TEXT
                 END-IF
              END-IF
           END-IF

           IF LK-REASON-CODE = SPACE
              IF LK-APPLY-DATE NOT NUMERIC
                 MOVE "E104" TO LK-REASON-CODE
                 MOVE "INVALID APPLY DATE" TO LK-REASON-TEXT
              ELSE
                 MOVE LK-APPLY-DATE TO WS-IN-DATE-N
                 PERFORM 2100-CHECK-DATE
              END-IF
           END-IF.

       2100-CHECK-DATE SECTION.
       2100-START.
           IF WS-IN-YYYY < 1989 OR WS-IN-YYYY > 2099
              MOVE "E104" TO LK-REASON-CODE
              MOVE "INVALID APPLY DATE" TO LK-REASON-TEXT
           END-IF

           IF LK-REASON-CODE = SPACE
              IF WS-IN-MM < 1 OR WS-IN-MM > 12
                 MOVE "E104" TO LK-REASON-CODE
                 MOVE "INVALID APPLY DATE" TO LK-REASON-TEXT
              END-IF
           END-IF

           IF LK-REASON-CODE = SPACE
              EVALUATE WS-IN-MM
                 WHEN 1
                 WHEN 3
                 WHEN 5
                 WHEN 7
                 WHEN 8
                 WHEN 10
                 WHEN 12
                    MOVE 31 TO WS-DAYS-IN-MONTH
                 WHEN 4
                 WHEN 6
                 WHEN 9
                 WHEN 11
                    MOVE 30 TO WS-DAYS-IN-MONTH
                 WHEN 2
                    DIVIDE WS-IN-YYYY BY 4
                       GIVING WS-LEAP-QUOTIENT
                       REMAINDER WS-LEAP-REM-4
                    DIVIDE WS-IN-YYYY BY 100
                       GIVING WS-LEAP-QUOTIENT
                       REMAINDER WS-LEAP-REM-100
                    DIVIDE WS-IN-YYYY BY 400
                       GIVING WS-LEAP-QUOTIENT
                       REMAINDER WS-LEAP-REM-400
                    IF WS-LEAP-REM-400 = 0
                       MOVE 29 TO WS-DAYS-IN-MONTH
                    ELSE
                       IF WS-LEAP-REM-4 = 0
                          AND WS-LEAP-REM-100 NOT = 0
                          MOVE 29 TO WS-DAYS-IN-MONTH
                       ELSE
                          MOVE 28 TO WS-DAYS-IN-MONTH
                       END-IF
                    END-IF
              END-EVALUATE
              IF WS-IN-DD < 1 OR WS-IN-DD > WS-DAYS-IN-MONTH
                 MOVE "E104" TO LK-REASON-CODE
                 MOVE "INVALID APPLY DATE" TO LK-REASON-TEXT
              END-IF
           END-IF.

       3000-OPEN-FILE SECTION.
       3000-START.
           OPEN INPUT KZRTMF
           IF WS-KZRTMF-ST = "00"
              SET FILE-OPENED TO TRUE
           ELSE
              MOVE "S901" TO LK-REASON-CODE
              MOVE "KZRTMF OPEN ERROR" TO LK-REASON-TEXT
              MOVE WS-KZRTMF-ST TO WS-DISP-ST
              DISPLAY "KZRTMF OPEN ERROR ST=" WS-DISP-ST
              SET HARD-ERROR TO TRUE
           END-IF.

       4000-SEARCH-RATE SECTION.
       4000-START.
           MOVE LOW-VALUE TO RT-RATE-KEY

           START KZRTMF KEY IS GREATER THAN OR EQUAL TO RT-RATE-KEY
              INVALID KEY
                 MOVE "E201" TO LK-REASON-CODE
                 MOVE "RATE MASTER NOT FOUND" TO LK-REASON-TEXT
              NOT INVALID KEY
                 PERFORM 4100-READ-LOOP
           END-START

           IF LK-REASON-CODE = SPACE
              IF FOUND-RATE
                 PERFORM 4300-SET-FOUND
              ELSE
                 MOVE "E201" TO LK-REASON-CODE
                 MOVE "RATE MASTER NOT FOUND" TO LK-REASON-TEXT
              END-IF
           END-IF.

       4100-READ-LOOP SECTION.
       4100-START.
           PERFORM UNTIL END-OF-FILE
              OR LK-REASON-CODE NOT = SPACE
              READ KZRTMF NEXT RECORD
                 AT END
                    SET END-OF-FILE TO TRUE
                 NOT AT END
                    PERFORM 4200-CHECK-RECORD
              END-READ
           END-PERFORM.

       4200-CHECK-RECORD SECTION.
       4200-START.
           IF RT-TAX-KIND = LK-TAX-KIND
              AND RT-EFFECTIVE-DATE <= WS-IN-DATE-N
              IF RT-RATE-DENOMINATOR = ZERO
                 MOVE "E203" TO LK-REASON-CODE
                 MOVE "INVALID RATE DENOMINATOR" TO LK-REASON-TEXT
                 MOVE RT-AUTH-REF-NO TO WS-DISP-KEY
                 DISPLAY "KZRTMF INVALID DENOMINATOR KEY="
                    WS-DISP-KEY
              ELSE
                 IF NOT FOUND-RATE
                    SET FOUND-RATE TO TRUE
                    PERFORM 4210-SAVE-BEST
                 ELSE
                    IF RT-EFFECTIVE-DATE > WS-BEST-DATE
                       PERFORM 4210-SAVE-BEST
                    END-IF
                 END-IF
              END-IF
           END-IF.

       4210-SAVE-BEST SECTION.
       4210-START.
           MOVE RT-EFFECTIVE-DATE    TO WS-BEST-DATE
           MOVE RT-AUTH-REF-NO       TO WS-BEST-KEY
           MOVE RT-RATE-NUMERATOR    TO WS-BEST-NUMERATOR
           MOVE RT-RATE-DENOMINATOR  TO WS-BEST-DENOMINATOR
           MOVE RT-AUTH-REF-NO       TO WS-BEST-AUTH-REF-NO.

       4300-SET-FOUND SECTION.
       4300-START.
           MOVE WS-BEST-KEY         TO LK-VALID-RATE-KEY
           MOVE WS-BEST-NUMERATOR   TO LK-RATE-NUMERATOR
           MOVE WS-BEST-DENOMINATOR TO LK-RATE-DENOMINATOR
           MOVE WS-BEST-AUTH-REF-NO TO LK-AUTH-REF-NO.

       8000-CLOSE-FILE SECTION.
       8000-START.
           CLOSE KZRTMF
           SET FILE-NOT-OPENED TO TRUE
           IF WS-KZRTMF-ST NOT = "00"
              MOVE "S902" TO LK-REASON-CODE
              MOVE "KZRTMF CLOSE ERROR" TO LK-REASON-TEXT
              MOVE WS-KZRTMF-ST TO WS-DISP-ST
              DISPLAY "KZRTMF CLOSE ERROR ST=" WS-DISP-ST
              SET HARD-ERROR TO TRUE
           END-IF.
