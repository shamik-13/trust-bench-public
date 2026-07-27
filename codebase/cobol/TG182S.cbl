       IDENTIFICATION DIVISION.
       PROGRAM-ID. TG182S.
      *---------------------------------------------------------------*
      * 変更履歴                                                      *
      * 版数  年月日        担当                    概要              *
      * 1.0   平成30年04月  システム部 為替・対外接続チーム 初版作成  *
      * 1.1   令和02年10月  システム部 為替・対外接続チーム 日付確認  *
      * 1.2   令和05年06月  システム部 為替・対外接続チーム 項目確認  *
      *---------------------------------------------------------------*

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-REC-LEN                 PIC 9(04) COMP.
       01  WS-DATE-X                  PIC X(08).
       01  WS-YYYY                    PIC 9(04).
       01  WS-MM                      PIC 9(02).
       01  WS-DD                      PIC 9(02).
       01  WS-QUOT                    PIC 9(04).
       01  WS-REM4                    PIC 9(04).
       01  WS-REM100                  PIC 9(04).
       01  WS-REM400                  PIC 9(04).
       01  WS-MAX-DD                  PIC 9(02).
       01  WS-LEAP-FLG                PIC X(01).
       01  WS-ERROR-FLG               PIC X(01).
       01  WS-FIELD-NAME              PIC X(30).
       01  WS-FIELD-START             PIC 9(04) COMP.
       01  WS-FIELD-LEN               PIC 9(04) COMP.
       01  WS-IDX                     PIC 9(04) COMP.
       01  WS-POS                     PIC 9(04) COMP.
       01  WS-CHECK-CHAR              PIC X(01).

       01  C-NORMAL                   PIC X(01) VALUE '0'.
       01  C-ERROR                    PIC X(01) VALUE '1'.
       01  C-REC-SIZE                 PIC 9(04) VALUE 120.
       01  C-ZERO                     PIC X(01) VALUE '0'.
       01  C-ONE                      PIC X(01) VALUE '1'.

       LINKAGE SECTION.
       01  TG182S-PARM.
           05  TG182S-IN-LENGTH       PIC 9(04) COMP.
           05  TG182S-IN-RECORD       PIC X(120).
           05  TG182S-OUT-STATUS      PIC X(01).
           05  TG182S-OUT-ITEM-NAME   PIC X(30).
           05  TG182S-OUT-POSITION    PIC 9(04) COMP.
           05  TG182S-OUT-MESSAGE     PIC X(80).

       PROCEDURE DIVISION USING TG182S-PARM.
       0000-MAIN.
           MOVE 0 TO RETURN-CODE
           PERFORM 1000-INIT
           PERFORM 2000-CHECK-RECORD
           PERFORM 9000-RETURN
           GOBACK
           .

       1000-INIT.
           MOVE C-NORMAL TO TG182S-OUT-STATUS
           MOVE SPACE    TO TG182S-OUT-ITEM-NAME
           MOVE ZERO     TO TG182S-OUT-POSITION
           MOVE SPACE    TO TG182S-OUT-MESSAGE
           MOVE C-NORMAL TO WS-ERROR-FLG
           MOVE TG182S-IN-LENGTH TO WS-REC-LEN
           .

       2000-CHECK-RECORD.
           PERFORM 2100-CHECK-LENGTH
           IF WS-ERROR-FLG = C-NORMAL
              PERFORM 2200-CHECK-BOUNDARY
           END-IF
           IF WS-ERROR-FLG = C-NORMAL
              PERFORM 2300-CHECK-NUMERIC
           END-IF
           IF WS-ERROR-FLG = C-NORMAL
              PERFORM 2400-CHECK-DATE
           END-IF
           IF WS-ERROR-FLG = C-NORMAL
              PERFORM 2500-CHECK-CENTER-SEQ
           END-IF
           .

       2100-CHECK-LENGTH.
           IF TG182S-IN-LENGTH NOT = C-REC-SIZE
              MOVE 'RECORD-LENGTH' TO TG182S-OUT-ITEM-NAME
              MOVE 1               TO TG182S-OUT-POSITION
              MOVE 'レコード長不正' TO TG182S-OUT-MESSAGE
              MOVE C-ERROR         TO TG182S-OUT-STATUS
              MOVE C-ERROR         TO WS-ERROR-FLG
           END-IF
           .

       2200-CHECK-BOUNDARY.
           IF TG182S-IN-RECORD(1:1) NOT = '1'
              MOVE 'DATA-KBN' TO TG182S-OUT-ITEM-NAME
              MOVE 1          TO TG182S-OUT-POSITION
              MOVE 'データ区分不正' TO TG182S-OUT-MESSAGE
              MOVE C-ERROR    TO TG182S-OUT-STATUS
              MOVE C-ERROR    TO WS-ERROR-FLG
           END-IF

           IF WS-ERROR-FLG = C-NORMAL
              IF TG182S-IN-RECORD(120:1) NOT = SPACE
                 MOVE 'LAST-RESERVE' TO TG182S-OUT-ITEM-NAME
                 MOVE 120            TO TG182S-OUT-POSITION
                 MOVE '最終バイト不正' TO TG182S-OUT-MESSAGE
                 MOVE C-ERROR        TO TG182S-OUT-STATUS
                 MOVE C-ERROR        TO WS-ERROR-FLG
              END-IF
           END-IF
           .

       2300-CHECK-NUMERIC.
           MOVE 'TYPE-CODE' TO WS-FIELD-NAME
           MOVE 2           TO WS-FIELD-START
           MOVE 2           TO WS-FIELD-LEN
           PERFORM 2310-CHECK-NUM-FIELD

           IF WS-ERROR-FLG = C-NORMAL
              MOVE 'CLIENT-CODE' TO WS-FIELD-NAME
              MOVE 4             TO WS-FIELD-START
              MOVE 10            TO WS-FIELD-LEN
              PERFORM 2310-CHECK-NUM-FIELD
           END-IF

           IF WS-ERROR-FLG = C-NORMAL
              MOVE 'BRANCH-CODE' TO WS-FIELD-NAME
              MOVE 14            TO WS-FIELD-START
              MOVE 3             TO WS-FIELD-LEN
              PERFORM 2310-CHECK-NUM-FIELD
           END-IF

           IF WS-ERROR-FLG = C-NORMAL
              MOVE 'ACCOUNT-TYPE' TO WS-FIELD-NAME
              MOVE 17             TO WS-FIELD-START
              MOVE 1              TO WS-FIELD-LEN
              PERFORM 2310-CHECK-NUM-FIELD
           END-IF

           IF WS-ERROR-FLG = C-NORMAL
              MOVE 'ACCOUNT-NO' TO WS-FIELD-NAME
              MOVE 18           TO WS-FIELD-START
              MOVE 7            TO WS-FIELD-LEN
              PERFORM 2310-CHECK-NUM-FIELD
           END-IF

           IF WS-ERROR-FLG = C-NORMAL
              MOVE 'PROCESS-DATE' TO WS-FIELD-NAME
              MOVE 25             TO WS-FIELD-START
              MOVE 8              TO WS-FIELD-LEN
              PERFORM 2310-CHECK-NUM-FIELD
           END-IF

           IF WS-ERROR-FLG = C-NORMAL
              MOVE 'AMOUNT' TO WS-FIELD-NAME
              MOVE 33       TO WS-FIELD-START
              MOVE 12       TO WS-FIELD-LEN
              PERFORM 2310-CHECK-NUM-FIELD
           END-IF

           IF WS-ERROR-FLG = C-NORMAL
              MOVE 'CENTER-SEQUENCE' TO WS-FIELD-NAME
              MOVE 45                TO WS-FIELD-START
              MOVE 8                 TO WS-FIELD-LEN
              PERFORM 2310-CHECK-NUM-FIELD
           END-IF
           .

       2310-CHECK-NUM-FIELD.
           PERFORM VARYING WS-IDX FROM 0 BY 1
              UNTIL WS-IDX >= WS-FIELD-LEN
                 OR WS-ERROR-FLG = C-ERROR
              COMPUTE WS-POS = WS-FIELD-START + WS-IDX
              MOVE TG182S-IN-RECORD(WS-POS:1) TO WS-CHECK-CHAR
              IF WS-CHECK-CHAR < '0' OR WS-CHECK-CHAR > '9'
                 MOVE WS-FIELD-NAME TO TG182S-OUT-ITEM-NAME
                 MOVE WS-POS        TO TG182S-OUT-POSITION
                 MOVE '数字項目不正' TO TG182S-OUT-MESSAGE
                 MOVE C-ERROR       TO TG182S-OUT-STATUS
                 MOVE C-ERROR       TO WS-ERROR-FLG
              END-IF
           END-PERFORM
           .

       2400-CHECK-DATE.
           MOVE TG182S-IN-RECORD(25:8) TO WS-DATE-X
           MOVE WS-DATE-X(1:4)         TO WS-YYYY
           MOVE WS-DATE-X(5:2)         TO WS-MM
           MOVE WS-DATE-X(7:2)         TO WS-DD
           MOVE C-ZERO                 TO WS-LEAP-FLG

           IF WS-YYYY < 2000 OR WS-YYYY > 2099
              MOVE 'PROCESS-DATE' TO TG182S-OUT-ITEM-NAME
              MOVE 25             TO TG182S-OUT-POSITION
              MOVE '年不正' TO TG182S-OUT-MESSAGE
              MOVE C-ERROR        TO TG182S-OUT-STATUS
              MOVE C-ERROR        TO WS-ERROR-FLG
           END-IF

           IF WS-ERROR-FLG = C-NORMAL
              IF WS-MM < 1 OR WS-MM > 12
                 MOVE 'PROCESS-DATE' TO TG182S-OUT-ITEM-NAME
                 MOVE 29             TO TG182S-OUT-POSITION
                 MOVE '月不正' TO TG182S-OUT-MESSAGE
                 MOVE C-ERROR        TO TG182S-OUT-STATUS
                 MOVE C-ERROR        TO WS-ERROR-FLG
              END-IF
           END-IF

           IF WS-ERROR-FLG = C-NORMAL
              DIVIDE WS-YYYY BY 4
                 GIVING WS-QUOT REMAINDER WS-REM4
              DIVIDE WS-YYYY BY 100
                 GIVING WS-QUOT REMAINDER WS-REM100
              DIVIDE WS-YYYY BY 400
                 GIVING WS-QUOT REMAINDER WS-REM400

              IF WS-REM4 = 0
                 IF WS-REM100 NOT = 0 OR WS-REM400 = 0
                    MOVE C-ONE TO WS-LEAP-FLG
                 END-IF
              END-IF

              EVALUATE WS-MM
                 WHEN 1
                 WHEN 3
                 WHEN 5
                 WHEN 7
                 WHEN 8
                 WHEN 10
                 WHEN 12
                    MOVE 31 TO WS-MAX-DD
                 WHEN 4
                 WHEN 6
                 WHEN 9
                 WHEN 11
                    MOVE 30 TO WS-MAX-DD
                 WHEN 2
                    IF WS-LEAP-FLG = C-ONE
                       MOVE 29 TO WS-MAX-DD
                    ELSE
                       MOVE 28 TO WS-MAX-DD
                    END-IF
              END-EVALUATE

              IF WS-DD < 1 OR WS-DD > WS-MAX-DD
                 MOVE 'PROCESS-DATE' TO TG182S-OUT-ITEM-NAME
                 MOVE 31             TO TG182S-OUT-POSITION
                 MOVE '日不正'       TO TG182S-OUT-MESSAGE
                 MOVE C-ERROR        TO TG182S-OUT-STATUS
                 MOVE C-ERROR        TO WS-ERROR-FLG
              END-IF
           END-IF
           .

       2500-CHECK-CENTER-SEQ.
           IF TG182S-IN-RECORD(45:8) = '00000000'
              MOVE 'CENTER-SEQUENCE' TO TG182S-OUT-ITEM-NAME
              MOVE 45                TO TG182S-OUT-POSITION
              MOVE 'センター連番不正' TO TG182S-OUT-MESSAGE
              MOVE C-ERROR           TO TG182S-OUT-STATUS
              MOVE C-ERROR           TO WS-ERROR-FLG
           END-IF
           .

       9000-RETURN.
           IF TG182S-OUT-STATUS = C-NORMAL
              MOVE '正常' TO TG182S-OUT-MESSAGE
              MOVE 0 TO RETURN-CODE
           ELSE
              MOVE 8 TO RETURN-CODE
           END-IF
           .

       END PROGRAM TG182S.
