       IDENTIFICATION DIVISION.
       PROGRAM-ID. CM150S.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CGCODF ASSIGN TO "CGCODF"
              ORGANIZATION IS INDEXED
              ACCESS MODE IS DYNAMIC
              RECORD KEY IS GC-CODE-ID
              FILE STATUS IS CGCODF-ST.

       DATA DIVISION.
       FILE SECTION.
       FD  CGCODF.
           COPY CGCODC.

       WORKING-STORAGE SECTION.
       01  CGCODF-ST                 PIC XX.
       01  WK-ABEND-SW               PIC X VALUE SPACE.
           88  WK-ABEND                    VALUE '1'.

       01  WK-IDX                    PIC 9(03) COMP-3.
       01  WK-OUT-IDX                PIC 9(03) COMP-3.
       01  WK-PREV-IDX               PIC 9(03) COMP-3.
       01  WK-DIGIT-CNT              PIC 9(02) COMP-3.
       01  WK-TODAY                  PIC 9(08).
       01  WK-VALID-SW               PIC X VALUE SPACE.
           88  WK-VALID                    VALUE '1'.
       01  WK-MSG                    PIC X(80).

       LINKAGE SECTION.
       01  CM150S-PARM.
           05  CM150S-IN-KANA        PIC X(80).
           05  CM150S-IN-ADDR-CD     PIC X(05).
           05  CM150S-IN-TEL         PIC X(20).
           05  CM150S-OUT-KANA       PIC X(80).
           05  CM150S-OUT-ADDR-CD    PIC X(05).
           05  CM150S-OUT-TEL        PIC X(11).
           05  CM150S-STATUS         PIC X(02).
           05  CM150S-REASON         PIC X(40).

       PROCEDURE DIVISION USING CM150S-PARM.
       0000-MAIN.
           MOVE 0 TO RETURN-CODE
           MOVE SPACE TO CM150S-OUT-KANA
                         CM150S-OUT-ADDR-CD
                         CM150S-OUT-TEL
                         CM150S-STATUS
                         CM150S-REASON
           MOVE FUNCTION CURRENT-DATE(1:8) TO WK-TODAY

           OPEN INPUT CGCODF
           IF CGCODF-ST NOT = '00'
              MOVE 12 TO RETURN-CODE
              STRING 'CGCODF OPEN ST=' CGCODF-ST
                DELIMITED BY SIZE INTO WK-MSG
              END-STRING
              DISPLAY WK-MSG
              MOVE '1' TO WK-ABEND-SW
           END-IF

           IF NOT WK-ABEND
              PERFORM 1000-NORMALIZE-KANA
              PERFORM 2000-VALIDATE-ADDR
              PERFORM 3000-VALIDATE-TEL
           END-IF

           IF CGCODF-ST = '00'
              CLOSE CGCODF
              IF CGCODF-ST NOT = '00'
                 MOVE 8 TO RETURN-CODE
                 STRING 'CGCODF CLOSE ST=' CGCODF-ST
                   DELIMITED BY SIZE INTO WK-MSG
                 END-STRING
                 DISPLAY WK-MSG
              END-IF
           END-IF

           GOBACK.

       1000-NORMALIZE-KANA.
           MOVE FUNCTION UPPER-CASE(CM150S-IN-KANA)
             TO CM150S-OUT-KANA

           INSPECT CM150S-OUT-KANA
             CONVERTING 'ｱｲｳｴｵ' TO 'アイウエオ'
           INSPECT CM150S-OUT-KANA
             CONVERTING 'ｶｷｸｹｺ' TO 'カキクケコ'
           INSPECT CM150S-OUT-KANA
             CONVERTING 'ｻｼｽｾｿ' TO 'サシスセソ'
           INSPECT CM150S-OUT-KANA
             CONVERTING 'ﾀﾁﾂﾃﾄ' TO 'タチツテト'
           INSPECT CM150S-OUT-KANA
             CONVERTING 'ﾅﾆﾇﾈﾉ' TO 'ナニヌネノ'
           INSPECT CM150S-OUT-KANA
             CONVERTING 'ﾊﾋﾌﾍﾎ' TO 'ハヒフヘホ'
           INSPECT CM150S-OUT-KANA
             CONVERTING 'ﾏﾐﾑﾒﾓ' TO 'マミムメモ'
           INSPECT CM150S-OUT-KANA
             CONVERTING 'ﾔﾕﾖ' TO 'ヤユヨ'
           INSPECT CM150S-OUT-KANA
             CONVERTING 'ﾗﾘﾙﾚﾛ' TO 'ラリルレロ'
           INSPECT CM150S-OUT-KANA
             CONVERTING 'ﾜｦﾝ' TO 'ワヲン'
           INSPECT CM150S-OUT-KANA
             CONVERTING 'ｰ－―‐' TO 'ーーーー'

           PERFORM VARYING WK-IDX FROM 1 BY 1 UNTIL WK-IDX > 80
              IF CM150S-OUT-KANA(WK-IDX:1) = X'09'
                 MOVE SPACE TO CM150S-OUT-KANA(WK-IDX:1)
              END-IF
           END-PERFORM

           INSPECT CM150S-OUT-KANA REPLACING ALL '　' BY SPACE
           PERFORM 1100-COMPRESS-SPACE.

       1100-COMPRESS-SPACE.
           MOVE 1 TO WK-OUT-IDX
           PERFORM VARYING WK-IDX FROM 1 BY 1 UNTIL WK-IDX > 80
              IF CM150S-OUT-KANA(WK-IDX:1) NOT = SPACE
                 MOVE CM150S-OUT-KANA(WK-IDX:1)
                   TO CM150S-OUT-KANA(WK-OUT-IDX:1)
                 ADD 1 TO WK-OUT-IDX
              ELSE
                 IF WK-OUT-IDX > 1
                    COMPUTE WK-PREV-IDX = WK-OUT-IDX - 1
                    IF CM150S-OUT-KANA(WK-PREV-IDX:1) NOT = SPACE
                       MOVE SPACE TO CM150S-OUT-KANA(WK-OUT-IDX:1)
                       ADD 1 TO WK-OUT-IDX
                    END-IF
                 END-IF
              END-IF
           END-PERFORM

           IF WK-OUT-IDX <= 80
              MOVE SPACE TO CM150S-OUT-KANA(WK-OUT-IDX:81 - WK-OUT-IDX)
           END-IF.

       2000-VALIDATE-ADDR.
           MOVE '1' TO WK-VALID-SW
           IF CM150S-IN-ADDR-CD NOT NUMERIC
              MOVE SPACE TO WK-VALID-SW
              MOVE '10' TO CM150S-STATUS
              MOVE 'ADDR NOT NUMERIC' TO CM150S-REASON
           END-IF

           IF WK-VALID
              MOVE CM150S-IN-ADDR-CD TO CM150S-OUT-ADDR-CD
              PERFORM 2100-READ-CODE
              IF NOT WK-VALID
                 STRING CM150S-IN-ADDR-CD(1:2) '000'
                   DELIMITED BY SIZE INTO CM150S-OUT-ADDR-CD
                 END-STRING
                 PERFORM 2100-READ-CODE
                 IF NOT WK-VALID
                    MOVE '11' TO CM150S-STATUS
                    MOVE 'ADDR CODE NOT FOUND' TO CM150S-REASON
                    MOVE SPACE TO CM150S-OUT-ADDR-CD
                 END-IF
              END-IF
           END-IF.

       2100-READ-CODE.
           MOVE SPACE TO WK-VALID-SW
           MOVE SPACE TO GC-CODE-ID
           STRING 'MUNI' CM150S-OUT-ADDR-CD
             DELIMITED BY SIZE INTO GC-CODE-ID
           END-STRING

           READ CGCODF KEY IS GC-CODE-ID
              INVALID KEY
                 MOVE SPACE TO WK-VALID-SW
              NOT INVALID KEY
                 IF GC-CODE-KBN = 'MUNI'
                    AND GC-CODE-VALUE = CM150S-OUT-ADDR-CD
                    AND GC-VALID-FROM-DT <= WK-TODAY
                    AND GC-VALID-TO-DT >= WK-TODAY
                    MOVE '1' TO WK-VALID-SW
                 END-IF
           END-READ

           IF CGCODF-ST NOT = '00'
              AND CGCODF-ST NOT = '23'
              MOVE 8 TO RETURN-CODE
              STRING 'CGCODF READ ST=' CGCODF-ST
                DELIMITED BY SIZE INTO WK-MSG
              END-STRING
              DISPLAY WK-MSG
              MOVE '99' TO CM150S-STATUS
              MOVE 'CODE READ ERROR' TO CM150S-REASON
           END-IF.

       3000-VALIDATE-TEL.
           MOVE SPACE TO CM150S-OUT-TEL
           MOVE 0 TO WK-DIGIT-CNT

           PERFORM VARYING WK-IDX FROM 1 BY 1 UNTIL WK-IDX > 20
              EVALUATE CM150S-IN-TEL(WK-IDX:1)
                 WHEN '0' THRU '9'
                    IF WK-DIGIT-CNT < 11
                       ADD 1 TO WK-DIGIT-CNT
                       MOVE CM150S-IN-TEL(WK-IDX:1)
                         TO CM150S-OUT-TEL(WK-DIGIT-CNT:1)
                    ELSE
                       MOVE '20' TO CM150S-STATUS
                       MOVE 'TEL TOO LONG' TO CM150S-REASON
                    END-IF
                 WHEN SPACE
                 WHEN '-'
                 WHEN '('
                 WHEN ')'
                    CONTINUE
                 WHEN OTHER
                    MOVE '21' TO CM150S-STATUS
                    MOVE 'TEL INVALID CHAR' TO CM150S-REASON
              END-EVALUATE
           END-PERFORM

           IF CM150S-STATUS = SPACE
              EVALUATE TRUE
                 WHEN WK-DIGIT-CNT = 10
                    IF CM150S-OUT-TEL(1:2) = '03'
                       OR CM150S-OUT-TEL(1:2) = '06'
                       CONTINUE
                    ELSE
                       IF CM150S-OUT-TEL(1:1) NOT = '0'
                          MOVE '22' TO CM150S-STATUS
                          MOVE 'TEL FIRST DIGIT' TO CM150S-REASON
                       END-IF
                    END-IF
                 WHEN WK-DIGIT-CNT = 11
                    IF CM150S-OUT-TEL(1:1) NOT = '0'
                       MOVE '22' TO CM150S-STATUS
                       MOVE 'TEL FIRST DIGIT' TO CM150S-REASON
                    END-IF
                 WHEN OTHER
                    MOVE '23' TO CM150S-STATUS
                    MOVE 'TEL LENGTH ERROR' TO CM150S-REASON
              END-EVALUATE
           END-IF

           IF CM150S-STATUS = SPACE
              MOVE '00' TO CM150S-STATUS
              MOVE 'OK' TO CM150S-REASON
           END-IF.
