       IDENTIFICATION DIVISION.
       PROGRAM-ID. CM210S.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CMCIFF ASSIGN TO "CMCIFF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-CMCIFF-ST.
           SELECT CGCODF ASSIGN TO "CGCODF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS GC-CODE-ID
               FILE STATUS IS WS-CGCODF-ST.

       DATA DIVISION.
       FILE SECTION.
       FD  CMCIFF.
           COPY CMCIFFC.
       FD  CGCODF.
           COPY CGCODC.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05 WS-CMCIFF-ST              PIC XX VALUE SPACE.
           05 WS-CGCODF-ST              PIC XX VALUE SPACE.

       01  WS-SWITCHES.
           05 WS-CIF-EOF-SW             PIC X VALUE '0'.
              88 CIF-EOF                      VALUE '1'.
           05 WS-CODE-EOF-SW            PIC X VALUE '0'.
              88 CODE-EOF                     VALUE '1'.
           05 WS-CIF-FOUND-SW           PIC X VALUE '0'.
              88 CIF-FOUND                    VALUE '1'.
           05 WS-CODE-FOUND-SW          PIC X VALUE '0'.
              88 CODE-FOUND                   VALUE '1'.

       01  WS-DATE-AREA.
           05 WS-CURR-DATE              PIC 9(8) VALUE ZERO.
           05 WS-CURR-YYYY              PIC 9(4) VALUE ZERO.
           05 WS-CURR-MMDD              PIC 9(4) VALUE ZERO.
           05 WS-BIRTH-YYYY             PIC 9(4) VALUE ZERO.
           05 WS-BIRTH-MMDD             PIC 9(4) VALUE ZERO.
           05 WS-AGE                    PIC S9(4) COMP VALUE ZERO.

       01  WS-WORK-AREA.
           05 WS-SEGMENT-KBN            PIC X(04) VALUE SPACE.
           05 WS-WARN-ST                PIC X(02) VALUE SPACE.
           05 WS-CHK-CODE-KBN           PIC X(10) VALUE SPACE.
           05 WS-CHK-CODE-VALUE         PIC X(10) VALUE SPACE.
           05 WS-INVALID-ITEM           PIC X(20) VALUE SPACE.

       01  WS-CONSTANTS.
           05 WS-KBN-CIF-STATUS         PIC X(10) VALUE 'CIFSTATUS'.
           05 WS-KBN-SEX                PIC X(10) VALUE 'SEX'.
           05 WS-KBN-SEGMENT            PIC X(10) VALUE 'CMSEGMENT'.
           05 WS-STATUS-ACTIVE          PIC X(02) VALUE '01'.
           05 WS-STATUS-EXCLUDE         PIC X(02) VALUE '08'.
           05 WS-STATUS-INACTIVE        PIC X(02) VALUE '09'.
           05 WS-SEX-MALE               PIC X(01) VALUE '1'.
           05 WS-SEX-FEMALE             PIC X(01) VALUE '2'.

       LINKAGE SECTION.
       01  LK-CM210S-PARM.
           05 LK-CIF-NO                 PIC X(12).
           05 LK-SEGMENT-KBN            PIC X(04).
           05 LK-STATUS                 PIC X(02).
           05 LK-REASON-CODE            PIC X(20).

       PROCEDURE DIVISION USING LK-CM210S-PARM.
       0000-MAIN.
           MOVE 0 TO RETURN-CODE
           MOVE SPACE TO LK-SEGMENT-KBN
                         LK-STATUS
                         LK-REASON-CODE
           PERFORM 1000-OPEN-FILES
           IF RETURN-CODE NOT = 0
               PERFORM 9000-END
           END-IF

           PERFORM 2000-READ-CIF

           IF RETURN-CODE = 0
               IF CIF-FOUND
                   PERFORM 3000-VALIDATE-INPUT
                   IF RETURN-CODE = 0
                       IF WS-WARN-ST = SPACE
                           PERFORM 4000-JUDGE-SEGMENT
                       END-IF
                   END-IF
               ELSE
                   MOVE '04' TO LK-STATUS
                   MOVE 'CIFミトウロク' TO LK-REASON-CODE
               END-IF
           END-IF

           PERFORM 8000-CLOSE-FILES
           PERFORM 9000-END.

       1000-OPEN-FILES.
           OPEN INPUT CMCIFF
           IF WS-CMCIFF-ST NOT = '00'
               DISPLAY 'CMCIFF オープン失敗 ST=' WS-CMCIFF-ST
               MOVE 8 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF

           OPEN INPUT CGCODF
           IF WS-CGCODF-ST NOT = '00'
               DISPLAY 'CGCODF オープン失敗 ST=' WS-CGCODF-ST
               MOVE 8 TO RETURN-CODE
           END-IF.

       2000-READ-CIF.
           MOVE '0' TO WS-CIF-EOF-SW
           MOVE '0' TO WS-CIF-FOUND-SW

           PERFORM UNTIL CIF-EOF OR CIF-FOUND
               READ CMCIFF
                   AT END
                       MOVE '1' TO WS-CIF-EOF-SW
                   NOT AT END
                       IF CF-CIF-NO = LK-CIF-NO
                           MOVE '1' TO WS-CIF-FOUND-SW
                       END-IF
               END-READ

               IF WS-CMCIFF-ST NOT = '00'
                  AND WS-CMCIFF-ST NOT = '10'
                   DISPLAY 'CMCIFF 読込失敗 ST=' WS-CMCIFF-ST
                   MOVE 8 TO RETURN-CODE
                   MOVE '1' TO WS-CIF-EOF-SW
               END-IF
           END-PERFORM.

       3000-VALIDATE-INPUT.
           MOVE SPACE TO WS-WARN-ST
           MOVE WS-KBN-CIF-STATUS TO WS-CHK-CODE-KBN
           MOVE CF-CIF-STATUS-KBN TO WS-CHK-CODE-VALUE
           PERFORM 3100-CHECK-CODE
           IF NOT CODE-FOUND
               MOVE '01' TO WS-WARN-ST
               MOVE 'CIF状態区分不正' TO LK-REASON-CODE
           END-IF

           IF CF-CIF-STATUS-KBN NOT = WS-STATUS-ACTIVE
              AND CF-CIF-STATUS-KBN NOT = WS-STATUS-EXCLUDE
              AND CF-CIF-STATUS-KBN NOT = WS-STATUS-INACTIVE
               MOVE '01' TO WS-WARN-ST
               MOVE 'CIF状態区分未定義' TO LK-REASON-CODE
           END-IF

           MOVE WS-KBN-SEX TO WS-CHK-CODE-KBN
           MOVE CF-SEX-KBN TO WS-CHK-CODE-VALUE
           PERFORM 3100-CHECK-CODE
           IF NOT CODE-FOUND
               MOVE '02' TO WS-WARN-ST
               MOVE '性別区分不正' TO LK-REASON-CODE
           END-IF

           IF CF-BIRTH-DT NOT NUMERIC
              OR CF-BIRTH-DT = ZERO
               MOVE '03' TO WS-WARN-ST
               MOVE '生年月日不正' TO LK-REASON-CODE
           END-IF

           IF WS-WARN-ST NOT = SPACE
               MOVE 'W1' TO LK-STATUS
           END-IF.

       3100-CHECK-CODE.
           CLOSE CGCODF
           OPEN INPUT CGCODF
           IF WS-CGCODF-ST NOT = '00'
               DISPLAY 'CGCODF 再オープン失敗 ST=' WS-CGCODF-ST
               MOVE 8 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF

           MOVE '0' TO WS-CODE-EOF-SW
           MOVE '0' TO WS-CODE-FOUND-SW

           PERFORM UNTIL CODE-EOF OR CODE-FOUND
               READ CGCODF NEXT RECORD
                   AT END
                       MOVE '1' TO WS-CODE-EOF-SW
                   NOT AT END
                       IF GC-CODE-KBN = WS-CHK-CODE-KBN
                          AND GC-CODE-VALUE = WS-CHK-CODE-VALUE
                          AND WS-CURR-DATE >= GC-VALID-FROM-DT
                          AND WS-CURR-DATE <= GC-VALID-TO-DT
                           MOVE '1' TO WS-CODE-FOUND-SW
                       END-IF
               END-READ

               IF WS-CGCODF-ST NOT = '00'
                  AND WS-CGCODF-ST NOT = '10'
                   DISPLAY 'CGCODF 読込失敗 ST=' WS-CGCODF-ST
                   MOVE 8 TO RETURN-CODE
                   MOVE '1' TO WS-CODE-EOF-SW
               END-IF
           END-PERFORM.

       4000-JUDGE-SEGMENT.
           MOVE FUNCTION CURRENT-DATE(1:8) TO WS-CURR-DATE
           MOVE WS-CURR-DATE(1:4) TO WS-CURR-YYYY
           MOVE WS-CURR-DATE(5:4) TO WS-CURR-MMDD
           MOVE CF-BIRTH-DT(1:4) TO WS-BIRTH-YYYY
           MOVE CF-BIRTH-DT(5:4) TO WS-BIRTH-MMDD

           COMPUTE WS-AGE = WS-CURR-YYYY - WS-BIRTH-YYYY
           IF WS-CURR-MMDD < WS-BIRTH-MMDD
               SUBTRACT 1 FROM WS-AGE
           END-IF

           EVALUATE TRUE
               WHEN CF-CIF-STATUS-KBN = WS-STATUS-EXCLUDE
                   MOVE 'EXCL' TO WS-SEGMENT-KBN
               WHEN CF-CIF-STATUS-KBN = WS-STATUS-INACTIVE
                   MOVE 'STOP' TO WS-SEGMENT-KBN
               WHEN WS-AGE < 20
                   MOVE 'YOUN' TO WS-SEGMENT-KBN
               WHEN WS-AGE >= 65
                   MOVE 'SENR' TO WS-SEGMENT-KBN
               WHEN CF-SEX-KBN = WS-SEX-MALE
                   MOVE 'ADLM' TO WS-SEGMENT-KBN
               WHEN CF-SEX-KBN = WS-SEX-FEMALE
                   MOVE 'ADLF' TO WS-SEGMENT-KBN
               WHEN OTHER
                   MOVE 'ADLT' TO WS-SEGMENT-KBN
           END-EVALUATE

           MOVE WS-KBN-SEGMENT TO WS-CHK-CODE-KBN
           MOVE WS-SEGMENT-KBN TO WS-CHK-CODE-VALUE
           PERFORM 3100-CHECK-CODE

           IF RETURN-CODE NOT = 0
               EXIT PARAGRAPH
           END-IF

           IF CODE-FOUND
               MOVE WS-SEGMENT-KBN TO LK-SEGMENT-KBN
               MOVE '00' TO LK-STATUS
               MOVE SPACE TO LK-REASON-CODE
           ELSE
               MOVE 'W2' TO LK-STATUS
               MOVE 'セグメント区分未定義' TO LK-REASON-CODE
           END-IF.

       8000-CLOSE-FILES.
           CLOSE CMCIFF
           IF WS-CMCIFF-ST NOT = '00'
              AND WS-CMCIFF-ST NOT = '42'
               DISPLAY 'CMCIFF クローズ失敗 ST=' WS-CMCIFF-ST
               MOVE 8 TO RETURN-CODE
           END-IF

           CLOSE CGCODF
           IF WS-CGCODF-ST NOT = '00'
              AND WS-CGCODF-ST NOT = '42'
               DISPLAY 'CGCODF クローズ失敗 ST=' WS-CGCODF-ST
               MOVE 8 TO RETURN-CODE
           END-IF.

       9000-END.
           GOBACK.
