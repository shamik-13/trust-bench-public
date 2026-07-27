       IDENTIFICATION DIVISION.
       PROGRAM-ID. CR290S.
       AUTHOR.     MFG共通基盤 システム部.
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-CONST.
           05  WS-MAX-LINE             PIC 9(03) VALUE 060.
           05  WS-HEAD-LINE            PIC 9(03) VALUE 005.
           05  WS-DETAIL-LINE          PIC 9(03) VALUE 001.
           05  WS-SUBTOTAL-LINE        PIC 9(03) VALUE 002.
           05  WS-TOTAL-LINE           PIC 9(03) VALUE 003.
       01  WS-WORK.
           05  WS-NEED-LINE            PIC 9(03) VALUE ZERO.
           05  WS-TEST-LINE            PIC 9(03) VALUE ZERO.
           05  WS-ABEND-SW             PIC X(01) VALUE SPACE.
      *
       LINKAGE SECTION.
       01  LK-CR290S.
           05  LK-CR290S-IN.
               10  LK-CR290S-RPT-ID    PIC X(08).
               10  LK-CR290S-CUR-LINE  PIC 9(03).
               10  LK-CR290S-PAGE-KBN  PIC X(01).
               10  LK-CR290S-ROW-KBN   PIC X(01).
           05  LK-CR290S-OUT.
               10  LK-CR290S-NEXT-LINE PIC 9(03).
               10  LK-CR290S-CTL-CD    PIC X(02).
               10  LK-CR290S-STS-CD    PIC X(02).
      *
       PROCEDURE DIVISION USING LK-CR290S.
       MAIN-RTN.
           MOVE 0 TO RETURN-CODE
           MOVE ZERO TO LK-CR290S-NEXT-LINE
           MOVE '00' TO LK-CR290S-CTL-CD
           MOVE '00' TO LK-CR290S-STS-CD
           MOVE SPACE TO WS-ABEND-SW
           PERFORM 1000-VALIDATE
           IF WS-ABEND-SW = SPACE
              PERFORM 2000-SET-LINE
           END-IF
           IF WS-ABEND-SW = SPACE
              PERFORM 3000-JUDGE-PAGE
           END-IF
           GOBACK.
      *
       1000-VALIDATE.
           IF LK-CR290S-RPT-ID = SPACE
              DISPLAY 'CR290S 帳票ＩＤ未設定'
              MOVE '91' TO LK-CR290S-STS-CD
              MOVE '90' TO LK-CR290S-CTL-CD
              MOVE 8 TO RETURN-CODE
              MOVE '1' TO WS-ABEND-SW
           END-IF
           IF WS-ABEND-SW = SPACE
              IF LK-CR290S-CUR-LINE > WS-MAX-LINE
                 DISPLAY 'CR290S 現在行番号範囲外 RPT='
                         LK-CR290S-RPT-ID
                 MOVE '92' TO LK-CR290S-STS-CD
                 MOVE '90' TO LK-CR290S-CTL-CD
                 MOVE 8 TO RETURN-CODE
                 MOVE '1' TO WS-ABEND-SW
              END-IF
           END-IF
           IF WS-ABEND-SW = SPACE
              IF LK-CR290S-PAGE-KBN NOT = '0'
                 AND LK-CR290S-PAGE-KBN NOT = '1'
                 DISPLAY 'CR290S 改ページ区分不正 RPT='
                         LK-CR290S-RPT-ID
                 MOVE '93' TO LK-CR290S-STS-CD
                 MOVE '90' TO LK-CR290S-CTL-CD
                 MOVE 8 TO RETURN-CODE
                 MOVE '1' TO WS-ABEND-SW
              END-IF
           END-IF
           IF WS-ABEND-SW = SPACE
              IF LK-CR290S-ROW-KBN NOT = 'M'
                 AND LK-CR290S-ROW-KBN NOT = 'S'
                 AND LK-CR290S-ROW-KBN NOT = 'G'
                 DISPLAY 'CR290S 明細区分不正 RPT='
                         LK-CR290S-RPT-ID
                 MOVE '94' TO LK-CR290S-STS-CD
                 MOVE '90' TO LK-CR290S-CTL-CD
                 MOVE 8 TO RETURN-CODE
                 MOVE '1' TO WS-ABEND-SW
              END-IF
           END-IF.
      *
       2000-SET-LINE.
           EVALUATE LK-CR290S-ROW-KBN
             WHEN 'M'
               MOVE WS-DETAIL-LINE TO WS-NEED-LINE
             WHEN 'S'
               MOVE WS-SUBTOTAL-LINE TO WS-NEED-LINE
             WHEN 'G'
               MOVE WS-TOTAL-LINE TO WS-NEED-LINE
           END-EVALUATE.
      *
       3000-JUDGE-PAGE.
           IF LK-CR290S-PAGE-KBN = '1'
              MOVE WS-HEAD-LINE TO LK-CR290S-NEXT-LINE
              ADD WS-NEED-LINE TO LK-CR290S-NEXT-LINE
              MOVE '10' TO LK-CR290S-CTL-CD
              MOVE '00' TO LK-CR290S-STS-CD
           ELSE
              ADD LK-CR290S-CUR-LINE TO WS-NEED-LINE
                  GIVING WS-TEST-LINE
              IF WS-TEST-LINE > WS-MAX-LINE
                 MOVE WS-HEAD-LINE TO LK-CR290S-NEXT-LINE
                 ADD WS-NEED-LINE TO LK-CR290S-NEXT-LINE
                 MOVE '10' TO LK-CR290S-CTL-CD
                 MOVE '00' TO LK-CR290S-STS-CD
              ELSE
                 MOVE WS-TEST-LINE TO LK-CR290S-NEXT-LINE
                 MOVE '00' TO LK-CR290S-CTL-CD
                 MOVE '00' TO LK-CR290S-STS-CD
              END-IF
           END-IF.
