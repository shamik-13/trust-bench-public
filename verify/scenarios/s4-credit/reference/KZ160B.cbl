       IDENTIFICATION DIVISION.
       PROGRAM-ID. KZ160B.
       AUTHOR.     BATCH.
      ******************************************************************
      * CREDIT EXPOSURE SUMMARY BATCH
      ******************************************************************

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZEXPF
               ASSIGN TO "KZEXPF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-KZEXPF.
           SELECT KZEXPRF
               ASSIGN TO "KZEXPRF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-KZEXPRF.

       DATA DIVISION.
       FILE SECTION.

       FD  KZEXPF.
           COPY KZEXPFC.

       FD  KZEXPRF.
           COPY KZEXPRC.

       WORKING-STORAGE SECTION.
       01  FS-KZEXPF                 PIC X(02) VALUE SPACES.
       01  FS-KZEXPRF                PIC X(02) VALUE SPACES.

       01  WS-END-FLAG               PIC X(01) VALUE 'N'.
           88  END-OF-KZEXPF                    VALUE 'Y'.
           88  NOT-END-OF-KZEXPF                VALUE 'N'.

       01  WS-ABEND-FLAG             PIC X(01) VALUE 'N'.
           88  ABEND-OCCURRED                   VALUE 'Y'.
           88  NO-ABEND                         VALUE 'N'.

       01  WS-PRODUCT-CAP            PIC 9(11) VALUE ZERO.
       01  WS-IN-COUNT               PIC 9(09) VALUE ZERO.
       01  WS-OUT-COUNT              PIC 9(09) VALUE ZERO.
       01  WS-MSG-COUNT              PIC Z(9).

       PROCEDURE DIVISION.
       MAIN-SECTION.
           PERFORM 1000-INIT
           IF NO-ABEND
               PERFORM 2000-MAIN UNTIL END-OF-KZEXPF
           END-IF
           PERFORM 9000-FINAL
           GOBACK
           .

       1000-INIT.
           MOVE ZERO TO RETURN-CODE
           SET NOT-END-OF-KZEXPF TO TRUE
           SET NO-ABEND TO TRUE

           DISPLAY 'KZ160B START'

           OPEN INPUT KZEXPF
           IF FS-KZEXPF NOT = '00'
               DISPLAY 'KZEXPF OPEN ERROR ST=' FS-KZEXPF
               MOVE 8 TO RETURN-CODE
               SET ABEND-OCCURRED TO TRUE
           END-IF

           IF NO-ABEND
               OPEN OUTPUT KZEXPRF
               IF FS-KZEXPRF NOT = '00'
                   DISPLAY 'KZEXPRF OPEN ERROR ST=' FS-KZEXPRF
                   MOVE 8 TO RETURN-CODE
                   SET ABEND-OCCURRED TO TRUE
               END-IF
           END-IF

           IF NO-ABEND
               PERFORM 2100-READ-KZEXPF
           END-IF
           .

       2000-MAIN.
           ADD 1 TO WS-IN-COUNT

           PERFORM 3000-SET-PRODUCT-CAP

           IF NO-ABEND
               PERFORM 4000-MAKE-OUTPUT
               PERFORM 5000-WRITE-KZEXPRF
           END-IF

           IF NO-ABEND
               PERFORM 2100-READ-KZEXPF
           ELSE
               SET END-OF-KZEXPF TO TRUE
           END-IF
           .

       2100-READ-KZEXPF.
           READ KZEXPF
               AT END
                   SET END-OF-KZEXPF TO TRUE
               NOT AT END
                   IF FS-KZEXPF NOT = '00'
                       DISPLAY 'KZEXPF READ ERROR ST=' FS-KZEXPF
                       MOVE 8 TO RETURN-CODE
                       SET ABEND-OCCURRED TO TRUE
                       SET END-OF-KZEXPF TO TRUE
                   END-IF
           END-READ
           .

       3000-SET-PRODUCT-CAP.
           EVALUATE EX-PRODUCT-TYPE
               WHEN '01'
                   MOVE 1000000 TO WS-PRODUCT-CAP
               WHEN '02'
                   MOVE 3000000 TO WS-PRODUCT-CAP
               WHEN '03'
                   MOVE 10000000 TO WS-PRODUCT-CAP
               WHEN OTHER
                   DISPLAY 'INVALID PRODUCT CUST=' EX-CUST-ID
                   DISPLAY 'INVALID PRODUCT TYPE=' EX-PRODUCT-TYPE
                   MOVE 12 TO RETURN-CODE
                   SET ABEND-OCCURRED TO TRUE
           END-EVALUATE
           .

       4000-MAKE-OUTPUT.
           MOVE SPACES TO KZEXPRF-REC
           MOVE EX-CUST-ID      TO XR-CUST-ID
           MOVE EX-PRODUCT-TYPE TO XR-PRODUCT-TYPE
           MOVE EX-EXPOSURE-AMT TO XR-EXPOSURE-AMT

           IF EX-EXPOSURE-AMT > WS-PRODUCT-CAP
               MOVE WS-PRODUCT-CAP TO XR-CAPPED-AMT
               MOVE 'Y'            TO XR-OVER-FLAG
           ELSE
               MOVE EX-EXPOSURE-AMT TO XR-CAPPED-AMT
               MOVE 'N'             TO XR-OVER-FLAG
           END-IF
           .

       5000-WRITE-KZEXPRF.
           WRITE KZEXPRF-REC
           IF FS-KZEXPRF NOT = '00'
               DISPLAY 'KZEXPRF WRITE ERROR ST=' FS-KZEXPRF
               MOVE 8 TO RETURN-CODE
               SET ABEND-OCCURRED TO TRUE
           ELSE
               ADD 1 TO WS-OUT-COUNT
           END-IF
           .

       9000-FINAL.
           IF FS-KZEXPF = '00' OR FS-KZEXPF = '10'
               CLOSE KZEXPF
               IF FS-KZEXPF NOT = '00'
                   DISPLAY 'KZEXPF CLOSE ERROR ST=' FS-KZEXPF
                   IF RETURN-CODE = 0
                       MOVE 8 TO RETURN-CODE
                   END-IF
               END-IF
           END-IF

           IF FS-KZEXPRF = '00'
               CLOSE KZEXPRF
               IF FS-KZEXPRF NOT = '00'
                   DISPLAY 'KZEXPRF CLOSE ERROR ST=' FS-KZEXPRF
                   IF RETURN-CODE = 0
                       MOVE 8 TO RETURN-CODE
                   END-IF
               END-IF
           END-IF

           MOVE WS-IN-COUNT TO WS-MSG-COUNT
           DISPLAY 'KZ160B INPUT COUNT=' WS-MSG-COUNT
           MOVE WS-OUT-COUNT TO WS-MSG-COUNT
           DISPLAY 'KZ160B OUTPUT COUNT=' WS-MSG-COUNT

           IF RETURN-CODE = 0
               DISPLAY 'KZ160B NORMAL END'
           ELSE
               DISPLAY 'KZ160B ABNORMAL END'
           END-IF
           .
