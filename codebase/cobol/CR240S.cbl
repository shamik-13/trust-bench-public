       IDENTIFICATION DIVISION.
       PROGRAM-ID. CR240S.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CCPOSF
               ASSIGN TO "CCPOSF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS PS-ORG-CD
               FILE STATUS IS WS-CCPOSF-ST.

       DATA DIVISION.
       FILE SECTION.
       FD  CCPOSF.
       COPY CCPOSC.

       WORKING-STORAGE SECTION.
       01  WS-CCPOSF-ST              PIC X(02) VALUE SPACE.

       01  WS-WORK.
           05 WS-OPENED-SW           PIC X(01) VALUE '0'.
              88 WS-CCPOSF-OPENED              VALUE '1'.
           05 WS-HARD-ERROR-SW       PIC X(01) VALUE '0'.
              88 WS-HARD-ERROR                 VALUE '1'.
           05 WS-SHUKKIN-STATUS      PIC X(01) VALUE SPACE.
           05 WS-NYUKIN-STATUS       PIC X(01) VALUE SPACE.
           05 WS-SHUKKIN-ORG         PIC X(10) VALUE SPACE.
           05 WS-NYUKIN-ORG          PIC X(10) VALUE SPACE.

       01  WS-RESULT-CODES.
           05 WS-CD-OK               PIC X(04) VALUE '0000'.
           05 WS-CD-AMT-ZERO         PIC X(04) VALUE '1001'.
           05 WS-CD-SAME-ORG         PIC X(04) VALUE '1002'.
           05 WS-CD-SHUKKIN-NF       PIC X(04) VALUE '2001'.
           05 WS-CD-NYUKIN-NF        PIC X(04) VALUE '2002'.
           05 WS-CD-SHUKKIN-STOP     PIC X(04) VALUE '3001'.
           05 WS-CD-NYUKIN-STOP      PIC X(04) VALUE '3002'.
           05 WS-CD-IO-ERROR         PIC X(04) VALUE '9001'.

       01  WS-RESULT-TEXTS.
           05 WS-TX-OK               PIC X(40)
              VALUE '検査正常'.
           05 WS-TX-AMT-ZERO         PIC X(40)
              VALUE '振替金額が正数でない'.
           05 WS-TX-SAME-ORG         PIC X(40)
              VALUE '出金組織と入金組織が同一'.
           05 WS-TX-SHUKKIN-NF       PIC X(40)
              VALUE '出金組織がCCPOSF未登録'.
           05 WS-TX-NYUKIN-NF        PIC X(40)
              VALUE '入金組織がCCPOSF未登録'.
           05 WS-TX-SHUKKIN-STOP     PIC X(40)
              VALUE '出金組織のポジション停止'.
           05 WS-TX-NYUKIN-STOP      PIC X(40)
              VALUE '入金組織のポジション停止'.
           05 WS-TX-IO-ERROR         PIC X(40)
              VALUE 'CCPOSF入出力異常'.

       LINKAGE SECTION.
       01  LK-CR240S-PARM.
           05 LK-SHUKKIN-ORG-CD      PIC X(10).
           05 LK-NYUKIN-ORG-CD       PIC X(10).
           05 LK-FURIKAE-AMT         PIC S9(13)V99 COMP-3.
           05 LK-KENSA-KBN           PIC X(01).
           05 LK-RIYU-CD             PIC X(04).
           05 LK-RIYU-TEXT           PIC X(40).

       PROCEDURE DIVISION USING LK-CR240S-PARM.
       MAIN-RTN.
           MOVE ZERO                 TO RETURN-CODE
           PERFORM INIT-RTN
           PERFORM OPEN-RTN
           IF NOT WS-HARD-ERROR
               PERFORM VALIDATE-RTN
           END-IF
           PERFORM CLOSE-RTN
           IF WS-HARD-ERROR
               MOVE 8                TO RETURN-CODE
           ELSE
               MOVE ZERO             TO RETURN-CODE
           END-IF
           GOBACK
           .

       INIT-RTN.
           MOVE '0'                  TO WS-HARD-ERROR-SW
           MOVE '0'                  TO WS-OPENED-SW
           MOVE SPACE                TO WS-SHUKKIN-STATUS
           MOVE SPACE                TO WS-NYUKIN-STATUS
           MOVE LK-SHUKKIN-ORG-CD    TO WS-SHUKKIN-ORG
           MOVE LK-NYUKIN-ORG-CD     TO WS-NYUKIN-ORG
           MOVE '0'                  TO LK-KENSA-KBN
           MOVE WS-CD-OK             TO LK-RIYU-CD
           MOVE WS-TX-OK             TO LK-RIYU-TEXT
           .

       OPEN-RTN.
           OPEN INPUT CCPOSF
           IF WS-CCPOSF-ST = '00'
               MOVE '1'              TO WS-OPENED-SW
           ELSE
               DISPLAY 'CCPOSF OPEN ERR'
               DISPLAY WS-CCPOSF-ST
               MOVE '1'              TO WS-HARD-ERROR-SW
               MOVE '9'              TO LK-KENSA-KBN
               MOVE WS-CD-IO-ERROR   TO LK-RIYU-CD
               MOVE WS-TX-IO-ERROR   TO LK-RIYU-TEXT
           END-IF
           .

       VALIDATE-RTN.
           EVALUATE TRUE
               WHEN LK-FURIKAE-AMT <= ZERO
                   MOVE '1'              TO LK-KENSA-KBN
                   MOVE WS-CD-AMT-ZERO   TO LK-RIYU-CD
                   MOVE WS-TX-AMT-ZERO   TO LK-RIYU-TEXT
               WHEN WS-SHUKKIN-ORG = WS-NYUKIN-ORG
                   MOVE '1'              TO LK-KENSA-KBN
                   MOVE WS-CD-SAME-ORG   TO LK-RIYU-CD
                   MOVE WS-TX-SAME-ORG   TO LK-RIYU-TEXT
               WHEN OTHER
                   PERFORM CHECK-SHUKKIN-RTN
                   IF LK-KENSA-KBN = '0'
                       PERFORM CHECK-NYUKIN-RTN
                   END-IF
           END-EVALUATE
           .

       CHECK-SHUKKIN-RTN.
           MOVE WS-SHUKKIN-ORG       TO PS-ORG-CD
           READ CCPOSF KEY IS PS-ORG-CD
               INVALID KEY
                   MOVE '1'              TO LK-KENSA-KBN
                   MOVE WS-CD-SHUKKIN-NF TO LK-RIYU-CD
                   MOVE WS-TX-SHUKKIN-NF TO LK-RIYU-TEXT
               NOT INVALID KEY
                   MOVE PS-POSITION-STATUS-KBN
                                            TO WS-SHUKKIN-STATUS
                   IF WS-SHUKKIN-STATUS NOT = '1'
                       MOVE '1'              TO LK-KENSA-KBN
                       MOVE WS-CD-SHUKKIN-STOP
                                              TO LK-RIYU-CD
                       MOVE WS-TX-SHUKKIN-STOP
                                              TO LK-RIYU-TEXT
                   END-IF
           END-READ

           IF WS-CCPOSF-ST NOT = '00'
              AND WS-CCPOSF-ST NOT = '23'
               DISPLAY 'CCPOSF READ OUT ERR'
               DISPLAY WS-CCPOSF-ST
               MOVE '1'              TO WS-HARD-ERROR-SW
               MOVE '9'              TO LK-KENSA-KBN
               MOVE WS-CD-IO-ERROR   TO LK-RIYU-CD
               MOVE WS-TX-IO-ERROR   TO LK-RIYU-TEXT
           END-IF
           .

       CHECK-NYUKIN-RTN.
           MOVE WS-NYUKIN-ORG        TO PS-ORG-CD
           READ CCPOSF KEY IS PS-ORG-CD
               INVALID KEY
                   MOVE '1'              TO LK-KENSA-KBN
                   MOVE WS-CD-NYUKIN-NF  TO LK-RIYU-CD
                   MOVE WS-TX-NYUKIN-NF  TO LK-RIYU-TEXT
               NOT INVALID KEY
                   MOVE PS-POSITION-STATUS-KBN
                                            TO WS-NYUKIN-STATUS
                   IF WS-NYUKIN-STATUS NOT = '1'
                       MOVE '1'              TO LK-KENSA-KBN
                       MOVE WS-CD-NYUKIN-STOP
                                              TO LK-RIYU-CD
                       MOVE WS-TX-NYUKIN-STOP
                                              TO LK-RIYU-TEXT
                   END-IF
           END-READ

           IF WS-CCPOSF-ST NOT = '00'
              AND WS-CCPOSF-ST NOT = '23'
               DISPLAY 'CCPOSF READ IN ERR'
               DISPLAY WS-CCPOSF-ST
               MOVE '1'              TO WS-HARD-ERROR-SW
               MOVE '9'              TO LK-KENSA-KBN
               MOVE WS-CD-IO-ERROR   TO LK-RIYU-CD
               MOVE WS-TX-IO-ERROR   TO LK-RIYU-TEXT
           END-IF
           .

       CLOSE-RTN.
           IF WS-CCPOSF-OPENED
               CLOSE CCPOSF
               IF WS-CCPOSF-ST NOT = '00'
                   DISPLAY 'CCPOSF CLOSE ERR'
                   DISPLAY WS-CCPOSF-ST
                   MOVE '1'              TO WS-HARD-ERROR-SW
                   MOVE '9'              TO LK-KENSA-KBN
                   MOVE WS-CD-IO-ERROR   TO LK-RIYU-CD
                   MOVE WS-TX-IO-ERROR   TO LK-RIYU-TEXT
               END-IF
           END-IF
           .
