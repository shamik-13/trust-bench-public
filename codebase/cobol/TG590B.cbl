       IDENTIFICATION DIVISION.
       PROGRAM-ID. TG590B.
      * 版数   年月日(和暦)   担当                         概要
      * 1.00   令和03.04.01   システム部 情報系/対外系チーム 新規作成
      * 1.10   令和04.10.03   システム部 情報系/対外系チーム 相手行属性追加
      * 1.20   令和06.01.15   システム部 情報系/対外系チーム 日次保守見直し
      *---------------------------------------------------------------*
      * 相手行マスタ日次保守バッチ                                    *
      *---------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TGNETCF ASSIGN TO "TGNETCF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS NC-COUNTER-BANK
               FILE STATUS IS WS-NC-STATUS.

           SELECT TGBANKF ASSIGN TO "TGBANKF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS BK-COUNTER-BANK
               FILE STATUS IS WS-BK-STATUS.

           SELECT TGACKF ASSIGN TO "TGACKF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-AK-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  TGNETCF.
           COPY TGNETCFC.

       FD  TGBANKF.
           COPY TGBANKFC.

       FD  TGACKF.
           COPY TGACKFC.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05 WS-NC-STATUS             PIC X(02) VALUE SPACES.
           05 WS-BK-STATUS             PIC X(02) VALUE SPACES.
           05 WS-AK-STATUS             PIC X(02) VALUE SPACES.

       01  WS-SWITCHES.
           05 WS-END-SW                PIC X(01) VALUE "N".
              88 END-OF-NET                     VALUE "Y".
           05 WS-HARD-ERROR-SW         PIC X(01) VALUE "N".
              88 HARD-ERROR                     VALUE "Y".
           05 WS-BANK-FOUND-SW         PIC X(01) VALUE "N".
              88 BANK-FOUND                     VALUE "Y".
           05 WS-UPDATE-NEEDED-SW      PIC X(01) VALUE "N".
              88 UPDATE-NEEDED                  VALUE "Y".

       01  WS-COUNTERS.
           05 WS-READ-CNT              PIC 9(09) VALUE ZERO.
           05 WS-BANK-UPD-CNT          PIC 9(09) VALUE ZERO.
           05 WS-ACK-CNT               PIC 9(09) VALUE ZERO.
           05 WS-WARN-CNT              PIC 9(09) VALUE ZERO.
           05 WS-ERR-CNT               PIC 9(09) VALUE ZERO.
           05 WS-CENTER-SEQ            PIC 9(07) VALUE ZERO.

       01  WS-WORK.
           05 WS-CTL-DT                PIC 9(08) VALUE ZERO.
           05 WS-NEW-STATUS            PIC X(01) VALUE SPACE.
           05 WS-RESULT-CD             PIC X(02) VALUE SPACE.
           05 WS-NOTICE-TYPE           PIC X(02) VALUE SPACE.
           05 WS-NOTICE-TEXT           PIC X(60) VALUE SPACES.
           05 WS-BANK-KEY              PIC X(07) VALUE SPACES.
           05 WS-ITEM-COUNT            PIC 9(05) VALUE ZERO.
           05 WS-OPENED-SW             PIC X(01) VALUE "N".
              88 FILES-OPENED                   VALUE "Y".

       01  WS-CONSTANTS.
           05 CN-FLAG-YES              PIC X(01) VALUE "Y".
           05 CN-FLAG-NO               PIC X(01) VALUE "N".
           05 CN-STATUS-ACTIVE         PIC X(01) VALUE "1".
           05 CN-STATUS-STOP           PIC X(01) VALUE "9".
           05 CN-STATUS-BEFORE         PIC X(01) VALUE "0".
           05 CN-STATUS-EXPIRED        PIC X(01) VALUE "8".
           05 CN-TYPE-OK               PIC X(02) VALUE "10".
           05 CN-TYPE-WARN             PIC X(02) VALUE "20".
           05 CN-TYPE-ERR              PIC X(02) VALUE "30".
           05 CN-RC-OK                 PIC X(02) VALUE "00".
           05 CN-RC-UPD                PIC X(02) VALUE "01".
           05 CN-RC-NO-BANK            PIC X(02) VALUE "21".
           05 CN-RC-DATE-ERR           PIC X(02) VALUE "22".
           05 CN-RC-STOP-INST          PIC X(02) VALUE "23".
           05 CN-RC-FLAG-ERR           PIC X(02) VALUE "24".

       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-INITIALIZE
           IF NOT HARD-ERROR
               PERFORM 2000-PROCESS
                   UNTIL END-OF-NET OR HARD-ERROR
           END-IF
           PERFORM 9000-FINALIZE
           GOBACK.

       1000-INITIALIZE.
           MOVE 0 TO RETURN-CODE
           OPEN INPUT TGNETCF
           IF WS-NC-STATUS NOT = "00"
               DISPLAY "TGNETCF OPEN ERROR ST=" WS-NC-STATUS
               MOVE "Y" TO WS-HARD-ERROR-SW
               MOVE 12 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF

           OPEN I-O TGBANKF
           IF WS-BK-STATUS NOT = "00"
               DISPLAY "TGBANKF OPEN ERROR ST=" WS-BK-STATUS
               MOVE "Y" TO WS-HARD-ERROR-SW
               MOVE 12 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF

           OPEN OUTPUT TGACKF
           IF WS-AK-STATUS NOT = "00"
               DISPLAY "TGACKF OPEN ERROR ST=" WS-AK-STATUS
               MOVE "Y" TO WS-HARD-ERROR-SW
               MOVE 12 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF

           MOVE "Y" TO WS-OPENED-SW
           PERFORM 2100-READ-NET.

       2000-PROCESS.
           ADD 1 TO WS-READ-CNT
           MOVE NC-CTL-DT TO WS-CTL-DT
           MOVE NC-COUNTER-BANK TO WS-BANK-KEY
           MOVE "N" TO WS-BANK-FOUND-SW
           MOVE "N" TO WS-UPDATE-NEEDED-SW

           PERFORM 2200-READ-BANK

           IF BANK-FOUND
               PERFORM 3000-VALIDATE-AND-UPDATE
           ELSE
               MOVE CN-TYPE-ERR TO WS-NOTICE-TYPE
               MOVE CN-RC-NO-BANK TO WS-RESULT-CD
               MOVE 1 TO WS-ITEM-COUNT
               MOVE "BANK NOT FOUND" TO WS-NOTICE-TEXT
               PERFORM 5000-WRITE-ACK
               ADD 1 TO WS-ERR-CNT
           END-IF

           IF NOT HARD-ERROR
               PERFORM 2100-READ-NET
           END-IF.

       2100-READ-NET.
           READ TGNETCF NEXT RECORD
               AT END
                   MOVE "Y" TO WS-END-SW
               NOT AT END
                   CONTINUE
           END-READ

           IF WS-NC-STATUS NOT = "00"
              AND WS-NC-STATUS NOT = "10"
               DISPLAY "TGNETCF READ ERROR ST=" WS-NC-STATUS
               MOVE "Y" TO WS-HARD-ERROR-SW
               MOVE 12 TO RETURN-CODE
           END-IF.

       2200-READ-BANK.
           MOVE WS-BANK-KEY TO BK-COUNTER-BANK
           READ TGBANKF
               INVALID KEY
                   MOVE "N" TO WS-BANK-FOUND-SW
               NOT INVALID KEY
                   MOVE "Y" TO WS-BANK-FOUND-SW
           END-READ

           IF WS-BK-STATUS NOT = "00"
              AND WS-BK-STATUS NOT = "23"
               DISPLAY "TGBANKF READ ERROR ST=" WS-BK-STATUS
               DISPLAY "BANK=" WS-BANK-KEY
               MOVE "Y" TO WS-HARD-ERROR-SW
               MOVE 12 TO RETURN-CODE
           END-IF.

       3000-VALIDATE-AND-UPDATE.
           IF BK-VALID-FROM > BK-VALID-TO
               MOVE CN-TYPE-ERR TO WS-NOTICE-TYPE
               MOVE CN-RC-DATE-ERR TO WS-RESULT-CD
               MOVE 1 TO WS-ITEM-COUNT
               MOVE "INVALID DATE RANGE" TO WS-NOTICE-TEXT
               PERFORM 5000-WRITE-ACK
               ADD 1 TO WS-ERR-CNT
               EXIT PARAGRAPH
           END-IF

           IF NC-OUT-FLAG NOT = CN-FLAG-YES
              AND NC-OUT-FLAG NOT = CN-FLAG-NO
               MOVE CN-TYPE-ERR TO WS-NOTICE-TYPE
               MOVE CN-RC-FLAG-ERR TO WS-RESULT-CD
               MOVE 1 TO WS-ITEM-COUNT
               MOVE "INVALID OUT FLAG" TO WS-NOTICE-TEXT
               PERFORM 5000-WRITE-ACK
               ADD 1 TO WS-ERR-CNT
               EXIT PARAGRAPH
           END-IF

           IF NC-IN-FLAG NOT = CN-FLAG-YES
              AND NC-IN-FLAG NOT = CN-FLAG-NO
               MOVE CN-TYPE-ERR TO WS-NOTICE-TYPE
               MOVE CN-RC-FLAG-ERR TO WS-RESULT-CD
               MOVE 1 TO WS-ITEM-COUNT
               MOVE "INVALID IN FLAG" TO WS-NOTICE-TEXT
               PERFORM 5000-WRITE-ACK
               ADD 1 TO WS-ERR-CNT
               EXIT PARAGRAPH
           END-IF

           IF BK-STATUS = CN-STATUS-STOP
              AND (NC-OUT-FLAG = CN-FLAG-YES
                   OR NC-IN-FLAG = CN-FLAG-YES)
               MOVE CN-TYPE-WARN TO WS-NOTICE-TYPE
               MOVE CN-RC-STOP-INST TO WS-RESULT-CD
               MOVE 1 TO WS-ITEM-COUNT
               MOVE "STOPPED BANK INSTRUCTION" TO WS-NOTICE-TEXT
               PERFORM 5000-WRITE-ACK
               ADD 1 TO WS-WARN-CNT
               EXIT PARAGRAPH
           END-IF

           PERFORM 3100-DETERMINE-STATUS

           IF UPDATE-NEEDED
               PERFORM 4000-REWRITE-BANK
               IF NOT HARD-ERROR
                   MOVE CN-TYPE-OK TO WS-NOTICE-TYPE
                   MOVE CN-RC-UPD TO WS-RESULT-CD
                   MOVE 1 TO WS-ITEM-COUNT
                   MOVE "BANK STATUS UPDATED" TO WS-NOTICE-TEXT
                   PERFORM 5000-WRITE-ACK
               END-IF
           ELSE
               MOVE CN-TYPE-OK TO WS-NOTICE-TYPE
               MOVE CN-RC-OK TO WS-RESULT-CD
               MOVE ZERO TO WS-ITEM-COUNT
               MOVE "NO BANK CHANGE" TO WS-NOTICE-TEXT
               PERFORM 5000-WRITE-ACK
           END-IF.

       3100-DETERMINE-STATUS.
           MOVE BK-STATUS TO WS-NEW-STATUS

           IF WS-CTL-DT < BK-VALID-FROM
               MOVE CN-STATUS-BEFORE TO WS-NEW-STATUS
           ELSE
               IF WS-CTL-DT > BK-VALID-TO
                   MOVE CN-STATUS-EXPIRED TO WS-NEW-STATUS
               ELSE
                   IF NC-OUT-FLAG = CN-FLAG-YES
                      OR NC-IN-FLAG = CN-FLAG-YES
                       MOVE CN-STATUS-ACTIVE TO WS-NEW-STATUS
                   ELSE
                       MOVE CN-STATUS-BEFORE TO WS-NEW-STATUS
                   END-IF
               END-IF
           END-IF

           IF WS-NEW-STATUS NOT = BK-STATUS
               MOVE WS-NEW-STATUS TO BK-STATUS
               MOVE "Y" TO WS-UPDATE-NEEDED-SW
           END-IF.

       4000-REWRITE-BANK.
           REWRITE TGBANKF-REC
           IF WS-BK-STATUS = "00"
               ADD 1 TO WS-BANK-UPD-CNT
           ELSE
               DISPLAY "TGBANKF REWRITE ERROR ST=" WS-BK-STATUS
               DISPLAY "BANK=" BK-COUNTER-BANK
               MOVE "Y" TO WS-HARD-ERROR-SW
               MOVE 12 TO RETURN-CODE
           END-IF.

       5000-WRITE-ACK.
           ADD 1 TO WS-CENTER-SEQ
           MOVE WS-CTL-DT TO AK-VALUE-DT
           MOVE WS-CENTER-SEQ TO AK-CENTER-SEQ
           MOVE WS-NOTICE-TYPE TO AK-NOTICE-TYPE
           MOVE WS-BANK-KEY TO AK-COUNTER-BANK
           MOVE WS-RESULT-CD TO AK-RESULT-CD
           MOVE WS-ITEM-COUNT TO AK-ITEM-COUNT
           MOVE SPACES TO AK-NOTICE-TEXT
           MOVE WS-NOTICE-TEXT TO AK-NOTICE-TEXT

           WRITE TGACKF-REC
           IF WS-AK-STATUS = "00"
               ADD 1 TO WS-ACK-CNT
           ELSE
               DISPLAY "TGACKF WRITE ERROR ST=" WS-AK-STATUS
               DISPLAY "BANK=" WS-BANK-KEY
               MOVE "Y" TO WS-HARD-ERROR-SW
               MOVE 12 TO RETURN-CODE
           END-IF.

       9000-FINALIZE.
           IF FILES-OPENED
               CLOSE TGNETCF
               IF WS-NC-STATUS NOT = "00"
                   DISPLAY "TGNETCF CLOSE ERROR ST=" WS-NC-STATUS
                   MOVE "Y" TO WS-HARD-ERROR-SW
                   MOVE 8 TO RETURN-CODE
               END-IF

               CLOSE TGBANKF
               IF WS-BK-STATUS NOT = "00"
                   DISPLAY "TGBANKF CLOSE ERROR ST=" WS-BK-STATUS
                   MOVE "Y" TO WS-HARD-ERROR-SW
                   MOVE 8 TO RETURN-CODE
               END-IF

               CLOSE TGACKF
               IF WS-AK-STATUS NOT = "00"
                   DISPLAY "TGACKF CLOSE ERROR ST=" WS-AK-STATUS
                   MOVE "Y" TO WS-HARD-ERROR-SW
                   MOVE 8 TO RETURN-CODE
               END-IF
           END-IF

           DISPLAY "TG590B READ COUNT=" WS-READ-CNT
           DISPLAY "TG590B UPDATE COUNT=" WS-BANK-UPD-CNT
           DISPLAY "TG590B ACK COUNT=" WS-ACK-CNT
           DISPLAY "TG590B WARN COUNT=" WS-WARN-CNT
           DISPLAY "TG590B ERROR COUNT=" WS-ERR-CNT

           IF NOT HARD-ERROR
               MOVE 0 TO RETURN-CODE
               DISPLAY "TG590B NORMAL END"
           ELSE
               IF RETURN-CODE = 0
                   MOVE 8 TO RETURN-CODE
               END-IF
               DISPLAY "TG590B ABNORMAL END RC=" RETURN-CODE
           END-IF.
