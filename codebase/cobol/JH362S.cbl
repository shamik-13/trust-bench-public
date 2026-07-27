       IDENTIFICATION DIVISION.
       PROGRAM-ID. JH362S.
      * 変更履歴
      * 版数  年月日(和暦)  担当                    概要
      * 1.00  H30.04.01    システム部 情報系チーム キャンペーン適格性判定新規作成
      * 1.01  R02.10.15    システム部 情報系チーム 対象商品コード判定条件追加
      * 1.02  R05.06.01    システム部 情報系チーム 顧客属性による除外判定見直し

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT JHREFMST-FILE ASSIGN TO "JHREFMST"
             ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC
             RECORD KEY IS RF-REF-TYPE-CD
             FILE STATUS IS WS-JHREFMST-ST.

       DATA DIVISION.
       FILE SECTION.
       FD  JHREFMST-FILE.
           COPY JHREFMSTC.

       WORKING-STORAGE SECTION.
       01  WS-JHREFMST-ST             PIC XX VALUE SPACE.
       01  WS-JHREFMST-OPEN-SW        PIC X  VALUE "0".
           88  JHREFMST-OPENED               VALUE "1".
       01  WS-END-SW                  PIC X  VALUE "0".
           88  REF-END                       VALUE "1".
       01  WS-MATCH-SW                PIC X  VALUE "0".
           88  REF-MATCHED                   VALUE "1".
       01  WS-LOW-KEY                 PIC X(10) VALUE "CPELG     ".
       01  WS-REF-PRI                 PIC 9(02) VALUE ZERO.
       01  WS-BEST-PRI                PIC 9(02) VALUE ZERO.
       01  WS-REF-CAMP-ID             PIC X(10) VALUE SPACE.
       01  WS-REF-SEG-CD              PIC X(04) VALUE SPACE.
       01  WS-REF-ATTR-CD             PIC X(02) VALUE SPACE.
       01  WS-REF-REASON-CD           PIC X(04) VALUE SPACE.
       01  WS-REF-REASON-TX           PIC X(40) VALUE SPACE.
       01  WS-DATE-NUM                PIC 9(08) VALUE ZERO.
       01  WS-VALID-FROM              PIC 9(08) VALUE ZERO.
       01  WS-VALID-TO                PIC 9(08) VALUE ZERO.
       01  WS-MSG                     PIC X(80) VALUE SPACE.

       LINKAGE SECTION.
       01  LK-JH362S-PARM.
           05  LK-CAMPAIGN-ID         PIC X(10).
           05  LK-CUSTOMER-ATTR-CD    PIC X(02).
           05  LK-SEGMENT-CD          PIC X(04).
           05  LK-DM-REJECT-FLAG      PIC X.
           05  LK-DORMANT-FLAG        PIC X.
           05  LK-JUDGE-DATE          PIC 9(08).
           05  LK-ELIGIBLE-FLAG       PIC X.
           05  LK-DENY-REASON-CD      PIC X(04).
           05  LK-REASON-TEXT         PIC X(40).

       PROCEDURE DIVISION USING LK-JH362S-PARM.

       0000-MAIN.
           PERFORM 1000-INIT
           PERFORM 2000-VALIDATE
           IF RETURN-CODE NOT = 0
              GOBACK
           END-IF

           IF LK-ELIGIBLE-FLAG = "N"
              GOBACK
           END-IF

           PERFORM 3000-OPEN-FILE
           IF RETURN-CODE NOT = 0
              PERFORM 9000-FINAL
              GOBACK
           END-IF

           PERFORM 4000-SEARCH-REF
           PERFORM 5000-SET-RESULT
           PERFORM 9000-FINAL
           GOBACK.

       1000-INIT.
           MOVE 0 TO RETURN-CODE
           MOVE "Y" TO LK-ELIGIBLE-FLAG
           MOVE SPACE TO LK-DENY-REASON-CD
           MOVE SPACE TO LK-REASON-TEXT
           MOVE "0" TO WS-END-SW
           MOVE "0" TO WS-MATCH-SW
           MOVE ZERO TO WS-BEST-PRI
           MOVE LK-JUDGE-DATE TO WS-DATE-NUM.

       2000-VALIDATE.
           IF LK-CAMPAIGN-ID = SPACE
              MOVE "N" TO LK-ELIGIBLE-FLAG
              MOVE "E001" TO LK-DENY-REASON-CD
              MOVE "CAMPAIGN ID IS REQUIRED" TO LK-REASON-TEXT
           ELSE
              IF LK-CUSTOMER-ATTR-CD NOT = "01"
                 AND LK-CUSTOMER-ATTR-CD NOT = "02"
                 AND LK-CUSTOMER-ATTR-CD NOT = "03"
                 AND LK-CUSTOMER-ATTR-CD NOT = "04"
                 MOVE "N" TO LK-ELIGIBLE-FLAG
                 MOVE "A001" TO LK-DENY-REASON-CD
                 MOVE "CUSTOMER ATTRIBUTE IS INVALID"
                   TO LK-REASON-TEXT
              ELSE
                 IF LK-SEGMENT-CD = SPACE
                    MOVE "N" TO LK-ELIGIBLE-FLAG
                    MOVE "S001" TO LK-DENY-REASON-CD
                    MOVE "SEGMENT CODE IS REQUIRED"
                      TO LK-REASON-TEXT
                 ELSE
                    IF LK-DM-REJECT-FLAG = "1"
                       MOVE "N" TO LK-ELIGIBLE-FLAG
                       MOVE "D001" TO LK-DENY-REASON-CD
                       MOVE "DM REJECT CUSTOMER" TO LK-REASON-TEXT
                    ELSE
                       IF LK-DORMANT-FLAG = "1"
                          MOVE "N" TO LK-ELIGIBLE-FLAG
                          MOVE "K001" TO LK-DENY-REASON-CD
                          MOVE "DORMANT ACCOUNT" TO LK-REASON-TEXT
                       ELSE
                          IF LK-JUDGE-DATE < 19000101
                             OR LK-JUDGE-DATE > 20991231
                             MOVE "N" TO LK-ELIGIBLE-FLAG
                             MOVE "E002" TO LK-DENY-REASON-CD
                             MOVE "JUDGE DATE IS INVALID"
                               TO LK-REASON-TEXT
                          END-IF
                       END-IF
                    END-IF
                 END-IF
              END-IF
           END-IF.

       3000-OPEN-FILE.
           OPEN INPUT JHREFMST-FILE
           IF WS-JHREFMST-ST = "00"
              MOVE "1" TO WS-JHREFMST-OPEN-SW
           ELSE
              MOVE 12 TO RETURN-CODE
              STRING "JHREFMST OPEN FAILED ST="
                     WS-JHREFMST-ST
                DELIMITED BY SIZE INTO WS-MSG
              DISPLAY WS-MSG
           END-IF.

       4000-SEARCH-REF.
           MOVE WS-LOW-KEY TO RF-REF-TYPE-CD
           START JHREFMST-FILE KEY IS >= RF-REF-TYPE-CD
              INVALID KEY
                 MOVE "1" TO WS-END-SW
              NOT INVALID KEY
                 CONTINUE
           END-START

           PERFORM UNTIL REF-END
              READ JHREFMST-FILE NEXT RECORD
                 AT END
                    MOVE "1" TO WS-END-SW
                 NOT AT END
                    IF WS-JHREFMST-ST NOT = "00"
                       MOVE 12 TO RETURN-CODE
                       STRING "JHREFMST READ FAILED ST="
                              WS-JHREFMST-ST
                         DELIMITED BY SIZE INTO WS-MSG
                       DISPLAY WS-MSG
                       MOVE "1" TO WS-END-SW
                    ELSE
                       IF RF-REF-TYPE-CD NOT = WS-LOW-KEY
                          MOVE "1" TO WS-END-SW
                       ELSE
                          PERFORM 4100-EVALUATE-REF
                       END-IF
                    END-IF
              END-READ
           END-PERFORM.

       4100-EVALUATE-REF.
           MOVE RF-REF-CD TO WS-REF-CAMP-ID
           MOVE RF-REF-NAME(1:2) TO WS-REF-PRI
           MOVE RF-REF-NAME(3:4) TO WS-REF-SEG-CD
           MOVE RF-REF-NAME(7:2) TO WS-REF-ATTR-CD
           MOVE RF-REF-NAME(9:4) TO WS-REF-REASON-CD
           MOVE SPACE TO WS-REF-REASON-TX
           MOVE RF-REF-NAME(13:28) TO WS-REF-REASON-TX
           MOVE RF-VALID-FROM-DATE TO WS-VALID-FROM
           MOVE RF-VALID-TO-DATE TO WS-VALID-TO

           IF RF-ACTIVE-FLAG = "1"
              AND WS-REF-CAMP-ID = LK-CAMPAIGN-ID
              AND WS-REF-SEG-CD = LK-SEGMENT-CD
              AND WS-REF-ATTR-CD = LK-CUSTOMER-ATTR-CD
              AND WS-DATE-NUM >= WS-VALID-FROM
              AND WS-DATE-NUM <= WS-VALID-TO
              IF NOT REF-MATCHED
                 MOVE "1" TO WS-MATCH-SW
                 MOVE WS-REF-PRI TO WS-BEST-PRI
                 MOVE WS-REF-REASON-CD TO LK-DENY-REASON-CD
                 MOVE WS-REF-REASON-TX TO LK-REASON-TEXT
              ELSE
                 IF WS-REF-PRI < WS-BEST-PRI
                    MOVE WS-REF-PRI TO WS-BEST-PRI
                    MOVE WS-REF-REASON-CD TO LK-DENY-REASON-CD
                    MOVE WS-REF-REASON-TX TO LK-REASON-TEXT
                 END-IF
              END-IF
           END-IF.

       5000-SET-RESULT.
           IF RETURN-CODE NOT = 0
              CONTINUE
           ELSE
              IF REF-MATCHED
                 MOVE "Y" TO LK-ELIGIBLE-FLAG
                 IF LK-DENY-REASON-CD = SPACE
                    MOVE "C000" TO LK-DENY-REASON-CD
                    MOVE "CAMPAIGN ELIGIBLE" TO LK-REASON-TEXT
                 END-IF
              ELSE
                 MOVE "N" TO LK-ELIGIBLE-FLAG
                 MOVE "M001" TO LK-DENY-REASON-CD
                 MOVE "NO ELIGIBLE CONDITION MATCHED"
                   TO LK-REASON-TEXT
              END-IF
           END-IF.

       9000-FINAL.
           IF JHREFMST-OPENED
              CLOSE JHREFMST-FILE
              IF WS-JHREFMST-ST NOT = "00"
                 MOVE 8 TO RETURN-CODE
                 STRING "JHREFMST CLOSE FAILED ST="
                        WS-JHREFMST-ST
                   DELIMITED BY SIZE INTO WS-MSG
                 DISPLAY WS-MSG
              END-IF
           END-IF.
