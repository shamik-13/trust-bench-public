       IDENTIFICATION DIVISION.
       PROGRAM-ID. CB715S.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-WORK-AREA.
           05  WS-EXC-CD              PIC X(04).
           05  WS-PGM-ID              PIC X(08).
           05  WS-SEV-RANK            PIC 9.
           05  WS-OWNER-CD            PIC X(04).
           05  WS-REASON-TEXT         PIC X(80).
           05  WS-EDIT-STATUS         PIC X(02).
           05  WS-SORT-RANK           PIC 9.

       01  WS-CONSTANTS.
           05  WK-LOWER               PIC X(26)
               VALUE "abcdefghijklmnopqrstuvwxyz".
           05  WK-UPPER               PIC X(26)
               VALUE "ABCDEFGHIJKLMNOPQRSTUVWXYZ".

       LINKAGE SECTION.
       01  LK-CB715S-PARM.
           05  LK-CB715S-IN-EXC-CD        PIC X(04).
           05  LK-CB715S-IN-PGM-ID        PIC X(08).
           05  LK-CB715S-OUT-SEVERITY     PIC 9.
           05  LK-CB715S-OUT-OWNER        PIC X(04).
           05  LK-CB715S-OUT-REASON       PIC X(80).
           05  LK-CB715S-OUT-STATUS       PIC X(02).
           05  LK-CB715S-OUT-SORT-KEY     PIC X(17).

       PROCEDURE DIVISION USING LK-CB715S-PARM.

       0000-MAIN.
           PERFORM 1000-INITIALIZE
           PERFORM 2000-VALIDATE
           IF WS-EDIT-STATUS = "00"
              PERFORM 3000-EDIT-REASON
           END-IF
           PERFORM 4000-SET-OUTPUT
           MOVE 0 TO RETURN-CODE
           GOBACK
           .

       1000-INITIALIZE.
           MOVE SPACES TO WS-WORK-AREA
           MOVE LK-CB715S-IN-EXC-CD TO WS-EXC-CD
           MOVE LK-CB715S-IN-PGM-ID TO WS-PGM-ID
           INSPECT WS-EXC-CD CONVERTING WK-LOWER TO WK-UPPER
           INSPECT WS-PGM-ID CONVERTING WK-LOWER TO WK-UPPER
           MOVE 0 TO WS-SEV-RANK
           MOVE "PEND" TO WS-OWNER-CD
           MOVE "リユウミヘンシュウ" TO WS-REASON-TEXT
           MOVE "00" TO WS-EDIT-STATUS
           .

       2000-VALIDATE.
           IF WS-EXC-CD = SPACES
              MOVE 9 TO WS-SEV-RANK
              MOVE "SYS " TO WS-OWNER-CD
              MOVE "レイガイリユウコードガミセッテイ"
                TO WS-REASON-TEXT
              MOVE "10" TO WS-EDIT-STATUS
           END-IF

           IF WS-EDIT-STATUS = "00"
              IF WS-PGM-ID = SPACES
                 MOVE 8 TO WS-SEV-RANK
                 MOVE "SYS " TO WS-OWNER-CD
                 MOVE "ケンチプログラムIDガミセッテイ"
                   TO WS-REASON-TEXT
                 MOVE "11" TO WS-EDIT-STATUS
              END-IF
           END-IF
           .

       3000-EDIT-REASON.
           EVALUATE WS-EXC-CD
              WHEN "E001"
                 MOVE 5 TO WS-SEV-RANK
                 MOVE "OPR " TO WS-OWNER-CD
                 MOVE "カードステータスフセイ"
                   TO WS-REASON-TEXT
              WHEN "E002"
                 MOVE 4 TO WS-SEV-RANK
                 MOVE "MER " TO WS-OWNER-CD
                 MOVE "カメイテンステータスフセイ"
                   TO WS-REASON-TEXT
              WHEN "E003"
                 MOVE 6 TO WS-SEV-RANK
                 MOVE "AUTH" TO WS-OWNER-CD
                 MOVE "ショウニンIDフトウゴウ"
                   TO WS-REASON-TEXT
              WHEN "E004"
                 MOVE 7 TO WS-SEV-RANK
                 MOVE "RSK " TO WS-OWNER-CD
                 MOVE "リヨウゲンドガクチョウカ"
                   TO WS-REASON-TEXT
              WHEN "E005"
                 MOVE 3 TO WS-SEV-RANK
                 MOVE "AMT " TO WS-OWNER-CD
                 MOVE "ショウニンガクトウリツガフイッチ"
                   TO WS-REASON-TEXT
              WHEN "E006"
                 MOVE 8 TO WS-SEV-RANK
                 MOVE "SYS " TO WS-OWNER-CD
                 MOVE "カードマスタミトウロク"
                   TO WS-REASON-TEXT
              WHEN "E007"
                 MOVE 2 TO WS-SEV-RANK
                 MOVE "OPR " TO WS-OWNER-CD
                 MOVE "カメイテンメイカナケタオーバー"
                   TO WS-REASON-TEXT
              WHEN "E008"
                 MOVE 6 TO WS-SEV-RANK
                 MOVE "MER " TO WS-OWNER-CD
                 MOVE "テスウリョウプランフセイゴウ"
                   TO WS-REASON-TEXT
              WHEN "E009"
                 MOVE 7 TO WS-SEV-RANK
                 MOVE "AUTH" TO WS-OWNER-CD
                 MOVE "ツウカクブンフセイ"
                   TO WS-REASON-TEXT
              WHEN "E010"
                 MOVE 5 TO WS-SEV-RANK
                 MOVE "TRD " TO WS-OWNER-CD
                 MOVE "トリケシウリアゲコンザイ"
                   TO WS-REASON-TEXT
              WHEN OTHER
                 MOVE 9 TO WS-SEV-RANK
                 MOVE "SYS " TO WS-OWNER-CD
                 MOVE "ミテイギノレイガイリユウコード"
                   TO WS-REASON-TEXT
                 MOVE "20" TO WS-EDIT-STATUS
           END-EVALUATE

           IF WS-EDIT-STATUS = "00"
              EVALUATE WS-PGM-ID
                 WHEN "CB710B  "
                    CONTINUE
                 WHEN OTHER
                    IF WS-SEV-RANK < 8
                       MOVE 8 TO WS-SEV-RANK
                    END-IF
                    MOVE "SYS " TO WS-OWNER-CD
                    MOVE "ケンチPGMタイショウガイ"
                      TO WS-REASON-TEXT
                    MOVE "21" TO WS-EDIT-STATUS
              END-EVALUATE
           END-IF
           .

       4000-SET-OUTPUT.
           MOVE WS-SEV-RANK TO LK-CB715S-OUT-SEVERITY
           MOVE WS-OWNER-CD TO LK-CB715S-OUT-OWNER
           MOVE WS-REASON-TEXT TO LK-CB715S-OUT-REASON
           MOVE WS-EDIT-STATUS TO LK-CB715S-OUT-STATUS

           COMPUTE WS-SORT-RANK = 9 - WS-SEV-RANK
           STRING WS-SORT-RANK DELIMITED BY SIZE
                  WS-OWNER-CD  DELIMITED BY SIZE
                  WS-EXC-CD    DELIMITED BY SIZE
                  WS-PGM-ID    DELIMITED BY SIZE
             INTO LK-CB715S-OUT-SORT-KEY
           END-STRING
           .
