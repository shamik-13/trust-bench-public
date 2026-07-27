       IDENTIFICATION DIVISION.
       PROGRAM-ID. JH421S.
      *
      *  変更履歴
      *  版数  年月日    担当  概要
      *  1.00  20240301  JK01  初版作成
      *  1.01  20240918  JK02  閉鎖口座警告判定を追加
      *  1.02  20250122  JK03  時系列逆転判定を強化
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT JHACDMF ASSIGN TO "JHACDMF"
             ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC
             RECORD KEY IS ACD-ACCT-NO
             FILE STATUS IS WS-JHACDMF-ST.

       DATA DIVISION.
       FILE SECTION.
       FD  JHACDMF.
           COPY JHACDC.

       WORKING-STORAGE SECTION.
       01  WS-JHACDMF-ST            PIC XX.
       01  WS-OPEN-SW               PIC X VALUE "0".
           88  WS-OPENED            VALUE "1".
       01  WS-FOUND-SW              PIC X VALUE "0".
           88  WS-FOUND             VALUE "1".
           88  WS-NOT-FOUND         VALUE "0".

       01  WS-ATTR-BUF              PIC X(512).
       01  WS-HASH-WORK.
           05 WS-HASH-SUM           PIC 9(10) VALUE ZERO.
           05 WS-HASH-MOD           PIC 9(10) VALUE ZERO.
           05 WS-HASH-DISP          PIC 9(10) VALUE ZERO.
       01  WS-CUR-HASH              PIC X(64).
       01  WS-EVT-HASH              PIC X(64).
       01  WS-IDX                   PIC 9(04) COMP.
       01  WS-LEN                   PIC 9(04) COMP.
       01  WS-CHAR-CODE             PIC 9(05) COMP.
       01  WS-TS-CHECK.
           05 WS-EVT-TS-N           PIC 9(14).
           05 WS-ACD-TS-N           PIC 9(14).

       LINKAGE SECTION.
       01  LK-JH421S-PARM.
           05 LK-EVT-ACCT-NO        PIC X(16).
           05 LK-EVT-AFTER-HASH     PIC X(64).
           05 LK-EVT-CHG-TS         PIC X(14).
           05 LK-EVT-STATUS-CD      PIC X(01).
           05 LK-JUDGE-CD           PIC X(01).
           05 LK-DETAIL-CD          PIC X(04).
           05 LK-REASON-TEXT        PIC X(80).

       PROCEDURE DIVISION USING LK-JH421S-PARM.
       0000-MAIN.
           MOVE 0 TO RETURN-CODE
           PERFORM 1000-INIT
           IF RETURN-CODE NOT = 0
              PERFORM 9000-END
           END-IF

           PERFORM 2000-READ-ACCOUNT
           IF RETURN-CODE NOT = 0
              PERFORM 9000-END
           END-IF

           PERFORM 3000-JUDGE
           PERFORM 9000-END
           GOBACK.

       1000-INIT.
           MOVE SPACE TO LK-JUDGE-CD
                         LK-DETAIL-CD
                         LK-REASON-TEXT
           MOVE LK-EVT-AFTER-HASH TO WS-EVT-HASH

           IF LK-EVT-ACCT-NO = SPACE
              MOVE "E" TO LK-JUDGE-CD
              MOVE "E001" TO LK-DETAIL-CD
              MOVE "口座番号未設定" TO LK-REASON-TEXT
              MOVE 8 TO RETURN-CODE
              EXIT PARAGRAPH
           END-IF

           IF LK-EVT-CHG-TS NOT NUMERIC
              MOVE "E" TO LK-JUDGE-CD
              MOVE "E002" TO LK-DETAIL-CD
              MOVE "変更時刻不正" TO LK-REASON-TEXT
              MOVE 8 TO RETURN-CODE
              EXIT PARAGRAPH
           END-IF

           IF LK-EVT-STATUS-CD NOT = "0"
              AND LK-EVT-STATUS-CD NOT = "1"
              AND LK-EVT-STATUS-CD NOT = "9"
              MOVE "E" TO LK-JUDGE-CD
              MOVE "E003" TO LK-DETAIL-CD
              MOVE "イベント口座状態不正" TO LK-REASON-TEXT
              MOVE 8 TO RETURN-CODE
              EXIT PARAGRAPH
           END-IF

           OPEN INPUT JHACDMF
           IF WS-JHACDMF-ST NOT = "00"
              MOVE "E" TO LK-JUDGE-CD
              MOVE "E101" TO LK-DETAIL-CD
              STRING "JHACDMF オープン失敗 ST="
                     WS-JHACDMF-ST
                DELIMITED BY SIZE INTO LK-REASON-TEXT
              END-STRING
              MOVE 12 TO RETURN-CODE
              EXIT PARAGRAPH
           END-IF
           MOVE "1" TO WS-OPEN-SW.

       2000-READ-ACCOUNT.
           MOVE "0" TO WS-FOUND-SW
           MOVE LK-EVT-ACCT-NO TO ACD-ACCT-NO

           READ JHACDMF KEY IS ACD-ACCT-NO
              INVALID KEY
                 IF WS-JHACDMF-ST = "23"
                    MOVE "0" TO WS-FOUND-SW
                 ELSE
                    MOVE "E" TO LK-JUDGE-CD
                    MOVE "E102" TO LK-DETAIL-CD
                    STRING "JHACDMF 読込失敗 ST="
                           WS-JHACDMF-ST
                      DELIMITED BY SIZE INTO LK-REASON-TEXT
                    END-STRING
                    MOVE 12 TO RETURN-CODE
                 END-IF
              NOT INVALID KEY
                 MOVE "1" TO WS-FOUND-SW
           END-READ.

       3000-JUDGE.
           IF WS-NOT-FOUND
              MOVE "A" TO LK-JUDGE-CD
              MOVE "A000" TO LK-DETAIL-CD
              MOVE "現行口座なし追加対象" TO LK-REASON-TEXT
              EXIT PARAGRAPH
           END-IF

           IF ACD-STATUS-CD NOT = "0"
              AND ACD-STATUS-CD NOT = "1"
              AND ACD-STATUS-CD NOT = "9"
              MOVE "E" TO LK-JUDGE-CD
              MOVE "E201" TO LK-DETAIL-CD
              MOVE "現行口座状態不正" TO LK-REASON-TEXT
              MOVE 8 TO RETURN-CODE
              EXIT PARAGRAPH
           END-IF

           IF ACD-LAST-CHG-TS NUMERIC
              MOVE LK-EVT-CHG-TS TO WS-EVT-TS-N
              MOVE ACD-LAST-CHG-TS TO WS-ACD-TS-N
              IF WS-EVT-TS-N < WS-ACD-TS-N
                 MOVE "R" TO LK-JUDGE-CD
                 MOVE "R001" TO LK-DETAIL-CD
                 MOVE "時系列逆転" TO LK-REASON-TEXT
                 EXIT PARAGRAPH
              END-IF
           ELSE
              MOVE "E" TO LK-JUDGE-CD
              MOVE "E202" TO LK-DETAIL-CD
              MOVE "現行最終変更時刻不正" TO LK-REASON-TEXT
              MOVE 8 TO RETURN-CODE
              EXIT PARAGRAPH
           END-IF

           IF (ACD-STATUS-CD = "9" OR ACD-CLOSE-DT NOT = SPACE)
              AND LK-EVT-STATUS-CD NOT = "9"
              MOVE "W" TO LK-JUDGE-CD
              MOVE "W001" TO LK-DETAIL-CD
              MOVE "閉鎖口座再オープン相当" TO LK-REASON-TEXT
              EXIT PARAGRAPH
           END-IF

           PERFORM 3100-CALC-CURRENT-HASH

           IF WS-EVT-HASH = WS-CUR-HASH
              MOVE "N" TO LK-JUDGE-CD
              MOVE "N000" TO LK-DETAIL-CD
              MOVE "属性差分なし" TO LK-REASON-TEXT
           ELSE
              MOVE "U" TO LK-JUDGE-CD
              MOVE "U000" TO LK-DETAIL-CD
              MOVE "属性差分あり更新対象" TO LK-REASON-TEXT
           END-IF.

       3100-CALC-CURRENT-HASH.
           MOVE SPACE TO WS-ATTR-BUF
           MOVE SPACE TO WS-CUR-HASH
           MOVE ZERO TO WS-HASH-SUM

           STRING ACD-ACCT-NO
                  ACD-CUSTOMER-ID
                  ACD-TRUST-PRODUCT-CD
                  ACD-BRANCH-CD
                  ACD-OPEN-DT
                  ACD-CLOSE-DT
                  ACD-STATUS-CD
              DELIMITED BY SIZE INTO WS-ATTR-BUF
           END-STRING

           MOVE FUNCTION LENGTH(WS-ATTR-BUF) TO WS-LEN
           PERFORM VARYING WS-IDX FROM 1 BY 1
             UNTIL WS-IDX > WS-LEN
              IF WS-ATTR-BUF(WS-IDX:1) NOT = SPACE
                 COMPUTE WS-CHAR-CODE =
                    FUNCTION ORD(WS-ATTR-BUF(WS-IDX:1))
                 COMPUTE WS-HASH-SUM =
                    FUNCTION MOD((WS-HASH-SUM * 131)
                    + WS-CHAR-CODE, 1000000007)
              END-IF
           END-PERFORM

           COMPUTE WS-HASH-MOD = FUNCTION MOD(WS-HASH-SUM, 1000000000)
           MOVE WS-HASH-MOD TO WS-HASH-DISP
           MOVE WS-HASH-DISP TO WS-CUR-HASH(1:10).

       9000-END.
           IF WS-OPENED
              CLOSE JHACDMF
              IF WS-JHACDMF-ST NOT = "00"
                 AND RETURN-CODE = 0
                 MOVE "E" TO LK-JUDGE-CD
                 MOVE "E901" TO LK-DETAIL-CD
                 STRING "JHACDMF クローズ失敗 ST="
                        WS-JHACDMF-ST
                   DELIMITED BY SIZE INTO LK-REASON-TEXT
                 END-STRING
                 MOVE 12 TO RETURN-CODE
              END-IF
              MOVE "0" TO WS-OPEN-SW
           END-IF.
