       IDENTIFICATION DIVISION.
       PROGRAM-ID. CG210S.
       AUTHOR.     MFG-KYOTSU-KIBAN.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CGCODF
               ASSIGN       TO "CGCODF"
               ORGANIZATION IS INDEXED
               ACCESS MODE  IS DYNAMIC
               RECORD KEY   IS GC-CODE-ID
               FILE STATUS  IS CGCODF-STS.

       DATA DIVISION.
       FILE SECTION.
       FD  CGCODF.
       COPY CGCODC.

       WORKING-STORAGE SECTION.
       01  CGCODF-STS                 PIC XX VALUE SPACE.
       01  WS-ABEND-SW                PIC X  VALUE SPACE.
           88  WS-ABEND                    VALUE "1".
       01  WS-INPUT-ERR-SW            PIC X  VALUE SPACE.
           88  WS-INPUT-ERR                VALUE "1".
       01  WS-DATE-CHECK.
           05  WS-BASE-DATE-N         PIC 9(8) VALUE ZERO.
           05  WS-FROM-DATE-N         PIC 9(8) VALUE ZERO.
           05  WS-TO-DATE-N           PIC 9(8) VALUE ZERO.

      * 標準ステータス
      * 00:利用可能 10:未登録 20:期限切れ 30:将来適用
      * 90:入力不正 99:ファイル障害
      * RETURN-CODE 0:正常終了または業務判定あり
      * RETURN-CODE 8:入出力障害等の異常終了

       LINKAGE SECTION.
       01  LK-CG210S-PARM.
           05  LK-CODE-KBN            PIC X(02).
           05  LK-CODE-VALUE          PIC X(20).
           05  LK-BASE-DT             PIC 9(08).
           05  LK-RESULT-STS          PIC X(02).
           05  LK-REASON-CD           PIC X(04).
           05  LK-REASON-TEXT         PIC X(40).

       PROCEDURE DIVISION USING LK-CG210S-PARM.
       0000-MAIN.
           MOVE 0 TO RETURN-CODE
           MOVE SPACE TO WS-ABEND-SW
           MOVE "99" TO LK-RESULT-STS
           MOVE "INIT" TO LK-REASON-CD
           MOVE SPACE TO LK-REASON-TEXT

           PERFORM 1000-OPEN
           IF NOT WS-ABEND
               PERFORM 2000-CHECK-INPUT
           END-IF
           IF NOT WS-ABEND
               IF WS-INPUT-ERR
                   MOVE "90" TO LK-RESULT-STS
                   MOVE "INP1" TO LK-REASON-CD
                   MOVE "入力項目不正" TO LK-REASON-TEXT
               ELSE
                   PERFORM 3000-READ-CODE
               END-IF
           END-IF
           PERFORM 9000-CLOSE

           IF WS-ABEND
               MOVE 8 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF

           GOBACK.

       1000-OPEN.
           OPEN INPUT CGCODF
           IF CGCODF-STS NOT = "00"
               DISPLAY "CGCODF オープン失敗 ST=" CGCODF-STS
               MOVE "99" TO LK-RESULT-STS
               MOVE "FIL1" TO LK-REASON-CD
               MOVE "コード表オープン失敗" TO LK-REASON-TEXT
               MOVE "1" TO WS-ABEND-SW
           END-IF.

       2000-CHECK-INPUT.
           MOVE SPACE TO WS-INPUT-ERR-SW

           IF LK-CODE-KBN = SPACE
               MOVE "1" TO WS-INPUT-ERR-SW
           END-IF

           IF LK-CODE-VALUE = SPACE
               MOVE "1" TO WS-INPUT-ERR-SW
           END-IF

           IF LK-BASE-DT = ZERO
               MOVE "1" TO WS-INPUT-ERR-SW
           END-IF

           IF NOT WS-INPUT-ERR
               MOVE LK-BASE-DT TO WS-BASE-DATE-N
               IF WS-BASE-DATE-N < 19000101
                   MOVE "1" TO WS-INPUT-ERR-SW
               END-IF
               IF WS-BASE-DATE-N > 20991231
                   MOVE "1" TO WS-INPUT-ERR-SW
               END-IF
           END-IF.

       3000-READ-CODE.
           MOVE SPACE         TO CGCODF-REC
           MOVE LK-CODE-KBN   TO GC-CODE-KBN
           MOVE LK-CODE-VALUE TO GC-CODE-VALUE

           READ CGCODF
               KEY IS GC-CODE-ID
               INVALID KEY
                   IF CGCODF-STS = "23"
                       MOVE "10" TO LK-RESULT-STS
                       MOVE "NREG" TO LK-REASON-CD
                       MOVE "コード未登録" TO LK-REASON-TEXT
                   ELSE
                       DISPLAY "CGCODF 読込失敗 ST=" CGCODF-STS
                       MOVE "99" TO LK-RESULT-STS
                       MOVE "FIL2" TO LK-REASON-CD
                       MOVE "コード表読込失敗" TO LK-REASON-TEXT
                       MOVE "1" TO WS-ABEND-SW
                   END-IF
               NOT INVALID KEY
                   PERFORM 3100-JUDGE-VALID
           END-READ.

       3100-JUDGE-VALID.
           MOVE GC-VALID-FROM-DT TO WS-FROM-DATE-N
           MOVE GC-VALID-TO-DT   TO WS-TO-DATE-N

           IF WS-BASE-DATE-N < WS-FROM-DATE-N
               MOVE "30" TO LK-RESULT-STS
               MOVE "FUTR" TO LK-REASON-CD
               MOVE "適用開始日前" TO LK-REASON-TEXT
           ELSE
               IF WS-TO-DATE-N NOT = ZERO
                   IF WS-BASE-DATE-N > WS-TO-DATE-N
                       MOVE "20" TO LK-RESULT-STS
                       MOVE "EXPD" TO LK-REASON-CD
                       MOVE "有効期限切れ" TO LK-REASON-TEXT
                   ELSE
                       MOVE "00" TO LK-RESULT-STS
                       MOVE "OK  " TO LK-REASON-CD
                       MOVE "利用可能" TO LK-REASON-TEXT
                   END-IF
               ELSE
                   MOVE "00" TO LK-RESULT-STS
                   MOVE "OK  " TO LK-REASON-CD
                   MOVE "利用可能" TO LK-REASON-TEXT
               END-IF
           END-IF.

       9000-CLOSE.
           CLOSE CGCODF
           IF CGCODF-STS NOT = "00"
               DISPLAY "CGCODF クローズ失敗 ST=" CGCODF-STS
               MOVE "99" TO LK-RESULT-STS
               MOVE "FIL3" TO LK-REASON-CD
               MOVE "コード表クローズ失敗" TO LK-REASON-TEXT
               MOVE "1" TO WS-ABEND-SW
           END-IF.
