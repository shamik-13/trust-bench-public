       IDENTIFICATION DIVISION.
       PROGRAM-ID. JH372S.
      * 変更履歴
      * 版数  年月日(和暦)   担当                    概要
      * 1.00  令和05.04.01  システム部 情報系チーム  新規作成
      * 1.01  令和05.10.16  システム部 情報系チーム  スコア項目追加
      * 1.02  令和06.03.18  システム部 情報系チーム  判定条件見直し
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT JHREFMSTF ASSIGN TO "JHREFMSTF"
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS IS WS-JHREFMSTF-ST.
       DATA DIVISION.
       FILE SECTION.
       FD  JHREFMSTF.
       COPY JHREFMSTC.
       WORKING-STORAGE SECTION.
       01  WS-JHREFMSTF-ST      PIC XX VALUE SPACES.
       01  WS-EOF-SW            PIC X VALUE "N".
           88  WS-EOF                 VALUE "Y".
           88  WS-NOT-EOF             VALUE "N".
       01  WS-READ-COUNT        PIC 9(09) VALUE ZERO.
       PROCEDURE DIVISION.
       MAIN-PARA.
           OPEN INPUT JHREFMSTF
           IF WS-JHREFMSTF-ST NOT = "00"
              DISPLAY "JHREFMSTF OPEN STATUS " WS-JHREFMSTF-ST
              GOBACK
           END-IF

           PERFORM UNTIL WS-EOF
              READ JHREFMSTF
                 AT END
                    SET WS-EOF TO TRUE
                 NOT AT END
                    ADD 1 TO WS-READ-COUNT
              END-READ
           END-PERFORM

           CLOSE JHREFMSTF
           DISPLAY "JHREFMSTF RECORDS " WS-READ-COUNT
           GOBACK.
