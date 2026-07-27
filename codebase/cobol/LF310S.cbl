       IDENTIFICATION DIVISION.
       PROGRAM-ID. LF310S.
      * ================================================================
      * 変更履歴
      * 版数 | 年月日   | 担当   | 概要
      * ----+---------+--------+----------------------------------
      * 1.0 | 20200101 | 開発部 | 初版実装
      * 1.1 | 20220615 | 開発部 | 貸付状態検証強化
      * ================================================================
      *> 貸付控除額算定サブルーチン
      *> 返戻金支払時に控除すべき貸付元利金合計を算定する
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LFLOANF ASSIGN TO EXTERNAL WK-LOAN-FILE
               ORGANIZATION IS INDEXED
               RECORD KEY IS LN-POL-NO
               ACCESS MODE IS DYNAMIC
               FILE STATUS IS WK-LOAN-FILE-STATUS.
       
       DATA DIVISION.
       FILE SECTION.
       FD  LFLOANF.
       COPY LFLOANC.
       
       WORKING-STORAGE SECTION.
       01  WK-LOAN-FILE               PIC X(8) VALUE 'LFLOANF '.
       01  WK-LOAN-FILE-STATUS        PIC XX.
       01  WK-TOTAL-DEDUCT            PIC 9(13)V99 VALUE 0.
       01  WK-LOAN-COUNT              PIC 9(5) VALUE 0.
       01  WK-ACTIVE-COUNT            PIC 9(5) VALUE 0.
       01  WK-LOAN-STATUS-NUM         PIC 99.
       01  WK-ERROR-MSG               PIC X(60).
       
       LINKAGE SECTION.
       01  LS-POLICY-NO               PIC X(10).
       01  LS-DEDUCT-AMT              PIC 9(13)V99.
       01  LS-RC                      PIC S9(4) COMP.
       
       PROCEDURE DIVISION USING LS-POLICY-NO
                                LS-DEDUCT-AMT
                                LS-RC.
       
       MAIN-LOGIC.
           MOVE 0 TO RETURN-CODE.
           INITIALIZE WK-TOTAL-DEDUCT
                      WK-LOAN-COUNT
                      WK-ACTIVE-COUNT.
           
      *>   入力値チェック
           IF LS-POLICY-NO = SPACES OR LS-POLICY-NO = LOW-VALUES
               DISPLAY '【LF310S】契約者番号不正'
               MOVE 8 TO RETURN-CODE
               MOVE 8 TO LS-RC
               GOBACK
           END-IF.
           
      *>   ファイルOPEN
           OPEN INPUT LFLOANF.
           IF WK-LOAN-FILE-STATUS NOT = '00'
               STRING '【LF310S】貸付ファイル OPEN 失敗 ST='
                      DELIMITED BY SIZE
                      WK-LOAN-FILE-STATUS
                      DELIMITED BY SIZE
                      INTO WK-ERROR-MSG
               END-STRING
               DISPLAY WK-ERROR-MSG
               MOVE 12 TO RETURN-CODE
               MOVE 12 TO LS-RC
               GOBACK
           END-IF.
           
      *>   初期ポジショニング
           MOVE LS-POLICY-NO TO LN-POL-NO.
           
           START LFLOANF
               KEY IS >= LN-POL-NO
               INVALID KEY
                   MOVE '99' TO WK-LOAN-FILE-STATUS
               NOT INVALID KEY
                   PERFORM CALC-DEDUCTION-LOOP
           END-START.
           
      *>   ファイルCLOSE
           CLOSE LFLOANF.
           
      *>   出力パラメータに結果を設定
           MOVE WK-TOTAL-DEDUCT TO LS-DEDUCT-AMT.
           MOVE 0 TO RETURN-CODE.
           MOVE 0 TO LS-RC.
           GOBACK.
       
       CALC-DEDUCTION-LOOP.
      *>   同一契約者の貸付レコード読込・集計
           PERFORM UNTIL WK-LOAN-FILE-STATUS NOT = '00'
               READ LFLOANF
                   AT END
                       MOVE '99' TO WK-LOAN-FILE-STATUS
                   NOT AT END
                       IF LN-POL-NO NOT = LS-POLICY-NO
                           MOVE '99' TO WK-LOAN-FILE-STATUS
                       ELSE
                           ADD 1 TO WK-LOAN-COUNT
                           
      *>                   貸付状態検証
                           MOVE FUNCTION NUMVAL(LN-LOAN-STATUS-KBN)
                               TO WK-LOAN-STATUS-NUM
                           
                           EVALUATE WK-LOAN-STATUS-NUM
                               WHEN 01
      *>                           正常貸付
                                   ADD LN-LOAN-BAL-AMT TO WK-TOTAL-DEDUCT
                                   ADD LN-ACCRUED-INT-AMT TO 
                                       WK-TOTAL-DEDUCT
                                   ADD 1 TO WK-ACTIVE-COUNT
                               
                               WHEN 02
      *>                           延滞貸付も控除対象
                                   ADD LN-LOAN-BAL-AMT TO WK-TOTAL-DEDUCT
                                   ADD LN-ACCRUED-INT-AMT TO 
                                       WK-TOTAL-DEDUCT
                                   ADD 1 TO WK-ACTIVE-COUNT
                               
                               WHEN OTHER
      *>                           03=解除, 04=満期は対象外
                                   CONTINUE
                           END-EVALUATE
                       END-IF
               END-READ
           END-PERFORM.
       
       END PROGRAM LF310S.
