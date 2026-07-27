       IDENTIFICATION DIVISION.
       PROGRAM-ID. JH390B.
      * 変更履歴
      * 版数  年月日(和暦)   担当                         概要
      * 1.00  令和04年04月01日 システム部 情報系チーム     新規作成
      * 1.01  令和05年10月16日 システム部 情報系チーム     抽出条件見直し
      * 1.02  令和06年07月22日 システム部 情報系チーム     DWH連携項目追加
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT JHCBALF ASSIGN TO "JHCBALF".
           SELECT JHSEGRF ASSIGN TO "JHSEGRF".
           SELECT JHCATTF ASSIGN TO "JHCATTF".
           SELECT JHDORMF ASSIGN TO "JHDORMF".
           SELECT JHHHGRPF ASSIGN TO "JHHHGRPF".
           SELECT JHREFMST ASSIGN TO "JHREFMST".
           SELECT JHCMPRF ASSIGN TO "JHCMPRF".
           SELECT JHXSELLF ASSIGN TO "JHXSELLF".
           SELECT JHBANDRF ASSIGN TO "JHBANDRF".
           SELECT JHSEGDST ASSIGN TO "JHSEGDST".
           SELECT JHDWHXTF ASSIGN TO "JHDWHXTF".
           SELECT JHMKEXPF ASSIGN TO "JHMKEXPF".
           SELECT JHDQERRF ASSIGN TO "JHDQERRF".
       DATA DIVISION.
       FILE SECTION.
       FD JHCBALF.
       COPY JHCBALFC.
       FD JHSEGRF.
       COPY JHSEGRFC.
       FD JHCATTF.
       COPY JHCATTC.
       FD JHDORMF.
       COPY JHDORMC.
       FD JHHHGRPF.
       COPY JHHHGRPC.
       FD JHREFMST.
       COPY JHREFMSTC.
       FD JHCMPRF.
       COPY JHCMPRC.
       FD JHXSELLF.
       COPY JHXSELLC.
       FD JHBANDRF.
       COPY JHBANDC.
       FD JHSEGDST.
       COPY JHSEGDC.
       FD JHDWHXTF.
       COPY JHDWHXTC.
       FD JHMKEXPF.
       COPY JHMKEXPC.
       FD JHDQERRF.
       COPY JHDQERRC.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID      PIC X(08).
       01  WS-EXTRACT-DATE    PIC 9(08).
       01  WS-CHECKPOINT-DATE PIC 9(08).
       01  WS-RERUN-FLAG      PIC X(01).
       01  WS-EOF-CB          PIC X(01).
       01  WS-EOF-SR          PIC X(01).
       01  WS-EOF-CP          PIC X(01).
       01  WS-EOF-XS          PIC X(01).
       01  WS-EOF-BD          PIC X(01).
       01  WS-EOF-SD          PIC X(01).
       01  WS-ABEND-FLAG      PIC X(01).
       01  WS-RETURN-CD       PIC S9(04) COMP.
       01  WS-FILE-STATUS     PIC X(02).
       01  WS-CURRENT-CUST-ID PIC X(12).
       01  WS-CURRENT-HOUSEHOLD-ID PIC X(12).
       01  WS-CURRENT-SEG-CD  PIC X(04).
       01  WS-CURRENT-SEG-NAME PIC X(20).
       01  WS-CURRENT-AVG-BAL PIC S9(11)V99 COMP-3.
       01  WS-CURRENT-DORMANT-FLAG PIC X(01).
       01  WS-CURRENT-DM-PERMIT-FLAG PIC X(01).
       01  WS-CURRENT-AGE-BAND-CD PIC X(03).
       01  WS-CURRENT-PREF-CD PIC X(02).
       01  WS-CURRENT-OCCUP-CD PIC X(04).
       01  WS-CURRENT-TRUST-REL-CD PIC X(02).
       01  WS-CURRENT-CAMPAIGN-ID PIC X(10).
       01  WS-CURRENT-XS-PRODUCT-CD PIC X(06).
       01  WS-CURRENT-XS-SCORE-RANK PIC X(02).
       01  WS-BALANCE-BAND-CD PIC X(04).
       01  WS-BASE-RATE       PIC S9(03)V9(06) COMP-3.
       01  WS-MARKETING-FEE   PIC S9(09)V99 COMP-3.
       01  WS-FLOOR-AMOUNT    PIC S9(09)V99 COMP-3.
       01  WS-CAP-AMOUNT      PIC S9(09)V99 COMP-3.
       01  WS-ADJUSTED-FEE    PIC S9(09)V99 COMP-3.
       01  WS-CB-READ-COUNT   PIC 9(09) COMP-3.
       01  WS-SR-READ-COUNT   PIC 9(09) COMP-3.
       01  WS-CA-READ-COUNT   PIC 9(09) COMP-3.
       01  WS-DM-READ-COUNT   PIC 9(09) COMP-3.
       01  WS-HG-READ-COUNT   PIC 9(09) COMP-3.
       01  WS-CP-READ-COUNT   PIC 9(09) COMP-3.
       01  WS-XS-READ-COUNT   PIC 9(09) COMP-3.
       01  WS-DX-WRITE-COUNT  PIC 9(09) COMP-3.
       01  WS-MX-WRITE-COUNT  PIC 9(09) COMP-3.
       01  WS-DQ-WRITE-COUNT  PIC 9(09) COMP-3.
       01  WS-EXCLUDE-ATTR-EXP-COUNT PIC 9(09) COMP-3.
       01  WS-EXCLUDE-DORMANT-COUNT PIC 9(09) COMP-3.
       01  WS-EXCLUDE-DM-NG-COUNT PIC 9(09) COMP-3.
       01  WS-ERR-SEG-MISS-COUNT PIC 9(09) COMP-3.
       01  WS-ERR-BAND-MISS-COUNT PIC 9(09) COMP-3.
       01  WS-ERR-ATTR-MISS-COUNT PIC 9(09) COMP-3.
       01  WS-ERR-HH-MISS-COUNT PIC 9(09) COMP-3.
       01  WS-ERR-BAL-MISMATCH-COUNT PIC 9(09) COMP-3.
       01  WS-BAND-CUSTOMER-COUNT PIC 9(09) COMP-3.
       01  WS-BAND-BALANCE-SUM PIC S9(13)V99 COMP-3.
       01  WS-SEG-CUSTOMER-COUNT PIC 9(09) COMP-3.
       01  WS-SEG-AVG-BAL-SUM PIC S9(13)V99 COMP-3.
       01  WS-SEG-AVG-BAL-MEAN PIC S9(11)V99 COMP-3.
       01  WS-DQ-SOURCE-DSID  PIC X(08).
       01  WS-DQ-ERROR-CD     PIC X(06).
       01  WS-DQ-ERROR-FIELD  PIC X(30).
       01  WS-DQ-ERROR-VALUE  PIC X(40).
       01  WS-DQ-SEVERITY-CD  PIC X(01).
       01  WS-JH322S-RET      PIC X(02).
       01  WS-JH352S-RET      PIC X(02).
       01  WS-JH372S-RET      PIC X(02).
       01  LK-SEG-BAL         PIC S9(11)V99 COMP-3.
       01  LK-SEG-CD          PIC X(04).
       01  LK-SEG-NAME        PIC X(20).
       01  LK-SEG-RET         PIC X(02).

       PROCEDURE DIVISION.
       010-INITIALIZE.
      *    作業域と処理状態を初期化する
           MOVE 'JH390B'           TO WS-PROGRAM-ID
           ACCEPT WS-EXTRACT-DATE  FROM DATE YYYYMMDD
           MOVE WS-EXTRACT-DATE    TO WS-CHECKPOINT-DATE
           MOVE 'N'                TO WS-RERUN-FLAG
           MOVE 'N'                TO WS-EOF-CB
           MOVE 'N'                TO WS-EOF-SR
           MOVE 'N'                TO WS-EOF-CP
           MOVE 'N'                TO WS-EOF-XS
           MOVE 'N'                TO WS-EOF-BD
           MOVE 'N'                TO WS-EOF-SD
           MOVE 'N'                TO WS-ABEND-FLAG
           MOVE ZERO               TO WS-RETURN-CD
           MOVE SPACES             TO WS-FILE-STATUS
      *    顧客単位の退避項目を初期化する
           MOVE SPACES             TO WS-CURRENT-CUST-ID
           MOVE SPACES             TO WS-CURRENT-HOUSEHOLD-ID
           MOVE SPACES             TO WS-CURRENT-SEG-CD
           MOVE SPACES             TO WS-CURRENT-SEG-NAME
           MOVE ZERO               TO WS-CURRENT-AVG-BAL
           MOVE SPACES             TO WS-CURRENT-DORMANT-FLAG
           MOVE SPACES             TO WS-CURRENT-DM-PERMIT-FLAG
           MOVE SPACES             TO WS-CURRENT-AGE-BAND-CD
           MOVE SPACES             TO WS-CURRENT-PREF-CD
           MOVE SPACES             TO WS-CURRENT-OCCUP-CD
           MOVE SPACES             TO WS-CURRENT-TRUST-REL-CD
           MOVE SPACES             TO WS-CURRENT-CAMPAIGN-ID
           MOVE SPACES             TO WS-CURRENT-XS-PRODUCT-CD
           MOVE SPACES             TO WS-CURRENT-XS-SCORE-RANK
           MOVE SPACES             TO WS-BALANCE-BAND-CD
      *    料率計算用の作業項目を初期化する
           MOVE ZERO               TO WS-BASE-RATE
           MOVE ZERO               TO WS-MARKETING-FEE
           MOVE ZERO               TO WS-FLOOR-AMOUNT
           MOVE ZERO               TO WS-CAP-AMOUNT
           MOVE ZERO               TO WS-ADJUSTED-FEE
      *    入出力件数を初期化する
           MOVE ZERO               TO WS-CB-READ-COUNT
           MOVE ZERO               TO WS-SR-READ-COUNT
           MOVE ZERO               TO WS-CA-READ-COUNT
           MOVE ZERO               TO WS-DM-READ-COUNT
           MOVE ZERO               TO WS-HG-READ-COUNT
           MOVE ZERO               TO WS-CP-READ-COUNT
           MOVE ZERO               TO WS-XS-READ-COUNT
           MOVE ZERO               TO WS-DX-WRITE-COUNT
           MOVE ZERO               TO WS-MX-WRITE-COUNT
           MOVE ZERO               TO WS-DQ-WRITE-COUNT
      *    除外理由別件数を初期化する
           MOVE ZERO               TO WS-EXCLUDE-ATTR-EXP-COUNT
           MOVE ZERO               TO WS-EXCLUDE-DORMANT-COUNT
           MOVE ZERO               TO WS-EXCLUDE-DM-NG-COUNT
           MOVE ZERO               TO WS-ERR-SEG-MISS-COUNT
           MOVE ZERO               TO WS-ERR-BAND-MISS-COUNT
           MOVE ZERO               TO WS-ERR-ATTR-MISS-COUNT
           MOVE ZERO               TO WS-ERR-HH-MISS-COUNT
           MOVE ZERO               TO WS-ERR-BAL-MISMATCH-COUNT
      *    集計用の作業項目を初期化する
           MOVE ZERO               TO WS-BAND-CUSTOMER-COUNT
           MOVE ZERO               TO WS-BAND-BALANCE-SUM
           MOVE ZERO               TO WS-SEG-CUSTOMER-COUNT
           MOVE ZERO               TO WS-SEG-AVG-BAL-SUM
           MOVE ZERO               TO WS-SEG-AVG-BAL-MEAN
      *    品質エラー出力用の作業項目を初期化する
           MOVE SPACES             TO WS-DQ-SOURCE-DSID
           MOVE SPACES             TO WS-DQ-ERROR-CD
           MOVE SPACES             TO WS-DQ-ERROR-FIELD
           MOVE SPACES             TO WS-DQ-ERROR-VALUE
           MOVE SPACES             TO WS-DQ-SEVERITY-CD
      *    外部サブルーチン戻り値と引数を初期化する
           MOVE SPACES             TO WS-JH322S-RET
           MOVE SPACES             TO WS-JH352S-RET
           MOVE SPACES             TO WS-JH372S-RET
           MOVE ZERO               TO LK-SEG-BAL
           MOVE SPACES             TO LK-SEG-CD
           MOVE SPACES             TO LK-SEG-NAME
           MOVE SPACES             TO LK-SEG-RET
           EXIT PARAGRAPH.
       020-OPEN-FILES.
      *    残高・セグメントなどの入力ファイルを開く
           OPEN INPUT JHCBALF
           IF WS-FILE-STATUS NOT = '00'
              MOVE 'Y'              TO WS-ABEND-FLAG
              MOVE 12               TO WS-RETURN-CD
              PERFORM 910-ERROR-HANDLING
              EXIT PARAGRAPH
           END-IF
           OPEN INPUT JHSEGRF
           IF WS-FILE-STATUS NOT = '00'
              MOVE 'Y'              TO WS-ABEND-FLAG
              MOVE 12               TO WS-RETURN-CD
              PERFORM 910-ERROR-HANDLING
              EXIT PARAGRAPH
           END-IF
      *    顧客属性・休眠・世帯・参照マスタを開く
           OPEN INPUT JHCATTF
           IF WS-FILE-STATUS NOT = '00'
              MOVE 'Y'              TO WS-ABEND-FLAG
              MOVE 12               TO WS-RETURN-CD
              PERFORM 910-ERROR-HANDLING
              EXIT PARAGRAPH
           END-IF
           OPEN INPUT JHDORMF
           IF WS-FILE-STATUS NOT = '00'
              MOVE 'Y'              TO WS-ABEND-FLAG
              MOVE 12               TO WS-RETURN-CD
              PERFORM 910-ERROR-HANDLING
              EXIT PARAGRAPH
           END-IF
           OPEN INPUT JHHHGRPF
           IF WS-FILE-STATUS NOT = '00'
              MOVE 'Y'              TO WS-ABEND-FLAG
              MOVE 12               TO WS-RETURN-CD
              PERFORM 910-ERROR-HANDLING
              EXIT PARAGRAPH
           END-IF
           OPEN INPUT JHREFMST
           IF WS-FILE-STATUS NOT = '00'
              MOVE 'Y'              TO WS-ABEND-FLAG
              MOVE 12               TO WS-RETURN-CD
              PERFORM 910-ERROR-HANDLING
              EXIT PARAGRAPH
           END-IF
      *    キャンペーン・商品候補の入力ファイルを開く
           OPEN INPUT JHCMPRF
           IF WS-FILE-STATUS NOT = '00'
              MOVE 'Y'              TO WS-ABEND-FLAG
              MOVE 12               TO WS-RETURN-CD
              PERFORM 910-ERROR-HANDLING
              EXIT PARAGRAPH
           END-IF
           OPEN INPUT JHXSELLF
           IF WS-FILE-STATUS NOT = '00'
              MOVE 'Y'              TO WS-ABEND-FLAG
              MOVE 12               TO WS-RETURN-CD
              PERFORM 910-ERROR-HANDLING
              EXIT PARAGRAPH
           END-IF
      *    集計・抽出・品質エラーの出力ファイルを開く
           OPEN OUTPUT JHBANDRF
           IF WS-FILE-STATUS NOT = '00'
              MOVE 'Y'              TO WS-ABEND-FLAG
              MOVE 12               TO WS-RETURN-CD
              PERFORM 910-ERROR-HANDLING
              EXIT PARAGRAPH
           END-IF
           OPEN OUTPUT JHSEGDST
           IF WS-FILE-STATUS NOT = '00'
              MOVE 'Y'              TO WS-ABEND-FLAG
              MOVE 12               TO WS-RETURN-CD
              PERFORM 910-ERROR-HANDLING
              EXIT PARAGRAPH
           END-IF
           OPEN OUTPUT JHDWHXTF
           IF WS-FILE-STATUS NOT = '00'
              MOVE 'Y'              TO WS-ABEND-FLAG
              MOVE 12               TO WS-RETURN-CD
              PERFORM 910-ERROR-HANDLING
              EXIT PARAGRAPH
           END-IF
           OPEN OUTPUT JHMKEXPF
           IF WS-FILE-STATUS NOT = '00'
              MOVE 'Y'              TO WS-ABEND-FLAG
              MOVE 12               TO WS-RETURN-CD
              PERFORM 910-ERROR-HANDLING
              EXIT PARAGRAPH
           END-IF
           OPEN OUTPUT JHDQERRF
           IF WS-FILE-STATUS NOT = '00'
              MOVE 'Y'              TO WS-ABEND-FLAG
              MOVE 12               TO WS-RETURN-CD
              PERFORM 910-ERROR-HANDLING
              EXIT PARAGRAPH
            END-IF
            .
       910-ERROR-HANDLING.
           EXIT PARAGRAPH.
