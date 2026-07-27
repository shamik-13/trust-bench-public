       IDENTIFICATION DIVISION.
       PROGRAM-ID. KZ150B.
       AUTHOR. SYSTEM-BU.
      *---------------------------------------------------------------*
      * 与信限度見直しバッチ
      * 変更履歴
      * 版数  年月日        担当                   概要
      * 1.00  平成30年04月  システム部 勘定系チーム 新規作成
      * 1.01  令和02年09月  システム部 勘定系チーム 残高基準改定
      * 1.02  令和05年04月  システム部 勘定系チーム 算定方式見直し
      *---------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KZACCTF ASSIGN TO "KZACCTF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS AC-ACCT-NO
               FILE STATUS IS WS-ACCT-ST.
           SELECT KZCUSTF ASSIGN TO "KZCUSTF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CU-CUST-ID
               FILE STATUS IS WS-CUST-ST.
           SELECT KZCRLF ASSIGN TO "KZCRLF"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-CRL-ST.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  KZACCTF.
           COPY KZACCTC4.
      *
       FD  KZCUSTF.
           COPY KZCUSTC.
      *
       FD  KZCRLF.
           COPY KZCRLFC.
      *
       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05 WS-ACCT-ST              PIC X(02) VALUE SPACE.
           05 WS-CUST-ST              PIC X(02) VALUE SPACE.
           05 WS-CRL-ST               PIC X(02) VALUE SPACE.
      *
       01  WS-END-FLAGS.
           05 WS-ACCT-EOF             PIC X(01) VALUE "N".
              88 ACCT-EOF                       VALUE "Y".
              88 ACCT-NOT-EOF                   VALUE "N".
      *
       01  WS-CONSTANTS.
           05 WS-AVG-BAL-THRESHOLD    PIC 9(09) VALUE 5000000.
           05 WS-RAISED-LIMIT         PIC 9(09) VALUE 3000000.
           05 WS-KYC-UNKNOWN          PIC X(01) VALUE "9".
      *
       01  WS-COUNTERS.
           05 WS-READ-ACCT-CNT        PIC 9(09) VALUE ZERO.
           05 WS-READ-CUST-CNT        PIC 9(09) VALUE ZERO.
           05 WS-WRITE-CRL-CNT        PIC 9(09) VALUE ZERO.
           05 WS-RAISE-CNT            PIC 9(09) VALUE ZERO.
           05 WS-HOLD-BAL-CNT         PIC 9(09) VALUE ZERO.
           05 WS-CUST-NF-CNT          PIC 9(09) VALUE ZERO.
      *
       01  WS-WORK.
           05 WS-NEW-LIMIT            PIC 9(09) VALUE ZERO.
           05 WS-RAISE-FLAG           PIC X(01) VALUE "N".
           05 WS-KYC-STATUS           PIC X(01) VALUE SPACE.
           05 WS-REASON               PIC X(04) VALUE SPACE.
      *
       PROCEDURE DIVISION.
       0000-MAIN SECTION.
      *
           MOVE 8 TO RETURN-CODE
           PERFORM 1000-OPEN-FILES
           PERFORM 2000-PROCESS-ACCOUNTS
           PERFORM 9000-CLOSE-FILES
           MOVE 0 TO RETURN-CODE
           DISPLAY "KZ150B 正常終了"
           DISPLAY "口座読込件数=" WS-READ-ACCT-CNT
           DISPLAY "顧客読込件数=" WS-READ-CUST-CNT
           DISPLAY "結果出力件数=" WS-WRITE-CRL-CNT
           DISPLAY "引上件数=" WS-RAISE-CNT
           DISPLAY "残高未満据置件数=" WS-HOLD-BAL-CNT
           DISPLAY "顧客不在件数=" WS-CUST-NF-CNT
           GOBACK
           .
      *
       1000-OPEN-FILES SECTION.
      *
           OPEN INPUT KZACCTF
           IF WS-ACCT-ST NOT = "00"
               DISPLAY "KZACCTF オープン失敗 ST=" WS-ACCT-ST
               PERFORM 9900-ABEND
           END-IF
      *
           OPEN INPUT KZCUSTF
           IF WS-CUST-ST NOT = "00"
               DISPLAY "KZCUSTF オープン失敗 ST=" WS-CUST-ST
               PERFORM 9900-ABEND
           END-IF
      *
           OPEN OUTPUT KZCRLF
           IF WS-CRL-ST NOT = "00"
               DISPLAY "KZCRLF オープン失敗 ST=" WS-CRL-ST
               PERFORM 9900-ABEND
           END-IF
           .
      *
       2000-PROCESS-ACCOUNTS SECTION.
      *
           SET ACCT-NOT-EOF TO TRUE
           PERFORM UNTIL ACCT-EOF
               PERFORM 2100-READ-ACCOUNT
               IF ACCT-NOT-EOF
                   ADD 1 TO WS-READ-ACCT-CNT
                   PERFORM 3000-LOOKUP-CUSTOMER
                   PERFORM 4000-DECIDE-LIMIT
                   PERFORM 5000-WRITE-RESULT
               END-IF
           END-PERFORM
           .
      *
       2100-READ-ACCOUNT SECTION.
      *
           READ KZACCTF NEXT RECORD
               AT END
                   SET ACCT-EOF TO TRUE
               NOT AT END
                   CONTINUE
           END-READ
      *
           IF WS-ACCT-ST NOT = "00" AND WS-ACCT-ST NOT = "10"
               DISPLAY "KZACCTF 読込失敗 ST=" WS-ACCT-ST
               PERFORM 9900-ABEND
           END-IF
           .
      *
       3000-LOOKUP-CUSTOMER SECTION.
      *
           MOVE AC-CUST-ID TO CU-CUST-ID
           READ KZCUSTF KEY IS CU-CUST-ID
               INVALID KEY
                   MOVE WS-KYC-UNKNOWN TO WS-KYC-STATUS
                   ADD 1 TO WS-CUST-NF-CNT
                   DISPLAY "KZCUSTF 顧客不在"
                   DISPLAY "顧客番号=" AC-CUST-ID
               NOT INVALID KEY
                   MOVE CU-KYC-STATUS TO WS-KYC-STATUS
                   ADD 1 TO WS-READ-CUST-CNT
           END-READ
      *
           IF WS-CUST-ST NOT = "00" AND WS-CUST-ST NOT = "23"
               DISPLAY "KZCUSTF 読込失敗 ST=" WS-CUST-ST
               PERFORM 9900-ABEND
           END-IF
           .
      *
       4000-DECIDE-LIMIT SECTION.
      *
           MOVE AC-CREDIT-LIMIT TO WS-NEW-LIMIT
           MOVE "N"             TO WS-RAISE-FLAG
           MOVE "HBAL"          TO WS-REASON
      *
           IF AC-AVG-BAL >= WS-AVG-BAL-THRESHOLD
               MOVE WS-RAISED-LIMIT TO WS-NEW-LIMIT
               MOVE "Y"             TO WS-RAISE-FLAG
               MOVE "RAIS"          TO WS-REASON
               ADD 1 TO WS-RAISE-CNT
           ELSE
               ADD 1 TO WS-HOLD-BAL-CNT
           END-IF
           .
      *
       5000-WRITE-RESULT SECTION.
      *
           INITIALIZE KZCRLF-REC
           MOVE AC-ACCT-NO       TO CR-ACCT-NO
           MOVE AC-CREDIT-LIMIT  TO CR-OLD-LIMIT
           MOVE WS-NEW-LIMIT     TO CR-NEW-LIMIT
           MOVE WS-RAISE-FLAG    TO CR-RAISE-FLAG
           MOVE WS-KYC-STATUS    TO CR-KYC-STATUS
           MOVE WS-REASON        TO CR-REASON
      *
           WRITE KZCRLF-REC
           IF WS-CRL-ST NOT = "00"
               DISPLAY "KZCRLF 書込失敗 ST=" WS-CRL-ST
               DISPLAY "口座番号=" AC-ACCT-NO
               PERFORM 9900-ABEND
           END-IF
           ADD 1 TO WS-WRITE-CRL-CNT
           .
      *
       9000-CLOSE-FILES SECTION.
      *
           CLOSE KZACCTF
           IF WS-ACCT-ST NOT = "00"
               DISPLAY "KZACCTF クローズ失敗 ST=" WS-ACCT-ST
               PERFORM 9900-ABEND
           END-IF
      *
           CLOSE KZCUSTF
           IF WS-CUST-ST NOT = "00"
               DISPLAY "KZCUSTF クローズ失敗 ST=" WS-CUST-ST
               PERFORM 9900-ABEND
           END-IF
      *
           CLOSE KZCRLF
           IF WS-CRL-ST NOT = "00"
               DISPLAY "KZCRLF クローズ失敗 ST=" WS-CRL-ST
               PERFORM 9900-ABEND
           END-IF
           .
      *
       9900-ABEND SECTION.
      *
           MOVE 12 TO RETURN-CODE
           DISPLAY "KZ150B 異常終了"
           DISPLAY "口座読込件数=" WS-READ-ACCT-CNT
           DISPLAY "結果出力件数=" WS-WRITE-CRL-CNT
           GOBACK
           .
