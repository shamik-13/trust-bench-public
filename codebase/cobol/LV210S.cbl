       IDENTIFICATION DIVISION.
       PROGRAM-ID. LV210S.
      *
      * 解約受付可否判定サブルーチン
      * 契約番号、受付日、契約状態、未完了異動の有無から
      * 解約受付可否を判定し、受付エラー区分と理由コードを返す。
      *
      * 版数    日付        担当      概要
      * 1.0     20220308    DV-TK     初版作成
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LFPOLF2 ASSIGN TO "LFPOLF2"
               ORGANIZATION IS INDEXED
               RECORD KEY IS PO-POL-NO
               FILE STATUS IS LS-LFPOLF2-ST.
           
           SELECT LVCHGF ASSIGN TO "LVCHGF"
               ORGANIZATION IS INDEXED
               RECORD KEY IS CH-CHANGE-ID
               FILE STATUS IS LS-LVCHGF-ST.
       
       DATA DIVISION.
       FILE SECTION.
       FD  LFPOLF2.
       COPY LFPOLF2C.
       
       FD  LVCHGF.
       COPY LVCHGC.
       
       WORKING-STORAGE SECTION.
       01  LS-LFPOLF2-ST              PIC XX VALUE SPACES.
       01  LS-LVCHGF-ST               PIC XX VALUE SPACES.
       
       01  WS-ERROR-CLASSIFICATION    PIC 9 VALUE 0.
       01  WS-REASON-CODE             PIC 999 VALUE 0.
       
       01  WS-CONTRACT-FOUND-SW       PIC X VALUE 'N'.
           88 CONTRACT-FOUND          VALUE 'Y'.
       01  WS-INCOMPLETE-CHANGE-SW    PIC X VALUE 'N'.
           88 HAS-INCOMPLETE-CHANGE   VALUE 'Y'.
       01  WS-EOF-CHANGE-SW           PIC X VALUE 'N'.
           88 END-OF-CHANGE-FILE      VALUE 'Y'.
       
       01  WS-SEARCH-POL-NUMBER       PIC 9(10) VALUE 0.
       01  WS-MSG-TEXT                PIC X(30) VALUE SPACES.
       
       LINKAGE SECTION.
       01  LS-CONTRACT-NUMBER         PIC 9(10).
       01  LS-RECEPTION-DATE          PIC 9(8).
       01  LS-ERROR-CLASS             PIC 9.
       01  LS-REASON-CODE             PIC 999.
       
       PROCEDURE DIVISION USING 
           LS-CONTRACT-NUMBER
           LS-RECEPTION-DATE
           LS-ERROR-CLASS
           LS-REASON-CODE.
       
       MAIN-PROCEDURE.
           PERFORM INITIALIZE-VARIABLES.
           
           PERFORM READ-POLICY-RECORD.
           
           IF NOT CONTRACT-FOUND
               MOVE 2 TO WS-ERROR-CLASSIFICATION
               MOVE 201 TO WS-REASON-CODE
               GO TO MAIN-FINALIZE
           END-IF.
           
           EVALUATE PO-CONTRACT-STATUS-KBN
               WHEN "1"
               WHEN "2"
                   PERFORM CHECK-INCOMPLETE-CHANGES
               WHEN OTHER
                   MOVE 2 TO WS-ERROR-CLASSIFICATION
                   MOVE 202 TO WS-REASON-CODE
                   GO TO MAIN-FINALIZE
           END-EVALUATE.
           
           IF HAS-INCOMPLETE-CHANGE
               MOVE 1 TO WS-ERROR-CLASSIFICATION
               MOVE 101 TO WS-REASON-CODE
           ELSE
               MOVE 0 TO WS-ERROR-CLASSIFICATION
               MOVE 0 TO WS-REASON-CODE
           END-IF.
       
       MAIN-FINALIZE.
           MOVE WS-ERROR-CLASSIFICATION TO LS-ERROR-CLASS.
           MOVE WS-REASON-CODE TO LS-REASON-CODE.
           
           MOVE 0 TO RETURN-CODE.
           GOBACK.
       
       INITIALIZE-VARIABLES.
           MOVE 0 TO WS-ERROR-CLASSIFICATION.
           MOVE 0 TO WS-REASON-CODE.
           MOVE 'N' TO WS-CONTRACT-FOUND-SW.
           MOVE 'N' TO WS-INCOMPLETE-CHANGE-SW.
           MOVE 'N' TO WS-EOF-CHANGE-SW.
           MOVE LS-CONTRACT-NUMBER TO WS-SEARCH-POL-NUMBER.
       
       READ-POLICY-RECORD.
           OPEN INPUT LFPOLF2.
           
           IF LS-LFPOLF2-ST NOT = "00"
               MOVE 3 TO WS-ERROR-CLASSIFICATION
               MOVE 301 TO WS-REASON-CODE
               CLOSE LFPOLF2
               GO TO MAIN-FINALIZE
           END-IF.
           
           MOVE WS-SEARCH-POL-NUMBER TO PO-POL-NO.
           READ LFPOLF2 KEY IS PO-POL-NO.
           
           EVALUATE LS-LFPOLF2-ST
               WHEN "00"
                   MOVE 'Y' TO WS-CONTRACT-FOUND-SW
               WHEN "23"
                   MOVE 'N' TO WS-CONTRACT-FOUND-SW
               WHEN OTHER
                   MOVE 'N' TO WS-CONTRACT-FOUND-SW
           END-EVALUATE.
           
           CLOSE LFPOLF2.
       
       CHECK-INCOMPLETE-CHANGES.
           OPEN INPUT LVCHGF.
           
           IF LS-LVCHGF-ST NOT = "00"
               MOVE 3 TO WS-ERROR-CLASSIFICATION
               MOVE 302 TO WS-REASON-CODE
               CLOSE LVCHGF
               GO TO MAIN-FINALIZE
           END-IF.
           
           MOVE 'N' TO WS-EOF-CHANGE-SW.
           PERFORM UNTIL END-OF-CHANGE-FILE
               READ LVCHGF NEXT RECORD
                   AT END
                       MOVE 'Y' TO WS-EOF-CHANGE-SW
                   NOT AT END
                       IF CH-POL-NO = WS-SEARCH-POL-NUMBER
                           IF CH-CHANGE-STATUS-KBN = "0"
                               MOVE 'Y' 
                                   TO WS-INCOMPLETE-CHANGE-SW
                               MOVE 'Y' TO WS-EOF-CHANGE-SW
                           END-IF
                       END-IF
               END-READ
           END-PERFORM.
           
           CLOSE LVCHGF.
