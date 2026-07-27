       IDENTIFICATION DIVISION.
       PROGRAM-ID. LF250S.
      * ======================================================
      * 版数: 001 / 日付: 20190615 / 担当: 契約経理IT / 作成
      * 目的: 契約状態判定サブルーチン
      *
      * 版数: 002 / 日付: 20210101 / 担当: 契約経理IT / 更新
      * 概要: 直近異動履歴の承認状況検査を追加
      * ======================================================
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CLASS 有効 IS '0'
           CLASS 停止 IS '1'
           CLASS 失効 IS '2'
           CLASS 解約 IS '3'
           CLASS 審査中 IS '4'.
       
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LFPOLF2-FILE ASSIGN TO WS-POL-DSNM
               ORGANIZATION IS INDEXED
               RECORD KEY IS PO-POL-NO
               FILE STATUS IS WS-LFPOLF2-STS.
           
           SELECT LVCHGF-FILE ASSIGN TO WS-CHG-DSNM
               ORGANIZATION IS INDEXED
               RECORD KEY IS CH-CHANGE-ID
               ALTERNATE RECORD KEY IS CH-POL-NO
               FILE STATUS IS WS-LVCHGF-STS.
       
       DATA DIVISION.
       FILE SECTION.
       FD  LFPOLF2-FILE.
       COPY LFPOLF2C.
       
       FD  LVCHGF-FILE.
       COPY LVCHGC.
       
       WORKING-STORAGE SECTION.
       01  WS-POL-DSNM            PIC X(50)
           VALUE 'LFPOLF2.VSAM'.
       01  WS-CHG-DSNM            PIC X(50)
           VALUE 'LVCHGF.VSAM'.
       01  WS-LFPOLF2-STS         PIC XX VALUE SPACES.
       01  WS-LVCHGF-STS          PIC XX VALUE SPACES.
       
       01  WS-WORK-AREA.
           05  WS-INPUT-POL-NO    PIC X(10).
           05  WS-CURR-STATUS     PIC 9.
           05  WS-LAST-CHG-ID     PIC X(20) VALUE SPACES.
           05  WS-LAST-CHG-TYPE   PIC 9.
           05  WS-LAST-CHG-STS    PIC 9.
           05  WS-CHG-FOUND       PIC 9 VALUE 0.
           05  WS-LOOP-LIMIT      PIC 999 VALUE 0.
           05  WS-LFPOLF2-OPEN    PIC 9 VALUE 0.
           05  WS-LVCHGF-OPEN     PIC 9 VALUE 0.
       
       LINKAGE SECTION.
       01  LS-POLICY-NUMBER       PIC X(10).
       01  LS-RESULT-CODES.
           05  LS-CANCEL-OK       PIC 9.
           05  LS-LEND-OK         PIC 9.
           05  LS-DIVID-OK        PIC 9.
       
       PROCEDURE DIVISION USING LS-POLICY-NUMBER LS-RESULT-CODES.
       
           MOVE 0 TO RETURN-CODE.
           MOVE LS-POLICY-NUMBER TO WS-INPUT-POL-NO.
           MOVE 0 TO LS-CANCEL-OK LS-LEND-OK LS-DIVID-OK.
           MOVE SPACES TO WS-LFPOLF2-STS WS-LVCHGF-STS.
           MOVE 0 TO WS-CHG-FOUND WS-LOOP-LIMIT.
           MOVE 0 TO WS-LFPOLF2-OPEN WS-LVCHGF-OPEN.
           
           OPEN INPUT LFPOLF2-FILE.
           IF WS-LFPOLF2-STS NOT = '00'
               MOVE 8 TO RETURN-CODE
               GO TO PARA-ABORT
           END-IF.
           MOVE 1 TO WS-LFPOLF2-OPEN.
           
           MOVE WS-INPUT-POL-NO TO PO-POL-NO.
           READ LFPOLF2-FILE
               AT END
                   MOVE 8 TO RETURN-CODE
                   GO TO PARA-ABORT
               NOT AT END
                   MOVE FUNCTION NUMVAL(PO-CONTRACT-STATUS-KBN)
                       TO WS-CURR-STATUS
           END-READ.
           
           CLOSE LFPOLF2-FILE.
           MOVE 0 TO WS-LFPOLF2-OPEN.
           
           EVALUATE WS-CURR-STATUS
               WHEN 0
                   MOVE 1 TO LS-CANCEL-OK
                   MOVE 1 TO LS-LEND-OK
                   MOVE 1 TO LS-DIVID-OK
               WHEN 1
                   MOVE 1 TO LS-CANCEL-OK
                   MOVE 0 TO LS-LEND-OK
                   MOVE 1 TO LS-DIVID-OK
               WHEN 2
                   MOVE 0 TO LS-CANCEL-OK
                   MOVE 0 TO LS-LEND-OK
                   MOVE 0 TO LS-DIVID-OK
               WHEN 3
                   MOVE 0 TO LS-CANCEL-OK
                   MOVE 0 TO LS-LEND-OK
                   MOVE 0 TO LS-DIVID-OK
               WHEN 4
                   MOVE 0 TO LS-CANCEL-OK
                   MOVE 0 TO LS-LEND-OK
                   MOVE 0 TO LS-DIVID-OK
               WHEN OTHER
                   MOVE 8 TO RETURN-CODE
                   GO TO PARA-ABORT
           END-EVALUATE.
           
           OPEN INPUT LVCHGF-FILE.
           IF WS-LVCHGF-STS NOT = '00'
               MOVE 8 TO RETURN-CODE
               GO TO PARA-ABORT
           END-IF.
           MOVE 1 TO WS-LVCHGF-OPEN.
           
           MOVE WS-INPUT-POL-NO TO CH-POL-NO.
           START LVCHGF-FILE
               KEY IS >= CH-POL-NO
               INVALID KEY
                   MOVE 1 TO WS-CHG-FOUND
               NOT INVALID KEY
                   PERFORM UNTIL WS-CHG-FOUND = 1
                       OR WS-LOOP-LIMIT >= 50
                       
                       READ LVCHGF-FILE
                           AT END
                               MOVE 1 TO WS-CHG-FOUND
                           NOT AT END
                               IF CH-POL-NO = WS-INPUT-POL-NO
                                   MOVE CH-CHANGE-ID 
                                       TO WS-LAST-CHG-ID
                                   MOVE FUNCTION NUMVAL(
                                       CH-CHANGE-TYPE-KBN)
                                       TO WS-LAST-CHG-TYPE
                                   MOVE FUNCTION NUMVAL(
                                       CH-CHANGE-STATUS-KBN)
                                       TO WS-LAST-CHG-STS
                                   MOVE 1 TO WS-CHG-FOUND
                               ELSE
                                   IF CH-POL-NO > WS-INPUT-POL-NO
                                       MOVE 1 TO WS-CHG-FOUND
                                   END-IF
                               END-IF
                       END-READ
                       
                       ADD 1 TO WS-LOOP-LIMIT
                   END-PERFORM
           END-START.
           
           CLOSE LVCHGF-FILE.
           MOVE 0 TO WS-LVCHGF-OPEN.
           
           IF WS-LAST-CHG-ID NOT = SPACES
               IF WS-LAST-CHG-STS NOT = 0
                   MOVE 0 TO LS-CANCEL-OK
                   MOVE 0 TO LS-LEND-OK
                   MOVE 0 TO LS-DIVID-OK
               END-IF
               
               IF WS-LAST-CHG-TYPE = 4
                   MOVE 0 TO LS-LEND-OK
               END-IF
           END-IF.
           
           MOVE 0 TO RETURN-CODE.
           GOBACK.
           
       PARA-ABORT.
           IF WS-LFPOLF2-OPEN = 1
               CLOSE LFPOLF2-FILE
               MOVE 0 TO WS-LFPOLF2-OPEN
           END-IF.
           IF WS-LVCHGF-OPEN = 1
               CLOSE LVCHGF-FILE
               MOVE 0 TO WS-LVCHGF-OPEN
           END-IF.
           GOBACK.
