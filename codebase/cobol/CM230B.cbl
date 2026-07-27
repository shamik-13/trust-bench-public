       IDENTIFICATION DIVISION.
       PROGRAM-ID. CM230B.
      *
      *===============================================================*
      *  変更履歴                                                     *
      *  版数  年月日    担当  概要                                  *
      *  1.00  20240430  CM01  初版作成                              *
      *  1.10  20240930  CM02  重複候補棚卸判定追加                  *
      *  1.20  20250331  CM03  統合キー状態不整合出力追加            *
      *===============================================================*
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CMCIFF ASSIGN TO "CMCIFF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-CMCIFF.
           SELECT CMATTF ASSIGN TO "CMATTF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS CA-CIF-NO
               FILE STATUS IS FS-CMATTF.
           SELECT CMKEYF ASSIGN TO "CMKEYF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-CMKEYF.
           SELECT CMDUPF ASSIGN TO "CMDUPF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS DP-CANDIDATE-ID
               FILE STATUS IS FS-CMDUPF.
           SELECT CKERRF ASSIGN TO "CKERRF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-CKERRF.
           SELECT CMRSLF ASSIGN TO "CMRSLF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-CMRSLF.
       DATA DIVISION.
       FILE SECTION.
       FD  CMCIFF.
           COPY CMCIFFC.
       FD  CMATTF.
           COPY CMATTC.
       FD  CMKEYF.
           COPY CMKEYFC.
       FD  CMDUPF.
           COPY CMDUPC.
       FD  CKERRF.
           COPY CKERRC.
       FD  CMRSLF.
           COPY CMRSLC.
       WORKING-STORAGE SECTION.
       01  FS-AREA.
           05 FS-CMCIFF              PIC XX VALUE SPACE.
           05 FS-CMATTF              PIC XX VALUE SPACE.
           05 FS-CMKEYF              PIC XX VALUE SPACE.
           05 FS-CMDUPF              PIC XX VALUE SPACE.
           05 FS-CKERRF              PIC XX VALUE SPACE.
           05 FS-CMRSLF              PIC XX VALUE SPACE.
       01  SW-AREA.
           05 SW-CIF-EOF             PIC X VALUE "N".
           05 SW-ATT-EOF             PIC X VALUE "N".
           05 SW-KEY-EOF             PIC X VALUE "N".
           05 SW-DUP-EOF             PIC X VALUE "N".
           05 SW-ABEND               PIC X VALUE "N".
       01  CONST-AREA.
           05 CN-PGM-ID              PIC X(08) VALUE "CM230B".
           05 CN-STS-ACT             PIC XX VALUE "01".
           05 CN-STS-EXC             PIC XX VALUE "08".
           05 CN-STS-INV             PIC XX VALUE "09".
           05 CN-KEY-ACT             PIC XX VALUE "01".
           05 CN-KEY-STOP            PIC XX VALUE "08".
           05 CN-KEY-DEL             PIC XX VALUE "09".
           05 CN-DUP-MIK             PIC X  VALUE "0".
           05 CN-DUP-HORYU           PIC X  VALUE "1".
           05 CN-DUP-KAK             PIC X  VALUE "9".
       01  DATE-AREA.
           05 WS-CUR-DATE            PIC 9(08).
           05 WS-CUR-TIME            PIC 9(08).
       01  COUNTER-AREA.
           05 CNT-CIF-IN             PIC 9(09) VALUE ZERO.
           05 CNT-ATT-IN             PIC 9(09) VALUE ZERO.
           05 CNT-KEY-IN             PIC 9(09) VALUE ZERO.
           05 CNT-DUP-IN             PIC 9(09) VALUE ZERO.
           05 CNT-RSL-OUT            PIC 9(09) VALUE ZERO.
           05 CNT-ERR-OUT            PIC 9(09) VALUE ZERO.
           05 CNT-RS-ID              PIC 9(12) VALUE ZERO.
           05 CNT-ER-ID              PIC 9(12) VALUE ZERO.
       01  WK-AREA.
           05 WK-IDX                 PIC 9(05) COMP VALUE ZERO.
           05 WK-FOUND-IDX           PIC 9(05) COMP VALUE ZERO.
           05 WK-ATT-FOUND           PIC X VALUE "N".
           05 WK-KEY-FOUND           PIC X VALUE "N".
           05 WK-DUP-FOUND           PIC X VALUE "N".
           05 WK-BAD-FOUND           PIC X VALUE "N".
           05 WK-CIF-EDIT            PIC X(20) VALUE SPACE.
           05 WK-KEY-EDIT            PIC X(30) VALUE SPACE.
           05 WK-REASON              PIC X(04) VALUE SPACE.
           05 WK-RESULT              PIC X(02) VALUE SPACE.
       01  ATT-TABLE.
           05 ATT-CNT                PIC 9(05) COMP VALUE ZERO.
           05 ATT-ENT OCCURS 20000 TIMES.
              10 T-CA-CIF-NO         PIC X(20).
              10 T-CA-KANJI-NAME     PIC X(80).
              10 T-CA-KANA-NAME      PIC X(80).
              10 T-CA-ADDR-CD        PIC X(20).
              10 T-CA-PHONE-NO       PIC X(20).
              10 T-CA-UPDATE-DT      PIC 9(08).
              10 T-CA-STATUS         PIC XX.
       01  KEY-TABLE.
           05 KEY-CNT                PIC 9(05) COMP VALUE ZERO.
           05 KEY-ENT OCCURS 20000 TIMES.
              10 T-CK-KEY-ID         PIC X(30).
              10 T-CK-CIF-NO         PIC X(20).
              10 T-CK-DIGIT-CNT      PIC 9(04).
              10 T-CK-STATUS         PIC XX.
       01  DUP-TABLE.
           05 DUP-CNT                PIC 9(05) COMP VALUE ZERO.
           05 DUP-ENT OCCURS 20000 TIMES.
              10 T-DP-CAND-ID        PIC X(30).
              10 T-DP-CIF-NO-1       PIC X(20).
              10 T-DP-CIF-NO-2       PIC X(20).
              10 T-DP-SCORE          PIC 9(03).
              10 T-DP-JUDGE          PIC X.
              10 T-DP-CREATE-DT      PIC 9(08).
       PROCEDURE DIVISION.
       MAIN-RTN.
           MOVE 0 TO RETURN-CODE
           PERFORM 0100-INIT
           IF SW-ABEND = "N"
              PERFORM 0200-LOAD-ATT
              PERFORM 0300-LOAD-KEY
              PERFORM 0400-LOAD-DUP
              PERFORM 1000-PROCESS-CIF
           END-IF
           PERFORM 9000-END
           GOBACK.
      *
       0100-INIT.
           MOVE FUNCTION CURRENT-DATE(1:8) TO WS-CUR-DATE
           MOVE FUNCTION CURRENT-DATE(9:8) TO WS-CUR-TIME
           OPEN INPUT CMCIFF CMATTF CMKEYF CMDUPF
                OUTPUT CKERRF CMRSLF
           IF FS-CMCIFF NOT = "00"
              DISPLAY "CMCIFF オープン失敗 ST=" FS-CMCIFF
              MOVE "Y" TO SW-ABEND
           END-IF
           IF FS-CMATTF NOT = "00"
              DISPLAY "CMATTF オープン失敗 ST=" FS-CMATTF
              MOVE "Y" TO SW-ABEND
           END-IF
           IF FS-CMKEYF NOT = "00"
              DISPLAY "CMKEYF オープン失敗 ST=" FS-CMKEYF
              MOVE "Y" TO SW-ABEND
           END-IF
           IF FS-CMDUPF NOT = "00"
              DISPLAY "CMDUPF オープン失敗 ST=" FS-CMDUPF
              MOVE "Y" TO SW-ABEND
           END-IF
           IF FS-CKERRF NOT = "00"
              DISPLAY "CKERRF オープン失敗 ST=" FS-CKERRF
              MOVE "Y" TO SW-ABEND
           END-IF
           IF FS-CMRSLF NOT = "00"
              DISPLAY "CMRSLF オープン失敗 ST=" FS-CMRSLF
              MOVE "Y" TO SW-ABEND
           END-IF
           IF SW-ABEND = "Y"
              MOVE 12 TO RETURN-CODE
           END-IF.
      *
       0200-LOAD-ATT.
           PERFORM UNTIL SW-ATT-EOF = "Y" OR SW-ABEND = "Y"
              READ CMATTF
                 AT END
                    MOVE "Y" TO SW-ATT-EOF
                 NOT AT END
                    IF FS-CMATTF = "00"
                       ADD 1 TO CNT-ATT-IN
                       IF ATT-CNT < 20000
                          ADD 1 TO ATT-CNT
                          MOVE CA-CIF-NO TO T-CA-CIF-NO(ATT-CNT)
                          MOVE CA-KANJI-NAME
                            TO T-CA-KANJI-NAME(ATT-CNT)
                          MOVE CA-KANA-NAME
                            TO T-CA-KANA-NAME(ATT-CNT)
                          MOVE CA-ADDR-CD TO T-CA-ADDR-CD(ATT-CNT)
                          MOVE CA-PHONE-NO TO T-CA-PHONE-NO(ATT-CNT)
                          MOVE CA-UPDATE-DT
                            TO T-CA-UPDATE-DT(ATT-CNT)
                          MOVE CA-ATTR-STATUS-KBN
                            TO T-CA-STATUS(ATT-CNT)
                       ELSE
                          DISPLAY "属性表件数超過"
                          MOVE "Y" TO SW-ABEND
                          MOVE 12 TO RETURN-CODE
                       END-IF
                    ELSE
                       DISPLAY "CMATTF 読込失敗 ST=" FS-CMATTF
                       MOVE "Y" TO SW-ABEND
                       MOVE 12 TO RETURN-CODE
                    END-IF
              END-READ
           END-PERFORM.
      *
       0300-LOAD-KEY.
           PERFORM UNTIL SW-KEY-EOF = "Y" OR SW-ABEND = "Y"
              READ CMKEYF
                 AT END
                    MOVE "Y" TO SW-KEY-EOF
                 NOT AT END
                    IF FS-CMKEYF = "00"
                       ADD 1 TO CNT-KEY-IN
                       IF KEY-CNT < 20000
                          ADD 1 TO KEY-CNT
                          MOVE CK-KEY-ID TO T-CK-KEY-ID(KEY-CNT)
                          MOVE CK-CIF-NO TO T-CK-CIF-NO(KEY-CNT)
                          MOVE CK-CHECK-DIGIT-CNT
                            TO T-CK-DIGIT-CNT(KEY-CNT)
                          MOVE CK-KEY-STATUS-KBN
                            TO T-CK-STATUS(KEY-CNT)
                       ELSE
                          DISPLAY "統合キー表件数超過"
                          MOVE "Y" TO SW-ABEND
                          MOVE 12 TO RETURN-CODE
                       END-IF
                    ELSE
                       DISPLAY "CMKEYF 読込失敗 ST=" FS-CMKEYF
                       MOVE "Y" TO SW-ABEND
                       MOVE 12 TO RETURN-CODE
                    END-IF
              END-READ
           END-PERFORM.
      *
       0400-LOAD-DUP.
           PERFORM UNTIL SW-DUP-EOF = "Y" OR SW-ABEND = "Y"
              READ CMDUPF
                 AT END
                    MOVE "Y" TO SW-DUP-EOF
                 NOT AT END
                    IF FS-CMDUPF = "00"
                       ADD 1 TO CNT-DUP-IN
                       IF DUP-CNT < 20000
                          ADD 1 TO DUP-CNT
                          MOVE DP-CANDIDATE-ID
                            TO T-DP-CAND-ID(DUP-CNT)
                          MOVE DP-CIF-NO-1 TO T-DP-CIF-NO-1(DUP-CNT)
                          MOVE DP-CIF-NO-2 TO T-DP-CIF-NO-2(DUP-CNT)
                          MOVE DP-MATCH-SCORE TO T-DP-SCORE(DUP-CNT)
                          MOVE DP-JUDGE-KBN TO T-DP-JUDGE(DUP-CNT)
                          MOVE DP-CREATE-DT TO T-DP-CREATE-DT(DUP-CNT)
                       ELSE
                          DISPLAY "重複候補表件数超過"
                          MOVE "Y" TO SW-ABEND
                          MOVE 12 TO RETURN-CODE
                       END-IF
                    ELSE
                       DISPLAY "CMDUPF 読込失敗 ST=" FS-CMDUPF
                       MOVE "Y" TO SW-ABEND
                       MOVE 12 TO RETURN-CODE
                    END-IF
              END-READ
           END-PERFORM.
      *
       1000-PROCESS-CIF.
           PERFORM UNTIL SW-CIF-EOF = "Y" OR SW-ABEND = "Y"
              READ CMCIFF
                 AT END
                    MOVE "Y" TO SW-CIF-EOF
                 NOT AT END
                    IF FS-CMCIFF = "00"
                       ADD 1 TO CNT-CIF-IN
                       PERFORM 1100-CHECK-ONE-CIF
                    ELSE
                       DISPLAY "CMCIFF 読込失敗 ST=" FS-CMCIFF
                       MOVE "Y" TO SW-ABEND
                       MOVE 12 TO RETURN-CODE
                    END-IF
              END-READ
           END-PERFORM.
      *
       1100-CHECK-ONE-CIF.
           MOVE SPACE TO WK-REASON WK-RESULT WK-KEY-EDIT
           PERFORM 1200-FIND-ATT
           PERFORM 1300-FIND-KEY
           PERFORM 1400-FIND-DUP
           IF CF-CIF-STATUS-KBN NOT = CN-STS-ACT
              AND CF-CIF-STATUS-KBN NOT = CN-STS-EXC
              AND CF-CIF-STATUS-KBN NOT = CN-STS-INV
              MOVE "10" TO WK-RESULT
              MOVE "CSTS" TO WK-REASON
              PERFORM 8100-WRITE-RESULT
              MOVE "E101" TO WK-REASON
              PERFORM 8200-WRITE-ERROR
           END-IF
           IF CF-CIF-STATUS-KBN = CN-STS-ACT
              IF WK-ATT-FOUND = "N"
                 MOVE "20" TO WK-RESULT
                 MOVE "AMIS" TO WK-REASON
                 PERFORM 8100-WRITE-RESULT
                 MOVE "E201" TO WK-REASON
                 PERFORM 8200-WRITE-ERROR
              ELSE
                 PERFORM 1500-CHECK-ATTR
              END-IF
              IF WK-KEY-FOUND = "N"
                 MOVE "30" TO WK-RESULT
                 MOVE "KMIS" TO WK-REASON
                 PERFORM 8100-WRITE-RESULT
                 MOVE "E301" TO WK-REASON
                 PERFORM 8200-WRITE-ERROR
              ELSE
                 PERFORM 1600-CHECK-KEY
              END-IF
           END-IF
           IF CF-CIF-STATUS-KBN NOT = CN-STS-ACT
              AND WK-KEY-FOUND = "Y"
              IF T-CK-STATUS(WK-FOUND-IDX) = CN-KEY-ACT
                 MOVE "40" TO WK-RESULT
                 MOVE "KACT" TO WK-REASON
                 PERFORM 8100-WRITE-RESULT
                 MOVE "E401" TO WK-REASON
                 PERFORM 8200-WRITE-ERROR
              END-IF
           END-IF
           IF WK-DUP-FOUND = "Y"
              MOVE "50" TO WK-RESULT
              MOVE "DUPN" TO WK-REASON
              PERFORM 8100-WRITE-RESULT
           END-IF.
      *
       1200-FIND-ATT.
           MOVE "N" TO WK-ATT-FOUND
           MOVE ZERO TO WK-IDX
           PERFORM VARYING WK-IDX FROM 1 BY 1
              UNTIL WK-IDX > ATT-CNT OR WK-ATT-FOUND = "Y"
              IF T-CA-CIF-NO(WK-IDX) = CF-CIF-NO
                 MOVE "Y" TO WK-ATT-FOUND
              END-IF
           END-PERFORM.
      *
       1300-FIND-KEY.
           MOVE "N" TO WK-KEY-FOUND
           MOVE ZERO TO WK-FOUND-IDX WK-IDX
           PERFORM VARYING WK-IDX FROM 1 BY 1
              UNTIL WK-IDX > KEY-CNT OR WK-KEY-FOUND = "Y"
              IF T-CK-CIF-NO(WK-IDX) = CF-CIF-NO
                 MOVE "Y" TO WK-KEY-FOUND
                 MOVE WK-IDX TO WK-FOUND-IDX
                 MOVE T-CK-KEY-ID(WK-IDX) TO WK-KEY-EDIT
              END-IF
           END-PERFORM.
      *
       1400-FIND-DUP.
           MOVE "N" TO WK-DUP-FOUND
           PERFORM VARYING WK-IDX FROM 1 BY 1
              UNTIL WK-IDX > DUP-CNT OR WK-DUP-FOUND = "Y"
              IF (T-DP-CIF-NO-1(WK-IDX) = CF-CIF-NO
                  OR T-DP-CIF-NO-2(WK-IDX) = CF-CIF-NO)
                 AND (T-DP-JUDGE(WK-IDX) = CN-DUP-MIK
                  OR T-DP-JUDGE(WK-IDX) = CN-DUP-HORYU)
                 MOVE "Y" TO WK-DUP-FOUND
              END-IF
           END-PERFORM.
      *
       1500-CHECK-ATTR.
           IF CA-KANJI-NAME = SPACE
              MOVE "21" TO WK-RESULT
              MOVE "KNJM" TO WK-REASON
              PERFORM 8100-WRITE-RESULT
           END-IF
           IF CA-KANA-NAME = SPACE
              MOVE "21" TO WK-RESULT
              MOVE "KNAM" TO WK-REASON
              PERFORM 8100-WRITE-RESULT
           END-IF
           IF CA-ADDR-CD = SPACE
              MOVE "22" TO WK-RESULT
              MOVE "ADRM" TO WK-REASON
              PERFORM 8100-WRITE-RESULT
           END-IF
           IF CA-ATTR-STATUS-KBN NOT = CN-STS-ACT
              MOVE "23" TO WK-RESULT
              MOVE "ASTS" TO WK-REASON
              PERFORM 8100-WRITE-RESULT
              MOVE "E202" TO WK-REASON
              PERFORM 8200-WRITE-ERROR
           END-IF.
      *
       1600-CHECK-KEY.
           IF T-CK-STATUS(WK-FOUND-IDX) NOT = CN-KEY-ACT
              MOVE "31" TO WK-RESULT
              MOVE "KSTS" TO WK-REASON
              PERFORM 8100-WRITE-RESULT
              MOVE "E302" TO WK-REASON
              PERFORM 8200-WRITE-ERROR
           END-IF
           IF T-CK-DIGIT-CNT(WK-FOUND-IDX) = ZERO
              MOVE "32" TO WK-RESULT
              MOVE "KDGT" TO WK-REASON
              PERFORM 8100-WRITE-RESULT
           END-IF.
      *
       8100-WRITE-RESULT.
           INITIALIZE CMRSLF-REC
           ADD 1 TO CNT-RS-ID CNT-RSL-OUT
           MOVE CNT-RS-ID TO RS-RESULT-ID
           MOVE CF-CIF-NO TO RS-CIF-NO
           MOVE WK-KEY-EDIT TO RS-KEY-ID
           MOVE WK-RESULT TO RS-RESULT-KBN
           MOVE WK-REASON TO RS-REASON-CD
           MOVE WS-CUR-DATE TO RS-OUTPUT-DT
           WRITE CMRSLF-REC
           IF FS-CMRSLF NOT = "00"
              DISPLAY "CMRSLF 書込失敗 ST=" FS-CMRSLF
              MOVE "Y" TO SW-ABEND
              MOVE 12 TO RETURN-CODE
           END-IF.
      *
       8200-WRITE-ERROR.
           INITIALIZE CKERRF-REC
           ADD 1 TO CNT-ER-ID CNT-ERR-OUT
           MOVE CNT-ER-ID TO ER-ERROR-ID
           MOVE CN-PGM-ID TO ER-SOURCE-PGM-ID
           MOVE CF-CIF-NO TO ER-CIF-NO
           MOVE WK-KEY-EDIT TO ER-KEY-ID
           MOVE WK-REASON TO ER-ERROR-CD
           MOVE WS-CUR-DATE TO ER-ERROR-DT
           WRITE CKERRF-REC
           IF FS-CKERRF NOT = "00"
              DISPLAY "CKERRF 書込失敗 ST=" FS-CKERRF
              MOVE "Y" TO SW-ABEND
              MOVE 12 TO RETURN-CODE
           END-IF.
      *
       9000-END.
           CLOSE CMCIFF CMATTF CMKEYF CMDUPF CKERRF CMRSLF
           DISPLAY "CM230B 顧客マスタ棚卸 終了"
           DISPLAY "CIF入力件数=" CNT-CIF-IN
           DISPLAY "属性入力件数=" CNT-ATT-IN
           DISPLAY "キー入力件数=" CNT-KEY-IN
           DISPLAY "候補入力件数=" CNT-DUP-IN
           DISPLAY "結果出力件数=" CNT-RSL-OUT
           DISPLAY "重大出力件数=" CNT-ERR-OUT
           IF SW-ABEND = "Y"
              IF RETURN-CODE = 0
                 MOVE 8 TO RETURN-CODE
              END-IF
           ELSE
              MOVE 0 TO RETURN-CODE
           END-IF.
