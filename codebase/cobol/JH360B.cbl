       IDENTIFICATION DIVISION.
       PROGRAM-ID. JH360B.
      ******************************************************************
      * 変更履歴
      * 版数  年月日    担当      概要
      * 1.00  20240115  JH開発    初版作成
      * 1.01  20240520  JH保守    休眠判定およびDM許諾判定を追加
      * 1.02  20250610  JH保守    JH362S適格判定呼出しを追加
      ******************************************************************
      * キャンペーン対象者抽出バッチ
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT JHSEGRF ASSIGN TO "JHSEGRF"
              ORGANIZATION IS LINE SEQUENTIAL
              FILE STATUS IS FS-JHSEGRF.
           SELECT JHCATTF ASSIGN TO "JHCATTF"
              ORGANIZATION IS INDEXED
              ACCESS MODE IS RANDOM
              RECORD KEY IS CA-CUST-ID
              FILE STATUS IS FS-JHCATTF.
           SELECT JHCBALF ASSIGN TO "JHCBALF"
              ORGANIZATION IS LINE SEQUENTIAL
              FILE STATUS IS FS-JHCBALF.
           SELECT JHDORMF ASSIGN TO "JHDORMF"
              ORGANIZATION IS INDEXED
              ACCESS MODE IS RANDOM
              RECORD KEY IS DM-CUST-ID
              FILE STATUS IS FS-JHDORMF.
           SELECT JHREFMST ASSIGN TO "JHREFMST"
              ORGANIZATION IS INDEXED
              ACCESS MODE IS RANDOM
              RECORD KEY IS RF-REF-TYPE-CD
              FILE STATUS IS FS-JHREFMST.
           SELECT JHCMPRF ASSIGN TO "JHCMPRF"
              ORGANIZATION IS LINE SEQUENTIAL
              FILE STATUS IS FS-JHCMPRF.

       DATA DIVISION.
       FILE SECTION.
       FD  JHSEGRF.
           COPY JHSEGRFC.
       FD  JHCATTF.
           COPY JHCATTC.
       FD  JHCBALF.
           COPY JHCBALFC.
       FD  JHDORMF.
           COPY JHDORMC.
       FD  JHREFMST.
           COPY JHREFMSTC.
       FD  JHCMPRF.
           COPY JHCMPRC.

       WORKING-STORAGE SECTION.
       01  FS-AREA.
           05 FS-JHSEGRF              PIC XX VALUE SPACES.
           05 FS-JHCATTF              PIC XX VALUE SPACES.
           05 FS-JHCBALF              PIC XX VALUE SPACES.
           05 FS-JHDORMF              PIC XX VALUE SPACES.
           05 FS-JHREFMST             PIC XX VALUE SPACES.
           05 FS-JHCMPRF              PIC XX VALUE SPACES.

       01  SW-AREA.
           05 SW-END-SEGRF            PIC X VALUE "N".
              88 END-SEGRF                 VALUE "Y".
           05 SW-END-CBALF            PIC X VALUE "N".
              88 END-CBALF                 VALUE "Y".
           05 SW-ABEND                PIC X VALUE "N".
              88 ABEND-FOUND               VALUE "Y".
           05 SW-SEG-FOUND            PIC X VALUE "N".
              88 SEG-FOUND                 VALUE "Y".
           05 SW-ATTR-FOUND           PIC X VALUE "N".
              88 ATTR-FOUND                VALUE "Y".
           05 SW-DORM-FOUND           PIC X VALUE "N".
              88 DORM-FOUND                VALUE "Y".

       01  CTL-AREA.
           05 WK-CAMPAIGN-ID          PIC X(10) VALUE "CMP202606".
           05 WK-EXTRACT-DATE         PIC 9(08) VALUE 20260618.
           05 WK-SYS-DATE             PIC 9(08).
           05 WK-IDX                  PIC 9(05) COMP VALUE ZERO.
           05 WK-SEG-CNT              PIC 9(05) COMP VALUE ZERO.
           05 WK-SEARCH-MAX           PIC 9(05) COMP VALUE 10000.
           05 WK-USE-SEG-CD           PIC X(04) VALUE SPACES.
           05 WK-USE-SEG-NAME         PIC X(20) VALUE SPACES.
           05 WK-DM-REJECT-FLAG       PIC X VALUE SPACE.
           05 WK-DORMANT-FLAG         PIC X VALUE SPACE.
           05 WK-REASON-CD            PIC X(04) VALUE SPACES.
           05 WK-DENY-REASON-CD       PIC X(04) VALUE SPACES.

       01  COUNT-AREA.
           05 CNT-SEGRF-READ          PIC 9(09) VALUE ZERO.
           05 CNT-CBALF-READ          PIC 9(09) VALUE ZERO.
           05 CNT-ATTR-MISS           PIC 9(09) VALUE ZERO.
           05 CNT-DORM-MISS           PIC 9(09) VALUE ZERO.
           05 CNT-SEG-SUPP            PIC 9(09) VALUE ZERO.
           05 CNT-ELIGIBLE            PIC 9(09) VALUE ZERO.
           05 CNT-DENY-DM             PIC 9(09) VALUE ZERO.
           05 CNT-DENY-DORM           PIC 9(09) VALUE ZERO.
           05 CNT-DENY-ATTR           PIC 9(09) VALUE ZERO.
           05 CNT-DENY-OTHER          PIC 9(09) VALUE ZERO.
           05 CNT-WRITE               PIC 9(09) VALUE ZERO.

       01  SEG-TABLE.
           05 SEG-ENTRY OCCURS 10000 TIMES.
              10 TB-SR-CUST-ID        PIC X(12).
              10 TB-SR-AVG-BAL        PIC S9(13)V99.
              10 TB-SR-SEG-CD         PIC X(04).
              10 TB-SR-SEG-NAME       PIC X(20).

       01  LK-JH362S-PARM.
           05 LK-CAMPAIGN-ID          PIC X(10).
           05 LK-CUSTOMER-ATTR-CD     PIC X(04).
           05 LK-SEGMENT-CD           PIC X(04).
           05 LK-DM-REJECT-FLAG       PIC X.
           05 LK-DORMANT-FLAG         PIC X.
           05 LK-ELIGIBLE-FLAG        PIC X.
           05 LK-DENY-REASON-CD       PIC X(04).

           COPY LK-SEG-PARM.

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE 0 TO RETURN-CODE
           ACCEPT WK-SYS-DATE FROM DATE YYYYMMDD
           MOVE WK-SYS-DATE TO WK-EXTRACT-DATE

           PERFORM 1000-OPEN-FILES
           IF ABEND-FOUND
              PERFORM 9000-ABEND
           END-IF

           PERFORM 1100-CHECK-CAMPAIGN
           IF ABEND-FOUND
              PERFORM 9000-ABEND
           END-IF

           PERFORM 2000-LOAD-SEGMENT
           IF ABEND-FOUND
              PERFORM 9000-ABEND
           END-IF

           PERFORM 3000-PROCESS-CANDIDATE
              UNTIL END-CBALF OR ABEND-FOUND

           IF ABEND-FOUND
              PERFORM 9000-ABEND
           END-IF

           PERFORM 8000-CLOSE-FILES
           IF ABEND-FOUND
              PERFORM 9000-ABEND
           END-IF

           DISPLAY "JH360B 正常終了"
           DISPLAY "残高読込件数=" CNT-CBALF-READ
           DISPLAY "補完判定件数=" CNT-SEG-SUPP
           DISPLAY "適格件数=" CNT-ELIGIBLE
           DISPLAY "出力件数=" CNT-WRITE
           DISPLAY "属性未登録除外=" CNT-ATTR-MISS
           DISPLAY "休眠未登録件数=" CNT-DORM-MISS
           DISPLAY "DM拒否除外=" CNT-DENY-DM
           DISPLAY "休眠除外=" CNT-DENY-DORM
           DISPLAY "属性除外=" CNT-DENY-ATTR
           DISPLAY "その他除外=" CNT-DENY-OTHER
           MOVE 0 TO RETURN-CODE
           GOBACK.

       1000-OPEN-FILES.
           OPEN INPUT JHSEGRF
           IF FS-JHSEGRF NOT = "00"
              DISPLAY "JHSEGRF オープン失敗 ST=" FS-JHSEGRF
              SET ABEND-FOUND TO TRUE
           END-IF

           OPEN INPUT JHCATTF
           IF FS-JHCATTF NOT = "00"
              DISPLAY "JHCATTF オープン失敗 ST=" FS-JHCATTF
              SET ABEND-FOUND TO TRUE
           END-IF

           OPEN INPUT JHCBALF
           IF FS-JHCBALF NOT = "00"
              DISPLAY "JHCBALF オープン失敗 ST=" FS-JHCBALF
              SET ABEND-FOUND TO TRUE
           END-IF

           OPEN INPUT JHDORMF
           IF FS-JHDORMF NOT = "00"
              DISPLAY "JHDORMF オープン失敗 ST=" FS-JHDORMF
              SET ABEND-FOUND TO TRUE
           END-IF

           OPEN INPUT JHREFMST
           IF FS-JHREFMST NOT = "00"
              DISPLAY "JHREFMST オープン失敗 ST=" FS-JHREFMST
              SET ABEND-FOUND TO TRUE
           END-IF

           OPEN OUTPUT JHCMPRF
           IF FS-JHCMPRF NOT = "00"
              DISPLAY "JHCMPRF オープン失敗 ST=" FS-JHCMPRF
              SET ABEND-FOUND TO TRUE
           END-IF.

       1100-CHECK-CAMPAIGN.
           MOVE "CAMPAIGN" TO RF-REF-TYPE-CD
           READ JHREFMST
              INVALID KEY
                 DISPLAY "キャンペーン参照定義なし"
                 SET ABEND-FOUND TO TRUE
              NOT INVALID KEY
                 IF RF-ACTIVE-FLAG NOT = "Y"
                    DISPLAY "キャンペーン参照定義が無効"
                    SET ABEND-FOUND TO TRUE
                 END-IF
                 IF RF-VALID-FROM-DATE > WK-EXTRACT-DATE
                    DISPLAY "キャンペーン適用開始日前"
                    SET ABEND-FOUND TO TRUE
                 END-IF
                 IF RF-VALID-TO-DATE < WK-EXTRACT-DATE
                    DISPLAY "キャンペーン適用終了後"
                    SET ABEND-FOUND TO TRUE
                 END-IF
           END-READ
           IF FS-JHREFMST NOT = "00" AND FS-JHREFMST NOT = "23"
              DISPLAY "JHREFMST 読込失敗 ST=" FS-JHREFMST
              SET ABEND-FOUND TO TRUE
           END-IF.

       2000-LOAD-SEGMENT.
           PERFORM UNTIL END-SEGRF OR ABEND-FOUND
              READ JHSEGRF
                 AT END
                    SET END-SEGRF TO TRUE
                 NOT AT END
                    ADD 1 TO CNT-SEGRF-READ
                    IF WK-SEG-CNT >= WK-SEARCH-MAX
                       DISPLAY "セグメント表容量超過"
                       SET ABEND-FOUND TO TRUE
                    ELSE
                       ADD 1 TO WK-SEG-CNT
                       MOVE SR-CUST-ID TO TB-SR-CUST-ID(WK-SEG-CNT)
                       MOVE SR-AVG-BAL TO TB-SR-AVG-BAL(WK-SEG-CNT)
                       MOVE SR-SEG-CD TO TB-SR-SEG-CD(WK-SEG-CNT)
                       MOVE SR-SEG-NAME TO TB-SR-SEG-NAME(WK-SEG-CNT)
                    END-IF
              END-READ
              IF FS-JHSEGRF NOT = "00" AND FS-JHSEGRF NOT = "10"
                 DISPLAY "JHSEGRF 読込失敗 ST=" FS-JHSEGRF
                 SET ABEND-FOUND TO TRUE
              END-IF
           END-PERFORM
           READ JHCBALF
              AT END
                 SET END-CBALF TO TRUE
              NOT AT END
                 ADD 1 TO CNT-CBALF-READ
           END-READ
           IF FS-JHCBALF NOT = "00" AND FS-JHCBALF NOT = "10"
              DISPLAY "JHCBALF 初回読込失敗 ST=" FS-JHCBALF
              SET ABEND-FOUND TO TRUE
           END-IF.

       3000-PROCESS-CANDIDATE.
           PERFORM 3100-INIT-CANDIDATE
           PERFORM 3200-FIND-SEGMENT
           IF NOT ABEND-FOUND
              PERFORM 3300-READ-ATTRIBUTE
           END-IF
           IF NOT ABEND-FOUND
              PERFORM 3400-READ-DORMANT
           END-IF

           IF NOT ABEND-FOUND
              IF ATTR-FOUND
                 PERFORM 3500-JUDGE-ELIGIBLE
                 IF NOT ABEND-FOUND
                    IF LK-ELIGIBLE-FLAG = "Y"
                       PERFORM 3600-WRITE-CAMPAIGN
                    ELSE
                       PERFORM 3700-COUNT-DENY
                    END-IF
                 END-IF
              ELSE
                 ADD 1 TO CNT-ATTR-MISS
                 ADD 1 TO CNT-DENY-ATTR
              END-IF
           END-IF

           IF NOT ABEND-FOUND
              READ JHCBALF
                 AT END
                    SET END-CBALF TO TRUE
                 NOT AT END
                    ADD 1 TO CNT-CBALF-READ
              END-READ
              IF FS-JHCBALF NOT = "00" AND FS-JHCBALF NOT = "10"
                 DISPLAY "JHCBALF 読込失敗 ST=" FS-JHCBALF
                 SET ABEND-FOUND TO TRUE
              END-IF
           END-IF.

       3100-INIT-CANDIDATE.
           MOVE "N" TO SW-SEG-FOUND
           MOVE "N" TO SW-ATTR-FOUND
           MOVE "N" TO SW-DORM-FOUND
           MOVE SPACES TO WK-USE-SEG-CD
           MOVE SPACES TO WK-USE-SEG-NAME
           MOVE SPACES TO WK-REASON-CD
           MOVE SPACES TO WK-DENY-REASON-CD
           MOVE "N" TO WK-DM-REJECT-FLAG
           MOVE "N" TO WK-DORMANT-FLAG
           INITIALIZE LK-JH362S-PARM
           INITIALIZE LK-SEG-PARM.

       3200-FIND-SEGMENT.
           PERFORM VARYING WK-IDX FROM 1 BY 1
              UNTIL WK-IDX > WK-SEG-CNT OR SEG-FOUND
              IF TB-SR-CUST-ID(WK-IDX) = CB-CUST-ID
                 SET SEG-FOUND TO TRUE
                 MOVE TB-SR-SEG-CD(WK-IDX) TO WK-USE-SEG-CD
                 MOVE TB-SR-SEG-NAME(WK-IDX) TO WK-USE-SEG-NAME
              END-IF
           END-PERFORM

           IF NOT SEG-FOUND OR WK-USE-SEG-CD = SPACES
              MOVE CB-AVG-BAL TO LK-SEG-BAL
              CALL "JH315S" USING LK-SEG-PARM
              IF LK-SEG-RET NOT = "00"
                 DISPLAY "JH315S 補完判定失敗"
                 DISPLAY "顧客=" CB-CUST-ID
                 SET ABEND-FOUND TO TRUE
              ELSE
                 MOVE LK-SEG-CD TO WK-USE-SEG-CD
                 MOVE LK-SEG-NAME TO WK-USE-SEG-NAME
                 MOVE "S315" TO WK-REASON-CD
                 ADD 1 TO CNT-SEG-SUPP
              END-IF
           ELSE
              MOVE "S001" TO WK-REASON-CD
           END-IF

           IF WK-USE-SEG-CD NOT = "SG01"
              AND WK-USE-SEG-CD NOT = "SG02"
              AND WK-USE-SEG-CD NOT = "SG03"
              AND WK-USE-SEG-CD NOT = "SG04"
              AND WK-USE-SEG-CD NOT = "SG05"
              DISPLAY "セグメントコード不正"
              DISPLAY "顧客=" CB-CUST-ID
              SET ABEND-FOUND TO TRUE
           END-IF.

       3300-READ-ATTRIBUTE.
           MOVE CB-CUST-ID TO CA-CUST-ID
           READ JHCATTF
              INVALID KEY
                 MOVE "N" TO SW-ATTR-FOUND
              NOT INVALID KEY
                 SET ATTR-FOUND TO TRUE
                 IF CA-DM-PERMIT-FLAG = "N"
                    MOVE "Y" TO WK-DM-REJECT-FLAG
                 ELSE
                    MOVE "N" TO WK-DM-REJECT-FLAG
                 END-IF
           END-READ
           IF FS-JHCATTF NOT = "00" AND FS-JHCATTF NOT = "23"
              DISPLAY "JHCATTF 読込失敗 ST=" FS-JHCATTF
              SET ABEND-FOUND TO TRUE
           END-IF

           IF ATTR-FOUND
              IF CA-DM-PERMIT-FLAG NOT = "Y"
                 AND CA-DM-PERMIT-FLAG NOT = "N"
                 DISPLAY "DM許諾フラグ不正"
                 DISPLAY "顧客=" CB-CUST-ID
                 SET ABEND-FOUND TO TRUE
              END-IF
              IF CA-AGE-BAND-CD = SPACES
                 DISPLAY "年齢帯コード未設定"
                 DISPLAY "顧客=" CB-CUST-ID
                 SET ABEND-FOUND TO TRUE
              END-IF
           END-IF.

       3400-READ-DORMANT.
           MOVE CB-CUST-ID TO DM-CUST-ID
           READ JHDORMF
              INVALID KEY
                 ADD 1 TO CNT-DORM-MISS
                 MOVE "N" TO WK-DORMANT-FLAG
              NOT INVALID KEY
                 SET DORM-FOUND TO TRUE
                 MOVE DM-DORMANT-FLAG TO WK-DORMANT-FLAG
           END-READ
           IF FS-JHDORMF NOT = "00" AND FS-JHDORMF NOT = "23"
              DISPLAY "JHDORMF 読込失敗 ST=" FS-JHDORMF
              SET ABEND-FOUND TO TRUE
           END-IF

           IF DORM-FOUND
              IF DM-DORMANT-FLAG NOT = "Y" AND
                 DM-DORMANT-FLAG NOT = "N"
                 DISPLAY "休眠フラグ不正"
                 DISPLAY "顧客=" CB-CUST-ID
                 SET ABEND-FOUND TO TRUE
              END-IF
           END-IF.

       3500-JUDGE-ELIGIBLE.
           MOVE WK-CAMPAIGN-ID TO LK-CAMPAIGN-ID
           MOVE CA-TRUST-REL-CD TO LK-CUSTOMER-ATTR-CD
           MOVE WK-USE-SEG-CD TO LK-SEGMENT-CD
           MOVE WK-DM-REJECT-FLAG TO LK-DM-REJECT-FLAG
           MOVE WK-DORMANT-FLAG TO LK-DORMANT-FLAG

           CALL "JH362S" USING LK-JH362S-PARM

           IF LK-ELIGIBLE-FLAG = "Y"
              ADD 1 TO CNT-ELIGIBLE
           ELSE
              IF LK-ELIGIBLE-FLAG NOT = "N"
                 DISPLAY "JH362S 適格結果不正"
                 DISPLAY "顧客=" CB-CUST-ID
                 SET ABEND-FOUND TO TRUE
              END-IF
           END-IF.

       3600-WRITE-CAMPAIGN.
           INITIALIZE JHCMPRF-REC
           MOVE WK-CAMPAIGN-ID TO CP-CAMPAIGN-ID
           MOVE CB-CUST-ID TO CP-CUST-ID
           MOVE WK-USE-SEG-CD TO CP-SEG-CD
           MOVE CA-AGE-BAND-CD TO CP-AGE-BAND-CD
           MOVE CB-AVG-BAL TO CP-AVG-BAL
           MOVE WK-REASON-CD TO CP-TARGET-REASON-CD
           MOVE WK-EXTRACT-DATE TO CP-EXTRACT-DATE

           WRITE JHCMPRF-REC
           IF FS-JHCMPRF NOT = "00"
              DISPLAY "JHCMPRF 書込失敗 ST=" FS-JHCMPRF
              SET ABEND-FOUND TO TRUE
           ELSE
              ADD 1 TO CNT-WRITE
           END-IF.

       3700-COUNT-DENY.
           MOVE LK-DENY-REASON-CD TO WK-DENY-REASON-CD
           EVALUATE WK-DENY-REASON-CD
              WHEN "DMNG"
                 ADD 1 TO CNT-DENY-DM
              WHEN "DORM"
                 ADD 1 TO CNT-DENY-DORM
              WHEN "ATTR"
                 ADD 1 TO CNT-DENY-ATTR
              WHEN OTHER
                 ADD 1 TO CNT-DENY-OTHER
           END-EVALUATE.

       8000-CLOSE-FILES.
           CLOSE JHSEGRF
           IF FS-JHSEGRF NOT = "00"
              DISPLAY "JHSEGRF クローズ失敗 ST=" FS-JHSEGRF
              SET ABEND-FOUND TO TRUE
           END-IF
           CLOSE JHCATTF
           IF FS-JHCATTF NOT = "00"
              DISPLAY "JHCATTF クローズ失敗 ST=" FS-JHCATTF
              SET ABEND-FOUND TO TRUE
           END-IF
           CLOSE JHCBALF
           IF FS-JHCBALF NOT = "00"
              DISPLAY "JHCBALF クローズ失敗 ST=" FS-JHCBALF
              SET ABEND-FOUND TO TRUE
           END-IF
           CLOSE JHDORMF
           IF FS-JHDORMF NOT = "00"
              DISPLAY "JHDORMF クローズ失敗 ST=" FS-JHDORMF
              SET ABEND-FOUND TO TRUE
           END-IF
           CLOSE JHREFMST
           IF FS-JHREFMST NOT = "00"
              DISPLAY "JHREFMST クローズ失敗 ST=" FS-JHREFMST
              SET ABEND-FOUND TO TRUE
           END-IF
           CLOSE JHCMPRF
           IF FS-JHCMPRF NOT = "00"
              DISPLAY "JHCMPRF クローズ失敗 ST=" FS-JHCMPRF
              SET ABEND-FOUND TO TRUE
           END-IF.

       9000-ABEND.
           DISPLAY "JH360B 異常終了"
           DISPLAY "残高読込件数=" CNT-CBALF-READ
           DISPLAY "出力件数=" CNT-WRITE
           MOVE 8 TO RETURN-CODE
           GOBACK.
