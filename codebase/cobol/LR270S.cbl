       IDENTIFICATION DIVISION.
       PROGRAM-ID. LR270S.
       AUTHOR.     みらい生命 システム部.
       DATE-WRITTEN. 2024-05-20.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LFRVSF ASSIGN TO "LFRVSF"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-LFRVSF-ST.

           SELECT LFCNTF ASSIGN TO "LFCNTF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CN-POL-NO
               FILE STATUS IS WS-LFCNTF-ST.

       DATA DIVISION.
       FILE SECTION.
       FD  LFRVSF.
           COPY LFRVSFC.

       FD  LFCNTF.
           COPY LFCNTFC.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05 WS-LFRVSF-ST              PIC XX VALUE SPACE.
           05 WS-LFCNTF-ST              PIC XX VALUE SPACE.

       01  WS-SWITCHES.
           05 WS-LFRVSF-EOF             PIC X VALUE "N".
              88 LFRVSF-END                  VALUE "Y".
           05 WS-RV-FOUND               PIC X VALUE "N".
              88 RV-FOUND                    VALUE "Y".
           05 WS-HARD-ERROR             PIC X VALUE "N".
              88 HARD-ERROR                  VALUE "Y".
           05 WS-CNT-FOUND              PIC X VALUE "N".
              88 CNT-FOUND                   VALUE "Y".

       01  WS-WORK.
           05 WS-DIFF-AMT               PIC S9(13) VALUE ZERO.
           05 WS-TEXT-CD                PIC X(06) VALUE SPACE.
           05 WS-REASON                 PIC X(60) VALUE SPACE.
           05 WS-DISPLAY-AMT            PIC -ZZZ,ZZZ,ZZZ,ZZ9.
           05 WS-DISPLAY-ST             PIC XX VALUE SPACE.

       01  WS-CONSTANTS.
           05 WC-YES                    PIC X VALUE "Y".
           05 WC-NO                     PIC X VALUE "N".
           05 WC-STS-NORMAL             PIC X VALUE "0".
           05 WC-STS-CREATED            PIC X VALUE "1".
           05 WC-STS-PRINTED            PIC X VALUE "2".
           05 WC-STS-REISSUE            PIC X VALUE "7".
           05 WC-STS-OLD-COMPAT         PIC X VALUE "8".
           05 WC-TYPE-RATE-REV          PIC X VALUE "1".
           05 WC-TYPE-PAY-CHANGE        PIC X VALUE "2".
           05 WC-TYPE-MATURITY          PIC X VALUE "3".
           05 WC-TYPE-ADDRESS           PIC X VALUE "4".
           05 WC-PAY-MONTHLY            PIC X VALUE "1".
           05 WC-PAY-HALF               PIC X VALUE "2".
           05 WC-PAY-YEARLY             PIC X VALUE "3".
           05 WC-PAY-LUMP               PIC X VALUE "4".

       LINKAGE SECTION.
       01  LR270S-PARM.
           05 LR-NOTICE-ID              PIC X(12).
           05 LR-RESULT-CD              PIC X.
           05 LR-FIXED-TEXT-CD          PIC X(06).
           05 LR-RESULT-MSG             PIC X(60).

       PROCEDURE DIVISION USING LR270S-PARM.
       0000-MAIN SECTION.
       0000-START.
           MOVE 0 TO RETURN-CODE
           PERFORM 1000-INITIALIZE
           PERFORM 1100-VALIDATE-PARM
           IF NOT HARD-ERROR
               PERFORM 2000-OPEN-FILES
           END-IF
           IF NOT HARD-ERROR
               PERFORM 3000-READ-NOTICE
           END-IF
           IF NOT HARD-ERROR
               PERFORM 4000-READ-CONTRACT
           END-IF
           IF NOT HARD-ERROR
               PERFORM 5000-SELECT-TEXT
           END-IF
           PERFORM 8000-CLOSE-FILES
           IF HARD-ERROR
               MOVE "E" TO LR-RESULT-CD
               MOVE WS-REASON TO LR-RESULT-MSG
               MOVE 8 TO RETURN-CODE
           ELSE
               MOVE "N" TO LR-RESULT-CD
               MOVE WS-TEXT-CD TO LR-FIXED-TEXT-CD
               MOVE "正常終了" TO LR-RESULT-MSG
               MOVE 0 TO RETURN-CODE
           END-IF
           GOBACK.

       1000-INITIALIZE SECTION.
           MOVE SPACE TO LR-RESULT-CD
           MOVE SPACE TO LR-FIXED-TEXT-CD
           MOVE SPACE TO LR-RESULT-MSG
           MOVE SPACE TO WS-REASON
           MOVE ZERO  TO WS-DIFF-AMT
           MOVE WC-NO TO WS-LFRVSF-EOF
           MOVE WC-NO TO WS-RV-FOUND
           MOVE WC-NO TO WS-CNT-FOUND
           MOVE WC-NO TO WS-HARD-ERROR.

       1100-VALIDATE-PARM SECTION.
           IF LR-NOTICE-ID = SPACE
               MOVE "通知ＩＤ未設定" TO WS-REASON
               MOVE WC-YES TO WS-HARD-ERROR
           END-IF.

       2000-OPEN-FILES SECTION.
           OPEN INPUT LFRVSF
           IF WS-LFRVSF-ST NOT = "00"
               MOVE WS-LFRVSF-ST TO WS-DISPLAY-ST
               STRING "LFRVSF オープン失敗 ST="
                      WS-DISPLAY-ST
                 DELIMITED BY SIZE INTO WS-REASON
               END-STRING
               DISPLAY WS-REASON
               MOVE WC-YES TO WS-HARD-ERROR
           END-IF

           IF NOT HARD-ERROR
               OPEN INPUT LFCNTF
               IF WS-LFCNTF-ST NOT = "00"
                   MOVE WS-LFCNTF-ST TO WS-DISPLAY-ST
                   STRING "LFCNTF オープン失敗 ST="
                          WS-DISPLAY-ST
                     DELIMITED BY SIZE INTO WS-REASON
                   END-STRING
                   DISPLAY WS-REASON
                   MOVE WC-YES TO WS-HARD-ERROR
               END-IF
           END-IF.

       3000-READ-NOTICE SECTION.
           PERFORM UNTIL LFRVSF-END OR RV-FOUND
               READ LFRVSF
                   AT END
                       MOVE WC-YES TO WS-LFRVSF-EOF
                   NOT AT END
                       IF RV-NOTICE-ID = LR-NOTICE-ID
                           MOVE WC-YES TO WS-RV-FOUND
                       END-IF
               END-READ
           END-PERFORM

           IF NOT RV-FOUND
               STRING "通知情報なし ID=" LR-NOTICE-ID
                 DELIMITED BY SIZE INTO WS-REASON
               END-STRING
               DISPLAY WS-REASON
               MOVE WC-YES TO WS-HARD-ERROR
           END-IF.

       4000-READ-CONTRACT SECTION.
           MOVE RV-POL-NO TO CN-POL-NO
           READ LFCNTF
               KEY IS CN-POL-NO
               INVALID KEY
                   MOVE WC-NO TO WS-CNT-FOUND
               NOT INVALID KEY
                   MOVE WC-YES TO WS-CNT-FOUND
           END-READ

           IF WS-LFCNTF-ST NOT = "00" AND WS-LFCNTF-ST NOT = "23"
               MOVE WS-LFCNTF-ST TO WS-DISPLAY-ST
               STRING "LFCNTF 読込失敗 ST="
                      WS-DISPLAY-ST
                 DELIMITED BY SIZE INTO WS-REASON
               END-STRING
               DISPLAY WS-REASON
               MOVE WC-YES TO WS-HARD-ERROR
           END-IF

           IF NOT HARD-ERROR AND NOT CNT-FOUND
               STRING "契約情報なし 証券番号=" RV-POL-NO
                 DELIMITED BY SIZE INTO WS-REASON
               END-STRING
               DISPLAY WS-REASON
               MOVE WC-YES TO WS-HARD-ERROR
           END-IF.

       5000-SELECT-TEXT SECTION.
           PERFORM 5100-VALIDATE-CODE
           IF NOT HARD-ERROR
               EVALUATE RV-NOTICE-TYPE-KBN
                   WHEN WC-TYPE-RATE-REV
                       PERFORM 5200-RATE-REVISION
                   WHEN WC-TYPE-PAY-CHANGE
                       PERFORM 5300-PAY-METHOD
                   WHEN WC-TYPE-MATURITY
                       MOVE "MTY001" TO WS-TEXT-CD
                   WHEN WC-TYPE-ADDRESS
                       MOVE "ADR001" TO WS-TEXT-CD
                   WHEN OTHER
                       MOVE "通知種別区分不正" TO WS-REASON
                       DISPLAY WS-REASON
                       MOVE WC-YES TO WS-HARD-ERROR
               END-EVALUATE
           END-IF

           IF NOT HARD-ERROR
               PERFORM 5900-OLD-CODE-COMPAT
           END-IF.

       5100-VALIDATE-CODE SECTION.
           EVALUATE RV-NOTICE-STATUS-KBN
               WHEN WC-STS-NORMAL
               WHEN WC-STS-CREATED
               WHEN WC-STS-PRINTED
               WHEN WC-STS-REISSUE
               WHEN WC-STS-OLD-COMPAT
                   CONTINUE
               WHEN OTHER
                   MOVE "通知状態区分不正" TO WS-REASON
                   DISPLAY WS-REASON
                   MOVE WC-YES TO WS-HARD-ERROR
           END-EVALUATE

           IF NOT HARD-ERROR
               EVALUATE CN-PAY-METHOD-KBN
                   WHEN WC-PAY-MONTHLY
                   WHEN WC-PAY-HALF
                   WHEN WC-PAY-YEARLY
                   WHEN WC-PAY-LUMP
                       CONTINUE
                   WHEN OTHER
                       MOVE "払込方法区分不正" TO WS-REASON
                       DISPLAY WS-REASON
                       MOVE WC-YES TO WS-HARD-ERROR
               END-EVALUATE
           END-IF.

       5200-RATE-REVISION SECTION.
           COMPUTE WS-DIFF-AMT = RV-NEW-PRM-AMT - RV-OLD-PRM-AMT
           IF WS-DIFF-AMT > ZERO
               MOVE "RVI001" TO WS-TEXT-CD
           ELSE
               IF WS-DIFF-AMT < ZERO
                   MOVE "RVD001" TO WS-TEXT-CD
               ELSE
                   MOVE "RVN001" TO WS-TEXT-CD
               END-IF
           END-IF
           MOVE WS-DIFF-AMT TO WS-DISPLAY-AMT
           DISPLAY "料率改定差額=" WS-DISPLAY-AMT.

       5300-PAY-METHOD SECTION.
           EVALUATE CN-PAY-METHOD-KBN
               WHEN WC-PAY-MONTHLY
                   MOVE "PAY101" TO WS-TEXT-CD
               WHEN WC-PAY-HALF
                   MOVE "PAY201" TO WS-TEXT-CD
               WHEN WC-PAY-YEARLY
                   MOVE "PAY301" TO WS-TEXT-CD
               WHEN WC-PAY-LUMP
                   MOVE "PAY401" TO WS-TEXT-CD
               WHEN OTHER
                   MOVE "払込方法区分不正" TO WS-REASON
                   DISPLAY WS-REASON
                   MOVE WC-YES TO WS-HARD-ERROR
           END-EVALUATE.

       5900-OLD-CODE-COMPAT SECTION.
      *    旧帳票連携では通知状態８の再印字に廃止コードを返す。
      *    受領側更改完了まで残す互換処理であり、新規通知では使わない。
           IF RV-NOTICE-STATUS-KBN = WC-STS-OLD-COMPAT
               EVALUATE WS-TEXT-CD
                   WHEN "RVI001"
                       MOVE "RVU900" TO WS-TEXT-CD
                   WHEN "RVD001"
                       MOVE "RVD900" TO WS-TEXT-CD
                   WHEN "RVN001"
                       MOVE "RVZ900" TO WS-TEXT-CD
                   WHEN "PAY101"
                       MOVE "PYM900" TO WS-TEXT-CD
                   WHEN "PAY201"
                       MOVE "PYH900" TO WS-TEXT-CD
                   WHEN "PAY301"
                       MOVE "PYY900" TO WS-TEXT-CD
                   WHEN "PAY401"
                       MOVE "PYL900" TO WS-TEXT-CD
                   WHEN OTHER
                       CONTINUE
               END-EVALUATE
           END-IF

           IF RV-NOTICE-STATUS-KBN = WC-STS-REISSUE
               EVALUATE WS-TEXT-CD
                   WHEN "MTY001"
                       MOVE "MTY701" TO WS-TEXT-CD
                   WHEN "ADR001"
                       MOVE "ADR701" TO WS-TEXT-CD
                   WHEN OTHER
                       CONTINUE
               END-EVALUATE
           END-IF.

       8000-CLOSE-FILES SECTION.
           IF WS-LFCNTF-ST NOT = SPACE
               CLOSE LFCNTF
               IF WS-LFCNTF-ST NOT = "00"
                   DISPLAY "LFCNTF クローズ失敗 ST=" WS-LFCNTF-ST
                   MOVE WC-YES TO WS-HARD-ERROR
                   MOVE "LFCNTF クローズ失敗" TO WS-REASON
               END-IF
           END-IF

           IF WS-LFRVSF-ST NOT = SPACE
               CLOSE LFRVSF
               IF WS-LFRVSF-ST NOT = "00"
                   DISPLAY "LFRVSF クローズ失敗 ST=" WS-LFRVSF-ST
                   MOVE WC-YES TO WS-HARD-ERROR
                   MOVE "LFRVSF クローズ失敗" TO WS-REASON
               END-IF
           END-IF.
