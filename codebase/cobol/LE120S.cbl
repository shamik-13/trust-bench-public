       IDENTIFICATION DIVISION.
       PROGRAM-ID. LE120S.
       AUTHOR. KEIRI-BATCH.
      ******************************************************************
      * 勘定科目変換サブルーチン
      * イベント区分、商品コード、支払区分から借方・貸方科目を返す。
      * 本処理は経理イベント分類のみを扱い、保険数理計算および
      * 返戻金算出は行わない。
      ******************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-WORK.
           05  WS-VALID-SW          PIC X VALUE SPACE.
               88  WS-VALID              VALUE '1'.
               88  WS-INVALID            VALUE '0'.
           05  WS-FOUND-SW          PIC X VALUE SPACE.
               88  WS-FOUND              VALUE '1'.
               88  WS-NOT-FOUND          VALUE '0'.
           05  WS-MSG               PIC X(40) VALUE SPACE.
       LINKAGE SECTION.
       01  LK-LE120S-PARM.
           05  LK-IN-EVENT-KBN      PIC X(02).
           05  LK-IN-SHOHIN-CD      PIC X(03).
           05  LK-IN-SHIHARAI-KBN   PIC X(01).
           05  LK-OUT-KARIKATA-CD   PIC X(06).
           05  LK-OUT-KASHIKATA-CD  PIC X(06).
           05  LK-OUT-STATUS        PIC X(02).
           05  LK-OUT-RIYU          PIC X(40).

       PROCEDURE DIVISION USING LK-LE120S-PARM.
       MAIN-SECTION.
           PERFORM 1000-SHOKI-SHORI
           PERFORM 2000-NYURYOKU-CHECK
           IF WS-VALID
              PERFORM 3000-KAMOKU-HENKAN
           END-IF
           PERFORM 9000-SHURYO-SHORI
           GOBACK
           .

       1000-SHOKI-SHORI.
           MOVE 0        TO RETURN-CODE
           MOVE SPACES   TO LK-OUT-KARIKATA-CD
                            LK-OUT-KASHIKATA-CD
                            LK-OUT-RIYU
                            WS-MSG
           MOVE '00'     TO LK-OUT-STATUS
           SET WS-INVALID   TO TRUE
           SET WS-NOT-FOUND TO TRUE
           .

       2000-NYURYOKU-CHECK.
           EVALUATE TRUE
             WHEN LK-IN-EVENT-KBN NOT = '01'
              AND LK-IN-EVENT-KBN NOT = '02'
              AND LK-IN-EVENT-KBN NOT = '03'
              AND LK-IN-EVENT-KBN NOT = '04'
              AND LK-IN-EVENT-KBN NOT = '05'
              AND LK-IN-EVENT-KBN NOT = '06'
                MOVE 'イベント区分不正' TO WS-MSG
             WHEN LK-IN-SHOHIN-CD NOT = '101'
              AND LK-IN-SHOHIN-CD NOT = '102'
              AND LK-IN-SHOHIN-CD NOT = '201'
              AND LK-IN-SHOHIN-CD NOT = '301'
                MOVE '商品コード不正' TO WS-MSG
             WHEN LK-IN-SHIHARAI-KBN NOT = '1'
              AND LK-IN-SHIHARAI-KBN NOT = '2'
              AND LK-IN-SHIHARAI-KBN NOT = '3'
                MOVE '支払区分不正' TO WS-MSG
             WHEN OTHER
                SET WS-VALID TO TRUE
           END-EVALUATE
           IF WS-INVALID
              MOVE '04'   TO LK-OUT-STATUS
              MOVE WS-MSG TO LK-OUT-RIYU
           END-IF
           .

       3000-KAMOKU-HENKAN.
           EVALUATE LK-IN-EVENT-KBN ALSO LK-IN-SHOHIN-CD
             WHEN '01' ALSO '101'
                MOVE '111010' TO LK-OUT-KARIKATA-CD
                MOVE '411101' TO LK-OUT-KASHIKATA-CD
                SET WS-FOUND TO TRUE
             WHEN '01' ALSO '102'
                MOVE '111010' TO LK-OUT-KARIKATA-CD
                MOVE '411102' TO LK-OUT-KASHIKATA-CD
                SET WS-FOUND TO TRUE
             WHEN '01' ALSO '201'
                MOVE '111020' TO LK-OUT-KARIKATA-CD
                MOVE '411201' TO LK-OUT-KASHIKATA-CD
                SET WS-FOUND TO TRUE
             WHEN '01' ALSO '301'
                MOVE '111030' TO LK-OUT-KARIKATA-CD
                MOVE '411301' TO LK-OUT-KASHIKATA-CD
                SET WS-FOUND TO TRUE
             WHEN '02' ALSO '101'
                MOVE '131101' TO LK-OUT-KARIKATA-CD
                MOVE '512101' TO LK-OUT-KASHIKATA-CD
                SET WS-FOUND TO TRUE
             WHEN '02' ALSO '102'
                MOVE '131102' TO LK-OUT-KARIKATA-CD
                MOVE '512102' TO LK-OUT-KASHIKATA-CD
                SET WS-FOUND TO TRUE
             WHEN '02' ALSO '201'
                MOVE '131201' TO LK-OUT-KARIKATA-CD
                MOVE '512201' TO LK-OUT-KASHIKATA-CD
                SET WS-FOUND TO TRUE
             WHEN '02' ALSO '301'
                MOVE '131301' TO LK-OUT-KARIKATA-CD
                MOVE '512301' TO LK-OUT-KASHIKATA-CD
                SET WS-FOUND TO TRUE
             WHEN '03' ALSO '101'
                MOVE '611101' TO LK-OUT-KARIKATA-CD
                PERFORM 3100-SHIHARAI-KASHI
                SET WS-FOUND TO TRUE
             WHEN '03' ALSO '102'
                MOVE '611102' TO LK-OUT-KARIKATA-CD
                PERFORM 3100-SHIHARAI-KASHI
                SET WS-FOUND TO TRUE
             WHEN '03' ALSO '201'
                MOVE '611201' TO LK-OUT-KARIKATA-CD
                PERFORM 3100-SHIHARAI-KASHI
                SET WS-FOUND TO TRUE
             WHEN '03' ALSO '301'
                MOVE '611301' TO LK-OUT-KARIKATA-CD
                PERFORM 3100-SHIHARAI-KASHI
                SET WS-FOUND TO TRUE
             WHEN '04' ALSO '101'
                MOVE '621101' TO LK-OUT-KARIKATA-CD
                PERFORM 3100-SHIHARAI-KASHI
                SET WS-FOUND TO TRUE
             WHEN '04' ALSO '102'
                MOVE '621102' TO LK-OUT-KARIKATA-CD
                PERFORM 3100-SHIHARAI-KASHI
                SET WS-FOUND TO TRUE
             WHEN '04' ALSO '201'
                MOVE '621201' TO LK-OUT-KARIKATA-CD
                PERFORM 3100-SHIHARAI-KASHI
                SET WS-FOUND TO TRUE
             WHEN '04' ALSO '301'
                MOVE '621301' TO LK-OUT-KARIKATA-CD
                PERFORM 3100-SHIHARAI-KASHI
                SET WS-FOUND TO TRUE
             WHEN '05' ALSO '101'
                MOVE '411101' TO LK-OUT-KARIKATA-CD
                MOVE '211101' TO LK-OUT-KASHIKATA-CD
                SET WS-FOUND TO TRUE
             WHEN '05' ALSO '102'
                MOVE '411102' TO LK-OUT-KARIKATA-CD
                MOVE '211102' TO LK-OUT-KASHIKATA-CD
                SET WS-FOUND TO TRUE
             WHEN '05' ALSO '201'
                MOVE '411201' TO LK-OUT-KARIKATA-CD
                MOVE '211201' TO LK-OUT-KASHIKATA-CD
                SET WS-FOUND TO TRUE
             WHEN '05' ALSO '301'
                MOVE '411301' TO LK-OUT-KARIKATA-CD
                MOVE '211301' TO LK-OUT-KASHIKATA-CD
                SET WS-FOUND TO TRUE
             WHEN '06' ALSO '101'
                MOVE '611101' TO LK-OUT-KARIKATA-CD
                MOVE '214101' TO LK-OUT-KASHIKATA-CD
                SET WS-FOUND TO TRUE
             WHEN '06' ALSO '102'
                MOVE '611102' TO LK-OUT-KARIKATA-CD
                MOVE '214102' TO LK-OUT-KASHIKATA-CD
                SET WS-FOUND TO TRUE
             WHEN '06' ALSO '201'
                MOVE '611201' TO LK-OUT-KARIKATA-CD
                MOVE '214201' TO LK-OUT-KASHIKATA-CD
                SET WS-FOUND TO TRUE
             WHEN '06' ALSO '301'
                MOVE '611301' TO LK-OUT-KARIKATA-CD
                MOVE '214301' TO LK-OUT-KASHIKATA-CD
                SET WS-FOUND TO TRUE
             WHEN OTHER
                MOVE '変換対象なし' TO WS-MSG
           END-EVALUATE
           IF WS-NOT-FOUND
              MOVE '04'   TO LK-OUT-STATUS
              MOVE WS-MSG TO LK-OUT-RIYU
           END-IF
           .

       3100-SHIHARAI-KASHI.
           EVALUATE LK-IN-SHIHARAI-KBN
             WHEN '1'
                MOVE '111010' TO LK-OUT-KASHIKATA-CD
             WHEN '2'
                MOVE '112010' TO LK-OUT-KASHIKATA-CD
             WHEN '3'
                MOVE '113010' TO LK-OUT-KASHIKATA-CD
           END-EVALUATE
           .

       9000-SHURYO-SHORI.
           IF LK-OUT-STATUS = '00'
              MOVE '正常終了' TO LK-OUT-RIYU
              MOVE 0 TO RETURN-CODE
           ELSE
              MOVE 0 TO RETURN-CODE
           END-IF
           .
