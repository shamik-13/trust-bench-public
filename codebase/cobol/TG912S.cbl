       IDENTIFICATION DIVISION.
       PROGRAM-ID. TG912S.
      *================================================================*
      *  変更履歴                                                      *
      *    版数    年月日    担当      概要                            *
      *    0.01    令和08年04月10日 システム部 為替・対外接続チーム    *
      *            初版起票                                            *
      *    0.02    令和08年05月18日 システム部 為替・対外接続チーム    *
      *            法人略語表追加                                      *
      *    0.03    令和08年06月12日 システム部 為替・対外接続チーム    *
      *            全銀カナ正規化処理実装                              *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-WORK.
           05 WS-SRC-KANA              PIC X(512).
           05 WS-TMP-KANA              PIC X(512).
           05 WS-OUT-KANA              PIC X(512).
           05 WS-SRC-LEN               PIC 9(04) COMP.
           05 WS-IDX                   PIC 9(04) COMP.
           05 WS-OUT-IDX               PIC 9(04) COMP.
           05 WS-BYTE                  PIC X(01).
           05 WS-PREV-SPACE            PIC X(01).
              88 WS-PREV-SPACE-ON      VALUE '1'.
              88 WS-PREV-SPACE-OFF     VALUE '0'.
           05 WS-HARD-ERR              PIC X(01).
              88 WS-HARD-ERR-ON        VALUE '1'.
              88 WS-HARD-ERR-OFF       VALUE '0'.

       LINKAGE SECTION.
       COPY LK-KANA-PARM.

       PROCEDURE DIVISION USING LK-KANA-PARM.
       0000-MAIN.
           MOVE 0                         TO RETURN-CODE
           SET WS-HARD-ERR-OFF            TO TRUE
           PERFORM 1000-INIT
           IF LK-KANA-RET = '08'
              GOBACK
           END-IF
           PERFORM 2000-HOJIN-FOLD
           PERFORM 3000-ZENGIN-CONVERT
           PERFORM 4000-SPACE-EDIT
           PERFORM 5000-SET-RESULT
           IF WS-HARD-ERR-ON
              MOVE 12                     TO RETURN-CODE
           END-IF
           GOBACK.

       1000-INIT.
           MOVE SPACES                    TO WS-SRC-KANA
           MOVE SPACES                    TO WS-TMP-KANA
           MOVE SPACES                    TO WS-OUT-KANA
           MOVE LK-RAW-KANA               TO WS-SRC-KANA
           IF FUNCTION TRIM(WS-SRC-KANA) = SPACES
              MOVE SPACES                 TO LK-NORM-KANA
              MOVE '08'                   TO LK-KANA-RET
           ELSE
              MOVE '00'                   TO LK-KANA-RET
           END-IF.

       2000-HOJIN-FOLD.
           MOVE WS-SRC-KANA               TO WS-TMP-KANA
           MOVE FUNCTION SUBSTITUTE(
                WS-TMP-KANA
                '特定非営利活動法人' 'ﾄｸﾋ')
                                           TO WS-SRC-KANA
           MOVE FUNCTION SUBSTITUTE(
                WS-SRC-KANA
                '社会福祉法人' 'ﾌｸ')
                                           TO WS-SRC-KANA
           MOVE FUNCTION SUBSTITUTE(
                WS-SRC-KANA
                '株式会社' 'ｶ'
                '有限会社' 'ﾕ'
                '合名会社' 'ﾒ'
                '合資会社' 'ｼ'
                '合同会社' 'ﾄﾞ')
                                           TO WS-SRC-KANA
           MOVE FUNCTION SUBSTITUTE(
                WS-SRC-KANA
                '医療法人' 'ｲ'
                '学校法人' 'ｶﾞｸ'
                '宗教法人' 'ｼﾕｳ'
                '財団法人' 'ｻﾞｲ'
                '社団法人' 'ｼﾔ')
                                           TO WS-SRC-KANA.

       3000-ZENGIN-CONVERT.
           MOVE FUNCTION SUBSTITUTE(
                WS-SRC-KANA
                '　' ' '
                '，' ','
                '．' '.'
                '。' '.'
                '、' ','
                '・' '.'
                '／' '/'
                '－' '-')
                                           TO WS-SRC-KANA
           MOVE FUNCTION SUBSTITUTE(
                WS-SRC-KANA
                'ー' '-'
                '（' '('
                '）' ')'
                '「' '('
                '」' ')'
                '￥' '\'
                '＋' '+'
                '＊' '*')
                                           TO WS-SRC-KANA
           MOVE FUNCTION SUBSTITUTE(
                WS-SRC-KANA
                '＆' '&'
                '％' '%'
                '＃' '#'
                '＠' '@'
                '！' '!'
                '？' '?'
                '：' ':'
                '；' ';')
                                           TO WS-SRC-KANA
           MOVE FUNCTION SUBSTITUTE(
                WS-SRC-KANA
                '０' '0' '１' '1' '２' '2' '３' '3')
                                           TO WS-SRC-KANA
           MOVE FUNCTION SUBSTITUTE(
                WS-SRC-KANA
                '４' '4' '５' '5' '６' '6' '７' '7')
                                           TO WS-SRC-KANA
           MOVE FUNCTION SUBSTITUTE(
                WS-SRC-KANA
                '８' '8' '９' '9')
                                           TO WS-SRC-KANA
           MOVE FUNCTION SUBSTITUTE(
                WS-SRC-KANA
                'Ａ' 'A' 'Ｂ' 'B' 'Ｃ' 'C' 'Ｄ' 'D')
                                           TO WS-SRC-KANA
           MOVE FUNCTION SUBSTITUTE(
                WS-SRC-KANA
                'Ｅ' 'E' 'Ｆ' 'F' 'Ｇ' 'G' 'Ｈ' 'H')
                                           TO WS-SRC-KANA
           MOVE FUNCTION SUBSTITUTE(
                WS-SRC-KANA
                'Ｉ' 'I' 'Ｊ' 'J' 'Ｋ' 'K' 'Ｌ' 'L')
                                           TO WS-SRC-KANA
           MOVE FUNCTION SUBSTITUTE(
                WS-SRC-KANA
                'Ｍ' 'M' 'Ｎ' 'N' 'Ｏ' 'O' 'Ｐ' 'P')
                                           TO WS-SRC-KANA
           MOVE FUNCTION SUBSTITUTE(
                WS-SRC-KANA
                'Ｑ' 'Q' 'Ｒ' 'R' 'Ｓ' 'S' 'Ｔ' 'T')
                                           TO WS-SRC-KANA
           MOVE FUNCTION SUBSTITUTE(
                WS-SRC-KANA
                'Ｕ' 'U' 'Ｖ' 'V' 'Ｗ' 'W' 'Ｘ' 'X')
                                           TO WS-SRC-KANA
           MOVE FUNCTION SUBSTITUTE(
                WS-SRC-KANA
                'Ｙ' 'Y' 'Ｚ' 'Z')
                                           TO WS-SRC-KANA
           MOVE FUNCTION SUBSTITUTE(
                WS-SRC-KANA
                'ａ' 'A' 'ｂ' 'B' 'ｃ' 'C' 'ｄ' 'D')
                                           TO WS-SRC-KANA
           MOVE FUNCTION SUBSTITUTE(
                WS-SRC-KANA
                'ｅ' 'E' 'ｆ' 'F' 'ｇ' 'G' 'ｈ' 'H')
                                           TO WS-SRC-KANA
           MOVE FUNCTION SUBSTITUTE(
                WS-SRC-KANA
                'ｉ' 'I' 'ｊ' 'J' 'ｋ' 'K' 'ｌ' 'L')
                                           TO WS-SRC-KANA
           MOVE FUNCTION SUBSTITUTE(
                WS-SRC-KANA
                'ｍ' 'M' 'ｎ' 'N' 'ｏ' 'O' 'ｐ' 'P')
                                           TO WS-SRC-KANA
           MOVE FUNCTION SUBSTITUTE(
                WS-SRC-KANA
                'ｑ' 'Q' 'ｒ' 'R' 'ｓ' 'S' 'ｔ' 'T')
                                           TO WS-SRC-KANA
           MOVE FUNCTION SUBSTITUTE(
                WS-SRC-KANA
                'ｕ' 'U' 'ｖ' 'V' 'ｗ' 'W' 'ｘ' 'X')
                                           TO WS-SRC-KANA
           MOVE FUNCTION SUBSTITUTE(
                WS-SRC-KANA
                'ｙ' 'Y' 'ｚ' 'Z')
                                           TO WS-SRC-KANA
           MOVE FUNCTION UPPER-CASE(WS-SRC-KANA)
                                           TO WS-SRC-KANA
           MOVE FUNCTION SUBSTITUTE(
                WS-SRC-KANA
                'ガ' 'ｶﾞ' 'ギ' 'ｷﾞ')
                                           TO WS-SRC-KANA
           MOVE FUNCTION SUBSTITUTE(
                WS-SRC-KANA
                'グ' 'ｸﾞ' 'ゲ' 'ｹﾞ')
                                           TO WS-SRC-KANA
           MOVE FUNCTION SUBSTITUTE(
                WS-SRC-KANA
                'ゴ' 'ｺﾞ' 'ザ' 'ｻﾞ')
                                           TO WS-SRC-KANA
           MOVE FUNCTION SUBSTITUTE(
                WS-SRC-KANA
                'ジ' 'ｼﾞ' 'ズ' 'ｽﾞ')
                                           TO WS-SRC-KANA
           MOVE FUNCTION SUBSTITUTE(
                WS-SRC-KANA
                'ゼ' 'ｾﾞ' 'ゾ' 'ｿﾞ')
                                           TO WS-SRC-KANA
           MOVE FUNCTION SUBSTITUTE(
                WS-SRC-KANA
                'ダ' 'ﾀﾞ' 'ヂ' 'ﾁﾞ')
                                           TO WS-SRC-KANA
           MOVE FUNCTION SUBSTITUTE(
                WS-SRC-KANA
                'ヅ' 'ﾂﾞ' 'デ' 'ﾃﾞ')
                                           TO WS-SRC-KANA
           MOVE FUNCTION SUBSTITUTE(
                WS-SRC-KANA
                'ド' 'ﾄﾞ' 'バ' 'ﾊﾞ')
                                           TO WS-SRC-KANA
           MOVE FUNCTION SUBSTITUTE(
                WS-SRC-KANA
                'ビ' 'ﾋﾞ' 'ブ' 'ﾌﾞ')
                                           TO WS-SRC-KANA
           MOVE FUNCTION SUBSTITUTE(
                WS-SRC-KANA
                'ベ' 'ﾍﾞ' 'ボ' 'ﾎﾞ')
                                           TO WS-SRC-KANA
           MOVE FUNCTION SUBSTITUTE(
                WS-SRC-KANA
                'パ' 'ﾊﾟ' 'ピ' 'ﾋﾟ')
                                           TO WS-SRC-KANA
           MOVE FUNCTION SUBSTITUTE(
                WS-SRC-KANA
                'プ' 'ﾌﾟ' 'ペ' 'ﾍﾟ')
                                           TO WS-SRC-KANA
           MOVE FUNCTION SUBSTITUTE(
                WS-SRC-KANA
                'ポ' 'ﾎﾟ' 'ヴ' 'ｳﾞ')
                                           TO WS-SRC-KANA
           MOVE FUNCTION SUBSTITUTE(
                WS-SRC-KANA
                'ア' 'ｱ' 'イ' 'ｲ' 'ウ' 'ｳ' 'エ' 'ｴ')
                                           TO WS-SRC-KANA
           MOVE FUNCTION SUBSTITUTE(
                WS-SRC-KANA
                'オ' 'ｵ' 'カ' 'ｶ' 'キ' 'ｷ' 'ク' 'ｸ')
                                           TO WS-SRC-KANA
           MOVE FUNCTION SUBSTITUTE(
                WS-SRC-KANA
                'ケ' 'ｹ' 'コ' 'ｺ' 'サ' 'ｻ' 'シ' 'ｼ')
                                           TO WS-SRC-KANA
           MOVE FUNCTION SUBSTITUTE(
                WS-SRC-KANA
                'ス' 'ｽ' 'セ' 'ｾ' 'ソ' 'ｿ' 'タ' 'ﾀ')
                                           TO WS-SRC-KANA
           MOVE FUNCTION SUBSTITUTE(
                WS-SRC-KANA
                'チ' 'ﾁ' 'ツ' 'ﾂ' 'テ' 'ﾃ' 'ト' 'ﾄ')
                                           TO WS-SRC-KANA
           MOVE FUNCTION SUBSTITUTE(
                WS-SRC-KANA
                'ナ' 'ﾅ' 'ニ' 'ﾆ' 'ヌ' 'ﾇ' 'ネ' 'ﾈ')
                                           TO WS-SRC-KANA
           MOVE FUNCTION SUBSTITUTE(
                WS-SRC-KANA
                'ノ' 'ﾉ' 'ハ' 'ﾊ' 'ヒ' 'ﾋ' 'フ' 'ﾌ')
                                           TO WS-SRC-KANA
           MOVE FUNCTION SUBSTITUTE(
                WS-SRC-KANA
                'ヘ' 'ﾍ' 'ホ' 'ﾎ' 'マ' 'ﾏ' 'ミ' 'ﾐ')
                                           TO WS-SRC-KANA
           MOVE FUNCTION SUBSTITUTE(
                WS-SRC-KANA
                'ム' 'ﾑ' 'メ' 'ﾒ' 'モ' 'ﾓ' 'ヤ' 'ﾔ')
                                           TO WS-SRC-KANA
           MOVE FUNCTION SUBSTITUTE(
                WS-SRC-KANA
                'ユ' 'ﾕ' 'ヨ' 'ﾖ' 'ラ' 'ﾗ' 'リ' 'ﾘ')
                                           TO WS-SRC-KANA
           MOVE FUNCTION SUBSTITUTE(
                WS-SRC-KANA
                'ル' 'ﾙ' 'レ' 'ﾚ' 'ロ' 'ﾛ' 'ワ' 'ﾜ')
                                           TO WS-SRC-KANA
           MOVE FUNCTION SUBSTITUTE(
                WS-SRC-KANA
                'ヲ' 'ｦ' 'ン' 'ﾝ' 'ァ' 'ｧ' 'ィ' 'ｨ')
                                           TO WS-SRC-KANA
           MOVE FUNCTION SUBSTITUTE(
                WS-SRC-KANA
                'ゥ' 'ｩ' 'ェ' 'ｪ' 'ォ' 'ｫ' 'ャ' 'ｬ')
                                           TO WS-SRC-KANA
           MOVE FUNCTION SUBSTITUTE(
                WS-SRC-KANA
                'ュ' 'ｭ' 'ョ' 'ｮ' 'ッ' 'ｯ' 'ヮ' 'ﾜ')
                                           TO WS-SRC-KANA
           MOVE FUNCTION SUBSTITUTE(
                WS-SRC-KANA
                'ヰ' 'ｲ' 'ヱ' 'ｴ' '゛' 'ﾞ' '゜' 'ﾟ')
                                           TO WS-SRC-KANA
           MOVE FUNCTION SUBSTITUTE(
                WS-SRC-KANA
                'ｶ゛' 'ｶﾞ' 'ｷ゛' 'ｷﾞ')
                                           TO WS-SRC-KANA
           MOVE FUNCTION SUBSTITUTE(
                WS-SRC-KANA
                'ｸ゛' 'ｸﾞ' 'ｹ゛' 'ｹﾞ')
                                           TO WS-SRC-KANA
           MOVE FUNCTION SUBSTITUTE(
                WS-SRC-KANA
                'ｺ゛' 'ｺﾞ' 'ｻ゛' 'ｻﾞ')
                                           TO WS-SRC-KANA
           MOVE FUNCTION SUBSTITUTE(
                WS-SRC-KANA
                'ｼ゛' 'ｼﾞ' 'ｽ゛' 'ｽﾞ')
                                           TO WS-SRC-KANA
           MOVE FUNCTION SUBSTITUTE(
                WS-SRC-KANA
                'ｾ゛' 'ｾﾞ' 'ｿ゛' 'ｿﾞ')
                                           TO WS-SRC-KANA
           MOVE FUNCTION SUBSTITUTE(
                WS-SRC-KANA
                'ﾀ゛' 'ﾀﾞ' 'ﾁ゛' 'ﾁﾞ')
                                           TO WS-SRC-KANA
           MOVE FUNCTION SUBSTITUTE(
                WS-SRC-KANA
                'ﾂ゛' 'ﾂﾞ' 'ﾃ゛' 'ﾃﾞ')
                                           TO WS-SRC-KANA
           MOVE FUNCTION SUBSTITUTE(
                WS-SRC-KANA
                'ﾄ゛' 'ﾄﾞ' 'ﾊ゛' 'ﾊﾞ')
                                           TO WS-SRC-KANA
           MOVE FUNCTION SUBSTITUTE(
                WS-SRC-KANA
                'ﾋ゛' 'ﾋﾞ' 'ﾌ゛' 'ﾌﾞ')
                                           TO WS-SRC-KANA
           MOVE FUNCTION SUBSTITUTE(
                WS-SRC-KANA
                'ﾍ゛' 'ﾍﾞ' 'ﾎ゛' 'ﾎﾞ')
                                           TO WS-SRC-KANA
           MOVE FUNCTION SUBSTITUTE(
                WS-SRC-KANA
                'ﾊ゜' 'ﾊﾟ' 'ﾋ゜' 'ﾋﾟ')
                                           TO WS-SRC-KANA
           MOVE FUNCTION SUBSTITUTE(
                WS-SRC-KANA
                'ﾌ゜' 'ﾌﾟ' 'ﾍ゜' 'ﾍﾟ')
                                           TO WS-SRC-KANA
           MOVE FUNCTION SUBSTITUTE(
                WS-SRC-KANA
                'ﾎ゜' 'ﾎﾟ' 'ｳ゛' 'ｳﾞ')
                                           TO WS-SRC-KANA.

       4000-SPACE-EDIT.
           MOVE SPACES                    TO WS-OUT-KANA
           MOVE 1                         TO WS-OUT-IDX
           SET WS-PREV-SPACE-ON           TO TRUE
           COMPUTE WS-SRC-LEN =
               FUNCTION LENGTH(FUNCTION TRIM(WS-SRC-KANA TRAILING))
           PERFORM VARYING WS-IDX FROM 1 BY 1
                   UNTIL WS-IDX > WS-SRC-LEN
              MOVE WS-SRC-KANA(WS-IDX:1)  TO WS-BYTE
              IF WS-BYTE = SPACE
                 IF WS-PREV-SPACE-OFF
                    IF WS-OUT-IDX <= LENGTH OF WS-OUT-KANA
                       MOVE SPACE         TO WS-OUT-KANA(WS-OUT-IDX:1)
                       ADD 1              TO WS-OUT-IDX
                    ELSE
                       SET WS-HARD-ERR-ON TO TRUE
                    END-IF
                 END-IF
                 SET WS-PREV-SPACE-ON     TO TRUE
              ELSE
                 IF WS-OUT-IDX <= LENGTH OF WS-OUT-KANA
                    MOVE WS-BYTE          TO WS-OUT-KANA(WS-OUT-IDX:1)
                    ADD 1                 TO WS-OUT-IDX
                    SET WS-PREV-SPACE-OFF TO TRUE
                 ELSE
                    SET WS-HARD-ERR-ON    TO TRUE
                 END-IF
              END-IF
           END-PERFORM
           IF WS-OUT-IDX > 1
              SUBTRACT 1                  FROM WS-OUT-IDX
              IF WS-OUT-KANA(WS-OUT-IDX:1) = SPACE
                 MOVE SPACE               TO WS-OUT-KANA(WS-OUT-IDX:1)
              END-IF
           END-IF.

       5000-SET-RESULT.
           MOVE SPACES                    TO LK-NORM-KANA
           MOVE FUNCTION TRIM(WS-OUT-KANA TRAILING)
                                           TO LK-NORM-KANA
           MOVE '00'                      TO LK-KANA-RET.
