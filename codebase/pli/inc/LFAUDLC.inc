 /*================================================================*/
 /* LFAUDLC -- LFAUDL レコードレイアウト (record layout). Shared/pinned. */
 /*================================================================*/
 DCL 1 LFAUDL_REC,
       3 AU_SEQ_NO          CHAR(16),
       3 AU_CON_NO          CHAR(16),
       3 AU_OPE_DT          FIXED DEC(8,0),
       3 AU_OPE_TIME        CHAR(10),
       3 AU_OPE_TYPE_KBN    CHAR(02),
       3 AU_OPE_ID          CHAR(10),
       3 AU_BEFORE_STATUS   CHAR(02),
       3 AU_AFTER_STATUS    CHAR(02),
       3 AU_RESULT_KBN      CHAR(02);
