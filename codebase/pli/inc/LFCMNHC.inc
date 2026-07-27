 /*================================================================*/
 /* LFCMNHC -- LFCMNH レコードレイアウト (record layout). Shared/pinned. */
 /*================================================================*/
 DCL 1 LFCMNH_REC,
       3 CH_CON_NO          CHAR(16),
       3 CH_CHG_DT          FIXED DEC(8,0),
       3 CH_CHG_TYPE_KBN    CHAR(02),
       3 CH_CHG_ITEM_KBN    CHAR(02),
       3 CH_OLD_VAL         CHAR(10),
       3 CH_NEW_VAL         CHAR(10),
       3 CH_OPE_ID          CHAR(10),
       3 CH_CHG_REASON_KBN  CHAR(02);
