 /*================================================================*/
 /* LFSTMFC -- LFSTMF レコードレイアウト (record layout). Shared/pinned. */
 /*================================================================*/
 DCL 1 LFSTMF_REC,
       3 ST_CON_NO          CHAR(16),
       3 ST_STM_YM          CHAR(10),
       3 ST_STM_TYPE_KBN    CHAR(02),
       3 ST_PRINT_DT        FIXED DEC(8,0),
       3 ST_DELIVERY_KBN    CHAR(02),
       3 ST_STM_STATUS_KBN  CHAR(02);
