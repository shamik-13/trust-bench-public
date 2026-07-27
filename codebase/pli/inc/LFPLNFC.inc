 /*================================================================*/
 /* LFPLNFC -- LFPLNF レコードレイアウト (record layout). Shared/pinned. */
 /*================================================================*/
 DCL 1 LFPLNF_REC,
       3 PL_CON_NO          CHAR(16),
       3 PL_PLN_PRINCIPAL_AMT FIXED DEC(13,2),
       3 PL_PLN_INT_RATE    FIXED DEC(5,4),
       3 PL_PLN_START_DT    FIXED DEC(8,0),
       3 PL_PLN_LAST_INT_DT FIXED DEC(8,0),
       3 PL_PLN_TOTAL_INT_AMT FIXED DEC(13,2),
       3 PL_PLN_STATUS_KBN  CHAR(02);
