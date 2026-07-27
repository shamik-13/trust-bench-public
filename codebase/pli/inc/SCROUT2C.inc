 /*================================================================*/
 /* SCROUT2C -- SCROUT2 レコードレイアウト (record layout). Shared/pinned. */
 /*================================================================*/
 DCL 1 SCROUT2_REC,
       3 RT_ROUTE_KEY       CHAR(10),
       3 RT_INSTR_CODE      CHAR(04),
       3 RT_SETL_KBN        CHAR(02),
       3 RT_CCY_KBN         CHAR(02),
       3 RT_DEST_KBN        CHAR(02),
       3 RT_PRIORITY_NO     CHAR(16),
       3 RT_ACTIVE_KBN      CHAR(02);
