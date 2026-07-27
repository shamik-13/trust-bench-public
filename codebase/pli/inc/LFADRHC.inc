 /*================================================================*/
 /* LFADRHC -- LFADRH レコードレイアウト (record layout). Shared/pinned. */
 /*================================================================*/
 DCL 1 LFADRH_REC,
       3 AD_CON_NO          CHAR(16),
       3 AD_CHG_DT          FIXED DEC(8,0),
       3 AD_OLD_ZIP         CHAR(10),
       3 AD_OLD_ADDR1       CHAR(10),
       3 AD_OLD_ADDR2       CHAR(10),
       3 AD_NEW_ZIP         CHAR(10),
       3 AD_NEW_ADDR1       CHAR(10),
       3 AD_NEW_ADDR2       CHAR(10),
       3 AD_OPE_ID          CHAR(10);
