 /*================================================================*/
 /* LFDIVF2C -- LFDIVF2 レコードレイアウト (record layout). Shared/pinned. */
 /*================================================================*/
 DCL 1 LFDIVF2_REC,
       3 DV_CON_NO          CHAR(16),
       3 DV_DIV_YR          CHAR(10),
       3 DV_DIV_AMT         FIXED DEC(13,2),
       3 DV_DIV_METHOD_KBN  CHAR(02),
       3 DV_DIV_APPLY_DT    FIXED DEC(8,0),
       3 DV_DIV_STATUS_KBN  CHAR(02);
