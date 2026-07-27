 /*================================================================*/
 /* LFBILFC -- LFBILF レコードレイアウト (record layout). Shared/pinned. */
 /*================================================================*/
 DCL 1 LFBILF_REC,
       3 LB_BIL_NO          CHAR(16),
       3 LB_CON_NO          CHAR(16),
       3 LB_BIL_YM          CHAR(10),
       3 LB_BIL_AMT         FIXED DEC(13,2),
       3 LB_DUE_DT          FIXED DEC(8,0),
       3 LB_BIL_STATUS_KBN  CHAR(02),
       3 LB_BIL_MODE_KBN    CHAR(02),
       3 LB_NOTIFY_DT       FIXED DEC(8,0);
