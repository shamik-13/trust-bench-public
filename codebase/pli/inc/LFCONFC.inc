 /*================================================================*/
 /* LFCONFC -- LFCONF レコードレイアウト (record layout). Shared/pinned. */
 /*================================================================*/
 DCL 1 LFCONF_REC,
       3 CN_CON_NO          CHAR(16),
       3 CN_PAY_MODE_KBN    CHAR(02),
       3 CN_DUE_YM          CHAR(10),
       3 CN_PAID_KBN        CHAR(02),
       3 CN_RESP_START_DT   FIXED DEC(8,0),
       3 CN_CON_STATUS_KBN  CHAR(02);
