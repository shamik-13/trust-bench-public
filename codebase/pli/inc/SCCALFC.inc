 /*================================================================*/
 /* SCCALFC -- SCCALF レコードレイアウト (record layout). Shared/pinned. */
 /*================================================================*/
 DCL 1 SCCALF_REC,
       3 CA_SESS_DT         FIXED DEC(8,0),
       3 CA_SESS_KBN        CHAR(02),
       3 CA_OPEN_TS         CHAR(14),
       3 CA_CLOSE_TS        CHAR(14);
