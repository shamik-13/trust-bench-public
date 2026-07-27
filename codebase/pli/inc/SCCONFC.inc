 /*================================================================*/
 /* SCCONFC -- SCCONF レコードレイアウト (record layout). Shared/pinned. */
 /*================================================================*/
 DCL 1 SCCONF_REC,
       3 CF_CONF_ID         CHAR(10),
       3 CF_EXEC_ID         CHAR(10),
       3 CF_CIF_NO          CHAR(16),
       3 CF_INSTR_CODE      CHAR(04),
       3 CF_SIDE_KBN        CHAR(02),
       3 CF_CONF_AMT        FIXED DEC(13,2),
       3 CF_CONF_TS         CHAR(14),
       3 CF_MATCH_KBN       CHAR(02),
       3 CF_ERROR_KBN       CHAR(02);
