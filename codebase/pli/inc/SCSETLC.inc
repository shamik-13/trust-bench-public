 /*================================================================*/
 /* SCSETLC -- SCSETL レコードレイアウト (record layout). Shared/pinned. */
 /*================================================================*/
 DCL 1 SCSETL_REC,
       3 ST_SETL_ID         CHAR(10),
       3 ST_EXEC_ID         CHAR(10),
       3 ST_CIF_NO          CHAR(16),
       3 ST_INSTR_CODE      CHAR(04),
       3 ST_TRADE_DT        FIXED DEC(8,0),
       3 ST_SETL_DT         FIXED DEC(8,0),
       3 ST_SETL_AMT        FIXED DEC(13,2),
       3 ST_SETL_KBN        CHAR(02),
       3 ST_STATUS_KBN      CHAR(02);
