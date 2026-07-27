 /*================================================================*/
 /* SCPOSIC -- SCPOSI レコードレイアウト (record layout). Shared/pinned. */
 /*================================================================*/
 DCL 1 SCPOSI_REC,
       3 PS_POS_KEY         CHAR(10),
       3 PS_CIF_NO          CHAR(16),
       3 PS_INSTR_CODE      CHAR(04),
       3 PS_BASE_DT         FIXED DEC(8,0),
       3 PS_SETTLED_QTY     FIXED DEC(8,0),
       3 PS_PENDING_IN_QTY  FIXED DEC(8,0),
       3 PS_PENDING_OUT_QTY FIXED DEC(8,0),
       3 PS_LOCK_QTY        FIXED DEC(8,0),
       3 PS_STATUS_KBN      CHAR(02);
