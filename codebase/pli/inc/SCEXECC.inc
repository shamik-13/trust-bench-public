 /*================================================================*/
 /* SCEXECC -- SCEXEC レコードレイアウト (record layout). Shared/pinned. */
 /*================================================================*/
 DCL 1 SCEXEC_REC,
       3 EX_EXEC_ID         CHAR(10),
       3 EX_ORDER_ID        CHAR(10),
       3 EX_CIF_NO          CHAR(16),
       3 EX_INSTR_CODE      CHAR(04),
       3 EX_SIDE_KBN        CHAR(02),
       3 EX_FILL_QTY        FIXED DEC(8,0),
       3 EX_FILL_AMT        FIXED DEC(13,2),
       3 EX_EXEC_TS         CHAR(14),
       3 EX_TRADE_DT        FIXED DEC(8,0);
