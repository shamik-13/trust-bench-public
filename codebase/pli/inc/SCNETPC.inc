 /*================================================================*/
 /* SCNETPC -- SCNETP レコードレイアウト (record layout). Shared/pinned. */
 /*================================================================*/
 DCL 1 SCNETP_REC,
       3 NP_NET_KEY         CHAR(10),
       3 NP_CIF_NO          CHAR(16),
       3 NP_INSTR_CODE      CHAR(04),
       3 NP_SETL_DT         FIXED DEC(8,0),
       3 NP_NET_QTY         FIXED DEC(8,0),
       3 NP_NET_AMT         FIXED DEC(13,2),
       3 NP_BUY_AMT         FIXED DEC(13,2),
       3 NP_SELL_AMT        FIXED DEC(13,2),
       3 NP_NET_KBN         CHAR(02),
       3 NP_STATUS_KBN      CHAR(02);
