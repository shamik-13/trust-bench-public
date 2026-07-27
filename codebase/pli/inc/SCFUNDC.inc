 /*================================================================*/
 /* SCFUNDC -- SCFUND レコードレイアウト (record layout). Shared/pinned. */
 /*================================================================*/
 DCL 1 SCFUND_REC,
       3 FD_FUND_KEY        CHAR(10),
       3 FD_CIF_NO          CHAR(16),
       3 FD_CCY_KBN         CHAR(02),
       3 FD_VALUE_DT        FIXED DEC(8,0),
       3 FD_REQ_AMT         FIXED DEC(13,2),
       3 FD_ALLOC_AMT       FIXED DEC(13,2),
       3 FD_SHORT_AMT       FIXED DEC(13,2),
       3 FD_STATUS_KBN      CHAR(02);
