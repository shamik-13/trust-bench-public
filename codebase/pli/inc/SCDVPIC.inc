 /*================================================================*/
 /* SCDVPIC -- SCDVPI レコードレイアウト (record layout). Shared/pinned. */
 /*================================================================*/
 DCL 1 SCDVPI_REC,
       3 DV_DVP_ID          CHAR(10),
       3 DV_SETL_ID         CHAR(10),
       3 DV_CIF_NO          CHAR(16),
       3 DV_INSTR_CODE      CHAR(04),
       3 DV_SETL_DT         FIXED DEC(8,0),
       3 DV_DELIVER_QTY     FIXED DEC(8,0),
       3 DV_PAY_AMT         FIXED DEC(13,2),
       3 DV_MATCH_STATUS    CHAR(02),
       3 DV_ROUTE_KBN       CHAR(02);
