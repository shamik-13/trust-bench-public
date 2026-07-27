 /*================================================================*/
 /* SCCACT2C -- SCCACT2 レコードレイアウト (record layout). Shared/pinned. */
 /*================================================================*/
 DCL 1 SCCACT2_REC,
       3 AC_ACTION_ID       CHAR(10),
       3 AC_INSTR_CODE      CHAR(04),
       3 AC_BASE_DT         FIXED DEC(8,0),
       3 AC_PAY_DT          FIXED DEC(8,0),
       3 AC_ACTION_KBN      CHAR(02),
       3 AC_RATE_DEC        FIXED DEC(5,4),
       3 AC_CASH_AMT        FIXED DEC(13,2),
       3 AC_STATUS_KBN      CHAR(02);
