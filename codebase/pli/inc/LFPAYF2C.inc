 /*================================================================*/
 /* LFPAYF2C -- LFPAYF2 レコードレイアウト (record layout). Shared/pinned. */
 /*================================================================*/
 DCL 1 LFPAYF2_REC,
       3 PY_PAY_SEQ_NO      CHAR(16),
       3 PY_CON_NO          CHAR(16),
       3 PY_PAY_DT          FIXED DEC(8,0),
       3 PY_PAY_AMT         FIXED DEC(13,2),
       3 PY_PAY_METHOD_KBN  CHAR(02),
       3 PY_APPLY_YM        CHAR(10),
       3 PY_CLEAR_KBN       CHAR(02),
       3 PY_BIL_NO          CHAR(16);
