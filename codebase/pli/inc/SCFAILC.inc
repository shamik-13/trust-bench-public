 /*================================================================*/
 /* SCFAILC -- SCFAIL レコードレイアウト (record layout). Shared/pinned. */
 /*================================================================*/
 DCL 1 SCFAIL_REC,
       3 FL_FAIL_ID         CHAR(10),
       3 FL_SETL_ID         CHAR(10),
       3 FL_CIF_NO          CHAR(16),
       3 FL_INSTR_CODE      CHAR(04),
       3 FL_SETL_DT         FIXED DEC(8,0),
       3 FL_FAIL_QTY        FIXED DEC(8,0),
       3 FL_FAIL_AMT        FIXED DEC(13,2),
       3 FL_REASON_KBN      CHAR(02),
       3 FL_AGE_DAYS        CHAR(10),
       3 FL_STATUS_KBN      CHAR(02);
