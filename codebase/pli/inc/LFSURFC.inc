 /*================================================================*/
 /* LFSURFC -- LFSURF レコードレイアウト (record layout). Shared/pinned. */
 /*================================================================*/
 DCL 1 LFSURF_REC,
       3 SU_CON_NO          CHAR(16),
       3 SU_SURR_VAL_AMT    FIXED DEC(13,2),
       3 SU_CALC_DT         FIXED DEC(8,0),
       3 SU_SURR_STATUS_KBN CHAR(02),
       3 SU_EXEC_DT         FIXED DEC(8,0),
       3 SU_SURR_REASON_KBN CHAR(02);
