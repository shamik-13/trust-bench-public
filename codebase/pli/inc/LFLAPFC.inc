 /*================================================================*/
 /* LFLAPFC -- LFLAPF レコードレイアウト (record layout). Shared/pinned. */
 /*================================================================*/
 DCL 1 LFLAPF_REC,
       3 LP_CON_NO          CHAR(16),
       3 LP_LAPSE_DT        FIXED DEC(8,0),
       3 LP_GRACE_END_DT    FIXED DEC(8,0),
       3 LP_REINSTATE_DUE_DT FIXED DEC(8,0),
       3 LP_LAPSE_PHASE_KBN CHAR(02),   /* 失効ライフサイクル: 01=猶予中 02=失効中 03=復活済 */
       3 LP_REINST_STATUS_KBN CHAR(02); /* 復活可否: (空白)=未判定 A=復活可能 X=復活不可 */
