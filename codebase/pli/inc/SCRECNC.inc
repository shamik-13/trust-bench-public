 /*================================================================*/
 /* SCRECNC -- SCRECN レコードレイアウト (record layout). Shared/pinned. */
 /*================================================================*/
 DCL 1 SCRECN_REC,
       3 RC_RECON_ID        CHAR(10),
       3 RC_SOURCE_KBN      CHAR(02),
       3 RC_TARGET_KBN      CHAR(02),
       3 RC_BUS_DT          FIXED DEC(8,0),
       3 RC_REF_ID          CHAR(10),
       3 RC_DIFF_AMT        FIXED DEC(13,2),
       3 RC_DIFF_QTY        FIXED DEC(8,0),
       3 RC_RESULT_KBN      CHAR(02);
