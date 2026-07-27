 /*================================================================*/
 /* LFBNCFC -- LFBNCF レコードレイアウト (record layout). Shared/pinned. */
 /*================================================================*/
 DCL 1 LFBNCF_REC,
       3 BN_CON_NO          CHAR(16),
       3 BN_BNC_SEQ         CHAR(10),
       3 BN_BNC_NAME_KANA   CHAR(40),
       3 BN_BNC_DOB         CHAR(10),
       3 BN_BNC_REL_KBN     CHAR(02),
       3 BN_BNC_RATIO       CHAR(10),
       3 BN_BNC_STATUS_KBN  CHAR(02);
