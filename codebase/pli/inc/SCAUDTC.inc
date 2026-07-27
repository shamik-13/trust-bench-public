 /*================================================================*/
 /* SCAUDTC -- SCAUDT レコードレイアウト (record layout). Shared/pinned. */
 /*================================================================*/
 DCL 1 SCAUDT_REC,
       3 AU_AUDIT_ID        CHAR(10),
       3 AU_PGM_ID          CHAR(10),
       3 AU_BUS_DT          FIXED DEC(8,0),
       3 AU_REF_ID          CHAR(10),
       3 AU_EVENT_KBN       CHAR(02),
       3 AU_BEFORE_STATUS   CHAR(02),
       3 AU_AFTER_STATUS    CHAR(02),
       3 AU_OP_TS           CHAR(14);
