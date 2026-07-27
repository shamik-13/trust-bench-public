/* SCAUDF2C.h -- SCAUDF2 record layout (shared/pinned). org VSAM-ESDS. */
#ifndef SCAUDF2C_H
#define SCAUDF2C_H
#include <stdint.h>

typedef struct scaudf2_rec {
    char     aud_audit_id[11];
    char     aud_actor_id[11];
    char     aud_action_kbn[3];
    char     aud_object_id[11];
    char     aud_result_code[5];
    char     aud_audit_ts[15];
} scaudf2_rec_t;

#endif /* SCAUDF2C_H */
