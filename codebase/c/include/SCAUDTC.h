/* SCAUDTC.h -- SCAUDTF record layout (shared/pinned). org VSAM-ESDS. */
#ifndef SCAUDTC_H
#define SCAUDTC_H
#include <stdint.h>

typedef struct scaudtf_rec {
    char     ad_audit_id[11];
    char     ad_event_ts[15];
    char     ad_service_id[11];
    char     ad_object_id[11];
    char     ad_event_kbn[3];
    char     ad_detail_code[5];
} scaudtf_rec_t;

#endif /* SCAUDTC_H */
