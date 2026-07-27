/* PSRPTFC.h -- PSRPTF record layout (shared/pinned). org 順編成. */
#ifndef PSRPTFC_H
#define PSRPTFC_H
#include <stdint.h>

typedef struct psrptf_rec {
    char     rpt_report_id[11];
    char     rpt_merchant_code[5];
    char     rpt_report_kbn[3];
    char     rpt_period_from[11];
    char     rpt_period_to[11];
    char     rpt_output_path[11];
    char     rpt_create_status[3];
} psrptf_rec_t;

#endif /* PSRPTFC_H */
