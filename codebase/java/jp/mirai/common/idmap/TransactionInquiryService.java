package jp.mirai.common.idmap;

public class TransactionInquiryService {
    private static final java.nio.charset.Charset 出力文字コード = java.nio.charset.StandardCharsets.UTF_8;

    private TransactionInquiryService() {
    }

    public static void main(String[] a) {
        try {
            照会要求 要求 = 照会要求.解析(a);
            照会結果 結果 = new TransactionInquiryService().照会する(要求);
            System.out.print(結果.表示文字列());
        } catch (IllegalArgumentException e) {
            System.err.println("入力不備: " + e.getMessage());
            System.err.println("使用方法: java jp.mirai.common.idmap.TransactionInquiryService COMPANY-CODE=BK LOCAL-TXN-NO=1000000001");
            System.err.println("使用方法: java jp.mirai.common.idmap.TransactionInquiryService AUDIT-ID=AU-BK-0001");
            System.err.println("使用方法: java jp.mirai.common.idmap.TransactionInquiryService GROUP-REF-NO=1010000000001");
            System.exit(2);
        }
    }

    private 照会結果 照会する(照会要求 要求) {
        台帳 ledger = 台帳.標準データ();
        java.util.List<取引明細> 取引候補 = new java.util.ArrayList<取引明細>();
        java.util.List<監査リンク> 監査候補 = new java.util.ArrayList<監査リンク>();
        java.util.List<ジャーナル> ジャーナル候補 = new java.util.ArrayList<ジャーナル>();
        java.util.List<名寄せ状態> 名寄せ候補 = new java.util.ArrayList<名寄せ状態>();

        if (要求.groupRefNo != null) {
            監査候補.addAll(new AuditLinkService(ledger.監査リンク一覧).逆引き(要求.groupRefNo.longValue()));
        } else if (要求.auditId != null) {
            for (監査リンク 監査 : ledger.監査リンク一覧) {
                if (監査.auditId.equals(要求.auditId)) {
                    監査候補.add(監査);
                }
            }
        } else {
            for (監査リンク 監査 : ledger.監査リンク一覧) {
                if (監査.companyCode.equals(要求.companyCode) && 監査.localTxnNo == 要求.localTxnNo.longValue()) {
                    監査候補.add(監査);
                }
            }
        }

        java.util.Set<String> 監査キー = new java.util.LinkedHashSet<String>();
        java.util.Set<String> ローカルキー = new java.util.LinkedHashSet<String>();
        java.util.Set<Long> グループキー = new java.util.LinkedHashSet<Long>();

        for (監査リンク 監査 : 監査候補) {
            監査キー.add(監査.auditId);
            ローカルキー.add(監査.companyCode + "\u0000" + 監査.localTxnNo);
            グループキー.add(Long.valueOf(監査.groupRefNo));
        }

        if (要求.auditId == null && 要求.groupRefNo == null && ローカルキー.isEmpty()) {
            ローカルキー.add(要求.companyCode + "\u0000" + 要求.localTxnNo);
        }

        for (取引明細 取引 : ledger.取引明細一覧) {
            if (ローカルキー.contains(取引.companyCode + "\u0000" + 取引.localTxnNo)) {
                取引候補.add(取引);
            }
        }
        for (ジャーナル 明細 : ledger.ジャーナル一覧) {
            if (監査キー.contains(明細.auditId) || グループキー.contains(Long.valueOf(明細.groupRefNo))) {
                ジャーナル候補.add(明細);
            }
        }
        for (名寄せ状態 名寄せ : ledger.名寄せ状態一覧) {
            if (ローカルキー.contains(名寄せ.companyCode + "\u0000" + 名寄せ.localTxnNo)) {
                名寄せ候補.add(名寄せ);
            }
        }

        java.util.Collections.sort(取引候補);
        java.util.Collections.sort(監査候補);
        java.util.Collections.sort(ジャーナル候補);
        java.util.Collections.sort(名寄せ候補);
        return new 照会結果(要求, 取引候補, 監査候補, ジャーナル候補, 名寄せ候補);
    }

    private static final class 照会要求 {
        private final String companyCode;
        private final Long localTxnNo;
        private final String auditId;
        private final Long groupRefNo;

        private 照会要求(String companyCode, Long localTxnNo, String auditId, Long groupRefNo) {
            this.companyCode = companyCode;
            this.localTxnNo = localTxnNo;
            this.auditId = auditId;
            this.groupRefNo = groupRefNo;
        }

        private static 照会要求 解析(String[] 引数) {
            java.util.Map<String, String> 値 = new java.util.LinkedHashMap<String, String>();
            for (String 要素 : 引数) {
                int 位置 = 要素.indexOf('=');
                if (位置 <= 0 || 位置 == 要素.length() - 1) {
                    throw new IllegalArgumentException("条件は項目=値で指定してください: " + 要素);
                }
                値.put(要素.substring(0, 位置).trim().toUpperCase(java.util.Locale.ROOT), 要素.substring(位置 + 1).trim());
            }

            boolean 会社取引指定 = 値.containsKey("COMPANY-CODE") || 値.containsKey("LOCAL-TXN-NO");
            boolean 監査指定 = 値.containsKey("AUDIT-ID");
            boolean グループ指定 = 値.containsKey("GROUP-REF-NO");
            int 条件数 = (会社取引指定 ? 1 : 0) + (監査指定 ? 1 : 0) + (グループ指定 ? 1 : 0);
            if (条件数 != 1) {
                throw new IllegalArgumentException("会社コードとLOCAL-TXN-NO、AUDIT-ID、GROUP-REF-NOのいずれか一種類を指定してください");
            }

            if (会社取引指定) {
                String 会社 = 必須(値, "COMPANY-CODE");
                long ローカル番号 = 正数(必須(値, "LOCAL-TXN-NO"), "LOCAL-TXN-NO");
                表示名.会社(会社);
                return new 照会要求(会社, Long.valueOf(ローカル番号), null, null);
            }
            if (監査指定) {
                return new 照会要求(null, null, 必須(値, "AUDIT-ID"), null);
            }
            return new 照会要求(null, null, null, Long.valueOf(正数(必須(値, "GROUP-REF-NO"), "GROUP-REF-NO")));
        }

        private static String 必須(java.util.Map<String, String> 値, String 名前) {
            String 内容 = 値.get(名前);
            if (内容 == null || 内容.length() == 0) {
                throw new IllegalArgumentException(名前 + "が未指定です");
            }
            return 内容;
        }

        private static long 正数(String 値, String 名前) {
            try {
                long 数値 = Long.parseLong(値);
                if (数値 <= 0L) {
                    throw new IllegalArgumentException(名前 + "は正の数で指定してください");
                }
                return 数値;
            } catch (NumberFormatException e) {
                throw new IllegalArgumentException(名前 + "は数値で指定してください");
            }
        }
    }

    private static final class 照会結果 {
        private final 照会要求 要求;
        private final java.util.List<取引明細> 取引明細一覧;
        private final java.util.List<監査リンク> 監査リンク一覧;
        private final java.util.List<ジャーナル> ジャーナル一覧;
        private final java.util.List<名寄せ状態> 名寄せ状態一覧;

        private 照会結果(照会要求 要求, java.util.List<取引明細> 取引明細一覧,
                java.util.List<監査リンク> 監査リンク一覧, java.util.List<ジャーナル> ジャーナル一覧,
                java.util.List<名寄せ状態> 名寄せ状態一覧) {
            this.要求 = 要求;
            this.取引明細一覧 = 取引明細一覧;
            this.監査リンク一覧 = 監査リンク一覧;
            this.ジャーナル一覧 = ジャーナル一覧;
            this.名寄せ状態一覧 = 名寄せ状態一覧;
        }

        private String 表示文字列() {
            StringBuilder sb = new StringBuilder();
            sb.append("取引照会結果").append('\n');
            sb.append("照会条件: ").append(条件表示()).append('\n');
            sb.append("取引明細件数: ").append(取引明細一覧.size()).append('\n');
            for (取引明細 取引 : 取引明細一覧) {
                sb.append("CMTXNF TXN-ID=").append(取引.txnId)
                        .append(" COMPANY-CODE=").append(取引.companyCode)
                        .append("(").append(表示名.会社(取引.companyCode)).append(")")
                        .append(" LOCAL-TXN-NO=").append(取引.localTxnNo)
                        .append(" TXN-AMT=").append(取引.txnAmt)
                        .append(" TXN-STATUS-KBN=").append(取引.txnStatusKbn)
                        .append("(").append(表示名.取引状態(取引.txnStatusKbn)).append(")")
                        .append('\n');
            }

            sb.append("監査リンク件数: ").append(監査リンク一覧.size()).append('\n');
            for (監査リンク 監査 : 監査リンク一覧) {
                sb.append("CMAUDF AUDIT-ID=").append(監査.auditId)
                        .append(" GROUP-REF-NO=").append(監査.groupRefNo)
                        .append(" COMPANY-CODE=").append(監査.companyCode)
                        .append("(").append(表示名.会社(監査.companyCode)).append(")")
                        .append(" LOCAL-TXN-NO=").append(監査.localTxnNo)
                        .append(" AUDIT-STATUS-KBN=").append(監査.auditStatusKbn)
                        .append("(").append(表示名.監査状態(監査.auditStatusKbn)).append(")")
                        .append('\n');
            }

            sb.append("ジャーナル件数: ").append(ジャーナル一覧.size()).append('\n');
            for (ジャーナル 明細 : ジャーナル一覧) {
                sb.append("CAJRNF JOURNAL-SEQ=").append(明細.journalSeq)
                        .append(" AUDIT-ID=").append(明細.auditId)
                        .append(" GROUP-REF-NO=").append(明細.groupRefNo)
                        .append(" EVENT-TYPE-KBN=").append(明細.eventTypeKbn)
                        .append("(").append(表示名.事象区分(明細.eventTypeKbn)).append(")")
                        .append(" JOURNAL-STATUS-KBN=").append(明細.journalStatusKbn)
                        .append("(").append(表示名.ジャーナル状態(明細.journalStatusKbn)).append(")")
                        .append('\n');
            }

            sb.append("名寄せ件数: ").append(名寄せ状態一覧.size()).append('\n');
            for (名寄せ状態 名寄せ : 名寄せ状態一覧) {
                sb.append("CXIDMF IDMAP-KEY=").append(名寄せ.idmapKey)
                        .append(" COMPANY-CODE=").append(名寄せ.companyCode)
                        .append("(").append(表示名.会社(名寄せ.companyCode)).append(")")
                        .append(" LOCAL-TXN-NO=").append(名寄せ.localTxnNo)
                        .append(" CUSTOMER-ALIAS-ID=").append(名寄せ.customerAliasId)
                        .append(" LINK-STATUS-KBN=").append(名寄せ.linkStatusKbn)
                        .append("(").append(表示名.名寄せ状態(名寄せ.linkStatusKbn)).append(")")
                        .append('\n');
            }
            return new String(sb.toString().getBytes(出力文字コード), 出力文字コード);
        }

        private String 条件表示() {
            if (要求.groupRefNo != null) {
                return "GROUP-REF-NO=" + 要求.groupRefNo;
            }
            if (要求.auditId != null) {
                return "AUDIT-ID=" + 要求.auditId;
            }
            return "COMPANY-CODE=" + 要求.companyCode + " LOCAL-TXN-NO=" + 要求.localTxnNo;
        }
    }

    private static final class AuditLinkService {
        private final java.util.List<監査リンク> 監査リンク一覧;

        private AuditLinkService(java.util.List<監査リンク> 監査リンク一覧) {
            this.監査リンク一覧 = 監査リンク一覧;
        }

        private java.util.List<監査リンク> 逆引き(long groupRefNo) {
            java.util.List<監査リンク> 結果 = new java.util.ArrayList<監査リンク>();
            for (監査リンク 監査 : 監査リンク一覧) {
                if (監査.groupRefNo == groupRefNo) {
                    結果.add(監査);
                }
            }
            return 結果;
        }
    }

    private static final class 台帳 {
        private final java.util.List<取引明細> 取引明細一覧;
        private final java.util.List<監査リンク> 監査リンク一覧;
        private final java.util.List<ジャーナル> ジャーナル一覧;
        private final java.util.List<名寄せ状態> 名寄せ状態一覧;

        private 台帳(java.util.List<取引明細> 取引明細一覧, java.util.List<監査リンク> 監査リンク一覧,
                java.util.List<ジャーナル> ジャーナル一覧, java.util.List<名寄せ状態> 名寄せ状態一覧) {
            this.取引明細一覧 = 取引明細一覧;
            this.監査リンク一覧 = 監査リンク一覧;
            this.ジャーナル一覧 = ジャーナル一覧;
            this.名寄せ状態一覧 = 名寄せ状態一覧;
        }

        private static 台帳 標準データ() {
            return new 台帳(
                    java.util.Arrays.asList(
                            new 取引明細("TX-BK-000001", "BK", 1000000001L, 1250000L, "01"),
                            new 取引明細("TX-SC-000142", "SC", 2000000142L, 8400000L, "01"),
                            new 取引明細("TX-CD-008801", "CD", 3000008801L, 19800L, "09"),
                            new 取引明細("TX-PY-019940", "PY", 4000019940L, 6200L, "01"),
                            new 取引明細("TX-LF-000077", "LF", 5000000077L, 430000L, "01")),
                    java.util.Arrays.asList(
                            new 監査リンク("AU-BK-0001", 1010000000001L, "BK", 1000000001L, "10"),
                            new 監査リンク("AU-SC-0142", 2020000000142L, "SC", 2000000142L, "10"),
                            new 監査リンク("AU-CD-8801", 3030000008801L, "CD", 3000008801L, "90"),
                            new 監査リンク("AU-PY-9940", 4040000019940L, "PY", 4000019940L, "10"),
                            new 監査リンク("AU-LF-0077", 5050000000077L, "LF", 5000000077L, "10")),
                    java.util.Arrays.asList(
                            new ジャーナル(82000001L, "AU-BK-0001", 1010000000001L, "01", "20"),
                            new ジャーナル(82000002L, "AU-BK-0001", 1010000000001L, "02", "20"),
                            new ジャーナル(82000142L, "AU-SC-0142", 2020000000142L, "01", "20"),
                            new ジャーナル(82008801L, "AU-CD-8801", 3030000008801L, "03", "91"),
                            new ジャーナル(82019940L, "AU-PY-9940", 4040000019940L, "01", "20"),
                            new ジャーナル(82000077L, "AU-LF-0077", 5050000000077L, "01", "20")),
                    java.util.Arrays.asList(
                            new 名寄せ状態("BK:1000000001", "BK", 1000000001L, "AL-BK-778812", "1"),
                            new 名寄せ状態("SC:2000000142", "SC", 2000000142L, "AL-SC-018420", "1"),
                            new 名寄せ状態("CD:3000008801", "CD", 3000008801L, "AL-CD-552901", "9"),
                            new 名寄せ状態("PY:4000019940", "PY", 4000019940L, "AL-PY-104477", "1"),
                            new 名寄せ状態("LF:5000000077", "LF", 5000000077L, "AL-LF-331205", "1")));
        }
    }

    private static final class 取引明細 implements Comparable<取引明細> {
        private final String txnId;
        private final String companyCode;
        private final long localTxnNo;
        private final long txnAmt;
        private final String txnStatusKbn;

        private 取引明細(String txnId, String companyCode, long localTxnNo, long txnAmt, String txnStatusKbn) {
            this.txnId = txnId;
            this.companyCode = companyCode;
            this.localTxnNo = localTxnNo;
            this.txnAmt = txnAmt;
            this.txnStatusKbn = txnStatusKbn;
        }

        public int compareTo(取引明細 他) {
            return this.txnId.compareTo(他.txnId);
        }
    }

    private static final class 監査リンク implements Comparable<監査リンク> {
        private final String auditId;
        private final long groupRefNo;
        private final String companyCode;
        private final long localTxnNo;
        private final String auditStatusKbn;

        private 監査リンク(String auditId, long groupRefNo, String companyCode, long localTxnNo, String auditStatusKbn) {
            this.auditId = auditId;
            this.groupRefNo = groupRefNo;
            this.companyCode = companyCode;
            this.localTxnNo = localTxnNo;
            this.auditStatusKbn = auditStatusKbn;
        }

        public int compareTo(監査リンク 他) {
            return this.auditId.compareTo(他.auditId);
        }
    }

    private static final class ジャーナル implements Comparable<ジャーナル> {
        private final long journalSeq;
        private final String auditId;
        private final long groupRefNo;
        private final String eventTypeKbn;
        private final String journalStatusKbn;

        private ジャーナル(long journalSeq, String auditId, long groupRefNo, String eventTypeKbn, String journalStatusKbn) {
            this.journalSeq = journalSeq;
            this.auditId = auditId;
            this.groupRefNo = groupRefNo;
            this.eventTypeKbn = eventTypeKbn;
            this.journalStatusKbn = journalStatusKbn;
        }

        public int compareTo(ジャーナル 他) {
            return Long.compare(this.journalSeq, 他.journalSeq);
        }
    }

    private static final class 名寄せ状態 implements Comparable<名寄せ状態> {
        private final String idmapKey;
        private final String companyCode;
        private final long localTxnNo;
        private final String customerAliasId;
        private final String linkStatusKbn;

        private 名寄せ状態(String idmapKey, String companyCode, long localTxnNo, String customerAliasId, String linkStatusKbn) {
            this.idmapKey = idmapKey;
            this.companyCode = companyCode;
            this.localTxnNo = localTxnNo;
            this.customerAliasId = customerAliasId;
            this.linkStatusKbn = linkStatusKbn;
        }

        public int compareTo(名寄せ状態 他) {
            return this.idmapKey.compareTo(他.idmapKey);
        }
    }

    private static final class 表示名 {
        private 表示名() {
        }

        private static String 会社(String 値) {
            if ("BK".equals(値)) return "みらい信託銀行";
            if ("SC".equals(値)) return "みらい証券";
            if ("CD".equals(値)) return "みらいカード";
            if ("PY".equals(値)) return "みらいペイ";
            if ("LF".equals(値)) return "みらい生命";
            if ("CM".equals(値)) return "MFG共通基盤";
            throw new IllegalArgumentException("会社コードが未定義です: " + 値);
        }

        private static String 取引状態(String 値) {
            if ("01".equals(値)) return "確定";
            if ("09".equals(値)) return "取消";
            return "未定義";
        }

        private static String 監査状態(String 値) {
            if ("10".equals(値)) return "連携済";
            if ("90".equals(値)) return "取消済";
            return "未定義";
        }

        private static String 事象区分(String 値) {
            if ("01".equals(値)) return "受付";
            if ("02".equals(値)) return "確定";
            if ("03".equals(値)) return "取消";
            return "未定義";
        }

        private static String ジャーナル状態(String 値) {
            if ("20".equals(値)) return "記録済";
            if ("91".equals(値)) return "取消記録";
            return "未定義";
        }

        private static String 名寄せ状態(String 値) {
            if ("1".equals(値)) return "有効";
            if ("9".equals(値)) return "無効";
            return "未定義";
        }
    }
}
