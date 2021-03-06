CREATE TABLE tbl_syutotal (
    nyugaikbn character(1) NOT NULL,
    ptid bigint NOT NULL,
    denpnum integer NOT NULL,
    sryka character(2),
    douji_sryka1 character(2),
    douji_sryka2 character(2),
    douji_sryka3 character(2),
    douji_sryka4 character(2),
    douji_sryka5 character(2),
    douji_sryka6 character(2),
    douji_sryka7 character(2),
    douji_sryka8 character(2),
    douji_sryka9 character(2),
    douji_sryka10 character(2),
    sryymd character(8),
    denpjtikbn character(1),
    createkbn character(1),
    denpprtymd character(8),
    skystymd character(8),
    skyedymd character(8),
    hkncombinum character(4),
    syuhknnum character(3),
    syuhknftnmoney integer,
    syucompftn integer,
    syucompftn_entani numeric(8,1),
    syucompykzftn integer,
    syucomptotal integer,
    syuykzftnkbn character(1),
    koh1hknnum character(3),
    koh1hknftnmoney integer,
    koh1compftn integer,
    koh1compftn_entani numeric(8,1),
    koh1compykzftn integer,
    koh1comptotal integer,
    koh1ykzftnkbn character(1),
    koh2hknnum character(3),
    koh2hknftnmoney integer,
    koh2compftn integer,
    koh2compftn_entani numeric(8,1),
    koh2compykzftn integer,
    koh2comptotal integer,
    koh2ykzftnkbn character(1),
    koh3hknnum character(3),
    koh3hknftnmoney integer,
    koh3compftn integer,
    koh3compftn_entani numeric(8,1),
    koh3compykzftn integer,
    koh3comptotal integer,
    koh3ykzftnkbn character(1),
    koh4hknnum character(3),
    koh4hknftnmoney integer,
    koh4compftn integer,
    koh4compftn_entani numeric(8,1),
    koh4compykzftn integer,
    koh4comptotal integer,
    koh4ykzftnkbn character(1),
    ptftnrate smallint,
    skykukbn character(1),
    ssu_hknten integer,
    ssu_money integer,
    ssu_tgmoney integer,
    ssu_tgmoney_tax integer,
    sdo_hknten integer,
    sdo_money integer,
    sdo_tgmoney integer,
    sdo_tgmoney_tax integer,
    ztk_hknten integer,
    ztk_money integer,
    ztk_tgmoney integer,
    ztk_tgmoney_tax integer,
    tyk_hknten integer,
    tyk_money integer,
    tyk_tgmoney integer,
    tyk_tgmoney_tax integer,
    csy_hknten integer,
    csy_money integer,
    csy_tgmoney integer,
    csy_tgmoney_tax integer,
    syc_hknten integer,
    syc_money integer,
    syc_tgmoney integer,
    syc_tgmoney_tax integer,
    sjt_hknten integer,
    sjt_money integer,
    sjt_tgmoney integer,
    sjt_tgmoney_tax integer,
    kns_hknten integer,
    kns_money integer,
    kns_tgmoney integer,
    kns_tgmoney_tax integer,
    gzu_hknten integer,
    gzu_money integer,
    gzu_tgmoney integer,
    gzu_tgmoney_tax integer,
    etc_hknten integer,
    etc_money integer,
    etc_tgmoney integer,
    etc_tgmoney_tax integer,
    nyn_hknten integer,
    nyn_money integer,
    nyn_tgmoney integer,
    nyn_tgmoney_tax integer,
    total_hknten integer,
    total_money integer,
    total_tgmoney integer,
    total_tgmoney_tax integer,
    tgmoney_tax_sai integer,
    jihi_1 integer,
    jihi_1_tax integer,
    jihi_2 integer,
    jihi_2_tax integer,
    jihi_3 integer,
    jihi_3_tax integer,
    jihi_4 integer,
    jihi_4_tax integer,
    jihi_5 integer,
    jihi_5_tax integer,
    jihi_6 integer DEFAULT 0,
    jihi_6_tax integer DEFAULT 0,
    jihi_7 integer DEFAULT 0,
    jihi_7_tax integer DEFAULT 0,
    jihi_8 integer DEFAULT 0,
    jihi_8_tax integer DEFAULT 0,
    jihi_9 integer DEFAULT 0,
    jihi_9_tax integer DEFAULT 0,
    jihi_10 integer DEFAULT 0,
    jihi_10_tax integer DEFAULT 0,
    jihi_total integer,
    jihi_total_tax integer,
    jihi_tax integer,
    rjn_ftn integer,
    koh_ftn integer,
    koh_ftn_entani numeric(8,1),
    chosei integer,
    grp_sgkmoney integer,
    hkntekmoney integer,
    discount_kbn character(2),
    discount_body character varying(160),
    discount_ratekbn character(2),
    discount_teiritu character(1),
    discount_rate integer,
    discount_money integer DEFAULT 0,
    rsishoshin_money integer,
    rsisaishin_money integer,
    rsisdo_money integer,
    rsietc_money integer,
    rsi_tax_sai integer,
    rsi_total integer,
    rsijibai_skymoney integer,
    skymoney_tax_sai integer,
    skymoney integer,
    nyukin_total integer,
    nyukin_kaisu integer,
    misyuriyu character(2),
    zaitaku character(1),
    kyufugai_shoshin_ten integer,
    kyufugai_sidou_ten integer,
    kyufugai_osin1_ten integer,
    kyufugai_osin2_ten integer,
    kyufugai_osin3_ten integer,
    kyufugai_gokei_ten integer,
    termid character varying(16),
    opid character varying(16),
    creymd character(8),
    upymd character(8),
    uphms character(6),
    msi_hknten integer DEFAULT 0,
    msi_money integer DEFAULT 0,
    msi_tgmoney integer DEFAULT 0,
    msi_tgmoney_tax integer DEFAULT 0,
    ssn_hknten integer DEFAULT 0,
    ssn_money integer DEFAULT 0,
    ssn_tgmoney integer DEFAULT 0,
    ssn_tgmoney_tax integer DEFAULT 0,
    hou_hknten integer DEFAULT 0,
    hou_money integer DEFAULT 0,
    hou_tgmoney integer DEFAULT 0,
    hou_tgmoney_tax integer DEFAULT 0,
    ryo_hknten integer DEFAULT 0,
    ryo_money integer DEFAULT 0,
    ryo_tgmoney integer DEFAULT 0,
    ryo_tgmoney_tax integer DEFAULT 0,
    shohou_sai integer DEFAULT 0,
    hospnum smallint NOT NULL,
    byr_hknten integer DEFAULT 0,
    byr_money integer DEFAULT 0,
    byr_tgmoney integer DEFAULT 0,
    byr_tgmoney_tax integer DEFAULT 0,
    chosei1 integer DEFAULT 0,
    chosei2 integer DEFAULT 0,
    hkntekkbn character(1)
);

ALTER TABLE ONLY tbl_syutotal
    ADD CONSTRAINT tbl_syutotal_primary_key PRIMARY KEY (hospnum, nyugaikbn, ptid, denpnum);

