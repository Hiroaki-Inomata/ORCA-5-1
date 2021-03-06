\set ON_ERROR_STOP

CREATE TABLE tbl_seikyu_new (
    sryym integer DEFAULT 0 NOT NULL,
    nyugaikbn character(1) NOT NULL,
    ptid numeric(10,0) DEFAULT 0 NOT NULL,
    receka character(2) NOT NULL,
    teisyutusaki smallint DEFAULT 0 NOT NULL,
    recesyubetu character(4) NOT NULL,
    hknjanum character(8) NOT NULL,
    hojokbn_key character(1) NOT NULL,
    tekstymd character(8) NOT NULL,
    ptnum character(20) NOT NULL,
    prefkbn character(1) DEFAULT '0'::bpchar,
    prefnum character(2) DEFAULT '00'::bpchar,
    age smallint DEFAULT 0,
    kyurate smallint DEFAULT 0,
    zaitaku character(1) DEFAULT ''::bpchar,
    skyym character(6),
    henreiym character(6),
    skykbn character(1),
    teiseikbn character(1),
    hknnum character(3),
    honkzkkbn character(1),
    hojokbn character(1) DEFAULT ''::bpchar,
    contkbn character(1) DEFAULT ''::bpchar,
    kigo character varying(80),
    num character varying(80),
    rjnhknnum character(3),
    kohnum1 character(3),
    kohnum2 character(3),
    kohnum3 character(3),
    kohnum4 character(3),
    jnissu1 smallint DEFAULT 0,
    jnissu2 smallint DEFAULT 0,
    jnissu3 smallint DEFAULT 0,
    jnissu4 smallint DEFAULT 0,
    jnissu5 smallint DEFAULT 0,
    khnten1 integer DEFAULT 0,
    kaisu1 integer DEFAULT 0,
    tensu1_1 integer DEFAULT 0,
    tensu1_2 integer DEFAULT 0,
    tensu1_3 integer DEFAULT 0,
    tensu1_4 integer DEFAULT 0,
    tensu1_5 integer DEFAULT 0,
    khnten2 integer DEFAULT 0,
    kaisu2 integer DEFAULT 0,
    tensu2_1 integer DEFAULT 0,
    tensu2_2 integer DEFAULT 0,
    tensu2_3 integer DEFAULT 0,
    tensu2_4 integer DEFAULT 0,
    tensu2_5 integer DEFAULT 0,
    khnten3 integer DEFAULT 0,
    kaisu3 integer DEFAULT 0,
    tensu3_1 integer DEFAULT 0,
    tensu3_2 integer DEFAULT 0,
    tensu3_3 integer DEFAULT 0,
    tensu3_4 integer DEFAULT 0,
    tensu3_5 integer DEFAULT 0,
    khnten4 integer DEFAULT 0,
    kaisu4 integer DEFAULT 0,
    tensu4_1 integer DEFAULT 0,
    tensu4_2 integer DEFAULT 0,
    tensu4_3 integer DEFAULT 0,
    tensu4_4 integer DEFAULT 0,
    tensu4_5 integer DEFAULT 0,
    khnten5 integer DEFAULT 0,
    kaisu5 integer DEFAULT 0,
    tensu5_1 integer DEFAULT 0,
    tensu5_2 integer DEFAULT 0,
    tensu5_3 integer DEFAULT 0,
    tensu5_4 integer DEFAULT 0,
    tensu5_5 integer DEFAULT 0,
    khnten6 integer DEFAULT 0,
    kaisu6 integer DEFAULT 0,
    tensu6_1 integer DEFAULT 0,
    tensu6_2 integer DEFAULT 0,
    tensu6_3 integer DEFAULT 0,
    tensu6_4 integer DEFAULT 0,
    tensu6_5 integer DEFAULT 0,
    khnten7 integer DEFAULT 0,
    kaisu7 integer DEFAULT 0,
    tensu7_1 integer DEFAULT 0,
    tensu7_2 integer DEFAULT 0,
    tensu7_3 integer DEFAULT 0,
    tensu7_4 integer DEFAULT 0,
    tensu7_5 integer DEFAULT 0,
    khnten8 integer DEFAULT 0,
    kaisu8 integer DEFAULT 0,
    tensu8_1 integer DEFAULT 0,
    tensu8_2 integer DEFAULT 0,
    tensu8_3 integer DEFAULT 0,
    tensu8_4 integer DEFAULT 0,
    tensu8_5 integer DEFAULT 0,
    khnten9 integer DEFAULT 0,
    kaisu9 integer DEFAULT 0,
    tensu9_1 integer DEFAULT 0,
    tensu9_2 integer DEFAULT 0,
    tensu9_3 integer DEFAULT 0,
    tensu9_4 integer DEFAULT 0,
    tensu9_5 integer DEFAULT 0,
    khnten10 integer DEFAULT 0,
    kaisu10 integer DEFAULT 0,
    tensu10_1 integer DEFAULT 0,
    tensu10_2 integer DEFAULT 0,
    tensu10_3 integer DEFAULT 0,
    tensu10_4 integer DEFAULT 0,
    tensu10_5 integer DEFAULT 0,
    khnten11 integer DEFAULT 0,
    kaisu11 integer DEFAULT 0,
    tensu11_1 integer DEFAULT 0,
    tensu11_2 integer DEFAULT 0,
    tensu11_3 integer DEFAULT 0,
    tensu11_4 integer DEFAULT 0,
    tensu11_5 integer DEFAULT 0,
    khnten12 integer DEFAULT 0,
    kaisu12 integer DEFAULT 0,
    tensu12_1 integer DEFAULT 0,
    tensu12_2 integer DEFAULT 0,
    tensu12_3 integer DEFAULT 0,
    tensu12_4 integer DEFAULT 0,
    tensu12_5 integer DEFAULT 0,
    khnten13 integer DEFAULT 0,
    kaisu13 integer DEFAULT 0,
    tensu13_1 integer DEFAULT 0,
    tensu13_2 integer DEFAULT 0,
    tensu13_3 integer DEFAULT 0,
    tensu13_4 integer DEFAULT 0,
    tensu13_5 integer DEFAULT 0,
    khnten14 integer DEFAULT 0,
    kaisu14 integer DEFAULT 0,
    tensu14_1 integer DEFAULT 0,
    tensu14_2 integer DEFAULT 0,
    tensu14_3 integer DEFAULT 0,
    tensu14_4 integer DEFAULT 0,
    tensu14_5 integer DEFAULT 0,
    khnten15 integer DEFAULT 0,
    kaisu15 integer DEFAULT 0,
    tensu15_1 integer DEFAULT 0,
    tensu15_2 integer DEFAULT 0,
    tensu15_3 integer DEFAULT 0,
    tensu15_4 integer DEFAULT 0,
    tensu15_5 integer DEFAULT 0,
    khnten16 integer DEFAULT 0,
    kaisu16 integer DEFAULT 0,
    tensu16_1 integer DEFAULT 0,
    tensu16_2 integer DEFAULT 0,
    tensu16_3 integer DEFAULT 0,
    tensu16_4 integer DEFAULT 0,
    tensu16_5 integer DEFAULT 0,
    khnten17 integer DEFAULT 0,
    kaisu17 integer DEFAULT 0,
    tensu17_1 integer DEFAULT 0,
    tensu17_2 integer DEFAULT 0,
    tensu17_3 integer DEFAULT 0,
    tensu17_4 integer DEFAULT 0,
    tensu17_5 integer DEFAULT 0,
    khnten18 integer DEFAULT 0,
    kaisu18 integer DEFAULT 0,
    tensu18_1 integer DEFAULT 0,
    tensu18_2 integer DEFAULT 0,
    tensu18_3 integer DEFAULT 0,
    tensu18_4 integer DEFAULT 0,
    tensu18_5 integer DEFAULT 0,
    khnten19 integer DEFAULT 0,
    kaisu19 integer DEFAULT 0,
    tensu19_1 integer DEFAULT 0,
    tensu19_2 integer DEFAULT 0,
    tensu19_3 integer DEFAULT 0,
    tensu19_4 integer DEFAULT 0,
    tensu19_5 integer DEFAULT 0,
    khnten20 integer DEFAULT 0,
    kaisu20 integer DEFAULT 0,
    tensu20_1 integer DEFAULT 0,
    tensu20_2 integer DEFAULT 0,
    tensu20_3 integer DEFAULT 0,
    tensu20_4 integer DEFAULT 0,
    tensu20_5 integer DEFAULT 0,
    khnten21 integer DEFAULT 0,
    kaisu21 integer DEFAULT 0,
    tensu21_1 integer DEFAULT 0,
    tensu21_2 integer DEFAULT 0,
    tensu21_3 integer DEFAULT 0,
    tensu21_4 integer DEFAULT 0,
    tensu21_5 integer DEFAULT 0,
    khnten22 integer DEFAULT 0,
    kaisu22 integer DEFAULT 0,
    tensu22_1 integer DEFAULT 0,
    tensu22_2 integer DEFAULT 0,
    tensu22_3 integer DEFAULT 0,
    tensu22_4 integer DEFAULT 0,
    tensu22_5 integer DEFAULT 0,
    khnten23 integer DEFAULT 0,
    kaisu23 integer DEFAULT 0,
    tensu23_1 integer DEFAULT 0,
    tensu23_2 integer DEFAULT 0,
    tensu23_3 integer DEFAULT 0,
    tensu23_4 integer DEFAULT 0,
    tensu23_5 integer DEFAULT 0,
    khnten24 integer DEFAULT 0,
    kaisu24 integer DEFAULT 0,
    tensu24_1 integer DEFAULT 0,
    tensu24_2 integer DEFAULT 0,
    tensu24_3 integer DEFAULT 0,
    tensu24_4 integer DEFAULT 0,
    tensu24_5 integer DEFAULT 0,
    khnten25 integer DEFAULT 0,
    kaisu25 integer DEFAULT 0,
    tensu25_1 integer DEFAULT 0,
    tensu25_2 integer DEFAULT 0,
    tensu25_3 integer DEFAULT 0,
    tensu25_4 integer DEFAULT 0,
    tensu25_5 integer DEFAULT 0,
    khnten26 integer DEFAULT 0,
    kaisu26 integer DEFAULT 0,
    tensu26_1 integer DEFAULT 0,
    tensu26_2 integer DEFAULT 0,
    tensu26_3 integer DEFAULT 0,
    tensu26_4 integer DEFAULT 0,
    tensu26_5 integer DEFAULT 0,
    khnten27 integer DEFAULT 0,
    kaisu27 integer DEFAULT 0,
    tensu27_1 integer DEFAULT 0,
    tensu27_2 integer DEFAULT 0,
    tensu27_3 integer DEFAULT 0,
    tensu27_4 integer DEFAULT 0,
    tensu27_5 integer DEFAULT 0,
    khnten28 integer DEFAULT 0,
    kaisu28 integer DEFAULT 0,
    tensu28_1 integer DEFAULT 0,
    tensu28_2 integer DEFAULT 0,
    tensu28_3 integer DEFAULT 0,
    tensu28_4 integer DEFAULT 0,
    tensu28_5 integer DEFAULT 0,
    khnten29 integer DEFAULT 0,
    kaisu29 integer DEFAULT 0,
    tensu29_1 integer DEFAULT 0,
    tensu29_2 integer DEFAULT 0,
    tensu29_3 integer DEFAULT 0,
    tensu29_4 integer DEFAULT 0,
    tensu29_5 integer DEFAULT 0,
    khnten30 integer DEFAULT 0,
    kaisu30 integer DEFAULT 0,
    tensu30_1 integer DEFAULT 0,
    tensu30_2 integer DEFAULT 0,
    tensu30_3 integer DEFAULT 0,
    tensu30_4 integer DEFAULT 0,
    tensu30_5 integer DEFAULT 0,
    khnten31 integer DEFAULT 0,
    kaisu31 integer DEFAULT 0,
    tensu31_1 integer DEFAULT 0,
    tensu31_2 integer DEFAULT 0,
    tensu31_3 integer DEFAULT 0,
    tensu31_4 integer DEFAULT 0,
    tensu31_5 integer DEFAULT 0,
    khnten32 integer DEFAULT 0,
    kaisu32 integer DEFAULT 0,
    tensu32_1 integer DEFAULT 0,
    tensu32_2 integer DEFAULT 0,
    tensu32_3 integer DEFAULT 0,
    tensu32_4 integer DEFAULT 0,
    tensu32_5 integer DEFAULT 0,
    khnten33 integer DEFAULT 0,
    kaisu33 integer DEFAULT 0,
    tensu33_1 integer DEFAULT 0,
    tensu33_2 integer DEFAULT 0,
    tensu33_3 integer DEFAULT 0,
    tensu33_4 integer DEFAULT 0,
    tensu33_5 integer DEFAULT 0,
    khnten34 integer DEFAULT 0,
    kaisu34 integer DEFAULT 0,
    tensu34_1 integer DEFAULT 0,
    tensu34_2 integer DEFAULT 0,
    tensu34_3 integer DEFAULT 0,
    tensu34_4 integer DEFAULT 0,
    tensu34_5 integer DEFAULT 0,
    khnten35 integer DEFAULT 0,
    kaisu35 integer DEFAULT 0,
    tensu35_1 integer DEFAULT 0,
    tensu35_2 integer DEFAULT 0,
    tensu35_3 integer DEFAULT 0,
    tensu35_4 integer DEFAULT 0,
    tensu35_5 integer DEFAULT 0,
    khnten36 integer DEFAULT 0,
    kaisu36 integer DEFAULT 0,
    tensu36_1 integer DEFAULT 0,
    tensu36_2 integer DEFAULT 0,
    tensu36_3 integer DEFAULT 0,
    tensu36_4 integer DEFAULT 0,
    tensu36_5 integer DEFAULT 0,
    khnten37 integer DEFAULT 0,
    kaisu37 integer DEFAULT 0,
    tensu37_1 integer DEFAULT 0,
    tensu37_2 integer DEFAULT 0,
    tensu37_3 integer DEFAULT 0,
    tensu37_4 integer DEFAULT 0,
    tensu37_5 integer DEFAULT 0,
    khnten38 integer DEFAULT 0,
    kaisu38 integer DEFAULT 0,
    tensu38_1 integer DEFAULT 0,
    tensu38_2 integer DEFAULT 0,
    tensu38_3 integer DEFAULT 0,
    tensu38_4 integer DEFAULT 0,
    tensu38_5 integer DEFAULT 0,
    khnten39 integer DEFAULT 0,
    kaisu39 integer DEFAULT 0,
    tensu39_1 integer DEFAULT 0,
    tensu39_2 integer DEFAULT 0,
    tensu39_3 integer DEFAULT 0,
    tensu39_4 integer DEFAULT 0,
    tensu39_5 integer DEFAULT 0,
    khnten40 integer DEFAULT 0,
    kaisu40 integer DEFAULT 0,
    tensu40_1 integer DEFAULT 0,
    tensu40_2 integer DEFAULT 0,
    tensu40_3 integer DEFAULT 0,
    tensu40_4 integer DEFAULT 0,
    tensu40_5 integer DEFAULT 0,
    khnten41 integer DEFAULT 0,
    kaisu41 integer DEFAULT 0,
    tensu41_1 integer DEFAULT 0,
    tensu41_2 integer DEFAULT 0,
    tensu41_3 integer DEFAULT 0,
    tensu41_4 integer DEFAULT 0,
    tensu41_5 integer DEFAULT 0,
    khnten42 integer DEFAULT 0,
    kaisu42 integer DEFAULT 0,
    tensu42_1 integer DEFAULT 0,
    tensu42_2 integer DEFAULT 0,
    tensu42_3 integer DEFAULT 0,
    tensu42_4 integer DEFAULT 0,
    tensu42_5 integer DEFAULT 0,
    khnten43 integer DEFAULT 0,
    kaisu43 integer DEFAULT 0,
    tensu43_1 integer DEFAULT 0,
    tensu43_2 integer DEFAULT 0,
    tensu43_3 integer DEFAULT 0,
    tensu43_4 integer DEFAULT 0,
    tensu43_5 integer DEFAULT 0,
    khnten44 integer DEFAULT 0,
    kaisu44 integer DEFAULT 0,
    tensu44_1 integer DEFAULT 0,
    tensu44_2 integer DEFAULT 0,
    tensu44_3 integer DEFAULT 0,
    tensu44_4 integer DEFAULT 0,
    tensu44_5 integer DEFAULT 0,
    khnten45 integer DEFAULT 0,
    kaisu45 integer DEFAULT 0,
    tensu45_1 integer DEFAULT 0,
    tensu45_2 integer DEFAULT 0,
    tensu45_3 integer DEFAULT 0,
    tensu45_4 integer DEFAULT 0,
    tensu45_5 integer DEFAULT 0,
    totalten1 integer DEFAULT 0,
    totalten2 integer DEFAULT 0,
    totalten3 integer DEFAULT 0,
    totalten4 integer DEFAULT 0,
    totalten5 integer DEFAULT 0,
    ftnmoney1 integer DEFAULT 0,
    ftnmoney_ftn1 integer DEFAULT 0,
    ftnmoney_koj1 integer DEFAULT 0,
    ftnmoney2 integer DEFAULT 0,
    ftnmoney_ftn2 integer DEFAULT 0,
    ftnmoney_koj2 integer DEFAULT 0,
    ftnmoney3 integer DEFAULT 0,
    ftnmoney_ftn3 integer DEFAULT 0,
    ftnmoney_koj3 integer DEFAULT 0,
    ftnmoney4 integer DEFAULT 0,
    ftnmoney_ftn4 integer DEFAULT 0,
    ftnmoney_koj4 integer DEFAULT 0,
    ftnmoney5 integer DEFAULT 0,
    ftnmoney_ftn5 integer DEFAULT 0,
    ftnmoney_koj5 integer DEFAULT 0,
    shokujinissu1 smallint DEFAULT 0,
    shokujiryoyohi1 integer DEFAULT 0,
    shokujiftn1 integer DEFAULT 0,
    shokujinissu2 smallint DEFAULT 0,
    shokujiryoyohi2 integer DEFAULT 0,
    shokujiftn2 integer DEFAULT 0,
    shokujinissu3 smallint DEFAULT 0,
    shokujiryoyohi3 integer DEFAULT 0,
    shokujiftn3 integer DEFAULT 0,
    shokujinissu4 smallint DEFAULT 0,
    shokujiryoyohi4 integer DEFAULT 0,
    shokujiftn4 integer DEFAULT 0,
    shokujinissu5 smallint DEFAULT 0,
    shokujiryoyohi5 integer DEFAULT 0,
    shokujiftn5 integer DEFAULT 0,
    shokujiftnkbn character(1) DEFAULT ''::bpchar,
    ykzftn1 integer DEFAULT 0,
    ykzftn2 integer DEFAULT 0,
    ykzftn3 integer DEFAULT 0,
    ykzftn4 integer DEFAULT 0,
    ykzftn5 integer DEFAULT 0,
    etc_kohnum1 character(3) DEFAULT ''::bpchar,
    etc_kohnum2 character(3) DEFAULT ''::bpchar,
    etc_kohnum3 character(3) DEFAULT ''::bpchar,
    etc_kohnum4 character(3) DEFAULT ''::bpchar,
    etc_jnissu1 smallint DEFAULT 0,
    etc_jnissu2 smallint DEFAULT 0,
    etc_jnissu3 smallint DEFAULT 0,
    etc_jnissu4 smallint DEFAULT 0,
    etc_totalten1 integer DEFAULT 0,
    etc_totalten2 integer DEFAULT 0,
    etc_totalten3 integer DEFAULT 0,
    etc_totalten4 integer DEFAULT 0,
    etc_ftnmoney1 integer DEFAULT 0,
    etc_ftnmoney_ftn1 integer DEFAULT 0,
    etc_ftnmoney_koj1 integer DEFAULT 0,
    etc_ftnmoney2 integer DEFAULT 0,
    etc_ftnmoney_ftn2 integer DEFAULT 0,
    etc_ftnmoney_koj2 integer DEFAULT 0,
    etc_ftnmoney3 integer DEFAULT 0,
    etc_ftnmoney_ftn3 integer DEFAULT 0,
    etc_ftnmoney_koj3 integer DEFAULT 0,
    etc_ftnmoney4 integer DEFAULT 0,
    etc_ftnmoney_ftn4 integer DEFAULT 0,
    etc_ftnmoney_koj4 integer DEFAULT 0,
    etc_ykzftn1 integer DEFAULT 0,
    etc_ykzftn2 integer DEFAULT 0,
    etc_ykzftn3 integer DEFAULT 0,
    etc_ykzftn4 integer DEFAULT 0,
    etc_shokujinissu1 smallint DEFAULT 0,
    etc_shokujiryoyohi1 integer DEFAULT 0,
    etc_shokujiftn1 integer DEFAULT 0,
    etc_shokujinissu2 smallint DEFAULT 0,
    etc_shokujiryoyohi2 integer DEFAULT 0,
    etc_shokujiftn2 integer DEFAULT 0,
    etc_shokujinissu3 smallint DEFAULT 0,
    etc_shokujiryoyohi3 integer DEFAULT 0,
    etc_shokujiftn3 integer DEFAULT 0,
    etc_shokujinissu4 smallint DEFAULT 0,
    etc_shokujiryoyohi4 integer DEFAULT 0,
    etc_shokujiftn4 integer DEFAULT 0,
    hknnum_s character(3),
    hknjanum_s character(8),
    creymd character(8),
    updymd character(8),
    hknid numeric(10,0) DEFAULT 0,
    rjnhknid numeric(10,0) DEFAULT 0,
    kohid1 numeric(10,0) DEFAULT 0,
    kohid2 numeric(10,0) DEFAULT 0,
    kohid3 numeric(10,0) DEFAULT 0,
    kohid4 numeric(10,0) DEFAULT 0,
    hospnum smallint NOT NULL,
    skykbn_receden character(1),
    tokki1 character(2),
    tokki2 character(2),
    tokki3 character(2),
    tokki4 character(2),
    tokki5 character(2),
    flg1 smallint DEFAULT 0,
    flg2 smallint DEFAULT 0,
    flg3 smallint DEFAULT 0,
    flg4 smallint DEFAULT 0,
    flg5 smallint DEFAULT 0,
    reserve_area character(50),
    henrei_siji_flg smallint DEFAULT 0
);

insert into tbl_seikyu_new select * from tbl_seikyu;

drop table tbl_seikyu;

alter table tbl_seikyu_new rename to tbl_seikyu;

ALTER TABLE ONLY tbl_seikyu
    ADD CONSTRAINT tbl_seikyu_primary_key PRIMARY KEY (hospnum, sryym, nyugaikbn, ptid, receka, teisyutusaki, recesyubetu, hknjanum, hojokbn_key, tekstymd);

CREATE INDEX idx_seikyu_skyym ON tbl_seikyu USING btree (hospnum, skyym, skykbn);

