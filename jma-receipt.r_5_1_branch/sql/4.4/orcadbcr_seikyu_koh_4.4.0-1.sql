\set ON_ERROR_STOP

CREATE TABLE tbl_seikyu_koh (
    hospnum smallint NOT NULL,
    sryym integer DEFAULT 0 NOT NULL,
    nyugaikbn character(1) NOT NULL,
    ptid bigint DEFAULT 0 NOT NULL,
    receka character(2) NOT NULL,
    teisyutusaki smallint DEFAULT 0 NOT NULL,
    recesyubetu character(4) NOT NULL,
    hknjanum character(8) NOT NULL,
    hojokbn_key character(1) NOT NULL,
    tekstymd character(8) NOT NULL,
    rennum	smallint DEFAULT 0  NOT NULL,
    kohnum 	character(3),
    kohid	smallint DEFAULT 0  NOT NULL,
    jnissu smallint DEFAULT 0,
    tensu1 integer DEFAULT 0,
    tensu2 integer DEFAULT 0,
    tensu3 integer DEFAULT 0,
    tensu4 integer DEFAULT 0,
    tensu5 integer DEFAULT 0,
    tensu6 integer DEFAULT 0,
    tensu7 integer DEFAULT 0,
    tensu8 integer DEFAULT 0,
    tensu9 integer DEFAULT 0,
    tensu10 integer DEFAULT 0,
    tensu11 integer DEFAULT 0,
    tensu12 integer DEFAULT 0,
    tensu13 integer DEFAULT 0,
    tensu14 integer DEFAULT 0,
    tensu15 integer DEFAULT 0,
    tensu16 integer DEFAULT 0,
    tensu17 integer DEFAULT 0,
    tensu18 integer DEFAULT 0,
    tensu19 integer DEFAULT 0,
    tensu20 integer DEFAULT 0,
    tensu21 integer DEFAULT 0,
    tensu22 integer DEFAULT 0,
    tensu23 integer DEFAULT 0,
    tensu24 integer DEFAULT 0,
    tensu25 integer DEFAULT 0,
    tensu26 integer DEFAULT 0,
    tensu27 integer DEFAULT 0,
    tensu28 integer DEFAULT 0,
    tensu29 integer DEFAULT 0,
    tensu30 integer DEFAULT 0,
    tensu31 integer DEFAULT 0,
    tensu32 integer DEFAULT 0,
    tensu33 integer DEFAULT 0,
    tensu34 integer DEFAULT 0,
    tensu35 integer DEFAULT 0,
    tensu36 integer DEFAULT 0,
    tensu37 integer DEFAULT 0,
    tensu38 integer DEFAULT 0,
    tensu39 integer DEFAULT 0,
    tensu40 integer DEFAULT 0,
    tensu41 integer DEFAULT 0,
    tensu42 integer DEFAULT 0,
    tensu43 integer DEFAULT 0,
    tensu44 integer DEFAULT 0,
    tensu45 integer DEFAULT 0,
    totalten integer DEFAULT 0,
    ftnmoney integer DEFAULT 0,
    ftnmoney_ftn integer DEFAULT 0,
    ftnmoney_koj integer DEFAULT 0,
    shokujinissu smallint DEFAULT 0,
    shokujiryoyohi integer DEFAULT 0,
    shokujiftn integer DEFAULT 0,
    ykzftn integer DEFAULT 0
);

insert into tbl_seikyu_koh
select hospnum,sryym,nyugaikbn,ptid,receka,teisyutusaki,recesyubetu,hknjanum,hojokbn_key,tekstymd,1,kohnum1,kohid1,jnissu2,tensu1_2,tensu2_2,tensu3_2,tensu4_2,tensu5_2,tensu6_2,tensu7_2,tensu8_2,tensu9_2,tensu10_2,tensu11_2,tensu12_2,tensu13_2,tensu14_2,tensu15_2,tensu16_2,tensu17_2,tensu18_2,tensu19_2,tensu20_2,tensu21_2,tensu22_2,tensu23_2,tensu24_2,tensu25_2,tensu26_2,tensu27_2,tensu28_2,tensu29_2,tensu30_2,tensu31_2,tensu32_2,tensu33_2,tensu34_2,tensu35_2,tensu36_2,tensu37_2,tensu38_2,tensu39_2,tensu40_2,tensu41_2,tensu42_2,tensu43_2,tensu44_2,tensu45_2,totalten2,ftnmoney2,ftnmoney_ftn2,ftnmoney_koj2,shokujinissu2,shokujiryoyohi2,shokujiftn2,ykzftn2 from tbl_seikyu where kohnum1 <> ''
union all
select hospnum,sryym,nyugaikbn,ptid,receka,teisyutusaki,recesyubetu,hknjanum,hojokbn_key,tekstymd,2,kohnum2,kohid2,jnissu3,tensu1_3,tensu2_3,tensu3_3,tensu4_3,tensu5_3,tensu6_3,tensu7_3,tensu8_3,tensu9_3,tensu10_3,tensu11_3,tensu12_3,tensu13_3,tensu14_3,tensu15_3,tensu16_3,tensu17_3,tensu18_3,tensu19_3,tensu20_3,tensu21_3,tensu22_3,tensu23_3,tensu24_3,tensu25_3,tensu26_3,tensu27_3,tensu28_3,tensu29_3,tensu30_3,tensu31_3,tensu32_3,tensu33_3,tensu34_3,tensu35_3,tensu36_3,tensu37_3,tensu38_3,tensu39_3,tensu40_3,tensu41_3,tensu42_3,tensu43_3,tensu44_3,tensu45_3,totalten3,ftnmoney3,ftnmoney_ftn3,ftnmoney_koj3,shokujinissu3,shokujiryoyohi3,shokujiftn3,ykzftn3 from tbl_seikyu where kohnum2 <> ''
union all
select hospnum,sryym,nyugaikbn,ptid,receka,teisyutusaki,recesyubetu,hknjanum,hojokbn_key,tekstymd,3,kohnum3,kohid3,jnissu4,tensu1_4,tensu2_4,tensu3_4,tensu4_4,tensu5_4,tensu6_4,tensu7_4,tensu8_4,tensu9_4,tensu10_4,tensu11_4,tensu12_4,tensu13_4,tensu14_4,tensu15_4,tensu16_4,tensu17_4,tensu18_4,tensu19_4,tensu20_4,tensu21_4,tensu22_4,tensu23_4,tensu24_4,tensu25_4,tensu26_4,tensu27_4,tensu28_4,tensu29_4,tensu30_4,tensu31_4,tensu32_4,tensu33_4,tensu34_4,tensu35_4,tensu36_4,tensu37_4,tensu38_4,tensu39_4,tensu40_4,tensu41_4,tensu42_4,tensu43_4,tensu44_4,tensu45_4,totalten4,ftnmoney4,ftnmoney_ftn4,ftnmoney_koj4,shokujinissu4,shokujiryoyohi4,shokujiftn4,ykzftn4 from tbl_seikyu where kohnum3 <> ''
union all
select hospnum,sryym,nyugaikbn,ptid,receka,teisyutusaki,recesyubetu,hknjanum,hojokbn_key,tekstymd,4,kohnum4,kohid4,jnissu5,tensu1_5,tensu2_5,tensu3_5,tensu4_5,tensu5_5,tensu6_5,tensu7_5,tensu8_5,tensu9_5,tensu10_5,tensu11_5,tensu12_5,tensu13_5,tensu14_5,tensu15_5,tensu16_5,tensu17_5,tensu18_5,tensu19_5,tensu20_5,tensu21_5,tensu22_5,tensu23_5,tensu24_5,tensu25_5,tensu26_5,tensu27_5,tensu28_5,tensu29_5,tensu30_5,tensu31_5,tensu32_5,tensu33_5,tensu34_5,tensu35_5,tensu36_5,tensu37_5,tensu38_5,tensu39_5,tensu40_5,tensu41_5,tensu42_5,tensu43_5,tensu44_5,tensu45_5,totalten5,ftnmoney5,ftnmoney_ftn5,ftnmoney_koj5,shokujinissu5,shokujiryoyohi5,shokujiftn5,ykzftn5 from tbl_seikyu where kohnum4 <> ''
;

ALTER TABLE ONLY tbl_seikyu_koh
    ADD CONSTRAINT tbl_seikyu_koh_primary_key PRIMARY KEY (hospnum, sryym, nyugaikbn, ptid, receka, teisyutusaki, recesyubetu, hknjanum, hojokbn_key, tekstymd,rennum);
