\set ON_ERROR_STOP

CREATE TABLE tbl_syunou_nyuin (
    nyugaikbn character(1) NOT NULL,
    ptid bigint NOT NULL,
    denpnum integer NOT NULL,
    roomnum character(8),
    byotonum character(2),
    rmsagaku integer,
    rmsagaku_tax_sai integer,
    shokuji1_nissu smallint,
    shokuji1 integer,
    shokuji2_nissu smallint,
    shokuji2 integer,
    shokuji3_nissu smallint,
    shokuji3 integer,
    shokuji4_nissu smallint,
    shokuji4 integer,
    shokuji5_nissu smallint,
    shokuji5 integer,
    shokuji6_nissu smallint,
    shokuji6 integer,
    shokuji7_nissu smallint,
    shokuji7 integer,
    syuskj_nissu smallint,
    syuskj_ryoyohi integer,
    syuskj_ftn integer,
    syuskj_ftn_rece integer,
    syuskj_ftnkbn character(1),
    koh1skj_nissu smallint,
    koh1skj_ryoyohi integer,
    koh1skj_ftn integer,
    koh1skj_ftn_rece integer,
    koh1skj_ftnkbn character(1),
    koh2skj_nissu smallint,
    koh2skj_ryoyohi integer,
    koh2skj_ftn integer,
    koh2skj_ftn_rece integer,
    koh2skj_ftnkbn character(1),
    koh3skj_nissu smallint,
    koh3skj_ryoyohi integer,
    koh3skj_ftn integer,
    koh3skj_ftn_rece integer,
    koh3skj_ftnkbn character(1),
    koh4skj_nissu smallint,
    koh4skj_ryoyohi integer,
    koh4skj_ftn integer,
    koh4skj_ftn_rece integer,
    koh4skj_ftnkbn character(1),
    ryoyohi_skj integer,
    skymoney_skj integer,
    skymoney_skj_tax integer,
    skymoney_skj_kei integer,
    ryoyohi_skj_jihi integer,
    skymoney_skj_jihi integer,
    skymoney_skj_jihi_tax integer,
    skymoney_skj_jihi_kei integer,
    day_1 character(1),
    day_2 character(1),
    day_3 character(1),
    day_4 character(1),
    day_5 character(1),
    day_6 character(1),
    day_7 character(1),
    day_8 character(1),
    day_9 character(1),
    day_10 character(1),
    day_11 character(1),
    day_12 character(1),
    day_13 character(1),
    day_14 character(1),
    day_15 character(1),
    day_16 character(1),
    day_17 character(1),
    day_18 character(1),
    day_19 character(1),
    day_20 character(1),
    day_21 character(1),
    day_22 character(1),
    day_23 character(1),
    day_24 character(1),
    day_25 character(1),
    day_26 character(1),
    day_27 character(1),
    day_28 character(1),
    day_29 character(1),
    day_30 character(1),
    day_31 character(1),
    nyuin_rrknum smallint,
    shokuji8_nissu smallint DEFAULT 0,
    shokuji8 integer DEFAULT 0,
    shokuji9_nissu smallint DEFAULT 0,
    shokuji9 integer DEFAULT 0,
    shokuji10_nissu smallint DEFAULT 0,
    shokuji10 integer DEFAULT 0,
    ryoyohi_life integer DEFAULT 0,
    skymoney_life integer DEFAULT 0,
    skymoney_life_tax integer DEFAULT 0,
    skymoney_life_kei integer DEFAULT 0,
    ryoyohi_life_jihi integer DEFAULT 0,
    skymoney_life_jihi integer DEFAULT 0,
    skymoney_life_jihi_tax integer DEFAULT 0,
    skymoney_life_jihi_kei integer DEFAULT 0,
    hospnum smallint NOT NULL
);

insert into tbl_syunou_nyuin (
nyugaikbn,
ptid,
denpnum,
roomnum,
byotonum,
rmsagaku,
rmsagaku_tax_sai,
shokuji1_nissu,
shokuji1,
shokuji2_nissu,
shokuji2,
shokuji3_nissu,
shokuji3,
shokuji4_nissu,
shokuji4,
shokuji5_nissu,
shokuji5,
shokuji6_nissu,
shokuji6,
shokuji7_nissu,
shokuji7,
syuskj_nissu,
syuskj_ryoyohi,
syuskj_ftn,
syuskj_ftn_rece,
syuskj_ftnkbn,
koh1skj_nissu,
koh1skj_ryoyohi,
koh1skj_ftn,
koh1skj_ftn_rece,
koh1skj_ftnkbn,
koh2skj_nissu,
koh2skj_ryoyohi,
koh2skj_ftn,
koh2skj_ftn_rece,
koh2skj_ftnkbn,
koh3skj_nissu,
koh3skj_ryoyohi,
koh3skj_ftn,
koh3skj_ftn_rece,
koh3skj_ftnkbn,
koh4skj_nissu,
koh4skj_ryoyohi,
koh4skj_ftn,
koh4skj_ftn_rece,
koh4skj_ftnkbn,
ryoyohi_skj,
skymoney_skj,
skymoney_skj_tax,
skymoney_skj_kei,
ryoyohi_skj_jihi,
skymoney_skj_jihi,
skymoney_skj_jihi_tax,
skymoney_skj_jihi_kei,
day_1,
day_2,
day_3,
day_4,
day_5,
day_6,
day_7,
day_8,
day_9,
day_10,
day_11,
day_12,
day_13,
day_14,
day_15,
day_16,
day_17,
day_18,
day_19,
day_20,
day_21,
day_22,
day_23,
day_24,
day_25,
day_26,
day_27,
day_28,
day_29,
day_30,
day_31,
nyuin_rrknum,
shokuji8_nissu,
shokuji8,
shokuji9_nissu,
shokuji9,
shokuji10_nissu,
shokuji10,
ryoyohi_life,
skymoney_life,
skymoney_life_tax,
skymoney_life_kei,
ryoyohi_life_jihi,
skymoney_life_jihi,
skymoney_life_jihi_tax,
skymoney_life_jihi_kei,
hospnum
)
select
nyugaikbn,
ptid,
denpnum,
roomnum,
byotonum,
rmsagaku,
rmsagaku_tax_sai,
shokuji1_nissu,
shokuji1,
shokuji2_nissu,
shokuji2,
shokuji3_nissu,
shokuji3,
shokuji4_nissu,
shokuji4,
shokuji5_nissu,
shokuji5,
shokuji6_nissu,
shokuji6,
shokuji7_nissu,
shokuji7,
syuskj_nissu,
syuskj_ryoyohi,
syuskj_ftn,
syuskj_ftn_rece,
syuskj_ftnkbn,
koh1skj_nissu,
koh1skj_ryoyohi,
koh1skj_ftn,
koh1skj_ftn_rece,
koh1skj_ftnkbn,
koh2skj_nissu,
koh2skj_ryoyohi,
koh2skj_ftn,
koh2skj_ftn_rece,
koh2skj_ftnkbn,
koh3skj_nissu,
koh3skj_ryoyohi,
koh3skj_ftn,
koh3skj_ftn_rece,
koh3skj_ftnkbn,
koh4skj_nissu,
koh4skj_ryoyohi,
koh4skj_ftn,
koh4skj_ftn_rece,
koh4skj_ftnkbn,
ryoyohi_skj,
skymoney_skj,
skymoney_skj_tax,
skymoney_skj_kei,
ryoyohi_skj_jihi,
skymoney_skj_jihi,
skymoney_skj_jihi_tax,
skymoney_skj_jihi_kei,
day_1,
day_2,
day_3,
day_4,
day_5,
day_6,
day_7,
day_8,
day_9,
day_10,
day_11,
day_12,
day_13,
day_14,
day_15,
day_16,
day_17,
day_18,
day_19,
day_20,
day_21,
day_22,
day_23,
day_24,
day_25,
day_26,
day_27,
day_28,
day_29,
day_30,
day_31,
nyuin_rrknum,
shokuji8_nissu,
shokuji8,
shokuji9_nissu,
shokuji9,
shokuji10_nissu,
shokuji10,
ryoyohi_life,
skymoney_life,
skymoney_life_tax,
skymoney_life_kei,
ryoyohi_life_jihi,
skymoney_life_jihi,
skymoney_life_jihi_tax,
skymoney_life_jihi_kei,
hospnum
from tbl_syunou 
where nyugaikbn = '1';

ALTER TABLE ONLY tbl_syunou_nyuin
    ADD CONSTRAINT tbl_syunou_nyuin_primary_key PRIMARY KEY (hospnum, nyugaikbn, ptid, denpnum);

