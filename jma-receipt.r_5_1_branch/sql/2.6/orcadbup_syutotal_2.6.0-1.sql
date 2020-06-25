--                                    --
-- ��Ǽ��ץơ��֥���ѹ�             --
--                                    --
-- ���ܤ��ɲ�                         --
--   ���񣶤��飱��  JIHI_6           --
--                   JIHI_6_TAX       --
--                   JIHI_7           --
--                   JIHI_7_TAX       --
--                   JIHI_8           --
--                   JIHI_8_TAX       --
--                   JIHI_9           --
--                   JIHI_9_TAX       --
--                   JIHI_10          --
--                   JIHI_10_TAX      --
--   ���ȶ��        DISCOUNT_MONEY   --
--                                    --
-- Create Date : 2005/07/14           --
--                                    --
--
-- TBL_SYUTOTALWK ����                  --

CREATE TABLE "tbl_syutotalwk" (
	"hospid" character(24) NOT NULL,
	"nyugaikbn" character(1) NOT NULL,
	"ptid" numeric(10,0) NOT NULL,
	"denpnum" numeric(7,0) NOT NULL,
	"sryka" character(2),
	"douji_sryka1" character(2),
	"douji_sryka2" character(2),
	"douji_sryka3" character(2),
	"douji_sryka4" character(2),
	"douji_sryka5" character(2),
	"douji_sryka6" character(2),
	"douji_sryka7" character(2),
	"douji_sryka8" character(2),
	"douji_sryka9" character(2),
	"douji_sryka10" character(2),
	"sryymd" character(8),
	"denpjtikbn" character(1),
	"createkbn" character(1),
	"denpprtymd" character(8),
	"skystymd" character(8),
	"skyedymd" character(8),
	"hkncombinum" character(4),
	"syuhknnum" character(3),
	"syuhknftnmoney" numeric(7,0),
	"syucompftn" numeric(7,0),
	"syucompftn_entani" numeric(8,1),
	"syucompykzftn" numeric(7,0),
	"syucomptotal" numeric(7,0),
	"syuykzftnkbn" character(1),
	"koh1hknnum" character(3),
	"koh1hknftnmoney" numeric(7,0),
	"koh1compftn" numeric(7,0),
	"koh1compftn_entani" numeric(8,1),
	"koh1compykzftn" numeric(7,0),
	"koh1comptotal" numeric(7,0),
	"koh1ykzftnkbn" character(1),
	"koh2hknnum" character(3),
	"koh2hknftnmoney" numeric(7,0),
	"koh2compftn" numeric(7,0),
	"koh2compftn_entani" numeric(8,1),
	"koh2compykzftn" numeric(7,0),
	"koh2comptotal" numeric(7,0),
	"koh2ykzftnkbn" character(1),
	"koh3hknnum" character(3),
	"koh3hknftnmoney" numeric(7,0),
	"koh3compftn" numeric(7,0),
	"koh3compftn_entani" numeric(8,1),
	"koh3compykzftn" numeric(7,0),
	"koh3comptotal" numeric(7,0),
	"koh3ykzftnkbn" character(1),
	"koh4hknnum" character(3),
	"koh4hknftnmoney" numeric(7,0),
	"koh4compftn" numeric(7,0),
	"koh4compftn_entani" numeric(8,1),
	"koh4compykzftn" numeric(7,0),
	"koh4comptotal" numeric(7,0),
	"koh4ykzftnkbn" character(1),
	"ptftnrate" numeric(3,0),
	"skykukbn" character(1),
	"ssu_hknten" numeric(7,0),
	"ssu_money" numeric(7,0),
	"ssu_tgmoney" numeric(7,0),
	"ssu_tgmoney_tax" numeric(7,0),
	"sdo_hknten" numeric(7,0),
	"sdo_money" numeric(7,0),
	"sdo_tgmoney" numeric(7,0),
	"sdo_tgmoney_tax" numeric(7,0),
	"ztk_hknten" numeric(7,0),
	"ztk_money" numeric(7,0),
	"ztk_tgmoney" numeric(7,0),
	"ztk_tgmoney_tax" numeric(7,0),
	"tyk_hknten" numeric(7,0),
	"tyk_money" numeric(7,0),
	"tyk_tgmoney" numeric(7,0),
	"tyk_tgmoney_tax" numeric(7,0),
	"csy_hknten" numeric(7,0),
	"csy_money" numeric(7,0),
	"csy_tgmoney" numeric(7,0),
	"csy_tgmoney_tax" numeric(7,0),
	"syc_hknten" numeric(7,0),
	"syc_money" numeric(7,0),
	"syc_tgmoney" numeric(7,0),
	"syc_tgmoney_tax" numeric(7,0),
	"sjt_hknten" numeric(7,0),
	"sjt_money" numeric(7,0),
	"sjt_tgmoney" numeric(7,0),
	"sjt_tgmoney_tax" numeric(7,0),
	"kns_hknten" numeric(7,0),
	"kns_money" numeric(7,0),
	"kns_tgmoney" numeric(7,0),
	"kns_tgmoney_tax" numeric(7,0),
	"gzu_hknten" numeric(7,0),
	"gzu_money" numeric(7,0),
	"gzu_tgmoney" numeric(7,0),
	"gzu_tgmoney_tax" numeric(7,0),
	"etc_hknten" numeric(7,0),
	"etc_money" numeric(7,0),
	"etc_tgmoney" numeric(7,0),
	"etc_tgmoney_tax" numeric(7,0),
	"nyn_hknten" numeric(7,0),
	"nyn_money" numeric(7,0),
	"nyn_tgmoney" numeric(7,0),
	"nyn_tgmoney_tax" numeric(7,0),
	"total_hknten" numeric(7,0),
	"total_money" numeric(7,0),
	"total_tgmoney" numeric(7,0),
	"total_tgmoney_tax" numeric(7,0),
	"tgmoney_tax_sai" numeric(7,0),
	"jihi_1" numeric(7,0),
	"jihi_1_tax" numeric(7,0),
	"jihi_2" numeric(7,0),
	"jihi_2_tax" numeric(7,0),
	"jihi_3" numeric(7,0),
	"jihi_3_tax" numeric(7,0),
	"jihi_4" numeric(7,0),
	"jihi_4_tax" numeric(7,0),
	"jihi_5" numeric(7,0),
	"jihi_5_tax" numeric(7,0),
	"jihi_total" numeric(7,0),
	"jihi_total_tax" numeric(7,0),
	"jihi_tax" numeric(7,0),
	"rjn_ftn" numeric(7,0),
	"koh_ftn" numeric(7,0),
	"koh_ftn_entani" numeric(8,1) DEFAULT 0,
	"chosei" numeric(7,0),
	"grp_sgkmoney" numeric(7,0),
	"hkntekmoney" numeric(7,0),
	"discount_kbn" character(2),
	"discount_body" character varying(160),
	"discount_ratekbn" character(2),
	"discount_teiritu" character(1),
	"discount_rate" numeric(7,0),
	"rsishoshin_money" numeric(7,0),
	"rsisaishin_money" numeric(7,0),
	"rsisdo_money" numeric(7,0),
	"rsietc_money" numeric(7,0),
	"rsi_tax_sai" numeric(7,0),
	"rsi_total" numeric(7,0),
	"rsijibai_skymoney" numeric(7,0),
	"skymoney_tax_sai" numeric(7,0),
	"skymoney" numeric(7,0),
	"nyukin_total" numeric(7,0),
	"nyukin_kaisu" numeric(7,0),
	"misyuriyu" character(2),
	"zaitaku" character(1),
	"kyufugai_shoshin_ten" numeric(7,0),
	"kyufugai_sidou_ten" numeric(7,0),
	"kyufugai_osin1_ten" numeric(7,0),
	"kyufugai_osin2_ten" numeric(7,0),
	"kyufugai_osin3_ten" numeric(7,0),
	"kyufugai_gokei_ten" numeric(7,0),
	"termid" character varying(16),
	"opid" character varying(16),
	"creymd" character(8),
	"upymd" character(8),
	"uphms" character(6),
	Constraint "tbl_syutotalwk_primary_key" Primary Key ("hospid", "nyugaikbn", "ptid", "denpnum")
);


-- TBL_SYUTOTAL �� TBL_SYUTOTALWK   --

insert into TBL_SYUTOTALWK
select * from TBL_SYUTOTAL;

-- TBL_SYUTOTAL ���               --

drop table TBL_SYUTOTAL;

--
-- TBL_SYUTOTAL ����                  --

CREATE TABLE "tbl_syutotal" (
	"hospid" character(24) NOT NULL,
	"nyugaikbn" character(1) NOT NULL,
	"ptid" numeric(10,0) NOT NULL,
	"denpnum" numeric(7,0) NOT NULL,
	"sryka" character(2),
	"douji_sryka1" character(2),
	"douji_sryka2" character(2),
	"douji_sryka3" character(2),
	"douji_sryka4" character(2),
	"douji_sryka5" character(2),
	"douji_sryka6" character(2),
	"douji_sryka7" character(2),
	"douji_sryka8" character(2),
	"douji_sryka9" character(2),
	"douji_sryka10" character(2),
	"sryymd" character(8),
	"denpjtikbn" character(1),
	"createkbn" character(1),
	"denpprtymd" character(8),
	"skystymd" character(8),
	"skyedymd" character(8),
	"hkncombinum" character(4),
	"syuhknnum" character(3),
	"syuhknftnmoney" numeric(7,0),
	"syucompftn" numeric(7,0),
	"syucompftn_entani" numeric(8,1),
	"syucompykzftn" numeric(7,0),
	"syucomptotal" numeric(7,0),
	"syuykzftnkbn" character(1),
	"koh1hknnum" character(3),
	"koh1hknftnmoney" numeric(7,0),
	"koh1compftn" numeric(7,0),
	"koh1compftn_entani" numeric(8,1),
	"koh1compykzftn" numeric(7,0),
	"koh1comptotal" numeric(7,0),
	"koh1ykzftnkbn" character(1),
	"koh2hknnum" character(3),
	"koh2hknftnmoney" numeric(7,0),
	"koh2compftn" numeric(7,0),
	"koh2compftn_entani" numeric(8,1),
	"koh2compykzftn" numeric(7,0),
	"koh2comptotal" numeric(7,0),
	"koh2ykzftnkbn" character(1),
	"koh3hknnum" character(3),
	"koh3hknftnmoney" numeric(7,0),
	"koh3compftn" numeric(7,0),
	"koh3compftn_entani" numeric(8,1),
	"koh3compykzftn" numeric(7,0),
	"koh3comptotal" numeric(7,0),
	"koh3ykzftnkbn" character(1),
	"koh4hknnum" character(3),
	"koh4hknftnmoney" numeric(7,0),
	"koh4compftn" numeric(7,0),
	"koh4compftn_entani" numeric(8,1),
	"koh4compykzftn" numeric(7,0),
	"koh4comptotal" numeric(7,0),
	"koh4ykzftnkbn" character(1),
	"ptftnrate" numeric(3,0),
	"skykukbn" character(1),
	"ssu_hknten" numeric(7,0),
	"ssu_money" numeric(7,0),
	"ssu_tgmoney" numeric(7,0),
	"ssu_tgmoney_tax" numeric(7,0),
	"sdo_hknten" numeric(7,0),
	"sdo_money" numeric(7,0),
	"sdo_tgmoney" numeric(7,0),
	"sdo_tgmoney_tax" numeric(7,0),
	"ztk_hknten" numeric(7,0),
	"ztk_money" numeric(7,0),
	"ztk_tgmoney" numeric(7,0),
	"ztk_tgmoney_tax" numeric(7,0),
	"tyk_hknten" numeric(7,0),
	"tyk_money" numeric(7,0),
	"tyk_tgmoney" numeric(7,0),
	"tyk_tgmoney_tax" numeric(7,0),
	"csy_hknten" numeric(7,0),
	"csy_money" numeric(7,0),
	"csy_tgmoney" numeric(7,0),
	"csy_tgmoney_tax" numeric(7,0),
	"syc_hknten" numeric(7,0),
	"syc_money" numeric(7,0),
	"syc_tgmoney" numeric(7,0),
	"syc_tgmoney_tax" numeric(7,0),
	"sjt_hknten" numeric(7,0),
	"sjt_money" numeric(7,0),
	"sjt_tgmoney" numeric(7,0),
	"sjt_tgmoney_tax" numeric(7,0),
	"kns_hknten" numeric(7,0),
	"kns_money" numeric(7,0),
	"kns_tgmoney" numeric(7,0),
	"kns_tgmoney_tax" numeric(7,0),
	"gzu_hknten" numeric(7,0),
	"gzu_money" numeric(7,0),
	"gzu_tgmoney" numeric(7,0),
	"gzu_tgmoney_tax" numeric(7,0),
	"etc_hknten" numeric(7,0),
	"etc_money" numeric(7,0),
	"etc_tgmoney" numeric(7,0),
	"etc_tgmoney_tax" numeric(7,0),
	"nyn_hknten" numeric(7,0),
	"nyn_money" numeric(7,0),
	"nyn_tgmoney" numeric(7,0),
	"nyn_tgmoney_tax" numeric(7,0),
	"total_hknten" numeric(7,0),
	"total_money" numeric(7,0),
	"total_tgmoney" numeric(7,0),
	"total_tgmoney_tax" numeric(7,0),
	"tgmoney_tax_sai" numeric(7,0),
	"jihi_1" numeric(7,0),
	"jihi_1_tax" numeric(7,0),
	"jihi_2" numeric(7,0),
	"jihi_2_tax" numeric(7,0),
	"jihi_3" numeric(7,0),
	"jihi_3_tax" numeric(7,0),
	"jihi_4" numeric(7,0),
	"jihi_4_tax" numeric(7,0),
	"jihi_5" numeric(7,0),
	"jihi_5_tax" numeric(7,0),
	"jihi_6" numeric(7,0) default 0,
	"jihi_6_tax" numeric(7,0) default 0,
	"jihi_7" numeric(7,0) default 0,
	"jihi_7_tax" numeric(7,0) default 0,
	"jihi_8" numeric(7,0) default 0,
	"jihi_8_tax" numeric(7,0) default 0,
	"jihi_9" numeric(7,0) default 0,
	"jihi_9_tax" numeric(7,0) default 0,
	"jihi_10" numeric(7,0) default 0,
	"jihi_10_tax" numeric(7,0) default 0,
	"jihi_total" numeric(7,0),
	"jihi_total_tax" numeric(7,0),
	"jihi_tax" numeric(7,0),
	"rjn_ftn" numeric(7,0),
	"koh_ftn" numeric(7,0),
	"koh_ftn_entani" numeric(8,1),
	"chosei" numeric(7,0),
	"grp_sgkmoney" numeric(7,0),
	"hkntekmoney" numeric(7,0),
	"discount_kbn" character(2),
	"discount_body" character varying(160),
	"discount_ratekbn" character(2),
	"discount_teiritu" character(1),
	"discount_rate" numeric(7,0),
	"discount_money" numeric(7,0) default 0,
	"rsishoshin_money" numeric(7,0),
	"rsisaishin_money" numeric(7,0),
	"rsisdo_money" numeric(7,0),
	"rsietc_money" numeric(7,0),
	"rsi_tax_sai" numeric(7,0),
	"rsi_total" numeric(7,0),
	"rsijibai_skymoney" numeric(7,0),
	"skymoney_tax_sai" numeric(7,0),
	"skymoney" numeric(7,0),
	"nyukin_total" numeric(7,0),
	"nyukin_kaisu" numeric(7,0),
	"misyuriyu" character(2),
	"zaitaku" character(1),
	"kyufugai_shoshin_ten" numeric(7,0),
	"kyufugai_sidou_ten" numeric(7,0),
	"kyufugai_osin1_ten" numeric(7,0),
	"kyufugai_osin2_ten" numeric(7,0),
	"kyufugai_osin3_ten" numeric(7,0),
	"kyufugai_gokei_ten" numeric(7,0),
	"termid" character varying(16),
	"opid" character varying(16),
	"creymd" character(8),
	"upymd" character(8),
	"uphms" character(6),
	Constraint "tbl_syutotal_primary_key" Primary Key ("hospid", "nyugaikbn", "ptid", "denpnum")
);

-- TBL_SYUTOTALWK �� TBL_SYUTOTAL   --

insert into TBL_SYUTOTAL
(hospid,
nyugaikbn,
ptid,
denpnum,
sryka,
douji_sryka1,
douji_sryka2,
douji_sryka3,
douji_sryka4,
douji_sryka5,
douji_sryka6,
douji_sryka7,
douji_sryka8,
douji_sryka9,
douji_sryka10,
sryymd,
denpjtikbn,
createkbn,
denpprtymd,
skystymd,
skyedymd,
hkncombinum,
syuhknnum,
syuhknftnmoney,
syucompftn,
syucompftn_entani,
syucompykzftn,
syucomptotal,
syuykzftnkbn,
koh1hknnum,
koh1hknftnmoney,
koh1compftn,
koh1compftn_entani,
koh1compykzftn,
koh1comptotal,
koh1ykzftnkbn,
koh2hknnum,
koh2hknftnmoney,
koh2compftn,
koh2compftn_entani,
koh2compykzftn,
koh2comptotal,
koh2ykzftnkbn,
koh3hknnum,
koh3hknftnmoney,
koh3compftn,
koh3compftn_entani,
koh3compykzftn,
koh3comptotal,
koh3ykzftnkbn,
koh4hknnum,
koh4hknftnmoney,
koh4compftn,
koh4compftn_entani,
koh4compykzftn,
koh4comptotal,
koh4ykzftnkbn,
ptftnrate,
skykukbn,
ssu_hknten,
ssu_money,
ssu_tgmoney,
ssu_tgmoney_tax,
sdo_hknten,
sdo_money,
sdo_tgmoney,
sdo_tgmoney_tax,
ztk_hknten,
ztk_money,
ztk_tgmoney,
ztk_tgmoney_tax,
tyk_hknten,
tyk_money,
tyk_tgmoney,
tyk_tgmoney_tax,
csy_hknten,
csy_money,
csy_tgmoney,
csy_tgmoney_tax,
syc_hknten,
syc_money,
syc_tgmoney,
syc_tgmoney_tax,
sjt_hknten,
sjt_money,
sjt_tgmoney,
sjt_tgmoney_tax,
kns_hknten,
kns_money,
kns_tgmoney,
kns_tgmoney_tax,
gzu_hknten,
gzu_money,
gzu_tgmoney,
gzu_tgmoney_tax,
etc_hknten,
etc_money,
etc_tgmoney,
etc_tgmoney_tax,
nyn_hknten,
nyn_money,
nyn_tgmoney,
nyn_tgmoney_tax,
total_hknten,
total_money,
total_tgmoney,
total_tgmoney_tax,
tgmoney_tax_sai,
jihi_1,
jihi_1_tax,
jihi_2,
jihi_2_tax,
jihi_3,
jihi_3_tax,
jihi_4,
jihi_4_tax,
jihi_5,
jihi_5_tax,
--jihi_6--
--jihi_6_tax--
--jihi_7--
--jihi_7_tax--
--jihi_8--
--jihi_8_tax--
--jihi_9--
--jihi_9_tax--
--jihi_10--
--jihi_10_tax--
jihi_total,
jihi_total_tax,
jihi_tax,
rjn_ftn,
koh_ftn,
koh_ftn_entani,
chosei,
grp_sgkmoney,
hkntekmoney,
discount_kbn,
discount_body,
discount_ratekbn,
discount_teiritu,
discount_rate,
--discount_money--
rsishoshin_money,
rsisaishin_money,
rsisdo_money,
rsietc_money,
rsi_tax_sai,
rsi_total,
rsijibai_skymoney,
skymoney_tax_sai,
skymoney,
nyukin_total,
nyukin_kaisu,
misyuriyu,
zaitaku,
kyufugai_shoshin_ten,
kyufugai_sidou_ten,
kyufugai_osin1_ten,
kyufugai_osin2_ten,
kyufugai_osin3_ten,
kyufugai_gokei_ten,
termid,
opid,
creymd,
upymd,
uphms
)
select 
hospid,
nyugaikbn,
ptid,
denpnum,
sryka,
douji_sryka1,
douji_sryka2,
douji_sryka3,
douji_sryka4,
douji_sryka5,
douji_sryka6,
douji_sryka7,
douji_sryka8,
douji_sryka9,
douji_sryka10,
sryymd,
denpjtikbn,
createkbn,
denpprtymd,
skystymd,
skyedymd,
hkncombinum,
syuhknnum,
syuhknftnmoney,
syucompftn,
syucompftn_entani,
syucompykzftn,
syucomptotal,
syuykzftnkbn,
koh1hknnum,
koh1hknftnmoney,
koh1compftn,
koh1compftn_entani,
koh1compykzftn,
koh1comptotal,
koh1ykzftnkbn,
koh2hknnum,
koh2hknftnmoney,
koh2compftn,
koh2compftn_entani,
koh2compykzftn,
koh2comptotal,
koh2ykzftnkbn,
koh3hknnum,
koh3hknftnmoney,
koh3compftn,
koh3compftn_entani,
koh3compykzftn,
koh3comptotal,
koh3ykzftnkbn,
koh4hknnum,
koh4hknftnmoney,
koh4compftn,
koh4compftn_entani,
koh4compykzftn,
koh4comptotal,
koh4ykzftnkbn,
ptftnrate,
skykukbn,
ssu_hknten,
ssu_money,
ssu_tgmoney,
ssu_tgmoney_tax,
sdo_hknten,
sdo_money,
sdo_tgmoney,
sdo_tgmoney_tax,
ztk_hknten,
ztk_money,
ztk_tgmoney,
ztk_tgmoney_tax,
tyk_hknten,
tyk_money,
tyk_tgmoney,
tyk_tgmoney_tax,
csy_hknten,
csy_money,
csy_tgmoney,
csy_tgmoney_tax,
syc_hknten,
syc_money,
syc_tgmoney,
syc_tgmoney_tax,
sjt_hknten,
sjt_money,
sjt_tgmoney,
sjt_tgmoney_tax,
kns_hknten,
kns_money,
kns_tgmoney,
kns_tgmoney_tax,
gzu_hknten,
gzu_money,
gzu_tgmoney,
gzu_tgmoney_tax,
etc_hknten,
etc_money,
etc_tgmoney,
etc_tgmoney_tax,
nyn_hknten,
nyn_money,
nyn_tgmoney,
nyn_tgmoney_tax,
total_hknten,
total_money,
total_tgmoney,
total_tgmoney_tax,
tgmoney_tax_sai,
jihi_1,
jihi_1_tax,
jihi_2,
jihi_2_tax,
jihi_3,
jihi_3_tax,
jihi_4,
jihi_4_tax,
jihi_5,
jihi_5_tax,
jihi_total,
jihi_total_tax,
jihi_tax,
rjn_ftn,
koh_ftn,
koh_ftn_entani,
chosei,
grp_sgkmoney,
hkntekmoney,
discount_kbn,
discount_body,
discount_ratekbn,
discount_teiritu,
discount_rate,
rsishoshin_money,
rsisaishin_money,
rsisdo_money,
rsietc_money,
rsi_tax_sai,
rsi_total,
rsijibai_skymoney,
skymoney_tax_sai,
skymoney,
nyukin_total,
nyukin_kaisu,
misyuriyu,
zaitaku,
kyufugai_shoshin_ten,
kyufugai_sidou_ten,
kyufugai_osin1_ten,
kyufugai_osin2_ten,
kyufugai_osin3_ten,
kyufugai_gokei_ten,
termid,
opid,
creymd,
upymd,
uphms
 from TBL_SYUTOTALWK;


-- TBL_SYUTOTALWK ���             --

drop table TBL_SYUTOTALWK;  