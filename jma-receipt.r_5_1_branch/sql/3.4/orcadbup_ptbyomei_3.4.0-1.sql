--                                    --
--  病名テーブルの変更                --
--                                    --
-- 項目の追加                         --
--     削除フラグ         dltflg      --
--     削除オペレータＩＤ dlt_opid    --
--                                    --
-- Create Date : 2006/10/31           --
--
                                    --
\set ON_ERROR_STOP

-- TBL_PTBYOMEIWK 作成                --

CREATE TABLE "tbl_ptbyomeiwk" (
	"hospid" character(24) NOT NULL,
	"ptid" numeric(10,0) NOT NULL,
	"sryka" character(2) NOT NULL,
	"sryymd" character(8) NOT NULL,
	"rennum" numeric(2,0) NOT NULL,
	"khnbyomeicd" character(7),
	"byomeiinputcd_1" character varying(20),
	"byomeiinputcd_2" character varying(20),
	"byomeiinputcd_3" character varying(20),
	"byomeiinputcd_4" character varying(20),
	"byomeiinputcd_5" character varying(20),
	"byomeiinputcd_6" character varying(20),
	"byomeicdsu" numeric(2,0) DEFAULT 0,
	"byomeicd_1" character(7),
	"byomeicd_2" character(7),
	"byomeicd_3" character(7),
	"byomeicd_4" character(7),
	"byomeicd_5" character(7),
	"byomeicd_6" character(7),
	"byomeicd_7" character(7),
	"byomeicd_8" character(7),
	"byomeicd_9" character(7),
	"byomeicd_10" character(7),
	"byomeicd_11" character(7),
	"byomeicd_12" character(7),
	"byomeicd_13" character(7),
	"byomeicd_14" character(7),
	"byomeicd_15" character(7),
	"byomeicd_16" character(7),
	"byomeicd_17" character(7),
	"byomeicd_18" character(7),
	"byomeicd_19" character(7),
	"byomeicd_20" character(7),
	"byomeicd_21" character(7),
	"utagaiflg" character(1),
	"syubyoflg" character(1),
	"manseikbn" numeric(2,0) DEFAULT 0,
	"nyugaikbn" character(1),
	"hkncombi" numeric(4,0) DEFAULT 0,
	"rezeflg" character(1),
	"rezemm" numeric(2,0) DEFAULT 0,
	"tenkikbn" character(1),
	"tenkiymd" character(8),
	"byomei" character varying(160),
	"byomeimoji" numeric(2,0) DEFAULT 0,
	"chartbyomei" character varying(160),
	"chartbyomeimoji" numeric(2,0) DEFAULT 0,
	"byomeihenflg" character(1),
	"hknbyomeiflg" character(1),
	"recedenflg" character(1),
	"hknnum" character(3),
	"khnbuicd_1" character(7),
	"khnbuicd_2" character(7),
	"khnbuicd_3" character(7),
	"termid" character varying(16),
	"opid" character varying(16),
	"creymd" character(8),
	"upymd" character(8),
	"uphms" character(6),
	Constraint "tbl_ptbyomeiwk_primary_key" Primary Key ("hospid", "ptid", "sryka", "sryymd", "rennum")
);

-- TBL_PTBYOMEI → TBL_PTBYOMEIWK   --

insert into tbl_ptbyomeiwk
select * from tbl_ptbyomei;

-- TBL_PTBYOMEI 削除               --

drop table tbl_ptbyomei;

-- TBL_PTBYOMEI 作成     --

CREATE TABLE "tbl_ptbyomei" (
	"hospid" character(24) NOT NULL,
	"ptid" numeric(10,0) NOT NULL,
	"sryka" character(2) NOT NULL,
	"sryymd" character(8) NOT NULL,
	"rennum" numeric(2,0) NOT NULL,
	"khnbyomeicd" character(7),
	"byomeiinputcd_1" character varying(20),
	"byomeiinputcd_2" character varying(20),
	"byomeiinputcd_3" character varying(20),
	"byomeiinputcd_4" character varying(20),
	"byomeiinputcd_5" character varying(20),
	"byomeiinputcd_6" character varying(20),
	"byomeicdsu" numeric(2,0) DEFAULT 0,
	"byomeicd_1" character(7),
	"byomeicd_2" character(7),
	"byomeicd_3" character(7),
	"byomeicd_4" character(7),
	"byomeicd_5" character(7),
	"byomeicd_6" character(7),
	"byomeicd_7" character(7),
	"byomeicd_8" character(7),
	"byomeicd_9" character(7),
	"byomeicd_10" character(7),
	"byomeicd_11" character(7),
	"byomeicd_12" character(7),
	"byomeicd_13" character(7),
	"byomeicd_14" character(7),
	"byomeicd_15" character(7),
	"byomeicd_16" character(7),
	"byomeicd_17" character(7),
	"byomeicd_18" character(7),
	"byomeicd_19" character(7),
	"byomeicd_20" character(7),
	"byomeicd_21" character(7),
	"utagaiflg" character(1),
	"syubyoflg" character(1),
	"manseikbn" numeric(2,0) DEFAULT 0,
	"nyugaikbn" character(1),
	"hkncombi" numeric(4,0) DEFAULT 0,
	"rezeflg" character(1),
	"rezemm" numeric(2,0) DEFAULT 0,
	"tenkikbn" character(1),
	"tenkiymd" character(8),
	"byomei" character varying(160),
	"byomeimoji" numeric(2,0) DEFAULT 0,
	"chartbyomei" character varying(160),
	"chartbyomeimoji" numeric(2,0) DEFAULT 0,
	"byomeihenflg" character(1),
	"hknbyomeiflg" character(1),
	"recedenflg" character(1),
	"hknnum" character(3),
	"khnbuicd_1" character(7),
	"khnbuicd_2" character(7),
	"khnbuicd_3" character(7),
	"dltflg"     character(1),
	"dlt_opid"   character varying(16),
	"termid" character varying(16),
	"opid" character varying(16),
	"creymd" character(8),
	"upymd" character(8),
	"uphms" character(6),
	Constraint "tbl_ptbyomei_primary_key" Primary Key ("hospid", "ptid", "sryka", "sryymd", "rennum")
);

-- TBL_PTBYOMEIWK → TBL_PTBYOMEI   --

insert into tbl_ptbyomei
(hospid,
ptid,
sryka,
sryymd,
rennum,
khnbyomeicd,
byomeiinputcd_1,
byomeiinputcd_2,
byomeiinputcd_3,
byomeiinputcd_4,
byomeiinputcd_5,
byomeiinputcd_6,
byomeicdsu,
byomeicd_1,
byomeicd_2,
byomeicd_3,
byomeicd_4,
byomeicd_5,
byomeicd_6,
byomeicd_7,
byomeicd_8,
byomeicd_9,
byomeicd_10,
byomeicd_11,
byomeicd_12,
byomeicd_13,
byomeicd_14,
byomeicd_15,
byomeicd_16,
byomeicd_17,
byomeicd_18,
byomeicd_19,
byomeicd_20,
byomeicd_21,
utagaiflg,
syubyoflg,
manseikbn,
nyugaikbn,
hkncombi,
rezeflg,
rezemm,
tenkikbn,
tenkiymd,
byomei,
byomeimoji,
chartbyomei,
chartbyomeimoji,
byomeihenflg,
hknbyomeiflg,
recedenflg,
hknnum,
khnbuicd_1,
khnbuicd_2,
khnbuicd_3,
dltflg,
dlt_opid,
termid,
opid,
creymd,
upymd,
uphms
)
select hospid,
ptid,
sryka,
sryymd,
rennum,
khnbyomeicd,
byomeiinputcd_1,
byomeiinputcd_2,
byomeiinputcd_3,
byomeiinputcd_4,
byomeiinputcd_5,
byomeiinputcd_6,
byomeicdsu,
byomeicd_1,
byomeicd_2,
byomeicd_3,
byomeicd_4,
byomeicd_5,
byomeicd_6,
byomeicd_7,
byomeicd_8,
byomeicd_9,
byomeicd_10,
byomeicd_11,
byomeicd_12,
byomeicd_13,
byomeicd_14,
byomeicd_15,
byomeicd_16,
byomeicd_17,
byomeicd_18,
byomeicd_19,
byomeicd_20,
byomeicd_21,
utagaiflg,
syubyoflg,
manseikbn,
nyugaikbn,
hkncombi,
rezeflg,
rezemm,
tenkikbn,
tenkiymd,
byomei,
byomeimoji,
chartbyomei,
chartbyomeimoji,
byomeihenflg,
hknbyomeiflg,
recedenflg,
hknnum,
khnbuicd_1,
khnbuicd_2,
khnbuicd_3,
' ',
'                ',
termid,
opid,
creymd,
upymd,
uphms
from tbl_ptbyomeiwk;

-- TBL_PTBYOMEIWK 削除             --

drop table tbl_ptbyomeiwk;

