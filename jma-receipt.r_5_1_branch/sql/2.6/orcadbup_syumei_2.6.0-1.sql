--                                    --
--  収納明細テーブルの変更              --
--                                    --
-- 項目の追加                         --
--     日報キー	      DAILYKEY        --
--     作成時間	      CREHMS          --
--                                    --
-- Update Date : 2005/07/25           --
--                                    --
CREATE TABLE "tbl_syumeiwk" (
	"hospid" character(24) NOT NULL,
	"nyugaikbn" character(1) NOT NULL,
	"ptid" numeric(10,0) NOT NULL,
	"denpnum" numeric(7,0) NOT NULL,
	"denpedanum" numeric(2,0) NOT NULL,
	"nyukinrennum" numeric(3,0) DEFAULT 0,
	"sryka" character(2),
	"meisaijoutaikbn" character(1),
	"sky_printymd" character(8),
	"sky_reprintymd" character(8),
	"ryosyu_printymd" character(8),
	"ryosyu_reprintymd" character(8),
	"skymoney" numeric(7,0) DEFAULT 0,
	"nyuhen_kbn" character(1),
	"nyuhen_money" numeric(7,0) DEFAULT 0,
	"nyuhen_ymd" character(8),
	"nyukin_hoho" character(2),
	"joutaikbn" character(1),
	"termid" character varying(16),
	"opid" character varying(16),
	"creymd" character(8),
	"upymd" character(8),
	"uphms" character(6)
);

-- tbl_syumei -> tbl_syumeiwk --

insert into tbl_syumeiwk
select * from tbl_syumei;

-- drop tbl_syumei --
drop table tbl_syumei;

-- create tbl_syumei --
CREATE TABLE "tbl_syumei" (
	"hospid" character(24) NOT NULL,
	"nyugaikbn" character(1) NOT NULL,
	"ptid" numeric(10,0) NOT NULL,
	"denpnum" numeric(7,0) NOT NULL,
	"denpedanum" numeric(2,0) NOT NULL,
	"nyukinrennum" numeric(3,0) DEFAULT 0,
	"sryka" character(2),
	"meisaijoutaikbn" character(1),
	"sky_printymd" character(8),
	"sky_reprintymd" character(8),
	"ryosyu_printymd" character(8),
	"ryosyu_reprintymd" character(8),
	"skymoney" numeric(7,0) DEFAULT 0,
	"nyuhen_kbn" character(1),
	"nyuhen_money" numeric(7,0) DEFAULT 0,
	"nyuhen_ymd" character(8),
	"nyukin_hoho" character(2),
	"joutaikbn" character(1),
	"dailykey" character(12),
	"termid" character varying(16),
	"opid" character varying(16),
	"creymd" character(8),
	"crehms" character(6),
	"upymd" character(8),
	"uphms" character(6),
	Constraint "tbl_syumei_primary_key" Primary Key ("hospid", "nyugaikbn", "ptid", "denpnum", "denpedanum")
);

-- tbl_syumeiwk -> tbl_syumei --
insert into tbl_syumei
(hospid,
nyugaikbn,
ptid,
denpnum,
denpedanum,
nyukinrennum,
sryka,
meisaijoutaikbn,
sky_printymd,
sky_reprintymd,
ryosyu_printymd,
ryosyu_reprintymd,
skymoney,
nyuhen_kbn,
nyuhen_money,
nyuhen_ymd,
nyukin_hoho,
joutaikbn,
dailykey,
termid,
opid,
creymd,
crehms,
upymd,
uphms )
select
hospid,
nyugaikbn,
ptid,
denpnum,
denpedanum,
nyukinrennum,
sryka,
meisaijoutaikbn,
sky_printymd,
sky_reprintymd,
ryosyu_printymd,
ryosyu_reprintymd,
skymoney,
nyuhen_kbn,
nyuhen_money,
nyuhen_ymd,
nyukin_hoho,
joutaikbn,
nyuhen_ymd || '0000',
termid,
opid,
creymd,
'000000',
upymd,
uphms 
from tbl_syumeiwk;

-- tbl_syumeiwk 削除 --

drop table tbl_syumeiwk;



