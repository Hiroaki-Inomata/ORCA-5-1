--                                     --
-- ´µ¼ÔÄê´üÀÁµáÍúÎò¥Æ¡¼¥Ö¥ë            --
--                                     --
-- ºï½ü¥Õ¥é¥° (DELFLG)                 --
-- ½èÍı¶èÊ¬ (SYOKBN)                   --
-- ´üÊÌ     (KBT)                      --
-- ¤òÄÉ²Ã                              --
-- Create Date : 2006/01/16            --
--                                     --

CREATE TABLE "tbl_ptteikirrkwk" (
	"hospid" character(24) NOT NULL,
	"ptid" numeric(10,0) NOT NULL,
	"sryym" character(6) NOT NULL,
	"sakkbn" character(1) NOT NULL,
	"hkncombinum" numeric(4,0) NOT NULL,
	"skystymd" character(8),
	"skyedymd" character(8),
	"skyten" numeric(9,0),
	"skymoney_tax_sai" numeric(7,0),
	"skymoney" numeric(9,0),
	"rrknum" numeric(3,0),
	"denpnum" numeric(7,0),
	"denpedanum" numeric(2,0),
	"skyupdflg" numeric(1,0),
	"termid" character varying(16),
	"opid" character varying(16),
	"creymd" character varying(8),
	"upymd" character varying(8),
	"uphms" character varying(6)
);


-- TBL_PTTEIKIRRK ¢ª TBL_PTTEIKIRRKWK   --

insert into tbl_ptteikirrkwk
select * from tbl_ptteikirrk;

DROP TABLE tbl_ptteikirrk;

CREATE TABLE "tbl_ptteikirrk" (
	"hospid" character(24) NOT NULL,
	"ptid" numeric(10,0) NOT NULL,
	"sryym" character(6) NOT NULL,
	"sakkbn" character(1) NOT NULL,
	"hkncombinum" numeric(4,0) NOT NULL,
	"denpnum" numeric(7,0),
	"delflg" numeric(1,0),
	"syokbn" character(1),
	"kbt" 	character(1),
	"skystymd" character(8),
	"skyedymd" character(8),
	"skyten" numeric(9,0),
	"skymoney_tax_sai" numeric(7,0),
	"skymoney" numeric(9,0),
	"rrknum" numeric(3,0),
	"denpedanum" numeric(2,0),
	"skyupdflg" numeric(1,0),
	"termid" character varying(16),
	"opid" character varying(16),
	"creymd" character varying(8),
	"upymd" character varying(8),
	"uphms" character varying(6),
	Constraint "tbl_ptteikirrk_primary_key" Primary Key ("hospid", "ptid", "sryym", "sakkbn", "hkncombinum", "denpnum")
);

insert into tbl_ptteikirrk (
hospid,
ptid,
sryym,
sakkbn,
hkncombinum,
denpnum,
delflg,
syokbn,
kbt,
skystymd,
skyedymd,
skyten,
skymoney_tax_sai,
skymoney,
rrknum,
denpedanum,
skyupdflg,
termid,
opid,
creymd,
upymd,
uphms
)
select
hospid,
ptid,
sryym,
sakkbn,
hkncombinum,
denpnum,
0,
'1',
sakkbn,
skystymd,
skyedymd,
skyten,
skymoney_tax_sai,
skymoney,
rrknum,
denpedanum,
skyupdflg,
termid,
opid,
creymd,
upymd,
uphms
from tbl_ptteikirrkwk;

drop table tbl_ptteikirrkwk;

update tbl_ptteikirrk set kbt = '1'
from tbl_syskanri a
where sakkbn = '9'
and sryym||'01' >= a.styukymd
and sryym||'01' <= a.edyukymd
and substr(a.kanritbl,1,2) = '99' ;


update tbl_ptteikirrk set kbt = '2'
from tbl_syskanri a
where sakkbn = '9'
and sryym||'01' >= a.styukymd
and sryym||'01' <= a.edyukymd
and substr(a.kanritbl,3,2) = '99' ;


update tbl_ptteikirrk set kbt = '3'
from tbl_syskanri a
where sakkbn = '9'
and a.kanricd = '5010'
and sryym||'01' >= a.styukymd
and sryym||'01' <= a.edyukymd
and substr(a.kanritbl,5,2) = '99' ;

