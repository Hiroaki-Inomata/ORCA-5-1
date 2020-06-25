--                                    --
--  受付テーブルの変更                --
--                                    --
-- 項目の追加                         --
--     送受信フラグ  SRFLG            --
--                                    --
-- Update Date : 2005/03/15           --
--                                    --

-- TBL_UKETUKEWK 作成                 --

CREATE TABLE "tbl_uketukewk" (
	"hospid" character(24) NOT NULL,
	"ukeymd" character(8) NOT NULL,
	"ukeid" numeric(5,0) NOT NULL,
	"uketime" character(6),
	"ptid" numeric(10,0),
	"name" character varying(100),
	"sryka" character(2),
	"drcd" character(5),
	"srynaiyo" character(2),
	"hkncombi" numeric(4,0) DEFAULT 0,
	"kaikeitime" numeric(4,0) DEFAULT 0,
	"keyyyktime" numeric(4,0) DEFAULT 0,
	"yyktime" numeric(4,0) DEFAULT 0,
	"yykid" numeric(2,0) DEFAULT 0,
	"hyojijun" numeric(4,0) DEFAULT 0,
	"kakren" character(2),
	"termid" character varying(16),
	"opid" character varying(16),
	"creymd" character(8),
	"upymd" character(8),
	"uphms" character(6),
	Constraint "tbl_uketukewk_primary_key" Primary Key ("hospid", "ukeymd", "ukeid")
);

-- TBL_UKETUKE → TBL_UKETUKEWK      --

insert into tbl_uketukewk
select * from tbl_uketuke;

-- TBL_UKETUKE 削除                  --

drop table tbl_uketuke;

-- TBL_UKETUKE 作成                  --

CREATE TABLE "tbl_uketuke" (
	"hospid" character(24) NOT NULL,
	"ukeymd" character(8) NOT NULL,
	"ukeid" numeric(5,0) NOT NULL,
	"uketime" character(6),
	"ptid" numeric(10,0),
	"name" character varying(100),
	"sryka" character(2),
	"drcd" character(5),
	"srynaiyo" character(2),
	"hkncombi" numeric(4,0) DEFAULT 0,
	"kaikeitime" numeric(4,0) DEFAULT 0,
	"keyyyktime" numeric(4,0) DEFAULT 0,
	"yyktime" numeric(4,0) DEFAULT 0,
	"yykid" numeric(2,0) DEFAULT 0,
	"hyojijun" numeric(4,0) DEFAULT 0,
	"kakren" character(2),
	"srflg" character(2),
	"termid" character varying(16),
	"opid" character varying(16),
	"creymd" character(8),
	"upymd" character(8),
	"uphms" character(6),
	Constraint "tbl_uketuke_primary_key" Primary Key ("hospid", "ukeymd", "ukeid")
);

-- TBL_UKETUKEWK → TBL_UKETUKE   --

insert into tbl_uketuke
(hospid,
ukeymd,
ukeid,
uketime,
ptid,
name,
sryka,
drcd,
srynaiyo,
hkncombi,
kaikeitime,
keyyyktime,
yyktime,
yykid,
hyojijun,
kakren,
srflg,
termid,
opid,
creymd,
upymd,
uphms
)
select hospid,
ukeymd,
ukeid,
uketime,
ptid,
name,
sryka,
drcd,
srynaiyo,
hkncombi,
kaikeitime,
keyyyktime,
yyktime,
yykid,
hyojijun,
kakren,
'  ',
termid,
opid,
creymd,
upymd,
uphms
 from tbl_uketukewk;

-- TBL_UKETUKEWK 削除             --

drop table tbl_uketukewk;

