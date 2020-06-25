--                                    --
-- 入力セット                         --
--                                    --
-- 項目の追加                         --
--   　　有効開始年月日 YUKOSTYMD     --
--   　　有効終了年月日 YUKOEDYMD     --
--                                    --
-- Create Date : 2006/03/01           --
--                                    --

-- TBL_INPUTSETWK 作成               --

CREATE TABLE "tbl_inputsetwk" (
	"hospid" character(24) NOT NULL,
	"setcd" character(6) NOT NULL,
	"setseq" numeric(3,0) NOT NULL,
	"inputcd" character(9),
	"suryo1" numeric(10,3) DEFAULT 0,
	"suryo2" numeric(10,3) DEFAULT 0,
	"kaisu" numeric(3,0) DEFAULT 0,
	"coment" character varying(80),
	"atai1" character(8),
	"atai2" character(8),
	"atai3" character(8),
	"atai4" character(8),
	"termid" character varying(16),
	"opid" character varying(16),
	"creymd" character(8),
	"upymd" character(8),
	"uphms" character(6),
	Constraint "tbl_inputsetwk_primary_key" Primary Key ("hospid", "setcd", "setseq")
);

-- TBL_INPUTSET → TBL_INPUTSETWK   --

insert into TBL_INPUTSETWK
select * from TBL_INPUTSET;

-- TBL_INPUTSET 削除               --

drop table TBL_INPUTSET;

-- TBL_INPUTSET 作成               --

CREATE TABLE "tbl_inputset" (
	"hospid" character(24) NOT NULL,
	"setcd" character(6) NOT NULL,
	"yukostymd" character(8) NOT NULL,
	"yukoedymd" character(8) NOT NULL,
	"setseq" numeric(3,0) NOT NULL,
	"inputcd" character(9),
	"suryo1" numeric(10,3) DEFAULT 0,
	"suryo2" numeric(10,3) DEFAULT 0,
	"kaisu" numeric(3,0) DEFAULT 0,
	"coment" character varying(80),
	"atai1" character(8),
	"atai2" character(8),
	"atai3" character(8),
	"atai4" character(8),
	"termid" character varying(16),
	"opid" character varying(16),
	"creymd" character(8),
	"upymd" character(8),
	"uphms" character(6),
	Constraint "tbl_inputset_primary_key" Primary Key ("hospid", "setcd", "yukostymd", "yukoedymd", "setseq")
);

-- TBL_INPUTSETWK → TBL_INPUTSET   --

insert into TBL_INPUTSET
(hospid,
setcd,
yukostymd,
yukoedymd,
setseq,
inputcd,
suryo1,
suryo2,
kaisu,
coment,
atai1,
atai2,
atai3,
atai4,
termid,
opid,
creymd,
upymd,
uphms
)
select
hospid,
setcd,
'00000000',
'99999999',
setseq,
inputcd,
suryo1,
suryo2,
kaisu,
coment,
atai1,
atai2,
atai3,
atai4,
termid,
opid,
creymd,
upymd,
uphms
from TBL_INPUTSETWK;

-- TBL_INPUTSETWK 削除             --

drop table TBL_INPUTSETWK;
