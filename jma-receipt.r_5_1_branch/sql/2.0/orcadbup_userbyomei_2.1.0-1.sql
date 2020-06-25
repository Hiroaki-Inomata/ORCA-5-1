--                                    --
--  自院病名テーブルの変更            --
--                                    --
-- 項目の追加                         --
--     病名種別       BYOMEISBT       --
--     基本病名コード KHNBYOMEICD     --
--     基本部位コード KHNBUICD        --
--                                    --
--     病名コード   ２１に変更        --
--     病名コード数 ９（０２）に変更  --
--                                    --
-- Create Date : 2003/09/17           --
--                                    --

-- TBL_USERBYOMEIWK 作成              --

CREATE TABLE "tbl_userbyomeiwk" (
	"hospid" character(24) NOT NULL,
	"cdsyu" character(1) NOT NULL,
	"byomeiinputcd" character varying(20) NOT NULL,
	"bunruicd" character(3),
	"dspseq" numeric(2,0) DEFAULT 50,
	"byomeicdsu" numeric(1,0) DEFAULT 0,
	"byomeicd_1" character(7),
	"byomeicd_2" character(7),
	"byomeicd_3" character(7),
	"byomeicd_4" character(7),
	"byomeicd_5" character(7),
	"byomeicd_6" character(7),
	"utagaiflg" character(1),
	"manseikbn" numeric(2,0) DEFAULT 0,
	"byomei" character varying(160),
	"byomeimoji" numeric(2,0) DEFAULT 0,
	"chartbyomei" character varying(160),
	"chartbyomeimoji" numeric(2,0) DEFAULT 0,
	"byomeihenflg" character(1),
	"hknbyomeiflg" character(1),
	"termid" character varying(16),
	"opid" character varying(16),
	"creymd" character(8),
	"upymd" character(8),
	"uphms" character(6),
	Constraint "tbl_userbyomeiwk_primary_key" Primary Key ("hospid", "cdsyu", "byomeiinputcd")
);

-- TBL_USERBYOMEI → TBL_USERBYOMEIWK   --

insert into tbl_userbyomeiwk
select * from tbl_userbyomei;

-- TBL_USERBYOMEI 削除               --

drop table tbl_userbyomei;

-- TBL_USERBYOMEI 作成               --

CREATE TABLE "tbl_userbyomei" (
	"hospid" character(24) NOT NULL,
	"cdsyu" character(1) NOT NULL,
	"byomeiinputcd" character varying(20) NOT NULL,
	"bunruicd" character(3),
	"dspseq" numeric(2,0) DEFAULT 50,
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
	"manseikbn" numeric(2,0) DEFAULT 0,
	"byomei" character varying(160),
	"byomeimoji" numeric(2,0) DEFAULT 0,
	"chartbyomei" character varying(160),
	"chartbyomeimoji" numeric(2,0) DEFAULT 0,
	"byomeihenflg" character(1),
	"hknbyomeiflg" character(1),
	"byomeisbt" character(1),
	"khnbyomeicd" character(7),
	"khnbuicd_1" character(7),
	"khnbuicd_2" character(7),
	"khnbuicd_3" character(7),
	"termid" character varying(16),
	"opid" character varying(16),
	"creymd" character(8),
	"upymd" character(8),
	"uphms" character(6),
	Constraint "tbl_userbyomei_primary_key" Primary Key ("hospid", "cdsyu", "byomeiinputcd")
);

-- TBL_USERBYOMEIWK → TBL_USERBYOMEI   --

insert into tbl_userbyomei
(hospid,
cdsyu,
byomeiinputcd,
bunruicd,
dspseq,
byomeicdsu,
byomeicd_1,
byomeicd_2,
byomeicd_3,
byomeicd_4,
byomeicd_5,
byomeicd_6,
utagaiflg,
manseikbn,
byomei,
byomeimoji,
chartbyomei,
chartbyomeimoji,
byomeihenflg,
hknbyomeiflg,
byomeisbt,
termid,
opid,
creymd,
upymd,
uphms
)
select hospid,
cdsyu,
byomeiinputcd,
bunruicd,
dspseq,
byomeicdsu,
byomeicd_1,
byomeicd_2,
byomeicd_3,
byomeicd_4,
byomeicd_5,
byomeicd_6,
utagaiflg,
manseikbn,
byomei,
byomeimoji,
chartbyomei,
chartbyomeimoji,
byomeihenflg,
hknbyomeiflg,
'0',
termid,
opid,
creymd,
upymd,
uphms
 from tbl_userbyomeiwk;

-- TBL_USERBYOMEIWK 削除             --

drop table tbl_userbyomeiwk;

