--                                     --
-- 定期請求履歴テーブル                --
--                                     --
-- 項目追加                            --
--   期別     (KBT)                    --
--                                     --
-- 項目削除                            --
--   作成区分         (SAKKBN)         --
--   請求開始年月日   (SKYSTYMD)       --
--   請求終了年月日   (SKYEDYMD)       --
--   請求点数（合計） (SKYTEN)         --
--   請求金額（合計） (SKYMONEY)       --
--   再請求区分       (SAISKYKBN)      --
--                                     --
-- Create Date : 2006/01/17            --
--                                     --

drop table tbl_teikirrk;

CREATE TABLE "tbl_teikirrk" (
	"sryym" character(6) NOT NULL,
	"kbt" character(1),
	"termid" character varying(16),
	"opid" character varying(16),
	"creymd" character varying(8),
	"upymd" character varying(8),
	"uphms" character varying(6),
	Constraint "tbl_teikirrk_primary_key" Primary Key ("sryym", "kbt")
);

insert into tbl_teikirrk (
sryym,
kbt,
termid,
opid,
creymd,
upymd,
uphms
)
select 
sryym,
kbt,
'',
'',
'',
'',
''
from tbl_ptteikirrk
group by sryym,kbt;
