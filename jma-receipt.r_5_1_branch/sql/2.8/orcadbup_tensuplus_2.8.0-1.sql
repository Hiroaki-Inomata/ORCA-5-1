--                                    --
-- 点数マスタプラス                   --
--                                    --
-- 薬剤情報全数量記載関連区分の追加   --
--                                    --
--   全数量記載区分(ALLSURYOKBN)      --
--   数量換算単位コード(KANZANTANICD) --
--   数量換算単位名称(KANZANTANINAME) --
--   数量換算値(KANZANCHI)            --
--                                    --
-- Create Date : 2006/03/13           --
--                                    --

-- TBL_TENSUPLUSWK  作成              --

create table TBL_TENSUPLUSWK (
SRYCD				char(9)		not null,
YUKOSTYMD			char(8)		not null,
YUKOEDYMD			char(8)		not null,
SHOYUKOKETA			numeric(2),
SHONAME				varchar(200),
SAIKETUKBN			numeric(1),
INPCHKKBN			numeric(1),
TENPUYAKKBN			char(2),
TERMID   			varchar(16),
OPID     			varchar(16),
CREYMD   			char(8),
UPYMD    			char(8),
UPHMS				char(6),
constraint TBL_TENSUPLUSWK_primary_key primary key (SRYCD,YUKOSTYMD,YUKOEDYMD));

-- TBL_TENSUPLUS → TBL_TENSUPLUSWK   --

insert into TBL_TENSUPLUSWK
select * from TBL_TENSUPLUS;

-- TBL_TENSUPLUS 削除               --

drop table TBL_TENSUPLUS;

-- TBL_TENSUPLUS 作成               --

create table TBL_TENSUPLUS (
SRYCD				char(9)		not null,
YUKOSTYMD			char(8)		not null,
YUKOEDYMD			char(8)		not null,
SHOYUKOKETA			numeric(2),
SHONAME				varchar(200),
SAIKETUKBN			numeric(1),
INPCHKKBN			numeric(1),
TENPUYAKKBN			char(2),
ALLSURYOKBN			char(1),
KANZANTANICD		char(3),
KANZANTANINAME		varchar(24),
KANZANCHI			numeric(10,5),
TERMID   			varchar(16),
OPID     			varchar(16),
CREYMD   			char(8),
UPYMD    			char(8),
UPHMS				char(6),
constraint TBL_TENSUPLUS_primary_key primary key (SRYCD,YUKOSTYMD,YUKOEDYMD));

-- TBL_TENSUPLUSWK → TBL_TENSUPLUS   --

insert into TBL_TENSUPLUS
(SRYCD,
YUKOSTYMD,
YUKOEDYMD,
SHOYUKOKETA,
SHONAME,
SAIKETUKBN,
INPCHKKBN,
TENPUYAKKBN,
ALLSURYOKBN,
KANZANTANICD,
KANZANTANINAME,
KANZANCHI,
TERMID,
OPID,
CREYMD,   
UPYMD,
UPHMS
)
select
SRYCD,
YUKOSTYMD,
YUKOEDYMD,
SHOYUKOKETA,
SHONAME,
SAIKETUKBN,
INPCHKKBN,
TENPUYAKKBN,
' ',
'   ',
'',
0,
TERMID,
OPID,
CREYMD,   
UPYMD,
UPHMS
from TBL_TENSUPLUSWK;

-- TBL_TENSUPLUSWK 削除             --

drop table TBL_TENSUPLUSWK;

-- デフォルト値設定             --

update tbl_tensuplus set ALLSURYOKBN='1',KANZANTANICD='000',KANZANCHI=1 where substr(srycd,1,1)='6';

vacuum tbl_tensuplus;

