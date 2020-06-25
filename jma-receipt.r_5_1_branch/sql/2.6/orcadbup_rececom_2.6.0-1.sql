--                                    --
-- レセプトコメントテーブルの変更     --
--                                    --
-- 項目長の変更                       --
--   　　コメント COMMENT             --
--                800 -> 1600         --
--                                    --
-- Create Date : 2003/03/05           --
--                                    --

-- TBL_RECECOMWK 作成                 --

create table TBL_RECECOMWK (
HOSPID				char(24)	not null,
PTID				numeric(10,0)	not null,
NYUGAIKBN			char(1)         not null  default '0',
SRYKA				char(2)         not null,
SRYYM   			char(6)         not null,
HKNCOMBI  			numeric(4,0)    not null,
SRYDD   			char(2)         not null,
COMMENT             varchar(800),
TERMID  			varchar(16),
OPID    			varchar(16),
CREYMD  			char(8),
UPYMD    			char(8),
UPHMS				char(6),
constraint TBL_RECECOMWK_primary_key primary key (HOSPID,PTID,NYUGAIKBN,SRYKA,
SRYYM,HKNCOMBI,SRYDD));

-- TBL_RECECOM → TBL_RECECOMWK   --

insert into TBL_RECECOMWK
select * from TBL_RECECOM;

-- TBL_RECECOM 削除               --

drop table TBL_RECECOM;

-- TBL_RECECOM 作成               --

create table TBL_RECECOM (
HOSPID				char(24)	not null,
PTID				numeric(10,0)	not null,
NYUGAIKBN			char(1)         not null  default '0',
SRYKA				char(2)         not null,
SRYYM   			char(6)         not null,
HKNCOMBI  			numeric(4,0)    not null,
SRYDD   			char(2)         not null,
COMMENT             varchar(1600),
TERMID  			varchar(16),
OPID    			varchar(16),
CREYMD  			char(8),
UPYMD    			char(8),
UPHMS				char(6),
constraint TBL_RECECOM_primary_key primary key (HOSPID,PTID,NYUGAIKBN,SRYKA,
SRYYM,HKNCOMBI,SRYDD));

-- TBL_RECECOMWK → TBL_RECECOM   --

insert into TBL_RECECOM
select * from TBL_RECECOMWK;

-- TBL_RECECOMWK 削除             --

drop table TBL_RECECOMWK;
