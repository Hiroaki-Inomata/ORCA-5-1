--                                --
-- ORCA DataBase Table Definition --
--                                --
-- 公費付加情報                   --
--                                --
-- Table Name  : TBL_KOHPLUS      --
-- Create Date : 2003/08/25       --
--                                --

create table TBL_KOHPLUS (
HOSPID			char(24) not null,
PREFNUM			char(2)	not null,
CITYNUM			char(3)	not null,
KOHNUM			char(3)	not null,
PTKBN			char(1)	not null,
YUKOSTYMD		char(8)	not null,
YUKOEDYMD		char(8),
FTNKBN			char(1),
NYUSKJRYYKBN		char(1),
RECESKYKBN		char(1),
SEIDONAME		varchar(100),
TERMID			varchar(16),
OPID			varchar(16),
CREYMD			char(8),
UPYMD			char(8),
UPHMS			char(6),
constraint TBL_KOHPLUS_primary_key primary key (HOSPID,PREFNUM,CITYNUM,
KOHNUM,PTKBN,YUKOSTYMD));
