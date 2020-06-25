--                                --
-- ORCA DataBase Table Definition --
--                                --
-- 統計処理パラメータ覚書         --
--                                --
-- Table Name  : TBL_TOUKEIMEMO   --
-- Create Date : 2004/09/29       --
--                                --

create table tbl_toukeimemo (
PGID				varchar(20) not null,
KANRICD				char(4) not null,
KBNCD				char(8) not null,
STYUKYMD                        char(8) not null,
EDYUKYMD                        char(8) not null,
MEMO				varchar(6000),
TERMID  			varchar(32),
OPID    			varchar(16),
CREYMD  			char(8),
UPYMD    			char(8),
UPHMS				char(6),
constraint tbl_toukeimemo_primary_key primary key (PGID,KANRICD,KBNCD,STYUKYMD,EDYUKYMD));
