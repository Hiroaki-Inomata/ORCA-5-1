--                                --
-- ORCA DataBase Table Definition --
--                                --
-- 統計処理パラメータ覚書         --
--                                --
-- Table Name    : TBL_TOUKEIMEMO --
-- 更新日        : 2006/05/17     --
-- パラメータ初期値追加           --

\set ON_ERROR_STOP

create table tbl_toukeimemowk (
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
UPHMS				char(6)
);


insert into tbl_toukeimemowk
select * from tbl_toukeimemo;

drop table tbl_toukeimemo;

create table tbl_toukeimemo (
PGID				varchar(20) not null,
KANRICD				char(4) not null,
KBNCD				char(8) not null,
STYUKYMD                        char(8) not null,
EDYUKYMD                        char(8) not null,
PARA1				varchar(20),
PARA2				varchar(20),
PARA3				varchar(20),
PARA4				varchar(20),
PARA5				varchar(20),
PARA6				varchar(20),
PARA7				varchar(20),
PARA8				varchar(20),
MEMO				varchar(6000),
TERMID  			varchar(32),
OPID    			varchar(16),
CREYMD  			char(8),
UPYMD    			char(8),
UPHMS				char(6),
constraint tbl_toukeimemo_primary_key primary key (PGID,KANRICD,KBNCD,STYUKYMD,EDYUKYMD));

insert into tbl_toukeimemo
(
PGID     ,
KANRICD	 ,
KBNCD	 ,
STYUKYMD ,
EDYUKYMD ,
PARA1	 ,
PARA2	 ,
PARA3	 ,
PARA4	 ,
PARA5	 ,
PARA6	 ,
PARA7	 ,
PARA8	 ,
MEMO	 ,
TERMID   ,
OPID     ,
CREYMD   ,
UPYMD    ,
UPHMS	
)
select  
PGID     ,
KANRICD	 ,
KBNCD	 ,
STYUKYMD ,
EDYUKYMD ,
''	 ,
''	 ,
''	 ,
''	 ,
''	 ,
''	 ,
''	 ,
''	 ,
MEMO	 ,
TERMID   ,
OPID     ,
CREYMD   ,
UPYMD    ,	
UPHMS	
from tbl_toukeimemowk;

drop table tbl_toukeimemowk;
