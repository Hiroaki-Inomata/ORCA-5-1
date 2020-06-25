--                                    --
-- レセプト管理テーブルの変更         --
--                                    --
--                                    --
-- 項目の追加                         --
--   　　　　日付指定　(STYMD)        --
--                     (EDYMD)        --
--   　　　　処理種別　(SYORISBT)     --
--                                    --
-- Create Date : 2005/01/13           --
--                                    --

-- TBL_RECEKANRIWK 作成               --

create table TBL_RECEKANRIWK (
PRTID               char(4)		not null,
SYOKBN              char(1)     not null, 
CREYMD              char(8)		not null,
CREHMS              char(6)		not null,
TEISYUTUSAKI        char(1)		not null,
RECESYUBETU         char(4)		not null,
NYUGAIKBN           char(1),
SRYYM               char(6),
PAGE                numeric(5,0)	default 0,
KENSU               numeric(5,0)	default 0,
RECEKBN             char(1),
TERMID              varchar(16),
constraint TBL_RECEKANRIWK_primary_key primary key (PRTID,SYOKBN,CREYMD,CREHMS,TEISYUTUSAKI,RECESYUBETU));


-- TBL_RECEKANRI → TBL_RECEKANRIWK   --

insert into TBL_RECEKANRIWK
select * from TBL_RECEKANRI;

-- TBL_RECEKANRI 削除               --

drop table TBL_RECEKANRI;

-- TBL_RECEKANRI 作成               --

create table TBL_RECEKANRI (
PRTID               char(4)		not null,
SYOKBN              char(1)     not null, 
CREYMD              char(8)		not null,
CREHMS              char(6)		not null,
TEISYUTUSAKI        char(1)		not null,
RECESYUBETU         char(4)		not null,
NYUGAIKBN           char(1),
SRYYM               char(6),
PAGE                numeric(5,0)	default 0,
KENSU               numeric(5,0)	default 0,
RECEKBN             char(1),
SYORISBT            char(1),
STYMD               char(8),
EDYMD               char(8),
TERMID              varchar(16),
constraint TBL_RECEKANRI_primary_key primary key (PRTID,SYOKBN,CREYMD,CREHMS,TEISYUTUSAKI,RECESYUBETU));

-- TBL_RECEKANRIWK → TBL_RECEKANRI  （一括分） --

insert into TBL_RECEKANRI
(PRTID,
SYOKBN,
CREYMD,
CREHMS,
TEISYUTUSAKI,
RECESYUBETU,
NYUGAIKBN,
SRYYM,
PAGE,
KENSU, 
RECEKBN,  
TERMID
)
select
PRTID,
SYOKBN,
CREYMD,
CREHMS,
TEISYUTUSAKI,
RECESYUBETU,
NYUGAIKBN,
SRYYM,
PAGE,
KENSU, 
RECEKBN,  
TERMID from TBL_RECEKANRIWK where SYOKBN = '1';


-- TBL_RECEKANRIWK → TBL_RECEKANRI  （個別分） --

insert into TBL_RECEKANRI
(PRTID,
SYOKBN,
CREYMD,
CREHMS,
TEISYUTUSAKI,
RECESYUBETU,
NYUGAIKBN,
SRYYM,
PAGE,
KENSU, 
RECEKBN,  
SYORISBT,
TERMID
)
select
PRTID,
SYOKBN,
CREYMD,
CREHMS,
TEISYUTUSAKI,
RECESYUBETU,
NYUGAIKBN,
SRYYM,
PAGE,
KENSU, 
RECEKBN,
'0',  
TERMID from TBL_RECEKANRIWK where SYOKBN = '2';



-- TBL_RECEKANRIWK 削除             --

drop table TBL_RECEKANRIWK;
