--                                      --
-- レセプト明細テーブルの変更           --
-- 項目追加 (編綴対応)                  --
--   　　　　診療科  　(SRYKA)          --
--   　　　　県内外印刷順(PREFKBN_SRT)  --
--   　　　　保険者印刷順(HKNJANUM_SRT) --
--   　　　　レセプト種別印刷順         --
--                    (RECESYUBETU_SRT) --
--   　　　　在総診識別(ZAITAKU)        --
--   　　　　長期識別  (CHOUKI)         --
--                                      --
-- Create Date : 2005/12/06             --
--                                      --


-- TBL_RECEPRTWWK 作成                 --
create table TBL_RECEPRTWWK (
PRTID				char(4)		not null,
RECEYM				char(6)		not null,
CREYMD				char(8)		not null,
CREHMS				char(6)		not null,
PREFKBN				char(1)		not null,
PREFNUM				char(2)		not null,
HKNJANUM			char(8)		not null,
TEKSTYMD			char(8)     not null,
TEISYUTUSAKI		char(1)		not null,
RECESYUBETU			char(4)		not null,
PTNUM				char(20)	not null,
SRYYM				char(6)		default '000000',
RENNUM				numeric(2,0)	default 0,
RECENUM				numeric(6,0)	default 0,
HKNKBN				char(1),
KANANAME			varchar(100),
TERMID				varchar(32),
PRTJYUN				numeric(2,0),
PRTRENNUM			numeric(6,0),
NYUGAIKBN			char(1),
FORMID				char(20),
PRTDATA				varchar(10000),
constraint TBL_RECEPRTWWK_primary_key primary key (PRTID,RECEYM,
CREYMD,CREHMS,PREFKBN,PREFNUM,HKNJANUM,TEKSTYMD,TEISYUTUSAKI,RECESYUBETU,PTNUM,SRYYM,RENNUM,RECENUM));

-- TBL_RECEPRT → TBL_RECEPRTWWK   --

insert into TBL_RECEPRTWWK
select * from TBL_RECEPRT;

-- TBL_RECEPRT 削除               --

drop table TBL_RECEPRT;

-- TBL_RECEPRT 作成               --

create table TBL_RECEPRT (
PRTID				char(4)		not null,
RECEYM				char(6)		not null,
CREYMD				char(8)		not null,
CREHMS				char(6)		not null,
TEISYUTUSAKI		char(1)		not null,
SRYKA				char(2)		not null default '00',
PREFKBN_SRT         char(2)		not null default '99',
HKNJANUM_SRT        char(1)		not null default '9',
HKNJANUM			char(8)		not null,
TEKSTYMD			char(8)     not null,
RECESYUBETU_SRT     char(3)		not null default '999',
RECESYUBETU			char(4)		not null,
PREFKBN				char(1)		not null,
PREFNUM				char(2)		not null,
PTNUM				char(20)	not null,
SRYYM				char(6)		default '000000',
RENNUM				numeric(2,0)	default 0,
RECENUM				numeric(6,0)	default 0,
HKNKBN				char(1),
KANANAME			varchar(100),
PRTJYUN				numeric(2,0),
PRTRENNUM			numeric(6,0),
NYUGAIKBN			char(1),
ZAITAKU				char(1)		not null default '9',
CHOUKI				char(1)		not null default '9',
HKNJANUM_S			char(8)		not null default '99999999',
FORMID				char(20),
PRTDATA				varchar(10000),
TERMID				varchar(64),
OPID				varchar(16),
constraint TBL_RECEPRT_primary_key primary key (PRTID,RECEYM,
CREYMD,CREHMS,TEISYUTUSAKI,SRYKA,PREFKBN_SRT,HKNJANUM_SRT,HKNJANUM,RECESYUBETU,TEKSTYMD,ZAITAKU,CHOUKI,PTNUM,RENNUM,RECENUM));

-- TBL_RECEPRTWWK → TBL_RECEPRT   --

insert into TBL_RECEPRT
(PRTID,
RECEYM,
CREYMD,
CREHMS,
PREFKBN,
PREFNUM,
HKNJANUM,
TEKSTYMD,
TEISYUTUSAKI, 
RECESYUBETU,  
PTNUM,
SRYYM,	
RENNUM,
RECENUM,
HKNKBN,	
KANANAME,
PRTJYUN,    
PRTRENNUM,
NYUGAIKBN,
FORMID,   
PRTDATA,
TERMID
)
select
PRTID,
RECEYM,
CREYMD,
CREHMS,
PREFKBN,
PREFNUM,
HKNJANUM,
TEKSTYMD,
TEISYUTUSAKI, 
RECESYUBETU,  
PTNUM,
SRYYM,	
RENNUM,
RECENUM,
HKNKBN,	
KANANAME,
PRTJYUN,    
PRTRENNUM,
NYUGAIKBN,
FORMID,   
PRTDATA,
TERMID
from TBL_RECEPRTWWK;

-- TBL_RECEPRTWWK 削除             --

drop table TBL_RECEPRTWWK;
