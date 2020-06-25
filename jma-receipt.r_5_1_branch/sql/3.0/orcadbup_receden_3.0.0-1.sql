--                                    --
-- レセプト電算データマスタ           --
-- 項目の追加                         --
--   　　症状詳記区分 SJKBN           --
--                                    --
-- Create Date : 2006/06/16           --
--                                    --

\set ON_ERROR_STOP

-- TBL_RECEDENWK  作成              --

create table TBL_RECEDENWK (
HOSPID			char(24)	not null,
SRYYM			numeric(6)	default 0,
NYUGAIKBN		char(1)		not null,
PTID			numeric(10)	default 0,
RECEKA			char(2)		not null,
TEISYUTUSAKI		numeric(1,0)	default 0,
RECESYUBETU		char(4)		not null,
HKNJANUM		char(8)		not null,
HOJOKBN_KEY		char(1)		not null,
TEKSTYMD		char(8)		not null,
RECKBN			char(1)		not null,
RENNUM			numeric(4,0)	default 0,
CREYMD			char(8),
TOTALTEN		numeric(10,0)	default 0,
RECEDATA		varchar(2500),
constraint TBL_RECEDENWK_primary_key primary key (HOSPID,SRYYM,NYUGAIKBN,
PTID,RECEKA,TEISYUTUSAKI,RECESYUBETU,HKNJANUM,HOJOKBN_KEY,TEKSTYMD,
RECKBN,RENNUM));

-- TBL_RECEDEN → TBL_RECEDENWK   --

insert into TBL_RECEDENWK
select * from TBL_RECEDEN;

-- TBL_RECEDEN 削除               --

drop table TBL_RECEDEN;

-- TBL_RECEDEN 作成               --

create table TBL_RECEDEN (
HOSPID			char(24)	not null,
SRYYM			numeric(6)	default 0,
NYUGAIKBN		char(1)		not null,
PTID			numeric(10)	default 0,
RECEKA			char(2)		not null,
TEISYUTUSAKI		numeric(1,0)	default 0,
RECESYUBETU		char(4)		not null,
HKNJANUM		char(8)		not null,
HOJOKBN_KEY		char(1)		not null,
TEKSTYMD		char(8)		not null,
RECKBN			char(1)		not null,
RENNUM			numeric(4,0)	default 0,
CREYMD			char(8),
TOTALTEN		numeric(10,0)	default 0,
SJKBN			char(2),
RECEDATA		varchar(2500),
constraint TBL_RECEDEN_primary_key primary key (HOSPID,SRYYM,NYUGAIKBN,
PTID,RECEKA,TEISYUTUSAKI,RECESYUBETU,HKNJANUM,HOJOKBN_KEY,TEKSTYMD,
RECKBN,RENNUM));

-- TBL_RECEDENWK → TBL_RECEDEN   --

insert into TBL_RECEDEN
(HOSPID,
SRYYM,
NYUGAIKBN,
PTID,
RECEKA,
TEISYUTUSAKI,
RECESYUBETU,
HKNJANUM,
HOJOKBN_KEY,
TEKSTYMD,
RECKBN,
RENNUM,
CREYMD,
TOTALTEN,
SJKBN,
RECEDATA
)
select
HOSPID,
SRYYM,
NYUGAIKBN,
PTID,
RECEKA,
TEISYUTUSAKI,
RECESYUBETU,
HKNJANUM,
HOJOKBN_KEY,
TEKSTYMD,
RECKBN,
RENNUM,
CREYMD,
TOTALTEN,
'07',
RECEDATA
from TBL_RECEDENWK where reckbn = '2';

insert into TBL_RECEDEN
(HOSPID,
SRYYM,
NYUGAIKBN,
PTID,
RECEKA,
TEISYUTUSAKI,
RECESYUBETU,
HKNJANUM,
HOJOKBN_KEY,
TEKSTYMD,
RECKBN,
RENNUM,
CREYMD,
TOTALTEN,
SJKBN,
RECEDATA
)
select
HOSPID,
SRYYM,
NYUGAIKBN,
PTID,
RECEKA,
TEISYUTUSAKI,
RECESYUBETU,
HKNJANUM,
HOJOKBN_KEY,
TEKSTYMD,
RECKBN,
RENNUM,
CREYMD,
TOTALTEN,
'  ',
RECEDATA
from TBL_RECEDENWK where reckbn = '1';


-- TBL_RECEDENWK 削除             --

drop table TBL_RECEDENWK;
