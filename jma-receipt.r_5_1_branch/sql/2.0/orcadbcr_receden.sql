--                                     --
-- レセプト電算データテーブルの作成    --
--                                     --
-- Create Date : 2003/08/07            --
--                                     --

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
RECEDATA		varchar(250),
constraint TBL_RECEDEN_primary_key primary key (HOSPID,SRYYM,NYUGAIKBN,
PTID,RECEKA,TEISYUTUSAKI,RECESYUBETU,HKNJANUM,HOJOKBN_KEY,TEKSTYMD,
RECKBN,RENNUM));
