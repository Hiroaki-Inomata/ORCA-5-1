--                                    --
-- ����ϫ���ݸ�����ơ��֥���ѹ�     --
--                                    --
-- ���ܤ��ɲ�                         --
--  ������ô���女����   DRCD         --
--  �������ݸ����̾     HOKENNAME    --
--                                    --
-- Create Date : 2005/01/07           --
--                                    --

-- TBL_PTRSIINFWK ����                --

create table TBL_PTRSIINFWK (
HOSPID				char(24)	not null,
PTID				numeric(10)	not null,
HKNID				numeric(10)     not null,
HKNKBN  			char(1),
HKNNUM  			char(3),
KOFUNUM  			char(14),
HIHKNJANAME			varchar(100),
JIGYOUNAME			varchar(80),
PREFNAME			varchar(20),
PREFKBN			        char(1),
CITYNAME			varchar(80),
CITYKBN			        char(1),
SINKEI  			char(1),
TENKI           		char(1),
SHOBYOYMD 			char(8),
SISIKBN			        char(1),
RYOSTYMD			char(8),
RYOEDYMD        		char(8),
SONSHOUKBN 			char(2),
SONSHOUKBN_EDANUM	        char(1),
SINSATUYMD			char(8),
SINSATUEND                      char(8),
KAKUNINYMD 			char(8),
SAKUJOKBN 			char(1),
COMMENT                         varchar(200),
SAIGAIKBN                       char(1),
JIBAISEIKBN                     char(1),
TERMID  			varchar(16),
OPID    			varchar(16),
CREYMD  			char(8),
UPYMD    			char(8),
UPHMS				char(6),
constraint TBL_PTRSIINFWK_primary_key primary key (HOSPID,PTID,HKNID));

-- TBL_PTRSIINF �� TBL_PTRSIINFWK   --

insert into TBL_PTRSIINFWK
select * from TBL_PTRSIINF;

-- TBL_PTRSIINF ���               --

drop table TBL_PTRSIINF;

-- TBL_PTRSIINF ����               --

create table TBL_PTRSIINF (
HOSPID				char(24)	not null,
PTID				numeric(10)	not null,
HKNID				numeric(10)     not null,
HKNKBN  			char(1),
HKNNUM  			char(3),
KOFUNUM  			char(14),
HIHKNJANAME			varchar(100),
JIGYOUNAME			varchar(80),
PREFNAME			varchar(20),
PREFKBN			        char(1),
CITYNAME			varchar(80),
CITYKBN			        char(1),
SINKEI  			char(1),
TENKI           		char(1),
SHOBYOYMD 			char(8),
SISIKBN			        char(1),
RYOSTYMD			char(8),
RYOEDYMD        		char(8),
SONSHOUKBN 			char(2),
SONSHOUKBN_EDANUM	        char(1),
SINSATUYMD			char(8),
SINSATUEND                      char(8),
KAKUNINYMD 			char(8),
SAKUJOKBN 			char(1),
COMMENT                         varchar(200),
SAIGAIKBN 			char(1),
JIBAISEIKBN                     char(1),
DRCD                            char(5)         default ' ',
HOKENNAME                       varchar(80)     default ' ',
TERMID  			varchar(16),
OPID    			varchar(16),
CREYMD  			char(8),
UPYMD    			char(8),
UPHMS				char(6),
constraint TBL_PTRSIINF_primary_key primary key (HOSPID,PTID,HKNID));

-- TBL_PTRSIINFWK �� TBL_PTRSIINF   --

insert into TBL_PTRSIINF
(HOSPID,
PTID,
HKNID,
HKNKBN,
HKNNUM,
KOFUNUM,
HIHKNJANAME,
JIGYOUNAME,
PREFNAME,
PREFKBN,
CITYNAME,
CITYKBN,
SINKEI,
TENKI,
SHOBYOYMD,
SISIKBN,
RYOSTYMD,
RYOEDYMD,
SONSHOUKBN,
SONSHOUKBN_EDANUM,
SINSATUYMD,
SINSATUEND,
KAKUNINYMD,
SAKUJOKBN,
COMMENT,
SAIGAIKBN,
JIBAISEIKBN,
-- DRCD --
-- HOKENNAME --
TERMID,
OPID,
CREYMD,
UPYMD,
UPHMS
)
select HOSPID,PTID,HKNID,HKNKBN,HKNNUM,KOFUNUM,HIHKNJANAME,JIGYOUNAME,PREFNAME,PREFKBN,
CITYNAME,CITYKBN,SINKEI,TENKI,SHOBYOYMD,SISIKBN,RYOSTYMD,RYOEDYMD,SONSHOUKBN,SONSHOUKBN_EDANUM,
SINSATUYMD,SINSATUEND,KAKUNINYMD,SAKUJOKBN,COMMENT,SAIGAIKBN,JIBAISEIKBN,TERMID,OPID,CREYMD,
UPYMD,UPHMS from TBL_PTRSIINFWK;

-- TBL_PTRSIINFWK ���             --

drop table TBL_PTRSIINFWK;

