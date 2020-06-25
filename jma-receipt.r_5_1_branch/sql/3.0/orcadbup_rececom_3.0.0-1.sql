--                                    --
-- �쥻�ץȥ����ȥơ��֥���ѹ�     --
--                                    --
-- ���ܤ��ɲ�                         --
--   �����ɾ��ܵ���ʬ SJKBN           --
--                                    --
-- Create Date : 2006/06/14           --
--                                    --

\set ON_ERROR_STOP

-- TBL_RECECOMWK ����                 --

create table TBL_RECECOMWK (
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
constraint TBL_RECECOMWK_primary_key primary key (HOSPID,PTID,NYUGAIKBN,SRYKA,
SRYYM,HKNCOMBI,SRYDD));

-- TBL_RECECOM �� TBL_RECECOMWK   --

insert into TBL_RECECOMWK
select * from TBL_RECECOM;

-- TBL_RECECOM ���               --

drop table TBL_RECECOM;

-- TBL_RECECOM ����               --

create table TBL_RECECOM (
HOSPID				char(24)	not null,
PTID				numeric(10,0)	not null,
NYUGAIKBN			char(1)         not null  default '0',
SRYKA				char(2)         not null,
SRYYM   			char(6)         not null,
HKNCOMBI  			numeric(4,0)    not null,
SRYDD   			char(2)         not null,
SJKBN   			char(2)         not null,
COMMENT             varchar(1600),
TERMID  			varchar(16),
OPID    			varchar(16),
CREYMD  			char(8),
UPYMD    			char(8),
UPHMS				char(6),
constraint TBL_RECECOM_primary_key primary key (HOSPID,PTID,NYUGAIKBN,SRYKA,
SRYYM,HKNCOMBI,SRYDD,SJKBN));

-- TBL_RECECOMWK �� TBL_RECECOM   --

insert into TBL_RECECOM
(HOSPID,
PTID,
NYUGAIKBN,
SRYKA,
SRYYM,
HKNCOMBI,
SRYDD,
SJKBN,
COMMENT,
TERMID,
OPID,
CREYMD,
UPYMD,
UPHMS
)
select
HOSPID,
PTID,
NYUGAIKBN,
SRYKA,
SRYYM,
HKNCOMBI,
SRYDD,
'07',
COMMENT,
TERMID,
OPID,
CREYMD,
UPYMD,
UPHMS

from TBL_RECECOMWK;

-- TBL_RECECOMWK ���             --

drop table TBL_RECECOMWK;
