--                                      --
-- �쥻�ץȴ����ơ��֥���ѹ�           --
--                                      --
--                                      --
-- �����ɲ� (�����б�)                  --
--   ����������ʤȤʤ���Ų�(SRYKA)    --
--   �����������⳰������(PREFKBN_SRT)  --
--   ���������ݸ��԰�����(HKNJANUM_SRT) --
--   ���������쥻�ץȼ��̰�����         --
--                    (RECESYUBETU_SRT) --
--   ���������ݸ����ֹ�(HKNJANUM)       --
--   ������������Ǽ���(ZAITAKU)        --
--   ��������Ĺ������  (CHOUKI)         --
--   ����������������(PAGE_ZAITAKU)   --
--   ��������Ĺ�����  (PAGE_CHOUKI)    --
--   ����������ʣ���  (PAGE_DUP)       --
--   �����������ֽ����(HSEQNUM)        --
--                                      --
-- Create Date : 2005/12/19             --
--                                      --

-- TBL_RECEKANRIWK ����               --

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
SYORISBT            char(1),
STYMD               char(8),
EDYMD               char(8),
TERMID              varchar(16),
constraint TBL_RECEKANRIWK_primary_key primary key (PRTID,SYOKBN,CREYMD,CREHMS,TEISYUTUSAKI,RECESYUBETU));


-- TBL_RECEKANRI �� TBL_RECEKANRIWK   --

insert into TBL_RECEKANRIWK
select * from TBL_RECEKANRI;

-- TBL_RECEKANRI ���               --

drop table TBL_RECEKANRI;

-- TBL_RECEKANRI ����               --

create table TBL_RECEKANRI (
PRTID               char(4)		not null,
SYOKBN              char(1)		not null,
CREYMD              char(8)		not null,
CREHMS              char(6)		not null,
TEISYUTUSAKI        char(1)		not null,
SRYKA               char(2)		not null default '00',
PREFKBN_SRT         char(2)		not null default '99',
HKNJANUM_SRT        char(1)		not null default '9',
HKNJANUM            char(8)		not null default '99999999',
RECESYUBETU         char(4)		not null,
RECESYUBETU_SRT     char(3)		not null default '999',
ZAITAKU             char(1)		not null default '9',
CHOUKI              char(1)		not null default '9',
NYUGAIKBN           char(1),
SRYYM               char(6),
PAGE                numeric(5,0)	default 0,
PAGE_ZAITAKU        numeric(5,0)	default 0,
PAGE_CHOUKI         numeric(5,0)	default 0,
PAGE_DUP            numeric(5,0)	default 0,
KENSU               numeric(5,0)	default 0,
RECEKBN             char(1),
SYORISBT            char(1),
STYMD               char(8),
EDYMD               char(8),
HSEQNUM             numeric(8,0)	default 0,
TERMID              varchar(64),
OPID                varchar(16),
constraint TBL_RECEKANRI_primary_key primary key (PRTID,SYOKBN,CREYMD,CREHMS,TEISYUTUSAKI,SRYKA,PREFKBN_SRT,HKNJANUM_SRT,HKNJANUM,RECESYUBETU,RECESYUBETU_SRT,ZAITAKU,CHOUKI));

-- TBL_RECEKANRIWK �� TBL_RECEKANRI --

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
STYMD,
EDYMD,
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
SYORISBT,
STYMD,
EDYMD,
TERMID from TBL_RECEKANRIWK;

-- INDEX idx_recekanri_sryym MAKE --
CREATE INDEX idx_recekanri_sryym ON tbl_recekanri USING btree (sryym);

-- TBL_RECEKANRIWK ���             --

drop table TBL_RECEKANRIWK;
