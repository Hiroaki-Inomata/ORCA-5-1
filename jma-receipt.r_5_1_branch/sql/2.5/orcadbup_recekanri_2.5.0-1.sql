--                                    --
-- �쥻�ץȴ����ơ��֥���ѹ�         --
--                                    --
--                                    --
-- ���ܤ��ɲ�                         --
--   �����������ջ��ꡡ(STYMD)        --
--                     (EDYMD)        --
--   ���������������̡�(SYORISBT)     --
--                                    --
-- Create Date : 2005/01/13           --
--                                    --

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

-- TBL_RECEKANRIWK �� TBL_RECEKANRI  �ʰ��ʬ�� --

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


-- TBL_RECEKANRIWK �� TBL_RECEKANRI  �ʸ���ʬ�� --

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



-- TBL_RECEKANRIWK ���             --

drop table TBL_RECEKANRIWK;
