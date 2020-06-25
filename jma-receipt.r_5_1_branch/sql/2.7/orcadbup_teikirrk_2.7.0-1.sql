--                                     --
-- �����������ơ��֥�                --
--                                     --
-- �����ɲ�                            --
--   ����     (KBT)                    --
--                                     --
-- ���ܺ��                            --
--   ������ʬ         (SAKKBN)         --
--   ���ᳫ��ǯ����   (SKYSTYMD)       --
--   ���Ὢλǯ����   (SKYEDYMD)       --
--   ���������ʹ�ס� (SKYTEN)         --
--   �����ۡʹ�ס� (SKYMONEY)       --
--   �������ʬ       (SAISKYKBN)      --
--                                     --
-- Create Date : 2006/01/17            --
--                                     --

drop table tbl_teikirrk;

CREATE TABLE "tbl_teikirrk" (
	"sryym" character(6) NOT NULL,
	"kbt" character(1),
	"termid" character varying(16),
	"opid" character varying(16),
	"creymd" character varying(8),
	"upymd" character varying(8),
	"uphms" character varying(6),
	Constraint "tbl_teikirrk_primary_key" Primary Key ("sryym", "kbt")
);

insert into tbl_teikirrk (
sryym,
kbt,
termid,
opid,
creymd,
upymd,
uphms
)
select 
sryym,
kbt,
'',
'',
'',
'',
''
from tbl_ptteikirrk
group by sryym,kbt;
