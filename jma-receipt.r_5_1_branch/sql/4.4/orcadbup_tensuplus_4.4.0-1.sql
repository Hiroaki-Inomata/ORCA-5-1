--                                    --
-- �����ޥ�����þ���                 --
--                                    --
-- ���ܤ��ɲ�                         --
-- ���󸡺���ɽ����ʬ                 --
--                                    --
-- Create Date : 2009/1/9             --
--                                    --

\set ON_ERROR_STOP

alter table tbl_tensuplus add column zenknskbn numeric(1,0);
alter table tbl_tensuplus alter zenknskbn set default 0;
update tbl_tensuplus set zenknskbn = 0;
