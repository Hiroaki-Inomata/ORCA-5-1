--                                    --
-- �������                           --
--                                    --
-- ���ܤ��ɲ�                         --
-- ����ؼ��ե饰                    --
--                                    --
-- Create Date : 2008/12/17           --
--                                    --

\set ON_ERROR_STOP


alter table tbl_seikyu
   add column henrei_siji_flg  numeric(01);

alter table tbl_seikyu
   alter henrei_siji_flg set default 0;

update tbl_seikyu set henrei_siji_flg = 0;
