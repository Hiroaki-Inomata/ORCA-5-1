--                                    --
-- �ץ���४�ץ����               --
--                                    --
-- ���ܤ��ѹ�                         --
-- ���ץ��������                     --
--   (5000) -> (50000)                --
--                                    --
-- Create Date : 2009/3/25            --
--                                    --

\set ON_ERROR_STOP

alter table tbl_prgoption alter column option type varchar(44000);
