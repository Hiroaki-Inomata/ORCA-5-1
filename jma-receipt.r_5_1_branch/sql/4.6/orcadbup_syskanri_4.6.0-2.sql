--                                    --
-- �����ƥ�����ǡ����ɲ�             --
-- ���Ŵѻ�ˡ���б�                   --
--   ���������������               --
--   ���ౡ��Ͽ������������           --
--                                    --
-- Create Date : 2011/01/07           --
--                                    --

\set ON_ERROR_STOP

update tbl_syskanri set kanritbl = substr(kanritbl,1,408) || ' 1' || substr(kanritbl,411,90) where kanricd = '5000' and substr(kanritbl,409,1) = '1' and styukymd >= '20120401';
