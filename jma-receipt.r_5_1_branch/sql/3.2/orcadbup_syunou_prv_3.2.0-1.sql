-- ���ܤ��ɲ�                         --
--   �ޤȤ�ȯ����ˡ�ե饰             --
--                                    --
-- Create Date : 2006/08/18           --
--                                    --
\set ON_ERROR_STOP

--    �����ɲ�                        --
alter table TBL_SYUNOU_PRV
   add column GRP_HAKHOUFLG  numeric(01);

--    ��������                       --
alter table TBL_SYUNOU_PRV
   alter GRP_HAKHOUFLG set default 0;
