--                                    --
-- ��Ǽ�ơ��֥���ѹ�                 --
--                                    --
-- ���ܤ��ɲ�                         --
--   �ޤȤ�ȯ����ˡ�ե饰             --
--                                    --
-- Create Date : 2006/08/18           --
--                                    --
\set ON_ERROR_STOP

--    �����ɲ�                        --
alter table TBL_SYUNOU
   add column GRP_HAKHOUFLG  numeric(01);

--    ��������                       --
alter table TBL_SYUNOU
   alter GRP_HAKHOUFLG set default 0;
