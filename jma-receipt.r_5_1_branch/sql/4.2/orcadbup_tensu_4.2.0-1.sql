--                                    --
-- �����ơ��֥���ѹ�       --
--                                    --
-- ���ܤ��ɲ�                         --
--   ������ѻٱ�û�                 --
--   ���Ŵѻ�ˡ�оݶ�ʬ               --
--   ��켱�̶�ʬ                     --
--                                    --
-- Create Date : 2008/03/07           --
--                                    --
\set ON_ERROR_STOP


--    �����ɲ�                        --
alter table tbl_tensu
   add column gazoopesup numeric(01);
alter table tbl_tensu
   add column iryokansatukbn numeric(01);
alter table tbl_tensu
   add column masuiskbkbn numeric(01);

--    ��������                       --
alter table tbl_tensu
   alter gazoopesup set default 0;
alter table tbl_tensu
   alter iryokansatukbn set default 0;
alter table tbl_tensu
   alter masuiskbkbn set default 0;

update tbl_tensu set gazoopesup=0,iryokansatukbn=0,masuiskbkbn=0;
