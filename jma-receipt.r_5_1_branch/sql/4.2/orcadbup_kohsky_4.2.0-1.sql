--                                    --
-- ���������ơ��֥���ѹ�           --
--                                    --
-- ���ܤ��ɲ�                         --
--   �ե饰                           --
--                                    --
-- Create Date : 2008/03/12           --
--                                    --
\set ON_ERROR_STOP


--    �����ɲ�                        --
alter table tbl_kohsky
   add column flg1 numeric(1);
alter table tbl_kohsky
   add column flg2 numeric(1);
alter table tbl_kohsky
   add column flg3 numeric(1);
alter table tbl_kohsky
   add column flg4 numeric(1);
alter table tbl_kohsky
   add column flg5 numeric(1);

--    ��������                       --
alter table tbl_kohsky
   alter flg1 set default 0;
alter table tbl_kohsky
   alter flg2 set default 0;
alter table tbl_kohsky
   alter flg3 set default 0;
alter table tbl_kohsky
   alter flg4 set default 0;
alter table tbl_kohsky
   alter flg5 set default 0;

update tbl_kohsky set flg1=0,flg2=0,flg3=0,flg4=0,flg5=0;
