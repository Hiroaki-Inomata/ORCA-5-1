--                                    --
-- ���ץ�⹽¤�ѹ�                   --
-- �ѥ�᥿��                         --
-- �ѥ�᥿�����ɲ�                   --
--                                    --
-- Create Date : 2008/08/06           --
--                                    --

\set ON_ERROR_STOP

alter table tbl_toukeimemo
   add column para9    varchar (20);
alter table tbl_toukeimemo
   add column para10   varchar (20);

update tbl_toukeimemo set para9  = '',para10='';
