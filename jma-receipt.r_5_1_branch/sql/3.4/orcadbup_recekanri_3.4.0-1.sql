--                                    --
-- �쥻�ץȴ����ơ��֥���ѹ�         --
--                                    --
-- ���ܤ��ɲ�                         --
--   ��������ե饰(KOUGAKU)          --
--   ���������(PAGE_KOUGAKU)         --
--   Ĺ����������(CHOUKI_KBN)         --
--   ��������������� (ZAITAKU_KBN)   --
--   �ǽ����Ͻ� (SRT_LAST)            --
--   �������(KOUGAKU_TEN)            --
--   ɽ���ѥ쥻�ץȼ���(SYUBETU_SRT1) --
--   ���⸩����ʬ(PREFKBN)            --

--                                    --
-- Create Date : 2007/03/01           --
--                                    --
\set ON_ERROR_STOP

--    �����ɲ�                        --
alter table tbl_recekanri
   add column KOUGAKU      char(1);
alter table tbl_recekanri
   add column PAGE_KOUGAKU numeric(5);
alter table tbl_recekanri
   add column SYUBETU_SRT1 char(4);
alter table tbl_recekanri
   add column PREFKBN      char(1);
alter table tbl_recekanri
   add column ZAITAKU_KBN  char(1);
alter table tbl_recekanri
   add column CHOUKI_KBN   char(1);
alter table tbl_recekanri
   add column SRT_LAST     char(1);
alter table tbl_recekanri
   add column KOUGAKU_TEN  numeric(7);

--    ��������                       --
alter table tbl_recekanri
   alter PAGE_KOUGAKU set default 0;
alter table tbl_recekanri
   alter KOUGAKU_TEN set default 0;
alter table tbl_recekanri
   alter  KOUGAKU  set default '9';

update tbl_recekanri set PAGE_KOUGAKU = 0,KOUGAKU_TEN = 0,KOUGAKU = '9';
