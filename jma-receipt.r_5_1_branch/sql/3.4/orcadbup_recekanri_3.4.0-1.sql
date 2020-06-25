--                                    --
-- レセプト管理テーブルの変更         --
--                                    --
-- 項目の追加                         --
--   高額点数フラグ(KOUGAKU)          --
--   高額点数頁(PAGE_KOUGAKU)         --
--   長期印刷設定(CHOUKI_KBN)         --
--   在総診等印刷設定 (ZAITAKU_KBN)   --
--   最終出力順 (SRT_LAST)            --
--   高額点数(KOUGAKU_TEN)            --
--   表示用レセプト種別(SYUBETU_SRT1) --
--   県内県外区分(PREFKBN)            --

--                                    --
-- Create Date : 2007/03/01           --
--                                    --
\set ON_ERROR_STOP

--    項目追加                        --
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

--    ゼロ設定                       --
alter table tbl_recekanri
   alter PAGE_KOUGAKU set default 0;
alter table tbl_recekanri
   alter KOUGAKU_TEN set default 0;
alter table tbl_recekanri
   alter  KOUGAKU  set default '9';

update tbl_recekanri set PAGE_KOUGAKU = 0,KOUGAKU_TEN = 0,KOUGAKU = '9';
