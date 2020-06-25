\set ON_ERROR_STOP
--
--
-- 収納履歴番号(SYURRKNUM)追加
-- 収納履歴枝番号(SYURRKEDANUM)追加
-- 収納履歴更新区分(SYURRKUPDKBN)追加
-- 
-- Create Date : 2006/12/19           --
--
--  項目追加                        --
alter table TBL_SYUMEI_PRV
   add column SYURRKNUM		integer;
alter table TBL_SYUMEI_PRV
   add column SYUEDANUM		integer;
alter table TBL_SYUMEI_PRV
   add column SYURRKUPDKBN	char(1);


--  ゼロ設定                       --
alter table TBL_SYUMEI_PRV
   alter SYURRKNUM	set default 0;
alter table TBL_SYUMEI_PRV
   alter SYUEDANUM	set default 0;

