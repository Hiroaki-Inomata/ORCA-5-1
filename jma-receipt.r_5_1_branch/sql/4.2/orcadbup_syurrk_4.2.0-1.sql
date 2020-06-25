--                                    --
-- 収納履歴テーブルの変更       --
--                                    --
-- 項目の追加                         --
--   病理診断   BYR                   --
--                                    --
-- Create Date : 2008/03/03           --
--                                    --
\set ON_ERROR_STOP


--    項目追加                        --
alter table TBL_SYURRK
   add column BYR_HKNTEN    numeric(07);
alter table TBL_SYURRK
   add column BYR_MONEY     numeric(07);
alter table TBL_SYURRK
   add column BYR_TGMONEY   numeric(07);
alter table TBL_SYURRK
   add column BYR_TGMONEY_TAX numeric(07);

--    ゼロ設定                       --
alter table TBL_SYURRK
   alter BYR_HKNTEN set default 0;
alter table TBL_SYURRK
   alter BYR_MONEY set default 0;
alter table TBL_SYURRK
   alter BYR_TGMONEY set default 0;
alter table TBL_SYURRK
   alter BYR_TGMONEY_TAX set default 0;

UPDATE TBL_SYURRK SET BYR_HKNTEN=0,BYR_MONEY=0,BYR_TGMONEY=0,BYR_TGMONEY_TAX=0;
