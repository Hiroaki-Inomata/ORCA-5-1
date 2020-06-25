--                                    --
-- レセプト明細テーブルの変更         --
--                                    --
-- 項目の追加                         --
--   主科用出力順識別番号             --
--                                    --
-- Create Date : 2006/08/31           --
--                                    --
\set ON_ERROR_STOP

--    項目追加                        --
alter table TBL_RECEPRT
   add column PRTJYUN_SYUKA  numeric(04);

--    ゼロ設定                       --
alter table TBL_RECEPRT
   alter PRTJYUN_SYUKA set default 0;

update TBL_RECEPRT set PRTJYUN_SYUKA = 0;

