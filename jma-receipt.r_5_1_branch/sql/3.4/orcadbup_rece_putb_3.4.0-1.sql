--                                    --
-- レセプト編綴順詳細設定の変更       --
--                                    --
-- 項目の追加                         --
--   グループ１の先頭の種別           --
--                                    --
-- Create Date : 2007/03/01           --
--                                    --
\set ON_ERROR_STOP

--    項目追加                        --
alter table tbl_rece_putb
   add column SYUBETU_GAI_SRT1    char(4);
alter table tbl_rece_putb
   add column SYUBETU_NYU_SRT1    char(4);

