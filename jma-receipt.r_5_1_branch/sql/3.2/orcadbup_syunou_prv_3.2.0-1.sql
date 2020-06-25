-- 項目の追加                         --
--   まとめ発行方法フラグ             --
--                                    --
-- Create Date : 2006/08/18           --
--                                    --
\set ON_ERROR_STOP

--    項目追加                        --
alter table TBL_SYUNOU_PRV
   add column GRP_HAKHOUFLG  numeric(01);

--    ゼロ設定                       --
alter table TBL_SYUNOU_PRV
   alter GRP_HAKHOUFLG set default 0;
