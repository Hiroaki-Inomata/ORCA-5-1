--                                    --
-- 収納プレビューテーブルの変更                 --
--                                    --
-- 項目の追加                         --
--   食事８〜食事１０                 --
--   食事療養費（保険）               --
--   食事負担額（保険：自己負担）     --
--   食事負担額（保険：自己負担消費税）--
--   食事負担額（保険：自己負担合計） --
--   食事療養費（自費）               --
--   食事負担額（自費：自己負担）     --
--   食事負担額（自費：自己負担消費税）--
--   食事負担額（自費：自己負担合計） --
--                                    --
-- Create Date : 2006/08/18           --
--                                    --
\set ON_ERROR_STOP

--    項目追加                        --
alter table TBL_SYUNOU_PRV
   add column SHOKUJI8_NISSU			numeric(02);
alter table TBL_SYUNOU_PRV
   add column SHOKUJI8					numeric(07);
alter table TBL_SYUNOU_PRV
   add column SHOKUJI9_NISSU			numeric(02);
alter table TBL_SYUNOU_PRV
   add column SHOKUJI9					numeric(07);
alter table TBL_SYUNOU_PRV
   add column SHOKUJI10_NISSU			numeric(02);
alter table TBL_SYUNOU_PRV
   add column SHOKUJI10					numeric(07);
alter table TBL_SYUNOU_PRV
   add column RYOYOHI_LIFE				numeric(07);
alter table TBL_SYUNOU_PRV
   add column SKYMONEY_LIFE				numeric(07);
alter table TBL_SYUNOU_PRV
   add column SKYMONEY_LIFE_TAX			numeric(07);
alter table TBL_SYUNOU_PRV
   add column SKYMONEY_LIFE_KEI			numeric(07);
alter table TBL_SYUNOU_PRV
   add column RYOYOHI_LIFE_JIHI			numeric(07);
alter table TBL_SYUNOU_PRV
   add column SKYMONEY_LIFE_JIHI		numeric(07);
alter table TBL_SYUNOU_PRV
   add column SKYMONEY_LIFE_JIHI_TAX	numeric(07);
alter table TBL_SYUNOU_PRV
   add column SKYMONEY_LIFE_JIHI_KEI	numeric(07);


--    ゼロ設定                       --
alter table TBL_SYUNOU_PRV
   alter SHOKUJI8_NISSU set default 0;
alter table TBL_SYUNOU_PRV
   alter SHOKUJI8 set default 0;
--
alter table TBL_SYUNOU_PRV
   alter SHOKUJI9_NISSU set default 0;
alter table TBL_SYUNOU_PRV
   alter SHOKUJI9 set default 0;
--
alter table TBL_SYUNOU_PRV
   alter SHOKUJI10_NISSU set default 0;
alter table TBL_SYUNOU_PRV
   alter SHOKUJI10 set default 0;
--
alter table TBL_SYUNOU_PRV
   alter RYOYOHI_LIFE set default 0;
alter table TBL_SYUNOU_PRV
   alter SKYMONEY_LIFE set default 0;
alter table TBL_SYUNOU_PRV
   alter SKYMONEY_LIFE_TAX set default 0;
alter table TBL_SYUNOU_PRV
   alter SKYMONEY_LIFE_KEI set default 0;
alter table TBL_SYUNOU_PRV
   alter RYOYOHI_LIFE_JIHI set default 0;
alter table TBL_SYUNOU_PRV
   alter SKYMONEY_LIFE_JIHI set default 0;
alter table TBL_SYUNOU_PRV
   alter SKYMONEY_LIFE_JIHI_TAX set default 0;
alter table TBL_SYUNOU_PRV
   alter SKYMONEY_LIFE_JIHI_KEI set default 0;
