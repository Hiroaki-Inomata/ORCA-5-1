--                            --
-- ＤＰＣマスター系テーブルのｖｉｅｗ作成   --
--                            --
-- Create Date : 2016/12/07   --
--                            --
-- 手術処置等１
DROP VIEW tbl_dpc_act1;
CREATE VIEW tbl_dpc_act1 AS SELECT * FROM master.tbl_dpc_act1;
-- 手術処置等２
DROP VIEW tbl_dpc_act2;
CREATE VIEW tbl_dpc_act2 AS SELECT * FROM master.tbl_dpc_act2;
-- 年齢出生時体重等
DROP VIEW tbl_dpc_age;
CREATE VIEW tbl_dpc_age AS SELECT * FROM master.tbl_dpc_age;
-- 分類名称
DROP VIEW tbl_dpc_bunrui;
CREATE VIEW tbl_dpc_bunrui AS SELECT * FROM master.tbl_dpc_bunrui;
-- 病態等分類
DROP VIEW tbl_dpc_byotai;
CREATE VIEW tbl_dpc_byotai AS SELECT * FROM master.tbl_dpc_byotai;
-- 診療コード変換
DROP VIEW tbl_dpc_cdchg;
CREATE VIEW tbl_dpc_cdchg AS SELECT * FROM master.tbl_dpc_cdchg;
-- ＩＣＤ１０
DROP VIEW tbl_dpc_icd10;
CREATE VIEW tbl_dpc_icd10 AS SELECT * FROM master.tbl_dpc_icd10;
-- 重症度等
DROP VIEW tbl_dpc_jusho;
CREATE VIEW tbl_dpc_jusho AS SELECT * FROM master.tbl_dpc_jusho;
-- ＭＤＣ名称
DROP VIEW tbl_dpc_mdc;
CREATE VIEW tbl_dpc_mdc AS SELECT * FROM master.tbl_dpc_mdc;
-- 手術
DROP VIEW tbl_dpc_ope;
CREATE VIEW tbl_dpc_ope AS SELECT * FROM master.tbl_dpc_ope;
-- 定義副傷病名
DROP VIEW tbl_dpc_subbyo;
CREATE VIEW tbl_dpc_subbyo AS SELECT * FROM master.tbl_dpc_subbyo;
-- 診断群分類点数
DROP VIEW tbl_dpc_tensu;
CREATE VIEW tbl_dpc_tensu AS SELECT * FROM master.tbl_dpc_tensu;
