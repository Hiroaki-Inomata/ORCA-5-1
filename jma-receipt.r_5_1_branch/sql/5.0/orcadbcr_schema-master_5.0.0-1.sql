--                            --
-- マスター系テーブルの分離   --
--                            --
-- Create Date : 2016/08/29   --
--                            --

-- マスター系のスキーマ作成

CREATE SCHEMA IF NOT EXISTS master;

--
-- テーブルをマスター系スキーマへ移動する
--

-- 点数
-- ALTER TABLE IF EXISTS tbl_tensu SET SCHEMA master;

-- 病名
ALTER TABLE IF EXISTS tbl_byomei SET SCHEMA master;
CREATE VIEW tbl_byomei AS SELECT * FROM master.tbl_byomei;
-- 検査分類
ALTER TABLE IF EXISTS tbl_kensasort SET SCHEMA master;
CREATE VIEW tbl_kensasort AS SELECT * FROM master.tbl_kensasort;
-- 相互作用
ALTER TABLE IF EXISTS tbl_interact SET SCHEMA master;
CREATE VIEW tbl_interact AS SELECT * FROM master.tbl_interact;
-- 症状措置
ALTER TABLE IF EXISTS tbl_sskijyo SET SCHEMA master;
CREATE VIEW tbl_sskijyo AS SELECT * FROM master.tbl_sskijyo;
-- 適応病名
ALTER TABLE IF EXISTS tbl_tekioubyomei SET SCHEMA master;
CREATE VIEW tbl_tekioubyomei AS SELECT * FROM master.tbl_tekioubyomei;
-- 一般名
ALTER TABLE IF EXISTS tbl_genericname SET SCHEMA master;
CREATE VIEW tbl_genericname AS SELECT * FROM master.tbl_genericname;
-- 包括チェック
ALTER TABLE IF EXISTS tbl_hktsantei SET SCHEMA master;
CREATE VIEW tbl_hktsantei AS SELECT * FROM master.tbl_hktsantei;
-- 包括診療コード
ALTER TABLE IF EXISTS tbl_hktsrycd SET SCHEMA master;
CREATE VIEW tbl_hktsrycd AS SELECT * FROM master.tbl_hktsrycd;
-- 感染症判定
ALTER TABLE IF EXISTS tbl_infection_judgment SET SCHEMA master;
CREATE VIEW tbl_infection_judgment AS SELECT * FROM master.tbl_infection_judgment;
-- 入院基本料
ALTER TABLE IF EXISTS tbl_nyuinkhn SET SCHEMA master;
CREATE VIEW tbl_nyuinkhn AS SELECT * FROM master.tbl_nyuinkhn;
-- 入院料加算チェック
ALTER TABLE IF EXISTS tbl_nyuksnchk SET SCHEMA master;
CREATE VIEW tbl_nyuksnchk AS SELECT * FROM master.tbl_nyuksnchk;
-- 入院レセプト記載略称
ALTER TABLE IF EXISTS tbl_nyuinryakusho SET SCHEMA master;
CREATE VIEW tbl_nyuinryakusho AS SELECT * FROM master.tbl_nyuinryakusho;
-- 同義語
ALTER TABLE IF EXISTS tbl_synonym SET SCHEMA master;
CREATE VIEW tbl_synonym AS SELECT * FROM master.tbl_synonym;

-- 電子点数表
ALTER TABLE IF EXISTS tbl_etensu_1 SET SCHEMA master;
CREATE VIEW tbl_etensu_1 AS SELECT * FROM master.tbl_etensu_1;
ALTER TABLE IF EXISTS tbl_etensu_2 SET SCHEMA master;
CREATE VIEW tbl_etensu_2 AS SELECT * FROM master.tbl_etensu_2;
ALTER TABLE IF EXISTS tbl_etensu_2_jma SET SCHEMA master;
CREATE VIEW tbl_etensu_2_jma AS SELECT * FROM master.tbl_etensu_2_jma;
ALTER TABLE IF EXISTS tbl_etensu_2_sample SET SCHEMA master;
CREATE VIEW tbl_etensu_2_sample AS SELECT * FROM master.tbl_etensu_2_sample;
ALTER TABLE IF EXISTS tbl_etensu_3_1 SET SCHEMA master;
CREATE VIEW tbl_etensu_3_1 AS SELECT * FROM master.tbl_etensu_3_1;
ALTER TABLE IF EXISTS tbl_etensu_3_2 SET SCHEMA master;
CREATE VIEW tbl_etensu_3_2 AS SELECT * FROM master.tbl_etensu_3_2;
ALTER TABLE IF EXISTS tbl_etensu_3_3 SET SCHEMA master;
CREATE VIEW tbl_etensu_3_3 AS SELECT * FROM master.tbl_etensu_3_3;
ALTER TABLE IF EXISTS tbl_etensu_3_4 SET SCHEMA master;
CREATE VIEW tbl_etensu_3_4 AS SELECT * FROM master.tbl_etensu_3_4;
ALTER TABLE IF EXISTS tbl_etensu_4 SET SCHEMA master;
CREATE VIEW tbl_etensu_4 AS SELECT * FROM master.tbl_etensu_4;
ALTER TABLE IF EXISTS tbl_etensu_5 SET SCHEMA master;
CREATE VIEW tbl_etensu_5 AS SELECT * FROM master.tbl_etensu_5;

-- 労働基準監督署
ALTER TABLE IF EXISTS tbl_labor_sio SET SCHEMA master;
CREATE VIEW tbl_labor_sio AS SELECT * FROM master.tbl_labor_sio;
-- 最低薬価
ALTER TABLE IF EXISTS tbl_generic_price SET SCHEMA master;
CREATE VIEW tbl_generic_price AS SELECT * FROM master.tbl_generic_price;
-- 介護保険者
ALTER TABLE IF EXISTS tbl_hknjainf_care SET SCHEMA master;
CREATE VIEW tbl_hknjainf_care AS SELECT * FROM master.tbl_hknjainf_care;
-- 選定療養費一般コード振替
ALTER TABLE IF EXISTS tbl_senteicdchg SET SCHEMA master;
CREATE VIEW tbl_senteicdchg AS SELECT * FROM master.tbl_senteicdchg;
-- 入院料置換
ALTER TABLE IF EXISTS tbl_nyuincdchg SET SCHEMA master;
CREATE VIEW tbl_nyuincdchg AS SELECT * FROM master.tbl_nyuincdchg;
-- 医薬品傷病名
ALTER TABLE IF EXISTS tbl_tekiou_medicine SET SCHEMA master;
CREATE VIEW tbl_tekiou_medicine AS SELECT * FROM master.tbl_tekiou_medicine;
ALTER TABLE IF EXISTS tbl_tekiou_disease SET SCHEMA master;
CREATE VIEW tbl_tekiou_disease AS SELECT * FROM master.tbl_tekiou_disease;
-- 医薬品分類
ALTER TABLE IF EXISTS tbl_generic_class SET SCHEMA master;
CREATE VIEW tbl_generic_class AS SELECT * FROM master.tbl_generic_class;
-- 向精神薬成分コード
ALTER TABLE IF EXISTS tbl_psychotropic SET SCHEMA master;
CREATE VIEW tbl_psychotropic AS SELECT * FROM master.tbl_psychotropic;
