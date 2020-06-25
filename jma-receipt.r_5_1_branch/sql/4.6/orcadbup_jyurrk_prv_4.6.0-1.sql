--                                    --
-- 受診履歴（プレビュー）             --
--                                    --
-- 整数項目のデータ型変更             --
--                                    --
-- Create Date : 2011/02/21           --

ALTER TABLE tbl_jyurrk_prv ALTER COLUMN ptid TYPE bigint;
ALTER TABLE tbl_jyurrk_prv ALTER COLUMN rennum TYPE smallint;
ALTER TABLE tbl_jyurrk_prv ALTER COLUMN douji_rennum TYPE smallint;
ALTER TABLE tbl_jyurrk_prv ALTER COLUMN kaikei_rennum TYPE smallint;
ALTER TABLE tbl_jyurrk_prv ALTER COLUMN edanum TYPE smallint;
ALTER TABLE tbl_jyurrk_prv ALTER COLUMN denpnum TYPE integer;
ALTER TABLE tbl_jyurrk_prv ALTER COLUMN grp_denpnum TYPE integer;
ALTER TABLE tbl_jyurrk_prv ALTER COLUMN grp_rennum TYPE smallint;
ALTER TABLE tbl_jyurrk_prv ALTER COLUMN zainum1 TYPE integer;
ALTER TABLE tbl_jyurrk_prv ALTER COLUMN zainum2 TYPE integer;
ALTER TABLE tbl_jyurrk_prv ALTER COLUMN zainum3 TYPE integer;
ALTER TABLE tbl_jyurrk_prv ALTER COLUMN zainum4 TYPE integer;
ALTER TABLE tbl_jyurrk_prv ALTER COLUMN zainum5 TYPE integer;
ALTER TABLE tbl_jyurrk_prv ALTER COLUMN zainum6 TYPE integer;
ALTER TABLE tbl_jyurrk_prv ALTER COLUMN zainum7 TYPE integer;
ALTER TABLE tbl_jyurrk_prv ALTER COLUMN zainum8 TYPE integer;
ALTER TABLE tbl_jyurrk_prv ALTER COLUMN zainum9 TYPE integer;
ALTER TABLE tbl_jyurrk_prv ALTER COLUMN zainum10 TYPE integer;
ALTER TABLE tbl_jyurrk_prv ALTER COLUMN zainum11 TYPE integer;
ALTER TABLE tbl_jyurrk_prv ALTER COLUMN zainum12 TYPE integer;
ALTER TABLE tbl_jyurrk_prv ALTER COLUMN zainum13 TYPE integer;
ALTER TABLE tbl_jyurrk_prv ALTER COLUMN zainum14 TYPE integer;
ALTER TABLE tbl_jyurrk_prv ALTER COLUMN zainum15 TYPE integer;
ALTER TABLE tbl_jyurrk_prv ALTER COLUMN hospnum TYPE smallint;
