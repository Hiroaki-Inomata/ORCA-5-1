--                                    --
-- 患者公費情報                       --
--                                    --
-- 整数項目のデータ型変更             --
--                                    --
-- Create Date : 2011/01/06           --

ALTER TABLE tbl_ptkohinf ALTER COLUMN ptid TYPE bigint;
ALTER TABLE tbl_ptkohinf ALTER COLUMN kohid TYPE bigint;
ALTER TABLE tbl_ptkohinf ALTER COLUMN hospnum TYPE smallint;
