--                                    --
-- 低所得者履歴                       --
--                                    --
-- 整数項目のデータ型変更             --
--                                    --
-- Create Date : 2011/01/06           --

ALTER TABLE tbl_tsyrrk ALTER COLUMN ptid TYPE bigint;
ALTER TABLE tbl_tsyrrk ALTER COLUMN hospnum TYPE smallint;
