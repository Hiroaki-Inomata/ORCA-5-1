--                                    --
-- 低年金履歴                       --
--                                    --
-- 整数項目のデータ型変更             --
--                                    --
-- Create Date : 2011/01/06           --

ALTER TABLE tbl_tnkrrk ALTER COLUMN ptid TYPE bigint;
ALTER TABLE tbl_tnkrrk ALTER COLUMN hospnum TYPE smallint;
