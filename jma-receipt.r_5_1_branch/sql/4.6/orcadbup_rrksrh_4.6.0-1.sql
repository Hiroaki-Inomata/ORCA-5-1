--                                    --
-- 照会用受診履歴                     --
--                                    --
-- 整数項目のデータ型変更             --
--                                    --
-- Create Date : 2011/01/06           --

ALTER TABLE tbl_rrksrh ALTER COLUMN hospnum TYPE smallint;
ALTER TABLE tbl_rrksrh ALTER COLUMN ptid TYPE bigint;
