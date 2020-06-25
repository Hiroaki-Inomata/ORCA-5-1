--                                    --
-- 入院履歴照会                       --
--                                    --
-- 整数項目のデータ型変更             --
--                                    --
-- Create Date : 2011/01/06           --

ALTER TABLE tbl_nrrksrh ALTER COLUMN hospnum TYPE smallint;
ALTER TABLE tbl_nrrksrh ALTER COLUMN ptid TYPE bigint;
ALTER TABLE tbl_nrrksrh ALTER COLUMN zainum TYPE integer;
ALTER TABLE tbl_nrrksrh ALTER COLUMN rrknum TYPE smallint;
