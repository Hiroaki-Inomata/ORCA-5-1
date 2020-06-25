--                                    --
-- 患者保険情報                       --
--                                    --
-- 整数項目のデータ型変更             --
--                                    --
-- Create Date : 2011/01/06           --

ALTER TABLE tbl_pthkninf ALTER COLUMN ptid TYPE bigint;
ALTER TABLE tbl_pthkninf ALTER COLUMN hknid TYPE bigint;
ALTER TABLE tbl_pthkninf ALTER COLUMN hospnum TYPE smallint;
