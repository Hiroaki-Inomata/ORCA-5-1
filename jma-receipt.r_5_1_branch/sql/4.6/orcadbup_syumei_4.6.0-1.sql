--                                    --
-- 収納明細                           --
--                                    --
-- 整数項目のデータ型変更             --
--                                    --
-- Create Date : 2011/01/06           --

ALTER TABLE tbl_syumei ALTER COLUMN ptid TYPE bigint;
ALTER TABLE tbl_syumei ALTER COLUMN denpnum TYPE integer;
ALTER TABLE tbl_syumei ALTER COLUMN denpedanum TYPE smallint;
ALTER TABLE tbl_syumei ALTER COLUMN nyukinrennum TYPE smallint;
ALTER TABLE tbl_syumei ALTER COLUMN skymoney TYPE integer;
ALTER TABLE tbl_syumei ALTER COLUMN nyuhen_money TYPE integer;
ALTER TABLE tbl_syumei ALTER COLUMN hospnum TYPE smallint;
ALTER TABLE tbl_syumei ALTER COLUMN syurrknum TYPE smallint;
ALTER TABLE tbl_syumei ALTER COLUMN syuedanum TYPE smallint;
