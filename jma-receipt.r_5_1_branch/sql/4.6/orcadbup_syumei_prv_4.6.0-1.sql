--                                    --
-- 収納明細（プレビュー）             --
--                                    --
-- 整数項目のデータ型変更             --
--                                    --
-- Create Date : 2011/02/21           --

ALTER TABLE tbl_syumei_prv ALTER COLUMN ptid TYPE bigint;
ALTER TABLE tbl_syumei_prv ALTER COLUMN denpnum TYPE integer;
ALTER TABLE tbl_syumei_prv ALTER COLUMN denpedanum TYPE smallint;
ALTER TABLE tbl_syumei_prv ALTER COLUMN nyukinrennum TYPE smallint;
ALTER TABLE tbl_syumei_prv ALTER COLUMN skymoney TYPE integer;
ALTER TABLE tbl_syumei_prv ALTER COLUMN nyuhen_money TYPE integer;
ALTER TABLE tbl_syumei_prv ALTER COLUMN hospnum TYPE smallint;
ALTER TABLE tbl_syumei_prv ALTER COLUMN syurrknum TYPE smallint;
ALTER TABLE tbl_syumei_prv ALTER COLUMN syuedanum TYPE smallint;
