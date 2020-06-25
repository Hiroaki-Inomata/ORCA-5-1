--                                    --
-- 保険組合せ                         --
--                                    --
-- 整数項目のデータ型変更             --
--                                    --
-- Create Date : 2011/01/06           --

ALTER TABLE tbl_hkncombi ALTER COLUMN ptid TYPE bigint;
ALTER TABLE tbl_hkncombi ALTER COLUMN hkncombinum TYPE smallint;
ALTER TABLE tbl_hkncombi ALTER COLUMN hknid TYPE bigint;
ALTER TABLE tbl_hkncombi ALTER COLUMN koh1id TYPE bigint;
ALTER TABLE tbl_hkncombi ALTER COLUMN koh2id TYPE bigint;
ALTER TABLE tbl_hkncombi ALTER COLUMN koh3id TYPE bigint;
ALTER TABLE tbl_hkncombi ALTER COLUMN koh4id TYPE bigint;
ALTER TABLE tbl_hkncombi ALTER COLUMN gai_kftnrate TYPE smallint;
ALTER TABLE tbl_hkncombi ALTER COLUMN gai_kkotgak TYPE integer;
ALTER TABLE tbl_hkncombi ALTER COLUMN gai_kjgngak TYPE integer;
ALTER TABLE tbl_hkncombi ALTER COLUMN gai_djgngak TYPE integer;
ALTER TABLE tbl_hkncombi ALTER COLUMN gai_djgncnt TYPE smallint;
ALTER TABLE tbl_hkncombi ALTER COLUMN gai_mnaijgngak TYPE integer;
ALTER TABLE tbl_hkncombi ALTER COLUMN gai_mgaijgngak TYPE integer;
ALTER TABLE tbl_hkncombi ALTER COLUMN gai_mjgncnt TYPE smallint;
ALTER TABLE tbl_hkncombi ALTER COLUMN nyu_kftnrate TYPE smallint;
ALTER TABLE tbl_hkncombi ALTER COLUMN nyu_kkotgak TYPE integer;
ALTER TABLE tbl_hkncombi ALTER COLUMN nyu_kjgngak TYPE integer;
ALTER TABLE tbl_hkncombi ALTER COLUMN nyu_djgngak TYPE integer;
ALTER TABLE tbl_hkncombi ALTER COLUMN nyu_djgncnt TYPE smallint;
ALTER TABLE tbl_hkncombi ALTER COLUMN nyu_mjgngak TYPE integer;
ALTER TABLE tbl_hkncombi ALTER COLUMN nyu_mjgncnt TYPE smallint;
ALTER TABLE tbl_hkncombi ALTER COLUMN tsy_gaikftnrate TYPE smallint;
ALTER TABLE tbl_hkncombi ALTER COLUMN tsy_gaikkotgak TYPE integer;
ALTER TABLE tbl_hkncombi ALTER COLUMN tsy_gaikjgngak TYPE integer;
ALTER TABLE tbl_hkncombi ALTER COLUMN tsy_gaidjgngak TYPE integer;
ALTER TABLE tbl_hkncombi ALTER COLUMN tsy_gaidjgncnt TYPE smallint;
ALTER TABLE tbl_hkncombi ALTER COLUMN tsy_gaimnaijgngak TYPE integer;
ALTER TABLE tbl_hkncombi ALTER COLUMN tsy_gaimgaijgngak TYPE integer;
ALTER TABLE tbl_hkncombi ALTER COLUMN tsy_gaimjgncnt TYPE smallint;
ALTER TABLE tbl_hkncombi ALTER COLUMN tsy_nyukftnrate TYPE smallint;
ALTER TABLE tbl_hkncombi ALTER COLUMN tsy_nyukkotgak TYPE integer;
ALTER TABLE tbl_hkncombi ALTER COLUMN tsy_nyukjgngak TYPE integer;
ALTER TABLE tbl_hkncombi ALTER COLUMN tsy_nyudjgngak TYPE integer;
ALTER TABLE tbl_hkncombi ALTER COLUMN tsy_nyudjgncnt TYPE smallint;
ALTER TABLE tbl_hkncombi ALTER COLUMN tsy_nyumjgngak TYPE integer;
ALTER TABLE tbl_hkncombi ALTER COLUMN tsy_nyumjgncnt TYPE smallint;
ALTER TABLE tbl_hkncombi ALTER COLUMN tnk_gaikftnrate TYPE smallint;
ALTER TABLE tbl_hkncombi ALTER COLUMN tnk_gaikkotgak TYPE integer;
ALTER TABLE tbl_hkncombi ALTER COLUMN tnk_gaikjgngak TYPE integer;
ALTER TABLE tbl_hkncombi ALTER COLUMN tnk_gaidjgngak TYPE integer;
ALTER TABLE tbl_hkncombi ALTER COLUMN tnk_gaidjgncnt TYPE smallint;
ALTER TABLE tbl_hkncombi ALTER COLUMN tnk_gaimnaijgngak TYPE integer;
ALTER TABLE tbl_hkncombi ALTER COLUMN tnk_gaimgaijgngak TYPE integer;
ALTER TABLE tbl_hkncombi ALTER COLUMN tnk_gaimjgncnt TYPE smallint;
ALTER TABLE tbl_hkncombi ALTER COLUMN tnk_nyukftnrate TYPE smallint;
ALTER TABLE tbl_hkncombi ALTER COLUMN tnk_nyukkotgak TYPE integer;
ALTER TABLE tbl_hkncombi ALTER COLUMN tnk_nyukjgngak TYPE integer;
ALTER TABLE tbl_hkncombi ALTER COLUMN tnk_nyudjgngak TYPE integer;
ALTER TABLE tbl_hkncombi ALTER COLUMN tnk_nyudjgncnt TYPE smallint;
ALTER TABLE tbl_hkncombi ALTER COLUMN tnk_nyumjgngak TYPE integer;
ALTER TABLE tbl_hkncombi ALTER COLUMN tnk_nyumjgncnt TYPE smallint;
ALTER TABLE tbl_hkncombi ALTER COLUMN tentnk TYPE smallint;
ALTER TABLE tbl_hkncombi ALTER COLUMN hospnum TYPE smallint;
