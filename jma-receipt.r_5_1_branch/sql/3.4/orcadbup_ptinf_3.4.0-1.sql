\set ON_ERROR_STOP
--
--  患者情報テーブルの変更           --
--
--  患者情報インデックス追加         --
--  (生年月日)                       --
-- 
-- Create Date : 2007/03/19          --
--
CREATE INDEX idx_ptinf_birthday ON tbl_ptinf USING btree (birthday);
