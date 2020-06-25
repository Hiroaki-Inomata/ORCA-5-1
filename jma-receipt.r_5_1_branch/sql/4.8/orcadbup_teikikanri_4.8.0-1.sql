---------------------
-- 定期請求関係テーブル項目属性変更
-- 定期請求管理
---------------------

\set ON_ERROR_STOP

-- tbl_teikikanri --
ALTER TABLE tbl_teikikanri alter column page    type integer ;
ALTER TABLE tbl_teikikanri alter column kensu   type integer ;
ALTER TABLE tbl_teikikanri alter column hospnum type smallint;

