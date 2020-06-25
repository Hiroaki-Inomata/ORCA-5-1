---------------------
-- 定期請求関係テーブル項目属性変更
-- 定期請求履歴
---------------------

\set ON_ERROR_STOP

-- tbl_teikirrk --
ALTER TABLE tbl_teikirrk alter column hospnum type smallint;
