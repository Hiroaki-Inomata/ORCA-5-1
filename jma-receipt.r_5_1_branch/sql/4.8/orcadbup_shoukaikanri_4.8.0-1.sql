---------------------
-- 患者照会検索条件管理テーブル項目追加
-- 表題区分
---------------------

\set ON_ERROR_STOP

-- tbl_teikirrk --
ALTER TABLE tbl_shoukaikanri add column titlekbn char(1);
update tbl_shoukaikanri set titlekbn = '1';
