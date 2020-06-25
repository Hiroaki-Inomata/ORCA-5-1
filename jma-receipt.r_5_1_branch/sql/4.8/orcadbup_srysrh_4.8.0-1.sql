---------------------
-- 診療行為照会テーブル項目追加
-- 包括区分
---------------------

\set ON_ERROR_STOP

-- tbl_teikirrk --
ALTER TABLE tbl_srysrh add column hokatsukbn char(1);
update tbl_srysrh set hokatsukbn ='0';
