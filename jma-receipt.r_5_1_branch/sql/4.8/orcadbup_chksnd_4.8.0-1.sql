---------------------
-- チェックマスタ２項目追加
-- 補足コメント、補足コメント条件区分
---------------------

\set ON_ERROR_STOP

-- tbl_teikirrk --
ALTER TABLE tbl_chksnd add column hosoku varchar(40);
ALTER TABLE tbl_chksnd add column hosoku_compkbn char(1);
update tbl_chksnd set hosoku = '';
update tbl_chksnd set hosoku_compkbn = '';

