-- tbl_order --


-- 入院オーダー
-- UKEHMS変更

\set ON_ERROR_STOP

ALTER TABLE tbl_order alter column ukehms type char(8);
update tbl_order set ukehms = trim(ukehms) || '00';

