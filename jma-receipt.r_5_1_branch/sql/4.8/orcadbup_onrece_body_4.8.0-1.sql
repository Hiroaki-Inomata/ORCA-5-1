-- tbl_onrece_body --

\set ON_ERROR_STOP

alter table tbl_onrece_body add column  receipt_kbn char(10);
update tbl_onrece_body set receipt_kbn='          ';