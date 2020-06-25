-- tbl_henrei_body --

\set ON_ERROR_STOP

alter table tbl_henrei_body add column  receipt_kbn char(10);
alter table tbl_henrei_body add column  s_receipt_kbn_1 char(10);
alter table tbl_henrei_body add column  s_receipt_kbn_2 char(10);
alter table tbl_henrei_body add column  s_receipt_kbn_3 char(10);
alter table tbl_henrei_body add column  s_receipt_kbn_4 char(10);
alter table tbl_henrei_body add column  s_receipt_kbn_5 char(10);
alter table tbl_henrei_body add column  s_receipt_kbn_6 char(10);
alter table tbl_henrei_body add column  s_receipt_kbn_7 char(10);
alter table tbl_henrei_body add column  s_receipt_kbn_8 char(10);
alter table tbl_henrei_body add column  s_receipt_kbn_9 char(10);
alter table tbl_henrei_body add column  s_receipt_kbn_10 char(10);
update tbl_henrei_body set receipt_kbn='          ',s_receipt_kbn_1='          ',s_receipt_kbn_2='          ',s_receipt_kbn_3='          ',s_receipt_kbn_4='          ',s_receipt_kbn_5='          ',s_receipt_kbn_6='          ',s_receipt_kbn_7='          ',s_receipt_kbn_8='          ',s_receipt_kbn_9='          ',s_receipt_kbn_10='          ';
