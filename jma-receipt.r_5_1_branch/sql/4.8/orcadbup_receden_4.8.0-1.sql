-- tbl_receden --

\set ON_ERROR_STOP

ALTER TABLE tbl_receden DROP CONSTRAINT tbl_receden_primary_key;
alter table tbl_receden add column receipt_kbn char(10);
update tbl_receden set receipt_kbn='          ';
alter table tbl_receden alter column receipt_kbn set not null;
ALTER TABLE ONLY tbl_receden
ADD CONSTRAINT tbl_receden_primary_key PRIMARY KEY (hospnum, sryym, nyugaikbn, ptid, receka, teisyutusaki, recesyubetu, hknjanum, hojokbn_key, kohid_key, tekstymd,receipt_kbn,reckbn,rennum);