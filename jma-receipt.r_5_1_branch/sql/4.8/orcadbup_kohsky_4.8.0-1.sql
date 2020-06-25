-- tbl_kohsky --

\set ON_ERROR_STOP

ALTER TABLE tbl_kohsky DROP CONSTRAINT tbl_kohsky_primary_key;
alter table tbl_kohsky add column receipt_kbn char(10);
update tbl_kohsky set receipt_kbn='          ';
alter table tbl_kohsky alter column receipt_kbn set not null;
ALTER TABLE ONLY tbl_kohsky
ADD CONSTRAINT tbl_kohsky_primary_key PRIMARY KEY (hospnum, sryym, nyugaikbn, ptid, receka, teisyutusaki, recesyubetu, hknjanum_key, hojokbn_key, kohid_key, tekstymd,receipt_kbn);
