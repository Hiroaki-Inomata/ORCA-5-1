-- tbl_seikyu_etc --

\set ON_ERROR_STOP

ALTER TABLE tbl_seikyu_etc DROP CONSTRAINT tbl_seikyu_etc_primary_key;
alter table tbl_seikyu_etc add column receipt_kbn char(10);
update tbl_seikyu_etc set receipt_kbn='          ';
alter table tbl_seikyu_etc alter column receipt_kbn set not null;
ALTER TABLE ONLY tbl_seikyu_etc
ADD CONSTRAINT tbl_seikyu_etc_primary_key PRIMARY KEY (hospnum, sryym, nyugaikbn, ptid, receka, teisyutusaki, recesyubetu, hknjanum, hojokbn_key, kohid_key, tekstymd,receipt_kbn);

