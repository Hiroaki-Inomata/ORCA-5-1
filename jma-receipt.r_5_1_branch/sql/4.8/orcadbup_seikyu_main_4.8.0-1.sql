-- tbl_seikyu_main --

\set ON_ERROR_STOP

ALTER TABLE tbl_seikyu_main DROP CONSTRAINT tbl_seikyu_main_primary_key;
alter table tbl_seikyu_main add column receipt_kbn char(10);
update tbl_seikyu_main set receipt_kbn='          ';
alter table tbl_seikyu_main alter column receipt_kbn set not null;
ALTER TABLE ONLY tbl_seikyu_main
ADD CONSTRAINT tbl_seikyu_main_primary_key PRIMARY KEY (hospnum, sryym, nyugaikbn, ptid, receka, teisyutusaki, recesyubetu, hknjanum, hojokbn_key, kohid_key, tekstymd,receipt_kbn);

