-- tbl_seikyu_koh --

\set ON_ERROR_STOP

ALTER TABLE tbl_seikyu_koh DROP CONSTRAINT tbl_seikyu_koh_primary_key;
alter table tbl_seikyu_koh add column receipt_kbn char(10);
update tbl_seikyu_koh set receipt_kbn='          ';
alter table tbl_seikyu_koh alter column receipt_kbn set not null;
ALTER TABLE ONLY tbl_seikyu_koh
ADD CONSTRAINT tbl_seikyu_koh_primary_key PRIMARY KEY (hospnum, sryym, nyugaikbn, ptid, receka, teisyutusaki, recesyubetu, hknjanum, hojokbn_key, kohid_key, tekstymd,receipt_kbn, rennum);

