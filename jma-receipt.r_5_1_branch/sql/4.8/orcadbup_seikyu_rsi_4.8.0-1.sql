-- tbl_seikyu_rsi --

\set ON_ERROR_STOP

ALTER TABLE tbl_seikyu_rsi DROP CONSTRAINT tbl_seikyu_rsi_primary_key;
alter table tbl_seikyu_rsi add column receipt_kbn char(10);
update tbl_seikyu_rsi set receipt_kbn='          ';
alter table tbl_seikyu_rsi alter column receipt_kbn set not null;
ALTER TABLE ONLY tbl_seikyu_rsi
ADD CONSTRAINT tbl_seikyu_rsi_primary_key PRIMARY KEY (hospnum, sryym, nyugaikbn, ptid, receka, teisyutusaki, recesyubetu, hknjanum, hojokbn_key, kohid_key, tekstymd,receipt_kbn);

