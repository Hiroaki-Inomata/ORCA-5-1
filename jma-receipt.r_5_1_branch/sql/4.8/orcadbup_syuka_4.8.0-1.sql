-- tbl_syuka --

\set ON_ERROR_STOP

ALTER TABLE tbl_syuka DROP CONSTRAINT tbl_syuka_primary_key;
alter table tbl_syuka add column receipt_kbn char(10);
update tbl_syuka set receipt_kbn='          ';
alter table tbl_syuka alter column receipt_kbn set not null;
ALTER TABLE ONLY tbl_syuka
ADD CONSTRAINT tbl_syuka_primary_key PRIMARY KEY (hospnum, nyugaikbn, ptid, ten_kbn, sryym, teisyutusaki, hknjanum, hojokbn, kohid ,receipt_kbn, recesyubetu, sryka);

