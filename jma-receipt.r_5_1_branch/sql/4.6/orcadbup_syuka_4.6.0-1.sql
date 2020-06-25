-- tbl_syuka --

\set ON_ERROR_STOP

ALTER TABLE tbl_syuka DROP CONSTRAINT tbl_syuka_primary_key;
alter table tbl_syuka add column kohid bigint default 0;
update tbl_syuka set kohid = 0;
alter table tbl_syuka alter column kohid set not null;
ALTER TABLE ONLY tbl_syuka
ADD CONSTRAINT tbl_syuka_primary_key PRIMARY KEY (hospnum, nyugaikbn, ptid, ten_kbn, sryym, teisyutusaki, hknjanum, hojokbn, kohid, recesyubetu, sryka);

