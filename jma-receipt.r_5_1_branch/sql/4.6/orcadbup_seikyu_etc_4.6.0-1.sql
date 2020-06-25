-- tbl_seikyu_etc --

\set ON_ERROR_STOP

ALTER TABLE tbl_seikyu_etc DROP CONSTRAINT tbl_seikyu_etc_primary_key;
alter table tbl_seikyu_etc add column kohid_key bigint default 0;
update tbl_seikyu_etc set kohid_key = 0;
alter table tbl_seikyu_etc alter column kohid_key set not null;
ALTER TABLE ONLY tbl_seikyu_etc
ADD CONSTRAINT tbl_seikyu_etc_primary_key PRIMARY KEY (hospnum, sryym, nyugaikbn, ptid, receka, teisyutusaki, recesyubetu, hknjanum, hojokbn_key, kohid_key, tekstymd);

