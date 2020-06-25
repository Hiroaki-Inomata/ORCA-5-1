-- tbl_kohsky --

\set ON_ERROR_STOP

ALTER TABLE tbl_kohsky DROP CONSTRAINT tbl_kohsky_primary_key;
alter table tbl_kohsky add column kohid_key bigint default 0;
update tbl_kohsky set kohid_key = 0;
alter table tbl_kohsky alter column kohid_key set not null;
ALTER TABLE ONLY tbl_kohsky
ADD CONSTRAINT tbl_kohsky_primary_key PRIMARY KEY (hospnum, sryym, nyugaikbn, ptid, receka, teisyutusaki, recesyubetu, hknjanum_key, hojokbn_key, kohid_key, tekstymd);
