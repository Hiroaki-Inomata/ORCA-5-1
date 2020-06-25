-- tbl_receden --

\set ON_ERROR_STOP

ALTER TABLE tbl_receden DROP CONSTRAINT tbl_receden_primary_key;
alter table tbl_receden add column kohid_key bigint default 0;
alter table tbl_receden add column crehms char(6);
update tbl_receden set kohid_key = 0;
update tbl_receden set crehms = '';
alter table tbl_receden alter column kohid_key set not null;
ALTER TABLE ONLY tbl_receden
ADD CONSTRAINT tbl_receden_primary_key PRIMARY KEY (hospnum, sryym, nyugaikbn, ptid, receka, teisyutusaki, recesyubetu, hknjanum, hojokbn_key, kohid_key, tekstymd, reckbn, rennum);
