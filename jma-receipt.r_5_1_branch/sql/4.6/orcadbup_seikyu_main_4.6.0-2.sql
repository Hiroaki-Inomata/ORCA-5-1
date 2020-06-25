-- tbl_seikyu_main --

\set ON_ERROR_STOP

ALTER TABLE tbl_seikyu_main DROP CONSTRAINT tbl_seikyu_main_primary_key;
alter table tbl_seikyu_main add column kohid_key bigint default 0;
alter table tbl_seikyu_main add column crehms char(6);
alter table tbl_seikyu_main add column updhms char(6);
update tbl_seikyu_main set kohid_key=0,crehms='',updhms='000000';
alter table tbl_seikyu_main alter column kohid_key set not null;
ALTER TABLE ONLY tbl_seikyu_main
ADD CONSTRAINT tbl_seikyu_main_primary_key PRIMARY KEY (hospnum, sryym, nyugaikbn, ptid, receka, teisyutusaki, recesyubetu, hknjanum, hojokbn_key, kohid_key, tekstymd);

