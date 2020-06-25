\set ON_ERROR_STOP

alter table tbl_kohsky add column hospnum numeric(2,0);
alter table tbl_kohsky drop column hospid;
update tbl_kohsky set hospnum = 1;
alter table tbl_kohsky alter column hospnum set not null;
alter table tbl_kohsky add constraint tbl_kohsky_primary_key primary key (hospnum, sryym, nyugaikbn, ptid, receka, teisyutusaki, recesyubetu, hknjanum_key, hojokbn_key, tekstymd);
