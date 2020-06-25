\set ON_ERROR_STOP

alter table tbl_hknjainf add column hospnum numeric;
alter table tbl_hknjainf drop column hospid;
update tbl_hknjainf set hospnum = 1;
alter table tbl_hknjainf alter column hospnum set not null;
alter table tbl_hknjainf add constraint tbl_hknjainf_primary_key primary key (hospnum, hknjanum);
