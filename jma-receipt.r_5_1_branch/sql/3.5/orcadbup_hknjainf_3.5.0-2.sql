\set ON_ERROR_STOP

alter table tbl_hknjainf add column hospnum2 numeric(2,0);
update tbl_hknjainf set hospnum2=hospnum ;
alter table tbl_hknjainf drop column hospnum;
alter table tbl_hknjainf add column hospnum numeric(2,0);
update tbl_hknjainf set hospnum=hospnum2 ;
alter table tbl_hknjainf drop column hospnum2;
alter table tbl_hknjainf alter column hospnum set not null;
alter table tbl_hknjainf add constraint tbl_hknjainf_primary_key primary key (hospnum, hknjanum);

