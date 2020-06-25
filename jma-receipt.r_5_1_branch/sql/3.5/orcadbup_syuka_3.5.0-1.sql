\set ON_ERROR_STOP

alter table tbl_syuka add column hospnum numeric(2,0);
alter table tbl_syuka drop column hospid;
update tbl_syuka set hospnum = 1;
alter table tbl_syuka alter column hospnum set not null;
alter table tbl_syuka add constraint tbl_syuka_primary_key primary key (hospnum, nyugaikbn, ptid, ten_kbn, sryym, teisyutusaki, hknjanum, hojokbn, recesyubetu, sryka);
create index idx_syuka_sryym on tbl_syuka(hospnum, nyugaikbn, sryym);
