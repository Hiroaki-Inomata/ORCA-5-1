\set ON_ERROR_STOP

alter table tbl_prtdata add column hospnum numeric(2,0);
update tbl_prtdata set hospnum = 1;
alter table tbl_prtdata alter column hospnum set not null;
alter table tbl_prtdata drop constraint tbl_prtdata_primary_key;
alter table tbl_prtdata add constraint tbl_prtdata_primary_key primary key (hospnum, tbl_key, rennum, tbl_group, shori_rennum, page);
