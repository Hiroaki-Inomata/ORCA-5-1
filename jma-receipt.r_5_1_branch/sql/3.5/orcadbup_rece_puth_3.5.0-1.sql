\set ON_ERROR_STOP

alter table tbl_rece_puth add column hospnum numeric(2,0);
alter table tbl_rece_puth drop column hospid;
update tbl_rece_puth set hospnum = 1;
alter table tbl_rece_puth alter column hospnum set not null;
alter table tbl_rece_puth add constraint tbl_rece_puth_primary_key primary key (hospnum, styukymd, edyukymd);
