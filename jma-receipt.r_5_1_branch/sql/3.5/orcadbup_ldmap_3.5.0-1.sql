\set ON_ERROR_STOP

alter table tbl_ldmap add column hospnum numeric(2,0);
update tbl_ldmap set hospnum = 1;
alter table tbl_ldmap alter column hospnum set not null;
alter table tbl_ldmap drop constraint tbl_ldmap_pkey;
alter table tbl_ldmap add constraint tbl_ldmap_primary_key primary key (hospnum, map_ldname);
