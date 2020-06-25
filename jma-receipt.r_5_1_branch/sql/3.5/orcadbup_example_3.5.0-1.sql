\set ON_ERROR_STOP

alter table tbl_example add column hospnum numeric(2,0);
alter table tbl_example drop column hospid;
update tbl_example set hospnum = 1;
alter table tbl_example alter column hospnum set not null;
alter table tbl_example add constraint tbl_example_primary_key primary key (hospnum, kubun, rennum);
