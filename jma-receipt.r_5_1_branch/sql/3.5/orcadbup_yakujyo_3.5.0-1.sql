\set ON_ERROR_STOP

alter table tbl_yakujyo add column hospnum numeric(2,0);
alter table tbl_yakujyo drop column hospid;
update tbl_yakujyo set hospnum = 1;
alter table tbl_yakujyo alter column hospnum set not null;
alter table tbl_yakujyo add constraint tbl_yakujyo_primary_key primary key (hospnum, srycd, teknum);
