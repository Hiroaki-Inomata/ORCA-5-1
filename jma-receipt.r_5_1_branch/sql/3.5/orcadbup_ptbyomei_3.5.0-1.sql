\set ON_ERROR_STOP

alter table tbl_ptbyomei add column hospnum numeric(2,0);
alter table tbl_ptbyomei drop column hospid;
update tbl_ptbyomei set hospnum = 1;
alter table tbl_ptbyomei alter column hospnum set not null;
alter table tbl_ptbyomei add constraint tbl_ptbyomei_primary_key primary key (hospnum, ptid, sryka, sryymd, rennum);
