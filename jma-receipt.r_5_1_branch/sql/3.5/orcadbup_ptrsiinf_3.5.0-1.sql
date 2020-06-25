\set ON_ERROR_STOP

alter table tbl_ptrsiinf add column hospnum numeric(2,0);
alter table tbl_ptrsiinf drop column hospid;
update tbl_ptrsiinf set hospnum = 1;
alter table tbl_ptrsiinf alter column hospnum set not null;
alter table tbl_ptrsiinf add constraint tbl_ptrsiinf_primary_key primary key (hospnum, ptid, hknid);
