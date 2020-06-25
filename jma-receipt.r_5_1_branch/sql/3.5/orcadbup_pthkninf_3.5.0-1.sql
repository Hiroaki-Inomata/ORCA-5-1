\set ON_ERROR_STOP

alter table tbl_pthkninf add column hospnum numeric(2,0);
alter table tbl_pthkninf drop column hospid;
update tbl_pthkninf set hospnum = 1;
alter table tbl_pthkninf alter column hospnum set not null;
alter table tbl_pthkninf add constraint tbl_pthkninf_primary_key primary key (hospnum, ptid, hknid);
