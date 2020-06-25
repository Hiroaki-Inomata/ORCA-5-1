\set ON_ERROR_STOP

alter table tbl_ptcom_prv add column hospnum numeric(2,0);
alter table tbl_ptcom_prv drop column hospid;
update tbl_ptcom_prv set hospnum = 1;
alter table tbl_ptcom_prv alter column hospnum set not null;
alter table tbl_ptcom_prv add constraint tbl_ptcom_prv_primary_key primary key (hospnum, ptid, zainum, srycd, rennum);
