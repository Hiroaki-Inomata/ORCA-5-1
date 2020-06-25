\set ON_ERROR_STOP

alter table tbl_pttainfuka add column hospnum numeric(2,0);
alter table tbl_pttainfuka drop column hospid;
update tbl_pttainfuka set hospnum = 1;
alter table tbl_pttainfuka alter column hospnum set not null;
alter table tbl_pttainfuka add constraint tbl_pttainfuka_primary_key primary key (hospnum, ptid, rrknum);
