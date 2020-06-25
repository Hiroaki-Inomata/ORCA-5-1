\set ON_ERROR_STOP

alter table tbl_opecom add column hospnum numeric(2,0);
alter table tbl_opecom drop column hospid;
update tbl_opecom set hospnum = 1;
alter table tbl_opecom alter column hospnum set not null;
alter table tbl_opecom add constraint tbl_opecom_primary_key primary key (hospnum, ptid, sryymd, srycd, rennum);
