\set ON_ERROR_STOP

alter table tbl_rececom add column hospnum numeric(2,0);
alter table tbl_rececom drop column hospid;
update tbl_rececom set hospnum = 1;
alter table tbl_rececom alter column hospnum set not null;
alter table tbl_rececom add constraint tbl_rececom_primary_key primary key (hospnum, ptid, nyugaikbn, sryka, sryym, hkncombi, srydd, sjkbn);
