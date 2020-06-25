\set ON_ERROR_STOP

alter table tbl_sryacctplus add column hospnum numeric(2,0);
alter table tbl_sryacctplus drop column hospid;
update tbl_sryacctplus set hospnum = 1;
alter table tbl_sryacctplus alter column hospnum set not null;
alter table tbl_sryacctplus add constraint tbl_sryacctplus_primary_key primary key (hospnum, nyugaikbn, ptid, sryka, sryym, zainum, rennum);
