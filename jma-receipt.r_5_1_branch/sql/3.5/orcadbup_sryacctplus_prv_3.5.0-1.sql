\set ON_ERROR_STOP

alter table tbl_sryacctplus_prv add column hospnum numeric(2,0);
alter table tbl_sryacctplus_prv drop column hospid;
update tbl_sryacctplus_prv set hospnum = 1;
alter table tbl_sryacctplus_prv alter column hospnum set not null;
alter table tbl_sryacctplus_prv add constraint tbl_sryacctplus_prv_primary_key primary key (hospnum, nyugaikbn, ptid, sryka, sryym, zainum, rennum);
