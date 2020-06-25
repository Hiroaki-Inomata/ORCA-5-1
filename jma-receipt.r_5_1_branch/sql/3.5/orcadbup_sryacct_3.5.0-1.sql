\set ON_ERROR_STOP

alter table tbl_sryacct add column hospnum numeric(2,0);
alter table tbl_sryacct drop column hospid;
update tbl_sryacct set hospnum = 1;
alter table tbl_sryacct alter column hospnum set not null;
alter table tbl_sryacct add constraint tbl_sryacct_primary_key primary key (hospnum, nyugaikbn, ptid, sryka, sryym, srykbn, zainum);
create index idx_sryacct_ptid_sryym on tbl_sryacct(hospnum, nyugaikbn, ptid, sryym);
