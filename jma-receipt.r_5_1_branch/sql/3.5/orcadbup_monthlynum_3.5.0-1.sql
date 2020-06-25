\set ON_ERROR_STOP

alter table tbl_monthlynum add column hospnum numeric(2,0);
alter table tbl_monthlynum drop column hospid;
update tbl_monthlynum set hospnum = 1;
alter table tbl_monthlynum alter column hospnum set not null;
alter table tbl_monthlynum add constraint tbl_monthlynum_primary_key primary key (hospnum, ptid, kohnum, kohid, sryym, nyugaikbn);
