\set ON_ERROR_STOP

alter table tbl_syutotal add column hospnum numeric(2,0);
alter table tbl_syutotal drop column hospid;
update tbl_syutotal set hospnum = 1;
alter table tbl_syutotal alter column hospnum set not null;
alter table tbl_syutotal add constraint tbl_syutotal_primary_key primary key (hospnum, nyugaikbn, ptid, denpnum);
