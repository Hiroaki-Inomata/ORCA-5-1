\set ON_ERROR_STOP

alter table tbl_syutotal_prv add column hospnum numeric(2,0);
alter table tbl_syutotal_prv drop column hospid;
update tbl_syutotal_prv set hospnum = 1;
alter table tbl_syutotal_prv alter column hospnum set not null;
alter table tbl_syutotal_prv add constraint tbl_syutotal_prv_primary_key primary key (hospnum, nyugaikbn, ptid, denpnum, creymd);
