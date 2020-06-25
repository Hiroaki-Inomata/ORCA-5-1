\set ON_ERROR_STOP

alter table tbl_sryact add column hospnum numeric(2,0);
alter table tbl_sryact drop column hospid;
update tbl_sryact set hospnum = 1;
alter table tbl_sryact alter column hospnum set not null;
alter table tbl_sryact add constraint tbl_sryact_primary_key primary key (hospnum, nyugaikbn, ptid, sryka, sryym, zainum, rennum);
