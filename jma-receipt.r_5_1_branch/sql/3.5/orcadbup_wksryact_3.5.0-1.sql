\set ON_ERROR_STOP

alter table tbl_wksryact add column hospnum numeric(2,0);
alter table tbl_wksryact drop column hospid;
update tbl_wksryact set hospnum = 1;
alter table tbl_wksryact alter column hospnum set not null;
alter table tbl_wksryact add constraint tbl_wksryact_primary_key primary key (hospnum, nyugaikbn, ptid, sryka, sryymd, hkncombi, zainum, rennum);
