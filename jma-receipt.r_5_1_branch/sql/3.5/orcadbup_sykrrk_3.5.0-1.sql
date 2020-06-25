\set ON_ERROR_STOP

alter table tbl_sykrrk add column hospnum numeric(2,0);
alter table tbl_sykrrk drop column hospid;
update tbl_sykrrk set hospnum = 1;
alter table tbl_sykrrk alter column hospnum set not null;
alter table tbl_sykrrk add constraint tbl_sykrrk_primary_key primary key (hospnum, ptid, sykkbn, sykymd);
