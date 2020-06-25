\set ON_ERROR_STOP

alter table tbl_ptnyuinrrk add column hospnum numeric(2,0);
alter table tbl_ptnyuinrrk drop column hospid;
update tbl_ptnyuinrrk set hospnum = 1;
alter table tbl_ptnyuinrrk alter column hospnum set not null;
alter table tbl_ptnyuinrrk add constraint tbl_ptnyuinrrk_primary_key primary key (hospnum, ptid, rrknum, rrkedanum);
