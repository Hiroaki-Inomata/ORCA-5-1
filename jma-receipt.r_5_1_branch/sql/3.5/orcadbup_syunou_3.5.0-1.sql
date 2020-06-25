\set ON_ERROR_STOP

alter table tbl_syunou add column hospnum numeric(2,0);
alter table tbl_syunou drop column hospid;
update tbl_syunou set hospnum = 1;
alter table tbl_syunou alter column hospnum set not null;
alter table tbl_syunou add constraint tbl_syunou_primary_key primary key (hospnum, nyugaikbn, ptid, denpnum);
drop index idx_syunou_ptid;
create index idx_syunou_ptid on tbl_syunou(hospnum, ptid);
