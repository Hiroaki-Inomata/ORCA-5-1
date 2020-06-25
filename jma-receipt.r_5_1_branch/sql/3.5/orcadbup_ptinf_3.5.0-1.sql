\set ON_ERROR_STOP

alter table tbl_ptinf add column hospnum numeric(2,0);
alter table tbl_ptinf drop column hospid;
update tbl_ptinf set hospnum = 1;
alter table tbl_ptinf alter column hospnum set not null;
alter table tbl_ptinf add constraint tbl_ptinf_primary_key primary key (hospnum, ptid);
drop index idx_ptinf_kananame;
create index idx_ptinf_kananame on tbl_ptinf(hospnum, kananame);
drop index idx_ptinf_name;
create index idx_ptinf_name on tbl_ptinf(hospnum, name);
