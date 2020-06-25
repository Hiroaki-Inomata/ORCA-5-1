\set ON_ERROR_STOP

alter table tbl_ptinf_prv add column hospnum numeric(2,0);
alter table tbl_ptinf_prv drop column hospid;
update tbl_ptinf_prv set hospnum = 1;
alter table tbl_ptinf_prv alter column hospnum set not null;
alter table tbl_ptinf_prv add constraint tbl_ptinf_prv_primary_key primary key (hospnum, ptid);
drop index idx_ptinf_prv_kananame;
create index idx_ptinf_prv_kananame on tbl_ptinf_prv(hospnum, kananame);
drop index idx_ptinf_prv_name;
create index idx_ptinf_prv_name on tbl_ptinf_prv(hospnum, name);
