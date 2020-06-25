\set ON_ERROR_STOP

alter table tbl_srycdchg add column hospnum numeric(2,0);
update tbl_srycdchg set hospnum = 1;
alter table tbl_srycdchg alter column hospnum set not null;
alter table tbl_srycdchg drop constraint  tbl_srycdchg_primary_key;
alter table tbl_srycdchg add constraint tbl_srycdchg_primary_key primary key (hospnum, ipnsrycd, rjnsrycd);
drop index idx_srycdchg_rjnsrycd;
create index idx_srycdchg_rjnsrycd on tbl_srycdchg(hospnum, rjnsrycd);
