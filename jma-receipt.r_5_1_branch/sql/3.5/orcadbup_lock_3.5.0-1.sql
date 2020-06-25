\set ON_ERROR_STOP

alter table tbl_lock add column hospnum numeric(2,0);
update tbl_lock set hospnum = 1;
alter table tbl_lock alter column hospnum set not null;
alter table tbl_lock drop constraint tbl_lock_pkey;
alter table tbl_lock add constraint tbl_lock_primary_key primary key (hospnum, lock_termid);
