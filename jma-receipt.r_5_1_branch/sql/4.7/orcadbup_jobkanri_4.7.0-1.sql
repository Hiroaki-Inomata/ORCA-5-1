\set ON_ERROR_STOP

alter table tbl_jobkanri add column updcnt1 numeric(10,0);
alter table tbl_jobkanri add column updcnt2 numeric(10,0);
update tbl_jobkanri set updcnt1 = 0,updcnt2 = 0;
