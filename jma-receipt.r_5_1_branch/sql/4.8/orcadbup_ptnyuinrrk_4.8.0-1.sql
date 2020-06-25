-- tbl_ptnyuinrrk --

\set ON_ERROR_STOP

alter table tbl_ptnyuinrrk add column toku_taisyonissu smallint;
alter table tbl_ptnyuinrrk alter column toku_taisyonissu set default 0;
update tbl_ptnyuinrrk set toku_taisyonissu = 0;

