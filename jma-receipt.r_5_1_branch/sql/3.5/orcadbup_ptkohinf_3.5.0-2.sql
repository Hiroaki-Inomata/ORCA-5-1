\set ON_ERROR_STOP

alter table tbl_ptkohinf add column tekedchkkbn char(1);
update tbl_ptkohinf set tekedchkkbn = '';

