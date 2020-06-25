\set ON_ERROR_STOP

alter table tbl_syutotal add column chosei1 integer;
alter table tbl_syutotal add column chosei2 integer;
update tbl_syutotal set chosei1 = chosei;
update tbl_syutotal set chosei2 = 0 ;

