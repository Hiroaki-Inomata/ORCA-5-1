\set ON_ERROR_STOP

alter table tbl_syunou_main add column chosei1 integer;
alter table tbl_syunou_main add column chosei2 integer;
update tbl_syunou_main set chosei1 = chosei;
update tbl_syunou_main set chosei2 = 0 ;

