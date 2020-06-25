\set ON_ERROR_STOP

alter table tbl_syunou_prv add column chosei1 integer;
alter table tbl_syunou_prv add column chosei2 integer;
update tbl_syunou_prv set chosei1 = chosei;
update tbl_syunou_prv set chosei2 = 0 ;

