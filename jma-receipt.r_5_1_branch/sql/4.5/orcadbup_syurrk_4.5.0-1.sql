\set ON_ERROR_STOP

alter table tbl_syurrk add column chosei1 integer;
alter table tbl_syurrk add column chosei2 integer;
update tbl_syurrk set chosei1 = chosei;
update tbl_syurrk set chosei2 = 0 ;

