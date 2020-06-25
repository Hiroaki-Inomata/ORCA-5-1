\set ON_ERROR_STOP

alter table tbl_ptnyuinrrk add column nyucalkbn  char(1);
update tbl_ptnyuinrrk set nyucalkbn ='2' ;
