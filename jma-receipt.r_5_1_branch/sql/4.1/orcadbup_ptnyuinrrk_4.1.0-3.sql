\set ON_ERROR_STOP

alter table tbl_ptnyuinrrk add column kaigokbn  char(1);
update tbl_ptnyuinrrk set kaigokbn ='1' ;
