\set ON_ERROR_STOP

alter table tbl_ptnyuinrrk add column tainrelkbn  char(1);
update tbl_ptnyuinrrk set tainrelkbn ='' ;
