\set ON_ERROR_STOP

alter table tbl_bunben add column teisyutubikbn  char(1);
update tbl_bunben set teisyutubikbn ='1' ;
