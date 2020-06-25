\set ON_ERROR_STOP

alter table tbl_ptnyuinrrk add column toku_santeiyoukenkbn  char(1)  default '';
update tbl_ptnyuinrrk set toku_santeiyoukenkbn ='' ;
