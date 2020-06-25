\set ON_ERROR_STOP

alter table tbl_receprt add column sort_1 varchar(10);
update tbl_receprt set sort_1 = ' ';
