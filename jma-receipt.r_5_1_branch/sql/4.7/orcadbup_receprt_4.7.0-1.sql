\set ON_ERROR_STOP

alter table tbl_receprt add column sort_2 varchar(50);
update tbl_receprt set sort_2 = ' ';
