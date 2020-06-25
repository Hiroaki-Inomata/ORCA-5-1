\set ON_ERROR_STOP

alter table tbl_hkncombi add column hyojikbn char(1);
update tbl_hkncombi set hyojikbn = ' ';

