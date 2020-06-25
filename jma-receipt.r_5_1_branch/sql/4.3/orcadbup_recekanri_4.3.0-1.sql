\set ON_ERROR_STOP

alter table tbl_recekanri add column prtkbn char(1);
update tbl_recekanri set prtkbn = ' ';
