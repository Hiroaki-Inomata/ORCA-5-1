\set ON_ERROR_STOP

alter table tbl_receprt add column drcd char(5);
alter table tbl_receprt add column drcd_sryka char(2);
update tbl_receprt set drcd = ' ';
update tbl_receprt set drcd_sryka = ' ';
