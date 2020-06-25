-- tbl_toukeimemo --

\set ON_ERROR_STOP

alter table tbl_toukeimemo add column usermemo varchar(5000);
update tbl_toukeimemo set usermemo = '' ;

