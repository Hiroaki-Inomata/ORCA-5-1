-- tbl_userbyomei --

\set ON_ERROR_STOP

alter table tbl_userbyomei add column hosoku_comment character varying(80);
update tbl_userbyomei set hosoku_comment = '';
