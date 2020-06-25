
\set ON_ERROR_STOP

alter table tbl_jibai_prt add column hknkbn character varying(1);
update tbl_jibai_prt set hknkbn = '';
