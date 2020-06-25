
\set ON_ERROR_STOP

alter table tbl_ptrsiinf add column jibaikbn character varying(1);
update tbl_ptrsiinf set jibaikbn = '';
