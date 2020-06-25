
\set ON_ERROR_STOP

alter table tbl_seikyu_main add column hknkbn_rsi character varying(1);
update tbl_seikyu_main set hknkbn_rsi = '';
