-- tbl_seikyu_main --

\set ON_ERROR_STOP

alter table tbl_seikyu_main add column direct_flg char(1);
update tbl_seikyu_main set direct_flg='0';

