
\set ON_ERROR_STOP

alter table tbl_tsyrrk add column kyoukaisoukbn character (1);
update tbl_tsyrrk set kyoukaisoukbn = '0';
