
\set ON_ERROR_STOP

alter table tbl_tnkrrk add column kyoukaisoukbn character (1);
update tbl_tnkrrk set kyoukaisoukbn = '0';
