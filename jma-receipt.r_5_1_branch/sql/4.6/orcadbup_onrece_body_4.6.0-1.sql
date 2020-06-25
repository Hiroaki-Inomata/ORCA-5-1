-- tbl_onrece_body --

\set ON_ERROR_STOP

alter table tbl_onrece_body add column kohid bigint default 0;
update tbl_onrece_body set kohid = 0;

