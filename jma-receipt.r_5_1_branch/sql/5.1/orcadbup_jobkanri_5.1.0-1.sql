\set ON_ERROR_STOP

alter table tbl_jobkanri add column karte_uid character varying(36);
alter table tbl_jobkanri add column orca_uid character varying(36);
update tbl_jobkanri set karte_uid = '',orca_uid = '';
