
\set ON_ERROR_STOP

alter table tbl_ptbyomei add column dischargeflg character varying(1);
alter table tbl_ptbyomei add column class1 smallint default 0;
alter table tbl_ptbyomei add column class2 smallint default 0;
update tbl_ptbyomei set dischargeflg = '';
