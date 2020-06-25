\set ON_ERROR_STOP

alter table tbl_ptbyomei add column hosoku_comment character varying(80);
alter table tbl_ptbyomei add column hosokucd_1 character varying(20);
alter table tbl_ptbyomei add column hosokucd_2 character varying(20);
alter table tbl_ptbyomei add column hosokucd_3 character varying(20);
update tbl_ptbyomei set hosoku_comment = ' ';
update tbl_ptbyomei set hosokucd_1 = ' ';
update tbl_ptbyomei set hosokucd_2 = ' ';
update tbl_ptbyomei set hosokucd_3 = ' ';
