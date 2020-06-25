-- tbl_henrei_body --

\set ON_ERROR_STOP

alter table tbl_henrei_body add column disaster character varying(2);
alter table tbl_henrei_body add column s_disaster_1 character varying(2);
alter table tbl_henrei_body add column s_disaster_2 character varying(2);
alter table tbl_henrei_body add column s_disaster_3 character varying(2);
alter table tbl_henrei_body add column s_disaster_4 character varying(2);
alter table tbl_henrei_body add column s_disaster_5 character varying(2);
alter table tbl_henrei_body add column s_disaster_6 character varying(2);
alter table tbl_henrei_body add column s_disaster_7 character varying(2);
alter table tbl_henrei_body add column s_disaster_8 character varying(2);
alter table tbl_henrei_body add column s_disaster_9 character varying(2);
alter table tbl_henrei_body add column s_disaster_10 character varying(2);
update tbl_henrei_body set disaster='00',s_disaster_1='',s_disaster_2='',s_disaster_3='',s_disaster_4='',s_disaster_5='',s_disaster_6='',s_disaster_7='',s_disaster_8='',s_disaster_9='',s_disaster_10='';
