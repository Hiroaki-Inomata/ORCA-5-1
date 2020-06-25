\set ON_ERROR_STOP

alter table tbl_prtkanri add  column tbl_uuid  character(36);
alter table tbl_prtkanri add  column gyoumu_cd  numeric(3);
alter table tbl_prtkanri add  column creymd  character varying(8);
update tbl_prtkanri set tbl_uuid='',gyoumu_cd=0,creymd='';
alter table tbl_prtkanri drop constraint  tbl_prtkanri_primary_key;
alter table tbl_prtkanri add constraint tbl_prtkanri_primary_key primary key (hospnum,tbl_key, rennum, tbl_group,tbl_uuid, shori_rennum);
