\set ON_ERROR_STOP

alter table tbl_prvkanri add  column tbl_uuid  character(36);
alter table tbl_prvkanri add  column gyoumu_cd  numeric(3);
update tbl_prvkanri set tbl_uuid='',gyoumu_cd=0;
alter table tbl_prvkanri drop constraint  tbl_prvkanri_primary_key;
alter table tbl_prvkanri add constraint tbl_prvkanri_primary_key primary key (hospnum,tbl_key, rennum, tbl_group,tbl_uuid, shori_rennum, page);
