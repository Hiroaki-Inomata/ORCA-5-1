\set ON_ERROR_STOP

alter table tbl_syskanri add column hospnum numeric(2,0);
update tbl_syskanri set hospnum = 1;
alter table tbl_syskanri alter column hospnum set not null;
alter table tbl_syskanri drop constraint  tbl_syskanri_primary_key;
alter table tbl_syskanri add constraint tbl_syskanri_primary_key primary key (hospnum, kanricd, kbncd, styukymd, edyukymd);
