\set ON_ERROR_STOP

alter table tbl_chk004 add column hospnum numeric(2,0);
update tbl_chk004 set hospnum = 1;
alter table tbl_chk004 alter column hospnum set not null;
alter table tbl_chk004 drop constraint tbl_chk004_primary_key;
alter table tbl_chk004 add constraint tbl_chk004_primary_key primary key (hospnum,kanricd, kbncd, styukymd, edyukymd);
