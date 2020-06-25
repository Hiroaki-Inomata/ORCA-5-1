\set ON_ERROR_STOP

alter table tbl_toukeimemo add column hospnum numeric(2,0);
update tbl_toukeimemo set hospnum = 1;
alter table tbl_toukeimemo alter column hospnum set not null;
alter table tbl_toukeimemo drop constraint tbl_toukeimemo_primary_key;
alter table tbl_toukeimemo add constraint tbl_toukeimemo_primary_key primary key (hospnum, pgid, kanricd, kbncd, styukymd, edyukymd);
