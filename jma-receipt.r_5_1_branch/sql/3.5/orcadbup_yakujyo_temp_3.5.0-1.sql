\set ON_ERROR_STOP

alter table tbl_yakujyo_temp add column hospnum numeric(2,0);
update tbl_yakujyo_temp set hospnum = 1;
alter table tbl_yakujyo_temp alter column hospnum set not null;
alter table tbl_yakujyo_temp drop constraint tbl_yakujyo_temp_primary_key;
alter table tbl_yakujyo_temp add constraint tbl_yakujyo_temp_primary_key primary key (hospnum, termid, srycd, yjcd, rennum);
