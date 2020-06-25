\set ON_ERROR_STOP

alter table tbl_teikikanri add column hospnum numeric(2,0);
update tbl_teikikanri set hospnum = 1;
alter table tbl_teikikanri alter column hospnum set not null;
alter table tbl_teikikanri drop constraint tbl_teikikanri_primary_key;
alter table tbl_teikikanri add constraint tbl_teikikanri_primary_key primary key (hospnum, btunum);
