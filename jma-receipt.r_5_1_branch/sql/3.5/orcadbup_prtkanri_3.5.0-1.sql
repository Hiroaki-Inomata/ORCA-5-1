\set ON_ERROR_STOP

alter table tbl_prtkanri add column hospnum numeric(2,0);
update tbl_prtkanri set hospnum = 1;
alter table tbl_prtkanri alter column hospnum set not null;
alter table tbl_prtkanri drop constraint tbl_prtkanri_primary_key;
alter table tbl_prtkanri add constraint tbl_prtkanri_primary_key primary key (hospnum, tbl_key, rennum, tbl_group, shori_rennum);
