\set ON_ERROR_STOP

alter table tbl_chktrd add column hospnum numeric(2,0);
update tbl_chktrd set hospnum = 1;
alter table tbl_chktrd alter column hospnum set not null;
alter table tbl_chktrd drop constraint tbl_chktrd_primary_key;
alter table tbl_chktrd add constraint tbl_chktrd_primary_key primary key (hospnum, chkkbn, srycd, yukostymd, yukoedymd, cdkbn, rennum);
