\set ON_ERROR_STOP

alter table tbl_para add column hospnum numeric(2,0);
update tbl_para set hospnum = 1;
alter table tbl_para alter column hospnum set not null;
alter table tbl_para drop constraint tbl_para_primary_key;
alter table tbl_para add constraint tbl_para_primary_key primary key (hospnum, gyoumuid, termid, filemei, edanum);
