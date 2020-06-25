\set ON_ERROR_STOP

alter table tbl_inputset add column hospnum numeric(2,0);
alter table tbl_inputset drop column hospid;
update tbl_inputset set hospnum = 1;
alter table tbl_inputset alter column hospnum set not null;
alter table tbl_inputset add constraint tbl_inputset_primary_key primary key (hospnum, setcd, yukostymd, yukoedymd, setseq);
