\set ON_ERROR_STOP

alter table tbl_jobkanri add column hospnum numeric(2,0);
update tbl_jobkanri set hospnum = 1;
alter table tbl_jobkanri alter column hospnum set not null;
alter table tbl_jobkanri drop constraint tbl_jobkanri_primary_key;
alter table tbl_jobkanri add constraint tbl_jobkanri_primary_key primary key (hospnum, jobid, shellid, run_opid);
