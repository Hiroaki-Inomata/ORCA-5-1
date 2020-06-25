\set ON_ERROR_STOP

alter table tbl_adrs add column hospnum numeric(2,0);
update tbl_adrs set hospnum = 1; 
alter table tbl_adrs alter column hospnum set not null;
alter table tbl_adrs drop constraint tbl_adrs_primary_key;
alter table tbl_adrs add constraint tbl_adrs_primary_key primary key (hospnum,lpubcd, post, rennum);
drop index idx_adrs_post;
create index idx_adrs_post on tbl_adrs(hospnum,post);
